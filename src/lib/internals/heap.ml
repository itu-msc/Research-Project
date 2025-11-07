open MainTypes

type data = Obj.t

type node = {
  mutable prev : node option;
  mutable next : node option;
  id : int;
  value : data signal_data Weak.t;
}

type linkedList = {
  mutable head : node option;
  mutable tail : node option;
  mutable cursor : node;  (* for iteration *)
  mutable len  : int;
  mutable next_id : int;
}

let dummy_id = -1

let node_get_data : type a. node -> a signal_data option = 
  fun n -> match Weak.get n.value 0 with
    | None -> None (* gc'ed *)
    | x -> Obj.magic x

let create_dummy_node () =
  { prev = None; next = None; value = Weak.create 1; id = dummy_id; }

let heap: linkedList = 
  let head = create_dummy_node () in
  let tail = create_dummy_node () in
  head.next <- Some tail;
  tail.prev <- Some head;
  { 
    head = Some (head); 
    tail = Some (tail); 
    cursor = tail;
    len = 0; 
    next_id = 0 
  }

let reset_cursor () =
  match heap.head with
  | None -> failwith "Heap invariant broken: head is None"
  | Some h -> 
      match h.next with
      | None -> failwith "Heap invariant broken: tail is None"
      | Some t -> 
          heap.cursor <- t;
          ()

let get_now_tail () =
  match heap.cursor.prev with
  | None -> failwith "Heap invariant broken: tail is None"
  | Some t -> t

let alloc : type a. a -> a signal oe -> a signal_data =
  fun s t -> 
    let cursor = heap.cursor in
    let signal_id = heap.next_id in
    let signal_data = { id = signal_id; head = s; tail = t; updated = false } in
    let signal_data_weak = 
      let w = Weak.create 1 in 
      let to_insert = Some (Obj.magic signal_data : data signal_data) in
      Weak.set w 0 to_insert; w 
    in
    let new_node = { 
      id = signal_id;
      prev = cursor.prev; next = Some cursor;
      value = signal_data_weak;
    } in
    (match cursor.prev with 
    | None -> failwith "Heap invariant broken: cursor prev is None"
    | Some p -> p.next <- Some new_node);
    cursor.prev <- Some new_node;
    heap.len <- heap.len + 1;
    heap.next_id <- heap.next_id + 1;
    signal_data

let insert s t = alloc s t |> ignore

let delete (node: node) =
  match node.prev, node.next with
  | Some p, Some n -> 
      p.next <- Some n;
      n.prev <- Some p;
      heap.len <- heap.len - 1;
      ()
  | _ -> failwith "Heap invariant broken: node to delete has None prev or next"

let update : type a. node -> a -> a signal oe -> unit =
  fun n s t -> 
    match node_get_data n with
    | None -> () (* gc'ed *)
    | Some d -> d.head <- s; d.tail <- t

let find (id: int) = 
  let rec aux n =
    match n with
    | None -> None
    | Some nn ->
        if nn = heap.cursor then None
        else if nn.id = id then Some nn
        else aux nn.next in
  aux heap.head

let print_heap () =
  let rec aux n = 
    match n with
    | None -> ()
    | Some nn when nn.id = dummy_id -> Printf.printf "| "; aux nn.next
    | Some nn -> match node_get_data nn with
      | None -> aux nn.next (* gc'ed *)
      | Some _ -> 
        let id = nn.id in
        if nn == heap.cursor then Printf.printf "(%d) " id
        else Printf.printf "%d " id; aux nn.next
  in
  aux heap.head;
  Printf.printf "\n"

(* does channel and oe have to be the same type here? *)
let rec ticked : type a . 'b channel -> a oe -> bool =
  fun k v ->
    match v with
    | Never -> false
    | App (_, x) -> 
      ticked k x
    | Wait k' -> 
      let id_k' = channel_id k' in
      let id_k  = channel_id k in 
      id_k' = id_k
    | Sync (u1, u2) ->
      ticked k u1 || ticked k u2
    | Trig s ->
      (* TODO: should we still check the signal is in heap? *)
      let data = signal_get_data s in
      data.updated
    | Tail s ->
      let data = signal_get_data s in
      ticked k data.tail

let rec advance : type a . 'b channel -> a oe -> 'b -> a =
  fun k u w ->
    match u with 
    | Wait k' -> 
      let id_k' = channel_id k' in
      let id_k  = channel_id k in
      if id_k = id_k' then Obj.magic w
      else failwith "Heap.adv: channel mismatch"
    | Tail s -> s
    | App (f, x) -> 
      let x_val = advance k x w in 
      let f_val = adv f x_val in
      f_val
    | Sync (v1, v2) ->
        let u1Ticked = ticked k v1 in
        let u2Ticked = ticked k v2 in
        (match (u1Ticked, u2Ticked) with
        | (true, false) ->
            Fst (advance k v1 w)
        | (false, true) ->
            let w2 = advance k v2 w in
            Snd w2
        | (true, true) ->
            let w1 = advance k v1 w in
            let w2 = advance k v2 w in
            Both (w1, w2)
        | (false, false) ->
            failwith "Heap.adv: neither side of Sync ticked")
    | Trig s ->
      let signal_data = signal_get_data s in
      if not signal_data.updated then failwith "Heap.adv: triggered signal not updated"
      else 
        (match signal_data.head with
        | None -> failwith "Heap.adv: triggered signal has None value" (* TODO: is this really an error? *)
        | Some v -> v)
    | Never -> failwith "Heap.adv: cannot advance Never oe"


let incr_cursor () = 
  match heap.cursor.next with
  | None -> failwith "cursor should never reach here"
  | Some next -> heap.cursor <- next

let step_cursor : 'a channel -> 'a -> unit = fun k v ->
  (* TODO: double-check if this is here we should delete :) *)
  match node_get_data heap.cursor with
  | None -> delete heap.cursor; incr_cursor ()
  | Some cursor_data -> 
    let {tail = v2; _} : 'a signal_data = cursor_data in
    if not @@ ticked k v2 then 
      let () = cursor_data.updated <- false in
      incr_cursor ()
    else
      let v' = signal_get_data (advance k v2 v) in
      update heap.cursor v'.head v'.tail;
      cursor_data.updated <- true;
      incr_cursor ()

(* add thread safe thing here *)
let step k v : unit = 
  let rec inner : unit -> unit = fun () ->
    if Option.is_none heap.cursor.next then ()
    else let () = step_cursor k v in inner () 
  in 
  reset_cursor ();
  inner ();
  Gc.full_major ()
