open MainTypes

type data = Obj.t

type node = {
  mutable prev : node option;
  mutable next : node option;
  value : payload;   (* strong ref to payload object *)
  id : int ref Weak.t; (* can delete node if id is no longer reachable from user *)
}

(* payload holds "strong" references, node.id decides wether to release the data *)
and payload = {
  mutable head : data; 
  mutable tail : data signal oe; 
  mutable updated : bool;         (* flag for "has been updated" *) (* should we move this to the node? *)
}

type linkedList = {
  mutable head : node option;
  mutable tail : node option;
  mutable cursor : node;  (* for iteration *)
  mutable len  : int;
  mutable next_id : int;
}

let dummy_id = ref (-1)

let create_id_weak_ref id_ref = 
  let w = Weak.create 1 in
  Weak.set w 0 (Some id_ref); 
  w
let get_id_ref w = Weak.get w 0

let create_dummy_node () =
  { prev = None; next = None; value = Obj.magic (); id = create_id_weak_ref dummy_id }

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

let alloc : type a. a -> a signal oe -> int ref =
  fun s t -> 
    let cursor = heap.cursor in
    let head : data = Obj.magic s in
    let tail : data signal oe = Obj.magic t in 
    let payload = { head; tail; updated = false } in
    let signal_id = ref heap.next_id in
    let new_node = { prev = cursor.prev; next = Some cursor; value = payload; id = create_id_weak_ref signal_id } in
    (match cursor.prev with 
    | None -> failwith "Heap invariant broken: cursor prev is None"
    | Some p -> p.next <- Some new_node);
    cursor.prev <- Some new_node;
    heap.len <- heap.len + 1;
    heap.next_id <- heap.next_id + 1;
    signal_id

let insert s t = alloc s t |> ignore

let delete (node: node) =
  match node.prev, node.next with
  | Some p, Some n -> 
      p.next <- Some n;
      n.prev <- Some p;
      heap.len <- heap.len - 1;
      ()
  | _ -> failwith "Heap invariant broken: node to delete has None prev or next"

(* is this what we want?? *)
(* let update (n: node) (s: 'a) (t: 'a signal oe) = *)
let update : type a. node -> a -> a signal oe -> unit =
  fun n s t ->
    n.value.head <- Obj.magic s;
    n.value.tail <- Obj.magic t;
    ()

let find (id: int) = 
  let rec aux n =
    match n with
    | None -> None
    | Some nn -> match get_id_ref nn.id with
      | None -> aux nn.next (* gc was here*)
      | Some {contents = nn_id} ->
        if nn = heap.cursor then None
        else if nn_id = id then Some nn
        else aux nn.next in
  aux heap.head

(* let _iter f = ()  *)

let print_heap () =
  let rec aux n = 
    match n with
    | None -> ()
    | Some nn -> match Weak.get nn.id 0 with
      | None -> aux nn.next (* gc'ed *) 
      | Some { contents = id } -> 
        Printf.printf "%d " id;
        aux nn.next in
  aux heap.head;
  Printf.printf "\n"

let payload_head : type a. payload -> a option = 
  fun payload -> Some (Obj.magic payload.head)
  
let payload_tail : type a . payload -> a signal oe option = 
  fun payload -> Some (Obj.magic payload.tail)

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
      let id = signal_id s in
      let signal_node = match find id with 
        | None -> failwith ("Heap.ticked: triggered signal with id " ^ string_of_int id ^ " not found")
        | Some n -> n 
      in
      signal_node.value.updated
    | Tail s ->
      let l = signal_id s in 
      let signal_node = match find l with 
        | None -> failwith ("Heap.ticked: tail signal with id " ^ string_of_int l ^ " not found")
        | Some n -> n 
      in
      let tail = match payload_tail signal_node.value with
        | None -> failwith "Heap.ticked: tail is None"
        | Some t -> t
      in
      ticked k tail

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
      let id = signal_id s in
      let signal_node = match find id with 
        | None -> failwith ("Heap.adv: triggered signal with id " ^ string_of_int id ^ " not found")
        | Some n -> n 
      in
      if signal_node.value.updated then
        let hd = payload_head signal_node.value in
        (match hd with
        | Some v -> v
        | None -> failwith "Heap.adv: triggered signal has None value")
      else
        failwith "Heap.adv: triggered signal not updated" (* what are we suposed to do here *)
    | Never -> failwith "Heap.adv: cannot advance Never oe"


let incr_cursor () = 
  match heap.cursor.next with
  | None -> failwith "cursor should never reach here"
  | Some next -> heap.cursor <- next

let step_cursor : 'a channel -> 'a -> unit = fun k v -> 
  let cur = heap.cursor in
  let cur_payload = cur.value in
  let v2 = cur_payload.tail in
  (* TODO: double-check if this is here we should delete :) *)
  (* match get_id_ref cur.id with *)
  match payload_tail cur_payload with
  | None -> delete heap.cursor; incr_cursor ()
  | Some _ -> 
    if not @@ ticked k v2 then 
      let () = cur_payload.updated <- false in
      incr_cursor ()
    else
      let l' = signal_id (advance k v2 v) in
      (* TODO: fix pattern matching *)
      let node = Option.get @@ find l' in
      let v1' = Option.get @@ payload_head node.value in
      let v2' = Option.get @@ payload_tail node.value in
      update cur v1' v2';
      cur_payload.updated <- true;
      incr_cursor ()

let step k v : unit = 
  let rec inner : unit -> unit = fun () ->
    if Option.is_none heap.cursor.next then ()
    else let () = step_cursor k v in inner () 
  in 
  reset_cursor ();
  inner ()