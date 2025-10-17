open MainTypes
open Stdlib

type data = Obj.t

type node = {
  mutable prev : node option;
  mutable next : node option;
  value : payload;   (* strong ref to payload object *)
  id : int;
}

and payload = {
  mutable 
  head : data Weak.t; (* weak reference to head node *)
  tail : data signal oe Weak.t; (* weak reference to tail node *)
  mutable updated : bool;         (* flag for "has been updated" *) (* should we move this to the node? *)
}

type linkedList = {
  mutable head : node option;
  mutable tail : node option;
  mutable cursor : node;  (* for iteration *)
  mutable len  : int;
  mutable next_id : int;
}

let create_dummy_node () =
  { prev = None; next = None; value = Obj.magic (); id = -1 }

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

let alloc : type a. a -> a signal oe -> int =
  fun s t -> 
    let cursor = heap.cursor in
    let head : data Weak.t = Weak.create 1 in
    Weak.set head 0 (Some (Obj.magic s));
    let tail : data signal oe Weak.t = Weak.create 1 in
    Weak.set tail 0 (Some (Obj.magic t));
    let payload = { head; tail; updated = false } in
    let new_node = { prev = cursor.prev; next = Some cursor; value = payload; id = heap.next_id } in
    (match cursor.prev with 
    | None -> failwith "Heap invariant broken: cursor prev is None"
    | Some p -> p.next <- Some new_node);
    cursor.prev <- Some new_node;
    heap.len <- heap.len + 1;
    let signal_id = heap.next_id in
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
    Weak.set n.value.head 0 (Some (Obj.magic s));
    Weak.set n.value.tail 0 (Some (Obj.magic t));
    (* n.value.updated <- true; *)
    ()

let find (id: int) = 
  let rec aux n =
    match n with
    | None -> None
    | Some nn ->
        if nn = heap.cursor then None
        else if nn.id = id then Some nn
        else aux nn.next in
  aux heap.head

(* let _iter f = ()  *)

let print_heap () =
  let rec aux n = 
    match n with
    | None -> ()
    | Some nn -> 
        Printf.printf "%d " nn.id;
        aux nn.next in
  aux heap.head;
  Printf.printf "\n"

let payload_head : type a. payload -> a option = 
  fun payload -> 
    match Weak.get (payload.head) 0 with
    | None -> None
    | Some v -> Some (Obj.magic v : a)
  
let payload_tail : type a . payload -> a signal oe option = 
  fun payload -> 
    match Weak.get (payload.tail) 0 with
    | None -> None
    | Some v -> Some (Obj.magic v : a signal oe) 

let rec ticked : type a . int channel -> a oe -> bool =
  fun k v ->
    match v with
    | Never -> false
    | App (_, x) -> 
      ticked k x
    | Wait (Index k') -> 
      let Index kv = k in 
      kv = k'
    | Sync (u1, u2) ->
      ticked k u1 || ticked k u2
    | Trig s ->
      let id = match s with Identifier i -> i in
      let signal_node = match find id with 
        | None -> failwith ("Heap.ticked: triggered signal with id " ^ string_of_int id ^ " not found")
        | Some n -> n 
      in
      signal_node.value.updated
    | Tail (Identifier l) -> 
      let signal_node = match find l with 
        | None -> failwith ("Heap.ticked: tail signal with id " ^ string_of_int l ^ " not found")
        | Some n -> n 
      in
      let tail = match payload_tail signal_node.value with
        | None -> failwith "Heap.ticked: tail is None"
        | Some t -> t
      in
      ticked k tail

let rec advance : type a . int channel -> a oe -> int -> a =
  fun k u w ->
    match u with 
    | Wait (Index k') -> 
      let kv = match k with Index _i -> _i in
      if kv = k' then Obj.magic w
      else failwith "Heap.adv: channel mismatch"
    | Tail s -> s
    | App (f, x) -> 
      let x_val = advance k x w in 
      let f_val = f() x_val in
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
      let id = match s with Identifier i -> i in
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

let step_cursor : int channel -> int -> unit = fun k v -> 
  let cur = heap.cursor in
  let cur_payload = cur.value in
  (* TODO: double-check if this is here we should delete :) *)
  match payload_tail cur_payload with
  | None -> delete heap.cursor; incr_cursor ()
  | Some v2 -> 
    if not @@ ticked k v2 then 
      let () = cur_payload.updated <- false in
      incr_cursor ()
    else
      let Identifier l' = advance k v2 v in
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