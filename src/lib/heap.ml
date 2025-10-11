open MainTypes

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

let insert s t = let open Stdlib in alloc s t |> ignore

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