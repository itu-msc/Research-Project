open MainTypes

type 'a node = {
  mutable prev : 'a node option;
  mutable next : 'a node option;
  value : 'a payload;   (* strong ref to payload object *)
  id : int;
}

and 'a payload = {
  mutable 
  head : 'a Weak.t; (* weak reference to head node *)
  tail : 'a signal oe Weak.t; (* weak reference to tail node *)
  mutable updated : bool;         (* flag for "has been updated" *)
}

type 'a linkedList = {
  mutable head : 'a node option;
  mutable tail : 'a node option;
  mutable cursor : 'a node;  (* for iteration *)
  mutable len  : int;
  mutable next_id : int;
}

let create_dummy_node () =
  { prev = None; next = None; value = Obj.magic (); id = -1 }

let heap: int linkedList = 
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

let insert (s: 'a) (t: 'a signal oe) = 
  let cursor = heap.cursor in
  let head = Weak.create 1 in
  Weak.set head 0 (Some s);
  let tail = Weak.create 1 in
  Weak.set tail 0 (Some t);
  let payload = { head; tail; updated = false } in
  let new_node = { prev = cursor.prev; next = Some cursor; value = payload; id = heap.next_id } in
  (match cursor.prev with 
  | None -> failwith "Heap invariant broken: cursor prev is None"
  | Some p -> p.next <- Some new_node);
  cursor.prev <- Some new_node;
  heap.next_id <- heap.next_id + 1;
  heap.len <- heap.len + 1;
  ()

let _delete = 0
let _update = 0
let _find (_label: int) = 0
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