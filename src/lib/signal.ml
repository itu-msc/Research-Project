open MainTypes
open Heap

module SignalUtils = struct 
  (* helper since we cannot pattern match, and the information is on the heap *)
  let hd_tail (s : 'a signal)  = 
    let id = match s with Identifier i -> i in
    let node = match find id with
      | None -> failwith ("map: signal with id " ^ string_of_int id ^ " not found")
      | Some n -> n 
    in 
    (* must go through heap to retrieve the head and tail directly from heap *)
    let hd : 'a = 
      match Heap.payload_head node.value with
      | None -> failwith "map: head is None"
      | Some h -> h
    in 
    let tl : 'a signal oe = 
      match Heap.payload_tail node.value with
      | None -> failwith "map: tail is None"
      | Some t -> t
    in
    (hd, tl)
end

let alloc_signal : 'a -> 'a signal oe -> 'a signal =
  fun s t -> Identifier (Heap.alloc s t) 

let ( @: ) : 'a -> 'a signal oe -> 'a signal = alloc_signal

let const x = x @: never

let rec mkSig k = (fun a -> a @: mkSig k) |> wait k

let rec map f s = match SignalUtils.hd_tail s with
  | hd, tl -> f hd @: (map f |> tl)

let rec switch s d = 
  let cont = function
    | Fst s' -> switch s' d
    | Snd d' -> d'
    | Both (_, d') -> d' in
  let hd, tl = SignalUtils.hd_tail s in
  hd @: (cont |> (sync tl d))

let head s = fst (SignalUtils.hd_tail s)