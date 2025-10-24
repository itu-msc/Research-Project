open Internals.MainTypes
open Internals.Heap

module SignalUtils = struct 
  (* helper since we cannot pattern match, and the information is on the heap *)
  let hd_tail (s : 'a signal)  = 
    let id = match s with Identifier i -> !i in
    let node = match find id with
      | None -> failwith ("SignalUtils: signal with id " ^ string_of_int id ^ " not found")
      | Some n -> n 
    in 
    (* must go through heap to retrieve the head and tail directly from heap *)
    let hd : 'a = 
      match payload_head node.value with
      | None -> failwith "SignalUtils: head is None"
      | Some h -> h
    in 
    let tl : 'a signal oe = 
      match payload_tail node.value with
      | None -> failwith "SignalUtils: tail is None"
      | Some t -> t
    in
    (hd, tl)
end

let alloc_signal : 'a -> 'a signal oe -> 'a signal =
  fun s t -> Identifier (alloc s t) 

let ( @: ) : 'a -> 'a signal oe -> 'a signal = alloc_signal

let const x = x @: never

let rec mkSig k = (fun a -> a @: mkSig k) |>> wait k

let rec map f s = match SignalUtils.hd_tail s with
  | hd, tl -> f hd @: (map f |>> tl)

let rec switch s d = 
  let cont = function
    | Fst s' -> switch s' d
    | Snd d' -> d'
    | Both (_, d') -> d' in
  let hd, tl = SignalUtils.hd_tail s in
  hd @: (cont |>> (sync tl d))

let head s = fst (SignalUtils.hd_tail s)

let rec zip xs ys =
  let cont = function
    | Fst xs' -> zip xs' ys
    | Snd ys' -> zip xs  ys'
    | Both (xs', ys') -> zip xs' ys'
  in
  (head xs, head ys) @: (cont |>> sync (tail xs) (tail ys))

let rec switchS s d = 
  let x, xs = SignalUtils.hd_tail s in
  let cont = function 
    | Fst xs' -> switchS xs' d
    | Snd f -> f x
    | Both(_,f) -> f x
  in
  x @: (cont |>> sync xs d)

let rec switchR s d = 
  let x, xs = SignalUtils.hd_tail s in
  let cont = function
    | Fst xs' -> switchR xs' d
    | Snd fs -> head fs x
    | Both (_, fs) -> head fs x
  in
  x @: (cont |>> sync xs d)

let pp_signal pp_a out s =
  let hd, tl = SignalUtils.hd_tail s in
  Format.fprintf out "%a :: oe(%a)" pp_a hd pp_oe tl