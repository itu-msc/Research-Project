open Types

module SignalUtils = struct 
  open Internals.Heap
  open Internals.MainTypes
  (* helper since we cannot pattern match, and the information is on the heap *)
  let hd_tail (s : 'a signal)  = 
    let id = signal_id s in
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
      | Some t -> Obj.magic t
    in
    (hd, tl)

    let alloc_signal : 'a -> 'a signal oe -> 'a signal = 
      fun s t -> signal_of_ref (alloc s t)
end

let ( @: ) : 'a -> 'a signal oe -> 'a signal = SignalUtils.alloc_signal

let const x = x @: never

let rec mkSig k = (fun a -> a @: mkSig k) |>> wait k

let init_signal k v =
  v @: mkSig k

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

let rec scan f b s = 
  let hd, tl = SignalUtils.hd_tail s in
  let b' = f b hd in
  b' @: (scan f b' |>> tl)

let sample xs ys = 
  map (fun x -> (x, head ys)) xs

let rec jump f s =
  let cont s = match f (head s) with
  | None -> jump f s
  | Some s' -> s'
  in
  head s @: (cont |>> (tail s))

(* TODO: probably a very wrong implementation *)
let rec interleave : ('a -> 'a -> 'a) -> 'a signal -> 'a signal -> 'a signal =
  fun f xs ys ->
    let cont = function
    | Fst xs'         -> f (head xs') (head ys ) @: (tail @@ interleave f xs' ys)
    | Snd ys'         -> f (head xs ) (head ys') @: (tail @@ interleave f xs ys')
    | Both (xs', ys') -> f (head xs') (head ys') @: (tail @@ interleave f xs' ys')
    in
    f (head xs) (head ys) @: (cont |>> (sync (tail xs) (tail ys) ))

(* TODO: Not thread safe, this can cause race conditions.
  When channels can be other things than int then change this to use float. *)
let clock_channel interval =
  let start = Unix.gettimeofday () +. interval in
  let chan = new_channel () in
  let stop_flag = ref false in
  let rec aux next =
    if !stop_flag then () else
    try
      let now = Unix.gettimeofday () in
      let wait_time = max 0.0 (next -. now) in
      Unix.sleepf wait_time;
      Internals.Heap.step chan (int_of_float next); (* remove int conversion if we want float *)
      aux (next +. interval)
    with exn ->
      prerr_endline ("clock thread error: " ^ Printexc.to_string exn);
      (* simple backoff to avoid tight crash loop, then continue schedule *)
      Unix.sleepf (max 0.1 interval);
      aux (next +. interval)
  in
  let th = Thread.create (fun () -> aux start) () in
  (* return channel and a stop function so caller can cancel the clock cleanly *)
  (chan, (fun () -> stop_flag := true; Thread.join th))

let clock_signal interval =
  let chan, stop = clock_channel interval in
  let signal = init_signal chan (int_of_float @@ Unix.gettimeofday ()) in
  (signal, stop)

