open Types

let ( @: ) = fun h t -> Internals.MainTypes.signal_of_data (Internals.Heap.alloc h t)

let const x = x @: never

let head s = (Internals.MainTypes.signal_get_data s).head

let mkSig k =
  let rec aux k = 
    let f = delay (fun a -> a @: aux k) in
    (* Printf.printf "aux creating an O> - memory of f: %d\n" (Obj.magic f); *)
    app f (wait k)
    
  in
  aux k

let init_signal k v =
  v @: mkSig k

let rec map f s = f (head s) @: (map f |>> tail s)

let rec switch s d = 
  let cont = function
    | Fst s' -> switch s' d
    | Snd d' -> d'
    | Both (_, d') -> d' in
  head s @: (cont |>> (sync (tail s) d))

let rec zip xs ys =
  let cont = function
    | Fst xs' -> zip xs' ys
    | Snd ys' -> zip xs  ys'
    | Both (xs', ys') -> zip xs' ys'
  in
  (head xs, head ys) @: (cont |>> sync (tail xs) (tail ys))

let rec switchS s d = 
  let x, xs = head s, tail s in
  let cont = function 
    | Fst xs' -> switchS xs' d
    | Snd f -> f x
    | Both(_,f) -> f x
  in
  x @: (cont |>> sync xs d)

let rec switchR s d = 
  let x, xs = head s, tail s in
  let cont = function
    | Fst xs' -> switchR xs' d
    | Snd fs -> head fs x
    | Both (_, fs) -> head fs x
  in
  x @: (cont |>> sync xs d)

let pp_signal pp_a out s =
  let hd, tl = head s, tail s in
  Format.fprintf out "%a :: oe(%a)" pp_a hd pp_oe tl

let rec scan f b s = 
  let hd, tl = head s, tail s in
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
      Internals.Heap.step chan next; (* remove int conversion if we want float *)
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
  let signal = init_signal chan (Unix.gettimeofday ()) in
  (signal, stop)

let output_signals = ref []

let console_output (s : string signal) : unit =
  output_signals := map print_endline s :: !output_signals

let start_event_loop () : unit = 
  while true do
    Thread.delay 0.05 (* adjust as needed; smaller = more responsive, larger = less CPU *)
  done