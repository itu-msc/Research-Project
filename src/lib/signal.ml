open Types

let ( @: ) = fun h t -> Internals.MainTypes.signal_of_data (Internals.Heap.alloc h t)

let const x = x @: never

let head s = (Internals.MainTypes.signal_get_data s).head

let rec mkSig d = (fun a -> a @: (mkSig d)) |>> d
let mkSig_of_channel k = mkSig @@ wait k

let init_signal k v =
  v @: mkSig_of_channel k

let rec map f s = f (head s) @: (map f |>> tail s)
let mapL f s = map f |>> s

let rec map2 f xs ys =
  let cont = function
    | Fst xs' -> map2 f xs' ys
    | Snd ys' -> map2 f xs  ys'
    | Both (xs',ys') -> map2 f xs' ys'
  in f (head xs) (head ys) @: (cont |>> sync (tail xs) (tail ys))

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

(** repeatedly switch whenever `d` ticks *)
let rec switchR s d =
  let d' = (fun s' x -> switchR (head s' x) (tail s') ) |>> d in
  switchS s d'

let pp_signal pp_a out s =
  let hd, tl = head s, tail s in
  Format.fprintf out "%a :: later(%a)" pp_a hd pp_later tl

let rec scan f b s =
  let hd, tl = head s, tail s in
  let b' = f b hd in
  b' @: (scan f b' |>> tl)

let scanL f b s = scan f b |>> s

let sample xs ys =
  map (fun x -> (x, head ys)) xs

let sampleL xs ys = 
  map (fun x -> (x, head ys)) |>> xs

let rec jump f s =
  let cont s = match f (head s) with
  | None -> jump f s
  | Some s' -> s'
  in
  head s @: (cont |>> (tail s))

let interleave : ('a -> 'a -> 'a) -> 'a signal -> 'a signal -> 'a signal =
  fun f xs ys ->
    (* Produce a value whenever either input signal advances.
       If only xs advances emit its new head; if only ys advances emit its new head;
       if both advance simultaneously emit f applied to their new heads.
       Initial element chosen as the current head of xs. *)
    let rec build current xs ys =
      let cont = function
        | Fst xs' -> build (head xs') xs' ys
        | Snd ys' -> build (head ys') xs ys'
        | Both (xs', ys') -> build (f (head xs') (head ys')) xs' ys'
      in
      current @: (cont |>> sync (tail xs) (tail ys))
    in
    build (f (head xs) (head ys)) xs ys

let filter_map p s = mkSig (trig (p (head s) @: mapL p (tail s)))
let filter_mapL p s = mkSig (trig (None @: mapL p s))

let filter p = filter_map (fun x -> if p x then Some x else None)

let filterL p = filter_mapL (fun x -> if p x then Some x else None)

let trigger f s1 s2 =
  sample s1 s2 |> map (fun (a,b) -> f a b)

let triggerL (f: 'a -> 'b -> 'c) (s1 : 'a signal later) (s2 : 'b signal) : 'c signal later =
  (fun s1' -> trigger f s1' s2) |>> s1

(* returns a channel that produces a tick every [interval] seconds,
    starting [interval] seconds from now *)
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
      Internals.Heap.step chan next;
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

let clock_signalL interval =
  let chan, stop = clock_channel interval in
  let signal = mkSig_of_channel chan in
  (signal, stop)

(* TODO: Make the output methods return a callback that can clean up the output signal from the list *)
let output_signals = ref []

let console_output (s : string signal) : unit =
  output_signals := map print_endline s :: !output_signals

let console_outputL (s : string signal later) : unit =
  output_signals := switch (const ()) (mapL print_endline s) :: !output_signals

let set_quit (s : 'a signal later) : unit =
  output_signals := switch (const ()) (mapL (fun _ -> exit 0) s) :: !output_signals

let start_event_loop () : unit =
  while true do
    Thread.delay 1.0 (* adjust as needed; smaller = more responsive, larger = less CPU *)
  done

(** Send newline-terminated strings from a later string signal
  to a TCP connection.

  Parameters:
  - address:  the IPv4 address to connect to (use Unix.inet_addr_loopback for localhost)
  - port:     the TCP port number to connect to
  - s:        a later string signal to send values from

  Behavior:
  - Opens a TCP (PF_INET, SOCK_STREAM) connection to (address, port) once when called
    and obtains an OCaml out_channel for writing.
  - Registers a sender that writes each incoming string value followed by "\n" and flushes.
  - On write errors the channel is closed and the internal connection is marked None;
    subsequent sends will be no-ops (there is no automatic reconnect logic).
  - The sender is stored in output_signals so it will be driven by the runtime/event loop. 
*)
let port_outputL address port (s : string signal later) : unit =
  let connect () =
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.connect sock (Unix.ADDR_INET (address, port));
    Unix.out_channel_of_descr sock
  in
  let out_chan = ref (Some (connect ())) in
  let send v =
    match !out_chan with
    | None ->
      ()
    | Some ch ->
      try
        output_string ch (v ^ "\n");
        (* print_endline ("sent to port " ^ string_of_int port ^ ": " ^ v); *)
        flush ch;
        ()
      with exn ->
        prerr_endline ("port_send_output error: " ^ Printexc.to_string exn);
        (try close_out ch with _ -> ());
        out_chan := None;
        ()
  in
  output_signals := switch (const ()) (mapL send s) :: !output_signals
