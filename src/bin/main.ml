open! Rizzo.Types
open! Rizzo.Signal
open! Rizzo.Channel

let _paper_example =
  let debug = false in
  let every_second, every_second_stop = clock_signal 1.0 in
  let port_input = mkSig_of_channel @@ port_input 9000 in
  let read_int = int_of_string_opt in
  let nats init = scan (fun n i -> if debug then print_endline ("tick: " ^ string_of_int (n + 1) ^ " time: " ^ (string_of_int (int_of_float i))); n + 1) init every_second in

  let console  = console_input () |> wait |> mkSig in
  let quit_sig = filter ((=) "quit") console in
  let show_sig = filter ((=) "show") console in
  let neg_sig  = filter ((=) "negate") console in
  (* let echo_sig = filter ((=) "echo") console in *)
  let num_sig  = filter_map read_int console in
  let sig' =
    interleave
      Fun.compose
      (map (fun _ n -> -n   ) ("" @: neg_sig))
      (map (fun m n -> m + n) (0  @: num_sig))
  in
  let rec nats' init =
    switchS (nats init) ((fun s n -> nats' (head s n)) |>> tail sig')
  in
  let show_nat = triggerD (fun _ n -> n) show_sig (nats' 0)  in
  console_outputD (mapD string_of_int show_nat);
  console_outputD (mapD (fun s -> "echo: " ^ s) port_input);
  set_quit quit_sig;
  start_event_loop ();
  every_second_stop ()
      

(* 
PowerShell to write to port 9000:
$client = New-Object System.Net.Sockets.TcpClient('localhost', 9000)
$stream = $client.GetStream()
$writer = New-Object System.IO.StreamWriter($stream)
$writer.AutoFlush = $true
$writer.WriteLine('show')
$writer.WriteLine('quit')
$writer.Close()
$stream.Close()
$client.Close() *)

(* let () =
  let one_sec, stop = clock_signal 1.0 in
  let inputChan = console_input () in
  let inputSig = init_signal inputChan "" in
  let countSig = scan (fun c _ -> c + 1) 0 inputSig in
  let countSeconds = scan (fun c _ -> c + 1) 0 one_sec in
  let thirdSec = 0 @: filter (fun c -> c mod 3 = 0) (tail countSeconds) in
  console_output (map (Format.asprintf "You have written something '%a' times after this many secconds" Format.pp_print_int) countSig);
  console_output (map (fun s ->
    Rizzo.Internals.Heap.print_heap ();
    "input 2: " ^ s
  ) inputSig);

  console_output (map (Format.asprintf "We have been going for %a" Format.pp_print_int) countSeconds);
  console_output (map (Format.asprintf "FIZZ: %a" Format.pp_print_int) thirdSec);

  start_event_loop ();
  stop () *)



(* Example usage of the 'oe' type *)

(* 

let () = 
  insert 42 (tail (const 1)); 
  insert 43 (tail (const 2)); 
  insert 44 (tail (const 3))
  
let () = print_heap ()

let () = 
  reset_cursor (); 
  insert 45 (tail (const 4)); 
  insert 46 (tail (const 5))

let () = print_heap () 
*)

(* open Unix *)


(* let every ?(start_now=false) period_s f =
  let next = ref (gettimeofday () +. if start_now then 0.0 else period_s) in
  let rec loop () =
    let now = gettimeofday () in
    let sleep_s = max 0.0 (!next -. now) in
    sleepf sleep_s;
    f ();
    next := !next +. period_s;
    loop ()
  in
  ignore (Thread.create loop ()) *)

(* let () =
  every 1.0 (fun () -> print_endline "tick");
  (* keep main alive; replace with your main logic *)
  let rec forever () = Unix.sleep 3600; forever () in
  forever () *)

(* not working on windows *)
(* let () =
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> print_endline "tick signal"));
  let it = { Unix.it_interval = 1.0; it_value = 1.0 } in
  ignore (Unix.setitimer Unix.ITIMER_REAL it);
  while true do
    Unix.sleep 10
  done in
  ignore () *)

(* let () =
  let rec tick () =
    Unix.sleep 1;
    print_endline "tick";
    tick ()
  in
  ignore (Thread.create tick ());
  (* keep main alive; replace with your main logic *)
  let rec forever () = Unix.sleep 3600; forever () in
  forever () *)
  
(* open Lwt.Infix *)

(* let rec tick () =
  Lwt_unix.sleep 1.0 >>= fun () ->
  print_endline "tick";
  tick ()

let () = Lwt_main.run (tick ()) *)
  