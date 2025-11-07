open! Rizzo.Types
open! Rizzo.Signal
open! Rizzo.Channel

let () =
  let inputChan = console_input () in
  let inputSig =  init_signal inputChan "" in

  let formattet_signal = map (fun v -> "Input 1: " ^ v) inputSig in
  
  console_output formattet_signal

(* let () = 
    let c = new_channel () in
    let _in_signal = "" @: mkSig c in
    (* let _some_signal = map Fun.id _in_signal in *)
    (* let _ = map ((^) "hi: ") in_signal in  *)
    print_endline "---";
    step c "";
    print_endline "---";
    step c "";
    print_endline "---";
    step c ""; *)

let () =
  (* let one_sec, stop = clock_signal 1.0 in *)
  let inputChan = console_input () in
  let inputSig = init_signal inputChan "" in
  let countSig = scan (fun c _ -> c + 1) 0 inputSig in
  (* let countSeconds = scan (fun c _ -> c + 1) 0 one_sec in *)
  console_output (map (Format.asprintf "You have written something '%a' times after this many secconds" Format.pp_print_int) countSig);
  console_output (map (fun s ->
    Rizzo.Internals.Heap.print_heap ();
    "input 2: " ^ s
  ) inputSig);

  (* console_output (map (Format.asprintf "We have been going for %a" Format.pp_print_int) countSeconds); *)

  start_event_loop ();
  (* stop () *)



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
  