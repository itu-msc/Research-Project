open Rizzo.Signal
open Rizzo.Types
open Rizzo.Channel

(* just see if the switch works, if it correctly changes the 'clock' at run time *)
let () = 
  let c1 : int channel = new_channel () in
  let slow_chan : int channel = new_channel () in
  let numbers = 0 @: mkSig (wait c1) in
  let numbers_and_double = zip numbers (map (( * ) 2) numbers) in
  let switched = switch numbers (mkSig (wait (slow_chan))) in
  let pp_int_signal = pp_signal Format.pp_print_int in
  let pp_int_pair_signal = pp_signal (fun out p -> Format.fprintf out "(%a, %a)" Format.pp_print_int (fst p) Format.pp_print_int (snd p)) in
  
  let rec inner cnt = 
    if cnt = 5 then 
      let () = step slow_chan 1050 in
      let () = Format.print_string "Stepped on the slow channel:\n" in
      Format.printf "[%a] switched: %a\n" Format.pp_print_int cnt pp_int_signal switched
    else 
      let () = Format.printf "[%a] switched: %a\n" Format.pp_print_int cnt pp_int_signal switched in
      let () = Format.printf "[%a] numbers_and_double: %a\n\n" Format.pp_print_int cnt pp_int_pair_signal numbers_and_double in
      let () = step c1 cnt in
      inner (cnt + 1) 
  in
  inner 0

let () = 
  let clock_signal, stop = clock_signal 1.0 in
  let chan = new_channel () in
  let other_signal = init_signal chan 0 in
  step chan 100;

  print_endline ("Head of clock_signale signal: " ^ string_of_float (head clock_signal));
  (* print_endline ("Tail of clockSignal: " ^ string_of_float (tail clock_signal)); *)
  Unix.sleep 3;
  stop () |> ignore;
  (* print_endline ("Head of fourty_two signal after 2s: " ^ string_of_float (head clock_signal)); *)
  print_endline ("Head of fourty_two signal after 3s: " ^ string_of_float (head clock_signal));
  print_endline ("Value of chan after 3s: " ^ string_of_int (head other_signal));
  ()