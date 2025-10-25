open Rizzo.Signal
open Rizzo.Types

(* just see if the switch works, if it correctly changes the 'clock' at run time *)
let () = 
  let c1 : int channel = new_channel () in
  let slow_chan : int channel = new_channel () in
  let numbers = 0 @: mkSig c1 in
  let numbers_and_double = zip numbers (map (( * ) 2) numbers) in
  let switched = switch numbers (mkSig slow_chan) in
  let pp_int_signal = pp_signal Format.pp_print_int in
  let pp_int_pair_signal = pp_signal (fun out p -> Format.fprintf out "(%a, %a)" Format.pp_print_int (fst p) Format.pp_print_int (snd p)) in
  
  let rec inner cnt = 
    let open! Rizzo.Internals.Heap in
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