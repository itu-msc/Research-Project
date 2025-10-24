open Rizzo.MainTypes
open Rizzo.Heap
open Rizzo.Signal
(* open Rizzo *)

let () = print_endline "Hello, World!"

let print_OA (u: string oa) =
  print_endline ("OA string: " ^ (adv u))

let () = 
  let c : int channel = new_channel () in 
  let s = 0 @: (tail (1 @: mkSig c)) in
  let s2 = map ((+) 2) s in
  let pp_int_sig = pp_signal Format.pp_print_int in
  let () = Format.printf "[Before step] s:      %a\n" pp_int_sig s in
  let () = Format.printf "[Before step] s2:     %a\n" pp_int_sig s2 in
  step c 5;
  let () = Format.printf "[After  step] s:      %a\n" pp_int_sig s in
  let () = Format.printf "[After  step] s2:     %a\n" pp_int_sig s2 in
  step c 10;
  let () = Format.printf "[After  step] s:      %a\n" pp_int_sig s in
  ()

(* Example usage of the 'oe' type *)
let fourty_two = const 42
let thirteen = map (fun _ -> 13) fourty_two

let () = 
  print_endline ("Head of fourty_two signal: " ^ string_of_int (head fourty_two));
  print_endline ("Head of thirteen signal: " ^ string_of_int (head thirteen));
  print_endline ("[Repeat] Head of thirteen signal: " ^ string_of_int (head thirteen))

let () = 
  print_string "Before GC: "; print_heap ();
  Gc.full_major ();
  step (new_channel ()) 2;
  print_string "After GC: "; print_heap ();
  Format.printf "[After GC] %a\n" (pp_signal Format.pp_print_int) thirteen;
  (head thirteen) |> ignore; (* use after free? haha *)
  print_heap ();
  ()


let _ = match new_channel () with c -> c

let () = print_OA (delay "Test OA");

(* Example usage of the 'oe' type *)

(* let () = 
  insert 42 (tail (const 1)); 
  insert 43 (tail (const 2)); 
  insert 44 (tail (const 3))

let () = print_heap ()

let () = 
  reset_cursor (); 
  insert 45 (tail (const 4)); 
  insert 46 (tail (const 5))

let () = print_heap () *)
