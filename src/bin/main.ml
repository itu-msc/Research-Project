open Rizzo.Types
open Rizzo.Signal
(* open Rizzo *)

let () = print_endline "Hello, World!"

let print_OA (u: string oa) =
  print_endline ("OA string: " ^ (adv u))

(* Example usage of the 'oe' type *)
let fourty_two = const 42
let thirteen = map (fun _ -> 13) fourty_two

(* let _ = Identifier *)

let () = 
  print_endline ("Head of fourty_two signal: " ^ string_of_int (head fourty_two));
  print_endline ("Head of thirteen signal: " ^ string_of_int (head thirteen));
  print_endline ("[Repeat] Head of thirteen signal: " ^ string_of_int (head thirteen))

let _ = match new_channel () with c -> c

let () = print_OA (delay "Test OA");

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
