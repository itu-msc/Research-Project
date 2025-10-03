open Rizzo.MainTypes
(* open Rizzo *)

let () = print_endline "Hello, World!"

let print_OA (u: string oa) =
  print_endline ("OA string: " ^ (u ()))

let () = print_OA (delay "Test OA");

(* Example usage of the 'oe' type *)

(* let test : int Rizzo.MainTypes.signal = 1 :: Never *)