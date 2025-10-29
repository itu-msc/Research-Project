open! Rizzo.Types
open! Rizzo.Signal
open! Rizzo.Channel
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

let () = print_OA (delay "Test OA")

let () = 
  let secSig, stop = clock_signal 1.0 in
  print_endline ("Head of secSig signal: " ^ string_of_int (head secSig));
  stop ()

let getInput () =
  let chan = new_channel () in
  (chan, fun v -> step chan v)

let console_input () =
  let chan, push = getInput () in
  Thread.create (fun () ->
    try
      while true do
        let line = read_line () in
        push line
      done
    with End_of_file -> ()) () |> ignore;
  chan

let () =
  let inputChan = console_input () in
  let inputSig = init_signal inputChan "" in
  let last_input = ref "" in
  while true do
    let v = head inputSig in
    if v <> "" && v <> !last_input then begin
      last_input := v;
      print_endline ("Input signal head: " ^ v)
    end;
    Thread.delay 0.05 (* adjust as needed; smaller = more responsive, larger = less CPU *)
  done




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
