open Rizzo.MainTypes
open Rizzo.Heap
open Rizzo.Signal


let fourty_two = const 42
let thirteen = map (fun _ -> 13) fourty_two

let () = 
  print_string "Before GC: "; print_heap ();
  Gc.full_major ();
  step (new_channel ()) 2;
  print_string "After GC: "; print_heap ();
  Format.printf "[After GC] %a\n" (pp_signal Format.pp_print_int) thirteen;
  (head thirteen) |> ignore; (* use after free? haha *)
  print_heap ();
  ()


let () =
  reset_cursor ();
  let chan1 : int channel = new_channel () in
  let sig1 = 0 @: mkSig chan1 in
  let sig2 = map (fun x -> 
    let y = x+4 in 
    "map sig1 of int to new signal of string " ^ string_of_int @@ y*y
  ) sig1 in
  let head1 = head sig1 in
  let head2 = head sig2 in
  print_endline ("Head of sig1: " ^ string_of_int head1);
  print_endline ("Head of sig2: " ^ head2);

  step chan1 5;

  print_endline ("Head of sig1 after step: " ^ string_of_int (head sig1));
  

(* gui example from Rizzo paper *)
type colour = Black
type button = Button of string signal * colour signal * unit channel
type textfield = TextField of string signal * colour signal * string channel
type widget = 
  | Btn of button
  | TF of textfield
  | Dyn of widget signal
  | NextTo of widget * widget
  | Above of widget * widget
  | Empty
let simple_button txt = Button (const txt, const Black, new_channel ())
let on_click (Button (_,_, k)) = k
let simple_tf txt = 
  let k : string channel = new_channel () in
  TextField (txt @: mkSig k, const Black, k)

let _gui = 
  let btn = simple_button "add" in
  let updateFun () w = const (Above (w, TF (simple_tf ""))) in
  let update = (fun () -> const @@ updateFun ()) |>> wait (on_click btn) in
  let fields = switchR (const Empty) update in 
  Above (Btn btn, Dyn fields)

let _updateFun _ w = 
  let remove = simple_button "remove" in
  let field = NextTo (TF (simple_tf ""), Btn remove) in 
  const (Above (w, Dyn (field @: ((fun () -> const Empty) |>> wait @@ on_click remove))))


  (* let id = alloc 0  in
  (match find id with
  | None -> failwith ("test_trigged: allocated node not found for id " ^ string_of_int id)
  | Some n -> n.value.updated <- true);
  let fired = ticked (Index 0) (Trig (Identifier id)) in
  if fired then
    print_endline ("test_trigged: ok (id=" ^ string_of_int id ^ ")")
  else begin
    prerr_endline ("test_trigged: FAILED (expected ticked=true for id " ^ string_of_int id ^ ")");
    exit 2
  end *)