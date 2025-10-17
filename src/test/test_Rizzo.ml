open Rizzo.MainTypes
open Rizzo.Heap
open Rizzo.Signal

let () =
  reset_cursor ();
  let sig1 = 0 @: mkSig (new_channel ()) in
  let sig2 = map (fun x -> 
    let y = x+4 in 
    "this was another type " ^ string_of_int @@ y*y
  ) sig1 in
  let head1 = head sig1 in
  let head2 = head sig2 in
  print_endline ("Head of sig1: " ^ string_of_int head1);
  print_endline ("Head of sig2: " ^ head2)
  
  

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