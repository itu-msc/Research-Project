let step = Internals.Heap.step

let getInput () =
  let chan = Types.new_channel () in
  (chan, fun v -> Internals.Heap.step chan v)

let console_input =
  let chan, push = getInput () in
  let started = ref false in
  fun () ->
    if not !started then begin
      started := true;
      Thread.create (fun () ->
        try
          while true do
            let line = read_line () in
            (* print_endline ("console input: " ^ line); *)
            push line
          done
        with End_of_file -> ()) () |> ignore
    end;
    chan
  
(* let chan_of_func f =
  let chan = Types.new_channel () in
  let rec aux () =
    let v = f () in
    Internals.Heap.step chan v;
    aux ()
  in
  Thread.create (fun () -> aux ()) () |> ignore;
  chan *)


let started_port_inputs : (int, string Types.channel) Hashtbl.t = Hashtbl.create 4

let port_input port =
  match Hashtbl.find_opt started_port_inputs port with
  | Some chan -> chan
  | None ->
    let chan, push = getInput () in
    Hashtbl.add started_port_inputs port chan;
    let server_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let addr = Unix.(ADDR_INET (inet_addr_loopback, port)) in
    Unix.bind server_sock addr;
    Unix.listen server_sock 5;
    Thread.create (fun () ->
      try
        while true do
          let (client_sock, _) = Unix.accept server_sock in
          let in_chan = Unix.in_channel_of_descr client_sock in
          Thread.create (fun () ->
            try
              while true do
                let line = input_line in_chan in
                push line
              done
            with End_of_file ->
              close_in in_chan;
              Unix.close client_sock) () |> ignore
        done
      with exn ->
        prerr_endline ("web client input error: " ^ Printexc.to_string exn);
        Unix.close server_sock) () |> ignore;
    chan
