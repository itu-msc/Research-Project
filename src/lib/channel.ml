let step = Internals.Heap.step

let getInput () =
  let chan = Types.new_channel () in
  (chan, fun v -> Internals.Heap.step chan v)

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