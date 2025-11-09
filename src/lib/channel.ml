open Types
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
            push line
          done
        with End_of_file -> ()) () |> ignore
    end;
    chan

let internal_trig_channel_thread = ref None
let internal_trig_channels : (Internals.Heap.data oe * Internals.Heap.data channel) list ref = ref []
let ensure_internal_trig_channel_thread_started () = 
  match !internal_trig_channel_thread with
  | Some _ -> ()
  | None -> internal_trig_channel_thread :=
      Option.some @@ 
      Thread.create (fun () -> 
        let open Internals.Heap in
        let handle (t,c) = 
          (* it is okay to do a Obj.magic here, 
             since neither ticked nor advance for 'trig' use the channel or value. 
          *)
          let dummy_chan : data channel = Obj.magic () in
          let dummy_val  : data         = Obj.magic () in
          if ticked dummy_chan t
          then let t' = advance dummy_chan t dummy_val in step c t'
          else ()
        in
        let rec loop () = 
          List.iter handle !internal_trig_channels; Thread.delay 0.05; loop () 
        in
        loop ()
      ) ()

let mk_trig_and_chan (s:'a option signal) : 'a channel = 
  ensure_internal_trig_channel_thread_started ();
  let intern_channel = Types.new_channel () in
  internal_trig_channels := (Obj.magic (trig s), Obj.magic intern_channel) :: !internal_trig_channels;
  intern_channel

let chan_of_trig = function
  | Internals.MainTypes.Trig s -> mk_trig_and_chan s
  | x -> failwith (Format.asprintf "Passed a non-trig value '%a' - very much illegal" pp_oe x) 