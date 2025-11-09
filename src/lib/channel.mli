open Types

val step : 'a channel -> 'a -> unit

val console_input : unit -> string channel

val mk_trig_and_chan : 'a option signal -> 'a channel
val chan_of_trig : 'a oe -> 'a channel