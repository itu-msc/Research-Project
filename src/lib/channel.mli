open Types

val step : 'a channel -> 'a -> unit

val console_input : unit -> string channel

val port_input : int -> string channel
