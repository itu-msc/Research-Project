open Types

val (@:) : 'a -> 'a signal oe -> 'a signal
val const : 'a -> 'a signal
val map : ('a -> 'b) -> 'a signal -> 'b signal
val mkSig : 'a channel -> 'a signal oe
val switch : 'a signal -> 'a signal oe -> 'a signal
val head : 'a signal -> 'a
val zip : 'a signal -> 'b signal -> ('a * 'b) signal
val switchS : 'a signal -> ('a -> 'a signal) oe -> 'a signal
val switchR : 'a signal -> (('a -> 'a signal) signal) oe -> 'a signal
val jump : ('a -> 'a signal option) -> 'a signal -> 'a signal
val sample : 'a signal -> 'b signal -> ('a * 'b) signal
val scan : ('b -> 'a -> 'b) -> 'b -> 'a signal -> 'b signal
val interleave : ('a -> 'a -> 'a) -> 'a signal -> 'a signal -> 'a signal
val pp_signal : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a signal -> unit
val init_signal : 'a channel -> 'a -> 'a signal
val clock_signal : float -> float signal * (unit -> unit)
val clock_channel : float -> float channel * (unit -> unit) (* returns channel and stopper *)

val console_output : string signal -> unit
val start_event_loop : unit -> unit