open Types

val (@:)        : 'a -> 'a signal oe -> 'a signal
val const       : 'a -> 'a signal
val map         : ('a -> 'b) -> 'a signal -> 'b signal
val mapD        : ('a -> 'b) -> 'a signal oe -> 'b signal oe
val mkSig       : 'a oe -> 'a signal oe
val mkSig_of_channel : 'a channel -> 'a signal oe
val switch      : 'a signal -> 'a signal oe -> 'a signal
val head        : 'a signal -> 'a
val zip         : 'a signal -> 'b signal -> ('a * 'b) signal
val switchS     : 'a signal -> ('a -> 'a signal) oe -> 'a signal
val switchR     : 'a signal -> (('a -> 'a signal) signal) oe -> 'a signal
val jump        : ('a -> 'a signal option) -> 'a signal -> 'a signal
val sample      : 'a signal -> 'b signal -> ('a * 'b) signal
val scan        : ('b -> 'a -> 'b) -> 'b -> 'a signal -> 'b signal
val scanD       : ('a -> 'b -> 'a) -> 'a -> 'b signal oe -> 'a signal oe
val interleave  : ('a -> 'a -> 'a) -> 'a signal -> 'a signal -> 'a signal
val filter      : ('a -> bool) -> 'a signal oe -> 'a signal oe
val filter_map  : ('a -> 'b option) -> 'a signal oe -> 'b signal oe
val triggerD    : ('a -> 'b -> 'c) -> 'a signal oe -> 'b signal -> 'c signal oe
val map2        : ('a -> 'b -> 'c) -> 'a signal -> 'b signal -> 'c signal

val pp_signal   : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a signal -> unit

val init_signal : 'a channel -> 'a -> 'a signal
val clock_signal : float -> float signal * (unit -> unit)
val clock_channel : float -> float channel * (unit -> unit) (* returns channel and stopper *)

val console_output : string signal -> unit
val console_outputD: string signal oe -> unit
val port_send_outputD : Unix.inet_addr -> int -> string signal oe -> unit
val set_quit : 'a signal oe -> unit
val start_event_loop : unit -> unit
