open Internals.MainTypes

val (@:) : 'a -> 'a signal oe -> 'a signal
val const : 'a -> 'a signal
val map : ('a -> 'b) -> 'a signal -> 'b signal
val mkSig : 'a channel -> 'a signal oe
val switch : 'a signal -> 'a signal oe -> 'a signal
val head : 'a signal -> 'a
val zip : 'a signal -> 'b signal -> ('a * 'b) signal
val switchS : 'a signal -> ('a -> 'a signal) oe -> 'a signal
val switchR : 'a signal -> (('a -> 'a signal) signal) oe -> 'a signal
val pp_signal : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a signal -> unit