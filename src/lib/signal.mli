open MainTypes

val (@:) : 'a -> 'a signal oe -> 'a signal
val const : 'a -> 'a signal
val map : ('a -> 'b) -> 'a signal -> 'b signal
val mkSig : 'a channel -> 'a signal oe
val switch : 'a signal -> 'a signal oe -> 'a signal
val head : 'a signal -> 'a