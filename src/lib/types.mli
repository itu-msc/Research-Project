type 'a oa
type 'a oe
type 'a channel
type 'a signal

type ('a, 'b) sync = 
| Fst of 'a
| Snd of 'b
| Both of 'a * 'b

val new_channel : unit -> 'a channel

val delay : 'a -> 'a oa
val adv : 'a oa -> 'a

val never : 'a oe
val app : ('a -> 'b) oa -> 'a oe -> 'b oe
val sync: 'a oe -> 'b oe -> ('a, 'b) sync oe
val wait : 'a channel -> 'a oe
val trig : 'a option signal -> 'a oe
val tail : 'a signal -> 'a signal oe
val ostar : ('a -> 'b) oa -> 'a oa -> 'b oa
val fa : ('a -> 'b) -> 'a oe -> 'b oe
val (|>>) : ('a -> 'b) -> 'a oe -> 'b oe

val pp_oe : Format.formatter -> 'a oe -> unit