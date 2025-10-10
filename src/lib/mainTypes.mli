(* not necessarily what we want - but we CAN make types opaque.
   Maybe we provide functions that use the type constructors of oe
*)
type 'a channel
type _ oa
type _ oe
type 'a signal = (::) of 'a * 'a signal oe
type ('a, 'b) sync =
| Fst of 'a
| Snd of 'b
| Both of 'a * 'b

val new_channel : unit -> 'a channel

val delay : 'a -> 'a oa
val adv   : 'a oa -> 'a

val never : 'a oe
(* that is the (>) from the paper *)
val app : ('a -> 'b) oa -> 'a oe -> 'b oe
val sync: 'a oe -> 'b oe -> ('a, 'b) sync oe
val wait : 'a channel -> 'a oe
val trig : 'a option signal -> 'a oe
val tail : 'a signal -> 'a signal oe

(* this is the ( * ) from the paper *)
val ostar : ('a -> 'b) oa -> 'a oa -> 'b oa
(* this is the triangle from the paper*)
val fa : ('a -> 'b) -> 'a oe -> 'b oe
val (|>) : ('a -> 'b) -> 'a oe -> 'b oe

val const : 'a -> 'a signal
val map : ('a -> 'b) -> 'a signal -> 'b signal
val mkSig : 'a channel -> 'a signal oe
val switch : 'a signal -> 'a signal oe -> 'a signal