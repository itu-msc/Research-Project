open Internals

type 'a oa = 'a MainTypes.oa
type 'a later = 'a MainTypes.later
type 'a channel = 'a MainTypes.channel
type 'a signal = 'a MainTypes.signal

type ('a, 'b) sync =   
  | Fst of 'a
  | Snd of 'b
  | Both of 'a * 'b

val new_channel : unit -> 'a channel

val delay : (unit -> 'a) -> 'a oa
val adv : 'a oa -> 'a

val never : 'a later
val app : ('a -> 'b) oa -> 'a later -> 'b later
val sync: 'a later -> 'b later -> ('a, 'b) sync later
val wait : 'a channel -> 'a later
val trig : 'a option signal -> 'a later
val tail : 'a signal -> 'a signal later
val ostar : ('a -> 'b) oa -> 'a oa -> 'b oa
val fa : ('a -> 'b) -> 'a later -> 'b later
val (|>>) : ('a -> 'b) -> 'a later -> 'b later

val pp_later : Format.formatter -> 'a later -> unit