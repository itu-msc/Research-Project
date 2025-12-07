open Internals

type 'a delayOnce = 'a MainTypes.delayOnce
type 'a later = 'a MainTypes.later
type 'a channel = 'a MainTypes.channel
type 'a signal = 'a MainTypes.signal

type ('a, 'b) sync =   
  | Fst of 'a
  | Snd of 'b
  | Both of 'a * 'b

val new_channel : unit -> 'a channel

val delay : (unit -> 'a) -> 'a delayOnce
val adv : 'a delayOnce -> 'a

val never : 'a later
val app : ('a -> 'b) delayOnce -> 'a later -> 'b later
val sync: 'a later -> 'b later -> ('a, 'b) sync later
val wait : 'a channel -> 'a later
val trig : 'a option signal -> 'a later
val tail : 'a signal -> 'a signal later
val ostar : ('a -> 'b) delayOnce -> 'a delayOnce -> 'b delayOnce
val fa : ('a -> 'b) -> 'a later -> 'b later
val (<<|) : ('a -> 'b) -> 'a later -> 'b later
val (|>>) : 'a later -> ('a -> 'b) -> 'b later

val pp_later : Format.formatter -> 'a later -> unit