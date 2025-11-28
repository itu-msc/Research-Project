(*
  This .mli file is here to hide the implementation details of various types...
  Unfortunately, it seems we must include a .mli file for our main types too, 
  otherwise the compiler isn't able to assert the types 'Types.later' and 'Internals.MainTypes.later'
  are the same.
  But we can expose more "low-level" functions here that let you retrieve IDs of channels/signals,
  which are necessary for the internal library implementation but should not be exposed to the user by default.

  I have left the constructors of the later type exposed because they are necessary for pattern matching (which is a nice QoL) 
  and if a user decides to use the App constructor instead of the app function, nothing bad will happen.
*)

type 'a delayOnce
type 'a channel

type ('a, 'b) sync = 
  | Fst of 'a
  | Snd of 'b
  | Both of 'a * 'b

type _ later = private
  | Never
  | App : ('a -> 'b) delayOnce * 'a later -> 'b later
  | Sync : 'a later * 'b later -> ('a, 'b) sync later
  | Wait : 'a channel -> 'a later
  | Trig : 'a option signal -> 'a later
  | Tail : 'a signal -> 'a signal later
and 'a signal  
and 'a signal_data = { 
  id: int; 
  mutable head: 'a; 
  mutable tail: 'a signal later; 
  mutable updated: bool
}

val new_channel : unit -> 'a channel
val channel_id : 'a channel -> int

val signal_id : 'a signal -> int
val signal_of_data : 'a signal_data -> 'a signal
val signal_get_data : 'a signal -> 'a signal_data

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
val (|>>) : ('a -> 'b) -> 'a later -> 'b later

val pp_later : Format.formatter -> 'a later -> unit