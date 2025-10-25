(*
  This .mli file is here to hide the implementation details of various types...
  Unfortunately, it seems we must include a .mli file for our main types too, 
  otherwise the compiler isn't able to assert the types 'Types.oe' and 'Internals.MainTypes.oe'
  are the same.
  But we can expose more "low-level" functions here that let you retrieve IDs of channels/signals,
  which are necessary for the internal library implementation but should not be exposed to the user by default.

  I have left the constructors of the oe type exposed because they are necessary for pattern matching (which is a nice QoL) 
  and if a user decides to use the App constructor instead of the app function, nothing bad will happen.
*)

type 'a oa
type 'a channel
type 'a signal

type ('a, 'b) sync = 
  | Fst of 'a
  | Snd of 'b
  | Both of 'a * 'b

type _ oe =   
  | Never
  | App : ('a -> 'b) oa * 'a oe -> 'b oe   (* this is the O>*)
  | Sync : 'a oe * 'b oe -> ('a, 'b) sync oe
  | Wait : 'a channel -> 'a oe
  (* Do these go here? *)
  | Trig : 'a option signal -> 'a oe
  | Tail : 'a signal -> 'a signal oe

val new_channel : unit -> 'a channel
val channel_id : 'a channel -> int

val signal_id : 'a signal -> int
val signal_of_ref : int ref -> 'a signal

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