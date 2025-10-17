(* not necessarily what we want - but we CAN make types opaque.
   Maybe we provide functions that use the type constructors of oe
*)


type 'a channel = Index of int
(* consider using a constructor *)
type 'a oa = unit -> 'a
type 'a signal = Identifier of int
type ('a, 'b) sync =
| Fst of 'a
| Snd of 'b
| Both of 'a * 'b

type _ oe = private
  | Never
  | App : ('a -> 'b) oa * 'a oe -> 'b oe   (* this is the O>*)
  | Sync : 'a oe * 'b oe -> ('a, 'b) sync oe
  | Wait : 'a channel -> 'a oe
  (* Do these go here? *)
  | Trig : 'a option signal -> 'a oe
  | Tail : 'a signal -> 'a signal oe

val new_channel : unit -> 'a channel
val adv_channel : 'a channel -> 'a

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
val (|>>) : ('a -> 'b) -> 'a oe -> 'b oe