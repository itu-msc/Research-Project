type 'a oa = unit -> 'a

type ('a, 'b) sync =
  | Fst of 'a
  | Snd of 'b
  | Both of 'a * 'b

let delay a : 'a oa = fun () -> a

(* TODO *)
type 'a channel = 'a 

type _ oe =
| Never
| App : ('a -> 'b) oa * 'a oe -> 'b oe   (* this is the O>*)
| Sync : 'a oe * 'b oe -> ('a, 'b) sync oe
| Wait : 'a channel -> 'a oe

(* TODO *)
type 'a signal = 
  | (::) of 'a * 'a signal oe

let fa (f: 'a -> 'b) (x: 'a oe) : 'b oe =
  App (delay f, x)

let ostar (f: ('a -> 'b) oa) (x: 'a oa) : 'b oa =
  fun () -> f () (x ()) (* TODO *)

