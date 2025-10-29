type 'a oa = unit -> 'a

type ('a, 'b) sync =
  | Fst of 'a
  | Snd of 'b
  | Both of 'a * 'b

(* a might get evaluated which we dont want, 
   it might be worth checking if we can get call by name or use Lazy
  https://stackoverflow.com/questions/71634864/does-ocaml-support-call-by-name-parameter-passing *)
let delay a : 'a oa = fun () -> a
let adv : 'a oa -> 'a = fun d -> d ()

(* TODO: Wasteful memory usage for a ref object *)
type 'a signal = Identifier of int ref
let signal_id (Identifier id_ref) = !id_ref
let signal_of_ref id_ref = Identifier id_ref

type 'a channel = Index of int
let channel_id (Index id) = id
let new_channel : unit -> 'a channel= 
  let next = ref 0 in
  fun () -> (let c = !next in next := !next + 1; Index c)

type _ oe =
  | Never
  | App : ('a -> 'b) oa * 'a oe -> 'b oe   (* this is the O>*)
  | Sync : 'a oe * 'b oe -> ('a, 'b) sync oe
  | Wait : 'a channel -> 'a oe
  | Trig : 'a option signal -> 'a oe
  | Tail : 'a signal -> 'a signal oe

let never = Never
let app  = fun f x -> App (f, x)
let sync = fun x y -> Sync (x, y)
let wait = fun c -> Wait c
let trig = fun s -> Trig s
let tail = fun s -> Tail s

let fa f x = app (delay f) x
let (|>>) = fa

let ostar (f: ('a -> 'b) oa) (x: 'a oa) : 'b oa = delay (adv f (adv x))


let rec pp_oe_helper : type a. Format.formatter -> a oe -> unit= 
  fun out -> function 
  | Never -> Format.fprintf out "never"
  | Wait (Index k) -> Format.fprintf out "wait %a"  Format.pp_print_int k
  | Tail (Identifier l) -> Format.fprintf out "tail %a" Format.pp_print_int !l
  | Sync (a,b) -> Format.fprintf out "sync (%a, %a)" pp_oe_helper a pp_oe_helper b
  | App (_, a) -> Format.fprintf out "app _ (%a)" pp_oe_helper a
  | Trig (Identifier l) -> Format.fprintf out "trig %a" Format.pp_print_int !l

let pp_oe out x = Format.fprintf out "%a" pp_oe_helper x