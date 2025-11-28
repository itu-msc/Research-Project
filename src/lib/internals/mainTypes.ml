type 'a oa = unit -> 'a

type ('a, 'b) sync =
  | Fst of 'a
  | Snd of 'b
  | Both of 'a * 'b

let delay (a: unit -> 'a) : 'a oa = a
let adv : 'a oa -> 'a = fun d -> d ()

type 'a channel = Index of int
let channel_id (Index id) = id
let new_channel : unit -> 'a channel= 
  let next = ref 0 in
  fun () -> (let c = !next in next := !next + 1; Index c)

type _ later =
  | Never
  | App : ('a -> 'b) oa * 'a later -> 'b later   (* this is the O>*)
  | Sync : 'a later * 'b later -> ('a, 'b) sync later
  | Wait : 'a channel -> 'a later
  | Trig : 'a option signal -> 'a later
  | Tail : 'a signal -> 'a signal later
and 'a signal_data = { 
  id: int; 
  mutable head: 'a;
  mutable tail: 'a signal later;
  mutable updated: bool
}
and 'a signal = SignalID of 'a signal_data

let signal_id (SignalID data) = data.id
let signal_of_data data = SignalID data
let signal_get_data (SignalID data) = data

let never = Never
let app  = fun f x -> App (f, x)
let sync = fun x y -> Sync (x, y)
let wait = fun c -> Wait c
let trig = fun s -> Trig s
let tail = fun s -> Tail s

let fa f x = app (delay (fun () -> f)) x
let (|>>) = fa

let ostar (f: ('a -> 'b) oa) (x: 'a oa) : 'b oa = delay (fun () -> (adv f (adv x)))

let rec pp_later_helper : type a. Format.formatter -> a later -> unit= 
  fun out -> function 
  | Never -> Format.fprintf out "never"
  | Wait (Index k) -> Format.fprintf out "wait %a"  Format.pp_print_int k
  | Tail (SignalID d) -> Format.fprintf out "tail %a" Format.pp_print_int d.id
  | Sync (a,b) -> Format.fprintf out "sync (%a, %a)" pp_later_helper a pp_later_helper b
  | App (_, a) -> Format.fprintf out "app _ (%a)" pp_later_helper a
  | Trig (SignalID d) -> Format.fprintf out "trig %a" Format.pp_print_int d.id

let pp_later out x = Format.fprintf out "%a" pp_later_helper x