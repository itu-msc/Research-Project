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

(* TODO *)
type 'a channel = Index of int
let new_channel = 
  let next = ref 0 in
  fun () -> (let c = !next in next := !next + 1; Index c)

let adv_channel (c : 'a channel) : 'a = match c with Index _i -> Obj.magic "hii"

type _ oe =
  | Never
  | App : ('a -> 'b) oa * 'a oe -> 'b oe   (* this is the O>*)
  | Sync : 'a oe * 'b oe -> ('a, 'b) sync oe
  | Wait : 'a channel -> 'a oe
  (* Do these go here? *)
  | Trig : 'a option signal -> 'a oe
  | Tail : 'a signal -> 'a signal oe
and
 'a signal = 
  | (::) of 'a * 'a signal oe

let never = Never
let app  = fun f x -> App (f, x)
let sync = fun x y -> Sync (x, y)
let wait = fun c -> Wait c
let trig = fun s -> Trig s
let tail = fun s -> Tail s

let fa f x = app (delay f) x
let (|>) = fa

let ostar (f: ('a -> 'b) oa) (x: 'a oa) : 'b oa = delay (adv f (adv x))

let rec switch (x::xs) (d ) =
  let cont = function
    | Fst xs' -> switch xs' d
    | Snd d' -> d'
    | Both (_, d') -> d' in
  x :: (cont |> (sync xs d))

let const x = x :: Never

let rec mkSig (k: 'a channel) : 'a signal oe = 
  (fun a -> a :: mkSig k) |> wait k

(* not correct as we dont have the correct *)
(* let rec map_sig (f: 'a -> 'b) (s: 'a signal) : 'b signal =
  match s with
  | a :: ax -> f a :: fa (map_sig f) ax *)

let rec map f (x :: xs) = f x :: (map f |> xs)

(*TODO: this only type-checks because channel is id
        mkSig takes a channel, but trig produces an ('a oe)
*)
(* let rec filter p s =
  let map_maybe : ('a -> bool) -> 'a signal oe -> 'a option signal oe = 
    fun f d -> map (fun x -> if f x then Some x else None) |> d
  in
  mkSig (Trig (None :: (map_maybe p s))) *)