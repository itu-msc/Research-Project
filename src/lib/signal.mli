open Types

(** Creates a fresh signal given a head and a tail in the form of a delayed computation. Similar to cons ([::]) on lists.
    
    Example: {[ let my_int_signal = 0 @: (filter_mapL int_of_string_opt console) ]}
*)
val (@:) : 'a -> 'a signal later -> 'a signal


(** Creates a constant signal.
    This signal will never tick, invoking [head] will always return the same value.
*)
val const : 'a -> 'a signal


(** Maps a function over a signal, producing a new signal.
    
    Example:
    {[
        let doubled_signal = map (fun x -> x * 2) my_int_signal
    ]}
*)
val map : ('a -> 'b) -> 'a signal -> 'b signal


(** Maps a function over a delayed signal, producing a new delayed signal. *)
val mapL : ('a -> 'b) -> 'a signal later -> 'b signal later


(** Creates a delayed signal from any delayed computation. 
    The primary use is to instantiate signals from external channels ['a channel] or internal channels [trig]

    Example: {[
        let every_second, every_second_stop = Rizzo.Channel.clock_signal 1.0 in
        (* counts the number of seconds since application startup *)
        let count_second = scan (fun n _ -> n + 1) 0 every_second in ...
    ]}
*)
val mkSig : 'a later -> 'a signal later


(** Shorthand for [mkSig (wait c)] *)
val mkSig_of_channel : 'a channel -> 'a signal later


(** Returns the value for the current time tick. 
    Every [head] call to a signal [s] will produce the same value until the tail of [s] ticks.
*)
val head : 'a signal -> 'a


(** Creates a signal that acts as the first until the second argument (a delayed computation)
    produces a signal, at which point the signal now acts as that.
    
    Example: {[
        let every_second, every_second_stop = Rizzo.Channel.clock_signal 1.0 in
        (* counts the number of seconds since application startup *)
        let count_second = scan (fun n _ -> n + 1) 0 every_second in
        let numbers_from_console : int signal later = filter_mapL int_of_string_opt console in
        let switched = switch count_second numbers_from_console in ...
    ]}
    Switch will act as [count_second] until the program receives a valid number through the [console].
    Hereafter, switched will output any number that is read from the [console].
*)
val switch : 'a signal -> 'a signal later -> 'a signal


val zip         : 'a signal -> 'b signal -> ('a * 'b) signal
val switchS     : 'a signal -> ('a -> 'a signal) later -> 'a signal
val switchR     : 'a signal -> (('a -> 'a signal) signal) later -> 'a signal
val jump        : ('a -> 'a signal option) -> 'a signal -> 'a signal
val sample      : 'a signal -> 'b signal -> ('a * 'b) signal
val sampleL     : 'a signal -> 'b signal later -> ('a * 'b) signal later
val scan        : ('b -> 'a -> 'b) -> 'b -> 'a signal -> 'b signal
val scanL       : ('a -> 'b -> 'a) -> 'a -> 'b signal later -> 'a signal later
val interleave  : ('a -> 'a -> 'a) -> 'a signal -> 'a signal -> 'a signal
val filter      : ('a -> bool) -> 'a signal -> 'a signal later
val filterL     : ('a -> bool) -> 'a signal later -> 'a signal later
val filter_map  : ('a -> 'b option) -> 'a signal -> 'b signal later
val filter_mapL : ('a -> 'b option) -> 'a signal later -> 'b signal later
val triggerL    : ('a -> 'b -> 'c) -> 'a signal later -> 'b signal -> 'c signal later
val map2        : ('a -> 'b -> 'c) -> 'a signal -> 'b signal -> 'c signal

val pp_signal   : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a signal -> unit

(** [init_signal c a] = [a @: mkSig_of_channel c]*)
val init_signal : 'a channel -> 'a -> 'a signal
val clock_signal : float -> float signal * (unit -> unit)
val clock_channel : float -> float channel * (unit -> unit) (* returns channel and stopper *)


(** Outputs the signal head to stdout on each tick *)
val console_output : string signal -> unit


(** Outputs the eventual value of later signal to stdout on each tick *)
val console_outputL: string signal later -> unit


(** Outputs the eventual value of later signal to the given port at address *)
val port_send_outputL : Unix.inet_addr -> int -> string signal later -> unit


(** Quits the program on first tick *)
val set_quit : 'a signal later -> unit


(** Misleading name, just blocks the current thread. *)
val start_event_loop : unit -> unit
