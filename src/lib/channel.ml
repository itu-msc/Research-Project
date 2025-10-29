open Types

let step : int channel -> int -> unit = 
  fun k v ->
    Internals.Heap.step k v