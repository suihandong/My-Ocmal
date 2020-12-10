open Lazee

(* The STREAM signature combines our past work on generators and streams
   into on signature.  It is defined below.
 *)
module type STREAM = sig
  type 'a lazee
  type 'a generator = unit -> 'a
  type 'a t = Cons of 'a * 'a t lazee

  (* Lazy functions *)
  val delay: (unit -> 'a) -> 'a lazee
  val demand: 'a lazee -> 'a

  (* Generator functions *)
  val random_int : int -> int -> int generator
  val random_char : char generator
  val generated_values : 'a generator -> 'a t

  (* Stream functions *)
  val head: 'a t -> 'a
  val tail: 'a t -> 'a t
  val take: int -> 'a t -> 'a list
  val filter: ('a -> bool) -> 'a t -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val zip: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  (* Add additional stream function ``val`` declarations here.
     These may be useful in later parts of the assignment.
   *)

end

(* Define Streams functor that takes a module with sig LAZEE and
   produces module with signature matching STREAM.  Give the header
   line. *)

module Stream (L: LAZEE) : STREAM = struct

 (* The three lines below illustrate how types and values from a module
    provided as input to a functor can be copied into the module produced
    by the functor.  You will need to do something similar to this in
    other files in this assignment.
  *)
  type 'a lazee = 'a L.t
  type 'a generator = unit -> 'a
  type 'a t = Cons of 'a * 'a t lazee
  let delay = L.delay
  let demand = L.demand

  (* Below, fill in the type and value implementations so that this
     functor satifises the STREAM signature as indicate in the functor
     "header" a few lines above.
   *)

  let head (s : 'a t) : 'a =
    match s with
    | Cons (h, tl) -> h

  let tail (s: 'a t) : 'a t =
    match s with
    | Cons (h, tl) -> demand tl

  let rec generated_values(gen: 'a generator) : 'a t =
    Cons(gen(), delay (fun () -> generated_values gen))

  let rec take (n: int) (s: 'a t) : 'a list =
    if n = 0 then []
    else match s with
         | Cons (h, tl) -> h :: take (n-1) (demand tl)

  let rec filter (f: 'a -> bool) (s: 'a t) : 'a t =
    match s with
    | Cons (h, tl) ->
      let r = delay(fun () -> filter f (demand tl))
      in
      if f h
      then Cons (h, r)
      else demand r

  let rec map (f: 'a -> 'b) (s: 'a t) : 'b t =
    match s with
    | Cons (h, tl) -> Cons (f h, delay(fun () -> map f (demand tl)))

  let rec zip (f: 'a -> 'b -> 'c) (s1: 'a t) (s2: 'b t) : 'c t =
    match s1, s2 with
    | Cons (h1, t1), Cons (h2, t2) ->
       Cons (f h1 h2, delay(fun () -> zip f (demand t1) (demand t2 )))

  let random_int (l : int) (h : int) : int generator =
    fun() -> (Random.int ((h+1) - l)) + l

  let random_char : char generator =
    fun () -> Char.chr (Random.int (127-32) + 32)

end

(* Add module declarations for ``Stream_lazy`` and ``Stream_slow``.
   These will have the form:

module Stream_lazy = ... use Stream functor here ...
module Stream_slow = ... use Stream functor here ...

 *)

module Lz_slow = Lazee_slow
module Lz_fast = Lazee
module Stream_lazy = Stream(Lz_fast)
module Stream_slow = Stream(Lz_slow)
