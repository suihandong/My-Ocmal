(* Below is a signature, HWK_06, that indicates the type and values
   that were implemented in Homework 06.
 *)
open Streams

module type HWK_06 = sig

  (* HWK_06 has a nested module, for streams. *)
  module S : Streams.STREAM

  (* Part 2 functions *)
  val n_random_chars : int -> char list S.generator
  val n_generated_values : 'a S.generator -> int -> 'a list S.generator
  val n_increasing_length_lists : 'a S.generator -> int ->
                                  'a list list S.generator
  val n_random_length_lists : 'a S.generator -> int -> int -> int ->
                              'a list list S.generator

  (* Part 3 functions *)
  val increasing_length_lists : 'a S.generator -> 'a list S.t
  val all_coordinates : (int * int) S.t
  val non_negative_coordinates : (int * int) S.t
  val all_list_length_pairs : 'a S.generator -> ('a list * 'a list) S.t

end

(* Define the Hwk_06 functor and its uses below.
 *)

module Hwk_06 (St : Streams.STREAM) : HWK_06 = struct
  module S = St


  (*Part 2 functions*)
  let rec n_random_chars (n: int) : char list S.generator =
    match n with
    |0 -> fun () -> []
    |n -> fun () -> (S.random_char ()) :: (n_random_chars (n-1) ())

  let rec n_generated_values (gen : 'a S.generator) (n : int) : 'a list S.generator =
    match n with
    |0 -> fun () -> []
    |n -> fun () -> (gen ()) :: (n_generated_values gen (n-1) ())

  (*helper function for n_increasing_length_lists*)
  let rec helper (gen : 'a S.generator) (n : int)
      : 'a list list S.generator =
    match (n-1) with
    |0 -> fun () -> [] :: []
    |i -> fun () -> (n_generated_values gen i ())
                      :: (helper gen i ())

  let rec n_increasing_length_lists (gen : 'a S.generator) (n : int)
      : 'a list list S.generator =
    let lst = helper gen n ()
    in
    fun () -> List.rev(lst)


  let rec n_random_length_lists (gen: 'a S.generator) (n: int) (low: int) (high: int)
      : ('a list list) S.generator =
    match n with
    |0 -> fun () -> []
    |n ->
        fun () -> (n_generated_values gen (S.random_int low high ()) ())
                    :: (n_random_length_lists gen (n-1) low high ())

  (*Part 3 functions*)
  let increasing_length_lists (gen : 'a S.generator) : 'a list S.t =
    let rec helper lst =
      S.Cons(lst, S.delay(fun () -> helper (gen () :: lst)))
    in helper []

  let rec all_coordinates  : (int * int) S.t =
    let rec helper ((x, y) : (int * int)) (lst : 'a list) : (int * int) S.t =
      S.Cons((x, y), if x = 0 && y = 0 then (S.delay(fun () -> helper (x+1, y) lst))
                   else if x >= y && x < 1-y then (S.delay(fun () -> helper (x+1, y) lst))
                   else if x >= 1-y && x > y && x != 0 then (S.delay(fun () -> helper (x, y+1) lst))
                   else if x <= y && x+y > 0 then (S.delay(fun () -> helper (x-1, y) lst))
                   else (S.delay(fun () -> helper (x, y-1) lst))
      )
    in helper (0,0) []

  let non_negative_coordinates =
    S.filter (fun (x, y) -> x >= 0 && y >= 0) all_coordinates

  let all_list_length_pairs (gen : 'a S.generator) : ('a list * 'a list) S.t =
    let f (x, y) = (S.take x (S.generated_values gen), S.take y (S.generated_values gen))
    in
    let s = non_negative_coordinates
    in
    S.map f s
end

module STREAM_slow = Streams.Stream_slow
module STREAM_lazy = Streams.Stream_lazy
module H6_fast = Hwk_06(STREAM_lazy)
module H6_slow = Hwk_06(STREAM_slow)
