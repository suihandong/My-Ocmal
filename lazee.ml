(* Organizing the lazy evaluation code into a module. *)

(* Here is a signature for what needs to be provided by any
   LAZEE module.
 *)
module type LAZEE = sig
  type 'a t
  val delay: (unit -> 'a) -> 'a t
  val demand: 'a t -> 'a
end


(* Fill in the contents of the Lazee module below based on the
   implementation from class and in the public class repository.
   You will need to rename the ``lazee`` type to ``t``.
 *)
module Lazee : LAZEE = struct
  type 'a t = 'a hidden ref
  and 'a hidden = Value of 'a
                  | Thunk of (unit -> 'a)

  let delay (unit_to_x: unit -> 'a) : 'a t = ref (Thunk unit_to_x)
  let force (l : 'a hidden ref) : unit = match !l with
    | Value _ -> ()
    | Thunk f -> l := Value (f ())
  let demand (l : 'a hidden ref) : 'a =
    force l;
    match !l with
    | Value v -> v
    | Thunk f -> raise (Failure "this should not happen")
end

(* Here is another module that implements the LAZEE signature above
   but it is not as efficient.  Explain why in THIS comment.

   -- write your explanation here --
   The module Lazee, which defined above can run only one time and take in all
   the values, but the module Lazee_slow, which defined below, need to be runned 
   each time to take in all values.

 *)
module Lazee_slow : LAZEE = struct
  type 'a t = unit -> 'a
  let delay (unit_to_x: unit -> 'a) : 'a t = unit_to_x
  let demand (l: 'a t) : 'a = l ()
end
