
module Hwk_07 (H6 : Hwk_06.HWK_06) 
  = struct

  let init n f =
    let rec helper i f = if i < n then f i :: helper (i+1) f else []
    in helper 0 f

  let test () = 
    assert (List.length (H6.n_random_chars 10 ()) = 10) ;
    assert ( (H6.n_random_chars 10 ()) <>
             (H6.n_random_chars 10 ()) );

    assert (List.length (H6.n_generated_values 
                           H6.S.random_char 10 ()) = 10) ;
    assert ( (H6.n_generated_values H6.S.random_char 10 ()) <>
             (H6.n_generated_values H6.S.random_char 10 ()) );

    assert ( List.length 
               (H6.n_increasing_length_lists 
                  (H6.S.random_int 0 9) 10 ()) = 10
           );
    assert (
        ( H6.n_increasing_length_lists (H6.S.random_int 0 9) 10 () <>
          H6.n_increasing_length_lists (H6.S.random_int 0 9) 10 () ) 
        && ( ( List.map 
                 List.length 
                 (H6.n_increasing_length_lists 
                    (H6.S.random_int 0 9) 10 ()) )
             = init 10 (fun x -> x)
           )
      );

    assert ( List.length 
               (H6.n_random_length_lists 
                  (H6.S.random_int 0 9) 10 0 9 ()) = 10
           );

    assert (
        ( (H6.S.take 10 
             (H6.increasing_length_lists (H6.S.random_int 0 9) )) <>
          (H6.S.take 10 
             (H6.increasing_length_lists (H6.S.random_int 0 9) )) )
        && ( ( List.map 
                 List.length 
                 (H6.S.take 10 
                    (H6.increasing_length_lists 
                     (H6.S.random_int 0 9) )) )
             = init 10 (fun x -> x)
           )
      );

    assert (H6.S.take 5 (H6.all_coordinates) = 
              [ (0,0); (1,0); (1,1); (0,1); (-1,1) ]);

    assert (H6.S.take 5 (H6.non_negative_coordinates) = 
              [ (0,0); (1,0); (1,1); (0,1); (2,0) ]);

    assert (H6.S.take 5
              (H6.S.map 
                 (fun (l1, l2) -> (List.length l1, List.length l2))
                 (H6.all_list_length_pairs H6.S.random_char)
              )
            =
              H6.S.take 5 (H6.non_negative_coordinates)
           )


end

(* Here we create two versions of your homework 6 solutions. One
   using the efficient lazy evaluation module ``Lazee`` and the
   other using the slow one, ``Lazee_slow``.
 *)
module H7_fast = Hwk_07 (Hwk_06.H6_fast) 
module H7_slow = Hwk_07 (Hwk_06.H6_slow)

(* Here we test both of them.
 *)
let () =
  H7_fast.test ();
  H7_slow.test ()


