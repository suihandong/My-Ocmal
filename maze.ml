let rec not_elem set x = not (List.mem x set)

let last p =
  p = (5, 1) || p = (3, 5)

type point = int * int

let maze_moves (p : point) : point list =
  match p with
  |(1,1) -> [(2,1)]
  |(1,2) -> [(2,2); (1,3)]
  |(1,3) -> [(1,2); (1,4); (2,3)]
  |(1,4) -> [(1,3); (1,5)]
  |(1,5) -> [(1,4); (2,5)]
  |(2,1) -> [(3,1); (1,1)]
  |(2,2) -> [(3,2); (1,2)]
  |(2,3) -> [(1,3)]
  |(2,4) -> [(2,5); (3,4)]
  |(2,5) -> [(1,5); (2,4)]
  |(3,1) -> [(2,1); (3,2)]
  |(3,2) -> [(3,1); (2,2); (4,2); (3,3)]
  |(3,3) -> [(3,2); (3,4); (4,3)]
  |(3,4) -> [(2,4); (3,3);(4,4)]
  |(3,5) -> []
  |(4,1) -> [(4,2)]
  |(4,2) -> [(3,2); (4,1)]
  |(4,3) -> [(3,3); (5,3)]
  |(4,4) -> [(3,4); (4,5)]
  |(4,5) -> [(4,4); (3,5);(5,5)]
  |(5,1) -> []
  |(5,2) -> [(5,3); (5,1)]
  |(5,3) -> [(4,3); (5,2); (5,4)]
  |(5,4) -> [(5,3)]
  |(5,5) -> [(4,5)]
  |(_,_) -> failwith "ERROE"

exception KeepLooking

let maze (u : unit) : (int * int) list option =
  let rec move point path =
    if last point then Some path
    else
      (match List.filter(not_elem path) (maze_moves point) with
        | [] -> raise KeepLooking
        | [x] -> move x (path @ [x])
        | [x; y] ->
          (try move x (path @ [x]) with
            | KeepLooking -> move y (path @ [y])
          )
        | [x; y; z] ->
          (try move x (path @ [x]) with
            | KeepLooking -> try move y (path @ [y]) with
                              | KeepLooking -> move z (path @ [z])
          )
        | [x; y; z; w] ->
          (try move x (path @ [x]) with
            | KeepLooking ->
                try move y (path @ [y]) with
                  | KeepLooking ->
                      try move z (path @ [z]) with
                        | KeepLooking -> move w (path @ [w])
           )
        | _ -> failwith "ERROE"
      )
  in
  try move (2, 3) [(2, 3)] with
    | KeepLooking -> None
