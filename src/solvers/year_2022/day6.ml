open Solvers.Signature

module Solver : Solver = struct
  let all_different s =
    let str_len = String.length s in
    let rec aux acc i e =
      if i > e then true
      else if List.mem s.[i] acc then false
      else aux (s.[i] :: acc) (i + 1) e
    in
    aux [] 0 (str_len - 1)

  let naloga1 data =
    let rec aux i =
      if all_different (String.sub data i 4) then i + 4 else aux (i + 1)
    in
    aux 0 |> string_of_int

  let naloga2 data _part1 =
    let rec aux i =
      if all_different (String.sub data i 14) then i + 14 else aux (i + 1)
    in
    aux 0 |> string_of_int
end
