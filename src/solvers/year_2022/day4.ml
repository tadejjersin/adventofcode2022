open Solvers.Signature

module Solver : Solver = struct
  let list_to_tuple lst = (List.hd lst, List.tl lst |> List.hd)

  let pair line =
    String.split_on_char ',' line
    |> List.map (fun x -> List.map int_of_string (String.split_on_char '-' x))
    |> List.map list_to_tuple |> list_to_tuple

  let naloga1 data =
    let lines = String.split_on_char '\n' data in
    let fully_contains pair =
      (fst (fst pair) <= fst (snd pair) && snd (fst pair) >= snd (snd pair))
      || (fst (fst pair) >= fst (snd pair) && snd (fst pair) <= snd (snd pair))
    in
    let rec aux acc = function
      | [] -> acc
      | x :: xs ->
          if fully_contains (pair x) then aux (acc + 1) xs else aux acc xs
    in
    aux 0 lines |> string_of_int

  let naloga2 data _part1 =
    let lines = String.split_on_char '\n' data in
    let fully_contains pair =
      snd (fst pair) < fst (snd pair) || fst (fst pair) > snd (snd pair)
    in
    let rec aux acc = function
      | [] -> acc
      | x :: xs ->
          if fully_contains (pair x) then aux acc xs else aux (acc + 1) xs
    in
    aux 0 lines |> string_of_int
end
