open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let naloga1 data =
    let lines = String.split_on_char '\n' data in
    let evaluate x =
      match (x.[0], x.[2]) with
      | 'A', 'Y' | 'B', 'Z' | 'C', 'X' -> 6
      | 'A', 'X' | 'B', 'Y' | 'C', 'Z' -> 3
      | _ -> 0
    in
    let rec aux acc = function
      | [] -> acc
      | x :: xs -> aux (acc + evaluate x + (int_of_char x.[2] - 87)) xs
    in
    aux 0 lines |> string_of_int

  let naloga2 data _part1 =
    let lines = String.split_on_char '\n' data in
    let evaluate x =
      match (x.[0], x.[2]) with
      | 'A', 'Y' | 'C', 'Z' | 'B', 'X' -> 1
      | 'C', 'X' | 'B', 'Y' | 'A', 'Z' -> 2
      | _ -> 3
    in
    let rec aux acc = function
      | [] -> acc
      | x :: xs -> aux (acc + evaluate x + (3 * (int_of_char x.[2] - 88))) xs
    in
    aux 0 lines |> string_of_int
end
