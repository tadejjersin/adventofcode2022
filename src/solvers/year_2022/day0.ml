open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let naloga1 data =
    let lines = String.split_on_char '\n' data in
    let rec aux acc1 acc2 xs =
      match xs with
      | [] -> max acc1 acc2
      | x :: xs -> (
          match x with
          | "" -> if acc2 > acc1 then aux acc2 0 xs else aux acc1 0 xs
          | y -> aux acc1 (int_of_string y + acc2) xs )
    in
    string_of_int (aux 0 0 lines)

  let naloga2 data _part1 =
    let change_list list x = x :: list |> List.sort compare |> List.tl in
    let lines = String.split_on_char '\n' data in
    let rec aux acc1 acc2 xs =
      match xs with
      | [] -> change_list acc1 acc2
      | x :: xs -> (
          match x with
          | "" -> aux (change_list acc1 acc2) 0 xs
          | y -> aux acc1 (int_of_string y + acc2) xs )
    in
    aux [ 0; 0; 0 ] 0 lines |> List.fold_left ( + ) 0 |> string_of_int
end
