open Solvers.Signature

module Solver : Solver = struct
  let get_file_sizes data =
    let lines = String.split_on_char '\n' data in
    let rec aux (dirnames : string list) (dirsizes : int list)
        (current_dirs : string list) i = function
      | [] -> dirsizes
      | x :: xs ->
          if String.sub x 0 4 = "$ cd" then
            match String.sub x 5 (String.length x - 5) with
            | "/" -> aux dirnames dirsizes [ "/" ] (i + 1) xs
            | ".." -> aux dirnames dirsizes (List.tl current_dirs) (i + 1) xs
            | y ->
                aux
                  ((y ^ string_of_int i) :: dirnames)
                  (0 :: dirsizes)
                  ((y ^ string_of_int i) :: current_dirs)
                  (i + 1) xs
          else if x.[0] = '$' || x.[0] = 'd' then
            aux dirnames dirsizes current_dirs (i + 1) xs
          else
            let file_size =
              String.sub x 0 (String.index x ' ') |> int_of_string
            in
            aux dirnames
              ( dirsizes
              |> List.mapi (fun i y ->
                     if List.mem (List.nth dirnames i) current_dirs then
                       y + file_size
                     else y) )
              current_dirs (i + 1) xs
    in
    aux [ "/" ] [ 0 ] [ "/" ] 0 lines

  let naloga1 data =
    get_file_sizes data
    |> List.filter (fun x -> x < 100000)
    |> List.fold_left ( + ) 0 |> string_of_int

  let naloga2 data _part1 =
    let f_sizes = get_file_sizes data in
    let unused_space = 70000000 - (f_sizes |> List.rev |> List.hd) in
    let need_to_delete = 30000000 - unused_space in
    f_sizes
    |> List.filter (fun x -> x > need_to_delete)
    |> List.sort compare |> List.hd |> string_of_int
end
