open Solvers.Signature

module Solver : Solver = struct
  let char_value c =
    let c' = int_of_char c in
    if c' >= 97 then c' - 96 else c' - 38

  let chunkify size lst =
    let rec aux chunk chunks n lst =
      match (n, lst) with
      | _, [] when chunk = [] -> List.rev chunks
      | _, [] -> List.rev (List.rev chunk :: chunks)
      | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
      | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
    in
    aux [] [] size lst

  let naloga1 data =
    let lines = String.split_on_char '\n' data in
    let find_item x =
      let len = String.length x / 2 in
      let rec aux i =
        if i = len then failwith "No element found"
        else if String.contains_from x len x.[i] then x.[i]
        else aux (i + 1)
      in
      aux 0
    in
    lines
    |> List.map (fun x -> find_item x |> char_value)
    |> List.fold_left ( + ) 0 |> string_of_int

  let naloga2 data _part1 =
    let lines = String.split_on_char '\n' data in
    let groups = chunkify 3 lines in
    let find_item_in_group x =
      let f = List.hd x in
      let len = f |> String.length in
      let rec aux i =
        if i = len then failwith "No element found"
        else if
          String.contains (List.nth x 1) f.[i]
          && String.contains (List.nth x 2) f.[i]
        then f.[i]
        else aux (i + 1)
      in
      aux 0
    in
    groups
    |> List.map (fun x -> find_item_in_group x |> char_value)
    |> List.fold_left ( + ) 0 |> string_of_int
end
