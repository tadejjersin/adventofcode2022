open Solvers.Signature

module Solver : Solver = struct
  let rec make_string lines i =
    match lines with
    | [] -> ""
    | x :: xs -> Char.escaped x.[i] ^ make_string xs i

  let split_string s i =
    ( Char.escaped s.[i],
      (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1)) )

  let check_biggest x xs =
    let x' = int_of_string x in
    let l = String.length xs in
    let rec aux ys i e =
      if i = e then true
      else if ys.[i] |> Char.escaped |> int_of_string >= x' then false
      else aux ys (i + 1) e
    in
    aux xs 0 l

  let check_pos lines vertical_lines (x, y) =
    let left_right = split_string (List.nth lines x) y
    and up_down = split_string (List.nth vertical_lines y) x in
    check_biggest (fst left_right) (fst (snd left_right))
    || check_biggest (fst left_right) (snd (snd left_right))
    || check_biggest (fst up_down) (fst (snd up_down))
    || check_biggest (fst up_down) (snd (snd up_down))

  let naloga1 data =
    let lines = String.split_on_char '\n' data in
    let vertical_lines =
      List.init (lines |> List.hd |> String.length) (make_string lines)
    in
    let eval_string s i =
      let str_len = String.length s in
      let rec aux l j e =
        if j = e then l
        else aux (check_pos lines vertical_lines (i, j) :: l) (j + 1) e
      in
      aux [] 0 str_len
    in

    lines
    |> List.mapi (fun i x -> eval_string x i)
    |> List.flatten
    |> List.map (fun x -> if x = true then 1 else 0)
    |> List.fold_left ( + ) 0 |> string_of_int

  let naloga2 data _part1 =
    let lines = String.split_on_char '\n' data in
    let vertical_lines =
      List.init (lines |> List.hd |> String.length) (make_string lines)
    in
    let left (x, (y, _)) =
      let x' = int_of_string x in
      let str_len = String.length y in
      let rec aux n s i =
        if i = str_len then 0
        else if s.[str_len - 1 - i] |> Char.escaped |> int_of_string < n then
          1 + aux n s (i + 1)
        else 1
      in
      aux x' y 0
    in
    let right (x, (_, z)) =
      let x' = int_of_string x in
      let str_len = String.length z in
      let rec aux n s i =
        if i = str_len then 0
        else if s.[i] |> Char.escaped |> int_of_string < n then
          1 + aux n s (i + 1)
        else 1
      in
      aux x' z 0
    in
    let score (x, y) =
      let left_right = split_string (List.nth lines x) y
      and up_down = split_string (List.nth vertical_lines y) x in
      left left_right * left up_down * right left_right * right up_down
    in
    let eval_string s i =
      let str_len = String.length s in
      let rec aux l j e =
        if j = e then l else aux (score (i, j) :: l) (j + 1) e
      in
      aux [] 0 str_len
    in
    lines
    |> List.mapi (fun i x -> eval_string x i)
    |> List.flatten |> List.sort compare |> List.rev |> List.hd |> string_of_int
end
