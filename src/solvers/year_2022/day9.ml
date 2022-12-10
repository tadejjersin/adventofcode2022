open Solvers.Signature

module Solver : Solver = struct
  let move_head direction (x, y) =
    match direction with
    | 'U' -> (x + 1, y)
    | 'D' -> (x - 1, y)
    | 'L' -> (x, y - 1)
    | 'R' -> (x, y + 1)
    | _ -> failwith "Invalid direction"

  let move_tail (h1, h2) (x, y) =
    if abs (h1 - x) <= 1 && abs (h2 - y) <= 1 then (x, y)
    else
      let x' =
        match h1 - x with
        | 2 -> x + 1
        | -2 -> x - 1
        | 1 | -1 -> h1
        | 0 -> x
        | _ -> failwith "How did we get here???"
      in
      let y' =
        match h2 - y with
        | 2 -> y + 1
        | -2 -> y - 1
        | 1 | -1 -> h2
        | 0 -> y
        | _ -> failwith "How did we get here???"
      in
      (x', y')

  let read_line line =
    (line.[0], String.sub line 2 (String.length line - 2) |> int_of_string)

  let line_commands line =
    let x = read_line line in
    List.init (snd x) (fun _ -> fst x)

  let rec count_dif lst acc a =
    match lst with
    | [] -> acc
    | x :: xs ->
        if List.mem x a then count_dif xs acc a
        else count_dif xs (acc + 1) (x :: a)

  let naloga1 data =
    let lines = String.split_on_char '\n' data in
    let commands = lines |> List.map line_commands |> List.flatten in
    let rec aux head_pos tail_pos acc comm =
      match comm with
      | [] -> acc
      | x :: xs ->
          let new_head = move_head x head_pos in
          let new_tail = move_tail new_head tail_pos in
          aux new_head new_tail (new_tail :: acc) xs
    in
    let tail_positions = aux (0, 0) (0, 0) [ (0, 0) ] commands in
    count_dif tail_positions 0 [] |> string_of_int

  let naloga2 data _part1 =
    let lines = String.split_on_char '\n' data in
    let commands = lines |> List.map line_commands |> List.flatten in
    let make_move positions direction =
      let _ =
        for i = 0 to 9 do
          if i = 0 then positions.(0) <- move_head direction positions.(0)
          else positions.(i) <- move_tail positions.(i - 1) positions.(i)
        done
      in
      positions
    in
    let rec aux positions acc comm =
      match comm with
      | [] -> acc
      | x :: xs ->
          let new_pos = make_move positions x in
          aux new_pos (new_pos.(9) :: acc) xs
    in
    aux
      [|
        (0, 0);
        (0, 0);
        (0, 0);
        (0, 0);
        (0, 0);
        (0, 0);
        (0, 0);
        (0, 0);
        (0, 0);
        (0, 0);
      |]
      [] commands
    |> (fun x -> count_dif x 0 [])
    |> string_of_int
end
