open Solvers.Signature

module Solver : Solver = struct
  let rec move a (n, from, to') =
    if n = 0 then a
    else
      let hd = a.(from) |> List.hd and tl = a.(from) |> List.tl in
      let _ = a.(from) <- tl in
      let _ = a.(to') <- hd :: a.(to') in
      move a (n - 1, from, to')

  let rec move' a (n, from, to') =
    let rec aux acc1 acc2 n =
      if n = 0 then
        let _ = acc1.(to') <- List.rev acc2 @ acc1.(to') in
        acc1
      else
        let hd = acc1.(from) |> List.hd and tl = acc1.(from) |> List.tl in
        let _ = acc1.(from) <- tl in
        aux acc1 (hd :: acc2) (n - 1)
    in
    aux a [] n

  let naloga1 data =
    let lines = String.split_on_char '\n' data in
    let a = [| []; []; []; []; []; []; []; []; [] |] in
    let rec helper a s start step i e =
      if start + (step * i) > e then a
      else if s.[start + (step * i)] = ' ' then helper a s start step (i + 1) e
      else
        let _ = a.(i) <- s.[start + (step * i)] :: a.(i) in
        helper a s start step (i + 1) e
    in
    let rec aux acc n x =
      if n = -1 then acc
      else aux (helper acc (List.hd x) 1 4 0 35) (n - 1) (List.tl x)
    in
    let a' = Array.map List.rev (aux a 7 lines) in
    let movefromto x =
      let l = String.length x in
      ( int_of_string (String.sub x 5 (l - 17)),
        int_of_string (Char.escaped x.[l - 6]) - 1,
        int_of_string (Char.escaped x.[l - 1]) - 1 )
    in
    let rec aux' acc n xs =
      if n < 10 then aux' acc (n + 1) (List.tl xs)
      else
        match xs with
        | [] -> acc
        | y :: ys -> aux' (move acc (movefromto y)) n ys
    in
    aux' a' 0 lines |> Array.map List.hd |> Array.map Char.escaped
    |> Array.fold_left ( ^ ) ""

  let naloga2 data _part1 =
    let lines = String.split_on_char '\n' data in
    let a = [| []; []; []; []; []; []; []; []; [] |] in
    let rec helper a s start step i e =
      if start + (step * i) > e then a
      else if s.[start + (step * i)] = ' ' then helper a s start step (i + 1) e
      else
        let _ = a.(i) <- s.[start + (step * i)] :: a.(i) in
        helper a s start step (i + 1) e
    in
    let rec aux acc n x =
      if n = -1 then acc
      else aux (helper acc (List.hd x) 1 4 0 35) (n - 1) (List.tl x)
    in
    let a' = Array.map List.rev (aux a 7 lines) in
    let movefromto x =
      let l = String.length x in
      ( int_of_string (String.sub x 5 (l - 17)),
        int_of_string (Char.escaped x.[l - 6]) - 1,
        int_of_string (Char.escaped x.[l - 1]) - 1 )
    in
    let rec aux' acc n xs =
      if n < 10 then aux' acc (n + 1) (List.tl xs)
      else
        match xs with
        | [] -> acc
        | y :: ys -> aux' (move' acc (movefromto y)) n ys
    in
    aux' a' 0 lines |> Array.map List.hd |> Array.map Char.escaped
    |> Array.fold_left ( ^ ) ""
end
