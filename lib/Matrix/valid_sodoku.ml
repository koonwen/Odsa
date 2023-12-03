(* Determine if a 9 x 9 Sudoku board is valid. Only the filled cells
   need to be validated according to the following rules: *)

(* Each row must contain the digits 1-9 without repetition. *)

(* Each column must contain the digits 1-9 without repetition. *)

(* Each of the nine 3 x 3 sub-boxes of the grid must contain the
   digits 1-9 without repetition. *)

(* Note: *)
(* A Sudoku board (partially filled) could be valid but is not
   necessarily solvable. *)
(* Only the filled cells need to be validated according to the
   mentioned rules. *)

let valid (board : int array array) =
  let check_row board i =
    let a = Array.make 9 0 in
    let row = board.(i) in
    Array.iteri (fun i v -> if v <> -1 then a.(i) <- a.(i) + 1) row;
    Array.for_all (fun v -> v <= 1) a
  in
  let check_col board j =
    let a = Array.make 10 0 in
    Array.iter
      (fun row ->
        let v = row.(j) in
        if v <> -1 then a.(v) <- a.(v) + 1)
      board;
    Array.for_all (fun v -> v <= 1) a
  in
  let check_sub board i j =
    let a = Array.make 9 0 in
    let idx = ref 0 in
    for i' = i to i + 2 do
      for j' = j to j + 2 do
        let v = board.(i').(j') in
        if v <> -1 then a.(!idx) <- a.(!idx)
      done
    done;
    Array.for_all (fun v -> v <= 1) a
  in
  let ans = ref true in
  for i = 0 to 8 do
    let row = check_row board i in
    let col = check_col board i in
    ans := !ans && row && col;
    (* Printf.printf "%d : row %b col %b -> ans : %b\n" i row col !ans; *)
    for j = 0 to 8 do
      if (i = 0 || i = 3 || i = 6) && (j == 0 || j = 3 || j = 6)
      then (
        let sub = check_sub board i j in
        (* Printf.printf "%d %d sub %b\n" i j sub; *)
        ans := !ans && sub)
    done
  done;
  (* print_newline (); *)
  !ans
;;

let%test "1" =
  let board =
    [| [| 5; 3; -1; -1; 7; -1; -1; -1; -1 |]
     ; [| 6; -1; -1; 1; 9; 5; -1; -1; -1 |]
     ; [| -1; 9; 8; -1; -1; -1; -1; 6; -1 |]
     ; [| 8; -1; -1; -1; 6; -1; -1; -1; 3 |]
     ; [| 4; -1; -1; 8; -1; 3; -1; -1; 1 |]
     ; [| 7; -1; -1; -1; 2; -1; -1; -1; 6 |]
     ; [| -1; 6; -1; -1; -1; -1; 2; 8; -1 |]
     ; [| -1; -1; -1; 4; 1; 9; -1; -1; 5 |]
     ; [| -1; -1; -1; -1; 8; -1; -1; 7; 9 |]
    |]
  in
  valid board = true
;;

let%test "2" =
  let board =
    [| [| 8; 3; -1; -1; 7; -1; -1; -1; -1 |]
     ; [| 6; -1; -1; 1; 9; 5; -1; -1; -1 |]
     ; [| -1; 9; 8; -1; -1; -1; -1; 6; -1 |]
     ; [| 8; -1; -1; -1; 6; -1; -1; -1; 3 |]
     ; [| 4; -1; -1; 8; -1; 3; -1; -1; 1 |]
     ; [| 7; -1; -1; -1; 2; -1; -1; -1; 6 |]
     ; [| -1; 6; -1; -1; -1; -1; 2; 8; -1 |]
     ; [| -1; -1; -1; 4; 1; 9; -1; -1; 5 |]
     ; [| -1; -1; -1; -1; 8; -1; -1; 7; 9 |]
    |]
  in
  valid board = false
;;
