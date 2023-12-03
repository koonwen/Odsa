open Base

let best_time prices =
  let a = ref 0 in
  for i = 1 to Array.length prices - 1 do
    let diff = prices.(i) - prices.(i - 1) in
    if diff > 0 then a := !a + diff
  done;
  !a
;;

let%test "1" = best_time [| 7; 1; 5; 3; 6; 4 |] = 7
let%test "2" = best_time [| 1; 2; 3; 4; 5 |] = 4
let%test "3" = best_time [| 7; 6; 4; 3; 1 |] = 0
