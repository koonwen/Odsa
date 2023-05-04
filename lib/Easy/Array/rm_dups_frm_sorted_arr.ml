open Base

let ( != ) a b = not @@ phys_equal a b
let pp_int_list = Fmt.(array ~sep:semi int)
let arr_pr arr = Fmt.(pf stdout "%a" pp_int_list arr)

let rm_dups_frm_sorted_arr (int_arr : int Array.t) =
  let pos = ref 0 in
  for i = 1 to Array.length int_arr - 1 do
    if int_arr.(i) != int_arr.(!pos)
    then (
      pos := !pos + 1;
      int_arr.(!pos) <- int_arr.(i))
  done
;;

let test_rm_dups_frm_sorted_arr arr =
  let max = Array.max_elt ~compare arr |> Option.value_exn in
  let min = Array.min_elt ~compare arr |> Option.value_exn in
  rm_dups_frm_sorted_arr arr;
  let curr = ref min in
  let state = ref true in
  for i = 0 to max - min do
    if arr.(i) != !curr then state := false else curr := !curr + 1
  done;
  !state
;;

let%test "1" = test_rm_dups_frm_sorted_arr [||]
let%test "2" = test_rm_dups_frm_sorted_arr [| 1; 1; 1; 2; 2; 2; 3; 4; 5; 5; 5; 5 |]
let%test "3" =
  test_rm_dups_frm_sorted_arr [| 1; 2; 3; 4; 5; 5; 5; 5; 5; 5; 6; 7; 8; 9; 10 |]
;;


