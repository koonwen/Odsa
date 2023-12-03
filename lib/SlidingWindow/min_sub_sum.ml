(* Given an array of positive integers nums and a positive integer
   target, return the minimal length of a subarray whose sum is
   greater than or equal to target. If there is no such subarray,
   return 0 instead. *)

let _min_sub_sum target nums =
  let curmin = ref max_int in
  let l, r = ref 0, ref 0 in
  let sum = ref nums.(0) in
  while !r < Array.length nums do
    if !sum = target
    then (
      curmin := min !curmin (!r - !l + 1);
      incr l;
      sum := !sum - nums.(!l - 1))
    else if !sum < target
    then (
      incr r;
      if !r < Array.length nums then sum := !sum + nums.(!r) else ())
    else (
      incr l;
      sum := !sum - nums.(!l - 1))
  done;
  if !curmin = max_int then 0 else !curmin
;;

let min_sub_sum target nums =
  let curmin = ref max_int in
  let l = ref 0 in
  let sum = ref 0 in
  for r = 0 to Array.length nums - 1 do
    sum := nums.(r) + !sum;
    while !sum >= target do
      if !sum = target then curmin := min !curmin (r - !l + 1);
      sum := !sum - nums.(!l);
      incr l
    done;
  done;
  if !curmin = max_int then 0 else !curmin
;;

let%expect_test "1" =
  let res = min_sub_sum 7 [| 2; 3; 1; 2; 4; 3 |] in
  print_int res;
  [%expect {|2|}]
;;

let%expect_test "2" =
  let res = min_sub_sum 7 [| 1; 4; 4 |] in
  print_int res;
  [%expect {|0|}]
;;

let%expect_test "3" =
  let res = min_sub_sum 7 [| 1; 1; 1; 1; 1; 1; 1; 1 |] in
  print_int res;
  [%expect {|7|}]
;;

let%expect_test "4" =
  let res = min_sub_sum 1 [| 2; 2; 2; 2; 2 |] in
  print_int res;
  [%expect {|0|}]
;;

(* Lesson: To prevent overstepping boundary of the array, use a for
   loop instead and a while loop within in *)
