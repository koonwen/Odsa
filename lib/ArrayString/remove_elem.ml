(* Given an integer array nums and an integer val, remove all
   occurrences of val in nums in-place. The order of the elements may
   be changed. Then return the number of elements in nums which are
   not equal to val.

   Consider the number of elements in nums which are not equal to val
   be k, to get accepted, you need to do the following things:

   Change the array nums such that the first k elements of nums
   contain the elements which are not equal to val. The remaining
   elements of nums are not important as well as the size of nums.

   Return k.
*)

(** [remove nums val'] extracts all instances of val' from the array
    and pushes the remaining elements to the front of the array, the
    number of elements not equal to val' is returned **)
let remove nums val' =
  let swap l r a =
    let tmp = a.(l) in
    a.(l) <- a.(r);
    a.(r) <- tmp
  in
  let l, r = ref 0, ref (Array.length nums - 1) in
  let cnt = ref 0 in
  while !l < !r do
    if nums.(!l) <> val'
    then (
      incr l;
      incr cnt)
    else if nums.(!r) = val'
    then decr r
    else swap !l !r nums
  done;
  !cnt
;;

(* This works because the first element is not overwritten and part of
   the first iteration *)
let remove2 nums val' =
  let i = ref 0 in
  let cnt = ref 0 in
  for j = 0 to Array.length nums - 1 do
    if nums.(j) <> val'
    then (
      nums.(!i) <- nums.(j);
      incr i;
      incr cnt)
  done;
  !cnt
;;

let check_front a k exp =
  let res = Array.sub a 0 (k - 1) |> Array.sort Int.compare in
  let exp = Array.sort Int.compare exp in
  res = exp
;;

let%test "1" =
  let nums = [| 3; 2; 2; 3 |] in
  let val' = 3 in
  let k = remove2 nums val' in
  k = 2 && check_front nums k [| 2; 2 |]
;;

let%test "2" =
  let nums = [| 0; 1; 2; 2; 3; 0; 4; 2 |] in
  let val' = 2 in
  let k = remove2 nums val' in
  k = 5 && check_front nums k [| 0; 1; 4; 0; 3 |]
;;

let%expect_test "3" =
  let nums = [| 1 |] in
  let val' = 2 in
  let _k = remove2 nums val' in
  Utils.print_int_array nums;
  [%expect {| 1 |}]
;;
