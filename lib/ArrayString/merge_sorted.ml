(* You are given two integer arrays nums1 and nums2, sorted in
   non-decreasing order, and two integers m and n, representing the
   number of elements in nums1 and nums2 respectively.

   Merge nums1 and nums2 into a single array sorted in non-decreasing
   order.

   The final sorted array should not be returned by the function, but
   instead be stored inside the array nums1. To accommodate this,
   nums1 has a length of m + n, where the first m elements denote the
   elements that should be merged, and the last n elements are set to
   0 and should be ignored. nums2 has a length of n. *)

(** [merge nums1 m nums2 n] performs a merge of `nums2` into `nums1`
    whereby size of array `nums1` is m + n *)
let merge nums1 m nums2 n =
  let p1, p2 = ref (m - 1), ref (n - 1) in
  let i = ref (m + n - 1) in
  while !p2 >= 0 do
    if !p1 >= 0 && nums1.(!p1) > nums2.(!p2)
    then (
      nums1.(!i) <- nums1.(!p1);
      decr p1)
    else (
      nums1.(!i) <- nums2.(!p2);
      decr p2);
    decr i
  done
;;

let%test "1" =
  let nums1 = [| 1; 2; 3; 0; 0; 0 |] in
  let m = 3 in
  let nums2 = [| 2; 5; 6 |] in
  let n = 3 in
  merge nums1 m nums2 n;
  nums1 = [| 1; 2; 2; 3; 5; 6 |]
;;

let%test "2" =
  let nums1 = [| 1 |] in
  let m = 1 in
  let nums2 = [||] in
  let n = 0 in
  merge nums1 m nums2 n;
  nums1 = [| 1 |]
;;

let%test "3" =
  let nums1 = [| 0 |] in
  let m = 0 in
  let nums2 = [| 1 |] in
  let n = 1 in
  merge nums1 m nums2 n;
  nums1 = [| 1 |]
;;

(** The idea here is that instead of trying to iterate forward and
    have to bubble up, we take advantage of the fact that the back is
    either full or empty and bubble down *)
