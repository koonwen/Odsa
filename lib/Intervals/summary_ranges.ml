(* You are given a sorted unique integer array nums.

   A range [a,b] is the set of all integers from a to b (inclusive).

   Return the smallest sorted list of ranges that cover all the
   numbers in the array exactly. That is, each element of nums is
   covered by exactly one of the ranges, and there is no integer x
   such that x is in one of the ranges but not in nums. *)

open Base

let summary_ranges (nums : int array) : string list =
  let l = ref [] in
  let hold = ref (nums.(0), nums.(0)) in
  let i = ref 1 in
  while !i < Array.length nums do
    if nums.(!i) = snd !hold + 1
    then hold := fst !hold, nums.(!i)
    else (
      l := !hold :: !l;
      hold := nums.(!i), nums.(!i));
    Int.incr i
  done;
  l := !hold :: !l;
  List.rev !l
  |> List.map ~f:(fun (p, n) ->
    if p <> n then Printf.sprintf "%d->%d" p n else Int.to_string p)
;;

let%test "1" = Poly.( = ) (summary_ranges [| 0; 1; 2; 4; 5; 7 |]) [ "0->2"; "4->5"; "7" ]

let%test "2" =
  Poly.( = ) (summary_ranges [| 0; 2; 3; 4; 6; 8; 9 |]) [ "0"; "2->4"; "6"; "8->9" ]
;;
