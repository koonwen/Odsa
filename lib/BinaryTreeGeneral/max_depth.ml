(* Given the root of a binary tree, return its maximum depth.

   A binary tree's maximum depth is the number of nodes along the
   longest path from the root node down to the farthest leaf node. *)

open Base

type 'a tree =
  | Lf of 'a
  | Node of 'a tree * 'a * 'a tree

let construct (nums : int option array) : 'a tree =
  let n = Array.length nums in
  let h = Int.ceil_log2 n in
  let min_i = (2 ** (h - 1)) - 1 in
  let rec build i =
    if i < min_i
    then (
      let l_child = build (((i + 1) * 2) - 1) in
      let r_child = build ((i + 1) * 2) in
      Node (l_child, nums.(i), r_child))
    else Lf nums.(i)
  in
  build 0
;;

let max_depth root =
  let rec aux = function
    | Node (l, _, r) -> max (aux l) (aux r) + 1
    | Lf _ -> 1
  in
  aux root
;;

let%test "1" =
  let tree = construct [| Some 3; Some 9; Some 20; None; None; Some 15; Some 7 |] in
  max_depth tree = 3
;;

let%test "2" =
  let tree = construct [| Some 1; None; Some 2 |] in
  max_depth tree = 2
;;
