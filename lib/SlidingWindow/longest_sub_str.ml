(* Given a string s, find the length of the longest substring without
   repeating characters. *)

module CSet = Set.Make (Char)

(* inefficient, O(n^2) *)
let _longest str =
  let ans = ref 0 in
  for i = 0 to String.length str do
    let set = ref CSet.empty in
    let rec walk i =
      if i > String.length str
      then CSet.cardinal !set
      else (
        match CSet.find_opt str.[i] !set with
        | None ->
          set := CSet.add str.[i] !set;
          walk i + 1
        | Some _ -> CSet.cardinal !set)
    in
    let n = walk i in
    ans := max n !ans
  done;
  !ans
;;

(* You want to do use a mutable pointer to walk with the right
   pointer and keep state *)
let longest str =
  let ans = ref 0 in
  let r = ref 0 in
  let set = ref CSet.empty in
  let rec walk r =
    if !r >= String.length str
    then CSet.cardinal !set
    else (
      match CSet.find_opt str.[!r] !set with
      | None ->
        set := CSet.add str.[!r] !set;
        incr r;
        walk r
      | Some _ ->
        set := CSet.add str.[!r] !set;
        CSet.cardinal !set)
  in
  String.iter (fun s ->
    let n = walk r in
    ans := max n !ans;
    set := CSet.remove s !set) str;
  !ans
;;

let%test "1" = longest "abcabcbb" = 3
let%test "2" = longest "bbbbb" = 1
let%test "3" = longest "pwwkew" = 3
