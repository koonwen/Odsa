(* Given two strings ransomNote and magazine, return true if
   ransomNote can be constructed by using the letters from magazine
   and false otherwise.

   Each letter in magazine can only be used once in ransomNote. *)

module CMap = Map.Make (Char)

let can_construct ransom_note magazine : bool =
  let map =
    String.fold_left
      (fun m c ->
        CMap.update
          c
          (function
           | None -> Some 1
           | Some v -> Some (v + 1))
          m)
      CMap.empty
      magazine
  in
  let _, ans =
    String.fold_left
      (fun (m, b) c ->
        match CMap.find_opt c m with
        | None -> m, false
        | Some v ->
          let new_map = if v - 1 = 0 then CMap.remove c m else CMap.add c (v - 1) m in
          new_map, true && b)
      (map, true)
      ransom_note
  in
  ans
;;

let%test "1" = can_construct "a" "b" = false
let%test "2" = can_construct "aa" "ab" = false
let%test "3" = can_construct "aa" "aab" = true
