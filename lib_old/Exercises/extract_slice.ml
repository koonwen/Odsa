let slice : 'a list -> int -> int -> 'a list = fun l start stop ->
  let rec aux i = function
    | [] -> []
    | h :: _ when i >= stop -> [h]
    | _ :: t when i < start -> aux (i + 1) t
    | h :: t -> h :: aux (i + 1) t
  in
  aux 0 l

let%expect_test "Simple" =
  let res = slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 in
  Utils.print_string_list res;
  [%expect {| ["c"; "d"; "e"; "f"; "g"] |}]
