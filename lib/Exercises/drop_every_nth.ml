let drop : 'a list -> int -> 'a list = fun l i ->
  if i <= 0 then failwith "Invalid i";
  let rec aux count = function
    | [] -> []
    | h :: t -> if count = 1 then aux i t else h :: aux (count - 1) t
  in
  aux i l

let%expect_test "Simple" =
  let res = drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 in
  Utils.print_string_list res;
  [%expect {| ["a"; "b"; "d"; "e"; "g"; "h"; "j"] |}]
