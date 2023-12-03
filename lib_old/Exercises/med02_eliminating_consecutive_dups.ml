(* Eliminating consequtive elements in a list *)

let rec compress : 'a list -> 'a list =
 fun l ->
  match l with
  | [] -> []
  | [ x ] -> [ x ]
  | h1 :: h2 :: t -> if h1 = h2 then compress (h1 :: t) else h1 :: compress (h2 :: t)
;;

let%expect_test "Simple" =
  let x =
    compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  in
  Utils.print_string_list x;
  [%expect {| ["a"; "b"; "c"; "a"; "d"; "e"] |}]
;;
