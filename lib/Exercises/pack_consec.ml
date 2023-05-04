let pack : 'a list -> 'a list list = fun l ->
  let rec pack_aux hold acc = function
    | [] -> hold :: acc
    | h :: t ->
      if List.length hold = 0 || h = List.hd hold
      then pack_aux (h :: hold) acc t
      else pack_aux [h] (hold :: acc) t
  in
  List.rev (pack_aux [] [] l)
;;

let print_list_string_list (l : string list list) : unit =
  Utils.(pp_print_list Format.std_formatter pp_string_list l)
;;

let%expect_test "Simple" =
  let res =
    pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ]
  in
  print_list_string_list res;
  [%expect {| [[a; a; a; a]; [b]; [c; c]; [a; a]; [d; d]; [e; e; e; e]] |}]
;;
