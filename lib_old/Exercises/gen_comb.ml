let rec extract : int -> 'a list -> 'a list list = fun i l ->
  if i = 0 then [[]] else
    (match l with
     | [] -> []
     | h :: t ->
       let with_h = List.map (fun l -> h :: l) (extract (i-1) t) in
       let without_h = extract i t in
       with_h @ without_h)

let print_string_list_list l =
  Utils.pp_print_list Format.std_formatter Utils.pp_string_list l

let%expect_test "Simple" =
  let res = extract 2 ["a"; "b"; "c"; "d"] in
  print_string_list_list res;
  [%expect {| [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]] |}]
