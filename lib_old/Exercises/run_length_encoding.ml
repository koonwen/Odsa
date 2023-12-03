type 'a rle =
  | One of 'a
  | Many of (int * 'a)

let encode : 'a list -> 'a rle list = fun l ->
  let rec aux hold acc = function
    | [] -> (match hold with Some x -> x :: acc | None -> acc)
    | rle :: tl ->
      (match hold with
      | Some (One x) when x = rle -> aux (Some (Many (2, x))) acc tl
      | Some (Many (i, x)) when x = rle -> aux (Some (Many (i + 1, x))) acc tl
      | Some x -> aux (Some (One rle)) (x :: acc) tl
      | _ -> aux (Some (One rle)) acc tl)
  in
  List.rev (aux None [] l)

let pp_print_node ppf ppa a =
  match a with
  | One a -> Format.fprintf ppf "One %a" ppa a
  | Many (i, a) -> Format.fprintf ppf "Many (%d, %a)" i ppa a

let print_string_node_list : string rle list -> unit = function l ->
  let pp_string_node ppf n = pp_print_node ppf Utils.pp_print_string n in
  Utils.pp_print_list Format.std_formatter pp_string_node l

let%expect_test "Simple" =
  let res = encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] in
  print_string_node_list res;
  [%expect {| [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] |}]
