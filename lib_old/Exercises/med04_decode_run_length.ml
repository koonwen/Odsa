(* Given a run-length code list generated as specified in the previous
   problem, construct its uncompressed version. *)
type ('a, 'b) node =
  | One of 'b
  | Many of ('a * 'b)

let rec decode = function
  | [] -> []
  | h :: t ->
    (match h with
     | One a -> a :: decode t
     | Many (i, a) -> if i = 0 then decode t else a :: decode (Many (i - 1, a) :: t))
;;

let pp_node ppf ppva ppvb = function
  | One b -> Format.fprintf ppf "%a" ppvb b
  | Many (a, b) -> Format.fprintf ppf "(%a, %a)" ppva a ppvb b
;;

let print_node_list =
  let open Format in
  let pp_int_string_node ppf = pp_node ppf pp_print_int pp_print_string in
  Format.(printf "%a" (pp_print_list ~pp_sep:Utils.pp_sep pp_int_string_node))
;;

let%expect_test "Simple" =
  let res =
    decode
      [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
  in
  Utils.print_string_list res;
  [%expect {|a; a; a; a; b; c; c; a; a; d; e; e; e; e|}]
;;
