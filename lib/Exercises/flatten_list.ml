(* Flattening a list *)

type 'a node =
  | One of 'a
  | Many of 'a node list

let pp_sep ppf () = Format.pp_print_string ppf "; "
let rec pp_node
    (pp_v : Format.formatter -> 'a -> unit)
    (ppf : Format.formatter)
    (node : 'a node) : unit =
  let open Format in
  match node with
  | One v -> printf "@[ "; pp_v ppf v; printf " @]"
  | Many v_l ->
    printf "@[[";
    List.iter (fun v -> pp_node pp_v ppf v) v_l;
    printf "]@]"

let print_string_node node =
  Format.(printf "%a" (pp_node pp_print_string) node)
let print_string_list l =
  Format.(printf "%a" (pp_print_list ~pp_sep pp_print_string) l)

let rec node_flatten : 'a node -> 'a list = function
  | One v -> [v]
  | Many v_l -> List.fold_left (fun acc v -> acc @ node_flatten v) [] v_l

let rec flatten : 'a node list -> 'a list = function
  | [] -> []
  | h :: t -> node_flatten h @ flatten t

let%expect_test "Simple" =
  let node_l = [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] in
  let res = flatten node_l in
  print_string_list res;
  [%expect {| a; b; c; d; e |}]


