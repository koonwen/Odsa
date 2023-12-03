[@@@warning "-32"]
open Format

let pp_sep ppf () = Format.pp_print_string ppf "; "
let pp_print_list ppf ppa a = fprintf ppf "[%a]" (Format.pp_print_list ~pp_sep ppa) a
let pp_string_list ppf l = pp_print_list ppf pp_print_string l
let print_string_list l = printf "%a" pp_string_list l

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
