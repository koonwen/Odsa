open Format

let pp_sep ppf () = Format.pp_print_string ppf "; "
let pp_print_list ppf ppa a = fprintf ppf "[%a]" (Format.pp_print_list ~pp_sep ppa) a
let pp_string_list ppf l = pp_print_list ppf pp_print_string l
let print_string_list l = printf "%a" pp_string_list l
