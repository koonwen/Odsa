let print_int_list l =
  let open Fmt in
  pr "%a" (list ~sep:semi int) l
;;

let print_int_array a =
  let open Fmt in
  pr "%a" (array ~sep:semi int) a
;;
