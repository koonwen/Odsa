let generic_list_sort (prop : 'a list -> int) (l : 'a list list): 'a list list =
  List.sort (fun a b -> Int.compare (prop a) (prop b)) l

let length_sort = generic_list_sort List.length

let freq_count hd tl =
  let len = List.length hd in
  let rec count = function
    | [] -> -1
    | h :: tl -> if List.length h = len then 1 + count tl else count tl
  in
  count tl

let freq_sort l = List.sort (fun a b -> Int.compare (freq_count a l) (freq_count b l)) l

let print_string_list_list l =
  Utils.pp_print_list Format.std_formatter Utils.pp_string_list l

let%expect_test "Simple" =
  let res = length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
             ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]] in
  print_string_list_list res;
  [%expect {| [["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["i"; "j"; "k"; "l"]] |}]

let%expect_test "Simple" =
  let res = freq_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
             ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]] in
  print_string_list_list res;
  [%expect {| [["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]] |}]
