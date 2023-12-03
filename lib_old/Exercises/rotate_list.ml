let rotate : 'a list -> int -> 'a list = fun l i ->
  let rec partition i acc = function
    | [] -> List.rev acc, []
    | h :: t when i = 1 -> List.rev (h :: acc), t
    | h :: t -> partition (i-1) (h :: acc) t
  in
  let len = List.length l in
  let rem = i mod len in
  let back, front = partition rem [] l in
  front @ back

let%expect_test "Simple" =
  let res = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 in
  Utils.print_string_list res;
  [%expect {| ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"] |}]
