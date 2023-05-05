let replicate : 'a list -> int -> 'a list = fun l count ->
  if count <= 0 then failwith "Invalid count";
  let rec aux hold count = function
    | [] -> []
    | h :: t as l ->
      if count <> 0
      then h :: aux hold (count - 1) l
      else aux hold hold t
  in
  aux count count l

let%expect_test "Simple" =
  let res = replicate ["a"; "b"; "c"] 3 in
  Utils.print_string_list res;
  [%expect {|["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]|}]
