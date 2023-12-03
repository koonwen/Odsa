let rand_select : 'a list -> int -> 'a list = fun l i ->
  let rec extract n count acc = function
    | [] -> failwith "Error"
    | h :: t when n = count -> h, (List.rev acc) @ t
    | h :: t -> extract n (count + 1) (h :: acc) t
  in
  let rec aux i l acc =
    if i <> 0 then
      (let rand = Random.int (List.length l) in
       let elem, ls = extract rand 0 [] l in
       aux (i-1) ls (elem :: acc))
    else acc
  in
  aux i l []

let%expect_test "Simple" =
  Random.init 0;
  let res = rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 in
  Utils.print_string_list res;
  [%expect {| ["b"; "d"; "g"] |}]
