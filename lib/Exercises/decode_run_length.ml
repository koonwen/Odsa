type 'a rle =
  | One of 'a
  | Many of (int * 'a)

let rec decode : 'a rle list -> string list = function
  | [] -> []
  | rle :: tl ->
    (match rle with
     | One x -> x :: decode tl
     | Many (i, x) ->
       if i = 1 then x :: decode tl
       else x :: decode (Many (i-1, x) :: tl)
    )

let %expect_test "Simple" =
  let res = decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] in
  Utils.print_string_list res;
  [%expect {|["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]|}]
