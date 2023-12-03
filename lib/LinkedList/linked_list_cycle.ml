(* Given head, the head of a linked list, determine if the linked list
   has a cycle in it.

   There is a cycle in a linked list if there is some node in the list
   that can be reached again by continuously following the next
   pointer. Internally, pos is used to denote the index of the node
   that tail's next pointer is connected to. Note that pos is not
   passed as a parameter.

   Return true if there is a cycle in the linked list. Otherwise,
   return false. *)

type 'a pointer =
  | Ptr of 'a ref
  | Null

type 'a my_list =
  { value : 'a
  ; mutable next : 'a my_list pointer
  }

let ( !! ) ({ next; _ } : 'a my_list) =
  match next with
  | Ptr v -> Some !v
  | Null -> None
;;

let is_cyclic (l : 'a my_list) =
  let rec aux l1 l2 =
    match l1, l2 with
    | None, None -> false
    | Some _, None -> false
    | Some vs1, Some vs2 ->
      if vs1 == vs2 then true else aux !!vs1 (Option.bind !!vs2 ( !! ))
    | None, Some _ -> failwith "Impossible"
  in
  aux !!l (Option.bind !!l ( !! ))
;;

let%test "1" =
  let l = { value = 0; next = Null } in
  is_cyclic l = false
;;

let%test "2" =
  let l = ref { value = 0; next = Null } in
  !l.next <- Ptr l;
  is_cyclic !l
;;
