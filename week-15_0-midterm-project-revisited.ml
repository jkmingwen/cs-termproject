(* week-15_0-midterm-project-revisited.ml *)
(* Introduction to Computer Science (YSC1212), Sem1, 2017-2018 *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Version of Wed 22 Nov 2017 *)
(* was: *)
(* Version of Tue 21 Nov 2017 *)
(* was: *)
(* Version of Sat 19 Nov 2017 *)

(* ********** *)

(*
   name: Koh Ming Wen Jaime
   student ID number: A0138868J
   e-mail address: jkmingwen@u.yale-nus.edu.sg
*)

(*
   name: Khwa Zhong Xuan
   student ID number: A0160801U
   e-mail address: zhongxuan@u.yale-nus.edu.sg
*)

(*
   name:
   student ID number:
   e-mail address:
*)

(*
   name:
   student ID number:
   e-mail address:
*)

(* ********** *)

module type ZEROTH_MINI_PROJECT =
sig
  exception Not_implemented_yet of string

  type 'a binary_tree' =
    | Leaf'
    | Node' of 'a binary_tree' * 'a * 'a binary_tree'
  val fold_right_binary_tree' : 'a -> ('a * 'b * 'a -> 'a) -> 'b binary_tree' -> 'a
  val super_fold_right_binary_tree' : 'a ->
                                      ('b binary_tree' * 'a * 'b * 'b binary_tree' * 'a -> 'a) ->
                                      'b binary_tree' ->
                                      'a

  val test_number_of_leaves'_int : (int binary_tree' -> int) -> bool
  val number_of_leaves' : 'a binary_tree' -> int

  val test_number_of_nodes'_int : (int binary_tree' -> int) -> bool
  val number_of_nodes' : 'a binary_tree' -> int

  val test_left_balanced'_int : (int binary_tree' -> bool) -> bool
  val left_balanced' : int binary_tree' -> bool

  type 'a left_binary_tree' =
    | Left_Leaf'
    | Left_Node' of 'a left_binary_tree' * 'a
  val fold_right_left_binary_tree' : 'a -> ('a * 'b -> 'a) -> 'b left_binary_tree' -> 'a
  val superfold_right_left_binary_tree' : 'a ->
                                          ('b left_binary_tree' * 'a * 'b -> 'a) ->
                                          'b left_binary_tree' ->
                                          'a

  val embed_left_binary_tree'_into_binary_tree' : 'a left_binary_tree' -> 'a binary_tree'
  val embed_left_binary_tree'_into_binary_tree'_alt : 'a left_binary_tree' -> 'a binary_tree'

  val project_binary_tree'_into_left_binary_tree' : 'a binary_tree' -> 'a left_binary_tree' option
  val project_binary_tree'_into_left_binary_tree'_alt : 'a binary_tree' -> 'a left_binary_tree' option

  val left_index : 'a left_binary_tree' -> int -> 'a option
  val left_index_alt : 'a left_binary_tree' -> int -> 'a option

  val test_left_stitch_int : (int left_binary_tree' -> int left_binary_tree' -> int left_binary_tree') -> bool
  val left_stitch : int left_binary_tree' -> int left_binary_tree' -> int left_binary_tree'
  val left_stitch_alt : int left_binary_tree' -> int left_binary_tree' -> int left_binary_tree'

  val test_left_rotate_int : (int binary_tree' -> int left_binary_tree') -> bool
  val left_rotate : int binary_tree' -> int left_binary_tree'
  val left_rotate_alt : int binary_tree' -> int left_binary_tree'
end;;

module Zeroth_mini_project : ZEROTH_MINI_PROJECT =
struct

  exception Not_implemented_yet of string;;

  (* ********** *)
  
  (* Part 2 *)

  type 'a binary_tree' =
    | Leaf'
    | Node' of 'a binary_tree' * 'a * 'a binary_tree';;
  
  let fold_right_binary_tree' leaf'_case node'_case t_init =
    let rec visit t =
      match t with
       | Leaf' ->
          leaf'_case
       | Node' (t1, v, t2) ->
          let ih1 = visit t1
          and ih2 = visit t2
          in node'_case (ih1, v, ih2)
    in visit t_init;;
  
  let super_fold_right_binary_tree' leaf'_case node'_case t_init =
    let rec visit t =
      match t with
       | Leaf' ->
          leaf'_case
       | Node' (t1, v, t2) ->
          let ih1 = visit t1
          and ih2 = visit t2
          in node'_case (t1, ih1, v, t2, ih2)
    in visit t_init;;

  (* ********** *)

  (*
     Question 2.1:
   
     Implement an alternative OCaml function that counts the number of
     leaves of a given binary tree, using a fold-right function.
  *)
  
  let test_number_of_leaves'_int candidate =
    (* test_number_of_leaves'_int : (int binary_tree' -> int) -> bool *)
    (candidate Leaf' = 1)
    &&
      (* Jaime's tests *)
      (candidate (Node' (Node' (Node' (Leaf',
                                       100,
                                       Leaf'),
                                10,
                                Leaf'),
                         1,
                         Leaf'))
       = 4)
    &&
      (candidate (Node' (Leaf',
                         10,
                         Node' (Leaf',
                                20,
                                Node' (Leaf',
                                       30,
                                       Node' (Leaf',
                                              40,
                                              Leaf')))))
       = 5)
    &&
      (candidate (Node' (Node' (Node' (Leaf',
                                       4,
                                       Leaf'),
                                2,
                                Node' (Leaf',
                                       5,
                                       Leaf')),
                         1,
                         Node' (Leaf',
                                3,
                                Node' (Node' (Leaf',
                                              7,
                                              Leaf'),
                                       6,
                                       Leaf'))))
       = 8);;
  
  let number_of_leaves' t =
    (* number_of_leaves' : 'a binary_tree' -> int *)
    fold_right_binary_tree' (1)
                            (fun (ih1, v, ih2) -> ih1 + ih2)
                            t;;
 
  let () = assert (test_number_of_leaves'_int number_of_leaves');;
  
  (* ********** *)

  (*
     Question 2.2:
  
     Implement an alternative OCaml function that counts the number of
     nodes of a given binary tree, using a fold-right function.
  *)

  let test_number_of_nodes'_int candidate =
    (* test_number_of_nodes'_int : (int binary_tree' -> int) -> bool *)
    (candidate Leaf' = 0)
    &&
      (* Jaime's tests: *)
      (candidate (Node' (Node' (Node' (Leaf',
                                       100,
                                       Leaf'),
                                10,
                                Leaf'),
                         1,
                         Leaf'))
       = 3)
    &&
      (candidate (Node' (Leaf',
                         10,
                         Node' (Leaf',
                                20,
                                Node' (Leaf',
                                       30,
                                       Node' (Leaf',
                                              40,
                                              Leaf')))))
       = 4)
    &&
      (candidate (Node' (Node' (Node' (Leaf',
                                       4,
                                       Leaf'),
                                2,
                                Node' (Leaf',
                                       5,
                                       Leaf')),
                         1,
                         Node' (Leaf',
                                3,
                                Node' (Node' (Leaf',
                                              7,
                                              Leaf'),
                                       6,
                                       Leaf'))))
       = 7);;
    
  let number_of_nodes' t =
    (* number_of_nodes' : 'a binary_tree' -> int *)
    fold_right_binary_tree' (0)
                            (fun (ih1, v, ih2) -> ih1 + ih2 + 1)
                            t;;
  
  let () = assert (test_number_of_nodes'_int number_of_nodes');;
    
  (* ********** *)

  (*
     Question 2.4:
   
     Implement an alternative OCaml function that tests whether a given
     binary tree is left-balanced, using a fold-right function.
  *)
  
  let test_left_balanced'_int candidate =
    (* test_left_balanced'_int : (int binary_tree' -> bool) -> bool *)
    (candidate Leaf'
     = true)
    &&
      (candidate (Node' (Leaf',
                         1,
                         Leaf'))
       = true)
    &&
      (candidate (Node' (Node' (Leaf',
                                2,
                                Leaf'),
                         1,
                         Leaf'))
       = true)
    &&
      (candidate (Node' (Node' (Node' (Leaf',
                                       3,
                                       Leaf'),
                                2,
                                Leaf'),
                         1,
                         Leaf'))
       = true)
    &&
      (candidate (Node' (Node' (Node' (Node' (Leaf',
                                              3,
                                              Leaf'),
                                       2,
                                       Leaf'),
                                1,
                                Leaf'),
                         0,
                         Leaf'))
       = true)
    &&
      (candidate (Node' (Node' (Leaf',
                                1,
                                Leaf'),
                         3,
                         Node' (Leaf',
                                0,
                                Leaf')))
       = false)
    &&
      (* Jaime's tests: *)
      (candidate (Node' (Node' (Node' (Leaf',
                                       100,
                                       Leaf'),
                                10,
                                Leaf'),
                         1,
                         Leaf'))
       = true)
    &&
      (candidate (Node' (Leaf',
                         10,
                         Node' (Leaf',
                                20,
                                Node' (Leaf',
                                       30,
                                       Node' (Leaf',
                                              40,
                                              Leaf')))))
       = false)
    &&
      (candidate (Node' (Node' (Node' (Leaf',
                                       4,
                                       Leaf'),
                                2,
                                Node' (Leaf',
                                       5,
                                       Leaf')),
                         1,
                         Node' (Leaf',
                                3,
                                Node' (Node' (Leaf',
                                              7,
                                              Leaf'),
                                       6,
                                       Leaf'))))
       = false)
  (* etc. *);;
  
  let left_balanced' t =
    (* left_balanced' : 'a binary_tree' -> bool *)
    super_fold_right_binary_tree' (true)
                                  (fun (t1, ih1, v, t2, ih2) -> (match t2 with
                                                                 | Leaf' ->
                                                                    ih1
                                                                 | Node' _ ->
                                                                    false))
                                  t;;
  
  let () = assert (test_left_balanced'_int left_balanced');;
  
  (* ********** *)
  
  (* Part 3 *)

  type 'a left_binary_tree' =
    | Left_Leaf'
    | Left_Node' of 'a left_binary_tree' * 'a;;

  let fold_right_left_binary_tree' left_leaf'_case left_node'_case lt_init =
    let rec left_visit lt =
      match lt with
      | Left_Leaf' ->
         left_leaf'_case
      | Left_Node' (lt', v) ->
         let ih = left_visit lt'
         in left_node'_case (ih, v)
    in left_visit lt_init;;

  let superfold_right_left_binary_tree' left_leaf'_case left_node'_case lt_init =
    let rec left_visit lt =
      match lt with
      | Left_Leaf' ->
         left_leaf'_case
      | Left_Node' (lt', v) ->
         let ih' = left_visit lt'
         in left_node'_case (lt', ih', v)
    in left_visit lt_init;;

  (* ********** *)

  (*
     Question 3.0:
   
     a. Implement an alternative OCaml function that embeds a left binary
        tree into a binary tree, using a fold-right function.
   
     b. Implement an alternative OCaml function that projects a binary
        tree into an optional left binary tree, using a fold-right function.
   *)

  (* a *)
  (* Implementing unit-tests *)
  (* Jaime's tests: *)    
let test_embed_left_binary_tree'_into_binary_tree' candidate =
  (candidate Left_Leaf' = Leaf')
  &&
    (candidate (Left_Node' (Left_Leaf', 1)) = (Node' (Leaf',
                                                      1,
                                                      Leaf')))
  &&
    (candidate (Left_Node' (Left_Node' (Left_Leaf', 1), 10)) = (Node' (Node' (Leaf',
                                                                              1,
                                                                              Leaf'),
                                                                       10,
                                                                       Leaf')))
  &&
    (candidate (Left_Node' (Left_Node' (Left_Node' (Left_Leaf', 1), 10), 100)) = (Node' (Node' (Node' (Leaf',
                                                                                                       1,
                                                                                                       Leaf'),
                                                                                                10,
                                                                                                Leaf'),
                                                                                         100,
                                                                                         Leaf')))
(* etc. *);;
  let rec embed_left_binary_tree'_into_binary_tree' t =
       (* embed_left_binary_tree'_into_binary_tree' : 'a left_binary_tree' -> 'a binary_tree' *)
    match t with
    | Left_Leaf' ->
       Leaf'
    | Left_Node' (t1, v) ->
       Node' (embed_left_binary_tree'_into_binary_tree' t1, v, Leaf');;
  
  let embed_left_binary_tree'_into_binary_tree'_alt t =
    (* embed_left_binary_tree'_into_binary_tree'_alt : 'a left_binary_tree' -> 'a binary_tree' *)
    fold_right_left_binary_tree' (Leaf')
                                 (fun (ih, v) -> Node' (ih, v, Leaf'))
                                 t;;

    let () = assert(test_embed_left_binary_tree'_into_binary_tree' embed_left_binary_tree'_into_binary_tree'_alt);;

  (* ***** *)
    (* b *)
    let test_project_binary_tree'_into_left_binary_tree' candidate =
      (candidate (Leaf') = Some (Left_Leaf'))
      &&
        (candidate (Node' (Leaf', 1, Leaf'))
         = Some (Left_Node' (Left_Leaf',
                             1)))
      &&
        (candidate (Node' (Node' (Node'(Leaf',
                                        1,
                                        Leaf'),
                                  2,
                                  Leaf'),
                           3,
                           Leaf'))
         = Some (Left_Node' (Left_Node' (Left_Node' (Left_Leaf',
                                                     1),
                                         2),
                             3)))
      &&
        (candidate (Node' (Node' (Leaf',
                                  1,
                                  Leaf'),
                           2,
                           Node' (Leaf',
                                  3,
                                  Leaf')))
         = None )    
    (* etc *)
    ;;
  
  let rec project_binary_tree'_into_left_binary_tree' t =
       (* project_binary_tree'_into_left_binary_tree' : 'a binary_tree' -> 'a left_binary_tree' option *)
    match t with
    | Leaf' ->
       Some Left_Leaf'
    | Node' (t1, v, t2) ->
       match t2  with
       | Leaf' ->
          (match project_binary_tree'_into_left_binary_tree' t1 with
           | Some t1' ->
              Some (Left_Node' (t1', v))
           | None ->
              None)
       | Node' _ ->
          None;;

  let project_binary_tree'_into_left_binary_tree'_alt t =
    (* project_binary_tree'_into_left_binary_tree'_alt : 'a binary_tree' -> 'a left_binary_tree' option *)
    super_fold_right_binary_tree' (Some (Left_Leaf'))
                                  (fun (t1, ih1, v, t2, ih2) ->
                                    match t2 with
                                    | Leaf' ->
                                       (match ih1 with
                                        | Some (t1') ->
                                           Some (Left_Node' (t1', v))
                                        | None ->
                                           None)
                                    | Node' _ ->
                                       None
                                  )
                                  t
  ;;

  let () = assert(test_project_binary_tree'_into_left_binary_tree' project_binary_tree'_into_left_binary_tree'_alt);;

  (* ********** *)

  (*
     Question 3.2 (optional):
   
     Implement an OCaml function that indexes a given left-tree at a given
     depth, using a fold-right function.
  *)

  let left_index lt_init n_init =
   (* left_index : 'a left_binary_tree' -> int -> 'a option *)
    if n_init < 0
    then raise (Failure "left_index")
    else let rec visit lt n =
           match lt with
           | Left_Leaf' ->
              None
           | Left_Node' (lt', v) ->
              if n = 0
              then Some v
              else visit lt' (n - 1)
         in visit lt_init n_init;;

  let left_index_alt lt_init n_init =
   (* left_index_alt : 'a left_binary_tree' -> int -> 'a option *)
    (fold_right_left_binary_tree' (fun n -> None)
                                 (fun (ih, v) ->
                                   fun n ->
                                   if n = 0
                                   then Some v
                                   else match ih (n-1) with
                                        | Some v' ->
                                           Some v'
                                        | None ->
                                           None)
                                 lt_init) n_init;;

  (* ********** *)

  (*
     Question 3.3:
   
     Implement an OCaml function that stitches together two left-balanced
     binary trees, using a fold-right function.
  *)

  let test_left_stitch_int candidate =
    (* test_left_stitch_int : (int left_binary_tree' -> int left_binary_tree' -> int left_binary_tree') -> bool *)
    (candidate Left_Leaf'
               Left_Leaf'
     = Left_Leaf')
    &&
      (candidate (Left_Node' (Left_Leaf', 1))
                 Left_Leaf'
       = Left_Node' (Left_Leaf', 1))
    &&
      (candidate Left_Leaf'
                 (Left_Node' (Left_Leaf', 1))
       = Left_Node' (Left_Leaf', 1))
    &&
      (candidate (Left_Node' (Left_Node' (Left_Leaf', 2), 1))
                 Left_Leaf'
       = Left_Node' (Left_Node' (Left_Leaf', 2), 1))
    &&
      (candidate Left_Leaf'
                 (Left_Node' (Left_Node' (Left_Leaf', 2), 1))
       = Left_Node' (Left_Node' (Left_Leaf', 2), 1))
    &&
      (candidate (Left_Node' (Left_Node' (Left_Leaf', 4), 3))
                 (Left_Node' (Left_Node' (Left_Leaf', 2), 1))
       = Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Leaf', 4), 3), 2), 1))
    &&
      (* Jaime's tests: *)
      (candidate (Left_Node' (Left_Node' (Left_Node' (Left_Leaf',
                                                      100),
                                          10),
                              1))
                 (Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Leaf',
                                                                  1),
                                                      10),
                                          100),
                              1000))
       = Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Leaf',
                                                                                             100),
                                                                                 10),
                                                                     1),
                                                         1),
                                             10),
                                 100),
                     1000))
    &&
      (candidate (Left_Node' (Left_Node' (Left_Leaf',
                                          4),
                              5))
                 (Left_Node' (Left_Node' (Left_Node' (Left_Leaf',
                                                      1),
                                          2),
                              3))
       = Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Leaf',
                                                                     4),
                                                         5),
                                             1),
                                 2),
                     3))
  (* etc. *);;

  let rec left_stitch lt1 lt2 =
    (* left_stitch : 'a left_binary_tree' -> 'a left_binary_tree' -> 'a left_binary_tree' *)
    match lt2 with
    | Left_Leaf' ->
       lt1
    | Left_Node' (lt2', n) ->
       let ih = left_stitch lt1 lt2' 
       in Left_Node' (ih, n)
  ;;

  let () = assert (test_left_stitch_int left_stitch);;

  let left_stitch_alt lt1 lt2 =
    (* left_stitch_alt : 'a left_binary_tree' -> 'a left_binary_tree' -> 'a left_binary_tree' *)
    fold_right_left_binary_tree' (lt1)
                                 (fun(ih, v) -> Left_Node' (ih, v))
                                 (lt2);;

  let () = assert (test_left_stitch_int left_stitch_alt);;

  (* ********** *)

  (*
    Question 3.4 (optional):
   
     Implement an OCaml function that rotates a binary tree to the left,
     using a fold-right function.
  *)

  let test_left_rotate_int candidate =
   (* test_left_rotate_int : (int binary_tree' -> int left_binary_tree') -> bool *)
    (candidate Leaf'
     = Left_Leaf')
    &&
    (candidate (Node' (Leaf',
                       1,
                       Leaf'))
     = Left_Node' (Left_Leaf',
                   1))
    &&
    (candidate (Node' (Node' (Leaf',
                              2,
                              Leaf'),
                       1,
                       Node' (Leaf',
                              3,
                              Leaf')))
     = Left_Node' (Left_Node' (Left_Node' (Left_Leaf',
                                           2),
                               1),
                   3))
    &&
    (candidate (Node' (Node' (Node' (Leaf',
                                     4,
                                     Leaf'),
                              2,
                              Node' (Leaf',
                                     5,
                                     Leaf')),
                       1,
                       Node' (Node' (Leaf',
                                     6,
                                     Leaf'),
                              3,
                              Node' (Leaf',
                                     7,
                                     Leaf'))))
     = Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Node' (Left_Leaf',
                                                                                           4),
                                                                               2),
                                                                   5),
                                                       1),
                                           6),
                               3),
                   7))
    (* etc. *);;
  
  let left_rotate t =
   (* left_rotate : 'a binary_tree' -> 'a left_binary_tree' *)
    match t with
    | Leaf' ->
       Left_Leaf'
    | Node' (t1, n, t2) ->
       let t1' = left_rotate t1
       and t2' = left_rotate t2
       in left_stitch (Left_Node' (t1', n)) t2'
  ;;
  
  (*
  let () = assert (test_left_rotate_int left_rotate);;
  *)

  let left_rotate_alt t =
   (* left_rotate_alt : 'a binary_tree' -> 'a left_binary_tree' *)
    fold_right_binary_tree' Left_Leaf'
                            (fun (ih1, v, ih2) -> left_stitch_alt (Left_Node' (ih1, v)) ih2)
                            t_init
  ;;
  
  (*
  let () = assert (test_left_rotate_int left_rotate_alt);;
  *)

  (* ********** *)

end;;

(* end of week-15_0-midterm-project-revisited.ml *)

"week-15_0-midterm-project-revisited.ml"
