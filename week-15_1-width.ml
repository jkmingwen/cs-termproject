(* week-15_1-width.ml *)
(* Introduction to Computer Science (YSC1212), Sem1, 2017-2018 *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Version of Sat 19 Nov 2017 *)

(* ********** *)

(*
   name: 
   student ID number:
   e-mail address:
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

module type FIRST_MINI_PROJECT =
sig
  exception Not_implemented_yet of string

  type 'a binary_tree =
    | Leaf of 'a
    | Node of 'a binary_tree * 'a binary_tree

  val test_width_int : (int binary_tree -> int) -> bool

  val width : 'a binary_tree -> int
end;;

module First_mini_project : FIRST_MINI_PROJECT =
struct

  exception Not_implemented_yet of string;;

  (* ********** *)
  
  type 'a binary_tree =
    | Leaf of 'a
    | Node of 'a binary_tree * 'a binary_tree;;
  
  (* ********** *)
  
  let test_width_int candidate_width =
   (* test_width_int : (int binary_tree -> int) -> bool *)
      (candidate_width (Leaf 1)
       = 1)
    &&
      (candidate_width (Node (Leaf 1,
                              Leaf 2))
       = 2)
    &&
      (candidate_width (Node (Leaf 1,
                              Node (Leaf 1,
                                    Leaf 2)))
       = 2)
    &&
      (candidate_width (Node (Node (Leaf 1,
                                    Leaf 2),
                              Leaf 2))
       = 2)
    &&
      (candidate_width (Node (Node (Leaf 1,
                                    Leaf 2),
                              Node (Leaf 3,
                                    Leaf 4)))
       = 4)
    &&
      (candidate_width (Node (Node (Node (Leaf 1,
                                          Leaf 2),
                                    Leaf 2),
                              Node (Node (Leaf 3,
                                          Leaf 4),
                                    Node (Leaf 5,
                                          Leaf 6))))
       = 6)
    &&
      (candidate_width (Node (Node (Node (Node (Leaf 1,
                                                Leaf 2),
                                          Leaf 2),
                                    Leaf 2),
                              Node (Node (Leaf 3,
                                          Node (Leaf 3,
                                                Leaf 4)),
                                    Node (Leaf 5,
                                          Leaf 6))))
       = 6)
    (* etc. *);;

  (* ********** *)
  let fold_right_binary_tree leaf_case node_case t_init =
    let rec visit t =
      match t with
      | Leaf v ->
         leaf_case v
      | Node (t1, t2) ->
         let ih1 = visit t1
         and ih2 = visit t2
         in node_case (ih1, ih2)
    in visit t_init;;
        
  let maximum xs =
    let rec max_aux xs n =
      match xs with
      | [] ->
         n
      | x :: xs' ->
         if x > n
         then max_aux xs' x
         else max_aux xs' n
    in max_aux xs 0
  ;;

  let rec conjoin xs ys =
    match xs with
    | [] ->
       ys
    | x :: xs' ->
       match ys with
       | [] ->
          xs
       | y :: ys' ->
          (x + y) :: (conjoin xs' ys')
  ;;
    
  let width t =
    (* width : 'a binary_tree -> int *)
    maximum (fold_right_binary_tree (fun v -> [1])
                                     (fun (ih1, ih2) -> List.append [1] (conjoin ih1 ih2))
                                     t);;

  let () = assert (test_width_int width);;

  (* ********** *)

end;;

(* ********** *)

(* end of week-15_1-width.ml *)

"week-15_1-width.ml"
