(* week-15_4-three-language-processors-for-arithmetic-expressions.ml *)
(* Introduction to Computer Science (YSC1212), Sem1, 2017-2018 *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Version of Wed 22 Nov 2017 *)
(* was: *)
(* Version of Mon 20 Nov 2017 *)
(* was: *)
(* Version of Sun 19 Nov 2017 *)

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

let show_int n =
  if n < 0
  then "(" ^ string_of_int n ^ ")"
  else string_of_int n;;

let show_string s =
 (* show_string : string -> string *)
  "\"" ^ s ^ "\"";;

let show_list show_yourself vs =
  match vs with
  | [] ->
     "[]"
  | v :: vs' ->
     let rec show_list_aux v vs' =
       match vs' with
       | [] ->
          show_yourself v
       | v' :: vs'' ->
          (show_yourself v) ^ "; " ^ (show_list_aux v' vs'')
     in "[" ^ show_list_aux v vs' ^ "]";;

(* ********** *)

module type FOURTH_MINI_PROJECT =
sig
  type arithmetic_expression =
    | Literal of int
    | Plus of arithmetic_expression * arithmetic_expression
    | Minus of arithmetic_expression * arithmetic_expression
    | Quotient of arithmetic_expression * arithmetic_expression
    | Remainder of arithmetic_expression * arithmetic_expression

  type source_program = Source_program of arithmetic_expression

  type expressible_value =
    | Expressible_int of int
    | Expressible_msg of string

  val int_test_interpret : (source_program -> expressible_value) -> bool
  val msg_test_interpret : (source_program -> expressible_value) -> bool
  val interpret : source_program -> expressible_value

  type byte_code_instruction = Push of int | Add | Sub | Quo | Rem
  val show_byte_code_instruction : byte_code_instruction -> string
  type target_program = Target_program of byte_code_instruction list

  type stackable_value = int
  type data_stack = stackable_value list
  val show_data_stack : data_stack -> string
  type result_of_decoding_and_execution =
    | OK of data_stack
    | KO of string
  val show_result_of_decoding_and_execution : result_of_decoding_and_execution -> string

  val test_decode_execute : (byte_code_instruction -> data_stack -> result_of_decoding_and_execution) -> bool
  val decode_execute : byte_code_instruction -> data_stack -> result_of_decoding_and_execution

  val int_test_run : (target_program -> expressible_value) -> bool
  val msg_test_run : (target_program -> expressible_value) -> bool
  val run : target_program -> expressible_value
  val traced_run : target_program -> expressible_value

  val test_compile : (source_program -> target_program) -> bool
  val compile : source_program -> target_program
  val compile_alt : source_program -> target_program

  val commutativity_test : (source_program -> expressible_value) ->
                           (source_program -> target_program) ->
                           (target_program -> expressible_value) ->
                           source_program ->
                           bool

  val generate_random_arithmetic_expression : int -> arithmetic_expression
end;;

module Fourth_mini_project : FOURTH_MINI_PROJECT =
struct

  exception Not_implemented_yet of string;;

  (* ********** *)
  
  type arithmetic_expression =
    | Literal of int
    | Plus of arithmetic_expression * arithmetic_expression
    | Minus of arithmetic_expression * arithmetic_expression
    | Quotient of arithmetic_expression * arithmetic_expression
    | Remainder of arithmetic_expression * arithmetic_expression;;
  
  type source_program =
    | Source_program of arithmetic_expression;;
  
  (* ********** *)
  
  type expressible_value =
    | Expressible_int of int
    | Expressible_msg of string;;
  
  (* ********** *)
  (* Task 1 *)
  let int_test_interpret candidate_interpret =
    (* int_test_interpret : (source_program -> expressible_value) -> bool *)
    (* Given tests: *)
      (candidate_interpret (Source_program (Literal 0))
       = Expressible_int 0)
    &&
      (candidate_interpret (Source_program (Plus (Literal 1, Literal 0)))
       = Expressible_int 1)
    &&
      (candidate_interpret (Source_program (Plus (Literal 10, Plus (Literal 1, Literal 0))))
       = Expressible_int 11)
    &&
      (candidate_interpret (Source_program (Plus (Plus (Literal 10, Literal 1), Literal 0)))
       = Expressible_int 11)
    &&
      (candidate_interpret (Source_program (Minus (Literal 10, Literal 10)))
       = Expressible_int 0)
  (* Jaime's tests: *)
    &&
      (candidate_interpret (Source_program (Minus ((Plus (Literal 10, Literal 10)), Literal 15)))
       = Expressible_int 5)
    &&
      (candidate_interpret (Source_program (Quotient ((Plus (Literal 10, Literal 10)), Literal 5)))
       = Expressible_int 4)
    &&
      (candidate_interpret (Source_program (Remainder ((Plus (Literal 10, Literal 10)), Literal 15)))
       = Expressible_int 5)
    &&
      (candidate_interpret (Source_program (Quotient ((Remainder ((Plus (Literal 10, Literal 10)), Literal 15)), Literal 5)))
       = Expressible_int 1)
  (* Zhongxuan's Tests: *)
    &&
      (candidate_interpret (Source_program (Minus (Literal 20, ((Plus (Literal 15, Literal 10))))))
       = Expressible_int (-5))
    &&
      (candidate_interpret (Source_program (Quotient ((Minus (Literal 10, Literal 20)), Literal 2)))
       = Expressible_int (-5))
    &&
      (candidate_interpret (Source_program (Remainder ((Plus (Literal (-10), Literal (-15))), Literal 6)))
      = Expressible_int (-1))
  (* etc. *);;
  
  let msg_test_interpret candidate_interpret =
    (* msg_test_interpret : (source_program -> expressible_value) -> bool *)
    (* Given tests: *)
      (candidate_interpret (Source_program (Quotient (Literal 0, Literal 0)))
       = Expressible_msg "quotient of 0 over 0")
    &&
      (candidate_interpret (Source_program (Remainder (Literal 0, Literal 0)))
       = Expressible_msg "remainder of 0 over 0")
  (* Jaime's tests: *)
    &&
      (candidate_interpret (Source_program (Quotient (Literal 5, Literal 0)))
       = Expressible_msg "quotient of 5 over 0")
    &&
      (candidate_interpret (Source_program (Remainder (Literal 5, Literal 0)))
       = Expressible_msg "remainder of 5 over 0")
    &&
      (candidate_interpret (Source_program (Quotient ((Remainder (Literal 5, Literal 0)), Literal 0)))
       = Expressible_msg "remainder of 5 over 0")
    &&
      (candidate_interpret (Source_program (Quotient ((Minus (Literal 5, Literal 0)), Literal 0)))
       = Expressible_msg "quotient of 5 over 0")
    &&
      (candidate_interpret (Source_program (Quotient ((Minus (Literal 4, Literal 5)), Literal 0)))
       = Expressible_msg "quotient of (-1) over 0")
  (* Zhongxuan's tests: *)
    &&
      (candidate_interpret (Source_program (Remainder ((Quotient (Literal 5, Literal 0)), Literal 0)))
       = Expressible_msg "quotient of 5 over 0")
    &&
      (candidate_interpret (Source_program (Plus ((Remainder (Literal 5, Literal 0)), (Quotient (Literal 10, Literal 0)))))
      = Expressible_msg "remainder of 5 over 0")
  (* etc. *);;
  
  (* ********** *)
  
  let interpret (Source_program e) =
    (* interpret : source_program -> expressible_value *)
    let rec evaluate e =
      (* evaluate : arithmetic_expression -> expressible_value *)
      match e with
      | Literal n ->
         Expressible_int n
      | Plus (e1, e2) ->
         (match evaluate e1 with
          | Expressible_int n1 ->
             (match evaluate e2 with
              | Expressible_int n2 ->
                 Expressible_int (n1 + n2)
              | Expressible_msg s ->
                 Expressible_msg s)
          | Expressible_msg s ->
             Expressible_msg s)
      | Minus (e1, e2) ->
         (match evaluate e1 with
          | Expressible_int n1 ->
             (match evaluate e2 with
              | Expressible_int n2 ->
                 Expressible_int (n1 - n2)
              | Expressible_msg s ->
                 Expressible_msg s)
          | Expressible_msg s ->
             Expressible_msg s)
      | Quotient (e1, e2) ->
         (match evaluate e1 with
          | Expressible_int n1 ->
             (match evaluate e2 with
              | Expressible_int n2 ->
                 if n2 = 0
                 then Expressible_msg ("quotient of " ^ (show_int n1) ^ " over 0")
                 else Expressible_int (n1 / n2)
              | Expressible_msg s ->
                 Expressible_msg s)
          | Expressible_msg s ->
             Expressible_msg s)
      | Remainder (e1, e2) ->
         (match evaluate e1 with
          | Expressible_int n1 ->
             (match evaluate e2 with
              | Expressible_int n2 ->
                 if n2 = 0
                 then Expressible_msg ("remainder of " ^ (show_int n1) ^ " over 0")
                 else Expressible_int (n1 mod n2)
              | Expressible_msg s ->
                 Expressible_msg s)
          | Expressible_msg s ->
             Expressible_msg s)
    in evaluate e;;
  
  let () = assert (int_test_interpret interpret);;
  
  let () = assert (msg_test_interpret interpret);;

  (* a *)
  (* Left to right *)
  (* b *)
  (* Yes, but only for error messages -
     e.g. interpret (Source_program (Plus ((Quotient (Literal 0, Literal 0)), (Remainder (Literal 0, Literal 0)))));; *)
  
  (* ********** *)

  type byte_code_instruction =
    | Push of int
    | Add
    | Sub
    | Quo
    | Rem;;
  
  let show_byte_code_instruction bci =
    match bci with
    | Push n ->
       "Push " ^ (show_int n)
    | Add ->
       "Add"
    | Sub ->
       "Sub"
    | Quo ->
       "Quo"
    | Rem ->
       "Rem";;

  type target_program =
    | Target_program of byte_code_instruction list;;
  
  (* ********** *)
  
  type stackable_value = int;;
    
  type data_stack = stackable_value list;;
    
  let show_data_stack ds =
    show_list show_int ds;;

  type result_of_decoding_and_execution =
    | OK of data_stack
    | KO of string;;
  
  let show_result_of_decoding_and_execution r =
    match r with
    | OK ds ->
       "OK " ^ show_data_stack ds
    | KO s ->
       "KO " ^ show_string s;;
  (* Task 2 *)
  let test_decode_execute candidate_decode_execute =
   (* test_decode_execute : (byte_code_instruction -> data_stack -> result_of_decoding_and_execution) -> bool *)
    (candidate_decode_execute (Push 33) [] =
       OK [33]) &&
    (candidate_decode_execute (Push 33) [34; 35] =
       OK [33; 34; 35]) &&
    (candidate_decode_execute Add [] =
       KO "stack underflow for Add") &&
    (candidate_decode_execute Add [10] =
       KO "stack underflow for Add") &&
    (candidate_decode_execute Add [10; 10] =
       OK [20]) &&
    (candidate_decode_execute Quo [0; 0] =
       KO "quotient of 0 over 0") &&
    (* Jaime's tests: *)
    (candidate_decode_execute Add [2; 2; 2] =
       OK [4; 2]) &&
    (candidate_decode_execute Quo [42; 0] =
       KO "quotient of 42 over 0") &&
    (candidate_decode_execute Quo [0; 42] =
       OK [0]) &&
    (candidate_decode_execute Rem [22; 0; 1] =
       KO "remainder of 22 over 0") &&
    (candidate_decode_execute Rem [0; 22; 1] =
       OK [0; 1]) &&
    (candidate_decode_execute Sub [4; 5; 6] =
       OK [-1; 6]) &&
    (candidate_decode_execute Sub [-1; 2; -3] =
       OK [-3; -3]) &&
  (* Zhongxuan's tests: *)
    (candidate_decode_execute Sub [] =
       KO "stack underflow for Sub") &&
    (candidate_decode_execute Quo [10] =
       KO "stack underflow for Quo") &&
    (candidate_decode_execute Rem [20] =
       KO "stack underflow for Rem") &&
    (candidate_decode_execute Add [20; -30; 10] =
       OK [-10; 10]) &&
    (candidate_decode_execute Rem [20; 5] =
       OK [0]) &&
    (candidate_decode_execute Quo [20; 50] =
       OK [0])
  (* etc. *);;
  
  let decode_execute bci ds =
    (* decode_execute : byte_code_instruction -> data_stack -> result_of_execution *)
    match bci with
    | Push n ->
       OK (n::ds)
    | Add ->
       (match ds with
        | [] ->
           KO "stack underflow for Add"
        | [_] ->
           KO "stack underflow for Add"
        | n1::n2::ds'' ->
           let n3 = n1 + n2
           in OK (n3::ds''))
    | Sub ->
       (match ds with
        | [] ->
           KO "stack underflow for Sub"
        | [_] ->
           KO "stack underflow for Sub"
        | n1::n2::ds'' ->
           let n3 = n1 - n2
           in OK (n3::ds''))
    | Quo ->
       (match ds with
        | [] ->
           KO "stack underflow for Quo"
        | [_] ->
           KO "stack underflow for Quo"
        | n1::n2::ds'' ->
           if n2 = 0
           then KO ("quotient of " ^ string_of_int n1 ^ " over 0")
           else 
             let n3 = n1 / n2
             in OK (n3 :: ds''))
    | Rem ->
       (match ds with
        | [] ->
           KO "stack underflow for Rem"
        | [_] ->
           KO "stack underflow for Rem"
        | n1::n2::ds'' ->
           if n2 = 0
           then KO ("remainder of " ^ string_of_int n1 ^ " over 0")
           else 
             let n3 = n1 mod n2
             in OK (n3::ds''))
  ;;
  
  let () = assert (test_decode_execute decode_execute);;
  
  (* ********** *)
  
  let int_test_run candidate_run =
    (* int_test_run : target_program -> expressible_value *)
    (* Given tests: *)
      (candidate_run (Target_program [Push 0])
       = Expressible_int 0)
    &&
      (candidate_run (Target_program [Push 1; Push 0; Add])
       = Expressible_int 1)
    &&
      (candidate_run (Target_program [Push 10; Push 1; Push 0; Add; Add])
       = Expressible_int 11)
    &&
      (candidate_run (Target_program [Push 10; Push 1; Add; Push 0; Add])
       = Expressible_int 11)
    &&
      (candidate_run (Target_program [Push 10; Push 10; Sub])
       = Expressible_int 0)
  (* Jaime's tests: *)
    &&
      (candidate_run (Target_program [Push 1; Push 2; Push 3; Sub; Quo])
       = Expressible_int 1)
    &&
      (candidate_run (Target_program [Push 4; Push 3; Quo; Push 2; Push 1; Rem; Add])
       = Expressible_int 1)
    &&
      (candidate_run (Target_program [Push (-1); Push (-2); Push (-3); Rem; Add; Push (-4); Rem])
       = Expressible_int 0)
    &&
      (candidate_run (Target_program [Push 4; Push 5; Sub; Push 0; Quo])
       = Expressible_int 0)
  (* Zhongxuan's tests: *)
    &&
      (candidate_run (Target_program [Push 4; Push 5; Push 10; Quo; Sub])
       = Expressible_int (-2))
    &&
      (candidate_run (Target_program [Push 5; Push (-33); Rem; Push 8; Add])
       = Expressible_int 5)
    &&
      (candidate_run (Target_program [Push (-3); Push (-10); Push 16; Rem; Quo])
       = Expressible_int (-2))
    (* etc. *);;
  
  let msg_test_run candidate_run =
    (* msg_test_run : target_program -> expressible_value *)
    (* Given tests: *)
      (candidate_run (Target_program [])
       = Expressible_msg "stack underflow at the end")
    &&
      (candidate_run (Target_program [Push 2; Push 3])
       = Expressible_msg "stack overflow at the end")
    &&
      (candidate_run (Target_program [Add])
       = Expressible_msg "stack underflow for Add")
    &&
      (candidate_run (Target_program [Push 2; Add])
       = Expressible_msg "stack underflow for Add")
    &&
      (candidate_run (Target_program [Push 0; Push 0; Quo])
       = Expressible_msg "quotient of 0 over 0")
    &&
      (candidate_run (Target_program [Push 0; Push 0; Rem])
       = Expressible_msg "remainder of 0 over 0")
  (* Jaime's tests: *)
    &&
      (candidate_run (Target_program [Push 0; Push 1; Quo])
       = Expressible_msg "quotient of 1 over 0")
    &&
      (candidate_run (Target_program [Push 0; Push 2; Rem])
       = Expressible_msg "remainder of 2 over 0")
    &&
      (candidate_run (Target_program [Push 5; Push 5; Sub; Push 0; Quo])
       = Expressible_msg "quotient of 0 over 0")
    &&
      (candidate_run (Target_program [Push 0; Quo; Push 4; Push 5; Sub])
       = Expressible_msg "stack underflow for Quo")
  (* Zhongxuan's tests: *)
    &&
      (candidate_run (Target_program [Push 0; Push 0; Push 0; Quo; Rem])
       = Expressible_msg "quotient of 0 over 0")
    &&
      (candidate_run (Target_program [Push 3; Push 2; Push 1; Add; Sub; Quo])
       = Expressible_msg "stack underflow for Quo")
    &&
      (candidate_run (Target_program [Push 3; Push 2; Push 1; Add])
       = Expressible_msg "stack overflow at the end")
    (* etc. *);;
  
  let run (Target_program bcis) =
   (* run : target_program -> expressible_value *)
    let rec fetch_decode_execute bcis ds =
         (* fetch_decode_execute : byte_code_instruction list -> data_stack -> result_of_execution *)
      match bcis with
      | [] ->
         OK ds
      | bci :: bcis' ->
         (match decode_execute bci ds with
          | OK ds' ->
            fetch_decode_execute bcis' ds'
          | KO s ->
             KO s)
    in match fetch_decode_execute bcis [] with
       | OK [] ->
          Expressible_msg "stack underflow at the end"
       | OK (n :: []) ->
          Expressible_int n
       | OK (n :: _ :: _) ->
          Expressible_msg "stack overflow at the end"
       | KO s ->
          Expressible_msg s;;
  
  let () = assert (int_test_run run);;
  
  let () = assert (msg_test_run run);;
  
  (* ***** *)
  
  let traced_run (Target_program bcis) =
   (* run : target_program -> expressible_value *)
    let rec fetch_decode_execute bcis ds =
         (* fetch_decode_execute : byte_code_instruction list -> data_stack -> result_of_execution *)
      Printf.printf "fetch_decode_execute\n  %s\n  %s\n"
                    (show_list show_byte_code_instruction bcis)
                    (show_data_stack ds);
      match bcis with
      | [] ->
         OK ds
      | bci :: bcis' ->
         (match decode_execute bci ds with
          | OK ds' ->
            fetch_decode_execute bcis' ds'
          | KO s ->
             KO s)
    in let r = fetch_decode_execute bcis []
       in Printf.printf "fetch_decode_execute <- %s\n"
                        (show_result_of_decoding_and_execution r);
          match r with
          | OK [] ->
             Expressible_msg "stack underflow at the end"
          | OK (n :: []) ->
             Expressible_int n
          | OK (n :: _ :: _) ->
             Expressible_msg "stack overflow at the end"
          | KO s ->
             Expressible_msg s;;
  
  (* ********** *)
  (* Task 4 *)  
  let test_compile candidate_compile =
    (* test_compile : (source_program -> byte_code_program) -> bool *)
    (* Given tests: *)
      (candidate_compile (Source_program (Literal 32))
       = Target_program [Push 32])
    &&
      (candidate_compile (Source_program (Plus (Literal 1, Literal 10)))
       = Target_program [Push 1; Push 10; Add])
    &&
      (candidate_compile (Source_program (Plus (Plus (Literal 1, Literal 10), Plus (Literal 20, Literal 2))))
       = Target_program [Push 1; Push 10; Add; Push 20; Push 2; Add; Add])
    &&
      (candidate_compile (Source_program (Plus (Literal 1, Plus (Literal 10, Plus (Literal 20, Literal 2)))))
       = Target_program [Push 1; Push 10; Push 20; Push 2; Add; Add; Add])
    &&
      (candidate_compile (Source_program (Plus (Literal 1, Plus (Plus (Literal 10, Literal 20), Literal 2))))
       = Target_program [Push 1; Push 10; Push 20; Add; Push 2; Add; Add])
    &&
      (candidate_compile (Source_program (Quotient (Literal 4, Literal 5)))
       = Target_program [Push 4; Push 5; Quo])
  (* Jaime's tests: *)
    &&
      (candidate_compile (Source_program (Quotient (Literal 1, (Minus (Literal 2, Literal 3)))))
       = Target_program [Push 1; Push 2; Push 3; Sub; Quo])
    &&
      (candidate_compile (Source_program (Plus (Quotient (Literal 4, Literal 3), Remainder(Literal 2, Literal 1))))
       = Target_program [Push 4; Push 3; Quo; Push 2; Push 1; Rem; Add])
    &&
      (candidate_compile (Source_program (Remainder (Plus (Literal (-1), Remainder (Literal (-2), Literal (-3))), Literal (-4))))
       = Target_program [Push (-1); Push (-2); Push (-3); Rem; Add; Push (-4); Rem])
  (* Zhongxuan's tests: *)
    &&
      (candidate_compile (Source_program (Minus (Plus (Literal 5, Literal 10), Quotient (Literal 15, Literal 20))))
       = Target_program [Push 5; Push 10; Add; Push 15; Push 20; Quo; Sub])
    &&
      (candidate_compile (Source_program (Plus (Remainder (Literal 5, Literal 10), Literal 15)))
       = Target_program [Push 5; Push 10; Rem; Push 15; Add])
    &&
      (candidate_compile (Source_program (Quotient (Literal 5, Plus (Literal 10, Literal 15))))
      = Target_program [Push 5; Push 10; Push 15; Add; Quo])
    (* etc. *);;
  
  let compile (Source_program e) =
    (* compile : source_program -> target_program *)
    let rec translate e =
      (* translate : arithmetic_expression -> byte_code_instruction list *)
      match e with
      | Literal n ->
         [Push n]
      | Plus (e1, e2) ->
         let bcis1 = translate e1
         and bcis2 = translate e2
         in (List.append bcis1 (List.append bcis2 [Add]))
      | Minus (e1, e2) ->
         let bcis1 = translate e1
         and bcis2 = translate e2
         in (List.append bcis1 (List.append bcis2 [Sub]))
      | Quotient (e1, e2) ->
         let bcis1 = translate e1
         and bcis2 = translate e2
         in (List.append bcis1 (List.append bcis2 [Quo]))
      | Remainder (e1, e2) ->
         let bcis1 = translate e1
         and bcis2 = translate e2
         in (List.append bcis1 (List.append bcis2 [Rem]))
    in Target_program (translate e);;

  let () = assert (test_compile compile);;

  (* a *)
  (* Left to right *)
  (* b *)
  (* Potential eg of difference for 4b:
       compile (Source_program (Plus (Quotient (Literal 1, Literal 0), Remainder (Literal 1, Literal 0))));; 
       Can implement a compile_right_to_left function to illustrate point
   *)

  let compile_alt (Source_program e) =
    (* compile : source_program -> target_program *)
    let rec translate e =
      (* translate : arithmetic_expression -> byte_code_instruction list *)
      match e with
      | Literal n ->
         [Push n]
      | Plus (e1, e2) ->
         let bcis2 = translate e2
         and bcis1 = translate e1
         in (List.append bcis2 (List.append bcis1 [Add]))
      | Minus (e1, e2) ->
         let bcis2 = translate e2
         and bcis1 = translate e1
         in (List.append bcis2 (List.append bcis1 [Sub]))
      | Quotient (e1, e2) ->
         let bcis2 = translate e2
         and bcis1 = translate e1
         in (List.append bcis2 (List.append bcis1 [Quo]))
      | Remainder (e1, e2) ->
         let bcis2 = translate e2
         and bcis1 = translate e1
         in (List.append bcis2 (List.append bcis1 [Rem]))
    in Target_program (translate e);;
    
  (* ********** *)
  (* Task 6 *)
  let commutativity_test candidate_interpret candidate_compile candidate_run p =
   (* commutativity_test : (source_program -> expressible_value) ->
                           (source_program -> target_program) ->
                           (target_program -> expressible_value) ->
                           source_program ->
                           bool *)
    candidate_interpret p = candidate_run (candidate_compile p);;

  (* ********** *)

let generate_random_arithmetic_expression n_init =
   (* generate_random_arithmetic_expression : int -> arithmetic_expression *)
    if n_init < 0
    then raise (Failure "generate_random_arithmetic_expression")
    else let rec generate n =
           if n = 0
           then Literal (Random.int 100)
           else match Random.int 5 with
                | 0 ->
                   Literal ~-(Random.int 100)
                | 1 ->
                   Plus (generate (n - 1), generate (n - 1))
                | 2 ->
                   Minus (generate (n - 1), generate (n - 1))
                | 3 ->
                   Quotient (generate (n - 1), generate (n - 1))
                | _ ->
                   Remainder (generate (n - 1), generate (n - 1))
         in generate n_init;;
    
  let test_commutativity candidate_interpret candidate_compile candidate_run =
    (commutativity_test candidate_interpret candidate_compile candidate_run (Source_program (Minus (Literal 58, Literal 74))))
    &&
      (commutativity_test candidate_interpret candidate_compile candidate_run (Source_program (Plus (Literal 71, Literal 54))))
    &&
      (commutativity_test candidate_interpret candidate_compile candidate_run (Source_program (Quotient (Literal 23, Literal 55))))
    &&
      (commutativity_test candidate_interpret candidate_compile candidate_run (Source_program (Remainder (Literal 3, Literal 0))))
    &&
      (commutativity_test candidate_interpret candidate_compile candidate_run (Source_program (generate_random_arithmetic_expression 5)))
    &&
      (commutativity_test candidate_interpret candidate_compile candidate_run (Source_program (generate_random_arithmetic_expression 6)))
  (* etc *) ;;

  (*  let () = assert (test_commutativity interpret compile run);; *)
          
  let test1 = Source_program (Minus (Literal 58, Literal 74));;
    interpret test1;;
    run (compile (test1));;
  let test2 = Source_program (Plus (Literal 71, Literal 54));;
    interpret test2;;
    run (compile (test2));;
  let test3 = Source_program (Quotient (Literal 23, Literal 55));;
    interpret test3;;
    run (compile (test3));;
  let test4 = Source_program (Remainder (Literal 3, Literal 0));;
    interpret test4;;
    run (compile (test4));;

  let () = assert (test_commutativity interpret compile_alt run);;
  
  (* ********** *)

end;;

(* end of week-15_4-three-language-processors-for-arithmetic-expressions.ml *)

"week-15_4-three-language-processors-for-arithmetic-expressions.ml"

