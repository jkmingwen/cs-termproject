(* week-15_3-sieve.ml *)
(* Introduction to Computer Science (YSC1212), Sem1, 2017-2018 *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
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

module type THIRD_MINI_PROJECT =
sig
  type int_stream

  val strike_out : int_stream -> int -> int_stream

  val partial_sums : int_stream -> int_stream

  val composite : int_stream -> int -> int_stream

  val sieve : int_stream -> int -> int_stream
end;;

module Third_mini_project : THIRD_MINI_PROJECT =
struct
  type int_stream =
    | Scons of int * (unit -> int_stream);;

  (* Defining delays *)
  let force thunk =
    thunk ();;

  let delay_value thunk =
    (* delay_value : (unit -> 'a) -> unit -> 'a *)
    let v = force thunk
    in fun () -> v;;

  let delay_name thunk =
    (* delay_name : (unit -> 'a) -> unit -> 'a *)
    thunk;;

  let delay_need thunk =
    (* delay_need : (unit -> 'a) -> unit -> 'a *)
    let memory_cell = ref None
    in fun () -> match !memory_cell with
                 | Some memorized_value ->
                    memorized_value
                 | None ->
                    let memorized_value = force thunk
                    in (memory_cell := Some memorized_value;
                        memorized_value);;
  (* Defining make functions to facilitate unit tests *)
  let make_stream delay seed next =
    (* make_stream : ((unit -> 'a stream) -> unit -> 'a stream) -> 'a -> ('a -> 'a) -> 'a stream *)
    let rec produce v =
      (* produce : 'a -> 'a stream *)
      Scons (v, delay (fun () -> produce (next v)))
    in produce seed;;

  let stream_prefix s_init n_init =
    if n_init < 0
    then raise (Failure "stream_prefix")
    else let rec consume s n =
           if n = 0
           then []
           else match s with
                | Scons (v, thunk) ->
                   v :: consume (force thunk) (n - 1)
         in consume s_init n_init;;

  (* Defining streams for unit-tests *)
  let nats_name = make_stream delay_name 0 (fun i -> i + 1);;
  let evens_name = make_stream delay_name 0 (fun i -> i + 2);;
  let odds_name = make_stream delay_name 1 (fun i -> i + 2);;
  let evens_need = make_stream delay_need 0 (fun i -> i + 2);;
  let odds_need = make_stream delay_need 1 (fun i -> i + 2);;
  let nats_need = make_stream delay_need 0 (fun i -> i + 1);;
  let pos_nats_name = make_stream delay_name 1 (fun i -> i + 1);;
  let alt_1_minus_1 = make_stream delay_need 1 (fun i -> i*(-1));;
  (* Question 1 *)
  (* Unit-tests for strike_out *)
let test_strike_out candidate =
  (* Given tests: *)
  ((stream_prefix (candidate nats_name 1) 5) = [0; 1; 3; 4; 6])
  &&
    ((stream_prefix (candidate nats_name 2) 7) = [0; 1; 2; 4; 5; 6; 8])
  &&
    ((stream_prefix (candidate pos_nats_name 1) 7) = [1; 2; 4; 5; 7; 8; 10])
  &&
    ((stream_prefix (candidate pos_nats_name 2) 7) = [1; 2; 3; 5; 6; 7; 9])
(* etc. *);;

  let strike_out s n =
    if n < 0
    then raise (Failure "strike_out")
    else let rec consume (Scons(v, thunk)) n' =
           if n' = (-1)
           then consume (force thunk) n
           else Scons(v,
                      delay_need (fun () ->
                          consume (force thunk) (n' - 1)))
         in consume s n;;

  let () = assert(test_strike_out strike_out);;
    
  (* Question 2 *)
  (* Unit-tests for partial_sums *)
let test_partial_sums candidate =
  (* Given tests: *)
  ((stream_prefix (candidate (nats_name)) 5) = [0; 1; 3; 6; 10])
  &&
  ((stream_prefix (candidate (nats_name)) 7) = [0; 1; 3; 6; 10; 15; 21])
  &&
  ((stream_prefix (candidate (odds_name)) 5) = [1; 4; 9; 16; 25])
  &&
  ((stream_prefix (candidate (pos_nats_name)) 7) = [1; 3; 6; 10; 15; 21; 28])
(* etc. *);;

  let partial_sums s =
    let rec walk (Scons(v, thunk)) a =
      let a = v + a
      in Scons (a,
                delay_need (fun () ->
                    walk (force thunk) a))
    in walk s 0;;
  (*  let () = assert(test_partial_sums partial sums);; *)
    
  (* Question 3 *)
  (* Unit-tests for composite *)
  let alt_1_minus_1 = make_stream delay_need 1 (fun i -> i*(-1));;
  let test_composite candidate =
    (* Same as Question 2: *)
    (stream_prefix (candidate pos_nats_name 0) 5 = [1; 4; 9; 16; 25])
  &&
    (stream_prefix (candidate alt_1_minus_1 0) 5 = [1; 2; 3; 4; 5])
  (* etc. *);;
    
  let composite s n =
    partial_sums (strike_out s n);;

  let () = assert(test_composite composite);;
    
  (* Question 4 *)
  (* Unit-tests for sieve *)
  let test_sieve candidate =
    (* Basic tests: (boring) *)
    (stream_prefix (candidate pos_nats_name 0) 5 = [1; 4; 9; 16; 25])
    &&
    (stream_prefix (candidate alt_1_minus_1 0) 5 = [1; 2; 3; 4; 5])
  (* etc. *);;

  let sieve s_init n_init =
    if n_init < 0
    then raise (Failure "sieve")
    else let rec walk s n =
           if n = 0
           then composite s n
           else walk (composite s n) (n-1)
         in walk s_init n_init
  ;;

  let () = assert(test_sieve sieve);;

  (* Defining initial stream with examples *)
  let initial_stream = make_stream delay_name 1 (fun i -> 0);;

  let test_hypothesis candidate_function =
    (stream_prefix (candidate_function initial_stream 0) 5
     = [1; 1; 1; 1; 1])
    &&
      (stream_prefix (candidate_function initial_stream 1) 6
       = [1; 2; 3; 4; 5; 6])
    &&
      (stream_prefix (candidate_function initial_stream 2) 6
       = [1; 4; 9; 16; 25; 36])
    &&
      (stream_prefix (candidate_function initial_stream 3) 5
       = [1; 8; 27; 64; 125])
    &&
      (stream_prefix (candidate_function initial_stream 4) 6
       = [1; 16; 81; 256; 625; 1296])
  (* etc. *);;

    let () = assert (test_hypothesis sieve);;

  (*

# stream_prefix (sieve initial_stream 0) 5;;
- : int list = [1; 1; 1; 1; 1]
# stream_prefix (sieve initial_stream 1) 5;;
- : int list = [1; 2; 3; 4; 5]
# stream_prefix (sieve initial_stream 2) 5;;
- : int list = [1; 4; 9; 16; 25]
# stream_prefix (sieve initial_stream 3) 5;;
- : int list = [1; 8; 27; 64; 125]
# stream_prefix (sieve initial_stream 4) 5;;
- : int list = [1; 16; 81; 256; 625]
                 
   *)

  (* Question 5 *)
  (* Defining initial stream with examples *)
    let alt_initial_stream = make_stream delay_name 10 (fun i -> 0);;

    let test_hypothesis_ten candidate_function =
      (stream_prefix (candidate_function initial_stream 0) 5
       = [10; 10; 10; 10; 10])
      &&
        (stream_prefix (candidate_function initial_stream 1) 6
         = [10; 20; 30; 40; 50; 60])
      &&
        (stream_prefix (candidate_function initial_stream 2) 6
         = [10; 40; 90; 160; 250; 360])
      &&
        (stream_prefix (candidate_function initial_stream 3) 5
         = [10; 80; 270; 640; 1250])
      &&
        (stream_prefix (candidate_function initial_stream 4) 6
         = [10; 160; 810; 2560; 6250; 12960])
    (* etc. *);;

    let () = assert (test_hypothesis sieve);;

(*
# stream_prefix (sieve alt_initial_stream 0) 5;;
- : int list = [10; 10; 10; 10; 10]
# stream_prefix (sieve alt_initial_stream 1) 5;;
- : int list = [10; 20; 30; 40; 50]
# stream_prefix (sieve alt_initial_stream 2) 5;;
- : int list = [10; 40; 90; 160; 250]
# stream_prefix (sieve alt_initial_stream 3) 5;;
- : int list = [10; 80; 270; 640; 1250]
# stream_prefix (sieve alt_initial_stream 4) 5;;
- : int list = [10; 160; 810; 2560; 6250]
 *)
    
end;;

(* ********** *)

(* end of week-15_3-sieve.ml *)

"week-15_3-sieve.ml"
