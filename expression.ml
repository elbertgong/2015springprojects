open Ast ;;
open ExpressionLibrary ;;

(* TIPS FOR PROBLEM 2:
 * 1. Read the writeup.
 * 2. Use the type definitions in the ast.ml as a reference. But don't worry
 *    about expressionLibrary.ml
 *)

(*>* Problem 2.1 *>*)

(* contains_var : tests whether an expression contains a variable "x"
 *     Examples : contains_var (parse "x^4") = true
 *                contains_var (parse "4+3") = false *)
let rec contains_var (e:expression) : bool =
    match e with
    | Var -> true
    | Binop (a, b, c) -> contains_var b || contains_var c
    | Unop (a, b) -> contains_var b
    | Num a -> false
;;


(*>* Problem 2.2 *>*)

(* evaluate : evaluates an expression for a particular value of x. Don't
 *            worry about handling 'divide by zero' errors.
 *  Example : evaluate (parse "x^4 + 3") 2.0 = 19.0 *)
let rec evaluate (e:expression) (x:float) : float =
    match e with
    | Var -> x
    | Num f -> f
    | Unop (u, e1) ->
      (match u with
      | Sin -> sin (evaluate e1)
      | Cos -> cos (evaluate e1)
      | Ln -> log (evaluate e1)
      | Neg -> -(evaluate e1))
    | Binop (b, e1, e2) ->
      match b with
      | Add -> (evaluate e1) +. (evaluate e2)
      | Sub -> (evaluate e1) -. (evaluate e2)
      | Mul -> (evaluate e1) *. (evaluate e2)
      | Div -> (evaluate e1) /. (evaluate e2)
      | Pow -> (evaluate e1) ** (evaluate e2)
 ;;


(*>* Problem 2.3 *>*)

(* See writeup for instructions. *)
let rec derivative (e:expression) : expression =
    match e with
    | Num _ -> Num 0.
    | Var -> 1.
    | Unop (u,e1) ->
        (match u with
        | Sin -> Binop(Mul,Unop(Cos, e1),derivative e1)
        | Cos -> Binop(Mul,Unop(Neg,Unop(Sin,e1)),derivative e1)
        | Ln -> Binop(Div, derivative e1, e1)
        | Neg -> Unop(Neg,derivative e1))
    | Binop (b,e1,e2) ->
        match b with
        | Add -> Binop(Add,derivative e1,derivative e2)
        | Sub -> Binop(Sub,derivative e1,derivative e2)
        | Mul -> Binop(Add,Binop(Mul,e1,derivative e2),
                        Binop(Mul,derivative e1,e2))
        | Div -> Binop(Div, Binop(Sub, Binop(Mul, derivative e1, e2), 
            Binop(Mul, e1, derivative e2)), Binop(Pow, e2, 2.))
        | Pow ->
            if contains_var e2
            then Binop(Mul, Binop(Pow, e1, e2), 
              Binop(Sum, Binop(Mul, derivative e2, Unop(Ln, e1)),
              Binop(Div, Binop(Mul, derivative e1, e2), e1)))
            else Binop(Mul, Binop(Mul, e2, derivative e1), 
                Binop(Pow, e1, Binop(Sub, h, 1.)))
;;

(* A helpful function for testing. See the writeup. *)
let checkexp strs xval =
    print_string ("Checking expression: " ^ strs ^ "\n");
    let parsed = parse strs in (
        print_string "contains variable : ";
        print_string (string_of_bool (contains_var parsed));
        print_endline " ";
        print_string "Result of evaluation: ";
        print_float (evaluate parsed xval);
        print_endline " ";
        print_string "Result of derivative: ";
        print_endline " ";
        print_string (to_string (derivative parsed));
        print_endline " "
    )
;;


(*>* Problem 2.4 *>*)

(* See writeup for instructions. *)
let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int)
    : float option =
    if lim = 0 then None else
    if abs (evaluate e g) <= epsilon then Some g else
    find_zero e (g - (evaluate e g /. evaluate (derivate e) g) epsilon (lim - 1)
;;


(*>* Problem 2.5 *>*)

(* Extra Credit:
 * Just leave it unimplemented if you don't want to do it.
 * See writeup for instructions. *)
let rec find_zero_exact (e:expression) : expression option =
    failwith "Not implemented"
;;


(*>* Problem 2.6 *>*)

let minutes_spent_on_part_2 () : int = failwith "Not implemented";;