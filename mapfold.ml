(* CS51 Problem Set 2 *)

(****************************************************)
(******       1.1: Sparking your INTerest      ******)
(****************************************************)

(* Solve each problem in this part using List.map, List.fold_right, or
 * List.filter.
 *
 * A solution, even a working one, that does not use one of these
 * higher-order functions, will receive little or no credit.
 * However, if you can express your solution to
 * one particular part in terms of another function from
 * another part, you may do so.
 *
 * You MAY NOT change the definition of these
 * functions to make them recursive. 
 *)


(*>* Problem 1.1.a *>*)

(*  negate_all : Flips the sign of each element in a list *)
let negate_all (nums:int list) : int list =
    List.map (fun x -> -x) nums
;;


(*>* Problem 1.1.b *>*)

(*  sum : Returns the sum of the elements in the list. *)
let sum (nums:int list) : int =
    List.fold_left ( + ) 0 nums
;;


(*>* Problem 1.1.c *>*)

(*  sum_rows : Takes a list of "rows", each an int list.
 *             Returns a one-dimensional list of ints, each int equal to the
 *             sum of the corresponding row in the input.
 *   Example : sum_rows [[1;2]; [3;4]] = [3; 7] *)
let sum_rows (rows:int list list) : int list =
    List.map sum rows
;;


(*>* Problem 1.1.d *>*)

(*  filter_odd : Retains only the odd numbers from the given list.
 *     Example : filter_odd [1;4;5;-3] = [1;5;-3]. *)
let filter_odd (nums:int list) : int list =
    List.filter (fun x -> if x mod 2 = 0 then false else true) nums
;;


(*>* Problem 1.1.e *>*)

(*  num_occurs : Returns the number of times a given number appears in a list.
 *     Example : num_occurs 4 [1;3;4;5;4] = 2 *)
let num_occurs (n:int) (nums:int list) : int =
    List.fold_left (fun a h -> if h = n then a + 1 else a) 0 nums
;;


(*>* Problem 1.1.f *>*)

(*  super_sum : Sums all of the numbers in a list of int lists
 *    Example : super_sum [[1;2;3];[];[5]] = 11 *)
let super_sum (nlists:int list list) : int =
    List.fold_left ( + ) 0 (sum_rows nlists)
;;

(*>* Problem 1.1.g *>*)

(*  filter_range : Returns a list of numbers in the input list within a
 *                 given range (inclusive), in the same order they appeared
 *                 in the input list.
 *       Example : filter_range [1;3;4;5;2] (1,3) = [1;3;2] *)
let filter_range (nums:int list) (range:int * int) : int list =
    match range with
    | (a, b) -> 
    List.fold_left (fun i h -> if h >= a && h <= b then i@[h] else i ) [] nums
;;



(****************************************************)
(**********       1.2 Fun with Types       **********)
(****************************************************)


(*>* Problem 1.2.a *>*)

(*  floats_of_ints : Converts an int list into a list of floats *)
let floats_of_ints (nums:int list) : float list =
    List.map float_of_int nums
;;


(*>* Problem 1.2.b *>*)

(*   log10s : Applies the log10 function to all members of a list of floats.
 *            The mathematical function log10 is not defined for
 *            numbers n <= 0, so undefined results should be None.
 *  Example : log10s [1.0; 10.0; -10.0] = [Some 0.; Some 1.; None] *)
let log10s (lst: float list) : float option list =
    let logoper (x: float) : float option =
    match x > 0. with
    | true -> Some (log10 x)
    | false -> None
in
List.map logoper lst
;;


(*>* Problem 1.2.c *>*)

(*  deoptionalize : Extracts values from a list of options.
 *        Example : deoptionalize [Some 3; None; Some 5; Some 10] = [3;5;10] *)
let deoptoper (x: 'a option) : 'a list =
    match x with
    | Some a -> [a]
    | None -> []

let deoptionalize (lst:'a option list) : 'a list =
List.fold_right (fun c d -> (deoptoper c) @ d) lst []
;;


(*>* Problem 1.2.d *>*)

(*  some_sum : Sums all of the numbers in a list of int options;
 *             ignores None values *)
let some_sum (nums:int option list) : int =
    let somesumopter (x: int option) : int =
    match x with
    | Some a -> a
    | None -> 0
in
List.fold_left (fun b c -> b + (somesumopter c)) 0 nums
;;


(*>* Problem 1.2.e *>*)

(*  mult_odds : Product of all of the odd members of a list.
 *    Example : mult_odds [1;3;0;2;-5] = -15 *)
let mult_odds (nums:int list) : int =
    List.fold_left (fun a b -> if b mod 2 = 0 then a else a * b) 1 nums
;;


(*>* Problem 1.2.f *>*)

(*  concat : Concatenates a list of lists. See the Ocaml library ref *)
let concat (lists:'a list list) : 'a list =
    List.fold_left (fun a b -> a @ b) [] lists
;;


(*>* Problem 1.2.g *>*)

(* the student's name and year *)
type name = string
type year = int
type student = name * year

(*  filter_by_year : returns the names of the students in a given year
 *         Example : let students = [("Joe",2010);("Bob",2010);("Tom",2013)];;
 *                   filter_by_year students 2010 => ["Joe";"Bob"] *)
let filter_by_year (slist:student list) (yr:year) : name list =
  let filteredlist : student list = 
  List.filter (fun x -> 
  match x with (_, b) -> if b = yr then true else false) slist
in
  List.map (fun x -> match x with (a, _) -> a) filteredlist
;;


(*>* Problem 1.3 *>*)

(* Please give us an honest estimate of how long this Part of the problem
 * set took you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)
let minutes_spent_on_part_1 () : int = 120;;
