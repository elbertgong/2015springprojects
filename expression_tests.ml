open Expression ;;
open Ast ;;
open ExpressionLibrary ;;

let test () =
  assert (contains_var (parse "x+3"));
  assert (not (contains_var (parse "2")));
  assert (not (contains_var (parse "4-3")));
  assert (contains_var (parse "3^2+~sin(cos(x))"));
  assert (not (contains_var (parse "ln(4/(8^2)")));
  assert (contains_var (parse "4^(4*~x"));

  assert (evaluate (parse "x^4 + 3") 2.0 = 19.0);
  assert ((evaluate (parse "x+3") 8.4) = 11.4);
  assert ((evaluate (parse "2") 3) = 2.);
  assert (evaluate (parse "3^2+~sin(cos(x))") 3.14159) > 9.84 &&
    (evaluate (parse "3^2+~sin(cos(x))") 3.14159) < 9.85 );
  assert ((evaluate (parse "ln(4/(8^(2/x)))") 10.) > .97 &&
    (evaluate (parse "ln(4/(8^(2/x)))") 10.) < .98);
  assert ((evaluate (parse "4^(4*~x") .2) > .32 &&
    (evaluate (parse "4^(4*~x") .2) < .33);
    
;;

test();;
print_endline "All tests passed.";;
