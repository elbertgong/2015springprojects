open Expression ;;
open Ast ;;
open ExpressionLibrary ;;

let test () =
  assert (contains_var (parse "x+3"));
  assert (not (contains_var (parse "2")));
  assert (not (contains_var (parse "4-3")));;
  assert (contains_var (parse "3^2+~sin(cos(x))"));
  assert (not (contains_var (parse "ln(4/(8^2))")));
  assert (contains_var (parse "4^(4*~x)"));

  assert ((evaluate (parse "x^4 + 3") 2.0) = 19.0);
  assert ((evaluate (parse "x-3") 8.4) = 5.4);
  assert ((evaluate (parse "2") 3.) = 2.);
  assert ((evaluate (parse "3^2+~sin(cos(x))") 3.14159) > 9.84 &&
    (evaluate (parse "3^2+~sin(cos(x))") 3.14159) < 9.85 );
  assert ((evaluate (parse "ln(4/(8^(2/x)))") 10.) > 0.97 &&
    (evaluate (parse "ln(4/(8^(2/x)))") 10.) < 0.98);
  assert ((evaluate (parse "4^(4*~x)") 0.2) > 0.32 &&
    (evaluate (parse "4^(4*~x)") 0.2) < 0.33);;

  assert ((to_string_smart (derivative (parse "x^4 + 3"))) = "4.*x^3.");
  assert ((to_string_smart (derivative (parse "sin(x) + cos(2*x)"))) = 
    "cos(x)+~sin(2.*x)*2.");
  assert ((to_string_smart (derivative (parse "ln(x - 8)"))) = "1./x");
  assert ((to_string_smart (derivative (parse "-100"))) = "0.");
  assert ((to_string_smart (derivative (parse "3^x"))) = "3.^x*(1*ln(3)+0.*3./3.)");

  assert ((find_zero (parse "x^2-6*x-16") 7. 0.01 5) > Some 7.99 &&
    (find_zero (parse "x^2-6*x-16") 7. 0.01 5) < Some 8.01);

;;


test();;
print_endline "All tests passed.";;
