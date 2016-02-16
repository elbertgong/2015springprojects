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

  assert (derivative (Num 4.) = Num 0.);
  assert (derivative (Var) = Num 1.);
  assert (derivative (Unop(Sin, Var)) = Binop(Mul,Unop(Cos,Var),Num 1.));
  assert (derivative (Unop(Cos, Var)) = Binop(Mul,Unop(Neg,Unop(Sin,Var)),Num 1.));
  assert (derivative (Unop(Ln, Var)) = Binop(Div, Num 1., Var));
  assert (derivative (Unop(Neg, Var)) = Unop(Neg, Num 1.));
  assert (derivative (Binop(Add, Var, Num 4.)) = Binop(Add,Num 1.,Num 0.));
  assert (derivative (Binop(Sub, Var, Num 4.)) = Binop(Sub,Num 1.,Num 0.));
  assert (derivative (Binop(Mul, Var, Num 4.)) = Binop(Add,Binop(Mul,Var,Num 0.),
    Binop(Mul,Num 1.,Num 4.)));
  assert (derivative (Binop(Div, Var, Num 4.)) = 
    Binop(Div, Binop(Sub, Binop(Mul, Num 1., Num 4.), 
    Binop(Mul, Var, Num 0.)), Binop(Pow, Num 4., Num 2.)));
  assert (derivative (Binop(Pow, Var, Num 4.)) = 
    Binop(Mul, Binop(Mul, Num 4., Num 1.), 
    Binop(Pow, Var, Binop(Sub, Num 4., Num 1.))));
  assert (derivative (Binop(Pow, Num 4., Var)) =Binop(Mul, Binop(Pow, Num 4., Var), 
    Binop(Add, Binop(Mul, Num 1., Unop(Ln, Num 4.)),
    Binop(Div, Binop(Mul, Num 0., Var), Num 4.))));

  assert ((find_zero (parse "x^2-6*x-16") 7. 0.01 5) > Some 7.99 &&
    (find_zero (parse "x^2-6*x-16") 7. 0.01 5) < Some 8.01);
  assert ((find_zero (parse "x^2-6*x-16") 6. 0.001 2) = None);
;;


test();;
print_endline "All tests passed.";;
