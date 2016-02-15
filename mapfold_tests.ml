open Mapfold ;;
  
let test () =
  assert ((negate_all []) = []);
  assert ((negate_all [1; -2; 0]) = [-1; 2; 0]);

  assert ((sums []) = 0);
  assert ((sums [3; 8; -12]) = -1);

  assert ((sum_rows [[1; 2]; [3; 4]; [5; -4]]) = [3; 7; 1]);
  assert ((sum_rows [[1; 2; 4]; []; [8; -12]]) = [7; 0; -1]);

  assert ((filter_odd [1; 4; 5; -3]) = [1; 5; -3]);
  assert ((filter_odd [2; 4; 8]) = []);

  assert ((num_occurs 4 [1; 3; 4; 5; 4]) = 2);
  assert ((num_occurs 2 [1; 4; 5; 3; 7]) = 0);

  assert ((super_sum [[1; 2; 4]; [3; 5]; []; [6]]) = 21);
  assert ((super_sum [[4; -2]; [0]; [7; -8; 7]]) = 8);

  assert ((filter_range [1; 3; 4; 5; 7; 0] (3, 5)) = [3; 4; 5]);
  assert ((filter_range [8; 2; 7; 1] (1, 2)) = [2; 1]);

  assert ((floats_of_ints [3; 5; 4; 2]) = [3.; 5.; 4.; 2.]);
  assert ((floats_of_ints [1; 9; 193; 0]) = [1.; 9.; 193.; 0.]);

  assert ((log10s [1.; 100.; 0.; -10.]) = 
    [Some 0.; Some 2.; None; None]);
  assert ((log10s [1.; 1000.]) = [Some 0.; Some 3.]);

;;

test();;
print_endline "All tests passed.";;
