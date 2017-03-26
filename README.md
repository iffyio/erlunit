# erlunit
An elementary unit testing framework for erlang

## Create module under test
Create file **calculator.erl** with the following content.

```erlang
-module (calculator).
-compile(export_all).

sum(Exp) ->
  Nums = [N || {N, _} <- [string:to_integer(E) || E <- string:tokens(Exp, "+")]],
  lists:sum(Nums).
```

## Create test functions (optional in a new module)
Use the -include directive to import assert macros from erlunit.hrl.  
Test functions are 0-arity functions with a **_test** suffix  
Create file **calculator_test.erl**

```erlang
-module (calculator_test).
-compile(export_all).
-include ("erlunit.hrl").

sum_test() ->
  Sum = calculator:sum("1+2+3"),
  % ?assert(6 == Sum),
  % ?assertNotEqual(5, Sum),
  % ?assertNot(5 == Sum),
  ?assertEqual(6, Sum).
```

## Run the test
Compile classes  (and erlunit module if not previously compiled)
```erlang
c(calculator).
c(calculator_test).
c(erlunit).
```
The erlunit:test/1 function runs all functions/0 with names ending with **_test** in the specified module.  

```erlang
erlunit:test(calculator_test).
```
Output 
```
===========================================================  
Finished in 2.0e-5 seconds.  
1 tests, 1 passed, 0 failures, 0 errors  
```
## Test that fails
Replace assert in **calculator_test** to fail the test  

```erlang
?assertEqual(5, Sum).
```
Recompile **calculator_test** and now the test fails with
```
    **calculator_test.erl: failed testcase**  
assertEqual failed in function "sum_test" (line 10)  
<6> expected but was <5>  
```

