-module (erlunit).
-export ([run_tests/1]).

run_etests(Module) ->
  eunit:test(list_to_atom(hd(Module))).
run_tests(Module) ->
  test(list_to_atom(hd(Module))).


test(Module) ->
  Funcs = get_test_functions(Module),
  Start = erlang:now(),
  Procs = lists:map(fun (F) -> run_test_case(Module, F) end, Funcs),
  Result = [{passed,0},{failed,0},{errors,0},{start_time,Start}],
  listen(length(Procs), Result).


% return all test functions in given module
get_test_functions(Module) ->
  Functions = Module:module_info(functions),
  TestFuncs = [ Name || {Name, _Arithy} <- Functions,is_test_function(Name)],
  TestFuncs.


% return true if function ends with _test
is_test_function(FuncName) ->
  case re:run(atom_to_list(FuncName), ".*_test$") of
    nomatch -> false;
    _ -> true
  end.


% run a test case in a process.
% monitor that process
run_test_case(Module, Function) ->
  erlang:monitor(process, spawn(Module, Function, [])).


% listen for N processes to each finish running test case
listen(0, Result) ->
  log_result(Result);

listen(N, Result) ->
  receive 
    {'DOWN', _, _, _, Reason} ->
      case Reason of
        normal ->
            listen(N-1, incr(passed, Result));
        {erlunit_error, Error} ->
            log_failed_test_case(Error),
            listen(N-1, incr(failed, Result));
        Error ->
            log_error(Error),
            listen(N-1, incr(errors, Result))
      end
  end.


% log final test results for module
log_result(Result) ->
  {_,Passed} = get_value(passed, Result),
  {_,Failed} = get_value(failed, Result),
  {_,Errors} = get_value(errors, Result),
  Time = get_elapsed_time(Result),
  io:format("~p~nFinished in ~p seconds.~n",
            [string:copies("=", 60), Time]),
  io:format("~p tests, ~p assertions, ~p failures, ~p errors~n",
            [Passed+Failed, Passed, Failed, Errors]).


% log details of a single failed test case
log_failed_test_case({Reason, Assertion}) ->
  {M,F,L} = get_module_function_and_line(Assertion),
  {Ex,Val} = get_expected_and_value(Assertion),
  io:format("    **~p.erl: failed testcase**~n", [M]),
  io:format("~p in function ~p (line ~p)~n", [Reason,F,L]),
  io:format("<~p> expected but was <~p>~n",[Ex,Val]).


% return expected and actual values for given assertion
get_expected_and_value(Assertion) ->
  [_,_,_,{expected,Ex},{value,Val}|_] = Assertion,
  {Ex,Val}.


% return module name, function and line number of assertion
get_module_function_and_line(Assertion) ->
    [{module, M},{function, F}, {line, L} | _] = Assertion,
    [_,Nameonly|_] = re:split(atom_to_list(F),
                              "^-(.+)/.+$",


% log details of unexpected error thrown by test case
log_error(Error) ->
  io:format("~p~n",[Error]).
                              [{return,list}]),
    {M, Nameonly, L}.


% return time difference from start
get_elapsed_time(Result) ->
  {_,StartTime} = get_value(start_time, Result),
  Now = erlang:now(),
  timer:now_diff(Now,StartTime)/1000000.


% key find at position
get_value(Key, Result) ->
  lists:keyfind(Key, 1, Result).


% increment int in tuple {Key, Value}
% return new list containing {key, value+1}
incr(Key, Result) ->
  {_,Val} = get_value(Key, Result),
  lists:keyreplace(Key, 1, Result, {Key,Val+1}).
