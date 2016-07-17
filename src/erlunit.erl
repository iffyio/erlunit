-module (erlunit).
-export ([run_tests/1]).

run_etests(Module) ->
  eunit:test(list_to_atom(hd(Module))).
run_tests(Module) ->
  test(list_to_atom(hd(Module))).

test(Module) ->
  Funcs = get_test_functions(Module),
  Start = erlang:now(),
  Result = lists:map(fun (F) -> run_test_function(Module, F) end, Funcs),
  listen(length(Result), 0, 0, Start).

get_test_functions(Module) ->
  Functions = Module:module_info(functions),
  TestFuncs = [ Name || {Name, _Arithy} <- Functions,is_test_function(Name)],
  TestFuncs.


print_result(Passed, Failed, Time) ->
  io:format("~p~nFinished in ~p seconds.~n",
            [string:copies("=", 60), Time]),
  io:format("Total: ~p, Passed: ~p, Failed: ~p~n",[Passed+Failed, Passed, Failed]).

print_failed_test_case({Reason, Details}) ->
  {M,F,L} = get_module_function_and_line(Details),
  {Ex,Val} = get_expected_and_value(Details),
  io:format("    **~p.erl: failed testcase**~n", [M]),
  io:format("~p in function ~p (line ~p)~n", [Reason,F,L]),
  io:format("<~p> expected but was <~p>~n",[Ex,Val]).

get_expected_and_value(Details) ->
  [_,_,_,{expected,Ex},{value,Val}|_] = Details,
  {Ex,Val}.

get_module_function_and_line(Details) ->
    [{module, M},{function, F}, {line, L} | _] = Details,
    %uhm ever heard of regex? TODO!
    Nameonly = hd(string:tokens(hd(string:tokens(atom_to_list(F), "-")), "/")),
    {M, Nameonly, L}.

listen(0, Passed, Failed, StartTime) ->
  Now = erlang:now(),
  Diff = timer:now_diff(Now,StartTime)/1000000,
  print_result(Passed, Failed, Diff);

listen(N, Passed, Failed, StartTime) ->
  receive 
    {'DOWN', _, _, _, Reason} -> 
      case Reason of
        normal ->
            listen(N-1, Passed+1, Failed, StartTime);
        _ -> 
            print_failed_test_case(Reason),
            listen(N-1, Passed, Failed+1, StartTime)
      end
  end.

run_test_function(Module, Function) ->
  erlang:monitor(process, spawn(Module, Function, [])).

is_test_function(FuncName) ->
  case re:run(atom_to_list(FuncName), ".*_test$") of
    nomatch -> false;
    _ -> true
  end.

