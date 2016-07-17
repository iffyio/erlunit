
-define (current_function(), (fun () -> 
    {_, {_,CF,_}} = process_info(self(), current_function),
    CF
  end)()).

-undef (assert).
-define (assert(BoolExpr), 
  (fun (BoolExpr) ->
    case BoolExpr of
      true -> ok;
      V -> exit({assertion_failed, [{module,?MODULE},
                                    {function, ?current_function()},
                                    {line, ?LINE},
                                    {expected, true},
                                    {value,V}]})
    end
  end)(BoolExpr)).

-define (asserts(BoolExpr), 
  (fun (BoolExpr) ->
    %io:format("current function is ~p~n", [?current_function()]),
    Function = ?current_function(),
    case BoolExpr of
      true -> ok;
      V -> {Function,{assertion_failed, [{module,?MODULE},
             {line, ?LINE},
             {expected, true},
             {value,V}]}}
    end
  end)(BoolExpr)).