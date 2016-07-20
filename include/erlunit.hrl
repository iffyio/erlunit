
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
                                    {reason, assert_failed},
                                    {expected, true},
                                    {value,V}]})
    end
  end)(BoolExpr)).
