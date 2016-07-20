
-define (current_function(), (fun () -> 
    {_, {_,CF,_}} = process_info(self(), current_function),
    CF
  end)()).

-undef (report).
-undef (boolAssert).
-undef (equalAssert).
-undef (assert).
-undef (assertNot).
-undef (assertEqual).
-undef (assertNotEqual).

-define (report(Expected_, Value_, Reason_),
          {assertion_failed, [{module,?MODULE},
                              {function, ?current_function()},
                              {line, ?LINE},
                              {reason, Reason_},
                              {expected, Expected_},
                              {value,Value_}]}).

-define (boolAssert(BoolExpr_, Expected_, Reason_),
          (fun (BoolExpr, Expected, Reason) ->
            case BoolExpr of
              Expected -> ok;
              V -> exit(?report(Expected, V, Reason))
            end
          end)(BoolExpr_, Expected_, Reason_)).

-define (assert(BoolExpr_),
  ?boolAssert(BoolExpr_, true, assert_failed)).


-define (assertNot(BoolExpr_),
  ?boolAssert(BoolExpr_, false, assertNot_failed)).


-define (equalAssert(Expected_, Value_, Ans_, Reason_),
  (fun (Expected, Value, Ans) ->
    case (Expected == Value) of
      Ans -> ok;
      _ -> exit(?report(Expected, Value, Reason_))
    end
  end)(Expected_, Value_, Ans_)).


-define (assertEqual(Expected_, Value_),
    ?equalAssert(Expected_, Value_,
                true, assertEqual_failed)).


-define (assertNotEqual(Expected_, Value_),
    ?equalAssert(Expected_, Value_,
                false, assertNotEqual_failed)).