-module(trove_tests).

-include_lib("eunit/include/eunit.hrl").

trove_test_() ->
    {setup,
     fun() ->
             ok
     end,
     fun(_) ->
             ok
     end,
     [
      {"trove is alive",
       fun() ->
               %% format is always: expected, actual
               ?assertEqual(howdy, trove:hello())
       end}
      ]}.

