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
               ?assert(is_integer(trove:default_lookup(foo)))
       end}
      ]}.

