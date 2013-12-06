-module(trove_eviction).

-include("trove.hrl").

-export([timeout/1, purge_used_once/1, least_recently_used/1]).

timeout(#trove_entry{ inserted_at = InsertTime }) ->
    Timeout = get_env(eviction_timeout, 900),
    epoch_time() > InsertTime + Timeout.

purge_used_once(#trove_entry{ last_lookup_at = undefined }) -> true;
purge_used_once(_) -> false.

least_recently_used(#trove_entry{ last_lookup_at = T }) ->
    Timeout = get_env(eviction_timeout, 900),
    epoch_time() > T + Timeout.

%%% Private
get_env(Setting, Default) ->
    case application:get_env(trove, Setting) of
        undefined ->
            Default;
        V -> V
    end.

epoch_time() ->
    {Mega, S, _Micro} = os:timestamp(),
    (Mega * 1000000) + S.
 

