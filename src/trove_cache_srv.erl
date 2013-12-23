-module(trove_cache_srv).

-behaviour(gen_server).

-include("trove.hrl").
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {
    cache_name, 
    tid, 
    max_keys, 
    eviction_fun, 
    eviction_type,
    lookup_fun, 
    key_count = 0, 
    hits = 0, 
    misses = orddict:new()
    }).

start_link(CacheName, Options) ->
    gen_server:start_link({local, CacheName}, ?MODULE, {CacheName, Options}, []).

init({CacheName, Options}) ->
    EvictionFun = proplists:get_value(eviction_fun, Options, {trove_eviction, timeout, []}),
    EvictionType = proplists:get_value(eviction_type, Options, per_key),
    LookupFun = proplists:get_value(lookup_fun, Options, {trove, default_lookup, []}),
    MaxKeys = proplists:get_value(max_keys, Options, infinity),

    set_timer(EvictionType),
     
    %%% XXX FIXME
    %%% spawn this into its own process
    Tid = ets:new(CacheName, [named_table, {keypos, 2}]),
    
    {ok, #state{
        cache_name = CacheName,
        tid = Tid,
        max_keys = MaxKeys,
        eviction_fun = EvictionFun,
        eviction_type = EvictionType,
        lookup_fun = LookupFun }}.

%%% Lookup
handle_call({lookup, Key}, _From, State = #state{ tid = Tid } ) ->
    {Response, NewState} = case ets:lookup(Tid, Key) of
        [] ->
            handle_miss(Key, State);
        [R] ->
            handle_hit(R, State)
    end,
    {reply, Response, NewState};

%%% Insertion
handle_call({insert, Key, Value}, _From, State = #state{ tid = Tid, key_count = K, max_keys = Max } ) when Max =:= infinity orelse K =< Max ->
    insert(Tid, Key, Value),
    {reply, ok, State#state{ key_count = ets:info(Tid, size) } };

handle_call({insert, List}, _From, State = #state{ tid = Tid, key_count = K, max_keys = Max } ) 
                        when is_list(List) andalso Max =:= infinity orelse K + length(List) < Max ->
    true = ets:insert(Tid, lists:map( fun format_kv/1, List)),
    {reply, ok, State#state{ key_count = ets:info(Tid, size) } };

%%% Eviction
handle_call({evict, Key}, _From, State = #state{ tid = Tid }) ->
    true = ets:delete(Tid, Key),
    {reply, ok, State#state{ key_count = ets:info(Tid, size) } };
handle_call(evict, _From, State = #state{ tid = Tid, eviction_fun = F, eviction_type = T }) ->
    R = evict(Tid, F, T),
    {reply, R, State#state{ key_count = ets:info(Tid, size) } };

%%% Info
handle_call({info, hits}, _From, State = #state{ hits = Hits }) ->
    {reply, Hits, State};

handle_call({info, misses}, _From, State = #state{ misses = Misses }) ->
    {reply, orddict:size(Misses), State};

handle_call({info, lookup_times}, _From, State = #state{ misses = Misses }) ->
    {reply, orddict:to_list(Misses), State};

handle_call({info, key_count}, _From, State = #state{ key_count = K, max_keys = Max}) ->
    {reply, [{key_count, K}, {max_keys, Max}], State};

handle_call({info, all}, _From, State = #state{ hits = Hits, misses = Misses, key_count = K, max_keys = Max }) ->
    {reply, [{hits, Hits}, {misses, orddict:size(Misses)}, {lookup_times, orddict:to_list(Misses)}, {key_count, K}, {max_keys, Max}], State};
    
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(evict, State = #state{ tid = Tid, eviction_fun = F, eviction_type = T}) ->
    evict(Tid, F, T),
    {noreply, State#state{ key_count = ets:info(Tid, size) } };
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
format_kv({Key, Value}) ->    
    #trove_entry{ key = Key, value = Value, inserted_at = epoch_time() }.

epoch_time() ->
    {Mega, S, _Micro} = os:timestamp(),
    (Mega * 1000000) + S.

%%% XXX FIXME: This needs to respect max key size
handle_miss(Key, State = #state{ tid = Tid, lookup_fun = {M, F, A}, misses = Misses }) ->
    Start = os:timestamp(),
    Value = M:F(A ++ [Key]),
    insert(Tid, Key, Value),
    End = os:timestamp(),
    LookupTime = timer:now_diff(End, Start) div 1000,
    %?LOG(debug, "lookup time ~w ms", [LookupTime]),
    {Value, State#state{ misses = orddict:store(Key, LookupTime, Misses), key_count = ets:info(Tid, size)}}.

handle_hit(R = #trove_entry{ key = Key, value = Value }, State = #state{ eviction_type = per_key, 
        lookup_fun = {Mod, Func, Arg}, eviction_fun = {M, F, A}, tid = Tid, hits = Hits }) ->
    
    case apply(M, F, A ++ [R]) of
        false ->
            ets:update_element(Tid, Key, {#trove_entry.last_lookup_at, epoch_time()}),
            {Value, State#state{ hits = Hits + 1 }};
        true ->
            V = apply(Mod, Func, Arg ++ [Key]),
            insert(Tid, Key, V),
            {V, State#state{ key_count = ets:info(Tid, size) }}
    end;
handle_hit(#trove_entry{ key = Key, value = Value }, State = #state{ tid = Tid, hits = Hits }) ->
    ets:update_element(Tid, Key, {#trove_entry.last_lookup_at, epoch_time()}),
    {Value, State#state{ hits = Hits + 1 }}.

get_env(Setting, Default) ->
    case application:get_env(trove, Setting) of
        undefined ->
            Default;
        V -> V
    end.

set_timer(per_table) ->
    Timeout = get_env(eviction_timeout, 900),
    erlang:send_after(Timeout * 1000, self(), evict);
set_timer(_) -> ok.

insert(Tid, Key, Value) ->
    true = ets:insert(Tid, format_kv({Key, Value})).

evict(Tid, {M, F, A}, T) ->
    [ ets:delete(Tid, Entry#trove_entry.key ) || Entry <- lists:filter(
        fun(R) -> apply(M, F, A ++ [R]) end, ets:tab2list(Tid) ) ],
    set_timer(T),
    ok.

%%% Unit tests
-ifdef(TEST).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

format_kv_test_() ->
    Key = <<"key">>,
    Value = 42,
    [
        ?_assertMatch(#trove_entry{ key = Key, value = Value, inserted_at = _ }, format_kv({Key, Value}))
    ].



-endif.
