-module(trove_cache_svr).

-behaviour(gen_server).

-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(trove_entry, { key, value, inserted_at, last_lookup_at }).
-record(state, {cache_name, tid, max_keys, eviction_fun, lookup_fun, key_count = 0, hits = 0, misses = 0}).

start_link(CacheName, Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [CacheName, Options], []).

init([CacheName, Options]) ->
    EvictionFun = proplists:get_value(eviction_fun, Options, {trove_eviction, timeout, []}),
    LookupFun = proplists:get_value(lookup_fun, Options, {trove, default_lookup, []}),
    MaxKeys = proplists:get_value(max_keys, Options, infinity),
    %%% XXX FIXME
    %%% spawn this into its own process
    Tid = ets:new(CacheName, [named_table, {keypos, 2}]),
    
    {ok, #state{
        cache_name = CacheName,
        tid = Tid,
        max_keys = MaxKeys,
        eviction_fun = EvictionFun,
        lookup_fun = LookupFun }}.

handle_call({lookup, Key}, _From, State = #state{ tid = Tid, hits = Hits } ) ->
    {Response, NewState} = case ets:lookup(Tid, Key) of
        [] ->
            handle_miss(Key, State);
        [R] ->
            % XXX - handle_hit(R, State)
            ets:update_element(Tid, Key, {#trove_entry.last_lookup_at, epoch_time()}),
            {R#trove_entry.value, State#state{ hits = Hits + 1 }}
    end,
    {reply, Response, NewState};

handle_call({insert, Key, Value}, _From, State = #state{ tid = Tid, key_count = K, max_keys = Max } ) when Max =:= infinity orelse K =< Max ->
    true = ets:insert(Tid, format_kv({Key, Value})),
    {reply, ok, State#state{ key_count = K + 1 } };
handle_call({insert, List}, _From, State = #state{ tid = Tid, key_count = K, max_keys = Max } ) 
                        when is_list(List) andalso Max =:= infinity orelse K + length(List) < Max ->
    true = ets:insert(Tid, lists:map( fun format_kv/1, List)),
    {reply, ok, State#state{ key_count = K + length(List)} };
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

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

handle_miss(Key, State = #state{ lookup_fun = {M, F, A}, misses = Misses }) ->
    Start = os:timestamp(),
    Value = M:F(A ++ [Key]),
    gen_server:call(self(), {insert, Key, Value}),
    End = os:timestamp(),
    LookupTime = timer:now_diff(End, Start) div 1000,
    io:format("DEBUG: lookup time ~w ms", [LookupTime]),
    {Value, State#state{ misses = Misses + 1 }}.

