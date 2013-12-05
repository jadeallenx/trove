-module(trove).

-export([
    start/0, 
    new/2, 
    insert/3, 
    lookup/2, 
    evict/1, 
    evict/2, 
    info/1, 
    info/2,
    default_lookup/1
    ]).

-spec start/0 :: () -> ok.
start() ->
    lager:start(),
    application:start(trove).

-spec default_lookup/1 :: (
    Key :: term() ) -> pos_integer().
default_lookup(_Key) ->
    random:seed(now()),
    random:uniform(1000).

-spec insert/3 :: (
    CacheName :: atom(),
    Key :: term(),
    Value :: term() ) -> ok.
insert(CacheName, Key, Value) ->
    gen_server:call(CacheName, {insert, Key, Value}).

-spec lookup/2 :: (
    CacheName :: atom(),
    Key :: term() ) -> Value :: term().
lookup(CacheName, Key) ->
    gen_server:call(CacheName, {lookup, Key}).

-spec evict/1 :: (
    CacheName :: atom() ) -> ok.
evict(CacheName) ->
    gen_server:call(CacheName, evict).

-spec evict/2 :: (
    CacheName :: atom(),
    Key :: term() ) -> ok.
evict(CacheName, Key) ->
    gen_server:call(CacheName, {evict, Key}).

-spec info/2 :: (
    CacheName :: atom(),
    Type :: hits | misses | lookup_times | key_count | all ) -> integer() | proplists:proplist().
info(CacheName, Type) ->
    gen_server:call(CacheName, {info, Type}).

-spec info/1 :: (
    CacheName :: atom() ) -> proplists:proplist().
info(CacheName) ->
    info(CacheName, all).

-spec new/2 :: (
    CacheName :: atom(),
    Options :: proplists:proplist() ) -> {ok, Pid :: pid()}.
new(CacheName, Options) ->
    trove_sup:add_cache(CacheName, Options).
