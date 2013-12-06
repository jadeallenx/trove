-module(trove).

-include("trove.hrl").

-export([
    start/0, 
    new/1,
    new/2, 
    insert/2,
    insert/3, 
    lookup/2, 
    evict/1, 
    evict/2, 
    destroy/1,
    info/1, 
    info/2,
    default_lookup/1
    ]).

-spec start/0 :: () -> ok.
% @doc Start the application
start() ->
    lager:start(),
    application:start(trove).

-spec default_lookup/1 :: (
    Key :: term() ) -> pos_integer().
% @doc The default way to lookup a value for a key
%
% @see new/2
default_lookup(Key) ->
    ?LOG(debug, "~p~n", [Key]),
    random:seed(now()),
    random:uniform(1000).

-spec insert/3 :: (
    CacheName :: atom(),
    Key :: term(),
    Value :: term() ) -> ok.
% @doc Insert a key and a value into a cache
%
% Key and value may be any arbitrary Erlang term. The cache must already available before insertions.
insert(CacheName, Key, Value) ->
    gen_server:call(CacheName, {insert, Key, Value}).

-spec insert/2 :: (
    CacheName :: atom(),
    List :: proplists:proplist() ) -> ok.
% @doc Insert a list of `{Key, Value}' tuples into a cache
insert(CacheName, List) when is_list(List) ->
    gen_server:call(CacheName, {insert, List}).

-spec lookup/2 :: (
    CacheName :: atom(),
    Key :: term() ) -> Value :: term().
% @doc Get the current value for a key from a cache.
%
% If the key is not found or has expired, it will be retrieved 
% using the `lookup_fun' given when the cache was started.
lookup(CacheName, Key) ->
    gen_server:call(CacheName, {lookup, Key}).

-spec evict/1 :: (
    CacheName :: atom() ) -> ok.
% @doc Apply an `eviction_fun' against a cache's current keys and values.
evict(CacheName) ->
    gen_server:call(CacheName, evict).

-spec evict/2 :: (
    CacheName :: atom(),
    Key :: term() ) -> ok.
% @doc Evict the given key from the cache.
evict(CacheName, Key) ->
    gen_server:call(CacheName, {evict, Key}).

-spec destroy/1 :: (
    CacheName :: atom() ) -> ok.
% @doc Destroy the entire cache.
%
% Also terminates its process in the Erlang VM.
destroy(CacheName) ->
    trove_sup:destroy(CacheName).

-spec info/2 :: (
    CacheName :: atom(),
    Type :: hits | misses | lookup_times | key_count | all ) -> integer() | proplists:proplist().
% @doc Get information about the state of the cache.
%
% Available items are:
% <ul>
%   <li>hits</li>
%   <li>misses</li>
%   <li>lookup_times (as a property list of `{Key, LookupTime}' in milliseconds)</li>
%   <li>key_count (as a property list)</li>
%   <li>all (as a property list)</li>
% </ul>
info(CacheName, Type) ->
    gen_server:call(CacheName, {info, Type}).

-spec info/1 :: (
    CacheName :: atom() ) -> proplists:proplist().
% @doc Get all information about a cache
info(CacheName) ->
    info(CacheName, all).

-spec new/2 :: (
    CacheName :: atom(),
    Options :: proplists:proplist() ) -> {ok, Pid :: pid()}.
% @doc Start a new cache
%
% Available options: 
% <ul>
%   <li>`max_keys' - a positive integer or `infinity'</li>
%   <li>`eviction_fun' - a {M, F, A} which accepts a `#trove_entry' record and returns a bool</li>
%   <li>`eviction_type' - `per_key' or `per_table'</li>
%   <li>`lookup_fun' - a {M, F, A} that takes a key and returns the value associated with it.</li>
% </ul>
%
% per\_key eviction is checked every time a key is accessed
% per\_table eviction runs off a timer (every 15 minutes by default)
% See trove\_eviction.erl for some "out of the box" eviction functions
%
% See below for the default values.
new(CacheName, Options) ->
    trove_sup:add_cache(CacheName, Options).

-spec new/1 :: (
    CacheName :: atom() ) -> {ok, Pid :: pid()}.
% @doc Start a new cache with default options.
%
% Equivalent to 
% <code>
%     trove:new(CacheName, 
%       [{max_keys, infinity},
%        {eviction_fun, {trove_eviction, timeout, []}},
%        {eviction_type, per_key},
%        {lookup_fun, {trove, default_lookup, []}]).
% </code>
new(CacheName) ->
    new(CacheName, []).
