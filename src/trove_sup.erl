-module(trove_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, add_cache/2]).

%% OTP callback
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 100, []}}}.

child_spec_srv(CacheName, Options) when is_atom(CacheName) ->
    {CacheName,
        {trove_cache, start_link, [CacheName, Options]},
        permanent,
        5000,
        worker,
        [trove_cache]}.

add_cache(CacheName, Options) ->
    supervisor:start_child(?MODULE, child_spec_srv(CacheName, Options)).

