-module(trove).

-export([start/0, default_lookup/1]).

start() ->
    lager:start(),
    ok.

default_lookup(_Key) ->
    random:seed(now()),
    random:uniform(1000).


