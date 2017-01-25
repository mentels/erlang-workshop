-module(abacus_app).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    abacus_sup:start_link().

stop(_) ->
    ok.
