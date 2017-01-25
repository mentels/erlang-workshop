-module(abacus_sup).

-export([start_link/0]).
-export([init/1]).

-define(DEFAULT_PORT, 1234).

%% API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Callbacks

init(_) ->
    Port = application:get_env(abacus, port, ?DEFAULT_PORT),
    Flags = #{strategy => one_for_one,
              intensity => 5,
              period => 1},
    Child = #{id => abacus_tcp,
              start => {abacus_tcp, start_link, [Port]},
              restart => permanent,
              shutdown => 5000,
              type => worker,
              modules => [abacus_tcp]
             },
    {ok, {Flags, [Child]}}.


