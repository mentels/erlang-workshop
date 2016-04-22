-module(abacus_tcp).

-export([start/1]).
-export([init/1]).

%% API

start(Port) ->
    Pid = spawn(?MODULE, init, [Port]),
    register(?MODULE, Pid).

%% Internal Functions

init(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, true},
                                               {reuseaddr, true}]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    loop(Socket).

loop(Socket) ->
    case wait_for_request(Socket) of
        {error, socket_closed} = Err ->
            Err;
        {ok, Request} ->
            ParsedRequest = (catch abacus_req:parse_request(Request)),
            Response = abacus_req:process_request(ParsedRequest),
            gen_tcp:send(Socket, Response),
            loop(Socket)
    end.

wait_for_request(Socket) ->
    receive
        {tcp, Socket, Request} ->
            {ok, Request};
        {tcp_closed, Socket} ->
            {error, socket_closed}
    end.
