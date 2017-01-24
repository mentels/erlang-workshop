-module(abacus_tcp).

-export([start/1, conn_cnt/0]).
-export([init/1]).

%% API

start(Port) ->
    Pid = spawn(?MODULE, init, [Port]),
    register(?MODULE, Pid).

conn_cnt() ->
    ?MODULE ! {abacus_req, self(), conn_cnt},
    receive
        {abacus_req, Reply} ->
            Reply
    end.

%% Internal Functions

init(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, true},
                                               {reuseaddr, true}]),
    accept_loop(ListenSocket, 0).

accept_loop(ListenSocket, ConnNum0) ->
    ConnNum1 = case gen_tcp:accept(ListenSocket, 500) of
                   {ok, Socket} ->
                       spawn_client(Socket),
                       ConnNum0 + 1;
                   {error, timeout} ->
                       ConnNum0
               end,
    ConnNum2 = receive
                   {abacus_req, From, conn_cnt} ->
                       From ! {abacus_req, {ok, ConnNum1}},
                       ConnNum1;
                   {'DOWN', _Ref, process, _Pid, _Reason} ->
                       ConnNum1 - 1
               after 500 ->
                       ConnNum1
               end,
    accept_loop(ListenSocket, ConnNum2).

spawn_client(Socket) ->
    {Pid, _Ref} = spawn_monitor(fun() -> client_loop(Socket) end),
    ok = gen_tcp:controlling_process(Socket, Pid).

client_loop(Socket) ->
    case wait_for_request(Socket) of
        {error, socket_closed} = Err ->
            Err;
        {ok, Request} ->
            ParsedRequest = (catch parse_request(Request)),
            Response = process_request(ParsedRequest),
            gen_tcp:send(Socket, Response),
            client_loop(Socket)
    end.

wait_for_request(Socket) ->
    receive
        {tcp, Socket, Request} ->
            {ok, Request};
        {tcp_closed, Socket} ->
            {error, socket_closed}
    end.

parse_request(Request) ->
    [Arg1, Op, Arg2 | _] = string:tokens(Request, " \n\r"),
    {parse_int(Arg1), parse_op(Op), parse_int(Arg2)}.

parse_int(Int) ->
    list_to_integer(Int).

parse_op([$+]) -> addition;
parse_op([$-]) -> subtraction;
parse_op([$*]) -> multiplication;
parse_op([$/]) -> division.

process_request({'EXIT', Error}) ->
    io_lib:format("~p~n",[{error, Error}]);
process_request({Arg1, Op, Arg2}) ->
    Result = abacus:Op(Arg1, Arg2),
    io_lib:format("~p~n", [Result]).
