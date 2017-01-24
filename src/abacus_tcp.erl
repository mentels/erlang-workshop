-module(abacus_tcp).

-behaviour(gen_server).

-record(state, {listen_socket,
                conn_cnt}).

-export([start_link/1,
         conn_cnt/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

%% API

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

conn_cnt() ->
    gen_server:call(?MODULE, {abacus_req, conn_cnt}).

%% Callbacks

init(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, true},
                                               {reuseaddr, true}]),
    gen_server:cast(?MODULE, {abacus_req, accept}),
    {ok, #state{listen_socket = ListenSocket, conn_cnt = 0}}.

handle_call({abacus_req, conn_cnt}, _From,
            #state{conn_cnt = Cnt} = State) ->
    {reply, {ok, Cnt}, State};
handle_call(_Req, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast({abacus_req, accept}, #state{conn_cnt = Cnt} = State) ->
    case gen_tcp:accept(State#state.listen_socket, 500) of
        {ok, Socket} ->
            spawn_client(Socket),
            gen_server:cast(?MODULE, {abacus_req, accept}),
            {noreply, State#state{conn_cnt = Cnt + 1}};
        {error, timeout} ->
            gen_server:cast(?MODULE, {abacus_req, accept}),
            {noreply, State}
    end;
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, _Pid, _Reason},
            #state{conn_cnt = Cnt} = State) ->
    {noreply, State#state{conn_cnt = Cnt - 1}};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Internal Functions

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
