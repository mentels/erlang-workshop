-module(abacus_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Records
%% ------------------------------------------------------------------

-record(state, {port, lsocket, socket}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Port]) ->
    process_flag(trap_exit, true),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, true},
                                               {reuseaddr, true}]),
    ok = gen_server:cast(?SERVER, accept),
    {ok, #state{port = Port, lsocket = ListenSocket}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unimplemented}, State}.

handle_cast(accept, #state{lsocket = LSocket} = State) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    {noreply, State#state{socket = Socket}};
handle_cast(_Request, State) ->
    {reply, State}.

handle_info({tcp, Socket, Request}, #state{socket = Socket} = State) ->
    handle_request(Socket, Request),
    {noreply, State};
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    ok = gen_server:cast(?SERVER, accept),
    {noreply, State#state{socket = undefined}}.

terminate(_Reason, #state{socket = Socket}) when is_port(Socket) ->
    gen_tcp:close(Socket);
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

handle_request(Socket, Request) ->
    ParsedRequest = (catch abacus_req:parse_request(Request)),
    Response = abacus_req:process_request(ParsedRequest),
    gen_tcp:send(Socket, Response).
