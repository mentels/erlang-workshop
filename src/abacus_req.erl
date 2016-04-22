-module(abacus_req).
-export([parse_request/1,
         process_request/1]).

%% API
parse_request(Request) ->
    [Arg1, Op, Arg2 | _] = string:tokens(Request, " \n\r"),
    {parse_int(Arg1), parse_op(Op), parse_int(Arg2)}.

process_request({'EXIT', Error}) ->
    io_lib:format("~p~n",[{error, Error}]);
process_request({Arg1, Op, Arg2}) ->
    Result = abacus:Op(Arg1, Arg2),
    io_lib:format("~p~n", [Result]).

%% Internal functions
parse_int(Int) ->
    list_to_integer(Int).

parse_op([$+]) -> addition;
parse_op([$-]) -> subtraction;
parse_op([$*]) -> multiplication;
parse_op([$/]) -> division.
