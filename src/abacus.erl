-module(abacus).

%% abacus: abacus library's entry point.

-export([addition/2,
         subtraction/2,
         multiplication/2,
         division/2]).

%% API

%% @doc Adds two integers
%%
%% This adds `X' to `Y'.
-spec addition(integer(), integer()) -> integer().
addition(X, Y) ->
    X+Y.

%% @doc Subtracts two integers
%%
%% This subtracts `Y' from `X'.
-spec subtraction(integer(), integer()) -> integer().
subtraction(X, Y) ->
    X-Y.

%% @doc Multiplies two integers
%%
%% This multiplies `X' `Y' times.
-spec multiplication(integer(), integer()) -> integer().
multiplication(X, Y) ->
    X*Y.

%% @doc Divides two integers
%%
%% This multiplies divides `X' by `Y'.
-spec division(integer(), integer()) -> integer().
division(X, Y) ->
    X div Y.

%% Internals
