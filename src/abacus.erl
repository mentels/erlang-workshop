-module(abacus).

%% abacus: abacus library's entry point.

-export([addition/2,
         subtraction/2,
         multiplication/2,
         division/2]).

%% API

addition(X, Y) ->
    X+Y.

subtraction(X, Y) ->
    X-Y.

multiplication(X, Y) ->
    X*Y.

division(X, Y) ->
    X div Y.

%% Internals
