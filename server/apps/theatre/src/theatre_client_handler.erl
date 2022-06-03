-module(theatre_client_handler).
-behaviour(cowboy_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Request, State) ->
    {cowboy_websocket, Request, State}.

websocket_init(State) ->
    {[{text, <<"Hello!">>}], State}.

websocket_handle(Frame = {text, _}, State) ->
    {[Frame], State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({log, Text}, State) ->
    {[{text, Text}], State};
websocket_info(_Info, State) ->
    {[
        {text, "Hello"},
        {text, <<"world!">>},
        {binary, <<0:8000>>}
    ], State}.