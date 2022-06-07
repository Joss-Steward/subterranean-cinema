-module(viewer_websocket_handler).
-behaviour(cowboy_handler).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).
-export([notify/2]).

init(Request, State) ->
    case cowboy_req:binding(id, Request) of
		undefined -> 
    		{ok, bad_request(<<"400 Session ID is required">>, Request), State};
		SessionId -> 
			case session_manager:get(SessionId) of
				not_found -> 
    				{ok, bad_request(<<"400 Session ID is invalid">>, Request), State};
				{ok, {_, SessionInfo}} ->
					Pid = maps:get(pid, SessionInfo),
					{cowboy_websocket, Request, #{ session_pid => Pid }}
			end
	end.

bad_request(Reason, Request) ->
	cowboy_req:reply(401, response_headers(), Reason, Request).

response_headers() -> #{
		<<"content-type">> => <<"text/plain">>
	}.

% Connect to session here because init() is 
%  called from a different process than websocket_init
websocket_init(State) ->
	io:format("websocket_init_state ~p~n", [State]),
	SessionPid = maps:get(session_pid, State),
	session:join(SessionPid, {?MODULE, notify, self()}),
    {[{text, <<"Joined Session~n">>}], State}.

websocket_handle(Frame = {text, _}, State) ->
	io:fwrite("ws message received: ~p~n", Frame),
    {[Frame], State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({notify, Message}, State) ->
    {[{text, Message}], State};
websocket_info(_Info, State) ->
	io:fwrite("unexpected message: ~p~n", _Info),
    {[], State}.

notify(Pid, Message) ->
	Pid ! {notify, Message}.