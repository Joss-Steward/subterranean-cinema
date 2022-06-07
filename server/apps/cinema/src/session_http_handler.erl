-module(session_http_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(#{method := <<"GET">>} = Request, State) ->
    Response = case cowboy_req:binding(id, Request) of
		undefined -> get_session(Request);
		SessionId -> get_session(SessionId, Request)
	end,
    {ok, Response, State};
init(#{method := <<"POST">>} = Request, State) ->
	#{media_id := MediaID} = cowboy_req:match_qs([{media_id, nonempty}], Request),
	Response = case session_manager:create(MediaID) of
		{error, nomedia} ->
			bad_request(<<"404 Media Not Found">>, Request);
		{ok, SessionId} -> 
			Body = jsx:encode(#{<<"session-id">> => SessionId}),
			good_request(Body, Request)
	end,
    {ok, Response, State}.

get_session(SessionId, Request) ->
	case session_manager:get(SessionId) of
		not_found -> 
			bad_request(<<"404 Session Not Found">>, Request);
		Session ->
			Body = jsx:encode(#{<<"session">> => Session}),
			good_request(Body, Request)
	end.

get_session(Request) ->
	Sessions = session_manager:get(),
	List = [ #{
		id => SessionId,
		runtime => Runtime,
		stream_url => StreamURL,
		title => Title
	} || {SessionId, 
		#{
			runtime := Runtime, 
			stream_url := StreamURL, 
			title := Title}
		} <- Sessions ],
	cowboy_req:reply(200, response_headers(), jsx:encode(List), Request).

good_request(Response, Request) ->
	cowboy_req:reply(200, response_headers(), Response, Request).

bad_request(Reason, Request) ->
	cowboy_req:reply(400, response_headers(), Reason, Request).

response_headers() -> #{
		<<"content-type">> => <<"text/plain">>
	}.