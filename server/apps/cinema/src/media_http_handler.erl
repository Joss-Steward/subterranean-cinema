-module(media_http_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(#{method := <<"GET">>} = Request, State) ->
	Media = media_manager:get(all),
	Body = jsx:encode(media_list_to_json(Media)),
	Response = good_request(Body, Request),
    {ok, Response, State}.

good_request(Response, Request) ->
	cowboy_req:reply(200, response_headers(), Response, Request).

response_headers() -> #{
		<<"content-type">> => <<"application/json">>
	}.

media_list_to_json(MediaList) ->
	[ #{
		id => MediaId,
		runtime => Runtime,
		stream_url => StreamURL,
		title => Title
	} || {MediaId, 
		#{runtime := Runtime, 
			stream_url := StreamURL, 
			title := Title}
		} <- MediaList ].