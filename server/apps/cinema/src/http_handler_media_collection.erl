-module(http_handler_media_collection).
-behaviour(cowboy_handler).

%% REST Callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2, content_types_accepted/2]).

%% Callback Callbacks
-export([from_json/2, to_json/2]).

init(Req, _) ->
    {cowboy_rest, Req, #{}}.

allowed_methods(Req, State) ->  
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, to_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, from_json}
	], Req, State}.

from_json(Req, State) ->
    case middleware:auth(Req) of
        {true, User, Req1} ->
            Fname = maps:get(fname, User),
            Lname = maps:get(lname, User),
            Message = [hello,  <<"Good day, ", Fname/binary, " ", Lname/binary>>];
        {false, Req1} ->
            Message = [hello, <<"Good day">>]
    end,
    {jiffy:encode(Message), Req1, State}.

to_json(Req, State) ->
	Media = media_manager:get(all),
	Body = jsx:encode(media_list_to_json(Media)),
    {Body, Req, State}.

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