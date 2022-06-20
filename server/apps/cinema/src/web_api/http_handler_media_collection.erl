-module(http_handler_media_collection).
-behaviour(cowboy_handler).

-include("../media/media_records.hrl").

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

from_json(Req0, State) ->
	{ok, Body, Req1} = cowboy_req:read_body(Req0, #{length => 100000, period => 5000}),
	BodyMap = jsx:decode(Body),
	Stream = maps:get(<<"stream_url">>, BodyMap),
	Title = maps:get(<<"title">>, BodyMap),
	Runtime = maps:get(<<"runtime">>, BodyMap),
	{ok, Id} = media_manager:add({Title, Runtime, Stream}),
	BasePath = cowboy_req:path(Req0),
	ResourceURL = [BasePath, <<"/">>, Id],
    {{created, ResourceURL}, Req1, State}.

to_json(Req, State) ->
	{ok, MediaListRecords} = media_manager:get(all),
	MediaList = [#{
		id => Item#media_item.id,
		runtime => Item#media_item.runtime,
		stream_url => Item#media_item.stream,
		title => Item#media_item.title
	} || Item <- MediaListRecords ],
	Body = jsx:encode(MediaList),
    {Body, Req, State}.