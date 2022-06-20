-module(http_handler_media_item).
-behaviour(cowboy_handler).

-include("../media/media_records.hrl").

%% REST Callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2, content_types_accepted/2]).
-export([resource_exists/2, delete_resource/2]).

%% Resource item callbacks
-export([media_item_to_json/2]).

init(Req, _) ->
    {cowboy_rest, Req, #{}}.

allowed_methods(Req, State) ->  
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, media_item_to_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
	{[], Req, State}.

resource_exists(Req, State0) ->
	MediaID = cowboy_req:binding(id, Req),
	case media_manager:get({id, MediaID}) of
		{ok, [Item | []]} ->
			State1 = maps:put(media_item, Item, State0),
    		{true, Req, State1};
		{ok, []} ->
			{false, Req, State0}
	end.

delete_resource(Req, State) ->
	Item = maps:get(media_item, State),
	media_manager:delete({id, Item#media_item.id}),
	{true, Req, State}.

media_item_to_json(Req, State) ->
	Item = maps:get(media_item, State),
	Response = #{
		id => Item#media_item.id,
		runtime => Item#media_item.runtime,
		stream_url => Item#media_item.stream,
		title => Item#media_item.title
	},
	Body = jsx:encode(Response),
    {Body, Req, State}.