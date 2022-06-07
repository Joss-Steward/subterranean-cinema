-module(media_manager).
-behaviour(gen_server).

%% gen_server exports
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

%% public API exports
-export([add/1, get/1, delete/1]).

-define(TABLE_NAME, ?MODULE).
-define(SERVER_NAME, ?MODULE).

%% public API

add(Metadata) ->
	gen_server:call(?SERVER_NAME, {add, Metadata}).

get(MediaID) ->
	gen_server:call(?SERVER_NAME, {get, MediaID}).

delete(MediaID) ->
	gen_server:cast(?SERVER_NAME, {delete, MediaID}).

%% gen_server callbacks

start_link() ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [], []).

init(_) ->
	Table = ets:new(?TABLE_NAME, [set, named_table]),
	ets:insert(?TABLE_NAME, {<<"111A">>, #{
		title => <<"A March Wedding">>,
		runtime => 65000,
		stream_url => <<"/media/march.mp4">>
	}}),
	ets:insert(?TABLE_NAME, {<<"111B">>, #{
		title => <<"Fantastic Four">>,
		runtime => 48751,
		stream_url => <<"/media/ballard.mp4">>
	}}),
	{ok, #{table => Table}}.

handle_call({add, Metadata}, _, State) ->
	MediaID = create_media_id(Metadata),
	add_media(MediaID, Metadata),
	{reply, {ok, MediaID}, State};
handle_call({get, MediaID}, _, State) ->
	Info = get_media(MediaID),
	{reply, Info, State}.

handle_cast({delete, MediaID}, State) ->
	delete_media(MediaID),
	{noreply, State};
handle_cast(_, State) ->
	{noreply, State}.

%% interal API

get_media(all) ->
	ets:match_object(?TABLE_NAME, {'$0', '$1'});
get_media(MediaID) ->
	case ets:lookup(?TABLE_NAME, MediaID) of
		[{MediaID, Info}] -> {ok, Info};
		[] -> none
	end.

add_media(MediaID, #{title := Title, runtime := Runtime, stream_url := StreamURL}) ->
	Metadata = #{
		title => Title,
		runtime => Runtime,
		stream_url => StreamURL
	},
	ets:insert(?TABLE_NAME, {MediaID, Metadata}).

delete_media(MediaID) ->
	ets:delete(?TABLE_NAME, MediaID).

create_media_id(Metadata) ->
	MediaID = erlang:phash2({Metadata, erlang:monotonic_time()}),
	MediaIdHex = integer_to_list(MediaID, 16),
	list_to_binary(MediaIdHex).