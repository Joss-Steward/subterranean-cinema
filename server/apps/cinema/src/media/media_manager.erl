-module(media_manager).
-behaviour(gen_server).

-include("media_records.hrl").
-include_lib("kernel/include/logger.hrl").

%% gen_server exports
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

%% public API exports
-export([add/1, get/1, delete/1]).

-define(TABLE_NAME, ?MODULE).
-define(SERVER_NAME, ?MODULE).

%% public API %%

add({Title, Runtime, Stream}) ->
	gen_server:call(?SERVER_NAME, {add, {Title, Runtime, Stream}}).

get(all) ->
	gen_server:call(?SERVER_NAME, {get, all});
get({id, MediaID}) ->
	gen_server:call(?SERVER_NAME, {get, {id, MediaID}}).

delete({id, MediaID}) ->
	gen_server:cast(?SERVER_NAME, {delete, {id, MediaID}}).

%% gen_server callbacks %%

start_link() ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [], []).

init(State) ->
	install([node()]),
	{ok, State}.

install(Nodes) ->
	case mnesia:create_schema(Nodes) of
		{error, {Node, {already_exists, Node}}} ->
			?LOG_INFO(#{message => "mnesia schema already exists on node", node => Node}),
			ok;
		ok -> ok
	end,
	rpc:multicall(Nodes, application, start, [mnesia]),
	case mnesia:create_table(media_item, 
		[{attributes, record_info(fields, media_item)},
		 {disc_copies, Nodes}]) of
			{atomic, ok} -> 
				?LOG_INFO(#{message => "mnesia table created successfully"}),
				ok;
			{aborted, {already_exists, _}} -> 
				?LOG_INFO(#{message => <<"media mnesia table already exists">>}),
				ok;
			{aborted, Reason} -> 
				?LOG_ERROR(#{message => <<"could not create media mnesia table">>, reason => Reason})
		end.

handle_call({add, MediaData}, _, State) ->
	{reply, add_media(MediaData), State};
handle_call({get, Req}, _, State) ->
	Info = get_media(Req),
	{reply, Info, State};
handle_call(Call, Pid, State) ->
	?LOG_ERROR(#{message => "invalid call received", call => Call, from => Pid}),
	{noreply, State}.

handle_cast({delete, MediaID}, State) ->
	delete_media(MediaID),
	{noreply, State};
handle_cast(_, State) ->
	{noreply, State}.

%% interal API %%

get_media(all) ->
	GetAll = fun() ->
		MatchHead = #media_item{id='$1', title='$2', runtime='$3', stream='$4'},
		Return =['$_'],
		mnesia:select(media_item, [{MatchHead, [], Return}], read)
	end,
	{atomic, MediaList} = mnesia:transaction(GetAll),
	{ok, MediaList};
get_media({id, MediaID}) ->
	GetAll = fun() ->
		MatchItem = #media_item{id=MediaID, title='_', runtime='_', stream='_'},
		mnesia:match_object(MatchItem)
	end,
	{atomic, MediaList} = mnesia:transaction(GetAll),
	{ok, MediaList}.

add_media({Title, Runtime, Stream}) ->
	{ok, Id} = snowflake:new(),
	MediaRecord = #media_item{
		id = Id,
		title = Title,
		runtime = Runtime,
		stream = Stream
	},
	Insert = fun() ->
		mnesia:write(MediaRecord)
	end,
	{atomic, ok} = mnesia:transaction(Insert),
	{ok, Id}.

delete_media({id, MediaID}) ->
	GetAll = fun() ->
		mnesia:delete({media_item, MediaID})
	end,
	{atomic, _} = mnesia:transaction(GetAll),
	ok.