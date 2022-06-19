-module(session_manager).
-behaviour(gen_server).

%% public API
-export([create/1, get/0, get/1, delete/1]).

%% gen_server exports
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(TABLE_NAME, ?MODULE).
-define(SERVER_NAME, ?MODULE).

%% public API

% Create a media session
create(MediaID) ->
	gen_server:call(?SERVER_NAME, {create, MediaID}).

% Get all current sessions
get() ->
	gen_server:call(?SERVER_NAME, {get}).

% Get a specific session
get(Session) ->
	gen_server:call(?SERVER_NAME, {get, Session}).

% Delete a session
delete(Session) ->
	gen_server:call(?SERVER_NAME, {delete, Session}).

%% gen_server callbacks

start_link() ->
	Args = [],
	Opts = [],
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, Args, Opts).

init(_) ->
	Table = ets:new(?TABLE_NAME, [set, named_table]),
	{ok, #{table => Table}}.

handle_call({get}, _, State) ->
	Sessions = get_sessions(),
	{reply, Sessions, State};
handle_call({get, Session}, _, State) ->
	Info = get_session(Session),
	{reply, Info, State};
handle_call({create, MediaID}, _, State) ->
	case media_manager:get(MediaID) of
		{ok, MediaInfo} ->
			SessionId = create_session_id(MediaID),
			{ok, Pid} = session_sup:start_child(SessionId, MediaInfo),
			erlang:monitor(process, Pid),
			Info = #{
				pid => Pid,
				media => MediaInfo
				},
			add_session(SessionId, Info),
			{reply, {ok, SessionId}, State};
		none ->
			{reply, {error, nomedia}, State}
	end.

handle_cast({delete, Session}, State) ->
	{ok, Pid} = get_session(Session),
	remove_session(Session),
    ok = session_sup:delete_child(Pid),
	{noreply, State};
handle_cast(_, State) ->
	{noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _}, State) ->
	erlang:demonitor(Ref),
	ets:match_delete(?TABLE_NAME, {'_', #{pid => Pid}}),
	{noreply, State};
handle_info(_, State) ->
	{noreply, State}.

%% internal API

get_sessions() ->
	ets:match_object(?TABLE_NAME, {'$0', '$1'}).

get_session(Session) ->
	case ets:lookup(?TABLE_NAME, Session) of
		[{Session, Info}] -> {ok, {Session, Info}};
		[] -> not_found
	end.
	
% TODO handle duplicate sessions
add_session(Session, Info) ->
	ets:insert(?TABLE_NAME, {Session, Info}).

% TODO handle non-existing sessions
remove_session(Session) ->
	ets:delete(?TABLE_NAME, Session).

create_session_id(MediaID) ->
	SessionId = erlang:phash2({MediaID, erlang:monotonic_time()}),
	SessionIdHex = integer_to_list(SessionId, 16),
	list_to_binary(SessionIdHex).
