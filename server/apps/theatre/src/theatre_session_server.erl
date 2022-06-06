-module(theatre_session_server).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).
-export([add_child/1, get_child/1, delete_child/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
	ets:new(?MODULE, [set, named_table]),
	{ok, []}.

handle_call({get, Session}, _, State) ->
	Result = case ets:lookup(?MODULE, Session) of
		[{Session, Pid}] -> {ok, Pid};
		[] -> not_found
	end,
	{reply, Result, State};
handle_call({add, Session}, _, State) ->
    {ok, Pid} = theatre_session_sup:start_child(),
	ets:insert(?MODULE, {Session, Pid}),
	{reply, Pid, State};
handle_call({delete, Session}, _, State) ->
	{ok, Pid} = get_child(Session),
    ok = theatre_session_sup:delete_child(Pid),
	ets:delete(?MODULE, Session),
	{reply, Pid, State}.

handle_cast(_, State) ->
	{noreply, State}.

add_child(Session) ->
	gen_server:call(?MODULE, {add, Session}).

get_child(Session) ->
	gen_server:call(?MODULE, {get, Session}).

delete_child(Session) ->
	gen_server:call(?MODULE, {delete, Session}).