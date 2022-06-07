-module(session_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([start_child/2, delete_child/1]).

-define(SHUTDOWN_TIMEOUT, 5000).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Child = {session, {session, start_link, []}, 
				temporary, ?SHUTDOWN_TIMEOUT, worker, [session]},
    {ok, {{simple_one_for_one, 3, 30}, [Child]}}.

start_child(SessionID, MediaInfo) ->
    supervisor:start_child(?MODULE, [{SessionID, MediaInfo}]).

delete_child(Pid) ->
    ok = supervisor:terminate_child(?MODULE, Pid).