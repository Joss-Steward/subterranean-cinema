-module(theatre_session_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([start_child/0, delete_child/1]).

-define(SHUTDOWN_TIMEOUT, 5000).
-define(WORKER(I), {I, {I, start_link, []}, temporary, ?SHUTDOWN_TIMEOUT, worker, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Child = ?WORKER(theatre_session_statem),
    {ok, {{simple_one_for_one, 3, 30}, [Child]}}.

start_child() ->
    supervisor:start_child(?MODULE, []).

delete_child(Pid) ->
    ok = supervisor:terminate_child(?MODULE, Pid).