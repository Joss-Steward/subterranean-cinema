%%%-------------------------------------------------------------------
%% @doc theatre top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cinema_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
			id => snowflake, 
			start => { snowflake, start_link, [] }, 
			restart => permanent
		},
        #{
            id => session_manager,
            start => { session_manager, start_link, [] },
            restart => permanent
        },
        #{
			id => session_sup, 
			start => { session_sup, start_link, [] }, 
			restart => permanent
		},
        #{
			id => media_manager, 
			start => { media_manager, start_link, [] }, 
			restart => permanent
		}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
