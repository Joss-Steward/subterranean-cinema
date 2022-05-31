%%%-------------------------------------------------------------------
%% @doc theatre public API
%% @end
%%%-------------------------------------------------------------------

-module(theatre_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {
            <<"localhost">>,
            [
                {<<"/start/[:file]">>, theatre_start_handler, []},
                {<<"/join/[:stream]">>, theatre_client_handler, []},
                {
                    <<"/">>, cowboy_static, 
                    {priv_file, theatre, "static/index.html"}
                }
            ]
        }
    ]),
    {ok, _} = cowboy:start_clear(theatre_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    theatre_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(theatre_listener).

%% internal functions
