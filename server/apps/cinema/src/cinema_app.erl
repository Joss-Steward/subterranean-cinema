%%%-------------------------------------------------------------------
%% @doc cinema application public API
%% @end
%%%-------------------------------------------------------------------

-module(cinema_app).

-behaviour(application).

-export([start/2, stop/1]).

% Application Entry Point

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {
            <<"127.0.0.1">>,
            [
				{<<"/api/session/[:id]">>, http_handler_session_collection, []},
				{<<"/api/media/:id">>, http_handler_media_item, []},
				{<<"/api/media">>, http_handler_media_collection, []},
                {<<"/join/[:id]">>, viewer_websocket_handler, []},
                {
                    <<"/">>, cowboy_static, {priv_file, theatre, "static/index.html"}
                },
                {
                    <<"/stream/[...]">>, cowboy_static, {dir, "../test/media"}
                }
            ]
        }
    ]),
    {ok, _} = cowboy:start_clear(
        ?MODULE,
        [{port, 8080}],
        #{
			env => #{dispatch => Dispatch},
			middlewares => [http_middleware_cors, cowboy_router, cowboy_handler]
		}
    ),
    cinema_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(?MODULE).

%% internal functions
