-module(http_middleware_cors).

-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, State) ->
    ReqWithCorsHeaders = set_cors_headers(Req),
    Method = cowboy_req:method(ReqWithCorsHeaders),

    case Method of
		<<"OPTIONS">> ->
			ReqFinal = cowboy_req:reply(200, ReqWithCorsHeaders),
			{stop, ReqFinal};
		_ ->
			%% continue as normal
			{ok, ReqWithCorsHeaders, State}
    end.

%% ===================================================================
%% Helpers
%% ===================================================================

cors_headers() -> #{
		<<"access-control-allow-origin">>  => <<"*">>,
		<<"access-control-allow-headers">> => <<"*">>,
		<<"access-control-allow-methods">> => <<"HEAD, GET, POST, OPTIONS">>,
		<<"access-control-max-age">>       => <<"1000">>
	}.

set_cors_headers(Req) ->
	cowboy_req:set_resp_headers(cors_headers(), Req).