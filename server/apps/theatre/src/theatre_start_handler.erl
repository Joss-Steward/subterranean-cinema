-module(theatre_start_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(#{method := <<"PUT">>} = Request, State) ->
    File = cowboy_req:binding(file, Request),
    Reply = cowboy_req:reply(200, 
        #{
            <<"content-type">> => <<"text/plain">>
        }, [<<"ok, started ">>, File], Request),
    {ok, Reply, State}.

