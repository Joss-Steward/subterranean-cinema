-module(session).
-behaviour(gen_statem).

-export([start_link/1, stop/1]).
-export([terminate/3, code_change/4, init/1, callback_mode/0]).
-export([play/1, pause/1, seek/2, join/2, leave/2]).
-export([playing/3, paused/3]).

-define(TICK_INTERVAL, 5000).

%% gen_statem callback API. 

start_link(Info) ->
    gen_statem:start_link(?MODULE, Info, []).

stop(Name) ->
    gen_statem:stop(Name).

%% Public API

play(Pid) ->
    gen_statem:cast(Pid, play).

pause(Pid) ->
    gen_statem:cast(Pid, pause).

seek(Pid, Timecode) ->
    gen_statem:cast(Pid, {seek, Timecode}).

join(Pid, {Module, Function, ViewerPid}) ->
    gen_statem:cast(Pid, {join, {Module, Function, ViewerPid}}).

leave(_, _) ->
    ok.

%% Mandatory callback functions
terminate(_Reason, _State, _Data) -> void.

code_change(_Vsn, State, Data, _Extra) -> {ok, State, Data}.

% Metadata 
%    session_id => "111A"
%    metadata =>
%    	stream_url => "ballard.mp4"
%    	runtime    => 12000
%    	title      => "Ballard Bridge Timelapse"

init({SessionId, Metadata}) ->
	% Initial State
    State = paused,
    Data = #{
		clients => [],
		session => SessionId,
		metadata => Metadata,
		position => 0
	},
	start_tick_timer(),
    {ok, State, Data}.

% Set statem mode to callback functions

callback_mode() -> state_functions.

start_tick_timer() ->
	erlang:start_timer(?TICK_INTERVAL, self(), {tick}).

%% State Callbacks

playing(cast, play, Data) ->
    {keep_state, Data};
playing(cast, pause, Data) ->
	Position = position(playing, Data),
	UpdatePosition = maps:update(position, Position, Data),
	RemovedStart = maps:remove(play_started, UpdatePosition),
    {next_state, paused, RemovedStart};
playing(cast, {seek, Timecode}, Data) ->
    StartTime = erlang:monotonic_time(millisecond),
	UpdatedStart = maps:update(play_started, StartTime, Data),
	UpdatedPosition = maps:update(position, Timecode, UpdatedStart),
	notify(seek, Timecode, Data),
	{keep_state, UpdatedPosition};
playing(cast, {join, MFP}, Data) ->
	Clients = maps:get(clients, Data),
	UpdatedClientData = maps:update(clients, [MFP | Clients], Data),
	{keep_state, UpdatedClientData};
playing(info, {timeout, _, {tick}}, Data) ->
	Position = position(playing, Data),
	notify(playing, Position, Data),
	start_tick_timer(),
	{keep_state, Data}.

paused(cast, pause, Data) ->
    {keep_state, Data};
paused(cast, play, Data) ->
    StartTime = erlang:monotonic_time(millisecond),
	NewData = maps:put(play_started, StartTime, Data),
	Position = maps:get(position, Data),
	notify(playing, Position, Data),
    {next_state, playing, NewData};
paused(cast, {seek, Timecode}, Data) ->
	NewData = maps:update(position, Timecode, Data),
	notify(seek, Timecode, Data),
	{keep_state, NewData};
paused(cast, {join, MFP}, Data) ->
	Clients = maps:get(clients, Data),
	UpdatedClientData = maps:update(clients, [MFP | Clients], Data),
	{keep_state, UpdatedClientData};
paused(info, {timeout, _, {tick}}, Data) ->
	Position = position(paused, Data),
	notify(paused, Position, Data),
	start_tick_timer(),
	{keep_state, Data}.

position(playing, Data) ->
	PlayStarted = maps:get(play_started, Data),
	Position = maps:get(position, Data),
	Position + (erlang:monotonic_time(millisecond) - PlayStarted);
position(paused, Data) ->
	maps:get(position, Data).

notify(State, Position, Data) ->
	Message = io_lib:format("~p ~p~n", [State, Position]),
	Clients = maps:get(clients, Data),
	lists:foreach(fun({M, F, P}) -> apply(M, F, [P, Message]) end, Clients).
