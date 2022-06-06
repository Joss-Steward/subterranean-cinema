-module(theatre_session_statem).
-behaviour(gen_statem).

-export([start/1, start_link/0, stop/1]).
-export([terminate/3, code_change/4, init/1, callback_mode/0]).
-export([play/1, pause/1, tick/1, seek/1, join/2, leave/2]).
-export([playing/3, paused/3]).

%% API.  This example uses a registered name name()
%% and does not link to the caller.
start(Name) ->
    gen_statem:start({local, Name}, ?MODULE, [], []).

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

stop(Name) ->
    gen_statem:stop(Name).

%% Public API

play(Pid) ->
    gen_statem:cast(Pid, play).

pause(Pid) ->
    gen_statem:cast(Pid, pause).

tick(Pid) ->
    gen_statem:cast(Pid, tick).

seek(Pid) ->
    gen_statem:cast(Pid, seek).

join(Pid, ClientPid) ->
    ok.

leave(Pid, ClientPid) ->
    ok.

-record(playback_state, {
    clients = [],
	% Movie Length in ms
    length = 0,
	% Playback position in ms
    playback = 0,
	% Playback last started time
	played_at = na
}).

%% Mandatory callback functions
terminate(_Reason, _State, _Data) ->
    void.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

init([]) ->
    %% Set the initial state + data.  Data is used only as a counter.
    State = paused,
    Data = #playback_state{
		clients = [],
		length = 1200000,
		playback = 0,
		played_at = na
		},
	start_tick_timer(),
    {ok, State, Data}.

callback_mode() -> state_functions.

start_tick_timer() ->
	erlang:start_timer(5000, self(), {tick}).

%% State Callbacks

playing(cast, play, Data) ->
    {keep_state, Data};
playing(cast, pause, Data) ->
	FurtherElapsed = erlang:monotonic_time(millisecond) - Data#playback_state.played_at,
	NewData = Data#playback_state{
		playback = Data#playback_state.playback + FurtherElapsed,
		played_at = na
		},
    {next_state, paused, NewData};
playing(cast, Event, Data) ->
	handle_event(cast, Event, Data);
playing(info, {timeout, _, {tick}}, Data) ->
	handle_event(cast, tick, Data).

paused(cast, pause, Data) ->
    {keep_state, Data};
paused(cast, play, Data) ->
    % Set state to playing
    Start = erlang:monotonic_time(millisecond),
	NewData = Data#playback_state{played_at = Start},
    {next_state, playing, NewData};
paused(cast, Event, Data) ->
	handle_event(cast, Event, Data);
paused(info, {timeout, _, {tick}}, Data) ->
	handle_event(cast, tick, Data).

%% Handle events common to all states
handle_event({call, From}, get_count, Data) ->
    %% Reply with the current count
    {keep_state, Data, [{reply, From, Data}]};
handle_event(cast, tick, Data) ->
	TotalTime = case Data#playback_state.played_at of
		na -> 
			Data#playback_state.playback;
		PlayedAt -> 
			FurtherElapsed = erlang:monotonic_time(millisecond) - PlayedAt,
			Data#playback_state.playback + FurtherElapsed
	end,
	io:fwrite("~p ~pms elapsed~n", [self(), TotalTime]),
	start_tick_timer(),
	{keep_state, Data};
handle_event(A, B, Data) ->
	io:fwrite("Handle Event: ~p, ~p~n", [A, B]),
    %% Ignore all other events
    {keep_state, Data}.

notify(State) ->
    io:fwrite("State changed to ~s~n", State),
    ok.
