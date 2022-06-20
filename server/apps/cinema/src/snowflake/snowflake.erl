% Quick and basic snowflake-ish ID generator
% Does not handle counter overflow right now, 
%   although that is unlikely to be an issue in this application.
%
% Format:
%   1 bit of 0's (padding)
%   41 bits for timestamp in millis
%   10 bits for node hash (either hostname or erlang node id if set)
%   12 bits for counter (resets to 0 every millisecond)

-module(snowflake).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% gen_server exports
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).
-export([new/0]).

%% Record for State
-record(snowflake_state, {timestamp, nodehash, counter}).

-define(SERVER_NAME, ?MODULE).

-define(MACHINE_ID_BITS, 10).

%% public API

new() ->
	gen_server:call(?SERVER_NAME, get_id).

%% gen_server callbacks

start_link() ->
	gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [], []).

init(_) ->
	TS = get_ts_milli(),
	NodeHash = node_to_machine_id(get_node()),
	{ok, #snowflake_state{
		timestamp = TS,
		nodehash = NodeHash,
		counter = 0
	}}.

handle_call(get_id, _, State) ->
	Id = snowflake_state_to_id(State),
	NewState = increment_state(State),
	?LOG_INFO("new id generated"),
	{reply, {ok, Id}, NewState};
handle_call(Call, _, State) ->
	?LOG_ERROR("invalid call received", #{call => Call}),
	{reply, false, State}.

handle_cast(Cast, State) ->
	?LOG_ERROR("invalid cast received", #{cast => Cast}),
	{noreply, State}.

increment_state(#snowflake_state{
		timestamp = LastTimeStamp,
		nodehash = NodeHash,
		counter = LastCounter
	}) ->
	TimeStamp = get_ts_milli(),
	NewCounter = case TimeStamp of
		LastTimeStamp -> LastCounter + 1;
		_ -> 0
	end,
	#snowflake_state{
		timestamp = TimeStamp,
		nodehash = NodeHash,
		counter = NewCounter
	}.

snowflake_state_to_id(#snowflake_state{
		timestamp = TimeStamp,
		nodehash = NodeHash,
		counter = Counter
	}) ->
	IdBinary = <<0:1, TimeStamp:41, NodeHash:10, Counter:12>>,
	binary:encode_hex(IdBinary).

-spec get_ts_milli() -> integer().
get_ts_milli() -> erlang:monotonic_time(millisecond).

-spec get_node() -> string().
get_node() ->
	case erlang:node() of
		'nonode@nohost' -> {ok, Hostname} = inet:gethostname(), Hostname;
		Node 			-> erlang:atom_to_list(Node)
	end.

node_to_machine_id(Node) ->
	erlang:phash2(Node, 1 bsl ?MACHINE_ID_BITS).