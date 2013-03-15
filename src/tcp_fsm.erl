%% Copyright
-module(tcp_fsm).
-author("biziwalker").

-behaviour(gen_fsm).

%% API
-export([start_link/0, set_socket/2]).

%% gen_fsm
-export([init/1, wait_for_socket/2, wait_for_data/2, handle_event/3,
  handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% API
start_link() ->
  error_logger:info_msg("Started tcp_fsm~n"),
  gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
  gen_fsm:send_event(Pid, {socket_ready, Socket}).

%% gen_fsm callbacks
-record(state, {socket, addr}).

init(_Args) ->
  {ok, wait_for_socket, #state{}}.

wait_for_socket({socket_ready, Socket}, State) when is_port(Socket) ->
  inet:setopts(Socket, [binary, {packet, raw}, {nodelay, true}, {active, once}, {keepalive, true}]),
  {ok, {Address, Port}} = inet:peername(Socket),
  error_logger:info_msg("IP: ~p~nPort: ~p~n", [Address, Port]),
  {next_state, wait_for_data, State#state{socket = Socket, addr = Address}}.

wait_for_data({data, Data}, State) when is_list(Data) ->
  wait_for_data({data, binary:list_to_bin(Data)}, State);
wait_for_data({data, Data}, #state{socket = Socket} = State) when is_binary(Data) ->
  inet:setopts(Socket, [binary, {packet, raw}, {nodelay, true}, {active, once}, {keepalive, true}]),
  tcp_router:route_data(Data, Socket),
  {next_state, wait_for_data, State};
wait_for_data(timeout, #state{addr = Address} = State) ->
  error_logger:info_msg("~p Client connection timeout - closing.~n", [Address]),
  {stop, normal, State};
wait_for_data(Other, State) ->
  error_logger:error_msg("State: wait_for_data~nData: ~w~n", [Other]),
  {next_state, wait_for_data, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info({tcp, _Socket, Data}, StateName, State) ->
  ?MODULE:StateName({data, Data}, State);
handle_info({tcp_closed, _Socket}, _StateName, #state{addr = Address} = State) ->
  error_logger:info_msg("~p Client disconnected.~n", [Address]),
  {stop, normal, State};
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
