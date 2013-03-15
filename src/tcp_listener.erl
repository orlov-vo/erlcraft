%% Copyright
-module(tcp_listener).
-author("biziwalker").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-export([accept_func/1]).

-define(LOGIC_MODULE, tcp_fsm).

%% API
start_link(Port) ->
  error_logger:info_msg("Started tcp_listener~n"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

%% gen_server callbacks
-record(state, {listener, module}).

init([Port]) ->
  Options = [{packet, raw}, {active, once}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Options) of
    {ok, LSocket} ->
      spawn_link(?MODULE, accept_func, [LSocket]),
      {ok, #state{listener = LSocket, module = ?LOGIC_MODULE}};
    {error, Reason} ->
      error_logger:error_msg("TCP Listener error start: ~p~n", [Reason]),
      {stop, Reason}
  end.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{listener = LSocket} = _State) ->
  gen_tcp:close(LSocket),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

accept_func(LSocket) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  error_logger:info_msg("Accept connection: ~p~n", [Socket]),
  {ok, Pid} = tcp_client_sup:start_child(),
  tcp_fsm:set_socket(Pid, Socket),
  ok = gen_tcp:controlling_process(Socket, Pid),
  accept_func(LSocket).
