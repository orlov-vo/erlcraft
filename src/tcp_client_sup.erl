%% Copyright
-module(tcp_client_sup).
-author("biziwalker").

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0]).

%% supervisor
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).

%% API
start_link() ->
  error_logger:info_msg("Started tcp_client_sup~n"),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
  supervisor:start_child(?MODULE, []).

%% supervisor callbacks
init([]) ->
  Flags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
  Spec = [
    {undefined,
      {tcp_fsm, start_link, []},
      temporary,
      2000,
      worker,
      [tcp_fsm]}],
  {ok, {Flags, Spec}}.
