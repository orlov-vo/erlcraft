%% Copyright
-module(erlcraft_sup).
-author("biziwalker").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% supervisor
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).

%% API
start_link(Port) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

%% supervisor callbacks
init([Port]) ->
  Flags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
  Spec = [
    {tcp_listener,
      {tcp_listener, start_link, [Port]},
      permanent,
      2000,
      worker,
      [tcp_listener]
    },
    {tcp_client_sup,
      {tcp_client_sup, start_link, []},
      permanent,
      2000,
      worker,
      [tcp_client_sup]}],
  {ok, {Flags, Spec}}.
