%% Copyright
-module(erlcraft_app).
-author("biziwalker").

-behaviour(application).

% application
-export([start/0, start/2, stop/1]).

-define(PORT, 25565).

% application callbacks
start() ->
  application:start(erlcraft).

start(_Type, _Args) ->
  error_logger:error_msg("Start erlcraft...~n", []),
  case erlcraft_sup:start_link(?PORT) of
    {ok, Pid} -> {ok, Pid};
    Error ->
      Error
  end.

stop(_State) ->
  ok.
