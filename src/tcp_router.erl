%% Copyright
-module(tcp_router).
-author("biziwalker").

%% API
-export([route_data/2, send_data/3]).

route_data(Data, Socket) when byte_size(Data) >= 1 ->
  OpCode = binary:first(Data),
  Body = binary:part(Data, 1, byte_size(Data) - 1),
  error_logger:info_msg("Recive packet with OpCode: ~.16#~nData: ~p~n", [OpCode, Body]),
  packetid(OpCode, Body, Socket).

send_data(OpCode, Data, Socket) ->
  Bin = <<OpCode:8, Data/binary>>,
  error_logger:info_msg("Send packet with OpCode: ~.16#~nData: ~p~n", [OpCode, Data]),
  gen_tcp:send(Socket, Bin).

packetid(16#01, _Data, _Socket) ->
  ok;
packetid(16#FE, _Data, Socket) ->
  Str = "Erlcraft server" ++ [16#A7] ++ "0" ++ [16#A7] ++ "5000",
  send_data(16#FF, <<(length(Str)):16, (unicode:characters_to_binary(Str, utf8, {utf16, big}))/binary>>, Socket);
packetid(OpCode, Data, _Socket) ->
  error_logger:error_msg("Recive packet with invalid OpCode: ~.16#~nData: ~p~n", [OpCode, Data]).
