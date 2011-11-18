%% Author: Rashid Darwish
%% Created: 2011-11-14
%% Description: Establishing the connection with torrents Peers

-module(handshake).
-compile(export_all).
-include("constants.hrl").
%%46.239.106.179
connect(Ip,Port,Hash) ->
    case gen_tcp:connect(Ip,Port,?INETHS) of
	{ok, Socket}  -> 
	    self() ! {ok, connected, Socket, Hash};

	{error, econnrefused} ->
	    self() ! {error, drop_connection, Ip}
    end.
	

sendHandShake(Socket, Info_Hash) ->
    ok = gen_tcp:send(Socket, [?PSTRLEN, ?PSTR,
			       ?RESERVED,
			       Info_Hash,
			       ?FWSID]).
    
recv(Socket, Info_Hash)->
    case gen_tcp:recv(Socket,?HANDSHAKE_SIZE) of
	{ok, <<?PSTRLEN, ?PSTR, 
	       _:8/binary, 
	       Info_Hash:20/binary, 
	       PeerID:20/binary>>} -> 
	    io:format("~n HANDSHAKING with ~p is complete!! ~n", [list_to_atom(binary_to_list(PeerID))]),
	    self() ! {ok,handshaked};

	{error, Error} -> self() ! {error, drop_connection, Error}
    end.

