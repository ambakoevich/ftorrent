%% @author Rashid Darwish.
%% @copyright Framework Studi
%% @version v0.1
%% @doc Created: 14-Nov-2011, handshake establishes connection with peers.

-module(handshake).
-compile(export_all).
-include("constants.hrl").

%% @connects to the peer
%% msg passing the socket to the connection server
%% Take 3 arguments peer Id, peer port and torrent´s Hash 
connect(Ip,Port,Hash) ->
    case gen_tcp:connect(Ip,Port,?INETHS) of
	{ok, Socket}  -> 
	    self() ! {ok, connected, Socket, Hash};

	{error, econnrefused} ->
	    self() ! {error, drop_connection, Ip};
	{error,etimedout} ->{error, drop_connection, Ip}
    end.
	
%% @doc Send handshake using Socket and Info_Hash which itself,
%% is retreived from .torrent file.
sendHandShake(Socket, Info_Hash) ->
    ok = gen_tcp:send(Socket, [?PSTRLEN, ?PSTR,
			       ?RESERVED,
			       Info_Hash,
			       ?FWSID]).

%% @doc Receive handshake from peer.    
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

