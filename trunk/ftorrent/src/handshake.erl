%% Author: Rashid Darwish
%% Created: 2011-10-22
%% Description: Establishing the connection with torrents Peers

-module(handshake).
-export([handshaking/0, recv/1]).



%% This Function establish 1st connection between peers,
%% and sending HandSHake request
handshaking() ->
    IP = "129.16.165.237",
    Port = 24785,
    Info_Hash = <<197,104,105,225,132,136,30,247,164,69,136,58,54,244,95,90,67,209,223,75>>,
    CONN_ID =  "-FWS0001-10000000001",
    Opt = [binary, {packet, 0}, {active, false}],

    {ok, Socket} = gen_tcp:connect(IP, Port, Opt),

    ok = gen_tcp:send(Socket, [19, "BitTorrent protocol",
			     <<0,0,0,0,0,0,0,0>>,
			     Info_Hash,
			     CONN_ID]),
    recv(Socket).

%% recv Function handel are incoming msg´s to the socket
 recv(Socket)->
    case gen_tcp:recv(Socket, 68) of
         {ok, <<19,"BitTorrent protocol", 
			 Reserved:8/binary, 
	 		 InfoHash:20/binary, 
			 PeerID:20/binary>>} -> io:format("~n HANDSHAKING with ~p is complete!! ~n", [list_to_atom(binary_to_list(PeerID))]);
	{error, closed} ->  io:format("~n ERROR CLOSED ~n")   
    end.