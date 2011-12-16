%% @ Author: Rashid Darwish
%% @ Version v0.1
%% @doc Created: 14-11-2011
%% Description: Spawnning new process for each ip, Receiving all msg´s from the peer, 
-module(connection_server).
-export([start/3,init/3,loop/1]).
-include("constants.hrl").

%% @doc Starts the connection server
%% Takes 3 arguments, peer Ip, peer port, torrent Hash
%% Returns Pid 
start(Ip,Port, Hash)-> 
    Pid = init(Ip,Port, Hash),
    Pid.

%% @doc spawnning a process for the peer
%% Takes 3 arguments, peer Ip, peer port, torrent Hash
%% Returns Pid 
init(Ip,Port, Hash)->
    Pid = spawn(?MODULE,loop,[0]),
    Pid ! {start_server,Ip,Port,Hash},
    Pid.


%% @doc loop receiving all msg´s from the peer
%% Takes 1 Socket argument, S
loop(Socket) ->
    receive                 
	{start_server,Ip,Port,Hash} ->  
	    handshake:connect(Ip,Port,Hash),
	    loop(0);

	{ok, connected, S, Hash}->
	    handshake:sendHandShake(S, Hash),
	    handshake:recv(S, Hash),
	    loop(S);

	{tcp,_,<<?BITFIELD, BitField/binary>>} -> 
	    pm ! {bitfield,self(),parse_bitfield:concat_list(BitField)},
	    loop(Socket);


	{tcp,_,<<?HAVE, Piece:32>>} ->
	    pm ! {have,self(), Piece},
	    loop(Socket);

	{ok,handshaked} ->  
	    ok =  inet:setopts(Socket, ?INETBF),
	    loop(Socket);

        {send_not_interested} ->
	    connection_mngr:send_not_interested(Socket),
	    loop(Socket);

	{send_interested} -> 
	    connection_mngr:send_interested(Socket),
	    loop(Socket);

        {ok,unchoke}->   
	    pm ! {select_piece,self()},
	    loop(Socket);

        {start_download, ChunkNumber, Size} ->
	    connection_mngr:request_piece(Socket,ChunkNumber),
	    connection_mngr:receiver(Socket,[],Size),
	    loop(Socket);

        {ok, piece_downloaded, ChunkNumber, Piece} ->
	    case  whereis(pm) of
		undefined -> 
		    loop(Socket);
		_ ->  pm ! {check_piece, self(), ChunkNumber, lists:reverse(Piece)},
		      io:format("~n piece_downloaded:~p~n",[ChunkNumber])
	    end,
	    
	    loop(Socket);

	{tcp,_,?KEEP_ALIVE} -> 
	    io:format("Keep_Alive ~w~n",[?KEEP_ALIVE]),
	    connection_mngr:send_keepAlive(Socket),
	    loop(Socket);

	{tcp,_,<<?CHOKE>>} ->
	    io:format("~nRESPONSE: ~w SHOCKED~n",[<<?CHOKE>>]),
	    loop(Socket);

	{tcp,_,<<?UNCHOKE>>} ->
            self() ! {ok,unchoke},
	    io:format("~nRESPONSE: ~w UNSHOCK~n",[<<?UNCHOKE>>]),
	    loop(Socket);
 
	{ok,keepAlive}->
	    connection_mngr:send_keepAlive(Socket),
	    loop(Socket);
	{error, drop_connection, Ip} ->
	    manager ! {ip_closed},
	    io:format("~nNo Such Peer: ~p~n",[Ip]);
        {error, drop_connection} ->
	    manager ! {ip_closed},
	    io:format("~n NO Response~n");
	R -> 
	    io:format("~n>>>> ~p~n", [R]),
	    loop(Socket)
    end.


