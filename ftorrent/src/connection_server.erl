-module(connection_server).
-compile(export_all).
-include("constants.hrl").

start(Ip,Port, Hash)-> 
    Pid= init(Ip,Port, Hash),
    Pid.

init(Ip,Port, Hash)->
    Pid = spawn(?MODULE,loop,[0]),
    Pid ! {start_server,Ip,Port,Hash},
    Pid.



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
	    pm ! {bitfield,self(),Socket,parse_bitfield:concat_list(BitField)},
	    loop(Socket);


	{tcp,_,<<?HAVE, Piece:32>>} ->
	    pm ! {have,self(), Piece},
	    loop(Socket);

	{ok,handshaked} ->  
	    ok =  inet:setopts(Socket, ?INETBF),
	    loop(Socket);

        {send_not_interested} ->
	    connection_mngr:send_not_interested(Socket),
	    connection_mngr:interested_loop(Socket),
	    loop(Socket);

	{send_interested} -> 
	    connection_mngr:send_interested(Socket),
	    connection_mngr:interested_loop(Socket),
	    loop(Socket);

        {ok,unchoke}->   
	    pm ! {select_piece,self()},
	    loop(Socket);

        {start_download, ChunkNumber, Size} ->
	    %%Size = 524288,
	    %% Size = 418851,
	    connection_mngr:request_piece(Socket,ChunkNumber),
	    connection_mngr:receiver(Socket,[],Size),
	    loop(Socket);

        {ok, piece_downloaded, ChunkNumber, Piece} ->
            pm ! {check_piece, self(), ChunkNumber, lists:reverse(Piece)},
	    io:format("~n piece_downloaded~n"),
	    loop(Socket);

	{tcp,_,?KEEP_ALIVE} -> 
	    io:format("Keep_Alive ~w~n",[?KEEP_ALIVE]),
	    loop(Socket);


	{error, drop_connection, Ip} ->
	    io:format("~nNo Such Peer: ~p~n",[Ip]);

	R -> 
	    io:format("~n>>>>REPLAY ~p~n", [R]),
	    loop(Socket)
    end.


