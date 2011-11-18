%% Author: Rashid Darwish
%% Created: 2011-11-14
%% Description: Establishing the connection with torrents Peers

-module(connection_mngr).
-compile(export_all).
-include("constants.hrl").


interested_loop(Socket)->
    receive
        {tcp,_,<<?CHOKE>>} ->
	    io:format("~nRESPONSE: ~w SHOCKED~n",[<<?CHOKE>>]),
	    interested_loop(Socket);
	{tcp,_,<<?UNCHOKE>>} ->
            self() ! {ok,unchoke},
	    io:format("~nRESPONSE: ~w UNSHOCK~n",[<<?UNCHOKE>>])
 end.

receiver(Socket,Piece,Size)->
    receive
	{tcp, _,<<?PIECE, ChunkNumber:32, Offset:32, Block/binary>>} -> 
	    io:format("~nOffset: ~p~n", [Offset]),
	    LastBlockSize = Size rem ?LENGTH,
	    case LastBlockSize of 
		0 when Offset/=(Size-?LENGTH)-> ok = gen_tcp:send(Socket, [<<?REQUEST, ChunkNumber:32, (Offset+?LENGTH):32, ?LENGTH:32>>]),
						  receiver(Socket,[Block|Piece], Size);
		LastBlockSize when LastBlockSize /= 0  -> 
		    if
			(Offset+?LENGTH ==(Size-LastBlockSize)) -> 
			    ok = gen_tcp:send(Socket, [<<?REQUEST, ChunkNumber:32, (Size-LastBlockSize):32, LastBlockSize:32>>]),
			     receiver(Socket,[Block|Piece],Size);
			(Offset<(Size-LastBlockSize))  -> 
			    ok = gen_tcp:send(Socket, [<<?REQUEST, ChunkNumber:32, (Offset+?LENGTH):32, ?LENGTH:32>>]),
			     receiver(Socket,[Block|Piece], Size);
			true -> self() ! {piece_downloaded, ChunkNumber},
                                receiver(Socket,[Block|Piece],Size)
		    end;
		_ -> self() ! {piece_downloaded, ChunkNumber},
		     receiver(Socket,[Block|Piece],Size)
	    end;

        {piece_downloaded, ChunkNumber}-> self() ! {ok, piece_downloaded, ChunkNumber, Piece}
    after 30000 ->   self() ! {error, no_response}	
    end.



send_interested(Socket)->
    ok = gen_tcp:send(Socket, [<<?INTERESTED>>]).

send_not_interested(Socket)->
    ok = gen_tcp:send(Socket, [<<?NOT_INTERESTED>>]).

request_piece(Socket,ChunkNumber)->
     ok = gen_tcp:send(Socket, [<<?REQUEST, ChunkNumber:32, ?BEGIN:32, ?LENGTH:32>>]).
						  



