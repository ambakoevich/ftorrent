%% @ Author: Rashid Darwish and David Giorgidze
%% @ Version v0.1
%% @doc Created: 14-11-2011
%% Description: Receiving all requested blocks and building the pieces
%% Requsting blocks, sending interested/not interested and keep alive msg´s

-module(connection_mngr).
-compile(export_all).
-include("constants.hrl").


%% @doc receiver receives all requested blocks data, building the pieces 
%% when the piece is builded msg passing it to the connection_server
%% Takes three arguments, Socket, Piece and piece Size
receiver(Socket,Piece,Size)->
    receive
	{tcp, _,<<?PIECE, ChunkNumber:32, Offset:32, Block/binary>>} -> 
	    %% io:format("~nOffset: ~p ~p ~p~n", [Offset, ChunkNumber, Socket]),
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

        {piece_downloaded, ChunkNumber}-> self() ! {ok, piece_downloaded, ChunkNumber, Piece};
	_ -> self() ! {error, drop_connection}
     after 30000 ->   self() ! {error, drop_connection}
			 
    end.


%% @doc sending interested msg to the peer
%% Argument Socket
send_interested(Socket)->
    ok = gen_tcp:send(Socket, [<<?INTERESTED>>]).

%% @doc sending not interested msg to the peer
%% Argument Socket
send_not_interested(Socket)->
    ok = gen_tcp:send(Socket, [<<?NOT_INTERESTED>>]).

%% @doc requesting a piece from the peer
%% Argument Socket, chunknumber
request_piece(Socket,ChunkNumber)->
     ok = gen_tcp:send(Socket, [<<?REQUEST, ChunkNumber:32, ?BEGIN:32, ?LENGTH:32>>]).
						  
%% @doc sending keep alive  msg to the peer
%% Argument Socket
send_keepAlive(Socket)->
    ok = gen_tcp:send(Socket, [?KEEP_ALIVE]).

