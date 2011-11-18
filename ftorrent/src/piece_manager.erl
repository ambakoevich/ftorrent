-module(piece_manager).
-compile(export_all).
-include("constants.hrl").



start()->
    io:format("piece_manager started..."),
    loop([]).
    
loop(Bitfield)->
    receive
        {bitfield,Pid,Socket, Bit} ->
	    io:format("~nBitField: ~p  ~n", [lists:sort(Bit)]),
	    interested(Pid),
	    loop(lists:sort(Bit));

	{have, Pid, Piece} -> 
	    io:format("~nHAVE: ~p~n", [Piece]),
	    loop(lists:sort([Piece|Bitfield]));

	{select_piece, Pid} -> 
	    L = select_piece(Bitfield, Pid),
	    loop(L);	  
     
	{check_piece, Pid, ChunkNumber, Piece} ->
	    io ! {print_to_file,Pid,(ChunkNumber*db:read("pieceSize")),Piece},
	    loop(Bitfield)
    end.




select_piece([Index|Bitfield], Pid)->
    check_size(Index, Pid),
    Bitfield.
    
interested(Pid)->
    Pid ! {send_interested}.

not_interested(Pid)->
   Pid ! {send_not_interested}.


check_size(Index, Pid)->
    case db:read("NoOfPieces")-1 of 
	    Index ->  Pid ! {start_download,Index,db:read("LastPieceSize")};
		_ -> Pid ! {start_download,Index,db:read("pieceSize")}
				    end.
					    
