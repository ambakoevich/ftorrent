-module(piece_manager).
-compile(export_all).
-include("constants.hrl").



start()->
    io:format("piece_manager started..."),
    loop([],[{[],self()}],[], []).
    
loop(WishList, DownloadedList, PeersList, Inprocess)->
    receive
        {bitfield,Pid, Bitfield} ->
            io:format("~nNewBit: ~p  ~n", [filter:join_set([{Bitfield, Pid}|PeersList], DownloadedList)]),
	    self() ! {check_interested, Pid},
	    UpdatedWishList = filter:join_set([{Bitfield, Pid}|PeersList], join_pieces(DownloadedList, Inprocess)),
	    loop(UpdatedWishList, DownloadedList, [{Bitfield, Pid}|PeersList], Inprocess);

	{have, Pid, Piece} -> 
	    io:format("~nHAVE: ~p~n", [Piece]),
	    UpdatedPeesList = filter:update_have(PeersList,Piece,Pid,[]),
	    UpdatedWishList = filter:join_set(UpdatedPeesList, join_pieces(DownloadedList, Inprocess)),
	    loop(UpdatedWishList, DownloadedList, UpdatedPeesList,Inprocess );

	{select_piece, Pid} ->
	    UpdatedWishList = filter:get_rarest(filter:join_set(PeersList, join_pieces(DownloadedList, Inprocess))),
	    PieceNumber =  select_piece(UpdatedWishList, Pid),
	    loop(UpdatedWishList, DownloadedList, PeersList, [PieceNumber|Inprocess]);	  
     
	{check_piece, Pid, ChunkNumber, Piece} ->
	    %% io ! {print_to_file,Pid,(ChunkNumber*db:read("pieceSize")),Piece},
	    self() ! {piece_validated, Pid, ChunkNumber, Piece},
	    loop(WishList, DownloadedList, PeersList, Inprocess);
	
	{piece_validated, Pid, ChunkNumber, Piece}->
	    io:format("~n piece_validated"),
	    io ! {print_to_file,Pid,(ChunkNumber*db:read("pieceSize")),Piece},
	    [{L, _}] = DownloadedList,
            Downloaded =  [{[ChunkNumber|L], self()}],
            Updated_Process = remove_piece(Piece,[Piece|Inprocess]),
	    UpdatedWishList = filter:get_rarest(filter:join_set(PeersList, join_pieces(Downloaded, Updated_Process))),
	    loop(UpdatedWishList,Downloaded, PeersList, Updated_Process);

	{check_interested, Pid} -> 
	    case filter:lookup(WishList, Pid) of 
		pid_not_there -> not_interested(Pid);
		_  -> interested(Pid)
	    end,
	    loop(WishList, DownloadedList, PeersList,Inprocess)

    end. 


select_piece(WishList, Pid)-> 
    io:format("~n AT select_piece  ~n"),
    {PieceNumber,[{_,_}]} = filter:lookup(WishList, Pid),
    case PieceNumber of
		pid_not_there -> 
	    io:format("pid_not_there ERRORRRRRRRRRRRRRR~n"),
	    Pid ! {ok,stayAlive};
		_  -> 
	    check_size(PieceNumber, Pid),
	    PieceNumber
	    end.
    
interested(Pid)->
    Pid ! {send_interested}.

not_interested(Pid)->
   Pid ! {send_not_interested}.


check_size(Index, Pid)->
    case db:read("NoOfPieces") of %% change -1
	    Index ->  Pid ! {start_download,Index,db:read("LastPieceSize")};
		_ -> Pid ! {start_download,Index,db:read("pieceSize")}
				    end.
					    

join_pieces(DownloadedList, Inprocess)->
    [{List, _}] = DownloadedList,
    [{lists:umerge(lists:sort(List), lists:sort(Inprocess)), self()}].


remove_piece(Piece,[Piece|Inprocess])->
    Inprocess;
remove_piece(Piece,[H|Inprocess])->
    [H|remove_piece(Piece, Inprocess)];
remove_piece(_,[]) -> [].



    
