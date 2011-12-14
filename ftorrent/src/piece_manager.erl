%% PIECE MANAGER.
-module(piece_manager).
-compile(export_all).
-include("constants.hrl").



start()->
    io:format("piece_manager started..."),
    loop([],[{[],self()}],[], [], []).
    
loop(WishList, DownloadedList, PeersList, Inprocess, HashList)->
    receive
	{ok, file_downloaded} -> 
	    flush_all(),
	    io:format("~n ***FILE DOWNLOADED*** ~n");
	    %% loop(WishList, DownloadedList, PeersList, Inprocess, HashList);
	
        {ok, Hash}-> 
	    loop(WishList, DownloadedList, PeersList, Inprocess, list_to_binary(Hash));
        {bitfield,Pid, Bitfield} ->
	    self() ! {check_interested, Pid},
	    UpdatedWishList = filter:join_set([{Bitfield, Pid}|PeersList], join_pieces(DownloadedList, Inprocess)),
	    loop(UpdatedWishList, DownloadedList, [{Bitfield, Pid}|PeersList], Inprocess,HashList);

	{have, Pid, Piece} -> 
	    UpdatedPeesList = filter:update_have(PeersList,Piece,Pid,[]),
	    UpdatedWishList = filter:join_set(UpdatedPeesList, join_pieces(DownloadedList, Inprocess)),
	    loop(UpdatedWishList, DownloadedList, UpdatedPeesList,Inprocess,HashList );

	{select_piece, Pid} ->
	    UpdatedWishList = filter:get_rarest(filter:join_set(PeersList, join_pieces(DownloadedList, Inprocess))),
            
	    PieceNumber =  select_piece(UpdatedWishList, Pid, Inprocess),
	    case is_integer(PieceNumber) of
		true->
		    loop(UpdatedWishList, DownloadedList, PeersList, [PieceNumber|Inprocess],HashList);
		false->
		    loop(UpdatedWishList, DownloadedList, PeersList,Inprocess,HashList)
	    end;

	{check_piece, Pid, ChunkNumber, Piece}->
            case validate_hash:find_hash(HashList,crypto:sha(Piece)) of

		false ->
		    Updated_Process = remove_piece(ChunkNumber,Inprocess),
		    UpdatedWishList = filter:get_rarest(filter:join_set(PeersList, join_pieces(DownloadedList, Updated_Process))),
		    pm ! {select_piece, Pid},
		    loop(UpdatedWishList,DownloadedList, PeersList, Updated_Process,HashList);

		true ->
		    io ! {print_to_file,Pid,(ChunkNumber*db:read("pieceSize")),Piece},
		    [{L, _}] = DownloadedList,
		    Downloaded =  [{[ChunkNumber|L], self()}],
		    Updated_Process = remove_piece(ChunkNumber,Inprocess),
		    UpdatedWishList = filter:get_rarest(filter:join_set(PeersList, join_pieces(Downloaded, Updated_Process))),
		     gui ! {piece_downloaded},
		    loop(UpdatedWishList,Downloaded, PeersList, Updated_Process,HashList)
	    end;   

		{check_interested, Pid} -> 
		    case filter:lookup(WishList, Pid) of 
			pid_not_there -> not_interested(Pid);
			_  -> interested(Pid)
		    end,
	    loop(WishList, DownloadedList, PeersList,Inprocess,HashList)
    end.




select_piece([], _Pid, []) ->
 self() ! {ok, file_downloaded};

select_piece([], Pid, Inprocess) ->
    case length(Inprocess) of 
	0 ->  Pid ! {ok, keepAlive};
	_ ->     check_size(lists:last(Inprocess), Pid)
    end;

select_piece(WishList, Pid, _)-> 
    {PieceNumber,[{_,_}]} = filter:lookup(WishList, Pid),
    case is_integer(PieceNumber) of
	true ->  
	    check_size(PieceNumber, Pid),
	    PieceNumber;
	false -> 
	    io:format("pid_not_there_error~n")
	    %%   Pid ! {ok, keepAlive},
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
remove_piece(_,[]) -> 
[].



flush_all()->
    receive 
	_->
	    flush_all()
    after 0 -> 
	    io ! {ok, extract}
    end.
	 
	    
