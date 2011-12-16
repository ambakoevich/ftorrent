%% @author Rashid Darwish
%% @copyright Framework Studio.
%% @version v0.1
%% @doc Created: 20-Nov-2011. receiving msgs from the connection manager and server
%% chooses a piece to download and from which peer to download it



-module(piece_manager).
-compile(export_all).
-include("constants.hrl").


%% @doc starts the piece manager
start()->
    io:format("piece_manager started..."),
    loop([],[{[],self()}],[], [], []).

%% @doc loop recieving the msg
%% msg passing the complete piece to io     
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



%% @doc selecting a piece to download
%% takes 3 arguments WishList, PID, Inprocess
%% wishlist is the piece needed to be download
%% pid identify which process to download from
%% Inprocess the pieces are already in action but not finished downloading 
select_piece([], _, []) ->
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
	    io:format("pid_not_there ~n")
    end.
 
%% @doc sending interested msg to a peer   
interested(Pid)->
    Pid ! {send_interested}.

%% @doc sending not_interested msg to a peer
not_interested(Pid)->
   Pid ! {send_not_interested}.

%% @doc checks the size of the piece,
%%  msg passing the size to download it
check_size(Index, Pid)->
	   case db:read("NoOfPieces") of 
	       Index ->  Pid ! {start_download,Index,db:read("LastPieceSize")};
	       _ -> Pid ! {start_download,Index,db:read("pieceSize")}
	   end.
					    
%% @doc join the pieces are already downloading in a downloadedlist
join_pieces(DownloadedList, Inprocess)->
    [{List, _}] = DownloadedList,
    [{lists:umerge(lists:sort(List), lists:sort(Inprocess)), self()}].

%% @doc remove the index of the piece from a list
remove_piece(Piece,[Piece|Inprocess])->
    Inprocess;
remove_piece(Piece,[H|Inprocess])->
    [H|remove_piece(Piece, Inprocess)];
remove_piece(_,[]) -> 
[].


%% @doc emptying all remaining msg from the mailbox after finishing downloading the file
flush_all()->
    receive 
	_->
	    flush_all()
    after 0 -> 
	    io ! {ok, extract}
    end.
	 
	    
