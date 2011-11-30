%% Author: Zarif Jawad, Paulius Vysniauskas, David Giorgidze
%% Created: 2011-11-07
%% Description : Extract all information form a torrent file
%%               and insert it to ets table. Build the line
%%               used by tracker module to connect to tracker.

-module(db).
-import(bencode,[readtorrent/1,info_hash/1]).
-compile(export_all).
-include("constants.hrl").

%%Create ets table and insert torrent file information in it
start(Torrent) ->
    ets:new(db,[bag,named_table]),
    {{dico, Data},_} = readtorrent(Torrent),
    insert_url(Data),
    insert_length(Data),
    %%insert_path(Data),
    insert_piece_length(Data),
    insert_filename(Data),
    insert_infohash_binary(Torrent),
    insert_infohash_hex(Torrent),
    insert_piece_info(Data),
    db.
    %% Data.

write(Key,Elem,Table)->
    ets:insert(Table,{Key,Elem}),
    Table.
destroy() ->
    ets:delete(db).
read(Key) ->
    [{_,Info}] =  ets:lookup(db,Key),
    Info.
delete(Key,Db) ->
    ets:delete(Db,Key), 
    Db.


sum([], Total) ->
    Total;
sum([H|T], Total) ->
    sum(T, H+Total).
    

insert_piece_info(Data)->
    %% {value, {{str,_},{dico,Info_list}}}= lists:keysearch({str,"info"},1,Data),
    %% {value,{{str,_},{num,Length}}}=lists:keysearch({str,"length"},1,Info_list),

    {value, {{str,_},{dico,Info_list}}} = lists:keysearch({str,"info"},1,Data),    
    case lists:keysearch({str,"files"},1,Info_list) of
	{value, {{str,_},{liste,Dico_list}}} ->
	    Length_list = parse_length(Dico_list),
	    Length = sum(Length_list, 0);
	_Nomatch ->
	    {value,{{str,_},{num,Length}}}=lists:keysearch({str,"length"},1,Info_list)	  
    end,
    
    %% Length_list = read("length"),
    %% Length = sum(Length_list, 0),


    {value,{{str,_},{num,Piece_length}}}=lists:keysearch({str,"piece length"},1,Info_list),
    Last_piece_length = Length rem Piece_length, 
    write("LastPieceSize",Last_piece_length,db),
    Last_block_length = Last_piece_length rem 16384,
    write("LastBlockLength",Last_block_length,db),
    No_of_pieces = Length div Piece_length,
    write("NoOfPieces",No_of_pieces,db).


%%retreive url from the torrent file and insert it to the ets table
insert_url(Data) ->
    {value, {{str,_},{str,URL}}}= lists:keysearch({str, "announce"},1,Data),
    %% URL.
    write("announce",URL,db).

%%retreive total length of the file from the torrent file and insert it to ets table
insert_length(Data) ->
    {value, {{str,_},{dico,Info_list}}} = lists:keysearch({str,"info"},1,Data),    
    case lists:keysearch({str,"files"},1,Info_list) of
	{value, {{str,_},{liste,Dico_list}}} ->
	    Length_list = parse_length(Dico_list),
	    Length = sum(Length_list,0),
	    write("length",Length,db);    
	_Nomatch ->
	    {value,{{str,_},{num,Length}}}=lists:keysearch({str,"length"},1,Info_list),
	    write("length",Length,db)    
    end.




%% Parse the nested or multiple files and return the length of theirs in a list
parse_length([]) ->
    [];
parse_length([{dico, L}|T]) ->
    {value,{{str, _},{num,Length}}}=lists:keysearch({str,"length"},1,L),
    [Length | parse_length(T)].


%% Parse the nested files and return the names of theirs in a list
parse_path([]) ->
    [];
parse_path([{dico, L}|T]) ->
    {value,{{str, _},{liste, Sublist}}}=lists:keysearch({str,"path"}, 1, L),
    S_list = parse_subpath(Sublist),
    %% {value,{{str, _},{liste, [{str, Name}]}}}=lists:keysearch({str,"path"}, 1, L),
    [S_list | parse_path(T)].


parse_subpath([]) ->
    [];
parse_subpath([{str, Name}|T]) ->
    [Name | parse_subpath(T)].


%% Insert all the file/path names into an ETS table
insert_path(Data) ->
    {value, {{str,_},{dico,Info_list}}} = lists:keysearch({str,"info"},1,Data), 
    case lists:keysearch({str,"files"},1,Info_list) of
	{value, {{str,_},{liste,Dico_list}}} ->
	    Path = parse_path(Dico_list);
	_Nomatch ->
	    Path = na
    end,
    write("path", Path, db).



   

%%retreive peice length of the file from the torrent file and insert it to ets table
insert_piece_length(Data) -> 
    {value, {{str,_},{dico,Info_list}}}= lists:keysearch({str,"info"},1,Data),
    {value,{{str,_},{num,Piece_length}}}=lists:keysearch({str,"piece length"},1,Info_list),
    %% Piece_length.
    write("pieceSize",Piece_length,db).


%%retreive filename from the torrent file and insert to ets table
insert_filename(Data)->
    {value, {{str,_},{dico,Info_list}}}= lists:keysearch({str,"info"},1,Data),
    {value,{{str,_},{str,File_name}}}=lists:keysearch({str,"name"},1,Info_list),
    write("FileName",File_name,db).




%%retreive hash_info from the torrent file and insert it to ets table
insert_infohash_binary(Torrent)-> 
    write("InfoHashBinary",info_hash(Torrent),db).
insert_infohash_hex(Torrent)->
    write("InfoHashHex", bencode:bin_to_hexstr(info_hash(Torrent)),db).

%%retreive the entire hash from the torrent file
retreive_hash_binary(Torrent) ->
    {{dico, Data},_} = readtorrent(Torrent),
    {value, {{str,_},{dico,Info_list}}}= lists:keysearch({str,"info"},1,Data),
    {value,{{str,_},{str,Pieces}}}=lists:keysearch({str,"pieces"},1,Info_list),
   Pieces.
   

%%Build the line which is used for connecting to tracker
concat(Announce,Hash,Peer_id,Port,Uploaded,Downloaded,Left,Compact)->
    Announce ++ "?info_hash=" ++ Hash ++ "&peer_id=" ++ Peer_id ++ "&port=" ++
	Port ++ "&uploaded=" ++ Uploaded ++ "&downloaded=" ++ Downloaded ++ 
	"&left=" ++ Left ++ "&compact=" ++ Compact.
concat()->
    concat(db:read("announce"),encode_hash(db:read("InfoHashHex")),?FWSID,?FWSPORT,"0","0",integer_to_list(db:read("length")),"1").

%%Encode the url which is used for connecting to tracker
encode_hash([A,B|Rest])->
    "%" ++ [A] ++ [B] ++ encode_hash(Rest);
encode_hash([]) ->
    [].

