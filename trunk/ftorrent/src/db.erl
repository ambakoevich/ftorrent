%% @author Zarif Jawad, Paulius Vysniauskas, David Giorgidze
%% @copyright Framework Studio
%% @version v0.1
%% @doc Created: 07-Nov-2011, db extracts all information
%% file and inserts it to ETS table. Builds the URL which
%% is used to connect to tracker.

-module(db).
-import(bencode,[readtorrent/1,info_hash/1]).
-export([start/1, write/3, destroy/0, read/1, delete/2, sum/2, insert_piece_info/1,encode_hash/1,convert/1]).
-export([insert_url/1,insert_length/1,insert_infohash_binary/1,retreive_hash_binary/1,concat/8,concat/0]).
-include("constants.hrl").

%% @doc Create ets table and insert torrent file information in it
start(Torrent) ->
    ets:new(db,[bag,named_table]),
    {{dico, Data},_} = readtorrent(Torrent),
    insert_url(Data),
    insert_length(Data),
    insert_path(Data),
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


%% @doc retreive url from the torrent file and insert it to the ets table
insert_url(Data) ->
    {value, {{str,_},{str,URL}}}= lists:keysearch({str, "announce"},1,Data),
    %% URL.
    write("announce",URL,db).

%% @doc retreive total length of the file from the torrent file and insert it to ets table
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

%% @doc Parse the nested or multiple files and return the length of theirs in a list
parse_length([]) ->
    [];
parse_length([{dico, L}|T]) ->
    {value,{{str, _},{num,Length}}}=lists:keysearch({str,"length"},1,L),
    [Length | parse_length(T)].

%% @doc Parse the nested files and return the names of theirs in a list
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


%% @doc Create a list containing pairs of length and path tuples/items. Arguments are both lists 
create_set([], _) ->
    [];
create_set(_, []) ->
    [];
create_set([Length | Tail_length], [Path | Tail_path]) when length([Length | Tail_length])==length([Path | Tail_path]) ->
    [{Length, Path} | create_set(Tail_length, Tail_path)].



%% @doc Insert all the file/path names into an ETS table
insert_path(Data) ->
    {value, {{str,_},{dico,Info_list}}} = lists:keysearch({str,"info"},1,Data), 
    case lists:keysearch({str,"files"},1,Info_list) of
	{value, {{str,_},{liste,Dico_list}}} ->
	    Length_list = parse_length(Dico_list),
	    Path_list = parse_path(Dico_list),
	    Path = create_set(Length_list, Path_list);
	_Nomatch ->
	    Path = na
    end,
    write("path", Path, db).

%% @doc retreive peice length of the file from the
%% torrent file and insert it to ets table
insert_piece_length(Data) -> 
    {value, {{str,_},{dico,Info_list}}}= lists:keysearch({str,"info"},1,Data),
    {value,{{str,_},{num,Piece_length}}}=lists:keysearch({str,"piece length"},1,Info_list),
    %% Piece_length.
    write("pieceSize",Piece_length,db).

%% @doc retreive filename from the torrent file and insert to ets table
insert_filename(Data)->
    {value, {{str,_},{dico,Info_list}}}= lists:keysearch({str,"info"},1,Data),
    {value,{{str,_},{str,File_name}}}=lists:keysearch({str,"name"},1,Info_list),
    Bin = list_to_binary(File_name),    
    write("FileName",convert(Bin),db).

%% @doc retreive hash_info from the torrent file and insert it to ets table
insert_infohash_binary(Torrent)-> 
    write("InfoHashBinary",info_hash(Torrent),db).
insert_infohash_hex(Torrent)->
    write("InfoHashHex", bencode:bin_to_hexstr(info_hash(Torrent)),db).

%% @doc retreive the entire hash from the torrent file
retreive_hash_binary(Torrent) ->
    {{dico, Data},_} = readtorrent(Torrent),
    {value, {{str,_},{dico,Info_list}}}= lists:keysearch({str,"info"},1,Data),
    {value,{{str,_},{str,Pieces}}}=lists:keysearch({str,"pieces"},1,Info_list),
   Pieces.
   
%% @doc Build the line which is used for connecting to tracker
concat(Announce,Hash,Peer_id,Port,Uploaded,Downloaded,Left,Compact)->
    Announce ++ "?info_hash=" ++ Hash ++ "&peer_id=" ++ Peer_id ++ "&port=" ++
	Port ++ "&uploaded=" ++ Uploaded ++ "&downloaded=" ++ Downloaded ++ 
	"&left=" ++ Left ++ "&compact=" ++ Compact.
concat()->
    concat(db:read("announce"),encode_hash(db:read("InfoHashHex")),?FWSID,?FWSPORT,"0","0",integer_to_list(db:read("length")),"1").

%% @doc Encode the url which is used for connecting to tracker
encode_hash([A,B|Rest])->
    "%" ++ [A] ++ [B] ++ encode_hash(Rest);
encode_hash([]) ->
    [].


%% @doc Convert file name characters into UTF8 encoding
convert(<<>>) ->
    [];
convert( <<Ch/utf8, Rest/binary>>) ->
    [Ch|convert(Rest)].
