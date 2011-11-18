%% Author: Zarif Jawad,Paulius Vysniauskas,David Giorgidze
%% Created: 2011-11-07
%% Description : Extract all information form a torrent file
%%               and insert it to ets table. Build the line
%%               used by tracker module to connect to tracker 

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
    insert_piece_length(Data),
    insert_filename(Data),
    insert_infohash_binary(Torrent),
    insert_infohash_hex(Torrent),
    insert_piece_info(Data),
    db.

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

insert_piece_info(Data)->
    {value, {{str,_},{dico,Info_list}}}= lists:keysearch({str,"info"},1,Data),
    {value,{{str,_},{num,Length}}}=lists:keysearch({str,"length"},1,Info_list),
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
    write("announce",URL,db).

%%retreive total length of the file from the torrent file and insert it to ets table
insert_length(Data) ->
    {value, {{str,_},{dico,Info_list}}}= lists:keysearch({str,"info"},1,Data),
{value,{{str,_},{num,Length}}}=lists:keysearch({str,"length"},1,Info_list),
    write("length",Length,db).

%%retreive peice length of the file from the torrent file and insert it to ets table
insert_piece_length(Data) -> 
    {value, {{str,_},{dico,Info_list}}}= lists:keysearch({str,"info"},1,Data),
{value,{{str,_},{num,Piece_length}}}=lists:keysearch({str,"piece length"},1,Info_list),
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

%%retreive peice binary of the hashed part of the torrent file and insert it to ets table
%%insert_piece_binary(Torrent) ->
%%    {{dico, Data},_} = readtorrent(Torrent),
%%    {value, {{str,_},{dico,Info_list}}}= lists:keysearch({str,"info"},1,Data),
%%{value,{{str,_},{str,Piece_binary}}}=lists:keysearch({str,"pieces"},1,Info_list),
%%    write("piece length",Piece_binary,db).
    
%%Build the line which is used for connecting to tracker
concat(Announce,Hash,Peer_id,Port,Uploaded,Downloaded,Left,Compact)->
    Announce ++ "?info_hash=" ++ Hash ++ "&peer_id=" ++ Peer_id ++ "&port=" ++
   Port ++ "&uploaded=" ++ Uploaded ++ "&downloaded=" ++ Downloaded ++ 
   "&left=" ++ Left ++ "&compact=" ++ Compact.
concat()->
    concat(db:read("announce"),encode_hash(db:read("InfoHashHex")),?FWSID,?FWSPORT,"0","0","0","1").

%%Encode the url which is used for connecting to tracker
encode_hash([A,B|Rest])->
    "%" ++ [A] ++ [B] ++ encode_hash(Rest);
encode_hash([]) ->
              [].
