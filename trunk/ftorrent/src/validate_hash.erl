%% @author Batbilig Bavuudorj.
%% @copyright Framework Studio.
%% @version v0.1
%% @doc Created 15-Nov-2011. Validate_hash is responsible for checking
%% the hash of a piece. Which later on is outputted in target file.

-module(validate_hash).
-export([find_hash/2, get_hash/1, hash_it/1]).


%% @doc Check if the given hashed piece(binary) exists in the given binary.
 
find_hash(<<>>, _) ->
    false;
find_hash(<<Piece:160/bitstring, _/bitstring>>, Piece)->
    true;
find_hash(<<_:160/bitstring, Rest/bitstring>>, Piece) ->
    find_hash(Rest, Piece).


%% @doc Return the concatenation of 20-byte hash info as a binary. 
get_hash(Torrent) ->    
    {_, [_,_,_,{_,{_,L2}}]} = bencode:info_hash(Torrent),
    list_to_binary(L2).


%% @doc Read the file and hash it using sha1. 
hash_it(File) ->
    case file:read_file(File) of
	{ok,Dat} ->
	    crypto:sha(Dat);
	{error, Reason} ->
	    {error, Reason}
		end.



%% Run these from the shell:
%%
%%
%% Bin = find_el:get_hash("test3.png.torrent").
%% Hash = find_el:hash_it("test3").
%% find_el:find_hash(Bin, Hash).


