-module(mm).
-compile(export_all).
%% -export([extend/1]).


%% Run from shell:
%%
%% Destination = "c:/Users/bilgee/Downloads/test".
%% File = "c:/Users/bilgee/Downloads/code_rep/ftorrent/src/   "FILENAME" ".
%% mm:extend(Destination, File).

%% p.s. db:start("...  .torrent") should be run beforehand!


%% Extract the given file into multiple file structure. Arguments are: Destination folder and (location of) a file to be extracted 
extend(Destination, File) ->
    File_items = db:read("path"),
    Bin = get_binary(File),
    Current_dir = create_parent(Destination),
    parse_items(File_items, Bin, Current_dir).



%% Parse the value of "path" from the ETS table i.e. "files" list of "info" dictionary of Metainfo file, and create appropriate file structures with the help of 'create_file()'
parse_items([], _Bin, _Current_dir) ->
    dictionary_parsed;

parse_items([{Length, Path} | Rest_items], Bin, Current_dir) ->
    <<Head_bin:Length/binary, Rest_bin/binary>> = Bin,
    create_file(Path, Head_bin, Current_dir),     
    parse_items(Rest_items, Rest_bin, Current_dir).
  


%% Create and return default/parent directory with the name of a torrent file
create_parent(Current_dir) ->
    Default_dir = Current_dir++"/"++db:read("FileName"),
    case file:make_dir(Default_dir) of
	ok ->
	    io:format("created default dir~n");	    
	{error, EReason} ->
	    EReason
    end,
    Default_dir.
  

%% Define whether an element in a path-list is a directory or a file type and create designated structure. Arguments are: a list of files, a binary chunk, and the current working directory
create_file([], _Bin, _Current_dir) ->    
    structure_created;

create_file([H|T], Bin, Current_dir) when length([H|T])==1 ->
    New_file = Current_dir++"/"++H,
    case file:write_file(New_file, Bin) of
	ok ->
	    io:format("created file ~p~n", [H]);
	{error, Reason} ->
	    Reason
    end;

create_file([H|T], Bin, Current_dir) when length([H|T])>1 ->
    New_dir = Current_dir++"/"++H,
    case file:make_dir(New_dir) of
	ok -> 
	    io:format("created dir ~p~n", [H]),
	    create_file(T, Bin, New_dir);
	{error, Reason} ->
	    Reason
    end.
    

%% Read the given file and return the binary of it. Otherwise return error message
get_binary(File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    Bin;
	{error, Reason} ->
	    Reason
    end.


