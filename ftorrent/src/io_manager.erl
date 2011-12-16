%% @author Rashid Darwish, Batbilig Bavuudorj
%% @copyright Framework Studio.
%% @version v0.1
%% @doc Created: 18-Nov-2011, Io_manager is responsible for writing data to file. 
%% Binary data is received from piece_manager and is written to file.

-module(io_manager).
-export([start/1, loop/2,print_to_file/3]).

%% @doc starts the module
start(Source_path)->
    Full_name = Source_path++"/"++db:read("FileName"),
    loop(Source_path, Full_name).

%% @doc receiving the builded pieces from the piece manager
%% and extracting the multi torrent files
%% implemented by Rashid and Batbilig
loop(Source_path, Full_name)->
    receive
	{ok, extract}-> 
	    case db:read("path") of
		na ->
		    %% {ok, Current_dir} = file:get_cwd(),
		    %% Current_dir = Source_path,
		    Dest = Source_path++"/"++"Torrent_Files",
		    file:copy(Full_name, Dest++"/"++db:read("FileName")),
		    file:delete(Full_name);

		_List ->
		    %% {ok, Current_dir} = file:get_cwd(),
		    %% Current_dir = Source_path,
		    Dest = Source_path++"/"++"Torrent_Files",
		    case file:make_dir(Dest) of
			ok ->
			    io:format("created dir ~p~n", [Dest]),
			    mm:extend(Dest, Full_name),
			    file:delete(Full_name);

			{error,eexist} ->
			    mm:extend(Dest, Full_name),
			    file:delete(Full_name);
			{error,Reason} ->
			    Reason
		    end
	    end;
	{print_to_file,Pid,Offset,Piece} ->
	    print_to_file(Full_name,Offset,Piece),
            pm ! {select_piece, Pid},
	    loop(Source_path, Full_name)
    end.

%% @doc writing the piece data to the file 
print_to_file(Name,Offset,Data)-> 
    {ok, File} = file:open(Name,[read,write,raw,binary]),
    file:pwrite(File,Offset,Data),
    file:close(File).
