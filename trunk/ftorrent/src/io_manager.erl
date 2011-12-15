%% @author Rashid Darwish, Batbilig Bavuudorj
%% @copyright Framework Studio.
%% @version v0.1
%% @doc Created: 18-Nov-2011, Io_manager is responsible for writing data to file. 
%% Binary data is received from piece_manager and is written to file.

-module(io_manager).
-compile(export_all).

%% @doc starts the module
start()->
    loop().

%% @doc receiving the builded pieces from the piece manager
%% and extracting the multi torrent files
%% implemented by Rashid and Batbilig
loop()->
    receive
	{ok, extract}-> 
	    case db:read("path") of
		na ->
		    {ok, Current_dir} = file:get_cwd(),
		    Dest = Current_dir++"/"++"Torrent_Files",
		    file:copy(db:read("FileName"), Dest++"/"++db:read("FileName")),
		    file:delete(db:read("FileName"));

    	_List ->
	    {ok, Current_dir} = file:get_cwd(),
	    Dest = Current_dir++"/"++"Torrent_Files",
	    case file:make_dir(Dest) of
		ok ->
		    io:format("created dir ~p~n", [Dest]),
		    mm:extend(Dest, Current_dir++"/"++db:read("FileName")),
		    file:delete(db:read("FileName"));
		     
		{error,eexist} ->
		    mm:extend(Dest, Current_dir++"/"++db:read("FileName")),
		file:delete(db:read("FileName"));
		{error,Reason} ->
		    Reason
	    end
    end;
    {print_to_file,Pid,Offset,Piece} ->
	    print_to_file(db:read("FileName"),Offset,Piece),
            pm ! {select_piece, Pid},
	    loop()
end.

%% @doc writing the piece data to the file 
print_to_file(Name,Offset,Data)-> 
    {ok, File} = file:open(Name,[read,write,raw,binary]),
    file:pwrite(File,Offset,Data),
    file:close(File).
