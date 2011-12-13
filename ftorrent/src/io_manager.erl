%% @author Rashid Darwish.
%% @copyright Framework Studio.
%% @version v0.1
%% @doc Created: 18-Nov-2011, Io_manager is responsible for writing data to file. 
%% Binary data is received from piece_manager and is written to file.

-module(io_manager).
-compile(export_all).

start()->
    loop().

loop()->
    receive
    {print_to_file,Pid,Offset,Piece} ->
	    print_to_file(db:read("FileName"),Offset,Piece),
            pm ! {select_piece, Pid},
	    loop()
end.

print_to_file(Name,Offset,Data)-> 
    {ok, File} = file:open(Name,[read,write,raw,binary]),
    file:pwrite(File,Offset,Data),
    file:close(File).
