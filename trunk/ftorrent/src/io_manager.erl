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
