%% Author: Zarif Jawad,Paulius Vysniauskas,David Giorgidze
%% Created: 2011-11-20

-module(main).
-compile(export_all).

start() ->
    Manager = spawn(manager, start, []),
    GUI = spawn(gui, start, [Manager]).   
    
