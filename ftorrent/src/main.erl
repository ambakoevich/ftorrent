%% Author: Zarif Jawad,Paulius Vysniauskas,David Giorgidze
%% Created: 2011-11-20
%% Running Main Application.
-module(main).
-compile(export_all).
-include("constants.hrl").

start() ->
    Manager = spawn(manager, start, []),
    GUI = spawn(gui, start, [Manager]),
    register(gui,GUI).   
    
