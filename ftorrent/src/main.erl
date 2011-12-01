%% Author: Zarif Jawad,Paulius Vysniauskas,David Giorgidze
%% Created: 2011-11-20
%% Running Main Application.
-module(main).
-compile(export_all).
-include("constants.hrl").

start() ->

    io:format(" ______                                                       \n"),
    io:format("|  ____|         _                                      _    \n"),          
    io:format("| |___         _| |_   __   _ __  _ __   __    _ __   _| |_  \n"),
    io:format("|  ___|  ____ |_   _| / _ \ | '__|| '__|/'__'| | '_  ||_   _| \n"),
    io:format("| |     |____|  | |_ ( (_))| |   | |   |  __/ | | | |  | |_  \n"),
    io:format("|_|             |___| \ __/ |_|   |_|   |____| |_| |_|  |___| \n\n\n"),
          

    Manager = spawn(manager, start, []),
    GUI = spawn(gui, start, [Manager]),
    register(gui,GUI).   
    
