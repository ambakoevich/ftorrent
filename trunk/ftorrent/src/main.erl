%% @author Zarif Jawad, Paulius Vysniauskas, David Giorgidze.
%% @copyright Framework Studio.
%% @version v0.1
%% @doc Created: 20-Nov-2011. Running main application.

-module(main).
-compile(export_all).
-include("constants.hrl").

start() ->
    Manager = spawn(manager, start, []),
    register(manager,Manager),
    GUI = spawn(gui, start, [Manager]),
    register(gui,GUI).
