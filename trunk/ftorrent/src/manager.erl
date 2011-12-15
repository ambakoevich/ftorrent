%% @author Zarif Jawad, Paulius Vysniauskas, David Giorgidze
%% @copyright Framework Studio
%% @version v0.1
%% @doc Created: 20-Nov-2011, Manager is the process which actually starts the application.
%% It sends request to the tracker, parses the response to get active
%% peers list and handshakes them.

-module(manager).
-include("constants.hrl").
-compile(export_all).

%% @doc Start the manager. Calling init function
start() ->
    init().

%% @doc Stop the manager	               
stop(Pid) ->
    Pid ! {stop, self()},
    receive
	stopped -> stopped
    end.

%% @doc Initialize the loop and create ETS table. Start the main loop
init() ->
    loop(ets:new(temp,[])).

%% @doc Main loop. Receives messages and initializes connections
loop(Torrent_info) ->
    receive
	{start_manager,Torrent, GUI_Pid, Fpath}->
            Torrent_info_new = db:start(Torrent),
	    io:format("Starting manager~n"),
	    register(pm,A = spawn(piece_manager, start,[])),
	    register(io,B = spawn(io_manager, start,[Fpath])),
	    link(A),
	    link(B),
	    pm ! {ok,db:retreive_hash_binary(Torrent)},
	    GUI_Pid ! {table, Torrent_info_new},
	    loop(Torrent_info_new);
	{connect, GUI_Pid} ->
	    io:format("Connecting~n"),
	    Resp = tracker:start(db:concat()),
	    %%io:format("~p~n", [Resp]),
	    Ips = bencode:get_ip_list(Resp),
	    io:format("~p~n",[Ips]),
            [{ip,Ip,port,{Port}}|_] = Ips,
            io:format("Ip is~p Port is ~p~n",[Ip,Port]),
	    Peer_list = handshake_peers(Ips, [], db:read("InfoHashBinary"), 10),
	    io:format("***THE LIST*** ~p~n", [Peer_list]),
            GUI_Pid ! {peer_list, Peer_list},
	    loop(Torrent_info);
	{stop} ->
	    io:format("Manager being killed"),
	    stopped
    end.

%% @doc Do handshake with all peers in the given list. Return peers IP list
handshake_peers([{ip, Ip, port, {Port}}|T], Acc, Hash, Limit) when Limit > 0 ->
    %%io:format(" limit is ~p~n",[Limit]),
    %% Pid = spawn(?MODULE,loop,[Ip,Port, Hash, 0]),
    Pid = connection_server:start(Ip,Port, Hash),
    link(Pid),
    List = [Ip|Acc],
    %% Pid ! {start},
    handshake_peers(T, List, Hash, Limit - 1);

handshake_peers([{ip, Ip, port, {Port}}|T], Acc, Hash, 0) ->
    Acc;
handshake_peers([], Acc, Hash, Limit) ->
    Acc.
