%% Author: Zarif Jawad,Paulius Vysniauskas,David Giorgidze
%% Created: 2011-11-20
%% Comment: Main Manager.
-module(manager).
-include("constants.hrl").
-compile(export_all).

start() ->
    init().
	        
stop(Pid) ->
    Pid ! {stop, self()},
    receive
	stopped -> stopped
    end.

init() ->
    loop(ets:new(temp,[])).

loop(Torrent_info) ->
    receive
	{start_manager,Torrent, GUI_Pid}->
            Torrent_info_new = db:start(Torrent),
	    io:format("Starting manager~n"),
	    register(pm,A = spawn(piece_manager, start,[])),
	    register(io,B = spawn(io_manager, start,[])),
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
