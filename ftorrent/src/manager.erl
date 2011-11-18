-module(manager).
-include("constants.hrl").
-compile(export_all).

start(Torrent) ->
    register(manager, spawn(?MODULE, init, [Torrent])),
    register(pm, spawn(piece_manager, start,[])),
    register(io, spawn(io_manager, start,[])),
    manager ! {connect, self()}.

stop() ->
    manager ! {stop, self()},
    receive
	stopped -> stopped
    end.

init(Torrent) ->
    loop(db:start(Torrent)).

loop(Torrent_info) ->
    receive
	{connect, Pid} ->
	  %%  io:format("Connecting~n"),
	    Resp = tracker:start(db:concat()),
%%	    io:format("~p~n", [Resp]),
	    Ips = bencode:get_ip_list(Resp),
	    io:format("~p~n",[Ips]),
            [{ip,Ip,port,{Port}}|_] = Ips,
            io:format("Ip is~p Port is ~p~n",[Ip,Port]),
	    handshake_peers(Ips, [], db:read("InfoHashBinary"), 100),
            loop(Torrent_info);
	{stop, Pid} ->
	    Pid ! stopped
    end.

handshake_peers([{ip, Ip, port, {Port}}|T], Acc, Hash, Limit) when Limit > 0 ->
    %%io:format(" limit is ~p~n",[Limit]),
    %% Pid = spawn(?MODULE,loop,[Ip,Port, Hash, 0]),
    Pid = connection_server:start(Ip,Port, Hash),
    [Pid|Acc],
    %% Pid ! {start},
    handshake_peers(T, Acc, Hash, Limit - 1);

handshake_peers([{ip, Ip, port, {Port}}|T], Acc, Hash, 0) ->
    Acc.



