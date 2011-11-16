%% Author: jawza
%% Created: Nov 16, 2011
%% Description: TODO: Add description to manager
-module(manager).

%%
%% Include files
%%

%%
%% Exported Functions
%%

-compile(export_all).

%%
%% API Functions
%%



%%
%% Local Functions

%%

start(Torrent) ->
    register(manager, spawn(?MODULE, init, [Torrent])),
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
	    %%io:format("Connecting~n"),
	    Resp = tracker:start(db:concat()),
	    %%io:format("~p~n", [Resp]),
	    Ips = bencode:get_ip_list("Tracker Response"),
	    %%io:format("~p~n",[Ips])
            [{ip,Ip,port,{Port}}|_] = Ips,
            io:format("Ip is~p Port is ~p~n",[Ip,Port]),
	    handshake_peers(Ips, [], db:read("Info Hash Binary"), 5),
            loop(Torrent_info);
	{stop, Pid} ->
	    Pid ! stopped
    end.

handshake_peers([{ip, Ip, port, {Port}}|T], Acc, Hash, Limit) when Limit > 0 ->
    io:format(" limit is ~p~n",[Limit]),
    Pid = spawn(?MODULE,handshake,[Ip,Port, Hash]),
    [Pid|Acc],
    Pid ! {start},
    handshake_peers(T, Acc, Hash, Limit - 1);

handshake_peers([{ip, Ip, port, {Port}}|T], Acc, Hash, 0) ->
    Acc.

handshake(Ip, Port, Hash) ->
    receive
	{start} ->  
	    {ok,Socket} = connection_mngr:connect(Ip,Port),
	    io:format("Socket is~p~n",[Socket]),   
            Resp1 = connection_mngr:sendHandShake(Socket, Hash),
            io:format("This is step 1~p~n",[Resp1]),
            Resp2 = connection_mngr:recv(Socket, Hash),
	    io:format("This is step 2~p~n",[Resp2]),
	    io:format("Before server conn~n"),
            Resp3 = connection_server:start(Socket),
	    io:format("Server conn ~p~n", [Resp3]),
	    handshake(Ip,Port,Hash)
    end.

