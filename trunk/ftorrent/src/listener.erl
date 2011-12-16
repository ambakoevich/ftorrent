
%% @doc Author: Rashid Darwish
%% @doc Created: 2011-12-11
%% Description: Server listening to the incomings msg from peers

-module(listener).
-export([start/0, init/1]).
-compile(export_all).
-include("constants.hrl").
start() ->
    io:format("~n Uploader Is Running~n"),
    {ok, ListenSocket} = gen_tcp:listen(8888, ?INETHS),
    init(ListenSocket).


init(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    Pid =spawn(?MODULE, init, [ListenSocket]),
    loop(Socket, Pid).


    
loop(Socket, Pid) ->
    case gen_tcp:recv(Socket, ?HANDSHAKE_SIZE) of

	{ok, <<?PSTRLEN, ?PSTR, 
	       _:8/binary, 
	       Info_Hash:20/binary, 
	       PeerID:20/binary>>} -> 
	     io:format("~n~p PID: ~p Wanna Handshake ~n", [list_to_atom(binary_to_list(PeerID)), Pid]),
	    {ok,{Ip,Port}} = inet:peername(Socket),
	    
	    io:format("~n~p PID: ~p ADRESS ~n", [ {ok,{Ip,Port}}, Pid]),
	    acceptHS(Socket, Info_Hash);
	
	    
	   
_ ->  gen_tcp:close(Socket)
	  
	     
    end.

acceptHS(Socket, Info_Hash)->
    ok = gen_tcp:send(Socket, [?PSTRLEN, ?PSTR,
			       ?ACCEPTEDHS,
			       Info_Hash,
			       ?FWSID]),
    %% io:format("~n~p PID: ~p Handshaked ~n", [Socket]),
     inet:setopts(Socket, ?INETBF),
    sendBitfield(Socket),
    inet:setopts(Socket, [{active, true}]),
    run(Socket).

sendBitfield(Socket)->
          %% Bit =  <<255,255,255,255,255,255,255,255,255,255,255>>,
    Bit = 255,
      ok = gen_tcp:send(Socket, <<?BITFIELD,Bit>>).


run(Socket)->
    receive
 	{tcp,_,<<?BITFIELD, BitField/binary>>} -> 
	     %% io:format("~nBITFIELD: ~p~n",[BitField]),
	    run(Socket);

	{tcp,_,<<?HAVE, B:32>>} -> 
	    %% io:format("~nHAVE: ~p~n",[B]),
	    run(Socket);

	{tcp,_,?KEEP_ALIVE} -> 
	    io:format("~nKEEP_ALIVE: ~n"),
	    run(Socket);

	{tcp,_,<<?CHOKE>>} -> 
	    io:format("~nCHOKE: ~n"),
	    run(Socket);

	{tcp,_,<<?UNCHOKE>>} -> 
	    io:format("~nUNCHOKE: ~n"),
	    run(Socket); 

	R->
	    io:format("~n>> ~p~n",[R]),
   run(Socket)
		end.


