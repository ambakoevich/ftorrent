%% @author Rashid Darwish
%% @copyright Framework Studio
%% @version v0.1
%% @doc Created: 20-Oct-2011, Tracker establishes connection with torrent trackers.

-module(tracker).
-export([start/1]).

%% @doc Starting the module
start(URL)->
    io:format("URL RES: ~p~n",[URL]),
    get_request(URL).

%% @doc get_request will call inets OTP and run the start function, 
%% inorder to use httpc:request which is http get request
get_request(Url)-> 
    inets:start(),
    {ok, Result} = httpc:request(Url),
    {_,_,Data} = Result,
    io:format("TRACKAR RES: ~p~n",[Data]),
    Data.




