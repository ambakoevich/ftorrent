%% Author: Rashid Darwish
%% Created: 2011-10-20
%% Description: Establishing the connection with torrents trackers

-module(tracker).
-export([start/1]).

 %% Running the module
start(URL)->
     io:format("URL RES: ~p~n",[URL]),
    get_request(URL).

 %% This function will call inets OTP and run the start function, 
 %% inorder to use httpc:request which is http get request
get_request(Url)-> 
    inets:start(),
    {ok, Result} = httpc:request(Url),
    {_,_,Data} = Result,
    io:format("TRACKAR RES: ~p~n",[Data]),
    Data.




