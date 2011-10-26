%% Author: Rashid Darwish
%% Created: 2011-10-20
%% Description: Establishing the connection with torrents trackers

-module(tracker).
-export([start/0]).

 %% Running the module
start()->
    URL = "http://torrent.ubuntu.com:6969/announce?info_hash=%c5hi%e1%84%88%1e%f7%a4E%88:6%f4_ZC%d1%dfK&peer_id=-FWS0001-10000000001&port=8888&uploaded=0&downloaded=0&left=0&compact=1",
    get_request(URL).

 %% This function will call inets OTP and run the start function, 
 %% inorder to use httpc:request which is http get request
get_request(Url)-> 
    inets:start(),
    {ok, Result} = httpc:request(Url),
    {_,_,Data} = Result,
    print_to_file(Data).
   
 %% Temp Function to test the module by printing the tracker 
 %% Response into file 
 print_to_file(Data)->
 {ok, File} = file:open("Tracker Response", write),
   file:write(File, Data),
   file:close(File).