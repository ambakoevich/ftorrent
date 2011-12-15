%% @author Paulius Vysniauskas
%% @copyright Framework Studio
%% @version v0.1
%% @doc Created: 10-Dec-2011, tests the tracker request. Gets two
%% identical responses from same tracker and compares them.

-module(tracker_tests).
-include_lib("eunit/include/eunit.hrl").
-import(tracker, [start/2, get_request/1]).

%% @doc Test the tracker response.
get_request_test() ->
    Url = "http://torrent.ubuntu.com:6969/announce?info_hash=%c5hi%e1%84%88%1e%f7%a4E%88:6%f4_ZC%d1%dfK&peer_id=-FWS0001-10000000001&port=8888&uploaded=0&downloaded=0&left=0&compact=1",
    inets:start(),
    {ok, Result} = httpc:request(Url),
    {_,_,Data} = Result,
    Data =:= get_request(Url).
