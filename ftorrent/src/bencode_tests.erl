%% @author Paulius Vysniauskas
%% @copyright Framework Studio
%% @version v0.1
%% @doc Created: 10-Dec-2011, tests some functions in the bencode module.

-module(bencode_tests).
-include_lib("eunit/include/eunit.hrl").
-import(bencode, [readtorrent/1, divide_byte/2]).

divide_byte_test() ->
    List = [{ip,"88.83.42.146",port,{6892}},
	    {ip,"88.191.129.45",port,{51413}},
	    {ip,"203.177.105.145",port,{26455}},
	    {ip,"216.58.5.72",port,{32178}},
	    {ip,"111.123.32.64",port,{33023}}],
    {{_, [_,_,_,{_,{_, L}}]},_} = readtorrent("Response"),
    Bin = list_to_binary(L),
    ?assertEqual(List, divide_byte(Bin, [])).
    
    
