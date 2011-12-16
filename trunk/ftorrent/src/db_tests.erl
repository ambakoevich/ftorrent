%% @author Paulius Vysniauskas
%% @copyright Framework Studio
%% @version v0.1
%% @doc Created: 10-Dec-2011, tests some functions in the database module.

-module(db_tests).
-include_lib("eunit/include/eunit.hrl").
-import(db, [encode_hash/1, parse_path/1]).

encode_hash_test() ->
    %%"86ca4eb985dd7c0d84eb55b681a8d850824cf712"
    String = "%86%ca%4e%b9%85%dd%7c%0d%84%eb%55%b6%81%a8%d8%50%82%4c%f7%12",
    ?assertEqual(String, encode_hash("86ca4eb985dd7c0d84eb55b681a8d850824cf712")).

%%parse_path_test() ->
    %%Result = readtorrent("larry lessig - the future of ideas.torrent"),
    %%{dico, Data} = Result.
