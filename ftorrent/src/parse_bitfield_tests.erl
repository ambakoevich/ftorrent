%% @author Paulius Vysniauskas
%% @copyright Framework Studio
%% @version v0.1
%% @doc Created: 10-Dec-2011, tests some functions in the parse_bitfield module.

-module(parse_bitfield_tests).
-include_lib("eunit/include/eunit.hrl").
-import(parse_bitfield, [concat_list/1]).

concat_list_test() ->
    {ok, Bin_list} = file:read_file("Bitfield_test"),
    List = binary_to_list(Bin_list),
    ?assertEqual(List, concat_list(<<213,133,117,170,208,42,189,159,19,131,121,17,42,5,37,110,248,229,222,250>>)).
