-module(filter_tests).
-include_lib("eunit/include/eunit.hrl").
-import(filter, [find/2, filter_values/2, sort_added/2, filter_peer_set/2, join_set/2, get_rarest/1, update_have/4, lookup/2]).

get_rarest_test_() ->
    L1 = {[1,2,3,45,6],pid1},
    L2 = {[1,2,5,7,8,3],pid2},
    Me = [{[1,3,45],mypid}],
    Peers= [L1,L2],
    D = filter:join_set(Peers,Me),
    Check = [{6,[{1,[pid1]}]},
	     {5,[{1,[pid2]}]},
	     {7,[{1,[pid2]}]},
	     {8,[{1,[pid2]}]},
	     {2,[{2,[pid1,pid2]}]}],
    [?_assertMatch(Check, get_rarest(D))].
