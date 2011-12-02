%% @author Batbilig Bavuudorj, David Giorgidze
%% @copyright Framework Studio
%% @version v0.1
%% @doc Creating Date: 24-Nov-2011, Filter is used to filter out values of one table/list
%% from other tables/lists. It provides a function which
%% joins two lists to one and sorts it later.

-module(filter).
-export([find/2, filter_values/2, sort_added/2, filter_peer_set/2, join_set/2, get_rarest/1, update_have/4, lookup/2]).
-import(histogram, [gen_hist/3, sort_hist/2]).
-compile(export_all).

%% @doc Check if the given element exists in the list
find([], _) ->
    false;
find([E|_T], E) ->
    true;
find([_H|T], E) ->
    find(T, E).

%% @doc Filter out all occurrences/elements of list two from list one
%% and return list one
filter_values([], _Mine) ->
    [];
filter_values([H|T], Mine) ->
    case find(Mine, H) of
	true ->
	    filter_values(T, Mine);
	false ->
	    [H | filter_values(T, Mine)]
    end.

%% @doc Add list two to list one and sort the list one, and return the joined list
sort_added(L, []) ->
    lists:sort(L);
sort_added(L, [H|T]) ->
    sort_added([H|L], T).


%% @doc Filter out a given list('pieces we have') from all the set of peers/lists.
%% This would return a list of 'wish pieces' from each peer or 'the pieces we need'
filter_peer_set([], _L) ->
    [];
filter_peer_set([{Peer, Pid} | Rest_peer], [{L,Mpid}]) ->
    [{lists:sort(filter_values(Peer, L)),Pid} | filter_peer_set(Rest_peer, [{L,Mpid}])].

%% @doc Join all the peers' filtered-have-list into an histogram dictionary
%% and return this dictionary
join_set(Peers,Me) ->
    Dic = orddict:new(),
    Wish_list = filter_peer_set(Peers, Me),
    join_set2(Wish_list, Dic).

join_set2([], Dic) ->
    Dic;
join_set2([{H, Pid}|T], Dic) ->
    Dic_updated = gen_hist(H, Dic, Pid),
    join_set2(T, Dic_updated).

%% @doc Get the rarest set by sorting the given dictionary in ascending order.
%% N = 2 in this case. 
get_rarest(D) ->
    sort_hist(D, 2).

%% @doc Update the given peer's(pid) list by finding the peer and
%% adding a new 'piece'. Return the list of all peers have-list.
%% Takes a list of peers, piece number, pid, and an accumulator/'[]'    
update_have([], _Piece, _Pid, Acc) ->
    Acc;
update_have([{H, Pid} | Rest_peers], Piece, Pid, Acc) ->
    update_have(Rest_peers, Piece, Pid, [{[Piece|H], Pid}|Acc]);
update_have([{H, Another_Pid} | Rest_peers], Piece, Pid, Acc) ->
    update_have(Rest_peers, Piece, Pid, [{H,Another_Pid}|Acc]).

%% @doc Lookup a peer-have-list by Pid number in a list of peers.
%% If finds one return it else return 'pid_not_there' msg
lookup([], _Pid) ->
    pid_not_there;

lookup([{Pieces, [{_Copy, Pid_list}]} | Rest], Pid) ->
    case find(Pid_list, Pid) of
	true -> {Pieces, [{_Copy, Pid_list}]};
	false -> lookup(Rest, Pid)
    end.


%% 'Run at shell':
%%
%% L1 = {[1,2,3,45,6],pid1}.
%% L2 = {[1,2,5,7,8,3],pid2}.
%% Me = [{[1,3,45],mypid}], ME = {[1,3,45],mypid}
%% Peers= [L1,L2].
%%
%% (This is optional.. filter:filter_peer_set(Peers, Me).)
%%
%% 
%%
%% D = filter:join_set(Peers,Me). 
%% filter:get_rarest(D).
%%
%% ALL = [L1,L2, ME].
%% filter:update_have(ALL, 88, mypid, []). 
