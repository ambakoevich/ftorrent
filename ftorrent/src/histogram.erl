%% @author Batbilig Bavuudorj
%% @copyright Framework Studio
%% @version v0.1
%% @doc Created: 19-Nov-2011, A purpose of this module
%% is to join the peer-have-lists. It takes a peer-have-list
%% and an empty dictionary to update the corresponding values to its keys.


-module(histogram).
-export([gen_hist/3, sort_hist/2]).
-import(orddict, [new/0, append/3,find/2,update/3]).

%% @doc Generate general purpose histogram
gen_hist([], D, _) ->
    D;
gen_hist([H|T], D, Pid) ->
    case find(H, D) of
    	error ->
	    D2 = append(H, {1, [Pid]}, D),
    	    gen_hist(T, D2, Pid);
    	{ok, _} ->
	    D2 = update(H, fun([{Old_head, Old_Pid}|_tail]) -> [{Old_head+1, Old_Pid++[Pid]}] end, D),
    	    gen_hist(T, D2, Pid)
    end.


%% @doc Sort the dictionary by Nth element of the tuple of values 
sort_hist(D, Nth_el) ->
    %% List = orddict:to_list(D),
    lists:keysort(Nth_el, D).
