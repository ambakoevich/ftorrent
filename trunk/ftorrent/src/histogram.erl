-module(histogram).
-export([gen_hist/3, sort_hist/2]).
%%-import(seq_erlang, [new/0, destroy/1, write/3, read/2]).
-import(orddict, [new/0, append/3,find/2,update/3]).


%% A purpose of this function is to join the peer-have-lists. It takes a peer-have-list and an empty dictionary to update the corresponding values to its keys.

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


%% Sort the dictionary by Nth element of the tuple of values 
sort_hist(D, Nth_el) ->
    %% List = orddict:to_list(D),
    lists:keysort(Nth_el, D).


%% Run at shell:
%%
%% Dic = orddict:new().
%% histogram:gen_hist("parrot", Dic).
