-module(parse_bitfield).
-export([decimal_to_bit/2, concat_list/1, define_index/2]).


%% The following codes are used to extract number of 'have' pieces from the bitfield received from a peer reply. 
%%

%% Convert a decimal into a list of bits.

decimal_to_bit(0, Acc) ->
    case length(Acc) of
	8 ->
	    Acc;
	_Other ->
	    decimal_to_bit(0, [0 | Acc])
		end;
decimal_to_bit(Bin, Acc) ->    
    case Bin rem 2 of
	1 ->
	    decimal_to_bit(Bin div 2,[1 | Acc]);
	0 ->
	    decimal_to_bit(Bin div 2, [0 | Acc])
		end.


%% Concatenate lists of bits/numbers. Arguments are a list of binary, and [] as an accumulator.

concat_list(Bin) ->
    L = binary_to_list(Bin),
    concat_list(L, []).
  
concat_list([], Acc) ->
    define_index(Acc, 0);
concat_list([H|T], Acc) ->
    concat_list(T, Acc++decimal_to_bit(H,[])).


%% Find a list of indices/pieces a peer has.

define_index([], _Index) ->
    [];
define_index([H|T], Index) -> 
    case H of
	1 ->
	    [Index | define_index(T, Index+1)];
	0 ->
	    define_index(T, Index+1)
		end.



%% Run from the shell:
%%
%%   parse_bitfield:concat_list(Binary).
%%
