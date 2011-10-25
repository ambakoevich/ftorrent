%% @author Zarif Jawad
%% @author Batbilig Bavuudorj

%% this module parses .torrent file and retrieves
%% all the neccessary information from it.

-module(torrent_parser).
-compile(export_all).

%% Convert bencode to erlang 
convert_to_erl([]) -> [];
convert_to_erl([$l|Xs]) -> convert_list(Xs, []);
convert_to_erl([$d|Xs]) -> convert_dictionary(Xs, []);
convert_to_erl([$i|Xs]) ->
    {Num, [$e|NotParsed]} = convert_integer(Xs),
    {{num, Num} , NotParsed};
convert_to_erl([X|Xs]) ->
    case char_is_integer(X) of
        true ->
            {Num, [$:|NotParsed]} = convert_integer([X|Xs]),
            {Str, NotParsed2} = lists:split(Num, NotParsed),
            {{str, Str} , NotParsed2};

        false -> wrong_file %% Wrong file input
    end.

%% Convert list of binaries 
convert_list(Xs, Acc) ->
    {Elem, NotParsed} = convert_to_erl(Xs),
    NewAcc = [Elem|Acc],
    case NotParsed of
        [$e|NotParsed2] -> {{liste, lists:reverse(NewAcc)}, NotParsed2};
        _               -> convert_list(NotParsed, NewAcc)
    end.

convert_dictionary(Xs, Acc) ->
    {Elem1, NotParsed1} = convert_to_erl(Xs),
    {Elem2, NotParsed2} =  
                convert_to_erl(NotParsed1),
    NewAcc = [{Elem1, Elem2} | Acc],
    case NotParsed2 of
        [$e|NotParsed3] -> {{dico, lists:reverse(NewAcc)}, NotParsed3};
        _               -> convert_dictionary(NotParsed2, NewAcc)
    end.

char_is_integer(X) ->
    lists:any(fun(Y) -> X == Y end, [$-,$0,$1,$2,$3,$4,$5,$6,$7,$8,$9]).

convert_integer(Xs) ->
    {StrNum, NotParsed} = lists:splitwith(fun char_is_integer/1, Xs),
    Num = erlang:list_to_integer(StrNum),
    {Num, NotParsed}.

%% Read all the information form a torrent file 
open_torrent(FileName) ->
    {ok, S} = file:open(FileName, [read]),
    Result = convert_to_erl(get_lines(S)),
    file:close(S),
    Result.


get_lines(S) -> get_lines(S, []).
get_lines(S, Acc) ->
    Line = io:get_line(S, ''),
    case Line of
        eof -> lists:concat(lists:reverse([Line|Acc]));
        _   -> get_lines(S, [Line|Acc])
    end.



%% erlang->bencode
from_erlang({str, Str}) ->
    Size = erlang:integer_to_list(length(Str)),
    Size ++ ":" ++ Str;
from_erlang({num, Num}) ->
    N = erlang:integer_to_list(Num),
    "i" ++ N ++ "e";
from_erlang({liste, L}) ->
    Ltorrent = lists:map(fun from_erlang/1, L),
    "l" ++ lists:concat(Ltorrent) ++ "e";
from_erlang({dico, D}) ->
    Dtorrent = lists:map(fun({X, Y}) -> from_erlang(X) ++ from_erlang(Y) end, D),
    "d" ++ lists:concat(Dtorrent) ++ "e".

%% Returns info hash part of a torrent file in hexadecimal format
get_hash(Torrent) ->    
    {{dico, Data},_} = open_torrent(Torrent),
    {value, {_, Info}} = lists:keysearch({str, "info"}, 1, Data),
    bin_to_hexstr(sha1(from_erlang(Info))).

%% Used to call the built in sha/1 hashing function   
sha1(X) ->
    crypto:sha(X).
        


%% wtf with utf8 ? [NOT WORKING]
my_split(N, Xs) -> my_split(N, Xs, []).

my_split(0, Rest, Acc) -> {lists:reverse(Acc), Rest};
my_split(N, [], Acc) -> {lists:reverse(Acc), []};
my_split(N, [X,Y|Xs], Acc) when X == 195 -> my_split(N - 1, Xs, [X,Y|Acc]);
my_split(N, [X|Xs], Acc) -> my_split(N - 1, Xs, [X|Acc]).


%% Converting binary to hexadecimal

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a+(N-10).
   
to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

%% Create a string of hexadecimal characters
 
list_to_hexstr([]) -> 
    [];
list_to_hexstr([H|T]) ->
    to_hex(H) ++ list_to_hexstr(T).

bin_to_hexstr(Bin) ->
    list_to_hexstr(binary_to_list(Bin)).

