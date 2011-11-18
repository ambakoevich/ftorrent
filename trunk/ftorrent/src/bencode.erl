-module(bencode).
-compile(export_all).

%% bencode->erlang
to_erlang([]) -> [];
to_erlang([$l|Xs]) -> parse_list(Xs, []);
to_erlang([$d|Xs]) -> parse_dico(Xs, []);
to_erlang([$i|Xs]) ->
    {Num, [$e|NotParsed]} = parse_integer(Xs),
    {{num, Num} , NotParsed};
to_erlang([X|Xs]) ->
    case char_is_integer(X) of
        true ->
            {Num, [$:|NotParsed]} = parse_integer([X|Xs]),
            {Str, NotParsed2} = lists:split(Num, NotParsed),
            {{str, Str} , NotParsed2};

        false -> wrong_file % lever une exception ?
    end.


parse_list(Xs, Acc) ->
    {Elem, NotParsed} = to_erlang(Xs),
    NewAcc = [Elem|Acc],
    case NotParsed of
        [$e|NotParsed2] -> {{liste, lists:reverse(NewAcc)}, NotParsed2};
        _               -> parse_list(NotParsed, NewAcc)
    end.

parse_dico(Xs, Acc) ->
    {Elem1, NotParsed1} = to_erlang(Xs),
    {Elem2, NotParsed2} =
        % pieces pose certaines problemes avec l'utf8...
        %case Elem1 of
            %{str, "pieces"} -> {{str, NotParsed1}, "ee"};
            %_        ->
                to_erlang(NotParsed1),
        %end,
    %io:format("~p -> ~p~n", [Elem1, Elem2]),
    %io:get_line("continue?"),
    NewAcc = [{Elem1, Elem2} | Acc],
    case NotParsed2 of
        [$e|NotParsed3] -> {{dico, lists:reverse(NewAcc)}, NotParsed3};
        _               -> parse_dico(NotParsed2, NewAcc)
    end.

char_is_integer(X) ->
    lists:any(fun(Y) -> X == Y end, [$-,$0,$1,$2,$3,$4,$5,$6,$7,$8,$9]).
parse_integer(Xs) ->
    {StrNum, NotParsed} = lists:splitwith(fun char_is_integer/1, Xs),
    Num = erlang:list_to_integer(StrNum),
    {Num, NotParsed}.

readtorrent(FileName) ->
    {ok, S} = file:open(FileName, [read]),
    Result = to_erlang(get_lines(S)),
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


info_hash(Torrent) ->    
    {{dico, Data},_} = readtorrent(Torrent),
    {value, {_, Info}} = lists:keysearch({str, "info"}, 1, Data),
    %%bin_to_hexstr(sha1(from_erlang(Info))).
    %%io_lib:format("~s", [sha1(from_erlang(Info))]).
      sha1(from_erlang(Info)).
   

sha1(X) ->
    %% crypto:start(),
    crypto:sha(X).
    %% crypto:stop(),
    %% S.

%peer_id() ->
    


%% wtf with utf8 ? [NOT WORKING]
my_split(N, Xs) -> my_split(N, Xs, []).

my_split(0, Rest, Acc) -> {lists:reverse(Acc), Rest};
my_split(N, [], Acc) -> {lists:reverse(Acc), []};
my_split(N, [X,Y|Xs], Acc) when X == 195 -> my_split(N - 1, Xs, [X,Y|Acc]);
my_split(N, [X|Xs], Acc) -> my_split(N - 1, Xs, [X|Acc]).


%%%CHanging from binary to 

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a+(N-10).
   
to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].
 
list_to_hexstr([]) -> 
    [];
list_to_hexstr([H|T]) ->
    to_hex(H) ++ list_to_hexstr(T).

bin_to_hexstr(Bin) ->
    list_to_hexstr(binary_to_list(Bin)).

%%%Here ends the original

%% Take a binary and return a list of tuples divided into 32-bit and 16-bit integer chunks.  
divide_byte(<<>>, Acc) ->
   lists:reverse(Acc);

divide_byte(<<E1:8,E2:8,E3:8,E4:8, Port:16, Rest/binary>>, Acc) ->
     IP = merge_ip(E1,E2,E3,E4),
     divide_byte(Rest, [{ip,IP, port,{Port}} | Acc]).

merge_ip(E1, E2, E3, E4) ->
    integer_to_list(E1) ++ "." ++ integer_to_list(E2) ++ "." ++ integer_to_list(E3) ++ "." ++ integer_to_list(E4).

get_ip_list(Tracker_res)->
    {{dico, Data},_} = to_erlang(Tracker_res),
    {value, {{str,_},{str,Peer_list}}}= lists:keysearch({str, "peers"},1,Data),
    divide_byte(list_to_binary(Peer_list),[]).
