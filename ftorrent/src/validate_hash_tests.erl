-module(validate_hash_tests).
-include_lib("eunit/include/eunit.hrl").
-import(validate_hash, [find_hash/2, get_hash/1, hash_it/1]).

find_hash_test_() ->
    Bin = get_hash("test3.png.torrent"),
    Hash = hash_it("test3"),
    [?_assert(find_hash(Bin, Hash))].
