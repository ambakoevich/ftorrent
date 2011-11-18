-define(PSTRLEN, 19).
-define(PSTR, "BitTorrent protocol").
-define(FWSID, "-FWS0001-10000000001").
-define(INETBF,  [{packet, 4}, {active, true}, {packet_size, 33000}]).
-define(INETHS, [binary, {packet, 0}, {active, false}]).
-define(RESERVED, <<0,0,0,0,0,0,0,0>>).
-define(BITFIELD_TIMEOUT, 20000).
-define(HANDSHAKE_SIZE, 68).
-define(FWSPORT,"8888").
-define(LENGTH, 16384).
-define(BEGIN,0).


%% Used in the server 
-define(ACCEPTEDHS, <<0,0,0,0,0,16,0,5>>).
-define(RESERV, 0:8).

%% Messages
-define(KEEP_ALIVE, <<>>).
-define(CHOKE, 0:8).
-define(UNCHOKE, 1:8).
-define(INTERESTED, 2:8).
-define(NOT_INTERESTED, 3:8).
-define(HAVE, 4:8).
-define(BITFIELD, 5:8).
-define(REQUEST, 6:8).
-define(PIECE, 7:8).
-define(CANCEL, 8:8).
