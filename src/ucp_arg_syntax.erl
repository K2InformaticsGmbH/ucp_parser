%% -------------------------------------------------------------------
%%
%%
%% Copyright (c) 2012 Mobile Interactive Group a Velti company. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%%% @author Roland Karlsson

%%% @doc
%%%  This module defines the parameter syntax for
%%%  EMI/UCP messages.
%%% @end

%%% @type opts() = [{Key::atom(),Value::term()}]

%%% @type time() = {{Year,Month,Day},{Hour,Minute,Second}}
%%%  Year = integer()
%%%  Month = integer()
%%%  Day = integer()
%%%  Hour = integer()
%%%  Minute = integer()
%%%  Second = integer()
%%% @end

%%% @type xser() = {Type::integer(), Data}
%%%   Data = binary() | string() | xsers()
%%% @type xsers() = [xser()]

-module(ucp_arg_syntax).

-include("ucp_defines.hrl").


%%% API EXPORTS

-export([make_num/2, make_hex/2, make_hex_coded/1]).

-export([make_time/2]).

-export([make_addr/1, make_ip_v4/1]).

-export([make_xsers/1, make_xser/1]).

-export([make_pack7/1, pack7/1]).

-export([make_checksum/1]).

-export([parse_num/1, parse_hex/1, parse_hex_coded/1]).

-export([parse_time/1]).

-export([parse_ip_v4/1]).

-export([parse_xsers/1]).

-export([parse_pack7/1, unpack7/1]).

-export([split_sm/3]).


%%% TEST EXPORTS

-export([bin8rev/1]).


%%% CONSTANTS

-define(BITS_IN_A_BYTE, 8).

%%% TODO: With pack7 coding it is probably 80. Without packing it
%%% would be 70.
-define(MAX_SM_SIZE, 80).

%%% MACROS


%%% RECORDS


%%% EXPORTED FUNCTIONS

%%% @doc Make a number of fixed size with leading zeroes.
%%% @spec make_num(Length::integer(), Int::integer()) -> string()
make_num(Length, Int) ->
    add_leading_zeroes(Length, erlang:integer_to_list(Int, 10)).

%%% @doc Make a hex number of fixed size with leading zeroes.
%%% @spec make_hex(Length::integer(), Int::integer()) -> string()
make_hex(Length, Int) ->
    add_leading_zeroes(Length, erlang:integer_to_list(Int, 16)).

%%% @doc Make hex coded text/data.
%%% @spec make_hex_coded(Text|Binary) -> string()
%%%  Text = string()
%%%  Binary = binary()
%%% @end
make_hex_coded(Binary) when is_binary(Binary) ->
    make_hex_coded(erlang:binary_to_list(Binary));
make_hex_coded(Text) ->
    lists:flatten([make_hex(2,C) || C <- Text]).

%%% @doc Make hex coded checksum.
%%% @spec make_checksum(String::string()) -> string()
make_checksum(String) ->
    make_hex(2,lists:sum(String) rem 256).

%%% @doc Make time.
%%%  10 and 12 char long, YYMMDDHHMM or YYMMDDHHMMSS
%%% @end
%%% @spec make_time(Length::integer(), Time::time()) -> string()
make_time(Length, {{Y, Mo, D},{H, Mi, S}}) ->
    make_num(2,D) ++
        make_num(2,Mo) ++
        make_num(2,Y rem 100) ++
        make_num(2,H) ++
        make_num(2,Mi) ++
        case Length of
            10 ->
                "";
            12 ->
                make_num(2,S)
        end.


%%% @doc Make address.
%%%  This code assumes that if you find a dot, its a V4 IP
%%%  address. Otherwise it shall not be converted
%%% @end
%%% @spec make_addr(Addr::string()) -> string()
make_addr(Addr) ->
    case string:chr(Addr, $:) of
        0 ->
            Addr;
        _ ->
            make_ip_v4(Addr)
    end.

%%% @doc Make IP address.
%%% @spec make_ip_v4(Addr::string()) -> string()
make_ip_v4(Addr) ->
    [IP, Port] =
        case string:chr(Addr, $:) of
            0 ->
                [Addr, ""];
            _ ->
                string:tokens(Addr, ":")
        end,
    IPL = string:tokens(IP, "."),
    [IP1, IP2, IP3, IP4] = [ add_leading_zeroes(3, N) || N <- IPL ],
    IP1 ++ IP2 ++ IP3 ++ IP4 ++ Port.

%%% @doc Make xsers string.
%%% @spec make_xsers(Xs::xsers()) -> string()
make_xsers(Xs) ->
    lists:flatten([make_xser(X) || X <- Xs ]).

%%% @doc Make xser string.
%%% @spec make_xser(Xs::xser()) -> string()
make_xser(#{tt := Type, ll := Len, dd := Data}) ->
    make_xser(Type, Len, Data);
make_xser(#{<<"tt">> := Type, <<"ll">> := Len, <<"dd">> := Data}) ->
    make_xser(Type, Len, Data).

make_xser(Type, Len, Data) ->
    make_hex(2, Type) ++ make_hex(2, Len) ++ make_hex_coded(Data).

%%% @doc Make 7 bit packet string.
%%%  This packing is according to the 3G TS 23.038
%%% @end
%%% @spec make_pack7(Unpacked) -> Packed::string()
%%%  Unpacked = binary() | string()
%%% @end
make_pack7(Unpacked) when is_list(Unpacked) ->
    make_hex_coded(pack7(erlang:list_to_binary(Unpacked)));
make_pack7(Unpacked) ->
    make_hex_coded(pack7(Unpacked)).

%%% @doc Make 7 bit packet string.
%%%  This packing is according to the 3G TS 23.038
%%% @end
%%% @spec pack7(Unpacked::binary()) -> Packed::binary()

%%% ======================================
%%% Some words regarding pack7 and unpack7
%%% ======================================

%%% The code is quite simple an looks like magic. The specifiaction in
%%% the 3G standard looks much more complicated. But - the complexity
%%% is only a matter of using the wrong endian when doing the
%%% specification. The data is packed from 8 to 7 bits in a little
%%% endian fashion (on bit and byte level). Bits, 7bytes and 8bytes
%%% all are least significant first. And then you fill the extra bits
%%% with zeroes. Unfortunately Erlang bitstrings also (like the
%%% description) are big endian. So - in order to make the code simple
%%% and magical the bitstring has to be reversed bytewise - and then
%%% reversed back in the end.

%%% One complication is when there are exactly 7 bits to fill. When
%%% unpacking those 7 bits is going to end up as an extra 0 byte. The
%%% solution according to the spec is in this case to fill with an
%%% extra CR (0001101) instead of the 0.

%%% This will create another complication. When the last character is
%%% CR and the packing is exactly modulo 8 bits, then you have to add
%%% an extra CR (padded with one zero) . Then the reciever will get 2
%%% CR at the end of the message - this is assumed to be OK.

%%% Then there is no more complications.

%%% TODO: this code is compact, elegant and easy to understand. But it
%%% is very inefficient. You have to reverse the data twice and you
%%% also have to add some bits in the beginning of the bitstring. The
%%% latter will have to move and shift data. A try to write an
%%% efficient code has failed due to a messy and hard to understand
%%% code. The endian mismatch makes it hard to code.
%%%
%%% Fortunately the UCP code does not have to do this for the entire
%%% message as this is communicated unpacked between the client and
%%% the SMSC. It is only used for the oadc field in the 5x
%%% operations. The data is also static - so it could be built once
%%% and then passed to the library coded. Then the inefficiency is not
%%% important.
%%%
%%% A relevant question is why the UCP protocol has to pack7 this
%%% field when no other field (that is packed in the actual SMS) needs
%%% to be packed in UCP. Probably a miss in some standardization
%%% meeting. An expensive miss in that case.

pack7(Unpacked) ->
    Reversed = bin8rev(Unpacked),
    Packed = << <<X:7>> || <<X:8>> <= Reversed >>,
    Padded =
        case erlang:bit_size(Packed) rem 8 of
            0 ->
                case Packed of
                    <<$\r:7, _/bits>> ->
                        <<0:1, $\r:7, Packed/bits>>;
                    _ ->
                        Packed
                end;
            1 ->
                <<$\r:7, Packed/bits>>;
            Rest ->
                <<0:(8-Rest), Packed/bits>>
        end,
    bin8rev(Padded).

%%% @doc Convert decimal strings to integer.
%%% @spec parse_num(String::string()) -> integer()
parse_num(String) ->
    erlang:list_to_integer(String, 10).

%%% @doc Convert hexadecimal strings to integer.
%%% @spec parse_hex(String::string()) -> integer()
parse_hex(String) ->
    erlang:list_to_integer(String, 16).

%%% @doc Convert hex coded text/data to text/data.
%%% @spec parse_hex_coded(String::string()) -> Text::string()
parse_hex_coded(String) ->
    parse_hex_coded(String, []).

parse_hex_coded([], Ack) ->
    Ack;
parse_hex_coded([A, B | T], Ack) ->
    parse_hex_coded(T, Ack ++ [parse_hex([A,B])]).

%%% @doc Convert time string to Erlang time structure.
%%%  NOTE: the year is two figures only, which is not
%%%  compatible with Erlang time.
%%% @end
%%% @spec parse_time(Time::string()) -> time()
parse_time([D1,D2,Mo1,Mo2,Y1,Y2,H1,H2,Mi1,Mi2]) ->
    parse_time([D1,D2,Mo1,Mo2,Y1,Y2,H1,H2,Mi1,Mi2,$0,$0]);
parse_time([D1,D2,Mo1,Mo2,Y1,Y2,H1,H2,Mi1,Mi2,S1,S2]) ->
    {{parse_num([Y1,Y2]),
      parse_num([Mo1,Mo2]),
      parse_num([D1,D2])},
     {parse_num([H1,H2]),
      parse_num([Mi1,Mi2]),
      parse_num([S1,S2])}}.

%%% @doc Convert IP V4.
%%% @spec parse_ip_v4(Address::string()) -> string()
parse_ip_v4([IP11,IP12,IP13,
             IP21,IP22,IP23,
             IP31,IP32,IP33,
             IP41,IP42,IP43 | Port]) ->
    IP1 = strip_leading_zeroes([IP11,IP12,IP13]),
    IP2 = strip_leading_zeroes([IP21,IP22,IP23]),
    IP3 = strip_leading_zeroes([IP31,IP32,IP33]),
    IP4 = strip_leading_zeroes([IP41,IP42,IP43]),
    case Port of
        "" ->
            IP1 ++ "." ++ IP2 ++ "." ++ IP3 ++ "." ++ IP4;
        _ ->
            IP1 ++ "." ++ IP2 ++ "." ++ IP3 ++ "." ++ IP4 ++ ":" ++ Port
    end.

%%% @doc Convert XSER.
%%%  Extra Services
%%%     With the XSer field, one or more additional services can be specified.
%%%     These services consist of IRA encoded data constructed in the
%%%     following common format: TTLLDD…
%%%     TT: represents two HEX characters defining the type of service. For a
%%%     description of available services refer to section “XSer Extra Services”
%%%     LL: represents two HEX characters defining the number of octets
%%%     present in the data field DD. (Note that the number of HEX characters
%%%     in the data DD is twice the number of octets)
%%%     DD: represents a stream of HEX characters defining the service
%%%     specific data itself.
%%%     If more than one additional service is to be specified in one message,
%%%     this service information is concatenated without any separators, i.e.
%%%     TT1LL1DD1…DD1TT2LL2DD2..DD2
%%%     The above construction is designed such that in the future additional
%%%     service types can be added to the XSer field. 
%%% @end
%%% @spec parse_xsers(Xs::string()) -> xsers()
parse_xsers(Xs) when length(Xs) rem 2 =:= 1 ->
    parse_xsers(Xs ++ "0");
parse_xsers(Xs) ->
    parse_xsers(Xs, first).

parse_xsers([], _) ->
    [];
parse_xsers([T1,T2,L1,L2 | Xs], Level) ->
    T = parse_hex([T1,T2]),
    L = parse_hex([L1,L2]),

    {HexData, Xs2} = lists:split(2*L, Xs),
    Xser =
        case {Level, T} of
            % NOTE: This is the only nested XSER
            {first, ?UCP_XSER_SERVICE_XDMA_CALL_BACK_NUMBER} ->
                #{tt => T, ll => L, dd => parse_xsers(HexData, notfirst)};
            _ ->
                #{tt => T, ll => L, dd => parse_hex_coded(HexData)}
        end,

    [ Xser | parse_xsers(Xs2, Level)].


%%% @doc Parse 7 bit packet string.
%%%  This packing is according to the 3G TS 23.038
%%% @end
%%% @spec parse_pack7(Packed::string()) -> Unpacked::string()
parse_pack7(Packed) ->
    BinPacked = erlang:list_to_binary(parse_hex_coded(Packed)),
    erlang:binary_to_list(unpack7(BinPacked)).

%%% @doc Unpack 7 bit packet binary.
%%%  This packing is according to the 3G TS 23.038
%%% @end
%%% @spec unpack7(Packed::binary()) -> Unpacked::binary()

%%% See the comment for pack7/1.

unpack7(Packed) ->
    Reversed = bin8rev(Packed),
    Unpadded =
        case erlang:bit_size(Packed) rem 7 of
            0 ->
                case Reversed of
                    <<$\r:7, R/bits>> ->
                        R;
                    _ ->
                        Reversed
                end;
            Rest ->
                <<0:Rest, U/bits>> = Reversed,
                U
        end,
    Unpacked = << <<X:8>> || <<X:7>> <= Unpadded >>,
    bin8rev(Unpacked).

%%% @doc Split an SM.
%%%
%%%  Splits the SM in chunks (of type string()) that are small enough
%%%  to be sent as an SM. The size of each chunk in bits (NB) is also
%%%  provided.
%%%
%%%  For binary/transparent data the user has to provide
%%%  the nb options telling how many of the bets are valid.
%%%
%%%  It is also possible to provide ports - which is the other subfield
%%%  in the UDC service.
%%% @end
%%% @spec split_sm(RefNr, SM, Opts) -> [{X, Part, PartSize}]
%%%  RefNr = integer()
%%%  Opts = opts()
%%%  SM = string() | binary()
%%%  PartSize = integer()
%%%  X = xser()
%%%  Part = string()
%%%  Length = integer()
%%% @end
split_sm(RefNr, SM, Opts) when is_binary(SM) ->
    split_sm(RefNr, erlang:binary_to_list(SM), Opts);
split_sm(RefNr, SM, Opts) ->
    case lists:keymember(nb, 1, Opts) of
        true ->
            {value, {nb,NB}} = lists:keysearch(nb, 1, Opts),
            Chunks = (length(SM) + ?MAX_SM_SIZE - 1) div ?MAX_SM_SIZE,
            split_sm(RefNr, SM, NB, ?MAX_SM_SIZE, 1, Chunks, Opts);
        false ->
            NB = length(SM) * ?BITS_IN_A_BYTE,
            split_sm(RefNr, SM, Opts ++ [{nb, NB}])
    end.

%%% @doc Reverses a binary , byte by byte.
%%% @spec bin8rev(Bin::binary()) -> RevBin::binary()
bin8rev(Bin) ->
    bin8rev(Bin, <<>>).

bin8rev(<<>>, Acc) ->
    Acc;
bin8rev(<<H, T/binary>>, Acc) ->
    bin8rev(T, <<H, Acc/binary>>).


%%% PRIVATE FUNCTIONS

%%% Two help functions for adding and removing zeroes

add_leading_zeroes(Length, String) ->
    L = length(String),
    if
        L == Length ->
            String;
        L < Length ->
            string:chars($0, Length-L, String)
    end.

strip_leading_zeroes([N]) ->
    [N];
strip_leading_zeroes([$0|Ns]) ->
    strip_leading_zeroes(Ns);
strip_leading_zeroes(Ns) ->
    Ns.


%%% Help for split_sm/3

split_sm(_,[], _, _, _, _, _) ->
    [];
split_sm(RefNr, S, NB, ChunkSize, Chunk, Chunks, Opts) when length(S) > ChunkSize ->
    {S1,S2} = lists:split(ChunkSize, S),
    ChunkNB = ChunkSize*?BITS_IN_A_BYTE,
    NewNB = NB - ChunkNB,
    X = split_sm_xser(RefNr, Chunk, Chunks, Opts),
    [{X, S1, ChunkNB} | split_sm(RefNr, S2, NewNB, ChunkSize, Chunk+1, Chunks, Opts) ];
split_sm(RefNr, S, NB, _, Chunk, Chunks, Opts) ->
    X = split_sm_xser(RefNr, Chunk, Chunks, Opts),
    [{X, S, NB}].

split_sm_xser(RefNr, Chunk, Chunks, Opts) ->
    Type = ?UCP_XSER_SERVICE_GSM_UDH,
    CType = ?UCP_XSER_SERVICE_GSM_UDH_CONCATENATED_SM,
    CData = [RefNr, Chunks, Chunk],

    case lists:keysearch(ports, 1, Opts) of
        {value, {ports, {DestPort,OrigPort}}} ->
            PType = ?UCP_XSER_SERVICE_GSM_UDH_APPLICATION_PORTS,
            PData = [DestPort, OrigPort],
            {Type, [{PType, PData},{CType, CData}]};
        _ ->
            {Type, [{CType, CData}]}
    end.
