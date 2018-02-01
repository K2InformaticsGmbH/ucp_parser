-module(ucp).

-export([parse/1, pack/1, parse_stream/1, cmdstr/2, 
    decode/1, encode/1]).

parse_stream(Bytes) ->
    case re:run(Bytes, "(\x02[^\x02\x03]+)",
                [global, {capture, [1], list}]) of
        nomatch -> {error, incomplete, Bytes};
        {match, Frames} ->
            case parse_frames(Frames, []) of
                [{_, parse_error}] -> {error, incomplete, Bytes};
                [{_, parse_error} | Ucps] ->
                    [LastFrame|_] = lists:last(Frames),
                    {Ucps, list_to_binary(LastFrame)};
                Ucps -> {Ucps, <<>>}
            end
    end.

parse_frames([], Ucps) -> Ucps;
parse_frames([[Frame|_]|Frames], Ucps) ->
    FramePatched = Frame++[3],
    parse_frames(Frames, [
        case (catch parse(FramePatched)) of
            {'EXIT', _} ->
                {list_to_binary(FramePatched), parse_error};
            Ucp ->
                {list_to_binary(FramePatched), Ucp}
        end
        | Ucps]).

parse(Bytes) when is_binary(Bytes) ->
    parse(binary_to_list(Bytes));
parse(UcpString) when is_list(UcpString) ->
    case {hd(UcpString), lists:last(UcpString)} of
        {2,3} -> ucp_syntax:parse(UcpString);
        {_,3} -> parse([2|UcpString]);
        {2,_} -> parse(UcpString++[3]);
        {_,_} -> parse([2|UcpString]++[3])
    end.

pack([{A,_}|_] = Ucp) when is_atom(A) ->
    ucp_syntax:pack(Ucp).

cmdstr(Cmd, Type) ->
    <<(cmd(Cmd))/binary,
      (if Type == response -> <<"_resp">>;
          true -> <<>> end)/binary>>.

-compile({inline,[cmd/1]}).
cmd(01) -> <<"call_input_01">>;
cmd(02) -> <<"multiple_address_call_input_02">>;
cmd(03) -> <<"call_input_with_supplementary_services_03">>;
cmd(30) -> <<"sms_message_transfer_30">>;
cmd(31) -> <<"smt_alert_31">>;
cmd(51) -> <<"submit_short_message_51">>;
cmd(52) -> <<"deliver_short_message_52">>;
cmd(53) -> <<"deliver_notification_53">>;
cmd(54) -> <<"modify_message_54">>;
cmd(55) -> <<"inquiry_message_55">>;
cmd(56) -> <<"delete_message_56">>;
cmd(57) -> <<"response_inquiry_message_57">>;
cmd(58) -> <<"response_delete_message_smsc_58">>;
cmd(60) -> <<"session_management_60">>;
cmd(61) -> <<"list_management_61">>;
cmd(C) when is_list(C)      -> <<"unknown_",(list_to_binary(C))/binary>>;
cmd(C) when is_binary(C)    -> <<"unknown_",C/binary>>;
cmd(C) when is_integer(C)   -> <<"unknown_",(integer_to_binary(C))/binary>>;
cmd(C) -> <<"unknown_",(list_to_binary(io_lib:format("~p",[C])))/binary>>.

-spec(decode(Str :: list() | binary()) -> {ok, map()} | {error, binary()}).
decode(Str) when is_list(Str); is_binary(Str) ->
    try
        {ok, pl_to_map(parse(Str))}
    catch
       _:_ ->
           io:format("Error: ~p~n", [erlang:get_stacktrace()]),
           {error, <<"Invalid UCP Message">>}
    end;
decode(_) ->
    {error, <<"Input to decode should be either binary or string">>}.

-spec(pl_to_map(list()) -> map()).
pl_to_map(PL) ->
    lists:foldl(fun pl_to_map/2, #{}, PL).

-spec(pl_to_map({atom(), term()}, map()) -> map()).
pl_to_map({K, V}, AccMap) when is_list(V) ->
    AccMap#{atom_to_binary(K, utf8) => list_to_binary(V)};
pl_to_map({K, V}, AccMap) ->
    AccMap#{atom_to_binary(K, utf8) => V}.

-spec(encode(PDU :: map()) -> {ok, binary()} | {error, binary()}).
encode(PDU) when is_map(PDU) ->
    [2|T_DATA] = pack(map_to_pl(PDU)),
    [3|UCP_DATA] = lists:reverse(T_DATA),
    {ok, list_to_binary(lists:reverse(UCP_DATA))};
encode(_) ->
    {error, <<"Input to encode should be map">>}.

-spec(map_to_pl(map()) -> list()).
map_to_pl(PDU) ->
    maps:fold(fun map_to_pl/3, [], PDU).

-spec(map_to_pl(term(), term(), list()) -> list()).
map_to_pl(K, V, Acc) when is_binary(V) ->
    [{binary_to_atom(K, utf8), binary_to_list(V)} | Acc];
map_to_pl(K, V, Acc) ->
    [{binary_to_atom(K, utf8), V} | Acc].

-ifdef(TEST).
%%
%% EUnit tests
%%

-include_lib("eunit/include/eunit.hrl").

ucp(OR, OT, Data) ->
    % stx  TRN(2) /  LENGTH(5) /  OR(1) /  OT(2) /  Data           /  CHK(2) etx
    Len =  2      +1 +5        +1 +1    +1 +2    +1 +length(Data)  +1 +2,
    LenStr = string:pad(integer_to_list(Len), 5, leading, $0),
    Pdu = ["00", "/", LenStr, "/", OR, "/", OT, "/", Data,        "/"],
    PduStr = lists:flatten(Pdu),
    % The <checksum> is derived by the addition of all bytes of the header,
    % data field separators and data fields (i.e., all characters after the
    % stx character, up to and including the last “/” before the checksum
    % field). The eight Least Significant Bits (LSB) of the result is then
    % represented as two printable characters. The character containing four
    % Most Significant Bits (MSB) (of those eight LSB) shall be transmitted
    % first. For example, if the checksum is 3A(hex), the representation shall
    % be the characters “3” (33(hex)) and “A” (41(hex)).
    Checksum = integer_to_list(lists:sum(PduStr) band 16#FF, 16),
    PduStr ++ Checksum.

ucp(OT, Data)      -> ucp("O", OT, Data).
ucp_ack(OT, Data)  -> ucp("R", OT, [$A, $/ | Data]).
ucp_nack(OT, Data) -> ucp("R", OT, [$N, $/ | Data]).

% 01 31 51 52 53 55 56 57 58 60
-define(TESTS,
[
 {"O-1_Num",    ucp("01","0123456789///2/")},
 {"O-1_AlNum",  ucp("01","0123456789///3/414243")},
 {"O-31",       ucp("31","0123456789/0100")},
 {"O-51_Num",   ucp("51","0123456789/0123456789/////////////////2//////////////")},
 %{"O-51a_Num", ucp("51","0123456789/0D41E110306C1E01/////////////////2//////////5039////")},
 {"O-51_AlNum", ucp("51","0123456789/0123456789/////////////////3//414243////////////")},
 %{"O-51a_AlNum", ucp("51","0123456789/0D41E110306C1E01/////////////////3//414243////////5039////")},
 {"O-51_Trans", ucp("51","0123456789/0123456789/////////////////4//58595A////////////")},
 %{"O-51a_Trans", ucp("51","0123456789/0D41E110306C1E01/////////////////4//58595A////////5039////")},
 %{"O-52_Num", ucp("52","0123456789/0123456789/////////////311299235959////2//////////////")},
 %{"O-52_AlNum", ucp("52","0123456789/0123456789/////////////311299235959////3//414243////////////")},
 %{"O-52_Trans", ucp("52","0123456789/0123456789/////////////311299235959////4//58595A////////////")},
 %{"O-53", ucp("53","0123456789/0123456789/////////////311299235959/0/0/311299235959/3//414243////////////")},
 {"O-55", ucp("55","0123456789/0123456789///////////////////////////////")},
 %{"O-56", ucp("56","0123456789/0123456789/////////////////3//414243////////////")},
 {"O-57", ucp("57","0123456789//////////////////3//414243////////////")},
 {"O-58", ucp("58","0123456789//////////////////3//414243////////////")},
 {"O-60", ucp("60","0123456789///1/313233414243//0100/////")},

 %{"R-1_NA", "00/00022/R/01/N/02//00"},
 {"R-1_A",  ucp_ack("01","")},
 {"R-31_A", ucp_ack("31","0000")},
 %{"R-51_A", ucp_ack("51","/")},
 {"R-52_A", ucp_ack("52","/")},
 {"R-53_A", ucp_ack("53","/")},
 {"R-55_A", ucp_ack("55","/")},
 {"R-56_A", ucp_ack("56","/")},
 {"R-57_A", ucp_ack("57","/")},
 {"R-58_A", ucp_ack("58","/")},
 {"R-60_A", ucp_ack("60","")}
]).

% parse_test_() ->
%     {inparallel,
%      [{T, fun() ->
%                   P = case catch parse(D) of
%                           {'EXIT', Error} ->
%                               ?debugFmt("~s ~p", [T, Error]),
%                               Error;
%                           Pr -> Pr
%                       end,
%                   ?assertMatch([_|_], P)
%           end}
%       || {T, D} <- ?TESTS
%      ]}.

encode_decode_test_() ->
    {inparallel,
        [{T,
            fun() ->
                {ok, D} = decode(P),
                ?assertEqual(true, is_map(D)),
                {ok, E} = encode(jsx:decode(jsx:encode(D), [return_maps])),
                %?debugFmt("PDU1 : ~p   PDU2 : ~p", [P, E]),
                ?assertEqual(true, is_binary(E)),
                ?assertEqual({ok, D}, decode(E))
            end
         } || {T,P} <- ?TESTS]
    }.

-endif. %TEST
