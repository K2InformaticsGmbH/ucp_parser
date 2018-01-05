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
        DecStr = parse(Str),
        {ok, pl_to_map(DecStr, #{})}
    catch
       _:_ ->
           io:format("Error: ~p~n", [erlang:get_stacktrace()]),
           {error, <<"Invalid UCP Message">>}
    end;
decode(_) ->
    {error, <<"Input to decode should be either binary or string">>}.

-spec(pl_to_map(list(), map()) -> map()).
pl_to_map([], AccMap) ->
    AccMap;
pl_to_map([{K, V}|R], AccMap) when is_list(V) ->
    pl_to_map(R, AccMap#{atom_to_binary(K, utf8) => list_to_binary(V)});
pl_to_map([{K, V}|R], AccMap) ->
    pl_to_map(R, AccMap#{atom_to_binary(K, utf8) => V}).

-spec(encode(PDU :: map()) -> {ok, binary()} | {error, binary()}).
encode(PDU) when is_map(PDU) ->
    [2|T_DATA] = pack(maps:fold(fun map_to_pl/3, [], PDU)),
    [3|UCP_DATA] = lists:reverse(T_DATA),
    {ok, list_to_binary(lists:reverse(UCP_DATA))};
encode(_) ->
    {error, <<"Input to encode should be map">>}.

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

-define(TESTS,
[
 {"O-1_Num", 
  "00/00032/O/01/0123456789///2//EB"},
 {"O-1_AlNum", 
  "00/00038/O/01/0123456789///3/414243/24"},
 {"O-2_Num", 
  "00/00045/O/02/2/0123456789/0123456789///2//8D"},
 {"O-2_AlNum", 
  "00/00051/O/02/2/0123456789/0123456789///3/414243/BD"},
 {"O-3_Num", 
  "00/00044/O/03/0123456789///0///////////2//25"},
 {"O-3_AlNum", 
  "00/00050/O/03/0123456789///0///////////3/414243/55"},
 {"O-30", 
  "00/00042/O/30/0123456789/////////414243/D9"},
 {"O-31", 
  "00/00032/O/31/0123456789/0100/F0"},
 {"O-51_Num", 
  "00/00070/O/51/0123456789/0123456789/////////////////2"
  "///////////////23"},
 {"O-51a_Num", 
  "00/00080/O/51/0123456789/0D41E110306C1E01/////////////////2"
  "//////////5039/////4B"},
 {"O-51_AlNum", 
  "00/00076/O/51/0123456789/0123456789/////////////////3//414243"
  "/////////////5C"},
 {"O-51a_AlNum", 
  "00/00086/O/51/0123456789/0D41E110306C1E01/////////////////3//414243"
  "////////5039/////84"},
 {"O-51a_AlNum", 
  "00/00086/O/51/0123456789/0D41E110306C1E01/////////////////3//414243"
  "////////5039/////84"},
 {"O-51_Trans", 
  "00/00076/O/51/0123456789/0123456789/////////////////4//58595A"
  "/////////////7C"},
 {"O-51a_Trans", 
  "00/00086/O/51/0123456789/0D41E110306C1E01/////////////////4//58595A"
  "////////5039/////A4"},
 {"O-52_Num", 
  "00/00082/O/52/0123456789/0123456789/////////////311299235959////2"
  "///////////////A1"},
 {"O-52_AlNum", 
  "00/00088/O/52/0123456789/0123456789/////////////311299235959////3//414243"
  "/////////////DA"},
 {"O-52_Trans", 
  "00/00088/O/52/0123456789/0123456789/////////////311299235959////4//58595A"
  "/////////////FA"},
 {"O-53", 
  "00/00102/O/53/0123456789/0123456789/////////////311299235959/0/0/"
  "311299235959/3//414243/////////////A8"},
 {"O-53", 
  "00/00102/O/53/0123456789/0123456789/////////////311299235959/0/0/"
  "311299235959/3//414243/////////////A8"},
 {"O-54_Num", 
  "00/00082/O/54/0123456789/0123456789/////////////311299235959////2"
  "///////////////A3"},
 {"O-54_AlNum", 
  "00/00088/O/54/0123456789/0123456789/////////////311299235959////3"
  "//414243/////////////DC"},
 {"O-54_Trans", 
  "00/00088/O/54/0123456789/0123456789/////////////311299235959////4"
  "//58595A/////////////FC"},
 {"O-55", 
  "00/00069/O/55/0123456789/0123456789////////////////////////////////FD"},
 {"O-56", 
  "00/00076/O/56/0123456789/0123456789/////////////////3//414243i"
  "/////////////61"},
 {"O-57", 
  "00/00066/O/57/0123456789//////////////////3//414243/////////////54"},
 {"O-58", 
  "00/00066/O/58/0123456789//////////////////3//414243/////////////55"},
 {"O-60", 
  "00/00055/O/60/0123456789///1/313233414243//0100//////5F"},
 {"O-61", 
  "00/00043/O/61/0123456789///1///0100//////FC"},

 {"R-1_A", 
  "00/00019/R/01/A//68"},
 {"R-2_A", 
  "00/00019/R/02/A//69"},
 {"R-3_A", 
  "00/00019/R/03/A//6A"},
 {"R-30_A", 
  "00/00020/R/30/A///91"},
 {"R-31_A", 
  "00/00020/R/31/A/0/93"},
 {"R-51_A", 
  "00/00020/R/51/A///94"},
 {"R-52_A", 
  "00/00020/R/52/A///95"},
 {"R-53_A", 
  "00/00020/R/53/A///96"},
 {"R-54_A", 
  "00/00020/R/54/A///97"},
 {"R-55_A", 
  "00/00020/R/55/A///98"},
 {"R-56_A", 
  "00/00020/R/56/A///99"},
 {"R-57_A", 
  "00/00020/R/57/A///9A"},
 {"R-58_A", 
  "00/00020/R/58/A///9B"},
 {"R-60_A", 
  "00/00019/R/60/A//6D"},
 {"R-61_A", 
  "00/00019/R/61/A//6E"},
 {"R-1_NA", 
  "00/00022/R/01/N/02//00"}
]).

parse_test_() ->
    {inparallel,
     [{T, fun() ->
                  P = case catch parse(D) of
                          {'EXIT', Error} ->
                              ?debugFmt("~s ~p", [T, Error]),
                              Error;
                          Pr -> Pr
                      end,
                  ?assertMatch([_|_], P)
          end}
      || {T, D} <- ?TESTS
     ]}.

encode_decode_test_() ->
    {inparallel,
        [{T,
            fun() ->
                {ok, D} = decode(P),
                ?assertEqual(true, is_map(D)),
                {ok, E} = encode(jsx:decode(jsx:encode(D), [return_maps])),
                ?assertEqual(true, is_binary(E)),
                ?assertEqual({ok, D}, decode(E))
            end}
            || {T,P} <- ?TESTS]
    }.


-endif. %TEST
