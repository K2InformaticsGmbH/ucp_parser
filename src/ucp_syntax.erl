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
%%%  This module defines the syntax for EMI/UCP messages.
%%%  The conversion is back and forth between EMI/UCP messages
%%%  and a proplist representation.
%%%
%%%  The existence of the module UCP_ARG_SYNTAX, for converting
%%%  arguments, is assumed.
%%%
%%%  Currently only UK/RoI H3G service is supported This means that
%%%  only the following messages have to be supported: 31/out, 51/out,
%%%  52/in, 53/in, 60/out,
%%%
%%%  To be able to write test code - both operations
%%%  and replies for 31, 51, 52, 53 and 60 are supported
%%%  in the code.
%%%
%%%  Moreover, as the 5x and 6x message series all have the
%%%  same layout - 5x and 6x also are supported.
%%% @end

%%% @type ucp() = [{Key::atom(),Value::term()}]

-module(ucp_syntax).

-include("ucp_defines.hrl").
-include("ucp_errors.hrl").

%%% API EXPORTS

-export([make_01/5, make_call_input/5,
         make_30/2, make_ms_message_transfer/2,
         make_31/3, make_mt_alert/3,
         make_51/5, make_submit_sm/5,
         make_52/5, make_delivery_sm/5,
         make_53/4, make_delivery_notification/4,
         make_55/4, make_56/5,
         make_60/5, make_session_management/5]).

-export([make_result/1,
         make_ack/1, make_ack/2,
         make_nack/2]).

-export([parse/1,pack/1]).

-export([supported/0, supported/2]).

-export([get_value/2]).

-export([prettify/1]).


%%% EXPORTED FUNCTIONS

%%% ----------------------------------------------------------
%%% Functions for building specific messages
%%% ----------------------------------------------------------

%%% @doc Make a call input (01) message.
%%% @spec make_01(Trn,AdC,OAdC,Msg,UCP_opt) -> Mess
%%%  Trn = integer()
%%%  AdC = string()
%%%  UCP_opt = ucp()
%%%  Mess = binary() | string()
%%% @end
make_01(Trn, AdC, OAdC, Msg, UCP_opt) ->
    UCP_opt_mt =
        case get_value(mt, UCP_opt) of
            undefined ->
                MT = ?UCP_MT_ALPHANUMERIC,
                [{mt, MT} | UCP_opt];
            _ ->
                UCP_opt
        end,
    UCP_data =
        [{adc, AdC},
         {oadc, OAdC},
         {msg, Msg} | UCP_opt_mt],
    Data = make_01_data(UCP_data),
    finalize(Data, Trn, "O", 1).

%%% @doc Same as make_01/5.
%%% @spec make_call_input(Trn,AdC,OAdC,Msg,UCP_opt) -> Mess
%%%  Trn = integer()
%%%  AdC = string()
%%%  Mess = string()
%%% @end
make_call_input(Trn, AdC, OAdC, Msg, UCP_opt) ->
    make_01(Trn, AdC, OAdC, Msg, UCP_opt).

%%% TODO: implement make 02

%%% TODO: implement make 03

%%% @doc Make a MS Message Transfer (30) message.
%%% @spec make_30(Trn,AdC) -> Mess
%%%  Trn = integer()
%%%  AdC = string()
%%%  Mess = string()
%%% @end
make_30(Trn, AdC) ->
    UCP_data =
        [{adc, AdC}],
    Data = make_30_data(UCP_data),
    finalize(Data, Trn, "O", 30).

%%% @doc Same as make_30/2.
%%% @spec make_ms_message_transfer(Trn,AdC) -> Mess
%%%  Trn = integer()
%%%  AdC = string()
%%%  Mess = string()
%%% @end
make_ms_message_transfer(Trn, AdC) ->
    make_30(Trn, AdC).

%%% @doc Make a MT alert (31) message.
%%% @spec make_31(Trn,AdC,PID) -> Mess
%%%  Trn = integer()
%%%  AdC = string()
%%%  PID = integer()
%%%  Mess = string()
%%% @end
make_31(Trn, AdC, PID) ->
    UCP_data =
        [{adc, AdC},
         {pid, PID}],
    Data = make_31_data(UCP_data),
    finalize(Data, Trn, "O", 31).

%%% @doc Same as make_31/3.
%%% @spec make_mt_alert(Trn,AdC,PID) -> Mess
%%%  Trn = integer()
%%%  AdC = string()
%%%  PID = integer()
%%%  Mess = string()
%%% @end
make_mt_alert(Trn, AdC, PID) ->
    make_31(Trn, AdC, PID).

%%% @doc Make a submit SM (51) message.
%%% @spec make_51(Trn,AdC,OAdC,Msg,UCP_opt) -> Mess
%%%  Trn = integer()
%%%  AdC = string()
%%%  UCP_opt = ucp()
%%%  Mess = binary() | string()
%%% @end
make_51(Trn, AdC, OAdC, Msg, UCP_opt) ->
    UCP_opt_mt =
        case get_value(mt, UCP_opt) of
            undefined ->
                MT = ?UCP_MT_ALPHANUMERIC,
                [{mt, MT} | UCP_opt];
            _ ->
                UCP_opt
        end,
    UCP_data =
        [{adc, AdC},
         {oadc, OAdC},
         {msg, Msg} | UCP_opt_mt],
    Data = make_5x_data(UCP_data),
    finalize(Data, Trn, "O", 51).

%%% @doc Same as make_51/5.
%%% @spec make_submit_sm(Trn,AdC,OAdC,Msg,UCP_opt) -> Mess
%%%  Trn = integer()
%%%  AdC = string()
%%%  OAdC = string()
%%%  Msg = string() | binary()
%%%  UCP_opt = ucp()
%%%  Mess = binary() | string()
%%% @end
make_submit_sm(Trn, AdC, OAdC, Msg, UCP_opt) ->
    make_51(Trn, AdC, OAdC, Msg, UCP_opt).

%%% @doc Make a delivery SM (52) message.
%%% @spec make_52(Trn,AdC,OAdC,Msg,UCP_opt) -> Mess
%%%  Trn = integer()
%%%  AdC = string()
%%%  OAdC = string()
%%%  Msg = string() | binary()
%%%  UCP_opt = ucp()
%%%  Mess = string()
%%% @end
make_52(Trn, AdC, OAdC, Msg, UCP_opt) ->
    UCP_opt_mt =
        case get_value(mt, UCP_opt) of
            undefined ->
                MT = ?UCP_MT_ALPHANUMERIC,
                [{mt, MT} | UCP_opt];
            _ ->
                UCP_opt
        end,
    UCP_data =
        [{adc, AdC},
         {oadc, OAdC},
         %% TODO: is it current UTC time ??
         {scts, calendar:universal_time()},
         {msg, Msg} | UCP_opt_mt],
    Data = make_5x_data(UCP_data),
    finalize(Data, Trn, "O", 52).

%%% @doc Same as make_52/5.
%%% @spec make_delivery_sm(Trn,AdC,OAdC,Msg,UCP_opt) -> Mess
%%%  Trn = integer()
%%%  AdC = string()
%%%  OAdC = string()
%%%  Msg = string() | binary()
%%%  UCP_opt = ucp()
%%%  Mess = string()
%%% @end
make_delivery_sm(Trn, AdC, OAdC, Msg, UCP_opt) ->
    make_52(Trn, AdC, OAdC, Msg, UCP_opt).

%%% @doc Make a delivery notification SM (53) message.
%%% @spec make_53(Trn,AdC,OAdC,UCP_opt) -> Mess
%%%  Trn = integer()
%%%  AdC = string()
%%%  OAdC = string()
%%%  UCP_opt = ucp()
%%%  Mess = string()
%%% @end
make_53(Trn, AdC, OAdC, UCP_opt) ->
    UCP_opt_mt =
        case get_value(mt, UCP_opt) of
            undefined ->
                MT = ?UCP_MT_ALPHANUMERIC,
                [{mt, MT} | UCP_opt];
            _ ->
                UCP_opt
        end,
    UCP_data =
        [{adc, AdC},
         {oadc, OAdC},
         %% TODO: is it current UTC time ??
         {scts, calendar:universal_time()} | UCP_opt_mt],
    Data = make_5x_data(UCP_data),
    finalize(Data, Trn, "O", 53).

%%% @doc Same as make_53/4.
%%% @spec make_delivery_notification(Trn,AdC,OAdC,UCP_opt) -> Mess
%%%  Trn = integer()
%%%  AdC = string()
%%%  OAdC = string()
%%%  UCP_opt = ucp()
%%%  Mess = string()
%%% @end
make_delivery_notification(Trn, AdC, OAdC, UCP_opt) ->
    make_53(Trn, AdC, OAdC, UCP_opt).

%%% TODO: implement make 54

%%% @doc Make a Inquiry Message Operation (55) message.
%%% @spec make_53(Trn,AdC,OAdC,UCP_opt) -> Mess
%%%  Trn = integer()
%%%  AdC = string()
%%%  OAdC = string()
%%%  UCP_opt = ucp()
%%% @end
make_55(Trn, AdC, OAdC, UCP_opt) ->
    UCP_data =
        [{adc, AdC},
         {oadc, OAdC} | UCP_opt],
    Data = make_5x_data(UCP_data),
    finalize(Data, Trn, "O", 55).

%%% @doc Make a Delete Message Operation (56) message.
%%% @spec make_52(Trn,AdC,OAdC,Msg,UCP_opt) -> Mess
%%%  Trn = integer()
%%%  AdC = string()
%%%  OAdC = string()
%%%  Msg = string() | binary()
%%%  UCP_opt = ucp()
%%%  Mess = string()
%%% @end
make_56(Trn, AdC, OAdC, Msg, UCP_opt) ->
    UCP_opt_mt =
        case get_value(mt, UCP_opt) of
            undefined ->
                MT = ?UCP_MT_ALPHANUMERIC,
                [{mt, MT} | UCP_opt];
            _ ->
                UCP_opt
        end,
    UCP_data =
        [{adc, AdC},
         {oadc, OAdC},
         %% TODO: is it current UTC time ??
         {scts, calendar:universal_time()},
         {msg, Msg} | UCP_opt_mt],
    Data = make_5x_data(UCP_data),
    finalize(Data, Trn, "O", 56).

%%% @doc Make a Response Inquiry Message Operation (57) message.
%%% @spec make_57(Trn,AdC,Mt,UCP_opt) -> Mess
%%%  Trn = integer()
%%%  AdC = string()
%%%  Mt = integer()
%%%  UCP_opt = ucp()
%%% @end
make_57(Trn, AdC, Mt, UCP_opt) ->
    UCP_data =
        [{adc, AdC}, {mt, Mt} | proplists:delete(
                                  adc, proplists:delete(mt, UCP_opt))],
    Data = make_5x_data(UCP_data),
    finalize(Data, Trn, "O", 57).

%%% @doc Make a Response Delete Message Operation (58) message.
%%% @spec make_57(Trn,AdC,Mt,UCP_opt) -> Mess
%%%  Trn = integer()
%%%  AdC = string()
%%%  Mt = integer()
%%%  UCP_opt = ucp()
%%% @end
make_58(Trn, AdC, Mt, UCP_opt) ->
    UCP_data =
        [{adc, AdC}, {mt, Mt} | proplists:delete(
                                  adc, proplists:delete(mt, UCP_opt))],
    Data = make_5x_data(UCP_data),
    finalize(Data, Trn, "O", 58).

%%% @doc Make a session management (60) message.
%%% @spec make_60(Trn,OAdC,STYP,PWD,UCP_opt) -> Mess
%%%  Trn = integer()
%%%  OAdC = string()
%%%  STYP = integer()
%%%  PWD = string()
%%%  UCP_opt = ucp()
%%%  Mess = string()
%%% @end
make_60(Trn, OAdC, STYP, PWD, UCP_opt) ->
    UCP_data =
        [{oadc,OAdC},
         {styp,STYP},
         {pwd,PWD},
         {vers,100} | UCP_opt],           % TODO: vers 0100 hard coded
    Data = make_6x_data(UCP_data),
    finalize(Data, Trn, "O", 60).

%%% @doc Same as make_60/5.
%%% @spec make_session_management(Trn,OAdC,STYP,PWD,UCP_opt) -> Mess
%%%  Trn = integer()
%%%  OAdC = string()
%%%  STYP = integer()
%%%  PWD = string()
%%%  UCP_opt = ucp()
%%%  Mess = string()
%%% @end
make_session_management(Trn, OAdC, STYP, PWD, UCP_opt) ->
    make_60(Trn, OAdC, STYP, PWD, UCP_opt).

%%% ----------------------------------------------------------
%%% Result, assumed positive for all supported
%%% incoming messages.
%%% ----------------------------------------------------------

%%% TODO: there are lots of nack replies in the specification. What
%%% shall be implemented ??

%%% @doc Make a result message.
%%%  Uses some heuristics for if it shall do an ACK or a NACK
%%% @end
%%% @spec make_result(UCP::ucp()) -> Mess::string()
make_result(UCP) ->
    case supported(get_value(ot, UCP), parse) of
        true ->
            make_ack(UCP);
        false ->
            %% TODO: or message type not suported ??
            {EC, _} = ?UCP_NACK_OPERATION_NOT_SUPPORTED,
            make_nack(UCP, EC)
    end.

%%% ----------------------------------------------------------
%%% Positive acknowledgement build function
%%% ----------------------------------------------------------

%%% @doc Make an ACK message.
%%% @spec make_ack(UCP::ucp()) -> Mess::string()
make_ack(UCP) ->
    case get_value(ot, UCP) of
        1 ->
            make_ack_generic(UCP);
        %% TODO - implement make ack for 02
        %% TODO - implement make ack for 03
        30 ->
            make_ack_30(UCP);
        31 ->
            make_ack_31(UCP);
        51 ->
            make_ack_51(UCP);
        52 ->
            make_ack_5x(UCP);
        53 ->
            make_ack_5x(UCP);
        54 ->
            make_ack_54(UCP);
        55 ->
            make_ack_5x(UCP);
        56 ->
            make_ack_5x(UCP);
        57 ->
            make_ack_5x(UCP);
        58 ->
            make_ack_5x(UCP);
        60 ->
            make_ack_generic(UCP);
        61 ->
            make_ack_generic(UCP)
    end.

make_ack(UCP, SM) ->
    case get_value(ot, UCP) of
        51 ->
            make_ack_51(UCP, SM)
    end.

%%% ----------------------------------------------------------
%%% Negative acknowledgement build function
%%% ----------------------------------------------------------

%%% @doc Make a NACK message.
%%% @spec make_nack(UCP::ucp(), Reason::integer()) -> Mess::string()
make_nack(UCP, Reason) ->
    %% TODO: shall we send the error text string in SM - or shall we
    %% just send a blank field ?? Both are valid as far as I know. And
    %% maybe if we send the text, we are supposed to localize the text
    %% for different languages and needs? The current choice is to not
    %% send the text string.

    {EC, SM} =
        case Reason of
            {_EC, _SM} ->
                %% {_EC, _SM};  %% No text
                {_EC, ""};      %% Text
            _EC ->
                {_EC, ""}
        end,

    ECStr = ucp_arg_syntax:make_num(2, EC),

    Data =
        "N" ++ "/" ++
        ECStr ++ "/" ++
        SM ++ "/",
    finalize(Data, get_value(trn, UCP), "R", get_value(ot, UCP)).

%%% ----------------------------------------------------------
%%% Parse function
%%% ----------------------------------------------------------

%%% @doc Parse a message.
%%% @spec parse(Mess::string()) -> UCP::ucp()
parse(RAW_UCP_string) ->
    [?STX|S] = RAW_UCP_string,
    {UCP_mess, [$/, CH1, CH2, ?ETX]} = lists:split(length(S) - 4, S),

    Chk = [CH1, CH2],
    case extract_arguments(UCP_mess) of
        [TrnStr, LengthStr, Type, OtStr | Data ] ->

            OT = ucp_arg_syntax:parse_num(OtStr),

            test_supported(Type, OT, Data),

            %% mpro adaption: do not fail any more upon nonmatching checksum
            % test_checksum(Chk, UCP_mess ++ "/"),  

            UCP =
                [{trn,    ucp_arg_syntax:parse_num(TrnStr)},
                 {length, ucp_arg_syntax:parse_num(LengthStr)},
                 {type,   Type},
                 {ot,     OT},
                 {chk,    Chk}],

            parse_data(UCP, Data);

        [TrnStr, LengthStr, Type] ->
            [{trn,    ucp_arg_syntax:parse_num(TrnStr)},
             {length, ucp_arg_syntax:parse_num(LengthStr)},
             {type,   Type},
             {chk,    Chk}]
    end.

%%% @doc Packs a message
%%% @spec pack(Ucp::ucp()) -> Mess::string()
pack(Ucp) ->
    Trn = proplists:get_value(trn, Ucp),
    Ot = proplists:get_value(ot, Ucp),
    case proplists:get_value(type, Ucp) of
        "O" ->
            case Ot of
                01 ->
                    AdC = proplists:get_value(adc, Ucp),
                    OAdC = proplists:get_value(oadc, Ucp),
                    Msg = proplists:get_value(msg, Ucp),
                    make_01(Trn, AdC, OAdC, Msg, Ucp);
                31 ->
                    AdC = proplists:get_value(adc, Ucp),
                    PID = proplists:get_value(pid, Ucp),
                    make_31(Trn, AdC, PID);
                51 ->
                    AdC = proplists:get_value(adc, Ucp),
                    OAdC = proplists:get_value(oadc, Ucp),
                    Msg = proplists:get_value(msg, Ucp),
                    make_51(Trn, AdC, OAdC, Msg, Ucp);
                52 ->
                    AdC = proplists:get_value(adc, Ucp),
                    OAdC = proplists:get_value(oadc, Ucp),
                    Msg = proplists:get_value(msg, Ucp),
                    make_52(Trn, AdC, OAdC, Msg, Ucp);
                53 ->
                    AdC = proplists:get_value(adc, Ucp),
                    OAdC = proplists:get_value(oadc, Ucp),
                    make_53(Trn, AdC, OAdC, Ucp);
                55 ->
                    AdC = proplists:get_value(adc, Ucp),
                    OAdC = proplists:get_value(oadc, Ucp),
                    make_55(Trn, AdC, OAdC, Ucp);
                56 ->
                    AdC = proplists:get_value(adc, Ucp),
                    OAdC = proplists:get_value(oadc, Ucp),
                    Msg = proplists:get_value(msg, Ucp),
                    make_56(Trn, AdC, OAdC, Msg, Ucp);
                57 ->
                    AdC = proplists:get_value(adc, Ucp),
                    Mt = proplists:get_value(mt, Ucp),
                    make_57(Trn,AdC,Mt,Ucp);
                58 ->
                    AdC = proplists:get_value(adc, Ucp),
                    Mt = proplists:get_value(mt, Ucp),
                    make_58(Trn,AdC,Mt,Ucp);
                60 ->
                    OAdC = proplists:get_value(oadc, Ucp),
                    STYP = proplists:get_value(styp, Ucp),
                    PWD = proplists:get_value(pwd, Ucp),
                    make_60(Trn, OAdC, STYP, PWD, Ucp);
                _ ->
                    error({unimplemented, "O", Ot})
            end;
        "R" ->
            make_ack(Ucp)
%            error({unimplemented, "R", Ot})
    end.

%%% ----------------------------------------------------------
%%% Test for what messages are supported to parse and build
%%% ----------------------------------------------------------

%%% @private
supported() ->
    Parse    = [01, 02, 03, 30, 31, 51, 52, 53, 54, 55, 56, 57, 58, 60, 61],
    Build    = [            30, 31, 51, 52, 53,                     60    ],
    ParseAck = [01, 02, 03, 30, 31, 51, 52, 53, 54, 55, 56, 57, 58, 60, 61],
    BuildAck = [            30, 31, 51, 52, 53, 54, 55, 56, 57, 58, 60, 61],
    {Parse, Build, ParseAck, BuildAck}.

%%% @private
supported(ID, parse) ->
    {List, _, _, _} = supported(),
    lists:member(ID, List);
supported(ID, build) ->
    {_, List, _, _} = supported(),
    lists:member(ID, List);
supported(ID, parse_ack) ->
    {_, _, List, _} = supported(),
    lists:member(ID, List);
supported(ID, build_ack) ->
    {_, _, _, List} = supported(),
    lists:member(ID, List);
supported(_ID, parse_nack) ->
    true;
supported(_ID, build_nack) ->
    true.

%%% ----------------------------------------------------------
%%% Fetches a value from a key-list.  This function has the same
%%% semantics as the fields in a record, i.e. undefined is a special
%%% value
%%% ----------------------------------------------------------

%%% @doc Gets a value from an ucp() term.
%%% @spec get_value(Key::atom(), UCP::ucp()) -> Value::term()
get_value(Key, UCP) ->
    case lists:keysearch(Key, 1, UCP) of
        {value, {_,Value}} ->
            Value;
        false ->
            undefined
    end.

%%% ----------------------------------------------------------
%%% Prettifyer of messages. It assumes all characters are readable
%%% except STX (2) and ETX (3).
%%% ----------------------------------------------------------

%%% @doc Makes messages printable with ~p.
%%% @spec prettify(string()) -> string()
prettify(S) ->
    lists:flatten([prf(C) || C <- S]).


%%% PRIVATE FUNCTIONS

%%% ----------------------------------------------------------
%%% Help function for prettify
%%% ----------------------------------------------------------

prf(?STX) ->
    " stx ";
prf(?ETX) ->
    " etx ";
prf(C) ->
    C.

%%% ----------------------------------------------------------
%%% Help function that makes data part of message from
%%% UCP keyvalue list of arguments.
%%% ----------------------------------------------------------

make_01_data(UCP) ->
    %% Special treatment of msg field is needed
    UCP2 = encode_msg_field(UCP),
    make_data(data_template_01(), UCP2, "").

%%% TODO: implement make 02 data

%%% TODO: implement make 03 data

make_30_data(UCP) ->
    make_data(data_template_30(), UCP, "").

make_31_data(UCP) ->
    make_data(data_template_31(), UCP, "").

make_5x_data(UCP) ->
    %% Special treatment of msg and oadc fields needed
    UCP2 = encode_msg_field(UCP),
    UCP3 = encode_oadc_field(UCP2),
    make_data(data_template_5x(), UCP3, "").

make_6x_data(UCP) ->
    make_data(data_template_6x(), UCP, "").

make_data([],_,Acc) ->
    Acc;
make_data([T|Ts], UCP, Acc) ->
    Arg = make_data_arg(T, UCP),
    make_data(Ts, UCP, Acc ++ Arg).

make_data_arg(Template, UCP) ->
    Key = erlang:element(1, Template),

    ArgStr =
        case get_value(Key, UCP) of
            undefined ->
                "";
            Arg ->
                case Template of
                    {Key,num,Size} ->
                        ucp_arg_syntax:make_num(Size, Arg);

                    {Key,hex_coded} ->
                        ucp_arg_syntax:make_hex_coded(Arg);

                    {Key,chrstr,_Size} ->
                        %% TODO: should check Size
                        Arg;

                    {Key,numstr,_Size} ->
                        %% TODO: should check Size
                        %% TODO: should check if its only numbers
                        Arg;

                    {Key,xsers,_Size} ->
                        ucp_arg_syntax:make_xsers(Arg);

                    {Key,time,Size} ->
                        ucp_arg_syntax:make_time(Size, Arg);

                    {Key,{ip,_List}} ->
                        ucp_arg_syntax:make_addr(Arg);
                    {Key,{ip,_List},_Size} ->
                        ucp_arg_syntax:make_addr(Arg)
                end
        end,

    ArgStr ++ "/".



%%% ----------------------------------------------------------
%%% Help functions for building ack and nack
%%% ----------------------------------------------------------

%%% TODO: In the ACK message, the OT - 0x, 30, 51 and 54 shall contain
%%% the "AdC:SCTS"(Service Center Time Stamp) data in the SM field.
%%% Currently it is implemented for 51 and 54.

%%% TODO - implement make ack for 01

%%% TODO - implement make ack for 02

%%% TODO - implement make ack for 03

%%% TODO - implement make ack for 30

make_ack_30(UCP) ->
    MVP = make_mvp(UCP),
    SM = make_adc_scts(UCP),
    Data =
        "A" ++ "/" ++
        MVP ++ "/" ++
        SM ++ "/",
    finalize(Data, get_value(trn, UCP), "R", get_value(ot, UCP)).

make_ack_31(UCP) ->
    SM = "0000",                                % TODO: always 0
    Data =
        "A" ++ "/" ++
        SM ++ "/",
    finalize(Data, get_value(trn, UCP), "R", get_value(ot, UCP)).

make_ack_51(UCP) ->
    make_ack_51(UCP, make_adc_scts(UCP)).

make_ack_51(UCP, SM) ->
    MVP = make_mvp(UCP),
    Data =
        "A" ++ "/" ++
        MVP ++ "/" ++
        SM ++ "/",
    finalize(Data, get_value(trn, UCP), "R", get_value(ot, UCP)).

make_ack_54(UCP) ->
    MVP = make_mvp(UCP),
    SM = make_adc_scts(UCP),
    Data =
        "A" ++ "/" ++
        MVP ++ "/" ++
        SM ++ "/",
    finalize(Data, get_value(trn, UCP), "R", get_value(ot, UCP)).

make_ack_5x(UCP) ->
    SM = "",                                    % SM empty if not 51 or 54
    MVP = "",                                   % MVP empty if not 51 or 54
    Data =
        "A" ++ "/" ++
        MVP ++ "/" ++
        SM ++ "/",
    finalize(Data, get_value(trn, UCP), "R", get_value(ot, UCP)).

make_ack_generic(UCP) ->
    SM = "",                                    % TODO: empty SM
    Data =
        "A" ++ "/" ++
        SM ++ "/",
    finalize(Data, get_value(trn, UCP), "R", get_value(ot, UCP)).

%%% ----------------------------------------------------------
%%% Help functions for make_ack
%%% ----------------------------------------------------------

make_mvp(UCP) ->
    case get_value(vp, UCP) of
        undefined ->
            "";
        VP ->
            %% TODO: assumes no change to VP, i.e. that the SMSC
            %% approves of any period.
            ucp_arg_syntax:make_time(10, VP)
    end.

make_adc_scts(UCP) ->
    ADC =
        case get_value(adc, UCP) of
            undefined -> "";
            AdC -> AdC
        end,
    %% TODO: is it current UTC time ??
    SCTS = ucp_arg_syntax:make_time(12, calendar:universal_time()),
    ADC ++ ":" ++ SCTS.

%%% The extra size, except data fields between stx and etx,
%%% is constant in UCP mesages. These are the fields:
%%% TRN:    2 chars
%%% "/"     1 char
%%% LEN:    5 chars
%%% "/"     1 char
%%% O/R:    1 char
%%% "/"     1 char
%%% OT:     2 char
%%% "/"     1 char
%%% CHKSUM: 2 char
-define(UCP_EXTRA_SIZE, 16).

finalize(Data, Trn, Type, Ot) ->
    TrnStr = ucp_arg_syntax:make_num(2, Trn),
    LenStr = ucp_arg_syntax:make_num(5, length(Data) + ?UCP_EXTRA_SIZE),
    OtStr = ucp_arg_syntax:make_num(2, Ot),
    S = TrnStr ++ "/" ++
        LenStr ++ "/" ++
        Type ++ "/" ++
        OtStr ++ "/" ++
        Data,
    ChkStr = ucp_arg_syntax:make_checksum(S),
    [?STX] ++ S ++ ChkStr ++ [?ETX].

%%% ----------------------------------------------------------
%%% Splits an UCP message into a list of arguments
%%% NOTE: string:tokens cannot be used - it cannot get
%%% empty tokens.
%%% ----------------------------------------------------------

extract_arguments(String) ->
    extract_arguments(String, []).

extract_arguments([], Ack) ->
    [Ack];
extract_arguments([$/|T], Ack) ->
    [Ack | extract_arguments(T, [])];
extract_arguments([H|T], Ack) ->
    extract_arguments(T, Ack ++ [H]).

%%% ----------------------------------------------------------
%%% Adding parsed UCP data field to the UCP keyvalue list
%%% ----------------------------------------------------------

parse_data(UCP, Data) ->
    case get_value(type, UCP) of
        "O" ->
            parse_operation_data(UCP, Data);
        "R" ->
            parse_result_data(UCP, Data)
    end.

parse_operation_data(UCP, Data) ->
    case get_value(ot, UCP) of
        01 ->
            parse_01_data(UCP, Data);
        02 ->
            parse_02_data(UCP, Data);
        03 ->
            parse_03_data(UCP, Data);
        30 ->
            parse_30_data(UCP, Data);
        31 ->
            parse_31_data(UCP, Data);
        51 ->
            parse_5x_data(UCP, Data);
        52 ->
            parse_5x_data(UCP, Data);
        53 ->
            parse_5x_data(UCP, Data);
        54 ->
            parse_5x_data(UCP, Data);
        55 ->
            parse_5x_data(UCP, Data);
        56 ->
            parse_5x_data(UCP, Data);
        57 ->
            parse_5x_data(UCP, Data);
        58 ->
            parse_5x_data(UCP, Data);
        60 ->
            parse_6x_data(UCP, Data);
        61 ->
            parse_6x_data(UCP, Data)
    end.

parse_01_data(UCP, Data) ->
    UCP_data = parse_template(data_template_01(), Data) ++ UCP,
    %% Special treatment of msg and oadc fields needed
    UCP_data2 = decode_msg_field(UCP_data),
    UCP_data3 = decode_oadc_field(UCP_data2),
    UCP ++ UCP_data3.

parse_02_data(UCP, Data) ->
    parse_template(data_template_02(), Data) ++ UCP.

parse_03_data(UCP, Data) ->
    parse_template(data_template_03(), Data) ++ UCP.

parse_30_data(UCP, Data) ->
    %% TODO: decode IP addresses
    parse_template(data_template_30(), Data) ++ UCP.

parse_31_data(UCP, Data) ->
    %% TODO: decode IP addresses
    parse_template(data_template_31(), Data) ++ UCP.

parse_5x_data(UCP, Data) ->
    UCP_data = parse_template(data_template_5x(), Data),
    %% Special treatment of msg and oadc fields needed
    UCP_data2 = decode_msg_field(UCP_data),
    UCP_data3 = decode_oadc_field(UCP_data2),
    %% TODO: decode IP addresses. This is only needed for client
    %% commands, i.e. 51, 54, 55 and 56.
    UCP ++ UCP_data3.

parse_6x_data(UCP, Data) ->
    %% TODO: decode IP addresses. Not 100% sure when this is made. The
    %% specification is not clear on this point.
    parse_template(data_template_6x(), Data) ++ UCP.

parse_result_data(UCP, Data) ->
    [ACK|Tail] = Data,

    case ACK of
        "A" ->
            case get_value(ot, UCP) of
                01 ->
                    parse_ack_data_generic(UCP, Tail);
                02 ->
                    parse_ack_data_generic(UCP, Tail);
                03 ->
                    parse_ack_data_generic(UCP, Tail);
                30 ->
                    parse_ack_data_generic(UCP, Tail);
                31 ->
                    parse_ack_data_generic(UCP, Tail);
                51 ->
                    parse_ack_data_5x(UCP, Tail);
                52 ->
                    parse_ack_data_5x(UCP, Tail);
                53 ->
                    parse_ack_data_5x(UCP, Tail);
                54 ->
                    parse_ack_data_5x(UCP, Tail);
                55 ->
                    parse_ack_data_5x(UCP, Tail);
                56 ->
                    parse_ack_data_5x(UCP, Tail);
                57 ->
                    parse_ack_data_5x(UCP, Tail);
                58 ->
                    parse_ack_data_5x(UCP, Tail);
                60 ->
                    parse_ack_data_generic(UCP, Tail);
                61 ->
                    parse_ack_data_generic(UCP, Tail)
            end;
        "N"  ->
            parse_nack_data(UCP, Tail)
    end.

parse_ack_data_generic(UCP, [""]) ->
    [{ack,"A"} | UCP];
parse_ack_data_generic(UCP, [SM]) ->
    [{ack,"A"},{sm, SM} | UCP];
parse_ack_data_generic(UCP, [[], SM]) ->
    [{ack, "A"}, {sm, SM} | UCP].


%%% TODO - implement parse ack data for 30

parse_ack_data_5x(UCP, ["", ""]) ->
    [{ack,"A"} | UCP];
parse_ack_data_5x(UCP, [MVP, ""]) ->
    [{ack,"A"},{mvp, MVP} | UCP];
parse_ack_data_5x(UCP, ["", SM]) ->
    [{ack,"A"}, {sm, SM} | UCP];
parse_ack_data_5x(UCP, [MVP, SM]) ->
    [{ack,"A"},{mvp, MVP}, {sm, SM} | UCP].

parse_nack_data(UCP, [ECStr, ""]) ->
    EC = ucp_arg_syntax:parse_num(ECStr),
    [{nack,"N"},{ec, EC} | UCP];
parse_nack_data(UCP, [ECStr, SM]) ->
    EC = ucp_arg_syntax:parse_num(ECStr),
    [{nack,"N"},{ec, EC}, {sm, SM} | UCP].

%%% Insert parsed data fields into UCP keyvalue list

parse_template(Template, Values) ->
    parse_template(Template, Values, []).

parse_template([], [], Acc) ->
    Acc;
parse_template([_|KT], [""|VT], Acc) ->
    %% Skip empty data field
    parse_template(KT, VT, Acc);
parse_template([{Key,Type,_Size}|KT], Vals, Acc) ->
    %% Remove size field - if present
    parse_template([{Key,Type}|KT], Vals, Acc);

parse_template([{Key,num}|KT], [Val|VT], Acc) ->
    ValParsed = ucp_arg_syntax:parse_num(Val),
    parse_template(KT, VT, [{Key, ValParsed}|Acc]);
parse_template([{Key,hex_coded}|KT], [Val|VT], Acc) ->
    ValParsed = ucp_arg_syntax:parse_hex_coded(Val),
    parse_template(KT, VT, [{Key, ValParsed}|Acc]);
parse_template([{Key,chrstr}|KT], [Val|VT], Acc) ->
    parse_template(KT, VT, [{Key, Val}|Acc]);
parse_template([{Key,numstr}|KT], [Val|VT], Acc) ->
    parse_template(KT, VT, [{Key, Val}|Acc]);
parse_template([{Key,xsers}|KT], [Val|VT], Acc) ->
    ValParsed = ucp_arg_syntax:parse_xsers(Val),
    parse_template(KT, VT, [{Key, ValParsed}|Acc]);
parse_template([{Key,time}|KT], [Val|VT], Acc) ->
    ValParsed = ucp_arg_syntax:parse_time(Val),
    parse_template(KT, VT, [{Key, ValParsed}|Acc]);
parse_template([{Key,{ip,_List}}|KT], [Val|VT], Acc) ->
    %% Nothing is done here to the IP address.
    %% You cannot tell yet if it shall be made.
    %% TODO: make this later
    parse_template(KT, VT, [{Key, Val}|Acc]).

%%% Template for data

%%% The list contains all fields needed in the data part of the
%%% message. Here is the layout:
%%% {Name::atom()}
%%% {Name::atom(), Type::atom() | IPType}
%%% {Name::atom(), Type::atom() | IPType, Size:integer()}
%%%   Type = num, hex_coded, chrstr, numstr, xsers, time
%%%   IPType = {ip, [{Name:atom(), Value:integer()}]}

data_template_01() ->
    [{adc,numstr,16},   %% Address of recipient
     {oadc,numstr,16},  %% Address of originator
     {ac,chrstr},       %% Authentication code originator
     {mt,num,1},        %% Message type
     {msg,chrstr,640}]. %% Message : Special treatment

data_template_02() ->
    [{npl,numstr},       %% Number of rads
     {rads,chrstr},      %% List of adc. TODO: needs special treatment
     {oadc,numstr,16},   %% Address of originator
     {ac,chrstr},        %% Authentication code originator
     {mt,num,1},         %% Message type
     {msg,chrstr}].      %% Message. TODO: needs special treatment

data_template_03() ->
    [{rad,chrstr},       %% Address of recipient (plus optional legitim. code)
                         %% TODO: needs special treatment
     {oadc,numstr,16},   %% Address of originator
     {ac,chrstr},        %% Authentication code originator
     {npl,numstr},       %% "0"
     {gas,chrstr},       %% Empty
     {rp,num,1},         %% Empty
     {pr,num,1},         %% Empty
     {lpr,numstr},       %% Empty
     {ur,num,1},         %% Empty
     {lur,numstr},       %% Empty
     {rc,num,1},         %% Empty
     {lrc,numstr},       %% Empty
     {dd,num,1},         %% Deferred delivery request = "1"
     {ddt,time,10},      %% Deferred delivery time
     {mt,num,1},         %% Message type
     {msg,chrstr}].      %% Message. TODO: needs special treatment

data_template_30() ->
    PID = ?UCP_PID_PC_OVER_TCPIP,
    [{adc,numstr,16},         %% Address of recipient
     {oadc,numstr,16},        %% Address of originator
     {ac,chrstr},             %% Authentication code originator
     {nrq,num,1},             %% Notification request = "1"
     {nad,{ip,[{npid,PID}]}}, %% Notification address of type npid
                              %% (IP if npid=539)
     {npid,num,4},            %% Notification PID value
     {dd,num,1},              %% Deferred delivery request = "1"
     {ddt,time,10},           %% Deferred delivery time
     {vp,time,10},            %% Validity period
     {msg,hex_coded}].        %% Message

data_template_31() ->
    PID = ?UCP_PID_PC_OVER_TCPIP,
    [{adc,{ip,[{pid,PID}]},16}, %% Address of SMT of type pid
                                %% (IP if pid=539)
     {pid,num,4}].              %% SMT pid value

data_template_5x() ->
    PID = ?UCP_PID_PC_OVER_TCPIP,
    [{adc,numstr,16},             %% Address of recipient
                                  %% (IP if xser recipient NPI = 5) <- TODO
     {oadc,chrstr,22},            %% Address of originator : special treatment
                                  %% (IP if xser originator NPI = 5) <- TODO
                                  %% (pack 7 if otoa=5039)
     {ac,numstr,16},              %% Authentication code originator
     {nrq,num,1},                 %% Notification request "0" or "1"
     {nadc,{ip,[{npid,PID}]},16}, %% Notification address of type npid
                                  %% (IP if npid=539)
     {nt,num,1},                  %% Notification type
     {npid,num,4},                %% Notification PID
     {lrq,num,1},                 %% Last resort address used "0" or "1"
     {lrad,{ip,[{lpid,PID}]},16}, %% Last resort address of type lpid
                                  %% (IP if lpid=539)
     {lpid,num,4},                %% Last resort address PID
     {dd,num,1},                  %% Deferred delivery request "0 or "1"
     {ddt,time,10},               %% Deferred delivery time
     {vp,time,10},                %% Deferred delivery validity period
     {rpid,num,5},                %% Replace PID
     {scts,time,12},              %% Service center time stamp
     {dst,num,1},                 %% Delivery status
     {rsn,num,3},                 %% Reason code
     {dscts,time,12},             %% Delivery time stamp
     {mt,num,1},                  %% Message type
     {nb,num,4},                  %% Number of bits
     {msg,chrstr,640},            %% Message : Special treatment
     {mms,num,1},                 %% More messages to send
     {pr,chrstr,1},               %% Priority requested
     {dcs,num,1},                 %% Deprecated data coding scheme
     {mcls,num,1},                %% Message class
     {rpi,num,1},                 %% Reply path
     {cpg,num,1},                 %% Reserved
     {rply,num,1},                %% Reserved
     {otoa,num,4},                %% Originator type of address
     {hplmn,numstr,16},           %% Home PLMN address
     {xser,xsers,400},            %% Extra services
     {res4,numstr},               %% Reserved
     {res5,numstr}].              %% Reserved

data_template_6x() ->
    ONPI = ?UCP_ONPI_SMSC_SPECIFIC,
    OPID = ?UCP_OPID_PC,
    [{oadc,{ip,[{onpi,ONPI},{opid,OPID}]},16}, %% Address of originator
                                               %% (IP if onpi,opid=5,39)
     {oton,num,1},              %% Originator type of number
     {onpi,num,1},              %% Originator numberin plan id
     {styp,num,1},              %% Subtype of operation
     {pwd,hex_coded},           %% Password
     {npwd,hex_coded},          %% New password
     {vers,num,4},              %% Version
     {ladc,{ip,[{lnpi,5}]},16}, %% Address of VSMSC list operation of type lnpi
                                %% (IP if lnpi=5)
     {lton,chrstr},             %% Type of number list operation
     {lnpi,chrstr},             %% Number plan ID for list address
     {opid,num,2},              %% Originator protocol identifier
     {res1,numstr}].            %% Reserved

%%% Encoding and decoding of the msg field for OT 5x

encode_msg_field(UCP) ->
    MT = get_value(mt, UCP),
    MSG = get_value(msg, UCP),

    case {MSG,MT} of
        {undefined,_} ->
            UCP;
        {_,?UCP_MT_ALPHANUMERIC} ->
            Encoded_MSG = ucp_arg_syntax:make_hex_coded(MSG),
            lists:keyreplace(msg, 1, UCP, {msg, Encoded_MSG});
        {_,?UCP_MT_TRANSPARENT_DATA} ->
            %% TODO: For this option to be valid the NB (number of
            %% bits) field need to be set. A test for this might be a
            %% good idea.
            Encoded_MSG = ucp_arg_syntax:make_hex_coded(MSG),
            lists:keyreplace(msg, 1, UCP, {msg, Encoded_MSG});
        {_,?UCP_MT_NUMERIC} ->
            UCP;
        {_,undefined} ->
            UCP
    end.

decode_msg_field(UCP) ->
    MT = get_value(mt, UCP),
    MSG = get_value(msg, UCP),

    case {MSG,MT} of
        {undefined,_} ->
            UCP;
        {_,?UCP_MT_ALPHANUMERIC} ->
            Decoded_MSG = ucp_arg_syntax:parse_hex_coded(MSG),
            lists:keyreplace(msg, 1, UCP, {msg, Decoded_MSG});
        {_,?UCP_MT_TRANSPARENT_DATA} ->
            Decoded_MSG = ucp_arg_syntax:parse_hex_coded(MSG),
            Binary_Decoded_MSG = erlang:list_to_binary(Decoded_MSG),
            lists:keyreplace(msg, 1, UCP, {msg, Binary_Decoded_MSG});
        {_,?UCP_MT_NUMERIC} ->
            UCP;
        {_,undefined} ->
            UCP
    end.


%%% Encoding and decoding of the oadc field for OT 5x

%%% TODO: as this is inefficient and the oadc field probably is the
%%% same for many (or all?) 5x operations - shall it be removed from
%%% the 5x encode/decode and assumed that the user provides packed
%%% data ??

encode_oadc_field(UCP) ->
    OTOA = get_value(otoa, UCP),
    OADC = get_value(oadc, UCP),
    case {OADC,OTOA} of
        {undefined,_} ->
            UCP;
        {_,?UCP_OTOA_TON_AND_NPI} ->
            UCP;
        {_,?UCP_OTOA_ALPHANUMERIC} ->
            Encoded_OADC = ucp_arg_syntax:make_pack7(OADC),
            Prefixed_OADC = ucp_arg_syntax:make_hex(2, length(Encoded_OADC)) ++ Encoded_OADC,
            lists:keyreplace(oadc, 1, UCP, {oadc, Prefixed_OADC});
        {_,undefined} ->
            UCP
    end.

decode_oadc_field(UCP) ->
    OTOA = get_value(otoa, UCP),
    OADC = get_value(oadc, UCP),
    case {OADC,OTOA} of
        {undefined,_} ->
            UCP;
        {_,?UCP_OTOA_TON_AND_NPI} ->
            UCP;
        {_,?UCP_OTOA_ALPHANUMERIC} ->
            Stripped_OADC = tl(tl(OADC)),
            Decoded_OADC = ucp_arg_syntax:parse_pack7(Stripped_OADC),
            lists:keyreplace(oadc, 1, UCP, {oadc, Decoded_OADC});
        {_,undefined} ->
            UCP
    end.

%%% Decoding of IP addresses

%%% TODO: here shall the decode code be written.
%%%
%%% There will be rather much searching and converting for several IP
%%% addresses and each time a 3x, 5x or 6x message is recieved (and
%%% also sent as the current implementation parses output also).

%%% Test if the Type/OT is supported by the parsing code

test_supported(Type, OT, Data) ->
    case Type of
        "O" ->
            true = supported(OT, parse);
        "R" ->
            [ACK | _] = Data,
            case ACK of
                "A" ->
                    true = supported(OT, parse_ack);
                "N" ->
                    true
            end
    end.

% %%% Test if the checksum is correct
% 
% test_checksum(Chk, String) ->
%     Chk = ucp_arg_syntax:make_checksum(String).
