%%% /Roland Karlsson

%%% ----------------------------------------------------------
%%% FILE: UCP_DEFINES.HRL
%%%
%%% Defines for the UCP protocol
%%% ----------------------------------------------------------

-ifndef(ucp_defines).
-define(ucp_defines, 1).

%%% Operation types

-define(UCP_OT_CALL_INPUT,               01).
-define(UCP_OT_MULTI_ADDR_CALL_INPUT,    02).
-define(UCP_OT_CALL_INPUT_SUPPLEMENTARY, 03).
-define(UCP_OT_MS_MESSAGE_TRANSFER,      30).
-define(UCP_OT_MT_ALERT,                 31).
-define(UCP_OT_SUBMIT_SM,                51).
-define(UCP_OT_DELIVERY_SM,              52).
-define(UCP_OT_DELIVERY_SM_NOTIFICATION, 53).
-define(UCP_OT_MODIFY_SM,                54).
-define(UCP_OT_INQUIRY_SM,               55).
-define(UCP_OT_DELETE_SM,                56).
-define(UCP_OT_RESPONSE_INQUIRY_SM,      57).
-define(UCP_OT_RESPONSE_DELETE_SM,       58).
-define(UCP_OT_SESSION_MANAGEMENT,       60).
-define(UCP_OT_PROVISIONING_ACTIONS,     61).

%%% PID Value (OT 3x and 5x)

-define(UCP_PID_MOBILE_STATION,   0100).
-define(UCP_PID_FAX_GROUP_3,      0122).
-define(UCP_PID_X400,             0131).
-define(UCP_PID_MENU_OVER_PSTN,   0138).
-define(UCP_PID_PC_OVER_PSTN,     0139).
-define(UCP_PID_PC_OVER_X25,      0339).
-define(UCP_PID_PC_OVER_ISDN,     0439).
-define(UCP_PID_PC_OVER_TCPIP,    0539).
-define(UCP_PID_PC_ABBREV_NUMBER, 0639).

%%% Message types (OT 0x and 5x) 

-define(UCP_MT_NUMERIC,          2).
-define(UCP_MT_ALPHANUMERIC,     3).
-define(UCP_MT_TRANSPARENT_DATA, 4).

%%% Sub type operation for session management (OT 60)

-define(UCP_STYP_OPEN_SESSION,                 1).
-define(UCP_STYP_RESERVED,                     2).
-define(UCP_STYP_CHANGE_PASSWORD,              3).
-define(UCP_STYP_OPEN_PROVISIONING_SESSION,    4).
-define(UCP_STYP_RESERVED2,                    5).
-define(UCP_STYP_CHANGE_PROVISIONING_PASSWORD, 6).

%%% Originator type of number (OT 60)

-define(UCP_OTON_INTERNATIONAL, 1).
-define(UCP_OTON_NATIONAL,      2).
-define(UCP_OTON_ABBREVIATED,   6).

%%% Originator type of address (OT 5x)

-define(UCP_OTOA_TON_AND_NPI,  1139).
-define(UCP_OTOA_ALPHANUMERIC, 5039).

%%% Originator protocol identifier (OT 60)

-define(UCP_OPID_MOBILE_STATION, 00).
-define(UCP_OPID_PC,             39).

%%% Originator numbering plan (OT 60)

-define(UCP_ONPI_E164,          1).
-define(UCP_ONPI_X121,          3).
-define(UCP_ONPI_SMSC_SPECIFIC, 5).

%%% Xser service (OT 5x)

-define(UCP_XSER_SERVICE_GSM_UDH,                16#01).
-define(UCP_XSER_SERVICE_GSM_DCS,                16#02).

-define(UCP_XSER_SERVICE_XDMA_MESSAGE_TYPE,      16#03).
-define(UCP_XSER_SERVICE_XDMA_MESSAGE_REFERENCE, 16#04).
-define(UCP_XSER_SERVICE_XDMA_PRIVACY,           16#05).
-define(UCP_XSER_SERVICE_XDMA_URGENCY,           16#06).
-define(UCP_XSER_SERVICE_XDMA_ACK_REQ,           16#07).
-define(UCP_XSER_SERVICE_XDMA_MESSAGE_UPDATING,  16#08).
-define(UCP_XSER_SERVICE_XDMA_CALL_BACK_NUMBER,  16#09).
-define(UCP_XSER_SERVICE_XDMA_RESPONSE_CODE,     16#0A).
-define(UCP_XSER_SERVICE_XDMA_TELESERVICE_ID,    16#0B).

-define(UCP_XSER_SERVICE_BILLING_IDENTIFIER,     16#0C).
-define(UCP_XSER_SERVICE_SINGLE_SHOT,            16#0D).
-define(UCP_XSER_SERVICE_ORIGINATOR_TON,         16#0E).
-define(UCP_XSER_SERVICE_ORIGINATOR_NPI,         16#0F).
-define(UCP_XSER_SERVICE_RECIPIENT_TON,          16#10).
-define(UCP_XSER_SERVICE_RECIPIENT_NPI,          16#11).

%%% Xser service GSM UDH subtype (OT 5x)

-define(UCP_XSER_SERVICE_GSM_UDH_CONCATENATED_SM, 0).
-define(UCP_XSER_SERVICE_GSM_UDH_APPLICATION_PORTS, 4).


%%% TODO - there are lots more constants in the specification


-endif.
