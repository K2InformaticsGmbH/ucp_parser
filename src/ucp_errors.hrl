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

%%% /Roland Karlsson

%%% ----------------------------------------------------------
%%% FILE: UCP_ERRORS.HRL
%%%
%%% Error codes
%%% ----------------------------------------------------------

-ifndef(ucp_errors).
-define(ucp_errors, 1).

%%% Delivery status codes used in notification messages (OT 53)

-define(UCP_DST_DELIVERED,     0).
-define(UCP_DST_BUFFERED,      1).
-define(UCP_DST_NOT_DELIVERED, 2).

%%% Error codes used in notification messages (OT 53)
%%% Decimal values and error string
%%% TODO: organize so you can search

-define(UCP_RSN_UNKNOWN_SUBSCRIBER,                {000, "Unknown subscriber"}).
-define(UCP_RSN_SERVICE_TEMPORARY_NOT_AVAILABLE,   {001, "Service temporary not available"}).
-define(UCP_RSN_SERVICE_TEMPORARY_NOT_AVAILABLE2,  {002, "Service temporary not available"}).
-define(UCP_RSN_SERVICE_TEMPORARY_NOT_AVAILABLE3,  {003, "Service temporary not available"}).
-define(UCP_RSN_SERVICE_TEMPORARY_NOT_AVAILABLE4,  {004, "Service temporary not available"}).
-define(UCP_RSN_SERVICE_TEMPORARY_NOT_AVAILABLE5,  {005, "Service temporary not available"}).
-define(UCP_RSN_SERVICE_TEMPORARY_NOT_AVAILABLE6,  {006, "Service temporary not available"}).
-define(UCP_RSN_SERVICE_TEMPORARY_NOT_AVAILABLE7,  {007, "Service temporary not available"}).
-define(UCP_RSN_SERVICE_TEMPORARY_NOT_AVAILABLE8,  {008, "Service temporary not available"}).
-define(UCP_RSN_ILLEGAL_ERROR_CODE,                {009, "Illegal error code"}).
-define(UCP_RSN_NETWORK_TIME_OUT,                  {010, "Network time out"}).
-define(UCP_RSN_FACILITY_NOT_SUPPORTED,            {100, "Facility not supported"}).
-define(UCP_RSN_UNKNOWN_SUBSCRIBER2,               {101, "Unknown subscriber"}).
-define(UCP_RSN_SERVICE_NOT_AVAILABLE,             {102, "Service not available"}).
-define(UCP_RSN_CALL_BARRED,                       {103, "Call barred"}).
-define(UCP_RSN_OPERATION_BARRED,                  {104, "Operation barred"}).
-define(UCP_RSN_SC_CONGESTION,                     {105, "SC congestion"}).
-define(UCP_RSN_SERVICE_NOT_SUPPORTED,             {106, "Service not supported"}).
-define(UCP_RSN_ABSENT_SUBSCRIBER,                 {107, "Absent subscriber"}).
-define(UCP_RSN_DELIVERY_FAIL,                     {108, "Delivery fail"}).
-define(UCP_RSN_SC_CONGESTION2,                    {109, "SC congestion"}).
-define(UCP_RSN_PROTOCOL_ERROR,                    {110, "Protocol error"}).
-define(UCP_RSN_MS_NOT_EQUIPPED,                   {111, "MS not equipped"}).
-define(UCP_RSN_UNKNOWN_SC,                        {112, "Unknown SC"}).
-define(UCP_RSN_SC_CONGESTION3,                    {113, "SC congestion"}).
-define(UCP_RSN_ILLEGAL_MS,                        {114, "Illegal MS"}).
-define(UCP_RSN_MS_NOT_A_SUBSCRIBER,               {115, "MS not a subscriber"}).
-define(UCP_RSN_ERROR_IN_MS,                       {116, "Error in MS"}).
-define(UCP_RSN_SMS_LOWER_LAYERS_NOT_PROVISIONED,  {117, "SMS lower layers not provisioned"}).
-define(UCP_RSN_SYSTEM_FAIL,                       {118, "System fail"}).
-define(UCP_RSN_PLMN_SYSTEM_FAILURE,               {119, "PLMN system failure"}).
-define(UCP_RSN_HLR_SYSTEM_FAILURE,                {120, "HLR system failure"}).
-define(UCP_RSN_VLR_SYSTEM_FAILURE,                {121, "VLR system failure"}).
-define(UCP_RSN_PREVIOUS_VLR_SYSTEM_FAILURE,       {122, "Previous VLR system failure"}).
-define(UCP_RSN_CONTROLLING_MSC_SYSTEM_FAILURE,    {123, "Controlling MSC system failure"}).
-define(UCP_RSN_VMSC_SYSTEM_FAILURE,               {124, "VMSC system failure"}).
-define(UCP_RSN_EIR_SYSTEM_FAILURE,                {125, "EIR system failure"}).
-define(UCP_RSN_SYSTEM_FAILURE,                    {126, "System failure"}).
-define(UCP_RSN_UNEXPECTED_DATA_VALUE,             {127, "Unexpected data value"}).
-define(UCP_RSN_ERRORR_IN_ADDRESS_SERVICE_CENTER,  {200, "Error in address service center"}).
-define(UCP_RSN_INVALID_ABSOLUTE_VALIDITY_PERIOD,  {201, "Invalid absolute validity period"}).
-define(UCP_RSN_SHORT_MESSAGE_EXCEEDS_MAXIMUM,     {202, "Short message exceeds maximum"}).
-define(UCP_RSN_UNABLE_TO_UNPACK_SMS_MESSAGE,      {203, "Unable to unpack SMS message"}).
-define(UCP_RSN_UNABLE_TO_CONVERT_TO_IRA_ALPHABET, {204, "Unable to convert to IRA alphabet"}).
-define(UCP_RSN_INVALID_VALIDITY_PERIOD_FORMAT,    {205, "Invalid validity period format"}).
-define(UCP_RSN_INVALID_DESTINATION_ADDRESS,       {206, "Invalid destination address"}).
-define(UCP_RSN_DUPLICATE_MESSAGE_SUBMIT,          {207, "Duplicate message submit"}).
-define(UCP_RSN_INVALID_MESSAGE_TYPE_INDICATOR,    {208, "Invalid message type indicator"}).

%%% Error codes used in NACK messages
%%% Decimal values and error string
%%% TODO: organize so you can search
%%% TODO: Special occurances - see Vodafone spec page 40

-define(UCP_NACK_CHECKSUM_ERROR,                    {01, "Checksum error"}).
-define(UCP_NACK_SYNTAX_ERROR,                      {02, "Syntax error"}).
-define(UCP_NACK_OPERATION_NOT_SUPPORTED,           {03, "Operation not supported by system"}).
-define(UCP_NACK_OPERATION_NOT_ALLOWED,             {04, "Operation not allowed"}).
-define(UCP_NACK_CALLING_BARRING_ACTIVE,            {05, "Calling barring active"}).
-define(UCP_NACK_ADC_INVALID,                       {06, "AdC invalid"}).
-define(UCP_NACK_AUTHENTICATION_FAILURE,            {07, "Authentication failure"}).
-define(UCP_NACK_LEGITIMISATION_ALL_CODE_FAILURE,   {08, "Legitimisation code for all code, failure"}).
-define(UCP_NACK_GA_NOT_VALID,                      {09, "GA not valid"}).
-define(UCP_NACK_REPETITION_NOT_ALLOWED,            {10, "Repetition not allowed"}).
-define(UCP_NACK_LEGITIMISATION_REPETITION_FAILURE, {11, "Legitimisation code for repetition, failure"}).
-define(UCP_NACK_PRIORITY_CALL_NOT_ALLOWED,         {12, "Priority call not allowed"}).
-define(UCP_NACK_LEGITIMISATION_PRIORITY_FAILURE,   {13, "Legitimisation code for priority call, failure"}).
-define(UCP_NACK_URGENT_MESSAGE_NOT_ALLOWED,        {14, "Urgent message not allowed"}).
-define(UCP_NACK_LEGITIMISATION_URGENT_FAILURE,     {15, "Legitimisation code for urgent message, failure"}).
-define(UCP_NACK_REVERSE_CHARGING_NOT_ALLOWED,      {16, "Reverse charging not allowed"}).
-define(UCP_NACK_LEGITIMISATION_REVERSE_FAILURE,    {17, "Legitimisation code for reverse charging, failure"}).
-define(UCP_NACK_DEFERRED_DELIVER_NOT_ALLOWED,      {18, "Deferred delivery not allowed"}).
-define(UCP_NACK_NEW_AC_NOT_VALID,                  {19, "New AC not valid"}).
-define(UCP_NACK_NEW_LEGITIMSATION_CODE_NOT_VALID,  {20, "New legitimisation code not valid"}).
-define(UCP_NACK_STANDARD_TEX_NOT_VALID,            {21, "Standard text not valid"}).
-define(UCP_NACK_TIME_PERIOD_NOT_VALID,             {22, "Time period not valid"}).
-define(UCP_NACK_MESSAGE_TYPE_NOT_SUPPORTED,        {23, "Message type not supported"}).
-define(UCP_NACK_MASSAGE_TOO_LONG,                  {24, "Message too long"}).
-define(UCP_NACK_REQUESTED_STANDARD_TEXT_NOT_VALID, {25, "Requested standard text not valid"}).
-define(UCP_NACK_MESSAGE_TYPE_NOT_VALID_FOR_PAGER,  {26, "Message type not valid for teh pager type"}).
-define(UCP_NACK_MASSAGE_NOT_FOUND_IN_SMSC,         {27, "Message not found in SMSC"}).
-define(UCP_NACK_SUBSCRIBER_HANG_UP,                {30, "Subscriber hang-up"}).
-define(UCP_NACK_FAX_GROUP_NOT_SUPPORTED,           {31, "Fax group not supported"}).
-define(UCP_NACK_FAX_MESSAGE_TYPE_NOT_SUPPORTED,    {32, "Fax message type not supported"}).
-define(UCP_NACK_ADDRESS_ALREADY_IN_LIST,           {33, "Address already in list"}).
-define(UCP_NACK_ADDRESS_NOT_IN_LIST,               {34, "Address not in list"}).
-define(UCP_NACK_LIST_FULL,                         {35, "List full"}).
-define(UCP_NACK_RPID_ALREADY_IN_USE,               {36, "RPID already in use"}).
-define(UCP_NACK_DELIVERY_IN_PROGRESS,              {37, "Delivery in progress"}).
-define(UCP_NACK_MESSAGE_FORWARDED,                 {38, "Message forwarded"}).

-endif.
