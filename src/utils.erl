% ==================================================================================
% Copyright (c) 2011 | IEISS | www.ieiss.com | info@ieiss.com
% 
% Permission is hereby granted, free of charge, to any person obtaining
% a copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the Software
% is furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
% OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
% ==================================================================================

-module(utils).
-include_lib("eunit/include/eunit.hrl").
-export([parse_to_dict/1
		,fragment_buffer_data/2
		,get_msg_type/1
		,check_msg_credentials/2
		,get_tag_text/1]).

%%-------------------------------------------------------------------
parse_to_dict(Data) ->
	try
		L=string:tokens(binary_to_list(Data), "\001"),
		{ok, dict:from_list([list_to_tuple(string:tokens(T,"=")) || T <- L])}
	catch
		_:_Reason -> {error, "17"}
	end.

%%-------------------------------------------------------------------
fragment_buffer_data(Data1, Data2) ->
	Data = <<Data1/binary, Data2/binary>>,
	process_buffer_data(re:split(Data, <<"(10=[0-9][0-9][0-9]\001)">>, [trim]), []).

%%-------------------------------------------------------------------
process_buffer_data([], Data) ->
	lists:append([<<>>], Data);

process_buffer_data([S], Data) ->
	lists:append([S], Data);

process_buffer_data([X,Y|Z], []) ->
	L = <<X/binary, Y/binary>>,
	CHK=verify_socket_data(L),
	process_buffer_data(Z, [{CHK, L}]);

process_buffer_data([X,Y|Z], SoFar) -> 
	L = <<X/binary, Y/binary>>,
	CHK=verify_socket_data(L),
	process_buffer_data(Z, lists:append(SoFar, [{CHK, L}])).

%%-------------------------------------------------------------------
verify_socket_data(_Data = <<"8=FIXT.1.1",1,"9="
		,BLen:2/binary,1,"35=",DataRest/binary>>) ->
	BL = list_to_integer(binary_to_list(BLen)),
	N = BL-3,
	<<Bin1:N/binary,Bin2/binary>> = DataRest,
	verify_checksum(BL+16, {Bin1, Bin2});

verify_socket_data(_Data = <<"8=FIXT.1.1",1,"9="
		,BLen:3/binary,1,"35=",DataRest/binary>>) ->
	BL = list_to_integer(binary_to_list(BLen)),
	N = BL-3,
	<<Bin1:N/binary,Bin2/binary>> = DataRest,
	verify_checksum(BL+17, {Bin1, Bin2});
verify_socket_data(_Data = <<"8=FIXT.1.1",1,"9="
		,BLen:4/binary,1,"35=",DataRest/binary>>) ->
	BL = list_to_integer(binary_to_list(BLen)),
	N = BL-3,
	<<Bin1:N/binary,Bin2/binary>> = DataRest,
	verify_checksum(BL+18, {Bin1, Bin2});
verify_socket_data(_Data = <<"8=FIXT.1.1",1,"9="
		,BLen:5/binary,1,"35=",DataRest/binary>>) ->
	BL = list_to_integer(binary_to_list(BLen)),
	N = BL-3,
	<<Bin1:N/binary,Bin2/binary>> = DataRest,
	verify_checksum(BL+19, {Bin1, Bin2});
verify_socket_data(_Data = <<"8=FIXT.1.1",1,"9="
		,BLen:6/binary,1,"35=",DataRest/binary>>) ->
	BL = list_to_integer(binary_to_list(BLen)),
	N = BL-3,
	<<Bin1:N/binary,Bin2/binary>> = DataRest,
	verify_checksum(BL+20, {Bin1, Bin2});
verify_socket_data(_Data = <<"8=FIXT.1.1",1,"9="
		,BLen:7/binary,1,"35=",DataRest/binary>>) ->
	BL = list_to_integer(binary_to_list(BLen)),
	N = BL-3,
	<<Bin1:N/binary,Bin2/binary>> = DataRest,
	verify_checksum(BL+21, {Bin1, Bin2});
verify_socket_data(_Data) ->
	false.

%%-------------------------------------------------------------------
verify_checksum(BL, {_, <<"10=",CHK:3/binary, 1>>}) when BL > 3 ->
	CHKSum = list_to_integer(binary_to_list(CHK)),
	CHKSum =:= BL rem 256;
verify_checksum(_BLen, _Body) ->
	false.

%%--------------------------------------------------------
get_msg_type("0") -> "Heartbeat";
get_msg_type("1") -> "TestRequest";
get_msg_type("2") -> "ResendRequest";
get_msg_type("3") -> "Reject";
get_msg_type("4") -> "SequenceReset";
get_msg_type("5") -> "Logout";
get_msg_type("6") -> "IOI";
get_msg_type("7") -> "Advertisement";
get_msg_type("8") -> "ExecutionReport";
get_msg_type("9") -> "OrderCancelReject";
get_msg_type("A") -> "Logon";
get_msg_type("AA") -> "DerivativeSecurityList";
get_msg_type("AB") -> "NewOrderMultileg";
get_msg_type("AC") -> "MultilegOrderCancelReplace";
get_msg_type("AD") -> "TradeCaptureReportRequest";
get_msg_type("AE") -> "TradeCaptureReport";
get_msg_type("AF") -> "OrderMassStatusRequest";
get_msg_type("AG") -> "QuoteRequestReject";
get_msg_type("AH") -> "RFQRequest";
get_msg_type("AI") -> "QuoteStatusReport";
get_msg_type("AJ") -> "QuoteResponse";
get_msg_type("AK") -> "Confirmation";
get_msg_type("AL") -> "PositionMaintenanceRequest";
get_msg_type("AM") -> "PositionMaintenanceReport";
get_msg_type("AN") -> "RequestForPositions";
get_msg_type("AO") -> "RequestForPositionsAck";
get_msg_type("AP") -> "PositionReport";
get_msg_type("AQ") -> "TradeCaptureReportRequestAck";
get_msg_type("AR") -> "TradeCaptureReportAck";
get_msg_type("AS") -> "AllocationReport";
get_msg_type("AT") -> "AllocationReportAck";
get_msg_type("AU") -> "Confirmation_Ack";
get_msg_type("AV") -> "SettlementInstructionRequest";
get_msg_type("AW") -> "AssignmentReport";
get_msg_type("AX") -> "CollateralRequest";
get_msg_type("AY") -> "CollateralAssignment";
get_msg_type("AZ") -> "CollateralResponse";
get_msg_type("B") -> "News";
get_msg_type("BA") -> "CollateralReport";
get_msg_type("BB") -> "CollateralInquiry";
get_msg_type("BC") -> "NetworkCounterpartySystemStatusRequest";
get_msg_type("BD") -> "NetworkCounterpartySystemStatusResponse";
get_msg_type("BE") -> "UserRequest";
get_msg_type("BF") -> "UserResponse";
get_msg_type("BG") -> "CollateralInquiryAck";
get_msg_type("BH") -> "ConfirmationRequest";
get_msg_type("BI") -> "TradingSessionListRequest";
get_msg_type("BJ") -> "TradingSessionList";
get_msg_type("BK") -> "SecurityListUpdateReport";
get_msg_type("BL") -> "AdjustedPositionReport";
get_msg_type("BM") -> "AllocationInstructionAlert";
get_msg_type("BN") -> "ExecutionAcknowledgement";
get_msg_type("BO") -> "ContraryIntentionReport";
get_msg_type("BP") -> "SecurityDefinitionUpdateReport";
get_msg_type("BQ") -> "SettlementObligationReport";
get_msg_type("BR") -> "DerivativeSecurityListUpdateReport";
get_msg_type("BS") -> "TradingSessionListUpdateReport";
get_msg_type("BT") -> "MarketDefinitionRequest";
get_msg_type("BU") -> "MarketDefinition";
get_msg_type("BV") -> "MarketDefinitionUpdateReport";
get_msg_type("BW") -> "ApplicationMessageRequest";
get_msg_type("BX") -> "ApplicationMessageRequestAck";
get_msg_type("BY") -> "ApplicationMessageReport";
get_msg_type("BZ") -> "OrderMassActionReport";
get_msg_type("C") -> "Email";
get_msg_type("CA") -> "OrderMassActionRequest";
get_msg_type("CB") -> "UserNotification";
get_msg_type("CC") -> "StreamAssignmentRequest";
get_msg_type("CD") -> "StreamAssignmentReport";
get_msg_type("CE") -> "StreamAssignmentReportACK";
get_msg_type("CF") -> "PartyDetailsListRequest";
get_msg_type("CG") -> "PartyDetailsListReport";
get_msg_type("D") -> "NewOrderSingle";
get_msg_type("E") -> "NewOrderList";
get_msg_type("F") -> "OrderCancelRequest";
get_msg_type("G") -> "OrderCancelReplaceRequest";
get_msg_type("H") -> "OrderStatusRequest";
get_msg_type("J") -> "AllocationInstruction";
get_msg_type("K") -> "ListCancelRequest";
get_msg_type("L") -> "ListExecute";
get_msg_type("M") -> "ListStatusRequest";
get_msg_type("N") -> "ListStatus";
get_msg_type("P") -> "AllocationInstructionAck";
get_msg_type("Q") -> "DontKnowTradeDK";
get_msg_type("R") -> "QuoteRequest";
get_msg_type("S") -> "Quote";
get_msg_type("T") -> "SettlementInstructions";
get_msg_type("V") -> "MarketDataRequest";
get_msg_type("W") -> "MarketDataSnapshotFullRefresh";
get_msg_type("X") -> "MarketDataIncrementalRefresh";
get_msg_type("Y") -> "MarketDataRequestReject";
get_msg_type("Z") -> "QuoteCancel";
get_msg_type("a") -> "QuoteStatusRequest";
get_msg_type("b") -> "MassQuoteAcknowledgement";
get_msg_type("c") -> "SecurityDefinitionRequest";
get_msg_type("d") -> "SecurityDefinition";
get_msg_type("e") -> "SecurityStatusRequest";
get_msg_type("f") -> "SecurityStatus";
get_msg_type("g") -> "TradingSessionStatusRequest";
get_msg_type("h") -> "TradingSessionStatus";
get_msg_type("i") -> "MassQuote";
get_msg_type("j") -> "BusinessMessageReject";
get_msg_type("k") -> "BidRequest";
get_msg_type("l") -> "BidResponse";
get_msg_type("m") -> "ListStrikePrice";
get_msg_type("n") -> "XML_non_FIX";
get_msg_type("o") -> "RegistrationInstructions";
get_msg_type("p") -> "RegistrationInstructionsResponse";
get_msg_type("q") -> "OrderMassCancelRequest";
get_msg_type("r") -> "OrderMassCancelReport";
get_msg_type("s") -> "NewOrderCross";
get_msg_type("t") -> "CrossOrderCancelReplaceRequest";
get_msg_type("u") -> "CrossOrderCancelRequest";
get_msg_type("v") -> "SecurityTypeRequest";
get_msg_type("w") -> "SecurityTypes";
get_msg_type("x") -> "SecurityListRequest";
get_msg_type("y") -> "SecurityList";
get_msg_type("z") -> "DerivativeSecurityListRequest";
get_msg_type(_) ->	"***Invalid Message Type***".

%%--------------------------------------------------------
get_tag_text("7") -> "BeginSeqNo";
get_tag_text("8") -> "BeginString";
get_tag_text("9") -> "BodyLength";
get_tag_text("10") -> "CheckSum";
get_tag_text("16") -> "EndSeqNo";
get_tag_text("34") -> "MsgSeqNum";
get_tag_text("35") -> "MsgType";
get_tag_text("36") -> "NewSeqNo";
get_tag_text("43") -> "PossDupFlag";
get_tag_text("45") -> "RefSeqNum";
get_tag_text("49") -> "SenderCompID";
get_tag_text("50") -> "SenderSubID";
get_tag_text("52") -> "SendingTime";
get_tag_text("56") -> "TargetCompID";
get_tag_text("57") -> "TargetSubID";
get_tag_text("58") -> "Text";
get_tag_text("89") -> "Signature";
get_tag_text("90") -> "SecureDataLen";
get_tag_text("91") -> "SecureData";
get_tag_text("93") -> "SignatureLength";
get_tag_text("95") -> "RawDataLength";
get_tag_text("96") -> "RawData";
get_tag_text("97") -> "PossResend";
get_tag_text("98") -> "EncryptMethod";
get_tag_text("108") -> "HeartBtInt";
get_tag_text("112") -> "TestReqID";
get_tag_text("115") -> "OnBehalfOfCompID";
get_tag_text("116") -> "OnBehalfOfSubID";
get_tag_text("122") -> "OrigSendingTime";
get_tag_text("123") -> "GapFillFlag";
get_tag_text("128") -> "DeliverToCompID";
get_tag_text("129") -> "DeliverToSubID";
get_tag_text("141") -> "ResetSeqNumFlag";
get_tag_text("142") -> "SenderLocationID";
get_tag_text("143") -> "TargetLocationID";
get_tag_text("144") -> "OnBehalfOfLocationID";
get_tag_text("145") -> "DeliverToLocationID";
get_tag_text("212") -> "XmlDataLen";
get_tag_text("213") -> "XmlData";
get_tag_text("347") -> "MessageEncoding";
get_tag_text("354") -> "EncodedTextLen";
get_tag_text("355") -> "EncodedText";
get_tag_text("369") -> "LastMsgSeqNumProcessed";
get_tag_text("371") -> "RefTagID";
get_tag_text("372") -> "RefMsgType";
get_tag_text("373") -> "SessionRejectReason";
get_tag_text("383") -> "MaxMessageSize";
get_tag_text("464") -> "TestMessageIndicator";
get_tag_text("553") -> "Username";
get_tag_text("554") -> "Password";
get_tag_text("627") -> "NoHops";
get_tag_text("628") -> "HopCompID";
get_tag_text("629") -> "HopSendingTime";
get_tag_text("630") -> "HopRefID";
get_tag_text("789") -> "NextExpectedMsgSeqNum";
get_tag_text("925") -> "NewPassword";
get_tag_text("1128") -> "ApplVerID";
get_tag_text("1129") -> "CstmApplVerID";
get_tag_text("1130") -> "RefApplVerID";
get_tag_text("1131") -> "RefCstmApplVerID";
get_tag_text("1137") -> "DefaultApplVerID";
get_tag_text("1156") -> "ApplExtID";
get_tag_text("1400") -> "EncryptedPasswordMethod";
get_tag_text("1401") -> "EncryptedPasswordLen";
get_tag_text("1402") -> "EncryptedPassword";
get_tag_text("1403") -> "EncryptedNewPasswordLen";
get_tag_text("1404") -> "EncryptedNewPassword";
get_tag_text("1406") -> "RefApplExtID";
get_tag_text("1407") -> "DefaultApplExtID";
get_tag_text("1408") -> "DefaultCstmApplVerID";
get_tag_text("1409") -> "SessionStatus";
get_tag_text(_) ->	"**Invalid Tag***".

%%--------------------------------------------------------
check_msg_credentials(ServerData, Msg) ->
	BaseMsg = find_base_msg(ServerData, Msg),
	verify_message(Msg, BaseMsg).

%%--------------------------------------------------------
find_base_msg(ServerData, Msg) ->
	case dict:fetch("35", Msg) of
		"0" -> dict:from_list(dict:fetch(in_heartbeat, ServerData));
		"1" -> dict:from_list(dict:fetch(in_testrequest, ServerData));
		"2" -> dict:from_list(dict:fetch(in_resendrequest, ServerData));
		"3" -> dict:from_list(dict:fetch(in_reject, ServerData));
		"4" -> dict:from_list(dict:fetch(in_sequencereset, ServerData)); 
		"5" -> dict:from_list(dict:fetch(in_logout, ServerData));
		"A" -> dict:from_list(dict:fetch(in_logon, ServerData));
		_ -> dict:from_list(dict:fetch(in_application, ServerData))
	end.

%%--------------------------------------------------------
verify_message(Msg, BaseMsg) ->
	L = dict:fetch_keys(BaseMsg),
	verify_tags(Msg, BaseMsg, true, L).

%%--------------------------------------------------------
verify_tags(_Msg, _BaseMsg, Status, []) ->	
	Status;
verify_tags(Msg, BaseMsg, _Status, [H|T]) ->	
	case check_tag_value(dict:find(H, Msg), dict:find(H, BaseMsg)) of
		true ->
			verify_tags(Msg, BaseMsg, true, T);
		false ->
			false
	end.

%%--------------------------------------------------------
check_tag_value(error, _BaseTagValue) -> false;
check_tag_value({ok, _TagValue}, {ok, ""}) -> true;
check_tag_value({ok, TagValue}, {ok, BaseTagValue}) -> (TagValue == BaseTagValue).
