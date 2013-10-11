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
		]).

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
