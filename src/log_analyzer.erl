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

-module(log_analyzer).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-import(utils, 
		[parse_to_dict/1
		,fragment_buffer_data/2
		,check_msg_credentials/2
		,get_tag_text/1
		,get_msg_type/1]).

%% API
-export([start_link/1, stop/1]).

%% gen_server callbacks
-export([init/1
		,handle_call/3
		,handle_cast/2
		,handle_info/2
		,terminate/2
		,code_change/3]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(S) ->
	gen_server:start_link({local, log_analyzer}, ?MODULE, S, []).

stop(_S) ->
    gen_server:cast(self(), stop).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(S) ->
	{ok, S}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({input_file, FileName}, State) ->
	case file:read_file(FileName) of
		{ok, Data} ->
			{{D1,D2,D3},{T1,T2,T3}}= erlang:localtime(),
			OF = string:join(["output", integer_to_list(D1),integer_to_list(D2),integer_to_list(D3)
						,integer_to_list(T1),integer_to_list(T2),integer_to_list(T3), ".txt"],""),
			[_H|T] = fragment_buffer_data( <<>> , Data),
			process_multiple_data(T, dict:store(out_file, OF, State));
		{error, Reason} -> 
			io:format("log_analyzer: error while reading file. Reason= ~w~n", [Reason])
	end,
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
process_multiple_data([{Status, Data}|T], State) ->
	process_data(Status, Data, State),
	process_multiple_data(T, State);
process_multiple_data([], _State) ->
	ok.

process_data(true, Data, State) ->
	process_message(parse_to_dict(Data), Data, State);

process_data(false, _Data, State) ->
	F=dict:fetch(out_file, State),
	file:write_file(F, <<"---------Message---------","\n"
						,"Garbled message detected.","\n\n">>, [append]).	
	
process_message({ok, M}, Data, State) ->
	F=dict:fetch(out_file, State),
	CHK = list_to_binary(atom_to_list(check_msg_credentials(State, M))),
	S = list_to_binary(string:join(string:tokens(binary_to_list(Data), "\001"), "|")),

	case dict:is_key("34", M) and dict:is_key("35", M) of
		true ->
			MsgType = list_to_binary(get_msg_type(dict:fetch("35", M))),
			MsgSeq = list_to_binary(dict:fetch("34", M)),
			file:write_file(F, <<"---------Message---------","\n",S/binary,"\n","Valid Message. BodyLength OK. CheckSum OK. "
								,"Credential Check= ",CHK/binary
								,". MsgSeqNum= ",MsgSeq/binary,". MsgType= ",MsgType/binary
								,"\n\n">>, [append]);
		false ->					
			ok			
	end;
	
process_message({error, _Reason}, Data, State) -> 
	S = list_to_binary(string:join(string:tokens(binary_to_list(Data), "\001"), "|")),
	file:write_file(dict:fetch(out_file, State), <<"---------Message---------","\n",S/binary,"\n"
						,"Invalid format. Message cannot be parsed.","\n\n">>, [append]).
