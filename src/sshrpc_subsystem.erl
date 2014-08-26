%% Copyright (c) 2009-2010 Kenji Rikitake. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Kenji Rikitake <kenji.rikitake@acm.org>
%% @copyright 2009-2010 Kenji Rikitake

%% @doc general sshrpc server-side subsystem.

-module(sshrpc_subsystem).

-behaviour(ssh_channel).

-include("sshrpc.hrl").

%% ssh_channel callbacks
-export([subsystem_spec/1]).
-export([init/1, handle_ssh_msg/2, handle_msg/2, terminate/2, code_change/3]).

%% state
-record(state, {
	  cm,
	  channel,
	  pending    % binary()
	 }).

%%====================================================================
%% API
%%====================================================================

%% subsystem name conforms the private method and algorithm
%% convention on RFC4251 Section 6.

-spec subsystem_spec(term()) -> {string(), {term(), term()}}.

subsystem_spec(Options) ->
        {"sshrpc@k2r.org", {?MODULE, Options}}.

%%====================================================================
%% ssh_channel callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} 
%%                        
%% Description: Initiates the CLI
%%--------------------------------------------------------------------

-spec init(term()) -> {ok, #state{}}.

init(_Options) ->
    {ok, #state{pending = <<>>}}.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description:
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%--------------------------------------------------------------------
%% Function: handle_ssh_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%                        
%% Description: Handles channel messages received on the ssh-connection.
%%--------------------------------------------------------------------
%% Type 0: RPC messages
%% other types: reserved
handle_ssh_msg({ssh_cm, _ConnectionManager, 
		{data, _ChannelId, 0, Data}}, 
	       State) ->
    State1 = handle_data(Data, State),
    {ok, State1};

handle_ssh_msg({ssh_cm, _, {eof, ChannelId}}, State) ->
    {stop, ChannelId, State};

handle_ssh_msg({ssh_cm, _, {signal, _, _}}, State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    {ok, State};

handle_ssh_msg({ssh_cm, _, {exit_signal, ChannelId, _, Error, _}}, State) ->
    Report = io_lib:format("Connection closed by peer ~n Error ~p~n",
                           [Error]),
    error_logger:error_report(Report),
    {stop, ChannelId, State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, 0}}, State) ->
    {stop, ChannelId, State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, Status}}, State) ->
    Report = io_lib:format("Connection closed by peer ~n Status ~p~n",
                           [Status]),
    error_logger:error_report(Report),
    {stop, ChannelId, State}.

%%--------------------------------------------------------------------
%% Function: handle_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%                        
%% Description: Handles other channel messages.
%%--------------------------------------------------------------------
handle_msg({ssh_channel_up, ChannelId, ConnectionManager}, State) ->
    {ok,  State#state{
	    channel = ChannelId,
	    cm = ConnectionManager}}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: Called when the channel process is trminated
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% handling received data
%% see ssh_sftpd.erl

handle_data(<<?UINT32(Len), Msg:Len/binary, Rest/binary>>,
	    #state{cm = ConnectionManager, 
		   channel = ChannelId,
		   pending = <<>>} = State) ->
    {Reply, Status} = exec_bin(Msg),
    Bin = term_to_binary({answer,{Reply, Status}}),
    Len = size(Bin),
    ssh_connection:send(ConnectionManager, ChannelId, 
			<<?UINT32(Len), Bin/binary>>),
    case Rest of
        <<>> -> State;
	_ -> handle_data(Rest, State)
    end;

handle_data(Data, State = #state{pending = <<>>}) ->
    State#state{pending = Data};

handle_data(Data, State = #state{pending = Pending}) ->
     handle_data(<<Pending/binary, Data/binary>>,
                 State#state{pending = <<>>}).

%% parsing a tuple and apply it

exec_bin(Cmdbin) -> 
    case binary_to_term(Cmdbin) of
	{mfa, {M, F, A}} -> 
	    case catch apply(M, F, A) of
		{'EXIT', _} = Exit ->
		    {{exec_badrpc, Exit}, -1};
		Reply ->
		    {Reply, 0}
	    end;
	Error -> 
	    {{exec_error,Error}, -1}
    end.

%% end of file

