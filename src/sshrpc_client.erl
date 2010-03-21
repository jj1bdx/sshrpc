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

%% @doc general sshrpc client-side subsystem (incomplete)
%% NOTE: this code does compile but WILL NOT WORK AS IS.

%% see ssh_sftp.erl

-module(sshrpc_client).

-behaviour(ssh_channel).

-include("sshrpc.hrl").

%% own functions
-export([sync_call/4, call/3]).

%% ssh_channel callbacks
-export([init/1, handle_ssh_msg/2, handle_msg/2, handle_call/3]).
-export([terminate/2, code_change/3]).
-export([start_channel/1, start_channel/2, start_channel/3, stop_channel/1]).

%% state
-record(state, {
	  cm,
	  channel,
	  from,
	  req_id,
	  reply_buf    % binary()
	 }).

-define(SSHRPC_PACKET_SIZE, 32768).
-define(SSHRPC_WINDOW_SIZE, 4*?SSHRPC_PACKET_SIZE).

%%====================================================================
%% API
%%====================================================================

start_channel(Cm) when is_pid(Cm) ->
    start_channel(Cm, []);
start_channel(Host) when is_list(Host) ->
    start_channel(Host, []).					 
start_channel(Cm, Opts) when is_pid(Cm) ->
    Timeout = proplists:get_value(timeout, Opts, infinity),
    case open_subsystem(Cm, []) of
	{ok, ChannelId, Cm} -> 
	    case ssh_channel:start(Cm, ChannelId, ?MODULE, [Cm, 
							    ChannelId, Timeout]) of
		{ok, Pid} ->
		    {ok, Pid, Cm};
		{error, Reason} ->
		    {error, Reason};
		ignore ->
		    {error, ignore}
	    end;
	Error ->
	    Error
    end;

start_channel(Host, Opts) ->
    start_channel(Host, 22, Opts).
start_channel(Host, Port, Opts) ->
    Timeout = proplists:get_value(timeout, Opts, infinity),
    case connect(Host, Port, proplists:delete(timeout, Opts)) of
	{ok, ChannelId, Cm} ->
	    case ssh_channel:start(Cm, ChannelId, ?MODULE, [Cm, 
							    ChannelId, Timeout]) of
		{ok, Pid} ->
		    {ok, Pid, Cm};
		{error, Reason} ->
		    {error, Reason};
		ignore ->
		    {error, ignore}
	    end;
	Error ->
	    Error	    
    end.

stop_channel(Pid) ->
    case process_info(Pid, [trap_exit]) of
	[{trap_exit, Bool}] ->
	    process_flag(trap_exit, true),
	    link(Pid),
	    exit(Pid, sshrpc_stop_channel),
	    receive 
		{'EXIT', Pid, normal} ->
		    ok
	    after 5000 ->
		    exit(Pid, kill),
		    receive 
			{'EXIT', Pid, killed} ->
			    ok
		    end
	    end,
	    process_flag(trap_exit, Bool),
	    ok;
	undefined ->
	    ok
    end.

%% remote eval function

sync_call(Pid, M, F, A) ->
    call(Pid, {mfa, {M, F, A}}, infinity).

%%====================================================================
%% SSh channel callbacks 
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} 
%%                        
%% Description: 
%%--------------------------------------------------------------------

init([Cm, ChannelId, Timeout]) ->
    erlang:monitor(process, Cm),
    case ssh_connection:subsystem(Cm, ChannelId,
				  "sshrpc@k2r.org", 
				  Timeout) of
	success ->
	    {ok, #state{cm = Cm,
			channel = ChannelId,
			req_id = 0, 
			reply_buf = <<>>}};
	failure ->
	    {stop, {error, "server failed to start sftp subsystem"}};
	Error ->
	    {stop, Error}
    end.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_call({{timeout, infinity}, Msg}, From, State) ->
    do_handle_call(Msg, From, State);

handle_call({{timeout, Timeout}, Msg}, From,  #state{req_id = Id} = State) ->
    timer:send_after(Timeout, {timeout, Id, From}),
    do_handle_call(Msg, From, State).

%% actual processing function
%% how should I handle the "From" argument?
do_handle_call({mfa, {M, F, A}}, From, #state{
				   cm = ConnectionManager,
				   channel = ChannelId} = State) ->
    send_command(ConnectionManager, ChannelId, {mfa, {M, F, A}}),
    %% io:format("do_handle_called with mfa called From = ~p~n", [From]),
    {noreply, State#state{from = From}}.

%%--------------------------------------------------------------------
%% Function: handle_ssh_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%                        
%% Description: Handles channel messages
%%--------------------------------------------------------------------

handle_ssh_msg({ssh_cm, _ConnectionManager, 
		{data, _ChannelId, 0, Data}}, #state{reply_buf = Data0} = 
	       State0) ->
    State = handle_reply(State0, <<Data0/binary,Data/binary>>),
    {ok, State};

handle_ssh_msg({ssh_cm, _ConnectionManager, {eof, _ChannelId}}, State) ->
    {ok, State};

handle_ssh_msg({ssh_cm, _, {signal, _, _}}, State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    {ok, State};

handle_ssh_msg({ssh_cm, _, {exit_signal, ChannelId, _, _, _}}, 
	       State) ->
    {stop, ChannelId,  State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, _}}, State) ->
    {stop, ChannelId, State}.

%%--------------------------------------------------------------------
%% Function: handle_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%                        
%% Description: Handles channel messages
%%--------------------------------------------------------------------

handle_msg({ssh_channel_up, _, _}, State) ->
    {ok, State};

%% Connection manager goes down
handle_msg({'DOWN', _Ref, _Type, _Process, _},  
	   #state{channel = ChannelId} = State) ->
    {stop, ChannelId, State};
 
%% Stopped by user
handle_msg({'EXIT', _, sshrpc_stop_channel}, 
	   #state{channel = ChannelId} = State) ->
    {stop, ChannelId, State};

handle_msg(_, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: Called when the channel process is terminated
%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% see ssh_channel manual
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% ssh_xfer:connect
connect(Host, Port, Opts) ->
    case ssh:connect(Host, Port, Opts) of
	{ok, CM} ->
	    open_subsystem(CM, Opts);
	Error -> Error
    end.

% internal
% see ssh_xfer:open_xfer/2

open_subsystem(CM, Opts) ->
    Timeout = proplists:get_value(timeout, Opts, infinity),
    case ssh_connection:session_channel(CM, 
					?SSHRPC_WINDOW_SIZE,
					?SSHRPC_PACKET_SIZE,
					Timeout) of
        {ok, ChannelId} ->
            {ok, ChannelId, CM};
        Error ->
            Error
    end.

%% general form of call: 
%% call(Pid, Operation, Timeout)
%% operation part: use tuple for multiple arguments
%% passing messages onto ssh_channel:call/3 has infinity timeout

call(Pid, Msg, TimeOut) ->
    ssh_channel:call(Pid, {{timeout, TimeOut}, Msg}, infinity).
	    
send_command(Pid, ChannelId, Cmd) ->
    Bin = term_to_binary(Cmd),
    Len = size(Bin),
    %% R13B04 ssh_connection:send/4 has a bug:
    %% if the send(ConnectionRef, ChannelId, Data, Timeout)'s
    %% Timeout = 'infinity' (an atom), this is interpreted as a Data in
    %% send(ConnectionRef, ChannelId, Type, Data); 
    %% the atom 'infinity' removed for disambiguation and a workaround
    ssh_connection:send(Pid, ChannelId, 
			<<?UINT32(Len), Bin/binary>>).

handle_reply(State, <<?UINT32(Len),Reply:Len/binary,Rest/binary>>) ->
    do_handle_reply(State, Reply, Rest);
handle_reply(State, Data) -> 
    State#state{reply_buf = Data}.

do_handle_reply(#state{from = From} = State, Reply, Rest) ->
    Msg = binary_to_term(Reply),
    %% io:format("ssh_channel:reply called with [From, Msg] = [~p, ~p]~n", [From, Msg]),
    ssh_channel:reply(From, Msg),
    handle_reply(State, Rest).

%% end of file


