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

%% @doc example code for testing SSH RPC client site
%% TODO: this code must be OTP-nized!

-module(client_subsystest).
-export([startup/0, linkup/0, doit/2, test/2]).

-include("sshrpc.hrl").

-define(TIMEOUT, 30000). % in milliseconds

%% specifyclient configuration key directory
%% (put id_rsa and id_rsa.pub)

-define(CLIENT_CONFIG, "/your/client/directory").

startup() ->
    ok = crypto:start(),
    ok = ssh:start().

linkup() ->
    {ok, Pid} = ssh:connect("127.0.0.1", % server address
		     11122, % port number
		     [
		      %% note: private user key (id_rsa) and
		      %% public host key (known_hosts) must exist
		      %% in the "user_dir" for public-key auth
		      %% NOTE WELL: user key has NULL password protected
		      %%            (password-protected is UNSUPPORTED)
		      {user_dir, ?CLIENT_CONFIG},
		      %% the following user/password pair needed for
		      %% plain password-based authentication
		      %% {user,"test"}
		      %% {password,"shiken"}
		      %%
		      %% nodelay must be set true for immediate response!
		      {nodelay, true}
		     ]),
    {ok, Chanid} = ssh_connection:session_channel(Pid, infinity),
    io:format("Pid: ~p Chanid: ~p ~n", [Pid, Chanid]),
    Status = ssh_connection:subsystem(Pid, Chanid, "sshrpc@k2r.org", ?TIMEOUT),
    io:format("subsystem connection status: ~p ~n", [Status]),
    {Pid, Chanid}.

doit({Pid, Chanid}, Command) ->
    Bin = term_to_binary(Command),
    Len = size(Bin),
    ssh_connection:send(Pid, Chanid, 0, <<?UINT32(Len), Bin/binary>>, ?TIMEOUT),
    ssh_receive(Pid, Chanid).

ssh_receive(CM, Channel) ->
    ssh_loop(CM, Channel, <<>>).

-define(RWINSIZE, 4096).

%% SSH client loop: this hasn't been OTP-nized yet.

ssh_loop(CM, Channel, Buf) ->
    receive
	stop ->
            % Closing channel
	    ssh_connection:close(CM, Channel);

	{ssh_cm, CM, {data, Channel, 0, Data}} ->
	    % looks like this is a must for flow control...
	    ssh_connection:adjust_window(CM, Channel, size(Data)),
	    State = handle_reply(<<Buf/binary, Data/binary>>),
	    case State of
		{moredata, _, Data1} ->
		    ssh_loop(CM, Channel, Data1);
		{success, Term, Data2} ->
		    case Term of
			{answer, {Reply, Status}} ->
			    io:format("Answer: Reply: ~p Status: ~p~n", 
				      [Reply, Status]),
			    case Data2 of
				<<>> -> ok;
				Other -> ssh_loop(CM, Channel, Other)
			    end;
			Other ->
			    io:format("[~p] Unknown Term returned: ~p~n", [?MODULE, Other])
		    end;
		Other ->
			    io:format("[~p] Unknown State returned: ~p~n", [?MODULE, Other])
	    end;

	{ssh_cm, CM, {closed, Channel}} ->
	    io:format("detached: ~p~n", [Channel]),
            % Closing channel
	    io:format("close channel: ~p~n", [Channel]),
	    ssh_connection:close(CM, Channel);

	{ssh_cm, CM, {eof, Channel}} ->
	    io:format("eof: ~p~n", [Channel]);

	E ->
	    io:format("[~p] Received: ~p~n", [?MODULE, E]),
	    ssh_loop(CM, Channel, Buf) 
    after 1000 ->
	    io:format("[~p] Timed Out 1sec~n", [?MODULE])
    end. 

handle_reply(<<?UINT32(Len), Msg:Len/binary, Rest/binary>>) ->
    %io:format("handle_reply: Len: ~p Msg: ~p Rest: ~p~n", [Len, Msg, Rest]),
    {success, binary_to_term(Msg), Rest};
handle_reply(Data) ->
    %io:format("handle_reply: Data: ~p~n", [Data]),
    {moredata, {}, Data}.

test(M,N) ->
    L = linkup(),
    Status = lists:map(fun(X) ->
			       doit(L, {mfa, {lists, seq, [1, M]}}),
			       io:format("NR: ~p Time: ~p~n", [X, erlang:now()]) end,
		       lists:seq(1,N)),
    io:format("Time: ~p Status: ~p~n", [erlang:now(),Status]),
    {Pid, Chanid} = L,
    ssh_connection:close(Pid, Chanid),
    ssh_loop(Pid, Chanid, <<>>).

%% end of file
