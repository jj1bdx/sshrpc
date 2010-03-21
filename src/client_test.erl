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

-module(client_test).
-export([startup/0, linkup/0, test/2, test/0]).

-include("sshrpc.hrl").

-define(TIMEOUT, 30000). % in milliseconds

%% specifyclient configuration key directory
%% (put id_rsa and id_rsa.pub)

-define(CLIENT_CONFIG, "/your/client_config").

startup() ->
    ok = crypto:start(),
    ok = ssh:start().

linkup() ->
    {ok, Pid, Cm} = sshrpc_client:start_channel("127.0.0.1", % server address
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
    io:format("Pid: ~p Cm: ~p ~n", [Pid, Cm]),
    {Pid, Cm}.

test(M,N) ->
    {Pid, _Cm} = linkup(),
    Status = lists:map(
	       fun(X) ->
		       Reply = sshrpc_client:sync_call(Pid, lists, seq, [1, M]),
		       io:format("NR: ~p Time: ~p~n", [X, erlang:now()]),
		       io:format("Reply: ~p~n", [Reply])
	       end,
	       lists:seq(1,N)),
    io:format("Time: ~p Status: ~p~n", [erlang:now(),Status]),
    sshrpc_client:stop_channel(Pid).

test() ->
    startup(),
    linkup(),
    test(1,1).

%% end of file
