sshrpc readme.txt
by Kenji Rikitake
kenji.rikitake@acm.org
25-FEB-2010

This is sshrpc, an experimental module for Erlang RPC over SSH.

* ssh module configuration requirement

Option {nodelay, true} MUST be set on both sides of
ssh:connect and ssh:daemon, for ensuring immediate
answers during the exchange.

* subsystem

sshrpc is implemented as an SSH subsystem, named as
       sshrpc@k2r.org
Note: this name is local and temporary

subsystem code: sshrpc_subsystem.erl

* client code

client code is still incomplete and under development.
client_subsystest.erl is an ad-hoc implementation;
sshrpc_client.erl will be the ssh_channel based implementation.

* NOTYET: sshrpc_client module functions

sync_call(Pid, Module, Function, Arguments) -> result | {badrpc, Reason}
    synchronous function evaluation on the remote node
    as connected through sshrpc subsystem via process Pid.

* SSH channel usage

sshrpc uses SSH Channel Type 0 for RPC exchange.
Usage for other Types are reserved.
Out-of-band signaling is not implemented.

* RPC exchange format

Larger binary packet for each exchange:
       << Length:32/unsigned-big-integer, 
          %% 4-byte binary (?UINT32())
          Content/binary 
	  %% of Erlang External Format of Length bytes
	  >>

Content for each exchange contains tuples only.

Command tuples:

{mfa, {Module, Function, Args}}
    execute erlang:apply(Module, Function, Args) at the server node.

{answer, {Reply, Status}}
    result of the {mfa, {...}} command tuple.

[End of memorandum]
