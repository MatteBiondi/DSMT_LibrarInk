%%%-------------------------------------------------------------------
%%% @author Federico
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. ago 2022 00:21
%%%-------------------------------------------------------------------
-author("Federico").
-record(librarink_mqs_state, {status=disconnected, master_pid, connection = none, channel= none, queues = [],
  consumer = none, consumer_tags = [], callback = none}).

-define(
  MQS_STATE(MasterPid, Connection, Channel),
  #librarink_mqs_state{status = connected, master_pid = MasterPid, connection = Connection, channel = Channel}
).
