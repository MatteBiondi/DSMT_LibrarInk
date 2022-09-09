%%%-------------------------------------------------------------------
%%% @doc
%%% Module that provide to the client an easy interface to handle
%%% data in MnesiaDB. The function that client use will be translated
%%% in a gen_server call to proxy
%%% @end
%%% Created : 05. set 2022 16:01
%%%-------------------------------------------------------------------
-module(librarink_mnesiaDB_client).

-define(TIMEOUT, 3000). % Milliseconds to wait for a reply

%% API
-export([write_copy/2, write_loan/2, write_reservation/2, delete_copy/2, delete_loan/2, delete_reservation/2,
  archive_loans/1, archive_reservations/1, read_copies/2, read_loans/2, read_ended_loans/1, read_reservations/2,
  read_ended_reservations/1, update_loan/2, update_reservation/2]).

%----------------WRITE OPERATIONS----------------
write_copy(To, Args) ->
  send_request(To, {write_copy, Args}).

write_loan(To, Args) ->
  send_request(To, {write_loan, Args}).

write_reservation(To, Args) ->
  send_request(To, {write_reservation, Args}).

%----------------DELETE OPERATIONS----------------
delete_copy(To, Args) ->
  send_request(To, {delete_copy, Args}).

delete_loan(To, Args) ->
  send_request(To, {delete_loan, Args}).

delete_reservation(To, Args) ->
  send_request(To, {delete_reservation, Args}).

%----------------ARCHIVE OPERATIONS----------------
archive_loans(To) ->
  send_request(To, {archive_loans, #{}}).

archive_reservations(To) ->
  send_request(To, {archive_reservations, #{}}).

%----------------READ OPERATIONS----------------
read_copies(To, Args) ->
  send_request(To, {read_copies, Args}).

read_loans(To, Args) ->
  send_request(To, {read_loans, Args}).

read_ended_loans(To) ->
  send_request(To, {read_ended_loans, #{}}).

read_reservations(To, Args) ->
  send_request(To, {read_reservations, Args}).

read_ended_reservations(To) ->
  send_request(To, {read_ended_reservations, #{}}).

%----------------UPDATE OPERATIONS----------------
update_loan(To, Args) ->
  send_request(To, {update_loan, Args}).

update_reservation(To, Args) ->
  send_request(To, {update_reservation, Args}).

%----------------PRIVATE OPERATIONS----------------
send_request(To, Request) ->
  try
    gen_server:call(To, Request, ?TIMEOUT)
  catch
    throw:E ->  {caught, thrown, E};
    exit:E ->   {caught, exited, E};
    error:E ->  {caught, error, E}
  end.