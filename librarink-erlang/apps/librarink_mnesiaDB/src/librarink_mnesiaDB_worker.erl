%%%-------------------------------------------------------------------
%%% @doc
%%% Module that handles requests received from the proxy, directly
%%% performing operations on the MnesiaDB instance by invoking the
%%% functions defined in librarink_mnesiaDB_fun
%%% @end
%%% Created : 07. set 2022 18:51
%%%-------------------------------------------------------------------
-module(librarink_mnesiaDB_worker).

%% API
-export([handle_request/3]).

%% @doc This functions handle all the possible requests that a client can ask in order to
%% interact with permanent data stored in Mnesia DB. If there is no error in the request,
%% the worker execute the appropriate function wrote in librarink_mnesiaDB_fun. At the end,
%% worker reply to the client sending the obtained result.
-spec(handle_request(Function :: atom(), Args::map(), From::tuple()) -> ok).

handle_request(write_copy, #{isbn := Isbn, id := Id}, From)->
  Result = perform_operation(add_book_copy,[Isbn, Id]),
  gen_server:reply(From, Result);

handle_request(write_loan, #{user := User, isbn := Isbn, id := Id}, From)->
  Result = perform_operation(from_reservation_to_loan,[User,Isbn,Id]),
  gen_server:reply(From, Result);

handle_request(write_reservation, #{user := User, isbn := Isbn}, From)->
  Result = perform_operation(add_book_reservation,[User,Isbn]),
  gen_server:reply(From, Result);

%------------------------------------------------------
handle_request(delete_copy, Args,From)->
  Result =
    case Args of
      #{isbn := Isbn, id := Id} ->
        perform_operation(delete_book_copy,[Isbn, Id]);
      #{isbn := Isbn} ->
        perform_operation(delete_all_book_copies,[Isbn]);
      _ ->
        {error, invalid_arguments}
    end,
  gen_server:reply(From, Result);

handle_request(delete_loan, Args,From)->
  Result =
    case Args of
      #{user := User, isbn := Isbn, id := Id} ->
        perform_operation(delete_lent_book,[User, Isbn, Id]);
      #{isbn := Isbn} ->
        perform_operation(delete_lent_by_book,[Isbn]);
      #{user := User} ->
        perform_operation(delete_lent_book_by_user,[User]);
      _ ->
        {error, invalid_arguments}
    end,
  gen_server:reply(From, Result);

handle_request(delete_reservation, Args,From)->
  Result =
    case Args of
      #{user := User, isbn := Isbn} ->
        perform_operation(delete_book_reservation,[User, Isbn]);
      #{user := User} ->
        perform_operation(delete_books_reservation_by_user,[User]);
      #{isbn := Isbn} ->
        perform_operation(delete_book_reservations_by_book,[Isbn]);
      _ ->
        {error, invalid_arguments}
    end,
  gen_server:reply(From, Result);

%------------------------------------------------------
handle_request(archive_loans, #{},From)->
  Result = perform_operation(get_and_delete_ended_loans,[]),
  gen_server:reply(From, Result);

handle_request(archive_reservations, #{},From)->
  Result = perform_operation(get_and_delete_ended_reservations,[]),
  gen_server:reply(From, Result);

%------------------------------------------------------
handle_request(read_copies, Args,From)->
  Result =
    case Args of
      #{type := all, isbn := Isbn} ->
        perform_operation(copies_by_book,[Isbn]);
      #{type := all} ->
        perform_operation(all_copies_all_book,[]);
      #{type := available, operation := count, isbn := Isbn} ->
        perform_operation(count_available_copies_by_book,[Isbn]);
      #{type := available, operation := list, isbn := Isbn} ->
        perform_operation(list_available_copies_by_book,[Isbn]);
      _ ->
        {error, invalid_arguments}
    end,
  gen_server:reply(From, Result);

handle_request(read_loans, Args,From)->
  Result =
    case Args of
      #{isbn := Isbn} ->
        perform_operation(lent_copies_by_book,[Isbn]);
      #{user := User} ->
        perform_operation(loans_by_user,[User]);
      #{isbn := Isbn, id := Id} ->
        perform_operation(loan_by_book_copy,[Isbn, Id]);
      #{} ->
        perform_operation(all_pending_loans,[]);
      _ ->
        {error, invalid_arguments}
    end,
  gen_server:reply(From, Result);

handle_request(read_ended_loans, #{},From)->
  Result = perform_operation(all_ended_loans,[]),
  gen_server:reply(From, Result);

handle_request(read_reservations, Args,From)->
  Result =
    case Args of
      #{user := User, isbn := Isbn} ->
        perform_operation(reservations_by_user_and_book,[User, Isbn]);
      #{user := User} ->
        perform_operation(reservations_by_user,[User]);
      #{isbn := Isbn} ->
        perform_operation(reservations_by_book,[Isbn]);
      #{} ->
        perform_operation(all_pending_reservations,[]);
      _ ->
        {error, invalid_arguments}
    end,
  gen_server:reply(From, Result);

handle_request(read_ended_reservations, #{},From)->
  Result = perform_operation(all_ended_reservations,[]),
  gen_server:reply(From, Result);

%------------------------------------------------------
handle_request(update_loan, #{type := terminate, isbn := Isbn, id := Id}, From)->
  Result = perform_operation(terminate_loan_by_book,[Isbn, Id]),
  gen_server:reply(From, Result);

handle_request(update_loan, #{type := renew, isbn := Isbn, id := Id}, From)->
  Result = perform_operation(renew_loan_by_book_copy,[Isbn, Id]),
  gen_server:reply(From, Result);

handle_request(update_reservation, #{type := cancel, user := User, isbn := Isbn}, From)->
  Result = perform_operation(cancel_reservation_by_book_and_user,[User, Isbn]),
  gen_server:reply(From, Result);

%------------------------------------------------------
handle_request(_, _, From)->
  gen_server:reply(From, {error, invalid_request}).

%------------------PRIVATE OPERATION-------------------
%% @doc This functions is used to call the mnesiaDB function implemented in
%% librarink_mnesiaDB_fun module. This function allows to reduce redundant
%% try...catch code. It takes function and arguments as parameters.
-spec(perform_operation(Fun :: atom(), Args::map()) ->
  {succeed, ok} |
  {succeed, Record::map()} |
  {succeed,{Records_counter :: pos_integer(), Records_list :: list(map())}} |
  {succeed, Copies_counter::pos_integer()} |
  {succeed, Records_list::list(map())} |
  {error, error_not_available_copy | error_zero_copies_available| undefined_book_copy | error_no_loan_found |
    error_no_reservation_found | error_cancellation_failed | reservation_not_found | unexpected_error |
    unavailable_copies_to_reserve | error_insert_failed | book_already_reserved | error_pending_loan |
    error_all_copies_reserved_or_lent | error_pending_loan_or_reservation | error_pending_reservation | undefined_book |
    E::term()}).
perform_operation(Fun, Args)->
  try
    apply(librarink_mnesiaDB_fun,Fun,Args)
  catch
    _:E ->  {error, E}
  end.
