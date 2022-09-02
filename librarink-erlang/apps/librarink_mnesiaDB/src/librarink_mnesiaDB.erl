-module(librarink_mnesiaDB).

-export([install/2]).

-include_lib("stdlib/include/ms_transform.hrl").
-import(calendar, [now_to_universal_time/1]).

%% Definition of db record
-record(librarink_lent_book, {user, isbn, physical_copy_id, start_date, stop_date}).
-record(librarink_reserved_book, {user, isbn, start_date, stop_date, canceled}).
-record(librarink_physical_book_copy, {isbn, physical_copy_id}).

%_________________________________________________________________________________
%todo controlla di avere operazioni per eliminare recor da passare a history
%todo metti le catch all clause
%todo Check codice ridondante
%todo prima di rimuovere loan o reserv devo controllare che siano cancellate o finite?
%todo metti i tipi agli spec
%todo metti a tutti gli spec e la desc
%todo metti metodi in export
%todo in loan e reservation vedi cosa fare pending e cosa accettare che sia finito
%% select   -> It works using match specifications or ets:fun2ms as a way to do queries
%% vs match -> It uses patterns such as those described in Meeting Your Match to return entire records from the database table
%% vs read  -> function will return a list of records with their primary key matching Key.
%% vs index_read

%% todo valuta se servono macro
% todo: rivedi le spec e le descrizioni sopra. Chi davvero da una eccezione? vedi da manuale

%_________________________________________________________________________________

%%%%%%%%%%%%%%%%%%%%%%
%%% INITIALIZATION
%%%%%%%%%%%%%%%%%%%%%%
%TODO desc + spec
install(ActiveNodes, BackupNodes) ->
  Nodes = ActiveNodes ++ BackupNodes,
  mnesia:create_schema(Nodes),
  rpc:multicall(Nodes, application, start, [mnesia]),
  DB_Directory = application:get_env(db_dir),
  application:set_env(mnesia, dir, DB_Directory),
  case mnesia:wait_for_tables([ librarink_lent_book,
                                librarink_reserved_book,
                                librarink_physical_book_copy], 5000) =:= ok of
    true ->
      ok;
    false ->
      mnesia:create_table(librarink_lent_book,
        [{attributes, record_info(fields, librarink_lent_book)},
          {index, [#librarink_lent_book.isbn, #librarink_lent_book.physical_copy_id]},
          {disc_copies, ActiveNodes},
          {disc_only_copies, BackupNodes},
          {type, bag}]),
      mnesia:create_table(librarink_reserved_book,
        [{attributes, record_info(fields, librarink_reserved_book)},
          {index, [#librarink_reserved_book.isbn]},
          {disc_copies, ActiveNodes},
          {disc_only_copies, BackupNodes},
          {type, bag}]),
      mnesia:create_table(librarink_physical_book_copy,
        [{attributes, record_info(fields, librarink_physical_book_copy)},
          {index, [#librarink_physical_book_copy.physical_copy_id]},
          {disc_copies, ActiveNodes},
          {disc_only_copies, BackupNodes},
          {type, bag}])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% START AND STOP MNESIA
%%%%%%%%%%%%%%%%%%%%%%%%%
%TODO desc + spec
start_librarink_mnesia(Nodes) ->
  rpc:multicall(Nodes, application, start, [mnesia]),
  ok.

end_librarink_mnesia(Nodes) ->
  rpc:multicall(Nodes, application, stop, [mnesia]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%
%%% INSERT OPERATIONS
%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc
%%  This function is called to add a physical book copy
%%  Type: - Insert operation
%%  In:   - book's isbn
%%        - book's copy id
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(add_book_copy ( Isbn, Physical_copy_id) -> {ok} | Exception).
add_book_copy(Isbn, Physical_copy_id) ->
  F = fun() ->
        mnesia:write(#librarink_physical_book_copy{ isbn = Isbn,
                                                    physical_copy_id = Physical_copy_id})
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to add a lent book
%%  Type: - Remove and Insert operation
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(from_reservation_to_loan ( User, Isbn ) -> {ok} | Exception).
from_reservation_to_loan(User, Isbn) ->
  F = fun() ->
        Timestamp =now_to_universal_time(timestamp()),
        %Find an available copy
        case list_available_copies_by_book(Isbn) of
          {_, []} ->
            %Strange situation, there should be at least an available (reserved) copy (the one for this user)
            error_not_available_copy;
          {_, [H | _]} ->
            {_, Selected_id} = H,
            %Check that there was a reservation for that user and book
            case reservations_by_user_and_book(User, Isbn) of
              {_,[Res | _]} ->
                %transforms the reservation into a loan
                %Put a stop_date to reservation
                Old_res = insert_element(1,Res,librarink_reserved_book),
                mnesia:write(Old_res#librarink_reserved_book{stop_date = Timestamp, canceled = false}),
                mnesia:delete_object(Old_res),
                %Insert a loan row
                New_loan = {librarink_lent_book, User, Isbn, Selected_id, Timestamp, null},
                mnesia:write(New_loan);
              _ ->
                unexpected_error
            end;
          _ ->
            unexpected_error
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to reserve a book
%%  Type: - Insert operation
%%  In:   - User, Isbn, Start_date, Stop_date
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(add_book_reservation ( User, Isbn) -> {ok} | Exception).
add_book_reservation(User, Isbn) ->
  F = fun() ->
        {_, Counter} =count_available_copies_by_book(Isbn),
        if
          Counter == 0 ->
            unavailable_copies_to_reserve;
          Counter > 0->
            mnesia:write(#librarink_reserved_book{
              user = User,
              isbn = Isbn,
              start_date = now_to_universal_time(timestamp()),
              stop_date = null,
              canceled = false});
          true ->
            error_insert_failed
        end
      end,
  mnesia:activity(transaction, F).



%%%%%%%%%%%%%%%%%%%%%%
%%% DELETE OPERATIONS
%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc
%%  This function is called to remove a specified physical book copy
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(delete_book_copy ( Isbn, Physical_copy_id) -> {ok} | Exception).
delete_book_copy(Isbn, Physical_copy_id) ->
  F = fun() ->
        %todo check che numero copie sia > numero di reservation sul libro
        %check there are no pending loan for that copy
        case loan_by_book_copy(Isbn, Physical_copy_id) of
          [] ->
            To_delete = #librarink_physical_book_copy{isbn = Isbn, physical_copy_id = Physical_copy_id},
            List = mnesia:match_object(To_delete),
            lists:foreach(
              fun(Row) ->
                mnesia:delete_object(Row)
              end,List
            );
          undefined_book_copy ->
            undefined_book_copy;
          _ ->
            error_pending_loan
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to remove a all physical copies of a
%%  specified book
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(delete_all_book_copies ( Isbn ) -> {ok} | Exception).
delete_all_book_copies(Isbn) ->
  F = fun() ->
        %check there are no pending loan or reservation for that book
        {_, Available_counter} = count_available_copies_by_book(Isbn),
        {All_copies_counter, _} = copies_by_book(Isbn),
        if
          All_copies_counter == Available_counter ->
            mnesia:delete({librarink_physical_book_copy, Isbn});
          true ->
            error_pending_loan_or_reservation
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to remove a specified book loan
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(delete_lent_book (User, Isbn, Id) -> {ok} | Exception).
delete_lent_book(User, Isbn, Id) ->
  F = fun() ->
        To_delete = #librarink_lent_book{user = User, isbn = Isbn, physical_copy_id = Id},
        List = mnesia:match_object(To_delete),
        lists:foreach(
          fun(Row) ->
            if
              Row#librarink_lent_book.stop_date =/= null ->
                mnesia:delete_object(Row);
              true ->
                error_pending_loan
            end
          end, List
        )
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to remove all loan for a specified book
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(delete_lent_by_book (Isbn) -> {ok} | Exception).
delete_lent_by_book(Isbn) ->
  delete_lent_book('_', Isbn, '_').


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to remove all book loans for a specified
%%  user
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(delete_lent_book_by_user (User) -> {ok} | Exception).
delete_lent_book_by_user(User) ->
  delete_lent_book(User, '_', '_').


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to remove a specified book reservation
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(delete_book_reservation (User, Isbn) -> {ok} | Exception).
delete_book_reservation(User, Isbn) ->
  F = fun() ->
        To_delete = #librarink_reserved_book{user = User, isbn = Isbn},
        List = mnesia:match_object(To_delete),
        lists:foreach(
          fun(Row) ->
            if
              Row#librarink_reserved_book.stop_date =/= null ->
                mnesia:delete_object(Row);
              true ->
                error_pending_reservation
            end
          end, List
        )
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to remove all book reservation for a
%%  specified user
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(delete_books_reservation_by_user (User) -> {ok} | Exception).
delete_books_reservation_by_user(User) ->
  delete_book_reservation(User, '_').


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to remove all book reservation for a
%%  specified book
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(delete_book_reservations_by_book (Isbn) -> {ok} | Exception).
delete_book_reservations_by_book(Isbn) ->
  delete_book_reservation('_', Isbn).


%%%%%%%%%%%%%%%%%%%%%%
%%% READ OPERATIONS
%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc
%%  This function is called to count and get all physical copies of a
%%  specified book
%%  Type: - Read operation
%%  In:   - Selected book's ISBN
%%  Out:  - {Counter, [tuples]} -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(copies_by_book (Isbn) -> {Counter, [tuple()]} | undefined | Exception).
copies_by_book(Isbn) ->
  F = fun() ->
    [{Isbn, Id} ||
      #librarink_physical_book_copy{physical_copy_id = Id} <- mnesia:read(librarink_physical_book_copy, Isbn)]
      end,
  Result_list = mnesia:activity(transaction, F),
  {length(Result_list) , Result_list}.


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to count and get available physical copies
%%  of a specified book
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(count_available_copies_by_book (Isbn) -> {ok} | Exception).
count_available_copies_by_book(Isbn) ->
  F = fun() ->
        All_copies = copies_by_book(Isbn),
        case All_copies of
          {0, _} ->
            undefined_book;
          {Counter, _} when Counter > 0 ->
            {Reserved_copies,_} = reservations_by_book(Isbn),
            {Lent_copies_counter, _} = lent_copies_by_book(Isbn),
            Available_counter = Counter - Reserved_copies - Lent_copies_counter,
            {ok, Available_counter};
            %Available -> Not lent neither reserved
          _ ->
            unexpected_error
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to get available physical copies
%%  of a specified book
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(list_available_copies_by_book (Isbn) -> {ok} | Exception).
list_available_copies_by_book(Isbn) ->
  F = fun() ->
        All_copies = copies_by_book(Isbn),
        case All_copies of
          {0, _} ->
            undefined_book;
          {Counter, List_of_copies} when Counter > 0->
            {_,Lent_copies} = lent_copies_by_book(Isbn),
            Extracted_lent_copies = [{Isbn, Id} ||
              #librarink_lent_book{physical_copy_id = Id} <- Lent_copies],
            Not_lent_copies = List_of_copies -- Extracted_lent_copies,
            {ok, Not_lent_copies};
          _ ->
            unexpected_error
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to count and get lent physical copies of a
%%  specified book
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(lent_copies_by_book (Isbn) -> {ok} | Exception).
lent_copies_by_book(Isbn) ->
  F = fun() ->
        case copies_by_book(Isbn)  =:= {0, _} of
          true ->
            undefined_book;
          false ->
            Match = ets:fun2ms(
              fun(#librarink_lent_book{ user = Record_user,
                                        isbn=Record_isbn,
                                        physical_copy_id = Record_id,
                                        start_date = Record_start,
                                        stop_date = Record_stop})
                when Record_isbn =:= Isbn and Record_stop =:= null ->
                {Record_user, Record_isbn, Record_id, Record_start, Record_stop}
              end
            ),
            mnesia:select(librarink_lent_book, Match)
        end
      end,
  Result_list = mnesia:activity(transaction, F),
  {length(Result_list) , Result_list}.


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to get information about lent physical
%%  book copies of a specified user
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(loans_by_user (User) -> {ok} | Exception).
loans_by_user(User) ->
  F = fun() ->
        [{User, Isbn, Id, Start_date, Stop_date} ||
          #librarink_lent_book{isbn = Isbn, physical_copy_id = Id, start_date = Start_date, stop_date = Stop_date}
            <- mnesia:read({librarink_lent_book, User})]
      end,
  Result_list = mnesia:activity(transaction, F),
  {length(Result_list), Result_list}.


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to get information about pending loan
%%  for a specified book copy
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(loan_by_book_copy ( Isbn, Copy_Id ) -> {ok} | Exception).
loan_by_book_copy(Isbn, Copy_Id) ->
  F = fun() ->
        {_ , List_of_copies} = copies_by_book(Isbn),
        case lists:member({Isbn, Copy_Id}, List_of_copies) of
          true ->
            Match = ets:fun2ms(
              fun(#librarink_lent_book{ user = Record_user,
                                        isbn = Record_isbn,
                                        physical_copy_id = Record_id,
                                        start_date = Record_start,
                                        stop_date = Record_stop})
                when Record_isbn =:= Isbn and Record_id =:= Copy_Id and Record_stop =:= null ->
                  {Record_user, Record_isbn, Record_id, Record_start, Record_stop}
              end
            ),
            mnesia:select(librarink_lent_book, Match);
          false ->
            undefined_book_copy
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to count and get information about books
%%  reservations for a specified user
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(reservations_by_user (User) -> {ok} | Exception).
reservations_by_user(User) ->
  F = fun() ->
    [{User, Isbn, Start_date, Stop_date, Cancelled} ||
      #librarink_reserved_book{isbn = Isbn, start_date = Start_date, stop_date = Stop_date, canceled = Cancelled}
        <- mnesia:read(librarink_reserved_book, User)]
      end,
  Result_list = mnesia:activity(transaction, F),
  {length(Result_list), Result_list}.


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to get information about pending
%%  reservation for a specified book and user
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(reservations_by_user_and_book ( User, Isbn ) -> {ok} | Exception).
reservations_by_user_and_book(User,Isbn) ->
  F = fun() ->
    case copies_by_book(Isbn) =:= {0, _} of
      true ->
        undefined_book;
      false ->
        Match = ets:fun2ms(
          fun(#librarink_reserved_book{ user = Record_user,
                                        isbn=Record_isbn,
                                        start_date = Record_start,
                                        stop_date = Record_stop,
                                        canceled = Record_cancelled})
            when Record_isbn =:= Isbn and Record_user =:= User and Record_stop =:= null and Record_cancelled =:= false ->
            {Record_user, Record_isbn, Record_start, Record_stop, Record_cancelled}
          end
        ),
        mnesia:select(librarink_reserved_book, Match)
    end
      end,
  Result_list = mnesia:activity(transaction, F),
  {length(Result_list) , Result_list}.


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to count and get information about
%%  pending reservation for a specified book
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(reservations_by_book ( Isbn ) -> {ok} | Exception).
reservations_by_book(Isbn) ->
  reservations_by_user_and_book('_', Isbn).


%%%%%%%%%%%%%%%%%%%%%%
%%% UPDATE OPERATIONS
%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc
%%  This function is called to terminate a book loan for a specified
%%  book copy
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(terminate_loan_by_book ( Isbn, Copy_id ) -> {ok} | Exception).
terminate_loan_by_book( Isbn, Copy_id ) ->
  F = fun() ->
        case loan_by_book_copy(Isbn, Copy_id) of
          undefined_book_copy->
            undefined_book_copy;
          []->
            error_no_loan_found;
          [Row]->
            Stop_date = now_to_universal_time(timestamp()),
            Db_row = insert_element(1,Row,librarink_lent_book),
            mnesia:write(Db_row#librarink_lent_book{stop_date = Stop_date}),
            mnesia:delete_object(Db_row)
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc
%%  This function is called to cancel a book reservation for a
%%  specified book and user
%%  Type: -
%%  In:   -
%%  Out:  - ok -> No error
%%        - Exception -> In case of error
%% @end
%%--------------------------------------------------------------------
-spec(cancel_reservation_by_book_and_user ( Isbn, User ) -> {ok} | Exception).
cancel_reservation_by_book_and_user(Isbn, User) ->
  F = fun() ->
        %vedi che le info siano esistenti, se tale libro esiste
        case reservations_by_user_and_book(User, Isbn) of
          undefined_book->
            undefined_book;
          {0,[]}->
            error_no_reservation_found;
          {_,[Row]}->
            Stop_date = now_to_universal_time(timestamp()),
            Db_row = insert_element(1,Row,librarink_reserved_book),
            mnesia:write(Db_row#librarink_reserved_book{stop_date = Stop_date, canceled = true}),
            mnesia:delete_object(Db_row);
          _ ->
            error_cancellation_failed
        end
      end,
  mnesia:activity(transaction, F).
