-module(librarink_mnesiaDB).

-export([install/2, start_librarink_mnesia/1, stop_librarink_mnesia/1, add_book_copy/2, from_reservation_to_loan/2,
  add_book_reservation/2, delete_book_copy/2, delete_all_book_copies/1, delete_lent_book/3, delete_lent_by_book/1,
  delete_lent_book_by_user/1, delete_book_reservation/2, delete_books_reservation_by_user/1,
  delete_book_reservations_by_book/1, copies_by_book/1, count_available_copies_by_book/1,
  list_available_copies_by_book/1, lent_copies_by_book/1, loans_by_user/1, loan_by_book_copy/2, reservations_by_user/1,
  reservations_by_user_and_book/2, reservations_by_book/1, terminate_loan_by_book/2,
  cancel_reservation_by_book_and_user/2, get_and_delete_ended_reservations/0, get_and_delete_ended_loans/0,
  all_copies_all_book/0, all_pending_reservations/0, all_ended_reservations/0, all_pending_loans/0, all_ended_loans/0]).

-include_lib("stdlib/include/ms_transform.hrl").
-import(calendar, [now_to_universal_time/1]).
-import(erlang, [insert_element/3, timestamp/0]).

%% Definition of DB record
-record(librarink_lent_book, {user, isbn, physical_copy_id, start_date, stop_date}).
-record(librarink_reserved_book, {user, isbn, start_date, stop_date, canceled}).
-record(librarink_physical_book_copy, {isbn, physical_copy_id}).


%_________________________________________________________________________________

%%%%%%%%%%%%%%%%%%%%%%
%%% INITIALIZATION
%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to start Mnesia, create schema and create
%%  all the tables
%%  In:   - ActiveNodes: List of active nodes
%%        - BackupNodes: List of backup nodes
%%  Out:  - install_succeeded -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(install(ActiveNodes::list() , BackupNodes::list()) -> install_succeeded ).
install(ActiveNodes, BackupNodes) ->
  %Create schema for all nodes received as parameter
  Nodes = ActiveNodes ++ BackupNodes,
  mnesia:create_schema(Nodes),
  %Activate Mnesia in all nodes
  rpc:multicall(Nodes, application, start, [mnesia]),
  %Create tables if not exist
  case mnesia:wait_for_tables([ librarink_lent_book,
                                librarink_reserved_book,
                                librarink_physical_book_copy], 5000) =:= ok of
    true ->
      install_succeeded;
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
          {type, bag}]),
      install_succeeded
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% START AND STOP MNESIA
%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to start Mnesia
%%  In:   - Nodes: List of nodes
%%  Out:  - ok -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(start_librarink_mnesia(Nodes::list()) ->   ok ).
start_librarink_mnesia(Nodes) ->
  rpc:multicall(Nodes, application, start, [mnesia]),
  ok.


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to stop Mnesia
%%  In:   - Nodes: List of nodes
%%  Out:  - ok -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(stop_librarink_mnesia(Nodes::list()) ->   ok ).
stop_librarink_mnesia(Nodes) ->
  rpc:multicall(Nodes, application, stop, [mnesia]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%
%%% INSERT OPERATIONS
%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to add a physical book copy
%%  Type: - Insert operation
%%  In:   - Isbn: Book's ISBN
%%        - Physical_copy_id: Book's copy id
%%  Out:  - ok -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(add_book_copy ( Isbn::string(), Physical_copy_id::string()) -> ok).
add_book_copy(Isbn, Physical_copy_id) ->
  F = fun() ->
        mnesia:write(#librarink_physical_book_copy{ isbn = Isbn,
                                                    physical_copy_id = Physical_copy_id})
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to transform a book reservation in a book
%%  loan
%%  Type: - Remove and Insert operation
%%  In:   - User: User's identifier
%%        - Isbn: Book's ISBN
%%  Out:  - ok -> No error
%%        - error_not_available_copy -> No loanable copies
%%        - reservation_not_found -> There is no reservation for this couple user - book
%%        - undefined_book -> The ISBN has no copies
%%        - unexpected_error -> Unexpected error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(from_reservation_to_loan ( User::string() , Isbn::string() )
      -> ok | error_not_available_copy | undefined_book | reservation_not_found | unexpected_error).
from_reservation_to_loan(User, Isbn) ->
  F = fun() ->
        Timestamp =now_to_universal_time(timestamp()),
        %Check that there was a reservation for that user and book
        case reservations_by_user_and_book(User, Isbn) of
          {0,_} ->
            reservation_not_found;
          {_,[Res | _]} ->
            %Find an available copy
            case list_available_copies_by_book(Isbn) of
              {_, [H | _]} ->
                {_, Selected_id} = H,
                %Transforms the reservation into a loan
                %Put a stop_date to reservation
                Old_res = insert_element(1,Res,librarink_reserved_book),
                mnesia:write(Old_res#librarink_reserved_book{stop_date = Timestamp, canceled = false}),
                mnesia:delete_object(Old_res),
                %Insert a loan row
                New_loan = {librarink_lent_book, User, Isbn, Selected_id, Timestamp, null},
                mnesia:write(New_loan);
              {0, []} ->
                %Strange situation, there should be at least an available (reserved) copy (the one for this user)
                error_not_available_copy;
              _ ->
                unexpected_error
            end;
          undefined_book ->
            undefined_book;
          _ ->
            unexpected_error
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to reserve a book for a user
%%  Type: - Insert operation
%%  In:   - User: User's identifier
%%        - Isbn: Book's identifier
%%  Out:  - ok -> No error
%%        - unavailable_copies_to_reserve -> There are no copies to reserve for the specified book
%%        - error_insert_failed -> Unexpected error occurs
%%        - book_already_reserved -> The couple User - ISBN already exists
%%        - undefined_book -> The ISBN has no copies
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(add_book_reservation ( User::string(), Isbn::string()) ->
  ok | unavailable_copies_to_reserve | error_insert_failed | book_already_reserved | undefined_book).
add_book_reservation(User, Isbn) ->
  F = fun() ->
        % Check if the user already reserved that book
        case reservations_by_user_and_book(User, Isbn) of
          {0, _} ->
            % Check number of copies for that book
            case count_available_copies_by_book(Isbn) of
              {_, Counter} when Counter > 0->
                % Add requested reservation
                mnesia:write(#librarink_reserved_book{
                  user = User,
                  isbn = Isbn,
                  start_date = now_to_universal_time(timestamp()),
                  stop_date = null,
                  canceled = false});
              {_, Counter} when Counter == 0 ->
                unavailable_copies_to_reserve;
              _ ->
                error_insert_failed
            end;
          {Reservations_counter, _} when Reservations_counter > 0 ->
            book_already_reserved;
          _ ->
            undefined_book
        end
      end,
  mnesia:activity(transaction, F).



%%%%%%%%%%%%%%%%%%%%%%
%%% DELETE OPERATIONS
%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to remove a specified physical book copy
%%  Type: - Delete operation
%%  In:   - Isbn: Book's identifier
%%        - Physical_copy_id: Book's copy id
%%  Out:  - ok -> No error
%%        - undefined_book_copy -> The ISBN has no copy with this id
%%        - error_pending_loan -> This copy is currently assigned to a user
%%        - error_all_copies_reserved_or_lent -> No available copy to delete
%%        - undefined_book  -> The ISBN has no copies
%%        - unexpected_error -> Unexpected error occurs
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(delete_book_copy ( Isbn::string(), Physical_copy_id::string()) ->
  ok | undefined_book_copy | error_pending_loan | error_all_copies_reserved_or_lent | undefined_book | unexpected_error).
delete_book_copy(Isbn, Physical_copy_id) ->
  F = fun() ->
        %check that number of available copies is strictly greater than the number of reservation for the specified book
        case count_available_copies_by_book(Isbn) of
          {ok, Counter} when Counter > 0 ->
            %check there are no pending loan for that copy
            case loan_by_book_copy(Isbn, Physical_copy_id) of
              {0,[]} ->
                To_delete = #librarink_physical_book_copy{isbn = Isbn, physical_copy_id = Physical_copy_id, _ = '_'},
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
            end;
          {ok, 0} ->
            error_all_copies_reserved_or_lent;
          undefined_book ->
            undefined_book;
          _ ->
            unexpected_error
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to remove all physical copies of a
%%  specified book
%%  Type: - Delete operation
%%  In:   - Isbn: Book's ISBN
%%  Out:  - ok -> No error
%%        - error_pending_loan_or_reservation -> This book is currently assigned to a user
%%        - undefined_book -> The ISBN has no copies
%%        - unexpected_error -> Unexpected error occurs
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(delete_all_book_copies ( Isbn::string() ) ->
  ok | error_pending_loan_or_reservation | undefined_book | unexpected_error).
delete_all_book_copies(Isbn) ->
  F = fun() ->
        %check there are no pending loan or reservation for that book
        case count_available_copies_by_book(Isbn) of
          {_, Available_counter} ->
            {All_copies_counter, _} = copies_by_book(Isbn),
            if
              All_copies_counter == Available_counter ->
                mnesia:delete({librarink_physical_book_copy, Isbn});
              true ->
                error_pending_loan_or_reservation
            end;
          undefined_book ->
            undefined_book;
          _ ->
            unexpected_error
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to remove a specified book loan
%%  Type: - Delete operation
%%  In:   - User: User's identifier
%%        - Isbn: Book's identifier
%%        - Id: Book's copy id
%%  Out:  - ok -> No error
%%        - error_pending_loan -> This book is currently assigned to a user and loan is not terminated
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(delete_lent_book (User::string(), Isbn::string(), Id::string()) ->
  ok | error_pending_loan).
delete_lent_book(User, Isbn, Id) ->
  F = fun() ->
        To_delete = #librarink_lent_book{user = User, isbn = Isbn, physical_copy_id = Id, _ = '_'},
        List = mnesia:match_object(To_delete),
        case List of
          [] ->
            ok;
          _ ->
            [Res|_] = lists:map(
              fun(Row) ->
                if
                  Row#librarink_lent_book.stop_date =/= null ->
                    mnesia:delete_object(Row);
                  true ->
                    error_pending_loan
                end
              end, List
            ),
            Res
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to get and remove all ended loan
%%  Type: - Read and delete operation
%%  In:   No input
%%  Out:  - {Records_counter, Records_list} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(get_and_delete_ended_loans () -> {Records_counter::pos_integer(), Records_list::list(tuple())}).
get_and_delete_ended_loans() ->
  F = fun() ->
        Res = all_ended_loans(),
        delete_lent_book('_', '_', '_'),
        Res
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to remove all loan for a specified book
%%  Type: - Delete operation
%%  In:   - Isbn: Book's identifier
%%  Out:  - ok -> No error
%%        - error_pending_loan -> This book is currently assigned to at least one user and loan is not terminated
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(delete_lent_by_book (Isbn::string()) -> ok | error_pending_loan).
delete_lent_by_book(Isbn) ->
  delete_lent_book('_', Isbn, '_').


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to remove all book loans for a specified
%%  user
%%  Type: - Delete operation
%%  In:   - User: User's identifier
%%  Out:  - ok -> No error
%%        - error_pending_loan -> Some books are currently assigned to the user and some loans are not terminated
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(delete_lent_book_by_user (User::string()) -> ok | error_pending_loan).
delete_lent_book_by_user(User) ->
  delete_lent_book(User, '_', '_').


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to remove a specified book reservation
%%  Type: - Delete operation
%%  In:   - User: User's identifier
%%        - Isbn: Book's identifier
%%  Out:  - ok -> No error
%%        - error_pending_reservation -> This book is currently assigned to the user and reservation is not terminated
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(delete_book_reservation (User::string(), Isbn::string()) -> ok | error_pending_reservation).
delete_book_reservation(User, Isbn) ->
  F = fun() ->
        To_delete = #librarink_reserved_book{user = User, isbn = Isbn, _ = '_'},
        List = mnesia:match_object(To_delete),
        case List of
          [] ->
            ok;
          _ ->
            [Res|_] = lists:map(
              fun(Row) ->
                if
                  Row#librarink_reserved_book.stop_date =/= null ->
                    mnesia:delete_object(Row);
                  true ->
                    error_pending_reservation
                end
              end, List
            ),
            Res
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to get and remove all ended book
%%  reservations
%%  Type: - Read and delete operation
%%  In:   No input
%%  Out:  - {Records_counter, Records_list} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(get_and_delete_ended_reservations () -> {Records_counter::pos_integer(), Records_list::list(tuple())}).
get_and_delete_ended_reservations() ->
  F = fun() ->
        Res = all_ended_reservations(),
        delete_book_reservation('_', '_'),
        Res
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to remove all book reservation for a
%%  specified user
%%  Type: - Delete operation
%%  In:   - User: User's identifier
%%  Out:  - ok -> No error
%%        - error_pending_reservation -> Some books are currently assigned the user and some reservations are not terminated
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(delete_books_reservation_by_user (User::string()) -> ok | error_pending_reservation).
delete_books_reservation_by_user(User) ->
  delete_book_reservation(User, '_').


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to remove all book reservation for a
%%  specified book
%%  Type: - Delete operation
%%  In:   - Isbn: Book's identifier
%%  Out:  - ok -> No error
%%        - error_pending_reservation -> This book is currently assigned to a user and reservation is not terminated
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(delete_book_reservations_by_book (Isbn::string()) -> ok | error_pending_reservation).
delete_book_reservations_by_book(Isbn) ->
  delete_book_reservation('_', Isbn).


%%%%%%%%%%%%%%%%%%%%%%
%%% READ OPERATIONS
%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to count and get all physical copies of a
%%  specified book
%%  Type: - Read operation
%%  In:   - Isbn: Selected book's ISBN
%%  Out:  - {Records_counter, Records_list} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(copies_by_book (Isbn::string()) -> {Records_counter::pos_integer(), Records_list::list(tuple())}).
copies_by_book(Isbn) ->
  F = fun() ->
        Record=#librarink_physical_book_copy{isbn = Isbn,_='_'},
        [ {Row_isbn, Row_id} ||
          #librarink_physical_book_copy{isbn = Row_isbn, physical_copy_id = Row_id} <- mnesia:match_object(Record)]
      end,
  Result_list = mnesia:activity(transaction, F),
  {length(Result_list) , Result_list}.


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to count and get all physical copies of
%%  all books
%%  Type: - Read operation
%%  In:   No input
%%  Out:  - {Records_counter, Records_list} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(all_copies_all_book () -> {Records_counter::pos_integer(), Records_list::list(tuple())}).
all_copies_all_book() ->
  copies_by_book('_').


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to count available physical copies
%%  of a specified book
%%  Type: - Read operation
%%  In:   - Isbn: Selected book's ISBN
%%  Out:  - {ok, Copies_counter} -> No error
%%        - unexpected_error -> Unexpected error occurs
%%        - undefined_book -> There are no copies for the selected book
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(count_available_copies_by_book (Isbn::string()) ->
  {ok, Copies_counter::pos_integer()} | unexpected_error | undefined_book).
count_available_copies_by_book(Isbn) ->
  F = fun() ->
        All_copies = copies_by_book(Isbn),
        case All_copies of
          {0, _} ->
            undefined_book;
          {Counter, _} when Counter > 0 ->
            %Available -> Not lent neither reserved
            {Reserved_copies,_} = reservations_by_book(Isbn),
            {Lent_copies_counter, _} = lent_copies_by_book(Isbn),
            Available_counter = Counter - Reserved_copies - Lent_copies_counter,
            {ok, Available_counter};
          _ ->
            unexpected_error
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to get available physical copies
%%  of a specified book
%%  Type: - Read operation
%%  In:   - Isbn: Selected book's ISBN
%%  Out:  - {ok, Records_list} -> No error
%%        - unexpected_error -> Unexpected error occurs
%%        - undefined_book -> There are no copies for the selected book
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(list_available_copies_by_book (Isbn::string()) ->
  {ok, Records_list::list(tuple())} | unexpected_error | undefined_book).
list_available_copies_by_book(Isbn) ->
  F = fun() ->
        All_copies = copies_by_book(Isbn),
        case All_copies of
          {0, _} ->
            undefined_book;
          {Counter, List_of_copies} when Counter > 0->
            {_,Lent_copies} = lent_copies_by_book(Isbn),
            %Available copies-> Not lent
            Extracted_lent_copies = [{Row_isbn, Row_id} ||
              #librarink_lent_book{isbn = Row_isbn, physical_copy_id = Row_id} <- Lent_copies],
            Not_lent_copies = List_of_copies -- Extracted_lent_copies,
            {ok, Not_lent_copies};
          _ ->
            unexpected_error
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to count and get lent physical copies of a
%%  specified book
%%  Type: - Read operation
%%  In:   - Isbn: Selected book's ISBN
%%  Out:  - {Records_counter, Records_list} -> No error
%%        - undefined_book -> There are no copies for the selected book
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(lent_copies_by_book (Isbn::string()) ->
  {Records_counter::pos_integer(), Records_list::list(tuple())} | undefined_book).
lent_copies_by_book(Isbn) ->
  F = fun() ->
        {Copies_counter, _} = copies_by_book(Isbn),
        case   Copies_counter == 0 of
          true ->
            undefined_book;
          false ->
            Result_list = get_pending_loans('_', Isbn, '_'),
            {length(Result_list) , Result_list}
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to get information about pending lent
%%  physical book copies of a specified user
%%  Type: - Read operation
%%  In:   - User: User's identifier
%%  Out:  - {Records_counter, Records_list} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(loans_by_user (User::string()) -> {Records_counter::pos_integer(), Records_list::list(tuple())}).
loans_by_user(User) ->
  F = fun() ->
        get_pending_loans(User, '_', '_')
      end,
  Result_list = mnesia:activity(transaction, F),
  {length(Result_list), Result_list}.


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to get information about pending loan
%%  for a specified book copy
%%  Type: - Read operation
%%  In:   - Isbn: Selected book's ISBN
%%        - Copy_Id: Book's copy id
%%  Out:  - {Records_counter, Records_list} -> No error
%%        - undefined_book_copy -> There are no copies with the specified id for the selected book
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(loan_by_book_copy ( Isbn::string(), Copy_Id::string() ) ->
  {Records_counter::pos_integer(), Records_list::list(tuple())} | undefined_book_copy).
loan_by_book_copy(Isbn, Copy_Id) ->
  F = fun() ->
        {_ , List_of_copies} = copies_by_book(Isbn),
        case lists:member({Isbn, Copy_Id}, List_of_copies) of
          true ->
            Result_list = get_pending_loans('_', Isbn, Copy_Id),
            {length(Result_list), Result_list};
          false ->
            undefined_book_copy
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to get information about all pending loan
%%  Type: - Read operation
%%  In:   No input
%%  Out:  - {Records_counter, Records_list} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(all_pending_loans() -> {Records_counter::pos_integer(), Records_list::list(tuple())}).
all_pending_loans() ->
  F = fun() ->
        get_pending_loans('_', '_', '_')
      end,
  Result_list = mnesia:activity(transaction, F),
  {length(Result_list) , Result_list}.


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to get information about ended loans
%%  Type: - Read operation
%%  In:   No input
%%  Out:  - {Records_counter, Records_list} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(all_ended_loans () -> {Records_counter::pos_integer(), Records_list::list(tuple())}).
all_ended_loans() ->
  F = fun() ->
        Match = ets:fun2ms(
          fun(#librarink_lent_book{ user = Record_user,
                                    isbn = Record_isbn,
                                    physical_copy_id = Record_id,
                                    start_date = Record_start,
                                    stop_date = Record_stop})
            when Record_stop =/= null ->
            {Record_user, Record_isbn, Record_id, Record_start, Record_stop}
          end
        ),
        mnesia:select(librarink_lent_book, Match)
      end,
  Result_list = mnesia:activity(transaction, F),
  {length(Result_list) , Result_list}.


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to get information about pending
%%  reservation for a specified book and user
%%  Type: - Read operation
%%  In:   - User: User's identifier
%%        - Isbn: Selected book's ISBN
%%  Out:  - {Records_counter, Records_list} -> No error
%%        - undefined_book -> There are no copies for the selected book
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(reservations_by_user_and_book ( User::string(), Isbn::string() ) ->
  {Records_counter::pos_integer(), Records_list::list(tuple())} | undefined_book).
reservations_by_user_and_book(User,Isbn) ->
  F = fun() ->
        {Copies_counter , _} = copies_by_book(Isbn),
        case Copies_counter == 0 of
          true ->
            undefined_book;
          false ->
            Record=#librarink_reserved_book{user = User, isbn = Isbn, stop_date = null,  _='_'},
            Result_list = [{Record_user, Record_isbn, Record_start, Record_stop, Record_cancelled} ||
              #librarink_reserved_book{ user = Record_user,
                                        isbn = Record_isbn,
                                        start_date = Record_start,
                                        stop_date = Record_stop,
                                        canceled = Record_cancelled}
                <- mnesia:match_object(Record)],
            {length(Result_list) , Result_list}
        end
      end,
  mnesia:activity(transaction, F).



%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to count and get information about books
%%  pending reservations for a specified user
%%  Type: - Read operation
%%  In:   - User: User's identifier
%%  Out:  - {Records_counter, Records_list} -> No error
%%        - undefined_book -> There are no books
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(reservations_by_user (User::string()) ->
  {Records_counter::pos_integer(), Records_list::list(tuple())} | undefined_book).
reservations_by_user(User) ->
  reservations_by_user_and_book(User, '_').


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to count and get information about
%%  pending reservations for a specified book
%%  Type: - Read operation
%%  In:   - Isbn: Selected book's ISBN
%%  Out:  - {Records_counter, Records_list} -> No error
%%        - undefined_book -> There are no copies for the selected book
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(reservations_by_book ( Isbn::string() ) ->
  {Records_counter::pos_integer(), Records_list::list(tuple())} | undefined_book).
reservations_by_book(Isbn) ->
  reservations_by_user_and_book('_', Isbn).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to count and get information about
%%  pending reservations
%%  Type: - Read operation
%%  In:   No input
%%  Out:  - {Records_counter, Records_list} -> No error
%%        - undefined_book -> There are no books
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(all_pending_reservations() -> {Records_counter::pos_integer(), Records_list::list(tuple())} | undefined_book).
all_pending_reservations() ->
  reservations_by_user_and_book('_', '_').


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to count and get information about all
%%  ended reservations
%%  Type: - Read operation
%%  In:   No input
%%  Out:  - {Records_counter, Records_list} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(all_ended_reservations() -> {Records_counter::pos_integer(), Records_list::list(tuple())}).
all_ended_reservations() ->
  F = fun() ->
        Match = ets:fun2ms(
          fun(#librarink_reserved_book{ user = Record_user,
                                        isbn=Record_isbn,
                                        start_date = Record_start,
                                        stop_date = Record_stop,
                                        canceled = Record_cancelled})
            when Record_stop =/= null ->
            {Record_user, Record_isbn, Record_start, Record_stop, Record_cancelled}
          end
        ),
        mnesia:select(librarink_reserved_book, Match)
      end,
  Result_list = mnesia:activity(transaction, F),
  {length(Result_list) , Result_list}.


%%%%%%%%%%%%%%%%%%%%%%
%%% UPDATE OPERATIONS
%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to terminate a book loan for a specified
%%  book copy
%%  Type: - Update operation
%%  In:   - Isbn: Selected book's ISBN
%%        - Copy_Id: Book's copy id
%%  Out:  - ok -> No error
%%        - undefined_book_copy -> There are no copies with the specified id for the selected book
%%        - error_no_loan_found -> The selected copy has no associated pending loan
%%        - unexpected_error -> Unexpected error occurs
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(terminate_loan_by_book ( Isbn::string(), Copy_id::string() ) ->
  ok | undefined_book_copy | error_no_loan_found | unexpected_error).
terminate_loan_by_book( Isbn, Copy_id ) ->
  F = fun() ->
        case loan_by_book_copy(Isbn, Copy_id) of
          undefined_book_copy->
            undefined_book_copy;
          {0,[]}->
            error_no_loan_found;
          {1,[Row]}->
            Stop_date = now_to_universal_time(timestamp()),
            % Create a valid record putting name of the table as first element
            Db_row = insert_element(1,Row,librarink_lent_book),
            % Write a new version of the record with set stop_date
            mnesia:write(Db_row#librarink_lent_book{stop_date = Stop_date}),
            % Delete the old version of the record
            mnesia:delete_object(Db_row);
          _ ->
            unexpected_error
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to cancel a book reservation for a
%%  specified book and user
%%  Type: - Update operation
%%  In:   - Isbn: Selected book's ISBN
%%        - User: User's identifier
%%  Out:  - ok -> No error
%%        - undefined_book -> There are no copies for the selected book
%%        - error_no_reservation_found -> The selected book has no associated pending reservation
%%        - error_cancellation_failed -> Cancellation fails because of an unexpected error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(cancel_reservation_by_book_and_user ( Isbn::string(), User::string() ) ->
  ok | undefined_book | error_no_reservation_found | error_cancellation_failed).
cancel_reservation_by_book_and_user(Isbn, User) ->
  F = fun() ->
        case reservations_by_user_and_book(User, Isbn) of
          undefined_book->
            undefined_book;
          {0,[]}->
            error_no_reservation_found;
          {_,[Row]}->
            Stop_date = now_to_universal_time(timestamp()),
            % Create a valid record putting name of the table as first element
            Db_row = insert_element(1,Row,librarink_reserved_book),
            % Write a new version of the record with set stop_date and cancelled
            mnesia:write(Db_row#librarink_reserved_book{stop_date = Stop_date, canceled = true}),
            % Delete the old version of the record
            mnesia:delete_object(Db_row);
          _ ->
            error_cancellation_failed
        end
      end,
  mnesia:activity(transaction, F).


%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE OPERATIONS
%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @private
%% @doc <pre>
%%  This function is called to get information about pending loans
%%  Type: - Read operation
%%  In:   - User: User's identifier
%%        - Isbn: Selected book's ISBN
%%        - Copy_Id: Book's copy id
%%  Out:  - {Records_counter, Records_list} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(get_pending_loans (User::string(), Isbn::string(), Copy_Id::string() ) ->
  {Records_counter::pos_integer(), Records_list::list(tuple())}).
get_pending_loans(User, Isbn, Copy_Id) ->
  Record=#librarink_lent_book{user = User, isbn = Isbn, physical_copy_id = Copy_Id, stop_date = null,  _='_'},
  [{Record_user, Record_isbn, Record_id, Record_start, Record_stop} ||
    #librarink_lent_book{ user = Record_user,
                          isbn = Record_isbn,
                          physical_copy_id = Record_id,
                          start_date = Record_start,
                          stop_date = Record_stop}
      <- mnesia:match_object(Record)].
