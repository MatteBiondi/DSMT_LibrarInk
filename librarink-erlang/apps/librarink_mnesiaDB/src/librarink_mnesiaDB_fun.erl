%%%-------------------------------------------------------------------
%% @doc
%% librarink_mnesiaDB_fun contains all the allowed operation on Mnesia DB.
%% @end
%%%-------------------------------------------------------------------
-module(librarink_mnesiaDB_fun).

-export([install/2, start_librarink_mnesia/1, stop_librarink_mnesia/1, add_book_copy/2, from_reservation_to_loan/3,
  add_book_reservation/2, delete_book_copy/2, delete_all_book_copies/1, delete_lent_book/3, delete_lent_by_book/1,
  delete_lent_book_by_user/1, delete_book_reservation/2, delete_books_reservation_by_user/1,
  delete_book_reservations_by_book/1, copies_by_book/1, count_available_copies_by_book/1,
  list_available_copies_by_book/1, lent_copies_by_book/1, loans_by_user/1, loan_by_book_copy/2, reservations_by_user/1,
  reservations_by_user_and_book/2, reservations_by_book/1, terminate_loan_by_book/2,
  cancel_reservation_by_book_and_user/2, get_and_delete_ended_reservations/0, get_and_delete_ended_loans/0,
  all_copies_all_book/0, all_pending_reservations/0, all_ended_reservations/0, all_pending_loans/0, all_ended_loans/0, renew_loan_by_book_copy/2]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("kernel/include/logger.hrl").

-import(calendar, [now_to_universal_time/1]).
-import(erlang, [insert_element/3, timestamp/0]).

%% Definition of DB record
-record(librarink_lent_book, {user, isbn, physical_copy_id, start_date, stop_date}).
-record(librarink_reserved_book, {user, isbn, start_date, stop_date, cancelled}).
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
%%  Out:  - {succeed, install_succeeded} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(install(ActiveNodes::list() , BackupNodes::list()) -> {succeed, install_succeeded} ).
install(ActiveNodes, BackupNodes) ->
  %Create schema for all nodes received as parameter
  Nodes = ActiveNodes ++ BackupNodes,
  ?LOG_INFO("Active, Backup: ~p~n",[{ActiveNodes, BackupNodes}]),
  Active = lists:member(node(), ActiveNodes),
  case Active of
    true ->   %Activate Mnesia in all nodes
      mnesia:create_schema(Nodes),
      rpc:multicall(Nodes, application, start, [mnesia]),
      case mnesia:wait_for_tables([ librarink_lent_book,
                                    librarink_reserved_book,
                                    librarink_physical_book_copy], 5000) =:= ok of
        true ->
          {succeed, install_succeeded};
        false ->  %Create tables if not exist
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
          {succeed, install_succeeded}
      end;
      _false -> {succeed, install_succeeded}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% START AND STOP MNESIA
%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to start Mnesia
%%  In:   - Nodes: List of nodes
%%  Out:  - {succeed, ok} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(start_librarink_mnesia(Nodes::list()) ->   {succeed, ok} ).
start_librarink_mnesia(Nodes) ->
  rpc:multicall(Nodes, application, start, [mnesia]),
  {succeed, ok}.


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to stop Mnesia
%%  In:   - Nodes: List of nodes
%%  Out:  - {succeed, ok} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(stop_librarink_mnesia(Nodes::list()) ->   {succeed, ok} ).
stop_librarink_mnesia(Nodes) ->
  rpc:multicall(Nodes, application, stop, [mnesia]),
  {succeed, ok}.

%%%%%%%%%%%%%%%%%%%%%%
%%% INSERT OPERATIONS
%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to add a physical book copy
%%  Type: - Insert operation
%%  In:   - Isbn: Book's ISBN
%%        - Physical_copy_id: Book's copy id
%%  Out:  - {succeed, ok} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(add_book_copy ( Isbn::binary(), Physical_copy_id::binary()) -> {succeed, ok}).
add_book_copy(Isbn, Physical_copy_id) ->
  F = fun() ->
        mnesia:write(#librarink_physical_book_copy{ isbn = Isbn,
                                                    physical_copy_id = Physical_copy_id})
      end,
  mnesia:activity(transaction, F),
  {succeed, ok}.


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to transform a book reservation in a book
%%  loan
%%  Type: - Remove and Insert operation
%%  In:   - User: User's identifier
%%        - Isbn: Book's ISBN
%%  Out:  - {succeed, ok} -> No error
%%        - {error, error_not_available_copy} -> The selected physical copy is unavailable
%%        - {error, error_zero_copies_available} -> No loanable copies
%%        - {error, reservation_not_found} -> There is no reservation for this couple user - book
%%        - {error, undefined_book} -> The ISBN has no copies
%%        - {error, unexpected_error} -> Unexpected error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(from_reservation_to_loan ( User::binary() , Isbn::binary(), Copy_id::binary() )
      ->  {succeed, New_loan::map()} | {error, error_not_available_copy | undefined_book | reservation_not_found |
          unexpected_error | error_zero_copies_available}).
from_reservation_to_loan(User, Isbn, Copy_id) ->
  F = fun() ->
        Timestamp =now_to_universal_time(timestamp()),
        %Check that there was a reservation for that user and book
        case reservations_by_user_and_book(User, Isbn) of
          {succeed,{0,_}} ->
            {error, reservation_not_found};
          {succeed,{_,[Res | _]}} ->
            %check if the specified copy is available
            case list_available_copies_by_book(Isbn) of
              {succeed, []} ->
                %Strange situation, there should be at least an available (reserved) copy (the one for this user)
                {error, error_zero_copies_available};
              {succeed, Available_copies} ->
                case lists:member(#{isbn => Isbn, id => Copy_id}, Available_copies) of
                  true ->
                    %Transforms the reservation into a loan
                    %Put a stop_date to reservation
                    Old_res = from_map_to_record(librarink_reserved_book, Res),
                    mnesia:write(Old_res#librarink_reserved_book{stop_date = Timestamp, cancelled = false}),
                    mnesia:delete_object(Old_res),
                    %Insert a loan row
                    New_loan = #{ user => User,
                                  isbn => Isbn,
                                  id => Copy_id,
                                  start_date => Timestamp,
                                  stop_date => null},
                    Loan_row = from_map_to_record(librarink_lent_book, New_loan),
                    mnesia:write(Loan_row),
                    {succeed, New_loan};
                  false -> {error, error_not_available_copy}
                end;
              _ ->
                {error, unexpected_error}
            end;
          {error, undefined_book} ->
            {error, undefined_book};
          _ ->
            {error, unexpected_error}
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to reserve a book for a user
%%  Type: - Insert operation
%%  In:   - User: User's identifier
%%        - Isbn: Book's identifier
%%  Out:  - {succeed, ok} -> No error
%%        - {error, unavailable_copies_to_reserve} -> There are no copies to reserve for the specified book
%%        - {error, error_insert_failed} -> Unexpected error occurs
%%        - {error, book_already_reserved} -> The couple User - ISBN already exists
%%        - {error, undefined_book} -> The ISBN has no copies
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(add_book_reservation ( User::binary(), Isbn::binary()) ->
  {succeed, ok} | {error, unavailable_copies_to_reserve | error_insert_failed | book_already_reserved | undefined_book}).
add_book_reservation(User, Isbn) ->
  F = fun() ->
        % Check if the user already reserved that book
        case reservations_by_user_and_book(User, Isbn) of
          {succeed,{0, _}} ->
            % Check number of copies for that book
            case count_available_copies_by_book(Isbn) of
              {succeed, Counter} when Counter > 0->
                % Add requested reservation
                mnesia:write(#librarink_reserved_book{
                  user = User,
                  isbn = Isbn,
                  start_date = now_to_universal_time(timestamp()),
                  stop_date = null,
                  cancelled = false}),
                {succeed, ok};
              {succeed, Counter} when Counter == 0 ->
                {error, unavailable_copies_to_reserve};
              _ ->
                {error, error_insert_failed}
            end;
          {succeed,{Reservations_counter, _}} when Reservations_counter > 0 ->
            {error, book_already_reserved};
          _ ->
            {error, undefined_book}
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
%%  Out:  - {succeed, ok} -> No error
%%        - {error, undefined_book_copy} -> The ISBN has no copy with this id
%%        - {error, error_pending_loan} -> This copy is currently assigned to a user
%%        - {error, error_all_copies_reserved_or_lent} -> No available copy to delete
%%        - {error, undefined_book}  -> The ISBN has no copies
%%        - {error, unexpected_error} -> Unexpected error occurs
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(delete_book_copy ( Isbn::binary(), Physical_copy_id::binary()) ->
  {succeed, ok} | {error, undefined_book_copy | error_pending_loan | error_all_copies_reserved_or_lent | undefined_book | unexpected_error}).
delete_book_copy(Isbn, Physical_copy_id) ->
  F = fun() ->
        %check that number of available copies is strictly greater than the number of reservation for the specified book
        case count_available_copies_by_book(Isbn) of
          {succeed, Counter} when Counter > 0 ->
            %check there are no pending loan for that copy
            case loan_by_book_copy(Isbn, Physical_copy_id) of
              {succeed,{0, []}} ->
                To_delete = #librarink_physical_book_copy{isbn = Isbn, physical_copy_id = Physical_copy_id, _ = '_'},
                List = mnesia:match_object(To_delete),
                lists:foreach(
                  fun(Row) ->
                    mnesia:delete_object(Row)
                  end,List
                ),
                {succeed, ok};
              {error, undefined_book_copy} ->
                {error, undefined_book_copy};
              _ ->
                {error, error_pending_loan}
            end;
          {succeed, 0} ->
            {error, error_all_copies_reserved_or_lent};
          {error, undefined_book} ->
            {error, undefined_book};
          _ ->
            {error, unexpected_error}
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to remove all physical copies of a
%%  specified book
%%  Type: - Delete operation
%%  In:   - Isbn: Book's ISBN
%%  Out:  - {succeed, ok} -> No error
%%        - {error, error_pending_loan_or_reservation} -> This book is currently assigned to a user
%%        - {error, undefined_book} -> The ISBN has no copies
%%        - {error, unexpected_error} -> Unexpected error occurs
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(delete_all_book_copies ( Isbn::binary() ) ->
  {succeed, ok} | {error, error_pending_loan_or_reservation | undefined_book | unexpected_error}).
delete_all_book_copies(Isbn) ->
  F = fun() ->
        %check there are no pending loan or reservation for that book
        case count_available_copies_by_book(Isbn) of
          {succeed, Available_counter} ->
            {succeed,{All_copies_counter, _}} = copies_by_book(Isbn),
            if
              All_copies_counter == Available_counter ->
                mnesia:delete({librarink_physical_book_copy, Isbn}),
                {succeed, ok};
              true ->
                {error, error_pending_loan_or_reservation}
            end;
          {error, undefined_book} ->
            {error, undefined_book};
          _ ->
            {error, unexpected_error}
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
%%  Out:  - {succeed, ok} -> No error
%%        - {error, error_pending_loan} -> This book is currently assigned to a user and loan is not terminated
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(delete_lent_book (User::binary(), Isbn::binary(), Id::binary()) ->
  {succeed, ok} | {error, error_pending_loan}).
delete_lent_book(User, Isbn, Id) ->
  F = fun() ->
        To_delete = #librarink_lent_book{user = User, isbn = Isbn, physical_copy_id = Id, _ = '_'},
        List = mnesia:match_object(To_delete),
        case List of
          [] ->
            {succeed, ok};
          _ ->
            [Res|_] = lists:map(
              fun(Row) ->
                if
                  Row#librarink_lent_book.stop_date =/= null ->
                    mnesia:delete_object(Row),
                    {succeed, ok};
                  true ->
                    {error, error_pending_loan}
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
%%  Out:  - {succeed,{Records_counter, Records_list}} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(get_and_delete_ended_loans () -> {succeed,{Records_counter :: pos_integer(), Records_list :: list(map())}}).
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
%%  Out:  - {succeed, ok} -> No error
%%        - {error, error_pending_loan} -> This book is currently assigned to at least one user and loan is not terminated
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(delete_lent_by_book (Isbn::binary()) -> {succeed, ok} | {error, error_pending_loan}).
delete_lent_by_book(Isbn) ->
  delete_lent_book('_', Isbn, '_').


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to remove all book loans for a specified
%%  user
%%  Type: - Delete operation
%%  In:   - User: User's identifier
%%  Out:  - {succeed, ok} -> No error
%%        - {error, error_pending_loan} -> Some books are currently assigned to the user and some loans are not terminated
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(delete_lent_book_by_user (User::binary()) -> {succeed, ok} | {error, error_pending_loan}).
delete_lent_book_by_user(User) ->
  delete_lent_book(User, '_', '_').


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to remove a specified book reservation
%%  Type: - Delete operation
%%  In:   - User: User's identifier
%%        - Isbn: Book's identifier
%%  Out:  - {succeed, ok} -> No error
%%        - {error, error_pending_reservation} -> This book is currently assigned to the user and reservation is not terminated
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(delete_book_reservation (User::binary(), Isbn::binary()) -> {succeed, ok} | {error, error_pending_reservation}).
delete_book_reservation(User, Isbn) ->
  F = fun() ->
        To_delete = #librarink_reserved_book{user = User, isbn = Isbn, _ = '_'},
        List = mnesia:match_object(To_delete),
        case List of
          [] ->
            {succeed, ok};
          _ ->
            [Res|_] = lists:map(
              fun(Row) ->
                if
                  Row#librarink_reserved_book.stop_date =/= null ->
                    mnesia:delete_object(Row),
                    {succeed, ok};
                  true ->
                    {error, error_pending_reservation}
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
%%  Out:  - {succeed,{Records_counter, Records_list}} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(get_and_delete_ended_reservations () ->
  {succeed,{Records_counter :: pos_integer(), Records_list :: list(map())}}).
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
%%  Out:  - {succeed, ok} -> No error
%%        - {error, error_pending_reservation} -> Some books are currently assigned the user and some reservations are not terminated
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(delete_books_reservation_by_user (User::binary()) -> {succeed, ok} | {error, error_pending_reservation}).
delete_books_reservation_by_user(User) ->
  delete_book_reservation(User, '_').


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to remove all book reservation for a
%%  specified book
%%  Type: - Delete operation
%%  In:   - Isbn: Book's identifier
%%  Out:  - {succeed, ok} -> No error
%%        - {error, error_pending_reservation} -> This book is currently assigned to a user and reservation is not terminated
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(delete_book_reservations_by_book (Isbn::binary()) -> {succeed, ok} | {error, error_pending_reservation}).
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
%%  Out:  - {succeed,{Records_counter, Records_list}} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(copies_by_book (Isbn::binary()) -> {succeed,{Records_counter :: pos_integer(), Records_list :: list(map())}}).
copies_by_book(Isbn) ->
  F = fun() ->
        Record=#librarink_physical_book_copy{isbn = Isbn, _='_'},
        [ #{isbn => Row_isbn, id => Row_id} ||
          #librarink_physical_book_copy{isbn = Row_isbn, physical_copy_id = Row_id} <- mnesia:match_object(Record)]
      end,
  Result_list = mnesia:activity(transaction, F),
  {succeed,{length(Result_list), Result_list}}.


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to count and get all physical copies of
%%  all books
%%  Type: - Read operation
%%  In:   No input
%%  Out:  - {succeed,{Records_counter, Records_list}} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(all_copies_all_book () -> {succeed,{Records_counter :: pos_integer(), Records_list :: list(map())}}).
all_copies_all_book() ->
  copies_by_book('_').


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to count available physical copies
%%  of a specified book
%%  Type: - Read operation
%%  In:   - Isbn: Selected book's ISBN
%%  Out:  - {succeed, Copies_counter} -> No error
%%        - {error, unexpected_error} -> Unexpected error occurs
%%        - {error, undefined_book} -> There are no copies for the selected book
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(count_available_copies_by_book (Isbn::binary()) ->
  {succeed, Copies_counter::pos_integer()} | {error, unexpected_error | undefined_book}).
count_available_copies_by_book(Isbn) ->
  F = fun() ->
        All_copies = copies_by_book(Isbn),
        case All_copies of
          {succeed,{0, _}} ->
            {error, undefined_book};
          {succeed,{Counter, _}} when Counter > 0 ->
            %Available -> Not lent neither reserved
            {succeed,{Reserved_copies, _}} = reservations_by_book(Isbn),
            {succeed,{Lent_copies_counter, _}} = lent_copies_by_book(Isbn),
            Available_counter = Counter - Reserved_copies - Lent_copies_counter,
            {succeed, Available_counter};
          _ ->
            {error, unexpected_error}
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to get available physical copies
%%  of a specified book
%%  Type: - Read operation
%%  In:   - Isbn: Selected book's ISBN
%%  Out:  - {succeed, Records_list} -> No error
%%        - {error, unexpected_error} -> Unexpected error occurs
%%        - {error, undefined_book} -> There are no copies for the selected book
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(list_available_copies_by_book (Isbn::binary()) ->
  {succeed, Records_list::list(map())} | {error, unexpected_error | undefined_book}).
list_available_copies_by_book(Isbn) ->
  F = fun() ->
        All_copies = copies_by_book(Isbn),
        case All_copies of
          {succeed,{0, _}} ->
            {error, undefined_book};
          {succeed,{Counter, List_of_copies}} when Counter > 0->
            {succeed,{_, Lent_copies}} = lent_copies_by_book(Isbn),
            %Available copies-> Not lent
            Extracted_lent_copies = [
              #{isbn => Row_isbn, id => Row_id} ||
              #{isbn := Row_isbn, id := Row_id} <- Lent_copies],
            Not_lent_copies = List_of_copies -- Extracted_lent_copies,
            {succeed, Not_lent_copies};
          _ ->
            {error, unexpected_error}
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to count and get lent physical copies of a
%%  specified book
%%  Type: - Read operation
%%  In:   - Isbn: Selected book's ISBN
%%  Out:  - {succeed,{Records_counter, Records_list}} -> No error
%%        - {error, undefined_book} -> There are no copies for the selected book
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(lent_copies_by_book (Isbn::binary()) ->
  {succeed,{Records_counter :: pos_integer(), Records_list :: list(map())}} | {error, undefined_book}).
lent_copies_by_book(Isbn) ->
  F = fun() ->
        {succeed,{Copies_counter, _}} = copies_by_book(Isbn),
        case   Copies_counter == 0 of
          true ->
            {error, undefined_book};
          false ->
            Result_list = get_pending_loans('_', Isbn, '_'),
            {succeed,{length(Result_list), Result_list}}
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to get information about pending lent
%%  physical book copies of a specified user
%%  Type: - Read operation
%%  In:   - User: User's identifier
%%  Out:  - {succeed,{Records_counter, Records_list}} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(loans_by_user (User::binary()) -> {succeed,{Records_counter :: pos_integer(), Records_list :: list(map())}}).
loans_by_user(User) ->
  F = fun() ->
        get_pending_loans(User, '_', '_')
      end,
  Result_list = mnesia:activity(transaction, F),
  {succeed,{length(Result_list), Result_list}}.


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to get information about pending loan
%%  for a specified book copy
%%  Type: - Read operation
%%  In:   - Isbn: Selected book's ISBN
%%        - Copy_Id: Book's copy id
%%  Out:  - {succeed,{Records_counter, Records_list}} -> No error
%%        - {error, undefined_book_copy} -> There are no copies with the specified id for the selected book
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(loan_by_book_copy ( Isbn::binary(), Copy_Id::binary() ) ->
  {succeed,{Records_counter :: pos_integer(), Records_list :: list(map())}} | {error, undefined_book_copy}).
loan_by_book_copy(Isbn, Copy_Id) ->
  F = fun() ->
        {succeed,{_, List_of_copies}} = copies_by_book(Isbn),
        case lists:member(#{isbn => Isbn, id => Copy_Id}, List_of_copies) of
          true ->
            Result_list = get_pending_loans('_', Isbn, Copy_Id),
            {succeed,{length(Result_list), Result_list}};
          false ->
            {error, undefined_book_copy}
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to get information about all pending loan
%%  Type: - Read operation
%%  In:   No input
%%  Out:  - {succeed,{Records_counter, Records_list}} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(all_pending_loans() -> {succeed,{Records_counter :: pos_integer(), Records_list :: list(map())}}).
all_pending_loans() ->
  F = fun() ->
        get_pending_loans('_', '_', '_')
      end,
  Result_list = mnesia:activity(transaction, F),
  {succeed,{length(Result_list), Result_list}}.


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to get information about ended loans
%%  Type: - Read operation
%%  In:   No input
%%  Out:  - {succeed,{Records_counter, Records_list}} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(all_ended_loans () -> {succeed,{Records_counter :: pos_integer(), Records_list :: list(map())}}).
all_ended_loans() ->
  F = fun() ->
        % Get DB records
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
        Res = mnesia:select(librarink_lent_book, Match),

        % Create maps from records
        [#{user => Record_user, isbn => Record_isbn, id => Record_id, start_date => Record_start, stop_date => Record_stop}
          || {Record_user, Record_isbn, Record_id, Record_start, Record_stop} <- Res]
      end,
  Result_list = mnesia:activity(transaction, F),
  {succeed,{length(Result_list), Result_list}}.


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to get information about pending
%%  reservation for a specified book and user
%%  Type: - Read operation
%%  In:   - User: User's identifier
%%        - Isbn: Selected book's ISBN
%%  Out:  - {succeed, {Records_counter, Records_list}} -> No error
%%        - {error, undefined_book} -> There are no copies for the selected book
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(reservations_by_user_and_book ( User::binary(), Isbn::binary() ) ->
  {succeed, {Records_counter :: pos_integer(), Records_list :: list(map())}} | {error, undefined_book}).
reservations_by_user_and_book(User,Isbn) ->
  F = fun() ->
        {succeed,{Copies_counter, _}} = copies_by_book(Isbn),
        case Copies_counter == 0 of
          true ->
            {error, undefined_book};
          false ->
            Record=#librarink_reserved_book{user = User, isbn = Isbn, stop_date = null,  _='_'},
            Result_list = [
              #{user => Record_user, isbn => Record_isbn, start_date => Record_start, stop_date => Record_stop, cancelled => Record_cancelled} ||
              #librarink_reserved_book{ user = Record_user,
                                        isbn = Record_isbn,
                                        start_date = Record_start,
                                        stop_date = Record_stop,
                                        cancelled = Record_cancelled}
                <- mnesia:match_object(Record)],
            {succeed,{length(Result_list), Result_list}}
        end
      end,
  mnesia:activity(transaction, F).



%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to count and get information about books
%%  pending reservations for a specified user
%%  Type: - Read operation
%%  In:   - User: User's identifier
%%  Out:  - {succeed,{Records_counter, Records_list}} -> No error
%%        - {error, undefined_book} -> There are no books
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(reservations_by_user (User::binary()) ->
  {succeed,{Records_counter :: pos_integer(), Records_list :: list(map())}} | {error, undefined_book}).
reservations_by_user(User) ->
  reservations_by_user_and_book(User, '_').


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to count and get information about
%%  pending reservations for a specified book
%%  Type: - Read operation
%%  In:   - Isbn: Selected book's ISBN
%%  Out:  - {succeed,{Records_counter, Records_list}} -> No error
%%        - {error, undefined_book} -> There are no copies for the selected book
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(reservations_by_book ( Isbn::binary() ) ->
  {succeed,{Records_counter :: pos_integer(), Records_list :: list(map())}} | {error, undefined_book}).
reservations_by_book(Isbn) ->
  reservations_by_user_and_book('_', Isbn).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to count and get information about
%%  pending reservations
%%  Type: - Read operation
%%  In:   No input
%%  Out:  - {succeed,{Records_counter, Records_list}} -> No error
%%        - {error, undefined_book} -> There are no books
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(all_pending_reservations() ->
  {succeed,{Records_counter :: pos_integer(), Records_list :: list(map())}} | {error, undefined_book}).
all_pending_reservations() ->
  reservations_by_user_and_book('_', '_').


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to count and get information about all
%%  ended reservations
%%  Type: - Read operation
%%  In:   No input
%%  Out:  - {succeed,{Records_counter, Records_list}} -> No error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(all_ended_reservations() -> {succeed,{Records_counter :: pos_integer(), Records_list :: list(map())}}).
all_ended_reservations() ->
  F = fun() ->
        % Get DB records
        Match = ets:fun2ms(
          fun(#librarink_reserved_book{ user = Record_user,
                                        isbn=Record_isbn,
                                        start_date = Record_start,
                                        stop_date = Record_stop,
                                        cancelled = Record_cancelled})
            when Record_stop =/= null ->
            {Record_user, Record_isbn, Record_start, Record_stop, Record_cancelled}
          end
        ),
        Res = mnesia:select(librarink_reserved_book, Match),

        % Create maps from records
        [#{user => Record_user, isbn => Record_isbn, start_date => Record_start, stop_date => Record_stop, cancelled => Record_cancelled}
          || {Record_user, Record_isbn, Record_start, Record_stop, Record_cancelled} <- Res]
      end,
  Result_list = mnesia:activity(transaction, F),
  {succeed,{length(Result_list), Result_list}}.


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
%%  Out:  - {succeed, ok} -> No error
%%        - {error, undefined_book_copy} -> There are no copies with the specified id for the selected book
%%        - {error, error_no_loan_found} -> The selected copy has no associated pending loan
%%        - {error, unexpected_error} -> Unexpected error occurs
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(terminate_loan_by_book ( Isbn::binary(), Copy_id::binary() ) ->
  {succeed, ok} | {error, undefined_book_copy | error_no_loan_found | unexpected_error}).
terminate_loan_by_book( Isbn, Copy_id ) ->
  F = fun() ->
        case loan_by_book_copy(Isbn, Copy_id) of
          {error, undefined_book_copy}->
            {error, undefined_book_copy};
          {succeed,{0, []}}->
            {error, error_no_loan_found};
          {succeed,{1, [Row]}}->
            Stop_date = now_to_universal_time(timestamp()),
            % Create a valid record putting name of the table as first element
            Db_row = from_map_to_record(librarink_lent_book, Row),
            % Write a new version of the record with set stop_date
            mnesia:write(Db_row#librarink_lent_book{stop_date = Stop_date}),
            % Delete the old version of the record
            mnesia:delete_object(Db_row),
            {succeed, ok};
          _ ->
            {error, unexpected_error}
        end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to renew a book loan for a specified
%%  book copy
%%  Type: - Update operation
%%  In:   - User: User's identifier
%%        - Isbn: Selected book's ISBN
%%        - Copy_Id: Book's copy id
%%  Out:  - {succeed, ok} -> No error
%%        - {error, undefined_book_copy} -> There are no copies with the specified id for the selected book
%%        - {error, error_no_loan_found} -> The selected copy has no associated pending loan
%%        - {error, unexpected_error} -> Unexpected error occurs
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(renew_loan_by_book_copy ( Isbn::binary(), Copy_id::binary() ) ->
  {succeed, ok} | {error, undefined_book_copy | error_no_loan_found | unexpected_error}).
renew_loan_by_book_copy( Isbn, Copy_id ) ->
  F = fun() ->
    case loan_by_book_copy(Isbn, Copy_id) of
      {error, undefined_book_copy}->
        {error, undefined_book_copy};
      {succeed,{0, []}}->
        {error, error_no_loan_found};
      {succeed,{1, [Row]}}->
        Today = now_to_universal_time(timestamp()),
        % Create a valid record putting name of the table as first element
        Db_row = from_map_to_record(librarink_lent_book,Row),
        % Write a new version of the record with set stop_date to end old loan
        mnesia:write(Db_row#librarink_lent_book{stop_date = Today}),
        % Write a new version of the record with set start_date to renew the loan
        mnesia:write(Db_row#librarink_lent_book{start_date = Today}),
        % Delete the old version of the record
        mnesia:delete_object(Db_row),
        {succeed, ok};
      _ ->
        {error, unexpected_error}
    end
      end,
  mnesia:activity(transaction, F).


%%--------------------------------------------------------------------
%% @doc <pre>
%%  This function is called to cancel a book reservation for a
%%  specified book and user
%%  Type: - Update operation
%%  In:   - User: User's identifier
%%        - Isbn: Selected book's ISBN
%%  Out:  - {succeed, ok} -> No error
%%        - {error, undefined_book} -> There are no copies for the selected book
%%        - {error, error_no_reservation_found} -> The selected book has no associated pending reservation
%%        - {error, error_cancellation_failed} -> Cancellation fails because of an unexpected error
%%        - Throws an exception in case of error
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(cancel_reservation_by_book_and_user ( User::binary(), Isbn::binary() ) ->
  {succeed, ok} | {error, undefined_book | error_no_reservation_found | error_cancellation_failed}).
cancel_reservation_by_book_and_user(User, Isbn) ->
  F = fun() ->
        case reservations_by_user_and_book(User, Isbn) of
          {error, undefined_book}->
            {error, undefined_book};
          {succeed,{0, []}}->
            {error, error_no_reservation_found};
          {succeed, {_, [Row]}}->
            Stop_date = now_to_universal_time(timestamp()),
            % Create a valid record putting name of the table as first element
            Db_row = from_map_to_record(librarink_reserved_book,Row),
            % Write a new version of the record with set stop_date and cancelled
            mnesia:write(Db_row#librarink_reserved_book{stop_date = Stop_date, cancelled = true}),
            % Delete the old version of the record
            mnesia:delete_object(Db_row),
            {succeed, ok};
          _ ->
            {error, error_cancellation_failed}
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
-spec(get_pending_loans (User::binary(), Isbn::binary(), Copy_Id::binary() ) ->
  {Records_counter::pos_integer(), Records_list::list(map())}).
get_pending_loans(User, Isbn, Copy_Id) ->
  Record=#librarink_lent_book{user = User, isbn = Isbn, physical_copy_id = Copy_Id, stop_date = null,  _='_'},
  [#{user => Record_user, isbn => Record_isbn, id => Record_id, start_date => Record_start, stop_date => Record_stop} ||
    #librarink_lent_book{ user = Record_user,
                          isbn = Record_isbn,
                          physical_copy_id = Record_id,
                          start_date = Record_start,
                          stop_date = Record_stop}
      <- mnesia:match_object(Record)].


%%--------------------------------------------------------------------
%% @private
%% @doc <pre>
%%  This function is called to build a valid DB tuple from a map. This
%%  function should be used with caution to avoid drops in performance.
%%  Type: - Transformation operation
%%  In:   - Record_type: The tpe of record to be created
%%        - Map: Map data to be transformed in record
%%  Out:  - Requested record with values from map argument
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec(from_map_to_record (librarink_physical_book_copy|librarink_reserved_book|librarink_lent_book, Map::map()) ->
  {Record::tuple()}).

from_map_to_record(librarink_physical_book_copy, Map)->
  #librarink_physical_book_copy{
    isbn = maps:get(isbn, Map),
    physical_copy_id = maps:get(id,Map)
  };

from_map_to_record(librarink_reserved_book, Map)->
  #librarink_reserved_book{
    user = maps:get(user,Map),
    isbn = maps:get(isbn, Map),
    start_date = maps:get(start_date,Map),
    stop_date = maps:get(stop_date,Map),
    cancelled = maps:get(cancelled,Map)
  };

from_map_to_record(librarink_lent_book, Map)->
  #librarink_lent_book{
    user = maps:get(user,Map),
    isbn = maps:get(isbn, Map),
    physical_copy_id = maps:get(id,Map),
    start_date = maps:get(start_date,Map),
    stop_date = maps:get(stop_date,Map)
  }.
