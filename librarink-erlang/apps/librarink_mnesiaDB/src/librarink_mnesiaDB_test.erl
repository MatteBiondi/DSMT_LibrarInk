%%%-------------------------------------------------------------------
%%% @hidden
%%% @doc
%%% @end
%%% Created : 03. set 2022 15:07
%%%-------------------------------------------------------------------

-module(librarink_mnesiaDB_test).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

%% Test generator, need to expand default timeout (5 secs)
install_test_() ->
  application:set_env(mnesia, dir, "_build/test/mnesia_database"),
  {timeout, 10, ?_assertEqual(install_succeeded, librarink_mnesiaDB:install([node()],[]))}.

add_test_() -> {"Insert", [
  {"Book",[
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("AAA", "ID-001")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("BBB", "ID-001")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("BBB", "ID-002")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("CCC", "ID-001")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("CCC", "ID-002")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("CCC", "ID-003")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("DDD", "ID-001")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("DDD", "ID-002")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("DDD", "ID-003")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("DDD", "ID-004")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("EEE", "ID-001")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("EEE", "ID-002")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("EEE", "ID-003")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("EEE", "ID-004")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("EEE", "ID-005")),
    ?_assertMatch({15,_},librarink_mnesiaDB:all_copies_all_book())
  ]},
  {"Reservation", [
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_reservation("federico", "AAA")),
    ?_assertEqual(book_already_reserved, librarink_mnesiaDB:add_book_reservation("federico", "AAA")),
    ?_assertEqual(unavailable_copies_to_reserve, librarink_mnesiaDB:add_book_reservation("matteo", "AAA")),
    ?_assertEqual(undefined_book, librarink_mnesiaDB:add_book_reservation("federico", "XYZ")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_reservation("matteo", "BBB")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_reservation("matteo", "CCC")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_reservation("gianluca", "BBB")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_reservation("federico", "CCC")),
    ?_assertMatch({5,_},librarink_mnesiaDB:all_pending_reservations()),
    ?_assertMatch({0,_},librarink_mnesiaDB:all_ended_reservations())
  ]},
  {"Loan", [
    ?_assertEqual(ok, librarink_mnesiaDB:from_reservation_to_loan("federico", "CCC")),
    ?_assertEqual(ok, librarink_mnesiaDB:from_reservation_to_loan("matteo", "BBB")),
    ?_assertEqual(reservation_not_found, librarink_mnesiaDB:from_reservation_to_loan("federico", "DDD")),
    ?_assertMatch({3,_},librarink_mnesiaDB:all_pending_reservations()),
    ?_assertMatch({2,_},librarink_mnesiaDB:all_pending_loans()),
    ?_assertMatch({0,_},librarink_mnesiaDB:all_ended_loans())
    ]}
]}.

delete_test_() ->{"Delete", [
  {"Book",[
    ?_assertEqual(ok, librarink_mnesiaDB:delete_book_copy("DDD", "ID-004")),
    ?_assertEqual(undefined_book_copy, librarink_mnesiaDB:delete_book_copy("DDD", "ID-999")),
    ?_assertEqual(undefined_book, librarink_mnesiaDB:delete_book_copy("ZZZ", "ID-001")),
    ?_assertEqual(error_pending_loan, librarink_mnesiaDB:delete_book_copy("CCC", "ID-001")),
    ?_assertEqual(error_pending_loan_or_reservation, librarink_mnesiaDB:delete_all_book_copies("CCC")),
    ?_assertNotEqual(ok, librarink_mnesiaDB:delete_book_copy("AAA", "ID-001")), %% Pending reservation
    ?_assertEqual(error_pending_loan_or_reservation, librarink_mnesiaDB:delete_all_book_copies("AAA")),
    ?_assertEqual(ok, librarink_mnesiaDB:delete_all_book_copies("DDD")),
    ?_assertMatch({11,_},librarink_mnesiaDB:all_copies_all_book()),
    ?_assertEqual(undefined_book, librarink_mnesiaDB:delete_all_book_copies("XYZ"))
  ]},
  {"Reservation", [
    ?_assertEqual(error_pending_reservation, librarink_mnesiaDB:delete_book_reservation("gianluca", "BBB")), %% Not deleted -> ok
    ?_assertEqual(error_pending_reservation, librarink_mnesiaDB:delete_book_reservation("federico", "AAA")),  %% Not Deleted -> ok
    ?_assertEqual(ok, librarink_mnesiaDB:cancel_reservation_by_book_and_user("AAA", "federico")),
    ?_assertEqual(ok, librarink_mnesiaDB:cancel_reservation_by_book_and_user("BBB", "gianluca")),
    ?_assertMatch({4,_},librarink_mnesiaDB:all_ended_reservations()),
    ?_assertEqual(ok, librarink_mnesiaDB:delete_book_reservation("federico", "AAA")),  %% Deleted
    ?_assertMatch({1,_},librarink_mnesiaDB:all_pending_reservations()),
    ?_assertMatch({3,_},librarink_mnesiaDB:all_ended_reservations()),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_reservation("matteo", "AAA")),
    ?_assertMatch({2,_},librarink_mnesiaDB:all_pending_reservations()),

    ?_assertEqual(ok, librarink_mnesiaDB:add_book_reservation("giovanni", "EEE")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_reservation("daniele", "EEE")),
    ?_assertMatch({4,_},librarink_mnesiaDB:all_pending_reservations()),
    ?_assertEqual(ok, librarink_mnesiaDB:cancel_reservation_by_book_and_user("EEE", "giovanni")),
    ?_assertEqual(ok, librarink_mnesiaDB:cancel_reservation_by_book_and_user("EEE", "daniele")),
    ?_assertMatch({5,_},librarink_mnesiaDB:all_ended_reservations()),
    ?_assertEqual(ok,librarink_mnesiaDB:delete_books_reservation_by_user("giovanni")),
    ?_assertEqual(ok,librarink_mnesiaDB:delete_books_reservation_by_user("daniele")),
    ?_assertMatch({3,_},librarink_mnesiaDB:all_ended_reservations()),

    ?_assertEqual(ok, librarink_mnesiaDB:add_book_reservation("giovanni", "EEE")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_reservation("daniele", "EEE")),
    ?_assertMatch({4,_},librarink_mnesiaDB:all_pending_reservations()),
    ?_assertEqual(ok, librarink_mnesiaDB:cancel_reservation_by_book_and_user("EEE", "giovanni")),
    ?_assertEqual(ok, librarink_mnesiaDB:cancel_reservation_by_book_and_user("EEE", "daniele")),
    ?_assertMatch({5,_},librarink_mnesiaDB:all_ended_reservations()),
    ?_assertEqual(ok,librarink_mnesiaDB:delete_book_reservations_by_book("EEE")),
    ?_assertMatch({3,_},librarink_mnesiaDB:all_ended_reservations()),

    ?_assertMatch({3,_},librarink_mnesiaDB:get_and_delete_ended_reservations()),
    ?_assertMatch({0,_},librarink_mnesiaDB:all_ended_reservations())
  ]},
  {"Loan",[
    ?_assertMatch({2,_},librarink_mnesiaDB:all_pending_loans()),
    ?_assertEqual(ok, librarink_mnesiaDB:terminate_loan_by_book("BBB", "ID-001")),
    ?_assertMatch({1,_},librarink_mnesiaDB:all_pending_loans()),
    ?_assertMatch({1,_},librarink_mnesiaDB:all_ended_loans()),
    ?_assertMatch(ok,librarink_mnesiaDB:delete_lent_book("matteo","BBB","ID-001")),
    ?_assertMatch(ok,librarink_mnesiaDB:delete_lent_book("matteo","BBB","ID-002")), %% Ok,not exists
    ?_assertMatch(error_pending_loan,librarink_mnesiaDB:delete_lent_book("federico","CCC","ID-001")),
    ?_assertMatch(error_pending_loan,librarink_mnesiaDB:delete_lent_by_book("CCC")),
    ?_assertMatch(error_pending_loan,librarink_mnesiaDB:delete_lent_book_by_user("federico")),
    ?_assertMatch({0,_},librarink_mnesiaDB:all_ended_loans()),
    ?_assertEqual(ok, librarink_mnesiaDB:terminate_loan_by_book("CCC", "ID-001")),
    ?_assertMatch({1,_}, librarink_mnesiaDB:get_and_delete_ended_loans())
  ]}
]}.

-endif.