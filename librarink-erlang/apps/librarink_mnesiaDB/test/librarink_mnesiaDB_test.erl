%%%-------------------------------------------------------------------
%%% @hidden
%%% @doc
%%% Test module for DB operations
%%% @end
%%% Created : 03. set 2022 15:07
%%%-------------------------------------------------------------------

-module(librarink_mnesiaDB_test).

-ifdef(EUNIT).

-import(erlang, [system_time/0]).

-include_lib("eunit/include/eunit.hrl").

%% Test generator, need to expand default timeout (5 secs)
install_test_() ->
  DirName = "_build/test/mnesia_database"++integer_to_list(system_time()),
  application:set_env(mnesia, dir, DirName),
  {timeout, 10, ?_assertEqual({succeed,install_succeeded}, librarink_mnesiaDB_fun:install([node()],[]))}.

add_test_() -> {"Insert", [
  {"Book",[
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_copy("AAA", "ID-001")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_copy("BBB", "ID-001")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_copy("BBB", "ID-002")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_copy("CCC", "ID-001")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_copy("CCC", "ID-002")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_copy("CCC", "ID-003")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_copy("DDD", "ID-001")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_copy("DDD", "ID-002")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_copy("DDD", "ID-003")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_copy("DDD", "ID-004")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_copy("EEE", "ID-001")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_copy("EEE", "ID-002")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_copy("EEE", "ID-003")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_copy("EEE", "ID-004")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_copy("EEE", "ID-005")),
    ?_assertMatch({succeed,{15, _}},librarink_mnesiaDB_fun:all_copies_all_book())
  ]},
  {"Reservation", [
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_reservation("federico", "AAA")),
    ?_assertEqual({error,book_already_reserved}, librarink_mnesiaDB_fun:add_book_reservation("federico", "AAA")),
    ?_assertEqual({error,unavailable_copies_to_reserve}, librarink_mnesiaDB_fun:add_book_reservation("matteo", "AAA")),
    ?_assertEqual({error,undefined_book}, librarink_mnesiaDB_fun:add_book_reservation("federico", "XYZ")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_reservation("matteo", "BBB")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_reservation("matteo", "CCC")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_reservation("gianluca", "BBB")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_reservation("federico", "CCC")),
    ?_assertMatch({succeed,{5, _}},librarink_mnesiaDB_fun:all_pending_reservations()),
    ?_assertMatch({succeed,{0, _}},librarink_mnesiaDB_fun:all_ended_reservations())
  ]},
  {"Loan", [
    ?_assertMatch({succeed,_}, librarink_mnesiaDB_fun:from_reservation_to_loan("federico", "CCC", "ID-001")),
    ?_assertMatch({succeed, _}, librarink_mnesiaDB_fun:from_reservation_to_loan("matteo", "BBB", "ID-001")),
    ?_assertEqual({error,reservation_not_found}, librarink_mnesiaDB_fun:from_reservation_to_loan("federico", "DDD", "ID-001")),
    ?_assertMatch({succeed,{3,_}},librarink_mnesiaDB_fun:all_pending_reservations()),
    ?_assertMatch({succeed,{2,_}},librarink_mnesiaDB_fun:all_pending_loans()),
    ?_assertMatch({succeed,{0,_}},librarink_mnesiaDB_fun:all_ended_loans())
    ]}
]}.

delete_test_() ->{"Delete", [
  {"Book",[
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:delete_book_copy("DDD", "ID-004")),
    ?_assertEqual({error,undefined_book_copy}, librarink_mnesiaDB_fun:delete_book_copy("DDD", "ID-999")),
    ?_assertEqual({error,undefined_book}, librarink_mnesiaDB_fun:delete_book_copy("ZZZ", "ID-001")),
    ?_assertEqual({error,error_pending_loan}, librarink_mnesiaDB_fun:delete_book_copy("CCC", "ID-001")),
    ?_assertEqual({error,error_pending_loan_or_reservation}, librarink_mnesiaDB_fun:delete_all_book_copies("CCC")),
    ?_assertNotEqual({succeed,ok}, librarink_mnesiaDB_fun:delete_book_copy("AAA", "ID-001")), %% Pending reservation
    ?_assertEqual({error,error_pending_loan_or_reservation}, librarink_mnesiaDB_fun:delete_all_book_copies("AAA")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:delete_all_book_copies("DDD")),
    ?_assertMatch({succeed,{11, _}},librarink_mnesiaDB_fun:all_copies_all_book()),
    ?_assertEqual({error,undefined_book}, librarink_mnesiaDB_fun:delete_all_book_copies("XYZ"))
  ]},
  {"Reservation", [
    ?_assertEqual({error,error_pending_reservation}, librarink_mnesiaDB_fun:delete_book_reservation("gianluca", "BBB")), %% Not deleted -> ok
    ?_assertEqual({error,error_pending_reservation}, librarink_mnesiaDB_fun:delete_book_reservation("federico", "AAA")),  %% Not Deleted -> ok
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:cancel_reservation_by_book_and_user("federico", "AAA")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:cancel_reservation_by_book_and_user("gianluca", "BBB")),
    ?_assertMatch({succeed,{4, _}},librarink_mnesiaDB_fun:all_ended_reservations()),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:delete_book_reservation("federico", "AAA")),  %% Deleted
    ?_assertMatch({succeed,{1, _}},librarink_mnesiaDB_fun:all_pending_reservations()),
    ?_assertMatch({succeed,{3, _}},librarink_mnesiaDB_fun:all_ended_reservations()),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_reservation("matteo", "AAA")),
    ?_assertMatch({succeed,{2, _}},librarink_mnesiaDB_fun:all_pending_reservations()),

    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_reservation("giovanni", "EEE")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_reservation("daniele", "EEE")),
    ?_assertMatch({succeed,{4, _}},librarink_mnesiaDB_fun:all_pending_reservations()),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:cancel_reservation_by_book_and_user("giovanni", "EEE")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:cancel_reservation_by_book_and_user("daniele", "EEE")),
    ?_assertMatch({succeed,{5, _}},librarink_mnesiaDB_fun:all_ended_reservations()),
    ?_assertEqual({succeed,ok},librarink_mnesiaDB_fun:delete_books_reservation_by_user("giovanni")),
    ?_assertEqual({succeed,ok},librarink_mnesiaDB_fun:delete_books_reservation_by_user("daniele")),
    ?_assertMatch({succeed,{3, _}},librarink_mnesiaDB_fun:all_ended_reservations()),

    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_reservation("giovanni", "EEE")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:add_book_reservation("daniele", "EEE")),
    ?_assertMatch({succeed,{4,_}},librarink_mnesiaDB_fun:all_pending_reservations()),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:cancel_reservation_by_book_and_user("giovanni", "EEE")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:cancel_reservation_by_book_and_user("daniele", "EEE")),
    ?_assertMatch({succeed,{5,_}},librarink_mnesiaDB_fun:all_ended_reservations()),
    ?_assertEqual({succeed,ok},librarink_mnesiaDB_fun:delete_book_reservations_by_book("EEE")),
    ?_assertMatch({succeed,{3,_}},librarink_mnesiaDB_fun:all_ended_reservations()),

    ?_assertMatch({succeed,{3,_}},librarink_mnesiaDB_fun:get_and_delete_ended_reservations()),
    ?_assertMatch({succeed,{0,_}},librarink_mnesiaDB_fun:all_ended_reservations())
  ]},
  {"Loan",[
    ?_assertMatch({succeed,{2,_}},librarink_mnesiaDB_fun:all_pending_loans()),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:terminate_loan_by_book("BBB", "ID-001")),
    ?_assertMatch({succeed,{1,_}},librarink_mnesiaDB_fun:all_pending_loans()),
    ?_assertMatch({succeed,{1,_}},librarink_mnesiaDB_fun:all_ended_loans()),
    ?_assertMatch({succeed,ok},librarink_mnesiaDB_fun:delete_lent_book("matteo","BBB","ID-001")),
    ?_assertMatch({succeed,ok},librarink_mnesiaDB_fun:delete_lent_book("matteo","BBB","ID-002")), %% Ok,not exists
    ?_assertMatch({error,error_pending_loan},librarink_mnesiaDB_fun:delete_lent_book("federico","CCC","ID-001")),
    ?_assertMatch({error,error_pending_loan},librarink_mnesiaDB_fun:delete_lent_by_book("CCC")),
    ?_assertMatch({error,error_pending_loan},librarink_mnesiaDB_fun:delete_lent_book_by_user("federico")),
    ?_assertMatch({succeed,{0, _}},librarink_mnesiaDB_fun:all_ended_loans()),
    ?_assertMatch({succeed,ok},librarink_mnesiaDB_fun:renew_loan_by_book_copy("CCC", "ID-001")),
    ?_assertEqual({succeed,ok}, librarink_mnesiaDB_fun:terminate_loan_by_book("CCC", "ID-001")),
    ?_assertMatch({succeed,{2, _}}, librarink_mnesiaDB_fun:get_and_delete_ended_loans())
  ]}
]}.

-endif.