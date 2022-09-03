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

add_test_() ->
  [
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("abc", "ID-001")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("abc", "ID-002")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("abc", "ID-003")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("abc", "ID-004")),
    ?_assertEqual(ok, librarink_mnesiaDB:add_book_copy("zzz", "ID-004"))
  ].

delete_test_() ->
  [
    ?_assertEqual(ok, librarink_mnesiaDB:delete_book_copy("abc", "ID-003"))
  ].

query_test_() ->
  [
    [?_assertMatch({3,[{"abc", "ID-001"},{"abc", "ID-002"},{"abc", "ID-004"}]},
      librarink_mnesiaDB:copies_by_book("abc")),
    ?_assertNotMatch({1,_},librarink_mnesiaDB:copies_by_book("abc"))],
    ?_assertMatch({0,_},librarink_mnesiaDB:copies_by_book("xyz")),
    ?_assertMatch({1,_},librarink_mnesiaDB:copies_by_book("zzz"))
  ].


-endif.