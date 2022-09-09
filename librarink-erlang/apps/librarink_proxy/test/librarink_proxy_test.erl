%%%-------------------------------------------------------------------
%%% @hidden
%%% Created : 06. set 2022 18:17
%%%-------------------------------------------------------------------
-module(librarink_proxy_test).
-export([gen_test/1, setup_env/0, stop_all/1]).
-include_lib("eunit/include/eunit.hrl").

request_test_() ->
    {
        setup,
        fun setup_env/0,
        fun stop_all/1,
        fun gen_test/1
    }.

gen_test(_) ->
    [request_isbn(), request_multi_simple(), request_multi(), request_notification(), request_error_simple(),
        request_timeout()].

request_isbn() ->
    Response = gen_server:call({proxy, node()}, {copies_by_book, #{isbn => "000-000-000-000-0"}}, 5000),
    [{"Request", ?_assertEqual({succeed, response}, Response)}].

request_multi() ->
    Response = gen_server:call({proxy, node()}, {read_copies, #{type => all}}, 5000),
    [{"Request", ?_assertEqual({succeed, {1, [response]}}, Response)}].

request_multi_simple() ->
    Response = gen_server:call({proxy, node()}, {delete_lent_book_by_user, #{user => "federico"}}, 5000),
    [{"Request", ?_assertEqual({succeed, ok}, Response)}].


request_error_simple() ->
    Response = gen_server:call({proxy, node()}, {error, #{isbn => "000-000-000-000-0"}}, 5000),
    [{"Request", ?_assertEqual({error, err}, Response)}].

request_timeout() ->
    Response = gen_server:call({proxy, node()}, {timeout, #{isbn => "000-000-000-000-0"}}, 30000),
    [{timeout, 30,?_assertEqual({error, timeout_exceeded}, Response)}].

request_notification() ->
    Response = gen_server:call({proxy, node()}, {write_copy, #{isbn => "000-000-000-000-0", id => "ID-001"}}, 5000),
    [
        {"Request", ?_assertEqual({succeed, response}, Response)},
        consume_mqs()
    ].

consume_mqs() ->
    receive
        Msg -> {"MQS_Consume", ?_assertEqual(<<"{\"add\":1,\"isbn\":\"000-000-000-000-0\"}">>, Msg)}
    end.

setup_env() ->
    application:set_env(librarink_proxy, proxy_name, proxy),
    application:set_env(librarink_proxy, mqs_host, "192.168.1.109"),
    application:set_env(librarink_proxy, exchange_type, <<"fanout">>),
    application:set_env(librarink_proxy, routing_key, <<"">>),
    application:set_env(librarink_proxy, request_timeout, 10000),
    application:set_env(librarink_proxy, mnesia_name, database),
    application:set_env(librarink_proxy, mnesia_nodes, [{'librarink@AMD-FX4300','librarinkBackup@AMD-FX4300'}]),
    application:start(librarink_proxy),

    database_mockup(),

    gen_server:start({local, database}, database, [], []),
    mqs_connection().

database_mockup() ->
    meck:new(database, [non_strict]),
    meck:expect(database, init, fun([]) -> {ok, {}} end),
    meck:expect(database, handle_call,
        fun(Request,From, State) ->
           case Request of
               {timeout, _} -> ok;
               {error, _} ->spawn(fun() -> gen_server:reply(From, {error, err}) end);
               {_,#{isbn := _Isbn}} -> spawn(fun() -> gen_server:reply(From, {succeed, response}) end);
               {delete_lent_book_by_user,_} -> spawn(fun() -> gen_server:reply(From, {succeed, ok}) end);
               _ -> spawn(fun() -> gen_server:reply(From, {succeed, {1, [response]}}) end)
           end,

            {noreply, State}
        end),
    meck:expect(database, handle_info, fun(_Info, State) ->  {noreply, State} end),
    meck:expect(database, handle_cast, fun(_Request, State) ->  {noreply, State} end),
    meck:expect(database, terminate, fun(_Reason, _State) -> ok end),
    meck:validate(database).

mqs_connection() ->
    {ok, Connection, Channel} = librarink_mqs_amqp:start_connection("192.168.1.109"),
    librarink_mqs_amqp:declare_queue(Channel, <<"test">>),
    librarink_mqs_amqp:bind_queue(Channel, <<"test">>, <<"000-000-000-000-0">>, <<"fanout">>, <<"">>),
    Self = self(),
    {ok, Pid, _} = librarink_mqs_amqp:start_consumer({Connection, Channel}, [<<"test">>], fun(Msg) -> Self ! Msg end),
    Pid.

stop_all(Pid) ->
    exit(Pid, shutdown),
    application:stop(librarink_proxy),
    gen_server:stop(database),
    meck:unload(database).