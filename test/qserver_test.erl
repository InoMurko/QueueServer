-module(qserver_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("qlib/include/qlib.hrl").

publish_test_() ->
	{foreach,
	fun setup/0,
	fun cleanup/1,
		[
		{"Connect and send commands and analyze response.", fun publish_and_ret/0 },
		{"Fifo test", fun fifo/0}
		]

	}.

setup() ->
	ok = application:ensure_started(mnesia),
	ok = application:ensure_started(qlib),
	ok = application:ensure_started(qserver),
	ok.

cleanup(_) ->
	application:stop(mnesia),
	mnesia:delete_schema([node()]),
	application:stop(qlib),
	application:stop(qserver),
	ok.

publish_and_ret() ->
	SomeHostInNet = "localhost", 
    {ok, Socket} = gen_tcp:connect(SomeHostInNet, 8081, [list, {active, once}]),
    Data = "in" ++ "TEST1" ++ "\r\n" ++ "in" ++ "TEST9" ++ "\r\n" ++ "out" ++ "\r\n" ++ "in" ++"TEST2" ++ "\r\n" ++ "out" ++ "\r\n" ++ "out" ++ "\r\n",
    ok = gen_tcp:send(Socket, Data),
    inet:setopts(Socket, [{active, false}, {packet, line}]),
    response(Socket),
	ok = gen_tcp:close(Socket),
    timer:sleep(1000),%stopping the application to soon makes ranch gen_server crash because it doesn't have enough time to destroy his queue
    ?assertEqual([], ets:tab2list(qlib_queue)).

response(Socket) ->
    response(Socket, 0).
response(Socket, Index) ->
    case {gen_tcp:recv(Socket, 0), Index} of
        {{ok, "TEST1" ++ _NL = Data}, 0} ->
        	?assertEqual("TEST1\r\n", Data),
            response(Socket, 1);
        {{ok, "TEST9" ++ _NL = Data}, 1} ->
        	?assertEqual("TEST9\r\n", Data),
            response(Socket, 2);
        {{ok, "TEST2" ++ _NL = Data}, 2} ->
        	?assertEqual("TEST2\r\n", Data);
        _Reason -> 
        	?assertEqual(0, _Reason)
    end.

fifo() ->
	ok = qlib_api:add("OneAndOnly" , <<"Message1">>),
	ok = qlib_api:destroy("OneAndOnly"),
	ok = qlib_api:add("OneAndOnly" , <<"Message">>),
	ok = qlib_api:add("JustOne" , <<"Message">>),
	ok = qlib_api:add("owner1" , <<"Message">>),
	ok = qlib_api:add("owner2", <<"TestMessage">>),
	ok = qlib_api:add("owner2", <<"TestMessage22221">>),
	ok = qlib_api:add("owner1" , <<"Message">>),
	ok = qlib_api:add("owner2", <<"TestMessage22222">>),
	ok = qlib_api:add("owner1" , <<"Message">>),
	ok = qlib_api:add("OneAndOnly" , <<"Message2">>),
	ok = qlib_api:add("owner2", <<"TestMessage22223">>),
	?assertEqual(<<"Message">>, qlib_api:get("owner1")),
	?assertEqual(<<"TestMessage">>, qlib_api:get("owner2")),
	?assertEqual(<<"TestMessage22221">>, qlib_api:get("owner2")),
	?assertEqual(<<"TestMessage22222">>, qlib_api:get("owner2")),
	?assertEqual(<<"TestMessage22223">>, qlib_api:get("owner2")),
	?assertEqual(<<"Message">>, qlib_api:get("owner1")),
	ok = qlib_api:add("owner2", <<"TestMessage">>),
	?assertEqual(<<"Message">>, qlib_api:get("owner1")),
	?assertEqual(<<"TestMessage">>, qlib_api:get("owner2")),
	?assertEqual('$end_of_table', qlib_api:get("owner1")),
	?assertEqual('$end_of_table', qlib_api:get("owner2")),
	ok = qlib_api:destroy("OneAndOnly"),
	ok = qlib_api:destroy("JustOne"),
	ok = qlib_api:destroy("JustNothing"),
	?assertEqual('$end_of_table', qlib_api:get("OneAndOnly")),
	?assertEqual('$end_of_table', qlib_api:get("JustOne")),
	?assertEqual([], ets:tab2list(qlib_queue)).
	