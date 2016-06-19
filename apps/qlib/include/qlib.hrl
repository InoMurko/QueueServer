-define(QLIB_QUEUE_TABLE, qlib_queue).
-define(END_OF_TABLE, '$end_of_table').

-ifdef(QLIB_DBG).

-define(dbg(F), io:format("~s:~w: ~s\n", [?MODULE, ?LINE, F])).
-define(dbg(F,A), io:format("~s:~w: " ++ F ++ "\n", [?MODULE, ?LINE] ++ A)).

-else.

-define(dbg(X), ok).
-define(dbg(X,Y), ok).

-endif.

-record(qlib_request,{
	key :: {erlang:timestamp(), Queue :: string()},
	message :: binary(),
	queueName :: string()
	}).