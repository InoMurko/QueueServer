%%%-------------------------------------------------------------------
%%% @doc This process exposes the API for the application. All
%%% external calls should go through this module.
%%% @end
%%%-------------------------------------------------------------------
-module(qlib_api).

-export([create_tables/0,
         delete_tables/0,
         add/2, get/1, destroy/1
        ]).

-include_lib("qlib/include/qlib.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates the necessary Mnesia tables.
%% @end
-spec create_tables() -> 'ok'.
create_tables() ->
    qlib_db:create_tables().

-spec delete_tables() -> 'ok'.
delete_tables() ->
    qlib_db:delete_tables().

%% @doc Adds a new entry to the FIFO queue.
%% This operation is O(logn) at the moment.
%% @end
-spec add(string(), binary()) -> 'ok'.
add(Queue, Msg) ->
	ID = Queue,
    qlib_db:add(?QLIB_QUEUE_TABLE, ID, Queue, Msg).

%% @doc Gets a entry from the FIFO queue.
%% @end
-spec get(string()) -> '$end_of_table' | binary().
get(Owner) ->
	qlib_db:first_select(?QLIB_QUEUE_TABLE, Owner).

%% @doc Remove all queue entries.
%% @end
-spec destroy(string()) -> ok.
destroy(Owner) ->
	qlib_db:remove_all(?QLIB_QUEUE_TABLE, Owner).
