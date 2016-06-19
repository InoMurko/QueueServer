%%%-------------------------------------------------------------------
%%% @doc This module implements the DB access functions for the whole
%%% qlib. The tables are defined in qlib.hrl, and
%%% are created during startup by this application.
%%% Due to the implementation using serialization, none of this operations
%%% need to be done through transactions to guarantee consistency.
%%% @end
%%%-------------------------------------------------------------------
-module(qlib_db).

-export([create_tables/0,
         table_definitions/0,
         delete_tables/0,
         add/4,
         first/1,
         first_select/2,
         delete/2,
         remove_all/2,
         remove_all_transaction/2,
         delete_chk/2
        ]).

-include_lib("qlib/include/qlib.hrl").

%% TODO Add comment about the logaritmic perfomance of using Mnesia
%% ordered_sets

%%%===================================================================
%%% API
%%%===================================================================
-spec table_definitions() -> list({atom(), atom(), list({'type', ets:type()})}).
table_definitions() ->
    [{?QLIB_QUEUE_TABLE, qlib_request, [{type, ordered_set}]}].

-spec create_tables() -> 'ok'.
create_tables() ->
    [{atomic, ok} = create_table(Def) || Def <- table_definitions()],
    ok.

-spec delete_tables() -> 'ok'.
delete_tables() ->
    [mnesia:delete_table(Name) || {Name, _Rec, _Opts} <- table_definitions()],
    ok.

%% @doc Creates and adds an entry to one of the Mnesia table. It is added
%% at the end of the ordered_set.
%% @end
-spec add(mnesia:tab(), string(), string(), binary()) -> 'ok'.
add(Table, ID, Queue, Msg) ->
    Key = {os:timestamp(), ID},
    QReq = #qlib_request{key = Key,
                        message = Msg,
                        queueName = Queue},
    add_record(Table, QReq).

-spec add_record(mnesia:tab(), #qlib_request{}) -> 'ok'.
add_record(Table, QRec) ->
    ok = mnesia:dirty_write(Table, QRec).

%% @doc There's the strong assumption that only one and the same process
%% is using this function, and that writes only happen at the end of
%% the queue! This would return the first insert, regardles of the queue
%% name.
%% @end
-spec first(mnesia:tab()) -> '$end_of_table' | #qlib_request{}.
first(Table) ->
    case mnesia:dirty_first(Table) of
        ?END_OF_TABLE ->
            ?END_OF_TABLE;
        Key ->
            [QRec] = mnesia:dirty_read(Table, Key),
            QRec
    end.

%% @doc There's the strong assumption that only one and the same process
%% is using this function, and that writes only happen at the end of
%% the queue! This would return the first insert that matches the queue
%% name.
%% @end
-spec first_select(mnesia:tab(), string()) -> '$end_of_table' | binary().
first_select(Table, Queue) ->
    F = fun() -> mnesia:select(Table,[{{'$1','$2','$3','$4'},
     [{'=:=','$4',{const, Queue}}],
     [{{'$2','$3'}}]}], 1, read) end,
    case mnesia:transaction(F) of
        {atomic, ?END_OF_TABLE} ->
            ?END_OF_TABLE;
        {atomic, {[QRec],_}} -> 
            %return just message
            {Key, Message} = QRec,
            delete_chk_q(Table, Key),
            Message
    end.

%% @doc Checks whether the key provided is the first, then deletes it.
-spec delete_chk(mnesia:tab(), {erlang:timestamp(), string()}) -> 'ok' | no_return().
delete_chk(Table, Key) ->
    case mnesia:dirty_first(Table) of
        Key ->
            mnesia:dirty_delete(Table, Key);
        First ->
            erlang:error({bad_delete, Table, Key, First})
    end.


%% @doc Checks whether the key provided is the first, then deletes it.
-spec remove_all(mnesia:tab(), string()) -> 'ok' | no_return().
remove_all(Table, Queue) ->
    case mnesia:dirty_select(Table, [{{'$1','$2','$3','$4'},
     [{'=:=','$4',{const, Queue}}],
     [{{'$2'}}]}]) of
        Keys = [_|_] ->
            [delete_chk_q(Table,Key)|| {Key} <- Keys],
            ok;
        [] -> ok;
        First ->
            erlang:error({bad_delete, Table, First})
    end.


%% @doc Checks whether the key provided is the first, then deletes it.
-spec remove_all_transaction(mnesia:tab(), string()) -> 'ok' | no_return().
remove_all_transaction(Table, Queue) ->
    F = fun() -> 
    case mnesia:select(Table,[{{'$1','$2','$3','$4'},
     [{'=:=','$4',{const, Queue}}],
     [{{'$2'}}]}]) of
        Keys = [_|_] ->
            [mnesia:delete({Table,Key})|| {Key} <- Keys],
            ok;
        [] -> ok;
        First ->
            erlang:error({bad_delete, Table, First})

        end
end,
_ = mnesia:transaction(F),
ok.


%% @doc Deletes an entry.
-spec delete_chk_q(mnesia:tab(), {erlang:timestamp(), string()}) -> 'ok' | no_return().
delete_chk_q(Table, Key) ->
    mnesia:dirty_delete({Table, Key}).

% -spec delete_chk_q(mnesia:tab(), {erlang:timestamp(), string()}) -> 'ok' | no_return().
% %% @doc Checks whether the key provided is the first, then deletes it.
% delete_chk_q(Table, Key) ->
%     mnesia:dirty_delete(Table, Key).

-spec delete(mnesia:first(), {erlang:timestamp(), string()}) -> 'ok' | no_return().
delete(Table, Key) ->
    mnesia:dirty_delete(Table, Key).

-spec create_table({mnesia:tab(), atom(), list()}) -> {atomic, ok} | tuple().
create_table({Table, Record, Options}) ->
    case mnesia:create_table(Table, [{record_name, Record}, {attributes, record_info(fields, qlib_request)}] ++ Options) of
        {atomic, ok} ->
            {atomic, ok};
        {aborted, E} ->
            error(E, [{Table, Options}])
    end.
