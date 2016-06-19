%%%-------------------------------------------------------------------
%%% @doc
%%% Create schema on the same node, create tables and start the top level
%%% supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(qlib_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
	mnesia:create_schema([node()]),
	qlib_api:create_tables(),
    qlib_sup:start_link().

stop(_State) ->
    ok.
