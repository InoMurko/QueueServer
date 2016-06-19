%%%-------------------------------------------------------------------
%% @doc Qserver top level supervisor that starts Ranch.
%% Each incoming connection will be handled by qserver_protocol_proc 
%% module.
%% @end
%%%-------------------------------------------------------------------

-module(qserver_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	{ok, RanchPort} = application:get_env(qserver, ranch_port),
	application:start(ranch),
	ranch:start_listener(qserver, 100,
      ranch_tcp, [{port, RanchPort}, {max_connections, 10000}],
      qserver_protocol_proc, []),
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
