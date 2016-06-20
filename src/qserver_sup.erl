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
init(_) ->
	RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	{ok, Port} = application:get_env(qserver, port),
	Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    ConnectionSupervisor = qserver_connection_sup,
    ConnectionSupChild = {ConnectionSupervisor, {ConnectionSupervisor, start_link, []}, Restart, Shutdown, Type, [ConnectionSupervisor]},
    ConnectionListener = qserver_listener,
    ConnectionListenerChild = {ConnectionListener, {ConnectionListener, start_link, [Port]}, Restart, Shutdown, Type, [ConnectionListener]},

    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,


	{ok, { SupFlags, [ConnectionListenerChild, ConnectionSupChild]} }.

%%====================================================================
%% Internal functions
%%====================================================================
