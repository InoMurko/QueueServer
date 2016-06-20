-module(qserver_connection_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_child/1
        ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ProtocolProcessor = qserver_protocol_proc,
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags,
          [{connection_id, {ProtocolProcessor, start_link, []}, temporary, 1000, worker, [ProtocolProcessor]} 
          ]}
    }.

start_child(Socket) ->
    supervisor:start_child(?MODULE, [Socket]).
    