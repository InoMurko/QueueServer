-module(qserver_listener).

-behaviour(gen_server).

-export([start_link/1,
         listen/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-export([accept_and_forward/1]).

-define(SERVER, ?MODULE). 

-record(state, {
    socket :: inets:socket()
}).


start_link(Port) ->
    {ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []),
    {ok, Pid}.

init([Port]) ->
    listen(Port),
    {ok, #state{}}.

listen(Port) ->
    ok = gen_server:cast(?MODULE, {start, Port}).

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({start, Port}, State) ->
    ListenOpts = [{packet, line}, {active, false}, {reuseaddr, true}],
    ListenOpts2 = lists:append(ListenOpts,[{ip, {0,0,0,0}}]),
    %listening on port 
    {ok, LSocket} = gen_tcp:listen(Port, ListenOpts2),
    spawn_link(?MODULE, accept_and_forward, [LSocket]),
    {noreply, State#state{socket = LSocket}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg,State) ->
    {noreply,State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------

accept_and_forward(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            case qserver_connection_sup:start_child(Socket) of
                {ok, ConnCtrlPID} ->
                    gen_tcp:controlling_process(Socket, ConnCtrlPID);
                _Error ->
                    ok
            end,
            accept_and_forward(ListenSocket);
        _Error ->
            accept_and_forward(ListenSocket)
    end.

