-module(qserver_protocol_proc).
-include_lib("qlib/include/qlib.hrl").

-behaviour(gen_server).

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {socket, queue, break}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 										Info
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This module receives a TCP connection.

%% API.

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) -> 
	inet:setopts(Socket, [{active, once}]),
	{ok, Break} = application:get_env(qserver, break),
	{ok, #state{break = Break, socket = Socket, queue = binary_to_list(base64:encode(crypto:rand_bytes(16)))}}.

%% @doc receives all connections and data and forwards to handle_call
%% @end where we can pattern match if we want.
handle_info({tcp, Socket, Data}, State = #state{ socket = Socket}) ->
	inet:setopts(Socket, [{active, once}]),
	handle_call(Data, Socket, State);

handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
	{stop, Reason, State};
handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{stop, normal, State}.

%% Send a message through a socket, then make it active again.
%% The difference between an active and a passive socket is that
%% an active socket will send incoming data as Erlang messages, while
%% passive sockets will require to be polled with gen_tcp:recv/2-3.
handle_call(Message, Socket, State = #state{socket = Socket, queue = _Queue}) ->
	ok = split_and_process(Message, State),
    {noreply, State};

handle_call({send, Data}, _From, State) ->
    ok = gen_tcp:send(State#state.socket, Data),
    {noreply, State};

handle_call(stop, _, P) ->
    {stop, shutdown, stopped, P}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	qlib_api:destroy(State#state.queue),
	gen_tcp:close(State#state.socket),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc We receive messages and start splitting it line by line
%% and responding to commands
%% @end
-spec split_and_process(string(), #state{}) -> 'ok'.
split_and_process(Message, State) ->
	split(string:tokens(Message, State#state.break), State).

-spec split(list(string()), #state{}) -> 'ok'.
split(["out" | Tail], State=#state{socket=Socket, queue=Queue, break = Break}) ->
	case qlib_api:get(Queue) of
		?END_OF_TABLE ->
			ok = gen_tcp:send(Socket, []),
			inet:setopts(Socket, [{active, once}]);
		Item ->
			ok = gen_tcp:send(Socket, Item ++ Break),
			inet:setopts(Socket, [{active, once}])
	end,
	split(string:tokens(Tail, Break), State);
split(["in" ++ Payload | Tail], State=#state{queue = Queue, break = Break}) ->
	ok = qlib_api:add(Queue, Payload),
	split(string:tokens(Tail, Break), State);
split([], _State) ->
	ok.

