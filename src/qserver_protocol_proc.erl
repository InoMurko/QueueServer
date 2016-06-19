-module(qserver_protocol_proc).
-include_lib("qlib/include/qlib.hrl").

-behaviour(gen_server).
-behaviour(ranch_protocol).
-compile(export_all).
%% API.
-export([start_link/4]).

%% gen_server.
-export([init/1]).
-export([init/4]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {socket, transport, queue}).

-define(BREAK, <<"\r\n">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 										Info
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This module receives a TCP connection via RANCH. By pattern matching it resolves
% to a proper handle_call fun. 




%% API.

start_link(Ref, Socket, Transport, Opts) ->
	proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% @doc This function is never called. We only define it so that
%% @end we can use the -behaviour(gen_server) attribute.
init([]) -> 
	{ok, undefined}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once},{packet,line}]),

    gen_server:enter_loop(?MODULE, [],
			  #state{socket=Socket, transport=Transport, queue = binary_to_list(base64:encode(crypto:rand_bytes(16)))}).

%% @doc receives all connections and data and forwards to handle_call
%% @end where we can pattern match if we want.
handle_info({tcp, Socket, Data}, State=#state{
		socket=Socket, transport=Transport}) ->
	Transport:setopts(Socket, [{active, once}]),
	handle_call(Data, Socket, State);

handle_info({tcp_closed, _Socket}, State) ->
	
	{stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
	{stop, Reason, State};
handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{stop, normal, State}.

handle_call(Message, _Socket, State=#state{socket = _Socket, transport = _Transport, queue = _Queue}) ->
    ok = split_and_process(Message, State),
    {noreply, State};

handle_call({send, Data}, _From, State) ->
    ok = gen_tcp:send(State#state.socket, Data),
    {noreply, State};

%% @doc General purpose ping-pong call.
handle_call(<<"ping">>, _From, State=#state{socket=Socket, transport=Transport})->
    Transport:setopts(Socket, [{active, once}]),
    Transport:send(Socket, <<"pong">>),
    {noreply, State};

handle_call(stop, _, P) ->
    {stop, shutdown, stopped, P}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	qlib_api:destroy(State#state.queue),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc We receive binary message and start splitting it line by line
%% and responding to commands
%% @end
-spec split_and_process(binary(), #state{}) -> 'ok'.
split_and_process(Message, State) ->
	split(binary:split(Message, ?BREAK), State).

-spec split(list(binary()), #state{}) -> 'ok'.
split([<<"out">> | [Tail]], State=#state{socket=Socket, transport=Transport, queue=Queue}) ->
	case qlib_api:get(Queue) of
		?END_OF_TABLE ->
			Transport:setopts(Socket, [{active, once}]),
			Transport:send(Socket, []);
		Item ->
			Transport:setopts(Socket, [{active, once}]),
			Transport:send(Socket, <<Item/binary, ?BREAK/binary>>)
	end,
	split(binary:split(Tail, ?BREAK), State);
split([<<_In:16, Payload/binary >> = _Message | [Tail]], State=#state{queue = Queue}) ->
	ok = qlib_api:add(Queue, Payload),
	split(binary:split(Tail, ?BREAK), State);
split([<<>>], _State) ->
	ok.
