-module(tcp_server).
-export([start_link/2]).
-export([socket_accept/2]).

-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(server_state, {
		sups :: dict() % список обслуживаемых супервайзеров
	}).

start_link(LSock, Sups) ->
	gen_server:start_link(?MODULE, {LSock, Sups}, []).

init({LSock, Sups}) ->
	spawn_link(?MODULE, socket_accept, [self(), LSock]),
	{ok,#server_state{sups = Sups}}
.

socket_accept(Pid, LSock) ->
	case gen_tcp:accept(LSock) of
		{ok, Socket} -> gen_server:cast(Pid, { accept, Socket }),
			socket_loop(Pid, Socket),
			socket_accept(Pid, LSock)
	end
.
% Прием сообщений и преобразование их в сообщения для gen_server
socket_loop(Pid, Socket) ->
	inet:setopts(Socket,[{active,once}]),
	receive
		{tcp,Socket,<<"get ", Section/binary >> } ->
			send_server_answer(Socket, Pid, {get, trim(Section)}),
			socket_loop(Pid, Socket);
		{ tcp, Socket, <<"\r\n">> } ->
			socket_loop(Pid, Socket);
		{tcp, Socket, _Any } ->
			ok = gen_tcp:send(Socket, <<"Command not recognise\r\n">>),
			socket_loop(Pid, Socket);
		{tcp_closed, Socket } -> ok
	end
.

send_server_answer(Socket, Pid, Msg) ->
			Ans = gen_server:call(Pid, Msg),
			gen_tcp:send(Socket, <<Ans, "\r\n">>)
.

%split(Binary) ->
%	trim(binary:split(Binary,<<" ">>)).
%
%trim([], Acc ) -> Acc;
%
%trim([X | List], Acc ) ->
% trim(List, [trim(X) | Acc]).

trim(Binary) ->
	binary:replace( binary:replace( binary:replace(
		Binary,<<"\r">>,<<"">>),<<"\n">>,<<"">>),<<" ">>,<<"">>)
.


handle_call({get, Section}, _From, State) ->
	try dict:fetch(Section, State#server_state.sups) of
		X -> { reply, jsx:encode(X), State }
	catch
		error:badarg -> {reply, <<"Section not found">>, State }
	end
.

terminate(_, _State) -> ok.

handle_cast({ accept, _Socket } , State) -> {noreply, State};

handle_cast( _ , State) ->
	{noreply, State}.

handle_info( _ , State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) -> { ok, State }.


