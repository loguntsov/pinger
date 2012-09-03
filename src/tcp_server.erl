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
socket_loop(Pid, Socket) ->
	inet:setopts(Socket,[{active,once}]),
	receive
		{tcp,Socket,<<"get ", Section/binary>> } ->
			gen_tcp:send(Socket, gen_server:call(Pid, {get, Section})),
			socket_loop(Pid, Socket);
		{ tcp, Socket, <<"\r\n">> } ->
			socket_loop(Pid, Socket);
		{tcp, Socket, Any } ->
			io:format("~p", [Any]),
			ok = gen_tcp:send(Socket, <<"Command not recognise\r\n">>),
			socket_loop(Pid, Socket);
		{tcp_closed, Socket } -> ok
	end
.

handle_call({get, Section}, _From, State) ->
	try dict:fetch(Section, State#server_state.sups) of
		X -> { reply, jsx:encode(X), State }
	catch
		error:badarg -> {reply, <<"Section not found\r\n">>, State }
	end
.

terminate(_, _State) -> ok.

handle_cast({ accept, _Socket } , State) -> {noreply, State};

handle_cast( _ , State) ->
	{noreply, State}.

handle_info( _ , State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) -> { ok, State }.


