-module(http_pinger).

-export([start_link/1, stop/1, stat/1, simple_start_link/3, id/1]).

-include("include/http_pinger.hrl").

-behaviour(gen_server).

-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-define(SERVER, ?MODULE).

simple_start_link(Url, Timeout, Period) ->
	start_link(#pinger{
		url = Url,
		timeout = Timeout,
		period = Period
	})
.

-spec start_link(State :: pinger ) -> { ok , pid()} | { error , timeout_large_that_period }.
start_link(State) ->
	if
		State#pinger.timeout > State#pinger.period -> { error, timeout_large_that_period };
		(State#pinger.timeout == infinity) and (State#pinger.period /= infinity) -> { error, timeout_large_that_period };
		true -> gen_server:start_link(?SERVER, State, [])
	end
.

init( State ) ->
	inets:start(),
	ping(State#pinger.period),
	{ok, { State, none, stats_new() }}
.

stop(Pid) ->
	gen_server:cast(Pid, stop)
.

-spec stat(Pid :: pid()) -> stats.
stat(Pid) ->
	gen_server:call(Pid, stat)
.

handle_info( ping, { State, Ref, Stats}) ->
	cancel_httpc_request(Ref),
	{ok, RequestId} = httpc:request(get, { State#pinger.url, []}, [{timeout, State#pinger.timeout }, {autoredirect, false}], [{sync, false}, {body_format, binary}]),
	NewStats = Stats#stats{ count_request = Stats#stats.count_request + 1 },
	ping(State#pinger.period),
	{noreply, { State, { os:timestamp(), RequestId } , NewStats }, State#pinger.timeout }
;

handle_info(timeout, { State, Ref, Stats }) ->
	cancel_httpc_request(Ref),
	{noreply, { State, none, Stats#stats{ timeout = Stats#stats.timeout + 1 } }}
;

handle_info({http , { ReqId0, Result }}, { State, Ref, Stats }) -> % Присылает результат httpc
			{ Time, ReqId } = excludeRef(Ref),
			if
				ReqId0 == ReqId ->
					case Result of
						{{ _ , Http_status, _ } , _ , _ } ->
							{ noreply, { State, none, time_statistic( Time, Stats#stats{
								status_counter = dict:update_counter( Http_status, 1, Stats#stats.status_counter )
							})}};
						{error, timeout} -> % Пришел таймаут от httpc
							timeout(),
							{ noreply, {State, none, Stats }};
						{error, { failed_connect, _ }} ->
							{ noreply, { State, none, Stats#stats{ connect_timeout = Stats#stats.connect_timeout + 1 }}}
					end;
				true -> timeout(),
					{noreply, { State, none, Stats }}
			end
;

handle_info( _ , State ) ->
	{noreply, State}
.

handle_call(stat, _From, { State, Ref, Stats }) -> % отправить статистику запрашиваемому потоку
			{ reply, json_prepare_stats(Stats), { State, Ref, Stats }}
.

handle_cast(stop, State) -> % останавливаем поток
		{ stop, normal, State }
.


terminate (_Reason, _State) -> ok.

timeout() ->
	self() ! timeout
.

code_change(_OldVsn, State, _Extra) -> { ok, State }.



-spec excludeRef(Ref :: { _ , _ }) -> { _Time, _ReqId }.
excludeRef(Ref) ->
	case Ref of
		none -> { none, none};
		{ _, _} = X -> X
	end
.

-spec ping(Period :: integer()) -> ok.
ping(Period) ->
	erlang:send_after(Period, self(), ping),
	ok
.

cancel_httpc_request(none) -> ok;

cancel_httpc_request({ _ , RequestId}) ->
	httpc:cancel_request(RequestId)
.

-spec time_statistic(Time :: timestamp, Stats :: stats) -> stats.
time_statistic(Time, Stats) ->
	Delta = timer:now_diff(os:timestamp(), Time),
	Stats#stats{
		response_time_min = time_min(Delta, Stats#stats.response_time_min),
		response_time_max = max(Delta, Stats#stats.response_time_max),
		response_time_sum = Stats#stats.response_time_sum + Delta
	}
.

stats_new() ->
	#stats{
		status_counter = dict:new(),
		response_time_min = 0,
		response_time_max = 0,
		response_time_sum = 0,
		timeout = 0,
		connect_timeout = 0,
		count_request = 0
	}
.
-spec id(Pinger :: pinger) -> {atom(), binary()}.
id(Pinger) -> {http_pinger, erlang:md5(term_to_binary(Pinger#pinger{id = undefined}))}.

time_min( X, 0) -> X;

time_min( 0, Y) -> Y;

time_min( X, Y ) ->
	min(X,Y)
.

-spec json_prepare_stats(Stats :: stats ) -> { _JSON_PREPARE }.
json_prepare_stats(Stats) ->
	{
		{ status_counter, dict:to_list(Stats#stats.status_counter)},
		{ response_time_min, Stats#stats.response_time_min },
		{ response_time_max, Stats#stats.response_time_max },
		{ response_time_sum, Stats#stats.response_time_sum },
		{ timeout, Stats#stats.timeout},
		{ connect_timeout, Stats#stats.connect_timeout },
		{ count_request , Stats#stats.count_request }
	}
.
