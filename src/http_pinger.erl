-module(http_pinger).

-export([start_link/1, stop/1, stat/1, run/1, simple_start_link/3, reset/1]).

-include("include/http_pinger.hrl").

simple_start_link(Url, Timeout, Period) ->
	start_link(#pinger{
		url = Url,
		timeout = Timeout,
		period = Period
	}).

-spec start_link(State :: pinger ) -> pid() | { error , timeout_large_that_period }.
start_link(State) ->
	if
		State#pinger.timeout > State#pinger.period -> { error, timeout_large_that_period };
		(State#pinger.timeout == infinity) and (State#pinger.period /= infinity) -> { error, timeout_large_that_period };
		true -> spawn_link(?MODULE, run, [State])
	end
.


stop(Pid) ->
	Pid ! stop,
	ok
.

-spec stat(Pid :: pid()) -> stats | { error, timeout }.
stat(Pid) ->
	Pid ! { stat , self() },
	receive
		{ http_pinger , { stat, Stats }}  -> Stats
	after 500 ->
		{ error, timeout }
	end
.

-spec reset(Pid :: pid()) -> ok.
reset(Pid) ->
	Pid ! reset,
	ok
.

run(State) ->
	inets:start(),
	if
		is_integer(State#pinger.period) ->
			{ok, TRef} = timer:send_interval(State#pinger.period, ping);
		true -> TRef = none
	end,
	loop(State, none, stats_new()),
	if
		is_integer(State#pinger.period) ->
			timer:cancel(TRef);
		true -> ok
	end,
	ok
.

loop(State, Ref , Stats) ->
	{ Time, ReqId } = case Ref of
		none -> { none, none};
		{ _, _} = X -> X
	end,
	receive
		ping ->
			timeout(Ref),
			{ok, RequestId} = httpc:request(get, { State#pinger.url, []}, [{timeout, State#pinger.timeout }, {autoredirect, false}], [{sync, false}, {body_format, binary}]),
			NewStats = Stats#stats{ count_request = Stats#stats.count_request + 1 },
			loop(State, { os:timestamp(), RequestId }, NewStats);
		{ http , { ReqId, Result } } -> % Присылает результат httpc
			case Result of
				{{ _ , Http_status, _ } , _ , _ } ->
					loop(State, none, time_statistic( Time, Stats#stats{
						status_counter = dict:update_counter( Http_status, 1, Stats#stats.status_counter )
					}));
				{error, timeout} -> % Пришел таймаут от httpc
					timeout(),
					loop(State, none, Stats);
				{error, { failed_connect, _ }} ->
					loop(State, none, Stats#stats{ connect_timeout = Stats#stats.connect_timeout + 1 })
			end;
		{ stat, Pid } -> % отправить статистику запрашиваемому потоку
			Pid ! { http_pinger, { stat , Stats } },
			loop(State, Ref, Stats);
		reset -> % сбросить статистику
			flush(),
			loop(State, none, stats_new());
		timeout -> loop(State, Ref, Stats#stats{ timeout = Stats#stats.timeout + 1 });
		stop -> ok;
		{'EXIT', Pid, Reason } -> Pid ! {exit, Reason };
		_ -> timeout(),
			loop(State, Ref, Stats)
		after State#pinger.timeout ->
			timeout(Ref),
			loop(State, none, Stats)
	end
.

flush() ->
	receive
		_ -> flush()
	after 500 -> ok
	end
.
timeout() ->
	self() ! timeout
.

timeout(none) -> ok;

timeout({ _ , RequestId}) ->
	httpc:cancel_request(RequestId),
	timeout()
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


time_min( X, 0) -> X;

time_min( 0, Y) -> Y;

time_min( X, Y ) ->
	min(X,Y)
.
