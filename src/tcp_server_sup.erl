-module(tcp_server_sup).
-export([start_link/2]).

-behavior(supervisor).
-export([init/1]).

-define(PORT, 11212).

% Количество рестартов чайлда ...
-define(RESTART_COUNT, 5).
% за время, после которого супервизор будет крошится
-define(SECONDS, 5).


start_link(Num, Supervisor_dict) ->
	supervisor:start_link(?MODULE, {Num, Supervisor_dict}).

init({Num, Sups_dict}) ->
		error_logger:info_report("Start tcp-server"),
		error_logger:info_report({ pids, Sups_dict}),
    {ok, ListenSock} = gen_tcp:listen(?PORT,[binary, {active, false},{packet,line}]),
		{ok,{{ one_for_one, ?RESTART_COUNT, ?SECONDS },
				lists:map(
					fun(Item) ->
						{ Item,  { tcp_server, start_link, [ListenSock, Sups_dict] }, permanent, 1000, worker, [] }
					end,
				lists:seq(1, Num))}}
.
