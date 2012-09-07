-module(pinger_app_sup).
-export([start_link/1]).

-behavour(supervisor).
-export([init/1]).

% Количество рестартов чайлда ...
-define(RESTART_COUNT, 5).
% за время, после которого супервизор будет крошится
-define(SECONDS, 5).

init(Options) ->
	error_logger:info_report({ options, Options}),
	{ ok, {{ one_for_one, ?RESTART_COUNT, ?SECONDS },
		lists:map(fun(Section) ->
			error_logger:info_report({ section, Section}),
			{ {http_pinger_sup, element(1,Section)} , {http_pinger_sup, start_link, [element(2,Section)]}, transient, 2000, supervisor, []}
		end, Options)
	}}.

start_link(Options) ->
	error_logger:info_report("Start app"),
	{ok, Pid} = supervisor:start_link({local, pinger_app}, ?MODULE, Options),
	error_logger:info_report({app_pid, Pid}),
	Pids = dict:from_list(lists:map(fun(Spec) ->
		{ element(1,Spec), element(2,Spec) }
	end,which_http_pinger_sup(Pid))),
	{ok, _Pid_tcp_server } = supervisor:start_child(Pid, { { tcp_sever_sup }, {tcp_server_sup, start_link, [1, Pids] }, permanent, 2000, supervisor, []}),
	{ok, Pid}
.

which_http_pinger_sup(Pid) ->
	lists:filter(fun(Spec) ->
		case element(1,Spec) of
			{ http_pinger_sup, _ } -> true;
			_ -> false
		end
	end,supervisor:which_children(Pid))
.
