-module(http_pinger_sup).

-export([start_link/1,stats/1, stop/1]).

-include("include/http_pinger.hrl").

-behavior(supervisor).
-export([init/1]).

% Количество рестартов чайлда ...
-define(RESTART_COUNT, 5).
% за время, после которого супервизор будет крошится
-define(SECONDS, 5).

-spec init(Urls_list :: [ pinger ] ) -> { ok, {one_for_all, integer(), integer()} , [ { _ } ]}.
init(Urls_list) -> { ok, {{ one_for_one, ?RESTART_COUNT, ?SECONDS },
	lists:map(fun(Item) ->
		Spec = { http_pinger:id(Item),
      {http_pinger, start_link, [Item]}, transient, 2000, worker, []},
		ok = supervisor:check_childspecs([Spec]),
		Spec
	end, Urls_list)}}.

-spec start_link(Urls :: [{url, timeout(), timeout()}]) -> {ok, pid()}.
start_link(Urls) ->
  Args = lists:map(fun(Item) ->
    A = #pinger{
      url = element(1,Item),
      timeout = element(2,Item),
      period = element(3,Item)
    },
		A#pinger{ id = http_pinger:id(A) }
  end, Urls),
  supervisor:start_link(?MODULE, Args).

-spec stats(Pid :: pid()) -> [{ term() , stats }].
stats(Pid) ->
	lists:map(fun({Id, Child, _Type, _Modules}) ->
				{Id, http_pinger:stat(Child)}
	end, supervisor:which_children(Pid))
.

-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
	lists:map(fun({Id, Child, _Type, _Modules}) ->
		{Id, supervisor:terminate_child(Pid, Child)}
	end, supervisor:which_children(Pid))
.
