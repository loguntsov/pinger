%% Copyright
-module(pinger_app).

-behaviour(application).

% application
-export([start/2, stop/1, start/0]).

% application callbacks
start(_Type, _Args) ->
	{ok, Options} = file:consult("ping.conf"),
	Pid = spawn_link(pinger_app_sup, start_link, Options),
	{ok, Pid}
.

start() -> start(normal, []).

stop(State) -> State.


