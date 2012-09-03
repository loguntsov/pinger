%% Copyright
-module(pinger_app).
-author("begemot").

-behaviour(application).

% application
-export([start/2, stop/1, start/0]).

% application callbacks
start(_Type, _Args) ->
	ok.

start() -> ok.

stop(_State) ->
	ok.
