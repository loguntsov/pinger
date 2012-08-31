-module(http_pinger_sup).

-include("include/http_pinger.hrl").

-behavior(gen_supervisor).

-spec init(Urls_list :: [#pinger]) -> [{Id, StartFunc, Restart, Shutdown, Type, Modules}].
init(Urls_list) ->
	lists:map(fun(Item) ->
		{"http_pinger"++Item#pinger.url, {http_pinger, init, [Item]},
	end, Urls_list)

ok.




