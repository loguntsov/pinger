#!/bin/sh
erl -sname vktema_redirect -pa ebin -pa deps/*/ebin -s vktema_redirect \
	-eval "io:format(\"~nVKTema redirect by UID~n\")."

