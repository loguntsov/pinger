#!/bin/sh
erl -sname pinger_app -pa ebin -pa deps/*/ebin -s pinger_app \
	-eval "io:format(\"~nVKTema redirect by UID~n\")."

