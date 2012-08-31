#!/bin/sh
erl -pa ebin -pa deps/*/ebin \
	-eval "io:format(\"~nVKTema redirect by UID~n\")."
