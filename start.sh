#!/bin/sh
cd `dirname $0`
exec erl +A 16 -pa $PWD/ebin $PWD/deps/*/ebin -config rel/files/sys.config -boot start_sasl -s reloader -s imagerl
