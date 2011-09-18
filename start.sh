#!/bin/sh

killall -9 gloss-web
sleep 0.1
cd `dirname $0`
nohup ./dist/build/gloss-web/gloss-web &
