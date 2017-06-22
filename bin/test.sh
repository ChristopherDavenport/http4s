#!/usr/bin/env bash

. $TRAVIS_BUILD_DIR/bin/setup.sh

start_watchdog 13m
sbt $SBT_EXTRA_OPTS ++$TRAVIS_SCALA_VERSION "${HTTP4S_MODULE}test"
exitCode=$?
stop_watchdog
exit ${exitCode}
