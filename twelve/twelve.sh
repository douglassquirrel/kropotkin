#!/bin/bash

set -e
source env.sh

TARGET=$1
if [[ $# -ge 2 && $2 = "stop" ]]; then
    echo "Stopping $TARGET"
    pkill -9 -g `pgrep -of "twelve.sh $TARGET"`
    exit 0
fi

echo "Starting $TARGET"
if [ $TARGET = "hailstone" ]; then
    ./deploy.sh "../examples/hailstone/hailstone" $HAILSTONE_BUILD_PORT
elif [ $TARGET = "test-harness" ]; then
    ./deploy.sh "../core/test-harness" $TEST_HARNESS_BUILD_PORT
elif [ $TARGET = "catalog" ]; then
    ./deploy.sh "../publisher/catalog" $CATALOG_BUILD_PORT
elif [ $TARGET = "secretary-build" ]; then
    ./deploy.sh "../publisher/secretary" $SECRETARY_BUILD_PORT
elif [ $TARGET = "secretary-run" ]; then
    ./deploy.sh "http://localhost:8001/secretary.tar"
elif [ $TARGET = "tycoon-build" ]; then
    ./deploy.sh "../publisher/tycoon" $TYCOON_BUILD_PORT
elif [ $TARGET = "tycoon-run" ]; then
    ./deploy.sh "http://localhost:8002/tycoon.tar"
elif [ $TARGET = "courier" ]; then
    ./deploy.sh "../courier"
else
    echo "Unknown service"
    exit 1
fi