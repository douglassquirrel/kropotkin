#!/bin/bash

TARGET=$1
if [[ $# -ge 2 && $2 = "stop" ]]; then
    echo "Stopping $TARGET"
    pkill -9 -g `pgrep -of "twelve.sh $TARGET"`
    exit 0
fi

export HAILSTONE_BUILD_PORT=9000\
       TEST_HARNESS_BUILD_PORT=9001\
       CATALOG_BUILD_PORT=8000\
       CATALOG_URL="http://localhost:8000/catalog.tar"\
       CATALOG_FILE=`mktemp -d`"catalog.s3"\
       SECRETARY_BUILD_PORT=8001\
       SECRETARY_INPUT_DIR="/tmp/secretary-input"\
       TYCOON_BUILD_PORT=8002\
       TYCOON_TEST_PORT=60000\
       TYCOON_PORT=9002

echo "Starting $TARGET"
if [ $TARGET = "hailstone" ]; then
    export SOURCE="../examples/hailstone/hailstone"\
           PORT=$HAILSTONE_BUILD_PORT
elif [ $TARGET = "test-harness" ]; then
    export SOURCE="../core/test-harness"\
           PORT=$TEST_HARNESS_BUILD_PORT
elif [ $TARGET = "catalog" ]; then
    export SOURCE="publisher/catalog"\
           PORT=$CATALOG_BUILD_PORT
elif [ $TARGET = "secretary-build" ]; then
    export SOURCE="publisher/secretary"\
           PORT=$SECRETARY_BUILD_PORT
elif [ $TARGET = "secretary-run" ]; then
    export SOURCE="http://localhost:8001/secretary.tar"
elif [ $TARGET = "tycoon-build" ]; then
    export SOURCE="publisher/tycoon"\
           PORT=$TYCOON_BUILD_PORT
elif [ $TARGET = "tycoon-run" ]; then
    export SOURCE="http://localhost:8002/tycoon.tar"
else
    echo "Unknown service"
    exit 1
fi
echo "Deploying service located at $SOURCE"

export OUTPUT_DIR=`mktemp -d`    
TARGET_DIR=`mktemp -d`

if [[ $SOURCE = "http"* ]]; then
    echo "Downloading slug from $SOURCE"
    (cd $TARGET_DIR; wget -P $TARGET_DIR $SOURCE; tar xvf `ls .`; rm *.tar)
else
    echo "Copying files from $SOURCE"
    cp -R $SOURCE/* $TARGET_DIR
fi

EXECUTABLE=`find $TARGET_DIR -maxdepth 1 -executable -type f ! -name '*~'`
if [ -z "$EXECUTABLE" -o `echo "$EXECUTABLE" | wc -l` -ne 1 ]; then echo "Could not locate unique executable"; exit 1; fi
EXECUTABLE=`basename $EXECUTABLE`


echo "Running executable $EXECUTABLE in $TARGET_DIR"
(cd $TARGET_DIR; ./$EXECUTABLE)
echo "Executable $EXECUTABLE finished in $TARGET_DIR"

if [ "$(ls -A $OUTPUT_DIR)" ]; then
    echo "Serving output files from $OUTPUT_DIR on port $PORT"
    (cd $OUTPUT_DIR; python -m SimpleHTTPServer $PORT)
else
    echo "No output, ending deployment"
fi