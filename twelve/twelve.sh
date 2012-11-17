#!/bin/bash

CATALOG_DIR=`mktemp -d`

TARGET=$1
echo $TARGET
if [ $TARGET = "hailstone" ]; then
    export SOURCE="../examples/hailstone/hailstone"\
           PORT=9090
elif [ $TARGET = "test-harness" ]; then
    export SOURCE="../core/test-harness"\
           PORT=9091
elif [ $TARGET = "catalog" ]; then
    export SOURCE="publisher/catalog"\
           PORT=8000 \
           CATALOG_FILE="$CATALOG_DIR/catalog.s3"
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