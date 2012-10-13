#!/bin/bash

ACTION=$1
TARGET=$2
echo "Performing action $ACTION for $TARGET"
    
if [ $ACTION = "build" ]; then
    TARGET_DIR="../$TARGET"
    if [ ! -d "$TARGET_DIR" ]; then echo "Could not locate target directory $TARGET_DIR"; exit 1; fi

    BUILD_DIR="/tmp/$TARGET/build"
    rm -rf $BUILD_DIR
    mkdir -p $BUILD_DIR
    cp -R $TARGET_DIR/* $BUILD_DIR
    BUILD_EXECUTABLE=`find ../hailstone/ -maxdepth 1 -executable -type f ! -name '*~'`
    if [ -z "$BUILD_EXECUTABLE" -o `echo "$BUILD_EXECUTABLE" | wc -l` -ne 1 ]; then echo "Could not locate unique executable"; exit 1; fi
    BUILD_EXECUTABLE=`basename $BUILD_EXECUTABLE`
    echo "Running build executable $BUILD_EXECUTABLE"
    (cd $BUILD_DIR; ./$BUILD_EXECUTABLE)
    echo "Build finished in $BUILD_DIR"
elif [ $ACTION = "run" ]; then
    echo "Action run is not yet implemented"
else
    echo "Action $ACTION not recognised"
fi
