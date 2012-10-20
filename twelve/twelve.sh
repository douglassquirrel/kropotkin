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
    BUILD_EXECUTABLE=`find $BUILD_DIR -maxdepth 1 -executable -type f ! -name '*~'`
    if [ -z "$BUILD_EXECUTABLE" -o `echo "$BUILD_EXECUTABLE" | wc -l` -ne 1 ]; then echo "Could not locate unique executable"; exit 1; fi
    BUILD_EXECUTABLE=`basename $BUILD_EXECUTABLE`
    echo "Running build executable $BUILD_EXECUTABLE"
    (cd $BUILD_DIR; ./$BUILD_EXECUTABLE)
    echo "Build finished in $BUILD_DIR"
elif [ $ACTION = "run" ]; then
    BUILD_DIR="/tmp/$TARGET/build"
    if [ ! -d "$BUILD_DIR" ]; then echo "Could not locate target directory $BUILD_DIR"; exit 1; fi
    RUN_DIR="/tmp/$TARGET-run-"`date +%Y-%B-%d-%H-%M-%S`
    rm -rf $RUN_DIR
    mkdir -p $RUN_DIR
    cp -R $BUILD_DIR/* $RUN_DIR
    RUN_EXECUTABLE=`find $RUN_DIR -maxdepth 1 -executable -type f ! -name '*~'`
    if [ -z "$RUN_EXECUTABLE" -o `echo "$RUN_EXECUTABLE" | wc -l` -ne 1 ]; then echo "Could not locate unique executable"; exit 1; fi
    RUN_EXECUTABLE=`basename $RUN_EXECUTABLE`
    echo "Running run executable $RUN_EXECUTABLE"
    (cd $RUN_DIR; ./$RUN_EXECUTABLE)
    echo "Run finished in $RUN_DIR"
else
    echo "Action $ACTION not recognised"
fi
