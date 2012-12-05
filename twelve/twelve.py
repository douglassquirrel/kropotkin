import os, sys

if (len(sys.argv) <= 1):
    print "Must provide target on command line"
    sys.exit(255)

target = sys.argv[1]

if (2 <= len(sys.argv) and sys.argv[2] == "stop"):
    print "Stopping " + target
    os.system('pkill -9 -g `pgrep -of "twelve.py $TARGET"`')
    sys.exit(0)

print "Starting " + target
target_dict = {"hailstone":       "../examples/hailstone/hailstone $HAILSTONE_BUILD_PORT",
               "test-harness":    "../core/test-harness $TEST_HARNESS_BUILD_PORT", 
               "catalog":         "publisher/catalog $CATALOG_BUILD_PORT",
               "secretary-build": "publisher/secretary $SECRETARY_BUILD_PORT",  
               "secretary-run":   "http://localhost:$SECRETARY_BUILD_PORT/secretary.tar",
               "tycoon-build":    "publisher/tycoon $TYCOON_BUILD_PORT",
               "tycoon-run":      "http://localhost:$TYCOON_BUILD_PORT/tycoon.tar"}
os.system("bash ./deploy.sh " + target_dict[target])
