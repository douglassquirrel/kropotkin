export TARGET=$1 PORT=$2 TEST_PORT=3141
./twelve.sh ../core >> /tmp/$TARGET.log 2>&1 &