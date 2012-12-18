mkdir library
wget -O library/catalog.tar $CATALOG_URL
tar xv -C library -f library/catalog.tar
chmod -x build.sh
chmod +x run.rkt
tar cvf $OUTPUT_DIR/secretary.tar *