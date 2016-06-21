make clean
scan-build -k -plist ./configure
scan-build -k -plist make -s -j2
make clean
