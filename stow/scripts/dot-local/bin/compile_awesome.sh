#!/usr/bin/env bash
owd="$(pwd)"
cd ~/Projects/awesome || exit
sudo rm -rf build
CMAKE_ARGS="-DLUA_EXECUTABLE=/usr/bin/lua5.3 -DGENERATE_DOC=OFF -DGENERATE_MANPAGES=OFF  -DLUA_LIBRARY=/usr/lib/liblua.so.5.3 -DLUA_INCLUDE_DIR=/usr/include/lua5.3" make
sudo make install
sudo cp /usr/local/share/xsessions/awesome.desktop /usr/share/xsessions
cd "$owd" || exit
