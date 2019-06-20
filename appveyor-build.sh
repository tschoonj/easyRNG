#!/usr/bin/env bash

if test "$BUILDSYSTEM" == "meson" ; then
	mkdir build
	cd build
	meson ..
	ninja
	ninja test
	ninja dist
elif test "$BUILDSYSTEM" == "autotools" ; then
	autoreconf -fi
	./configure
	make
	make check
	make distcheck
else
	echo "unknown BUILDSYSTEM!"
	exit 1
fi
