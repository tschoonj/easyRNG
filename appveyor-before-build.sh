#!/usr/bin/env bash

set -e
set -x

wget -q http://www.lrz.de/services/software/mathematik/gsl/fortran/download/fgsl-1.1.0.tar.gz
tar xfz fgsl-1.1.0.tar.gz
cd fgsl-1.1.0
./configure
make
make install
cd ..


