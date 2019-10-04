#!/usr/bin/env bash

set -e
set -x

wget -q https://doku.lrz.de/download/attachments/43321199/fgsl-1.3.0.tar.gz
tar xfz fgsl-1.3.0.tar.gz
cd fgsl-1.3.0
wget -q https://github.com/reinh-bader/fgsl/commit/c306e9f936983df5bab68f8ba55006c0f88bc775.diff
patch -p1 < c306e9f936983df5bab68f8ba55006c0f88bc775.diff
./configure
make
make install
cd ..


