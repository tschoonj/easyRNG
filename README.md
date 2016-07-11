# easyRNG

A lightweight and easy-to-use library that wraps C++11's random number generators, making them available from C and Fortran.
easyRNG is licensed under the 3-clause BSD license.

## Features

* Add random number generator and distributions to your C and Fortran code (or C++ if you really don't like the `random` template API :smile:)
* API based on the [GNU scientific library's (GSL) random number generators](https://www.gnu.org/software/gsl/) and its [Fortran bindings FGSL](http://www.lrz.de/services/software/mathematik/gsl/fortran/)
* Has no dependencies, except a C++11 compliant compiler
* Tested on Linux, Mac OS X and Windows
* Distributed under the permissive 3-clause BSD license, while GSL and FGSL are licensed under the more restrictive GPLv3.

easyRNG is currently under heavy development: so far only (partial) support for C has been implemented (random number generators only, no distributions yet)

[![Build status](https://ci.appveyor.com/api/projects/status/q3aj9obkyswj2smv?svg=true)](https://ci.appveyor.com/project/tschoonj/easyrng)  [![Build Status](https://travis-ci.org/tschoonj/easyRNG.svg?branch=master)](https://travis-ci.org/tschoonj/easyRNG)


