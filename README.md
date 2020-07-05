[![Language grade: C/C++](https://img.shields.io/lgtm/grade/cpp/g/tschoonj/easyRNG.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/tschoonj/easyRNG/context:cpp) ![CI](https://github.com/tschoonj/easyRNG/workflows/CI/badge.svg?branch=master&event=push)

# easyRNG

A lightweight and easy-to-use library that wraps C++11's random number generators, making them available from C and Fortran.
easyRNG is licensed under the 3-clause BSD license.

## Features

* Add random number generator and distributions to your C and Fortran code (or C++ if you really don't like the `random` template API :smile:)
* API based on the [GNU scientific library's (GSL) random number generators](https://www.gnu.org/software/gsl/) and its [Fortran bindings FGSL](http://www.lrz.de/services/software/mathematik/gsl/fortran/)
* Has no dependencies, except a C++11 compliant compiler (and optionally a Fortran compiler)
* Thoroughly tested on Linux, Mac OS X and Windows
* Distributed under the permissive 3-clause BSD license, while GSL and FGSL are licensed under the more restrictive GPLv3.

## Documentation

Doxygen was used to generate [documentation](https://tschoonj.github.io/easyRNG), including usage instructions and examples.

## Downloads

See the [installation instructions](https://tschoonj.github.io/easyRNG/installation_instructions.html) on how to obtain a copy of easyRNG. Source tarballs are available, as well as binary packages for several Linux distributions.

That's it! Don't hesitate to open an issue if something does not work as it's supposed to...

