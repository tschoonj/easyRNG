SUBDIRS = src tests docs
ACLOCAL_AMFLAGS = -I m4

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = easyRNG.pc
if ENABLE_FORTRAN
pkgconfig_DATA += easyRNGf03.pc
endif

EXTRA_DIST = easyRNG.pc.in easyRNG.spec.in meson.build meson_options.txt install_fortran_mod.py easyRNGf03.pc.in
