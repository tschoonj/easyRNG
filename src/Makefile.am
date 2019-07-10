lib_LTLIBRARIES = libeasyRNG.la

easyRNGincludedir = $(includedir)/easyRNG

easyRNGinclude_HEADERS = easy_rng.h easy_randist.h

libeasyRNG_la_SOURCES = $(easyRNGinclude_HEADERS) easy_rng.cpp easy_rng_private.h easy_randist.cpp
libeasyRNG_la_LDFLAGS = -version-info @LIB_CURRENT@:@LIB_REVISION@:@LIB_AGE@ $(LDFLAGS_EXTRA)
libeasyRNG_la_CXXFLAGS = $(HIDDEN_VISIBILITY_CXXFLAGS)
 

if ENABLE_FORTRAN
libeasyRNG_la_SOURCES += easy_rng_f.F90
nodist_easyRNGinclude_HEADERS = easyrng.mod 
BUILT_SOURCES = easyrng.mod
libeasyRNG_la_LIBADD = $(FCLIBS)
easyrng.mod: libeasyRNG.la
endif

clean-local:
	rm -rf easyrng.mod

EXTRA_DIST = meson.build
