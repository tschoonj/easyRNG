lib_LTLIBRARIES = libeasyRNG.la

easyRNGincludedir = $(includedir)/easyRNG

easyRNGinclude_HEADERS = easy_rng.h easy_randist.h

libeasyRNG_la_SOURCES = $(easyRNGinclude_HEADERS) easy_rng.cpp easy_rng_private.h easy_randist.cpp
libeasyRNG_la_LDFLAGS = -version-info @LIB_CURRENT@:@LIB_REVISION@:@LIB_AGE@ $(LDFLAGS_EXTRA)
 
