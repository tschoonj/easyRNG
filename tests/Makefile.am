check_PROGRAMS = test1


test1_SOURCES = test1.c
test1_CFLAGS = -I$(top_srcdir)/src
test1_LDADD = ../src/libeasyRNG.la -lm

if ENABLE_GSL_TEST
check_PROGRAMS += test2
test2_SOURCES = test2.c
test2_CFLAGS = -I$(top_srcdir)/src $(gsl_CFLAGS)
test2_LDADD = ../src/libeasyRNG.la $(gsl_LIBS) -lm
endif

check_PROGRAMS += test3
test3_SOURCES = test3.c
test3_CFLAGS = -I$(top_srcdir)/src
test3_LDADD = ../src/libeasyRNG.la -lm

if ENABLE_FORTRAN
check_PROGRAMS += test4
test4_SOURCES = test4.F90
test4_FCFLAGS = -I$(top_builddir)/src
test4_LDADD = ../src/libeasyRNGf03.la

if ENABLE_FGSL_TEST
check_PROGRAMS += test5
test5_SOURCES = test5.F90
test5_FCFLAGS = -I$(top_builddir)/src $(fgsl_CFLAGS)
test5_LDADD = ../src/libeasyRNGf03.la ../src/libeasyRNG.la $(fgsl_LIBS) -lm
endif

check_PROGRAMS += test6
test6_SOURCES = test6.F90
test6_FCFLAGS = -I$(top_builddir)/src
test6_LDADD = ../src/libeasyRNGf03.la ../src/libeasyRNG.la -lm
endif

TESTS = $(check_PROGRAMS)

EXTRA_DIST = meson.build
