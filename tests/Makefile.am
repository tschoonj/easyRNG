check_PROGRAMS = test1


test1_SOURCES = test1.c
test1_CFLAGS = -I$(top_srcdir)/src
test1_LDADD = ../src/libeasyRNG.la

if ENABLE_GSL_TEST
check_PROGRAMS += test2
test2_SOURCES = test2.c
test2_CFLAGS = -I$(top_srcdir)/src $(gsl_CFLAGS)
test2_LDADD = ../src/libeasyRNG.la $(gsl_LIBS)
endif

check_PROGRAMS += test3
test3_SOURCES = test3.c
test3_CFLAGS = -I$(top_srcdir)/src
test3_LDADD = ../src/libeasyRNG.la


TESTS = $(check_PROGRAMS)
