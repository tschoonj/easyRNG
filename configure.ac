AC_INIT([easyRNG],[1.0],[Tom.Schoonjans@me.com],[easyRNG],[https://tschoonj.github.io/easyRNG])
AC_PREREQ([2.60])
AC_CONFIG_SRCDIR([src/easy_rng.cpp])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_CANONICAL_HOST

LIB_CURRENT=0
LIB_REVISION=0
LIB_AGE=0
AC_SUBST(LIB_CURRENT)
AC_SUBST(LIB_REVISION)
AC_SUBST(LIB_AGE)

#m4_pattern_allow([AS_TR_SH])
AC_CONFIG_MACRO_DIR([m4])

AC_USE_SYSTEM_EXTENSIONS
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])

#libtool
LT_PREREQ([2.0.0])

LT_INIT()
AC_PROG_LIBTOOL

AC_PROG_CC
if test `AS_BASENAME([$CC])` = $CC ; then
  AC_CHECK_PROG(CC_FULL, [$CC], $CC, [none])
  #this next line may never be reached...
  if test x$CC_FULL = "xnone" ; then
          AC_MSG_ERROR([no C compiler was found on the system.])
  fi
fi
AM_PROG_CC_C_O


AC_PROG_CXX
if test `AS_BASENAME([$CXX])` = $CXX ; then
  AC_CHECK_PROG(CXX_FULL, [$CXX], $CXX, [none])
  #this next line may never be reached...
  if test x$CXX_FULL = "xnone" ; then
          AC_MSG_ERROR([no C++ compiler was found on the system.])
  fi
fi
AC_PROG_CXX_C_O

AX_CXX_COMPILE_STDCXX_11(noext, mandatory)

LDFLAGS_EXTRA=""
case "$host" in
	*mingw*)
		LDFLAGS_EXTRA+="-no-undefined"
		;;
esac
AC_SUBST(LDFLAGS_EXTRA)


#check for Fortran support
BUILD_FORTRAN=no
FGSL_FOUND=no
AC_ARG_ENABLE([fortran], [AS_HELP_STRING([--disable-fortran],[build without the Fortran bindings])],[enable_fortran=$enableval],[enable_fortran=auto] )

#initialize pkg-config
PKG_PROG_PKG_CONFIG

if test "x$enable_fortran" != xno ; then

	AC_PROG_FC
	if test -z $FC ; then
		if test "x$enable_fortran" = xyes ; then
			AC_MSG_ERROR([Fortran compiler was not found. Specify a compiler with the FC variable])
		else
			AC_MSG_WARN([Fortran compiler was not found. Specify a compiler with the FC variable])
		fi
	else
		# check if this is a good enough Fortran compiler
		#check for target support
		AC_FC_SRCEXT([F90],[
			AC_MSG_CHECKING([whether the fortran compiler supports the 2008 feature: c_loc for targeted arrays])
			AC_LANG_PUSH(Fortran)
			AC_COMPILE_IFELSE([[
SUBROUTINE test(c)
USE, INTRINSIC :: iso_c_binding
IMPLICIT NONE
INTEGER(c_size_t), DIMENSION(:), INTENT(inout), TARGET:: c
TYPE(C_PTR) :: ptr

ptr = C_LOC(c)

ENDSUBROUTINE
			]], [C_LOC_TARGET=yes], [C_LOC_TARGET=no])
			AC_LANG_POP(Fortran)
			AC_MSG_RESULT([$C_LOC_TARGET])
		])
		if test "x$enable_fortran" = xyes && test "x$C_LOC_TARGET" = xno ; then
			AC_MSG_ERROR([Fortran compiler does not support a required feature from the 2008 standard.])
		elif test "x$enable_fortran" = xauto && test "x$C_LOC_TARGET" = xno ; then
			AC_MSG_WARN([Fortran compiler does not support a required feature from the 2008 standard.])
		else 
			BUILD_FORTRAN=yes
			AC_MSG_NOTICE([Building Fortran bindings])
			# Add fortran linker flags: necessary since the C++ linker will be used...
			# Not necessary right now: to be added when need arises...
			#AC_FC_LIBRARY_LDFLAGS
			PKG_CHECK_MODULES([fgsl], fgsl >= 1.0.0, [
				#FGSL found
				AC_DEFINE([HAVE_FGSL], [], [FGSL found])
				FGSL_FOUND=yes
			], [
				FGSL_FOUND=no
			])
		fi
	fi

fi
AM_CONDITIONAL([ENABLE_FORTRAN],[test "x$BUILD_FORTRAN" = xyes])

GSL_FOUND=no
PKG_CHECK_MODULES([gsl],gsl >= 1.13, [
	#GSL found
	AC_DEFINE([HAVE_GSL], [], [GSL found])
	GSL_FOUND=yes
], [
	GSL_FOUND=no
])
AM_CONDITIONAL([ENABLE_GSL_TEST],[test "x$GSL_FOUND" = xyes])

AM_CONDITIONAL([ENABLE_FGSL_TEST],[test "x$FGSL_FOUND" = xyes])
AC_CHECK_PROGS([DOXYGEN], [doxygen])
if test -z "$DOXYGEN";
   then AC_MSG_WARN([Doxygen not found - continuing without Doxygen support])
fi
AM_CONDITIONAL([HAVE_DOXYGEN], [test -n "$DOXYGEN"])
AM_COND_IF([HAVE_DOXYGEN], [AC_CONFIG_FILES([docs/Doxyfile])])


AC_CONFIG_FILES([Makefile src/Makefile tests/Makefile easyRNG.pc docs/Makefile easyRNG.spec])
AC_CONFIG_HEADERS([config.h])

AC_OUTPUT
