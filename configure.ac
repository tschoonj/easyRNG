AC_INIT([easyRNG],[0.1],[Tom.Schoonjans@me.com])
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
AC_ARG_ENABLE([fortran], [AS_HELP_STRING([--disable-fortran],[build without the Fortran bindings])],[enable_fortran=$enableval],[enable_fortran=check] )

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

		if test "x$enable_fortran" = xyes && "x$C_LOC_TARGET" = xno; then
			AC_MSG_ERROR([Fortran compiler does not support a required feature from the 2008 standard.])
		elif test "x$enable_fortran" = xauto && "x$C_LOC_TARGET" = xno; then
			AC_MSG_WARN([Fortran compiler does not support a required feature from the 2008 standard.])
		else 
			BUILD_FORTRAN=yes
			AC_MSG_NOTICE([Building Fortran bindings])
		fi
	fi

fi
AM_CONDITIONAL([ENABLE_FORTRAN],[test "x$BUILD_FORTRAN" = xyes])

#initialize pkg-config
PKG_PROG_PKG_CONFIG

GSL_FOUND=no
PKG_CHECK_MODULES([gsl],gsl >= 1.13, [
	#GSL found
	AC_DEFINE([HAVE_GSL], [], [GSL found])
	GSL_FOUND=yes
], [
	GSL_FOUND=no
])
AM_CONDITIONAL([ENABLE_GSL_TEST],[test "x$GSL_FOUND" = xyes])


AC_CONFIG_FILES([Makefile src/Makefile tests/Makefile easyRNG.pc])
AC_CONFIG_HEADERS([config.h])

AC_OUTPUT
