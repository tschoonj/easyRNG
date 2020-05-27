!Copyright (c) 2016, Tom Schoonjans
!All rights reserved.
!
!Redistribution and use in source and binary forms, with or without
!modification, are permitted provided that the following conditions are met:
!
!* Redistributions of source code must retain the above copyright notice, this
!  list of conditions and the following disclaimer.
!
!* Redistributions in binary form must reproduce the above copyright notice,
!  this list of conditions and the following disclaimer in the documentation
!  and/or other materials provided with the distribution.
!
!* Neither the name of easyRNG nor the names of its
!  contributors may be used to endorse or promote products derived from
!  this software without specific prior written permission.
!
!THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
!AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
!IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
!FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
!DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
!SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
!CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
!OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
!OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

!> \file easy_rng_f.F90
!!  \brief The random number generator API
!!  \author Tom Schoonjans
!!
!!  This header contains all functions and definitions that are necessary to instantiate a random number generator,
!!  as well as to obtain uniformly distributed random numbers.
!!  The API is strongly modelled after the GNU Scientific Library's gsl_rng.h header, with some notable exceptions:
!!  
!!  * easy_rng_state() and easy_rng_size() are not implemented as the required information is not accessible in the C++11 templates
!!  * easy_rng_equal() has been added to compare two random number generators for equality
!!  * easy_rng_fwrite and easy_rng_fread write a string (ASCII) representation of the state to file, instead of a binary stream.
!!  * Only 9 types of random number generators are offered here, in accordance with the C++11 standard. The GNU Scientific library offers dozens more. It should be noted though that the GNU Scientific library does not have the 64-bit implementation of the Mersenne-Twister (easy_rng_mt19937_64)
!!
!!  \note the description of the different types of random number generators on this page was sourced from [cppreference.com](http://en.cppreference.com/w/cpp/numeric/random). Please consult this website for more information.
!!

MODULE easyRNG

USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV

IMPLICIT NONE

TYPE :: easy_rng_type
  PRIVATE
  INTEGER (C_INT) :: id
ENDTYPE easy_rng_type

TYPE :: easy_rng
  PRIVATE
  TYPE (C_PTR) :: rng
ENDTYPE easy_rng

TYPE :: easy_ran_discrete_t
  PRIVATE
  TYPE (C_PTR) :: ran_discrete_t
ENDTYPE

TYPE (C_PTR), BIND(C, NAME='easy_rng_minstd_rand0') :: easy_rng_minstd_rand0_c
TYPE (C_PTR), BIND(C, NAME='easy_rng_minstd_rand') :: easy_rng_minstd_rand_c
TYPE (C_PTR), BIND(C, NAME='easy_rng_mt19937') :: easy_rng_mt19937_c
TYPE (C_PTR), BIND(C, NAME='easy_rng_mt19937_64') :: easy_rng_mt19937_64_c
TYPE (C_PTR), BIND(C, NAME='easy_rng_ranlux24_base') :: easy_rng_ranlux24_base_c
TYPE (C_PTR), BIND(C, NAME='easy_rng_ranlux48_base') :: easy_rng_ranlux48_base_c
TYPE (C_PTR), BIND(C, NAME='easy_rng_ranlux24') :: easy_rng_ranlux24_c
TYPE (C_PTR), BIND(C, NAME='easy_rng_ranlux48') :: easy_rng_ranlux48_c
TYPE (C_PTR), BIND(C, NAME='easy_rng_knuth_b') :: easy_rng_knuth_b_c
TYPE (C_PTR), BIND(C, NAME='easy_rng_default') :: easy_rng_default_c

TYPE (easy_rng_type), PARAMETER :: easy_rng_minstd_rand0 = easy_rng_type(0)
TYPE (easy_rng_type), PARAMETER :: easy_rng_minstd_rand = easy_rng_type(1)
TYPE (easy_rng_type), PARAMETER :: easy_rng_mt19937 = easy_rng_type(2)
TYPE (easy_rng_type), PARAMETER :: easy_rng_mt19937_64 = easy_rng_type(3)
TYPE (easy_rng_type), PARAMETER :: easy_rng_ranlux24_base = easy_rng_type(4)
TYPE (easy_rng_type), PARAMETER :: easy_rng_ranlux48_base = easy_rng_type(5)
TYPE (easy_rng_type), PARAMETER :: easy_rng_ranlux24 = easy_rng_type(6)
TYPE (easy_rng_type), PARAMETER :: easy_rng_ranlux48 = easy_rng_type(7)
TYPE (easy_rng_type), PARAMETER :: easy_rng_knuth_b = easy_rng_type(8)
TYPE (easy_rng_type), PARAMETER :: easy_rng_default = easy_rng_type(9)

CONTAINS

!easy_rng * easy_rng_alloc (const easy_rng_type * T);
#ifdef _WIN32
#ifdef __GFORTRAN__
!GCC$ ATTRIBUTES DLLEXPORT:: easy_rng_alloc
#else
!DEC$ ATTRIBUTES DLLEXPORT:: easy_rng_alloc
#endif
#endif
FUNCTION easy_rng_alloc(T) RESULT(rv)
  TYPE (easy_rng_type), INTENT(IN) :: T
  TYPE (easy_rng) :: rv
  TYPE (C_PTR) :: T_c

  INTERFACE
    FUNCTION easy_rng_alloc_c(T) BIND(C, NAME='easy_rng_alloc') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: T
      TYPE (C_PTR) :: rv
    ENDFUNCTION easy_rng_alloc_c

    SUBROUTINE easy_exit(status) BIND(C, NAME='exit')
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      INTEGER (C_INT), VALUE, INTENT(IN) :: status
    ENDSUBROUTINE easy_exit
  ENDINTERFACE

  WRITE (output_unit, '(A,I3)') 'type id: ', T%id 

  SELECT CASE (T%id)
        CASE (0)
                T_c = easy_rng_minstd_rand0_c
        CASE (1)
                T_c = easy_rng_minstd_rand_c
        CASE (2)
                T_c = easy_rng_mt19937_c
        CASE (3)
                T_c = easy_rng_mt19937_64_c
        CASE (4)
                T_c = easy_rng_ranlux24_base_c
        CASE (5)
                T_c = easy_rng_ranlux48_base_c
        CASE (6)
                T_c = easy_rng_ranlux24_c
        CASE (7)
                T_c = easy_rng_ranlux48_c
        CASE (8)
                T_c = easy_rng_knuth_b_c
        CASE (9)
                T_c = easy_rng_default_c
        CASE DEFAULT
                WRITE (error_unit, '(A)') 'easy_rng_alloc: Invalid RNG type detected'
                CALL easy_exit(1)
  ENDSELECT

  rv%rng = easy_rng_alloc_c(T_c)

ENDFUNCTION easy_rng_alloc

!void easy_rng_set (const easy_rng * r, unsigned long int s);
SUBROUTINE easy_rng_set(r, s)
  TYPE (easy_rng), INTENT(IN) :: r
  INTEGER (C_LONG), INTENT(IN) :: s

  INTERFACE
    SUBROUTINE easy_rng_set_c(r, s) BIND(C, NAME='easy_rng_set')
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      INTEGER (C_LONG), VALUE, INTENT(IN) :: s
    ENDSUBROUTINE easy_rng_set_c
  ENDINTERFACE

  CALL easy_rng_set_c(r%rng, s)

ENDSUBROUTINE easy_rng_set

!void easy_rng_free (easy_rng * r);
SUBROUTINE easy_rng_free(r)
  TYPE (easy_rng), INTENT(IN) :: r

  INTERFACE
    SUBROUTINE easy_rng_free_c(r) BIND(C, NAME='easy_rng_free')
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
    ENDSUBROUTINE easy_rng_free_c
  ENDINTERFACE

  CALL easy_rng_free_c(r%rng)

ENDSUBROUTINE easy_rng_free

!unsigned long int easy_rng_get (const easy_rng * r);
FUNCTION easy_rng_get(r) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  INTEGER (C_LONG) :: rv

  INTERFACE
    FUNCTION easy_rng_get_c(r) BIND(C, NAME='easy_rng_get') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      INTEGER (C_LONG) :: rv
    ENDFUNCTION easy_rng_get_c
  ENDINTERFACE

  rv = easy_rng_get_c(r%rng)

ENDFUNCTION easy_rng_get

!double easy_rng_uniform (const easy_rng * r);
FUNCTION easy_rng_uniform(r) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE) :: rv

  INTERFACE
    FUNCTION easy_rng_uniform_c(r) BIND(C, NAME='easy_rng_uniform') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE) :: rv
    ENDFUNCTION easy_rng_uniform_c
  ENDINTERFACE

  rv = easy_rng_uniform_c(r%rng)

ENDFUNCTION easy_rng_uniform

!double easy_rng_uniform_pos (const easy_rng * r);
FUNCTION easy_rng_uniform_pos(r) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE) :: rv

  INTERFACE
    FUNCTION easy_rng_uniform_pos_c(r) BIND(C, NAME='easy_rng_uniform_pos') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE) :: rv
    ENDFUNCTION easy_rng_uniform_pos_c
  ENDINTERFACE

  rv = easy_rng_uniform_pos_c(r%rng)

ENDFUNCTION easy_rng_uniform_pos

!unsigned long int easy_rng_uniform_int (const easy_rng * r, unsigned long int n);
FUNCTION easy_rng_uniform_int(r, n) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  INTEGER (C_LONG), INTENT(IN) :: n
  INTEGER (C_LONG) :: rv

  INTERFACE
    FUNCTION easy_rng_uniform_int_c(r, n) BIND(C, NAME='easy_rng_uniform_int') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      INTEGER (C_LONG), VALUE, INTENT(IN) :: n
      INTEGER (C_LONG) :: rv
    ENDFUNCTION easy_rng_uniform_int_c
  ENDINTERFACE

  rv = easy_rng_uniform_int_c(r%rng, n)

ENDFUNCTION easy_rng_uniform_int

!const char * easy_rng_name (const easy_rng * r);
FUNCTION easy_rng_name(r) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  CHARACTER (KIND=C_CHAR, LEN=50) :: rv
  CHARACTER (KIND=C_CHAR), POINTER, DIMENSION(:) :: name_f
  TYPE (C_PTR) :: name_c
  INTEGER :: i

  INTERFACE
    FUNCTION easy_rng_name_c(r) BIND(C, NAME='easy_rng_name') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      TYPE (C_PTR) :: rv
    ENDFUNCTION easy_rng_name_c
    FUNCTION strlen_c(s) BIND(C, NAME='strlen') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: s
      INTEGER (C_SIZE_T) :: rv
    ENDFUNCTION strlen_c
  ENDINTERFACE

  name_c = easy_rng_name_c(r%rng)
  CALL C_F_POINTER(name_c, name_f, [strlen_c(name_c)])

  DO i=1, LEN(rv)
    IF (i .LE. SIZE(name_f)) THEN
      rv(i:i) = name_f(i)
    ELSE
      rv(i:i) = ' '
    ENDIF
  ENDDO

ENDFUNCTION easy_rng_name

!unsigned long int easy_rng_max (const easy_rng * r);
FUNCTION easy_rng_max(r) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  INTEGER (C_LONG) :: rv

  INTERFACE
    FUNCTION easy_rng_max_c(r) BIND(C, NAME='easy_rng_max') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      INTEGER (C_LONG) :: rv
    ENDFUNCTION easy_rng_max_c
  ENDINTERFACE

  rv = easy_rng_max_c(r%rng)

ENDFUNCTION easy_rng_max

!unsigned long int easy_rng_min (const easy_rng * r);
FUNCTION easy_rng_min(r) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  INTEGER (C_LONG) :: rv

  INTERFACE
    FUNCTION easy_rng_min_c(r) BIND(C, NAME='easy_rng_min') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      INTEGER (C_LONG) :: rv
    ENDFUNCTION easy_rng_min_c
  ENDINTERFACE

  rv = easy_rng_min_c(r%rng)

ENDFUNCTION easy_rng_min

!/* Not going to happen...
!void * easy_rng_state (const easy_rng * r);

!size_t easy_rng_size (const easy_rng * r);
!*/

!const easy_rng_type ** easy_rng_types_setup (void);
FUNCTION easy_rng_types_setup() RESULT(RV)
  TYPE (easy_rng_type), POINTER, DIMENSION(:) :: rv
  INTEGER :: length, i

  ALLOCATE(rv(10))
  rv(1) = easy_rng_minstd_rand0
  rv(2) = easy_rng_minstd_rand
  rv(3) = easy_rng_mt19937
  rv(4) = easy_rng_mt19937_64
  rv(5) = easy_rng_ranlux24_base
  rv(6) = easy_rng_ranlux48_base
  rv(7) = easy_rng_ranlux24
  rv(8) = easy_rng_ranlux48
  rv(9) = easy_rng_knuth_b
  rv(10) = easy_rng_default

ENDFUNCTION easy_rng_types_setup

!const easy_rng_type * easy_rng_env_setup (void);
FUNCTION easy_rng_env_setup() RESULT(rv)
  TYPE (easy_rng_type) :: rv
  TYPE (C_PTR) :: rv_c
 
  INTERFACE
    FUNCTION easy_rng_env_setup_c() BIND(C, NAME='easy_rng_env_setup') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR) :: rv
    ENDFUNCTION easy_rng_env_setup_c

    SUBROUTINE easy_exit(status) BIND(C, NAME='exit')
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      INTEGER (C_INT), VALUE, INTENT(IN) :: status
    ENDSUBROUTINE easy_exit
  ENDINTERFACE

  rv_c = easy_rng_env_setup_c()

  IF (C_ASSOCIATED(rv_c, easy_rng_minstd_rand0_c)) THEN
          rv%id = 0
  ELSEIF (C_ASSOCIATED(rv_c, easy_rng_minstd_rand_c)) THEN
          rv%id = 1
  ELSEIF (C_ASSOCIATED(rv_c, easy_rng_mt19937_c)) THEN
          rv%id = 2
  ELSEIF (C_ASSOCIATED(rv_c, easy_rng_mt19937_64_c)) THEN
          rv%id = 3
  ELSEIF (C_ASSOCIATED(rv_c, easy_rng_ranlux24_base_c)) THEN
          rv%id = 4
  ELSEIF (C_ASSOCIATED(rv_c, easy_rng_ranlux48_base_c)) THEN
          rv%id = 5
  ELSEIF (C_ASSOCIATED(rv_c, easy_rng_ranlux24_c)) THEN
          rv%id = 6
  ELSEIF (C_ASSOCIATED(rv_c, easy_rng_ranlux48_c)) THEN
          rv%id = 7
  ELSEIF (C_ASSOCIATED(rv_c, easy_rng_knuth_b_c)) THEN
          rv%id = 8
  ELSEIF (C_ASSOCIATED(rv_c, easy_rng_default_c)) THEN
          rv%id = 9
  ELSE
          WRITE (error_unit, '(A)') 'easy_rng_setup: unknown RNG type returned'
          CALL easy_exit(1)
  ENDIF


ENDFUNCTION easy_rng_env_setup

!int easy_rng_memcpy (easy_rng * dest, const easy_rng * src);
FUNCTION easy_rng_memcpy(dest, src) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: src
  TYPE (easy_rng), INTENT(INOUT) :: dest
  INTEGER (C_INT) :: rv

  INTERFACE
    FUNCTION easy_rng_memcpy_c(dest, src) BIND(C, NAME='easy_rng_memcpy') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: dest, src
      INTEGER (C_INT) :: rv
    ENDFUNCTION easy_rng_memcpy_c
  ENDINTERFACE

  rv = easy_rng_memcpy_c(dest%rng, src%rng)

ENDFUNCTION easy_rng_memcpy

!easy_rng * easy_rng_clone (const easy_rng * r);
FUNCTION easy_rng_clone(r) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  TYPE (easy_rng) :: rv

  INTERFACE
    FUNCTION easy_rng_clone_c(r) BIND(C, NAME='easy_rng_clone') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      TYPE (C_PTR) :: rv
    ENDFUNCTION easy_rng_clone_c
  ENDINTERFACE

  rv%rng = easy_rng_clone_c(r%rng)

ENDFUNCTION easy_rng_clone

!int easy_rng_equal(const easy_rng * ra, const easy_rng *rb);
FUNCTION easy_rng_equal(ra, rb) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: ra, rb
  INTEGER (C_INT) :: rv

  INTERFACE
    FUNCTION easy_rng_equal_c(ra, rb) BIND(C, NAME='easy_rng_equal') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: ra, rb
      INTEGER (C_INT) :: rv
    ENDFUNCTION easy_rng_equal_c
  ENDINTERFACE

  rv = easy_rng_equal_c(ra%rng, rb%rng)

ENDFUNCTION easy_rng_equal

! will not implement these two for now as they require wrappers around fopen and fclose... VERY un-Fortran like
!int easy_rng_fwrite (FILE * stream, const easy_rng * r);

!int easy_rng_fread (FILE * stream, easy_rng * r);


!double easy_ran_gaussian (const easy_rng * r, double sigma);
FUNCTION easy_ran_gaussian(r, sigma) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE), INTENT(IN) :: sigma
  REAL (C_DOUBLE) :: rv

  INTERFACE
    FUNCTION easy_ran_gaussian_c(r, sigma) BIND(C, NAME='easy_ran_gaussian') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE), VALUE, INTENT(IN) :: sigma
      REAL (C_DOUBLE) :: rv
    ENDFUNCTION easy_ran_gaussian_c
  ENDINTERFACE

  rv = easy_ran_gaussian_c(r%rng, sigma)

ENDFUNCTION easy_ran_gaussian

!double easy_ran_gaussian_ziggurat (const easy_rng * r, double sigma);
FUNCTION easy_ran_gaussian_ziggurat(r, sigma) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE), INTENT(IN) :: sigma
  REAL (C_DOUBLE) :: rv

  INTERFACE
    FUNCTION easy_ran_gaussian_ziggurat_c(r, sigma) BIND(C, NAME='easy_ran_gaussian_ziggurat') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE), VALUE, INTENT(IN) :: sigma
      REAL (C_DOUBLE) :: rv
    ENDFUNCTION easy_ran_gaussian_ziggurat_c
  ENDINTERFACE

  rv = easy_ran_gaussian_ziggurat_c(r%rng, sigma)

ENDFUNCTION easy_ran_gaussian_ziggurat

!double easy_ran_gaussian_ratio_method (const easy_rng * r, double sigma);
FUNCTION easy_ran_gaussian_ratio_method(r, sigma) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE), INTENT(IN) :: sigma
  REAL (C_DOUBLE) :: rv

  INTERFACE
    FUNCTION easy_ran_gaussian_ratio_method_c(r, sigma) BIND(C, NAME='easy_ran_gaussian_ratio_method') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE), VALUE, INTENT(IN) :: sigma
      REAL (C_DOUBLE) :: rv
    ENDFUNCTION easy_ran_gaussian_ratio_method_c
  ENDINTERFACE

  rv = easy_ran_gaussian_ratio_method_c(r%rng, sigma)

ENDFUNCTION easy_ran_gaussian_ratio_method

!double easy_ran_ugaussian (const easy_rng * r);
FUNCTION easy_ran_ugaussian(r) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE) :: rv

  INTERFACE
    FUNCTION easy_ran_ugaussian_c(r) BIND(C, NAME='easy_ran_ugaussian') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE) :: rv
    ENDFUNCTION easy_ran_ugaussian_c
  ENDINTERFACE

  rv = easy_ran_ugaussian_c(r%rng)

ENDFUNCTION easy_ran_ugaussian

!double easy_ran_ugaussian_ratio_method (const easy_rng * r);
FUNCTION easy_ran_ugaussian_ratio_method(r) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE) :: rv

  INTERFACE
    FUNCTION easy_ran_ugaussian_ratio_method_c(r) BIND(C, NAME='easy_ran_ugaussian_ratio_method') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE) :: rv
    ENDFUNCTION easy_ran_ugaussian_ratio_method_c
  ENDINTERFACE

  rv = easy_ran_ugaussian_ratio_method_c(r%rng)

ENDFUNCTION easy_ran_ugaussian_ratio_method

!double easy_ran_exponential (const easy_rng * r, double mu);
FUNCTION easy_ran_exponential(r, mu) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE), INTENT(IN) :: mu
  REAL (C_DOUBLE) :: rv

  INTERFACE
    FUNCTION easy_ran_exponential_c(r, mu) BIND(C, NAME='easy_ran_exponential') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE), VALUE, INTENT(IN) :: mu
      REAL (C_DOUBLE) :: rv
    ENDFUNCTION easy_ran_exponential_c
  ENDINTERFACE

  rv = easy_ran_exponential_c(r%rng, mu)

ENDFUNCTION easy_ran_exponential

!double easy_ran_cauchy (const easy_rng * r, double a);
FUNCTION easy_ran_cauchy(r, a) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE), INTENT(IN) :: a
  REAL (C_DOUBLE) :: rv

  INTERFACE
    FUNCTION easy_ran_cauchy_c(r, a) BIND(C, NAME='easy_ran_cauchy') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE), VALUE, INTENT(IN) :: a
      REAL (C_DOUBLE) :: rv
    ENDFUNCTION easy_ran_cauchy_c
  ENDINTERFACE

  rv = easy_ran_cauchy_c(r%rng, a)

ENDFUNCTION easy_ran_cauchy

!double easy_ran_gamma (const easy_rng * r, double a, double b);
FUNCTION easy_ran_gamma(r, a, b) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE), INTENT(IN) :: a, b
  REAL (C_DOUBLE) :: rv

  INTERFACE
    FUNCTION easy_ran_gamma_c(r, a, b) BIND(C, NAME='easy_ran_gamma') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE), VALUE, INTENT(IN) :: a, b
      REAL (C_DOUBLE) :: rv
    ENDFUNCTION easy_ran_gamma_c
  ENDINTERFACE

  rv = easy_ran_gamma_c(r%rng, a, b)

ENDFUNCTION easy_ran_gamma

!double easy_ran_flat (const easy_rng * r, double a, double b);
FUNCTION easy_ran_flat(r, a, b) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE), INTENT(IN) :: a, b
  REAL (C_DOUBLE) :: rv

  INTERFACE
    FUNCTION easy_ran_flat_c(r, a, b) BIND(C, NAME='easy_ran_flat') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE), VALUE, INTENT(IN) :: a, b
      REAL (C_DOUBLE) :: rv
    ENDFUNCTION easy_ran_flat_c
  ENDINTERFACE

  rv = easy_ran_flat_c(r%rng, a, b)

ENDFUNCTION easy_ran_flat

!double easy_ran_lognormal (const easy_rng * r, double zeta, double sigma);
FUNCTION easy_ran_lognormal(r, zeta, sigma) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE), INTENT(IN) :: zeta, sigma
  REAL (C_DOUBLE) :: rv

  INTERFACE
    FUNCTION easy_ran_lognormal_c(r, zeta, sigma) BIND(C, NAME='easy_ran_lognormal') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE), VALUE, INTENT(IN) :: zeta, sigma
      REAL (C_DOUBLE) :: rv
    ENDFUNCTION easy_ran_lognormal_c
  ENDINTERFACE

  rv = easy_ran_lognormal_c(r%rng, zeta, sigma)

ENDFUNCTION easy_ran_lognormal

!double easy_ran_chisq (const easy_rng * r, double nu);
FUNCTION easy_ran_chisq(r, nu) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE), INTENT(IN) :: nu
  REAL (C_DOUBLE) :: rv

  INTERFACE
    FUNCTION easy_ran_chisq_c(r, nu) BIND(C, NAME='easy_ran_chisq') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE), VALUE, INTENT(IN) :: nu
      REAL (C_DOUBLE) :: rv
    ENDFUNCTION easy_ran_chisq_c
  ENDINTERFACE

  rv = easy_ran_chisq_c(r%rng, nu)

ENDFUNCTION easy_ran_chisq

!double easy_ran_fdist (const easy_rng * r, double nu1, double nu2);
FUNCTION easy_ran_fdist(r, nu1, nu2) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE), INTENT(IN) :: nu1, nu2
  REAL (C_DOUBLE) :: rv

  INTERFACE
    FUNCTION easy_ran_fdist_c(r, nu1, nu2) BIND(C, NAME='easy_ran_fdist') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE), VALUE, INTENT(IN) :: nu1, nu2
      REAL (C_DOUBLE) :: rv
    ENDFUNCTION easy_ran_fdist_c
  ENDINTERFACE

  rv = easy_ran_fdist_c(r%rng, nu1, nu2)

ENDFUNCTION easy_ran_fdist

!double easy_ran_tdist (const easy_rng * r, double nu);
FUNCTION easy_ran_tdist(r, nu) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE), INTENT(IN) :: nu
  REAL (C_DOUBLE) :: rv

  INTERFACE
    FUNCTION easy_ran_tdist_c(r, nu) BIND(C, NAME='easy_ran_tdist') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE), VALUE, INTENT(IN) :: nu
      REAL (C_DOUBLE) :: rv
    ENDFUNCTION easy_ran_tdist_c
  ENDINTERFACE

  rv = easy_ran_tdist_c(r%rng, nu)

ENDFUNCTION easy_ran_tdist

!double easy_ran_weibull (const easy_rng * r, double a, double b);
FUNCTION easy_ran_weibull(r, a, b) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE), INTENT(IN) :: a, b
  REAL (C_DOUBLE) :: rv

  INTERFACE
    FUNCTION easy_ran_weibull_c(r, a, b) BIND(C, NAME='easy_ran_weibull') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE), VALUE, INTENT(IN) :: a, b
      REAL (C_DOUBLE) :: rv
    ENDFUNCTION easy_ran_weibull_c
  ENDINTERFACE

  rv = easy_ran_weibull_c(r%rng, a, b)

ENDFUNCTION easy_ran_weibull

!// Thought that std::extreme_value_distribution was matching Gumbel Type 1, but that looks incorrect
!//double easy_ran_gumbel1 (const easy_rng * r, double a, double b);

!easy_ran_discrete_t * easy_ran_discrete_preproc (size_t K, const double * P);
FUNCTION easy_ran_discrete_preproc(P) RESULT(rv)
  REAL (C_DOUBLE), INTENT(IN), DIMENSION(:), TARGET :: P
  TYPE (easy_ran_discrete_t) :: rv

  INTERFACE
    FUNCTION easy_ran_discrete_preproc_c(K, P) BIND(C, NAME='easy_ran_discrete_preproc') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      INTEGER (C_SIZE_T), INTENT(IN), VALUE :: K
      TYPE (C_PTR), INTENT(IN), VALUE :: P
      TYPE (C_PTR) :: rv
    ENDFUNCTION easy_ran_discrete_preproc_c
  ENDINTERFACE

  rv%ran_discrete_t = easy_ran_discrete_preproc_c(SIZE(P, KIND=C_SIZE_T), C_LOC(P))

ENDFUNCTION easy_ran_discrete_preproc

!size_t easy_ran_discrete (const easy_rng * r, const easy_ran_discrete_t * g);
FUNCTION easy_ran_discrete(r, g) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  TYPE (easy_ran_discrete_t), INTENT(IN) :: g
  INTEGER (C_SIZE_T) :: rv

  INTERFACE
    FUNCTION easy_ran_discrete_c(r, g) BIND(C, NAME="easy_ran_discrete") RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), INTENT(IN), VALUE :: r, g
      INTEGER (C_SIZE_T) :: rv
    ENDFUNCTION easy_ran_discrete_c
  ENDINTERFACE

  rv = easy_ran_discrete_c(r%rng, g%ran_discrete_t)

ENDFUNCTION easy_ran_discrete

!void easy_ran_discrete_free (easy_ran_discrete_t * g);
SUBROUTINE easy_ran_discrete_free(g)
  TYPE (easy_ran_discrete_t), INTENT(IN) :: g

  INTERFACE
    SUBROUTINE easy_ran_discrete_free_c(g) BIND(C, NAME="easy_ran_discrete_free")
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), INTENT(IN), VALUE :: g
    ENDSUBROUTINE easy_ran_discrete_free_c
  ENDINTERFACE

  CALL easy_ran_discrete_free_c(g%ran_discrete_t)

ENDSUBROUTINE easy_ran_discrete_free

!unsigned int easy_ran_poisson (const easy_rng * r, double mu);
FUNCTION easy_ran_poisson(r, mu) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE), INTENT(IN) :: mu
  INTEGER (C_INT) :: rv

  INTERFACE
    FUNCTION easy_ran_poisson_c(r, mu) BIND(C, NAME='easy_ran_poisson') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE), VALUE, INTENT(IN) :: mu
      INTEGER (C_INT) :: rv
    ENDFUNCTION easy_ran_poisson_c
  ENDINTERFACE

  rv = easy_ran_poisson_c(r%rng, mu)

ENDFUNCTION easy_ran_poisson

!unsigned int easy_ran_bernoulli (const easy_rng * r, double p);
FUNCTION easy_ran_bernoulli(r, p) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE), INTENT(IN) :: p
  INTEGER (C_INT) :: rv

  INTERFACE
    FUNCTION easy_ran_bernoulli_c(r, p) BIND(C, NAME='easy_ran_bernoulli') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE), VALUE, INTENT(IN) :: p
      INTEGER (C_INT) :: rv
    ENDFUNCTION easy_ran_bernoulli_c
  ENDINTERFACE

  rv = easy_ran_bernoulli_c(r%rng, p)

ENDFUNCTION easy_ran_bernoulli

!unsigned int easy_ran_binomial (const easy_rng * r, double p, unsigned int n);
FUNCTION easy_ran_binomial(r, p, n) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE), INTENT(IN) :: p
  INTEGER (C_INT), INTENT(IN) :: n
  INTEGER (C_INT) :: rv

  INTERFACE
    FUNCTION easy_ran_binomial_c(r, p, n) BIND(C, NAME='easy_ran_binomial') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE), VALUE, INTENT(IN) :: p
      INTEGER (C_INT), VALUE, INTENT(IN) :: n
      INTEGER (C_INT) :: rv
    ENDFUNCTION easy_ran_binomial_c
  ENDINTERFACE

  rv = easy_ran_binomial_c(r%rng, p, n)

ENDFUNCTION easy_ran_binomial

!// C++11 allows only for integer n's, unlike GSL where n is double
!unsigned int easy_ran_negative_binomial (const easy_rng * r, double p, unsigned int n);
FUNCTION easy_ran_negative_binomial(r, p, n) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE), INTENT(IN) :: p
  INTEGER (C_INT), INTENT(IN) :: n
  INTEGER (C_INT) :: rv

  INTERFACE
    FUNCTION easy_ran_negative_binomial_c(r, p, n) BIND(C, NAME='easy_ran_negative_binomial') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE), VALUE, INTENT(IN) :: p
      INTEGER (C_INT), VALUE, INTENT(IN) :: n
      INTEGER (C_INT) :: rv
    ENDFUNCTION easy_ran_negative_binomial_c
  ENDINTERFACE

  rv = easy_ran_negative_binomial_c(r%rng, p, n)

ENDFUNCTION easy_ran_negative_binomial

!unsigned int easy_ran_geometric (const easy_rng * r, double p);
FUNCTION easy_ran_geometric(r, p) RESULT(rv)
  TYPE (easy_rng), INTENT(IN) :: r
  REAL (C_DOUBLE), INTENT(IN) :: p
  INTEGER (C_INT) :: rv

  INTERFACE
    FUNCTION easy_ran_geometric_c(r, p) BIND(C, NAME='easy_ran_geometric') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: r
      REAL (C_DOUBLE), VALUE, INTENT(IN) :: p
      INTEGER (C_INT) :: rv
    ENDFUNCTION easy_ran_geometric_c
  ENDINTERFACE

  rv = easy_ran_geometric_c(r%rng, p)

ENDFUNCTION easy_ran_geometric


ENDMODULE easyRNG
