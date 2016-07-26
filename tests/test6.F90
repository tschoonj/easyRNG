
#define NSAMPLES 10000000



SUBROUTINE assert(condition, line)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
IMPLICIT NONE

LOGICAL, INTENT(IN) :: condition
INTEGER, INTENT(IN) :: line

INTERFACE
  SUBROUTINE easy_exit(exit_status) BIND(C, NAME='exit')
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER (C_INT), INTENT(IN), VALUE :: exit_status
  ENDSUBROUTINE easy_exit
ENDINTERFACE

IF (condition .EQV. .FALSE.) THEN
  WRITE (error_unit, '(A,I4)') 'assert failure at line ', line
  CALL easy_exit(1_C_INT)
ENDIF

ENDSUBROUTINE assert

SUBROUTINE test_uniform(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_rng_uniform'

DO i=1, NSAMPLES
  val = easy_rng_uniform(rng)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - 0.5) < 1E-3, __LINE__)
CALL assert(ABS(stddev - SQRT(1.0/12.0)) < 1E-3, __LINE__)

ENDSUBROUTINE test_uniform

SUBROUTINE test_uniform_pos(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_rng_uniform_pos'

DO i=1, NSAMPLES
  val = easy_rng_uniform_pos(rng)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - 0.5) < 1E-3, __LINE__)
CALL assert(ABS(stddev - SQRT(1.0/12.0)) < 1E-3, __LINE__)

ENDSUBROUTINE test_uniform_pos

SUBROUTINE test_uniform_int(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_rng_uniform_int'

DO i=1, NSAMPLES
  val = REAL(easy_rng_uniform_int(rng, 10000_C_LONG), C_DOUBLE)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - 5000) < 10, __LINE__)
CALL assert(ABS(stddev - SQRT(10000.0**2/12.0)) < 10, __LINE__)

ENDSUBROUTINE test_uniform_int

SUBROUTINE test_gaussian(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_ran_gaussian'

DO i=1, NSAMPLES
  val = easy_ran_gaussian(rng, 5.0_C_DOUBLE)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - 0.0) < 1E-2, __LINE__)
CALL assert(ABS(stddev - 5.0) < 1E-2, __LINE__)

ENDSUBROUTINE test_gaussian

SUBROUTINE test_ugaussian(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_ran_ugaussian'

DO i=1, NSAMPLES
  val = easy_ran_ugaussian(rng)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - 0.0) < 1E-2, __LINE__)
CALL assert(ABS(stddev - 1.0) < 1E-2, __LINE__)

ENDSUBROUTINE test_ugaussian

SUBROUTINE test_exponential(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_ran_exponential'

DO i=1, NSAMPLES
  val = easy_ran_exponential(rng, 10.0_C_DOUBLE)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - 1.0/10.0) < 1E-2, __LINE__)
CALL assert(ABS(stddev - 1.0/10.0) < 1E-2, __LINE__)

ENDSUBROUTINE test_exponential

SUBROUTINE test_gamma(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_ran_gamma'

DO i=1, NSAMPLES
  val = easy_ran_gamma(rng, 10.0_C_DOUBLE, 5.0_C_DOUBLE)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - 50.0) < 1E-1, __LINE__)
CALL assert(ABS(stddev - SQRT(10.0 * 5.0 * 5.0)) < 1E-2, __LINE__)

ENDSUBROUTINE test_gamma

SUBROUTINE test_flat(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_ran_flat'

DO i=1, NSAMPLES
  val = easy_ran_flat(rng, 95.0_C_DOUBLE, 458.0_C_DOUBLE)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - (458.0 + 95.0)/2.0) < 1.0, __LINE__)
CALL assert(ABS(stddev - SQRT(1.0/12.0) * (458.0 - 95.0)) < 1.0, __LINE__)

ENDSUBROUTINE test_flat

SUBROUTINE test_lognormal(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev
REAL (C_DOUBLE), PARAMETER :: zeta = 0.5, sigma = 0.2

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_ran_lognormal'

DO i=1, NSAMPLES
  val = easy_ran_lognormal(rng, zeta, sigma)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - EXP(zeta + sigma * sigma / 2.0)) < 1.0, __LINE__)
CALL assert(ABS(stddev - SQRT((EXP(sigma * sigma) - 1.0) * EXP(2.0 * zeta + sigma * sigma) )) < 1.0, __LINE__)

ENDSUBROUTINE test_lognormal

PROGRAM test6
USE, INTRINSIC :: ISO_C_BINDING
USE :: easyRNG
IMPLICIT NONE

TYPE (easy_rng) :: rng

INTERFACE 
  FUNCTION easy_time(timer) BIND(C, NAME='time') RESULT(rv)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    TYPE (C_PTR), INTENT(IN), VALUE :: timer
    INTEGER (C_LONG) :: rv
  ENDFUNCTION easy_time
ENDINTERFACE

rng = easy_rng_alloc(easy_rng_mt19937)
CALL easy_rng_set(rng, easy_time(C_NULL_PTR))

CALL test_uniform(rng)
CALL test_uniform_pos(rng)
CALL test_uniform_int(rng)
CALL test_gaussian(rng)
CALL test_ugaussian(rng)
CALL test_exponential(rng)
CALL test_gamma(rng)
CALL test_flat(rng)
CALL test_lognormal(rng)


CALL easy_rng_free(rng)

ENDPROGRAM test6
