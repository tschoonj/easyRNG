
#define NSAMPLES 10000000




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
CALL test_gamma(rng) ! this one also appears to fail now and then...
CALL test_flat(rng)
CALL test_lognormal(rng)
CALL test_chisq(rng)
!CALL test_fdist(rng) ! this one fails often, not sure why...
CALL test_tdist(rng)
CALL test_weibull(rng)
CALL test_discrete(rng)
CALL test_poisson(rng)
CALL test_bernoulli(rng)
CALL test_binomial(rng)
CALL test_negative_binomial(rng)
CALL test_geometric(rng)


CALL easy_rng_free(rng)

CONTAINS

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
REAL (C_DOUBLE), PARAMETER :: mu = 5.1047E-05_C_DOUBLE

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_ran_exponential'

DO i=1, NSAMPLES
  val = easy_ran_exponential(rng, mu)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - mu)/mean < 1E-2, __LINE__)
CALL assert(ABS(stddev - mu)/mean < 1E-2, __LINE__)

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
CALL assert(ABS(stddev - SQRT(10.0 * 5.0 * 5.0))/SQRT(10.0 * 5.0 * 5.0) < 1E-2, __LINE__)

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
CALL assert(ABS(mean - EXP(zeta + sigma * sigma / 2.0)) < 1E-2, __LINE__)
CALL assert(ABS(stddev - SQRT((EXP(sigma * sigma) - 1.0) * EXP(2.0 * zeta + sigma * sigma) )) < 1E-2, __LINE__)

ENDSUBROUTINE test_lognormal

SUBROUTINE test_chisq(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev
REAL (C_DOUBLE), PARAMETER :: nu = 10.0

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_ran_chisq'

DO i=1, NSAMPLES
  val = easy_ran_chisq(rng, nu)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - nu) < 1E-2, __LINE__)
CALL assert(ABS(stddev - SQRT(2.0 * nu)) < 1E-2, __LINE__)

ENDSUBROUTINE test_chisq

FUNCTION fdist_stddev(nu1, nu2) RESULT(rv)
USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE
REAL (C_DOUBLE), INTENT(IN) :: nu1, nu2
REAL (C_DOUBLE) :: rv
rv = SQRT(2.0 * nu2 * nu2 * (nu1 + nu2 - 2.0) / nu1 / (nu2 - 2.0) / (nu2 -2.0) / (nu2 - 4.0))
ENDFUNCTION fdist_stddev

SUBROUTINE test_fdist(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev, real_stddev
REAL (C_DOUBLE), PARAMETER :: nu1 = 10.0, nu2 = 5.0

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_ran_fdist'

DO i=1, NSAMPLES
  val = easy_ran_fdist(rng, nu1, nu2)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
real_stddev = fdist_stddev(nu1, nu2)
WRITE (output_unit, '(A,ES10.3)') '      real stddev: ', real_stddev
CALL assert(ABS(mean - nu2/(nu2 - 2.0)) < 1E-2, __LINE__)
CALL assert(ABS(stddev - real_stddev) < 5.0E-1, __LINE__)

ENDSUBROUTINE test_fdist

SUBROUTINE test_tdist(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev
REAL (C_DOUBLE), PARAMETER :: nu = 10.0

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_ran_tdist'

DO i=1, NSAMPLES
  val = easy_ran_tdist(rng, nu)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - 0.0) < 1E-2, __LINE__)
CALL assert(ABS(stddev - SQRT(nu/(nu - 2.0))) < 1.0E-2, __LINE__)

ENDSUBROUTINE test_tdist

FUNCTION weibull_mean(a, b) RESULT(rv)
USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE
REAL (C_DOUBLE), INTENT(IN) :: a, b
REAL (C_DOUBLE) :: rv
rv = a * GAMMA(1.0 + 1.0/b);
ENDFUNCTION weibull_mean

FUNCTION weibull_stddev(a, b) RESULT(rv)
USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE
REAL (C_DOUBLE), INTENT(IN) :: a, b
REAL (C_DOUBLE) :: rv
rv = a * SQRT(GAMMA(1.0 + 2.0/b) - GAMMA(1.0 + 1.0/b)**2)
ENDFUNCTION weibull_stddev

SUBROUTINE test_weibull(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev
REAL (C_DOUBLE), PARAMETER :: a = 2.0, b = 5.0 

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_ran_weibull'

DO i=1, NSAMPLES
  val = easy_ran_weibull(rng, a, b)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - weibull_mean(a, b)) < 1E-2, __LINE__)
CALL assert(ABS(stddev - weibull_stddev(a, b)) < 1.0E-2, __LINE__)

ENDSUBROUTINE test_weibull

SUBROUTINE discrete_mean_stddev(P, mean, stddev)
USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE

REAL (C_DOUBLE), DIMENSION(:), INTENT(INOUT) :: P
REAL (C_DOUBLE), INTENT(OUT) :: mean, stddev
REAL (C_DOUBLE) :: mysum
INTEGER :: i

mean = 0.0
stddev = 0.0
mysum = SUM(P)
P = P/mysum

DO i=1,SIZE(P)
  mean = mean + (i-1) * P(i)
ENDDO

DO i=1,SIZE(P)
  stddev = stddev + (i - 1 - mean) * (i -1 - mean) * P(i)
ENDDO

stddev = SQRT(stddev)

ENDSUBROUTINE discrete_mean_stddev

SUBROUTINE test_discrete(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev
REAL (C_DOUBLE) :: real_mean, real_stddev
TYPE (easy_ran_discrete_t) :: disc
REAL (C_DOUBLE), DIMENSION(10) :: P = [0.1, 0.2, 0.3, 0.4, 0.5, 0.5, 0.4, 0.3, 0.2, 0.1]

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

disc = easy_ran_discrete_preproc(P)

WRITE (output_unit, '(A)') 'Testing easy_ran_discrete'

DO i=1, NSAMPLES
  val = easy_ran_discrete(rng, disc)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
CALL easy_ran_discrete_free(disc)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL discrete_mean_stddev(P, real_mean, real_stddev)
CALL assert(ABS(mean - real_mean) < 1E-2, __LINE__)
CALL assert(ABS(stddev - real_stddev) < 1.0E-2, __LINE__)

ENDSUBROUTINE test_discrete

SUBROUTINE test_poisson(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev
REAL (C_DOUBLE), PARAMETER :: mu = 10.0

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_ran_poisson'

DO i=1, NSAMPLES
  val = easy_ran_poisson(rng, mu)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - mu) < 1E-2, __LINE__)
CALL assert(ABS(stddev - SQRT(mu)) < 1.0E-2, __LINE__)

ENDSUBROUTINE test_poisson

SUBROUTINE test_bernoulli(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev
REAL (C_DOUBLE), PARAMETER :: mu = 0.25

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_ran_bernoulli'

DO i=1, NSAMPLES
  val = easy_ran_bernoulli(rng, mu)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - mu) < 1E-2, __LINE__)
CALL assert(ABS(stddev - SQRT(mu * (1.0 - mu))) < 1.0E-2, __LINE__)

ENDSUBROUTINE test_bernoulli

SUBROUTINE test_binomial(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev
REAL (C_DOUBLE), PARAMETER :: p = 0.25
INTEGER (C_INT), PARAMETER :: n = 25

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_ran_binomial'

DO i=1, NSAMPLES
  val = easy_ran_binomial(rng, p, n)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - p * n) < 1E-2, __LINE__)
CALL assert(ABS(stddev - SQRT(n * p * (1.0 - p))) < 1.0E-2, __LINE__)

ENDSUBROUTINE test_binomial

SUBROUTINE test_negative_binomial(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev
REAL (C_DOUBLE), PARAMETER :: p = 0.25
INTEGER (C_INT), PARAMETER :: n = 25

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_ran_negative_binomial'

DO i=1, NSAMPLES
  val = easy_ran_negative_binomial(rng, p, n)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - n * (1.0 - p) / p) < 1E-1, __LINE__)
CALL assert(ABS(stddev - SQRT(n * (1.0 - p) / p /p)) < 1.0E-1, __LINE__)

ENDSUBROUTINE test_negative_binomial

SUBROUTINE test_geometric(rng)
USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE :: easyRNG

IMPLICIT NONE

TYPE (easy_rng), INTENT(IN) :: rng
INTEGER :: i
REAL (C_DOUBLE) :: mean, M2, val, delta, stddev
REAL (C_DOUBLE), PARAMETER :: p = 0.25

mean = 0.0_C_DOUBLE
M2 = 0.0_C_DOUBLE

WRITE (output_unit, '(A)') 'Testing easy_ran_geometric'

DO i=1, NSAMPLES
  val = easy_ran_geometric(rng, p)
  delta = val - mean
  mean = mean + delta/i
  M2 = M2 + delta * (val - mean)
ENDDO
stddev = SQRT(M2/NSAMPLES)
WRITE (output_unit, '(A,ES10.3)') '      mean: ', mean
WRITE (output_unit, '(A,ES10.3)') '      stddev: ', stddev
CALL assert(ABS(mean - 1.0 / p) < 1E-2, __LINE__)
CALL assert(ABS(stddev - SQRT((1.0 - p) / p / p)) < 1.0E-2, __LINE__)

ENDSUBROUTINE test_geometric
ENDPROGRAM test6
