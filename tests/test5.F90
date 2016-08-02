#ifndef NRUNS
#define NRUNS 10000000
#endif

SUBROUTINE run_test(ftype, etype)
  USE, INTRINSIC :: ISO_C_BINDING
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE :: fgsl
  USE :: easyRNG
  IMPLICIT NONE

  TYPE (fgsl_rng_type), INTENT(INOUT) :: ftype
  TYPE (easy_rng_type), INTENT(IN) :: etype
  TYPE (fgsl_rng) :: frng
  TYPE (easy_rng) :: erng
  REAL (C_DOUBLE) :: fstart, fend
  REAL (C_DOUBLE) :: estart, eend
  INTEGER (C_LONG) :: i
  INTEGER (C_LONG) :: rng_rv

  frng = fgsl_rng_alloc(ftype)
  erng = easy_rng_alloc(etype)

  CALL CPU_TIME(fstart)
  DO i=1,NRUNS
    rng_rv = fgsl_rng_get(frng)
  ENDDO
  CALL CPU_TIME(fend)

  CALL CPU_TIME(estart)
  DO i=1,NRUNS
    rng_rv = easy_rng_get(erng)
  ENDDO
  CALL CPU_TIME(eend)

  WRITE (output_unit, '(A,A,A,A)') 'Comparing easyRNGs ', TRIM(easy_rng_name(erng)), &
    ' with FGSLs ', TRIM(fgsl_rng_name(frng))
  WRITE (output_unit, '(A,F10.3)') 'easyRNG ', eend - estart
  WRITE (output_unit, '(A,F10.3)') 'FGSL ', fend - fstart
  CALL fgsl_rng_free(frng)
  CALL easy_rng_free(erng)

ENDSUBROUTINE run_test


PROGRAM test5
  USE :: fgsl
  USE :: easyRNG
  IMPLICIT NONE

  CALL run_test(fgsl_rng_mt19937, easy_rng_mt19937)
  CALL run_test(fgsl_rng_ranlux, easy_rng_ranlux24)
  CALL run_test(fgsl_rng_ranlux389, easy_rng_ranlux48)
  CALL run_test(fgsl_rng_minstd, easy_rng_minstd_rand0)
  CALL run_test(fgsl_rng_fishman20, easy_rng_minstd_rand)
ENDPROGRAM test5


