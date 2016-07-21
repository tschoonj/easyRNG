PROGRAM test4

USE :: easyRNG
USE, INTRINSIC :: ISO_FORTRAN_ENV

IMPLICIT NONE

! exit is a commonly found extension but I want this example to be fully standard compliant...
INTERFACE
  SUBROUTINE easy_exit(exit_status) BIND(C, NAME='exit')
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER (C_INT), INTENT(IN), VALUE :: exit_status
  ENDSUBROUTINE easy_exit
ENDINTERFACE


TYPE (easy_rng) :: rng
TYPE (easy_rng) :: rng_clone
INTEGER (C_LONG) :: val
INTEGER :: i
TYPE (easy_rng_type), POINTER, DIMENSION(:) :: all_types

all_types => easy_rng_types_setup()

DO i=1,SIZE(all_types)

  rng = easy_rng_alloc(all_types(i))

  CALL easy_rng_set(rng, 1234_C_LONG)

  WRITE(output_unit, '(A, I25)') 'get: ', easy_rng_get(rng)
  WRITE(output_unit, '(A, ES15.6)') 'uniform: ', easy_rng_uniform(rng)
  WRITE(output_unit, '(A, ES15.6)') 'uniform_pos: ', easy_rng_uniform_pos(rng)
  WRITE(output_unit, '(A, I3)') 'uniform_int: ', easy_rng_uniform_int(rng, 10_8)
  WRITE(output_unit, '(A, A)') 'name: ', easy_rng_name(rng)
  WRITE(output_unit, '(A, I25)') 'min: ', easy_rng_min(rng)
  WRITE(output_unit, '(A, I25)') 'max: ', easy_rng_max(rng)

  ! test cloning
  rng_clone = easy_rng_clone(rng)

  IF (easy_rng_equal(rng, rng_clone) .EQ. 0) THEN
    CALL easy_exit(1)
  ENDIF

  val = easy_rng_get(rng_clone)

  IF (easy_rng_equal(rng, rng_clone) .EQ. 1) THEN
    CALL easy_exit(1)
  ENDIF

  CALL easy_rng_free(rng)
  CALL easy_rng_free(rng_clone)

ENDDO

ENDPROGRAM test4
