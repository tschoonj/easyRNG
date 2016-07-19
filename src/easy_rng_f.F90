

MODULE easyRNG

USE, INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

TYPE, BIND(C) :: easy_rng_type
  PRIVATE
  TYPE (C_PTR) :: rng_type
ENDTYPE easy_rng_type

TYPE :: easy_rng
  PRIVATE
  TYPE (C_PTR) :: rng
ENDTYPE easy_rng

TYPE (easy_rng_type), BIND(C, NAME='easy_rng_minstd_rand0') :: easy_rng_minstd_rand0
TYPE (easy_rng_type), BIND(C, NAME='easy_rng_minstd_rand') :: easy_rng_minstd_rand
TYPE (easy_rng_type), BIND(C, NAME='easy_rng_mt19937') :: easy_rng_mt19937
TYPE (easy_rng_type), BIND(C, NAME='easy_rng_mt19937_64') :: easy_rng_mt19937_64
TYPE (easy_rng_type), BIND(C, NAME='easy_rng_ranlux24_base') :: easy_rng_ranlux24_base
TYPE (easy_rng_type), BIND(C, NAME='easy_rng_ranlux48_base') :: easy_rng_ranlux48_base
TYPE (easy_rng_type), BIND(C, NAME='easy_rng_ranlux24') :: easy_rng_ranlux24
TYPE (easy_rng_type), BIND(C, NAME='easy_rng_ranlux48') :: easy_rng_ranlux48
TYPE (easy_rng_type), BIND(C, NAME='easy_rng_knuth_b') :: easy_rng_knuth_b
TYPE (easy_rng_type), BIND(C, NAME='easy_rng_default') :: easy_rng_default

CONTAINS

!easy_rng * easy_rng_alloc (const easy_rng_type * T);
FUNCTION easy_rng_alloc(T) RESULT(rv)
  TYPE (easy_rng_type), INTENT(IN) :: T
  TYPE (easy_rng) :: rv

  INTERFACE
    FUNCTION easy_rng_alloc_c(T) BIND(C, NAME='easy_rng_alloc') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR), VALUE, INTENT(IN) :: T
      TYPE (C_PTR) :: rv
    ENDFUNCTION easy_rng_alloc_c
  ENDINTERFACE

  rv%rng = easy_rng_alloc_c(T%rng_type)

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
ENDMODULE easyRNG
