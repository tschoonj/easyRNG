

MODULE easyRNG

USE, INTRINSIC :: ISO_C_BINDING
USE, INTRINSIC :: ISO_FORTRAN_ENV

IMPLICIT NONE

TYPE, BIND(C) :: easy_rng_type
  PRIVATE
  TYPE (C_PTR) :: rng_type
ENDTYPE easy_rng_type

TYPE :: easy_rng
  PRIVATE
  TYPE (C_PTR) :: rng
ENDTYPE easy_rng

TYPE :: easy_ran_discrete_t
  PRIVATE
  TYPE (C_PTR) :: ran_discrete_t
ENDTYPE

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
  TYPE (C_PTR) :: types_c
  TYPE (C_PTR), POINTER, DIMENSION(:) :: types_c_all
  TYPE (easy_rng_type), POINTER, DIMENSION(:) :: rv
  INTEGER :: length, i

  INTERFACE
    FUNCTION easy_rng_types_setup_c() BIND(C, NAME='easy_rng_types_setup') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR) :: rv
    ENDFUNCTION easy_rng_types_setup_c
  ENDINTERFACE

  length = 0

  types_c = easy_rng_types_setup_c()

  ! start by assuming a very, very long array
  CALL C_F_POINTER(types_c, types_c_all, [1024])
  DO i=1, 1024
    IF (.NOT. C_ASSOCIATED(types_c_all(i))) THEN
      EXIT
    ENDIF
    length = length + 1
  ENDDO

  CALL C_F_POINTER(types_c, rv, [length])

ENDFUNCTION easy_rng_types_setup

!const easy_rng_type * easy_rng_env_setup (void);
FUNCTION easy_rng_env_setup() RESULT(rv)
  TYPE (easy_rng_type) :: rv
 
  INTERFACE
    FUNCTION easy_rng_env_setup_c() BIND(C, NAME='easy_rng_env_setup') RESULT(rv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      TYPE (C_PTR) :: rv
    ENDFUNCTION easy_rng_env_setup_c
  ENDINTERFACE

  rv%rng_type = easy_rng_env_setup_c()

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
