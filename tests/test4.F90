PROGRAM test4

USE :: easyRNG

TYPE (easy_rng) :: rng
INTEGER (C_LONG) :: val

rng = easy_rng_alloc(easy_rng_mt19937)

CALL easy_rng_set(rng, 1234_C_LONG)

val = easy_rng_get(rng)

CALL easy_rng_free(rng)

ENDPROGRAM test4
