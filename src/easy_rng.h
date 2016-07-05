

#ifndef EASY_RNG_H
#define EASY_RNG_H

#ifdef __cplusplus
extern "C" {
#endif

struct _easy_rng;
typedef struct _easy_rng easy_rng;

struct _easy_rng_type;
typedef struct _easy_rng_type easy_rng_type;

easy_rng * easy_rng_alloc (const easy_rng_type * T);

void easy_rng_set (const easy_rng * r, unsigned long int s);

void easy_rng_free (easy_rng * r);

unsigned long int easy_rng_get (const easy_rng * r);

double easy_rng_uniform (const easy_rng * r);

double easy_rng_uniform_pos (const easy_rng * r);

unsigned long int easy_rng_uniform_int (const easy_rng * r, unsigned long int n);

extern const easy_rng_type *minstd_rand0;
extern const easy_rng_type *minstd_rand;
extern const easy_rng_type *mt19937;
extern const easy_rng_type *mt19937_64;
extern const easy_rng_type *ranlux24_base;
extern const easy_rng_type *ranlux48_base;
extern const easy_rng_type *ranlux24;
extern const easy_rng_type *ranlux48;
extern const easy_rng_type *knuth_b;



#ifdef __cplusplus
}
#endif


#endif
