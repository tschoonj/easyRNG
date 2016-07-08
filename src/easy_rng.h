

#ifndef EASY_RNG_H
#define EASY_RNG_H

#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

struct _easy_rng;
typedef struct _easy_rng easy_rng;

typedef struct {
	const char *name;
	unsigned long int max;
	unsigned long int min;
} easy_rng_type;

easy_rng * easy_rng_alloc (const easy_rng_type * T);

void easy_rng_set (const easy_rng * r, unsigned long int s);

void easy_rng_free (easy_rng * r);

unsigned long int easy_rng_get (const easy_rng * r);

double easy_rng_uniform (const easy_rng * r);

double easy_rng_uniform_pos (const easy_rng * r);

unsigned long int easy_rng_uniform_int (const easy_rng * r, unsigned long int n);

const char * easy_rng_name (const easy_rng * r);

unsigned long int easy_rng_max (const easy_rng * r);

unsigned long int easy_rng_min (const easy_rng * r);

/* Not going to happen...
void * easy_rng_state (const easy_rng * r);

size_t easy_rng_size (const easy_rng * r);
*/

const easy_rng_type ** easy_rng_types_setup (void);

// TODO
const easy_rng_type * easy_rng_env_setup (void);

int easy_rng_memcpy (easy_rng * dest, const easy_rng * src);

easy_rng * easy_rng_clone (const easy_rng * r);

int easy_rng_equal(const easy_rng * ra, const easy_rng *rb);

int easy_rng_fwrite (FILE * stream, const easy_rng * r);

int easy_rng_fread (FILE * stream, easy_rng * r);

extern const easy_rng_type *easy_rng_minstd_rand0;
extern const easy_rng_type *easy_rng_minstd_rand;
extern const easy_rng_type *easy_rng_mt19937;
extern const easy_rng_type *easy_rng_mt19937_64;
extern const easy_rng_type *easy_rng_ranlux24_base;
extern const easy_rng_type *easy_rng_ranlux48_base;
extern const easy_rng_type *easy_rng_ranlux24;
extern const easy_rng_type *easy_rng_ranlux48;
extern const easy_rng_type *easy_rng_knuth_b;
extern const easy_rng_type *easy_rng_default;


extern unsigned long int easy_rng_default_seed;


#ifdef __cplusplus
}
#endif


#endif
