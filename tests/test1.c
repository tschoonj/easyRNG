#include "easy_rng.h"
#include <stdio.h>

int main(int argc, char *argv[]) {
	easy_rng *rng = easy_rng_alloc(easy_rng_mt19937);

	easy_rng_set(rng, 1234);

	fprintf(stdout, "get: %lu\n", easy_rng_get(rng));
	fprintf(stdout, "uniform: %g\n", easy_rng_uniform(rng));
	fprintf(stdout, "uniform_pos: %g\n", easy_rng_uniform_pos(rng));
	fprintf(stdout, "uniform_int: %lu\n", easy_rng_uniform_int(rng, 10));

	easy_rng_free(rng);

	return 0;
}
