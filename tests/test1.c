#include "easy_rng.h"
#include <stdio.h>

int main(int argc, char *argv[]) {
	easy_rng *rng = easy_rng_alloc(easy_rng_mt19937);

	easy_rng_set(rng, 1234);

	fprintf(stdout, "get: %lu\n", easy_rng_get(rng));
	fprintf(stdout, "uniform: %g\n", easy_rng_uniform(rng));
	fprintf(stdout, "uniform_pos: %g\n", easy_rng_uniform_pos(rng));
	fprintf(stdout, "uniform_int: %lu\n", easy_rng_uniform_int(rng, 10));
	fprintf(stdout, "name: %s\n", easy_rng_name(rng));
	fprintf(stdout, "min: %lu\n", easy_rng_min(rng));
	fprintf(stdout, "max: %lu\n", easy_rng_max(rng));
	int i;
	const easy_rng_type **all_types = easy_rng_types_setup();
	fprintf(stdout, "All available RNGs:\n");
	for (int i = 0 ; all_types[i] != NULL ; i++)
		fprintf(stdout, "\tname %i: %s\n", i, all_types[i]->name);

	easy_rng_free(rng);

	return 0;
}
