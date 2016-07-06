#include "easy_rng.h"
#include <stdio.h>

int main(int argc, char *argv[]) {
	easy_rng *rng = easy_rng_alloc(easy_rng_mt19937);

	easy_rng_set(rng, 1234);

	fprintf(stdout, "value: %lu\n", easy_rng_get(rng));

	easy_rng_free(rng);

	return 0;
}
