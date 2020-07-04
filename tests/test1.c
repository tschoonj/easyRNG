#include "easy_rng.h"
#include <stdio.h>
#include <assert.h>

#ifdef _MSC_VER
#include <io.h>
#define unlink _unlink
#else
#include <unistd.h>
#endif

int main(int argc, char *argv[]) {
	int i;
	const easy_rng_type **all_types = easy_rng_types_setup();
	fprintf(stdout, "All available RNGs:\n");
	for (i = 0 ; all_types[i] != NULL ; i++) {
		fprintf(stdout, "\tname %i: %s\n", i, all_types[i]->name);

		easy_rng *rng = easy_rng_alloc(all_types[i]);

		easy_rng_set(rng, 1234);

		fprintf(stdout, "get: %lu\n", easy_rng_get(rng));
		fprintf(stdout, "uniform: %g\n", easy_rng_uniform(rng));
		fprintf(stdout, "uniform_pos: %g\n", easy_rng_uniform_pos(rng));
		fprintf(stdout, "uniform_int: %lu\n", easy_rng_uniform_int(rng, 10));
		fprintf(stdout, "name: %s\n", easy_rng_name(rng));
		fprintf(stdout, "min: %lu\n", easy_rng_min(rng));
		fprintf(stdout, "max: %lu\n", easy_rng_max(rng));

		// check cloning
		easy_rng *rng_clone = easy_rng_clone(rng);
		assert(easy_rng_equal(rng, rng_clone));
		easy_rng_get(rng_clone);
		assert(!easy_rng_equal(rng, rng_clone));
		easy_rng_free(rng_clone);

		// check writing state to file and reading it back in
		FILE *file;
		if ((file = fopen("test.txt", "wb")) == NULL) {
			return 1;
		}
		easy_rng_fwrite(file, rng);
		fclose(file);

		rng_clone = easy_rng_alloc(all_types[i]);
		if ((file = fopen("test.txt", "rb")) == NULL) {
			return 1;
		}
		easy_rng_fread(file, rng_clone);
		fclose(file);
		assert(easy_rng_equal(rng, rng_clone));
		unlink("test.txt");

		easy_rng_free(rng);
		easy_rng_free(rng_clone);
	}
	return 0;
}
