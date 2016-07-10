#include <gsl/gsl_rng.h>
#include <easy_rng.h>
#include <time.h>

#define NRUNS 1000000000

void run_test(const gsl_rng_type *gtype, const easy_rng_type *etype) {
	gsl_rng *grng = gsl_rng_alloc(gtype);
	easy_rng *erng = easy_rng_alloc(etype);
	
	clock_t gstart, gend;
	clock_t estart, eend;
	int i;

	gstart = clock();
	
	for (i = 0 ; i < NRUNS ; i++)
		gsl_rng_get(grng);

	gend = clock();

	estart = clock();
	
	for (i = 0 ; i < NRUNS ; i++)
		easy_rng_get(erng);

	eend = clock();

	gsl_rng_free(grng);
	easy_rng_free(erng);
	
	fprintf(stdout, "Comparing easyRNG's %s with GSL's %s\n", etype->name, gtype->name);
	fprintf(stdout, "easyRNG %g\n", ((double) (eend - estart))/CLOCKS_PER_SEC);
	fprintf(stdout, "GSL %g\n", ((double) (gend - gstart))/CLOCKS_PER_SEC);
}


int main(int argc, char *argv[]) {

	run_test(gsl_rng_mt19937, easy_rng_mt19937);
	run_test(gsl_rng_ranlux, easy_rng_ranlux24);
	run_test(gsl_rng_ranlux389, easy_rng_ranlux48);
	run_test(gsl_rng_minstd, easy_rng_minstd_rand0);
	run_test(gsl_rng_fishman20, easy_rng_minstd_rand);

	return 0;
}


