#include "easy_randist.h"
#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <time.h>

#define NSAMPLES 10000000

void test_uniform(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	fprintf(stdout, "Testing easy_rng_uniform\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = easy_rng_uniform(rng);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	assert(fabs(mean - 0.5) < 1E-3);
	assert(fabs(stddev - sqrt(1.0/12.0)) < 1E-3);
}

void test_uniform_pos(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	fprintf(stdout, "Testing easy_rng_uniform_pos\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = easy_rng_uniform_pos(rng);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	assert(fabs(mean - 0.5) < 1E-3);
	assert(fabs(stddev - sqrt(1.0/12.0)) < 1E-3);
}

void test_uniform_int(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	fprintf(stdout, "Testing easy_rng_uniform_int\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = (double) easy_rng_uniform_int(rng, 10000);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	assert(fabs(mean - 5000) < 10);
	assert(fabs(stddev - sqrt(10000.0 * 10000.0/12.0)) < 10);
}

void test_gaussian(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	fprintf(stdout, "Testing easy_ran_gaussian\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = easy_ran_gaussian(rng, 5.0);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	assert(fabs(mean - 0.0) < 1E-2);
	assert(fabs(stddev - 5.0) < 1E-2);
}

void test_ugaussian(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	fprintf(stdout, "Testing easy_ran_ugaussian\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = easy_ran_ugaussian(rng);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	assert(fabs(mean - 0.0) < 1E-2);
	assert(fabs(stddev - 1.0) < 1E-2);
}

void test_exponential(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;
	double mu = 5.1047E-05;

	fprintf(stdout, "Testing easy_ran_exponential\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = easy_ran_exponential(rng, mu);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	assert(fabs(mean - mu)/mean < 1E-2);
	assert(fabs(stddev - mu)/mean < 1E-2);
}

void test_gamma(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	fprintf(stdout, "Testing easy_ran_gamma\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = easy_ran_gamma(rng, 10.0, 5.0);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	assert(fabs(mean - 50.0) < 1E-1);
	assert(fabs(stddev - sqrt(10.0 * 5.0 * 5.0)) < 1E-1);
}

void test_flat(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	fprintf(stdout, "Testing easy_ran_flat\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = easy_ran_flat(rng, 95.0, 458.0);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	assert(fabs(mean - (458.0 + 95.0)/2.0) < 1);
	assert(fabs(stddev - sqrt(1/12.0) * (458.0 - 95.0)) < 1);
}

void test_lognormal(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	double zeta = 0.5, sigma = 0.2;

	fprintf(stdout, "Testing easy_ran_lognormal\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = easy_ran_lognormal(rng, zeta, sigma);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	assert(fabs(mean - exp(zeta + sigma * sigma / 2.0)) < 1E-2);
	assert(fabs(stddev - sqrt((exp(sigma * sigma) - 1.0) * exp(2.0 * zeta + sigma * sigma) )) < 1.0E-2);
}

void test_chisq(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	double nu = 10.0;

	fprintf(stdout, "Testing easy_ran_chisq\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = easy_ran_chisq(rng, nu);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	assert(fabs(mean - nu) < 1E-2);
	assert(fabs(stddev - sqrt(2.0 * nu)) < 1.0E-2);
}

double fdist_stddev(double nu1, double nu2) {
	return sqrt( 2.0 * nu2 * nu2 * (nu1 + nu2 - 2.0) / nu1 / (nu2 - 2.0) / (nu2 -2.0) / (nu2 - 4.0));
}

void test_fdist(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	double nu1 = 10.0, nu2 = 5.0;

	fprintf(stdout, "Testing easy_ran_fdist\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = easy_ran_fdist(rng, nu1, nu2);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	double real_stddev = fdist_stddev(nu1, nu2);
	fprintf(stdout, "\treal stddev: %g\n", real_stddev);
	assert(fabs(mean - nu2/(nu2 - 2)) < 1E-2);
	assert(fabs(stddev - real_stddev) < 5.0E-1);
}

void test_tdist(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	double nu = 10.0;

	fprintf(stdout, "Testing easy_ran_tdist\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = easy_ran_tdist(rng, nu);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	assert(fabs(mean - 0.0) < 1E-2);
	assert(fabs(stddev - sqrt(nu/(nu - 2.0))) < 1E-2);
}

double weibull_mean(double a, double b) {
	return a * tgamma(1.0 + 1.0/b);
}

double weibull_stddev(double a, double b) {
	return a * sqrt(tgamma(1.0 + 2.0/b) - tgamma(1.0 + 1.0/b) * tgamma(1.0 + 1.0/b));
}

void test_weibull(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	double a = 2.0, b = 5.0;

	fprintf(stdout, "Testing easy_ran_weibull\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = easy_ran_weibull(rng, a, b);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	double real_mean = weibull_mean(a, b);
	double real_stddev = weibull_stddev(a, b);
	//fprintf(stdout, "\treal mean: %g\n", real_mean);
	//fprintf(stdout, "\treal stddev: %g\n", real_stddev);
	assert(fabs(mean - real_mean) < 1E-2);
	assert(fabs(stddev - real_stddev) < 1E-2);
}

void discrete_mean_stddev(size_t N, double *P, double *mean, double *stddev) {
	// first normalize the array
	double sum = 0.0;
	int i = 0;
	for (i = 0 ; i < N ; i++) {
		sum += P[i];
	}
	for (i = 0 ; i < N ; i++) {
		P[i] /= sum;
	}
	*mean = 0.0;
	for (i = 0 ; i < N ; i++) {
		*mean += i * P[i];
	}
	*stddev = 0.0;
	for (i = 0 ; i < N ; i++) {
		*stddev += (i - *mean) * (i - *mean) * P[i];
	}
	*stddev = sqrt(*stddev);
}

void test_discrete(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	double P[10] = {0.1, 0.2, 0.3, 0.4, 0.5, 0.5, 0.4, 0.3, 0.2, 0.1};

	easy_ran_discrete_t * disc = easy_ran_discrete_preproc(10, P);

	fprintf(stdout, "Testing easy_ran_discrete\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = (double) easy_ran_discrete(rng, disc);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	easy_ran_discrete_free(disc);
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	double real_mean, real_stddev;
	discrete_mean_stddev(10, P, &real_mean, &real_stddev);
	//fprintf(stdout, "\treal mean: %g\n", real_mean);
	//fprintf(stdout, "\treal stddev: %g\n", real_stddev);
	assert(fabs(mean - real_mean) < 1E-2);
	assert(fabs(stddev - real_stddev) < 1E-2);
}

void test_poisson(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	double mu = 10.0;

	fprintf(stdout, "Testing easy_ran_poisson\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = easy_ran_poisson(rng, mu);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	assert(fabs(mean - mu) < 1E-2);
	assert(fabs(stddev - sqrt(mu)) < 1E-2);
}

void test_bernoulli(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	double mu = 0.25;

	fprintf(stdout, "Testing easy_ran_bernoulli\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = easy_ran_bernoulli(rng, mu);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	assert(fabs(mean - mu) < 1E-2);
	assert(fabs(stddev - sqrt(mu*(1.0-mu))) < 1E-2);
}

void test_binomial(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	double p = 0.25;
	unsigned int n = 25;

	fprintf(stdout, "Testing easy_ran_binomial\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = easy_ran_binomial(rng, p, n);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	assert(fabs(mean - p * n) < 1E-2);
	assert(fabs(stddev - sqrt(n * p *(1.0-p))) < 1E-2);
}

void test_negative_binomial(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	double p = 0.25;
	unsigned int n = 25;

	fprintf(stdout, "Testing easy_ran_negative_binomial\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = easy_ran_negative_binomial(rng, p, n);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	assert(fabs(mean - n * (1.0 - p) / p) < 1E-1);
	assert(fabs(stddev - sqrt(n * (1.0 - p) / p / p)) < 1E-1);
}

void test_geometric(easy_rng *rng) {
	int i;
	double mean = 0.0, M2 = 0.0;

	double p = 0.25;

	fprintf(stdout, "Testing easy_ran_geometric\n");

	for (i = 0 ; i < NSAMPLES ; i++) {
		double val = easy_ran_geometric(rng, p);
		double delta = val - mean;
		mean += delta/(i + 1);
		M2 += delta * (val - mean);
	}
	double stddev = sqrt(M2/NSAMPLES);
	fprintf(stdout, "\tmean: %g\n", mean);
	fprintf(stdout, "\tstddev: %g\n", stddev);
	assert(fabs(mean - 1.0/p) < 1E-2);
	assert(fabs(stddev - sqrt((1.0 - p) / p / p)) < 1E-2);
}

int main(int argc, char *argv[]) {

	easy_rng *rng = easy_rng_alloc(easy_rng_mt19937);
	// ensure we get a different seed every time...
	easy_rng_set(rng, (long unsigned int) time(NULL));

	// run some checks on the distributions
	test_uniform(rng);
	test_uniform_pos(rng);
	test_uniform_int(rng);
	test_gaussian(rng);
	test_ugaussian(rng);
	test_exponential(rng);
	//test_cauchy(); // mean and stddev are undefined here
	test_gamma(rng);
	test_flat(rng);
	test_lognormal(rng);
	test_chisq(rng);
	test_fdist(rng);
	test_tdist(rng);
	test_weibull(rng);
	test_discrete(rng);
	test_poisson(rng);
	test_bernoulli(rng);
	test_binomial(rng);
	test_negative_binomial(rng);
	test_geometric(rng);

	easy_rng_free(rng);
	
	return 0;
}
