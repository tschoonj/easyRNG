#ifndef EASY_RANDIST_H
#define EASY_RANDIST_H

#include "easy_rng.h"

#ifdef __cplusplus
extern "C" {
#endif

double easy_ran_gaussian (const easy_rng * r, double sigma);
double easy_ran_gaussian_ziggurat (const easy_rng * r, double sigma);
double easy_ran_gaussian_ratio_method (const easy_rng * r, double sigma);

double easy_ran_ugaussian (const easy_rng * r);
double easy_ran_ugaussian_ratio_method (const easy_rng * r);

double easy_ran_exponential (const easy_rng * r, double mu);

double easy_ran_cauchy (const easy_rng * r, double a);

double easy_ran_gamma (const easy_rng * r, double a, double b);

double easy_ran_flat (const easy_rng * r, double a, double b);

double easy_ran_lognormal (const easy_rng * r, double zeta, double sigma);

double easy_ran_chisq (const easy_rng * r, double nu);

double easy_ran_fdist (const easy_rng * r, double nu1, double nu2);

double easy_ran_tdist (const easy_rng * r, double nu);

double easy_ran_weibull (const easy_rng * r, double a, double b);

// Thought that std::extreme_value_distribution was matching Gumbel Type 1, but that looks incorrect
//double easy_ran_gumbel1 (const easy_rng * r, double a, double b);

struct _easy_ran_discrete_t;
typedef struct _easy_ran_discrete_t easy_ran_discrete_t;

easy_ran_discrete_t * easy_ran_discrete_preproc (size_t K, const double * P);

size_t easy_ran_discrete (const easy_rng * r, const easy_ran_discrete_t * g);

void easy_ran_discrete_free (easy_ran_discrete_t * g);

unsigned int easy_ran_poisson (const easy_rng * r, double mu);

unsigned int easy_ran_bernoulli (const easy_rng * r, double p);

unsigned int easy_ran_binomial (const easy_rng * r, double p, unsigned int n);

// C++11 allows only for integer n's, unlike GSL where n is double
unsigned int easy_ran_negative_binomial (const easy_rng * r, double p, unsigned int n);

unsigned int easy_ran_geometric (const easy_rng * r, double p);

#ifdef __cplusplus
}
#endif

#endif
