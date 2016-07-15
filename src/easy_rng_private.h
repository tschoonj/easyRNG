#ifndef EASY_RNG_PRIVATE_H
#define EASY_RNG_PRIVATE_H

#include "easy_rng.h"
#include <iostream>
#include <random>

class _easy_rng_base {
	public:
	virtual unsigned long int get() = 0;
	virtual void set(unsigned long int seed) = 0;
	virtual double uniform() = 0;
	virtual double uniform_pos() = 0;
	virtual unsigned long int uniform_int(unsigned long int n) = 0;
	virtual ~_easy_rng_base() {}
	virtual void print(std::ostream &out) = 0;
	virtual void scan(std::istream &in) = 0;
	virtual bool equal(_easy_rng_base &sec) = 0;
	virtual double gaussian(double sigma) = 0;
	virtual double exponential(double mu) = 0;
	virtual double cauchy(double a) = 0;
	virtual double gamma(double a, double b) = 0;
	virtual double flat(double a, double b) = 0;
	virtual double lognormal(double zeta, double sigma) = 0;
	virtual double chisq(double nu) = 0;
	virtual double fdist(double nu1, double nu2) = 0;
	virtual double tdist(double nu) = 0;
	virtual double weibull(double a, double b) = 0;
	//virtual double gumbel1(double a, double b) = 0;
	virtual size_t discrete(std::discrete_distribution<size_t> &dis) = 0;
	virtual unsigned int poisson(double mu) = 0;
	virtual unsigned int bernoulli(double p) = 0;
	virtual unsigned int binomial(double p, unsigned int n) = 0;
	virtual unsigned int negative_binomial(double p, unsigned int n) = 0;
	virtual unsigned int geometric(double p) = 0;
};

extern "C" struct _easy_rng {
	const easy_rng_type *type;
	_easy_rng_base *rng;	
};



#endif
