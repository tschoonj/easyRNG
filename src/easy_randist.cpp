/*
Copyright (c) 2016, Tom Schoonjans
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of easyRNG nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "easy_randist.h"
#include "easy_rng_private.h"
#include <random>
#include <cstdlib>

extern "C" double easy_ran_gaussian (const easy_rng * r, double sigma) {
	return r->rng->gaussian(sigma);
}

extern "C" double easy_ran_gaussian_ziggurat (const easy_rng * r, double sigma) {
	return r->rng->gaussian(sigma);
}

extern "C" double easy_ran_gaussian_ratio_method (const easy_rng * r, double sigma) {
	return r->rng->gaussian(sigma);
}

extern "C" double easy_ran_ugaussian (const easy_rng * r) {
	return r->rng->gaussian(1.0);
}

extern "C" double easy_ran_ugaussian_ratio_method (const easy_rng * r) {
	return r->rng->gaussian(1.0);
}

extern "C" double easy_ran_exponential (const easy_rng * r, double mu) {
	return r->rng->exponential(mu);
}

extern "C" double easy_ran_cauchy (const easy_rng * r, double a) {
	return r->rng->cauchy(a);
}

extern "C" double easy_ran_gamma (const easy_rng * r, double a, double b) {
	return r->rng->gamma(a, b);
}

extern "C" double easy_ran_flat (const easy_rng * r, double a, double b) {
	return r->rng->flat(a, b);
}

extern "C" double easy_ran_lognormal (const easy_rng * r, double zeta, double sigma) {
	return r->rng->lognormal(zeta, sigma);
}

extern "C" double easy_ran_chisq (const easy_rng * r, double nu) {
	return r->rng->chisq(nu);
}

extern "C" double easy_ran_fdist (const easy_rng * r, double nu1, double nu2) {
	return r->rng->fdist(nu1, nu2);
}

extern "C" double easy_ran_tdist (const easy_rng * r, double nu) {
	return r->rng->tdist(nu);
}

extern "C" double easy_ran_weibull (const easy_rng * r, double a, double b) {
	return r->rng->weibull(a, b);
}
/*
extern "C" double easy_ran_gumbel1 (const easy_rng * r, double a, double b) {
	return r->rng->gumbel1(a, b);
}*/

extern "C" struct _easy_ran_discrete_t {
	std::discrete_distribution<size_t> *dis;
};

extern "C" easy_ran_discrete_t * easy_ran_discrete_preproc (size_t K, const double * P) {
	easy_ran_discrete_t *rv = (easy_ran_discrete_t *) malloc(sizeof(easy_ran_discrete_t));
	rv->dis = new std::discrete_distribution<size_t>(P, P + K);
	return rv;
}

extern "C" size_t easy_ran_discrete (const easy_rng * r, const easy_ran_discrete_t * g) {
	return r->rng->discrete(*g->dis);
}

extern "C" void easy_ran_discrete_free (easy_ran_discrete_t * g) {
	delete g->dis;
	free(g);
}

extern "C" unsigned int easy_ran_poisson (const easy_rng * r, double mu) {
	return r->rng->poisson(mu);
}

extern "C" unsigned int easy_ran_bernoulli (const easy_rng * r, double p) {
	return r->rng->bernoulli(p);
}

extern "C" unsigned int easy_ran_binomial (const easy_rng * r, double p, unsigned int n) {
	return r->rng->binomial(p, n);
}

extern "C" unsigned int easy_ran_negative_binomial (const easy_rng * r, double p, unsigned int n) {
	return r->rng->negative_binomial(p, n);
}

extern "C" unsigned int easy_ran_geometric (const easy_rng * r, double p) {
	return r->rng->geometric(p);
}
