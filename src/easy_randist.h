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

/** \file easy_randist.h
 *  \brief The random number distributions API
 *  \author Tom Schoonjans
 *
 *  This header provides wrappers around the random number distributions that are
 *  offered by C++11's \c random templates.
 *  [GSL's random number distributions](https://www.gnu.org/software/gsl/manual/html_node/Random-Number-Distributions.html), upon which this API is based,
 *  offers considerably more types of distributions. In case one of these (rather exotic) distributions is needed, the user is recommended to use GSL instead of this library.
 *  \note The functions outlined here are fully equivalent to their GSL counterparts. This means that in some cases, arguments passed to or values returned by the C++11 random number distributions were changed in order to obtain identical distributions. The main consequence is that one should not rely on the C++11 standard and its implementations to explain the output of these methods.
 * \note The documentation on this page, including the equations of the probability density functions, was sourced from Wikipedia. Links to the relevant pages have been included for each method.
 */ 

#ifndef EASY_RANDIST_H
#define EASY_RANDIST_H

#include "easy_rng.h"

#ifdef __cplusplus
extern "C" {
#endif

/** Generate double precision real numbers according to a [normal distribution](https://en.wikipedia.org/wiki/Normal_distribution)
 *
 * The probability density function of the normal distribution is defined as: \f[
 * f(x; \sigma) = \frac{1}{\sigma\sqrt{2\pi}}\exp\left(-\frac{1}{2}\left(\frac{x}{\sigma}\right)^2\right)
 * \f]
 * with \f$\sigma\f$ the standard deviation.
 * \warning Since the C++11 standard does not specify which algorithm should be used to implement this distribution, easy_ran_gaussian_ziggurat() and easy_ran_gaussian_ratio_method() map to this function.
 * \param r The random number generator instance
 * \param sigma The standard deviation of the normal distribution
 * \returns A double precision real number, sampled from a normal distribution
 */
EASYRNG_EXTERN
double easy_ran_gaussian (const easy_rng * r, double sigma);

/** Generate double precision real numbers according to a [normal distribution](https://en.wikipedia.org/wiki/Normal_distribution)
 *
 * \warning Since the C++11 standard does not specify which algorithm should be used to implement this distribution, this function maps to easy_ran_gaussian().
 * \param r The random number generator instance
 * \param sigma The standard deviation of the normal distribution
 * \returns A double precision real number, sampled from a normal distribution
 */
EASYRNG_EXTERN
double easy_ran_gaussian_ziggurat (const easy_rng * r, double sigma);

/** Generate double precision real numbers according to a [normal distribution](https://en.wikipedia.org/wiki/Normal_distribution)
 *
 * \warning Since the C++11 standard does not specify which algorithm should be used to implement this distribution, this function maps to easy_ran_gaussian().
 * \param r The random number generator instance
 * \param sigma The standard deviation of the normal distribution
 * \returns A double precision real number, sampled from a normal distribution
 */
EASYRNG_EXTERN
double easy_ran_gaussian_ratio_method (const easy_rng * r, double sigma);

/** Generate double precision real numbers according to a [unit normal distribution](https://en.wikipedia.org/wiki/Normal_distribution#Standard_normal_distribution)
 *
 * The probability density function of the unit (or standard) normal distribution is defined as: \f[
 * f(x) = \frac{1}{\sqrt{2\pi}}\exp\left(-\frac{x^2}{2}\right)
 * \f]
 * \warning Since the C++11 standard does not specify which algorithm should be used to implement this distribution, easy_ran_ugaussian_ratio_method() map to this function.
 * \param r The random number generator instance
 * \returns A double precision real number, sampled from a unit normal distribution
 */
EASYRNG_EXTERN
double easy_ran_ugaussian (const easy_rng * r);

/** Generate double precision real numbers according to a [unit normal distribution](https://en.wikipedia.org/wiki/Normal_distribution#Standard_normal_distribution)
 *
 * \warning Since the C++11 standard does not specify which algorithm should be used to implement this distribution, this function maps to easy_ran_ugaussian().
 * \param r The random number generator instance
 * \returns A double precision real number, sampled from a unit normal distribution
 */
EASYRNG_EXTERN
double easy_ran_ugaussian_ratio_method (const easy_rng * r);

/** Generate double precision random numbers according to an [exponential distribution](https://en.wikipedia.org/wiki/Exponential_distribution)
 *
 * The probability density function of the exponential distribution is defined as: \f[
 * f(x;\mu) = \frac{1}{\mu}\exp\left(-\frac{x}{\mu}\right)
 * \f]
 * with \f$\mu\f$ the inverse rate (also the distribution mean or scale factor) for positive \f$x\f$
 * \param r The random number generator instance
 * \param mu The inverse rate
 * \returns A positive double precision real number, sampled from an exponential distribution
 */
EASYRNG_EXTERN
double easy_ran_exponential (const easy_rng * r, double mu);

/** Generate double precision random numbers according to a [Cauchy distribution](https://en.wikipedia.org/wiki/Cauchy_distribution)
 *
 * The probability density function of the Cauchy (or Lorentz, as it is widely known in spectroscopy) distribution is defined as: \f[
 * f(x;a) = \frac{1}{\pi a\left[1 + \left(\frac{x}{a}\right)^2\right]}
 * \f]
 * with \f$a\f$ the scale parameter
 * \param r The random number generator instance
 * \param a The scale parameter
 * \returns A double precision real number, sampled from a Cauchy distribution
 */
EASYRNG_EXTERN
double easy_ran_cauchy (const easy_rng * r, double a);

/** Generate double precision random numbers according to a [Gamma distribution](https://en.wikipedia.org/wiki/Gamma_distribution)
 *
 * The probability density function of the Gamma distribution is defined as: \f[
 * f(x;k,\theta) = \frac{1}{\Gamma(k) \theta^k} x^{k-1} \exp\left(-\frac{x}{\theta}\right)
 * \f]
 * with \f$k\f$ the shape parameter and \f$\theta\f$ for positive \f$x\f$
 * \param r The random number generator instance
 * \param k The shape parameter (must be strictly positive)
 * \param theta The scale parameter (must be strictly positive)
 * \returns A positive double precision real number, sampled from a Gamma distribution
 */
EASYRNG_EXTERN
double easy_ran_gamma (const easy_rng * r, double k, double theta);

/** Generate double precision random numbers according to a [flat distribution](https://en.wikipedia.org/wiki/Uniform_distribution_%28continuous%29)
 *
 * The probability density function of the flat (uniform) distribution is defined as: \f[
 * f(x;a,b) = \frac{1}{b - a}
 * \f]
 * with \f$a\f$ and \f$b\f$ the minimum and maximum values of the interval respectively.
 * \param r The random number generator instance
 * \param a The minimum value of the sampling interval
 * \param b The maximum value of the sampling interval
 * \returns A double precision real number, sampled from a flat distribution
 */
EASYRNG_EXTERN
double easy_ran_flat (const easy_rng * r, double a, double b);

/** Generate double precision random numbers according to a [log-normal distribution](https://en.wikipedia.org/wiki/Log-normal_distribution)
 *
 * The probability density function of the log-normal distribution is defined as: \f[
 * f(x;\mu,\sigma) = \frac{1}{x \sigma \sqrt{2 \pi}} \exp\left(-\frac{(\ln(x) - \mu)^2}{2 \sigma^2}\right)
 * \f]
 * with \f$\mu\f$ the location parameter and \f$\sigma\f$ the scale factor, for positive \f$x\f$
 * \param r The random number generator instance
 * \param mu The position parameter
 * \param sigma The scale parameter (must be strictly positive)
 * \returns A positive double precision real number, sampled from a log-normal distribution
 */
EASYRNG_EXTERN
double easy_ran_lognormal (const easy_rng * r, double mu, double sigma);

/** Generate double precision random numbers according to a [chi-squared distribution](https://en.wikipedia.org/wiki/Chi-squared_distribution)
 *
 * The probability density function of the chi-squared distribution is defined as: \f[
 * f(x;k) = \frac{1}{2^{\frac{k}{2}}\Gamma\left(\frac{k}{2}\right)}x^{\frac{k}{2}-1}\exp\left(-\frac{x}{2}\right)
 * \f]
 * with \f$k\f$ the degrees of freedom, for positive \f$x\f$
 * \param r The random number generator instance
 * \param k The degrees of freedom
 * \returns A positive double precision real number, sampled from a chi-squared distribution
 */
EASYRNG_EXTERN
double easy_ran_chisq (const easy_rng * r, double k);

/** Generate double precision random numbers according to an [F distribution](https://en.wikipedia.org/wiki/F-distribution)
 *
 * The probability density function of the F-distribution is defined as: \f[
 * f(x;d_1,d_2) = \frac{\sqrt{\frac{(d_1x)^{d_1}d_2^{d_2}}{(d_1x+d_2)^{(d_1+d_2)}}}}{xB\left(\frac{d_1}{2},\frac{d_2}{2}\right)}
 * \f]
 * with \f$d_1\f$ and \f$d_2\f$ the degrees of freedom and B the [Beta function](https://en.wikipedia.org/wiki/Beta_function), for positive \f$x\f$
 * \param r The random number generator instance
 * \param d_1 The first degrees of freedom
 * \param d_2 The second degrees of freedom
 * \returns A positive double precision real number, sampled from a F-distribution
 */
EASYRNG_EXTERN
double easy_ran_fdist (const easy_rng * r, double d_1, double d_2);

/** Generate double precision random numbers according to a [Student's t-distribution](https://en.wikipedia.org/wiki/Student's_t-distribution)
 *
 * The probability density function of the Student's t-distribution is defined as: \f[
 * f(x;\nu) = \frac{\Gamma\left(\frac{\nu+1}{2}\right)}{\sqrt{\nu\pi}\Gamma\left(\frac{\nu}{2}\right)}\left(1+\frac{x^2}{\nu}\right)^{-\frac{\nu+1}{2}}
 * \f]
 * with \f$\nu\f$ the degrees of freedom.
 * \param r The random number generator instance
 * \param nu The degrees of freedom
 * \returns A double precision real number, sampled from a Student's t-distribution
 */
EASYRNG_EXTERN
double easy_ran_tdist (const easy_rng * r, double nu);

/** Generate double precision random numbers according to a [Weibull distribution](https://en.wikipedia.org/wiki/Weibull_distribution)
 *
 * The probability density function of the Weibull distribution is defined as: \f[
 * f(x;\lambda,k) = \frac{k}{\lambda}\left(\frac{x}{\lambda}\right)^{k-1}\exp\left(-\left(\frac{x}{\lambda}\right)^k\right)
 * \f]
 * with \f$\lambda\f$ the scale parameter and \f$k\f$ the shape parameter, for positive \f$x\f$
 * \param r The random number generator instance
 * \param lambda The scale parameter
 * \param k The shape parameter
 * \returns A double precision real number, sampled from a Weibull distribution
 */
EASYRNG_EXTERN
double easy_ran_weibull (const easy_rng * r, double lambda, double k);

// Thought that std::extreme_value_distribution was matching Gumbel Type 1, but that looks incorrect
//double easy_ran_gumbel1 (const easy_rng * r, double a, double b);

#ifndef DOXYGEN_SHOULD_SKIP_THIS
struct _easy_ran_discrete_t;
#endif

/** An intentionally opaque structure containing a lookup table of a discrete distribution
 *
 * Instances will only be available through pointers, and should be obtained and freed after usage through easy_ran_discrete_preproc() and easy_ran_discrete_free(), respectively.
 */
typedef struct _easy_ran_discrete_t easy_ran_discrete_t;

/** Generate a lookup table for a discrete distribution
 *
 * Free this table after usage with easy_ran_discrete_free()
 * \param K The length of the array \c P
 * \param P An array of length \c K, containing the (positive) weights of the events
 * \returns The lookup table. Pass this variable to easy_ran_discrete() to generate discrete random numbers based on the contents of \c P
 * \warning The Fortran API drops the \c K argument as the size of the P array can be determined from the variable itself.
 */
EASYRNG_EXTERN
easy_ran_discrete_t * easy_ran_discrete_preproc (size_t K, const double * P);

/** Generate positive integers according to a discrete distribution
 *
 * \param r The random number generator instance
 * \param g The lookup table
 * \returns A positive integer, sampled from a discrete distribution
 */
EASYRNG_EXTERN
size_t easy_ran_discrete (const easy_rng * r, const easy_ran_discrete_t * g);

/** Frees the memory associated with a discrete distribution lookup table
 *
 * \param g The lookup table
 */
EASYRNG_EXTERN
void easy_ran_discrete_free (easy_ran_discrete_t * g);

/** Generate positive integers according to a [Poisson distribution](https://en.wikipedia.org/wiki/Poisson_distribution)
 *
 * The probability function of the Poisson distribution is defined as: \f[
 * f(k;\lambda) = \frac{\lambda^k\exp(-\lambda)}{k!}
 * \f]
 * with \f$\lambda\f$ the average number of events in an interval, for positive integers \f$k\f$
 * \param r The random number generator instance
 * \param lambda The average number of events in an interval
 * \returns A positive integer, sampled from a Poisson distribution
 */
EASYRNG_EXTERN
unsigned int easy_ran_poisson (const easy_rng * r, double lambda);

/** Generate zeroes and ones according to a [Bernoulli distribution](https://en.wikipedia.org/wiki/Bernoulli_distribution)
 *
 * The probability function of the Bernoulli distribution is defined as: \f[
 * f(k;p) = p^k(1-p)^{1-k}
 * \f]
 * with \f$p\f$ the success probability between 0 and 1, for integers \f$k\f$ restricted to 0 and 1
 * \param r The random number generator instance
 * \param p The success probability
 * \returns 0 or 1, sampled from a Bernoulli distribution
 */
EASYRNG_EXTERN
unsigned int easy_ran_bernoulli (const easy_rng * r, double p);

/** Generate positive integers according to a [binomial distribution](https://en.wikipedia.org/wiki/Binomial_distribution)
 *
 * The probability function of the binomial distribution is defined as: \f[
 * f(k;p,n) = {{n}\choose{k}}p^k(1-p)^{n-k}
 * \f]
 * with \f$p\f$ the success probability between 0 and 1 for each trial, and \f$n\f$ the number of trials, for positive integers \f$k\f$
 * \param r The random number generator instance
 * \param p The success probability for each trial
 * \param n The number of trials
 * \returns A positive integer, sampled from a binomial distribution
 */
EASYRNG_EXTERN
unsigned int easy_ran_binomial (const easy_rng * r, double p, unsigned int n);

/** Generate positive integers according to a [negative binomial distribution](https://en.wikipedia.org/wiki/Negative_binomial_distribution)
 *
 * The probability function of the negative binomial distribution is defined as: \f[
 * f(k;p,n) = {{n+k-1}\choose{k}}p^n(1-p)^k
 * \f]
 * with \f$p\f$ the success probability between 0 and 1 for each trial, and \f$n\f$ the number of successful trials, for positive integers \f$k\f$, the number of failures
 * \warning C++11 allows only for integer n's, unlike GSL where n is double
 * \param r The random number generator instance
 * \param p The success probability for each trial
 * \param n The number of successful trials
 * \returns A positive integer, sampled from a negative binomial distribution
 */
EASYRNG_EXTERN
unsigned int easy_ran_negative_binomial (const easy_rng * r, double p, unsigned int n);

/** Generate positive integers according to a [geometric distribution](https://en.wikipedia.org/wiki/Geometric_distribution)
 *
 * The probability function of the geometric distribution is defined as: \f[
 * f(k;p) = (1-p)^{k-1}p
 * \f]
 * with \f$p\f$ the success probability between 0 and 1 for each trial, for positive integers \f$k\f$ greater than one.
 * \param r The random number generator instance
 * \param p The success probability for each trial
 * \returns A positive integer, sampled from a geometric distribution
 */
EASYRNG_EXTERN
unsigned int easy_ran_geometric (const easy_rng * r, double p);

#ifdef __cplusplus
}
#endif

#endif
