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

#include "easy_rng_private.h"
#include <string>
#include <sstream>
#include <fstream>
#include <cstdlib>
#include <random>
#include <cstring>

template<class _easy_rng_cxx11>
class _easy_rng_tmpl : public _easy_rng_base {
	private:
	_easy_rng_cxx11 rng;
	public:
	virtual void print(std::ostream &out) {
		out << rng;
	}
	virtual void scan(std::istream &in) {
		in >> rng;
	}
	virtual bool equal(_easy_rng_base &sec) {
		return rng == dynamic_cast<_easy_rng_tmpl &>(sec).rng;
	}
	virtual unsigned long int get() {
		auto rv = rng();
		//std::cout << "sizeof(rv): " << sizeof(rv) << std::endl;
		//std::cout << "orig value: " << rv << std::endl;
		return rv;
	}
	virtual void set(unsigned long int seed) {
		rng.seed(seed);
	}
	virtual double uniform() {
		return std::generate_canonical<double,std::numeric_limits<double>::digits>(rng);
	}
	virtual double uniform_pos() {
		double rv;
		do {
			rv = uniform();
		} while (rv == 0);
		return rv;
	}
	virtual unsigned long int uniform_int(unsigned long int n) {
		std::uniform_int_distribution<unsigned long int> dis(0, n-1);
		return dis(rng);
	}
	virtual double gaussian(double sigma) {
		std::normal_distribution<double> dis(0.0, sigma);
		return dis(rng);
	}
	virtual double exponential(double mu) {
		std::exponential_distribution<double> dis(mu);
		return dis(rng);
	}
	virtual double cauchy(double a) {
		std::cauchy_distribution<double> dis(0.0, a);
		return dis(rng);
	}
	virtual double gamma(double a, double b) {
		std::gamma_distribution<double> dis(a, b);
		return dis(rng);
	}
	virtual double flat(double a, double b) {
		std::uniform_real_distribution<double> dis(a, b);
		return dis(rng);
	}
	virtual double lognormal(double zeta, double sigma) {
		std::lognormal_distribution<double> dis(zeta, sigma);
		return dis(rng);
	}
	virtual double chisq(double nu) {
		std::chi_squared_distribution<double> dis(nu);
		return dis(rng);
	}
	virtual double fdist(double nu1, double nu2) {
		std::fisher_f_distribution<double> dis(nu1, nu2);
		return dis(rng);
	}
	virtual double tdist(double nu) {
		std::student_t_distribution<double> dis(nu);
		return dis(rng);
	}
	virtual double weibull(double a, double b) {
		std::weibull_distribution<double> dis(b, a);
		return dis(rng);
	}
	/*virtual double gumbel1(double a, double b) {
		std::extreme_value_distribution<double> dis(a, b);
		return dis(rng);
	}*/
	virtual size_t discrete(std::discrete_distribution<size_t> &dis) {
		return dis(rng);
	}
	virtual unsigned int poisson(double mu) {
		std::poisson_distribution<unsigned int> dis(mu);
		return dis(rng);
	}
	virtual unsigned int bernoulli(double p) {
		std::bernoulli_distribution dis(p);
		return dis(rng);
	}
	virtual unsigned int binomial(double p, unsigned int n) {
		std::binomial_distribution<unsigned int> dis(n, p);
		return dis(rng);
	}
	virtual unsigned int negative_binomial(double p, unsigned int n) {
		std::negative_binomial_distribution<unsigned int> dis(n, p);
		return dis(rng);
	}
	virtual unsigned int geometric(double p) {
		std::geometric_distribution<unsigned int> dis(p);
		// the C++11 returns the number of failures before the first success,
		// while we need the number of trials needed to get one success...
		return dis(rng) + 1;
	}
};



#define ADD_RNG(rng_name) \
	static const easy_rng_type rng_name = { \
		.name = #rng_name, \
		.max = static_cast<unsigned long int>(std::rng_name::max()), \
		.min = static_cast<unsigned long int>(std::rng_name::min()), \
	}; \
	const easy_rng_type *easy_rng_ ## rng_name = &rng_name;

ADD_RNG(minstd_rand0)
ADD_RNG(minstd_rand)
ADD_RNG(mt19937)
ADD_RNG(mt19937_64)
ADD_RNG(ranlux24_base)
ADD_RNG(ranlux48_base)
ADD_RNG(ranlux24)
ADD_RNG(ranlux48)
ADD_RNG(knuth_b)

const easy_rng_type *easy_rng_default = &mt19937;
unsigned long int easy_rng_default_seed = 0;

const easy_rng_type *all_types[10] = {
	easy_rng_minstd_rand0,
	easy_rng_minstd_rand,
	easy_rng_mt19937,
	easy_rng_mt19937_64,
	easy_rng_ranlux24_base,
	easy_rng_ranlux48_base,
	easy_rng_ranlux24,
	easy_rng_ranlux48,
	easy_rng_knuth_b,
	nullptr
};

#define IF_RNG(rng_name) if (T == easy_rng_ ## rng_name) {\
	rng = new _easy_rng_tmpl<std::rng_name>(); \
	}

extern "C" easy_rng * easy_rng_alloc (const easy_rng_type * T) {
	easy_rng *rv;
	_easy_rng_base *rng;
	
	//allocate the RNG and seed it with the default value
	IF_RNG(minstd_rand0)
	else IF_RNG(minstd_rand)
	else IF_RNG(mt19937)
	else IF_RNG(mt19937_64)
	else IF_RNG(ranlux24_base)
	else IF_RNG(ranlux48_base)
	else IF_RNG(ranlux24)
	else IF_RNG(ranlux48)
	else IF_RNG(knuth_b)
	else {
		std::cerr << "easy_rng_alloc: invalid easy_rng_type" << std::endl;
		return nullptr;
	}
	rv = (easy_rng *) malloc(sizeof(struct _easy_rng));
	rv->rng = rng;
	rv->type = T;
	rng->set(easy_rng_default_seed);

	return rv;
}

extern "C" void easy_rng_set (const easy_rng * r, unsigned long int s) {
	r->rng->set(s);
}

extern "C" void easy_rng_free (easy_rng * r) {
	delete r->rng;
	free(r);
}

extern "C" unsigned long int easy_rng_get (const easy_rng * r) {
	return r->rng->get();
}

extern "C" double easy_rng_uniform (const easy_rng * r) {
	return r->rng->uniform();
}

extern "C" double easy_rng_uniform_pos (const easy_rng * r) {
	return r->rng->uniform_pos();
}

extern "C" unsigned long int easy_rng_uniform_int (const easy_rng * r, unsigned long int n) {
	return r->rng->uniform_int(n);
}

extern "C" const char * easy_rng_name (const easy_rng * r) {
	return r->type->name;
}

extern "C" unsigned long int easy_rng_max (const easy_rng * r) {
	return r->type->max;
}

extern "C" unsigned long int easy_rng_min (const easy_rng * r) {
	return r->type->min;
}

extern "C" const easy_rng_type ** easy_rng_types_setup (void) {
	return all_types;
}

extern "C" const easy_rng_type * easy_rng_env_setup (void) {
	const char *p = getenv ("EASY_RNG_TYPE");

	if (p != nullptr) {
		const easy_rng_type **t;

		easy_rng_default = 0;

		for (t = easy_rng_types_setup() ; *t != 0 ; ++t) {
			if (strcmp (p, (*t)->name) == 0) {
				easy_rng_default = *t;
				break;
			}
		}

		if (easy_rng_default == 0) {
			std::cerr << "EASY_RNG_TYPE=" << p << " not recognized" << std::endl;
			std::cerr << "Valid generator types are:" << std::endl;

			for (t = easy_rng_types_setup(); *t != 0; t++) {
				std::cerr << (*t)->name << std::endl;
			}
			easy_rng_default = easy_rng_mt19937;
			return nullptr;
		}

		std::cout << "EASY_RNG_TYPE=" << easy_rng_default->name << std::endl;
	}
	else {
		easy_rng_default = easy_rng_mt19937;
	}

	p = getenv ("EASY_RNG_SEED");
	unsigned long int new_seed = 0;

	if (p != nullptr) {
		new_seed = strtoul(p, 0, 0);
		std::cout << "EASY_RNG_SEED=" << new_seed << std::endl;
	}

	easy_rng_default_seed = new_seed;

	return easy_rng_default;
}

extern "C" int easy_rng_memcpy (easy_rng * dest, const easy_rng * src) {
	if (dest->type != src->type) {
		std::cerr << "easy_rng_memcpy: dest and src must have the same easy_rng_type" << std::endl;
		return -1;
	}
	// copy the state
	std::stringstream fout;
	src->rng->print(fout);
	dest->rng->scan(fout);

	return 0;
}

extern "C" easy_rng * easy_rng_clone (const easy_rng * r) {
	easy_rng *rv = easy_rng_alloc(r->type);
	easy_rng_memcpy(rv, r);

	return rv;
}

extern "C" int easy_rng_equal(const easy_rng * ra, const easy_rng *rb) {
	if (ra->type != rb->type)
		return 0;
	if (ra->rng->equal(*(rb->rng)) == true)
		return 1;
	else
		return 0;
}

extern "C" int easy_rng_fwrite (FILE * stream, const easy_rng * r) {
	std::ostringstream out;
	r->rng->print(out);
	std::string outstr = out.str();
	size_t length = outstr.length();
	size_t written = fwrite(&length, sizeof(size_t), 1, stream);
	if (written != 1)
		return -1;
	written = fwrite(outstr.c_str(), sizeof(char), length, stream);
	if (written != outstr.length())
		return -1;
	return 0;
}

extern "C" int easy_rng_fread (FILE * stream, easy_rng * r) {
	size_t length;
	size_t read = fread(&length, sizeof(size_t), 1, stream);
	if (read != 1)
		return -1;
	char *buffer = (char *) malloc(sizeof(char) * (length + 1));
	read = fread(buffer, sizeof(char), length, stream);
	if (read != length) {
		free(buffer);
		return -1;
	}
	buffer[length] = '\0';
	std::string instr(buffer);
	std::istringstream in(instr);
	r->rng->scan(in);
	free(buffer);
	return 0;
}
