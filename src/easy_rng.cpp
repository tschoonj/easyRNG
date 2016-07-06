#include "easy_rng.h"
#include <string>
#include <iostream>
#include <random>
#include <cstdlib>


class _easy_rng_base {
	public:
	virtual unsigned long int get() = 0;
	virtual void set(unsigned long int seed) = 0;
	virtual double uniform() = 0;
	virtual double uniform_pos() = 0;
	virtual unsigned long int uniform_int(unsigned long int n) = 0;
	virtual ~_easy_rng_base() {}
};

template<class _easy_rng_cxx11>
class _easy_rng_tmpl : public _easy_rng_base {
	private:
	_easy_rng_cxx11 rng;
	public:
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
		std::uniform_real_distribution<double> dis(0, 1);
		return dis(rng);
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
};

extern "C" struct _easy_rng {
	const easy_rng_type *type;
	_easy_rng_base *rng;	
};

#define ADD_RNG(rng_name) \
	static const easy_rng_type rng_name = { \
		.name = #rng_name, \
		.max = std::rng_name::max(), \
		.min = std::rng_name::min(), \
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

