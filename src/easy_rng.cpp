#include "easy_rng.h"
#include <string>


class _easy_rng_base {
	virtual unsigned long int get() = 0;
	virtual void set(unsigned long int seed) = 0;
	virtual ~_easy_rng_base() {}
};

extern "C" struct _easy_rng_type {
	std::string name;
};

extern "C" struct _easy_rng {
	struct _easy_rng_type type;
	_easy_rng_base *rng;	
};
