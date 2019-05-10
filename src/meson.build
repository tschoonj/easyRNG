headers = ['easy_rng.h', 'easy_randist.h']

private_headers = ['easy_rng_private.h']

sources = ['easy_rng.cpp', 'easy_randist.cpp']

if get_option('with-fortran')
	sources += 'easy_rng_f.F90'
endif

install_headers(headers, subdir: 'easyRNG')

easyRNG_lib = library('easyRNG', sources + private_headers, version: version, darwin_versions: darwin_versions, install: true)

easyRNG_lib_dep = declare_dependency(link_with: easyRNG_lib, include_directories: include_directories('.'))
