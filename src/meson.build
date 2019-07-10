headers = files('easy_rng.h', 'easy_randist.h')

private_headers = files('easy_rng_private.h')

sources = files('easy_rng.cpp', 'easy_randist.cpp')

if get_option('with-fortran')
	sources += files('easy_rng_f.F90')
endif

install_headers(headers, subdir: 'easyRNG')

easyRNG_sources = headers + private_headers + sources

easyRNG_lib = library('easyRNG', easyRNG_sources, version: version, darwin_versions: darwin_versions, install: true, include_directories: rootdir)

easyRNG_lib_dep = declare_dependency(link_with: easyRNG_lib, include_directories: include_directories('.'))
