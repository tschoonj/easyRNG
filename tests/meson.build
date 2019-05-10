test1 = executable('test1', 'test1.c', dependencies: easyRNG_lib_dep, install: false)
test('test1', test1)

if gsl_dep.found()
	test2 = executable('test2', 'test2.c', dependencies: [easyRNG_lib_dep, gsl_dep], install: false)
	test('test2', test2)
endif

test3 = executable('test3', 'test3.c', dependencies: easyRNG_lib_dep, install: false)
test('test3', test3, timeout: 300)

if get_option('with-fortran')
	test4 = executable('test4', 'test4.F90', dependencies: easyRNG_lib_dep, install: false)
	test('test4', test4)

	if fgsl_dep.found()
		test5 = executable('test5', 'test5.F90', dependencies: easyRNG_lib_dep, install: false)
		test('test5', test5)
	endif

	test6 = executable('test6', 'test6.F90', dependencies: easyRNG_lib_dep, install: false)
	test('test6', test6, timeout: 300)
endif
