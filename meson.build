project('easyRNG', ['c', 'cpp'], version:'1.2', license: 'BSD', default_options: ['cpp_std=c++11'])

# versioning stuff -> maintain compatibility with libtool!
# # a) If binary compatibility has been broken (eg removed or changed interfaces)
# #    change to C+1:0:0.
# # b) If interfaces have been changed or added, but binary compatibility has
# #    been preserved, change to C+1:0:A+1
# # c) If the interface is the same as the previous version, change to C:R+1:A
lib_current = 0
lib_revision = 1
lib_age = 0

version = '@0@.@1@.@2@'.format((lib_current - lib_age), lib_age, lib_revision)

current = lib_current - lib_age
interface_age = lib_revision
darwin_versions = [current + 1, '@0@.@1@'.format(current + 1, interface_age)]

gsl_dep = dependency('gsl', version: '>=1.13', required:false)

pkgconfig = import('pkgconfig')

if get_option('with-fortran')
	add_languages('fortran')
	fc = meson.get_compiler('fortran')

	c_loc_test = '''
SUBROUTINE test(c)
USE, INTRINSIC :: iso_c_binding
IMPLICIT NONE
INTEGER(c_size_t), DIMENSION(:), INTENT(inout), TARGET:: c
TYPE(C_PTR) :: ptr

ptr = C_LOC(c)

ENDSUBROUTINE
'''

	if not fc.compiles(c_loc_test)
		error('The Fortran compiler does not support C_LOC for targeted arrays')
	endif
	
	fgsl_dep = dependency('fgsl', version: '>=1.0.0', required:false)

	meson.add_install_script('install_fortran_mod.py')
endif

subdir('src')

# generate pkg-config file
pkgconfig.generate(
	filebase: 'easyRNG',
	name: 'easyRNG',
	description: 'wrapper around C++11\'s random number generators for use in C and Fortran',
	version: meson.project_version(),
	libraries: easyRNG_lib,
	subdirs: 'easyRNG',
	variables: ['with_fortran=' + (get_option('with-fortran') ? 'yes' : 'no')],
)

# generate RPM SPEC file
rpm_config = configuration_data()
rpm_config.set('VERSION', meson.project_version())
configure_file(input: 'easyRNG.spec.in', output: 'easyRNG.spec', configuration: rpm_config, install: false)

subdir('tests')

doxygen = find_program('doxygen', required : false)
if doxygen.found()
	subdir('docs')
endif
