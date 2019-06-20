


if HAVE_DOXYGEN
doxyfile.stamp: $(top_srcdir)/src/easy_rng.h $(top_srcdir)/src/easy_randist.h $(top_srcdir)/src/easy_rng_f.F90 $(EXTRA_DIST)
	$(DOXYGEN) Doxyfile
	echo Timestamp > doxyfile.stamp

easyRNG_docdir = $(datadir)/doc/easyRNG/html


install-data-local:
	$(MKDIR_P) $(DESTDIR)$(easyRNG_docdir)
	$(MKDIR_P) $(DESTDIR)$(easyRNG_docdir)/search
	$(INSTALL_DATA) $(builddir)/html/*.css $(DESTDIR)$(easyRNG_docdir)
	$(INSTALL_DATA) $(builddir)/html/*.html $(DESTDIR)$(easyRNG_docdir)
	$(INSTALL_DATA) $(builddir)/html/*.js $(DESTDIR)$(easyRNG_docdir)
	$(INSTALL_DATA) $(builddir)/html/*.png $(DESTDIR)$(easyRNG_docdir)
	$(INSTALL_DATA) $(builddir)/html/search/*.css $(DESTDIR)$(easyRNG_docdir)/search
	$(INSTALL_DATA) $(builddir)/html/search/*.html $(DESTDIR)$(easyRNG_docdir)/search
	$(INSTALL_DATA) $(builddir)/html/search/*.js $(DESTDIR)$(easyRNG_docdir)/search
	$(INSTALL_DATA) $(builddir)/html/search/*.png $(DESTDIR)$(easyRNG_docdir)/search


CLEANFILES = doxyfile.stamp

all-local: doxyfile.stamp
clean-local:
	rm -rf html
uninstall-local:
	rm -rf $(DESTDIR)$(easyRNG_docdir)

endif

EXTRA_DIST = DoxygenLayout.xml extra_pages.dox meson.build
