cdoxdeps = Doxyfile

if DOXYDOC

all: html/index.html

html/index.html: $(cdoxdeps)
	doxygen $<

install-data-local:
	for pkg in html ; do \
	  test -d $$pkg && \
	  $(mkinstalldirs) $(DESTDIR)$(pkgdocdir)/$$pkg && \
	  find $$pkg -type f -exec $(INSTALL_DATA) '{}' $(DESTDIR)$(pkgdocdir)/$$pkg/ \; ; \
	done || true
#	  $(mkinstalldirs) $(DESTDIR)$(docdir)/$$pkg && \
#	  find $$pkg -type f -exec $(INSTALL_DATA) '{}' $(DESTDIR)$(docdir)/$$pkg/ \; ;

uninstall-local:
	for pkg in html ; do \
	  rm -rf $(DESTDIR)$(pkgdocdir)/$$pkg; \
	done || true
#	  rm -rf $(DESTDIR)$(docdir)/$${pkg};

clean-local:
	rm -rf html

endif

EXTRA_DIST = Doxyfile.in fpp mainpage.dox
