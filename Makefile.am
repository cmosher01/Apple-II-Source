#@configure_input@
ACLOCAL_AMFLAGS=-I m4 --install
SUBDIRS=src

.PHONY: site doc

site: doc

doc:
	$(MAKE) -C $@ all
	$(MAKE) -C $@ site

