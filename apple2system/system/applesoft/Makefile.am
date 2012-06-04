#@configure_input@
mypkgdir=$(prefix)/lib/apple2/system/a2p

AUTOMAKE_OPTIONS=-Wno-override

INCDIR=$(srcdir)/../include

ZEROSEGS=-bd 0 -bb 0 -bz 0

.m4.s65:
	$(M4) -E -I $(INCDIR) $(M4FLAGS) $< >$@

.s65.o65:
	$(XA65) -C -M -R -c -bt 0 $(ZERO_SEGS) -o $@ $(XA65FLAGS) $<

.o65.r65:
	$(LDO65) -bt $(CODE_SEG) $(ZERO_SEGS) -o $@ $(LD65FLAGS) $^

.r65.a65:
	$(RELOC65) -xt -o $@ $(RELOC65FLAGS) $<



mypkg_DATA=applesoft.a65
noinst_DATA=applesoft.md5
EXTRA_DIST=applesoft.m4 applesoft.md5 $(INCDIR)/asm.m4h

# $D000
applesoft.r65: CODE_SEG=53248

VPATH += $(INCDIR)

applesoft.o65: applesoft.m4 asm.m4h

define_new_md5: all
	$(MD5SUM) --binary applesoft.a65 >$(srcdir)/applesoft.md5

check_SCRIPTS=unit_test.sh
TESTS=$(check_SCRIPTS)

unit_test.sh:
	touch $@
	if test "X$(MD5SUM)" == X ; \
	then \
		echo "echo \"md5sum program not found, so cannot run this test\" >&2" >>$@ ; \
		echo "exit 1" >>$@ ; \
	else \
		echo "$(MD5SUM) --check --status --warn $(srcdir)/applesoft.md5" >>$@ ; \
	fi
	chmod +x $@



CLEANFILES=unit_test.sh *.a65 *.r65 *.s65 *.o65
