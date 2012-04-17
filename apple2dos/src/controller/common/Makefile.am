AUTOMAKE_OPTIONS=-Wno-override

INCDIR=$(srcdir)/../../include
COMDIR=$(srcdir)/../common



ZERO_SEGS=-bd 0 -bb 0 -bz 0

.m4.s65:
	$(M4) -E -I $(INCDIR) -I $(COMDIR) $(M4FLAGS_LOCAL) $(M4FLAGS) $< >$@

#arbitrary code seg start (-bt 2048) because -bt 0 tends to cause strange errors
.s65.o65:
	$(XA65) -C -M -R -c -bt 2048 $(ZERO_SEGS) -o $@ $(XA65FLAGS) $<

.o65.r65:
	$(LDO65) -bt $(CODE_SEG) $(ZERO_SEGS) -o $@ $(LD65FLAGS) $^

.r65.a65:
	$(RELOC65) -xt -o $@ $(RELOC65FLAGS) $<



mypkgdir=$(prefix)/lib/apple2/dos/$(DOS_SECTORS)sector

mypkg_DATA=disk2.a65
noinst_DATA=disk2.md5
EXTRA_DIST=$(COMDIR)/disk2.m4 $(srcdir)/disk2.md5 $(INCDIR)/asm.m4h

disk2.r65: M4FLAGS_LOCAL=-DVERSION=$(DOS_SECTORS)
disk2.r65: CODE_SEG=0

VPATH += $(COMDIR) $(INCDIR)

disk2.s65: disk2.m4 asm.m4h

check_SCRIPTS=unit_test.sh
TESTS=$(check_SCRIPTS)

.PROCIOUS: disk2.a65

unit_test.sh:
	touch $@
	if test "X$(MD5SUM)" == X ; \
	then \
		echo "echo \"md5sum program not found, so cannot run this test\" >&2" >>$@ ; \
		echo "exit 1" >>$@ ; \
	else \
		echo "$(MD5SUM) --check --status --warn $(srcdir)/disk2.md5" >>$@ ; \
	fi
	chmod +x $@



CLEANFILES=unit_test.sh *.a65 *.r65 *.s65 *.o65
