#@configure_input@
mypkgdir=$(prefix)/lib/apple2/system/a2

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



M4S=fp1.m4 miniasm1.m4 fp2.m4 miniasm2.m4 f669.m4 sweet16.m4
OBJS=fp1.o65 miniasm1.o65 fp2.o65 miniasm2.o65 f669.o65 sweet16.o65

mypkg_DATA=other.a65
noinst_DATA=other.md5
EXTRA_DIST=$(M4S) other.s65 other.md5 $(INCDIR)/asm.m4h

other.r65: other.o65 $(OBJS)
# $F425
other.r65: CODE_SEG=62501

VPATH += $(INCDIR)

fp1.o65: fp1.m4 asm.m4h
miniasm1.o65: miniasm1.m4 asm.m4h
fp2.o65: fp2.m4 asm.m4h
miniasm2.o65: miniasm2.m4
f669.o65: f669.m4
sweet16.o65: sweet16.m4 asm.m4h

define_new_md5: all
	$(MD5SUM) --binary other.a65 >$(srcdir)/other.md5

check_SCRIPTS=unit_test.sh
TESTS=$(check_SCRIPTS)

unit_test.sh:
	touch $@
	if test "X$(MD5SUM)" == X ; \
	then \
		echo "echo \"md5sum program not found, so cannot run this test\" >&2" >>$@ ; \
		echo "exit 1" >>$@ ; \
	else \
		echo "$(MD5SUM) --check --status --warn $(srcdir)/other.md5" >>$@ ; \
	fi
	chmod +x $@



CLEANFILES=unit_test.sh *.a65 *.r65 *.s65 *.o65
