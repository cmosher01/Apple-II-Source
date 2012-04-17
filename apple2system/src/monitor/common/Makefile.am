AUTOMAKE_OPTIONS=-Wno-override

INCDIR=$(srcdir)/../../include
COMDIR=$(srcdir)/../common

ZEROSEGS=-bd 0 -bb 0 -bz 0

.m4.s65:
	$(M4) -E -I $(INCDIR) -I $(COMDIR) $(M4FLAGS) $< >$@

.s65.o65:
	$(XA65) -C -M -R -c -bt 0 $(ZERO_SEGS) -o $@ $(XA65FLAGS) $<

.o65.r65:
	$(LDO65) -bt $(CODE_SEG) $(ZERO_SEGS) -o $@ $(LD65FLAGS) $^

.r65.a65:
	$(RELOC65) -xt -o $@ $(RELOC65FLAGS) $<

OBJS=lores.o65 disasm.o65 debug.o65 paddles.o65 display1.o65 math.o65 display2.o65 cassette.o65 keyin.o65 cmd.o65 vectors.o65
M4S=$(COMDIR)/lores.m4 $(COMDIR)/disasm.m4 $(COMDIR)/debug.m4 $(COMDIR)/paddles.m4 $(COMDIR)/display1.m4 $(COMDIR)/math.m4 $(COMDIR)/display2.m4 $(COMDIR)/cassette.m4 $(COMDIR)/keyin.m4 $(COMDIR)/cmd.m4 $(COMDIR)/vectors.m4

mypkg_DATA=monitor.a65
noinst_DATA=monitor.md5
EXTRA_DIST=$(M4S) $(COMDIR)/symbols.m4h $(srcdir)/monitor.s65 $(srcdir)/monitor.md5 $(INCDIR)/asm.m4h

monitor.r65: monitor.o65 $(OBJS)
monitor.r65: M4FLAGS=-DVERSION=$(MONITOR_VERS)
# monitor starts at address $F800 = 63488
monitor.r65: CODE_SEG=63488

VPATH += $(COMDIR) $(INCDIR)

lores.s65: lores.m4 symbols.m4h asm.m4h
disasm.s65: disasm.m4 symbols.m4h asm.m4h
debug.s65: debug.m4 symbols.m4h asm.m4h
paddles.s65: paddles.m4 symbols.m4h asm.m4h
display1.s65: display1.m4 symbols.m4h asm.m4h
math.s65: math.m4 symbols.m4h asm.m4h
display2.s65: display2.m4 symbols.m4h asm.m4h
cassette.s65: cassette.m4 symbols.m4h asm.m4h
keyin.s65: keyin.m4 symbols.m4h asm.m4h
cmd.s65: cmd.m4 symbols.m4h asm.m4h
vectors.s65: vectors.m4 symbols.m4h asm.m4h

define_new_md5: all
	$(MD5SUM) --binary monitor.a65 >$(srcdir)/monitor.md5

check_SCRIPTS=unit_test.sh
TESTS=$(check_SCRIPTS)

unit_test.sh:
	touch $@
	if test "X$(MD5SUM)" == X ; \
	then \
		echo "echo \"md5sum program not found, so cannot run this test\" >&2" >>$@ ; \
		echo "exit 1" >>$@ ; \
	else \
		echo "$(MD5SUM) --check --status --warn $(srcdir)/monitor.md5" >>$@ ; \
	fi
	chmod +x $@



CLEANFILES=unit_test.sh *.a65 *.r65 *.s65 *.o65
