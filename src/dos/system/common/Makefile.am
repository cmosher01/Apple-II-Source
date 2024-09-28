AUTOMAKE_OPTIONS=-Wno-override

INCDIR=$(srcdir)/../../../include
COMDIR=$(srcdir)/../common



SUFFIXES=.d16 .d13 .a65 .r65 .o65 .s65 .m4

ZERO_SEGS=-bd 0 -bb 0 -bz 0

.m4.s65:
	$(M4) -E -I $(INCDIR) -I $(COMDIR) $(M4FLAGS_LOCAL) $(M4FLAGS) $< >$@

.s65.o65:
	$(XA65) -C -MASM -XXA23 -R -c -bt 2048 $(ZERO_SEGS) -o $@ $(XA65FLAGS) $<

.o65.r65:
	$(LDO65) -bt $(CODE_SEG) $(ZERO_SEGS) -o $@ $(LD65FLAGS) $^

.r65.a65:
	$(RELOC65) -xt -o $@ $(RELOC65FLAGS) $<

.a65.d13:
	dd bs=256 skip=27 <$< >$@
	dd bs=256 count=27 <$< >>$@
	dd bs=256 count=184 </dev/zero >>$@
	printf "\x00\x11\x0C\x02" >>$@
	dd bs=1 count=35 </dev/zero >>$@
	printf "\x7A" >>$@
	dd bs=1 count=12 </dev/zero >>$@
	printf "\x23\x10\x00\x01" >>$@
	dd bs=1 count=200 </dev/zero >>$@
	dd bs=256 count=233 </dev/zero >>$@

.a65.d16:
	dd bs=256 skip=27 <$< >$@
	dd bs=256 count=27 <$< >>$@
	dd bs=256 count=235 </dev/zero >>$@
	printf "\x00\x11\x0F\x03" >>$@
	dd bs=1 count=35 </dev/zero >>$@
	printf "\x7A" >>$@
	dd bs=1 count=12 </dev/zero >>$@
	printf "\x23\x0D\x00\x01" >>$@
	dd bs=1 count=200 </dev/zero >>$@
	dd bs=256 count=287 </dev/zero >>$@



OBJS=reloc.o65 main.o65 filemgr.o65 boot1.o65 boot2.o65 rwts.o65 rwtsapi.o65
M4S=$(COMDIR)/reloc.m4 $(COMDIR)/main.m4 $(COMDIR)/filemgr.m4 $(COMDIR)/boot1.m4 $(COMDIR)/boot2.m4 $(COMDIR)/rwts.m4 $(COMDIR)/rwtsapi.m4



mypkgdir=$(prefix)/lib/apple2/dos/$(DOS_SECTORS)sector/dos$(DOS_VERS)

mypkg_DATA=dos.d$(DOS_SECTORS)
noinst_DATA=dos.md5
EXTRA_DIST=$(M4S) $(COMDIR)/symbols.m4h $(COMDIR)/zpabs.m4h $(srcdir)/dos.m4 $(srcdir)/dos.md5 $(INCDIR)/asm.m4h

dos.r65: dos.o65 $(OBJS)
dos.r65: M4FLAGS_LOCAL=-DVERSION=$(DOS_VERS)
# dos is assembled at address $1B00
dos.r65: CODE_SEG=6912

VPATH += $(COMDIR) $(INCDIR)

reloc.s65: reloc.m4 symbols.m4h asm.m4h
main.s65: main.m4 symbols.m4h asm.m4h
filemgr.s65: filemgr.m4 symbols.m4h asm.m4h
boot1.s65: boot1.m4 symbols.m4h asm.m4h
boot2.s65: boot2.m4 symbols.m4h asm.m4h
rwts.s65: rwts.m4 symbols.m4h asm.m4h
rwtsapi.s65: rwtsapi.m4 symbols.m4h asm.m4h

check_SCRIPTS=unit_test.sh
TESTS=$(check_SCRIPTS)

.SECONDARY:

unit_test.sh:
	touch $@
	if test "X$(MD5SUM)" == X ; \
	then \
		echo "echo \"md5sum program not found, so cannot run this test\" >&2" >>$@ ; \
		echo "exit 1" >>$@ ; \
	else \
		echo "$(MD5SUM) --status --check $(srcdir)/dos.md5" >>$@ ; \
	fi
	chmod +x $@



CLEANFILES=unit_test.sh *.d16 *.d13 *.a65 *.r65 *.o65 *.s65
