include(`asm.m4h')
include(`symbols.m4h')
include(`zpabs.m4h')



MASTERDOS       JMP   DOSCOLD

DOSRELOC
                                ; (A3) --> DOSTOP
 LDA #$BF ;;; LDA   #>DOSTOP
                STAZ(A3H)
                LDX   #<DOSTOP
                STXZ(A3L)

                                ; check for existence of RAM,
                                ; starting at page $BF and working downwards
L1B0B           LDY   #0
                LDA   (A3L,X)   ; (note: X is always 0)
                STA   PROSCRTH
L1B11           TYA
                EOR   PROSCRTH
                STA   PROSCRTH
                TYA
                EOR   (A3L,X)   ; (note: X is always 0)
                STA   (A3L,X)   ; (note: X is always 0)
                CMP   PROSCRTH
                BNE   L1B24
                INY
                BNE   L1B11
                BEQ   L1B28

L1B24
                DECZ(A3H)
                BNE   L1B0B     ; always branches
ifelse(eval(VERSION < 320),1,`
L1B28           JSR   L1B28SR
',`
                                ; upon entry, A3 --> first byte of highest RAM page (e.g., $BF00)
L1B28
                LDAZ(A3H)
                AND   #$DF      ; $BF becomes $9F
                STAZ(A4H)
                STXZ(A4L)       ; (A4) --> $9F00
                LDA   (A4L,X)
                PHA             ; save (A4) in case we need to restore it below
                STA   PROSCRTH
L1B35           TYA
                EOR   PROSCRTH
                STA   PROSCRTH
                TYA
                EOR   (A3L,X)
                STA   (A4L,X)
                CMP   PROSCRTH
                BNE   L1B4C
                INY
                BNE   L1B35
                                ; comes here if 9F page maps to BF page
                LDY   A4H       ; $9F
                PLA             ; (A4)
                JMP   L1B51




L1B4C           PLA
                STA   (A4L,X)   ; restore (A4)
                LDY   A3H       ; $BF
')


                                ; upon entry Y contains highest destination page (e.g., $BF)
L1B51           INY
                STY   DSTPAGELIM ; page limit (e.g., $C0)
                SEC
                TYA
                SBC   PAGECNT
                STA   DSTPAGE   ; first page to reloc to (e.g., $9D)
                SEC
                SBC   SRCPAGE   ; first page to reloc from (e.g., $1D)
                BEQ   MASTERDOS ; if no reloc is necessary (already at dst addr), branch to boot routine



                STA   OFFSET    ; $80
                LDA   SRCPAGE   ; $1D
                STA   ADOSTART+1 ; fixup dos-start vector

                                ; fix this pointer inside DOS: it used to
                                ; point to us (the relocation), but since we
                                ; are doing the relocating now, change it
                                ; so it just goes right to the DOS cold-start routine
 LDA #$1D  ;;; LDA   #>DOSCOLD
                STA   TODOSCLD2+2
                LDA   #<DOSCOLD
                STA   TODOSCLD2+1


                                ; fix up DOS vectors (ranges defined
                                ; in FIXVECTBL).
                LDX   #0
                STXZ(A3L)

FIXVEC          LDA   FIXVECTBL,X
                TAY
                LDA   FIXVECTBL+1,X
                STAZ(A3H)
                JMP   L1B93

L1B86           CLC
                LDA   (A3L),Y
                ADC   OFFSET
                STA   (A3L),Y
                INY
                BNE   L1B93
                INCZ(A3H)
L1B93           INY
                BNE   L1B98
                INCZ(A3H)
L1B98
                LDAZ(A3H)
                CMP   FIXVECTBL+3,X
                BCC   L1B86
                TYA
                CMP   FIXVECTBL+2,X
                BCC   L1B86

                TXA
                CLC
                ADC   #4
                TAX
                CPX   FIXVECTBLSIZ
                BCC   FIXVEC



                                ; Do the fixups.
                                ; Look through the DOS machine code instructions
                                ; and look for absolute references to addresses
                                ; within the code that is being relocated, and
                                ; add the offset (in pages) to the high-order byte.
                LDX   #0
FIXCOD          STX   CURDIRNX
                LDA   FIXCODTBL,X
                STAZ(A3L)
                LDA   FIXCODTBL+1,X
                STAZ(A3H)

FIXCODLOOP      LDX   #0
                LDA   (A3L,X)
                JSR   INSDS2
                LDYZ(VOLDSK)
                CPY   #2
                BNE   FIXCODNOCH
                LDA   (A3L),Y
                CMP   SRCPAGE
                BCC   FIXCODNOCH
                CMP   SRCPAGELIM
                BCS   FIXCODNOCH
                ADC   OFFSET
                STA   (A3L),Y
FIXCODNOCH      SEC
                LDAZ(VOLDSK)
                ADCZ(A3L)
                STAZ(A3L)
                LDA   #$00
                ADCZ(A3H)
                STAZ(A3H)
                LDX   CURDIRNX
                CMP   FIXCODTBL+3,X
                BCC   FIXCODLOOP
                LDAZ(A3L)
                CMP   FIXCODTBL+2,X
                BCC   FIXCODLOOP

                TXA
                CLC
                ADC   #4
                TAX
                CPX   FIXCODTBLSIZ
                BCC   FIXCOD

                                ; Copy the image to the new location
RELOC           LDA   #$3F
                STAZ(A3H)
                LDY   DSTPAGELIM
                DEY
                STYZ(A4H)
                LDA   #0
                STAZ(A3L)
                STAZ(A4L)
                TAY
RELOOP2          LDA   (A3L),Y
                STA   (A4L),Y
                INY
                BNE   RELOOP2
                DEC   PAGECNT2
                BEQ   REDONE
                DECZ(A3H)
                DECZ(A4H)
                BNE   RELOOP2
REDONE
                                ; Done relocating, so now cold-start it
                JMP   IMGCOLVT



                                ; Table of vectors within DOS that we need to
                                ; fix up. Format:
                                ; START,LIMIT
FIXVECTBLSIZ    ASM_DATA(FIXVECTBLLIM-FIXVECTBL) ; size of table in bytes
FIXVECTBL       ASM_ADDR(ADOSFNB1)
                ASM_ADDR(CHAINTRY)
                ASM_ADDR(RUNTRY)
                ASM_ADDR(ADBSCERR)
                ASM_ADDR(IMGINTV+2)
                ASM_ADDR(IMGINTV+4)
                ASM_ADDR(IMGFPV)
                ASM_ADDR(IMGFPV+4)
                ASM_ADDR(IMGARAMV)
                ASM_ADDR(IMGARAMV+4)
                ASM_ADDR(IMGARAMV+6)
                ASM_ADDR(IMGARAMV+8)
                ASM_ADDR(ADRIOB)
                ASM_ADDR(FMXTNTRY)
                ASM_ADDR(ADROFIOB)
                ASM_ADDR(IBTYPE)
                ASM_ADDR(IBDCTP)
                ASM_ADDR(IBBUFP)
FIXVECTBLLIM

                ASM_RES(12)

                                ; Table of code within DOS that we need to
                                ; fix up. Format:
                                ; START,LIMIT
FIXCODTBLSIZ    ASM_DATA(FIXCODTBLLIM-FIXCODTBL) ; size of table in bytes
FIXCODTBL       ASM_ADDR(DOSCOLD)
                ASM_ADDR(CMDTXTBL)
                ASM_ADDR(FMXTNTRY)
                ASM_ADDR(CURDIRTK)
ifelse(eval(VERSION < 330),1,`
                ASM_ADDR(BOOT2)
',`
                ASM_ADDR(APPNDFLG)
')
                ASM_ADDR(NMPG2RD)
ifelse(eval(VERSION < 320),1,`
                ASM_ADDR(BOOT1-2)
                ASM_ADDR(BOOT2-$102) ; NONSENSE ENTRIES?
',`
ifelse(eval(VERSION < 330),1,`
                ASM_ADDR(BOOT1-2)
                ASM_ADDR(BOOT1-2) ; NONSENSE ENTRIES?
',`
                ASM_ADDR(WRITADR)
                ASM_ADDR(FREE1)
')
')
                ASM_ADDR(PRENIBL)
ifelse(eval(VERSION < 321),1,`
                ASM_ADDR(ONTABLE-1)
',`
ifelse(eval(VERSION < 330),1,`
                ASM_ADDR(ONTABLE+3)
',`
                ASM_ADDR(ONTABLE)
')
')
ifelse(eval(VERSION >= 331),1,`
                ASM_ADDR(CKIFAPND)
                ASM_ADDR(CMPATCH)
')
                ASM_ADDR(RWTS)
ifelse(eval(VERSION >= 330),1,`
                ASM_ADDR(SECFLGS)
                ASM_ADDR(CLOBCARD)
')
                ASM_ADDR(DOSLIM-1)
FIXCODTBLLIM

ifelse(eval(VERSION < 330),1,`
                ASM_RES(4)
')

SRCPAGE         ASM_DATA($1D) ;;; (>ADOSFNB1)
SRCPAGELIM      ASM_DATA($40) ;;; (>DOSLIM)

DSTPAGE         ASM_RES(1)
DSTPAGELIM      ASM_RES(1)

PAGECNT         ASM_DATA($23) ; >DOSLIM-ADOSFNB1  number of pages to be moved
OFFSET          ASM_RES(1)    ; number of pages to add, to reloc addresses
PAGECNT2        ASM_DATA($23) ; (redundant?)

ifelse(eval(VERSION < 320),1,`
                ASM_DATA($13)                ; UNUSED CODE???
                BMI   *-$46
                JSR   L1C7B
                LDA   #$80
                STA   $13
                PLA
                BNE   *-$2E
L1C7B           LDX   MEMSIZ
')
ifelse(eval(VERSION < 330),1,`
                LDA   MEMSIZ+1               ; UNUSED CODE???
                STX   FRETOP
                STA   FRETOP+1
                LDY   #$00
                STY   $8B
                LDA   $6D
                LDX   $6E
                STA   $9B
                STX   $9C
                LDA   #$55
                LDX   #$00
                STA   $5E
                STX   $5F
L1C97           CMP   $52
                BEQ   L1CA0
                JSR   *+$7F
                BEQ   L1C97
L1CA0           LDA   #$07
                STA   $8F
                LDA   VARTAB
                LDX   VARTAB+1
                STA   $5E
                STX   $5F
L1CAC           CPX   $6C
                BNE   L1CB4
                CMP   $6B
                BEQ   L1CB9
L1CB4           JSR   OUTHNDTB
                BEQ   L1CAC
L1CB9           STA   $94
                STX   $95
                LDA   #$03
                STA   $8F
L1CC1           LDA   $94
                LDX   $95
                CPX   $6E
                BNE   L1CD0
                CMP   $6D
                BNE   L1CD0
                JMP   *+$8C
L1CD0           STA   $5E
                STX   $00
DOSNMBF1        LDY   #$00
                LDA   ($5E),Y
                TAX
                INY
                LDA   ($5E),Y
                PHP
                INY
                LDA   ($5E),Y
                ADC   $94
                STA   $94
                INY
                LDA   ($5E),Y
                ADC   $95
                STA   $95
                PLP
                BPL   L1CC1
                TXA
                BMI   L1CC1
                ASM_ADDR($1CA6,$1BA6,$1AA6,$1A80,$5E65,$5E85)
                ASM_DATA($90,$02,$E6)
',`
                                             ; unused:
ifelse(eval(VERSION == 330),1,`
                ASM_RES($04)
')
                ASM_RES($52)
DOSNMBF1                                     ; this label represents the first DOS buffer
                ASM_RES($2D)
')
