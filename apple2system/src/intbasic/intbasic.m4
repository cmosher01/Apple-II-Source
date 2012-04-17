include(`asm.m4h')
;
; Game BASIC
; (later named: Apple BASIC)
; (later named: Integer BASIC)
;
; Originally written by Steve Wozniak in 1975.
;
; Disassembled by Chris Mosher (using cc65)
; from the Apple I and Apple ][ ROMs @ $E000.
;
; Intended for the www.cc65.org assembler.
;
;
;
; This assembler source file will build an exact
; binary image of the Apple I or Apple ][ ROM version
; of "Integer BASIC".
;
; Define VERSION as 1 or 2 to specify which version to build.
; Apple I : VERSION=1
; Apple ][: VERSION=2
;
;
;
; For example to build the Apple ][ version:
;
;    ca65 -t apple2 -D VERSION=2 -o intbasic.obj intbasic.a65
;
; And link it with a command such as:
;
;    ld65 -C intbasic.cfg intbasic.obj
;
; where intbasic.cfg contains:
;
;    MEMORY { E000ROM: start=$E000, size=$2000, file="E000.ROM"; }
;    SEGMENTS { CODE: load=E000ROM, type=RO; }
;
; The resulting E000.ROM file will contain the built image.
;
;
;
; There are also 3 known bugs (in the Apple ][ version,
; but only 1 in the Apple I version). To build a version
; with these bugs FIXED, define BUGFIX. For example, to
; build the Apple ][ version with known bugs fixed:
;
;    ca65 -t apple2 -D VERSION=2 -D BUGFIX intbasic.a65
;


CTRL_C    =       $03 | %10000000
CR        =       $0D | %10000000



WNDWDTH   =       $21
CH        =       $24
CV        =       $25
GBAS      =       $26
H2        =       $2C
V2        =       $2D
PROMPT    =       $33
A1        =       $3C
A2        =       $3E
LOMEM     =       $4A
HIMEM     =       $4C
RND       =       $4E
NOUNSTKL  =       $50
SYNSTKH   =       $58
NOUNSTKH  =       $78
SYNSTKL   =       $80
NOUNSTKC  =       $A0
TXTNDXSTK =       $A8
TXTNDX    =       $C8
LEADBL    =       $C9
PP        =       $CA
PV        =       $CC
ACC       =       $CE
SRCH      =       $D0
TOKNDXSTK =       $D1
SRCH2     =       $D2
IFFLAG    =       $D4
CRFLAG    =       $D5
VERBNOW   =       $D6
PRFLAG    =       $D7
XSAVE     =       $D8
RUNFLAG   =       $D9
AUX       =       $DA
PR        =       $DC
PN        =       $DE
PX        =       $E0
P1        =       $E2
P2        =       $E4
P3        =       $E6
TOKNDX    =       $F1
PCON      =       $F2
AUTOINC   =       $F4
AUTOLN    =       $F6
AUTOFLAG  =       $F8
CHAR      =       $F9
LEADZR    =       $FA
FORNDX    =       $FB
GOSUBNDX  =       $FC
SYNSTKDX  =       $FD
SYNPAG    =       $FE

STACK     =       $0100

IN        =       $0200

ifelse(`VERSION',1,`

; PIA registers for the Apple I (one)
; for keyboard input and
; screen output
KBD       =       $D010
KBDCR     =       $D011
DSP       =       $D012

',`

; Apple ][ keyboard input
KBD       =       $C000
KBDSTRB   =       $C010

; Apple ][ Monitor ROM routines
PLOT      =       $F800
HLINE     =       $F819
VLINE     =       $F828
GBASCALC  =       $F847
SETCOL    =       $F864
PREAD     =       $FB1E
SETTXT    =       $FB39
SETGR     =       $FB40
VTAB      =       $FC22
NXTCHAR   =       $FD75
CROUT     =       $FD8E
COUT      =       $FDED
INPORT    =       $FE8B
OUTPORT   =       $FE95
WRITE     =       $FECD
WRITE0    =       $FECF
READ      =       $FEFD
BELL      =       $FF3A

')















BASIC:
        ifelse(`VERSION',1,`
        JMP     OLDENT
        ',`
        JSR     COLD
BASIC2: JMP     WARM
        ')



        ifelse(`VERSION',1,`
READKEY:
        LDA     KBDCR
        BPL     READKEY
        LDA     KBD

        ',`

DOPROMPT:
        STA     PROMPT
        JMP     COUT
        ')

        RTS



LE00C:  TXA
        AND     #$20
        BEQ     LE034
LE011:  LDA     #HICHAR(` ')
        STA     P2
        JMP     COUT
LE018:  LDA     #$20
LE01A:  CMP     CH
        BCS     LE02A
        LDA     #CR
        LDY     #$07
LE022:  JSR     COUT
        LDA     #HICHAR(` ')
        DEY
        BNE     LE022
LE02A:  LDY     #$00
        LDA     (P1),Y
        INC     P1
        BNE     LE034
        INC     P1+1
LE034:  RTS
COMMA_LIST:
        JSR     GET16BIT
        JSR     LE576
LE03B:  LDA     P1
        CMP     P3
        LDA     P1+1
        SBC     P3+1
        BCS     LE034
        JSR     UNPACK
        JMP     LE03B
LISTCMD:LDA     PP
        STA     P1
        LDA     PP+1
        STA     P1+1
        LDA     HIMEM
        STA     P3
        LDA     HIMEM+1
        STA     P3+1
        BNE     LE03B
LISTNUM:JSR     GET16BIT
        JSR     LE56D
        LDA     P2
        STA     P1
        LDA     P2+1
        STA     P1+1
        BCS     LE034
UNPACK: STX     XSAVE
        LDA     #HICHAR(` ')
        STA     LEADZR
        JSR     LE02A
        TYA
LE077:  STA     P2
        JSR     LE02A
        TAX
        JSR     LE02A
        JSR     LE51B
LE083:  JSR     LE018
        STY     LEADZR
        TAX
        BPL     LE0A3
        ASL
        BPL     LE077
        LDA     P2
        BNE     LE095
        JSR     LE011
LE095:  TXA
LE096:  JSR     COUT
LE099:  LDA     #$25
        JSR     LE01A
        TAX
        BMI     LE096
        STA     P2
LE0A3:  CMP     #$01
        BNE     LE0AC
        LDX     XSAVE
        ifelse(`VERSION',1,`
        JMP     LE3CD
        ',`
        JMP     CROUT
        ')
LE0AC:  PHA
        STY     ACC
        LDX     #>SYNTABL2
        STX     ACC+1
        CMP     #$51
        BCC     LE0BB
        DEC     ACC+1
        SBC     #$50
LE0BB:  PHA
        LDA     (ACC),Y
LE0BE:  TAX
        DEY
        LDA     (ACC),Y
        BPL     LE0BE
        CPX     #$C0
        BCS     LE0CC
        CPX     #$00
        BMI     LE0BE
LE0CC:  TAX
        PLA
        SBC     #$01
        BNE     LE0BB
        BIT     P2
        BMI     LE0D9
        JSR     LEFF8
LE0D9:  LDA     (ACC),Y
        BPL     LE0ED
        TAX
        AND     #$3F
        STA     P2
        CLC
        ADC     #$A0
        JSR     COUT
        DEY
        CPX     #$C0
        BCC     LE0D9
LE0ED:  JSR     LE00C
        PLA
        CMP     #$5D
        BEQ     LE099
        CMP     #$28
        BNE     LE083
        BEQ     LE099
PAREN_SUBSTR:
        JSR     LE118
        STA     NOUNSTKL,X
        CMP     NOUNSTKH,X
LE102:  BCC     LE115
LE104:  LDY     #$2B
LE106:  JMP     LE3E0
COMMA_SUBSTR:
        JSR     GETBYTE
        CMP     NOUNSTKL,X
        BCC     LE104
        JSR     LEFE4
        STA     NOUNSTKH,X
LE115:  JMP     HE823
LE118:  JSR     GETBYTE
        BEQ     LE104
        SEC
        SBC     #$01
        RTS
HE121:  JSR     LE118
        STA     NOUNSTKL,X
        CLC
        SBC     NOUNSTKH,X
        JMP     LE102
LE12C:  LDY     #$14
        BNE     LE106
DIMSTR: JSR     LE118
        INX
LE134:  LDA     NOUNSTKL,X
        STA     AUX
        ADC     ACC
        PHA
        TAY
        LDA     NOUNSTKH,X
        STA     AUX+1
        ADC     ACC+1
        PHA
        CPY     PP
        SBC     PP+1
        BCS     LE12C
        LDA     AUX
        ADC     #$FE
        STA     AUX
        LDA     #$FF
        TAY
        ADC     AUX+1
        STA     AUX+1
LE156:  INY
        LDA     (AUX),Y
        CMP     PV,Y
        BNE     DIMERR
        TYA
        BEQ     LE156
LE161:  PLA
        STA     (AUX),Y
        STA     PV,Y
        DEY
        BPL     LE161
        INX
        RTS
        NOP
DIMERR: LDY     #$80
LE16F:  BNE     LE106
INPUTSTR:
        LDA     #$00
        JSR     LE70A
        LDY     #$02
        STY     NOUNSTKH,X
        JSR     LE70A
        ifelse(`VERSION',1,`
        LDA     #$BF
        JSR     COUT
        LDY     #$00
        JSR     LE29E
        STY     $78,X
        NOP
        NOP
        NOP
        ',`
        STX     XSAVE
        TAX
        INC     PROMPT ; PROMPT FROM > TO ?
        JSR     LF351
        DEC     PROMPT ; PROMPT FROM ? TO >
        TXA
        LDX     XSAVE
        STA     NOUNSTKH,X
        ')
HE18C:  LDA     NOUNSTKL+1,X
        STA     ACC
        LDA     NOUNSTKH+1,X
        STA     ACC+1
        INX
        INX
        JSR     LE1BC
LE199:  LDA     RND,X
        CMP     $76,X
        BCS     LE1B4
        INC     RND,X
        TAY
        LDA     (ACC),Y
        LDY     NOUNSTKL,X
        CPY     P2
        BCC     LE1AE
        LDY     #$83
        BNE     LE16F
LE1AE:  STA     (AUX),Y
        INC     NOUNSTKL,X
        BCC     LE199
LE1B4:  LDY     NOUNSTKL,X
        TXA
        STA     (AUX),Y
        ifelse(`VERSION',1,`
        INX
        INX
        RTS
        ',`
        JMP     LF223
        ')
LE1BC:  LDA     NOUNSTKL+1,X
        STA     AUX
        SEC
        SBC     #$02
        STA     P2
        LDA     NOUNSTKH+1,X
        STA     AUX+1
        SBC     #$00
        STA     P2+1
        LDY     #$00
        LDA     (P2),Y
        CLC
        SBC     AUX
        STA     P2
        RTS
HE1D7:  LDA     NOUNSTKL+3,X
        STA     ACC
        LDA     NOUNSTKH+3,X
        STA     ACC+1
        LDA     NOUNSTKL+1,X
        STA     AUX
        LDA     NOUNSTKH+1,X
        STA     AUX+1
        INX
        INX
        INX
        LDY     #$00
        STY     NOUNSTKH,X
        STY     NOUNSTKC,X
        INY
        STY     NOUNSTKL,X
LE1F3:  LDA     HIMEM+1,X
        CMP     $75,X
        PHP
        PHA
        LDA     RND+1,X
        CMP     $77,X
        BCC     LE206
        PLA
        PLP
        BCS     LE205
LE203:  LSR     NOUNSTKL,X
LE205:  RTS
LE206:  TAY
        LDA     (ACC),Y
        STA     P2
        PLA
        TAY
        PLP
        BCS     LE203
        LDA     (AUX),Y
        CMP     P2
        BNE     LE203
        INC     RND+1,X
        INC     HIMEM+1,X
        BCS     LE1F3
HE21C:  JSR     HE1D7
        JMP     NOT
MULT:   JSR     LE254
LE225:  ASL     ACC
        ROL     ACC+1
        BCC     LE238
        CLC
        LDA     P3
        ADC     AUX
        STA     P3
        LDA     P3+1
        ADC     AUX+1
        STA     P3+1
LE238:  DEY
        BEQ     LE244
        ASL     P3
        ROL     P3+1
        BPL     LE225
        JMP     LE77E
LE244:  LDA     P3
        JSR     LE708
        LDA     P3+1
        STA     NOUNSTKC,X
        ASL     P2+1
        BCC     LE279
        JMP     NEGATE
LE254:  LDA     #$55
        STA     P2+1
        JSR     LE25B
LE25B:  LDA     ACC
        STA     AUX
        LDA     ACC+1
        STA     AUX+1
        JSR     GET16BIT
        STY     P3
        STY     P3+1
        LDA     ACC+1
        BPL     LE277
        DEX
        ASL     P2+1
        JSR     NEGATE
        JSR     GET16BIT
LE277:  LDY     #$10
LE279:  RTS
MOD:    JSR     LEE6C
        BEQ     LE244
        ASM_DATA($FF)


LE280:
        ifelse(`VERSION',1,`
        CMP     #$84
        BNE     LE286
        LSR     $F8
LE286:  CMP     #$DF
        BEQ     LE29B
        CMP     #$9B
        BEQ     LE294
        STA     $0200,Y
        INY
        BPL     LE29E
LE294:  LDY     #ERRMSG19
        JSR     ERRMSG
LE299:  LDY     #$01
LE29B:  DEY
        BMI     LE294
LE29E:  JSR     READKEY
        NOP
        NOP
        JSR     COUT
        CMP     #CR
        BNE     LE280
        LDA     #$DF
        STA     $0200,Y
        RTS
OLDENT: JSR     OLDCOLD
WARM:   JSR     LE3CD
LE2B6:  ASM_DATA($46)
LE2B7:  CMP     $BEA9,Y
        JSR     COUT

        ',`

        INC     PROMPT
        LDY     #$00
        JSR     LE3CE
        DEC     PROMPT
        RTS
SCRN:   JSR     GETBYTE
        LSR
        PHP
        JSR     GBASCALC
        JSR     GETBYTE
        TAY
        LDA     (GBAS),Y
        PLP
        BCC     LE29F
        LSR
        LSR
        LSR
LE29E:  LSR     ; TODO FIX
LE29F:  AND     #$0F
        LDY     #$00
        JSR     LE708
        STY     NOUNSTKC,X
        DEY
        STY     PRFLAG
COMMA_SCRN:
        RTS
        ASM_RES(4,$FF)
        JSR     OLDCOLD
WARM:   JSR     CROUT
LE2B6:  LSR     RUNFLAG
        LDA     #HICHAR(`>') ; THE PROMPT
        JSR     DOPROMPT
        ')

        LDY     #$00
        STY     LEADZR
        BIT     AUTOFLAG
        BPL     LE2D1
        LDX     AUTOLN
        LDA     AUTOLN+1
        JSR     LE51B
        LDA     #HICHAR(` ')
        JSR     COUT
LE2D1:  LDX     #$FF
        TXS
        ifelse(`VERSION',1,`
        JSR     LE29E
        ',`
        JSR     LE3CE
        ')
        STY     TOKNDX
        TXA
        STA     TXTNDX
        LDX     #$20
        JSR     LE491
        LDA     TXTNDX
        ADC     #$00
        STA     PX
        LDA     #$00
        TAX
        ADC     #$02
        STA     PX+1
        LDA     (PX,X)
        AND     #$F0
        CMP     #$B0
        BEQ     LE2F9
        JMP     LE883
LE2F9:  LDY     #$02
LE2FB:  LDA     (PX),Y
        STA     PV+1,Y
        DEY
        BNE     LE2FB
        JSR     LE38A
        LDA     TOKNDX
        SBC     TXTNDX
        CMP     #$04
        BEQ     LE2B6
        STA     (PX),Y
        LDA     PP
        SBC     (PX),Y
        STA     P2
        LDA     PP+1
        SBC     #$00
        STA     P2+1
        LDA     P2
        CMP     PV
        LDA     P2+1
        SBC     PV+1
        BCC     LE36B
LE326:  LDA     PP
        SBC     (PX),Y
        STA     P3
        LDA     PP+1
        SBC     #$00
        STA     P3+1
        LDA     (PP),Y
        STA     (P3),Y
        INC     PP
        BNE     LE33C
        INC     PP+1
LE33C:  LDA     P1
        CMP     PP
        LDA     P1+1
        SBC     PP+1
        BCS     LE326
LE346:  LDA     P2,X
        STA     PP,X
        DEX
        BPL     LE346
        LDA     (PX),Y
        TAY
LE350:  DEY
        LDA     (PX),Y
        STA     (P3),Y
        TYA
        BNE     LE350
        BIT     AUTOFLAG
        BPL     LE365
LE35C:  LDA     AUTOLN+1,X
        ADC     AUTOINC+1,X
        STA     AUTOLN+1,X
        INX
        BEQ     LE35C
LE365:  BPL     LE3E5
        BRK
        BRK
        BRK
        BRK
LE36B:  LDY     #$14
        BNE     LE3E0
COMMA_DEL:
        JSR     GET16BIT
        LDA     P1
        STA     P3
        LDA     P1+1
        STA     P3+1
        JSR     LE575
        LDA     P1
        STA     P2
        LDA     P1+1
        STA     P2+1
        BNE     LE395
DEL:    JSR     GET16BIT
LE38A:  JSR     LE56D
        LDA     P3
        STA     P1
        LDA     P3+1
        STA     P1+1
LE395:  LDY     #$00
LE397:  LDA     PP
        CMP     P2
        LDA     PP+1
        SBC     P2+1
        BCS     LE3B7
        LDA     P2
        BNE     LE3A7
        DEC     P2+1
LE3A7:  DEC     P2
        LDA     P3
        BNE     LE3AF
        DEC     P3+1
LE3AF:  DEC     P3
        LDA     (P2),Y
        STA     (P3),Y
        BCC     LE397
LE3B7:  LDA     P3
        STA     PP
        LDA     P3+1
        STA     PP+1
        RTS

LE3C0:  JSR     COUT
        INY
ERRMSG: LDA     ERRMSGTBL,Y
        BMI     LE3C0



        ifelse(`VERSION',1,`

COUT:   CMP     #CR
        BNE     LE3D3
LE3CD:  LDA     #$00
        STA     $24
        LDA     #CR
LE3D3:  INC     $24
LE3D5:  BIT     DSP
        BMI     LE3D5
        STA     DSP

        ',`

        ORA     #$80
        JMP     COUT
LE3CE:  TYA
        TAX
        JSR     NXTCHAR
        TXA
        TAY
        LDA     #$DF
        STA     IN,Y
        LDX     #$FF
        RTS

        ')



        RTS
LE3DE:  LDY     #$06
LE3E0:  JSR     LEED3
        BIT     RUNFLAG
LE3E5:  BMI     LE3EA
        JMP     LE2B6
LE3EA:  JMP     LEB9A
LE3ED:  ROL
        ADC     #$A0
        CMP     IN,X
        BNE     LE448
        LDA     (SYNPAG),Y
        ASL
        BMI     LE400
        DEY
        LDA     (SYNPAG),Y
        BMI     LE428
        INY
LE400:  STX     TXTNDX
        TYA
        PHA
        LDX     #$00
        LDA     (SYNPAG,X)
        TAX
LE409:  LSR
        ifelse(`VERSION',1,`
        EOR     #%01001000
        ',`
        EOR     #%01000000
        ')
        ORA     (SYNPAG),Y
        CMP     #$C0
        BCC     LE413
        INX
LE413:  INY
        BNE     LE409
        PLA
        TAY
        TXA
        ifelse(`VERSION',1,`
        JMP     LE4C0
        ',`
        JMP     LF2F8
        ')
LE41C:  INC     TOKNDX
        LDX     TOKNDX
        BEQ     LE3DE
        STA     IN,X
LE425:  RTS
LE426:  LDX     TXTNDX
LE428:  LDA     #HICHAR(` ')
LE42A:  INX
        CMP     IN,X
        BCS     LE42A
        LDA     (SYNPAG),Y
        AND     #$3F
        LSR
        BNE     LE3ED
        LDA     IN,X
        BCS     LE442
        ADC     #$3F
        CMP     #$1A
        BCC     LE4B1
LE442:  ADC     #$4F
        CMP     #$0A
        BCC     LE4B1
LE448:  LDX     SYNSTKDX
LE44A:  INY
        LDA     (SYNPAG),Y
        AND     #$E0
        CMP     #$20
        BEQ     LE4CD
        LDA     TXTNDXSTK,X
        STA     TXTNDX
        LDA     TOKNDXSTK,X
        STA     TOKNDX
LE45B:  DEY
        LDA     (SYNPAG),Y
        ASL
        BPL     LE45B
        DEY
        BCS     LE49C
        ASL
        BMI     LE49C
        LDY     SYNSTKH,X
        STY     SYNPAG+1
        LDY     SYNSTKL,X
        INX
        BPL     LE44A
LE470:  BEQ     LE425
        CMP     #$7E
        BCS     LE498
        DEX
        BPL     LE47D
        ; CALL-APPLE, MAR 1983, P. 114
ifdef(`BUGFIX',
        LDY     #ERRMSG04
        ',`
        LDY     #ERRMSG01
        ')
        BPL     LE4A6
LE47D:  STY     SYNSTKL,X
        LDY     SYNPAG+1
        STY     SYNSTKH,X
        LDY     TXTNDX
        STY     TXTNDXSTK,X
        LDY     TOKNDX
        STY     TOKNDXSTK,X
        AND     #$1F
        TAY
        LDA     SYNTABLNDX,Y
LE491:  ASL
        TAY
        LDA     #$76
;TODO        LDA     #SYNTABL>>9
        ROL
        STA     SYNPAG+1
LE498:  BNE     LE49B
        INY
LE49B:  INY
LE49C:  STX     SYNSTKDX
        LDA     (SYNPAG),Y
        BMI     LE426
        BNE     LE4A9
        LDY     #$0E
LE4A6:  JMP     LE3E0
LE4A9:  CMP     #$03
        BCS     LE470
        LSR
        LDX     TXTNDX
        INX
LE4B1:  LDA     IN,X
        BCC     LE4BA
        CMP     #$A2
        BEQ     LE4C4
LE4BA:  CMP     #$DF
        BEQ     LE4C4
        STX     TXTNDX
LE4C0:  JSR     LE41C
        INY
LE4C4:  DEY
        LDX     SYNSTKDX
LE4C7:  LDA     (SYNPAG),Y
        DEY
        ASL
        BPL     LE49C
LE4CD:  LDY     SYNSTKH,X
        STY     SYNPAG+1
        LDY     SYNSTKL,X
        INX
        LDA     (SYNPAG),Y
        AND     #$9F
        BNE     LE4C7
        STA     PCON
        STA     PCON+1
        TYA
        PHA
        STX     SYNSTKDX
        LDY     SRCH,X
        STY     LEADBL
        CLC
LE4E7:  LDA     #$0A
        STA     CHAR
        LDX     #$00
        INY
        LDA     IN,Y
        AND     #$0F
LE4F3:  ADC     PCON
        PHA
        TXA
        ADC     PCON+1
        BMI     LE517
        TAX
        PLA
        DEC     CHAR
        BNE     LE4F3
        STA     PCON
        STX     PCON+1
        CPY     TOKNDX
        BNE     LE4E7
        LDY     LEADBL
        INY
        STY     TOKNDX
        JSR     LE41C
        PLA
        TAY
        LDA     PCON+1
        BCS     LE4C0
LE517:  LDY     #$00
        BPL     LE4A6
LE51B:  STA     PCON+1
        STX     PCON
        LDX     #$04
        STX     LEADBL
LE523:  LDA     #$B0
        STA     CHAR
LE527:  LDA     PCON
        CMP     NUMLOW,X
        LDA     PCON+1
        SBC     NUMHI,X
        BCC     LE540
        STA     PCON+1
        LDA     PCON
        SBC     NUMLOW,X
        STA     PCON
        INC     CHAR
        BNE     LE527
LE540:  LDA     CHAR
        INX
        DEX
        BEQ     LE554
        CMP     #$B0
        BEQ     LE54C
        STA     LEADBL
LE54C:  BIT     LEADBL
        BMI     LE554
        LDA     LEADZR
        BEQ     LE55F
LE554:  JSR     COUT
        BIT     AUTOFLAG
        BPL     LE55F
        STA     IN,Y
        INY
LE55F:  DEX
        BPL     LE523
        RTS
NUMLOW:
        ASM_DATA(<1)
        ASM_DATA(<10)
        ASM_DATA(<100)
        ASM_DATA(<1000)
        ASM_DATA(<10000)

NUMHI:
        ASM_DATA(>1)
        ASM_DATA(>10)
        ASM_DATA(>100)
        ASM_DATA(>1000)
        ASM_DATA(>10000)

LE56D:  LDA     PP
        STA     P3
        LDA     PP+1
        STA     P3+1
LE575:  INX
LE576:  LDA     P3+1
        STA     P2+1
        LDA     P3
        STA     P2
        CMP     HIMEM
        LDA     P2+1
        SBC     HIMEM+1
        BCS     LE5AC
        LDY     #$01
        LDA     (P2),Y
        SBC     ACC
        INY
        LDA     (P2),Y
        SBC     ACC+1
        BCS     LE5AC
        LDY     #$00
        LDA     P3
        ADC     (P2),Y
        STA     P3
        BCC     LE5A0
        INC     P3+1
        CLC
LE5A0:  INY
        LDA     ACC
        SBC     (P2),Y
        INY
        LDA     ACC+1
        SBC     (P2),Y
        BCS     LE576
LE5AC:  RTS
NEW:    LSR     AUTOFLAG
        LDA     HIMEM
        STA     PP
        LDA     HIMEM+1
        STA     PP+1
CLR:    LDA     LOMEM
        STA     PV
        LDA     LOMEM+1
        STA     PV+1
        LDA     #$00
        STA     FORNDX
        STA     GOSUBNDX
        STA     SYNPAG
        LDA     #$00
        STA     $1D
        RTS
LE5CC:  LDA     SRCH

        ifelse(`VERSION',1,`
        ADC     #$05
        STA     $D2
        LDA     $D1
        ADC     #$00
        STA     $D3
        LDA     $D2
        CMP     $CA
        LDA     $D3
        SBC     $CB
        BCC     LE5E5
        ',`')

LE5CE:  JMP     LE36B



        ifelse(`VERSION',1,`
LE5E5:  LDA     $CE
        STA     ($D0),Y
        LDA     $CF
        INY
        STA     ($D0),Y
        LDA     $D2
        INY
        STA     ($D0),Y
        LDA     $D3
        INY
        STA     ($D0),Y
        LDA     #$00
        INY
        STA     ($D0),Y
        INY
        STA     ($D0),Y
        LDA     $D2
        STA     $CC
        LDA     $D3
        STA     $CD
        LDA     $D0
        BCC     LE64F
LE60C:  STA     $CE
        STY     $CF
        JSR     GETVERB
        BMI     LE623
        CMP     #$40
        BEQ     LE623
        JMP     LE628
        ASL     $C9
        EOR     #$D0
        ASM_DATA($07)
        LDA     #$49
LE623:  STA     $CF
        JSR     GETVERB
LE628:  LDA     $4B
        STA     $D1
        LDA     $4A
LE62E:  STA     $D0
        CMP     $CC
        LDA     $D1
        SBC     $CD
        BCS     LE5CC
        LDA     ($D0),Y
        INY
        CMP     $CE
        BNE     LE645
        LDA     ($D0),Y
        CMP     $CF
        BEQ     LE653
LE645:  INY
        LDA     ($D0),Y
        PHA
        INY
        LDA     ($D0),Y
        STA     $D1
        PLA
LE64F:  LDY     #$00
        BEQ     LE62E
LE653:  LDA     $D0
        ADC     #$03
        JSR     LE70A
        LDA     $D1
        ADC     #$00
        STA     $78,X
        LDA     $CF
        CMP     #$40
        BNE     LE682
        DEY
        TYA
        JSR     LE70A
        DEY
        STY     $78,X
        LDY     #$03
LE670:  INC     $78,X
        INY
        LDA     ($D0),Y
        BMI     LE670
        BPL     LE682

        ',`

LE5D1:  LDY     #$FF
LE5D3:  STY     XSAVE
LE5D5:  INY
        LDA     (PX),Y
        BMI     LE5E0
        CMP     #$40
        BNE     LE646
        STA     XSAVE
LE5E0:  CMP     (SRCH),Y
        BEQ     LE5D5
LE5E4:  LDA     (SRCH),Y
LE5E6:  INY
        LSR
        BNE     LE5E4
        LDA     (SRCH),Y
        PHA
        INY
        LDA     (SRCH),Y
        TAY
        PLA
LE5F2:  STA     SRCH
        STY     TOKNDXSTK
        CMP     PV
        BNE     LE5D1
        CPY     PV+1
        BNE     LE5D1
        LDY     #$00
LE600:  INY
        LDA     (PX),Y
        BMI     LE600
        EOR     #$40
        BEQ     LE600
        TYA
        ADC     #$04
        PHA
        ADC     SRCH
        TAY
        LDA     TOKNDXSTK
        ADC     #$00
        PHA
        CPY     PP
        SBC     PP+1
        BCS     LE5CE
        STY     PV
        PLA
        STA     PV+1
        PLA
        TAY
        LDA     #$00
        DEY
        STA     (SRCH),Y
        DEY
        STA     (SRCH),Y
        DEY
        LDA     PV+1
        STA     (SRCH),Y
        DEY
        LDA     PV
        STA     (SRCH),Y
        DEY
        LDA     #$00
LE637:  STA     (SRCH),Y
        DEY
        BMI     LE5D3
        LDA     (PX),Y
        BNE     LE637
LE640:  LDA     LOMEM
        LDY     LOMEM+1
        BNE     LE5F2
LE646:  LDA     (SRCH),Y
        CMP     #$40
        BCS     LE5E6
        STA     SYNSTKL+31,X
        TYA
        ADC     #$03
        PHA
        ADC     SRCH
        JSR     LE70A
LE657:  JSR     GETVERB
        DEY
        BNE     LE657
        TYA
        ADC     TOKNDXSTK
        STA     NOUNSTKH,X
        PLA
        BIT     XSAVE
        BMI     LE684
        TAY
        LDA     #$00
        JSR     LE70A
        STA     NOUNSTKH,X
LE66F:  LDA     (SRCH),Y
        BPL     LE682
        INC     NOUNSTKH,X
        INY
        BNE     LE66F
        ASM_DATA($09)

        ')

LE679:  LDA     #$00
        STA     IFFLAG
        STA     CRFLAG
        LDX     #$20
LE681:  PHA
LE682:  LDY     #$00
LE684:  LDA     (PX),Y
LE686:  BPL     LE6A0
        ASL
        ifelse(`VERSION',1,`
        BMI     LE60C
        ',`
        BMI     LE640
        ')
        JSR     GETVERB
        JSR     LE708
        JSR     GETVERB
        STA     NOUNSTKC,X
LE696:  BIT     IFFLAG
        BPL     LE69B
        DEX
LE69B:  JSR     GETVERB
        BCS     LE686
LE6A0:  CMP     #$28
        BNE     LE6C3
        LDA     PX
        JSR     LE70A
        LDA     PX+1
        STA     NOUNSTKH,X
        BIT     IFFLAG
        BMI     LE6BC
        LDA     #$01
        JSR     LE70A
        LDA     #$00
        STA     NOUNSTKH,X
LE6BA:  INC     NOUNSTKH,X
LE6BC:  JSR     GETVERB
        BMI     LE6BA
        BCS     LE696
LE6C3:  BIT     IFFLAG
        BPL     LE6CD
        CMP     #$04
        BCS     LE69B
        LSR     IFFLAG
LE6CD:  TAY
        STA     VERBNOW
        LDA     LE980,Y
        AND     #%01010101
        ASL
        STA     PRFLAG
LE6D8:  PLA
        TAY
        LDA     LE980,Y
        AND     #%10101010
        CMP     PRFLAG
        BCS     LE6EC
        TYA
        PHA
        ifelse(`VERSION',1,`
        JSR     GETVERB
        ',`
        JSR     LF3EB
        ')
        LDA     VERBNOW
        BCC     LE681
LE6EC:  LDA     VERBADRL,Y
        STA     ACC
        LDA     VERBADRH,Y
        STA     ACC+1
        JSR     LE6FC
        JMP     LE6D8
LE6FC:  JMP     (ACC)

GETVERB:
        INC     PX
        BNE     LE705
        INC     PX+1
LE705:  LDA     (PX),Y
        RTS
LE708:  STY     $77,X
LE70A:  DEX
        BMI     LE710
        STA     NOUNSTKL,X
        RTS
LE710:  LDY     #$66
LE712:  JMP     LE3E0

GET16BIT: ;get a 16-bit value
        LDY     #$00
        LDA     NOUNSTKL,X
        STA     ACC
        LDA     NOUNSTKC,X
        STA     ACC+1
        LDA     NOUNSTKH,X
        BEQ     LE731
        STA     ACC+1
        LDA     (ACC),Y
        PHA
        INY
        LDA     (ACC),Y
        STA     ACC+1
        PLA
        STA     ACC
        DEY
LE731:  INX
        RTS

HE733:  JSR     HE74A
NOT:    JSR     GET16BIT
        TYA
        JSR     LE708
        STA     NOUNSTKC,X
        CMP     ACC
        BNE     LE749
        CMP     ACC+1
        BNE     LE749
        INC     NOUNSTKL,X
LE749:  RTS
HE74A:  JSR     SUBTRACT
        JSR     SGN
ABS:    JSR     GET16BIT
        BIT     ACC+1
        BMI     LE772
LE757:  DEX
LE758:  RTS
SGN:    JSR     GET16BIT
        LDA     ACC+1
        BNE     LE764
        LDA     ACC
        BEQ     LE757
LE764:  LDA     #$FF
        JSR     LE708
        STA     NOUNSTKC,X
        BIT     ACC+1
        BMI     LE758
NEGATE: JSR     GET16BIT
LE772:  TYA
        SEC
        SBC     ACC
        JSR     LE708
        TYA
        SBC     ACC+1
        BVC     LE7A1
LE77E:  LDY     #$00
        BPL     LE712
SUBTRACT:
        JSR     NEGATE
ADDITION:
        JSR     GET16BIT
        LDA     ACC
        STA     AUX
        LDA     ACC+1
        STA     AUX+1
        JSR     GET16BIT
LE793:  CLC
        LDA     ACC
        ADC     AUX
        JSR     LE708
        LDA     ACC+1
        ADC     AUX+1
        BVS     LE77E
LE7A1:  STA     NOUNSTKC,X
POSITIVE:
        RTS

TAB:
        ifelse(`VERSION',1,`
        JSR     GET16BIT
        ',`
        JSR     GETBYTE
        ')

        ifelse(`VERSION',1,`
        LDY     $CE
        BEQ     LE7B0
        DEY
        LDA     $CF
        BEQ     LE7BC
LE7B0:  RTS
        ',`
        TAY
        BNE     LE7AD
        JMP     LEECB
LE7AD:  DEY
LE7AE:  JMP     LF3F4
        ')

LE7B1:  LDA     CH
        ORA     #$07
        TAY
        INY
LE7B7:  ifelse(`VERSION',1,`
        LDA     #HICHAR(` ')
        JSR     COUT
LE7BC:  CPY     $24
        ',`
        BNE     LE7AE
        INY
        BNE     LE7B1
        ')
        BCS     LE7B7
        RTS
ifelse(eval(VERSION` > 1'),1,`
        BRK
        BRK
        ',`')



HE7C1:  JSR     LE7B1
PRNTNUM:JSR     GET16BIT
LE7C7:  LDA     ACC+1
        BPL     LE7D5
        LDA     #$AD
        JSR     COUT
        JSR     LE772
        BVC     PRNTNUM
LE7D5:  DEY
        STY     CRFLAG
        STX     ACC+1
        LDX     ACC
        JSR     LE51B
        LDX     ACC+1
        RTS
AUTO:   JSR     GET16BIT
        LDA     ACC
        STA     AUTOLN
        LDA     ACC+1
        STA     AUTOLN+1
        DEY
        STY     AUTOFLAG
        INY
        LDA     #$0A
LE7F3:  STA     AUTOINC
        STY     AUTOINC+1
        RTS
COMMA_AUTO:
        JSR     GET16BIT
        LDA     ACC
        LDY     ACC+1
        BPL     LE7F3
HE801:  JSR     GET16BIT
        LDA     NOUNSTKL,X
        STA     AUX
        LDA     NOUNSTKH,X
        STA     AUX+1
        LDA     ACC
        STA     (AUX),Y
        INY
        LDA     ACC+1
        ifelse(`VERSION',1,`
        STA     ($DA),Y
        INX
        ',`
        JMP     LF207
        ')
LET:    RTS
BEGIN_LINE:
        PLA
        PLA
COLON:  BIT     CRFLAG
        BPL     LE822
PRINT_CR:
        ifelse(`VERSION',1,`
        JSR     LE3CD
        ',`
        JSR     CROUT
        ')
HE820:  LSR     CRFLAG
LE822:  RTS
HE823:  LDY     #$FF
        STY     PRFLAG
RIGHT_PAREN:
        RTS
IF:     JSR     LEFCD
        BEQ     LE834
        LDA     #$25
        STA     VERBNOW
        DEY
        STY     IFFLAG
LE834:  INX
        RTS
LE836:  LDA     PP
        LDY     PP+1
        BNE     LE896
GOSUB:  LDY     #$41
        LDA     GOSUBNDX
        ifelse(`VERSION',1,`
MAXGOSUB = $08
        ',`
MAXGOSUB = $10
        ')
        CMP     #MAXGOSUB
        BCS     LE8A2
        TAY
        INC     GOSUBNDX
        LDA     PX
        STA     STACK,Y
        LDA     PX+1
        STA     STACK+(1*MAXGOSUB),Y
        LDA     PR
        STA     STACK+(2*MAXGOSUB),Y
        LDA     PR+1
        STA     STACK+(3*MAXGOSUB),Y
GOTO:   JSR     GET16BIT
        JSR     LE56D
        BCC     LE867
        LDY     #$37
        BNE     LE8A2
LE867:  LDA     P2
        LDY     P2+1
LE86B:  STA     PR
        STY     PR+1
        ifelse(`VERSION',1,`
        BIT     KBDCR
        BMI     LE8C3
        ',`')
        CLC
        ADC     #$03
        BCC     LE875
        INY
LE875:  LDX     #$FF
        STX     RUNFLAG
        TXS
        STA     PX
        STY     PX+1
        ifelse(eval(VERSION` > 1'),1,`
        JSR     LF02E
        LDY     #$00
        ',`')
LE883:  JSR     LE679
        BIT     RUNFLAG
        BPL     ENDCMD
        CLC
        LDY     #$00
        LDA     PR
        ADC     (PR),Y
        LDY     PR+1
        BCC     LE896
        INY
LE896:  CMP     HIMEM
        BNE     LE86B
        CPY     HIMEM+1
        BNE     LE86B
        ifelse(`VERSION',1,`
        LDY     #$34
        ',`
        LDY     #$31
        ')
        LSR     RUNFLAG
LE8A2:  JMP     LE3E0
RETURN: LDY     #$4A
        LDA     GOSUBNDX
        BEQ     LE8A2
        DEC     GOSUBNDX
        TAY
        LDA     STACK+(2*MAXGOSUB)-1,Y
        STA     PR
        LDA     STACK+(3*MAXGOSUB)-1,Y
        STA     PR+1
        LDX     !SYNPAG+1,Y
        LDA     STACK+(1*MAXGOSUB)-1,Y
LE8BE:  TAY
        TXA
        JMP     LE875
LE8C3:  LDY     #ERRMSG12
        JSR     ERRMSG
        LDY     #$01
        LDA     (PR),Y
        TAX
        INY
        LDA     (PR),Y
        JSR     LE51B
ENDCMD: JMP     WARM
LE8D6:  DEC     FORNDX
NEXT:   LDY     #$5B
        LDA     FORNDX
LE8DC:  BEQ     LE8A2
        TAY
        LDA     NOUNSTKL,X
        CMP     STACK+($4*MAXFOR)-1,Y
        BNE     LE8D6
        LDA     NOUNSTKH,X
        CMP     STACK+($5*MAXFOR)-1,Y
        BNE     LE8D6
        LDA     STACK+($6*MAXFOR)-1,Y
        STA     AUX
        LDA     STACK+($7*MAXFOR)-1,Y
        STA     AUX+1
        JSR     GET16BIT
        DEX
        JSR     LE793
        JSR     HE801
        DEX
        LDY     FORNDX
        LDA     STACK+($D*MAXFOR)-1,Y
        STA     SYNSTKL+31,X
        LDA     STACK+($C*MAXFOR)-1,Y
        LDY     #$00
        JSR     LE708
        JSR     SUBTRACT
        JSR     SGN
        JSR     GET16BIT
        LDY     FORNDX
        LDA     ACC
        BEQ     LE925
        EOR     STACK+($7*MAXFOR)-1,Y
        BPL     LE937
LE925:  LDA     STACK+($8*MAXFOR)-1,Y
        STA     PR
        LDA     STACK+($9*MAXFOR)-1,Y
        STA     PR+1
        LDX     STACK+($A*MAXFOR)-1,Y
        LDA     STACK+($B*MAXFOR)-1,Y
        BNE     LE8BE
LE937:  DEC     FORNDX
        RTS
FOR:    LDY     #$54
        LDA     FORNDX
        ifelse(`VERSION',1,`
MAXFOR = $08
        ',`
MAXFOR = $10
        ')
        CMP     #MAXFOR
        BEQ     LE8DC
        INC     FORNDX
        TAY
        LDA     NOUNSTKL,X
        STA     STACK+(4*MAXFOR),Y
        LDA     NOUNSTKH,X
        ifelse(`VERSION',1,`
        STA     $0128,Y
        ',`
        JMP     LF288
        ')
        RTS
TO:     JSR     GET16BIT
        LDY     FORNDX
        LDA     ACC
        STA     STACK+($C*MAXFOR)-1,Y
        LDA     ACC+1
        STA     STACK+($D*MAXFOR)-1,Y
        LDA     #$01
        STA     STACK+($6*MAXFOR)-1,Y
        LDA     #$00
LE966:  STA     STACK+($7*MAXFOR)-1,Y
        LDA     PR
        STA     STACK+($8*MAXFOR)-1,Y
        LDA     PR+1
        STA     STACK+($9*MAXFOR)-1,Y
        LDA     PX
        STA     STACK+($A*MAXFOR)-1,Y
        LDA     PX+1
        STA     STACK+($B*MAXFOR)-1,Y
        RTS
        ifelse(`VERSION',1,`
LE97E:  JSR     GET16BIT
        LDY     $FB
        LDA     $CE
        STA     $012F,Y
        LDA     $CF
        JMP     LE966
        .RES 11
        ',`
        ASM_DATA($20,$15)
        ')
LE980:  ASM_DATA($00,$00,$00,$AB,$03,$03,$03,$03)
        ASM_DATA($03,$03,$03,$03,$03,$03,$03,$03)
        ASM_DATA($03,$03,$3F,$3F,$C0,$C0,$3C,$3C)
        ASM_DATA($3C,$3C,$3C,$3C,$3C,$30,$0F,$C0)
        ifelse(`VERSION',1,`
        ASM_DATA($CC)
        ',`
        ASM_DATA($C3)
        ')
        ASM_DATA($FF,$55,$00,$AB,$AB,$03,$03)
        ASM_DATA($FF,$FF,$55,$FF,$FF,$55,$CF,$CF)
        ASM_DATA($CF,$CF,$CF,$FF,$55)
        ifelse(`VERSION',1,`
        ASM_DATA($C3,$C3,$C3)
        ',`
        ASM_DATA($C6,$C6,$C6)
        ')
        ASM_DATA($55,$F0,$F0,$CF)
        ifelse(`VERSION',1,`
        ASM_DATA($56,$56,$56)
        ',`
        ASM_DATA($CF,$55,$01)
        ')
        ASM_DATA($55)
        ASM_DATA($FF,$FF,$55,$03,$03,$03,$03,$03)
        ASM_DATA($03,$03)
        ifelse(`VERSION',1,`
        ASM_DATA($FF,$FF,$FF)
        ',`
        ASM_DATA($03,$03,$03)
        ')
        ASM_DATA($03,$03,$03)
        ASM_DATA($03,$03,$03,$03,$03,$03,$03,$03)
        ASM_DATA($03,$03,$03,$03,$03,$00,$AB,$03)
        ASM_DATA($57,$03,$03,$03,$03,$07,$03,$03)
        ASM_DATA($03,$03,$03,$03,$03,$03,$03,$03)
        ASM_DATA($03,$03,$AA,$FF)
        ifelse(`VERSION',1,`
        ASM_DATA($FF,$FF,$FF,$FF)
        ',`
        ASM_DATA($03,$03,$03,$03)
        ASM_DATA($03,$03,$03,$03,$03,$03,$03,$03)
        ')





define(`LHSPLITLO',`ifelse($1,,,`ASM_DATA(<$1) NL() $0(shift($@))')')
define(`LHSPLITHI',`ifelse($1,,,`ASM_DATA(>$1) NL() $0(shift($@))')')
define(`LHSPLIT',`VERBADRL LHSPLITLO($@) NL() VERBADRH LHSPLITHI($@)')

ifelse(`VERSION',1,`
LHSPLIT(BEGIN_LINE,$FFFF,$FFFF,COLON,LISTNUM,COMMA_LIST,LISTCMD,RUNNUM,RUN,DEL,COMMA_DEL,NEW,CLR,AUTO,COMMA_AUTO,LEE54,LEF80,LEF96,ADDITION,SUBTRACT,MULT,DIVIDE,HE733,HE74A,LEC13,LEC06,LEC0B,HE74A,LEC01,VAND,VOR,MOD,$0000,$FFFF,HE823,COMMA_SUBSTR,GOTO,LET,HEFB6,HEBCB,$FFFF,$FFFF,PAREN_SUBSTR,$FFFF,$FFFF,HEF24,PEEK,RNDCMD,SGN,ABS,$0000,$FFFF,HE823,POSITIVE,NEGATE,NOT,HE823,HE1D7,HE21C,LEN,LEEC2,LEEAE,LEEBA,HE823,$FFFF,$FFFF,HE121,DIMSTR,DIMNUM,PRNTSTR,PRNTNUM,HE820,HEE00,HE7C1,$FFFF,$FFFF,$FFFF,CALLCMD,DIMSTR,DIMNUM,TAB,ENDCMD,HEFB6,INPUT_PROMPT,HEBAA,FOR,HE801,TO,LE97E,NEXT,NEXT,RETURN,GOSUB,$FFFF,LET,GOTO,IF,PRNTSTR,PRNTNUM,PRINT_CR,GETVAL255,LEF0C,COLOR,GETVAL255,COMMA_PLOT,GETVAL255,LEEA6,LEEB0,GETVAL255,LEEBC,LEEC6,$EE57,HE18C,HE801,RIGHT_PAREN,$FFFF,$FFFF,$FFFF,$FFFF,$FFFF)
',`
LHSPLIT(BEGIN_LINE,$FFFF,$FFFF,COLON,LOAD,SAVE,CON,RUNNUM,RUN,DEL,COMMA_DEL,NEW,CLR,AUTO,COMMA_AUTO,MAN,VHIMEM,VLOMEM,ADDITION,SUBTRACT,MULT,DIVIDE,HE733,HE74A,HF25B,HF24E,HF253,HE74A,HF249,VAND,VOR,MOD,EXP,$FFFF,HE823,COMMA_SUBSTR,GOTO,LET,HEFB6,HEBCB,$FFFF,$FFFF,PAREN_SUBSTR,$FFFF,$FFFF,HEF24,PEEK,RNDCMD,SGN,ABS,PDL,$FFFF,HE823,POSITIVE,NEGATE,NOT,HE823,HE1D7,HE21C,LEN,ASC,SCRN,COMMA_SCRN,HE823,$FFFF,$FFFF,HE121,DIMSTR,DIMNUM,PRNTSTR,PRNTNUM,HE820,HEE00,HE7C1,HF3BA,SETTXT,SETGR,CALLCMD,DIMSTR,DIMNUM,TAB,ENDCMD,HEFB6,INPUT_PROMPT,HEBAA,FOR,HE801,TO,STEP,NEXT,NEXT,RETURN,GOSUB,$FFFF,LET,GOTO,IF,PRNTSTR,PRNTNUM,PRINT_CR,POKE,GETVAL255,COLOR,GETVAL255,COMMA_PLOT,GETVAL255,COMMA_HLIN,AT_HLIN,GETVAL255,COMMA_VLIN,AT_VLIN,IVTAB,HE18C,HE801,RIGHT_PAREN,$FFFF,LISTNUM,COMMA_LIST,LISTCMD,POPCMD,NODSP_STR,NODSP_NUM,NOTRACE,DSP_NUM,DSP_STR,TRACE,PRSLOT,INSLOT)
')







ERRMSGTBL:

ERRMSG00 = *-ERRMSGTBL
            HLASCII(`>32767')

ERRMSG01 = *-ERRMSGTBL
            HLASCII(`TOO LONG')

ERRMSG02 = *-ERRMSGTBL
            HLASCII(`SYNTAX')

ERRMSG03 = *-ERRMSGTBL
            HLASCII(`MEM FULL')

ERRMSG04 = *-ERRMSGTBL
            HLASCII(`TOO MANY PARENS')

ERRMSG05 = *-ERRMSGTBL
            HLASCII(`STRING')

ERRMSG06 = *-ERRMSGTBL
            HLASCII(`NO END')

ERRMSG07 = *-ERRMSGTBL
            HLASCII(`BAD BRANCH')

ERRMSG08 = *-ERRMSGTBL
            ifelse(`VERSION',1,`
            HLASCII(`>8 GOSUBS')
            ',`
            HLASCII(`16 GOSUBS')
            ')

ERRMSG09 = *-ERRMSGTBL
            HLASCII(`BAD RETURN')

ERRMSG10 = *-ERRMSGTBL
            ifelse(`VERSION',1,`
            HLASCII(`>8 FORS')
            ',`
            HLASCII(`16 FORS')
            ')

ERRMSG11 = *-ERRMSGTBL
            HLASCII(`BAD NEXT')

ERRMSG12 = *-ERRMSGTBL
            HLASCII(`STOPPED AT ')

ERRMSG13 = *-ERRMSGTBL
            HLASCII(`*** ')

ERRMSG14 = *-ERRMSGTBL
            HIASCII(` ERR')
            ASM_DATA($0D)

ERRMSG15 = *-ERRMSGTBL
            HLASCII(`>255')

ERRMSG16 = *-ERRMSGTBL
            HLASCII(`RANGE')

ERRMSG17 = *-ERRMSGTBL
            HLASCII(`DIM')

ERRMSG18 = *-ERRMSGTBL
            HLASCII(`STR OVFL')

ERRMSG19 = *-ERRMSGTBL
            HIASCII(`\')
            ASM_DATA($0D)

ERRMSG20 = *-ERRMSGTBL
            HIASCII(`RETYPE LINE')
            ASM_DATA($8D)

ERRMSG21 = *-ERRMSGTBL
            LOASCII(`?')

LEB9A:  LSR     RUNFLAG
        BCC     LEBA1
        JMP     LE8C3
LEBA1:  LDX     ACC+1
        TXS
        LDX     ACC
        LDY     #CR
        BNE     LEBAC
HEBAA:  LDY     #ERRMSG21
LEBAC:  JSR     ERRMSG
        STX     ACC
        TSX
        STX     ACC+1

        ifelse(`VERSION',1,`
        LDY     #$FE
        STY     $D9
        INY
        STY     $C8
        JSR     LE299
        STY     $F1
        ',`
        JSR     LF366
        STY     TOKNDX
        LDA     #$FF
        STA     TXTNDX
        ASL
        STA     RUNFLAG
        ')

        LDX     #$20
        ifelse(`VERSION',1,`
        LDA     #$30
        ',`
        LDA     #$15
        ')
        JSR     LE491
        INC     RUNFLAG
        LDX     ACC
HEBCB:  LDY     TXTNDX
        ASL
LEBCE:  STA     ACC
        INY
        LDA     IN,Y
        ifelse(`VERSION',1,`
        CMP     #$74
        ',`
        CMP     #$80
        ')
        BEQ     HEBAA
        EOR     #$B0
        CMP     #$0A
        BCS     LEBCE
        INY
        INY
        STY     TXTNDX
        LDA     IN,Y
        PHA
        LDA     STACK+255,Y
        LDY     #$00
        JSR     LE708
        PLA
        STA     NOUNSTKC,X
        LDA     ACC
        ifelse(`VERSION',1,`
        CMP     #$C7
        ',`
        CMP     #$33
        ')
        BNE     LEBFA
        JSR     NEGATE
LEBFA:  JMP     HE801



        ASM_RES(3,$FF)



define(`BASCMD',`
ifelse($2,,,ASM_DATA($2))
ifelse($3,,,ASM_DATA($3))
ifelse($4,,,ASM_DATA($4))
ifelse($5,,,ASM_DATA($5))
ifelse($2,,
	`ASM_DATA(HICHAR(SAFESUB(`$1',len(`$1')-1))+`$'20)',
	`ASM_DATA(HICHAR(SAFESUB(`$1',len(`$1')-1))-`$'20)')
STR_FORCHAR(__,
  STR_REVERSE(SAFESUB(`$1',0,len(`$1')-1)),
  `ASM_DATA(HICHAR(__)-`$'20) NL()')
')


        ASM_DATA($50)



        ifelse(`VERSION',1,`


LEC01:  JSR     LEC13
        BNE     LEC1B
LEC06:  JSR     LEC0B
        BNE     LEC1B
LEC0B:  JSR     SUBTRACT
        JSR     NEGATE
        BVC     LEC16
LEC13:  JSR     SUBTRACT
LEC16:  JSR     SGN
        LSR     $50,X
LEC1B:  JMP     NOT

        ASM_RES(2,$FF)

SYNTABLNDX:
        ASM_DATA($C1,$FF,$7F,$D1,$CC,$C7,$CF,$CE)
        ASM_DATA($C5,$9A,$98,$8B,$96,$95,$93,$BF)
        ASM_DATA($B2,$32,$2D,$2B,$BC,$B0,$AC,$BE)
        ASM_DATA($35,$8E,$61,$FF,$FF,$FF,$DD,$FB)

VAND:   JSR     LEFC9
        ORA     RND+1,X
        BPL     LEC4C
VOR:    JSR     LEFC9
        AND     RND+1,X
LEC4C:  STA     NOUNSTKL,X
        BPL     LEC1B
        JMP     LEFC9

SYNTABL:
        ASM_DATA($40,$60,$8D,$60,$8B)
        ASM_DATA($00)
        ASM_DATA($7E,$8C,$33,$00,$00,$60,$03,$BF,$12)



        ',`



        ASM_DATA($20,$4F,$C0)

SYNTABL:
        BASCMD(`AT')
        BASCMD(`MOD')
        BASCMD(`OR')
        BASCMD(`AND')
        BASCMD(`STEP')
        BASCMD(`TO')
        BASCMD(`THEN')

        ASM_DATA($5C,$80,$00)
        BASCMD(`-',$40,$60)
        BASCMD(`+',$60)
        ASM_DATA($7F,$1D,$20)
ASM_DATA($7E,$8C)
;        BASCMD(`,',$7E)
        ASM_DATA($33,$00,$00,$60,$03,$BF,$12)

        BASCMD(`IN#',$47)
        BASCMD(`PR#',$67)
        BASCMD(`TRACE')
        BASCMD(`DSP',$79)
        BASCMD(`DSP',$69)
        BASCMD(`NOTRACE')
        BASCMD(`NODSP',$79)
        BASCMD(`NODSP',$69)
        BASCMD(`POP')
        BASCMD(`LIST')
ASM_DATA($60,$8C)
;        BASCMD(`,',$60)
        BASCMD(`LIST',$20)



        ')



        ASM_DATA($00,$40,$89)
        ASM_DATA($C9)
;        BASCMD(`)')
        BASCMD(`=',$47)
        BASCMD(`=',$17,$68)
        ASM_DATA($0A)

        ifelse(`VERSION',1,`
        ASM_DATA($00)

        BASCMD(`-',$40,$60)
        BASCMD(`+',$60)
        ASM_DATA($00)

ASM_DATA($7E,$8C)
;        BASCMD(`,',$7E)

        ASM_DATA($3C,$00,$00,$60,$03,$BF,$1B,$4B)

        ',`

        BASCMD(`VTAB',$58,$7B,$67)
        BASCMD(`AT',$67)
ASM_DATA($07,$8C)
;        BASCMD(`,',$07)
        BASCMD(`VLIN',$07)

        ')

        BASCMD(`AT',$67)
ASM_DATA($07,$8C)
;        BASCMD(`,',$07)
        BASCMD(`HLIN',$07)
ASM_DATA($67,$8C)
;        BASCMD(`,',$67)
        BASCMD(`PLOT',$07)
        BASCMD(`COLOR=',$67)
ASM_DATA($67,$8C)
;        BASCMD(`,',$67)
        BASCMD(`POKE',$07)
        BASCMD(`PRINT')
        BASCMD(`PRINT',$7F,$0E,$27)
        BASCMD(`PRINT',$7F,$0E,$28)
        BASCMD(`IF',$64,$07)
        BASCMD(`GOTO',$67)
        BASCMD(`LET',$78)

        ifelse(`VERSION',1,`
        ASM_DATA($78)
        ',`
        ASM_DATA($6B)
        ')

        BASCMD(`REM',$7F,$02)
        BASCMD(`GOSUB',$67)
        BASCMD(`RETURN')
ASM_DATA($7E,$8C)
;        BASCMD(`,',$7E)
        BASCMD(`NEXT',$39)
        BASCMD(`STEP',$67)
        BASCMD(`TO',$27)
        BASCMD(`=',$07)
        BASCMD(`FOR',$19)
        BASCMD(`INPUT',$7F,$05,$37)
        BASCMD(`INPUT',$7F,$05,$28)
        BASCMD(`INPUT',$7F,$05,$2A)
        BASCMD(`END')

SYNTABL2:
        ASM_DATA(0)
        ifelse(`VERSION',1,`
        ASM_RES(2,$FF)
        ',`')

        BASCMD(`TAB',$47)
        BASCMD(`DIM',$7F,$0D,$30)
        BASCMD(`DIM',$7F,$0D,$23)
        BASCMD(`CALL',$67)

        ifelse(`VERSION',1,`
        ASM_DATA($00,$40,$80,$C0,$C1,$80,$00)
ASM_DATA($47,$8C)
;        BASCMD(`,',$47)
        ',`
        BASCMD(`GR')
        BASCMD(`TEXT')
        ASM_DATA(0)

        ASM_DATA($4D)
ASM_DATA($CC,$67,$8C)
;        BASCMD(`,')
;        BASCMD(`,',$67)
        ')

ASM_DATA($68,$8C)
;        BASCMD(`,',$68)
        ASM_DATA($db,$67,$9b,$68,$9b)
;        BASCMD(`;')
;        BASCMD(`;',$67)
;        BASCMD(`;',$68)

ASM_DATA($50,$8C)
;        BASCMD(`,',$50)
ASM_DATA($63,$8C)
;        BASCMD(`,',$63)
        ASM_DATA($7f,$01,$51,$07,$88)
;        BASCMD(`(',$7F,$01,$51,$07)
        BASCMD(` $',$29)
        BASCMD(`$')

        ifelse(`VERSION',1,`
        ASM_DATA($80)
        ',`
        ASM_DATA($19)
        ')
        ASM_DATA($57,$71,$07,$88)
;        BASCMD(`(',$57,$71,$07)
        ASM_DATA($14)
        ifelse(`VERSION',1,`
        BASCMD(`LOMEM')
        BASCMD(`HIMEM')
        BASCMD(`COLOR')
        ',`
ASM_DATA($71,$07,$8C)
;        BASCMD(`,',$71,$07)
ASM_DATA($07,$88,$ae,$b2,$a3,$b3)
;        BASCMD(`SCRN(',$07)
ASM_DATA($71,$08,$88,$a3,$b3,$a1)
;        BASCMD(`ASC(',$71,$08)
        ')
ASM_DATA($71,$08,$88,$ae,$a5,$ac)
;        BASCMD(`LEN(',$71,$08)
        BASCMD(`#',$68)
        BASCMD(`=',$08,$68)
        ASM_DATA($08,$71,$07,$88)
;        BASCMD(`(',$08,$71,$07)

        ASM_DATA($60)

        ifelse(`VERSION',1,`
        BASCMD(`NOT',$76)
        BASCMD(`-',$76)
        BASCMD(`+',$76)
        ',`
        BASCMD(`NOT',$75)
        BASCMD(`-',$75)
        BASCMD(`+',$75)
        ')

        ASM_DATA($51,$07,$88)
;        BASCMD(`(',$51,$07)
        BASCMD(`RNDX',$19)

        ifelse(`VERSION',1,`
        BASCMD(`USR')
        ',`
        BASCMD(`PDL')
        ')

        BASCMD(`ABS')
        BASCMD(`SGN')
        BASCMD(`RND')
        BASCMD(`PEEK')
        ASM_DATA($51,$07,$88)
;        BASCMD(`(',$51,$07)
        BASCMD(`!',$39)
        BASCMD(`!')
        ASM_DATA($4F,$7F,$0F,$2F,0)

        ASM_DATA($51)
        ASM_DATA($06,$88)
;        BASCMD(`(',$06)

        ASM_DATA($29)

        ASM_DATA($C2)     ;TODO
        ASM_DATA($0C,$82) ;TODO
ASM_DATA($57,$8C)
;        BASCMD(`,',$57)
ASM_DATA($6A,$8C)
;        BASCMD(`,',$6A)
        BASCMD(`THEN',$42)
        BASCMD(`THEN',$60)
ASM_DATA($4F,$7E,$1E,$35,$8C)
;        BASCMD(`,',$4F,$7E,$1E,$35)
        ASM_DATA($27,$51,$07,$88)
;        BASCMD(`(',$27,$51,$07)
        BASCMD(`+',$09)
ASM_DATA($FE)
;        BASCMD(`^')
        BASCMD(`MOD')
        BASCMD(`OR')
        BASCMD(`AND')
        BASCMD(`<')
        BASCMD(`<>')
        BASCMD(`<=')
        BASCMD(`>')
        BASCMD(`>=')
        BASCMD(`#')
        BASCMD(`=')
        BASCMD(`/')
        BASCMD(`*')
        BASCMD(`-')
        BASCMD(`+')
        ASM_DATA(0)

        ifelse(`VERSION',1,`
        BASCMD(`LOMEM=',$47)
        BASCMD(`HIMEM=',$76)
        BASCMD(`OFF')
        ',`
        BASCMD(`LOMEM:',$47)
        BASCMD(`HIMEM:',$67)
        BASCMD(`MAN')
        ')

ASM_DATA($60,$8C)
;        BASCMD(`,',$60)
        BASCMD(`AUTO',$20)
        BASCMD(`CLR')

        ifelse(`VERSION',1,`
        BASCMD(`SCR')
        ',`
        BASCMD(`NEW')
        ')

ASM_DATA($60,$8C)
;        BASCMD(`,',$60)
        BASCMD(`DEL',$20)
        BASCMD(`RUN')
        BASCMD(`RUN',$60)

        ifelse(`VERSION',1,`
        BASCMD(`LIST')
        BASCMD(`LIST',$60,$8C,$20)
        ',`
        BASCMD(`CON')
        BASCMD(`SAVE')
        BASCMD(`LOAD')
        ')

        BASCMD(`:',$7A,$7E)

        ASM_DATA($22,$20,$00,$60,$03,$BF,$60)
        ASM_DATA($03,$BF,$1F)

HEE00:  JSR     LE7B1
PRNTSTR:INX
        INX
        LDA     RND+1,X
        STA     AUX
        LDA     $77,X
        STA     AUX+1
        LDY     RND,X
LEE0F:  TYA
        CMP     $76,X
        BCS     LEE1D
        LDA     (AUX),Y
        JSR     COUT
        INY
        JMP     LEE0F
LEE1D:  LDA     #$FF
        STA     CRFLAG
        RTS
LEN:    INX
        LDA     #$00
        STA     NOUNSTKH,X
        STA     NOUNSTKC,X
        LDA     $77,X
        SEC
        SBC     RND+1,X
        STA     NOUNSTKL,X
        JMP     HE823
        ASM_DATA($FF)
GETBYTE:JSR     GET16BIT
        LDA     ACC+1
        BNE     LEE63
        LDA     ACC
        RTS
COMMA_PLOT:
        JSR     GETBYTE
        LDY     TXTNDX
        CMP     #$30
        BCS     LEE68
        CPY     #$28
        BCS     LEE68
        ifelse(`VERSION',1,`
        RTS
        NOP
        NOP
        ',`
        JMP     PLOT
        ')
COLOR:  JSR     GETBYTE
        ifelse(`VERSION',1,`
        RTS
        NOP
LEE53:  TXA
LEE54:  LDX     #$01
LEE56:  LDY     $CE,X
        STY     $4C,X
        LDY     $48,X
        STY     $CA,X
        DEX
        BEQ     LEE56
        TAX
        RTS
        ',`
        JMP     SETCOL
MAN:    LSR     AUTOFLAG
        RTS
IVTAB:  JSR     LF3B3
        CMP     #$18
        BCS     LEE68
        STA     CV
        JMP     VTAB
        ')
LEE63:  LDY     #$77
LEE65:  JMP     LE3E0
LEE68:  LDY     #$7B
        BNE     LEE65
LEE6C:  JSR     LE254
        LDA     AUX
        BNE     LEE7A
        LDA     AUX+1
        BNE     LEE7A
        JMP     LE77E
LEE7A:  ASL     ACC
        ROL     ACC+1
        ROL     P3
        ROL     P3+1
        LDA     P3
        CMP     AUX
        LDA     P3+1
        SBC     AUX+1
        BCC     LEE96
        STA     P3+1
        LDA     P3
        SBC     AUX
        STA     P3
        INC     ACC
LEE96:  DEY
        BNE     LEE7A
        RTS
        ASM_RES(6,$FF)
CALLCMD: JSR     GET16BIT
        JMP     (ACC)

        ifelse(`VERSION',1,`

LEEA6:  LDA     HIMEM
        BNE     LEEAC
        DEC     HIMEM+1
LEEAC:  DEC     HIMEM
LEEAE:  LDA     $48
LEEB0:  BNE     LEEB4
        DEC     $49
LEEB4:  DEC     $48
LEEB6:  LDY     #0
        LDA     (HIMEM),Y
LEEBA:  STA     ($48),Y
LEEBC:  LDA     PP
        CMP     HIMEM
        LDA     PP+1
LEEC2:  SBC     HIMEM+1
        BCC     LEEA6
LEEC6:  JMP     LEE53

        ',`

COMMA_HLIN:
        JSR     GETBYTE
        CMP     TXTNDX
        BCC     LEE68
        STA     H2
        RTS
AT_HLIN:JSR     GETBYTE
        CMP     #$30
        BCS     LEE68
        LDY     TXTNDX
        JMP     HLINE
COMMA_VLIN:
        JSR     GETBYTE
        CMP     TXTNDX
        BCC     LEE68
        STA     V2
        RTS
AT_VLIN:JSR     GETBYTE

        ')

        CMP     #$28
LEECB:  BCS     LEE68
        TAY
        LDA     TXTNDX
        ifelse(`VERSION',1,`
        RTS
        NOP
        NOP
        ',`
        JMP     VLINE
        ')
LEED3:  TYA
        TAX
        LDY     #ERRMSG13
        JSR     ERRMSG
        TXA
        TAY
        JSR     ERRMSG
        LDY     #$72
        ifelse(`VERSION',1,`
        JMP     ERRMSG
        ',`
        JMP     LF161
        ')
LEEE4:
        ifelse(`VERSION',1,`
        JSR     GET16BIT
        ',`
        JSR     LF23F
        ')

LEEE7:  ASL     ACC
        ROL     ACC+1
        BMI     LEEE7
        BCS     LEECB
        BNE     LEEF5
        CMP     ACC
        BCS     LEECB
LEEF5:  RTS
PEEK:   JSR     GET16BIT
        LDA     (ACC),Y
        STY     SYNSTKL+31,X
        JMP     LE708

GETVAL255:
        JSR     GETBYTE
        LDA     ACC

        ifelse(`VERSION',1,`
        PHA
        ',`
        STA     TXTNDX
        RTS
        ')

POKE:   JSR     GET16BIT
        ifelse(`VERSION',1,`
        PLA
        ',`
        LDA     TXTNDX
        ')
        STA     (ACC),Y
LEF0C:  RTS

        ifelse(`VERSION',1,`
        ASM_RES(3,$FF)
        ',`')

DIVIDE: JSR     LEE6C
        LDA     ACC
        STA     P3
        LDA     ACC+1
        STA     P3+1
        JMP     LE244
DIMNUM: JSR     LEEE4
        JMP     LE134
HEF24:  JSR     LEEE4
        LDY     NOUNSTKH,X
        LDA     NOUNSTKL,X
        ADC     #$FE
        BCS     LEF30
        DEY
LEF30:  STA     AUX
        STY     AUX+1
        CLC
        ADC     ACC
        STA     NOUNSTKL,X
        TYA
        ADC     ACC+1
        STA     NOUNSTKH,X
        LDY     #$00
        LDA     NOUNSTKL,X
        CMP     (AUX),Y
        INY
        LDA     NOUNSTKH,X
        SBC     (AUX),Y
        BCS     LEECB
        JMP     HE823
RNDCMD: JSR     GET16BIT
        LDA     RND
        JSR     LE708
        LDA     RND+1
        BNE     LEF5E
        CMP     RND
        ADC     #$00
LEF5E:  AND     #$7F
        STA     RND+1
        STA     NOUNSTKC,X
        LDY     #$11
LEF66:  LDA     RND+1
        ASL
        CLC
        ADC     #$40
        ASL
        ROL     RND
        ROL     RND+1
        DEY
        BNE     LEF66
        LDA     ACC
        JSR     LE708
        LDA     ACC+1
        STA     NOUNSTKC,X
        JMP     MOD
LEF80:  JSR     GET16BIT
        LDY     ACC
        ifelse(`VERSION',1,`
        CPY     HIMEM
        ',`
        CPY     LOMEM
        ')
        LDA     ACC+1
        ifelse(`VERSION',1,`
        SBC     HIMEM+1
        ',`
        SBC     LOMEM+1
        ')
        BCC     LEFAB
        ifelse(`VERSION',1,`
        STY     $48
        ',`
        STY     HIMEM
        ')
        LDA     ACC+1
        ifelse(`VERSION',1,`
        STA     $49
        ',`
        STA     HIMEM+1
        ')

LEF93:
        ifelse(`VERSION',1,`
        JMP     LEEB6
        ',`
        JMP     NEW
        ')
LEF96:  JSR     GET16BIT
        LDY     ACC
        ifelse(`VERSION',1,`
        CPY     $CA
        ',`
        CPY     HIMEM
        ')
        LDA     ACC+1
        ifelse(`VERSION',1,`
        SBC     $CB
        ',`
        SBC     HIMEM+1
        ')
        BCS     LEFAB
        STY     LOMEM
        LDA     ACC+1
        STA     LOMEM+1

        ifelse(`VERSION',1,`
        JMP     CLR
        ',`
        BCC     LEF93
        ')

LEFAB:  JMP     LEECB

        ifelse(`VERSION',1,`
        NOP
        NOP
        NOP
        NOP
        JSR     LEFC9
        ',`
        ASM_RES(8,$FF)
        ')
HEFB6:  JSR     INPUTSTR
        JMP     LEFBF
INPUT_PROMPT:
        JSR     PRNTSTR
LEFBF:  LDA     #$FF
        STA     TXTNDX
        ifelse(`VERSION',1,`
        LDA     #$74
        ',`
        LDA     #$80
        ')
        STA     IN
        RTS
LEFC9:  JSR     NOT
        INX
LEFCD:  JSR     NOT
        LDA     NOUNSTKL,X
        RTS
OLDCOLD:LDA     #$00
        STA     LOMEM
        STA     HIMEM
        LDA     #$08
        STA     LOMEM+1
        LDA     #$10
        STA     HIMEM+1
        JMP     NEW
LEFE4:  CMP     NOUNSTKH,X
        BNE     LEFE9
        CLC
LEFE9:  JMP     LE102
RUN:    JSR     CLR
        JMP     LE836
RUNNUM: JSR     CLR
        JMP     GOTO
LEFF8:  CPX     #$80
        BNE     LEFFD
        DEY
LEFFD:  JMP     LE00C


        ifelse(eval(VERSION` > 1'),1,





`
COLD:   LDY     #$00
        STY     NOUNSTKC
        STY     LOMEM
        STY     HIMEM
        LDA     #$08
        STA     LOMEM+1
        STA     HIMEM+1
LF00E:  INC     HIMEM+1
        LDA     (HIMEM),Y
        EOR     #$FF
        STA     (HIMEM),Y
        CMP     (HIMEM),Y
        BNE     LF022
        EOR     #$FF
        STA     (HIMEM),Y
        CMP     (HIMEM),Y
        BEQ     LF00E
LF022:  JMP     NEW
LF025:  JMP     LF179
        JSR     LF032
        JMP     LE8BE
LF02E:  LDX     PX
        LDA     PX+1
LF032:  LDY     KBD
        CPY     #CTRL_C
        BNE     LF025
        BIT     KBDSTRB
        STX     NOUNSTKL
        STA     NOUNSTKL+1
        LDA     PR
        STA     NOUNSTKH
        LDA     PR+1
        STA     NOUNSTKH+1
        JMP     LE8C3
        ASM_RES(2,$FF)
VHIMEM: JSR     GET16BIT
        STX     XSAVE
        LDX     #$FE
        SEC
LF055:  LDA     SRCH,X
        STA     P3,X
        LDA     RND,X
        SBC     SRCH,X
        STA     PR,X
        INX
        BNE     LF055
        BCC     LF0AF
        DEX
LF065:  LDA     PP+1,X
        STA     P3+1,X
        SBC     AUX+1,X
        STA     P2+1,X
        INX
        BEQ     LF065
        BCC     LF07C
        LDA     PV
        CMP     P2
        LDA     PV+1
        SBC     P2+1
        BCC     LF08F
LF07C:  JMP     LE36B
LF07F:  LDA     (P3),Y
        STA     (P2),Y
        INC     P2
        BNE     LF089
        INC     P2+1
LF089:  INC     P3
        BNE     LF08F
        INC     P3+1
LF08F:  LDA     P3
        CMP     HIMEM
        LDA     P3+1
        SBC     HIMEM+1
        BCC     LF07F
LF099:  LDX     #$FE
LF09B:  LDA     P3,X
        STA     RND,X
        LDA     PV,X
        SBC     PR,X
        STA     PV,X
        INX
        BNE     LF09B
        LDX     XSAVE
        RTS
LF0AB:  LDA     (HIMEM),Y
        STA     (ACC),Y
LF0AF:  LDA     ACC
        BNE     LF0B5
        DEC     ACC+1
LF0B5:  DEC     ACC
        LDA     HIMEM
        BNE     LF0BD
        DEC     HIMEM+1
LF0BD:  DEC     HIMEM
        CMP     PP
        LDA     HIMEM+1
        SBC     PP+1
        BCC     LF0AB
        BCS     LF099
VLOMEM: JSR     GET16BIT
        LDY     ACC
ifdef(`BUGFIX',`
        CPY     PP
        ',`
        CPY     #PP
        ')
        LDA     ACC+1
        SBC     PP+1
LF0D4:  BCS     LF07C
        STY     LOMEM
        LDA     ACC+1
        STA     LOMEM+1
        JMP     CLR
LOAD:   STX     XSAVE
        JSR     LF11E
        JSR     READ
        LDX     #$FF
        SEC
LF0EA:  LDA     HIMEM+1,X
        SBC     ACC+1,X
        STA     AUX+1,X
        INX
        BEQ     LF0EA
        BCC     LF07C
        LDA     PV
        CMP     AUX
        LDA     PV+1
        SBC     AUX+1
        BCS     LF0D4
        LDA     ACC
        BNE     LF107
        LDA     ACC+1
        BEQ     LF118
LF107:  LDA     AUX
        STA     PP
        LDA     AUX+1
        STA     PP+1
        JSR     LF12C
        JSR     READ
LF115:  LDX     XSAVE
        RTS
LF118:  JSR     BELL
        JMP     LF115
LF11E:  LDY     #$CE
        STY     A1
        INY
        STY     A2
        LDY     #$00
        STY     $3D
        STY     $3F
        RTS
LF12C:  LDA     PP,X
        STA     A1,X
        LDY     HIMEM,X
        STY     A2,X
        DEX
        BPL     LF12C
        LDA     A2
        BNE     LF13D
        DEC     $3F
LF13D:  DEC     A2
        RTS
        STX     XSAVE
SAVE:   SEC
        LDX     #$FF
LF145:  LDA     HIMEM+1,X
        SBC     PP+1,X
        STA     ACC+1,X
        INX
        BEQ     LF145
        JSR     LF11E
        JSR     WRITE
        LDX     #$01
        JSR     LF12C
        LDA     #$1A
        JSR     WRITE0
        LDX     XSAVE
        RTS
LF161:  JSR     ERRMSG
        JMP     BELL
POPCMD: LDA     GOSUBNDX
        BNE     LF16E
        JMP     RETURN
LF16E:  DEC     GOSUBNDX
        RTS
TRACE:  LDA     #$FF
        STA     NOUNSTKC
        RTS
NOTRACE:LSR     NOUNSTKC
        RTS
LF179:  BIT     NOUNSTKC
        BPL     LF196
LF17D:  LDA     #$A3
        JSR     COUT
        LDY     #$01
        LDA     (PR),Y
        TAX
        INY
        LDA     (PR),Y
        JSR     LE51B
        LDA     #HICHAR(` ')
        JMP     COUT
        LDA     PR
        LDY     PR+1
LF196:  RTS

SYNTABLNDX:
        ASM_DATA($C1,$00,$7F,$D1,$CC,$C7,$CF,$CE)
        ASM_DATA($C5,$9A,$98,$8D,$96,$95,$93,$BF)
        ASM_DATA($B2,$32,$12,$0F,$BC,$B0,$AC,$BE)
        ASM_DATA($35,$0C,$61,$30,$10,$0B,$DD,$FB)

LF1B7:  LDY     #$00
        JSR     LE7C7
        LDA     #HICHAR(` ')
        JMP     COUT
        ASM_RES(8)
LF1C9:  LDY     LOMEM
        LDA     LOMEM+1
LF1CD:  PHA
        CPY     AUX
        SBC     AUX+1
        BCS     LF1F0
        PLA
        STY     SRCH
        STA     TOKNDXSTK
        LDY     #$FF
LF1DB:  INY
        LDA     (SRCH),Y
        BMI     LF1DB
        CMP     #$40
        BEQ     LF1DB
        INY
        INY
        LDA     (SRCH),Y
        PHA
        DEY
        LDA     (SRCH),Y
        TAY
        PLA
        BNE     LF1CD
LF1F0:  PLA
        LDY     #$00
LF1F3:  LDA     (SRCH),Y
        BMI     LF1FC
        LSR
        BEQ     LF202
        LDA     #$A4
LF1FC:  JSR     COUT
        INY
        BNE     LF1F3
LF202:  LDA     #$BD
        JMP     COUT
LF207:  STA     (AUX),Y
        INX
        LDA     SYNSTKL+31,X
        BEQ     LF23E
        JMP     LF3D5
        ASM_DATA($A0)
LF212:  BMI     LF21B
        LDA     PR
        LDY     PR+1
        JSR     LF17D
LF21B:  JSR     LF1C9
        LDX     XSAVE
        JMP     LF1B7
LF223:  INX
        INX
        LDA     SYNSTKL+31,X
        BEQ     LF248
        JMP     LF3E0
LF22C:  BMI     LF235
        LDA     PR
        LDY     PR+1
        JSR     LF17D
LF235:  JSR     LF1C9
        LDX     XSAVE
        JMP     LF409
        INX
LF23E:  RTS
LF23F:  JSR     GET16BIT
        INC     ACC
        BNE     LF248
        INC     ACC+1
LF248:  RTS
HF249:  JSR     HF25B
        BNE     LF263
HF24E:  JSR     HF253
        BNE     LF263
HF253:  JSR     SUBTRACT
        JSR     NEGATE
        BVC     LF25E
HF25B:  JSR     SUBTRACT
LF25E:  JSR     SGN
        LSR     NOUNSTKL,X
LF263:  JMP     NOT
VAND:   JSR     LEFC9
        ORA     RND+1,X
        BPL     LF272
VOR:    JSR     LEFC9
        AND     RND+1,X
LF272:  STA     NOUNSTKL,X
        BPL     LF263
        JMP     LEFC9
STEP:   JSR     GET16BIT
        LDY     FORNDX
        LDA     ACC
        STA     STACK+95,Y
        LDA     ACC+1
        JMP     LE966
LF288:  STA     STACK+80,Y
LF28B:  DEY
        BMI     LF2DF
        LDA     STACK+64,Y
        CMP     NOUNSTKL,X
        BNE     LF28B
        LDA     STACK+80,Y
        CMP     NOUNSTKH,X
        BNE     LF28B
        DEC     FORNDX
LF29E:  LDA     STACK+$41,Y
        STA     STACK+$40,Y
        LDA     STACK+$51,Y
        STA     STACK+$50,Y
        LDA     STACK+$C1,Y
        STA     STACK+$C0,Y
        LDA     STACK+$D1,Y
        STA     STACK+$D0,Y
        LDA     STACK+$61,Y
        STA     STACK+$60,Y
        LDA     STACK+$71,Y
        STA     STACK+$70,Y
        LDA     STACK+$81,Y
        STA     STACK+$80,Y
        LDA     STACK+$91,Y
        STA     STACK+$90,Y
        LDA     STACK+$A1,Y
        STA     STACK+$A0,Y
; DOS 3.3 SYSTEM MASTER: INTBASIC
ifdef(`BUGFIX',`
        LDA     STACK+$B1,Y
        STA     STACK+$B0,Y
        ',`
        LDA     STACK+$A1,Y
        STA     STACK+$A0,Y
        ')
        INY
        CPY     FORNDX
        BCC     LF29E
LF2DF:  RTS
NODSP_STR:
        INX
NODSP_NUM:
        LDA     #$00
LF2E3:  PHA
        LDA     NOUNSTKL,X
        SEC
        SBC     #$03
        STA     ACC
        LDA     NOUNSTKH,X
        SBC     #$00
        STA     ACC+1
        PLA
        LDY     #$00
        STA     (ACC),Y
        INX
        RTS
LF2F8:  CMP     #$85
        BCS     LF2FF
        JMP     LE4C0
LF2FF:  LDY     #$02
        JMP     LE448
DSP_NUM:INX
DSP_STR:LDA     #$01
        BNE     LF2E3
        INX
CON:    LDA     NOUNSTKH
        STA     PR
        LDA     NOUNSTKH+1
        STA     PR+1
        LDA     NOUNSTKL
        LDY     NOUNSTKL+1
        JMP     LE875
        LDA     #$01
        BNE     LF2E3
ASC:    LDA     NOUNSTKL,X
        CMP     NOUNSTKH,X
        BCC     LF326
        JMP     LEE68
LF326:  TAY
        LDA     NOUNSTKL+1,X
        STA     ACC
        LDA     NOUNSTKH+1,X
        STA     ACC+1
        LDA     (ACC),Y
        LDY     #$00
        INX
        INX
        JSR     LE708
        JMP     LF404
PDL:    JSR     GETBYTE
        STX     XSAVE
        AND     #$03
        TAX
        JSR     PREAD
        LDX     XSAVE
        TYA
        LDY     #$00
        JSR     LE708
        STY     NOUNSTKC,X
        RTS
LF351:  JSR     NXTCHAR
LF354:  TXA
        PHA
LF356:  LDA     IN,X
        CMP     #CTRL_C
        BNE     LF360
        JMP     BASIC2
LF360:  DEX
        BPL     LF356
        PLA
        TAX
        RTS
LF366:  JSR     LE280
        TYA
        TAX
        JSR     LF354
        TXA
        TAY
        RTS
EXP:    JSR     GET16BIT
        LDA     ACC+1
        BPL     LF380
        TYA
        DEX
        JSR     LE708
        STY     NOUNSTKC,X
LF37F:  RTS
LF380:  STA     TOKNDXSTK
        LDA     ACC
        STA     SRCH
        JSR     GET16BIT
        LDA     ACC
        STA     SRCH2
        LDA     ACC+1
        STA     SRCH2+1
        LDA     #$01
        JSR     LE708
        STY     NOUNSTKC,X
LF398:  LDA     SRCH
        BNE     LF3A0
        DEC     TOKNDXSTK
        BMI     LF37F
LF3A0:  DEC     SRCH
        LDA     SRCH2
        LDY     #$00
        JSR     LE708
        LDA     SRCH2+1
        STA     NOUNSTKC,X
        JSR     MULT
        JMP     LF398
LF3B3:  JSR     GETBYTE
        CLC
        ADC     #$FF
LF3B9:  RTS
HF3BA:  JSR     LE7B1
        LSR     CRFLAG
        RTS
        STX     RUNFLAG
        TXS
        JSR     LF02E
        JMP     LE883
PRSLOT: JSR     GETBYTE
        STX     XSAVE
        JSR     OUTPORT
        LDX     XSAVE
        RTS
        ASM_DATA($FE)
LF3D5:  BIT     RUNFLAG
        BPL     LF3B9
        STX     XSAVE
        BIT     NOUNSTKC
        JMP     LF212
LF3E0:  BIT     RUNFLAG
        BPL     LF3B9
        STX     XSAVE
        BIT     NOUNSTKC
        JMP     LF22C
LF3EB:  LDY     #$00
        JMP     GETVERB
LF3F0:  TAY
        JSR     CROUT
LF3F4:  TYA
        SEC
        SBC     WNDWDTH
        BCS     LF3F0
        STY     CH
        RTS
        BRK
        BRK
        BRK
        ASM_RES(4,$FF)
LF404:  STY     NOUNSTKC,X
        JMP     HE823
LF409:  LDY     #$00
        BEQ     LF411
LF40D:  JSR     COUT
        INY
LF411:  LDA     (AUX),Y
        BMI     LF40D
        LDA     #$FF
        STA     CRFLAG
        RTS
INSLOT: JSR     GETBYTE
        STX     XSAVE
        JSR     INPORT
        LDX     XSAVE
        RTS

',`')
