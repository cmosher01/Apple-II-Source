include(`asm.m4h')
include(`symbols.m4h')



ifelse(eval(VERSION `< 2'),1,`


;-----------------------------------------------------------------------
;
; MATH ROUTINES
;
; 16-bit multiplication and division functions (not used anywhere).
;
;-----------------------------------------------------------------------





MULPM    JSR   MD1        ;ABS VAL OF AC AUX
MUL      LDY   #$10       ;INDEX FOR 16 BITS
MUL2     LDA   ACL        ;ACX * AUX + XTND
         LSR              ; TO AC, XTND
         BCC   MUL4       ;IF NO CARRY,
         CLC              ; NO PARTIAL PROD.
         LDX   #$FE
MUL3     LDA   XTNDL+2,X  ;ADD MPLCND (AUX)
         ADC   AUXL+2,X   ; TO PARTIAL PROD
         STA   XTNDL+2,X  ; (XTND)
         INX
         BNE   MUL3
MUL4     LDX   #$03
MUL5     ROR   ACL,X
         DEX
         BPL   MUL5
         DEY
         BNE   MUL2
         RTS



DIVPM    JSR   MD1        ;ABS VAL OF AC, AUX.
DIV      LDY   #$10       ;INDEX FOR 16 BITS
DIV2     ASL   ACL
         ROL   ACH
         ROL   XTNDL      ;XTND/AUX
         ROL   XTNDH      ;  TO AC.
         SEC
         LDA   XTNDL
         SBC   AUXL       ;MOD TO XTND.
         TAX
         LDA   XTNDH
         SBC   AUXH
         BCC   DIV3
         STX   XTNDL
         STA   XTNDH
         INC   ACL
DIV3     DEY
         BNE   DIV2
         RTS



MD1      LDY   #$00       ;ABS VAL OF AC, AUX
         STY   SIGN       ;  WITH RESULT SIGN
         LDX   #AUXL      ;  IN LSB OF SIGN.
         JSR   MD3
         LDX   #ACL
MD3      LDA   LOC1,X     ;X SPECIFIES AC OR AUX
         BPL   MDRTS
         SEC
         TYA
         SBC   LOC0,X     ;COMPL SPECIFIED REG
         STA   LOC0,X     ;  IF NEG.
         TYA
         SBC   LOC1,X
         STA   LOC1,X
         INC   SIGN
MDRTS    RTS








',`










LFB60   JSR     HOME
        LDY     #$08
LFB65   LDA     LFB08,Y
        STA     $040E,Y
        DEY
        BNE     LFB65
        RTS
        LDA     $03F3
        EOR     #$A5
        STA     $03F4
        RTS
LFB78   CMP     #$8D
        BNE     LFB94
        LDY     $C000
        BPL     LFB94
        CPY     #$93
        BNE     LFB94
        BIT     $C010
LFB88   LDY     $C000
        BPL     LFB88
        CPY     #$83
        BEQ     LFB94
        BIT     $C010
LFB94   JMP     VIDOUT
LFB97   SEC
        JMP     ESC1
LFB9B   TAY
        LDA     $FA48,Y ; TODO
        JSR     LFB97
        JSR     RDKEY
LFBA5   CMP     #$CE
        BCS     LFB97
        CMP     #$C9
        BCC     LFB97
        CMP     #$CC
        BEQ     LFB97
        BNE     LFB9B
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
')
