include(`asm.m4h')
include(`symbols.m4h')
;-----------------------------------------------------------------------
;
; DISASSEMBLER
;
; Handles disassembling 6502 instructions.
;
;-----------------------------------------------------------------------





INSDS1   LDX   PCL        ;PRINT PCL,H
         LDY   PCH
         JSR   PRYX2
         JSR   PRBLNK     ;FOLLOWED BY A BLANK
         LDA   (PCL,X)    ;GET OP CODE
INSDS2   TAY
         LSR              ;EVEN/ODD TEST
         BCC   IEVEN
         ROR              ;BIT 1 TEST
         BCS   ERR        ;XXXXXX11 INVALID OP
         CMP   #$A2
         BEQ   ERR        ;OPCODE $89 INVALID
         AND   #$87       ;MASK BITS
IEVEN    LSR              ;LSB INTO CARRY FOR L/R TEST
         TAX
         LDA   FMT1,X     ;GET FORMAT INDEX BYTE
         JSR   SCRN2      ;R/L H-BYTE ON CARRY
         BNE   GETFMT
ERR      LDY   #$80       ;SUBSTITUTE $80 FOR INVALID OPS
         LDA   #$00       ;SET PRINT FORMAT INDEX TO 0
GETFMT   TAX
         LDA   FMT2,X     ;INDEX INTO PRINT FORMAT TABLE
         STA   FORMAT     ;SAVE FOR ADR FIELD FORMATTING
         AND   #$03       ;MASK FOR 2-BIT (LENGTH-1)
         STA   LENGTH
         TYA              ;OPCODE
         AND   #$8F       ;MASK FOR 1XXX1010 TEST
         TAX              ; SAVE IT
         TYA              ;OPCODE TO A AGAIN
         LDY   #$03
         CPX   #$8A
         BEQ   MNNDX3
MNNDX1   LSR
         BCC   MNNDX3     ;FORM INDEX INTO MNEMONIC TABLE
         LSR
MNNDX2   LSR              ;1) 1XXX1010->00101XXX
         ORA   #$20       ;2) XXXYYY01->00111XXX
         DEY              ;3) XXXYYY10->00110XXX
         BNE   MNNDX2     ;4) XXXYY100->00100XXX
         INY              ;5) XXXXX000->000XXXXX
MNNDX3   DEY
         BNE   MNNDX1
         RTS



         ASM_DATA($FF,$FF,$FF)



INSTDSP  JSR   INSDS1     ;GEN FMT, LEN BYTES
         PHA              ;SAVE MNEMONIC TABLE INDEX
PRNTOP   LDA   (PCL),Y
         JSR   PRBYTE
         LDX   #1         ;PRINT 2 BLANKS
PRNTBL   JSR   PRBL2
         CPY   LENGTH     ;PRINT INST (1-3 BYTES)
         INY              ;IN A 12 CHR FIELD
         BCC   PRNTOP
         LDX   #3         ;CHAR COUNT FOR MNEMONIC PRINT
         CPY   #4
         BCC   PRNTBL
         PLA              ;RECOVER MNEMONIC INDEX
         TAY
         LDA   MNEML,Y
         STA   LMNEM      ;FETCH 3-CHAR MNEMONIC
         LDA   MNEMR,Y    ;  (PACKED IN 2-BYTES)
         STA   RMNEM
PRMN1    LDA   #0
         LDY   #5
PRMN2    ASL   RMNEM      ;SHIFT 5 BITS OF
         ROL   LMNEM      ;  CHARACTER INTO A
         ROL              ;    (CLEARS CARRY)
         DEY
         BNE   PRMN2
         ADC   #HICHAR(`?') ;ADD "?" OFFSET
         JSR   COUT       ;OUTPUT A CHAR OF MNEM
         DEX
         BNE   PRMN1
         JSR   PRBLNK     ;OUTPUT 3 BLANKS
         LDY   LENGTH
         LDX   #6         ;CNT FOR 6 FORMAT BITS
PRADR1   CPX   #3
         BEQ   PRADR5     ;IF X=3 THEN ADDR.
PRADR2   ASL   FORMAT
         BCC   PRADR3
         LDA   CHAR1-1,X
         JSR   COUT
         LDA   CHAR2-1,X
         BEQ   PRADR3
         JSR   COUT
PRADR3   DEX
         BNE   PRADR1
         RTS
PRADR4   DEY
         BMI   PRADR2
         JSR   PRBYTE
PRADR5   LDA   FORMAT
         CMP   #$E8       ;HANDLE REL ADR MODE
         LDA   (PCL),Y    ;SPECIAL (PRINT TARGET,
         BCC   PRADR4     ;  NOT OFFSET)
RELADR   JSR   PCADJ3
         TAX              ;PCL,PCH+OFFSET+1 TO A,Y
         INX
         BNE   PRNTYX     ;+1 TO Y,X
         INY

PRNTYX   TYA
PRNTAX   JSR   PRBYTE     ;OUTPUT TARGET ADR
PRNTX    TXA              ;  OF BRANCH AND RETURN
         JMP   PRBYTE



PRBLNK   LDX   #3         ;BLANK COUNT
PRBL2    LDA   #HICHAR(` ') ;LOAD A SPACE
         JSR   COUT       ;OUTPUT A BLANK
         DEX
         BNE   PRBL2      ;LOOP UNTIL COUNT=0
         RTS



PCADJ    SEC              ;0=1-BYTE, 1=2-BYTE
PCADJ2   LDA   LENGTH     ;  2=3-BYTE
PCADJ3   LDY   PCH
         TAX              ;TEST DISPLACEMENT SIGN
         BPL   PCADJ4     ;  (FOR REL BRANCH)
         DEY              ;EXTEND NEG BY DEC PCH
PCADJ4   ADC   PCL
         BCC   RTS2       ;PCL+LENGTH(OR DISPL)+1 TO A
         INY              ;  CARRY INTO Y (PCH)
RTS2     RTS







                          ; * FMT1 BYTES:    XXXXXXY0 INSTRS
                          ; * IF Y=0         THEN LEFT HALF BYTE
                          ; * IF Y=1         THEN RIGHT HALF BYTE
                          ; *                   (X=INDEX)
FMT1     ASM_DATA($04,$20,$54,$30,$0D)
         ASM_DATA($80,$04,$90,$03,$22)
         ASM_DATA($54,$33,$0D,$80,$04)
         ASM_DATA($90,$04,$20,$54,$33)
         ASM_DATA($0D,$80,$04,$90,$04)
         ASM_DATA($20,$54,$3B,$0D,$80)
         ASM_DATA($04,$90,$00,$22,$44)
         ASM_DATA($33,$0D,$C8,$44,$00)
         ASM_DATA($11,$22,$44,$33,$0D)
         ASM_DATA($C8,$44,$A9,$01,$22)
         ASM_DATA($44,$33,$0D,$80,$04)
         ASM_DATA($90,$01,$22,$44,$33)
         ASM_DATA($0D,$80,$04,$90)
         ASM_DATA($26,$31,$87,$9A) ;$ZZXXXY01 INSTR'S

FMT2     ASM_DATA($00)        ;ERR
         ASM_DATA($21)        ;IMM
         ASM_DATA($81)        ;Z-PAGE
         ASM_DATA($82)        ;ABS
         ASM_DATA($00)        ;IMPLIED
         ASM_DATA($00)        ;ACCUMULATOR
         ASM_DATA($59)        ;(ZPAG,X)
         ASM_DATA($4D)        ;(ZPAG),Y
         ASM_DATA($91)        ;ZPAG,X
         ASM_DATA($92)        ;ABS,X
         ASM_DATA($86)        ;ABS,Y
         ASM_DATA($4A)        ;(ABS)
         ASM_DATA($85)        ;ZPAG,Y
         ASM_DATA($9D)        ;RELATIVE
CHAR1
         ASM_DATA(HICHAR(`,'))
         ASM_DATA(HICHAR(`)'))
         ASM_DATA(HICHAR(`,'))
         ASM_DATA(HICHAR(`#'))
         ASM_DATA(HICHAR(`('))
         ASM_DATA(HICHAR(`$'))

CHAR2    ASM_DATA(HICHAR(`Y'))
         ASM_DATA(0)
         HIASCII(`X$$')
         ASM_DATA(0)



                          ; * MNEML IS OF FORM:
                          ; *  (A) XXXXX000
                          ; *  (B) XXXYY100
                          ; *  (C) 1XXX1010
                          ; *  (D) XXXYYY10
                          ; *  (E) XXXYYY01
                          ; *      (X=INDEX)
MNEML    ASM_DATA($1C,$8A,$1C,$23,$5D,$8B)
         ASM_DATA($1B,$A1,$9D,$8A,$1D,$23)
         ASM_DATA($9D,$8B,$1D,$A1,$00,$29)
         ASM_DATA($19,$AE,$69,$A8,$19,$23)
         ASM_DATA($24,$53,$1B,$23,$24,$53)
         ASM_DATA($19,$A1)    ;(A) FORMAT ABOVE

         ASM_DATA($00,$1A,$5B,$5B,$A5,$69)
         ASM_DATA($24,$24)    ;(B) FORMAT

         ASM_DATA($AE,$AE,$A8,$AD,$29,$00)
         ASM_DATA($7C,$00)    ;(C) FORMAT

         ASM_DATA($15,$9C,$6D,$9C,$A5,$69)
         ASM_DATA($29,$53)    ;(D) FORMAT

         ASM_DATA($84,$13,$34,$11,$A5,$69)
         ASM_DATA($23,$A0)    ;(E) FORMAT

MNEMR    ASM_DATA($D8,$62,$5A,$48,$26,$62)
         ASM_DATA($94,$88,$54,$44,$C8,$54)
         ASM_DATA($68,$44,$E8,$94,$00,$B4)
         ASM_DATA($08,$84,$74,$B4,$28,$6E)
         ASM_DATA($74,$F4,$CC,$4A,$72,$F2)
         ASM_DATA($A4,$8A)    ;(A) FORMAT

         ASM_DATA($00,$AA,$A2,$A2,$74,$74)
         ASM_DATA($74,$72)    ;(B) FORMAT

         ASM_DATA($44,$68,$B2,$32,$B2,$00)
         ASM_DATA($22,$00)    ;(C) FORMAT

         ASM_DATA($1A,$1A,$26,$26,$72,$72)
         ASM_DATA($88,$C8)    ;(D) FORMAT

         ASM_DATA($C4,$CA,$26,$48,$44,$44)
         ASM_DATA($A2,$C8)    ;(E) FORMAT
