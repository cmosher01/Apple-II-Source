include(`asm.m4h')
include(`symbols.m4h')

;-----------------------------------------------------------------------
;
; DEBUGGER
;
; Handles stepping, register display, IRQ, BRK.
;
;-----------------------------------------------------------------------


ifelse(eval(VERSION `< 2'),1,`

         ASM_DATA($FF,$FF,$FF)

STEP     JSR   INSTDSP    ;DISASSEMBLE ONE INST
         PLA              ;  AT (PCL,H)
         STA   RTNL       ;ADJUST TO USER
         PLA              ;  STACK. SAVE
         STA   RTNH       ;  RTN ADR.
         LDX   #$08
XQINIT   LDA   INITBL-1,X ;INIT XEQ AREA
         STA   XQT,X
         DEX
         BNE   XQINIT
         LDA   (PCL,X)    ;USER OPCODE BYTE
         BEQ   XBRK       ;SPECIAL IF BREAK
         LDY   LENGTH     ;LEN FROM DISASSEMBLY
         CMP   #$20
         BEQ   XJSR       ;HANDLE JSR, RTS, JMP,
         CMP   #$60       ;  JMP (), RTI SPECIAL
         BEQ   XRTS
         CMP   #$4C
         BEQ   XJMP
         CMP   #$6C
         BEQ   XJMPAT
         CMP   #$40
         BEQ   XRTI
         AND   #$1F
         EOR   #$14
         CMP   #$04       ;COPY USER INST TO XEQ AREA
         BEQ   XQ2        ;  WITH TRAILING NOPS
XQ1      LDA   (PCL),Y    ;CHANGE REL BRANCH
XQ2      STA   XQT,Y      ;  DISP TO 4 FOR
         DEY              ;  JMP TO BRANCH OR
         BPL   XQ1        ;  NBRANCH FROM XEQ.
         JSR   RESTORE    ;RESTORE USER REG CONTENTS.
         JMP   XQT        ;XEQ USER OP FROM RAM

IRQ      STA   ACC        ;  (RETURN TO NBRANCH)
         PLA
         PHA              ;**IRQ HANDLER
         ASL
         ASL
         ASL
         BMI   BREAK      ;TEST FOR BREAK
         JMP   (IRQLOC)   ;USER ROUTINE VECTOR IN RAM

BREAK    PLP
         JSR   SAV1       ;SAVE REGS ON BREAK
         PLA              ;  INCLUDING PC
         STA   PCL
         PLA
         STA   PCH
XBRK     JSR   INSDS1     ;PRINT USER PC.
         JSR   RGDSP1     ;  AND REGS
         JMP   MON        ;GO TO MONITOR

XRTI     CLC
         PLA              ;SIMULATE RTI BY EXPECTING
         STA   STATUS     ;  STATUS FROM STACK, THEN RTS
XRTS     PLA              ;RTS SIMULATION
         STA   PCL        ;  EXTRACT PC FROM STACK
         PLA              ;  AND UPDATE PC BY 1 (LEN=0)
PCINC2   STA   PCH
PCINC3   LDA   LENGTH     ;UPDATE PC BY LEN
         JSR   PCADJ3
         STY   PCH
         CLC
         BCC   NEWPCL
XJSR     CLC
         JSR   PCADJ2     ;UPDATE PC AND PUSH
         TAX              ;  ONTO STACK FOR
         TYA              ;  JSR SIMULATE
         PHA
         TXA
         PHA
         LDY   #$02
XJMP     CLC
XJMPAT   LDA   (PCL),Y
         TAX              ;LOAD PC FOR JMP,
         DEY              ;  (JMP) SIMULATE.
         LDA   (PCL),Y
         STX   PCH
NEWPCL   STA   PCL
         BCS   XJMP
RTNJMP   LDA   RTNH
         PHA
         LDA   RTNL
         PHA

',`

IRQ      STA   ACC
         PLA
         PHA
         ASL
         ASL
         ASL
         BMI   BREAK
         JMP   (IRQLOC)

BREAK    PLP
         JSR   SAV1
         PLA
         STA   PCL
         PLA
         STA   PCH
         JMP   ($03F0)
         JSR   INSDS1
         JSR   RGDSP1
         JMP   MON

RESET2   CLD
         JSR   SETNORM
         JSR   INIT
         JSR   SETVID
         JSR   SETKBD
         LDA   $C058
         LDA   $C05A
         LDA   $C05D
         LDA   $C05F
         LDA   $CFFF
         BIT   $C010
         CLD
         JSR   BELL
         LDA   $03F3
         EOR   #%10100101
         CMP   $03F4
         BNE   LFAA6
         LDA   $03F2
         BNE   LFAA3
         LDA   #$E0
         CMP   $03F3
         BNE   LFAA3
LFA9B    LDY   #$03
         STY   $03F2
         JMP   BASIC

LFAA3    JMP   ($03F2)
LFAA6    JSR   LFB60
         LDX   #$05
LFAAB    LDA   LFAFC,X
         STA   $03EF,X
         DEX
         BNE   LFAAB
         LDA   #$C8
         STX   $00
         STA   $01
LFABA    LDY   #$07
         DEC   $01
         LDA   $01
         CMP   #$C0
         BEQ   LFA9B
         STA   $07F8
LFAC7    LDA   ($00),Y
         CMP   LFB01,Y
         BNE   LFABA
         DEY
         DEY
         BPL   LFAC7
         JMP   ($0000)
         NOP
         NOP
')







REGDSP   JSR   CROUT      ;DISPLAY USER REG
RGDSP1   LDA   #<ACC      ;  CONTENTS WITH
         STA   A3L        ;  LABELS
         LDA   #>ACC
         STA   A3H
         LDX   #$FB
RDSP1    LDA   #HICHAR(` ')
         JSR   COUT
         ;LDA   RTBL-$FB,X
         LDA   RTBL+$FF05,X

         JSR   COUT
         LDA   #HICHAR(`=')
         JSR   COUT
         LDA   ACC+5,X
         JSR   PRBYTE
         INX
         BMI   RDSP1
LFAFC    RTS





ifelse(eval(VERSION < 2),1,`

BRANCH   CLC              ;BRANCH TAKEN,
         LDY   #$01       ;  ADD LEN+2 TO PC
         LDA   (PCL),Y
         JSR   PCADJ3
         STA   PCL
         TYA
         SEC
         BCS   PCINC2

NBRNCH   JSR   SAVE       ;NORMAL RETURN AFTER
         SEC              ;  XEQ USER OF
         BCS   PCINC3     ;GO UPDATE PC



INITBL   NOP
         NOP              ;DUMMY FILL FOR
         JMP   NBRNCH     ;  XEQ AREA
         JMP   BRANCH

',`

         ASM_DATA($59,$FA,$00,$E0)
LFB01    ASM_DATA($45,$20,$FF,$00,$FF,$03,$FF)
LFB08    ASM_DATA($3C)
         HIASCII(`APPLE ][')
         HIASCII(`DBA')
         ASM_DATA($FF)
         HIASCII(`C')
         ASM_DATA($FF,$FF,$FF)
')

RTBL     HIASCII(`AXYPS')
