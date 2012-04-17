include(`asm.m4h')
                          ;***********************
                          ;*                     *
                          ;*   APPLE-II PSEUDO   *
                          ;* MACHINE INTERPRETER *
                          ;*                     *
                          ;*   COPYRIGHT 1977    *
                          ;* APPLE COMPUTER INC  *
                          ;*                     *
                          ;* ALL RIGHTS RESERVED *
                          ;*     S. WOZNIAK      *
                          ;*                     *
                          ;***********************


R0L      =     $00
R0H      =     $01
R14H     =     $1D
R15L     =     $1E
R15H     =     $1F

SAVE     =     $FF4A
RESTORE  =     $FF3F





SW16     JSR   SAVE       ;PRESERVE 6502 REG CONTENTS
         PLA
         STA   R15L       ;INIT SWEET16 PC
         PLA              ;FROM RETURN
         STA   R15H       ;  ADDRESS
SW16B    JSR   SW16C      ;INTERPRET AND EXECUTE
         JMP   SW16B      ;ONE SWEET16 INSTR.
SW16C    INC   R15L
         BNE   SW16D      ;INCR SWEET16 PC FOR FETCH
         INC   R15H
SW16D    LDA   #>UF7FD
         PHA              ;PUSH ON STACK FOR RTS
         LDY   #$0
         LDA   (R15L),Y   ;FETCH INSTR
         AND   #$F        ;MASK REG SPECIFICATION
         ASL              ;DOUBLE FOR TWO BYTE REGISTERS
         TAX              ;TO X REG FOR INDEXING
         LSR
         EOR   (R15L),Y   ;NOW HAVE OPCODE
         BEQ   TOBR       ;IF ZERO THEN NON-REG OP
         STX   R14H       ;INDICATE'PRIOR RESULT REG'
         LSR
         LSR              ;OPCODE*2 TO LSB'S
         LSR
         TAY              ;TO Y REG FOR INDEXING
         LDA   OPTBL-2,Y  ;LOW ORDER ADR BYTE
         PHA              ;ONTO STACK
         RTS              ;GOTO REG-OP ROUTINE
TOBR     INC   R15L
         BNE   TOBR2      ;INCR PC
         INC   R15H
TOBR2    LDA   BRTBL,X    ;LOW ORDER ADR BYTE
         PHA              ;ONTO STACK FOR NON-REG OP
         LDA   R14H       ;'PRIOR RESULT REG' INDEX
         LSR              ;PREPARE CARRY FOR BC, BNC.
         RTS              ;GOTO NON-REG OP ROUTINE
RTNZ     PLA              ;POP RETURN ADDRESS
         PLA
         JSR   RESTORE    ;RESTORE 6502 REG CONTENTS
         JMP   (R15L)     ;RETURN TO 6502 CODE VIA PC
SETZ     LDA   (R15L),Y   ;HIGH-ORDER BYTE OF CONSTANT
         STA   R0H,X
         DEY
         LDA   (R15L),Y   ;LOW-ORDER BYTE OF CONSTANT
         STA   R0L,X
         TYA              ;Y-REG CONTAINS 1
         SEC
         ADC   R15L       ;ADD 2 TO PC
         STA   R15L
         BCC   SET2
         INC   R15H
SET2     RTS
OPTBL    ASM_DATA(`<SETX-1')  ;1X
BRTBL    ASM_DATA(`<RTN-1')   ;0
         ASM_DATA(`<LD-1')    ;2X
         ASM_DATA(`<BR-1')    ;1
         ASM_DATA(`<ST-1')    ;3X
         ASM_DATA(`<BNC-1')   ;2
         ASM_DATA(`<LDAT-1')  ;4X
         ASM_DATA(`<BC-1')    ;3
         ASM_DATA(`<STAT-1')  ;5X
         ASM_DATA(`<BP-1')    ;4
         ASM_DATA(`<LDDAT-1') ;6X
         ASM_DATA(`<BM-1')    ;5
         ASM_DATA(`<STDAT-1') ;7X
         ASM_DATA(`<BZ-1')    ;6
         ASM_DATA(`<POP-1')   ;8X
         ASM_DATA(`<BNZ-1')   ;7
         ASM_DATA(`<STPAT-1') ;9X
         ASM_DATA(`<BM1-1')   ;8
         ASM_DATA(`<ADD-1')   ;AX
         ASM_DATA(`<BNM1-1')  ;9
         ASM_DATA(`<SUB-1')   ;BX
         ASM_DATA(`<BK-1')    ;A
         ASM_DATA(`<POPD-1')  ;CX
         ASM_DATA(`<RS-1')    ;B
         ASM_DATA(`<CPR-1')   ;DX
         ASM_DATA(`<BS-1')    ;C
         ASM_DATA(`<INR-1')   ;EX
         ASM_DATA(`<NUL-1')   ;D
         ASM_DATA(`<DCR-1')   ;FX
         ASM_DATA(`<NUL-1')   ;E
         ASM_DATA(`<NUL-1')   ;UNUSED
         ASM_DATA(`<NUL-1')   ;F
SETX     BPL   SETZ       ;ALWAYS TAKEN
LD       LDA   R0L,X
BK       =     *-1
         STA   R0L
         LDA   R0H,X      ;MOVE RX TO R0
         STA   R0H
         RTS
ST       LDA   R0L
         STA   R0L,X      ;MOVE R0 TO RX
         LDA   R0H
         STA   R0H,X
         RTS
STAT     LDA   R0L
STAT2    STA   (R0L,X)    ;STORE BYTE INDIRECT
         LDY   #$0
STAT3    STY   R14H       ;INDICATE R0 IS RESULT NEG
INR      INC   R0L,X
         BNE   INR2       ;INCR RX
         INC   R0H,X
INR2     RTS
LDAT     LDA   (R0L,X)    ;LOAD INDIRECT (RX)
         STA   R0L        ;TO R0
         LDY   #$0
         STY   R0H        ;ZERO HIGH-ORDER R0 BYTE
         BEQ   STAT3      ;ALWAYS TAKEN
POP      LDY   #$0        ;HIGH ORDER BYTE = 0
         BEQ   POP2       ;ALWAYS TAKEN
POPD     JSR   DCR        ;DECR RX
         LDA   (R0L,X)    ;POP HIGH ORDER BYTE @RX
         TAY              ;SAVE IN Y-REG
POP2     JSR   DCR        ;DECR RX
         LDA   (R0L,X)    ;LOW-ORDER BYTE
         STA   R0L        ;TO R0
         STY   R0H
POP3     LDY   #$0        ;INDICATE R0 AS LAST RESULT REG
         STY   R14H
         RTS
LDDAT    JSR   LDAT       ;LOW-ORDER BYTE TO R0, INCR RX
         LDA   (R0L,X)    ;HIGH-ORDER BYTE TO R0
         STA   R0H
         JMP   INR        ;INCR RX
STDAT    JSR   STAT       ;STORE INDIRECT LOW-ORDER
         LDA   R0H        ;BYTE AND INCR RX.  THEN
         STA   (R0L,X)    ;STORE HIGH-ORDER BYTE.
         JMP   INR        ;INCR RX AND RETURN
STPAT    JSR   DCR        ;DECR RX
         LDA   R0L
         STA   (R0L,X)    ;STORE R0 LOW BYTE @RX
         JMP   POP3       ;INDICATE R0 AS LAST RSLT REG
DCR      LDA   R0L,X
         BNE   DCR2       ;DECR RX
         DEC   R0H,X
DCR2     DEC   R0L,X
         RTS
SUB      LDY   #$0        ;RESULT TO R0
CPR      SEC              ;NOTE Y-REG = 13*2 FOR CPR
         LDA   R0L
         SBC   R0L,X
         STA   R0L,Y      ;R0-RX TO RY
         LDA   R0H
         SBC   R0H,X
SUB2     STA   R0H,Y
         TYA              ;LAST RESULT REG*2
         ADC   #$0        ;CARRY TO LSB
         STA   R14H
         RTS
ADD      LDA   R0L
         ADC   R0L,X
         STA   R0L        ;R0+RX TO R0
         LDA   R0H
         ADC   R0H,X
         LDY   #$0        ;R0 FOR RESULT
         BEQ   SUB2       ;FINISH ADD
BS       LDA   R15L       ;NOTE X-REG IS 12*2!
         JSR   STAT2      ;PUSH LOW PC BYTE VIA R12
         LDA   R15H
         JSR   STAT2      ;PUSH HIGH-ORDER PC BYTE
BR       CLC
BNC      BCS   BNC2       ;NO CARRY TEST
BR1      LDA   (R15L),Y   ;DISPLACEMENT BYTE
         BPL   BR2
         DEY
BR2      ADC   R15L       ;ADD TO PC
         STA   R15L
         TYA
         ADC   R15H
         STA   R15H
BNC2     RTS
BC       BCS   BR
         RTS
BP       ASL              ;DOUBLE RESULT-REG INDEX
         TAX              ;TO X REG FOR INDEXING
         LDA   R0H,X      ;TEST FOR PLUS
         BPL   BR1        ;BRANCH IF SO
         RTS
BM       ASL              ;DOUBLE RESULT-REG INDEX
         TAX
         LDA   R0H,X      ;TEST FOR MINUS
         BMI   BR1
         RTS
BZ       ASL              ;DOUBLE RESULT-REG INDEX
         TAX
         LDA   R0L,X      ;TEST FOR ZERO
         ORA   R0H,X      ;(BOTH BYTES)
         BEQ   BR1        ;BRANCH IF SO
         RTS
BNZ      ASL              ;DOUBLE RESULT-REG INDEX
         TAX
         LDA   R0L,X      ;TEST FOR NON-ZERO
         ORA   R0H,X      ;(BOTH BYTES)
         BNE   BR1        ;BRANCH IF SO
         RTS
BM1      ASL              ;DOUBLE RESULT-REG INDEX
         TAX
         LDA   R0L,X      ;CHECK BOTH BYTES
         AND   R0H,X      ;FOR $FF (MINUS 1)
         EOR   #$FF
         BEQ   BR1        ;BRANCH IF SO
         RTS
BNM1     ASL              ;DOUBLE RESULT-REG INDEX
         TAX
         LDA   R0L,X
         AND   R0H,X      ;CHECK BOTH BYTES FOR NO $FF
         EOR   #$FF
         BNE   BR1        ;BRANCH IF NOT MINUS 1
NUL      RTS
RS       LDX   #$18       ;12*2 FOR R12 AS STACK POINTER
         JSR   DCR        ;DECR STACK POINTER
         LDA   (R0L,X)    ;POP HIGH RETURN ADDRESS TO PC
         STA   R15H
         JSR   DCR        ;SAME FOR LOW-ORDER BYTE
         LDA   (R0L,X)
         STA   R15L
         RTS
RTN      JMP   RTNZ

UF7FD    ASM_DATA(`>RTNZ')
         ASM_RES(2,$FF)
