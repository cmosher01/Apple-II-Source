include(`asm.m4h')
                        ;***********************
                        ;*                     *
                        ;*  APPLE-II FLOATING  *
                        ;*   POINT ROUTINES    *
                        ;*                     *
                        ;*  COPYRIGHT 1977 BY  *
                        ;* APPLE COMPUTER INC. *
                        ;*                     *
                        ;* ALL RIGHTS RESERVED *
                        ;*                     *
                        ;*     S. WOZNIAK      *
                        ;*                     *
                        ;***********************

SIGN      =    $F3
X2        =    $F4
M2        =    $F5
X1        =    $F8
M1        =    $F9
E         =    $FC

OVLOC     =    $03F5





ADD       CLC           ;CLEAR CARRY
          LDX  #$2      ;INDEX FOR 3-BYTE ADD.
ADD1      LDA  M1,X     ;
          ADC  M2,X     ;ADD A BYTE OF MANT2 TO MANT1
          STA  M1,X     ;
          DEX           ;INDEX TO NEXT MORE SIGNIF. BYTE.
          BPL  ADD1     ;LOOP UNTIL DONE.
          RTS           ;RETURN
MD1       ASL  SIGN     ;CLEAR LSB OF SIGN.
          JSR  ABSWAP   ;ABS VAL OF M1, THEN SWAP WITH M2
ABSWAP    BIT  M1       ;MANT1 NEGATIVE?
          BPL  ABSWAP1  ;NO, SWAP WITH MANT2 AND RETURN.
          JSR  FCOMPL   ;YES, COMPLEMENT IT.
          INC  SIGN     ;INCR SIGN, COMPLEMENTING LSB.
ABSWAP1   SEC           ;SET CARRY FOR RETURN TO MUL/DIV.
SWAP      LDX  #$4      ;INDEX FOR 4 BYTE SWAP.
SWAP1     STY  E-1,X    ;
          LDA  X1-1,X   ;SWAP A BYTE OF EXP/MANT1 WITH
          LDY  X2-1,X   ;EXP/MANT2 AND LEAVE A COPY OF
          STY  X1-1,X   ;MANT1 IN E (3 BYTES).  E+3 USED
          STA  X2-1,X   ;
          DEX           ;ADVANCE INDEX TO NEXT BYTE
          BNE  SWAP1    ;LOOP UNTIL DONE.
          RTS           ;RETURN
FLOAT     LDA  #$8E     ;INIT EXP1 TO 14,
          STA  X1       ;THEN NORMALIZE TO FLOAT.
NORM1     LDA  M1       ;HIGH-ORDER MANT1 BYTE.
          CMP  #$C0     ;UPPER TWO BITS UNEQUAL?
          BMI  RTS1     ;YES, RETURN WITH MANT1 NORMALIZED
          DEC  X1       ;DECREMENT EXP1.
          ASL  M1+2     ;
          ROL  M1+1     ;SHIFT MANT1 (3 BYTES) LEFT.
          ROL  M1       ;
NORM      LDA  X1       ;EXP1 ZERO?
          BNE  NORM1    ;NO, CONTINUE NORMALIZING.
RTS1      RTS           ;RETURN.
FSUB      JSR  FCOMPL   ;CMPL MANT1,CLEARS CARRY UNLESS 0
SWPALGN   JSR  ALGNSWP  ;RIGHT SHIFT MANT1 OR SWAP WITH
FADD      LDA  X2       ;
          CMP  X1       ;COMPARE EXP1 WITH EXP2.
          BNE  SWPALGN  ;IF #,SWAP ADDENDS OR ALIGN MANTS.
          JSR  ADD      ;ADD ALIGNED MANTISSAS.
ADDEND    BVC  NORM     ;NO OVERFLOW, NORMALIZE RESULT.
          BVS  RTLOG    ;OV: SHIFT M1 RIGHT, CARRY INTO SIGN
ALGNSWP   BCC  SWAP     ;SWAP IF CARRY CLEAR,
                        ;ELSE SHIFT RIGHT ARITH.
RTAR      LDA  M1       ;SIGN OF MANT1 INTO CARRY FOR
          ASL           ;RIGHT ARITH SHIFT.
RTLOG     INC  X1       ;INCR X1 TO ADJUST FOR RIGHT SHIFT
          BEQ  OVFL     ;EXP1 OUT OF RANGE.
RTLOG1    LDX  #$FA     ;INDEX FOR 6:BYTE RIGHT SHIFT.
ROR1      ROR  E+3,X    ;
          INX           ;NEXT BYTE OF SHIFT.
          BNE  ROR1     ;LOOP UNTIL DONE.
          RTS           ;RETURN.
FMUL      JSR  MD1      ;ABS VAL OF MANT1, MANT2
          ADC  X1       ;ADD EXP1 TO EXP2 FOR PRODUCT EXP
          JSR  MD2      ;CHECK PROD. EXP AND PREP. FOR MUL
          CLC           ;CLEAR CARRY FOR FIRST BIT.
MUL1      JSR  RTLOG1   ;M1 AND E RIGHT (PROD AND MPLIER)
          BCC  MUL2     ;IF CARRY CLEAR, SKIP PARTIAL PROD
          JSR  ADD      ;ADD MULTIPLICAND TO PRODUCT.
MUL2      DEY           ;NEXT MUL ITERATION.
          BPL  MUL1     ;LOOP UNTIL DONE.
MDEND     LSR  SIGN     ;TEST SIGN LSB.
NORMX     BCC  NORM     ;IF EVEN,NORMALIZE PROD,ELSE COMP
FCOMPL    SEC           ;SET CARRY FOR SUBTRACT.
          LDX  #$3      ;INDEX FOR 3 BYTE SUBTRACT.
COMPL1    LDA  #$0      ;CLEAR A.
          SBC  X1,X     ;SUBTRACT BYTE OF EXP1.
          STA  X1,X     ;RESTORE IT.
          DEX           ;NEXT MORE SIGNIFICANT BYTE.
          BNE  COMPL1   ;LOOP UNTIL DONE.
          BEQ  ADDEND   ;NORMALIZE (OR SHIFT RT IF OVFL).
FDIV      JSR  MD1      ;TAKE ABS VAL OF MANT1, MANT2.
          SBC  X1       ;SUBTRACT EXP1 FROM EXP2.
          JSR  MD2      ;SAVE AS QUOTIENT EXP.
DIV1      SEC           ;SET CARRY FOR SUBTRACT.
          LDX  #$2      ;INDEX FOR 3-BYTE SUBTRACTION.
DIV2      LDA  M2,X     ;
          SBC  E,X      ;SUBTRACT A BYTE OF E FROM MANT2.
          PHA           ;SAVE ON STACK.
          DEX           ;NEXT MORE SIGNIFICANT BYTE.
          BPL  DIV2     ;LOOP UNTIL DONE.
          LDX  #$FD     ;INDEX FOR 3-BYTE CONDITIONAL MOVE
DIV3      PLA           ;PULL BYTE OF DIFFERENCE OFF STACK
          BCC  DIV4     ;IF M2<E THEN DON'T RESTORE M2.
          STA  M2+3,X   ;
DIV4      INX           ;NEXT LESS SIGNIFICANT BYTE.
          BNE  DIV3     ;LOOP UNTIL DONE.
          ROL  M1+2     ;
          ROL  M1+1     ;ROLL QUOTIENT LEFT, CARRY INTO LSB
          ROL  M1       ;
          ASL  M2+2     ;
          ROL  M2+1     ;SHIFT DIVIDEND LEFT
          ROL  M2       ;
          BCS  OVFL     ;OVFL IS DUE TO UNNORMED DIVISOR
          DEY           ;NEXT DIVIDE ITERATION.
          BNE  DIV1     ;LOOP UNTIL DONE 23 ITERATIONS.
          BEQ  MDEND    ;NORM. QUOTIENT AND CORRECT SIGN.
MD2       STX  M1+2     ;
          STX  M1+1     ;CLEAR MANT1 (3 BYTES) FOR MUL/DIV.
          STX  M1       ;
          BCS  OVCHK    ;IF CALC. SET CARRY,CHECK FOR OVFL
          BMI  MD3      ;IF NEG THEN NO UNDERFLOW.
          PLA           ;POP ONE RETURN LEVEL.
          PLA           ;
          BCC  NORMX    ;CLEAR X1 AND RETURN.
MD3       EOR  #$80     ;COMPLEMENT SIGN BIT OF EXPONENT.
          STA  X1       ;STORE IT.
          LDY  #$17     ;COUNT 24 MUL/23 DIV ITERATIONS.
          RTS           ;RETURN.
OVCHK     BPL  MD3      ;IF POSITIVE EXP THEN NO OVFL.
OVFL      JMP  OVLOC





UF4FC     ASM_RES(4,$FF)
