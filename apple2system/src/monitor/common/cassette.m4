include(`asm.m4h')
include(`symbols.m4h')

TAPEOUT  =     $C020
TAPEIN   =     $C060

HEADR    LDY   #$4B       ;WRITE A*256 'LONG 1'
         JSR   ZERDLY     ;  HALF CYCLES
         BNE   HEADR      ;  (650 USEC EACH)
         ADC   #$FE
         BCS   HEADR      ;THEN A 'SHORT 0'
         LDY   #$21       ;  (400 USEC)
WRBIT    JSR   ZERDLY     ;WRITE TWO HALF CYCLES
         INY              ;  OF 250 USEC ('0')
         INY              ;  OR 500 USEC ('0')
ZERDLY   DEY
         BNE   ZERDLY
         BCC   WRTAPE     ;Y IS COUNT FOR
         LDY   #$32       ;  TIMING LOOP
ONEDLY   DEY
         BNE   ONEDLY
WRTAPE   LDY   TAPEOUT
         LDY   #$2C
         DEX
         RTS



RDBYTE   LDX   #$08       ;8 BITS TO READ
RDBYT2   PHA              ;READ TWO TRANSITIONS
         JSR   RD2BIT     ;  (FIND EDGE)
         PLA
         ROL              ;NEXT BIT
         LDY   #$3A       ;COUNT FOR SAMPLES
         DEX
         BNE   RDBYT2
         RTS

RD2BIT   JSR   RDBIT
RDBIT    DEY              ;DECR Y UNTIL
         LDA   TAPEIN     ; TAPE TRANSITION
         EOR   LASTIN
         BPL   RDBIT
         EOR   LASTIN
         STA   LASTIN
         CPY   #$80       ;SET CARRY ON Y
         RTS
