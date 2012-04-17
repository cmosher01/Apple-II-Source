include(`asm.m4h')
include(`symbols.m4h')

ASCBEL = $07 | %10000000
ASCBS  = $08 | %10000000

;-----------------------------------------------------------------------
;
; DISPLAY AND READ KEYS
;
; Handles reading keypresses, escape, displaying characters, scrolling,
; clearing, etc. (Also includes cassette tape handler.)
;
;-----------------------------------------------------------------------





SPKR     =     $C030



BASCALC  PHA              ;CALC BASE ADR IN BASL,H
         LSR              ;  FOR GIVEN LINE NO
         AND   #%00000011 ;  0<=LINE NO.<=$17
         ORA   #%00000100 ;ARG=000ABCDE, GENERATE
         STA   BASH       ;  BASH=000001CD
         PLA              ;  AND
         AND   #%00011000 ;  BASL=EABAB000
         BCC   BSCLC2
         ADC   #$80-1
BSCLC2   STA   BASL
         ASL
         ASL
         ORA   BASL
         STA   BASL
         RTS



BELL1    CMP   #ASCBEL    ;BELL CHAR? (CNTRL-G)
         BNE   RTS2B      ;  NO, RETURN
         LDA   #$40       ;DELAY .01 SECONDS
         JSR   WAIT
         LDY   #$C0
BELL2    LDA   #$0C       ;TOGGLE SPEAKER AT
         JSR   WAIT       ;  1 KHZ FOR .1 SEC.
         LDA   SPKR
         DEY
         BNE   BELL2
RTS2B    RTS



STOADV   LDY   CH         ;CURSOR H INDEX TO Y-REG
         STA   (BASL),Y   ;STORE CHAR IN LINE
ADVANCE  INC   CH         ;INCREMENT CURSOR H INDEX
         LDA   CH         ;  (MOVE RIGHT)
         CMP   WNDWDTH    ;BEYOND WINDOW WIDTH?
         BCS   CR         ;  YES CR TO NEXT LINE
RTS3     RTS              ;  NO,RETURN

VIDOUT   CMP   #$A0       ;CONTROL CHAR?
         BCS   STOADV     ;  NO,OUTPUT IT.

         TAY              ;INVERSE VIDEO?
         BPL   STOADV     ;  YES, OUTPUT IT.

         CMP   #ASCCR     ;CR?
         BEQ   CR         ;  YES.
         CMP   #ASCLF     ;LINE FEED?
         BEQ   LF         ;  IF SO, DO IT.
         CMP   #ASCBS     ;BACK SPACE? (CNTRL-H)
         BNE   BELL1      ;  NO, CHECK FOR BELL.

BS       DEC   CH         ;DECREMENT CURSOR H INDEX
         BPL   RTS3       ;IF POS, OK. ELSE MOVE UP
         LDA   WNDWDTH    ;SET CH TO WNDWDTH-1
         STA   CH
         DEC   CH         ;(RIGHTMOST SCREEN POS)
UP       LDA   WNDTOP     ;CURSOR V INDEX
         CMP   CV
         BCS   RTS4       ;IF TOP LINE THEN RETURN
         DEC   CV         ;DEC CURSOR V-INDEX

VTAB     LDA   CV         ;GET CURSOR V-INDEX
VTABZ    JSR   BASCALC    ;GENERATE BASE ADR
         ADC   WNDLFT     ;ADD WINDOW LEFT INDEX
         STA   BASL       ;TO BASL
RTS4     RTS






ESC1     EOR   #$C0       ;ESC?
         BEQ   HOME       ;  IF SO, DO HOME AND CLEAR
         ADC   #$FD       ;ESC-A OR B CHECK
         BCC   ADVANCE    ;  A, ADVANCE
         BEQ   BS         ;  B, BACKSPACE
         ADC   #$FD       ;ESC-C OR D CHECK
         BCC   LF         ;  C, DOWN
         BEQ   UP         ;  D, GO UP
         ADC   #$FD       ;ESC-E OR F CHECK
         BCC   CLREOL     ;  E, CLEAR TO END OF LINE
         BNE   RTS4       ;  NOT F, RETURN
                          ;  F, CLEAR TO END OF PAGE
CLREOP   LDY   CH         ;CURSOR H TO Y INDEX
         LDA   CV         ;CURSOR V TO A-REGISTER
CLEOP1   PHA              ;SAVE CURRENT LINE ON STK
         JSR   VTABZ      ;CALC BASE ADDRESS
         JSR   CLEOLZ     ;CLEAR TO EOL, SET CARRY
         LDY   #0         ;CLEAR FROM H INDEX=0 FOR REST
         PLA              ;INCREMENT CURRENT LINE
         ADC   #0         ;(CARRY IS SET)
         CMP   WNDBTM     ;DONE TO BOTTOM OF WINDOW?
         BCC   CLEOP1     ;  NO, KEEP CLEARING LINES
         BCS   VTAB       ;  YES, TAB TO CURRENT LINE



HOME     LDA   WNDTOP     ;INIT CURSOR V
         STA   CV         ;  AND H-INDICES
         LDY   #0
         STY   CH         ;THEN CLEAR TO END OF PAGE
         BEQ   CLEOP1


CR       LDA   #0         ;CURSOR TO LEFT OF INDEX
         STA   CH         ;(RET CURSOR H=0)
LF       INC   CV         ;INCR CURSOR V(DOWN 1 LINE)
         LDA   CV
         CMP   WNDBTM     ;OFF SCREEN?
         BCC   VTABZ      ;  NO, SET BASE ADDR
         DEC   CV         ;DECR CURSOR V (BACK TO BOTTOM)

SCROLL   LDA   WNDTOP     ;START AT TOP OF SCRL WNDW
         PHA
         JSR   VTABZ      ;GENERATE BASE ADR
SCRL1    LDA   BASL       ;COPY BASL,H
         STA   BAS2L      ;  TO BAS2L,H
         LDA   BASH
         STA   BAS2H
         LDY   WNDWDTH    ;INIT Y TO RIGHTMOST INDEX
         DEY              ;  OF SCROLLING WINDOW
         PLA
         ADC   #1         ;INCR LINE NUMBER
         CMP   WNDBTM     ;DONE?
         BCS   SCRL3      ;  YES, FINISH
         PHA
         JSR   VTABZ      ;FORM BASL,H (BASE ADDR)
SCRL2    LDA   (BASL),Y   ;MOVE A CHR UP ON LINE
         STA   (BAS2L),Y
         DEY              ;NEXT CHAR OF LINE
         BPL   SCRL2
         BMI   SCRL1      ;NEXT LINE (ALWAYS TAKEN)

SCRL3    LDY   #0         ;CLEAR BOTTOM LINE
         JSR   CLEOLZ     ;GET BASE ADDR FOR BOTTOM LINE
         BCS   VTAB       ;CARRY IS SET
CLREOL   LDY   CH         ;CURSOR H INDEX
CLEOLZ   LDA   #HICHAR(` ')
CLEOL2   STA   (BASL),Y   ;STORE BLANKS FROM 'HERE'
         INY              ;  TO END OF LINES (WNDWDTH)
         CPY   WNDWDTH
         BCC   CLEOL2
         RTS



WAIT     SEC
WAIT2    PHA
WAIT3    SBC   #1
         BNE   WAIT3      ;1.0204 USEC
         PLA              ;(13+27/2*A+5/2*A*A)
         SBC   #1
         BNE   WAIT2
         RTS



NXTA4    INC   A4L        ;INCR 2-BYTE A4
         BNE   NXTA1      ;  AND A1
         INC   A4H
NXTA1    LDA   A1L        ;INCR 2-BYTE A1.
         CMP   A2L
         LDA   A1H        ;  AND COMPARE TO A2
         SBC   A2H
         INC   A1L        ;  (CARRY SET IF >=)
         BNE   RTS4B
         INC   A1H
RTS4B    RTS
