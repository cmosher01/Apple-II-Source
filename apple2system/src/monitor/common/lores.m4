include(`asm.m4h')
include(`symbols.m4h')

LORESHEIGHT = TEXTHEIGHT*2

;-----------------------------------------------------------------------
;
; LO-RES GRAPHICS
;
; Some low-resolution-graphics routines.
;
;-----------------------------------------------------------------------





PLOT     LSR              ;Y-COORD/2
         PHP              ;SAVE LSB IN CARRY
         JSR   GBASCALC   ;CALC BASE ADR IN GBASL,H
         PLP              ;RESTORE LSB FROM CARRY
         LDA   #%00001111 ;MASK $0F IF EVEN
         BCC   RTMASK
         ADC   #%11100000 ;MASK $F0 IF ODD
RTMASK   STA   MASK
PLOT1    LDA   (GBASL),Y  ;DATA
         EOR   COLOR      ; EOR COLOR
         AND   MASK       ;  AND MASK
         EOR   (GBASL),Y  ;   EOR DATA
         STA   (GBASL),Y  ;    TO DATA
         RTS

HLINE    JSR   PLOT       ;PLOT SQUARE
HLINE1   CPY   H2         ;DONE?
         BCS   RTS1       ; YES, RETURN
         INY              ; NO, INC INDEX (X-COORD)
         JSR   PLOT1      ;PLOT NEXT SQUARE
         BCC   HLINE1     ;ALWAYS TAKEN
VLINEZ   ADC   #1         ;NEXT Y-COORD
VLINE    PHA              ; SAVE ON STACK
         JSR   PLOT       ; PLOT SQUARE
         PLA
         CMP   V2         ;DONE?
         BCC   VLINEZ     ; NO, LOOP
RTS1     RTS



CLRSCR   LDY   #LORESHEIGHT-1 ;MAX Y, FULL SCRN CLR
         BNE   CLRSC2     ;ALWAYS TAKEN
CLRTOP   LDY   #TEXTWIDTH-1 ;MAX Y, TOP SCREEN CLR
CLRSC2   STY   V2         ;STORE AS BOTTOM COORD
                          ; FOR VLINE CALLS
         LDY   #TEXTWIDTH-1 ;RIGHTMOST X-COORD (COLUMN)
CLRSC3   LDA   #0         ;TOP COORD FOR VLINE CALLS
         STA   COLOR      ;CLEAR COLOR (BLACK)
         JSR   VLINE      ;DRAW VLINE
         DEY              ;NEXT LEFTMOST X-COORD
         BPL   CLRSC3     ;LOOP UNTIL DONE
         RTS



GBASCALC PHA              ;FOR INPUT 000DEFGH
         LSR
         AND   #%00000011
         ORA   #%00000100 ;  GENERATE GBASH=000001FG
         STA   GBASH
         PLA              ;  AND GBASL=HDEDE000
         AND   #%00011000
         BCC   GBCALC
         ADC   #$80-1
GBCALC   STA   GBASL
         ASL
         ASL
         ORA   GBASL
         STA   GBASL
         RTS



NXTCOL   LDA   COLOR      ;INCREMENT COLOR BY 3
         CLC
         ADC   #3
SETCOL   AND   #%00001111 ;SETS COLOR=17*A MOD 16
         STA   COLOR
         ASL              ;BOTH HALF BYTES OF COLOR EQUAL
         ASL
         ASL
         ASL
         ORA   COLOR
         STA   COLOR
         RTS



SCRN     LSR              ;READ SCREEN Y-COORD/2
         PHP              ;SAVE LSB (CARRY)
         JSR   GBASCALC   ;CALC BASE ADDRESS
         LDA   (GBASL),Y  ;GET BYTE
         PLP              ;RESTORE LSB FROM CARRY

SCRN2    BCC   RTMSKZ     ;IF EVEN, USE LO H
         LSR
         LSR
         LSR              ;SHIFT HIGH HALF BYTE DOWN
         LSR
RTMSKZ   AND   #%00001111 ;MASK 4-BITS
         RTS
