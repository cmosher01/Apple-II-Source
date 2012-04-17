include(`asm.m4h')
include(`symbols.m4h')

;-----------------------------------------------------------------------
;
; INITIALIZE DISPLAY
;
; Handles initializing the display.
;
;-----------------------------------------------------------------------





TXTCLR   =     $C050
TXTSET   =     $C051
MIXSET   =     $C053
LOWSCR   =     $C054
LORES    =     $C056


TEXTBOTTOMLINES = 4


INIT     LDA   #0         ;CLR STATUS FOR DEBUG
         STA   STATUS     ;  SOFTWARE
         LDA   LORES
         LDA   LOWSCR     ;INIT VIDEO MODE



SETTXT   LDA   TXTSET     ;SET FOR TEXT MODE
         LDA   #0         ;  FULL SCREEN WINDOW
         BEQ   SETWND

SETGR    LDA   TXTCLR     ;SET FOR GRAPHICS MODE
         LDA   MIXSET     ;  LOWER 4 LINES AS
         JSR   CLRTOP     ;  TEXT WINDOW
         LDA   #TEXTHEIGHT-TEXTBOTTOMLINES



SETWND   STA   WNDTOP     ;SET FOR 40 COL WINDOW
         LDA   #0         ;  TOP IN A-REG,
         STA   WNDLFT     ;  BTTM AT LINE 24
         LDA   #TEXTWIDTH
         STA   WNDWDTH
         LDA   #TEXTHEIGHT
         STA   WNDBTM     ;  VTAB TO ROW 23
         LDA   #TEXTHEIGHT-1
TABV     STA   CV         ;VTABS TO ROW IN A-REG
         JMP   VTAB
