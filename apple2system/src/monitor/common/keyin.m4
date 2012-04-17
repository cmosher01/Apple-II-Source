include(`asm.m4h')
include(`symbols.m4h')

KBD      =     $C000
KBDSTRB  =     $C010

RDKEY    LDY   CH
         LDA   (BASL),Y   ;SET SCREEN TO FLASH
         PHA
         AND   #$3F
         ORA   #$40
         STA   (BASL),Y
         PLA

         JMP   (KSWL)     ;GO TO USER KEY-IN
KEYIN    INC   RNDL
         BNE   KEYIN2     ;INCR RND NUMBER
         INC   RNDH
KEYIN2   BIT   KBD        ;KEY DOWN?
         BPL   KEYIN      ;  LOOP
         STA   (BASL),Y   ;REPLACE FLASHING SCREEN
         LDA   KBD        ;GET KEYCODE
         BIT   KBDSTRB    ;CLR KEY STROBE
         RTS



ESC      JSR   RDKEY      ;GET KEYCODE
ifelse(eval(VERSION `< 2'),1,`
         JSR   ESC1       ;  HANDLE ESC FUNC.
',`
         JSR   LFBA5
')

RDCHAR   JSR   RDKEY      ;READ KEY
         CMP   #$9B       ;ESC?
         BEQ   ESC        ;  YES, DON'T RETURN
         RTS






NOTCR    LDA   INVFLG
         PHA
         LDA   #$FF
         STA   INVFLG     ;ECHO USER LINE
         LDA   IN,X       ;  NON INVERSE
         JSR   COUT
         PLA
         STA   INVFLG

         LDA   IN,X
         CMP   #$88       ;CHECK FOR EDIT KEYS
         BEQ   BCKSPC     ;  BS, CTRL-X
         CMP   #$98
         BEQ   CANCEL
         CPX   #$F8       ;MARGIN?
         BCC   NOTCR1
         JSR   BELL       ;  YES, SOUND BELL
NOTCR1   INX              ;ADVANCE INPUT INDEX
         BNE   NXTCHAR

CANCEL   LDA   #HICHAR(`\') ;BACKSLASH AFTER CANCELLED LINE
         JSR   COUT
GETLNZ   JSR   CROUT      ;OUTPUT CR

GETLN    LDA   PROMPT
         JSR   COUT       ;OUTPUT PROMPT CHAR
         LDX   #$01       ;INIT INPUT INDEX
BCKSPC   TXA              ;  WILL BACKSPACE TO 0
         BEQ   GETLNZ
         DEX

NXTCHAR  JSR   RDCHAR
NXTCHAR1 CMP   #PICK      ;USE SCREEN CHAR
         BNE   CAPTST     ;  FOR CTRL-U
         LDA   (BASL),Y
CAPTST   CMP   #$E0
         BCC   ADDINP     ;CONVERT TO CAPS
         AND   #$DF
ADDINP   STA   IN,X       ;ADD TO INPUT BUF
         CMP   #$8D
         BNE   NOTCR
         JSR   CLREOL     ;CLR TO EOL IF CR
