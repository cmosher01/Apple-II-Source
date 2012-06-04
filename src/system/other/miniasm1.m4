include(`asm.m4h')
                          ;***********************
                          ;*                     *
                          ;*      APPLE-II       *
                          ;*   MINI-ASSEMBLER    *
                          ;*                     *
                          ;*  COPYRIGHT 1977 BY  *
                          ;* APPLE COMPUTER INC. *
                          ;*                     *
                          ;* ALL RIGHTS RESERVED *
                          ;*                     *
                          ;*     S. WOZNIAK      *
                          ;*      A. BAUM        *
                          ;***********************
FORMAT   =     $2E
LENGTH   =     $2F
MODE     =     $31
PROMPT   =     $33
YSAV     =     $34
L        =     $35
PCL      =     $3A
PCH      =     $3B
A1H      =     $3D
A2L      =     $3E
A2H      =     $3F
A4L      =     $42
A4H      =     $43
FMT      =     $44

IN       =     $0200

INSDS2   =     $F88E
INSTDSP  =     $F8D0
PRBL2    =     $F94A
PCADJ    =     $F953
CHAR1    =     $F9B4
CHAR2    =     $F9BA
MNEML    =     $F9C0
MNEMR    =     $FA00
CURSUP   =     $FC1A
GETLNZ   =     $FD67
COUT     =     $FDED
BL1      =     $FE00
A1PCLP   =     $FE78
BELL     =     $FF3A
GETNUM   =     $FFA7
TOSUB    =     $FFBE
ZMODE    =     $FFC7
CHRTBL   =     $FFCC



REL      SBC   #$81       ;IS FMT COMPATIBLE
         LSR              ;WITH RELATIVE MODE?
         BNE   ERR3       ;  NO.
         LDY   A2H
         LDX   A2L        ;DOUBLE DECREMENT
         BNE   REL2
         DEY
REL2     DEX
         TXA
         CLC
         SBC   PCL        ;FORM ADDR-PC-2
         STA   A2L
         BPL   REL3
         INY
REL3     TYA
         SBC   PCH
ERR3     BNE   ERR        ;ERROR IF >1-BYTE BRANCH
FINDOP   LDY   LENGTH
FNDOP2   LDA   A1H,Y      ;MOVE INST TO (PC)
         STA   (PCL),Y
         DEY
         BPL   FNDOP2
         JSR   CURSUP
         JSR   CURSUP     ;RESTORE CURSOR
         JSR   INSTDSP    ;TYPE FORMATTED LINE
         JSR   PCADJ      ;UPDATE PC
         STY   PCH
         STA   PCL
         JMP   NXTLINE    ;GET NEXT LINE
FAKEMON3 JSR   TOSUB      ;GO TO DELIM HANDLER
         LDY   YSAV       ;RESTORE Y-INDEX
FAKEMON  JSR   GETNUM     ;READ PARAM
         STY   YSAV       ;SAVE Y-INDEX
         LDY   #$17       ;INIT DELIMITER INDEX
FAKEMON2 DEY              ;CHECK NEXT DELIM
         BMI   RESETZ     ;ERR IF UNRECOGNIZED DELIM
         CMP   CHRTBL,Y   ;COMPARE WITH DELIM TABLE
         BNE   FAKEMON2   ;NO MATCH
         CPY   #$15       ;MATCH, IS IT CR?
         BNE   FAKEMON3   ;NO, HANDLE IT IN MONITOR
         LDA   MODE
         LDY   #$0
         DEC   YSAV
         JSR   BL1        ;HANDLE CR OUTSIDE MONITOR
         JMP   NXTLINE
TRYNEXT  LDA   A1H        ;GET TRIAL OPCODE
         JSR   INSDS2     ;GET FMT+LENGTH FOR OPCODE
         TAX
         LDA   MNEMR,X    ;GET LOWER MNEMONIC BYTE
         CMP   A4L        ;MATCH?
         BNE   NEXTOP     ;NO, TRY NEXT OPCODE.
         LDA   MNEML,X    ;GET UPPER MNEMONIC BYTE
         CMP   A4H        ;MATCH?
         BNE   NEXTOP     ;NO, TRY NEXT OPCODE
         LDA   FMT
         LDY   FORMAT     ;GET TRIAL FORMAT
         CPY   #$9D       ;TRIAL FORMAT RELATIVE?
         BEQ   REL        ;YES.
NREL     CMP   FORMAT     ;SAME FORMAT?
         BEQ   FINDOP     ;YES.
NEXTOP   DEC   A1H        ;NO, TRY NEXT OPCODE
         BNE   TRYNEXT
         INC   FMT        ;NO MORE, TRY WITH LEN=2
         DEC   L          ;WAS L=2 ALREADY?
         BEQ   TRYNEXT    ;NO.
ERR      LDY   YSAV       ;YES, UNRECOGNIZED INST.
ERR2     TYA
         TAX
         JSR   PRBL2      ;PRINT ^ UNDER LAST READ
         LDA   #$DE       ;CHAR TO INDICATE ERROR
         JSR   COUT       ;POSITION.
RESETZ   JSR   BELL
NXTLINE  LDA   #HICHAR(`!')
         STA   PROMPT     ;INITIALIZE PROMPT
         JSR   GETLNZ     ;GET LINE.
         JSR   ZMODE      ;INIT SCREEN STUFF
         LDA   IN         ;GET CHAR
         CMP   #$A0       ;ASCII BLANK?
         BEQ   SPACE      ;YES
         INY
         CMP   #$A4       ;ASCII '$' IN COL 1?
         BEQ   FAKEMON    ;YES, SIMULATE MONITOR
         DEY              ;NO, BACKUP A CHAR
         JSR   GETNUM     ;GET A NUMBER
         CMP   #$93       ;':' TERMINATOR?
ERR4     BNE   ERR2       ;NO, ERR.
         TXA
         BEQ   ERR2       ;NO ADR PRECEDING COLON.
         JSR   A1PCLP     ;MOVE ADR TO PCL, PCH.
SPACE    LDA   #$3        ;COUNT OF CHARS IN MNEMONIC
         STA   A1H
NXTMN    JSR   GETNSP     ;GET FIRST MNEM CHAR.
NXTM     ASL
         SBC   #$BE       ;SUBTRACT OFFSET
         CMP   #$C2       ;LEGAL CHAR?
         BCC   ERR2       ;NO.
         ASL              ;COMPRESS-LEFT JUSTIFY
         ASL
         LDX   #$4
NXTM2    ASL              ;DO 5 TRIPLE WORD SHIFTS
         ROL   A4L
         ROL   A4H
         DEX
         BPL   NXTM2
         DEC   A1H        ;DONE WITH 3 CHARS?
         BEQ   NXTM2      ;YES, BUT DO 1 MORE SHIFT
         BPL   NXTMN      ;NO
FORM1    LDX   #$5        ;5 CHARS IN ADDR MODE
FORM2    JSR   GETNSP     ;GET FIRST CHAR OF ADDR
         STY   YSAV
         CMP   CHAR1,X    ;FIRST CHAR MATCH PATTERN?
         BNE   FORM3      ;NO
         JSR   GETNSP     ;YES, GET SECOND CHAR
         CMP   CHAR2,X    ;MATCHES SECOND HALF?
         BEQ   FORM5      ;YES.
         LDA   CHAR2,X    ;NO, IS SECOND HALF ZERO?
         BEQ   FORM4      ;YES.
         CMP   #$A4       ;NO,SECOND HALF OPTIONAL?
         BEQ   FORM4      ;YES.
         LDY   YSAV
FORM3    CLC              ;CLEAR BIT-NO MATCH
FORM4    DEY              ;BACK UP 1 CHAR
FORM5    ROL   FMT        ;FORM FORMAT BYTE
         CPX   #$3        ;TIME TO CHECK FOR ADDR.
         BNE   FORM7      ;NO
         JSR   GETNUM     ;YES
         LDA   A2H
         BEQ   FORM6      ;HIGH-ORDER BYTE ZERO
         INX              ;NO, INCR FOR 2-BYTE
FORM6    STX   L          ;STORE LENGTH
         LDX   #$3        ;RELOAD FORMAT INDEX
         DEY              ;BACKUP A CHAR
FORM7    STX   A1H        ;SAVE INDEX
         DEX              ;DONE WITH FORMAT CHECK?
         BPL   FORM2      ;NO.
         LDA   FMT        ;YES, PUT LENGTH
         ASL              ;IN LOW BITS
         ASL
         ORA   L
         CMP   #$20
         BCS   FORM8      ;ADD "$" IF NONZERO LENGTH
         LDX   L          ;AND DON'T ALREADY HAVE IT
         BEQ   FORM8
         ORA   #$80
FORM8    STA   FMT
         STY   YSAV
         LDA   IN,Y       ;GET NEXT NONBLANK
         CMP   #HICHAR(`;') ; START OF COMMENT?
         BEQ   FORM9      ;YES
         CMP   #$8D       ;CARRIAGE RETURN?
         BNE   ERR4       ;NO, ERR.
FORM9    JMP   TRYNEXT
GETNSP   LDA   IN,Y
         INY
         CMP   #HICHAR(` ') ;GET NEXT NON BLANK CHAR
         BEQ   GETNSP
         RTS
