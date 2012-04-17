include(`asm.im4')
                                ;6502 HEX MONITOR LISTING
                                ;
                                ;Page 0 Variables
XAML            =   $24
XAMH            =   $25
STL             =   $26
STH             =   $27
L               =   $28
H               =   $29
YSAV            =   $2A
MODE            =   $2B
                                ;Other Variables
IN              =   $0200
                                ;PIA
NMI             =   $0F00
IRQ             =   $0000

KBD             =   $D010
KBDCR           =   $D011
DSP             =   $D012
DSPCR           =   $D013

CR              =   $0D | %10000000
ESC             =   $1B | %10000000



RESET           CLD             ;Clear decimal arithmetic mode.
                CLI
                LDY #%01111111  ;Mask for DSP data direction register
                STY DSP         ;Set it up.
                LDA #%10100111  ;KBD and DSP control register mask.
                STA KBDCR       ;Enable interrupts, set CA1, CB1, for
                STA DSPCR       ; positive edge sense/output mode.





NOTCR           CMP #HICHAR(`_') ;"<-"?

                BEQ BACKSPACE   ;Yes.
                CMP #ESC        ;ESC?
                BEQ ESCAPE      ;Yes.
                INY             ;Advance text index.
                BPL NEXTCHAR    ;Auto ESC if > 127.

ESCAPE          LDA #HICHAR(`\') ;"\".
                JSR ECHO        ;Output it.
GETLINE         LDA #CR         ;CR.
                JSR ECHO        ;Output it.
                LDY #1          ;Initiallize text index.


BACKSPACE       DEY             ;Backup text index.
                BMI GETLINE     ;Beyond start of line, reinitialize


NEXTCHAR        LDA KBDCR       ;Key ready?
                BPL NEXTCHAR    ;Loop until ready.
                LDA KBD         ;Load character. B7 should be '1'.
                STA IN,Y        ;Add to text buffer.
                JSR ECHO        ;Display character.

                CMP #CR         ;CR?
                BNE NOTCR       ;No.





                LDY #$FF        ;Reset text index.
                LDA #0          ;For XAM mode.
                TAX             ;0->X.
SETSTOR         ASL             ;Leaves $7B if setting STOR mode.
SETMODE         STA MODE        ;$00 = XAM, $7B = STOR, $A3 = BLOK XAM.

BLSKIP          INY             ;Advance text index.
NEXTITEM        LDA IN,Y        ;Get character.
                CMP #CR         ;CR?
                BEQ GETLINE     ;Yes, done this line.

                CMP #HICHAR(`.') ;"."?
                BCC BLSKIP      ;Skip delimiter.

                BEQ SETMODE     ;Set BLOCK XAM mode.

                CMP #HICHAR(`:') ;":"?
                BEQ SETSTOR     ;Yes, set STOR mode.

                CMP #HICHAR(`R') ;"R"?
                BEQ RUN         ;Yes, run user program.

                STX L           ;$00->L.
                STX H           ; and H.
                STY YSAV        ;Save Y for comparison.
NEXTHEX         LDA IN,Y        ;Get character for hex test.
                EOR #%10110000  ;Map digits to $0-9.
                CMP #$0A        ;Digit?
                BCC DIG         ;Yes.
                ADC #$88        ;Map letter "A"-"F" to $FA-FF.
                CMP #$FA        ;Hex letter?
                BCC NOTHEX      ;No, character not hex.

DIG             ASL
                ASL             ;Hex digit to MSD of A.
                ASL
                ASL
                LDX #4          ;Shift count.
HEXSHIFT        ASL             ;Hex digit left, MSB to carry.
                ROL L           ;Rotate into LSD.
                ROL H           ;Rotate into MSD's.
                DEX             ;Done 4 shifts?
                BNE HEXSHIFT    ;No, loop.
                INY             ;Advance text index.
                BNE NEXTHEX     ;Always taken. Check next character for hex.



NOTHEX          CPY YSAV        ;Check if L, H empty (no hex digits).
                BEQ ESCAPE      ;Yes, generate ESC sequence.
                BIT MODE        ;Test MODE byte.
                BVC NOTSTOR     ;B6 = 0 for STOR, 1 for XAM and BLOCK XAM
                LDA L           ;LSD's of hex data.
                STA (STL,X)     ;Store at current 'store index'.
                INC STL         ;Increment store index.
                BNE NEXTITEM    ;Get next item. (no carry).
                INC STH         ;Add carry to 'store index' high order.

TONEXTITEM      JMP NEXTITEM    ;Get next command item.



RUN             JMP (XAML)      ;Run at current XAM index.



NOTSTOR         BMI XAMNEXT     ;B7 = 0 for XAM, 1 for BLOCK XAM.
                LDX #2          ;Byte count.

SETADR          LDA L-1,X       ;Copy hex data to
                STA STL-1,X     ; 'store index'.
                STA XAML-1,X    ;And to 'XAM index'.
                DEX             ;Next of 2 bytes.
                BNE SETADR      ;Loop unless X = 0.



NXTPRNT         BNE PRDATA      ;NE means no address to print.
                LDA #CR         ;CR.
                JSR ECHO        ;Output it.
                LDA XAMH        ;'Examine index' high-order byte.
                JSR PRBYTE      ;Output it in hex format.
                LDA XAML        ;Low-order 'examine index' byte.
                JSR PRBYTE      ;Output it in hex format.
                LDA #HICHAR(`:') ;":".
                JSR ECHO        ;Output it.

PRDATA          LDA #HICHAR(` ') ;Blank.
                JSR ECHO        ;Output it.
                LDA (XAML,X)    ;Get data byte at 'examine index'
                JSR PRBYTE      ;Output it in hex format.
XAMNEXT         STX MODE        ;0->MODE (XAM mode).
                LDA XAML
                CMP L           ;Comapre 'examine index' to hex data.
                LDA XAMH
                SBC H
                BCS TONEXTITEM  ;Not less, so no more data to output.

                INC XAML
                BNE MOD8CHK     ;Increment 'examine index'.
                INC XAMH
MOD8CHK         LDA XAML        ;Check low-order 'examine index' byte
                AND #%00000111  ; For MOD 8=0
                BPL NXTPRNT     ;Always taken.



PRBYTE          PHA             ;Save A for LSD.
                LSR
                LSR
                LSR             ;MSD to LSD position.
                LSR
                JSR PRHEX       ;Output hex digit.
                PLA             ;Restore A.

PRHEX           AND #%00001111  ;Make LSD for hex print.
                ORA #%10110000  ;Add "0".
                CMP #(HICHAR(`9'))+1 ;Digit?
                BCC ECHO        ;Yes, output it.
                ADC #$06        ;Add offset for letter.
                                ;(adds 7 because C is set)

ECHO            BIT DSP         ;DA bit (B7) cleared yet?
                BMI ECHO        ;No, wait for display.
                STA DSP         ;Output character. Sets DA.
                RTS             ;Return.



                ASM_RES(2)    ;unused



M6502VEC        ASM_ADDR(NMI)
                ASM_ADDR(RESET)
                ASM_ADDR(IRQ)
