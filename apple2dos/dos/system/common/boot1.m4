include(`asm.m4h')
include(`symbols.m4h')
                                ; =================================
                                ; IMAGE OF BOOT1 ($B600-$B6FF)
                                ; =================================

                                ; (for DOS 3.3 only:)
                                ; - STORED ON TRK0/SEC0.
                                ; - READ IN AT $800-$8FF BY DISK CONTROLLER ROM (BOOT0).
                                ; - EXECUTION BEGINS AT $0801 & USES THE CONTROLLERS READ-
                                ; SECTOR SUBROUTINE (BTRDSEC, $CS00, WHERE S = SLOT # OF
                                ; CARD) TO READ IN TRK0/SEC9 DOWN TO TRK0/SEC1 (THAT IS,
                                ; $BFFF ----> $B600).

BOOT1                           ; IMAGE OF SEC2RD08.  DENOTES # OF
                                ; SECS TO BE READ FROM TRK0 DURING
                                ; BOOT0.
                ifelse(eval(VERSION >= 330),1,`
                ASM_DATA(1)
                ')






                ifelse(eval(VERSION < 320),1,`


                ASM_RES($0C)
                                ; upon entry, A3 --> first byte of highest RAM page (e.g., $BF00)
L1B28SR
                LDA   A3H
                AND   #$DF      ; $BF becomes $9F
                STA   A4H
                STX   A4L       ; (A4) --> $9F00
                LDA   (A4L,X)
                PHA             ; save (A4) in case we need to restore it below
                STA   PROSCRTH
L1B35           TYA
                EOR   PROSCRTH
                STA   PROSCRTH
                TYA
                EOR   (A3L,X)
                STA   (A4L,X)
                CMP   PROSCRTH
                BNE   L1B4D
                INY
                BNE   L1B35
                                ; comes here if 9F page maps to BF page
                LDY   A4H       ; $9F
                PLA             ; (A4)
                RTS
L1B4D           PLA
                STA (A4L,X)
                LDY A3H
                RTS

                ASM_RES($98)

                ')



                ifelse(eval(VERSION < 330),1,`
                                ; track 0 sector 0 starts here
                                ; (note that 13-sector disks have different
                                ; nibble encoding for track 0 sector 0 than
                                ; the normal 5 & 3 encoding on the rest of
                                ; the disk
                                ; The old Disk ][ ROMs read this into
                                ; $300-$3FF and JMP $301
BOOT1LOAD       ASM_DATA($99)

DISK2READ       =     $C65D
BUF1            =     $0800
BUF2            =     BUF1+$0100
BOOT1RUN        =     $0300

L0301           LDA   $0800,Y
                ASL
                ASL
                ASL
                STA   $0800,Y
                INY
                BNE   L0301

                LDX   SLT16ZPG
                LDA   #$09
                STA   $27
                LDA   $03CC
                STA   $41
                STY   $40
                TXA
                LSR
                LSR
                LSR
                LSR
                ORA   #$C0
                STA   PTR2RDSC+1
                LDA   #<DISK2READ
                STA   PTR2RDSC

L0327           JSR   BOOT1RUN+L0343-BOOT1LOAD
                JSR   BOOT1RUN+NIB-BOOT1LOAD
                LDA   $3D
                EOR   $03FF
                BEQ   L033A
                INC   $41
                INC   $3D
                BNE   L0327

L033A           STA   PTR2RDSC
                LDA   $03CC
                STA   PTR2RDSC+1
                INC   PTR2RDSC+1

L0343           JMP   (PTR2RDSC)



NIB             LDX   #$32
                LDY   #$00
L034A           LDA   BUF1,X
                LSR
                LSR
                LSR
                STA   $3C
                LSR
                STA   $2A
                LSR
                ORA   BUF2,X
                STA   ($40),Y
                INY
                LDA   BUF1+$33,X
                LSR
                LSR
                LSR
                LSR
                ROL   $3C
                LSR
                ROL   $2A
                ORA   BUF2+$33,X
                STA   ($40),Y
                INY
                LDA   BUF1+$66,X
                LSR
                LSR
                LSR
                LSR
                ROL   $3C
                LSR
                ROL   $2A
                ORA   BUF2+$66,X
                STA   ($40),Y
                INY
                LDA   $2A
                AND   #$07
                ORA   BUF2+$99,X
                STA   ($40),Y
                INY
                LDA   $3C
                AND   #$07
                ORA   BUF2+$CC,X
                STA   ($40),Y
                INY
                DEX
                BPL   L034A
                LDA   BUF1+$99
                LSR
                LSR
                LSR
                ORA   BUF2+$FF
                STA   ($40),Y
                LDX   SLT16ZPG
                RTS

                ASM_RES($27,$FF)
                ASM_DATA($36)
                ASM_RES($30,$FF)
IMG8FD          ASM_RES(2,$FF)







                ',`



                                ; Disk ][ controller ROM (usually at $C600)
                                ; starts execution here after loading into $800



BT1EXCB6        LDA PT2BTBUF+1  ; IMAGE OF BT1EXC08. GET NEXT PAGE
                                ; TO BE READ IN.
                CMP #$09        ; IS IT PAGE 9 (IE. 1ST PAGE READ
                                ; BY BOOT1)?
                BNE SKPRELB6    ; NO - ALREADY USED BY BOOT1 TO
                                ; READ PAGE9, SO SKIP POINTER
                                ; INITIALIZATION GIVEN BELOW.

                                ; INITIALIZE THE POINTER (PTR2RDSC)
                                ; TO POINT AT BOOT0S READ SECTOR
                                ; SUBROUTINE (BTRDSEC, $CS5C;
                                ; WHERE S=SLOT#, NORMALLY $C65C).

                LDA SLT16ZPG    ; (A) = SLOT*16 FROM ZERO PAGE.
                LSR             ; DIVIDE BY 16.
                LSR
                LSR
                LSR
                ORA #$C0        ; MERGE WITH $C0 TO GET $CS, WHERE
                                ; S=SLOT#.
                STA PTR2RDSC+1  ; STORE HI BYTE OF CONTROLLERS
                                ; READ-SECTOR SUBROUTINE ADDR.
                LDA #<BTRDSEC ; GET LOW BYTE OF SUBRTN ADR.
                STA PTR2RDSC    ; (LOW BYTE IS A CONSTANT (#$5C) &
                                ; IS THERE4 NOT VARIABLE WITH SLOT
                                ; USED (AS IS HI BYTE).)


                                ; READ IN 9 SECTORS REPRESENTED BY
                                ; TRK0/SEC9 DOWN TO TRK0/SEC1 INTO
                                ; $BFFF TO $B600.  NOTE THAT THE
                                ; SECS ARE READ IN FROM HIGHER TO
                                ; LOWER MEMORY. THESE SECS CONTAIN
                                ; THE IMAGE OF BOOT1, PART OF THE
                                ; FILE MANAGER AND ALMOST ALL OF
                                ; RWTS & ITS ASSOCIATED ROUTINES.


                                ; CALCULATE THE TARGET ADDR FOR
                                ; THE FIRST SECTOR TO BE READ IN.

                CLC
                LDA BT1LDADR+1  ; CONTAINS $B6 ON 48K SLAVE.
                ADC BT1PG2RD    ; CONTAINS #09 ON 48K SLAVE.
                STA BT1LDADR+1  ; (A) = $BF ON 48K SLAVE.

                                ; DETERMINE # OF PAGES (SECS) LEFT
                                ; TO READ, PHYSICAL SECTOR# & TARGET
                                ; ADDRESS.  THEN, GO READ IN THE
                                ; NEXT SECTOR.

SKPRELB6        LDX BT1PG2RD    ; IMAGE OF SKRPREL08 ($81F).
                                ; (X)=PAGES LEFT TO READ (MINUS 1).
                                ; (ALSO DOUBLES AS LOGICAL SEC#.
                                ; VARIES FROM $09-->$FF).
                BMI PRP4B2B6    ; WHEN (X) = #$FF, THEN WE HAVE
                                ; READ ALL THE SECS IN, SO GO XIT.
                LDA PHYSECP8+65536-$2E00,X
                                ; EQUIVALENT TO "LDA $84D,X".
                                ; CONVERT LOGICAL SEC# TO PHYS SEC#
                STA BOOTSEC     ; STORE PHYSICAL SEC# IN PAGE0.
                DEC BT1PG2RD    ; REDUCE PAGES (SECS) LEFT TO READ
                                ; FOR NEXT TIME AROUND.
                LDA BT1LDADR+1  ; POINT BUFFER POINTER AT TARGET
                STA PT2BTBUF+1  ; ADR.  (VARIES FROM $BF TO $B6
                                ; ON 48K SLAVE).
                DEC BT1LDADR+1  ; REDUCE HI BYTE OF I/O BUF FOR
                                ; NEXT TIME AROUND.  (VARIES FROM
                                ; $BF TO $B5 ON 48K SLAVE.)
                LDX SLT16ZPG    ; SET (X) = SLOT*16.
                JMP (PTR2RDSC)  ; EQUIVALENT TO "JMP ($8FD)" OR
                                ; "JMP $CS5C" TO GO READ IN THE
                                ; NEXT SECTOR.
                                ; ************ NOTE *************
                                ; * GOES TO BT1EXC08 ($801) AFTER
                                ; * EACH SECTOR IS READ IN.
                                ; * (REMEMBER, BT1EXCB6 IS A
                                ; * CARBON COPY OF BT1EXC08.)
                                ; *******************************


                                ; PREPARE FOR BOOT2.

PRP4B2B6        INC BT1LDADR+1  ; IMAGE OF PR4B208 ($839).
                                ; POINT AT THE LOAD ADR FOR BOOT2.
                INC BT1LDADR+1  ; (AFTR INCS, = $B7 ON 48K SLAVE).

                                ; SET FULL SCREEN TEXT & DESIGNATE
                                ; KEYBOARD & SCREEN AS I/O DEVICES.

                JSR SETKBD      ; SIMULATE IN#0 (SET KSW: KEYIN).
                JSR SETVID      ; SIMULATE PR#0 (SET CSW: COUT1).
                JSR INIT        ; SIMULATE A "TEXT" STATEMENT.
                LDX SLT16ZPG    ; (X) = SLOT*16.

                                ; ------ GO TO BOOT2 ------

                JMP (BT1LDADR)  ; JMP TO BOOT2 ($B700 ON 48K SLAVE)


                                ; =================================
                                ; TABLE OF PHYSICAL SECTOR #S.
                                ; ($B64D - $B65C)
                                ; - AN IMAGE OF THIS TABLE IS
                                ; HOUSED AT $84D DURING THE BOOT.
                                ; =================================
PHYSECP8        ;.REPEAT $10, LOGSECTNUM
                ;ASM_DATA(($100) - 2*LOGSECTNUM - (LOGSECTNUM+6)/7) .MOD $10
                ;.ENDREP
define(`forloop', `pushdef(`$1', `$2')_forloop($@)popdef(`$1')')
define(`_forloop', `$4`'ifelse($1, `$3', `', `define(`$1', incr($1))$0($@)')')

forloop(`LOGSECTNUM',0,15,`
                ASM_DATA(eval((256 - (2*LOGSECTNUM) - ((LOGSECTNUM+6)/7)) % 16))
')













                                ; =================================
                                ; OTHER ERROR PATCH.
                                ; ($B65D - $B670)
                                ; - USED WHEN FM DRIVER GETS AN
                                ; ERROR THAT IS NOT AN OUT-OF-
                                ; DATA ERROR.
                                ; - ALSO CONTAINS THE APPEND FLAG.
                                ; =================================

APPNDFLG        ASM_DATA($00)       ; APPEND FLAG.

                                ; ERROR WAS DEADLY, SO BETTER
                                ; RELEASE THE FILES BUFFER AND
                                ; MAKE SURE THE APPEND FLAG IS OFF.

OTHRERR         JSR GETBUFF     ; LOCATE BUFFER BELONGING TO THE
                                ; FILE. (PUT ADR IN A3L/H PTR.)
                BCS TOERRMSG    ; NO BUFFER WAS ASSIGNED, SO NO
                                ; NEED TO RELEASE THE FILES BUF OR
                                ; TURN OFF THE APPEND FLAG.
                LDA #0          ; ZERO OUT THE APPEND FLAG & SET
                TAY             ; (Y) TO NDEX THE 1ST BYTE OF THE
                STA APPNDFLG    ; DOS NAME BUFFER.
                STA (A3L),Y     ; RELEASE FILES DOS BUFFER.
TOERRMSG        LDA RTNCODFM    ; GET ERROR CODE TO INDEX ERR MSG.
                JMP ERRHNDLR    ; EXIT & GO PRINT ERROR MESSAGE.


                                ; =================================
                                ; PATCH TO HANDLE APPEND.
                                ; ($B671 - $B685)
                                ; =================================

                                ; PREPARE TO MANUALLY BACK UP THE
                                ; FILE POINTER IF NECESSARY.  NEED
                                ; TO BACK IT UP ONE BYTE IF A $00
                                ; BYTE WAS ENCOUNTERED IN A DATA
                                ; SECTOR.  HOWEVER, IF A ZEROED-
                                ; OUT DATA PAIR (LISTED IN A T/S
                                ; LIST) OR A ZEROED-OUT T/S LINK
                                ; WAS ENCOUNTERED, THEN THE FILE
                                ; POINTER IS POSITIONED CORRECTLY.
                                ; (P.S. AT ONE TIME IN THE HISTORY
                                ; OF DOS, THE POSITION FUNCTION
                                ; WAS USED TO BACK UP THE POINTER.)
                                ; ENTER WITH APPEND FLAG SET IF A
                                ; $00 WAS DETECTED IN A (NON-EMPTY)
                                ; FILES T/S LIST.

CKAPFLG         LDA APPNDFLG    ; IS APPEND FLAG ON?
                BEQ CLRAPFLG    ; NO - FLAG IS OFF.
                INC RECNMBFM    ; YES -SO INCREMENT THE FILE MGRS
                BNE CLRAPFLG    ; VERSION OF THE RECORD # BECAUSE
                INC RECNMBFM+1  ; RECNMBFM IS POINTING AT THE LAST
                                ; VALID DATA BYTE AND WE WANT IT TO
                                ; POINT AT THE NEXT POTENTIAL
                                ; RECORD NUMBER.  (P.S.  REMEMBER
                                ; THAT RECNMBFM LAGS RECNMBWA BY
                                ; ONE BYTE.)
CLRAPFLG        LDA #0          ; DONT NEED THE APPEND FLAG ANY
                STA APPNDFLG    ; MORE, SO TURN IT OFF.

                ifelse(eval(VERSION == 330),1,`
                JMP BK2APND
                ',`
                ifelse(eval(VERSION == 331),1,`
                JMP CMPATCH
                ',`
                ifelse(eval(VERSION == 332),1,`
                JMP RSETPTRS
                ')
                ')
                ')

                                ; ==================================
                                ; PATCH TO VERIFY A RANGE-OF-BYTES.
                                ; (CALLED BY RWRANGE TO R/W RANGE
                                ; OF BYTES & THEN VERIFY THE DATA.)
                                ; ==================================

VRFYRWNG        STA SUBCODFM    ; PUT RANGE-OF-BYTES SUBCODE IN
                                ; RWTSS IOB.
                JSR FMDRIVER    ; CALL FM DRIVER TO READ/WRITE A
                                ; RANGE OF BYTES.
                JSR CMDCLOSE    ; CLOSE FILE & THEN VERIFY DATA.
                JMP CMDVERFY


                                ; ================================
                                ; APPEND PATCH.
                                ; ($B692 -$B6B2)
                                ; - USED TO HANDLE AN END-OF-DATA
                                ; ERROR.
                                ; ================================

                                ; THIS PATCH IS DIFFICULT TO UNDER-
                                ; STAND BECAUSE IT IS A PATCH THAT
                                ; HAS BEEN ADDED TO REPAIR PREVIOUS
                                ; PATCHES AND BECAUSE EXECUTION FAILS
                                ; ON OCCASSION.  AS A RESULT, THE APPEND
                                ; COMMAND HAS EVOLVED INTO A CLASSIC
                                ; CASE OF SPAGHETTI PROGRAMMING.
                                ; PARTS OF THE FOLLOWING ROUTINES
                                ; SEEM USELESS.  THEY MAY JUST BE
                                ; RESIDUAL (BUT INNOCUOUS) INSTRUCS
                                ; THAT ARE LEFT OVER FROM AN EARLIER
                                ; VERSION OF DOS.

                                ; CHECK IF FILE POINTER AND WASTEBYT
                                ; ARE ZEROES.

APNDPTCH        LDY #$13
CK4ZEROS        LDA (A4L),Y
                BNE SETAPFLG    ; UNLESS WE ARE DEALING WITH A
                                ; USELESS FILE THAT WAS PREVIOUSLY
                                ; OPENED. BUT NEVER CLOSED, THIS
                                ; INSTRUCTION IS ALWAYS TAKEN.
                INY
                CPY #$17
                BNE CK4ZEROS

                                ; THE PURPOSE OF THE FOLLOWING
                                ; INSTRUCTIONS IS NOT UNDERSTOOD.
                                ; THIS SECTION OF CODE MAY HAVE BEEN
                                ; DESIGNED TO DEAL WITH USELESS FILES
                                ; THAT WERE OPENED BUT NEVER CLOSED.
                                ; WHATEVER THE ORIGINAL PURPOSE WAS,
                                ; THE FOLLOWING CODE APPEARS TO KEEP
                                ; THE FILE POINTER AT #$000000 WHEN
                                ; AN EMPTY FILE (IE. FILE WITH NO
                                ; DATA) IS ENCOUNTERED.

                LDY #$19        ; COPY IMAGE OF RECNMBWA/+1 AND
COPYRECS        LDA (A4L),Y     ; BYTOFFWA/+1 THAT WERE JUST
                STA RECLENFM-$19,Y
                                ; STORED IN THE WORK BUFFER TO
                INY             ; RECNMBFM/+1 AND BYTOFFFM/+1 IN
                CPY #$1D        ; THE FM PARAMETER LIST.
                BNE COPYRECS

FMDVRTN         JMP BK2FMDRV

                                ; SET THE APPEND FLAG.
                                ; (NEVER ENTERED IF DEALING WITH
                                ; AN EMPTY FILE.)

SETAPFLG        LDX #$FF
                STX APPNDFLG    ; SET THE APPEND FLAG.
                BNE FMDVRTN     ; ALWAYS.


                ifelse(eval(VERSION == 332),1,`
                                ; =================================
                                ; YET ANOTHER APPEND PATCH.
                                ; ($B6B3 - $B6CE)
                                ; =================================

                                ; BUGGY ROUTINE USED TO BACK UP THE
                                ; FILE POINTER.  IF A FILE IS $FFFF
                                ; (#65536) BYTES LONG (OR SOME
                                ; MULTIPLE THEREOF), THE APPEND CMD
                                ; WILL FAIL BECAUSE THIS ROUTINE
                                ; NEGLECTS TO BACK UP THE HI BYTE
                                ; OF THE FILE POINTER (FILPTSEC+1).

RSETPTRS        LDA RECNMBFM    ; RECONCILE RECORD NUMBER VERSIONS
                STA FILPTBYT    ; AND LOWEST TWO BYTE OF THE FILE
                STA RECNMBWA    ; POINTER.
                LDA RECNMBFM+1
                STA WASTEBYT    ; APPEARS TO BE IRRELEVANT.
                                ; OBVIOUSLY HAS SOMETHING TO DO
                                ; WITH CK4ZEROS ROUTINE DESCRIBED
                                ; ABOVE.
                STA RECNMBWA+1
                STA FILPTSEC
                TSX             ; RESET THE STK POINTER SO WE CAN
                STX STKSAV      ; USE THE FMEXIT ROUTINE TO RETURN
                                ; TO THE CALLER OF THE APPEND CMD.
                                ; (NOTE: THIS WILL BE EXCEPTIONAL
                                ; EXIT ROUTE FOR FMEXIT.)
                JMP GOODFMXT    ; EXIT THE APPEND CMD VIA FMEXIT
                                ; ROUTINE. EXECUTION ACTUALLY RTNS
                                ; TO AFTRCMD ($A17D) LOCATED IN THE
                                ; CMD PARSING AND PROCESSING
                                ; ROUTINES.
                ',`
                ASM_RES(28)
                ')


                                ; =================================
                                ; FREE SPACE
                                ; =================================

                ASM_RES(1)            ; ($B6CF)


                ifelse(eval(VERSION == 330),1,`
                                ; =================================
                                ; STRAY CODE (NOT USED).
                                ; ($B6D0-$B6E7.)
                                ; =================================

                                ; - DO A HOME & THEN PRINT "B01-00"
                                ; - CONSIDER TO BE FREE SPACE.

                JSR HOME
                LDA #HICHAR(`B')
                JSR COUT
                LDA #$01
                JSR PRBYTE
                LDA #HICHAR(`-')
                JSR COUT
                LDA #$00
                JSR PRBYTE
                RTS
                ',`
                ASM_RES(24)
                ')


                                ; =================================
                                ; FREE SPACE
                                ; =================================

                ASM_RES(21)           ; ($B6E8-$B6FC.)

                ')
















                                ; =================================
                                ; STORAGE FOR BOOT1.
                                ; ($B6FD - $B6FF)
                                ; (IMAGE MOVED TO $8FD-$8FF.)
                                ; =================================


                ifelse(eval(VERSION >= 330),1,`
IMG8FD          ASM_ADDR(BOOT1) ; VARIES FROM $B600-$BF00 ON 48K
                                ; SLAVE.  (EVENTUALLY POINTS TO THE
                                ; START OF BOOT2 AT $B700.  IMAGE
                                ; OF BT1LDADR AT $8FD.)
                ')



BT1LDADR        = IMG8FD-(BOOT1-$0800)

IMG8FF
BT1PG2RD        = IMG8FF-(BOOT1-$800)
                ASM_DATA($09)       ; ASM_RES(1) ;CONTAINS # OF PAGES TO READ WHEN
                                ; EXECUTING BOOT1.  ALSO DOUBLES AS
                                ; LOGICAL SEC#.  INITIALLY = $01.
                                ; VARIES FROM:$09 --> $00 --> $FF.
                                ; (IMAGE OF BT1PG2RD AT $8FF.)
