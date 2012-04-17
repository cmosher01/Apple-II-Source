include(`asm.m4h')
include(`symbols.m4h')
                                ; =========================================
                                ; BOOT2 ($B700 - $B749).
                                ; =========================================

                                ; - READS IN THE REST OF DOS STARING AT
                                ; TRK02/SEC04 DOWN TO TRK00/SEC0A
                                ; ($B5FF --> $9B00).  (SECTORS 0A & 0B
                                ; OF TRK 0 ($9CFF - $9B00) ARE
                                ; EMPTY.)
                                ; - AFTER THE REST OF DOS IS READ IN,
                                ; EXECUTION JUMPS TO DOSS COLDSTART
                                ; ROUTINE (DOSCLD, $9D84).
                                ; - NOTE THAT ON ENTRY:  (X) = SLOT * 16.
                                ; :::::::::::::::::::::::::::::::::::::::::

                                ; PREPARE RWTSS INPUT-OUTOUT BLOCK
                                ; (IOB) AND DESIGNATE THE NUMBER OF
                                ; SECTORS TO READ.

BOOT2           STX IBSLOT      ; (X) = SLOT*16 WANTED.
                STX IOBPSN      ; LAST-USED SLOT*16.
                LDA #1          ; DESIGNATE BOTH LAST-USED & WNTD
                STA IOBPDN      ; DRIVES TO BE DRIVE#1.
                STA IBDRVN
                LDA NMPG2RD     ; SET NUMBER OF PAGES TO READ.
                STA BT2PGCTR    ; COUNTER FOR NUMBER OF PGS TO READ.

                ifelse(eval(VERSION < 330),1,`
                LDA #0
                STA IBTRK
                LDA BT1RSTSC
                STA IBSECT
                LDA BT1STPAG
                STA IBBUFP+1
                ',`
                LDA #2          ; START WITH TRK$02/SEC$04.
                STA IBTRK       ; TRACK.
                LDA #4
                STA IBSECT      ; SECTOR.
                LDY BTSTAGE+1   ; (Y)=HI BYTE OF ADDR OF IMAGE OF
                                ; BOOT1 (#$B6 ON 48K SLAVE).
                DEY             ; DEFINE I/O BUF AS 1 PAGE BELOW
                STY IBBUFP+1    ; BOOT1.
                ')

                LDA #1          ; OPCODE FOR READ.
                STA IBCMD

                                ; CONVERT FROM (X) = SLOT*16
                                ; TO (X) = SLOT.

                TXA             ; (X) = SLOT * 16.
                LSR             ; DIVIDE BY 16.
                LSR
                LSR
                LSR
                TAX             ; (X) = SLOT.

                                ; INITIALIZE PAGE-4 LOCATIONS WITH
                                ; TRACK #S ASSOC WITH THE DRIVES.

                LDA #0
                STA TRK4DRV2,X
                STA TRK4DRV1,X

                                ; CALL ROUTINE TO READ IN THE REST OF DOS.

                JSR RWPAGES     ; GO READ A GROUP OF PAGES (SECS).
                LDX #$FF        ; COMPLETELY CLEAR OUT THE STACK.
                TXS
                STX IBVOL       ; SET VOL TO $FF (COMPLEMENT OF 0)
                ifelse(eval(VERSION < 330),1,`
                JSR SETVID
                ',`
                JMP CLOBCARD    ; GO CLOBBER THE LANGUAGE CARD AND
                                ; SET VIDEO OUTPUT (SIMUL8 PR#0).
                                ; (RETURNS TO NEXT INSTRUC BELOW.)
                ')

BK2BOOT2        JSR SETKBD      ; SIMULATE "IN#0" STATEMENT.
                                ; (THAT IS, SELECT KEYBOARD.)
TODOSCLD2       JMP DOSRELOC    ; JUMP INTO DOSS COLDSTART ROUTINE
                                ; (AT $9DE4) TO BUILD THE DOS BUFS
                                ; AND THE PAGE-3 VECTOR TABLE AND
                                ; THEN RUN THE "HELLO" PROGRAM.
                                ; ************* NOTE ************
                                ; * THIS INSTRUC IS A HACKERS
                                ; * DREAM.  FOR INSTANCE, YOU CAN
                                ; * CHANGE THE JUMP TO POINT TO
                                ; * YOUR OWN PASSWORD OR TIME-
                                ; * BOMB PROGRAM THAT YOU HAVE
                                ; * DEVIOUSLY IMBEDDED IN AN
                                ; * UNUSED SECTION OF DOS.
                                ; *******************************
                                ; Mosher: note: on a master disk, it
                                ; jumps to the DOS relocation routine;
                                ; on a slave disk, it just jumps to
                                ; the coldstart as indicated above.


                                ; ================================
                                ; WRITE DOS IMAGE ON TRKS 0 - 2.
                                ; (USED BY INIT FUNCTION.)
                                ;
                                ; WRITE TRK02/SEC04 ($B5FF) DOWN
                                ; TO TRK00/SEC0C ($9D00).
                                ; ================================

WRDOSIMG
                ifelse(eval(VERSION < 330),1,`
                LDA   IBBUFP+1
                STA   BT1STPAG
                SEC
                LDA   BTSTAGE+1
                SBC   BT1STPAG
                STA   NMPG2RD
                LDA   #$00
                STA   IBTRK
                STA   IBSECT
                STA   IBBUFP
                LDA   BTSTAGE+1
                STA   IBBUFP+1
                STA   IMG8FD+1
                LDA   #$0A
                STA   BT2PGCTR
                STA   BT1RSTSC
                LDA   #$48
                STA   IMG8FF
                LDA   #$02
                STA   IBCMD
                JSR   RWPAGES
                LDA   BT1STPAG
                STA   IBBUFP+1
                LDA   NMPG2RD
                STA   BT2PGCTR
                JSR   RWPAGES
                RTS

RWPAGES         LDA   ADROFIOB+1
                LDY   ADROFIOB
                ifelse(eval(VERSION < 320),1,`
                JSR   RWTS
                ',`
                JSR   ENTERWTS
                ')
                LDY   IBSECT
                INY
                CPY   #$0D
                BNE   L37A9
                LDY   #$00
                INC   IBTRK
L37A9           STY   IBSECT
                INC   IBBUFP+1
                DEC   BT2PGCTR
                BNE   RWPAGES
                RTS
                ')
                ifelse(eval(VERSION < 320),1,`
VERFY           LDA   #$0C
                JSR   HNDLCMD1
                LDA   #$06
                CMP   RTNCODFM
                BNE   TOCLOSE
                JMP   TONOTFND
TOCLOSE         JMP   CMDCLOSE
                ASM_RES(24)
                ')





                ifelse(eval(VERSION >= 320),1,`

                ifelse(eval(VERSION > 321),1,`
                LDA BTSTAGE+1   ; CALC # OF PAGES TO WRITE:
                SEC             ; (#$B6 - #$9D = #$19 OR #25.)
                SBC IBBUFP+1
                STA BT2PGCTR    ; SET COUNTER FOR 25 PAGES.
                LDA BTSTAGE+1
                STA IBBUFP+1
                DEC IBBUFP+1    ; START WITH PAGE #$B5.
                LDA #2          ; START WITH TRK02/SEC04.
                STA IBTRK
                LDA #4
                STA IBSECT
                LDA #2          ; SET WRITE COMMAND.
                STA IBCMD
                JSR RWPAGES     ; WRT TRK02/SEC04 TO TRK00/SEC00.

                                ; WRITE TRK00/SEC09 ($BFFF) DOWN
                                ; TO TRK00/SEC00 ($9D00).

                LDA BTSTAGE+1   ; STORE HI BYTE OF ADRRESS OF THE
                STA IMG8FD+1    ; START OF BOOT1 (#$B6).
                CLC             ; CALC HI BYTE ADR OF TRK00/SEC09
                ADC #9          ; (#$B6 + #$09 = #$BF).
                STA IBBUFP+1    ; SET BUF TO SEND INFO TO PAGE #$BF.
                LDA #10         ; DESIGNATE 10 PAGES TO WRITE.
                STA BT2PGCTR    ; (#$BFFF - #$B600).
                SEC
                SBC #1
                STA IMG8FF      ; SIGNIFY THAT THERE ARE 9 PAGES TO
                                ; BE READ WHEN BOOT1 IS EXECUTED.
                STA IBSECT      ; START WRITING WITH TRK00/SEC09.
                JSR RWPAGES     ; WRITE TRK00/SEC09 TO TRK00/SEC00.
                RTS

                ASM_RES(6)            ; UNUSED.

                                ; =================================
                                ; READ/WRITE A GROUP OF PAGES.
                                ; =================================

RWPAGES         LDA ADROFIOB+1  ; INIT (A)/(Y) WITH HI/LOW BYTS OF
                LDY ADROFIOB    ; ADR OF RWTSS IOB FOR ENTRY TO RWTS.
                JSR ENTERWTS    ; ENTER INTO RWTS TO READ/WRITE SEC.
                LDY IBSECT      ; GET # OF SEC JUST READ OR WRITTEN
                DEY             ; VAL FOR NEXT SEC TO READ/WRITE (WHEN
                                ; EXECUTING BOOT1, #$09 --> #$FF).
                BPL SAMETRK     ; BRANCH TO USE THE SAME TRACK.

                                ; START A NEW TRACK.

                LDY #$0F        ; START WITH SEC 15.
                NOP
                NOP
                DEC IBTRK       ; REDUCE TRK# WANTED.

                                ; ADJUST POINTER TO IOB & TEST IF THERE
                                ; ARE ANY MORE SECTORS TO READ/WRITE.

SAMETRK         STY IBSECT      ; STORE NUMBER OF SEC WANTED.
                DEC IBBUFP+1    ; REDUCE BUFFERS PAGE ADR.
                DEC BT2PGCTR    ; REDUCE COUNTER FOR # OF SECS TO READ.
                BNE RWPAGES     ; MORE SECS TO READ.
                RTS

                ')



                                ; ===================================
                                ; ROUTE EXECUTION TO RWTS.
                                ; ===================================

                                ; NORMAL ROUTE TO ENTER RWTS FROM
                                ; CUSTOM ASSEMBLY LANGUAGE PRGMS.
                                ; ON ENTRY - THE IOB AND DCT TBLS
                                ; MUST ALREADY BE SET UP.
                                ; - (Y) AND (A) = LOW AND
                                ; HI BYTES (RESPECTIVELY)
                                ; OF ADDR OF IOB TABLE.
                                ; (P.S.  NOTE THAT THE $3D9 VECTOR
                                ; JUMPS TO HERE!!!)

ENTERWTS        PHP             ; SAVE STATUS ON STK.
                                ; (C)=0 IF SEEKING OR READING.
                                ; (C)=1 IF WRITING OR FORMATTING.
                SEI             ; SET INTERRUPT DISABLE FLAG TO
                                ; PREVENT ANY FURTHER MASKABLE
                                ; INTERRUPTS WHEN DOING REAL-TIME
                                ; PROGRAMMING.
                JSR RWTS        ; ENTER RWTS PROPER TO DO OPERATION:
                                ; $00=SEEK, $01=READ,
                                ; $02=WRITE, $03=FORMAT.
                BCS ERRENTER    ; OPERATION WAS NOT SUCCESSFUL.
                                ; NVR TAKEN IF JUST SEEKING BECAUSE
                                ; NO ERROR SIGNALLING ROUTINES ARE
                                ; ASSOCIATED WITH THAT OPERATION.
                PLP             ; THROW STATUS OFF STACK.
                CLC             ; SIGNAL SUCCESSFUL.
                RTS

ERRENTER        PLP             ; THROW STATUS OFF STACK.
                SEC             ; SIGNAL UNSUCCESSFUL.
                RTS


                                ; =================================
                                ; SET UP RWTSS IOB TO WRITE DOS.
                                ; =================================
PRPWRDOS        LDA FIRDOSPG+1  ; DESIGNATE START OF DOS AS ADR OF
                STA IBBUFP+1    ; I/O BUF IN RWTSS IOB.
                LDA #0
                STA IBBUFP
                LDA VOLWA       ; COMPLEMENT VOL#.
                EOR #$FF
                STA IBVOL
                RTS


                                ; =================================
                                ; ZERO OUT THE CURRENT (256-BYTE)
                                ; BUFFER.
                                ; =================================
ZCURBUF         LDA #0
                TAY
ZCURBUF1        STA (A4L),Y
                INY
                BNE ZCURBUF1
                RTS




                ')







                                ; =================================
                                ; PARAMETERS FOR BOOT2.
                                ; ($B7DF - $B7E7)
                                ; =================================

                ASM_RES(1)          ; UNUSED ($B7DF).
NMPG2RD         ASM_DATA($1B)       ; # OF PAGES TO READ (#27).
BT2PGCTR        ASM_RES(1)          ; # OF PAGES LEFT TO READ (VARIES).
BT1RSTSC        ASM_DATA($0A)       ; FIRST SEC # IN STAGE (#10).
BT1STPAG        ASM_DATA($1B) ;(>MASTERDOS) ; BASE PAGE TO READ INTO?

ADROFIOB        ASM_ADDR(IBTYPE)    ; ADR OF RWTSS IOB (NORM $B7E8).

BTSTAGE                         ; ADR OF START OF IMAGE OF BOOT1.
                ifelse(eval(VERSION < 320),1,`
                ASM_ADDR(BOOT2-$100)
                ',`
                ASM_ADDR(BOOT1)
                ')



                                ; =================================
                                ; RWTSS INPUT/OUTPUT BLOCK (IOB).
                                ; ($B7E8 - $B7FA)
                                ; =================================

IBTYPE          ASM_DATA($01)       ; TABLE TYPE (SHOULD BE $01).
IBSLOT          ASM_RES(1)          ; SLOT WANTED * 16.
IBDRVN          ASM_RES(1)          ; DRIVE WANTED ($01 OR $02).
IBVOL           ASM_RES(1)          ; VOL WANTED ($00 GOOD FOR ALL).
IBTRK           ASM_RES(1)          ; TRK WANTED.
IBSECT          ASM_RES(1)          ; LOGICAL SEC WANTED.
IBDCTP          ASM_ADDR(DEVTPC)    ; PTS TO DEVICE CHARACTERISTIC TBL.
                                ; (NORMALLY, $B7FB).
IBBUFP          ASM_RES(2)          ; PTS TO RWTSS I/O BUFFER.
IBSECSZ         ASM_ADDR($100)      ; SEC SIZ IN BYTS (LOW/HI FORMAT). ; mosher ??? ASM_RES(2) ???
IBCMD           ASM_RES(1)          ; RWTS COMMAND CODE:
                                ; $00=SEEK, $01=READ,
                                ; $02=WRITE, $04=FORMAT.
IBSTAT          ASM_RES(1)          ; ERROR CODE:
                                ; $00=NO ERRS, $10=WRIT PROT,
                                ; $20=VOL MISMTCH, $40=I/O(DRV)ERR
                                ; $80=READ ERR.
IBSMOD          ASM_RES(1)          ; VOLUME FOUND.
IOBPSN          ASM_RES(1)          ; SLOT*16 OF LAST ACCESS (FOUND).
IOBPDN          ASM_RES(1)          ; DRIVE # OF LAST ACCESS (FOUND).
                ASM_RES(2)          ; UNUSED ($B7F9-$B7FA).


                                ; =================================
                                ; DEVICE CHARACTERISTIC TABLE.
                                ; ($B7FB - $B7FF)
                                ; =================================

DEVTPC          ASM_DATA($00)       ; DEVICE TYPE.
PPTC            ASM_DATA($01)       ; PHASES/TRK. (EVEN VAL = 1PHASE,
                                ; ODD VAL = 2PHASE.)  THEREFORE,
                                ; $01=2 PHASE (WHICH TRANSLATES TO
                                ; TO ARM MOVEMENTS PER TRACK).
MONTC           ASM_DATA($EF,$D8)   ; MOTOR-ON-TIME COUNT.
                ASM_RES(1)          ; UNUSED ($B7FF).
