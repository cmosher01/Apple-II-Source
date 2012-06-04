include(`asm.m4h')
include(`symbols.m4h')
                                ; ================================
                                ; PRENIBBLE DATA ($B800-$B829).
                                ; CONVERT 256 MEMORY BYTES TO 342
                                ; SIX-BIT NIBBLES AS SHOWN BELOW.
                                ; ================================

                                ; ON ENTRY:  PTR2BUF PTS AT DATA BUF.
                                ; ON EXIT:  (A) = ?.
                                ; (X) = #$FF.
                                ; (Y) = #$FF.
                                ; (C) = 1.

                                ; RWTSBUF1
                                ; BB00: 0 0 00-7 00-6 00-5 00-4 00-3 00-2
                                ; BB01: 0 0 01-7 01-6 01-5 01-4 01-3 01-2
                                ; BB02: 0 0 02-7 02-6 02-5 02-4 02-3 02-2
                                ; .
                                ; .
                                ; .
                                ; BBFF: 0 0 FF-7 FF-6 FF-5 FF-4 FF-3 FF-2
                                ;
                                ; RWTSBUF2
                                ; BC00: 0 0 01-0 01-1 AB-0 AB-1 55-0 55-1
                                ; BC01: 0 0 00-0 00-1 AA-0 AA-1 54-0 54-1
                                ; BC02: 0 0 FF-0 FF-1 A9-0 A9-1 53-0 53-1
                                ; .
                                ; .
                                ; .
                                ; BC54: 0 0 AD-0 AD-1 57-0 57-1 01-0 01-1
                                ; BC55: 0 0 AC-0 AC-1 56-0 56-1 00-0 00-1

                                ; WHERE "AC-0" = BIT0 OF MEMORY BYTE
                                ; WHICH IS OFFSET
                                ; #$AC BYTES INTO
                                ; THE DATA SECTOR.
                                ; THE FOLLOWING BITS ARE DUPLICATED
                                ; IN $BC00-$BC01 & $BC54-$BC55 BUT
                                ; ARE IGNORED IN $BC00-$BC01:
                                ; 01-0,01-1,00-0,00-1.

PRENIBL
                ifelse(eval(VERSION < 330),1,`
                                ; Note: DOS 3.2 and DOS 3.1 use "5 and 3" encoding,
                                ; instead of the "6 and 2" encoding used by DOS 3.3.
                LDX   #$32
                LDY   #$00
PRENIB1         LDA   (PTR2BUF),Y
                STA   PT2BTBUF
                LSR   
                LSR   
                LSR   
                STA   RWTSBUF1,X
                INY
                LDA   (PTR2BUF),Y
                STA   HOLDPRES
                LSR   
                LSR   
                LSR   
                STA   RWTSBUF1+$33,X
                INY
                LDA   (PTR2BUF),Y
                STA   DESTRK
                LSR   
                LSR   
                LSR   
                STA   RWTSBUF1+$66,X
                INY
                LDA   (PTR2BUF),Y
                LSR   
                ROL   DESTRK
                LSR   
                ROL   HOLDPRES
                LSR   
                ROL   PT2BTBUF
                STA   RWTSBUF1+$99,X
                INY
                LDA   (PTR2BUF),Y
                LSR   
                ROL   DESTRK
                LSR   
                ROL   HOLDPRES
                LSR   
                STA   RWTSBUF1+$CC,X
                LDA   PT2BTBUF
                ROL
                AND   #%00011111
                STA   RWTSBUF2,X
                LDA   HOLDPRES
                AND   #%00011111
                STA   RWTSBUF2+$33,X
                LDA   DESTRK
                AND   #%00011111
                STA   RWTSBUF2+$66,X
                INY
                DEX
                BPL   PRENIB1
                LDA   (PTR2BUF),Y
                TAX
                AND   #$07
                STA   RWTSBUF2+$99
                TXA
                LSR   
                LSR   
                LSR   
                STA   RWTSBUF1+$FF
                ',`
                LDX #0
                LDY #2
PRENIB1         DEY
                LDA (PTR2BUF),Y ; GET BYTE FROM "DATA" BUFFER.
                LSR             ; PUT ITS LWR 2 BITS INTO RWTSBUF1.
                ROL RWTSBUF2,X
                LSR
                ROL RWTSBUF2,X
                STA RWTSBUF1,Y  ; PUT REMAINING 6 BITS N RWTSBUF1.
                INX
                CPX #$56
                BCC PRENIB1
                LDX #0
                TYA
                BNE PRENIB1     ; REPEAT UNTIL @ BYTE OF RWTSBUF2
                                ; HAS 6 BITS.
                LDX #$55        ; MAKE SURE BITS 6 & 7 OF RWTSBUF2
PRENIB2         LDA RWTSBUF2,X  ; ARE ZEROES.
                AND #%00111111
                STA RWTSBUF2,X
                DEX
                BPL PRENIB2
                ')

                RTS


                                ; ====================================
                                ; WRITE SECTOR TO DISK ($B82A-$B8B7).
                                ; ====================================

                                ; ON ENTRY: (X) = SLOT*16
                                ; ON EXIT:  (C) = 1 = WRIT PROT ERR.
                                ; IF NO ERROR: (C) = 0
                                ; (A) = ?
                                ; (X) = SLOT*16
                                ; (Y) = #$00


WRITESEC        SEC             ; (C)=1, ASSUME WRITE PROTECTED
                                ; ERROR AS DEFAULT CONDITION.
                ifelse(eval(VERSION >= 330),1,`
                STX FRMTSLOT    ; SAVE SLOT*16 IN PAGES 0 & 6.
                STX SLOTPG6
                ')
                LDA Q6H,X       ; CHK IF DISK IS WRITE PROTECTED.
                LDA Q7L,X
                BMI PROTECTD    ; BRANCH IF WRITE PROTECTED.
                ifelse(eval(VERSION < 330),1,`
                STX FRMTSLOT    ; SAVE SLOT*16 IN PAGES 0 & 6.
                STX SLOTPG6
                ')
                LDA RWTSBUF2    ; GET 1ST 2-ENCODED BYTE AND SAVE
                STA HOLDNIBL    ; IT FOR LATER USE.

                                ; WRITE 5-SYNC GAP BETWEEN ADDRESS
                                ; EPILOGUE & DATA PROLOGUE.

                LDA #$FF        ; (A) = SYNC BYTE.
                STA Q7H,X       ; WRITE 1 SYNC BYTE.
                ORA Q6L,X
                PHA             ; (3 CYC)
                PLA             ; (4 CYC)
                NOP             ; (2 CYC)
                ifelse(eval(VERSION < 330),1,`
                LDY #10
                ',`
                LDY #4          ; WRITE 4 MORE SYNCS (2 CYC).
                ')
WRITE4FF
                ifelse(eval(VERSION < 330),1,`
                ORA HOLDNIBL
                ',`
                PHA             ; (3 CYC)
                PLA             ; (4 CYC)
                ')
                JSR WRITE2      ; (12 CYC BEFORE, 6 AFTER.)
                DEY             ; (2 CYC)
                BNE WRITE4FF    ; (2 OR 3 CYC)

                                ; WRITE DATA PROLOGUE ("D5 AA AD").

                LDA #$D5        ; (2 CYC)
                JSR WRITE1      ; (14 CYCS BEFORE, 6 AFTER.)
                LDA #$AA        ; (2 CYC)
                JSR WRITE1      ; (14 CYCS BEFORE, 6 AFTER.)
                LDA #$AD        ; (2 CYC)
                JSR WRITE1      ; (14 CYCS BEFORE, 6 AFTER.)

                                ; CONVERT & WRITE CONTENTS OF RWTS
                                ; BUFFERS TO DISK.  (WHEN FORMATTING,
                                ; THESE BUFS ARE ZEROED OUT.  THE
                                ; "$00" BYTES IN BUF ARE LATER TRANS-
                                ; LATED TO "$96" BYTES ON THE DISK.)

                                ; CONVERT & WRITE 2-ENCODED
                                ; NIBBLES FROM RWTSBUF2.
                                ; (EOR TO CALC (X) & THEN USE (X)
                                ; AS INDEX TO TBL OF DISK BYTES.)
                                ;
                                ; #0 EOR $BC55 = (X)
                                ; $BC55 EOR $BC54 = (X)
                                ; $BC54 EOR $BC53 = (X)
                                ; .   .     .     .
                                ; .   .     .     .
                                ; .   .     .     .
                                ; $BC01 EOR $BC00 = (X)

                TYA             ; (A) = 0.
                ifelse(eval(VERSION < 330),1,`
                LDY #$9A
                ',`
                LDY #$56        ; (DEC #86.)
                ')
                BNE DOEOR       ; ALWAYS.
GETNIBL         LDA RWTSBUF2,Y
DOEOR           EOR RWTSBUF2-1,Y
                TAX             ; INDEX TO DISK BYTE.
                LDA DSKNBTBL,X  ; GET DISK BYTE.
                LDX FRMTSLOT    ; (X) = SLOT*16.
                STA Q6H,X       ; WRITE BYTE.
                LDA Q6L,X
                DEY             ; (Y) = $56 --> #$00.
                BNE GETNIBL     ; (WRITE $56 OR DEC #86 BYTES.)

                                ; CONVERT & WRITE 6-ENCODED
                                ; NIBBLES FROM RWTSBUF1.
                                ;
                                ; $BC00 EOR $BB00 = (X)
                                ; $BB00 EOR $BB01 = (X)
                                ; $BB01 EOR $BB02 = (X)
                                ; .    .    .      .
                                ; .    .    .      .
                                ; .    .    .      .
                                ; $BBFE EOR $BBFF = (X)

                LDA HOLDNIBL    ; NORMALLY = CONTENTS OF $BC00.
                NOP
SCNDEOR         EOR RWTSBUF1,Y
                TAX             ; INDEX TO DISK BYTE.
                LDA DSKNBTBL,X  ; GET DISK BYTE TO WRITE.
                LDX SLOTPG6     ; (X) = SLOT*16.
                STA Q6H,X       ; WRITE 87TH ---> 341ST BYTES.
                LDA Q6L,X
                LDA RWTSBUF1,Y
                INY             ; (Y) = #$00 ---> #$FF.
                BNE SCNDEOR

                                ; CONVERT & WRITE DATA CHECKSUM.
                                ; (342ND BYTE, $BBFF ------> (X).)

                TAX             ; INDEX TO TABLE OF DISK BYTES.
                LDA DSKNBTBL,X  ; GET DISK BYTE TO WRITE.
                LDX FRMTSLOT    ; (X) = SLOT*16.
                JSR WRITE3      ; (5 CYCS BEFORE, 6 AFTER.)

                                ; WRITE DATA EPILOGUE ("DE AA EB").

                LDA #$DE        ; (2 CYC)
                JSR WRITE1      ; (14 CYCS BEFORE, 6 AFTER.)
                LDA #$AA        ; (2 CYC)
                JSR WRITE1      ; (14 CYCS BEFORE, 6 AFTER.)
                LDA #$EB        ; (2 CYC)
                JSR WRITE1      ; (14 CYCS BEFORE, 6 AFTER.)

                                ; WRITE A SYNC BYTE.
                ifelse(eval(VERSION >= 330),1,`
                LDA #$FF        ; (2 CYC)
                JSR WRITE1      ; (14 CYCS BEFORE, 6 AFTER.)
                ')
                LDA Q7L,X       ; SET READ MODE.
PROTECTD        LDA Q6L,X
                RTS


                                ; ====================================
                                ; WRITE BYTE WITH VARIOUS DELAYS.
                                ; (DELAYS PRIOR TO ENTRY & THOSE
                                ; SHOWN BELOW RESULT IN WRITING BYTES
                                ; EVERY 32 MACHINE CYCLES.)
                                ; ====================================

WRITE1          CLC             ; (2 CYC)
WRITE2          PHA             ; (3 CYC)
                PLA             ; (4 CYC)
WRITE3          STA Q6H,X       ; (5 CYC) - SHIFT REGISTER)
                ORA Q6L,X       ; (4 CYC - STROBE LATCH)
                RTS             ; (6 CYC)


                                ; =================================
                                ; POSTNIBBLE DATA ($B8C2 - $B8DB).
                                ; CONVERT 6- & 2-ENCODED BYTES IN
                                ; RWTSS TWO BUFFERS TO NORMAL
                                ; MEMORY BYTES (USUALLY PLACED IN
                                ; DOS DATA SECTOR BUFFER).
                                ; =================================

                                ; ON ENTRY:  (X) = SLOT*16.
                                ; PTR2BUF = PTS TO DATA BUF.
                                ; ON EXIT:  (A) = ?
                                ; (X) = ?
                                ; (Y) = BYTE COUNT USED
                                ; FOR RWTBUF2.
                                ; (C) = 1.


                ifelse(eval(VERSION >= 330),1,`
POSTNB16        LDY #0
POSTNIB1        LDX #$56        ; (DEC #86.)
POSTNIB2        DEX
                BMI POSTNIB1
                LDA RWTSBUF1,Y  ; SET (A) = 6-ENCODED BYTE.
                LSR RWTSBUF2,X  ; PUT LWR 2 BITS OF 2-ENCODED BYTE
                ROL             ; INTO ORIGNAL 6-ENCODED BYTE TO
                LSR RWTSBUF2,X  ; CREATE A NORMAL MEMORY BYTE.
                ROL  
                STA (PTR2BUF),Y ; PUT NORMAL MEMORY BYTE IN RWTSS
                INY             ; BUF (NORMALLY DOS DATA SEC BUF).
                CPY PROSCRTH
                BNE POSTNIB2
                RTS
                ')


                                ; ===================================
                                ; READ DATA SECTOR INTO RWTSS BUFS.
                                ; ===================================

                                ; CONDITIONS FOR $B8DC - $B943:
                                ; ON ENTRY:  (X) = SLOT*16
                                ; ON EXIT:  (C) = 1 IF ERROR
                                ; IF NO ERR: (C) = 0.
                                ; (A) = #$AA.
                                ; (X) = SLOT*16.
                                ; (Y) = #$00.

                                ; FIND DATA PROLOGUE ("D5 AA AD").

READATA         LDY #32         ; SET (Y) = 32 ATTEMPTS TO FIND
REDUCEY         DEY             ; THE DATA PROLOGUE.
                BEQ ERRTN       ; ERROR - CANT FIND DAT PROLOGUE.
PRODATD5        LDA Q6L,X       ; GET BYTE FROM DATA PROLOGUE.
                BPL PRODATD5    ; WAIT FOR FULL BYTE.
VERSD5          EOR #$D5        ; CHK IF BYTE WAS "D5".
                BNE REDUCEY     ; WASNT "D5", REDUCE COUNTER.
                NOP             ; STALL 2 CYCLES.
PRODATAA        LDA Q6L,X       ; READ NEXT DATA PROLOGUE BYTE.
                BPL PRODATAA    ; WAIT FOR FULL BYTE.
                CMP #$AA        ; WAS IT AN "AA"?
                BNE VERSD5      ; NO - GO RESTART SEQUENCE.
                ifelse(eval(VERSION < 330),1,`
                LDY #$9A
                ',`
                LDY #$56        ; SET (Y) FOR LATER USE IN READ
                                ; DATA ROUTINE.
                ')
PRODATAD        LDA Q6L,X       ; READ NEXT BYTE IN DATA PROLOGUE.
                BPL PRODATAD    ; WAIT FOR FULL BYTE.
                CMP #$AD        ; WAS IT AN "AD"?
                BNE VERSD5      ; NO - GO RESTART SEQUENCE.

                                ; READ 1ST 86 BYTES OF DATA INTO
                                ; RWTSBUF2 ($BC55 --> $BC00).
                                ;
                                ; USE DISK BYTE AS INDEX TO THE
                                ; NDX2NIBL TABLE WHICH CONTAINS
                                ; OFFSETS THAT WE WOULD BE USING
                                ; IF WE WERE ACCESSING A TABLE
                                ; OF DISK BYTES WHEN WRITING.
                                ; (IE. WE ARE JUST DOING OPPOSITE
                                ; OF WRITING.)
                                ; EOR VALUE FROM NDX2NIBL TABLE
                                ; WITH PREVIOUS EOR RESULT.  (ON
                                ; ENTRY, USE #$00 FOR PREVIOUS
                                ; EOR RESULT.)

                LDA #0          ; INITIALIZE (A) FOR LATER EORING.
RDUCY           DEY             ; REDUCE INDEX TO RWTSBUF2.
                STY PROSCRTH    ; SAVE INDEX.
RDSKBYT         LDY Q6L,X       ; (Y) = DISK BYTE.
                BPL RDSKBYT     ; WAIT FOR FULL BYTE.
                EOR NDX2NIBL-N2NOFF,Y
                                ; USE (Y) AS INDEX TO TABLE OF
                                ; 2-ENCODED NIBBLES.
                LDY PROSCRTH    ; RETRIEVE INDEX TO SECOND BUF.
                STA RWTSBUF2,Y  ; STORE 2-ENCODED NIBL N RWTSBUF2.
                BNE RDUCY       ; Z-FLG CONDITIONED FRM THE "LDY".

                                ; READ REST OF SEC INTO RWTSBUF1
                                ; ($BB00 --> $BBFF).
                                ;
                                ; USE DISK BYTE AS INDEX TO THE
                                ; NDX2NIBL TABLE WHICH CONTAINS
                                ; OFFSETS THAT WE WOULD BE USING
                                ; IF WE WERE ACCESSING A TABLE
                                ; OF DISK BYTES WHEN WRITING.
                                ; (IE. WE ARE JUST DOING OPPOSITE
                                ; OF WRITING.)
                                ; EOR VALUE FROM NDX2NIBL TABLE
                                ; WITH PREVIOUS EOR RESULT.

SAVYNDX         STY PROSCRTH    ; SAVE INDEX TO RWTSBUF1.
RDSKBYT2        LDY Q6L,X       ; (Y) = DISK BYTE.
                BPL RDSKBYT2    ; WAIT FOR FULL BYTE.
                EOR NDX2NIBL-N2NOFF,Y
                                ; GET 6-ENCODED NIBL FRM TBL.
                LDY PROSCRTH    ; GET INDEX TO RWTSBUF1.
                STA RWTSBUF1,Y  ; STORE 6-ENCODED NIBL N RWTSBUF1.
                INY
                BNE SAVYNDX     ; MORE DISK BYTES TO READ.

                                ; READ DATA CHECKSUM.

RDCHECK         LDY Q6L,X       ; GET DATA CHECKSUM BYTE FROM DSK.
                BPL RDCHECK     ; WAIT FOR FULL BYTE.
                CMP NDX2NIBL-N2NOFF,Y
                                ; DOES CONVERTED CHKSUM EQUAL
                                ; THE VALUE IN $BBFF?
                                ; REMEMBER: VAL IN $BBFF IS RESULT
                                ; OF PREVIOUS CUMMULATIVE EORING.
                                ; THERE4, THIS COMPARISON WITH (A)
                                ; DETECTS ANY (NON-CANCELLING)
                                ; ERROR(S) THAT MAY HAVE OCCURRED
                                ; IN THE ENTIRE SECTOR!!!
                BNE ERRTN       ; ERROR -DIDNT MATCH WITH CHKSUM.
                                ; HACKERS OFTEN CHANGE THESE TWO
                                ; BYTES 2 "CLC" AND "RTS" INSTRUCS
                                ; IN ORDER TO DEFEAT DATA CHECKSUM
                                ; AND IGNORE THE DATA EPILOGUE.

                                ; READ 1ST TWO BYTES (ONLY) OF
                                ; DATA EPILOGUE ("DE AA EB").

EPIRDDE         LDA Q6L,X       ; READ 1ST BYTE OF DATA EPILOGUE.
                BPL EPIRDDE     ; WAIT FOR FULL BYTE.
                CMP #$DE        ; IS IT A "DE"?
                BNE ERRTN       ; NO - GOT AN ERROR.
                NOP             ; STALL 2 CYCLES.
EPIRDAA         LDA Q6L,X       ; READ 2ND BYTE OF DATA EPILOGUE.
                BPL EPIRDAA     ; WAIT FOR FULL BYTE.
                CMP #$AA        ; IS IT AN "AA"?
                BEQ GOODRTN     ; YES - GOT A GOOD READ.
ERRTN           SEC             ; SIGNAL BAD READ.
                RTS             ; HACKERS OFTEN CHANGE THE "SEC" TO
                                ; "CLC" TO DEFEAT ERROR CHECKING.


                                ; ====================================
                                ; READ ADDRESS ROUTINE ($B944-$B99F).
                                ; ====================================

                                ; ON ENTRY:  (X) = SLOT*16.
                                ; ON EXIT:  (C) = 1 IF ERROR.
                                ; IF NO ERRS: (A) = #$AA.
                                ; (Y) = #$00.
                                ; (X) = SLOT*16.
                                ; $2C = CHECKSUM VAL FOUND.
                                ; $2D = SEC # FOUND.
                                ; $2E = TRK # FOUND.
                                ; $2F = VOL # FOUND.

                                ; READ THE ADDRESS HEADER.

RDADDR
                ifelse(eval(VERSION < 330),1,`
                LDY #$F8
                ',`
                LDY #$FC        ; SET 772 CHANCES TO FIND CORRECT
                ')
                STY PROSCRTH    ; ADR PROLOGUE (#$FCFC-#$10000).
KICKNTR         INY
                BNE TRYD5
                INC PROSCRTH
                BEQ ERRTN       ; ERROR - CANT FIND PROLOGUE.

                                ; FIND ADR PROLOGUE ("D5 AA 96").

TRYD5           LDA Q6L,X
                BPL TRYD5       ; WAIT FOR A FULL BYTE.
VERSUSD5        CMP #$D5        ; WAS IT "D5"?
                BNE KICKNTR     ; NO - TRY AGAIN.
                NOP             ; WAIT 2 CYCLES.
TRYAA           LDA Q6L,X
                BPL TRYAA       ; WAIT FOR FULL BYTE.
                CMP #$AA        ; WAS IT "AA"?
                BNE VERSUSD5    ; NO - RETRY SEQUENCE.
                LDY #3          ; SET (Y) FOR LATER READING OF VOL,
                                ; TRK, SEC & CHKSUM INFO FROM THE
                                ; ADRRESS FIELD.
TRY96           LDA Q6L,X
                BPL TRY96       ; WAIT FOR A FULL BYTE.
                ifelse(eval(VERSION < 330),1,`
                CMP #$B5        ; WAS IT "B5"?
                ',`
                CMP #$96        ; WAS IT "96"?
                ')
                BNE VERSUSD5    ; NO - RETRY SEQUENCE.

                                ; READ ODD-EVEN ENCODED VOL, TRK,
                                ; SEC & CHECKSUM FROM ADR FIELD.
                                ; (DURING READING, CALC A RUNNING
                                ; CHECKSUM.)
                                ; FROM: BYTE1: 1  B7  1 B5  1 B3  1  B1
                                ; BYTE1: B6 1  B4 1  B2 1  B0  1
                                ; ---------------------------------
                                ; TO:   BYTE:  B7 B6 B5 B4 B3 B2 B1 B0

                LDA #0          ; INTIALIZE FOR RUNNING CHECKSUM.
CALCK           STA CKSUMCAL
GETHDR          LDA Q6L,X       ; GET ODD-ENCODED BYTE.
                BPL GETHDR      ; WAIT FOR A FULL BYTE.
                ROL             ; SHIFT BITS, PUT (C)=1 IN BIT0.
                STA PROSCRTH    ; SAVE SHIFTED VERSION.
RDHDR           LDA Q6L,X       ; GET EVEN-CODED BYTE.
                BPL RDHDR       ; WAIT FOR A FULL BYTE.
                AND PROSCRTH    ; MERGE & CREATE NORM MEMORY BYTE.
                STA CKSUMDSK,Y  ; STORE INFO READ FROM ADDR FIELD
                                ; IN Z-PAGE:
                                ; 2F = VOL FND, 2E = TRK FND,
                                ; 2D = SEC FND, 2C = CHECKSUM FND.
                                ; (NOTE "STA:" FORCES 3-BYT CODE.)
                EOR CKSUMCAL    ; UPDATE RUNNING CHECKSUM.
                DEY
                BPL CALCK
                TAY             ; PUT CHECKSUM FOUND IN (Y).
                BNE ERRTN       ; IF CHKSUM FOUND < > 0 THEN ERR.
                                ; HACKERS OFTEN CHANGE THESE TWO
                                ; BYTES TO "CLC" AND "RTS" INSTRUCS
                                ; IN ORDER TO DEFEAT THE ADR CHKSUM
                                ; AND IGNORE THE ADR EPILOGUE.

                                ; READ 1ST 2 BYTES (ONLY) OF
                                ; ADDRESS EPILOGUE ("DE AA EB").

TRYEPIDE        LDA Q6L,X       ; GET 1ST BYTE.
                BPL TRYEPIDE    ; WAIT FOR A FULL BYTE.
                CMP #$DE        ; WAS IT A "DE"?
                BNE ERRTN       ; NO - TRY AGAIN.
                NOP             ; STALL 2 CYCLES.
TRYEPIAA        LDA Q6L,X       ; GET 2ND BYTE.
                BPL TRYEPIAA    ; WAIT FOR A FULL BYTE.
                CMP #$AA        ; WAS IT AN "AA"?
                BNE ERRTN       ; NO - RETRY SEQUENCE.
GOODRTN         CLC             ; SIGNAL GOOD READ.
                RTS

                ifelse(eval(VERSION < 330),1,`
POSTNB16        LDX   #$32
                LDY   #$00
POSTNIB1        LDA   RWTSBUF2,X
                LSR   
                LSR   
                LSR   
                STA   HOLDPRES
                LSR   
                STA   PT2BTBUF
                LSR   
                ORA   RWTSBUF1,X
                STA   (PTR2BUF),Y
                INY
                LDA   RWTSBUF2+$33,X
                LSR   
                LSR   
                LSR   
                LSR   
                ROL   HOLDPRES
                LSR   
                ROL   PT2BTBUF
                ORA   RWTSBUF1+$33,X
                STA   (PTR2BUF),Y
                INY
                LDA   RWTSBUF2+$66,X
                LSR   
                LSR   
                LSR   
                LSR   
                ROL   HOLDPRES
                LSR   
                ROL   PT2BTBUF
                ORA   RWTSBUF1+$66,X
                STA   (PTR2BUF),Y
                INY
                LDA   PT2BTBUF
                AND   #$07
                ORA   RWTSBUF1+$99,X
                STA   (PTR2BUF),Y
                INY
                LDA   HOLDPRES
                AND   #$07
                ORA   RWTSBUF1+$CC,X
                STA   (PTR2BUF),Y
                INY
                DEX
                BPL   POSTNIB1
                LDA   RWTSBUF2+$99
                LSR   
                LSR   
                LSR   
                ORA   RWTSBUF1+$FF
                STA   (PTR2BUF),Y
                RTS
                ')
                                ; ======================================
                                ; MOVE DISK ARM TO A GIVEN HALFTRACK
                                ; POSITION ($B9A0-$B9FF).
                                ; ======================================

                                ; ON ENTRY: (X) = SLOT*16
                                ; (A) = DESTINATION HALFTRK.
                                ; PRESTRK = CURRENT HALFTRK.
                                ; ON EXIT:  (A) = ?
                                ; (X) = SLOT*16.
                                ; (Y) = ?
                                ; DESTRK = FINAL HALFTRK.
                                ; PRESTRK = FINAL HALFTRK.
                                ; HOLDPRES = PREVIOUS HALFTRK.

SEEKABS
                ifelse(eval(VERSION >= 321),1,`
                STX SLT16ZPG    ; SAVE SLOT*16 IN ZERO PAGE.
                ')
                STA DESTRK      ; SAVE DESTINATION HALFTRK#.
                CMP PRESTRK     ; DESTINATION 1/2TRK=PRES 1/2TRK?
                BEQ ARRIVED     ; YES-WERE ALREADY THERE, SO XIT.
                ifelse(eval(VERSION < 321),1,`
                STX SLT16ZPG    ; SAVE SLOT*16 IN ZERO PAGE.
                ')
                LDA #0          ; INIT COUNTER FOR # OF TRKS MOVED.
                STA STPSDONE

                                ; SAVE CURRENT HALFTRK POSN AND CALC #
                                ; OF HALFTRKS NEED TO MOVE MINUS 1.

SAVCURTK        LDA PRESTRK     ; SAVE CURRENT HALFTRK POSITON.
                STA HOLDPRES
                SEC             ; CALC (PRESTRK-DESTRK).
                SBC DESTRK
                BEQ ATDESTN     ; AT DESTINATION SO GO SHUTDOWN.
                BCS MOVDOWN     ; PRES 1/2TRK > DESTINATION 1/2TRK
                                ; SO WANT TO MOVE TO LOWER 1/2TRK#.

                                ; WANT TO MOVE TO HIGHER HALFTRK#.
                                ; (PRESTRK - DESTRK = NEG RESULT.)

                EOR #$FF        ; CONVERT NEG TO POS.
                INC PRESTRK     ; MOVING UP,SO INC CURRENT HALFTRK
                                ; POSN FOR NEXT TIME AROUND.
                BCC CKDLYNDX    ; ALWAYS.

                                ; WANT TO MOVE TO LOWER HALFTRK#.
                                ; (PRESTRK - DESTRK = POS RESULT.)

MOVDOWN         ADC #$FE        ; SIMULATE A SUBTRATION OF 1.
                                ; ACTUALLY ADDING MINUS 1 (#$FF)
                                ; BECAUSE (C)=1.  WANT (A) TO EQUAL
                                ; 1 LESS THAN # OF HALFTRKS TO MOV.
                DEC PRESTRK     ; MOVING DOWN, REDUCE PRES HALFTRK
                                ; NUMBER FOR NEXT TIME AROUND.

                                ; CHECK TO SEE WHICH INDEX TO USE
                                ; TO ACCESS THE DELAY TABLE.  IF
                                ; WE ARE WITHIN 12 STEPS OF THE
                                ; DESTINATION OR START POSNS, USE
                                ; CLOSEST DISTANCE TO START OR END
                                ; POSN TO INDEX THE DELAY TABLES.
                                ; DELAY TABLES ARE ONLY 12 BYTES
                                ; LONG, SO IF MORE THAN 12 STEPS
                                ; AWAY FROM BOTH START & DESTN,
                                ; USE LAST INDEX (Y=12) TO ACCESS
                                ; THE TABLE.

                                ; CHECK IF CLOSER TO DESTN OR
                                ; START POSN.

CKDLYNDX        CMP STPSDONE    ; COMPARE # OF HALFTRKS ALREADY
                                ; MOVED VS # HALFTRKS NEED TO MOVE.
                BCC CLSR2ND     ; CLOSER TO DESTN THAN START.

                                ; CLOSER TO START.

                LDA STPSDONE    ; (A) = DISTANCE FROM START POSN.

                                ; ENTRY PT IF CLOSER TO END.

CLSR2ND         CMP #12         ; ARE WE WITHIN 12 STEPS OF START
                                ; OR DESTINATION POSN?
                ifelse(eval(VERSION < 321),1,`
                BCC   L3A4F
                LDA   #$0B
L3A4F           TAY
                LDA   PRESTRK
                AND   #$03
                ASL
                ORA   SLT16ZPG
                TAX
                LDA   MAG0ON,X
                LDA   ONTABLE,Y
                JSR   DELAY
                LDA   HOLDPRES
                AND   #$03
                ASL
                ORA   SLT16ZPG
                TAX
                LDA   MAG0FF,X
                LDA   OFFTABLE,Y
                JSR   DELAY
                INC   $26
                BNE   SAVCURTK
ATDESTN         LDA   #$FF
                JSR   DELAY
                LDX   SLT16ZPG
ARRIVED         RTS
                ',`
                BCS TURNON      ; WE ARE AT OR BEYOND 12 STEPS FRM
                                ; START OR DESTN POSN SO USE OLD
                                ; INDEX TO ACCESS DELAY TABLE.
                TAY             ; USE PRES DISTANCE TO INDEX TABLE.
TURNON          SEC             ; (C)=1 SO GET ODD INDEX TO BASE
                                ; ADR SO MAGNET WILL BE TURNED ON.
                JSR ONOROFF     ; TURN MAGNET ON TO SUCK STEPPER
                                ; MOTOR TO CORRECT HALFTRACK POSN.
                LDA ONTABLE,Y   ; GET TIME TO LEAVE MAGNET ON.
                JSR DELAY       ; DELAY TO GIVE DRIVE TIME TO ACT
                                ; BEFORE MAGNET TURNED OFF AGAIN
                                ; BECAUSE COMPUTER IS TOO FAST FOR
                                ; PERIPHERAL & WANT SMOOTH MOVT.
                LDA HOLDPRES    ; (A) = LAST HALFTRK POSN.
                CLC             ; CLR (C) SO INDEX WILL COME OUT
                                ; EVEN & THERE4 MAGNET WILL BE
                                ; TURNED OFF.
                JSR ENTRYOFF    ; TURN OFF THE MAGNET ASSOC WITH
                                ; PREVIOUS POSN.
                LDA OFFTABLE,Y  ; GET TIME TO LEAVE MAGNET OFF.
                JSR DELAY       ; LEAVE MAGNET OFF FOR A WHILE TO
                                ; GIVE ARM TIME TO BE PROPERLY
                                ; ALIGNED.  (NEED TIME TO SUCK IT
                                ; OVER & ALSO TO DECREASE BOUNCE
                                ; OR OVER SHOOT.)
                INC STPSDONE
                BNE SAVCURTK    ; ALWAYS.


                                ; ----------------------------------
                                ; ARRIVED AT DESTINATION HALFTRACK.
                                ; ----------------------------------

ATDESTN         JSR DELAY       ; WAIT ON PERIPHERAL AGAIN.

                                ; TURN LAST-USED MAGNET OFF SO EXIT
                                ; WITH ALL PHASES (IE.MAGNETS) OFF.
                                ; NOTE:  THIS IS VERY IMPORTANT
                                ; BECAUSE MAG1ON IS WIRED INTO THE
                                ; WRITE-PROTECT SWITCH!!!

                CLC             ; CLR (C) SO NDX WILL COME OUT AS
                                ; EVEN & THERE4 MAGNET WILL BE
                                ; TURNED OFF.

                                ; ---------------------------------
                                ; TURN MAGNET ON OR OFF.
                                ; ---------------------------------

ONOROFF         LDA PRESTRK     ; USE HALFTRK POSN TO INDEX MAGNET
ENTRYOFF        AND #%00000011  ; ONLY KEEP LWR 2 BITS OF HALFTRK#
                                ; BECAUSE ONLY 4 MAGNETS (0,1,2,3)
                ROL             ; MULTIPLY HALFTRK * 2 & ADD (C).
                                ; IF (C)=0, RESULT EVEN, MAG OFF
                                ; IF (C)=1, RESULT ODD, MAGNET ON.
                ORA SLT16ZPG    ; MERGE INDEX TO MAGNET WITH SLOT#.
                TAX             ; USE (X) TO INDEX MAGNET ON/OFF.
                LDA MAG0FF,X    ; USE MAGNET-0-OFF AS BASE ADR.
                LDX SLT16ZPG    ; RESTORE (X) = SLOT*16.
ARRIVED         RTS
                ')

                                ; =================================
                                ; FREE SPACE ($B9FD-$B9FF).
                                ; =================================

                ifelse(eval(VERSION == 330),1,`
                LOASCII(`*  ')
                ',`ifelse(eval(VERSION >= 331),1,`
                ASM_RES(3)
                ')
                ')


                                ; ==================================
                                ; MAIN DELAY ROUTINE IN DOS.
                                ; AMT OF DELAY = 100*(A) MICROSECS.
                                ; ==================================
DELAY
		ifdef(`NODELAY',`
                                ; PATCH TO REMOVE DELAY ROUTINE (FOR USE IN EMULATORS ONLY!)
                LDX #0
                LDA #0
                STA MTRTIME
                STA MTRTIME+1
                RTS
                NOP
                NOP
                NOP
                NOP
                NOP
                NOP
                NOP
                ',`
                LDX #17
DLY1            DEX
                BNE DLY1
                INC MTRTIME
                BNE DLY2
                INC MTRTIME+1
DLY2            SEC
                SBC #1
                BNE DELAY
		')
                RTS


                                ; =================================
                                ; DELAY TIMES FOR STEPPER MOTOR
                                ; MOVEMENTS. (VALUE * 100 = DELAY
                                ; IN MICROSECONDS.)
                                ; =================================


                                ; ---------------------------------
                                ; TIMES TO LEAVE MAGNET ON.
                                ; ($BA11 - $BA1C)
                                ; ---------------------------------

ONTABLE         ASM_DATA($01,$30,$28,$24,$20,$1E,$1D,$1C,$1C,$1C,$1C,$1C)


                                ; ---------------------------------
                                ; TIMES TO LEAVE MAGNET OFF.
                                ; ($BA1D - $BA28)
                                ; ---------------------------------

OFFTABLE        ASM_DATA($70,$2C,$26,$22,$1F,$1E,$1D,$1C,$1C,$1C,$1C,$1C)

                ifelse(eval(VERSION == 321),1,`
                ASM_DATA($1C,$1C,$1C,$1C)
                ')

                                ; =================================
                                ; TABLE OF DISK BYTES.
                                ; ($BA29 - $BA68)
                                ; =================================

                ifelse(eval(VERSION >= 330),1,`
DSKNBTBL        ASM_DATA($96,$97,$9A,$9B,$9D,$9E,$9F)
                ASM_DATA($A6,$A7,$AB,$AC,$AD,$AE,$AF)
                ASM_DATA($B2,$B3,$B4,$B5,$B6,$B7,$B9,$BA,$BB,$BC,$BD,$BE,$BF)
                ASM_DATA($CB,$CD,$CE,$CF)
                ASM_DATA($D3,$D6,$D7,$D9,$DA,$DB,$DC,$DD,$DE,$DF)
                ASM_DATA($E5,$E6,$E7,$E9,$EA,$EB,$EC,$ED,$EE,$EF)
                ASM_DATA($F2,$F3,$F4,$F5,$F6,$F7,$F9,$FA,$FB,$FC,$FD,$FE,$FF)
                ')














                                ; =================================
                                ; CHECK IF USING APPEND CMD.
                                ; (RECENT PATCH, $BA69-$BA75)
                                ; =================================

                                ; RAN OUT OF DATA SO BETTER CHECK
                                ; IF WE ARE APPENDING.

                ifelse(eval(VERSION >= 331),1,`
CKIFAPND        LDX NDX2CMD     ; GET COMMAND INDEX.
                CPX #$1C        ; ARE WE APPENDING?
                BEQ RTNCKAPN    ; YES - LEAVE APPEND FLAG ON.
                LDX #0          ; NO - MAKE SURE APPEND FLG OFF.
                STX APPNDFLG
RTNCKAPN        RTS


                                ; =================================
                                ; CLOBBER THE 80-COLUMN CARD.
                                ; (RECENT PATCH, $BA76-$BA83)
                                ; =================================

CONTCLOB        LDA #$FF        ; SET MODE FLAG FOR CARD.
                STA $4FB        ; SCRATCH PAD MEMORY FOR SLOT3.
                STA COL80OFF    ; TURN OFF THE ALTERNATE CHAR SET.
                STA ALTCAHR0FF
                JMP INIT        ; SIMULATE A TEXT STATEMENT.





                ifelse(eval(VERSION == 332),1,`
                                ; =================================
                                ; FREE SPACE
                                ; ($BA84 - $BA95)
                                ; =================================
CMPATCH         ASM_RES($10)
                ',`ifelse(eval(VERSION == 331),1,`
CMPATCH         LDA RECNMBFM
                STA FILPTBYT
                STA RECNMBWA
                TSX
                STX STKSAV
                JMP GOODFMXT
                ')
                ')




                ASM_RES(2)

                ',`ifelse(eval(VERSION == 330),1,`
                ASM_DATA($B3, $B3, $A0, $E0, $B3, $C3, $C5, $B3, $A0, $E0, $B3, $C3, $C5)
                ASM_DATA($B3, $A0, $E0, $B3, $B3, $C5, $AA, $A0, $82, $B3, $B3, $C5)
                ASM_DATA($AA, $A0, $82, $C5, $B3, $B3, $AA, $88, $82, $C5, $B3, $B3)
                ASM_DATA($AA, $88, $82, $C5, $C4, $B3, $B0, $88)
                ')
                ')
NDX2NIBL
                ifelse(eval(VERSION < 330),1,`
N2NOFF = $A8
                ASM_RES(3)
                ASM_DATA($00,$01,$08,$10,$18,$02,$03,$04,$05,$06,$20,$28,$30,$07,$09,$38)
                ASM_DATA($40,$0A,$48,$50,$58,$0B,$0C,$0D,$0E,$0F,$11,$12,$13,$14,$15,$16)
                ASM_DATA($17,$19,$1A,$1B,$1C,$1D,$1E,$21,$22,$23,$24,$60,$68,$25,$26,$70)
                ASM_DATA($78,$27,$80,$88,$90,$29,$2A,$2B,$2C,$2D,$2E,$2F,$31,$32,$33,$98)
                ASM_DATA($A0,$34,$A8,$B0,$B8,$35,$36,$37,$39,$3A,$C0,$C8,$D0,$3B,$3C,$D8)
                ASM_DATA($E0,$3E,$E8,$F0,$F8)
                ',`
                                ; ==================================
                                ; TABLE OF BYTES USED WHEN READING.
                                ; ($BA96 - $BAFF)
                                ; - USED TO TRANSLATE A DISK BYTE
                                ; TO A 2- OR 6-ENCODED NIBBLE
                                ; NEEDED FOR THE RWTS BUFFERS).
                                ; ==================================

                                ; NUMBERS > $3F REPRESENT ILLEGAL
                                ; DISK BYTES VALUES THAT ARE SIMPLY
                                ; USED AS SPACERS IN TABLE.
                                ; mosher: note: values
                                ; $00-$3F are valid
                                ; $96-$FF are spacers
N2NOFF = $96
                ASM_DATA($00,$01)                     ; VALID INDICES.
                ASM_DATA($98,$99)                     ; 2 SPACERS.
                ASM_DATA($02,$03)                     ; VALID INDICES.
                ASM_DATA($9C)                         ; 1 SPACER.
                ASM_DATA($04,$05,$06)                 ; VALID INDICES.
                ASM_DATA($A0,$A1,$A2,$A3,$A4,$A5)     ; 6 SPACERS.
                ASM_DATA($07,$08)                     ; VALID INDICES.
                ASM_DATA($A8,$A9,$AA)                 ; 3 SPACERS.
                ASM_DATA($09,$0A,$0B,$0C,$0D)         ; VALID INDICES.
                ASM_DATA($B0,$B1)                     ; 2 SPACERS.
                ASM_DATA($0E,$0F,$10,$11,$12,$13)     ; VALID INDICES.
                ASM_DATA($B8)                         ; 1 SPACER.
                ASM_DATA($14,$15,$16,$17,$18,$19,$1A) ; VALID INDICES.
                ASM_DATA($C0,$C1,$C2,$C3,$C4,$C5)     ; 6 SPACERS.
                ASM_DATA($C6,$C7,$C8,$C9,$CA)         ; 5 SPACERS.
                ASM_DATA($1B)                         ; VALID INDEX.
                ASM_DATA($CC)                         ; 1 SPACER.
                ASM_DATA($1C,$1D,$1E)                 ; VALID INDICES.
                ASM_DATA($D0,$D1,$D2)                 ; 3 SPACERS.
                ASM_DATA($1F)                         ; VALID INDEX.
                ASM_DATA($D4,$D5)                     ; 2 SPACERS.
                ASM_DATA($20,$21)                     ; VALID INDICES.
                ASM_DATA($D8)                         ; 1 SPACER.
                ASM_DATA($22,$23,$24,$25,$26,$27,$28) ; VALID INDICES.
                ASM_DATA($E0,$E1,$E2,$E3,$E4)         ; 5 SPACERS.
                ASM_DATA($29,$2A,$2B)                 ; VALID INDICES.
                ASM_DATA($E8)                         ; 1 SPACER.
                ASM_DATA($2C,$2D,$2E,$2F,$30,$31,$32) ; 7 INDICES.
                ASM_DATA($F0,$F1)                     ; 2 SPACERS.
                ASM_DATA($33,$34,$35,$36,$37,$38)     ; VALID INDICES.
                ASM_DATA($F8)                         ; 1 SPACER.
                ASM_DATA($39,$3A,$3B,$3C,$3D,$3E,$3F) ; VALID INDICES.

                ')


                                ; =================================
                                ; BUFFER ($BB00-BBFF) OF 6-ENCODED
                                ; NIBBLES (IE. 00XXXXXX, WHERE
                                ; X = 0 OR 1, BITS 6 & 7 ARE
                                ; ALWAYS 0).
                                ; USED AS A TRANSITION BUFFER
                                ; BETWEEN NORMAL MEMORY BYTES
                                ; AND DISK BYTES.
                                ; =================================

RWTSBUF1        ASM_RES($100)


                                ; ==================================
                                ; BUFFER ($BC00-$BC55) OF 2-ENCODED
                                ; NIBBLES.
                                ; ==================================

                                ; NIBBLES ARE OF THE FORM:
                                ; 0  0  JJ-0 JJ-1 KK-0 KK-1 LL-0 LL-1
                                ; WHERE BITS 6 & 7 ARE ALWAYS 0.
                                ; HOWEVER THE OTHER BITS REPRESENT
                                ; A MIXTURE OF BITS FROM DIFFERENT
                                ; ORIGINAL MEMORY BYTES.
                                ; (IE. JJ-0 REPRESENTS BIT 0 FROM
                                ; ORIGINAL MEMORY BYTE JJ.)

RWTSBUF2
                ifelse(eval(VERSION < 330),1,`
                ASM_RES(154)
                ',`
                ASM_RES(86)
                ')

                ifelse(eval(VERSION < 330),1,`
DSKNBTBL        ASM_DATA($AB,$AD,$AE,$AF)
                ASM_DATA($B5,$B6,$B7,$BA,$BB,$BD,$BE,$BF)
                ASM_DATA($D6,$D7,$DA,$DB,$DD,$DE,$DF)
                ASM_DATA($EA,$EB,$ED,$EE,$EF)
                ASM_DATA($F5,$F6,$F7,$FA,$FB,$FD,$FE,$FF)

                ASM_DATA($1C,$1C,$1C)
                ASM_RES(3)
                LDY SECDSK
                LDA PHYSECTR13,Y
                LDY #5
                ifelse(eval(VERSION == 321),1,`
                JMP RTTRK+1 ; ??? BUG?
                ',`
                JMP L3E0A
                ')
FREE1
                ASM_RES(6)

PHYSECTR13      ASM_DATA($00,$05,$0A,$02,$07,$0C,$04,$09,$01,$06,$0B,$03,$08)
                ASM_RES(35)
                ',`


                                ; ======================================
                                ; WRITE ADDRESS HEADER ($BC56-$BCC3).
                                ; (ONLY USED BY RWTSS FORMAT COMMAND).
                                ; ======================================

                                ; ON ENTRY: (X) = SLOT*16.
                                ; (Y) = # OF SELF SYNCS
                                ; TO WRITE.
                                ; HOLDAA  = #$AA.
                                ; FRMTSEC = SEC #.
                                ; FRMTVOL = VOL #.
                                ; FRMTKCTR = TRK #.
                                ; ON EXIT:  (A) = ?
                                ; (X) = SLOT*16.
                                ; (Y) = #$00.
                                ; (C) = 0 IF NOT PROTECTED.
                                ; = 1 IF WRITE PROTECTED.

WRITADR         SEC             ; (C)=1, ASSUME ERROR AS DEFAULT.
                LDA Q6H,X       ; CHK IF DISK IS WRITE PROTECTED.
                LDA Q7L,X
                BMI SET4RD      ; BRANCH IF WRITE PROTECTED.


                                ; NOT WRITE PROTECTED SO PREP TO
                                ; WRITE A GAP OF 40-CYCLE SYNC
                                ; BYTES BTWN SECS.  (THIS ROUTINE
                                ; WRITES 2 DIF SIZES OF GAPS. GAP1
                                ; PRECEEDS SEC $00.  IT INITIALLY
                                ; CONSISTS OF 128 SELF-SYNC BYTES
                                ; BUT IS LATER PARTIALLY OVER-
                                ; WRITTEN BY SEC $0F.  GAP3 OCCURS
                                ; BTWN THE ADDR FIELD OF THE
                                ; PRECEEDING SEC & THE DATA FIELD
                                ; OF THE NEXT SEC.  ITS LENGTH
                                ; VARIES WITH THE TRK # AND THE
                                ; SPEED OF THE SPECIFIC DRIVE
                                ; BEING USED.)


                LDA #$FF        ; (A) = SYNC BYTE.
                STA Q7H,X       ; SET WRITE MODE.
                CMP Q6L,X
                PHA             ; (3 CYC)
                PLA             ; (4 CYC)
WRTSYNC         JSR WTADDRTN    ; (12 CYC)
                JSR WTADDRTN    ; (12 CYC)
                STA Q6H,X       ; (5 CYC)
                CMP Q6L,X       ; (4 CYC), WRITE BYTE.
                NOP             ; (2 CYC)
                DEY             ; (2 CYC)
                BNE WRTSYNC     ; (3 CYC ON BRNCH, 2 ON FALL THRU)

                                ; WRITE ADDRESS PROLOGUE.
                                ; ("D5 AA 96", 32-CYCLE BYTES.)

                LDA #$D5        ; (2 CYC)
                JSR WRBYTE3     ; (24 CYC BEFORE, 6 AFTER)
                LDA #$AA        ; (2 CYC)
                JSR WRBYTE3     ; (24 CYC BEFORE, 6 AFTER)
                LDA #$96        ; (2 CYC)
                JSR WRBYTE3     ; (24 CYC BEFORE, 6 AFTER)

                                ; WRITE VOL, TRK & SECTOR AS
                                ; ODD/EVEN ENCODED BYTES.
                                ; (32 CYCLES BETWEEN BYTES.)

                LDA FRMTVOL     ; (A) = VOLUME #, (3 CYC).
                JSR WRBYTE1     ; WRITE BYTES FOR VOLUME.
                                ; (JSR INSTRUCTION = 6 CYC.)
                LDA FRMTKCTR    ; WRITE BYTES FOR TRK.
                                ; (3 CYC + 6 FROM BEFORE.)
                JSR WRBYTE1     ; (6 CYC + 17 MORE CYC, WITH
                                ; 6 RESIDUAL CYC.)
                LDA FRMTSEC     ; WRITE BYTES FOR SEC.
                JSR WRBYTE1     ; (CYCLES AS PER ABOVE.)

                                ; CALCULATE ADDRESS CHECKSUM.

                LDA FRMTVOL     ; (3 CYC + 6 FROM BEFORE)
                EOR FRMTKCTR    ; (3 CYC)
                EOR FRMTSEC     ; (3 CYC)
                PHA             ; SAVE CKSUM ON STK (3 CYC).

                                ; ODD ENCODE THE ADDRESS CHECKSUM.

                LSR             ; (2 CYC)
                ORA HOLDAA      ; (3 CYC)
                STA Q6H,X       ; (5 CYC - WRITE BYTE)
                LDA Q6L,X       ; (4 CYC)

                                ; EVEN ENCODE THE ADDRESS CHECKSUM.

                PLA             ; (3 CYC)
                ORA #%10101010  ; (2 CYC)
                JSR WRBYTE2     ; (26 CYC BEFORE WRITE, 6 AFTER)

                                ; WRITE ADDRESS EPILOGUE.
                                ; ("DE AA EB", 32-CYCLE BYTES.)

                LDA #$DE        ; (2 CYC + 6 LEFT OVER FRM B4.)
                JSR WRBYTE3     ; (24 CYC BEFORE WRITE, 6 AFTER)
                LDA #$AA        ; (2 CYC + 6 LEFT OVER FROM B4)
                JSR WRBYTE3     ; (24 CYC B4 WRITE, 6 LFT OVER)
                LDA #$EB        ; (2 CYC + 6 LEFT OVER FROM B4)
                JSR WRBYTE3     ; (24 CYC BEFORE WRITE, 6 AFTER)
                CLC
SET4RD          LDA Q7L,X       ; SET READ MODE.
                LDA Q6L,X
WTADDRTN        RTS


                                ; =================================
                                ; WRITE DOUBLE AND SINGLE BYTE
                                ; SUBRTNS WITH DIFFERENT DELAYS.
                                ; =================================

                                ; NOTE:  A "JSR" INSTRUCTION
                                ; REQUIRES 6 CYCLES.  THEREFORE
                                ; 6 CYCLES + ANY OTHER OVERHEAD
                                ; SHOULD BE ADDED TO THE SUBRTNS
                                ; GIVEN BELOW IN ORDER TO ARRIVE
                                ; AT A 32-CYCLE COUNT BETWEEN @
                                ; BYTE WRITTEN.

WRBYTE1         PHA             ; (3 CYC)

                                ; CALC & WRITE ODD-ENCODED BYTE.

                LSR             ; (2 CYC)
                ORA HOLDAA      ; (3 CYC)
                STA Q6H,X       ; (5 CYC)
                CMP Q6L,X       ; (4 CYC)

                                ; CALC & WRITE EVEN-ENCODED BYTE.

                PLA             ; (4 CYC)
                NOP             ; (2 CYC)
                NOP             ; (2 CYC)
                NOP             ; (2 CYC)
                ORA #%10101010  ; (2 CYC)
WRBYTE2         NOP             ; (2 CYC)
WRBYTE3         NOP             ; (2 CYC)
                PHA             ; (3 CYC)
                PLA             ; (4 CYC)
                STA Q6H,X       ; (5 CYC)
                CMP Q6L,X       ; (4 CYC)
                RTS             ; (6 CYC LEFT OVER AFTER WRITE)


                                ; =================================
                                ; FREE SPACE
                                ; ($BCDF - $BCFF)
                                ; =================================
FREE1
                ifelse(eval(VERSION >= 331),1,`
                ASM_RES(33)
                ',`ifelse(eval(VERSION == 330),1,`

                ifdef(`FRANKLIN',`
                                ; CAM: This is the only difference in Franklins DOS
                                ;       (well... this routine AND the patch to call this routine)
                                ; Since Franklin Ace had lower case letters, this routine
                                ; will upcase any commands the user types in before sending to DOS.
FRANK           LDA BUF200,X
                CMP #HICHAR(`a')
                BCC FRANK1
                CMP #HICHAR(`{')
                BCS FRANK1
                AND #$DF
FRANK1          RTS

                ',`
                                ; UNUSED  GARBAGE - CONSIDER FREE.
                ASM_DATA($88)
                ASM_DATA($A5,$E8,$91,$A0,$94,$88,$96,$E8)
                ASM_DATA($91,$A0,$94,$88,$96)
                ')
                ASM_DATA($91,$91,$C8)
                ASM_DATA($94,$D0,$96,$91,$91,$C8,$94,$D0)
                ASM_DATA($96,$91,$A3,$C8,$A0,$A5,$85,$A4)
                ')
                ')

                ')
