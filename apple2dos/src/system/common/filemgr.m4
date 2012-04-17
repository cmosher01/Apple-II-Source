include(`asm.m4h')
include(`symbols.m4h')

                                ; =================================
                                ; FILE MANAGERS CONSTANTS TABLE.
                                ; ($AAC1 - $AAC8)
                                ; =================================

ADRIOB          ASM_ADDR(IBTYPE)    ; PTS TO RWTSS IOB. NOTE:THE $3E3
                                ; VECTOR LOADS FROM ADRIOB.
ADRVTOC         ASM_ADDR(VTOCBUFF)  ; PTS TO VTOC SECTOR BUFFER.
ADRDIRBF        ASM_ADDR(DIRECBUF)  ; PTS TO DIRECTORY SECTOR BUFFER.
DOSNDPL1        ASM_ADDR(DOSLIM)    ; PTS TO LAST BYTE OF DOS PLUS 1.


                                ; ======================================
                                ; FILE MANAGERS FUNCTION HNDLR ENTRY
                                ; POINT TABLE ($AAC9 - $AAE4).
                                ; (ALL ADDRS ARE 1 LESS THAN THE ACTUAL
                                ; ENTRY POINT BECAUSE THE THE FUNCTIONS
                                ; ARE ENTERED VIA A "STACK JUMP".)
                                ; ======================================

FMFUNCTB        ASM_ADDR(GOODFMXT-1) ; NULL - EXIT WITH NO ERRORS.
                ASM_ADDR(FNOPEN-1)
                ASM_ADDR(FNCLOSE-1)
                ASM_ADDR(FNREAD-1)
                ASM_ADDR(FNWRITE-1)
                ASM_ADDR(FNDELETE-1)
                ASM_ADDR(FNCATLOG-1)
                ASM_ADDR(FNLOCK-1)
                ASM_ADDR(FNUNLOCK-1)
                ASM_ADDR(FNRENAME-1)
                ASM_ADDR(FNPOSN-1)
                ASM_ADDR(FNINIT-1)
                ASM_ADDR(FNVERIFY-1)
                ASM_ADDR(GOODFMXT-1) ; NULL - EXIT WITH NO ERRORS.


                                ; ====================================
                                ; FILE MANAGER READ-SUBFUNCTION ENTRY
                                ; POINT TABLE ($AAE5 - $AAF0).
                                ; (P.S.  SUBFUNCTIONS EMPLOYING THE
                                ; POSITION OPTION ARE AVAILABLE TO
                                ; THE USER BUT NEVER CALLED BY DOS.)
                                ; ====================================

                                ; SUBFUNCTION INDEX.
RDSUBTBL        ASM_ADDR(GOODFMXT-1) ; (0), EXIT.
                ASM_ADDR(READONE-1)  ; (1), READ ONE BYTE.
                ASM_ADDR(READRNG-1)  ; (2), READ A RANGE OF BYTES.
                ASM_ADDR(PSNRDONE-1) ; (3), POSITION & READ ONE BYTE.
                ASM_ADDR(PSNRDRNG-1) ; (4), POSN & READ RNGE OF BYTES.
                ASM_ADDR(GOODFMXT-1) ; (5), EXIT.


                                ; =====================================
                                ; FILE MANAGER WRITE SUBFUNCTION ENTRY
                                ; POINT TABLE ($AAF1 - $AAFC).
                                ; (P.S.  SUBFUNCTIONS EMPLOYING THE
                                ; POSITION OPTION ARE AVAILABLE TO
                                ; THE USER BUT NEVER CALLED BY DOS.)
                                ; =====================================

                                ; SUBFUNCTION INDEX.
WRSUBTBL        ASM_ADDR(GOODFMXT-1) ; (0), EXIT.
                ASM_ADDR(WRITEONE-1) ; (1), WRITE ONE BYTE.
                ASM_ADDR(WRITERNG-1) ; (2), WRITE A RANGE OF BYTES.
                ASM_ADDR(PSNWRONE-1) ; (3), POSITION & WRITE ONE BYTE.
                ASM_ADDR(PSNWRRNG-1) ; (4), POSN & WRITE RNG OF BYTES.
                ASM_ADDR(GOODFMXT-1) ; (5), EXIT.

















                                ; =========================================
                                ; FILE MANAGER EXTERNAL ENTRY POINT.
                                ; -NOTE: THE $3D6 VECTOR LOADS FROM HERE!!
                                ; -ENABLES USER TO ACCESS FILE MANAGER
                                ; FROM CUSTOM ASSEMBLY LANGUAGE ROUTINES.
                                ; =========================================

FMXTNTRY
                ifelse(eval(VERSION >= 320),1,`
                CPX #0
                BEQ FMXTCMD     ; ALLOW NEW FILE-SIMULATE AN "INIT".
                LDX #02         ; REQUIRES OLD FILE (SIMULATE LOAD).
FMXTCMD         STX NDX2CMD     ; SET INDEX TO COMMAND.
                ')


                                ; =================================
                                ; FILE MANAGER PROPER.
                                ; ---------------------------------

FILEMGR         TSX             ; SAVE STK PTR SO WE CAN LATER RTN
                STX STKSAV      ; TO AFTRFUNC ($A6AB) LOCATED IN
                                ; THE FMDRIVER ROUTINE ($A6A8).
                                ; (NOTE THAT APPEND CMD EMPLOYS
                                ; THE RSETPTRS ROUTINE ($B6B3) TO
                                ; RE-ADJUST STKSAV ($B39B) SO THAT
                                ; EXECUTION ACTUALLY RETURNS TO
                                ; AFTRCMD ($A6AB) LOCATED IN THE
                                ; DOS CMD PARSING AND PROCESSING
                                ; ROUTINES.)
                JSR RSTRFMWA    ; COPY FM WRK BUF (IN DOS CHAIN) TO
                                ; FM WRK AREA (NOT IN DOS CHAIN).
                LDA OPCODEFM    ; CHK IF OPCODE IS LEGAL.
                CMP #13         ; (MUST BE LESS THAN 13.)
                BCS TOERROP     ; OPCODE TOO LARGE - RANGE ERROR.
                ASL             ; DOUBLE VAL OF OPCODE & PUT IT IN
                TAX             ; (X) SO IT INDEXES TABLE OF ADRS.
                LDA FMFUNCTB+1,X
                                ; STICK ADDRESS (MINUS 1) OF THE
                PHA             ; FUNCTION HANDLER ON THE STACK
                LDA FMFUNCTB,X  ; (HI BYTE FIRST).
                PHA
                RTS             ; DO A STACK JMP TO THE FUNCTIONS
                                ; ENTRY POINT.

TOERROP         JMP RNGERROP    ; GO HANDLE RANGE ERROR.


                                ; =================================
                                ; OPEN FUNCTION HANDLER.
                                ; =================================

FNOPEN          JSR COMNOPEN    ; OPEN PREEXISTING FILE OR CREATE
                                ; NEW FILE IF CMD ALLOWS NEW FILES
                JMP GOODFMXT    ; EXIT CLEANLY. EVENTUALLY RTNS TO
                                ; AFTRFUNC ($A6AB) LOCATED IN THE
                                ; FM DRIVER ROUTINE ($A6A8).


                                ; =================================
                                ; COMMON OPEN ROUTINE.
                                ; =================================

COMNOPEN        JSR ZWRKAREA    ; INITIALIZE THE FM WORK AREA WITH
                                ; DEFAULT VALUES.
                                ; (DONT CONFUSE WITH FM WRK BUF
                                ; WHICH IS IN DOS BUF CHAIN.)
                LDA #1          ; DESCRIBE SECTOR LENGTH AS 256
                STA SECSIZWA+1  ; BYTES (IN FM WORK AREA).

                                ; GET RECORD LENGTH FROM FM PARM
                                ; LIST & PUT IT IN FM WORK AREA.
                                ; (DONT ALLOW A 0 LENGTH.  IF
                                ; ZERO, CHANGE IT TO ONE.)

                LDX RECLENFM+1
                LDA RECLENFM
                BNE STRECLEN    ; NON-ZERO REC LENGTH IS OK.
                CPX #0
                BNE STRECLEN
                INX             ; WAS ZERO, MAKE IT ONE INSTEAD.
STRECLEN        STA RECLENWA    ; PUT LENGTH IN FM WORK AREA.
                STX RECLENWA+1
                JSR GETFNTRY    ; TRY TO FIND DIRECTORY SEC FOR FILE.
                BCC FILLINWA    ; BRNCH IF FOUND DIR SEC WITH SAME
                                ; NAME IN FILE DESCRIPTION ENTRY.

                                ; THE NAMED FILE WAS NOT FOUND IN
                                ; THE DIRECTORY, SO PREPARE A NEW
                                ; FILE ENTRY IN CASE THE CMD CAN
                                ; LEGALLY CREATE A NEW FILE.
                ifelse(eval(VERSION >= 320),1,`
                STX CURDIRNX    ; OFFSET TO NEW DESCRIP ENTRY IN
                                ; CASE WANT TO CREATE A NEW FILE.

                                ; CHECK TO SEE IF CMD CAN
                                ; LEGALLY CREATE A NEW FILE.

                LDX NDX2CMD     ; (X) = INDEX REPRESENTING CMD.
                LDA CMDATTRB,X  ; GET 1ST BYTE CONTAINING DESCRIP
                                ; OF THE COMMANDS ATTRIBUTES.
                LDX CURDIRNX    ; (X)=INDEX FOR A NEW FILE DESCRIP
                                ; ENTRY INTO DIRECTORY SEC.
                LSR             ; (C)=BIT0 OF 1ST ATTRIBUTE BYTE.
                BCS CREATNEW    ; IF (C)=1 CMD CAN MAKE NEW FILE.

                                ; COMMAND CANT CREATE NEW FILE.
                                ; SEE WHICH LANGUAGE WERE USING
                                ; & EXIT WITH APPROPRIATE ERROR.

NEWILLGL        LDA CONDNFLG    ; $00=WARMSTART, $01=READING,
                                ; $40=A(RAM), $80=COLDSTART,
                                ; $C0=INTEGER.
                CMP #$C0        ; IS INTEGER IN ROM?
                BNE TOFILNOT    ; NO.
                JMP LNGNOTAV    ; HANDLE LANGUAGE-NOT-AVAIL ERROR.

TOFILNOT        JMP FILENOT     ; HANDLE FILE-NOT-FOUND ERROR.
                ')


                                ; ---------------------------------
                                ; CREATE A NEW FILE:
                                ; - INIT FILE SIZE = 1 SEC LONG.
                                ; - ALLOCATE SECS FOR DATA SECS
                                ; & UPDATE FILE SIZE.
                                ; - WRITE UPDATED VTOC TO DISK.
                                ; - PUT LINK IN FILE DESCRIPTION
                                ; ENTRY & UPDATE FILE SIZE.
                                ; - WRITE UPDATED VTOC TO DISK.
                                ; - WRITE NEW T/S LIST TO DISK.
                                ; --------------------------------

CREATNEW        LDA #0          ; INITIALIZE FILE SIZE = 1 SECTOR.
                STA FIL1SIZE+1,X
                                ; (BECAUSE AT 1ST, ONLY STARTING
                LDA #1          ; OUT WITH A T/S LIST SECTOR.)
                STA FIL1SIZE,X
                STX CURDIRNX    ; SAVE BYTE OFFSET INTO DIR SEC
                                ; FOR FILE DESCRIPTION.
                ifelse(eval(VERSION < 320),1,`
                STX BYTNXD1R
                ')
                JSR ASGNTKSC    ; FIND TRK/SEC VALS FOR NEW FILE.

                                ; FINISH SETTING UP PARAMETERS
                                ; IN THE FILE MANAGERS WORK AREA.
                                ; (P.S. DONT CONFUSE FM WORK AREA
                                ; WITH THE VARIOUS WORK BUFFERS
                                ; LOCATED IN THE DOS BUFFER CHAIN.)

                LDX CURDIRNX    ; OFFSET IN DIR SEC FOR NEW DESCRIP
                STA FIL1TSSC,X  ; PUT SEC VAL IN DIRECTORY SECTOR.
                STA TSL1SEC     ; PUT SEC VALUE OF 1ST T/S LIST
                                ; SECTOR IN THE FM WORK AREA.
                STA CURTSSEC    ; PUT CUR SEC VAL IN FM WRK AREA.
                LDA ASIGNTRK    ; DO THE SAME FOR TRACK VALUE.
                STA FIL1TSTK,X
                STA TSL1TRK
                STA CURTSTRK
                LDA FILTYPFM    ; (FROM FM PARM LIST.)
                STA FIL1TYPE,X  ; PUT FILE TYPE IN DIREC SEC BUF.

                                ; WRITE DIRECTORY SECTOR BUFFER.

                JSR WRDIRECT    ; WRITE DIRECTORY SEC BUF IN CAT.

                                ; WRITE T/S LIST SECTOR BUFFER.

                JSR SELTSBUF    ; GET ADR OF T/S LIST SEC BUF FROM
                                ; THE FM PARM LIST.
                JSR ZCURBUF     ; ZERO OUT T/S LIST SECTOR BUF.
                JSR WRITETS     ; WRITE ZEROED OUT T/S LIST SEC.
                                ; (IF THE WRITE SUBFUNCTION IS
                                ; LATER ENTERED TO WRITE NEW DATA
                                ; TO THE DISK, THE ZERO BYTES ARE
                                ; DETECTED & USED AS SIGNALS THAT
                                ; NEW DATA PAIR SHOULD BE WRITTEN
                                ; INTO THE T/S LIST SECTOR.)
                LDX CURDIRNX    ; OFFSET IN DIREC SEC FOR NEW FILE.
                LDA #6          ; DEFAULT RETURN CODE VALUE TO THAT
                STA RTNCODFM    ; FOR A FILE-NOT-FOUND ERROR.

                                ; FILL IN THE FM WORK AREA BUFFER.
                                ; (ROUTINE COMMON TO OPENING A
                                ; NEW OR PRE-EXISTING FILE.)

FILLINWA        LDA FIL1TSTK,X  ; T/S LIST TRK (FRM DIRECTORY SEC)
                STA TSL1TRK
                LDA FIL1TSSC,X  ; T/S LIST SEC (FRM DIRECTORY SEC)
                STA TSL1SEC
                LDA FIL1TYPE,X  ; FILE TYPE (FROM DIRECTORY SEC).
                STA FILTYPFM
                STA FILTYPWA
                LDA FIL1SIZE,X  ; FILE SIZE (FROM DIRECTORY SEC).
                STA FILENSEC
                LDA FIL1SIZE+1,X
                STA FILENSEC+1

                ifelse(eval(VERSION >= 320),1,`
                STX BYTNXD1R    ; INDEX INTO DIREC SEC FOR DESCRIP.
                ')
                LDA #$FF        ; PRETEND THAT THE LAST DATA SEC
                STA RELPREV     ; USED HAD A RELATIVE SECTOR #
                STA RELPREV+1   ; (IN RELATION TO THE ENTIRE FILE)
                                ; OF #$FFFF.  NOTE: THIS VALUE IS
                                ; LATER USED TO TRICK THE READ AND
                                ; WRITE SUBFUNCTIONS INTO IGNORING
                                ; THE DATA SECTOR CURRENTLY IN
                                ; MEMORY.
                LDA MXIN1TSL    ; DICTATE THAT A T/S LIST CAN ONLY
                STA MXSCURTS    ; DESCRIBE $7A (#122) DATA SECS.
                                ; NOTE: THIS VAL IS LATER USED BY
                                ; THE READ AND WRITE SUBFUNCTIONS
                                ; TO DECIDE WHETHER OR NOT THE T/S
                                ; LIST CURRENTLY IN MEMORY SHOULD
                                ; BE USED.

                                ; READ FIRST T/S LIST SECTOR TO
                                ; THE T/S LIST SEC BUFFER.
                                ; (EVENTHOUGH IT IS NOT NEEDED BY
                                ; THE RENAME, LOCK OR UNLOCK
                                ; FUNCTIONS, THE FIRST T/S LIST IS
                                ; AUTOMATICALLY READ INTO THE T/S
                                ; LIST BUFFER IF THE FILE WAS
                                ; FOUND.)

                CLC             ; (C)=0 =SIGNAL 1ST T/S LIST SEC.
                JMP READTS      ; GO READ IN THE T/S LIST SEC.


                                ; ==================================
                                ; INITIALIZE (IE. ZERO OUT) THE FM
                                ; WORK AREA SO IT CAN BE CUSTOMIZED
                                ; IN ACCORDANCE WITH THE CALLING
                                ; FUNCTION.  (ALTHOUGH SOME WORK
                                ; BYTES MAY NOT BE SUBSEQUENTLY
                                ; ALTERED, DONT BE LULLED INTO
                                ; THINKING THAT THEY ARE NOT
                                ; IMPORTANT.  ZERO VALUES ARE JUST
                                ; AS RELEVANT AS NON-ZERO VALUES.
                                ; ALSO BE CAUTIONED NOT TO CONFUSE
                                ; THE FM WORK AREA WITH ITS IMAGE
                                ; (DOS WORK BUFFER) THAT IS HOUSED
                                ; IN THE CHAIN OF DOS BUFFERS.)
                                ; ==================================

                                ; ZERO OUT THE FM WORK AREA.

ZWRKAREA        LDA #0
                TAX             ; INITIALIZE X-INDEX.
ZEROWRKA        STA FMWKAREA,X  ; PUT $00 BYTE IN WORK AREA.
                INX
                CPX #45         ; WORK AREA IS 45 BYTES LONG.
                BNE ZEROWRKA

                                ; BEGIN CUSTOMIZING THE WORK AREA.
                                ; GET VOL, DRV, SLOT & CATALOG TRK
                                ; VALUES FROM THE FM PARM LIST.
                                ; PUT DRV, SLOT*16, CAT TRK AND
                                ; COMPLEMENTED VOL # IN THE WORK
                                ; AREA.

                LDA VOLFM       ; VOLUME #.
                EOR #$FF        ; CALC 1S COMPLEMENT OF VOL #.
                STA VOLWA
                LDA DRVFM       ; DRIVE #.
                STA DRVWA
                LDA SLOTFM      ; GET SLOT #.
                ASL             ; CALC SLOT * 16.
                ASL
                ASL
                ASL
                TAX             ; SET (X) = SLOT*16.
                STX SLOT16WA
                LDA #CATTRK     ; NORMAL CAT TRK = #17.
                STA TRKWA
                RTS


                                ; =================================
                                ; CLOSE FUNCTION HANDLER.
                                ; =================================

FNCLOSE         JSR CKDATUP     ; WRITE DATA SEC BUF IF NECESSARY.
                JSR CKTSUPDT    ; WRITE T/S LIST SEC BUF IF NEC.
                JSR FIXMAP      ; FREE UP SECS THAT WERE ALLOCATED
                                ; BUT NOT USED. WHENEVER SOMETHING
                                ; IS WRITTEN TO THE DISK, THE WHOLE
                                ; TRK IS ALLOCATED IN THE VTOC
                                ; WHETHER IT IS NEEDED OR NOT.
                                ; THERE4, ONCE DONE WRITING, GO
                                ; BACK & FREE UP UNNEEDED SECS.

                                ; WAS LAST OPERATION A WRITE?

                LDA #%00000010  ; IF BIT 1 SET, THEN WAS WRITE.
                AND UPDATFLG
                BEQ TOGDFMXT    ; WASNT A WRITE SO CAN JUST EXIT
                                ; BECAUSE NO NEED TO UPDATE DIR SEC.

                                ; LAST OPERATION WAS A WRITE, SO
                                ; BETTER PREPARE TO FIX UP THE
                                ; FILE-SIZE BYTES AND T/S LINKS
                                ; IN DIRECTORY SECTORS, ETC.

                JSR READVTOC    ; READ VOLUME TABLE OF CONTENTS.

                                ; READ IN ALL DIR SECS UP TO THE
                                ; ONE CONTAINING THE FILE DESCRIP
                                ; ENTRY FOR THE FILE WE ARE CLOSING.

                LDA #0
                CLC             ; (C)=0=SIGNAL TO READ 1ST DIR SEC
PURGEDIR        JSR RDDIRECT    ; GO READ A DIRECTORY SEC.
                SEC             ; (C)=1, 1ST DIR SEC ALREADY READ
                DEC SECNXD1R    ; INDEX FOR # OF DIRECTORY SECS.
                BNE PURGEDIR    ; IF 0, THEN JUST READ DIREC SEC
                                ; PERTAINING TO THE FILE WANTED.

                                ; UPDATE THE FILE SIZE & WRITE THE
                                ; DIRECTORY SECTOR TO THE DISK.

                LDX BYTNXD1R    ; (X) = OFFSET OF FILE DESCRIPTION
                                ; IN THE DIRECTORY SECTOR.
                LDA FILENSEC    ; # SECS IN FILE (FRM FM WRK AREA)
                STA FIL1SIZE,X
                LDA FILENSEC+1
                STA FIL1SIZE+1,X
                JSR WRDIRECT    ; WRITE UPDATED DIRECTORY SECTOR.

TOGDFMXT        JMP GOODFMXT    ; CLEAN EXIT.  EVENTUALLY RTNS TO
                                ; AFTRFUNC ($A6AB) LOCATED IN THE
                                ; FMDRIVER ROUTINE ($A6A8).


                                ; =================================
                                ; RENAME FUNCTION HANDLER.
                                ; =================================

FNRENAME        JSR COMNOPEN    ; LOCATE FILE WITH SAME NAME & OPN
                                ; IT IF ITS NOT ALREADY OPEN.
                LDA FILTYPWA    ; GET FILE TYPE (FROM WORK AREA).
                BMI TOFILOCK    ; ERROR-CANT RENAME A LOCKED FILE
                LDA RENAMBUF    ; GET ADR OF SECONDARY NAME BUFFER
                STA A4L         ; FROM FM PARM LIST & PUT IN A4L/H
                LDA RENAMBUF+1
                STA A4H
                LDX CURDIRNX    ; (X) = INDEX INTO CURRENT DIR SEC.
                JSR NWDESCRP    ; COPY NEW NAME TO DIREC SEC BUF.
                JSR WRDIRECT    ; WRIT MODIFIED DIR SEC BUF TO DSK.
                JMP GOODFMXT    ; TAKE THE GOOD ROUTE HOME.
                                ; EVENTUALLY RETURNS TO AFTRFUNC
                                ; LOCATED IN FMDRIVER RTN ($A6A8).

                                ; =================================
                                ; READ FUNCTION HANDLER.
                                ; =================================

FNREAD          LDA SUBCODFM    ; CHK IF SUBCODE IS LEGAL.
                CMP #5          ; (MUST BE < = 5.)
                BCS TOERRSUB    ; RANGE ERROR - ILLEGAL SUBCODE.

                ASL             ; SUBCODE*2, BECAUSE 2 BYTES/ADR.
                TAX             ; INDEX TABLE OF SUBFUNCTION ADRS.
                LDA RDSUBTBL+1,X
                                ; GET ADDR (MINUS 1) OF SUBFUNCT
                PHA             ; ENTRY POINT & PUT IT ON THE STK
                LDA RDSUBTBL,X  ; (HI BYTE FIRST). THEN DO A STACK
                PHA             ; JUMP TO EXECUTE THE GIVEN READ
                RTS             ; SUBFUNCTION.

TOERRSUB        JMP RNGERRSB    ; GO HANDLE RANGE ERROR.

TOFILOCK        JMP FILELOKD    ; GO HANDLE LOCKED FILE ERROR.


                                ; =================================
                                ; WRITE FUNCTION HANDLER.
                                ; =================================

FNWRITE         LDA FILTYPWA    ; CHK IF FILE IS LOCKED.
                BMI TOFILOCK    ; ERROR - CANT WRITE TO LCKD FILE.

                LDA SUBCODFM    ; CHK IF SUBCODE IS LEGAL.
                CMP #5          ; (MUST BE < = 5.)
                BCS TOERRSUB    ; ERROR - ILLEGAL SUBCODE.

                ASL             ; SUBCODE*2, BECAUSE 2 BYTES/ADR.
                TAX             ; INDEX TABLE OF SUBFUNCTION ADRS.
                LDA WRSUBTBL+1,X
                                ; GET ADR (MINUS 1) OF SUBFUNCT
                PHA             ; ENTRY POINT & STICK ON THE STACK
                LDA WRSUBTBL,X  ; (HI BYTE FIRST). THEN DO A STACK
                PHA             ; JUMP TO EXECUTE THE GIVEN WRITE
                RTS             ; SUBFUNCTION.


                                ; =================================
                                ; POSITION AND READ-ONE-BYTE
                                ; SUBFUNCTION HANDLER.
                                ; =================================

PSNRDONE        JSR CALCFPTR    ; USING R-, L- & B-PARAMETERS,CALC
                                ; THE POSN OF FILE PTR WANTED.


                                ; ==================================
                                ; READ-ONE-BYTE SUBFUNCTION HANDLER
                                ; ==================================

READONE         JSR RDDATA      ; GET DATA BYTE FROM DATA SEC BUF.
                                ; (IF DESIRED DATA SECTOR IS NOT
                                ; ALREADY IN MEMORY, THEN READ IT
                                ; IN. HOWEVER, 1ST CHK IF PRESENT
                                ; INFO IN DATA SEC NEEDS TO BE
                                ; UPDATED SO DONT OVERWRITE DATA
                                ; IN BUF & LOOSE INFO.)

                STA ONEIOBUF    ; PUT BYTE READ IN THE 1-BYTE BUF
                                ; CONTAINED IN THE FM PARM LIST.
                JMP GOODFMXT    ; EXIT THE FILE MGR.  EVENTUALLY
                                ; RTNS TO AFTRFUNC ($A6AB) LOCATED
                                ; IN THE FMDRIVER ROUTINE ($A6A8).


                                ; =================================
                                ; POSITION & READ-A-RANGE-OF-BYTES
                                ; SUBFUNCTION HANDLER.
                                ; =================================

PSNRDRNG        JSR CALCFPTR    ; USING R-, L- & B-PARMS, CALC THE
                                ; POSN OF FILE POINTER WANTED.


                                ; =================================
                                ; READ-RANGE-OF-BYTES
                                ; SUBFUNCTION HANDLER.
                                ; =================================

READRNG         JSR DECRWLEN    ; DECREMENT THE # OF BYTES TO READ.
                                ; (DONE READING WHEN LEN2RDWR=0.)
                JSR RDDATA      ; GET DATA BYTE FROM DATA SEC BUF.
                                ; (IF DESIRED DATA SECTOR IS NOT
                                ; ALREADY IN MEMORY, THEN READ IT
                                ; IN. HOWEVER, 1ST CHK IF PRESENT
                                ; INFO IN DATA SEC NEEDS TO BE
                                ; UPDATED SO DONT OVERWRITE DATA
                                ; IN BUF & LOOSE INFO.)
                PHA             ; SAVE SINGLE BYTE READ ON STK.
                JSR INCIOBUF    ; INC THE CURNT TARGET RAM MEMORY
                                ; LOCATION (BYTRNG) & POINT A4L/H
                                ; AT TARGET LOCATION IN I/O BUF.
                LDY #0          ; INITIALIZE (Y) INDEX.
                PLA             ; RETRIEVE BYTE READ.
                STA (A4L),Y     ; PUT SINGLE BYTE READ INTO THE
                                ; TARGET MEMORY LOCATION.
                JMP READRNG     ; GO BK TO READ NEXT BYTE OF DATA.


                                ; =================================
                                ; SUBROUTINE TO READ A DATA BYTE.
                                ; =================================

RDDATA          JSR NXTDATRD    ; CHK IF DATA SEC WE WANT TO READ
                                ; IS ALREADY IN MEMORY.  IF NOT,
                                ; READ DATA SEC INTO DATA SEC BUF.
                                ; HOWEVER, 1ST CHK IF DAT SEC BUF
                                ; NEEDS UPDATING SO DONT OVRWRITE
                                ; DATA SEC BUF & LOSE INFO.
                BCS NDATERR     ; BRANCH IF RAN OUT OF DATA SECS
                                ; WHILE READING.
                LDA (A4L),Y     ; GET SINGLE BYTE FRM DATA SEC BUF
                PHA             ; SAVE IT ON STK.
                JSR INCREC      ; EITHER INC REC # OR BYTE OFFSET
                                ; INTO RECORD.
                JSR INCFILPT    ; INC BYTE OFFSET INTO CURRENT
                                ; DATA SEC OR, IF AT END OF SEC,
                                ; INC THE SECTOR OFFSET INTO THE
                                ; ENTIRE FILE.
                PLA             ; GET DATA BYTE RD (BACK OFF STK).
                RTS

NDATERR         JMP ENDOFDAT    ; RAN OUT OF DATA WHILE READING.


                                ; =================================
                                ; POSITION & WRITE-ONE-BYTE
                                ; SUBFUNCTION HANDLER.
                                ; =================================

PSNWRONE        JSR CALCFPTR    ; USING R-, L- & B-PARMS, CALC
                                ; POSN OF FILE POINTER WANTED.


                                ; =================================
                                ; WRITE-ONE-BYTE SUBFUNCTION HNDLR
                                ; =================================

WRITEONE        LDA ONEIOBUF    ; GET BYTE TO WRITE FROM ONE-BYTE
                                ; BUFFER IN FM PARAMETER LIST.
                JSR WRTDATA     ; STORE DATA TO WRITE IN THE DATA
                                ; SECTOR BUFFER.  IF DATA SEC BUF
                                ; IS FULL, THEN WRITE IT TO DISK
                                ; AND UPDATE T/S LIST BUF.
                JMP GOODFMXT    ; EXIT FILE MANAGER.  EVENTUALLY
                                ; RTNS TO AFTRFUNC ($A6AB) LOCATED
                                ; IN THE FMDRIVER ROUTINE ($A6A8).


                                ; =================================
                                ; POSITION & WRITE-RANGE-OF-BYTES
                                ; SUBFUNCTION HANDLER.
                                ; =================================

PSNWRRNG        JSR CALCFPTR    ; USING R-, L- & B-PARMS, CALC
                                ; POSN OF FILE POINTER WANTED.


                                ; =================================
                                ; WRITE RANGE-OF-DATA-BYTES
                                ; SUBFUNCTION HANDLER.
                                ; =================================

WRITERNG        JSR INCIOBUF    ; POINT A4L/H AT SOURCE BYTE.
                LDY #0          ; SET (Y) TO INDEX SOURCE BUF.
                LDA (A4L),Y     ; GET BYTE TO WRITE.
                JSR WRTDATA     ; PUT DATA BYTE IN DATA SEC BUF.
                                ; (WRITE DATA SEC BUF TO DISK IF
                                ; NECESSARY.  )
                JSR DECRWLEN    ; CHECK IF DONE WRITING.  IF NOT,
                                ; REDUCE COUNTER FOR # OF BYTES
                                ; LEFT TO WRITE.
                JMP WRITERNG    ; GO BACK TO WRITE NEXT DATA BYTE.


                                ; =================================
                                ; SUBROUTINE WHICH WRITES BYTE.
                                ; =================================

WRTDATA         PHA             ; SAVE BYTE TO WRITE ON STK.
                JSR NXTDATRD    ; READ NXT DATA SEC BUF IF NEEDED.
                PLA             ; GET DATA BYTE TO WRITE OFF STK.
                STA (A4L),Y     ; PUT DATA BYTE IN DATA SEC BUF.
                LDA #%01000000  ; SET BIT6 TO SIGNAL DATA SEC BUF
                ORA UPDATFLG    ; HAS CHANGED & THEREFOR, THE DISK
                STA UPDATFLG    ; REQUIRES UPDATING.
                JSR INCREC      ; EITHER INC THE RECORD NUMBER OR
                                ; INC THE BYTE OFFSET INTO RECORD.
                JMP INCFILPT    ; INC THE BYTE OFFSET INTO THE
                                ; CURRENT DATA SECTOR.  IF AT THE
                                ; END OF SECTOR, INC THE OFFSET
                                ; INTO THE ENTIRE FILE INSTEAD.


                                ; =================================
                                ; LOCK & UNLOCK FUNCTION HANDLERS.
                                ; =================================

FNLOCK          LDA #$80        ; SET HI BIT IN LOCK/UNLOCK MASK.
                STA LOKUNMSK
                BNE COMNLOCK    ; ALWAYS.


FNUNLOCK        LDA #0          ; MAKE SURE HI BIT CLR IN LOKUNMSK
                STA LOKUNMSK

COMNLOCK        JSR COMNOPEN    ; LOCATE THE FILE WITH THE SAME
                                ; NAME & OPEN IT.
                LDX CURDIRNX    ; (X) = INDEX TO FILE DESCRIPTION
                                ; ENTRY IN THE DIRECTORY SEC BUF.
                LDA FIL1TYPE,X  ; GET OLD FILE TYPE.
                AND #$7F        ; SHUT HI BIT OFF.
                ORA LOKUNMSK    ; MERGE WITH LOKUNMSK TO SET OR CLR
                                ; (LOCK OR UNLOCK) HI BIT.
                STA FIL1TYPE,X  ; STICK MODIFIED FILE TYPE BACK IN
                                ; FILE DESCRIP PART OF DIR SEC BUF
                JSR WRDIRECT    ; WRITE UPDATED DIREC SEC TO DISK.

TOOKFMXT        JMP GOODFMXT    ; EXIT FM CLEANLY. EVENTUALLY RTNS
                                ; TO AFTRFUNC ($A6AB) LOCATED IN
                                ; THE FMDRIVER ROUTINE ($A6A8).


                                ; =================================
                                ; POSITION FUNCTION HANDLER.
                                ; =================================

FNPOSN          JSR CALCFPTR    ; USE R-, B- & L-PARAMETERS TO CALC
                                ; POSITION OF FILE POINTER WANTED.
                JMP GOODFMXT    ; EXIT FILE MANAGER CLEANLY.
                                ; EVENTUALLY RETURNS TO AFTRFUNC
                                ; ($A6AB) LOCATED IN THE FMDRIVER
                                ; ROUTINE ($A6A8).


                                ; =================================
                                ; VERIFY FUNCTION HANDLER.
                                ; =================================

FNVERIFY        JSR COMNOPEN    ; LOCATE FILE WITH SAME NAME
                                ; & OPEN IT.
VRFYREAD        JSR NXTDATRD    ; READ NEXT DATA SEC IN.  (ASSUME
                                ; DATA SEC WE WANT NOT PRESENTLY
                                ; IN MEMORY.)
                ifelse(eval(VERSION < 320),1,`
                BCS TOOKFMXT2
                ',`
                BCS TOOKFMXT    ; END OF FILE DETECTED - EXIT FM.
                                ; EVENTUALLT RETURNS TO AFTRFUNC
                                ; ($A6AB) LOCATED IN THE FMDRIVER
                                ; ROUTINE ($A6A8).
                ')
                INC FILPTSEC    ; KICK UP FILE POINTER POSN
                BNE VRFYREAD    ; AND THEN GO BACK TO READ NEXT
                INC FILPTSEC+1  ; DATA SECTOR UNTIL ENCOUNTER END
                JMP VRFYREAD    ; OF FILE MARKER.


                ifelse(eval(VERSION < 320),1,`
TOOKFMXT2       JMP GOODFMXT
                ')
                                ; =================================
                                ; DELETE FUNCTION HANDLER.
                                ; =================================

FNDELETE        JSR COMNOPEN    ; LOCATE FILE WITH SAME NAME AND
                                ; THEN OPEN IT.
                LDX CURDIRNX    ; (X) = INDEX TO FILE DESCRIPTION
                                ; IN DIRECTORY SECTOR BUFFER.
                LDA FIL1TYPE,X  ; GET FILE TYPE FROM DIR SEC BUF.
                BPL ALTRNTRY    ; BRANCH IF FILE NOT LOCKED.
                JMP FILELOKD    ; ERR - CANT DELETE A LOCKED FILE
                                ; SO GO HANDLE ERR & EXIT.

                                ; GET TRK # OF FILES 1ST T/S LIST
                                ; FROM FILE DESCRIP ENTRY IN DIR
                                ; SEC BUF.  PUT IT IN THE WRK AREA
                                ; & WRITE IT OVER THE LAST CHAR
                                ; POSN IN THE APPROPRIATE FILE
                                ; NAME FIELD IN DIRECTORY SECTOR.

ALTRNTRY        LDX CURDIRNX    ; (X) = INDEX TO FILE DESCRIPTION
                                ; ENTRY IN DIRECTORY SEC BUFFER.
                LDA FIL1TSTK,X  ; GET TRK # OF 1ST T/S LIST SEC.
                STA TSL1TRK     ; COPY IT INTO WORK AREA & LAST CHR
                STA FIL1NAME+29,X
                                ; POSN OF FILE NAME FLD DSCRP
                LDA #$FF        ; REPLACE ORIG TRK # OF T/S LIST
                STA FIL1TSTK,X  ; WITH $FF TO SIGNAL FILE DELETED.
                LDY FIL1TSSC,X  ; PUT SEC # OF FILES 1ST T/S LST
                STY TSL1SEC     ; IN THE WORK AREA.
                JSR WRDIRECT    ; WRITE MODIFIED DIREC SEC TO DISK.

                                ; READ T/S LIST SEC INTO T/S LIST BUF.
                                ; (NEED THIS INFO SO KNOW WHAT SECS
                                ; TO RELEASE.)

                CLC             ; (C)=0, SIGNAL 1ST T/S LIST SEC.
RDTS4DEL        JSR READTS      ; GO READ IN T/S LIST.
                BCS DONEDEL     ; BRNCH IF JUST READ LAST T/S LIST
                                ; SEC ASSOCIATED WITH THIS FILE.
                JSR SELTSBUF    ; POINT A4L/H AT T/S LIST SECTOR
                                ; BUFFER.(GET ADR FRM FM PRM LST.)

                                ; FREE ALL SECS LISTED IN THE T/S
                                ; LIST THAT CURRENTLY OCCUPIES THE
                                ; T/S LIST BUFFER.

                LDY #12         ; 1ST DATA SEC PAIR LISTED IS
                                ; OFFSET 12 BYTES FROM START OF
                                ; T/S LIST BUFFER.
DELFREE         STY CURDIRNX    ; SET (Y) = INDEX TO THE DATA PAIR
                                ; LISTED IN THE T/S LIST BUFFER.
                LDA (A4L),Y     ; GET TRK # OF DATA SEC.
                BMI BYPASDEL    ; NEG TRK # ILLEGAL. (CAN USE AS
                                ; A PROTECTION SCHEME.)
                BEQ BYPASDEL    ; TRK # OF 0 = NO MORE DATA SECS
                                ; LISTED IN CURRENT T/S LIST.
                PHA             ; SAVE TRK # OF DATA SEC ON STK.
                INY             ; GET SEC # OF DATA SEC.
                LDA (A4L),Y
                TAY             ; CONDITION (Y)=SEC & (A)=TRK FOR
                PLA             ; ENTRY INTO ROUTN TO FREE UP SECS.
                JSR FREESEC     ; FREE UP SECTOR FROM DELETED FILE
BYPASDEL        LDY CURDIRNX    ; SET (Y) TO INDEX START DATA PAIR.
                INY             ; KICK UP (Y) TO PT AT NXT DATA PR.
                INY
                BNE DELFREE     ; IF (Y) < > 0, THEN NEVER RAN OFF
                                ; END OF T/S LIST SEC BUF YET, SO
                                ; GO CHK IF MORE DATA PRS TO DO.

                                ; FREE UP SECTOR CONTAINING THE
                                ; T/S LIST.

                LDA CURTSTRK    ; TRK # OF CURRENT T/S LIST SEC.
                LDY CURTSSEC    ; SEC # OF CURRENT T/S LIST SEC.
                JSR FREESEC     ; GO FREE UP THE T/S LIST SEC.
                SEC
                BCS RDTS4DEL    ; ALWAYS.

                                ; DONE FREEING UP ALL SECTORS
                                ; ASSOCIATED WITH THIS FILE, SO
                                ; NOW WRITE MODIFIED VTOC BACK
                                ; TO THE DISK.

DONEDEL         JSR WRITVTOC    ; WRITE UPDATED VTOC TO DISK.
                JMP GOODFMXT    ; GO EXIT FM CLEANLY.  EVENTUALLY
                                ; RTNS TO AFTRFUNC ($A6AB) LOCATED
                                ; IN THE FMDRIVER ROUTINE ($A6A8).


                                ; =================================
                                ; SUBROUTINE TO FREE UP A SECTOR
                                ; IN A TRKMAP OF THE VTOC.
                                ; =================================

FREESEC         SEC             ; SET CARRY SO FREE UP PRESENT SEC
                                ; WHEN START ROTATING ASSIGNMENT
                                ; MAP.
                JSR SUB2FREE    ; ADJUST ASSIGNMENT MAP TO FREE UP
                                ; SEC BY SETTING BIT CORRESPONDING
                                ; TO SEC #.  NEXT, MERGE ASIGNMAP
                                ; WITH THE APPROP BIT MAP IN VTOC.
                LDA #0          ; ZERO OUT ASIGNSEC, ASIGNTRK
                ifelse(eval(VERSION < 330),1,`
                LDX #3
                ',`
                LDX #5          ; & ASIGNMAP (6 BYTES) IN WRK AREA
                ')
RELEASEC        STA ASIGNSEC,X
                DEX
                BPL RELEASEC
                RTS


                                ; =================================
                                ; CATALOG FUNCTION HANDLER.
                                ; =================================

FNCATLOG        JSR ZWRKAREA    ; INITIALISE THE FM WORK AREA.
                LDA #$FF        ; ALLOW ANY VOL TO BE CATALOGED.
                STA VOLWA       ; (WHEN RWTS LATER ENTERED, THIS
                                ; VALUE IS EORED WITH  #$FF TO
                                ; SIMULATED A COMPLEMENTED VOL #
                                ; OF 0 (IE. #$FF EOR #$FF = #$00).
                                ; AFTER RWTS READS THE ADDRESS
                                ; CHECKSUM, IT CHECKS TO SEE IF THE
                                ; CORRECT VOL # WAS READ OFF THE
                                ; DISK.  IF THE COMPLEMENT OF THE
                                ; VOL # IS 0, OR IF IT MATCHES THE
                                ; VOL # READ OFF DSK, EXECUTION
                                ; PROCEEDS AS IF THE CORRECT VOL
                                ; WAS FOUND.)
                JSR READVTOC    ; READ VOLUME TABLE OF CONTENTS.
                LDA #22         ; SET INDEX TO ALLOW 22 SCREEN
                STA SCRNSRCH    ; LINES BETWEEN PAUSES.

                                ; PRINT 2 <CR>S & THE WORDS
                                ; "DISK VOLUME".
                JSR CRCATLOG    ; PRT <CR> & TEST IF PAUSE NEEDED.
                JSR CRCATLOG    ; DO IT AGAIN.
                LDX #11         ; 12 CHARS TO PRT (11 TO 0).
PRDSKVOL        LDA DSKVOLUM,X  ; GET CHAR OF REVERSE STRING.
                JSR COUT        ; PRINT CHAR.
                DEX
                BPL PRDSKVOL    ; MORE CHARS IN STRING.
                STX A5H         ; NONSENSE INSTRUCTION (BECAUSE THE
                                ; HI BYTE IS NOT USED IN THE
                                ; PRVOLNMB ROUTINE ($AE42).)
                LDA IBSMOD      ; GET VOL# FOUND (FROM RWTSS IOB)
                STA A5L         ; AND PUT IT IN A5L.
                JSR PRVOLNMB    ; GO PRINT VOL # (BUGGY ROUTINE).
                JSR CRCATLOG    ; PRT <CR> & TEST IF PAUSE NEEDED.
                JSR CRCATLOG    ; DO IT AGAIN.

                                ; READ DIRECTORY SECTOR INTO THE
                                ; DIRECTORY SECTOR BUFFER.

                CLC             ; (C)=0, READ 1ST DIR SEC.
                                ; (C)=1, READ NEXT DIR SEC.
RDDIRSEC        JSR RDDIRECT    ; GO READ DIRECTORY SECTOR.
                BCS TOFMXTOK    ; RAN OUT OF DIR SECS SO GO EXIT.
                LDX #0          ; INITIALIZE INDEX INTO DIR SEC.

                                ; ANALYZE THE TRACK NUMBER.

DESCRPTK        STX CURDIRNX    ; SAVE NDEX TO ENTRIES IN DIR SEC.
                LDA FIL1TSTK,X  ; TRK # OF FILES 1ST T/S LIST
                                ; (FROM THE FILES DESCRIPTION IN
                                ; THE DIRECTORY SECTOR).
                BEQ TOFMXTOK    ; TRK # = 0, SO NO MORE ENTRIES IN
                                ; CURRENT DIRECTORY BUFFER.
                BMI NXDESCRP    ; TRK#=$FF=DELETED FILE SO SKIP IT

                                ; CHK FILE STATUS & PRINT LOCKED
                                ; SYMBOL ("*") OR SPACE.

                LDY #HICHAR(` ') ; DEFAULT (Y)=<SPC> N CASE NOT LCK
                LDA FIL1TYPE,X  ; GET FILE TYPE.
                BPL PRLOCODE    ; HI BIT CLR SO FILE UNLOCKED.
                LDY #HICHAR(`*') ; RESET (Y) = LOCKED SYMBOL.
PRLOCODE        TYA             ; EITHER PRINT "*" OR <SPC>.
                JSR COUT

                                ; PRINT CHAR CODE FOR FILE TYPE &
                                ; A TRAILING <SPC>.

                LDA FIL1TYPE,X  ; GET FILE TYPE AGAIN & MAKE SURE
                ifelse(eval(VERSION < 320),1,`
                AND #%00000111
                LDY #3
CHRTYPIX        LSR
                ',`
                AND #%01111111  ; HI BYTE IS OFF SO CAN INDEX TBL
                                ; THAT CONTAINS SYMBOLS FOR TYPES.
                LDY #7          ; SET (Y) TO INDICATE 7 RELEVANT
                                ; BITS AFTER SHIFT OUT HI BIT.
                ASL             ; THROW AWAY HI BIT.
CHRTYPIX        ASL             ; SHIFT REST OF BITS UNTIL HI SET.
                ')
                BCS PRTFTYPE    ; # OF SHIFTS TO SET (C) DESIGNATES
                                ; INDEX TO TYPE CHAR TABLE.
                DEY             ; REDUCE COUNT OF SHIFTS.
                BNE CHRTYPIX    ; NO SET BITS ENCOUNTERED YET, SO
                                ; GO BACK TO DO MORE SHIFTS.
PRTFTYPE        LDA FTYPETBL,Y  ; GOT A SET BIT SO NOW GET CHAR
                                ; FROM TABLE OF TYPE SYMBOLS.
                JSR COUT        ; PRINT TYPE SYMBOL.
                LDA #HICHAR(` ') ; PRINT TRAILING <SPC>.
                JSR COUT

                                ; PRINT FILE SIZE (EXPRESSED IN
                                ; TERMS OF SECTORS).

                LDA FIL1SIZE,X  ; GET LOW & HI BYTES OF FILE SIZE
                STA A5L         ; (IN SECTORS) FROM FILE DESCRIP
                LDA FIL1SIZE+1,X
                                ; IN CUR DIR SEC & STICK THEM IN
                STA A5H         ; IN A5L/H.
                JSR PRVOLNMB    ; PRINT FILE SIZE.
                                ; *** NOTE *** - ROUTINE IS BUGGY.
                                ; (DOESNT USE HI BYTE, SO FILES
                                ; > 255 SECS LONG ARE EXPRESSED
                                ; AS 256 MOD.)
                LDA #HICHAR(` ') ; PRINT <SPC> AFTER SIZE.
                JSR COUT

                                ; PRINT THE FILE NAME.

                INX             ; KICK (X) UP BECAUSE NAME STARTS AT
                INX             ; 4TH BYTE FROM START OF FILE
                INX             ; DESCRIPTION ENTRY.
                LDY #29         ; COUNTER FOR 30 CHRS/NAME (0-29).
PRTFNAME        LDA FIL1TSTK,X  ; GET CHR FOR FILE NAME & PRINT IT.
                JSR COUT        ; (P.S.  BECAUSE THE OUTPUT HOOK
                                ; STILL PTS TO DOSS OUTPUT HNDLR,
                                ; CTRL-D AND A SUBSUBSEQUENT DOS
                                ; CMD CAN BE EMBEDDED IN THE FILE
                                ; NAME AS A PROTECTION SCHEME.)
                INX             ; KICK UP INDEX INTO FILE DESCRIP.
                DEY             ; REDUCE THE CHARACTER COUNTER.
                BPL PRTFNAME    ; BRANCH IF MORE CHARS TO PRT.
                JSR CRCATLOG    ; PRT <CR> AFTR NAME, TEST FOR PAUS.

                                ; KICK UP INDEX INTO CURRENT DIRECTORY
                                ; SECTOR BUF TO PT AT START OF NEXT
                                ; FILE DESCRIPTION ENTRY.

NXDESCRP        JSR NXPLUS35    ; ADD 35 BYTES TO INDEX SO IT PTS
                                ; TO NEXT ENTRY IN CURRENT DIR SEC.
                BCC DESCRPTK    ; (C) = 0, SO GO LOOK FOR MORE
                                ; ENTRIES IN THIS PARTICULAR
                                ; DIRECTORY SECTOR.
                BCS RDDIRSEC    ; (C) = 1, SO THERE ARENT MORE
                                ; ENTRIES IN THIS DIRECT SECTOR.
                                ; THERE4, GO BACK TO READ IN
                                ; ANOTHER DIRECTORY SECTOR.


                                ; =================================
                                ; EXIT FILE MANAGER CLEANLY.
                                ; =================================

TOFMXTOK        JMP GOODFMXT    ; EVENTUALY RETURNS TO AFTRFUNC
                                ; ($A6AB) LOCATED IN THE FMDRIVER
                                ; ROUTINE ($A6A8).


                                ; =================================
                                ; PRINT <CR> & CHK FOR PAUSE.
                                ; =================================

CRCATLOG        LDA #CR         ; PRINT A <CR>.
                JSR COUT
                DEC SCRNSRCH    ; DEC INDEX TO SEE IF PAUSE NEEDED.
                BNE CRCATRTN    ; PAUSE NOT REQUIRED.
                JSR RDKEY       ; PAUSE SO INFO CAN BE ABSORBED
                LDA #21         ; BEFORE SCROLLED OFF SCRN.
                STA SCRNSRCH    ; RESET INDEX FOR FRESH SCRN INFO.
CRCATRTN        RTS


                                ; =================================
                                ; CONVERT 1 HEX BYTE TO 3-DIGIT
                                ; DECIMAL (WITH LEADING ZEROES IF
                                ; APPLICABLE).
                                ; =================================

                                ; NOTE:  THIS IS A BUGGY ROUTINE
                                ; - DOESNT USE HI BYTE SO NUMBERS
                                ; GREATER THAN 255 ARE EXPRESSED
                                ; AS 256 MOD.
                                ;
                                ; (PS. TO USE AS A STAND-ALONE
                                ; ROUTINE, LOAD A5L WITH HEX & CALL
                                ; PRVOLNMB.  DESTROYS (A) & (Y) REGS.
                                ; CONVERSION DONE BY SIMULATING
                                ; DIVISION VIA SUCCESSIVE SUBTRACTIONS
                                ; OF POWERS OF 10.)

PRVOLNMB        LDY #2          ; INDEX TO # OF CONVERSION FACTORS
                                ; AND DIGITS.
ZONSTK          LDA #0          ; INIT COUNT OF # OF SUBTRS DONE.
                PHA             ; SAVE COUNT ON STACK.
GETVNMB         LDA A5L         ; GET LOW BTE HEX AND CMP IT TO
                CMP BASETEN,Y   ; TABLE OF CONVERSION FACTORS.
                                ; CONVERSION TABLE CONTAINS POWERS
                                ; OF 10: 10^2=100, 10^1=10, 10^0=1
                BCC TONEGASC    ; BRANCH IF # < CONVERSION FACTOR.
                SBC BASETEN,Y   ; SUBTRACT THE CONVERSION FACTOR.
                STA A5L         ; STORE THE REMAINDER.
                LDA A5H         ; NONSENSE - NOT USED.
                SBC #0          ; NONSENSE - NOT USED.
                STA A5H         ; NONSENSE - NOT USED.
                PLA             ; GET COUNTER OF # OF SUBTS AND
                ADC #0          ; ADD (C). IF REMAIN. > CONVERSION
                PHA             ; FACTOR, ADD 1, ELSE ADD NOTHING.
                JMP GETVNMB     ; GO BACK TO DO MORE SUBTS WITH
                                ; SAME CONVERSION FACTOR.
TONEGASC        PLA             ; GET RESULT OF DIV (IE. WHOLE #
                                ; OF SUBTRACTIONS).
                ORA #$B0        ; CONVERT COUNT TO NEG ASCII CHAR.
                JSR COUT        ; PRT CHAR.
                DEY             ; 3 CHARS/VOL# (IE. 2 TO 0).
                BPL ZONSTK      ; (3 CONVERSION FACTORS.)
                RTS


                                ; =================================
                                ; COPY CONTENTS OF FM WORK BUFFER
                                ; (IN DOS CHAIN) TO FM WORK AREA
                                ; (NOT IN DOS CHAIN).
                                ; =================================

RSTRFMWA        JSR SELWKBUF    ; PT A4L/H AT FM WORK BUF.
                                ; (GET ADR FRM FM PARAMETER LIST.)
                LDY #0          ; ZERO OUT RETURN CODE IN FM PARM
                STY RTNCODFM    ; LIST TO SIGNAL NO ERORS.
STORFMWK        LDA (A4L),Y     ; COPY FM WORK BUF TO FM WORK AREA.
                STA FMWKAREA,Y
                INY
                CPY #45         ; 45 BYTES TO COPY (0 TO 44).
                BNE STORFMWK
                CLC             ; WHY?????
                RTS


                                ; =================================
                                ; COPY FM WORK AREA TO FM WORK BUF
                                ; (LOCATED IN CHAIN OF DOS BUFS).
                                ; =================================

CPYFMWA         JSR SELWKBUF    ; PT A4L/H AT FM WORK BUF (CHAIN).
                                ; (GET ADR FROM FM PARM LIST.)
                LDY #0          ; INITIALIZE INDEX.
STORWRK         LDA FMWKAREA,Y  ; COPY WORK AREA ----->  WORK BUF.
                STA (A4L),Y
                INY
                CPY #45         ; 45 BYTES TO COPY (0 TO 44).
                BNE STORWRK
                RTS


                                ; =================================
                                ; INIT FUNCTION HANDLER.
                                ; =================================

FNINIT          JSR ZWRKAREA    ; INIT THE FM WRK AREA (NON-CHAIN)
                LDA #4          ; OPCODE FOR FORMAT.

                                ; GO FORMAT THE DISK.

                JSR RWTSDRV1    ; CALL RWTS DRIVER TO FORMAT DISK.

                                ; PUT VOL#, # OF NEXT TRK TO BE
                                ; ASSIGNED & ASSIGNMENT DIRECTION
                                ; IN THE VTOC SECTOR BUFFER.

                LDA VOLWA       ; COMPLEMENTED VOL# (FRM WRK AREA)
                EOR #$FF        ; UNCOMPL. IT & PUT VOL# IN VTOC.
                STA VOLUSED
                LDA #CATTRK     ; USE TRK # 17 FOR CATALOG TRK.
                STA NXTRKUSE    ; SET CAT TRK AS NXT TRK TO ALLOC.
                                ; (NXTRKUSE IS LOCATED IN VTOC.)
                LDA #1          ; ALLOCATION DIRECTION = FORWARD.
                STA DRECTION

                                ; ZERO OUT VTOC FROM FRESECMAP TO
                                ; END OF SECTOR.  (NOTE:  THIS
                                ; ASSIGNS ALL TRACKS.)

                LDX #FRESECMAP-VTOCBUFF ; OFFSET INTO VTOC TO FRESECMAP.
                LDA #0
ZVTOC           STA VTOCBUFF,X  ; ZERO OUT A TRKMAP BYTE.
                INX
                BNE ZVTOC

                                ; FREE UP ALL TRKS EXCEPT TRKS 0,
                                ; 1, 2 & CAT TRK (#17).
                                ;
                                ; NOTE: DOS ONLY OCCUPIES SECTORS
                                ; 0 TO 4 ON TRK 2, BUT ROUTINE
                                ; TAKES REST OF SECS ON TRK 2 OUT
                                ; OF CIRCULATION.

                LDX #$03*4      ; OFFSET TO START OF TRKMAP3.
FREETRK         CPX #TRKPERDSK*4      ; OFFSET 1 PAST END OF TRKMAP34.
                BEQ FREEDTKS    ; DONE UP TO END OF TRKMAP34.
                LDY #3          ; INIT INDEX TO END OF FRETKMSK.
FREEIT          LDA FRETKMSK,Y  ; FREE MOST TRKMAPS.  (STARTING
                                ; WITH TRKMAP3).
                                ; NOTE:  NOT FREEING UP VTOC SEC
                                ; OR TRKS USED BY DOS.
                STA FRESECMAP,X   ; FREE TRKS BY PLACING FOLLOWING
                                ; BYTES IN @ TRKMAP:  "FF FF 00 00"
                INX             ; KICK UP INDEX TO BYTE IN TRKMAP.
                DEY             ; REDUCE INDEX TO FRETKMSK BECAUSE
                                ; LOADING FROM END OF FRETKMSK:
                                ; "00 00 FF FF".
                BPL FREEIT      ; (4BYTES/TRKMAP & 4BYTS/FRETKMSK)
                CPX #4*CATTRK   ; OFFSET TO START OF TRKMAP17
                                ; (IE. CATALOG TRACK).
                BNE FREETRK     ; HAVE WE FREED TRKS 3 TO 16 YET?
                                ; IF NOT - BRANCH.
                LDX #$12*4      ; OFFSET TO TRK 18 (IE. SKIP THE
                                ; CATALOG TRACK).
                BNE FREETRK     ; ALWAYS - GO FREE TRKS 18 TO 34.
FREEDTKS        JSR WRITVTOC    ; WRITE FREED UP VTOC TO DISK.

                                ; CLEAR OUT THE DIRECTORY SEC BUF.

                LDX #0
                TXA
CLRDIREC        STA DIRECBUF,X
                INX
                BNE CLRDIREC
                JSR PT2DIRBF    ; DESIGNATE DIR SEC BUF AS I/O BUF
                LDA #CATTRK     ; (A) = # OF CAT TRK.
                LDY SECPERTK    ; # OF SECS/TRK = 16.
                DEY
                DEY
                STA IBTRK       ; PUT CAT TRK IN IOB.

                                ; WRITE DIRECTORY SECS (TRK17,
                                ; SECS 15 TO 1) TO DISK.

SETLNKTK        STA DIRLNKTK    ; SET TRK/SEC VALS FOR LINK TO NEXT
SETLNKSC        STY DIRLNKSC    ; DIRECTORY SECTOR.
                INY             ; GET SEC # TO WRITE & PUT IT IN
                STY IBSECT      ; RWTSS IOB.
                LDA #2          ; WRITE OPCODE.
                JSR RWTSDRV1    ; WRITE DIRECTORY SEC TO DISK.
                LDY DIRLNKSC    ; SEC VAL OF NEXT DIR SEC TO WRITE.
                DEY             ; KICK IT DWN (WILL INC IT LATER).
                BMI DOIMAGE     ; DON'T DO SEC 0 BECAUSE THATS VTOC
                BNE SETLNKSC    ; GO WRITE SECS 2 TO 15.
                TYA
                BEQ SETLNKTK    ; GO BACK TO WRITE SEC 1 AND ZERO
                                ; OUT THE DIRECTORY SECTOR LINKS.

                                ; ROUTINE TO WRITE THE DOS
                                ; IMAGE ON TRACKS 0 TO 2.

DOIMAGE         JSR PRPWRDOS    ; GET READY TO WRITE DOS IMAGE.
                JSR WRDOSIMG    ; WRITE DOS IMAGE TO DISK.
                JMP GOODFMXT    ; EXIT FM CLEANLY. EVENTUALLY RTNS
                                ; TO AFTRFUNC ($A6AB) LOCATED IN
                                ; THE FMDRIVER ROUTINE ($A6A8).
                ifelse(eval(VERSION < 320),1,`
PRPWRDOS        LDA SUBCODFM
                STA IBBUFP+1
                LDA #0
                STA IBBUFP
                LDA VOLWA
                EOR #$FF
                STA IBVOL
                RTS
                ')

                                ; =================================
                                ; POINT A4L/H AT A SPECIFIC
                                ; SECTION OF A DOS BUFFER.
                                ; =================================

SELWKBUF        LDX #0          ; SELECT WORK BUFFER.
                BEQ PT2FMBUF    ; ALWAYS.
SELTSBUF        LDX #2          ; SELECT T/S LIST BUFFER.
                BNE PT2FMBUF    ; ALWAYS.
SELDABUF        LDX #4          ; SELECT DATA BUFFER.
PT2FMBUF        LDA WRKBUFFM,X  ; GET ADDR OF SELECTED BUF FROM
                STA A4L         ; FM PARM LST & PUT IT IN POINTER.
                LDA WRKBUFFM+1,X
                STA A4H
                RTS

                ifelse(eval(VERSION < 320),1,`
ZCURBUF         LDA #0
                TAY
L2E53           STA (A4L),Y
                INY
                BNE L2E53
                RTS
                ')

                                ; =================================
                                ; CHK IF DATA SEC BUF HAS CHANGED
                                ; SINCE LAST READ OR WRITE.
                                ; =================================

CKDATUP         BIT UPDATFLG    ; CHK BIT6 SO SEE IF CHANGED.
                BVS WRITDATA    ; TAKE BRANCH IF CHANGED.
                RTS


                                ; =================================
                                ; WRITE PRESENT DATA SECTOR BUFFER
                                ; TO THE DISK.  (UPDATES DISK SO
                                ; CAN READ IN NEXT DATA SEC WITH-
                                ; OUT OVERWRITING AND THEREFORE
                                ; LOSING PREVIOUS DATA.)
                                ; ---------------------------------

WRITDATA        JSR PRPDAIOB    ; PREPARE RWTSS IOB FOR WRITING
                                ; DATA SEC BUF TO DISK.
                LDA #2          ; OPCODE FOR WRITE CMD.
                JSR RWTSDRVR    ; CALL DRIVER TO WRT DATA SEC BUF.
                LDA #%10111111  ; SHUT BIT6 OFF IN UPDATE FLAG TO
                AND UPDATFLG    ; SIGNAL THAT THE DATA SECTOR BUF
                STA UPDATFLG    ; IS UP TO DATE.
                RTS


                                ; ===================================
                                ; CHK IF T/S LIST REQUIRES UPDATING.
                                ; (HAS T/S LIST BUF CHANGED SINCE
                                ; LAST READ OR WRITE?)
                                ; ===================================

CKTSUPDT        LDA UPDATFLG
                BMI WRITETS     ; IF BIT7 SET, UPDATING REQUIRED.
                RTS


                                ; =================================
                                ; UPDATE T/S LIST SEC BUF ON DISK.
                                ; ---------------------------------

WRITETS         JSR SETTSIOB    ; PREPARE RWTSS IOB FOR WRITING
                                ; T/S LIST BUFFER TO DISK.
                LDA #2          ; RWTSS WRITE OPCODE.
                JSR RWTSDRVR    ; CALL RWTS DRIVER TO WRITE T/S LST
                LDA #$7F        ; CLR BIT7 OF UPDATE FLAG TO SIGNAL
                AND UPDATFLG    ; THAT T/S LIST IS UP TO DATE.
                STA UPDATFLG
                RTS


                                ; =================================
                                ; PREPARE RWTSS IOB FOR READING
                                ; OR WRITING T/S LIST SEC.
                                ; =================================

SETTSIOB        LDA TSBUFFM     ; GET ADR OF T/S LIST BUF FROM
                STA IBBUFP      ; FM PARM LIST & DESIGNATE AS I/O
                LDA TSBUFFM+1   ; BUF IN RWTSS IOB.
                STA IBBUFP+1
                LDX CURTSTRK    ; SET (X)/(Y) = TRK/SEC OF CURRENT
                LDY CURTSSEC    ; T/S LIST SECTOR.
                RTS


                                ; =================================
                                ; READ T/S LIST SEC TO BUFFER.
                                ; =================================

READTS          PHP             ; SAVE CARRY ON STK.
                                ; (C)=0=READ 1ST T/S LIST SEC.
                                ; (C)=1=READ NEXT T/S LIST SEC.
                JSR CKTSUPDT    ; WRITE T/S LIST SEC BUF IF
                                ; UPDATING REQUIRED.  (IF T/S LST
                                ; BUF HAS CHANGED SINCE LAST READ
                                ; OR WRITE, THEN WRITE IT BACK TO
                                ; DISK SO DONT OVERWRITE BUF AND
                                ; LOSE INFO WHEN RD NEW T/S LIST.)
                JSR SETTSIOB    ; PREP RWTSS IOB FOR READING T/S
                                ; LIST SEC.
                JSR SELTSBUF    ; POINT A4L/H AT T/S LIST BUF.
                                ; (GETS ADR FROM FM PARM LIST.)
                PLP             ; GET SAVED (C) BACK FROM STK.
                BCS RDNXTTS     ; IF (C)=1, ALREADY READ 1ST T/S
                                ; LIST SEC, SO GO READ NEXT ONE.

                                ; READ 1ST T/S LIST SEC.
                                ; (CARRY WAS CLR.)

RDFIRSTS        LDX TSL1TRK     ; SET (X)/(Y)=TRK/SEC OF 1ST T/S
                LDY TSL1SEC     ; LIST SECTOR.
                JMP RDTSLST     ; GO READ T/S LIST SEC INTO BUF.
                                ; ---

                                ; READ NEXT T/S LIST SEC.
                                ; (CARRY WAS SET.)

RDNXTTS         LDY #1          ; INDEX INTO T/S LIST BUF.
                LDA (A4L),Y     ; TRK FOR LNK TO NEXT T/S LIST SEC.
                BEQ TSLNKZRO    ; LINK ZEROED OUT SO NO MORE
                                ; T/S LIST SECS FOR FILE.
                TAX             ; (X) = NEXT T/S LIST TRK.
                INY
                LDA (A4L),Y     ; SEC# FOR LNK TO NXT T/S LIST SEC.
                TAY             ; (Y) = NEXT T/S LIST SEC.
                JMP RDTSLST     ; GO READ NEXT T/S LIST SEC IN.
                                ; ---

                                ; T/S LINK ZEROED OUT SO
                                ; DECIDE IF ERROR OR NOT.

TSLNKZRO        LDA OPCODEFM    ; CHK R/W STATUS TO SEE IF WANT TO
                CMP #4          ; ADD ANOTHER T/S LIST.
                BEQ UPDATETS    ; WRITING - GO UPDATE LINK.
                SEC             ; WERE READING & LNK ZEROED OUT,
                RTS             ; SO RTN WITH (C)=1 SO GENERATE AN
                                ; ===             ;ERROR. (REMEMBER, PREVIOUSLY SET
                                ; RTN CODE TO A DEFAULT VALUE FOR
                                ; FILE-NOT-FOUND.) HOWEVER, IF
                                ; READING IS BEING DONE FOR AN
                                ; APPEND OR VERIFICATION, THEN THE
                                ; SET (C) JUST DENOTES THAT HAVE
                                ; REACHED THE END OF THE FILE.

                                ; WRITING & LNK ZEROED OUT SO MUST
                                ; ALLOCATE A NEW T/S LIST SECTOR.

UPDATETS        JSR ASGNTKSC    ; FIND & RESERVE TRK/SEC VALS FOR
                                ; A NEW T/S LIST SECTOR.

                                ; LINK THE NEW T/S LIST SECTOR TO
                                ; THE LAST T/S LIST SECTOR AND
                                ; THEN WRITE THE UPDATED VERSION
                                ; OF THE LAST T/S LIST SECTOR TO
                                ; THE DISK.

LNKOLDNW        LDY #2          ; OFFSET TO SEC# LINK BYTE.
                STA (A4L),Y     ; PUT NEW SEC VAL IN LINK AND THEN
                PHA             ; SAVE IT ON STK.
                DEY             ; OFFSET TO TRK BYTE OF LINK.
                LDA ASIGNTRK    ; PUT NEW TRK VAL IN LINK.
                STA (A4L),Y
                PHA             ; SAVE TRK VAL ON STK.
                JSR WRITETS     ; WRITE UPDATED T/S LIST TO DISK.

                                ; SET UP A BRAND NEW T/S LIST
                                ; SECTOR & WRITE IT TO DISK.

ZOUTTS          JSR ZCURBUF     ; ZERO OUT T/S LIST BUF.
                LDY #5          ; AT OFFSETS 5 & 6 INTO THE NEW T/S
                LDA RELASTP1    ; LIST, PUT REL SEC # PLUS 1 (IN
                STA (A4L),Y     ; RELATION TO THE ENTIRE FILE) OF
                INY             ; THE 1ST DATA PAIR THAT WILL BE
                LDA RELASTP1+1  ; DESCRIBED IN THIS NEW T/S LIST.
                STA (A4L),Y     ; (POSSIBLE VALUES ARE:  $007A,
                                ; 2*$007A, 3*$007A AND 4*$007A.
                PLA             ; GET (X)/(Y)=TRK/SEC VALS FOR THIS
                TAX             ; NEW T/S LIST SEC OFF STACK.
                PLA
                TAY
                LDA #2          ; WRITE OPCODE FOR RWTS.
                BNE RDWRTS      ; ALWAYS - GO WRITE T/S LIST SEC.
                                ; ---

                                ; SUBROUTINE TO READ T/S LIST SEC.

RDTSLST         LDA #1          ; READ OPCODE FOR RWTS.

                                ; COMMON TO READ/WRITE T/S LIST.

RDWRTS          STX CURTSTRK    ; NEW T/S LIST SECTOR TRK/SEC VALS
                STY CURTSSEC    ; BECOME CURRENT TRK/SEC VALS.
                JSR RWTSDRVR    ; CALL RWTS DRIVER TO READ OR WRITE
                                ; THE CURRENT T/S LIST SECTOR.

                                ; UPDATE FM WORK AREA BUF (NON-CHAIN).

                LDY #5          ; OFFSET INTO CURRENT T/S LIST BUF.
                LDA (A4L),Y     ; STORE REL SEC # OF 1ST DAT PAIR
                STA RELFIRST    ; THAT CAN BE DESCRIBED IN THIS
                                ; T/S LIST. (POSSIBLE VALUES ARE:
                                ; $0000, $007A, 2*$007A, 3*$007A
                                ; AND 4*$007A.)
                CLC             ; ADD THE MAXIMUM # OF DATA SECS
                ADC MXSCURTS    ; THAT CAN POSSIBLY BE DESCRIBED
                                ; IN THIS T/S LIST.
                STA RELASTP1    ; STORE THE MAXIMUM RELATIVE SEC #
                INY             ; (PLUS 1) OF THE LAST DATA PAIR
                                ; THAT CAN POSSIBLY BE DESCRIBED
                                ; IN THIS T/S LIST.
                LDA (A4L),Y
                STA RELFIRST+1
                ADC MXSCURTS+1
                STA RELASTP1+1  ; (RELASTP1/+1 IS ALWAYS SET TO
                                ; $007A BY FNOPEN.  (POSSIBLE VALS
                                ; ARE:  $007A, 2*$007A, 3*$007A,
                                ; 4*$007A OR 5*$007A.)
                CLC             ; RETURN WITH NO ERRORS SIGNAL.
                RTS


                                ; =================================
                                ; READ DATA SEC TO DATA SEC BUF.
                                ; =================================

READDATA        JSR PRPDAIOB    ; SET UP RWTSS IOB TO RD DATA SEC.
                LDA #1          ; READ OPCODE FOR RWTS.
                JMP RWTSDRVR    ; CALL RWTS DRIVER TO READ DAT SEC.


                                ; =================================
                                ; PREP RWTSS IOB TO READ
                                ; OR WRITE THE DATA SECTOR.
                                ; ==================================

PRPDAIOB        LDY DATBUFFM    ; GET ADR OF DATA SEC BUF FROM THE
                LDA DATBUFFM+1  ; FM PARM LIST & DESIGNATE IT AS
                STY IBBUFP      ; THE I/O BUF FOR RWTSS IOB.
                STA IBBUFP+1
                LDX CURDATRK    ; ENTER RWTS DRIVER WITH (X)/(Y)
                LDY CURDATSC    ; CONTAINING THE TRK/SEC VALUES OF
                RTS             ; THE DATA SECTOR.

                                ; =================================
                                ; READ THE VOLUME TABLE
                                ; OF CONTENTS (VTOC).
                                ; =================================

READVTOC        LDA #1          ; READ OPCODE FOR RWTS.
                BNE RDWRVTOC    ; ALWAYS.


                                ; =================================
                                ; WRITE VOLUME TABLE
                                ; OF CONTENTS (VTOC).
                                ; ---------------------------------

WRITVTOC        LDA #2          ; WRITE OPCODE FOR RWTS.

                                ; COMMON TO READ/WRITE VTOC.

RDWRVTOC        LDY ADRVTOC     ; GET ADR OF VTOC FRM FM CONSTANTS
                STY IBBUFP      ; TABLE & DESIGNATE IT AS THE I/O
                LDY ADRVTOC+1   ; BUF IN RWTSS IOB.
                STY IBBUFP+1
                LDX TRKWA       ; ENTER RWTS DRIVER WITH (X)/(Y)
                LDY #0          ; EQUAL TO TRK/SEC VALS OF VTOC.
                JMP RWTSDRVR    ; CALL DRIVER TO READ/WRITE VTOC.


                                ; =================================
                                ; READ A DIRECTORY SECTOR.
                                ; =================================

RDDIRECT        PHP             ; SAVE (C) ON STK:
                                ; (C) = 0 = READ 1ST DIR SEC.
                                ; (C) = 1 = READ NEXT DIR SEC.

                JSR PT2DIRBF    ; DESIGNATE DIR SEC BUF AS I/O BUF
                                ; IN RWTSS IOB.
                PLP             ; CHK IF DEALING WITH 1ST DIR SEC
                BCS RDNXTDIR    ; NO - GO READ NEXT DIR SEC.

                                ; READ THE FIRST DIRECTORY SECTOR.
                                ; (C) = 0

RDFIRDIR        LDY FIRDIRSC    ; (Y)/(X)=TRK/SEC VALS OF 1ST
                LDX FIRDIRTK    ; DIRECTORY SEC (FROM VTOC BUF).
                BNE DODIRRD     ; ALWAYS - GO READ IN DIREC SEC.

                                ; READ THE NEXT DIRECTORY SECTOR.
                                ; (C) = 1

RDNXTDIR        LDX DIRLNKTK    ; GET TRK OF NXT DIR SEC FROM LINK
                                ; IN CURRENT DIRECTORY SECTOR.
                BNE GETDIRLK    ; LINK NOT ZEROED OUT.
                SEC             ; LINK ZEROED OUT -EXIT WITH (C)=1
                RTS             ; TO SIGNAL NO MORE DIREC SECS.
                                ; ===

GETDIRLK        LDY DIRLNKSC    ; GET SEC OF NEXT DIR SEC FRM LINK
                                ; BYTES IN CURRENT DIRECTORY SEC.

                                ; CALL TO READ IN DIRECTORY SECTOR.

DODIRRD         STX CURDIRTK    ; SAV TRK/SEC VALS OF DIRECTORY WE
                STY CURDIRSC    ; ARE ABOUT TO READ SO THEY WILL BE
                                ; THE CURRENT DIRECTORY SEC VALS
                                ; FOR THE NEXT TIME AROUND.
                LDA #1          ; READ OPCODE FOR RWTS.
                JSR RWTSDRVR    ; CALL RWTS DRIVER TO DO THE READ.
                CLC             ; LINK DIDNT ZERO OUT SO SIGNAL
                RTS             ; MORE DIREC SECS EXIST & THEN XIT


                                ; =================================
                                ; WRITE THE DIRECTORY BUFFER.
                                ; =================================

WRDIRECT        JSR PT2DIRBF    ; DESIGNATE DIREC SEC BUF AS I/O
                                ; BUF IN RWTSS IOB.
                LDX CURDIRTK    ; ENTER RWTS DRIVER WITH (X)/(Y)
                LDY CURDIRSC    ; EQUAL TO THE TRK/SEC VALS OF THE
                                ; DIRECTORY SECTOR.
                LDA #2          ; WRITE OPCODE FOR RWTS.
                JMP RWTSDRVR    ; CALL DRIVER TO WRITE DIREC SEC.


                                ; ==================================
                                ; DESIGNATE DIRECTORY SECTOR BUFFER
                                ; AS I/O BUFFER IN RWTSS IOB.
                                ; ==================================

PT2DIRBF        LDA ADRDIRBF    ; GET ADDR OF DIREC SEC BUF FROM
                STA IBBUFP      ; FM CONSTANTS TBL & DESIGNATE IT
                LDA ADRDIRBF+1  ; AS I/O BUFFER IN RWTSS IOB.
                STA IBBUFP+1
                RTS


                                ; ==================================
                                ; READ/WRITE TRACK/SECTOR DRIVERS.
                                ; ==================================

RWTSDRVR        STX IBTRK       ; ENTER WITH (X) = TRK WANTED.
                STY IBSECT      ; (Y) = SEC WANTED.
RWTSDRV1        STA IBCMD       ; (A) = OPCODE FOR RWTS
                                ; RWTSDRV1 = ENTRY PT USED BY INIT
                                ; FUNCTION HNDLR WHEN FORMATTING.
                CMP #2          ; IS CMD A WRITE?  NOTE: THE "CMP"
                                ; CONDITIONS THE CARRY AS FOLLOWS:
                                ; (C)=0 =SEEK ($00) OR READ ($01)
                                ; (C)=1 =WRITE($02) OR FORMAT($03)
                BNE SKPWRSET    ; BRANCH IF NOT WRITING.
                ORA UPDATFLG    ; CONDITION UPDATE FLG TO DESIGNATE
                STA UPDATFLG    ; LAST OPERATION WAS A WRITE (FOR
                                ; NEXT TIME AROUND).

                                ; FINISH SETTING UP RWTSS
                                ; INPUT/OUTPUT BLOCK (IOB).

SKPWRSET        LDA VOLWA       ; PUT COMPLEMENTED VOL IN IOB.
                EOR #$FF
                STA IBVOL
                LDA SLOT16WA    ; PUT SLOT*16 IN IOB.
                STA IBSLOT
                LDA DRVWA       ; PUT DRIVE # IN IOB.
                STA IBDRVN
                LDA SECSIZWA    ; PUT SEC LENGTH IN IOB  (STANDARD
                STA IBSECSZ     ; SIZE OF #256 OR $0100 BYTES).
                LDA SECSIZWA+1
                STA IBSECSZ+1
                LDA #1
                STA IBTYPE
                LDY ADRIOB      ; SET (Y) & (A) TO POINT AT
                LDA ADRIOB+1    ; RWTSS IOB.


                                ; =================================
                                ; CALL RWTS
                                ; =================================

                ifelse(eval(VERSION < 320),1,`
                JSR RWTS
                ',`
                JSR ENTERWTS    ; SAV STATUS,SET INTERRUPT DISABLE
                                ; FLAG & CALL RWTS TO PERFORM TASK
                ')
                                ; (SEEK, READ, WRITE, FORMAT).
                LDA IBSMOD      ; GET VOL FOUND (FRM IOB) & PUT IT
                STA VOLFM       ; IN THE FM PARAMETER LIST.
                LDA #$FF        ; PUT VOL WANTED IN IOB.
                STA IBVOL       ; (USE 255 AS DEFAULT VAL FOR NEXT
                                ; TIME.  ACTUALLY USING 0 BECAUSE
                                ; #$FF EOR #$FF = $00).
                BCS ERRWTSDR    ; OPERATION WAS UNSUCCESSFUL.
                RTS             ; ABOVE BRANCH NEVER TAKEN WHEN
                                ; USING SEEK CMD, BECAUSE NO ERROR
                                ; CHKING ROUTINES ARE ENCOUNTERED
                                ; BY A LONE SEEK OPERATION.


                                ; =================================
                                ; OPERATION WAS NOT SUCCESSFUL.
                                ; =================================

ERRWTSDR        LDA IBSTAT      ; GET RWTSS ERROR CODE.

                                ; TRANSLATE RWTS ERROR CODE (A)
                                ; TO FM ERROR CODE (Y).  (DOS LATER
                                ; EMPLOYS FM ERROR CODE IN ROUTINE
                                ; USED TO PRINT ERROR MESSAGES.)

                LDY #7          ; SET (Y) FOR FM ERROR CODE.
                CMP #$20        ; VOL MISMATCH?
                BEQ SETFMERR    ; YES.
                LDY #4          ; NO.
                CMP #$10        ; WRITE PROTECTED?
                BEQ SETFMERR    ; YES.
                LDY #8          ; NO - MUST HAVE BEEN OTHER SO
SETFMERR        TYA             ; DESIGNATE IT AS GENERAL I/O ERR.
                JMP BADFMXIT    ; GO HANDLE ERROR.
                                ; ANY ERR ENCOUNTERED WHEN FORMAT-
                                ; TING IS TREATED AS AN I/O ERROR.


                                ; ==========================================
                                ; READ DATA SEC IF NECESSARY.
                                ; ==========================================

                                ; CHECK IF NEED TO READ THE NEXT DATA
                                ; SECTOR INTO THE DATA SECTOR BUFFER.
                                ;
                                ; - IS DATA SEC WE WANT ALREADY IN MEMORY?
                                ; - IF SO, DOES IT REQUIRE UPDATING?

                                ; IF THE SECTOR OFFSET INTO FILE (FILPTSEC)
                                ; EQUALS THE RELATIVE  SECTOR NUMBER OF THE
                                ; LAST SECTOR READ OR WRITTEN (RELPREV),
                                ; THEN THE SECTOR WE WANT IS PRESENTLY IN
                                ; MEMORY.  IF IT IS NOT IN MEMORY, READ IN
                                ; THE DATA SECTOR WANTED.  HOWEVER, FIRST
                                ; CHECK IF THE DATA SECTOR HAS CHANGED
                                ; SINCE THE LAST READ OR WRITE.  IF IT HAS,
                                ; THE DISK MUST BE UPDATED BEFORE WE READ
                                ; IN THE NEW DATA SECTOR SO WE DONT OVER-
                                ; WRITE THE DATA SECTOR BUFFER AND LOOSE
                                ; INFORMATION.
                                ; NOTE: - IF THIS SUBROUTINE IS CALLED
                                ; FROM A WRITE SUBFUNCTION AND
                                ; FILPTSEC IS NOT EQUAL TO RELPREV,
                                ; THEN THE DATA SECTOR BUFFER MUST
                                ; BE FULL AND SHOULD BE WRITTEN
                                ; TO THE DISK BEFORE ANY MORE
                                ; INFORMATION IS READ IN.
                                ; - IF THE FILE WAS JUST OPENED, THE
                                ; OPEN SUBFUNC SET FILPTSEC=#$0000
                                ; AND RELPREV=#$FFFF SO ALWAYS
                                ; FORCE READING OF A NEW DATA SEC
                                ; EVEN IF THE CORRECT SECTOR IS
                                ; ALREADY IN MEMORY.

NXTDATRD        LDA FILPTSEC    ; LAST SEC USED VS CUR SEC WNTD.
                CMP RELPREV
                BNE CKWCURDA    ; NOT SAME - WILL EVENTUALLY HAVE
                                ; TO READ NEW DATA SEC IN.
                LDA FILPTSEC+1  ; MAYBE SAME - CHK HI BYTES.
                CMP RELPREV+1
                BEQ XITNXDAT    ; SAME SO GO EXIT.


                                ; =====================================
                                ; WRITE THE DATA SECTOR IF NECESSARY.
                                ; -------------------------------------

                                ; DATA SEC WE WANT IS NOT PRESENTLY IN
                                ; MEMORY.  CHK IF NEED TO WRITE THE
                                ; CURRENT DATA SEC BUF BEFORE WE READ
                                ; IN THE WANTED DATA SECTOR.

CKWCURDA        JSR CKDATUP     ; CHK UPDATE FLG TO SEE IF DATA SEC
                                ; BUF HAS CHANGED SINCE LAST R/W.
                                ; IF IT HAS, WRITE DATA BUF TO DSK.

                                ; SHOULD THE CURRENT T/S LIST BE USED?
                                ; (THAT IS, SHOULD THE DATA SECTOR BE
                                ; LISTED IN THE PRESENT T/S LIST SECTOR?
                                ; IF NOT, THEN WILL NEED TO READ IN THE
                                ; CORRECT T/S LIST.)

                                ; IS THE SECTOR OFFSET INTO THE FILE OF
                                ; THE PRESENT DATA SECTOR LESS THAN THE
                                ; RELATIVE SECTOR NUMBER OF THE FIRST
                                ; DATA SECTOR THAT CAN BE DESCRIBED IN
                                ; THE T/S LIST PRESENTLY IN MEMORY?
                                ; (IF LESS, THEN NEED TO READ IN A
                                ; DIFFERENT T/S LIST SECTOR.)

CKCURTS         LDA FILPTSEC+1  ; SEC OFFSET INTO FILE ASSOCIATED
                CMP RELFIRST+1  ; WITH THE PRESENT DATA SECTOR
                                ; VERSUS THE RELATIVE SEC# OF THE
                                ; FIRST DATA SEC DESCRIBED IN THE
                                ; PRESENT T/S LIST.
                BCC NEEDNXTS    ; DATA SEC WANTED REPRESENTS A
                                ; SMALLER OFFSET INTO FILE SO NEED
                                ; A DIFFERENT T/S LIST.
                BNE CKCURTS1    ; SEC OFFSET OF WANTED DATA SEC IS
                                ; LARGER THAN THAT OF THE 1ST DAT
                                ; SEC THAT CAN BE DESCRIBED IN THE
                                ; PRESENT T/S LIST SO IT MAY STILL
                                ; BELONG TO THIS T/S LIST.
                LDA FILPTSEC    ; HI BYTES SAME -SO CMP LOW BYTES.
                CMP RELFIRST
                BCC NEEDNXTS    ; SEC OFFSET OF WNTD FILE IS LESS
                                ; SO READ IN A DIFFERENT T/S LIST.
                                ; (START BY READING THE FILES
                                ; FIRST T/S LIST.)

                                ; SECTOR OFFSET ASSOCIATED WITH THE DATA
                                ; SECTOR WE WANT IS EITHER GREATER THAN
                                ; OR EQUAL TO THE RELATIVE SECTOR OFFSET
                                ; ASSOCIATED WITH THE FIRST DATA SECTOR
                                ; THAT CAN BE DESCRIBED IN THIS T/S LIST.
                                ; THEREFORE, COMPARE THE SECTOR OFFSET
                                ; OF THE SECTOR WANTED WITH THE RELATIVE
                                ; SECTOR NUMBER (PLUS 1) OF THE LAST
                                ; SECTOR THAT CAN BE DESCRIBED IN THE
                                ; PRESENT T/S LIST.

CKCURTS1        LDA FILPTSEC+1  ; SEC OFFSET OF DATA SEC WANTED
                CMP RELASTP1+1  ; VS RELATIVE SEC # OF THE LAST
                                ; DATA SEC THAT CAN POSSIBLY BE
                                ; DESCRIBED IN THE PRES T/S LIST.
                BCC GETDATPR    ; SEC OFFSET ASSOC WITH DATA SEC
                                ; WANTED IS DESCRIBED IN THE
                                ; PRESENT T/S LIST.
                BNE NEEDNXTS    ; SEC OFFSET OF PRESENT DATA SEC
                                ; IS LARGER THAN THAT OF THE LAST
                                ; DATA SECTOR (PLUS 1) THAT CAN
                                ; POSSIBLY BE DESCRIBED IN THE
                                ; PRESENT T/S LIST, SO WE NEED A
                                ; NEW T/S LIST SECTOR.
                LDA FILPTSEC    ; HI BYTES SAME -SO CMP LOW BYTES.
                CMP RELASTP1
                BCC GETDATPR    ; SEC OFFSET ASSOCIATED WITH THE
                                ; DATA SECTOR WE WANT IS LESS THAN
                                ; THE RELATIVE SEC # (PLUS 1) OF
                                ; THE LAST DATA SECTOR THAT CAN
                                ; POSSIBLY BE LISTD IN THE PRESENT
                                ; T/S LIST.  (THERE4, THE WANTED
                                ; DATA SEC SHOULD BE DESCRIBED IN
                                ; THE PRESENT T/S LIST.)

                                ; THE DATA SECTOR WE WANT IS NOT
                                ; LISTED IN THE PRESENT T/S LIST,
                                ; SO WE MUST READ A DIFFERENT T/S
                                ; LIST SECTOR.  (NOTE: ROUTINE IS
                                ; ENTERED WITH (C) = 1 IF NEED A
                                ; HIGHERED NUMBERED T/S LIST.
                                ; IF (C) = 0, THEN THE FILE POINTER
                                ; WAS BACKED UP AND WE NEED A SMALLER
                                ; NUMBERED T/S LIST.)

NEEDNXTS        JSR READTS      ; READ IN THE NEXT (OR FIRST) T/S
                                ; LIST.  HOWEVER, FIRST CHECK THE
                                ; UPDATE FLAG TO SEE IF THE T/S
                                ; LST PRESENTLY IN MEMORY REQUIRES
                                ; UPDATING BEFORE WE READ IN THE
                                ; NEXT (OR FIRST) T/S LIST SECTOR.
                                ; IF UPDATING IS REQUIRED, WRITE
                                ; THE PRESENT T/S LIST.
                BCC CKCURTS     ; GO BACK & CHK IF THIS IS CORRECT
                                ; T/S LIST.
                RTS             ; RTN WITH (C)=1 TO SIGNAL THAT WE
                                ; ===             ;RAN OUT OF T/S LISTS WHILE
                                ; READING  OR TO DO GOOD EXIT IF
                                ; RAN OUT OF DATA WHILE VERIFYING
                                ; OR TO SIGNAL POSSIBLE ERROR WHILE
                                ; APPENDING.

                                ; KNOW DATA SEC SHOULD BE DESCRIBED
                                ; IN PRESENT T/S LIST SO NOW CALC
                                ; OFFSET INTO T/S LIST WHERE DATA
                                ; PAIR SHOULD BE LOCATED.

GETDATPR        SEC             ; CALC OFFSET TO DATA PAIR:
                LDA FILPTSEC    ; SEC OFFSET OF DATA SEC INTO FILE
                SBC RELFIRST    ; MINUS REL INDEX OF 1ST DATA SEC
                                ; DESCRIBED IN PRES T/S LIST.
                ASL             ; TIMES 2 BECAUSE 2 BYTES USED TO
                                ; DESCRIBE A DATA PAIR.
                ADC #12         ; ADD 12 BECAUSE 1ST DATA PAIR IS
                TAY             ; ALWAYS LISTED 12 BYTES FROM THE
                                ; START OF THE T/S LIST BUFFER.
                JSR SELTSBUF    ; POINT A4L/H AT THE T/S LIST BUF.
                LDA (A4L),Y     ; GET TRK # OF DATA SEC PAIR.
                BNE RDDATSEC    ; GO READ IN THE DATA SECTOR.

                                ; THE TRK# PART OF THE DATA SECTOR
                                ; PAIR LISTED IN THE T/S LIST WAS
                                ; ZERO.  THEREFORE, THERE ARE NO
                                ; MORE DATA SECTOR PAIRS DESCRIBED
                                ; IN THIS T/S LIST.

                LDA OPCODEFM    ; CHECK TO SEE IF WRITING OR NOT.
                CMP #4          ; OPCODE FOR WRITE.
                BEQ NEWPAIR     ; BRANCH IF WRITING.
                SEC             ; NOT WRITING & RAN OUT OF DATA
                RTS             ; SECTOR PAIRS DESCRIBED IN THE
                                ; ===             ;PRESENT T/S LIST, SO GO XIT WITH
                                ; CARRY SET TO SIGNAL NO MORE DATA.

                                ; SINCE WRITING & RAN OUT OF DATA
                                ; PAIRS, MUST ADD A NEW DATA PAIR
                                ; TO THE T/S LIST.

NEWPAIR         JSR NWDATKSC    ; ADD NEW DATA SEC PR TO T/S LIST,
                                ; ZERO OUT DATA SEC BUF & SET
                                ; UPDATE FLAG TO SIGNAL THAT BOTH
                                ; T/S LIST SEC & DATA SEC REQUIRE
                                ; UPDATING.
                JMP SETPREV     ; NOTE:  IF YOU FOLLOW THIS JUMP
                                ; ---             ;THRU, YOU MAY REALIZE THAT SOME-
                                ; TIMES WE EVENTUALLY EXIT THE
                                ; PRESENT FUNCTION WITHOUT WRITING
                                ; THE T/S LIST AND DATA SEC BUFS
                                ; BACK TO THE DISK. HOWEVER, AFTER
                                ; THE SUBFUNCTIONS ARE EXITED, THE
                                ; CLOSE FUNCTION EVENTUALLY TESTS
                                ; THE STATUS OF THE UPDATE FLAG
                                ; (UPDATFLG, $B5D5) & THEN WRITES
                                ; THE T/S LIST & DAT SEC BUFS BACK
                                ; TO THE DISK.

                                ; DATA SECTOR PAIR ASSOCIATED WITH
                                ; THE DATA SECTOR WANTED WAS
                                ; CONTAINED IN THE CURRENT T/S LIST,
                                ; SO NOW READ IN THE DATA SECTOR.

RDDATSEC        STA CURDATRK    ; SAVE TRK/SEC VALS OF CURRENT DATA
                INY             ; SECTOR IN THE WORK AREA.
                LDA (A4L),Y     ; SEC# OF CURRENT DATA SEC.
                STA CURDATSC
                JSR READDATA    ; GO READ IN THE DATA SECTOR.

                                ; SAVE SECTOR OFFSET INTO FILE
                                ; ASSOC WITH DATA SEC JUST READ.

SETPREV         LDA FILPTSEC    ; CURRENT SEC OFFSET INTO FILE.
                STA RELPREV     ; OFFSET IN FILE OF LAST DAT READ.
                LDA FILPTSEC+1
                STA RELPREV+1

                                ; EXIT READ NEXT DATA SECTOR ROUTINE.

XITNXDAT        JSR SELDABUF    ; POINT A4L/H AT DATA SEC BUF.
                LDY FILPTBYT    ; (Y)=BYT OFFSET INTO CUR DATA SEC.
                CLC             ; EXIT CLEANLY.
                RTS


                                ; =================================
                                ; DESIGNATE TRK/SEC VALS FOR NEW
                                ; DATA SECTOR & ADD THE NEW DATA
                                ; SECTOR PAIR TO THE T/S LIST.
                                ; =================================

NWDATKSC        STY SCRNSRCH    ; SAVE OFFSET TO DAT PR IN T/S LST.
                JSR ASGNTKSC    ; FIND & DESIGNATE AN AVAIL SEC.
                LDY SCRNSRCH    ; RETRIEVE OFFSET TO DATA PAIR.
                INY
                STA (A4L),Y     ; PUT NEW SEC# IN THE T/S LIST AND
                STA CURDATSC    ; WORK AREA.
                DEY
                LDA ASIGNTRK    ; PUT TRK# IN T/S LST & WORK AREA.
                STA (A4L),Y
                STA CURDATRK
                JSR SELDABUF    ; POINT A4L/H AT DATA SEC BUF.
ZOUTDAT         JSR ZCURBUF     ; ZERO OUT DATA SEC BUF.
                LDA #%11000000  ; SET BOTH BITS 6 & 7 IN FLAG TO
                ORA UPDATFLG    ; SIGNAL THAT BOTH DATA & T/S LIST
                STA UPDATFLG    ; SECTORS REQUIRE UPDATING.
                RTS


                                ; =================================
                                ; ADJUST RECORD # OR BYTE OFFSET
                                ; INTO A GIVEN RECORD.
                                ; =================================

                                ; THIS ROUTINE IS USED BOTH WHEN
                                ; READING AND WRITING. THE PATTERN
                                ; OF EXECUTION VARIES WITH THE
                                ; STRUCTURE OF A FILE.  HOWEVER
                                ; SOME FILES ARE TREATED AS IF
                                ; THEY HAVE ONE TYPE OF STRUCTURE
                                ; WHEN THEY ARE BEING READ AND
                                ; ANOTHER TYPE OF STRUCTURE WHEN
                                ; THEY ARE BEING WRITTEN:
                                ; - RANDOM ACCESS TEXT FILES HAVE
                                ; A FIXED RECORD LENGTH ASSIGNED
                                ; BY THE USER.
                                ; - SEQUENTIAL TEXT & APPLESOFT
                                ; FILES HAVE A RECORD LENGTH OF
                                ; ONE.
                                ; - DURING A LOAD OR BLOAD, BINARY
                                ; & APPLESOFT FILES ARE CONSIDERED
                                ; TO BE COMPOSED OF A COLLECTION
                                ; OF ONE-BYTE LONG RECORDS.
                                ; - WHEN SAVING OR BSAVING HOWEVER,
                                ; THESE FILES ARE TREATED AS IF
                                ; THEY CONSIST OF 1 LONG RECORD.

                                ; COPY THE RECORD NUMBER FROM THE
                                ; WRK AREA TO THE FM PARM LIST.

INCREC          LDX RECNMBWA    ; CURRENT RECORD #.
                STX RECNMBFM    ; RECORD # IN PARAMETER LIST.
                LDX RECNMBWA+1
                STX RECNMBFM+1

                                ; COPY CURRENT BYTE OFFSET INTO
                                ; RECORD FROM THE WORK AREA TO
                                ; THE FM PARAMETER LIST.

                LDX BYTOFFWA    ; GET OFFSET INTO RECORD (WRK AREA)
                LDY BYTOFFWA+1
                STX BYTOFFFM    ; STORE IT IN FM PARAMETER LIST.
                STY BYTOFFFM+1

                                ; INC THE BYTE OFFSET INTO RECORD.
                                ; IF IT EQUALS THE RECORD LENGTH,
                                ; THEN RESET THE OFFSET TO 0 AND
                                ; KICK UP THE RECORD NUMBER.

                INX
                BNE BYTVSREC
                INY
BYTVSREC        CPY RECLENWA+1  ; FIXED VAL VIA OPEN CMD, ELSE
                                ; L-PARM VIA SAVE OR BSAVE CMD
                                ; (FROM WORK AREA).
KIKOFF1         BNE SETBYTOF
                CPX RECLENWA
KIKOFF2         BNE SETBYTOF
                LDX #0          ; OFFSET INTO RECORD WAS SAME AS
                LDY #0          ; RECORD LENGTH SO PREP TO RESET
                                ; OFFSET INTO RECORD TO ZERO.
                INC RECNMBWA
NOKIKOFF        BNE SETBYTOF
                INC RECNMBWA+1

                                ; ON FALL THRU OR ENTRY FROM NOKIKOFF,
                                ; RESET THE OFFSET INTO THE RECORD
                                ; TO ZERO.  ON BRANCHED ENTRY FROM
                                ; KIKOFF1 OR KIKOFF2, INCREMENT THE
                                ; OFFSET INTO THE RECORD.

SETBYTOF        STX BYTOFFWA
                STY BYTOFFWA+1
                RTS


                                ; =================================
                                ; INCREMENT THE BYTE OFFSET INTO
                                ; THE CURRENT SECTOR.  IF AT THE
                                ; END OF THE SECTOR, INCREMENT THE
                                ; OFFSET INTO THE ENTIRE FILE
                                ; INSTEAD.
                                ; =================================

INCFILPT        INC FILPTBYT    ; KICK UP OFFSET INTO THE SECTOR.
                BNE INCPTRTN    ; NOT AT END OF CURRENT SECTOR.

                INC FILPTSEC    ; OFFSET INTO SEC WRAPPED AROUND,
                BNE INCPTRTN    ; SO AT END OF SEC, SO KICK UP THE
                                ; OFFSET INTO THE ENTIRE FILE.
                INC FILPTSEC+1  ; INC HI BYTE IF NECESSARY.
INCPTRTN        RTS


                                ; =================================
                                ; POINT A4L/H AT CUR DESTN/SOURCE
                                ; MEMORY LOCATION (CURIOBUF) FOR
                                ; READING/WRITING.
                                ; INC DESTN/SOURCE ADR FOR NEXT
                                ; TIME AROUND.
                                ; =================================

INCIOBUF        LDY CURIOBUF    ; GET ADR OF DEST'N/SOURCE LOCN
                LDX CURIOBUF+1  ; FROM FM PARM LIST.
                STY A4L         ; POINT A4L/H AT TARGET.
                STX A4H
                INC CURIOBUF    ; KICK UP ADR OF DESTN/SOURCE LOC
                BNE INCIORTN
                INC CURIOBUF+1
INCIORTN        RTS


                                ; =================================
                                ; REDUCE COUNT OF THE # OF BYTES
                                ; LEFT TO READ OR WRITE.  WHEN THE
                                ; COUNTER = 0, EXIT FILE MANAGER.
                                ; =================================

DECRWLEN        LDY LEN2RDWR    ; LENGTH TO READ OR LENGTH-1 TO
                                ; WRITE (FROM FM PARM LIST).
                BNE DECLENRW    ; MORE BYTES TO READ OR WRITE.
                LDX LEN2RDWR+1  ; LOW BYTE WAS 0, CHK HI BYTE.
                BEQ RWLEN0      ; COUNTER = 0 SO DONE READ/WRITE.
                DEC LEN2RDWR+1
DECLENRW        DEC LEN2RDWR    ; REDUCE COUNTER.
                RTS

RWLEN0          JMP GOODFMXT    ; DONE READ/WRITE, EXIT FM.


                                ; =========================================
                                ; LOCATE OR CREATE A FILE ENTRY IN THE
                                ; DIRECTORY BUFFER.
                                ; MAKE TWO SEARCHES IF NECESSARY:
                                ; - SEARCH1 - TRY TO LOCATE ENTRY WITH THE
                                ; SAME NAME AS THE FILE WANTED.
                                ; - SEARCH2 - COULDNT LOCATE ENTRY
                                ; CORRESPONDING TO THE FILE
                                ; WANTED SO CREATE A NEW ENTRY
                                ; IN THE 1ST AVAILABLE SPACE
                                ; IN THE DIRECTORY SECTOR IN
                                ; CASE THE CMD CAN CREATE A
                                ; NEW FILE.
                                ; =========================================

GETFNTRY        JSR READVTOC    ; READ IN VTOC SO CAN GET LINK TO
                                ; TRKMAPS & LINK TO 1ST DIR SEC.

                                ; PT A4L/H AT PRIMARY NAME BUF.

                LDA FNAMBUFM    ; GET ADR OF NAME BUF FROM THE
                STA A4L         ; FM PARM LIST & PUT IT IN A4L/H.
                LDA FNAMBUFM+1
                STA A4H


                                ; TRY TO FIND THE DIRECTORY SECTOR
                                ; CONTAINING THE NAME OF THE WANTED
                                ; FILE.  MAKE TWO SEARCHES IF NECESSARY.
                                ; ON THE FIRST SEARCH, TRY TO FIND THE
                                ; WANTED FILE NAME.  IF THAT DOESNT
                                ; WORK, DO A 2ND SEARCH TO LOCATE THE
                                ; 1ST AVAILABLE SPACE IN A DIRECTORY
                                ; SECTOR WHERE WE CAN STORE A NEW FILE
                                ; DESCRIPTION IN CASE THE CMD CAN CREATE
                                ; A NEW FILE.

                LDA #1          ; INIT SRCH COUNTER (SCRNSRCH), IN
SETSRCH         STA SCRNSRCH    ; FM SCRATCH SPACE FOR 2 SEARCHES
                                ; (1 = SEARCH1, 0 = SEARCH2).
                LDA #0          ; INIT OFFSET OF FILE DESCRIP FROM
                STA SECNXD1R    ; THE VERY 1ST DIRECTORY SEC.
                CLC             ; (C)=0=SIGNAL TO RD 1ST DIR SEC.
GETDIRSC        INC SECNXD1R    ; KICK UP OFFSET FROM 1ST DIR.
                                ; (ON FIRST ENTRY:  $00 ---> $01.)
                JSR RDDIRECT    ; GO READ  DIREC SEC INTO DIREC BUF
                BCS CHNGSRCH    ; LNK ZEROED OUT, NO MORE DIRECT
                                ; SECTORS, SO GO SWITCH SEARCHES.
                LDX #0
CKDIRTRK        STX CURDIRNX    ; OFFSET OF FILE DESCRIP INTO THE
                                ; CURRRENT DIRECTORY SECTOR.
                LDA FIL1TSTK,X  ; GET TRK# OF 1ST T/S LIST SEC
                                ; FOR A PARTICULAR FILE FROM THE
                                ; FILE DESCRIP ENTRY IN DIR SEC.
                BEQ CHRSRCHA    ; IF TRK#=0, NO MORE FILES IN THIS
                                ; DIRECTORY SECTOR.
                BMI CHRSRCHB    ; SKIP DELETED FILE.  (WHEN A FILE
                                ; IS DELETED, #$FF IS PUT IN BYTE
                                ; WHERE TRK# OF 1ST T/S LIST IS
                                ; USUALLY KEPT.)

                                ; COMPARE NAME FOUND IN THE FILE
                                ; DESCRIPTION ENTRY PORTION OF THE
                                ; DIRECTORY SECTOR WITH NAME WNTD.
                                ; (ON ENTRY, A4L/H POINTS AT THE
                                ; PRIMARY FILE NAME BUFFER.)

                LDY #0          ; INIT INDEX TO NAME BUFFER.
                INX             ; POINT (X) AT 1ST CHAR POSN IN
                INX             ; NAME FIELD OF DESCRIP ENTRY.
CMPNAMES        INX
                LDA (A4L),Y     ; GET CHAR OF NAME WANTED FROM THE
                                ; PRIMARY FILENAME BUFFER.
                CMP FIL1TSTK,X  ; CHAR OF NAME WANTED VS CHAR IN
                                ; DESCRIPTION ENTRY OF DIREC SEC.
                BNE DONTMTCH    ; CHARS (IE., NAMES) DONT MATCH.
                INY
                CPY #30         ; DONE ALL CHARS YET (0 TO 29)?
                BNE CMPNAMES    ; CHRS MTCH, BRANCH IF MORE TO CHK.
                LDX CURDIRNX    ; ALL CHARS MTCHD, SO NAMES MTCHD.
                CLC             ; RTN WITH (X) = INDEX TO FILE
                RTS             ; DESCRIP IN CURRENT DIRECTORY AND
                                ; ===             ;WITH (C)=0 TO SIGNAL THAT THE
                                ; CORRECT FILE DESCRIP WAS FOUND.

                                ; ADVANCE INDEX TO POINT AT THE NEXT
                                ; POTENTIAL FILE DESCRIPTION ENTRY.

DONTMTCH        JSR NXPLUS35    ; NAMES DIDNT MATCH, SO ADD 35 TO
                                ; INDEX TO POINT IT AT NEXT ENTRY.
                                ; (CHK TO MAKE SURE DONT INDEX
                                ; RIGHT OFF END OF DIRECTORY SEC.)
                BCC CKDIRTRK    ; MORE POTENTIAL FILE DESCRIPS TO
                                ; CHK IN THIS DIRECTORY SECTOR.
                BCS GETDIRSC    ; GO GET NEXT DIRECTORY SECTOR.
                                ; ---

                                ; IF JUST COMPLETED FIRST SEARCH,
                                ; GO BACK TO DO SECOND SEARCH.

CHRSRCHA        LDY SCRNSRCH    ; (1=SEARCH1, 0=SEARCH2)
                BNE SETSRCH     ; GO SWITCH TO SECOND SEARCH.

                                ; IF FIRST SEARCH, SKIP DELETED FILES.
                                ; IF SECOND SEARCH, FALL THRU TO STORE
                                ; THE (NEW) DESCRIPTION IN THE 1ST
                                ; UNUSED SPACE IN THE DIRECTORY.

CHRSRCHB        LDY SCRNSRCH    ; (1=SEARCH1, 0=SEARCH2)
                BNE DONTMTCH

                                ; COULDNT LOCATE THE FILE NAME IN THE
                                ; DIRECTORY DESCRIPTION ENTRIES, SO
                                ; BEGIN CREATING A NEW DESCRIPTION IN
                                ; THE FIRST AVAILABLE SPACE IN A
                                ; DIRECTORY (IN CASE CMD CAN LEGALLY
                                ; CREATE A NEW FILE).

NWDESCRP        LDY #0          ; INIT INDEX TO PRIMARY NAME BUF.
                INX             ; SET INDEX TO 1ST CHAR POSN IN
                INX             ; THE NAME FIELD OF FILE DESCRIP
SETNWNAM        INX             ; ENTRY SPACE N THE DIRECTORY SEC.
                LDA (A4L),Y     ; COPY CHAR FROM PRIMARY NAME BUF
                STA FIL1TSTK,X  ; TO DIREC DESCRIP ENTRY AREA.
                INY
                CPY #30         ; 30 CHARS IN NAME (0 TO 29).
                BNE SETNWNAM    ; BRANCH IF MORE CHARS TO COPY.
                LDX CURDIRNX    ; RTN WITH INDEX TO FILE DESCRIP
                SEC             ; IN CURRENT DIRECTORY SECTOR AND
                RTS             ; WITH (C)=1 TO SIGNAL A NEW ENTRY
                                ; WAS JUST CREATED.


                                ; =====================================
                                ; ADD 35 TO THE OFFSET TO POINT THE
                                ; INDEX AT THE NEXT FILE DESCRIPTION
                                ; ENTRY.  (CHECK TO MAKE SURE THAT WE
                                ; DONT INDEX RIGHT OFF THE END OF THE
                                ; DIRECTORY SECTOR.)
                                ; =====================================

NXPLUS35        CLC             ; ADD 35 TO THE INDEX.  (EACH FILE
                LDA CURDIRNX    ; DESCRIPTION IS 35 BYTES LONG.)
                ADC #35
                TAX             ; CHK IF MORE SPACE FOR ENTRIES IN
                CPX #245        ; CURRENT DIRECTORY.
                RTS             ; EXIT WITH (C) CONDITIONED:
                                ; (C)=0=MORE SPACE IN DIRECTORY.
                                ; (C)=1=RAN OFF END OF DIRECTORY.


                                ; ====================================
                                ; - IF FIRST SEARCH, SWITCH TO SECOND
                                ; SEARCH.
                                ; - IF SECOND SEARCH, LINK ZEROED OUT
                                ; (BECAUSE THERE ISNT ENOUGH ROOM
                                ; ON DISK FOR A NEW ENTRY.  THERE4,
                                ; GO EXIT WITH A DISK-FULL-ERROR
                                ; MESSAGE.
                                ; ====================================

CHNGSRCH        LDA #0          ; SET (A) = 0 SO WE CAN RESET
                                ; SCRNSRCH IF WE HAVE TO GO BACK TO
                                ; DO A SECOND SEARCH.
                LDY SCRNSRCH    ; (1=SEARCH1, 0=SEARCH2.)
                BNE SETSRCH     ; JUST DID FIRST SEARCH, SO NOW GO
                                ; BACK TO START SECOND SEARCH.
                JMP DISKFULL    ; EVEN THE SECOND SEARCH WAS
                                ; UNSUCCESSFUL, SO GO HANDLE A
                                ; DISK-FULL ERROR.


                                ; ========================================
                                ; ASSIGN TRK(S)/SEC(S) FOR THE NEW FILE.
                                ; ========================================

                                ; NOTE:  THIS ROUTINE USUALLY ASSIGNS
                                ; MORE SECTORS THAN ARE NEEDED.  EACH
                                ; TIME A TRK WITH ONE OR MORE FREE SECS
                                ; IS LOCATED, THE ENTIRE TRACK IS
                                ; IS ASSIGNED.  THE UNNEEDED SECS ARE
                                ; LATER RELEASED BY THE CLOSE COMMAND.
                                ;
                                ; NOTE:  DOS DOES NOT SUPPORT THE
                                ; ALLOCATION OF TRK 0 FOR A FILE.
                                ; INSTEAD, TRK 0 IS RESERVED FOR STORING
                                ; PART OF THE DOS IMAGE.  HOWEVER, AS
                                ; SHOWN BELOW, TRK 0 HAS SPECIAL
                                ; SIGNIFICANCE FOR THE FOLLOWING FLAGS:
                                ; ASIGNTRK = TRK# BEING ASSIGNED OR, IF
                                ; ASIGNTRK=0, THEN IT IS A
                                ; SIGNAL TO GET NEXT TRK TO
                                ; ASSIGN FROM THE VTOC.
                                ; TRK0YET = 0 = HAVENT ENCOUNTERED TRK0
                                ; YET.
                                ; = 1 = TRK0 HAS BEEN ENCOUNTERED.
                                ; (TRK 0 IS USED AS A REFERENCE POINT.
                                ; THE FIRST TIME TRK0 IS ENCOUNTERED,
                                ; TRK0YET IS SET TO 1.  THE NEXT TIME
                                ; TRK0 IS ENCOUNTERED, THE ENTIRE DISK
                                ; HAS BEEN SEARCHED.)


                                ; HAS A TRK ALREADY BEEN ASSIGNED FOR
                                ; THIS FILE?

ASGNTKSC        LDA ASIGNTRK
                BEQ PRPNWTRK    ; BRANCH IF NO TRK ASSIGNED YET.
                                ; (ALWAYS TAKEN 1ST TIME "JSR" TO
                                ; HERE FRM CREATNEW. HOWEVER, WHEN
                                ; WE LATER JSR TO HERE, ASIGNTRK
                                ; EQUALS THE TRK # FOR T/S LIST.)

                                ; A TRK WAS ALREADY ASSIGNED, SO NOW SEE
                                ; IF THERE ARE ANY FREE SECS WHICH WE
                                ; CAN USE ON THIS TRK.

ANYAVAIL        DEC ASIGNSEC    ; NEXT SECTOR BE ASSIGNED.
                BMI ASGNWTRK    ; IF DEC FROM $00 ==> #$FF, THEN
                                ; NO MORE FREE SECS ON THIS TRK.

                                ; CHECK IF ANY SECS ARE FREE.

                CLC             ; ROLL BITS IN THE 4-BYTES OF
                LDX #4          ; ASIGNMAP AS AS UNIT - ROLL THEM
                                ; BACK TO STANDARD POSITION.
ADJSTMAP        ROL ASIGNMAP-1,X
                                ; IF C=1, SEC ASSOC WITH ROLLED
                DEX             ; BIT POSN IS FREE TO BE ASSIGNED
                BNE ADJSTMAP    ; TO A NEW FILE.
                BCC ANYAVAIL    ; SEC NOT FREE - GO GET NEXT ONE.

                                ; FOUND A FREE SECTOR.

                INC FILENSEC    ; SEC WAS FREE, SO KICK FILE SIZE
                BNE XWITHFRE    ; UP BY 1 & RTN WITH FREE SEC# IN
                INC FILENSEC+1  ; (A) SO IT CAN LATER BE USED FOR
XWITHFRE        LDA ASIGNSEC    ; THE T/S LIST SECTOR.
                RTS             ; (ACTUALLY ONLY GOOD EXIT AVAIL.)
                                ; ===

                                ; PREPARE TO ASSIGN A NEW TRK (BECAUSE
                                ; ALL SECS WERE ASSIGNED ON THE LAST TRK.)

ASGNWTRK        LDA #0          ; SET SIGNAL TO ASSIGN NEW TRK.
                STA ASIGNTRK

                                ; CONTINUE PREPS TO ASSIGN NEW TRK
                                ; OR BEGIN PREPARATIONS TO ASSIGN
                                ; THE FIRST TRK.

PRPNWTRK        LDA #0
                STA TRK0YET     ; SIGNAL NOT ALL TRKS CHECKED YET.
                JSR READVTOC    ; READ IN THE VTOC TO FIND NEXT
                                ; TRK TO USE.

                                ; FIND & REASSIGN FREE SECS FOR THE
                                ; NEXT TRK.  CALC NEXT TRK TO ASIGN.

GETNWTRK        CLC
                LDA NXTRKUSE    ; GET NEXT TRK# TO ASSIGN.
                ADC DRECTION    ; DIRECTION (+1/-1) OF ASSIGNMENT.
                BEQ CKIFFULL    ; IF 0, GO SEE IF CHKD ALL TRKS.

                                ; IS THE TRACK NUMBER LEGAL?

                CMP TKPERDSK    ; # OF TRKS ON DISK (FROM VTOC).
                BCC CHK4FREE    ; BRANCH IF TRK # IS VALID.

                                ; TRK# TOO LARGE, SO REVERSE DIRECTN.

                LDA #$FF        ; (A) = -1.
                BNE SRCH4TRK    ; ALWAYS.

                                ; AT TRK0, SEE IF CHKD ALL TRKS.
                                ; IF AT TRK0 FOR 1ST TIME, SET
                                ; FLAG.  IF 2ND TIME AT TRK0, GO
                                ; ISSUE DISK-FULL-ERROR MESSAGE
                                ; (BECAUSE WE SEARCHED ALL TRKS &
                                ; DIDNT FIND ANY FREE SECTORS).

CKIFFULL        LDA TRK0YET
                BNE TODSKFUL    ; 2ND TIME = DISK FULL.

                                ; START THE SECOND SEARCH.

                LDA #1          ; SET FLAG TO INDICATE THAT THE
                STA TRK0YET     ; PENDING SEARCH WILL BE 2ND ONE.

                                ; START SEARCH AT CATALOG
                                ; TRK PLUS OR MINUS ONE.

SRCH4TRK        STA DRECTION    ; SET THE SEARCH DIRECTION.
                CLC             ; BEGIN THE SEARCH ONE TRACK AWAY
                ADC #CATTRK     ; FROM THE CATALOG TRK.

                                ; CHECK TRKS TRKMAP FOR FREE SECS.

CHK4FREE        STA NXTRKUSE
                STA ASIGNTRK
                TAY             ; IRRELEVANT.
                ASL             ; TRK*4 BECAUSE 4BYTES/TRK N TRKMAP.
                ASL
                TAY             ; INDEX FROM LAST BYTE OF FRESECMAP.
                LDX #4          ; INDEX TO THE ASIGNMAP.
                CLC             ; (C)=0, ASSUME NO FREE SEC AVAIL.
CPYTKMAP        LDA FRESECMAP+3,Y ; COPY BYTE FROM TRKMAP TO THE
                STA ASIGNMAP-1,X
                                ; ASSIGNMENT MAP.
                BEQ NXMAPBYT    ; 0 = SEC USED.

                                ; FOUND A FREE SECTOR.

                SEC             ; (C) = 1 = FREE SECTOR FOUND.
                LDA #0          ; PUT 0 IN TRKMAP TO REASSIGN ALL 8
                STA FRESECMAP+3,Y ; SECS REPRESENTED BY THIS BYTE.
                                ; (REMEMBER ONLY 2 BYTES OF TRKMAP
                                ; ACTUALLY REPRESENT SECS.  THE
                                ; OTHER 2 BYTES ARE DUDS.)
NXMAPBYT        DEY             ; REDUCE INDICES TO MAPS.
                DEX
                BNE CPYTKMAP    ; NOT DONE TRANSFERRING ALL BYTES
                                ; FROM TRKMAP TO ASIGNMAP YET.

                                ; CHECK IF FOUND A FREE SECTOR.

                BCC GETNWTRK    ; IF (C)=0,THEN NO FREE SECS FOUND
                                ; YET SO GO BACK TO GET A NEW TRK.
                JSR WRITVTOC    ; UPDATE THE VTOC ON THE DISK.
                LDA SECPERTK    ; RESET ASIGNSEC WITH # SECS/TRK.
                STA ASIGNSEC    ; (IE 1 GREATER THAN HIGHEST SEC#.)
                BNE ANYAVAIL    ; ALWAYS.


                                ; =================================
                                ; GO ISSUE DISK-FULL-ERROR MSG.
                                ; =================================

TODSKFUL        JMP DISKFULL    ; GO HANDLE ERROR.
I
                                ; =====================================
                                ; FREE UP SECTORS THAT WERE ASSIGNED
                                ; BUT NOT USED.
                                ; =====================================

                                ; WHENEVER, SOMETHING IS WRITTEN TO
                                ; THE DISK, THE WHOLE TRK IS ALLOCATED
                                ; IN THE VTOC WHETHER IT IS NEEDED OR
                                ; NOT.  THERE4, ONCE WE ARE DONE
                                ; WRITING, GO BACK & FREE UP THE
                                ; UNNEEDED SECTORS.

FIXMAP          LDA ASIGNTRK    ; HAS A TRK ALREADY BEEN ASSIGNED?
                BNE FREEXTRA    ; YES - GO FREE UP EXTRA SECS.
                RTS             ; NO - GO EXIT.

FREEXTRA        PHA             ; SAVE TRK # ON STK.
                JSR READVTOC    ; READ IN THE VTOC.
                LDY ASIGNSEC    ; SET (Y) = # OF NEXT SECTOR WHICH
                                ; COULD HAVE BEEN WRITTEN.
                PLA             ; GET ASSIGNMENT TRK# BAK FRM STK.
                CLC             ; DONT WANT TO FREE LAST SEC USED,
                                ; SO CLR (C) HERE SO DONT FREE IT
                                ; UP WHEN BEGIN ROTATING MAPS IN
                                ; SUB2FREE ROUTINE.
                JSR SUB2FREE    ; ADJUST ASIGNMAP TO FREE UP SECS
                                ; BY SETTING BIT CORRESPONDING TO
                                ; THE SEC #.  NEXT, MERGE ASIGNMAP
                                ; WITH THE APPROPRIATE TRK MAP IN
                                ; THE VTOC.
                LDA #0          ; NO MORE DEALLOCATIONS NEEDED.
                STA ASIGNTRK
                JMP WRITVTOC    ; WRITE CORRECTED VTOC BACK TO DSK.


                                ; ================================
                                ; SUBROUTINE TO FREE UP SECS THAT
                                ; WERE DELETED OR PREMATURELY
                                ; ASSIGNED BUT NOT USED.
                                ; ================================

                                ; THIS TRICKY LITTLE ROUTINE IS
                                ; EASY TO UNDERSTAND ONCE YOU
                                ; REALIZE THAT:
                                ; 1) UNLIKE THE VTOC, ASIGNMAP
                                ; DOES NOT CONTAIN UNNECESSARILY
                                ; ASSIGNED SECTORS.
                                ; 2) IF THE DISK WAS JUST WRITTEN
                                ; TO, ASIGNMAP MAP DOES NOT HOUSE
                                ; ANY NEWLY ASSIGNED SECS (EVEN
                                ; IF THOSE SECS ARE NECESSARY OR
                                ; NOT).
                                ; 3) SECS ARE NORMALLY ASSIGNED
                                ; IN DESCENDING ORDER.
                                ; 4) # OF RORS = # OF SECS THAT
                                ; NEED TO BE ASSIGNED.
                                ;
                                ; FOR EX:  IF LAST TRK HAD NEVER
                                ; BEEN ASSIGNED & ONLY SECS $0F
                                ; AND $0E WERE NEEDED, THEN ON
                                ; ENTRY TO SUB2FREE, THE 1ST 2
                                ; BYTES WOULD APPEAR AS FOLLOWS:
                                ; SEC NMB: CBA98765 43210FED
                                ; BIT VAL: 11111111 11111...
                                ; AFTER THE 1ST ROR, THE STATUS
                                ; OF SEC $0D IS DETERMINED BY THE
                                ; ENTRY STATUS OF THE CARRY (C):
                                ; SEC NMB: DCBA9876 543210FE
                                ; BIT VAL: C1111111 111111..
                                ; ON NEXT ROR, THE "CPY SECPERTK"
                                ; INSTRUC CONDITIONS (C) TO CLR
                                ; FOR SEC $0E:
                                ; SEC NMB: EDCBA987 6543210F
                                ; BIT VAL: 0C111111 1111111.
                                ; ON NEXT ROR, THE "CPY SECPERTK"
                                ; INSTRUC CONDITIONS (C) TO CLR
                                ; FOR SEC $0F:
                                ; SEC NMB: FEDCBA98 76543210
                                ; BIT VAL: 00C11111 11111111
                                ; WHEN ASIGNMAP IS MERGED WITH THE
                                ; CORRESPONDING TRKMAP IN THE VTOC,
                                ; THE SECS THAT WERE UNNECESSARILY
                                ; ASSIGNED IN THE VTOC ARE FREED.
                                ; (NOTE:  WHEN USED IN THE DELETE
                                ; FUNCTION, SUB2FREE IS REPEATEDLY
                                ; CALLED BY FREESEC TO FREE ONE
                                ; SECTOR AT A TIME.)

SUB2FREE        LDX #$FC        ; SET INDEX TO SHIFT 4 BYTES AS A
                                ; UNIT (#$FC --> #$FF).
STNDARD         ROR ASIGNMAP-$FC,X
                                ; 4 BYTES PER MAP FOR @ TRK.
                INX
                BNE STNDARD
                INY             ; WHEN (Y) = 16, ASIGNMAP IS BACK
                                ; IN STANDARD POSN.
                CPY SECPERTK    ; CONDITION (C) FOR NEXT SHIFT.
                BNE SUB2FREE
                ASL             ; TRK*4 TO INDEX TRKMAP.
                ASL
                TAY
                BEQ SUB2RTN     ; TRK VAL OF 0 NOT ALLOWED.

                                ; ASIGNMAP NOW REFLECTS TRUE SEC
                                ; ASSIGNMENT & IS IN STANDARD
                                ; POSITION.  THERE4, MERGE IT
                                ; WITH THE APPROPRIATE TRKMAP IN
                                ; THE VTOC TO FREE UP EXTRA SECS.

                LDX #4
MERGMAPS        LDA ASIGNMAP-1,X
                ORA FRESECMAP+3,Y ; DO THE MERGE.
                STA FRESECMAP+3,Y
                DEY             ; REDUCE INDICES.
                DEX
                BNE MERGMAPS
SUB2RTN         RTS


                                ; ==========================================
                                ; CALCULATE THE EXACT POSITION OF THE
                                ; 3-BYTE FILE POINTER:
                                ; ==========================================

                                ; - FILPTSEC = SECTOR OFFSET (IN LOW/HI
                                ; FORMAT) INTO THE ENTIRE FILE (2 BYTES).
                                ; - FILPTBYT = BYTE OFFSET INTO THE CURRENT
                                ; SECTOR (1 BYTE).
                                ;
                                ; ALL 3 BYTES BYTES DEFINE THE EXACT POSN
                                ; OF THE FILE POINTER VIA THE FOLLOWING
                                ; FORMULA:
                                ; (REC# * REC LENGTH) + BYTE OFFSET
                                ; INTO THE RECORD.
                                ; WHERE:
                                ; RECNMBFM=RECORD# FROM R-PARAMETER (SET
                                ; BY USER WHEN USING RANDOM
                                ; ACCESS FILES OR SIMPLY
                                ; INCREMENTED WHEN USING OTHER
                                ; FILE TYPES).
                                ; RECLENWA=RECORD LENGTH PARSED FROM
                                ; L-PARAMETER & ASSIGNED WITH
                                ; THE OPEN CMD (ELSE DEFAULTED
                                ; TO A SIZE OF 1).
                                ; BYTOFFFM=OFFSET INTO THE CURRENT RECORD
                                ; (SET BY USER WHEN USING THE
                                ; OPEN CMD OR OCCASIONALLY USED
                                ; WITH SEQUENTIAL FILES AS A
                                ; B-PARAMETER).
                                ; NOTE THAT YOU CAN ACTUALLY DIRECTLY
                                ; ACCESS ANY BYTE IN ANY FILE BY
                                ; BYPASSING THE COMMAND INTERPRETER AND
                                ; SETTING THE L-, B- AND R-PARAMETERS
                                ; HOWEVER YOU WANT.

CALCFPTR        LDA RECNMBFM    ; PUT RECORD # IN MULTIPLIER AND
                STA FILPTBYT    ; ALSO SAVE IT IN THE WORK AREA.
                STA RECNMBWA
                LDA RECNMBFM+1
                STA FILPTSEC
                STA RECNMBWA+1
                LDA #0          ; ZERO OUT HI ORDER BYTE OF SEC
                STA FILPTSEC+1  ; OFFSET INTO THE ENTIRE FILE.

                                ; CALCULATE:  REC # * REC LENGTH.
                                ; THIS ROUTINE IS JUST A BASIC
                                ; MULTPLICATION OF TWO 16-BIT
                                ; NUMBERS.  IT MAY AT FIRST SEEM
                                ; CONFUSING BECAUSE FILPTSEC &
                                ; FILPTBYT ARE USED BOTH FOR HOLDING
                                ; THE MULTIPLIER (RECORD #) AND PART
                                ; OF THE PRODUCT RESULT.  HOWEVER,
                                ; THE BITS OF THE PRODUCTS DONT GET
                                ; MIXED UP WITH THE BITS OF THE
                                ; MULTIPLIER BECAUSE ROLLING IN A
                                ; PRODUCT BIT ALSO ROLLS OUT THE
                                ; LAST-USED MULTIPLIER BIT (IE.,
                                ; THERE IS NO BIT OVERLAP).

                LDY #16         ; 16 BITS / ONE 2-BYTE NUMBER.
NMBXLEN         TAX             ; SAVE PART OF RUNNING PRODUCT.
                                ; (ON 1ST ENTRY, SET (X) = 0.)
                LDA FILPTBYT    ; GET (A) = MULTIPLIER.
                LSR             ; PUT MULTIPLIER BIT IN CARRY.
                BCS NMBXLEN1    ; IF (C)=1, GO ADD MULTIPLICAND.
                TXA             ; (A) = PART OF RUNNING PRODUCT.
                BCC NMBXLEN2    ; ALWAYS, NO USE +ING MULTIPLICAND
                                ; BECAUSE BIT IN MULTIPLIER IS A 0
                                ; SO JUST GO SHIFT RUNNING PRODUCT
NMBXLEN1        CLC             ; + MULTIPLICAND TO RUNNING VERSION
                LDA FILPTSEC+1  ; OF SHIFTED PRODUCT.
                ADC RECLENWA
                STA FILPTSEC+1
                TXA             ; SET (A)=LOW BYT OF RUNNING PROD.
                ADC RECLENWA+1
NMBXLEN2        ROR             ; SHIFT (AS A UNIT) RUNNING RESULT
                ROR FILPTSEC+1  ; 1 BIT RIGHT FOR NEXT TIME AROUND.
                ROR FILPTSEC    ; SHIFT LOWER 2 BYTES OF RUNNING
                ROR FILPTBYT    ; PRODUCT & AT SAME TIME THROW OUT
                                ; LAST-USED MULTIPLIER BIT.
                DEY             ; REDUCE BIT COUNTER.
                BNE NMBXLEN     ; TAKE IF NOT USED ALL 16 BITS YET

                                ; COPY BYTE OFFSET INTO RECORD FROM
                                ; THE FM PARAMETER LIST TO THE WORK
                                ; AREA.

                ifelse(eval(VERSION >= 331),1,`
                CLC
                ')

                LDA BYTOFFFM
                STA BYTOFFWA

                                ; CALCULATE THE LOWEST ORDER BYTE OF
                                ; THE FILE POINTER:
                                ; BYTOFFWA = OFFSET INTO CURRENT RECORD.
                                ; = BYTE OFFSET INTO RECORD
                                ; + (RECORD LENGTH * RECORD #).

                ADC FILPTBYT
                STA FILPTBYT
                LDA BYTOFFFM+1
                STA BYTOFFWA+1
                ADC FILPTSEC
                STA FILPTSEC

                ifelse(eval(VERSION >= 331),1,`
                BCC CALCRTS
                INC FILPTSEC+1
                ',`
                LDA #0
                ADC FILPTSEC+1
                STA FILPTSEC+1
                ')

CALCRTS         RTS

                ifelse(eval(VERSION >= 331),1,`
                ASM_RES(2)
                ')

                                ; =================================
                                ; EXIT FM WITH OR WITHOUT ERRS.
                                ; =================================

LNGNOTAV        LDA #1
                BNE BADFMXIT    ; ALWAYS.
RNGERROP        LDA #2
                BNE BADFMXIT    ; ALWAYS.
RNGERRSB        LDA #3
                BNE BADFMXIT    ; ALWAYS.
WRITPROT        LDA #4
                BNE BADFMXIT    ; ALWAYS.
ENDOFDAT        LDA #5
                BNE BADFMXIT    ; ALWAYS.
FILENOT         LDA #6
                BNE BADFMXIT    ; ALWAYS.
DISKFULL
                ifelse(eval(VERSION < 320),1,`
                LDA #9
                BNE BADFMXIT
                ',`
                JMP FULLPTCH
                NOP
                ')

FILELOKD        LDA #10
                BNE BADFMXIT    ; ALWAYS.

GOODFMXT        LDA RTNCODFM
                CLC             ; (C)=0 TO SIGNAL GOOD OPERATION.
                BCC FMEXIT

BADFMXIT        SEC             ; (C) = 1 TO SIGNAL UNSUCCESSFUL.

                                ; COMMON FM EXIT ROUTINE.
                                ; NOTE THAT ROUTINE IS EXITED BY
                                ; RESETTING THE STACK POINTER.
                                ; USUALLY RETURNS TO CALLER OF THE
                                ; FUNCTION (IE. NORMALLY RETURNS TO
                                ; AFTRFUNC, $A6AB) IN THE FMDRIVER
                                ; ROUTINE ($A6A8).)  HOWEVER, THERE
                                ; IS ONE EXCEPTION.  THE APPEND CMD
                                ; HNDLR (CMDAPPND, $A298) MAY CALL
                                ; THIS ROUTINE SEVERAL TIMES.  MOST
                                ; OF THE TIME, EXECUTION RTNS TO
                                ; AFTRFUNC.  HOWEVER, THE LAST TIME
                                ; FMEXIT IS CALLED, THE CONTENTS OF
                                ; STKSAV ($B39B) ARE ALTERED BEFORE
                                ; FMEXIT IS ENTERED.  THIS FORCES
                                ; AN EXIT TO CALLER OF APPEND CMD
                                ; HNDLR (RATHER THAN TO CALLER OF
                                ; FUNCTION HNDLR).  IN THIS LATER
                                ; INSTANCE, EXECUTION ACTUALLY RTNS
                                ; TO AFTRCMD ($A17D) LOCATED IN THE
                                ; COMMAND PARSING & PROCESSING ROUTINES.


FMEXIT          PHP
                STA RTNCODFM    ; STORE RTN CODE IN FM PARM LIST.
                ifelse(eval(VERSION >= 320),1,`
                LDA #0          ; AVOID THAT INFAMOUS $48 BUG.
                STA STATUS
                ')
                JSR CPYFMWA     ; COPY WORK AREA TO WORK BUFFER.
                PLP             ; RETRIEVE STATUS OF SUCCESS OF
                                ; OPERATION BACK OFF STK.
                LDX STKSAV      ; ADJUST STK POINTER TO FORCE EXIT
                TXS             ; TO CALLER EVEN IF SEVERAL
                RTS             ; SUBRTNS DEEPER THAN ORIG ENTRY.

















                                ; =================================
                                ; FILE MANAGER SCRATCH SPACE.
                                ; ($B397 - $B3A3)
                                ; =================================

CURDIRTK        ASM_RES(1)          ; TRK# OF CURRENT DIRECTORY SEC.
CURDIRSC        ASM_RES(1)          ; SEC# OF CURRENT DIRECTORY SEC.
WABUFADR        ASM_RES(2)          ; UNUSED ($B399-$B39A).
STKSAV          ASM_RES(1)          ; SECOND STACK POINTER SAVE AREA.
                                ; USED TO RESET THE STACK TO FORCE
                                ; EXECUTION TO RTN TO A DESIRED
                                ; ROUTINE.  (NORMALLY RETURNS TO
                                ; AFTRFUNC ($A6AB) AFTR @ FUNCTION
                                ; IS CALLED. HOWEVER, ALSO USED BY
                                ; THE APPEND CMD TO FORCE EXECUTION
                                ; TO RTN TO AFTRCMD ($A17D).)
                                ; P.S.  DONT CONFUSE STKSAV WITH
                                ; THE 1ST STACK POINTER SAVE AREA
                                ; (STKSAVED, $AA59).
CURDIRNX        ASM_RES(1)          ; BYTE OFFSET OF FILE DESCRIPTION
                                ; ENTRY INTO CURRENT DIREC SEC.
SCRNSRCH        ASM_RES(1)          ; - CATALOG SCRN LINE COUNTER.
                                ; - DESCENDING CNTR FOR # OF SRCHS
                                ; DONE TO LOCATE A MATCHING NAME
                                ; OR EMPTY SPACE FOR A NEW FILE
                                ; DESCRIP ENTRY IN DIRECTORY SEC
                                ; (SEARCH1 = 1, SEARCH2 = 0).
                                ; - OFFSET OF DAT PR FROM START OF
                                ; A GIVEN T/S LIST.
TRK0YET         ASM_RES(2)          ; ASSIGNMENT FLAG = SIGNAL IF TRK0
                                ; ENCOUNTERED YET (ONLY 1ST BYTE
                                ; USED):
                                ; $00=TRK0 NOT ENCOUNTERED YET.
                                ; $01 = TRK0 ENCOUNTERED.
                                ; (USED TO SEE IF ENTIRE DISK HAS
                                ; BEEN SCANNED WHEN LOOKING FOR FREE
                                ; SECS TO ASSIGN.)
LOKUNMSK        = TRK0YET       ; LOCK/UNLOCK MASK ($80/$00).
FRETKMSK
                ASM_DATA_W(0)
                ifelse(eval(VERSION < 330),1,`
                ASM_DATA_W(%1111111111111000) ; 13 SECTORS
                ',`
                ASM_DATA_W(%1111111111111111) ; 16 SECTORS
                ')
                                ; 4-BYTE MASK USED BY INIT FUNCN
                                ; TO FREE AN ENTIRE TRACK.


                                ; =================================
                                ; CONVERSION TABLE FOR PRVOLNMB.
                                ; ($B3A4 - $B3A6)
                                ; =================================

BASETEN         ASM_DATA(1,10,100)  ; 10^0=1, 10^1=10, 10^2=100.


                                ; =================================
                                ; CHARACTER CODE FILE TYPE SYMBOLS
                                ; USED BY THE CATALOG ROUTINE.
                                ; ($B3A7 - $B3AE)
                                ; =================================

                ifelse(eval(VERSION < 320),1,`
FTYPETBL        HIASCII(`TBAI')     ; TEXT, BINARY, APPLESOFT, INTEGER
                ',`
FTYPETBL        HIASCII(`TIABSRAB') ; TEXT, INTEGER, APPLESOFT, BINARY
                ')
                                ; S-TYPE, R(ELOCATABLE)-TYPE,
                                ; A-TYPE, B-TYPE.  THESE CODES ARE
                                ; FREQUENTLY CHANGED BY HACKERS.)


                                ; =================================
                                ; DISK VOLUME SPELLED BACKWARDS.
                                ; ($B3AF - $B3BA)
                                ; =================================

DSKVOLUM        HIASCII(STR_REVERSE(`DISK VOLUME '))
                                ; THESE CHRS ARE OFTEN CHANGED
                                ; TO PERSONALIZE A CATALOG.






                                ; =================================
                                ; VOLUME TABLE OF CONTENTS (VTOC)
                                ; BUFFER ($B3BB - $B4BA).
                                ; =================================

VTOCBUFF        ASM_RES(1)          ; UNUSED ($B3BB).
                                ; REFERENCE FOR START OF TABLE.
FIRDIRTK        ASM_DATA(CATTRK)    ; TRK# OF 1ST DIRECTORY SECTOR.
FIRDIRSC                        ; SEC# OF 1ST DIRECTORY SECTOR.
                ifelse(eval(VERSION < 330),1,`
                ASM_DATA($0C)
                ',`
                ASM_DATA($0F)
                ')

DOSVERSION                      ; DOS VERSION RELEASE NUMBER.
                ASM_DATA(eval(VERSION / 10 % 10))

                ASM_RES(2)          ; UNUSED ($B3BF-$B3C0).
VOLUSED         ASM_RES(1)          ; DISK VOL# (NORMALLY, $FE).
                ASM_RES($20)        ; UNUSED ($B3C2-$B3E1).
MXIN1TSL        ASM_DATA($7A)       ; MAX # OF SECS THAT CAN BE LISTED
                                ; IN A T/S LIST SEC ($7A, #122).
                ASM_RES(8)          ; UNUSED ($B3E3-$B3EA).
NXTRKUSE        ASM_RES(1)          ; NEXT TRACK TO ASSIGN.
DRECTION        ASM_RES(1)          ; TRK ASGNMENT DIRECTN (+1 OR -1)
                ASM_RES(2)          ; UNUSED ($B3ED-$B3EE).
TKPERDSK        ASM_DATA(TRKPERDSK) ; TRKS/DISK (#35).
                                ; AFTER DRIVE ADJUSTMENT, VALUE IN
                                ; TKPERDSK CAN BE ALTERED TO ENABLE
                                ; THE USE OF AN EXTRA TRK ON DISK.
SECPERTK
                ifelse(eval(VERSION < 330),1,`
                ASM_DATA($0D)       ; SECS/TRK ($0D, 13, $00 --> $0C)
                ',`
                ASM_DATA($10)       ; SECS/TRK ($10, 16, $00 --> $0F)
                ')

BYTPERSC        ASM_DATA_W($100)      ; BYTES/SECTOR ($0100, #256).


                                ; ------------------------------------
                                ; TRACK/SECTOR MAPS IN VTOC.
                                ; ($B3F3 - $B47E)
                                ; ------------------------------------

                                ; MAPS OF SECTOR USAGE ON EACH TRK.
                                ; EACH TRK IS REPRESENTED BY FOUR
                                ; BYTES.  THE BITS IN THESE BYTES
                                ; DEFINE THE SECTOR USAGE BY THE
                                ; FOLLOWING CONVENTION:
                                ; SET BIT = FREE SECTOR.
                                ; CLEAR BIT = USED SECTOR.
                                ; 1ST BYTE:  FEDC BA98
                                ; 2ND BYTE:  7654 3210
                                ; (THE 3RD & 4TH BYTES ASSOCIATED
                                ; WITH EACH TRACK ARE NOT USED.)

FRESECMAP       ASM_RES(TRKPERDSK*4)

                ASM_RES(60)         ; UNUSED ($B47F-$B4BA).

                                ; END VTOC
                                ; =================================






                                ; =================================
                                ; DIRECTORY SECTOR BUFFER.
                                ; ($B4BB - $B5BA)
                                ; =================================

DIRECBUF        ASM_DATA($00)       ; UNUSED-REFERENCE FOR START OF BUF.
DIRLNKTK        ASM_RES(1)          ; TRK OF NEXT DIRECTORY SECTOR.
DIRLNKSC        ASM_RES(1)          ; SECTOR OF NEXT DIRECTORY SECTOR.
                ASM_RES(8)          ; UNUSED ($B4BC-$B4C5).


                                ; ---------------------------------
                                ; FILE DESCRIPTION ENTRIES.
                                ; ($B4C6 - $B5BA)
                                ; ---------------------------------

FIL1TSTK        ASM_RES(1)          ; FILE1: T/S LIST TRK ($B4C6)
FIL1TSSC        ASM_RES(1)          ; T/S LIST SEC ($B4C7)
FIL1TYPE        ASM_RES(1)          ; FILE TYPE ($B4C8)
FIL1NAME        ASM_RES(30)         ; NAME ($B4C9-$B4E6)
FIL1SIZE        ASM_RES(2)          ; SIZE ($B4E7-$B4E8)

                ASM_RES(6*(1+1+1+30+2))


                                ; =================================
                                ; FILE MANAGER PARAMETER LIST.
                                ; ($B5BB - $B5D0)
                                ; =================================

FMPRMLST
FIRDOSPG
OPCODEFM        ASM_RES(1)          ; FM OPERATION CODE.
SUBCODFM        ASM_RES(1)          ; FM OPERATION SUBCODE.
RECLENFM
RENAMBUF
RECNMBFM        ASM_RES(2)          ; FM PARM LST VERSION OF RECORD #.
                                ; (USUALLY 1 LESS THAN RECNMBWA.)
                                ; (SEE DESCRIPTION OF THE RECORD
                                ; NUMBER GIVEN IN THE WORK AREA
                                ; LISTED BELOW.)
VOLFM
BYTOFFFM        ASM_RES(1)          ; BYTE OFFSET INTO RECORD.
DRVFM           ASM_RES(1)
SLOTFM
LEN2RDWR        ASM_RES(1)      ; LENGTH TO READ OR LENGTH-1
                                ; TO WRITE.  (INIT VAL & COUNTER).
                                ; ALSO USED AS A TEMPORARY BUF TO
                                ; TRANSFER ADDR & LENGTH BYTES TO
                                ; ONEIOBUF BUF WHEN USING THE
                                ; WRITE-ONE-BYTE AND READ-ONE-BYTE
                                ; SUBFUNCTIONS.
FILTYPFM        ASM_RES(1)      ; FILE TYPE CODE (INCLUDING THE
                                ; LOCKED/UNLOCKED STATUS).
FNAMBUFM
ONEIOBUF
CURIOBUF        ASM_RES(2)          ; ADDR OF CURRENT I/O BUFFER.
RTNCODFM        ASM_RES(1)          ; RETURN CODE.  (CONTRARY TO WHAT
                                ; SOME ARTICLES SUGGEST, THE VALUE
                                ; IN THIS BYTE IS ONLY RELEVANT IF
                                ; AN ERROR OCCURS.)
                ASM_RES(1)          ; UNUSED ($B5C6).
WRKBUFFM        ASM_RES(2)          ; PTS TO WORK BUF (IN DOS CHAIN).
TSBUFFM         ASM_RES(2)          ; PTS TO T/S LIST SEC BUF (CHAIN).
DATBUFFM        ASM_RES(2)          ; PTS TO DATA SECTOR BUF (CHAIN).
NXTNAME         ASM_RES(2)          ; PTS TO NEXT DOS NAME BUFFER IN
                                ; CHAIN. (SET UP, BUT NOT USED.)
                ASM_RES(2)          ; UNUSED ($B5CF-$B5D0).


;FMPRMLST        = OPCODEFM      ; START OF FM PARAMETER LIST.

;FIRDOSPG        = OPCODEFM      ; CONTAINS 1ST PAGE# OF DOS WHEN
                                ; WRITING DOS IMAGE DURING INIT.

;RECLENFM        = RECNMBFM      ; FM PARM LIST VERSION OF THE
                                ; RECORD LENGTH.  (SEE DESCRIPTION
                                ; OF RECORD LENGTH DESCRIBED IN
                                ; THE WORK AREA LISTING BELOW.)
;RENAMBUF        = RECLENFM      ; PTS TO BUFFER CONTAINING NEW FILE
                                ; NAME.  (USED IN RENAME CMD.)
;VOLFM           = BYTOFFFM      ; VOLUME# WANTED.
;DRVFM           = BYTOFFFM+1    ; DRIVE# WANTED.

;SLOTFM          = LEN2RDWR      ; SLOT NUMBER.
;FILTYPFM        = LEN2RDWR+1    ; FILE TYPE CODE (INCLUDING THE
                                ; LOCKED/UNLOCKED STATUS).
;FNAMBUFM        = CURIOBUF      ; FILE NAME ADDRESS.
;ONEIOBUF        = CURIOBUF      ; A ONE-BYTE I/O BUFFER.


                                ; =================================
                                ; FILE MANAGER WORK AREA.
                                ; ($B5D1 - $B5FF)
                                ; (DONT CONFUSE WITH THE VARIOUS
                                ; WORK BUFFERS CONTAINED IN THE
                                ; CHAIN OF DOS BUFFERS.)
                                ; =================================

TSL1TRK         ;= FMWKAREA      ; TRK# OF 1ST T/S LIST SEC.
FMWKAREA        ASM_RES(1)          ; SIMPLY USED FOR REFERENCE POINT.

TSL1SEC         ASM_RES(1)          ; SEC # OF 1ST T/S LIST SEC.
CURTSTRK        ASM_RES(1)          ; TRK# OF CURRENT T/S LIST SEC.
CURTSSEC        ASM_RES(1)          ; SEC# OF CURRENT T/S LIST SEC.
UPDATFLG        ASM_RES(1)          ; FLAG TO UPDATE DIFFERENT TYPES
                                ; OF SECTORS:
                                ; $02=LAST OPERATION WAS A WRITE.
                                ; $20=VTOC NEEDS UPDATING.
                                ; $40=DATA SEC NEEDS UPDATING.
                                ; $80=T/S LST SEC NEEDS UPDATING.
CURDATRK        ASM_RES(1)          ; TRK# OF CURRENT DATA SECTOR.
CURDATSC        ASM_RES(1)          ; SEC# OF CURRENT DATA SECTOR.
SECNXD1R        ASM_RES(1)          ; OFFSET OF FILE DESCRIPTION FROM
                                ; THE VERY FIRST DIRECTORY SEC.
BYTNXD1R        ASM_RES(1)          ; OFFSET OF FILE DESCRIPTION INTO
                                ; THE CURRENT DIRECTORY SEC.
MXSCURTS        ASM_RES(2)          ; MAXIMUM # OF SECS THAT CAN BE
                                ; LISTED IN A T/S LIST.
RELFIRST        ASM_RES(2)          ; RELATIVE SEC # (IN RELATION TO
                                ; THE ENTIRE FILE) OF THE FIRST
                                ; DATA SEC THAT IS (OR CAN BE)
                                ; LISTED IN THE CURRENT T/S LIST.
                                ; (POSSIBLE VALS ARE:$0000, $007A,
                                ; 2*$007A, 3*$007A OR 4*$007A.)
RELASTP1        ASM_RES(2)          ; ONE GREATER THAN THE MAXIMUM
                                ; RELATIVE SEC# (IN RELATION TO THE
                                ; ENTIRE FILE) OF THE LAST DAT SEC
                                ; THAT CAN POSSIBLY BE DESCRIBED
                                ; IN THE CURRENT T/S LIST.
                                ; (POSSIBLE VALUES ARE: $007A,
                                ; 2*$007A, 3*$007A, 4*$007A AND
                                ; 5*$007A.)
RELPREV         ASM_RES(2)          ; RELATIVE SEC# (IN RELATION TO THE
                                ; ENTIRE FILE) OF THE LAST DAT SEC
                                ; THAT WAS READ OR WRITTEN.
                                ; (POSSIBLE VAL ARE: $0000, $007A,
                                ; 2*$007A, 3*$007A OR 4*$007A.)
SECSIZWA        ASM_DATA_W($100)      ; BYTES/SECTOR (#256, $00-$FF).

                                ; --- FILE POINTER ---
                                ; (3 BYTES)
FILPTSEC        ASM_RES(2)          ; SECTOR OFFSET OF PTR (2 BYTES).
FILPTBYT        ASM_RES(1)          ; BYTE OFFSET PORTION OF POINTER.

WASTEBYT        ASM_RES(1)          ; WEIRD FLG USED BY THE APPEND CMD
                                ; (VIA RSETPTRS, $B6B3) WHEN
                                ; BACKING UP THE FILE POINTER AND
                                ; TESTED (IN APNDPTCH, $B692)
                                ; WHENEVER AN OUT-OF-DATA ERROR IS
                                ; ENCOUNTERED.
RECLENWA        ASM_RES(2)          ; RECORD LENGTH.
RECNMBWA        ASM_RES(2)          ; CURRENT RECORD NUMBER.
BYTOFFWA        ASM_RES(2)          ; CURRENT BYTE OFFSET INTO RECORD.
                                ; - RANDOM ACCESS TXT FILES HAVE A
                                ; FIXED RECORD LNGTH ASSIGNED BY
                                ; THE USER.
                                ; - SEQUENTIAL TEXT AND APPLESOFT
                                ; FILES HAVE A RECORD LENGTH OF
                                ; ONE.
                                ; - DURING A LOAD OR BLOAD, APPLE-
                                ; SOFT OR BINARY FILES ARE
                                ; CONSIDERED TO BE COMPOSED OF A
                                ; COLLECTION OF ONE-BYTE LONG
                                ; RECORDS.
                                ; - WHEN SAVING OR BSAVING HOWEVER
                                ; THESE FILES ARE TREATED AS IF
                                ; THEY CONSIST OF A SINGLE VERY
                                ; LONG RECORD.
FILENSEC        ASM_RES(2)          ; FILE LENGTH IN SECTORS.
ASIGNSEC        ASM_RES(1)          ; NEXT SECTOR TO ASSIGN.
ASIGNTRK        ASM_RES(1)          ; NEXT TRK TO ASSIGN.
ASIGNMAP        ASM_RES(4)          ; 4-BYTE MAP OF SEC USAGE ON THE
                                ; TRK BEING ASSIGNED.
FILTYPWA        ASM_RES(1)          ; FILE TYPE CODE (INCLUDING THE
                                ; LOCKED OR UNLOCKED STATUS).
SLOT16WA        ASM_RES(1)          ; SLOT*16.
DRVWA           ASM_RES(1)          ; DRIVE NUMBER.
VOLWA           ASM_RES(1)          ; 1S COMPLEMENT OF VOLUME NUMBER.
TRKWA           ASM_RES(1)          ; TRACK NUMBER.
                ASM_RES(5)          ; UNUSED ($B5FB-$B5FF).
