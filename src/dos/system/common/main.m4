include(`asm.m4h')
include(`symbols.m4h')

                                ; ====================================
                                ; RELOCATABLE ADDRESS CONSTANTS TABLE.
                                ; ($9D00 - $9D0F)
                                ; (ADRS CAN BE CHANGED TO POINT TO
                                ; CUSTOMIZED ROUTINES, TABLES, ETC.)
                                ; ====================================

ADOSFNB1
                ifelse(eval(VERSION < 330),1,`
                ASM_ADDR(DOSNMBF1-1) ; PTS TO FIRST DOS BUFFER AT ITS
                                ; FILE NAME FIELD.
                ',`
                ASM_ADDR(DOSNMBF1)
                ')

ADINPTCP        ASM_ADDR(INPTINCP); PTS TO DOS INPUT INTERCEPT ROUTINE.
ADOPUTCP        ASM_ADDR(OPUTINCP); PTS TO DOS OUTPUT INTERCEPT ROUTINE.
ADRPFNBF        ASM_ADDR(PRIMFNBF); PTS TO PRIMARY FILENAME BUFFER.
ADRSFNBF        ASM_ADDR(SCNDFNBF); PTS TO SECONDARY FILENAME BUFFER.
ADLENADR        ASM_ADDR(LENADRBF); PTS TO 2-BYTE BUF THAT RECEIVES
                                ; BLOAD ADR & LENGTH READ FROM DSK
ADOSTART        ASM_ADDR(MASTERDOS); PTS TO 1ST BYTE OF DOS.
ADFMPARM        ASM_ADDR(OPCODEFM); PTS TO FM PARAMETER LIST.



                                ; =====================================
                                ; TABLE OF OUTPUT HANDLER ENTRY POINTS
                                ; ($9D10 - $9D1D)
                                ; (ADR-1 OF ROUTNS WHICH HANDLE PRINT
                                ; STATEMENTS CONTAINING DOS CMDS.)
                                ; =====================================

OUTHNDTB        ASM_ADDR(OPUTHDL0-1) ; EVALUATE START OF INPUT LINE.
                ASM_ADDR(OPUTHDL1-1) ; COLLECT THE DOS COMMAND.
                ASM_ADDR(OPUTHDL2-1) ; PRINT A <CR> AND RETURN.
                ASM_ADDR(OPUTHDL3-1) ; PROCESS THE INPUT INFORMATION.
                ASM_ADDR(OPUTHDL4-1) ; WRITE DATA TO DISK.
                ASM_ADDR(OPUTHDL5-1) ; ANALYZE 1ST CHR OF DATA FRM DSK
                ASM_ADDR(OPUTHDL6-1) ; IGNORE INPUT PROMPT ("?").



                                ; =======================================
                                ; COMMAND HANDLER ENTRY POINT TABLE
                                ; ($9D1E - $9D55)
                                ; ALL ADRS ARE ONE LESS THAN THE ACTUAL
                                ; ENTRY POINTS BECAUSE THESE ROUTNS ARE
                                ; ENTERED VIA A "STACK JUMP".  IF YOU
                                ; CREATE A NEW DOS CMD, PLACE THE ADR-1
                                ; OF THE NEW CMD IN THE FOLLOWING TABLE.
                                ; (ALSO MAKE SURE THAT THE FIRST CMD IN
                                ; THE TABLE CAN CREATE A NEW FILE.)
                                ; SOME AUTHORS DISABLE SPECIFIC DOS
                                ; CMDS BY POINTING THE CMDS TABLE ADR
                                ; AT DOSS COLDSTART ROUTINE OR BY
                                ; PLACING AN "RTS" OPCODE AT THE CMDS
                                ; ENTRY POINT.
                                ; =======================================

CMDTBL          ASM_ADDR(CMDINIT-1)  ; $A54F-1
                ASM_ADDR(CMDLOAD-1)  ; $A413-1
                ASM_ADDR(CMDSAVE-1)  ; $A397-1
                ASM_ADDR(CMDRUN-1)   ; $A4D1-1
                ASM_ADDR(CMDCHAIN-1) ; $A4F0-1
                ASM_ADDR(CMDELETE-1) ; $A263-1
                ASM_ADDR(CMDLOCK-1)  ; $A271-1
                ASM_ADDR(CMDUNLOK-1) ; $A275-1
                ASM_ADDR(CMDCLOSE-1) ; $A2EA-1
                ASM_ADDR(CMDREAD-1)  ; $A51B-1
                ASM_ADDR(CMDEXEC-1)  ; $A5C6-1
                ASM_ADDR(CMDWRITE-1) ; $A510-1
                ASM_ADDR(CMDPOSN-1)  ; $A5DD-1
                ASM_ADDR(CMDOPEN-1)  ; $A2A3-1
                ASM_ADDR(CMDAPPND-1) ; $A298-1
                ASM_ADDR(CMDRENAM-1) ; $A281-1
                ASM_ADDR(CMDCATLG-1) ; $A56E-1
                ASM_ADDR(CMDMON-1)   ; $A233-1
                ASM_ADDR(CMDNOMON-1) ; $A23D-1
                ASM_ADDR(CMDPR-1)    ; $A229-1
                ASM_ADDR(CMDIN-1)    ; $A22E-1
                ASM_ADDR(CMDMXFIL-1) ; $A251-1
                ASM_ADDR(CMDFP-1)    ; $A57A-1
                ASM_ADDR(CMDINT-1)   ; $A59E-1
                ASM_ADDR(CMDBSAVE-1) ; $A331-1
                ASM_ADDR(CMDBLOAD-1) ; $A35D-1
                ASM_ADDR(CMDBRUN-1)  ; $A38E-1
                ASM_ADDR(CMDVERFY-1) ; $A27D-1



                                ; ===========================================
                                ; ACTIVE BASIC ENTRY POINT VECTOR TABLE
                                ; ($9D56 - $9D61)
                                ; (NOTE:  TBL CUTOMIZED BY DOS ACCORDING TO
                                ; THE CURRENT VERSION OF BASIC THAT IS BEING
                                ; USED.  ADRS SHOWN AS OPERANDS BELOW,
                                ; ASSUME THAT APPLESOFT BASIC IS ACTIVE.)
                                ; ===========================================

CHAINTRY        ASM_ADDR(BASICCHN); ADR OF CHAIN ENTRY PT TO BASIC.
RUNTRY          ASM_ADDR(RUNINTGR); ADR OF BASICS RUN CMD.
ADBSCERR        ASM_ADDR(BASICERR); ADR OF BASICS ERROR HANDLER.
TOCLDVEC        ASM_ADDR(BASICCLD); ADR OF BASICS COLD START ROUTN
TOWRMVEC        ASM_ADDR(BASICWRM); ADR OF BASICS WARM START ROUTN

                ifelse(eval(VERSION >= 320),1,`
RLOCNTRY
                ifelse(eval(VERSION == 330),1,`
                ASM_ADDR(SETLINKS); ADR OF ROUTN THAT ENABLES BASIC
                                ; PRGMS TO BE RELOCATABLE.
                ',`
                ASM_RES(2)
                ')
                ')

                                ; ====================================
                                ; IMAGE OF INTEGER BASICS ENTRY
                                ; POINT VECTOR TABLE ($9D62 - $9D6B).
                                ; ====================================

IMGINTV         ASM_ADDR(BASICCHN)
                ASM_ADDR(RUNINTGR)
                ASM_ADDR(BASICERR)
                ASM_ADDR(BASICCLD)
                ASM_ADDR(BASICWRM)


                                ; =======================================
                                ; IMAGE OF ROM APPLESOFT BASICS ENTRY
                                ; POINT VECTOR TABLE ($9D6C - $9D77).
                                ; =======================================

IMGFPV          ASM_ADDR(RUNFPROM)
                ASM_ADDR(RUNFPROM)
                ASM_ADDR(BSCERHLR)
                ASM_ADDR(BASICCLD)
                ifelse(eval(VERSION < 320),1,`
                ASM_RES(2)
                ',`
                ASM_ADDR(RESTART)
                ASM_ADDR(SETLINKS)
                ')


                                ; ========================================
                                ; IMAGE OF RAM APPLESOFT BASICS  ENTRY
                                ; POINT VECTOR TABLE ($9D78 - $9D83).
                                ; (NOTE: A(RAM) REFERS TO A DISK-BASED
                                ; VERSION OF APPLESOFT BASIC THAT IS
                                ; HOUSED ON THE "SYSTEMS MASTER" DISK.)
                                ; ========================================

IMGARAMV        ASM_ADDR(FPRAMRUN)
                ASM_ADDR(FPRAMRUN)
                ASM_ADDR($1067)
                ASM_ADDR(DOSCOLD)
                ifelse(eval(VERSION < 320),1,`
                ASM_RES(2)
                ',`
                ASM_ADDR($C3C)
                ASM_ADDR($CF2)
                ')























                                ; =================================
                                ; DOSS COLD START ROUTINE.
                                ; (PS. DONT CONFUSE WITH BASICS
                                ; COLD START ROUTINE - BASICCLD.)
                                ; =================================

                                ; GET SLOT & DRV #S & STORE THEM AS
                                ; DEFAULT VALS IN CASE NO SUCH PARMS
                                ; WERE ISSUED WITH THE COMMAND.

DOSCOLD         LDA IBSLOT      ; SLOT# * 16 FROM RWTSS IOB TBL.
                LSR             ; DIVIDE BY 16.
                LSR
                LSR
                LSR
                STA SLOTPRSD    ; PUT SLOT IN PARSED TABLE.
                LDA IBDRVN      ; DRV # FROM RWTSS IOB.
                STA DRVPRSD     ; PUT DRIVE # IN PARSED TABLE.

                                ; CHK WHICH BASIC IS IN ROM.

                LDA BASICCLD    ; SET IDENTIFYING BYTE WITH
                EOR #$20        ; $20 = INTEGER OR $40 = A(ROM).
                BNE ISFPROM     ; BRANCH IF APPLESOFT IS IN ROM.

                                ; INTEGER BASIC IS ACTIVE.

ISINT           STA ACTBSFLG    ; SET ACTIVE BASIC FLAG
                                ; TO DENOTE INTEGER (#$00).

                                ; COPY IMAGE OF INTEGER BASICS
                                ; ENTRY POINT VECTOR TABLE TO THE
                                ; ACTIVE BASIC ENTRY VECTOR TABLE.

                LDX #10
INT2BSIC        LDA IMGINTV-1,X
                STA CHAINTRY-1,X
                DEX
                BNE INT2BSIC    ; 10 BYTES TO COPY (10 --> 1).
                JMP BYPASWRM

                                ; COPY IMAGE OF APPLESOFTS ENTRY
                                ; POINT VECTOR TABLE TO THE ACTIVE
                                ; BASIC ENTRY POINT VECTOR TABLE.

ISFPROM         LDA #$40        ; SET ACTIVE BASIC FLAG TO #$40
                STA ACTBSFLG    ; SO SIGNAL APPLESOFT ROM.
                LDX #IMGARAMV-IMGFPV
AROM2BSC        LDA IMGFPV-1,X
                STA CHAINTRY-1,X
                DEX
                BNE AROM2BSC
BYPASWRM        SEC             ; (C) = 1, SIGNAL FOR COLDSTART.
                                ; (C) = 0, SIGNAL FOR WARMSTART.
                BCS CMWRMCLD    ; FORCE BRANCH TO BYPASS PART OF
                                ; THE WARMSTART ROUTINE.


                                ; =================================
                                ; DOSS WARMSTART ROUTINE.
                                ; (PS. DONT CONFUSE WITH BASICS
                                ; WARMSTART ROUTINE - RESTART.)
                                ; =================================

DOSWARM
                ifelse(eval(VERSION >= 320),1,`
                LDA ACTBSFLG    ; SEE WHICH LANGUAGE IS UP.
                BNE CKBASIC     ; IF A(ROM), #$40 OR A(RAM), #$80.

                                ; INTEGER WAS UP.

                LDA #$20        ; (A) = OPCODE FOR "JSR" INSTRUC.
                BNE DTRMNBSC    ; ALWAYS.


                                ; ACTIVE BASIC FLAG DENOTED THAT A
                                ; VERSION OF APPLESOFT WAS ACTIVE,
                                ; SO NOW CHECK IF DEALING WITH
                                ; A(RAM) OR A(ROM).

CKBASIC         ASL             ; MULTIPLY CODE TIMES 2.
                BPL FORWARM     ; BRANCH IF A(RAM).
                                ; (IE. A(RAM) YEILDS $40 * 2 = $80
                                ; & A(ROM) YEILDS $80 * 2 = $00.)

                                ; USING A(ROM).

                LDA #$4C        ; (A) = OPCODE FOR "JMP" INSTRUC.
DTRMNBSC        JSR SETROM      ; GO TEST CARD OR MOTHERBOARD TO
                                ; INSURE THAT DEVICE CONTAINING
                                ; ROM VERSION WE WANT IS SELECTED.
                ')

FORWARM         CLC             ; (C) = 0, SIGNAL FOR WARMSTART.
CMWRMCLD        PHP             ; SAVE (C) DENOTING WARM OR COLD.
                JSR INITIOHK    ; INITIALIZE THE I/O HOOKS SO THAT
                                ; DOS INTERCEPTS ALL IN/OUTPUT.
                ifelse(eval(VERSION < 320),1,`
                LDA #$70
                ',`
                LDA #0
                ')
                STA CIOCUMUL    ; SIMULATE A "NOMON" COMMAND.
                                ; NOTE: CAN "NOP" OUT THIS INSTRUC
                                ; TO DEFEAT "NOMONCIO" WHEN COLD-
                                ; OR WARMSTARTING.
                ifelse(eval(VERSION < 320),1,`
                LDA #0
                ')
                STA OPUTCOND    ; SET CONDITION 0.
                PLP             ; GET SAVED STATUS BACK OFF STK &
                ROR             ; ROTATE (C) INTO HI BIT OF (A) TO
                STA CONDNFLG    ; SET CONDNFLG = $00 FOR WARMSTART
                                ; OR CONDNFLG = $80 FOR COLDSTART.
                BMI LANGCOLD    ; BRANCH IF DOING COLDSTART.

LANGWARM        JMP (TOWRMVEC)  ; JMPS TO BASICS WARMSTART ROUTN
                                ; (RESTART) AT $D43C.

LANGCOLD        JMP (TOCLDVEC)  ; JMPS TO BASICS COLDSTART ROUTN
                                ; (BASICCLD) AT $E000.


                                ; =================================
                                ; INITIAL PROCESSING WHEN FIRST
                                ; KEYBOARD INPUT REQUEST IS MADE
                                ; BY BASIC AFTER A COLDSTART.
                                ; =================================

                                ; TEST IF USING A(RAM) OR NOT.
                                ; ON ENTRY, (A) = CONTENTS OF CONDNFLG.
                                ; = $00 = WARM START.
                                ; = $01 = READING.
                                ; = $80 = COLD START.
                                ; = $C0 = USING A(RAM).

KEYCOLD         ASL             ; (A) * 2 TO DROP OUT HI BIT.
                BPL SKPDARAM    ; BRANCH IF NOT USING A(RAM).
                STA ACTBSFLG    ; USING A(RAM) - WAS LOADED BY
                                ; INTEGER BASIC ROM
                                ; - ACTV BSC FLG=$80

                                ; COPY IMAGE OF A(RAM)S ENTRY
                                ; POINT VECTOR TABLE TO THE
                                ; ACTIVE BASIC ENTRY POINT VECTOR
                                ; TABLE.

                LDX #DOSCOLD-IMGARAMV
ARAM2BSC        LDA IMGARAMV-1,X
                STA CHAINTRY-1,X
                DEX
                BNE ARAM2BSC

                                ; BLANK OUT THE PRIMARY FILENAME
                                ; BUFFER TO MAKE SURE A "HELLO"
                                ; FILE WONT BE RUN.

                LDX #29         ; 30 BYTES TO BLANK (29 --> 00).
BLNKPRIM        LDA SCNDFNBF,X  ; COPY BLANK SECONDARY TO PRIMARY.
                STA PRIMFNBF,X
                DEX
                BPL BLNKPRIM

                                ; BUILD DOS BUFS & SET CONDITION.

SKPDARAM        LDA MAXDFLT     ; SET MXFILVAL TO DEFAULT VAL OF 3
                STA MXFILVAL    ; NOTE: DEFAULT VAL CAN BE CHANGED
                                ; BY SETTING MAXDFLT BTWN 1 - 16
                                ; AND THEN INITING A DISK.
                JSR BILDBUFS    ; GO BUILD THE DOS BUFFERS.
                LDA EXECFLAG    ; CHK IF AN EXEC FILE IS RUNNING.
                BEQ SKPDEXEC    ; BRANCH IF NOT EXECING.

                PHA             ; YES - WE ARE EXECING.
                JSR PT2EXEC     ; GET ADR OF BUF WERE EXECING IN.
                PLA             ; RETRIEVE EXEC FLG FROM STK.
                LDY #0          ; RESERVE DOS BUFFER FOR EXECING
                STA (A3L),Y     ; IN CASE WE WANT TO MODIFY DOS TO
                                ; EXEC ON THE BOOT?

SKPDEXEC        JSR RESTAT0     ; SET CONDNFLG=0 SO SIGNAL FILE
                                ; NOT BEING READ.
                                ; RESET OPUTCOND=0 (TO SIGNAL WANT
                                ; TO EVALU8 START OF LINE ON RTN).

                                ; CHECK IF DISK WAS JUST BOOTED.
                                ; (IF JUST BOOTED DISK, NDX2CMD
                                ; CONTAINS A $00 WHICH WAS ETCHED
                                ; ON THE DISK WHEN THE DISK WAS
                                ; ORIGINALLY INITED.)
                ifelse(eval(VERSION >= 320),1,`
                LDA NDX2CMD     ; WAS LAST CMD AN "INIT"?
                BNE OLDBOOT     ; NO - TAKE BRANCH.
                ')

                                ; DISK WAS JUST BOOTED SO COPY IMAGE
                                ; OF DOSS ENTRY POINT VECTOR TABLE
                                ; TO PAGE 3.

                ifelse(eval(VERSION < 320),1,`
                LDX #INPUTCLD-IMGDOSVT
                ',`
                LDX #INPTINCP-IMGDOSVT-1
                ')

STOR3DOS        LDA IMGDOSVT,X  ; COPY IMAGE TO PAGE 3.
                STA PG3DOSVT,X
                DEX
                BPL STOR3DOS

                ifelse(eval(VERSION < 320),1,`
                LDA   NDX2CMD
                BNE   OLDBOOT
                LDA   PRIMFNBF
                EOR   #$A0
                BEQ   OLDBOOT
                JMP   CMDRUN
                ',`
                                ; PROGRAM THE RESET KEY TO POINT AT
                                ; DOSS WARMSTART ROUTINE.
                                ;
                                ; NOTE THAT THE RESET KEY CAN BE
                                ; PROGRAMMED TO PT AT ANY LOCATION
                                ; SIMPLY BY PUTTING THE ADR OF THE
                                ; TARGET ROUTINE (IN LOW/HI FORMAT)
                                ; IN $3F2/$3F3.  NEXT, EOR THE
                                ; CONTENTS OF $3F3 WITH THE NUMBER
                                ; #$A5 AND PUT THE RESULT IN $3F4.
                                ; (IF $3F3 EOR #$A5 < > $3F4, THEN
                                ; DISK REBOOTS & A COLDSTART IS DONE.)

                LDA IMGDOSVT+2  ; WRITE OVER OLD (OUT-DATED) IMAGE
                STA RESETVEC+1  ; WHICH CONTAINED A "JSR MON"
                EOR #$A5        ; INSTRUC APPLIC TO NON-AUTOSTART.
                STA VLDBYTE     ; SET VALIDATION (POWER UP) BYTE.
                LDA IMGDOSVT+1
                STA RESETVEC

                                ; SET THE COMMAND INDEX TO RUN
                                ; THE "HELLO" FILE.

                LDA #6          ; CMD INDEX FOR "RUN".
                                ; (CAN BE CHANGED TO "BRUN", ETC.)
                BNE DOPENDNG    ; ALWAYS.
                ')

                                ; CHECK IF A COMMAND IS PENDING.

OLDBOOT         LDA NEXTCMD
                BEQ NOPEND      ; NO CMD PENDING.

                                ; GO DO THE PENDING COMMAND.  IF THE
                                ; DISK WAS JUST BOOTED, THEN RUN THE
                                ; "HELLO" FILE.  NOTE THAT THE ACTUAL
                                ; NAME OF THE HELLO FILE RESIDES IN
                                ; THE PRIMARY FILENAME BUFFER.  IT
                                ; WAS WRITTEN TO THE DISK AS PART OF
                                ; THE DOS IMAGE WHEN THE DISK WAS INITED.
                                ; THERE4, IF YOU WANT TO CHANGE THE
                                ; NAME OF THE HELLO FILE, YOU MUST:
                                ; (1) CHNG NAME IN FILENAME FIELD
                                ; OF DIRECTORY SEC. (EASIEST JUST
                                ; TO USE THE RENAME CMD.)
                                ; (2) ZAP PRIMARY NAME BUF ON DISK
                                ; (TRK$01/SEC$09/OFFSETS$75-$92).

DOPENDNG        STA NDX2CMD     ; SET CMD INDEX & GO DO CMD.
                ifelse(eval(VERSION < 320),1,`
                JMP TORESTAT0
                ',`
                JMP DODOSCMD
                ')
NOPEND
                ifelse(eval(VERSION < 320),1,`
                JMP DOSEXIT
                ',`
                RTS
                ')


                                ; =================================
                                ; IMAGE OF DOS VECTOR TABLE.
                                ; (COPIED TO PAGE 3, $3D0 - $3FF.)
                                ; =================================

                                ; PAGE-3 ADDR & FUNCTION

IMGDOSVT        JMP DOSWARM     ; $3D0 - GO TO DOSS WARM START
                                ; ROUTINE.  LEAVE PGRM INTACT.
IMGCOLVT        JMP DOSCOLD     ; $3D3 - GO TO DOSS COLD START
                                ; ROUTN. RESET HIMEM, REBUILD DOS
                                ; BUFS & WIPE OUT PRGM.
                JMP FMXTNTRY    ; $3D6 - ALLOW USER TO ACCESS FM
                                ; VIA HIS OWN ASSEMBLY LANG PRGMS.
                ifelse(eval(VERSION < 320),1,`
                JMP RWTS
                ',`
                JMP ENTERWTS    ; $3D9 - ALLOW USER TO ACCESS RWTS
                                ; VIA HIS OWN ASSEMBLY LANG PRGMS.
                ')
                LDA ADFMPARM+1  ; $3DC - LOCATE FM PARAMETER LIST.
                LDY ADFMPARM
                RTS

                LDA ADRIOB+1    ; $3E3 - LOCATE RWTSS I/O BLOCK.
                LDY ADRIOB
                RTS

                ifelse(eval(VERSION >= 320),1,`
                JMP INITIOHK    ; $3EA - PT I/O HOOKS AT DOSS
                                ; INTERCEPT HANDLERS.
                NOP
                NOP
                JMP OLDBRK      ; $3EF - GOES TO MONITORS ROUTINE
                                ; WHICH HANDLES "BRK" INSTRUCS.
                JMP MON         ; AFTER THIS INSTRUCTION IS COPIED
                                ; TO PAGE 3 (AT $3F2 - $3F4), THE
                                ; ROUTINE AT $9E30 OVERWRITES THE
                                ; PAGE-3 IMAGE TO PROGRAM THE RESET
                                ; KEY. THIS OVERWRITTING IS DONE TO
                                ; ACCOMMODATE NEWER "AUTOSTART"
                                ; ROM USED IN THE APPLE II+, IIE &
                                ; IIC MACHINES.  THE PAGE-3 LOCS
                                ; ARE DESCRIBED BELOW:
                                ; $3F2/$3F3 - ADR OF RESET HNDLING
                                ; ROUTN (IN LOW/HI FORMAT).
                                ; NORMALLY=ADR OF DOSWARM ($9DBF).
                                ; $3F4 - IMG OF VALIDATION BYTE.
                                ; NORMALLY CONTAINS #$38 BECAUSE:
                                ; #$9D EOR #$A5 = #$38.

                JMP MONRTS      ; $3F5 - DISABLE &-VECTOR.
                JMP MON         ; $3F8 - LET CTL-Y ENTER MONITOR.
                JMP MON         ; $3FB - HNDL NON-MASK INTERUPTS.
                ASM_ADDR(MON)   ; ROUTN TO HNDL MASKABLE INTERUPTS
                ')


                                ; ==================================
                                ; DOSS INPUT INTERCEPT ROUTINE.
                                ; (INTERCEPTS ALL INPUT FROM
                                ; KEYBOARD OR DISK.)
                                ; ==================================

INPTINCP        JSR PREP4DOS    ; SAVE THE REGS & RESTORE I/O HKS
                                ; TO PT TO THE TRUE I/O HANDLERS.
                                ; ADJUST STK PTR & SAVE IT SO WE
                                ; CAN LATER RTN TO THE ROUTINE THAT
                                ; CALLED ROUTINE THAT CONTAINED
                                ; THE "JSR PREP4DOS" INSTRUCTION.
                LDA CONDNFLG    ; TEST IF DOING WARMSTART.
                BEQ INPUTWRM    ; YES - BRANCH IF WARM.

                ifelse(eval(VERSION < 320),1,`
                BPL INPUTCLD
                JMP KEYCOLD
INPUTCLD        LDA ASAVED

                ',`
                                ; READING FILE OR COLDSTARTING.

                PHA             ; SAVE THE CONDITION FLAG.
                LDA ASAVED      ; GET THE SUBSTITUTE CURSOR & PUT
                STA (BASL),Y    ; IT BACK ON THE SCREEN.
                PLA             ; GET CONDITION FLAG BACK IN (A).
                BMI INPUTCLD    ; BRANCH FOR COLD.
                JMP READTEXT    ; READ FILE BYTE.

                                ; DOING A COLDSTART.

INPUTCLD        JSR KEYCOLD     ; SET IMPORTANT PAGE 3 VECTORS.
                                ; SET MXFILVAL=3 & BUILD DOS BUFS.
                                ; SET WARMSTART FLAG.
                                ; EXECUTE PENDING COMMAND.
                LDY CH          ; GET HORZ CURSOR POSN.
                LDA #$60        ; GET CURSOR.
                ')

                STA (BASL),Y    ; REINSTATE CURSOR ON SCREEN.

                                ; USING WARMSTART STATUS.
                                ; AT THIS POINT IN TIME, BOTH
                                ; CONDNFLG & OPUTCOND = 0 FOR
                                ; WARM- & COLDSTARTS.
                ifelse(eval(VERSION < 320),1,`
                JMP READTEXT
                ')

INPUTWRM        LDA EXECFLAG    ; ARE WE EXECING?
                BEQ INPTNOXC    ; NO.
                ifelse(eval(VERSION < 320),1,`
                JMP READEXEC
                ',`
                JSR READEXEC    ; YES - GO READ AN EXEC FILE BYTE.
                ')
INPTNOXC        LDA #3          ; SET OUTPUT CONDITION = 3 BECAUSE
                STA OPUTCOND    ; WANT TO PROCESS INPUT INFO.
                JSR RESTOREG    ; RESTORE (A), (Y) & (X) REGS.
                JSR TOTRUIN     ; GET CHAR & PUT IT ON SCREEN VIA
                                ; THE TRUE OUTPUT HANDLER (COUT1).
                STA ASAVED      ; SAVE CHAR & (X).
                ifelse(eval(VERSION >= 320),1,`
                STX XSAVED
                ')
                JMP DOSEXIT     ; EXIT DOS.

TOTRUIN         JMP (KSW)       ; JUMP TO THE TRUE INPUT HANDLER.


                                ; =================================
                                ; DOSS OUTPUT INTERCEPT ROUTINE.
                                ; (INTERCEPTS ALL OUPUT TO SCREEN
                                ; OR OTHER PERIPHERALS.)
                                ; =================================

OPUTINCP        JSR PREP4DOS    ; SAVE REGS & RESTORE I/O HKS TO PT
                                ; TO TRUE I/O HANDLERS.  ADJUST STK
                                ; PTR & SAVE IT SO CAN LATER RTN
                                ; TO THE ROUTINE THAT CALLED THE
                                ; ROUTINE THAT CONTAINED THE
                                ; "JSR PREP4DOS" INSTRUCTION.

                                ; USE CURRENT OPUTCOND TO INDEX
                                ; TABLE CONTAINING ADDRS (MINUS 1)
                                ; OF THE OUTPUT CONDITION HANDLERS.
                                ; DO A "STACK JUMP" TO THE APPROPRIATE
                                ; HANDLER.

                LDA OPUTCOND
                ASL             ; TIMES 2 BECAUSE 2 BYTES / ADDRESS.
                TAX             ; SET (X) TO INDEX TABLE OF ADRS.
                LDA OUTHNDTB+1,X
                                ; PUT ADR OF OUTPUT HNDLR ON STK
                PHA             ; (HI BYTE FIRST) & THEN DO A
                LDA OUTHNDTB,X  ; "STACK JUMP" TO THE APPROPRIATE
                PHA             ; HANDLER.
                LDA ASAVED      ; GET CHAR TO BE PRINTED.
                RTS             ; EXECUTE THE STACK JUMP.


                                ; ================================
                                ; PREPARE FOR PROCESSING BY DOS.
                                ; SAVE THE REGS & RESET THE I/O
                                ; HOOKS TO POINT TO THE TRUE I/O
                                ; HANDLERS.
                                ; ================================

PREP4DOS        STA ASAVED      ; SAVE (A), (Y) & (X) REGS.
                STX XSAVED
                STY YSAVED
                TSX             ; ADJUST STK PTR & SAVE IT SO WHEN
                INX             ; WE LATER RESTORE IT & HIT AN
                INX             ; RTS, WE CAN RTN TO ROUTINE THAT
                STX STKSAVED    ; CALLED ROUTINE THAT CONTAINED
                                ; THE "JSR PREP4DOS" INSTRUCTION.

                                ; DISCONNECT DOS - THIS ENTRY POINT
                                ; IS FREQUENTLY USED BY ASSEMBLY
                                ; LANGUAGE PROGRAMMERS TO DISCONNECT
                                ; DOS COMPLETELY.

UNCONDOS        LDX #3          ; RESTORE THE I/O HKS TO PT TO THE
                                ; TRUE I/O HANDLERS, EX:
SETRUHKS        LDA CSWTRUE,X   ; KSWTRUE: KEYIN --> KSW: KEYIN
                STA CSW,X       ; CSWTRUE: COUT1 --> CSW: COUT1
                DEX
                BPL SETRUHKS    ; 4 BYTES (0 TO 3) TO MOVE.
                RTS


                                ; =================================
                                ; OUTPUT HANDLER 0.
                                ; (EVALUATE START OF LINE.)
                                ; =================================

OPUTHDL0
                ifelse(eval(VERSION >= 320),1,`
                LDX RUNTRUPT    ; CONTAINS A NONZERO VALUE IF RUN
                BEQ NONTRUPT    ; CMD WAS INTERRUPTED TO DO A LOAD
                JMP FINSHRUN    ; FINISH OFF THE RUN COMMAND.
                ')

                                ; FILE NOT BEING READ.

NONTRUPT        LDX CONDNFLG    ; GET CONDITION FLAG. CHECK IF WE
                                ; ARE DOING A WARMSTART ($00),
                                ; COLDSTART ($80), USING A(RAM)
                                ; ($C0) OR DOING A READ ($01).
                BEQ SETIGNOR    ; BRANCH IF WARMSTARTING.

                                ; DOING COLDSTART, READING FILE OR USING A(RAM).
                                ; CHECK (A) TO SEE IF USING "?" ASSOCIATED
                                ; WITH READING AN INPUT STATEMENT.

                CMP #HICHAR(`?')        ; IF READING, USING "?" AS PROMPT.
                BEQ OPUTHDL6    ; GO DSPLY INPUT CONDITIONALLY IF
                                ; GETTING READY TO READ A TEXT FILE
                                ; BYTE.
                CMP PROMPT      ; ARE WE PRINTING A PROMPT?
                ifelse(eval(VERSION < 320),1,`
                BEQ OPUTHDL6
                ',`
                BEQ SET2EVAL    ; BRANCH IF ABOUT TO PRINT PROMPT.
                ')
SETIGNOR        LDX #2          ; SET CONDITION 2 FOR DEFAULT TO
                STX OPUTCOND    ; SIGNAL SHOULD IGNORE NON-DOS
                                ; COMMANDS.

                CMP DCTRLCHR    ; IS CHAR = DOSS CTRL CHAR?
                BNE OPUTHDL2    ; NO.
                DEX             ; LINE STARTED WITH DOSS CTRL CHR
                STX OPUTCOND    ; SO SET CONDITION 1.
                DEX             ; (X) = 1 --> 0.
                STX NDX2INBF    ; INDEX TO 1ST POSN IN INPUT BUF


                                ; =================================
                                ; OUTPUT HANDLER 1.
                                ; - COLLECT THE DOS COMMAND.
                                ; (IE. - STICK CHAR IN THE INPUT
                                ; BUF & THEN GO DISPLAY CHAR OR
                                ; ELSE GO PARSE CMD.)
                                ; =================================

OPUTHDL1        LDX NDX2INBF    ; GET INDEX TO INPUT BUFFER.
PUTINBUF        STA BUF200,X    ; PUT CHAR IN INPUT BUFFER.
                INX             ; KICK UP INDEX FOR NEXT BUF POSN.
                STX NDX2INBF
                CMP #CR        ; WAS CHAR A <CR>?
                BNE DSPLYCMD    ; NO.
                JMP PARSECMD    ; YES - GOT END OF INPUT, SO NOW
                                ; SEE IF IT IS A DOS CMD.

                                ; =================================
                                ; OUTPUT HANDLER 2.
                                ; (IGNORE NON-DOS COMMAND.)
                                ; =================================

OPUTHDL2        CMP #CR        ; <CR>?
                BNE DSPLYALL    ; NO - (WAS AN "RH BRACKETT"?)
SET2EVAL        LDX #0          ; SET CONDITION0 - EVALUATE START
                STX OPUTCOND    ; OF LINE.
                JMP DSPLYALL    ; GO DISPLAY CHAR UNCONDITIONALLY.


                                ; =================================
                                ; OUTPUT HANDLER 3.
                                ; (PROCESS INPUT INFO.)
                                ; =================================

OPUTHDL3        LDX #0          ; SET CONDITION 0 WHEN INPUT ENDS.
                STX OPUTCOND
                CMP #CR
                BEQ ASUMIMED    ; YES.


TESTEXEC        LDA EXECFLAG    ; ARE WE EXECING?
                BEQ DSPLYALL    ; NO.
                BNE DSPLYINP    ; YES.  EXECFLAG CONTAINS THE 1ST
                                ; CHR OF NAME OF THE EXEC FILE.

ASUMIMED
                ifelse(eval(VERSION >= 320),1,`
                PHA             ; SAVE CHAR ON STK.
                SEC             ; (C)=1, DFLT, ASSUME IMMED MODE.
                LDA EXECFLAG    ; ARE WE EXECING?
                BNE TESTMODE    ; BRANCH IF EXECING.
                JSR CKBSCRUN    ; NOT EXECING, SO SEE IF BASIC IS
                                ; RUNNING A PRGM OR NOT.
                                ; (C) = 0, EITHER BASIC RUNNING.
                                ; (C) = 1 IF IMMEDIATE.
TESTMODE        PLA             ; RETRIEVE CHAR FROM STK.
                BCC TESTEXEC    ; BASIC RUNNING, DSPLY INPUT & XIT
                ')

                                ; EXECING OR IN IMMEDIATE MODE
                                ; (BECAUSE (C) = 1).

                LDX XSAVED      ; RETRIEVE (X).
                JMP PUTINBUF    ; GO PUT CHR IN INPUT BUF (COND 1)


                                ; =================================
                                ; OUTPUT HANDLER 4.
                                ; (WRITING DATA.)
                                ; =================================

OPUTHDL4        CMP #CR        ; <CR>?
                BNE CMWRTBYT    ; NO.
                LDA #5          ; SET CONDITION 5.
                STA OPUTCOND
CMWRTBYT        JSR WRITEXT     ; GO WRITE DATA BYTE.
                JMP DSPLYOUT    ; DISPLAY OUTPUT CONDITIONALLY.


                                ; ===================================
                                ; OUTPUT HANDLER 5.
                                ; (EVALUATE START OF DATA TO WRITE)
                                ; ===================================

OPUTHDL5        CMP DCTRLCHR    ; IS CHAR = DOSS CTRL CHAR?
                                ; ************* NOTE ***********
                                ; * DOSS CTRL CHAR CANCELS THE
                                ; * WRITE MODE.
                                ; ******************************

                BEQ OPUTHDL0    ; YES - SO GO TO CONDITION 0.
                CMP #$8A        ; IS CHAR AN <LF>?
                BEQ CMWRTBYT    ; YES -GO WRITE IT, STAY IN COND 5
                LDX #4          ; NO -RESET TO CONDITION4 - SIGNAL
                STX OPUTCOND    ; WANT TO WRITE ANOTHER LINE.
                BNE OPUTHDL4    ; ALWAYS.


                                ; =================================
                                ; OUTPUT HANDLER 6.
                                ; (SKIP THE QUESTION MARK PROMPT.)
                                ; =================================

OPUTHDL6
                LDA #0          ; SET CONDITION 0.
                STA OPUTCOND
                BEQ DSPLYINP    ; ALWAYS.
                                ; GO CONDITIONALLY DISPLAY INPUT.


                                ; =================================
                                ; FINISH OFF THE RUN COMMAND
                                ; (BECAUSE IT WAS INTERRUPTED TO
                                ; DO A LOAD.)
                                ; =================================

FINSHRUN
                ifelse(eval(VERSION >= 320),1,`
                LDA #0          ; ZERO OUT THE RUN INTERRUPT FLAG.
                STA RUNTRUPT
                JSR INITIOHK    ; RESET I/O HOOKS TO PT AT DOS.
                JMP RUNFPINT    ; JUMP BACK INTO THE RUN COMMAND
                                ; TO FINISH IT OFF.
                                ; ************* NOTE ************
                                ; * THE STACK WAS RESET SO WE
                                ; * RETURN AT THE CORRECT LEVEL.
                                ; *******************************
                ')


                                ; ======================================
                                ; COMMON ROUTN TO FINISH OFF MOST DOS
                                ; CMDS.  THE WRITE & READ CMD HNDLRS
                                ; RTN HERE.  CMDWRITE ($A510) SETS
                                ; OPUTCOND=5 BEFORE RETURNING.
                                ; CMDREAD ($A51B) RTNS WITH CONDNFLG=1.
                                ; ======================================

FINSHCMD        LDA BUF200      ; GET FIRST CHAR IN BUF.
                CMP DCTRLCHR    ; WAS CMD DONE VIA DOSS CTRL CHR?
                BEQ DSPLYCMD    ; YES.


                                ; CANCEL CMD BY REPLACING THE CHAR
                                ; WITH A <CR> & THEN FALL THRU TO
                                ; CONTINUE THE EXIT SEQUENCE.
                ifelse(eval(VERSION < 320),1,`
                LDA #$A0
                STA BUF200
                LDA #CR
                STA BUF200+1
                ',`
                LDA #CR
                STA BUF200      ; SET 200: 8D.
                ')

                LDX #0          ; SET INDEX TO START OF INPUT BUF.
                STX XSAVED


                                ; ---------------------------------------
                                ; DISPLAY CHARACTER OUPUT CONDITIONALLY.
                                ; (THAT IS, PREPARE TO SEND THE CHAR
                                ; TO THE OUTPUT DEVICE.)
                                ; ---------------------------------------

DSPLYCMD        LDA #%01000000  ; SET BIT6 TO SEE IF USING "MON C"
                BNE DSPLYCHR    ; ALWAYS.
DSPLYOUT        LDA #%00010000  ; SET BIT4 TO SEE IF USING "MON O"
                BNE DSPLYCHR    ; ALWAYS.
DSPLYINP        LDA #%00100000  ; SET BIT5 TO SEE IF USING "MON I"
DSPLYCHR        AND CIOCUMUL    ; TEST FLAG:  SEE IF SHOULD DSPLY.
                BEQ DOSEXIT     ; NO DISPLAY -SPECIFIC BIT WAS OFF
                                ; MON/NOMON CLR/SET SPECIFIC BITS.


                                ; ---------------------------------
                                ; DISPLAY THE CHARACTER.
                                ; ---------------------------------

DSPLYALL        JSR RESTOREG    ; RESTORE (A), (Y) & (X) REGS.
                JSR GODSPLY     ; OUTPUT  CHR VIA TRU OUTPUT HNDLR
                ifelse(eval(VERSION >= 320),1,`
                STA ASAVED      ; SAVE (A), (Y) & (X) REGS.
                STY YSAVED
                STX XSAVED
                ')


                                ; =================================
                                ; ROUTINE TO EXIT DOS.
                                ; POINT THE I/O HKS AT DOS AND
                                ; RESET THE STACK POINTER.
                                ; ---------------------------------

DOSEXIT         JSR INITIOHK    ; RESET I/O HKS TO POINT TO DOS.
                LDX STKSAVED    ; RETRIEVE SAVED STACK POINTER.
                TXS
RESTOREG        LDA ASAVED      ; RESTORE (A), (Y) & (X) REGS.
                LDY YSAVED
                LDX XSAVED
                ifelse(eval(VERSION >= 320),1,`
                SEC             ; WHY?????
                ')
                RTS             ; ************ NOTE *************
                                ; * IF THIS RTS IS ENCOUNTERED
                                ; * VIA A FALL THRU FROM DOSEXIT,
                                ; * THEN RTN TO THE ROUTINE THAT
                                ; * CALLED THE ROUTINE THAT
                                ; * CONTAINED THE "JSR PREP4DOS"
                                ; * INSTRUCTION.
                                ; *******************************


                                ; =================================
                                ; CHARACTER OUTPUT HANDLER.
                                ; =================================

GODSPLY         JMP (CSW)       ; USUALLY POINTS TO THE TRUE OUTPUT
                                ; HANDLER (COUT1, $FDF0 IF SCRN).

CRVIADOS
                ifelse(eval(VERSION < 320),1,`
                BIT CIOCUMUL
                BVC   L1F6E
                ')

                LDA #CR

                ifelse(eval(VERSION < 320),1,`
                JSR GODSPLY
L1F6E           RTS
                ',`
                JMP GODSPLY     ; USUALLY PRINTS A <CR> THRU THE
                                ; OUTPUT HANDLER (COUT1). HOWEVER,
                                ; WHEN ACCESSED BY RUNFPINT($A4DC)
                                ; DURING A COLDSTART, GOES INTO
                                ; DOSS OUTPUT INTERCEPT ROUTINE
                                ; (OPUTINCP, $9EBD).
                ')


                                ; =================================
                                ; DOSS COMMAND PARSING ROUTINE.
                                ; =================================

PARSECMD        LDY #$FF        ; INITIALIZE INDEX TO CMD TXT TBL.
                STY NDX2CMD
                INY             ; (Y) = 0.
                STY NEXTCMD     ; SIGNAL NO PENDING COMMAND FOR
                                ; NEXT TIME AROUND.
GETCHR1         INC NDX2CMD     ; INDEX TO COMMAND TEXT TABLE.
                                ; (0 ON ENTRY.)
                LDX #0          ; INITIALIZE INDEX TO INPUT CHARS.
                PHP             ; SAVE STATUS (WITH Z=1) ON STK.
                                ; (DFLT STATUS, ASSUME CHRS MTCH.)
                LDA BUF200,X    ; GET FIRST CHAR IN INPUT BUFFER.
                CMP DCTRLCHR    ; IF IT IS NOT DOSS CTRL CHAR,
                BNE SAVLINDX    ; SET LINE INDEX TO 0.  IF IT IS
                INX             ; DOSS CTRL CHAR, SET INDEX TO 1
                                ; SO SKIP CTRL CHAR.

                                ; DOES THE INPUT CHAR EQUAL A CHAR
                                ; IN DOSS CMD TEXT TABLE (CMDTXTBL)?
                                ; (NOTE:  LAST CHAR IN @ CMD IS NEGATIVE
                                ; ASCII, REST OF CHARS IN A GIVEN CMD
                                ; ARE POSITIVE ACSII.)

SAVLINDX        STX NDX2INBF    ; SAVE INDEX TO INPUT BUF.
INVSCMD         JSR PURGECMD    ; GET CHAR FROM INPUT BUFFER
                                ; (IGNORE SPACES).
                AND #$7F        ; STRIP HI BIT OFF OF CHAR.
                EOR CMDTXTBL,Y  ; DOES INPUT CHAR MATCH A CMD CHR?
                                ; IF POS INPUT CHAR/POS ASCII CMD
                                ; CHAR MATCH, THEN (A) = 0.
                                ; IF POS INPUT CHR / NEG ASCII CMD
                                ; CHAR MATCH, THEN (A) = $80.
                INY             ; KICK UP INDEX TO NEXT CHAR IN
                                ; THE COMMAND TEXT TABLE.
                ASL             ; IF POS/POS MATCH (A)=0 & (C)=0.
                                ; IF POS/NEG MTCH (A)=$80 & (C)=1.
                BEQ CKIFCHRS    ; CHAR MATCHED SO GO CHK CARRY.

                                ; INPUT CHARS < > TEXT CMD CHAR.

                PLA             ; PULL SAVED STATUS OFF STK TO CLR
                                ; Z-FLG (BECAUSE IF SAVED STATUS
                                ; HAD (Z) OR (C) = 1, THEN NEW (A)
                                ; WILL HAVE AT LEAST 1 BIT SET SO
                                ; THEN (A) < > 0.)
                PHP             ; PUSH STATUS ON STK (WITH Z-FLAG
                                ; CLR & (C) CONDITIONED AS PER
                                ; ABOVE "ASL" INSTRUCTION.

                                ; SEE IF THER ARE ANY MORE CHARS TO
                                ; CHECK IN THE TEXT OF A GIVEN CMD.

CKIFCHRS        BCC INVSCMD     ; IF (C)=0, MORE CHARS TO CHK IN
                                ; GIVEN CMD LISTED IN TABLE.
                                ; IF (C)=1, CHKD LAST CHR IN TBL.

                                ; FINISHED CHECKING TEXT
                                ; OF A PARTICULAR COMMAND.

                PLP             ; GET STATUS OFF STACK.

                                ; DID COMMAND MATCH?

                BEQ PRPDOCMD    ; IF LAST CHR MATCHED, THEN ENTIRE
                                ; CMD MATCHED SO GO PROCESS CMD.

                                ; CHECK IF SEARCHED ENTIRE TABLE.

                LDA CMDTXTBL,Y  ; LAST CHAR DIDNT MATCH, SO NOT
                BNE GETCHR1     ; CORRECT CMD.  THERE4, GO CHK IF
                                ; NEXT CHAR BYTE IN TABLE IS $00.
                                ; IF NOT $00, GO CHK REMAINING
                                ; CMDS IN TBL.  IF IT IS $00, THEN
                                ; DONE ENTIRE TBL & NO CMDS MTCHD.

                                ; EITHER AT END OF TABLE AND NO
                                ; CMDS MATCHED OR ELSE DETECTED A
                                ; BSAVE CMD WITH NO ACCOMPANYING
                                ; A- OR L- PARAMETERS.

                                ; CHECK IF DOSS CTRL CHAR WAS USED.

CKIFCTRL        LDA BUF200      ; IS 1ST CHAR IN THE INPUT BUFFER
                CMP DCTRLCHR    ; EQUAL TO DOSS CTRL CHAR?
                BEQ CHKIFCR     ; YES.
                JMP DSPLYALL    ; NO -GO DSPLY CHR & THEN XIT DOS.

                                ; WAS DOSS CTRL CHAR THE ONLY CHAR ON LINE?

CHKIFCR         LDA BUF200+1    ; GET 2ND BYTE IN INPUT BUFFER.
                CMP #CR         ; WAS IT A <CR>?
                BNE PRSYNERR    ; NO -NOT SIMPLY DEALING WITH CMD
                                ; CANCELLING DOS CTRL CHR AND <CR>
                                ; SO GO ISSUE A SYNTAX-ERROR MSG.
                JSR RESTAT0     ; YES - SET CONDITION 0.
                JMP DSPLYCMD    ; GO DISPLAY <CR>.
PRSYNERR        JMP SYNTXERR    ; EITHER A CTRL CHR DENOTED THAT A
                                ; DOS CMD WAS WANTED & NO MATCHING
                                ; CMD WAS FOUND (ELSE DETECTED A
                                ; BSAVE CMD WITH NO A- OR L-PARMS)
                                ; SO GO GIVE DOSS SYNTAX ERR MSG.


                                ; ========================================
                                ; PREPARE TO EXECUTE THE DOS COMMAND.
                                ; ON ENTRY - A DOS CMD WAS PARSED.
                                ; - NDX2CMD = COMMAND CODE.
                                ; - I/O HKS PT TO TRUE HANDLERS.
                                ; NOTE THAT THIS ROUTINE MAKES EXTENSIVE
                                ; USE OF A TABLE (CMDATTRB, $A909-$A940)
                                ; WHICH CONTAINS AN ENCODED LIST OF THE
                                ; ATTRIBUTES ASSOCIATED WITH @ COMMAND.
                                ; ========================================

PRPDOCMD
                ifelse(eval(VERSION < 320),1,`
                LDA NDX2CMD
                ASL  
                STA NDX2CMD
                TAY
                ',`
                ASL NDX2CMD     ; DOUBLE INDEX BECAUSE 2 BYTES/ADDR
                LDY NDX2CMD     ; IN TABLE OF DOS CMD ENTRY PTS.
                JSR CKBSCRUN    ; CHECK IF BASIC IS RUNNING A PGM:
                BCC CHKIFRUN    ; (C)=0= BASIC RUNNING.
                                ; (C)=1= BASIC NOT RUNNING.

                                ; USING IMMEDIATE MODE, SO NOW CHK
                                ; IF CMD IS LEGAL IN THAT MODE.

IMMED           LDA #%00000010  ; CHK BIT1 OF CMDATTRB TO SEE IF
                AND CMDATTRB,Y  ; CMD IS LEGAL IN THAT MODE.
                BEQ CHKIFRUN    ; BRANCH IF LEGAL.
NODIRCMD        LDA #15         ; SET RETURN CODE TO SIGNAL THAT
                JMP ERRHNDLR    ; WE GOT A NOT-DIRECT-COMMAND ERR.

                                ; RUNNING PROGRAM OR ELSE COMMAND
                                ; COMPLIES WITH IMMEDIATE MODE.

CHKIFRUN        CPY #6          ; CHECK TO SEE IF CMD WAS A "RUN".
                BNE TST4NAME    ; BRANCH IF NOT.
                STY PROMPT      ; PUT AN $06 IN PROMPT IF COMMAND
                                ; WAS A "RUN".

                                ; CHECK TO SEE IF A FILENAME
                                ; IS APPLICABLE TO THE COMMAND.
                ')

TST4NAME        LDA #%00100000  ; BIT5 = 1 IF FILENAME APPLICABLE.
                AND CMDATTRB,Y
                BEQ FNOTAPPL    ; BRANCH IF NAME NOT APPLICABLE.
                                ; (EX. CMDS: CATALOG, PR#, IN#,
                                ; MON, NOMON, MAXFILES, FP & INT.)

                                ; FILENAME APPLICABLE TO CMD ISSUED
                                ; SO BLANK OUT FILENAME BUFFERS IN
                                ; ANTICIPATION OF RECEIVING NAME.

FNXPCTD         JSR BLNKFNBF    ; BLANK BOTH 1ST & 2ND NAME BUFS.
                PHP             ; SAVE STATUS (Z-FLAG=1) ON STK.
                                ; NOTE:  Z-FLAG USED TO SIGNAL IF
                                ; DEALING WITH PRIMARY OR
                                ; SECONDARY NAME BUFFERS.
FNAMCHR1        JSR PURGECMD    ; GET 1ST CHAR IN NAME. (IGNORE
                                ; LEADING SPACES.)
                BEQ DONEFN      ; GOT "," OR <CR> - CHK IF DONE
                                ; (BECAUSE THESE NOT LEGAL FOR NAME).

                                ; CHK IF CHAR IS LEGAL TENDER FOR NAME.
                                ; (KNOW IT WASNT A <SPC>, "," OR <CR>
                                ; BUT IT STILL MAY NOT BE LEGAL.)

                ASL            ; (C) = HI BIT OF CHAR.
                BCC LGLFNCHR    ; IF INV OR FLSH, OK FOR NAME.
                                ; BUT, NOTE THAT THEY CAN ONLY BE
                                ; POKED INTO INPUT BUF FROM A
                                ; RUNNING PRGM. (THIS TECHNIQUE IS
                                ; USED IN MANY PROTECTN SCHEMES.)
                BMI LGLFNCHR    ; ACTUALLY TESTING BIT6 BECAUSE JUST
                                ; DID AN "ASL" IF BYTE IS 11XXXXXX
                                ; (IE. $C0, NORMAL "@" OR GREATER)
                                ; CHAR IS LEGAL FOR NAME.
                JMP CKIFCTRL    ; GOT ILLEGAL NAME CHAR SO GO DO A
                                ; FEW MORE CHKS ON CTRL CHRS, ETC
                                ; & EXIT DOS. (WAS IT A CTRL CHAR,
                                ; NUMBER OR ONE OF THE FOLLOWING
                                ; NORMAL CHARS:SPC, !, ", #, $, %,
                                ; &, APOS, (, ), *, +, COMMA, -, ., /
                                ; :, ;, <, -, >, OR ?

                                ; CHAR IS LEGAL TENDER FOR NAME.

LGLFNCHR        ROR             ; RESTORE NAME CHAR.
                JMP SVFNCHAR    ; SAVE IT IN 1ST OR 2ND NAME BUF.

                                ; PROCESS REST OF CHARS.

NCHR2ETC        JSR CMDCHAR     ; GET 2ND & SUBQUENT CHRS IN NAME
                BEQ DONEFN      ; GOT A <CR> OR "," SO GO CHK IF
                                ; JUST FINISHED SECOND FILENAME.

                                ; PUT CHARS IN FILENAME BUF.

SVFNCHAR        STA PRIMFNBF,Y  ; (Y)=OFFSET FRM PRIMARY NAME BUF.
                INY
                CPY #60         ; TOTAL OF 60 CHARS IN BOTH BUFS.
                BCC NCHR2ETC    ; HAVENT HIT ",", EOL MARKER (IE.
                                ; <CR>) OR DONE ALL 60 CHARS YET.

                                ; DONE ALL 60 CHARS SO IGNORE REST
                                ; OF CHARS UNTIL GET "," OR <CR>.

PURGEFN         JSR CMDCHAR     ; GET NEXT CHAR.
                ifelse(eval(VERSION < 320),1,`
                BEQ PURGEFN     ; BUG???
                ',`
                BNE PURGEFN     ; NOT $00 YET SO GO GET NXT CHAR.
                ')

                                ; JUST FINISHED NAME, SO CHK IF IT
                                ; WAS FIRST OR SECOND FILENAME.

DONEFN          PLP             ; RETRIEVE STATUS FROM STACK.
                BNE CKFN2LGL    ; Z-FLAG CLR SO DONE 2ND FILENAME.

                                ; JUST FINISHED FIRST NAME, SO SEE
                                ; IF A SECOND FILENAME IS REQUIRED.
                                ; (THAT IS, ARE WE DEALING WITH
                                ; THE "RENAME" COMMAND?)

FINFIRST        LDY NDX2CMD     ; GET INDEX ASSOC WITH CMD ISSUED.
                LDA #%00010000  ; CHK BIT 4 OF CMDATTRB TO SEE IF
                AND CMDATTRB,Y  ; 2ND NAME REQUIRED (IE RENAME ?).
                BEQ CKFN1LGL    ; 2ND NAME NOT APPLICABLE SO GO
                                ; & CHK 1ST NAME BUF.

                                ; SECONDARY FILENAME APPLICABLE SO
                                ; DEALING WITH "RENAME" COMMAND.

                LDY #30         ; (Y) = INDEX TO START OF 2ND NAME.
                PHP             ; PUT STATUS ON STK (Z-FLG = 0) TO
                BNE FNAMCHR1    ; SIGNAL DEALING WITH 2ND NAME &
                                ; GO BACK TO GET ITS CHARS.
                                ; ALWAYS TAKE BRANCH.

                                ; DONE PROCESSING ASSOC WITH SECONDARY
                                ; FILENAME SO NOW CHK IF ANY OF THESE
                                ; CHARS WERE LEGAL & THERE4 STUCK IN
                                ; THE SECONDARY FILENAME BUFFER.

CKFN2LGL        LDA SCNDFNBF    ; CHK 1ST BYTE IN SECONDARY.
                CMP #HICHAR(` ')        ; IF IT IS A <SPC>, THEN NO CHARS
                BEQ GOXITDOS    ; WERE GOOD & SO A 2ND NAME WAS
                                ; REQUIRED BUT NOT ISSUED,SO EXIT.

                                ; ONLY PRIMARY FILENAME APPLICABLE
                                ; SO CHK IF WE GOT ANY CHRS THAT
                                ; WERE LEGAL & THERE4 PUT IN THE
                                ; PRIMARY FILENAME BUFFER.

CKFN1LGL        LDA PRIMFNBF    ; IF 1ST CHAR IN BUF IS A <SPC>,
                CMP #HICHAR(` ')        ; A LEGAL PRIMARY FILENAME WAS NOT
                                ; ISSUED SO FALL THRU TO SEE IF IT
                                ; WAS REQUIRED OR OPTIONAL.
                BNE DFLTPRSD    ; BRANCH IF GOT PRIMARY FILENAME.

                                ; PRIMARY FILENAME WAS NOT ISSUED
                                ; SO CHK IF IT WAS REQUIRED OR OPTIONAL.
                                ; (IE. WAS CMD A CLOSE, LOAD, SAVE OR RUN?)

                LDY NDX2CMD     ; GET INDEX ASSOC WITH CMD.
                LDA #%11000000  ; CHK BITS 7 & 6 TO SEE IF A NAME
                AND CMDATTRB,Y  ; IS REQUIRED.
                BEQ GOXITDOS    ; A PRIMARY NAME IS REQUIRED BUT
                                ; WAS NOT ISSUED, SO GO EXIT DOS.

                                ; WAS COMMAND A "CLOSE"?

                BPL DFLTPRSD    ; NAME WASNT PRESENT, BUT IS NO
                                ; BIG DEAL BECAUSE IT WAS OPTIONAL.
GOXITDOS        JMP CKIFCTRL    ; CMD WAS LOAD, RUN OR SAVE WHICH
                                ; CAN ALSO BE BASIC COMMANDS.


                                ; ======================================
                                ; BLANK OUT BOTH PRIMARY ($AA75-$AA92)
                                ; AND SECONDARY ($AA93-$AAB0) FILENAME
                                ; BUFFERS.
                                ; ======================================

BLNKFNBF
                ifelse(eval(VERSION < 320),1,`
                LDA #0
                ')
                LDY #60         ; 30 BYTES IN EACH BUFFER.
BLNK1RST        LDA #HICHAR(` ')        ; BLANK.
STORBLNK        STA PRIMFNBF-1,Y
                DEY
                BNE STORBLNK    ; MORE BYTES TO BLANK OUT.
                RTS


                                ; ===================================
                                ; FILENAME NOT APPLICABLE TO COMMAND
                                ; (EX. CATALOG, MON, NOMON, FP, INT,
                                ; PR#, IN# OR MAXFILES).
                                ; ===================================

FNOTAPPL        STA PRIMFNBF    ; PUT A $00 IN THE FIRST BYTE OF
                                ; THE PRIMARY FILENAME BUFFER.
                                ; ************* NOTE ************
                                ; * ALTHOUGH THIS SEEMS LIKE A
                                ; * BEGNIN INSTRUCTION, IT IS
                                ; * IMPORTANT BECAUSE IT IS LATER
                                ; * USED TO INSURE THAT A MATCHING
                                ; * DOS FILENAME BUF WONT BE
                                ; * FOUND WHEN THE GETBUFF ROUTN
                                ; * ($A764) IS EVENTUALLY USED BY
                                ; * THE VARIOUS DOS CMDS.  AS A
                                ; * RESULT, THE HIGHEST NUMBERED
                                ; * (LOWEST IN MEMORY) FREE DOS
                                ; * FILENAME BUFFER WILL BE
                                ; * SELECTED.
                                ; *******************************

                                ; COMMAND DIDNT REQUIRE A FILENAME
                                ; SO NOW CHK & SEE IF IT EXPECTS ANY
                                ; NUMERIC ARGUMENTS.  (IE. WAS IT PR#,
                                ; IN# OR MAXFILES CMD?)

                LDA #%00001100  ; TEST BITS 2 & 3 TO SEE IF IN#,
                AND CMDATTRB,Y  ; PR# OR MAXFILES NUMERIC
                                ; OPERAND IS EXPECTED.
                BEQ DFLTPRSD    ; BRANCH IF NOT EXPECTED.

                                ; IN#,PR# OR MAXFILES NUMERIC
                                ; OPERAND EXPECTED.

INPRMAX         JSR CNVRTASC    ; CONVERT ASCII NUMBER ARGUMENT TO
                                ; HEX WITH RESULT IN A5L/H.
                ifelse(eval(VERSION < 320),1,`
                BCS GOXITDOS
                ',`
                BCS TOSYNTX     ; CHR NOT #, EXIT WITH SYNTAX ERR.
                ')
                TAY             ; (Y) = HI BYTE OF CONVERTED CHAR.
                ifelse(eval(VERSION < 320),1,`
                BNE GOXITDOS
                ',`
                BNE ARGRNGER    ; RANGE ERROR - BECAUSE VALUE > 255.
                ')
                CPX #17
                ifelse(eval(VERSION < 320),1,`
                BCS GOXITDOS
                ',`
                BCS ARGRNGER    ; RANGE ERROR BECAUSE IF MAXFILES OR
                                ; IN# OR PR# ARGUMENT > 16, THEN
                                ; VALUE TOO LARGE.
                ')

                                ; WAS COMMAND A PR# OR IN#?

                LDY NDX2CMD     ; CHK TO SEE IF A SLOT VALUE IS
                LDA #%00001000  ; APPLICABLE TO THE COMMAND.
                AND CMDATTRB,Y
                BEQ MAXFMIN     ; SLOT VAL NOT APPLICABLE SO MUST
                                ; BE DEALING WITH MAXFILES.

                                ; COMMAND WAS PR# OR IN# SO NOW CHECK
                                ; IF SLOT VALUE IS TOO LARGE OR NOT.
                                ; (LEGAL RANGE IS 0 TO 7.)

                CPX #8          ; TOO LARGE?
                BCS GOXITDOS    ; YES -BAIL OUT.
                BCC DFLTPRSD    ; NO -SET DFLTS & CONTINUE PARSING

                                ; CHECK MINIMUM VALUE FOR MAXFILES.
                                ; (LEGAL RANGE IS 1 TO 16.)

MAXFMIN         TXA
                ifelse(eval(VERSION < 320),1,`
                BEQ GOXITDOS
                ',`

                BNE DFLTPRSD    ; NOT 0, SO OKAY.

                                ; ARGUMENT FOR MAXFILES, SLOT, IN#
                                ; OR PR# WERE ILLEGAL.

ARGRNGER        LDA #2          ; SET RETURN CODE FOR RANGE ERROR.
                JMP ERRHNDLR    ; GO HANDLE THE ERROR.

TOSYNTX         JMP SYNTXERR    ; EXIT VIA SYNTAX ERROR.
                ')
                                ; INITIALIZE CUMLOPTN & PARSED TABLE.

DFLTPRSD        LDA #0
                STA CUMLOPTN    ; SET CUMLOPTN TO DEFAULT VAL OF 0
                                ; TO ASSUME NO OPTIONS ISSUED.
                STA MONPRSD     ; SET DEFAULT VALS IN PARSED TBL.
                ifelse(eval(VERSION < 320),1,`
                STA VOLPRSD2    ; THAT IS ASSUME THAT:
                STA LENPRSD+1   ; - C, I, O WERENT ISSUED.
                STA TEMPBYT
                ',`
                STA VOLPRSD    ; THAT IS ASSUME THAT:
                STA LENPRSD     ; - C, I, O WERENT ISSUED.
                STA LENPRSD+1   ; - VOL # AND LENGTH ARE 0.
                JSR ZEROPTCH    ; SET TEMPBYT & BYTPRSD TO 0 DFLTS
                ')
                LDA NDX2INBF    ; IRREL, MIGHT AS WELL BE 3 "NOP"S
                                ; (MADE OBSOLETE BY ZEROPTCH).

                                ; DO MORE PARSING OF COMMAND LINE.
                                ; (IGNORE ANY COMMAS OR SPACES.)

NXCMDCHR        JSR PURGECMD    ; GET NEXT CHAR.
                BNE CHKOPTNS    ; IF IT ISNT A <CR> OR COMMA THEN
                                ; MAYBE ITS AN OPTION, SO TAKE
                                ; BRANCH TO CHECK IT OUT.
                CMP #CR         ; WAS IT A <CR>?
                BNE NXCMDCHR    ; NO - SO MUST HAVE BEEN A COMMA.
                                ; BRANCH BACK TO IGNORE COMMAS.

                                ; GOT A <CR> (IE. EOL MARKER), SO NOW
                                ; DONE PARSING & MUST MAKE SURE THAT
                                ; THE CUMMULATIVE RECORD OF THE OPTIONS
                                ; WE ENCOUNTERED IS APPLICABLE TO THE CMD.

                LDX NDX2CMD
                LDA CUMLOPTN    ; CHK IF OPTIONS ARE LEGAL.
                ORA CMDATTRB+1,X
                EOR CMDATTRB+1,X
                BNE GOXITDOS    ; ILLEGAL SO EXIT.

                                ; CUMMULATIVE RECORD OF OPTIONS CAN
                                ; LEGALLY BE ASSOCIATED WITH COMMAND.

                LDX TEMPBYT     ; TEMPBYT = 0 AS SET IN ZEROPTCH.
                ifelse(eval(VERSION < 320),1,`
                BEQ TORESTAT0
                ',`
                BEQ TODOSCMD    ; ALWAYS.
                ')

                                ; MEANINGLESS INSTRUCTIONS (MADE
                                ; OBSOLETE BY INCLUSION OF ZEROPTCH).

                STA TEMPBYT
                STX NDX2INBF
                BNE NXCMDCHR

                                ; CHECK IF CHAR IS AN OPTION.
                                ; (IE. A, B, R, L, S, D, V, C, I, O.)

CHKOPTNS        LDX #10
CKNXOPTN        CMP OPTNTXT-1,X
                BEQ OPTNOK      ; FOUND AN OPTION.
                DEX
                BNE CKNXOPTN    ; HAVENT CHKD ALL OPTIONS YET.
TOTOSYNT
                ifelse(eval(VERSION < 320),1,`
                BEQ TODOSCMD
                ',`
                BEQ TOSYNTX     ; COULDNT FIND A MATCH.
                                ; (SYNTAX ERR - CHAR NOT OPTION.)
                                ; GOT AN OPTION SO CHK IF IT WAS
                                ; "C", "I" OR "O".  (IE. IS A
                                ; NUMERIC ARGUMENT EXPECTED?)
                ')

OPTNOK          LDA OPTNISSD-1,X
                BMI CIOPTNS     ; IF HI BIT=0, THEN OPTION WAS A
                                ; "C", "I" OR "O" AND NO NUMERIC
                                ; ARGUMENT IS NEEDED.

                                ; UPDATE CUMLOPTN TO REFLECT
                                ; THE LATEST OPTION.

                ORA CUMLOPTN
                STA CUMLOPTN

                                ; NOW CHK IF NUMERIC ARGUMENT THAT
                                ; WAS ISSUED WITH OPTION IS LEGAL.

                DEX             ; REDUCE COUNTER THAT WAS KICKED UP
                                ; IN ANTICIPATION OF MORE CHARS
                                ; IN THE CMDCHAR ROUTINE.
                STX NDX2OPTN    ; SAVE INDEX TO OPTION.
                JSR CNVRTASC    ; CONVERT ASCII # TO HEX.

                                ; WAS IT A NUMERIC CHARACTER?

                ifelse(eval(VERSION < 320),1,`
                BCS TODOSCMD
                ',`
                BCS TOSYNTX     ; NO - SYNTAX ERROR.
                ')

                                ; CHARACTER WAS NUMERIC.

                LDA NDX2OPTN    ; RETRIEVE INDEX TO OPTION.
                ASL             ; TIMES 4 BECAUSE GOING TO CHK MIN
                ASL             ; & MAX VALS OF LEGAL RANGES ASSOC
                                ; WITH OPTION (2 BYTES @).
                TAY             ; (Y) = INDEX TO LEGAL RANGE TBL.

                                ; CHECK IF ARGUMENT IS TOO LARGE.

                LDA A5H         ; GET HI BYTE OF ARGUMENT.
                BNE CKMAXVAL    ; BRANCH IF NOT 0.
                LDA A5L         ; HI BYTE WAS 0 SO CHK LOW BYTE.
                CMP OPTNRNG,Y
                ifelse(eval(VERSION < 320),1,`
                BCC TODOSCMD
                ',`
                BCC ARGRNGER    ; RANGE ERR -ARGUMENT < MIN LEGAL.
                ')

                                ; CHECK IF ARGUMENT < = MAX LEGAL PLUS 1.

                LDA A5H
CKMAXVAL        CMP OPTNRNG+3,Y ; CMP HI BYTE TO MAX LEGAL VAL.
                BCC SVALOPTN    ; LESS THAN MAX SO ARGUMENT OK.
TOARGRNG
                ifelse(eval(VERSION < 320),1,`
                BNE TODOSCMD
                ',`
                BNE ARGRNGER    ; ARGUMENT > MAX LGL, SO RNG ERR.
                ')
                LDA A5L         ; NOW CHK IF LOW BYTE OF ARGUMENT
                CMP OPTNRNG+2,Y ; COMPLIES TO MAX LEGAL LOW BYTE.
                BCC SVALOPTN    ; ARGUMENT IS LEGAL.
                ifelse(eval(VERSION < 320),1,`
                BNE TODOSCMD
                ',`
                BNE TOARGRNG    ; ARGUMENT IS ILLEGAL.
                ')

                                ; SAVE THE OPTION VALUE IN THE PARSED TABLE.

SVALOPTN        LDA TEMPBYT     ; OBSOLETE, TEMPBYT WAS SET TO 0
                BNE NXCMDCHR    ; IN ZEROPTCH SO ALWAYS FALL THRU.
                TYA             ; (Y)-->(A)=INDEX TO OPTION RNGS.
                LSR             ; DIVIDE BY 2 BECAUSE @ OPTION RANGE
                                ; TABLE HAS 4 BYTES, BUT @ PARSED
                                ; VAL ENTRY IS ONLY 2 BYTES LONG.
                TAY             ; PUT INDEX TO PARSED TABLE IN (Y).
                LDA A5H         ; STORE ARGUMENT IN PARSED TBL.
                STA VOLPRSD+1,Y
                LDA A5L
                STA VOLPRSD,Y

                                ; GO SEE IF ANY MORE OPTIONS ARE
                                ; PRESENT ON THE COMMAND LINE.

TONXOPTN        JMP NXCMDCHR

                                ; OPTION WAS A "C", "I" OR "O".

CIOPTNS         PHA             ; PUT (A) = OPTNISSD ON STK.
                LDA #%10000000  ; UPDATE CUMLOPTN TO SIGNAL THAT
                ORA CUMLOPTN    ; "C", "I" OR "O" OPTIONS ISSUED.
                STA CUMLOPTN
                PLA             ; GET (A) = OPTNISSD BACK FRM STK.
                AND #%01111111  ; TURN HI BIT OFF.
                ORA MONPRSD     ; UPDATE MONPRSD IN PARSED TABLE.
                STA MONPRSD
                BNE TONXOPTN    ; GO SEE IF ANY MORE OPTIONS.
                ifelse(eval(VERSION >= 320),1,`
                BEQ TOTOSYNT    ; IRRELEVANT.
                ')


                                ; =================================
                                ; FINAL PROCESSING OF DOS COMMAND.
                                ; =================================

TODOSCMD
                ifelse(eval(VERSION < 320),1,`
                JMP CKIFCTRL
TORESTAT0       JSR RESTAT0     ; RESET CONDITION TO 0.  THAT IS,
                                ; SET CONDNFLG AND OPUTCOND = 0.
                JSR CLRFMPRM    ; CLEAR OUT THE FM PARAMETER LIST
                                ; SO WE CAN CUSTOMIZE IT IN
                                ; ACCORDANCE WITH THE SPECIFIC DOS
                                ; COMMAND HANDLER CALLED.
                ')
                JSR DODOSCMD    ; GO DO THE DOS COMMAND.


                                ; ---------------------------------------
                                ;
                                ; - MOST, BUT NOT ALL, DOS CMDS RTN HERE.
                                ; - IF AN ERROR IS ENCOUNTERED, EXECUTION
                                ; EXITS DOSS ERROR HANDLER VIA RESTART
                                ; ($D43C) OR BASICS ERROR-HANDLING
                                ; ROUTINE (BSCERHLR, $D865).
                                ; - FP EXITS BACK INTO DOSS COLDSTART
                                ; ROUTINE (DOSCOLD, $9D84).
                                ; - INT & CHAIN GO INTO INTEGER BASIC IF
                                ; THE INTEGER LANGUAGE IS AVAILABLE.
                                ; - THE WRITE & READ CMDS RETURN TO THE
                                ; FINSHCMD ROUTINE ($9F83) SHOWN BELOW.
                                ; - BLOAD RTNS TO AFTRCMD IF IT WAS NOT
                                ; CALLED BY THE BRUN CMD.  OTHERWISE,
                                ; BLOAD RTNS TO THE BRUN CMD HNDLR AT
                                ; $A391.
                                ; - BRUN EXECUTES THE BINARY FILE BEFORE
                                ; RETURNING. IF THE BRUNED PGM PERFORMED
                                ; ANY INPUT OR OUTPUT, OR IF THE BINARY
                                ; FILE WAS BRUN WITH MON IN EFFECT, THE
                                ; PRGM GETS HUNG UP IN AN ININITE LOOP.
                                ; (SEE FORMATTED DISASSEMBLY OF THE CMD
                                ; PARSING & PROCESSING ROUTINES FOR
                                ; FURTHER DETAILS.)
                                ; - THE LOAD CMD GOES INTO APPLESOFT (AT
                                ; $D4F2) TO RESET THE PRGM LINK PTRS
                                ; AND THEN GOES ON TO THE RESTART
                                ; ROUTINE ($D43C).  IF THE LOAD CMD WAS
                                ; CALLED FROM A RUN, EXECUTION JUMPS
                                ; BACK INTO THE RUN CMD HNDLR AT
                                ; RUNFPINT ($A4DC).
                                ; - THE RUN CMD EXITS INTO APPLESOFT AT
                                ; STKINI ($D683) & EVENTUALLY RTNS TO
                                ; THE RESTART ROUTINE ($D43C).
                                ;
                                ; -----------------------------------------

AFTRCMD         JMP FINSHCMD



                                ; =================================
                                ; DO THE DOS COMMAND.
                                ; =================================

DODOSCMD
                ifelse(eval(VERSION >= 320),1,`
                JSR RESTAT0     ; RESET CONDITION TO 0.  THAT IS,
                                ; SET CONDNFLG AND OPUTCOND = 0.
                JSR CLRFMPRM    ; CLEAR OUT THE FM PARAMETER LIST
                                ; SO WE CAN CUSTOMIZE IT IN
                                ; ACCORDANCE WITH THE SPECIFIC DOS
                                ; COMMAND HANDLER CALLED.
                ')
                LDA NDX2CMD     ; GET (A) = INDEX TO COMMAND.
                TAX             ; (X) = INDEX TO TBL OF ENTRY PTS.
                LDA CMDTBL+1,X  ; GET ADR-1 OF THE CMDS ROUTINE &
                PHA             ; PUT IT ON STACK (HI BYTE 1ST).
                LDA CMDTBL,X
                PHA
                RTS             ; DO A "STK JMP" TO PROCESS CMD.


                                ; =================================
                                ; GET CHAR FROM INPUT BUFFER, SET
                                ; Z-FLAG IF <CR> OR COMMA.
                                ; =================================

CMDCHAR         LDX NDX2INBF    ; (X) = INDEX TO INPUT BUFFER.
                ifdef(`FRANKLIN',`
                JSR FRANK
                ',`
                LDA BUF200,X    ; GET NEXT CHAR.
                ')
                CMP #CR         ; IS IT A <CR>?
                BEQ CMDCHRTS    ; YES.
                INX             ; (X)=INDEX FOR NXT ANTICIPATED CHR.
                STX NDX2INBF
                CMP #HICHAR(`,')        ; COMMA?
CMDCHRTS        RTS             ; EXIT WITH Z-FLAG CONDITIONED.
                                ; Z=1 IF CHAR IS A <CR> OR COMMA.


                                ; ========================================
                                ; GET 1ST NON-SPACE CHAR FROM INPUT BUF.
                                ; SET Z-FLAG IF IT IS <CR> OR ",".
                                ; ========================================

PURGECMD        JSR CMDCHAR     ; GET 1ST CHAR.
                BEQ CMDCHRTS    ; EXIT IF <CR> OR COMMA.
                CMP #HICHAR(` ')        ; SPACE?
                BEQ PURGECMD    ; YES - IGNORE LEADING SPACES.
                RTS


                                ; =================================
                                ; CLEAR OUT THE FM PARAMETER LIST.
                                ; - SO WE CAN CUSTOMIZE IT IN
                                ; ACCORANCE WITH THE SPECIFIC
                                ; DOS COMMAND HANDLER CALLED.
                                ; =================================

CLRFMPRM        LDA #0
                LDY #$16        ; 22 BYTES TO ZERO OUT.
ZFMPARM         STA FMPRMLST-1,Y
                                ; STORE ZERO BYTE.
                DEY             ; $16 --> $01, EXIT AT $00.
                BNE ZFMPARM
                RTS


                                ; ======================================
                                ; CONVERT ASCII TO HEX OR DEC.
                                ; ON ENTRY: NDX2INBF INDEXES INPUT BUF.
                                ; ON EXIT: A5L/H AND (X,A) = LOW/HI
                                ; BYTES OF RESULT.
                                ; (C) = 0 = GOOD CONVERSION.
                                ; (C) = 1 = INVALID CHARS.
                                ; ======================================

CNVRTASC        LDA #0          ; ZERO OUT LOCS TO HOLD RESULT.
                STA A5L         ; LOW BYTE OF RESULT.
                STA A5H         ; HI BYTE OF RESULT.
                JSR PURGECMD    ; GET 1ST NON-SPACE CHAR.
                PHP             ; SAVE STATUS (Z-FLAG) ON STK.
                                ; (IF <CR> OR COMMA, Z-FLAG = 1.)

                                ; CHK TO SEE IF WANT TO CONVERT
                                ; ASCII TO HEX OR ASCII TO DEC.

                CMP #HICHAR(`$')        ; IS HEX SYMBOL PRESENT?
                BEQ ASC2HEX     ; YES - BRANCH FOR ASCII TO HEX.


                                ; --------------------------------
                                ; ASCII TO DEC CONVERSION WANTED.
                                ; --------------------------------

                PLP             ; GET STATUS DENOTING IF CR OR ","
                JMP CKIFDONE    ; BEGIN DEC CONVERSION OF 1ST CHR

ASC2DEC         JSR PURGECMD    ; GET 2ND & SUBSEQUENT ASCII CHARS
                                ; TO BE CONVERTED TO DECIMAL.
                                ; (IGNORE SPACES.)

CKIFDONE        BNE SUBTRASC    ; BRANCH IF NOT <CR> OR COMMA.
                                ; (ALWAYS FALL THRU IF ACCESSED FRM
                                ; THE HEX CONVERSION ROUTINE.)

                                ; SUCCESSFUL CONVERSION - EXIT
                                ; WITH A5L/H AND (X,A) CONTAINING
                                ; LOW/HI BYTES OF RESULT.

                LDX A5L         ; RESULT LOW.
                LDA A5H         ; RESULT HI.
                CLC             ; (C)=0 TO SIGNAL GOOD CONVERSION.
                RTS             ; EXIT TO CALLER OF CNVRTASC.

                                ; CHECK VALIDITY OF ASCII CHARS FOR
                                ; REPRESENTATION OF DECIMAL NUMBERS.

SUBTRASC        SEC
                SBC #$B0        ; SUBTRACT ASCII "0".
                BMI NOTASCII    ; ERROR BECAUSE < 0.
                CMP #$0A        ; DECIMAL 10.
                BCS NOTASCII    ; ERROR BECAUSE > 9.

                                ; MULTIPLY RUNNING RESULT * 10
                                ; AND THEN ADD NEW DIGIT.

                JSR DOUBLE      ; GET RESULT * 2.
                ADC A5L
                TAX             ; (X) = LOW RESULT * 2 + NEW DIGIT
                LDA #0
                ADC A5H         ; ADD (C) TO HI BYTE OF RESULT.
                TAY             ; (Y) = HI BYTE OF RESULT*2 + (C).
                JSR DOUBLE      ; (A) = RESULT * 8.
                JSR DOUBLE
                TXA
                ADC A5L
                STA A5L         ; (RESULT*2+NEW DIGIT)+(RESULT*8).
                TYA
                ADC A5H         ; ADD (C) TO UPDATE HI BYTE.
                STA A5H
                BCC ASC2DEC     ; BRANCH IF # <65536.

                                ; ERROR - INVALID ASCII NUMBER.

NOTASCII        SEC             ; EXIT WITH (C)=1 TO SIGNAL ERROR.
                RTS             ; RETURN TO CALLER OF CNVRTASC.

                                ; MULTIPLY 2-BYTE RESULT TIMES 2.

DOUBLE          ASL A5L         ; "ROLL" HI & LOW BYTES AS A UNIT.
                ROL A5H         ; (PICK UP (C) IN HI BYTE.)
                ifelse(eval(VERSION < 320),1,`
                BCS NOTASCII
                ')
                RTS


                                ; ---------------------------------
                                ; CONVERT ASCII CHARS TO HEX.
                                ; ---------------------------------

ASC2HEX         PLP             ; THROW SAVED STATUS OFF STACK.
GETASCII        JSR PURGECMD    ; GET 1ST & SUBSEQUENT CHARS THAT
                                ; OCCUR AFTER THE HEX ("$") SYMBOL
                BEQ CKIFDONE    ; GO EXIT IF <CR> OR COMMA.

                                ; CHECK VALIDITY OF ASCII CHARS
                                ; FOR CONVERSION TO HEX.

                SEC
                SBC #$B0        ; SUBTRACT ASCII "0".
                BMI NOTASCII    ; ERROR BECAUSE < 0.
                CMP #$0A
                BCC PRP2DUBL    ; VALID: 0 <--> 9.
                SBC #7          ; CHK HI RANGE OF HEX #S.
                BMI NOTASCII    ; ERROR BECAUSE > $09 AND < $0A.
                CMP #$10
                BCS NOTASCII    ; ERROR BECAUSE > $0F.

                                ; MOVE RESULT IN A5L/H UP A NIBBLE
                                ; BY ROLLING IT AS A UNIT (IE. *16).

PRP2DUBL
                ifelse(eval(VERSION < 320),1,`
                JSR DOUBLE      ; MULTIPLY RESULT * 2.
                JSR DOUBLE      ; MULTIPLY RESULT * 2.
                JSR DOUBLE      ; MULTIPLY RESULT * 2.
                JSR DOUBLE      ; MULTIPLY RESULT * 2.
                ',`
                LDX #4          ; (X) = # OF TIMES TO DOUBLE UNIT.
TIMES2          JSR DOUBLE      ; MULTIPLY RESULT * 2.
                DEX
                BNE TIMES2      ; MORE MULTIPLICATION TO DO.
                ')

                                ; MERGE HEX REPRESENTATION OF DIGIT
                                ; INTO LOW NIBBLE POSN OF RESULT.
                                ;
                                ; NOTE BUG:  NO CHK IS MADE TO TRAP
                                ; NUMBERS > $FFFF.  IF TOO MANY #S
                                ; ARE INPUT, ONLY THE LAST 4 DIGITS
                                ; ARE REFLECTED IN THE RESULT.

                ORA A5L
                STA A5L
                JMP GETASCII    ; GO GET NEXT CHAR TO CONVERT.


                                ; ==================================
                                ; PR# COMMAND HANDLER.
                                ; ==================================

                                ; ON ENTRY, A5L/H CONTAINS THE HEX
                                ; VALUE OF THE ARGUMENT (SLOT #)
                                ; THAT WAS ISSUED WITH THE CMD.
                                ; THE ARGUMENT WAS PREVIOUSLY
                                ; SCREENED BY THE INPRMAX ($A0AA)
                                ; ROUTINE.  IF SLOT# = 0, THEN THE
                                ; OUTPUT HOOK (CSW) POINTS TO COUT1
                                ; ($FDF0).  OTHERWISE, CSW POINTS
                                ; TO $CS00 (WHERE S = SLOT #).

CMDPR           LDA A5L         ; GET SLOT NUMBER.
                JMP OUTPORT     ; USE MONITOR ROM TO SET OUTPUT HK.
                                ; RTN TO THE CALLER OF THE PR# CMD.
                                ; (OFTEN RETURNS 2 AFTRCMD ($A17D)
                                ; ASSOCIATED WITH THE COMMAND
                                ; PARSING & PROCESSING ROUTINES.)


                                ; =================================
                                ; THE IN# COMMAND HANDLER.
                                ; =================================

                                ; ON ENTRY, A5L/H CONTAINS THE HEX
                                ; VALUE OF THE ARGUMENT (SLOT #)
                                ; THAT WAS ISSUED WITH THE CMD.
                                ; THE ARGUMENT WAS PREVIOUSLY
                                ; SCREENED BY THE INPRMAX ($A0AA)
                                ; ROUTINE.  IF SLOT# = 0, THEN THE
                                ; INPUT HOOK (KSW) POINTS TO KEYIN
                                ; ($FD1B).  OTHERWISE, KSW POINTS
                                ; TO $CS00 (WHERE S = SLOT #).

CMDIN           LDA A5L         ; GET SLOT NUMBER.
                JMP INPORT      ; USE MONITOR ROM TO SET INPUT HK.
                                ; RTN TO THE CALLER OF THE IN# CMD.
                                ; (OFTEN RETURNS TO AFTRCMD ($A17D)
                                ; ASSOCIATED WITH THE COMMAND
                                ; PARSING & PROCESSING ROUTINES.)



                                ; ======================================
                                ; THE MON AND NOMON COMMAND HANDLERS.
                                ; ======================================

                                ; NOTE:  THE MON & NOMON COMMANDS AND
                                ; THEIR ALPHABETIC ARGUMENTS (C,I,O)
                                ; ARE FIRST DETECTED VIA COMMAND
                                ; PARSING.  CIOCUMUL IS TESTED AT THE
                                ; DSPLYCHR ($9F9F) PORTION OF THE VIDEO
                                ; PARSING ROUTINE TO SEE IF A CHAR
                                ; SHOULD BE SENT TO THE SCREEN OR
                                ; NOT.  IT IS EASY TO BECOME CONFUSED
                                ; OVER THE DISTINCTION BETWEEN
                                ; CIOCUMUL AND MONPRSD.  THE FORMER
                                ; REPRESENTS THE CUMMULATIVE UPDATED
                                ; RECORD OF THE C, I, O ARGUMENTS
                                ; WHEREAS THE LATER DESCRIBES THE
                                ; MOST RECENT ADDITIONS OF THE
                                ; C, I, O ARGUMENTS PRESENT IN THE
                                ; TABLE OF PARSED VALUES.
                                ; THE ALPHABETIC ARUMENTS (C, I, O)
                                ; ARE REPRESENTED BY THE FOLLOWING
                                ; SPECIFIC BITS:
                                ; C = $40 = %01000000
                                ; I = $20 = %00100000
                                ; O = $10 = %00010000
                                ; COMBINATION OF ARGUMENTS ARE SIMPLY
                                ; DESCRIBED BY THE APPROPRIATE BIT
                                ; SETTINGS: EX. CIO = $70 = $01110000

CMDMON          LDA CIOCUMUL    ; GET PREV CUMMULATIVE RECORD AND
                ORA MONPRSD     ; MERGE WITH THE LATEST PARSED VAL
                STA CIOCUMUL    ; TO UPDATE CUMMULATIVE RECORD.
                RTS             ; RTN TO THE CALLER OF THE MON CMD.
                                ; (OFTEN RETURNS TO AFTRCMD ($A17D)
                                ; ASSOCIATED WITH THE COMMAND
                                ; PARSING & PROCESSING ROUTINES.)

CMDNOMON        BIT MONPRSD     ; TEST BIT6 IN PARSED TBL TO SEE IF
                                ; "NOMON C" IS SELECTED.
                BVC CLRMON      ; BRANCH IF "C" NOT INCLUDED.
                JSR CRVIADOS    ; "C" WAS INCLUDED, SO PRT <CR>
                                ; BECAUSE CMD (BUT NOT <CR>) WAS
                                ; ALREADY PRINTED.

CLRMON          LDA #%01110000  ; SHUT OFF BITS IN PARSED TABLE
                EOR MONPRSD     ; THAT CORRESPOND TO THE ALPHABETIC
                                ; ARGUMENTS ISSUED WITH NOMON CMD.
                AND CIOCUMUL    ; NOW MAKE SURE THAT THOSE BITS
                STA CIOCUMUL    ; ARE OFF IN CUMMULATIVE RECORD.
                RTS             ; RTN TO CALLER OF THE NOMON CMD.
                                ; (OFTEN RETURNS TO AFTRCMD ($A17D)
                                ; ASSOCIATED WITH THE COMMAND
                                ; PARSING & PROCESSING ROUTINES.)


                                ; ==================================
                                ; THE MAXFILES COMMAND HANDLER.
                                ; ==================================

                                ; THE MAXFILES COMMAND IS USED TO
                                ; DEFINE THE # OF FILES THAT MAY
                                ; BE OPENED AT ONE TIME (IE. 1-16).
                                ; A DEFAULT VALUE OF 3 IS USED FOR
                                ; COLDSTARTS (IE, WHEN THE DISK IS
                                ; BOOTED) OR WHEN THE FP OR INT
                                ; COMMANDS ARE ISSUED.  THIS VALUE
                                ; CAN BE CHANGED BY ALTERING THE
                                ; CONTENTS OF $AAB1. (THE MAXFILES
                                ; CMD CAN BE TRICKED INTO BUILDING
                                ; ITS BUFFERS AT A LOWER LOCATION
                                ; IN ORDER TO CREATE A SANCTUARY
                                ; WHERE CUSTOM MACHINE LANGUAGE
                                ; ROUTINES CANT BE OVERWRITTEN BY
                                ; BASIC.  SEE FORMATTED DISMBLY
                                ; OF MAXFILES CMD FOR DETAILS.)

CMDMXFIL        LDA #0          ; SHUT OFF THE EXEC FLG.
                STA EXECFLAG
                LDA A5L         ; GET ARGUMENT ISSUED WITH CMD AND
                PHA             ; SAVE IT ON THE STK.
                                ; NOTE:  ARGUMENT WAS PREVIOUSLY
                                ; SCREENED TO INSURE THAT IS IS
                                ; BTWN 1 AND 16 ($A0AA - $A0C7).
                JSR CLOSEALL    ; CLOSE ALL OPEN FILES.
                PLA             ; RETRIEVE ARGUEMENT ISSUED WITH
                STA MXFILVAL    ; CMD & STORE IT IN THE MAIN
                                ; VARIABLES TABLE.
                JMP BILDBUFS    ; GO BUILD MXFILVAL # OF DOS BUFS.
                                ; RTNS TO CALLER OF MAXFILES CMD.
                                ; (OFTEN RETURNS TO AFTRCMD ($A17D)
                                ; ASSOCIATED WITH THE COMMAND
                                ; PARSING & PROCESSING ROUTINES.)


                                ; =================================
                                ; THE DELETE COMMAND HANDLER.
                                ; =================================

CMDELETE        LDA #5          ; OPCODE FOR DELETE.
                JSR HNDLCMD1    ; CLOSE THE FILE & RLEASE ITS BUF.
                                ; REASSIGN A DOS BUF TO THE FILE.
                                ; CHNG FILE DESCRP IN DIR SEC BUF.
                                ; WRITE UPDATED DIR SEC BUF TO DSK.
                                ; FREE UP DATA & T/S LIST SECTORS.
                                ; WRITE UPDATED VTOC TO DISK.
                JSR GETBUFF     ; FIND REASSIGNED DOS BUF.
                LDY #0          ; FREE UP DOS BUF OF FILE BY
                TYA             ; STORING A $00 IN 1ST BYTE OF
                STA (A3L),Y     ; THE DOS FILE NAME BUFFER.
                RTS             ; EXIT TO CALLER OF THE DELETE CMD.
                                ; (OFTEN RETURNS TO AFTRCMD ($A17D)
                                ; ASSOCIATED WITH THE COMMAND
                                ; PARSING & PROCESSING ROUTINES.)


                                ; ==================================
                                ; THE LOCK AND UNLOCK CMD HANDLERS.
                                ; ==================================

CMDLOCK         LDA #7          ; LOCK OPCODE.
                BNE LOKUNLOK    ; ALWAYS.
CMDUNLOK        LDA #8          ; UNLOCK OPCODE.


                                ; ---------------------------------
                                ; ROUTINE COMMON TO LOCK, UNLOCK
                                ; AND VERIFY COMMAND HANDLERS.
                                ; ---------------------------------

LOKUNLOK        JSR HNDLCMD1    ; CALL PART OF THE MAIN COMMAND
                                ; HANDLER ROUTINE TO LOCK, UNLOCK
                                ; OR VERIFY THE FILE.
                JMP CMDCLOSE    ; EXIT COMMAND VIA CLOSE.
                                ; RTN TO THE CALLER OF THE COMMAND.
                                ; (OFTEN RETURNS TO AFTRCMD ($A17D)
                                ; ASSOCIATED WITH THE COMMAND
                                ; PARSING & PROCESSING ROUTINES.)


                                ; =================================
                                ; THE VERIFY COMMAND HANDLER.
                                ; =================================

CMDVERFY
                ifelse(eval(VERSION < 320),1,`
                JMP VERFY
                ASM_RES(1)
                ',`
                LDA #12         ; VERIFY OPCODE.
                BNE LOKUNLOK    ; GO CALL THE CMD HNDLR TO VERIFY
                                ; FILE & THEN EXIT VIA CLOSE CMD.
                ')


                                ; =================================
                                ; THE RENAME COMMAND HANDLER.
                                ; =================================

CMDRENAM        LDA ADRSFNBF    ; COPY ADR OF SECONDARY FILENAME TO
                STA RENAMBUF    ; RENAME BUF IN FM PARAMETER LIST.
                LDA ADRSFNBF+1
                STA RENAMBUF+1
                LDA #9          ; RENAME OPCODE.
                STA TEMPBYT
                JSR CLOSIFOP    ; CLOSE FILE IF IT IS ALREADY OPEN
                                ; AND THEN DO THE RENAME FUNCTION:
                                ; -COPY NEW FILE NAME TO DIRECTORY
                                ; SECTOR BUFFER & THEN WRITE THE
                                ; UPDATED DIREC SEC BACK TO DISK.
                JMP CMDCLOSE    ; EXIT RENAME CMD VIA CLOSE CMD.
                                ; (OFTEN RETURNS TO AFTRCMD ($A17D)
                                ; ASSOCIATED WITH THE COMMAND
                                ; PARSING & PROCESSING ROUTINES.)


                                ; =================================
                                ; THE APPEND COMMAND HANDLER.
                                ; =================================

CMDAPPND        JSR CMDOPEN     ; GO OPEN THE FILE TO BE APPENDED.
                ifelse(eval(VERSION < 320),1,`
                LDA   #6
                CMP   RTNCODFM
                BNE   READ2END
                RTS
                ')

READ2END        JSR RDTXTBYT    ; GO READ A TEXT FILE BYTE.  (USE
                                ; THE READ FUNCTION AND READ-ONE-
                                ; BYTE SUBFUNCTION.)
                BNE READ2END    ; TAKE BRANCH IF DEALING WITH A
                                ; VALID (IE. NON-ZERO) DATA BYTE.
                                ; HAVENT ENCOUNTERED AN END-OF-
                                ; FILE MARKER ($00) YET,SO GO BACK
                                ; TO READ THE REST OF THE FILE.
                ifelse(eval(VERSION < 330),1,`
                JMP BK2APND
                ',`
                JMP CKAPFLG     ; FOUND END OF FILE,SO NOW GO BACK
                                ; UP THE FILE POINTER IF NECESSARY
                                ; AND EVENTUALLY EXIT THE APPEND
                                ; CMD HANDLER VIA RSETPTRS ($B6B3)
                                ; AND FMEXIT ($B386).  NOTE THAT
                                ; RSETPTRS RESETS THE SAVED STACK
                                ; POINTER (STKSAV, $B39B) SO WE
                                ; EVENTUALLY RETURN TO THE CALLER
                                ; OF THE APPEND COMMAND. EXECUTION
                                ; OFTEN RETURNS TO AFTRCMD ($A17D)
                                ; LOCATED IN THE COMMAND PARSING
                                ; AND PROCESSING ROUTINES.
                ')


                                ; =================================
                                ; THE OPEN COMMAND HANDLER.
                                ; =================================

CMDOPEN
                ifelse(eval(VERSION >= 320),1,`
                LDA #0          ; CODE FOR TEXT FILE.
                JMP OPNCKTYP    ; GO OPEN THE FILE & CHK ITS TYPE.
                                ; RTN TO THE CALLER OF THE OPEN CMD
                                ; (OFTEN RETURNS TO AFTRCMD ($A17D)
                                ; ASSOCIATED WITH THE COMMAND
                                ; PARSING & PROCESSING ROUTINES.)
                ')


                                ; ====================================
                                ; COMMON FILE MANAGER CMD HNDLR CODE.
                                ; ====================================

HNDLCMD         LDA #1          ; OPEN OPCODE.
HNDLCMD1        STA TEMPBYT     ; STORE OPCODE IN TEMPBYT.
                LDA LENPRSD     ; GET L-PARAMETER FROM PARSED TBL.
                BNE SAVLENFM    ; CHECK IF A NON-ZERO L-PARM WAS
                                ; ISSUED WITH THE COMMAND.
                LDA LENPRSD+1
                BNE SAVLENFM
                LDA #1          ; LNGTH WAS 0 SO MAKE IT 1 INSTEAD
                STA LENPRSD
SAVLENFM        LDA LENPRSD     ; PUT LENGTH IN FM PARAMETER LIST.
                STA RECLENFM    ; NOTE:  RECORD LENGTH = 1 FOR
                LDA LENPRSD+1   ; SEQUENTIAL FILES, ELSE PARSED
                STA RECLENFM+1  ; LENGTH FOR RANDOM ACCESS FILES.


                                ; ----------------------------------
                                ; CLOSE FILE IF IT IS ALREADY OPEN.
                                ; ----------------------------------

CLOSIFOP        JSR CMDCLOSE    ; CLOSE IF ALREADY OPEN.
                LDA A5H         ; A5L/H POINTS AT HIGHEST NUMBERED
                                ; (LOWEST IN MEMORY) FREE DOS BUF.
                BNE SAVFNPTR    ; BRANCH IF FOUND A FREE BUFFER.
                JMP NOBUFERR    ; COULDNT LOCATE A FREE BUFFER SO
                                ; GO ISSUE OUT OF BUFS MSG.
SAVFNPTR        STA A3H         ; RESET A3L/H TO POINT AT DOS BUF
                LDA A5L         ; THAT WILL USE FOR FILENAME FIELD.
                STA A3L
                JSR CPYPFN      ; REASSIGN A DOS BUFFER TO THE FILE
                                ; WE WANT TO OPEN.
                JSR BUFS2PRM    ; GET ADDRS OF THE VARIOUS DOS
                                ; BUFFERS FROM THE CHAIN BUFFER &
                                ; PUT THEM IN THE FM PARM LIST.
                JSR CPY2PARM    ; PUT VOL, DRV, SLOT & ADDR OF THE
                                ; PRIMARY FILENAME BUFFER IN THE
                                ; FM PARAMETER LIST.
                LDA TEMPBYT     ; GET OPCODE BACK FROM TEMPBYT AND
                STA OPCODEFM    ; STICK IT IN THE FM PARM LIST.
                JMP FMDRIVER    ; USE FILE MANAGER TO DO FUNCTION.


                                ; =================================
                                ; THE CLOSE COMMAND HANDLER.
                                ; =================================

CMDCLOSE        LDA PRIMFNBF    ; GET 1ST CHR FRM PRMRY NAME BUF.
                CMP #HICHAR(` ')        ; DONT ALLOW LEADING SPACES.
                BEQ CLOSEALL    ; LEADING SPC = SIGNAL TO CLOSE ALL
                                ; FILES.  (A CLOSE CMD WAS ISSUED
                                ; WITH NO ACCOMPANYING FILE NAME.)
                JSR GETBUFF     ; LOCATE A DOS BUFFER WITH SAME
                                ; NAME, ELSE LOCATE A FREE BUFFER.

EVENTXIT
                ifelse(eval(VERSION < 320),1,`
                BCS CLOSERTSX   ; EVENTUALLY EXIT VIA THIS ROUTE!!
                ',`
                BCS CLOSERTS    ; EVENTUALLY EXIT VIA THIS ROUTE!!
                ')

                JSR CLOSEONE    ; MATCHING FILENAME WAS FOUND SO
                                ; GO CLOSE THAT FILE.
                JMP CMDCLOSE    ; GO BACK TO POINT A5L/H AT A FREE
                                ; DOS BUFFER & EXIT VIA EVENTXIT
                                ; ($A2F4) AND CLOSERTS ($A330).
                ifelse(eval(VERSION < 320),1,`
CLOSERTSX       RTS
                ')

                                ; ---------------------------------
                                ; CLOSE A SPECIFIC FILE
                                ; (& FREE ITS BUFFER).
                                ; ---------------------------------
CLOSEONE        JSR CKEXCBUF    ; CHK IF CURRENT FILENAME BUFFER
                                ; BELONGS TO AN EXEC FILE.
                BNE FREEBUFF    ; BRANCH IF NOT EXECING FROM THIS
                                ; PARTICULAR FILE.  NOTE, ALWAYS
                                ; TAKE BRANCH IF CLOSEONE ($A2FC)
                                ; IS ACCESSED VIA CLOSEALL ($A316)
                LDA #0          ; CLOSING AN EXEC FILE SO SHUT OFF
                STA EXECFLAG    ; THE EXEC FLAG. NOTE:THIS INSTRUC
                                ; IS NEVER CARRIED OUT IF ACCESSED
                                ; VIA CLOSEALL.  (AN ACTIVE EXEC
                                ; FILE WAS ALREADY DETECTED AND
                                ; SKIPPED BY THE "BEQ CHKNXBUF"
                                ; INSTRUCTION AT $A323.)
FREEBUFF        LDY #0          ; FREE UP DOS BUF BY POKING A $00
                TYA             ; IN 1ST BYT OF DOS FILENAME BUF.
                STA (A3L),Y
                JSR BUFS2PRM    ; GET ADDRS OF THE VARIOUS DOS
                                ; BUFS FROM THE CHAIN BUF & PUT
                                ; THEM IN THE FM PARAMETER LIST.
                LDA #2          ; PUT OPCODE FOR CLOSE FUNCTION
                STA OPCODEFM    ; IN THE FM PARAMETER LIST.
                JMP FMDRIVER    ; GO TO THE FILE MANAGER DRIVER TO
                                ; DO THE CLOSE FUNCTION.


                                ; ---------------------------------------
                                ; CLOSE ALL FILES (EXCEPT AN EXEC FILE).
                                ; ENTER CLOSEALL WHEN IT IS ACCESSED VIA
                                ; A DIRECT CALL OR IF THE FIRST CHAR IN
                                ; THE PRIMAY FILENAME FIELD WAS A SPACE.
                                ; ---------------------------------------

CLOSEALL        JSR GETFNBF1    ; PUT ADR OF 1ST DOS FILENAME BUF
                                ; (LOCATED IN CHAIN OF DOS BUFS)
                                ; IN THE A3L/H POINTER.
                BNE CKIFEXEC    ; ALWAYS.
CHKNXBUF        JSR GETNXBUF    ; GET ADR OF NEXT DOS FILENAME BUF
                                ; FROM DOS CHAIN POINTERS BUFFER
                                ; (OFFSET 37 & 36 BYTES FROM 1ST
                                ; CHR OF PRESENT DOS FILENAME BUF)
                BEQ CLOSERTS    ; LNK ZEROED OUT -ALL FILES CLOSED
                                ; (EXIT CLOSEALL VIA THIS ROUTE.)
CKIFEXEC        JSR CKEXCBUF    ; CHK IF CURRENT DOS FILENAME BUF
                                ; BELONGS TO TO AN EXEC FILE.
                BEQ CHKNXBUF    ; EXEC ACTIVE SO DONT CLOSE ITS
                                ; BUFFER OUT OR WILL END UP
                                ; IN NEVER-NEVER LAND.  AFTER ALL,
                                ; DONT WANT TO CLOSE BUFFER IF WE
                                ; ARE USING IT TO EXEC (IE. WOULD
                                ; BE LIKE BURYING SELVES ALIVE)!!
                JSR GETFNBY1    ; GET 1ST BYTE IN DOS NAME BUF.
                BEQ CHKNXBUF    ; THIS FILE IS ALREADY CLOSED SO
                                ; GO BACK TO CLOSE REST OF FILES.
                JSR CLOSEONE    ; FILE WAS OPEN SO GO CLOSE IT.
                JMP CLOSEALL    ; GO TO CLOSERTS VIA CLOSEALL!!!
CLOSERTS        RTS             ; EXIT TO CALLER OF THE CLOSE CMD.
                                ; (OFTEN EXITS TO AFTRCMD ($A17D)
                                ; LOCATED IN THE COMMAND PARSING &
                                ; PROCESSING ROUTINES.)


                                ; =================================
                                ; THE BSAVE COMMAND HANDLER.
                                ; =================================

CMDBSAVE        LDA #%00001001  ; TEST BITS0 & 3 OF CUMLOPTN TO SEE
                AND CUMLOPTN    ; IF A(DDR) & L(ENGTH) PARAMETERS
                CMP #%00001001  ; WERE ISSUED WITH THE BSAVE CMD.
                BEQ DOBSAV      ; BOTH A- & L-PARMS PRESENT.
                JMP CKIFCTRL    ; GOT A SYNTAX ERROR.

DOBSAV          LDA #4          ; CODE FOR BINARY FILE.
                JSR OPNCKTYP    ; CLOSE (IF NECESSARY) & THEN OPN.
                LDA ADRPRSD+1   ; PREPARE TO WRITE ADDR TO DISK.
                LDY ADRPRSD
                JSR WRADRLEN    ; CALL WRITE-ONE-BYTE SUBFUNCTION
                                ; TWICE TO PUT A(DDR)-PARAMETER IN
                                ; DATA SEC BUF. (NOTE: LEN2RDWR IS
                                ; USED AS A TEMPORAY BUFFER FOR
                                ; FOR DATA TRANSFER.)

                LDA LENPRSD+1   ; PREPARE TO WRITE FILE LENGTH.
                LDY LENPRSD
                JSR WRADRLEN    ; CALL WRITE-ONE-BYTE SUBROUTINE TO
                                ; WRITE LENGTH AS THE 3RD & 4TH
                                ; BYTES IN THE DATA SEC BUF.
                                ; (LATER BUF WRITTEN AS 1ST SEC
                                ; OF FILE.  LEN2RDWR IS AGAIN USED
                                ; AS A TEMP BUF FOR DATA TRANSFER.)

                                ; NOW PREPARE TO WRITE THE REST
                                ; OF THE BINARY FILE TO THE DISK.

                LDA ADRPRSD+1   ; PUT ADDR OF SOURCE BUFFER IN
                LDY ADRPRSD     ; THE FM PARAMETER LIST.
                JMP RDWRANGE    ; GO TO WRITE-RANGE ROUTINE TO
                                ; WRITE REST OF FILE TO THE DISK.
                                ; (FILE IS ALSO VERIFIED AND THEN
                                ; EXITED VIA THE CLOSE COMMAND.)
                                ; EXECUTION EVENTUALLY RETURNS TO
                                ; THE CALLER OF THE BSAVE COMMAND.
                                ; OFTEN, RETURNS TO AFTRCMD ($A17D)
                                ; LOCATED IN THE CMD PARSING AND
                                ; PROCESSING ROUTINES.


                                ; =================================
                                ; BLOAD COMMAND HANDLER
                                ; =================================

CMDBLOAD        JSR HNDLCMD     ; CALL THE FM COMMAND HANDLER TO
                                ; OPEN THE FILE.
                ifelse(eval(VERSION < 320),1,`
                LDA   #6
                CMP   RTNCODFM
                BNE   LCK4BLOD
                JMP   TONOTFND
                ')
                                ; COMPARE FILE TYPE WANTED
                                ; WITH FILE TYPE FOUND.

LCK4BLOD        LDA #$7F        ; STRIP LOCK BIT FROM FILE TYPE
                AND FILTYPFM    ; FOUND (VIA OPEN FUNCTION).
                CMP #4          ; WAS FILE FOUND A BINARY FILE?
                BEQ ADR4BLOD    ; YES.
                ifelse(eval(VERSION < 320),1,`
                JMP NOTBINARY
                ',`
                JMP TYPMISM     ; NO - GO ISSUE FILE-MISMATCH MSG.
                ')

                                ; REDUNDANT CODE!  CLOSE (IF NECESSARY)
                                ; AND THEN OPEN THE FILE AGAIN.

ADR4BLOD        LDA #4          ; CODE FOR BINARY FILE.
                JSR OPNCKTYP    ; CLOSE & REOPEN FILE.
                JSR RDADRLEN    ; READ THE BLOAD ADR FROM THE DISK
                                ; INTO LEN2RDWR.
                TAX             ; X=LOW BYTE OF BLOAD ADR FRM DSK
                LDA CUMLOPTN    ; CHK CUMLOPTN TO SEE IF AN A(DDR)
                AND #%00000001  ; WAS ISSUED WITH THE BLOAD CMD.
                BNE LEN4BLOD    ; YES -SO IGNORE ADR READ FROM DSK
                                ; & USE THE ACTUAL PARSED
                                ; A-PARAMETER INSTEAD.
                STX ADRPRSD     ; STORE ADR READ FRM DSK IN PARSED
                STY ADRPRSD+1   ; TABLE.  (THIS WAY CAN ALWAYS USE
                                ; VAL IN TABLE FOR BLOAD ADR.)

LEN4BLOD        JSR RDADRLEN    ; READ THE BLOAD LENGTH OFF DSK.
                                ; (PUT RESULTS IN LEN2RDWR.)
                LDX ADRPRSD     ; SET (X)/(Y) = EITHER ORIG PARSED
                LDY ADRPRSD+1   ; A-PARM ADR OR BLOAD ADR FRM DSK.
                ifelse(eval(VERSION < 320),1,`
                JMP READREST
                ',`
                JMP LODINTFP    ; GO READ THE REST OF THE FILE IN.
                                ; EXITS VIA THE CLOSE COMMAND.
                                ; RETURNS TO CALLER OF THE BLOAD
                                ; CMD. (IF BLOAD CMD NOT CALLED BY
                                ; BRUN, THEN OFTEN RTNS TO AFTRCMD
                                ; ($A17D) LOCATED IN THE COMMAND
                                ; PARSING & PROCESSING ROUTINES.)
                ')


                                ; =================================
                                ; BRUN COMMAND HANDLER
                                ; =================================

CMDBRUN         JSR CMDBLOAD    ; BLOAD THE PRGM.
                JSR INITIOHK    ; POINT THE I/O HOOKS AT DOS.
                                ; NOTE:  THIS CAN CREATE SOME
                                ; EXOTIC BUGS IF THE BRUNED PRGM
                                ; PRINTS ANY INFO OR IF "MON" IS
                                ; ACTIVE.  SEE FORMATTED DISMBLY
                                ; OF BRUN CMD FOR EXPLANATION.)
                JMP (ADRPRSD)   ; BEGIN EXECUTION OF BINARY PRGM.
                                ; EXECUTION NORMALLY RETURNS TO
                                ; AFTRCMD ($A17D) LOCATED IN THE
                                ; CMD PARSING AND PROCESSING
                                ; ROUTINES.  ALSO NOTE THAT
                                ; THE COMPUTER MAY HANG ON CERTAIN
                                ; OCCASSIONS.  (SEE THE FORMATTED
                                ; DISASSEMBLY OF THE BRUN CMD FOR
                                ; EXPLANATION.)


                                ; =================================
                                ; SAVE COMMAND HANDLER
                                ; =================================

CMDSAVE         LDA ACTBSFLG    ; CHK WHICH BASIC IS ACTIVE.
                BEQ SAVINTGR    ; BRANCH IF USING INTEGER.
                                ; INT=$00, A(ROM)=$40, A(RAM)=$80.
                ifelse(eval(VERSION >= 320),1,`
                LDA PROTFLG     ; IF PROTECTION FLAG IS ON (IE. IS
                                ; NEGATIVE), THEN ALL APPLESOFT
                                ; CMDS BECAUSE RUN & DOSS SAVE CMD
                                ; CAUSES A PHONY PROGRAM-TOO-LARGE
                                ; MESSAGE TO BE GENERATED.
                BPL SAVAPSFT    ; BRANCH IF PROTECTION FLG IS OFF.
                JMP TOOLARGE    ; PROTECTED!!! - SPIT OUT PHONY
                                ; PROGRAM-TOO-LARGE MESSAGE & XIT.
                ')


                                ; ---------------------------------
                                ; SAVE AN APPLESOFT FILE
                                ; ---------------------------------

SAVAPSFT        LDA #2          ; CODE FOR APPLESOFT FILE.
                JSR OPNCKTYP    ; GO OPEN THE NAMED FILE & CHECK
                                ; ITS TYPE.
                SEC             ; CALC THE LNGTH OF THE PRGM SO WE
                LDA PRGEND      ; CAN WRITE IT AS 1ST TWO BYTES:
                SBC TXTTAB      ; LENGTH = PRGEND - TXTTAB
                TAY
                LDA PRGEND+1
                SBC TXTTAB+1

                JSR WRADRLEN    ; WRITE THE LENGTH OF THE FP FILE
                                ; BY USING THE WRITE-ONE-BYTE
                                ; SUBFUNCTION TWICE.

                                ; PREPARE TO WRITE REST OF FILE.

                LDA TXTTAB+1    ; PRGM START = START OF OUTPUT BUF
                LDY TXTTAB
                JMP RDWRANGE    ; GO TO THE WRITE-RANGE-OF-BYTES
                                ; ROUTINE TO WRITE THE REST OF FILE
                                ; AND THEN VERIFY IT.
                                ; AFTER VERIFICATION, THE SAVE CMD
                                ; IS EXITED VIA THE CLOSE CMD.
                                ; EXECUTION THEN RETURNS TO CALLER
                                ; OF THE SAVE CMD.  (OFTEN RTNS TO
                                ; AFTRCMD ($A17D) LOCATED IN THE
                                ; DOS CMD PARSING AND PROCESSING
                                ; ROUTINES.)


                                ; ---------------------------------
                                ; SAVE AN INTEGER FILE
                                ; ---------------------------------

SAVINTGR        LDA #1          ; CODE FOR INTEGER FILE.
                JSR OPNCKTYP    ; GO OPEN THE INTEGER FILE.

                SEC             ; CALC LENGTH OF FILE.
                LDA HIMEM
                SBC INTPGMST
                TAY
                LDA HIMEM+1
                SBC INTPGMST+1

                JSR WRADRLEN    ; WRITE LNGTH TO DSK BY CALLING THE
                                ; WRITE-ONE-BYTE SUBFUNCTION TWICE
                LDA INTPGMST+1
                LDY INTPGMST
                JMP RDWRANGE    ; WRITE THE REST OF FILE TO DSK.


                                ; =================================
                                ; OPEN NAMED FILE & CHECK ITS TYPE
                                ; =================================

OPNCKTYP        STA FILTYPFM    ; PUT CODE FOR FILE TYPE IN THE
                PHA             ; FM PARAMETER LIST & SAVE ON STK.
                                ; ($00=TXT, $01=INT, $02=FP,
                                ; $04=BIN, $08=S-TYPE, $10=RELOC,
                                ; $20=A-TYPE AND $40=B-TYPE.)
                JSR HNDLCMD     ; USE THE FM CMD HANDLER TO OPEN.
                PLA             ; PULL THE FILE TYPE CODE FRM STK.
                JMP CHKFTYPE    ; GO CHK IF TYPE WNTD = TYPE FOUND


                                ; ============================================
                                ; WRITE TWO BYTES.
                                ; --------------------------------------------
                                ; CODE WHICH WRITES ADDRESS AND LENGTH VALUES
                                ; TO THE DATA SECTOR BUFFER.  (LATER, THE
                                ; DATA SEC BUF IS WRITTEN TO THE DISK.)
                                ; CALLS WRITE-ONE-BYTE SUBFUNCTION TWICE.
                                ; NOTE THAT LEN2RDWR IS USED AS A TEMPORARY
                                ; BUFFER FOR DATA TRANSFER AS SHOWN BELOW:
                                ; - LOW BYTE OF ADR OR LENGTH --> LEN2RDWR
                                ; --> ONEIOBUF --> DATA SECTOR BUFFER.
                                ; - HI BYTE OF ADR OR LENGTH --> LEN2RDWR+1
                                ; --> ONEIOBUF --> DATA SECTOR BUFFER.
                                ; ============================================

WRADRLEN        STY LEN2RDWR    ; PUT LOW BYTE IN FM PARM LIST IN
                                ; CASE THIS IS A L-PARM & WE NEED
                                ; IT AS A COUNTER WHEN LATER WRITE
                                ; RANGE OF BYTES TO DISK.
                STY ONEIOBUF    ; PUT BYTE TO WRITE IN PARM LIST.
                STA LEN2RDWR+1  ; PUT HI BYTE IN FM PARM LIST IN
                                ; CASE THIS IS A L-PARM & WE NEED
                                ; IT AS COUNTER WHEN LATER WRITE
                                ; RANGE OF BYTES TO DISK.
                LDA #4          ; PUT WRITE OPCODE IN FM PARM LIST
                STA OPCODEFM
                LDA #1          ; PUT ONE-BYTE SUBCODE IN PARM LST
                STA SUBCODFM
                JSR FMDRIVER    ; CALL FM DRV TO WRITE 1ST BYTE.
                LDA LEN2RDWR+1  ; PUT HI BYTE TO WRITE IN PARM LST
                STA ONEIOBUF
                JMP FMDRIVER    ; GO WRITE HI BYTE TO FILE.


                                ; =================================
                                ; READ/WRITE A RANGE OF BYTES
                                ; =================================

RDWRANGE        STY CURIOBUF    ; PUT ADR OF OUTPUT BUF IN PRM LST
                STA CURIOBUF+1
                LDA #2          ; SET SUBCODE FOR RANGE OF BYTES.
                ifelse(eval(VERSION < 330),1,`
                STA SUBCODFM
                ',`
                JMP VRFYRWNG    ; GO CALL THE FILE MANAGER TO WRITE
                                ; DATA TO THE DISK.  NEXT VERIFY
                                ; THE INFO & CLOSE THE FILE.
                ')


                                ; =================================
                                ; CALL FM DRIVER & THEN CLOSE FILE
                                ; =================================

CLOSEFM         JSR FMDRIVER    ; CALL THE FM DRIVER TO READ/WRITE.
                JMP CMDCLOSE    ; GO CLOSE THE FILE.


                                ; ========================================
                                ; GO ISSUE A FILE-TYPE-MISMATCH ERROR MSG
                                ; ========================================

TOTYPMIS        JMP TYPMISM     ; GO HANDLE MISMATCH ERROR.


                                ; =================================
                                ; LOAD COMMAND HANDLER
                                ; =================================

CMDLOAD         JSR CLOSEALL    ; CLOSE ALL FILES (EXCEPT ACTIVE
                                ; EXEC FILE).
OPENLOAD        JSR HNDLCMD     ; GO OPEN THE FILE.

                ifelse(eval(VERSION < 320),1,`
                LDA #6
                CMP RTNCODFM
                BNE OPENLOAD2
TONOTFND        JSR CMDELETE
                LDA #6
                JMP ERRHNDLR
OPENLOAD2       LDA #%01111111
                ',`
                LDA #%00100011  ; SET BITS IN (A) TO RESTRICT LOAD
                                ; CMD TO APLSFT ($02), INTGR ($01)
                                ; OR A-TYPE ($20) FILES.
                ')
                AND FILTYPFM    ; TYPE FOUND (VIA OPEN FUNCTION).
                BEQ TOTYPMIS    ; ERR -NOT ONE OF THE ABOVE TYPES.
                                ; GO ISSUE TYPE-MISMATCH ERR MSG.
                ifelse(eval(VERSION < 320),1,`
                AND #3
                BEQ TOTYPMIS
                ')

                STA FILTYPFM    ; SAVE TYPE WANTED IN FM PARM LIST

                LDA ACTBSFLG    ; CHK WHICH LANG IS ACTIVE:
                                ; INT=$00, FP=$40, A(RAM)=$80)
                BEQ LODINTGR    ; BRANCH IF USING INTEGER.

                LDA #2          ; CODE FOR APPLESOFT (FP).
                JSR SELCTBSC    ; CHK IF TYPE WANTED IS APPLESOFT.

                JSR RDADRLEN    ; READ LENGTH OF THE FP PRGM FROM
                                ; THE 1ST 2 BYTES OF THE FILE.

                                ; CHK TO SEE IF THERE IS ENOUGH ROOM
                                ; BETWEEN PRGM START POSN & MEMSIZ
                                ; TO ACCOMODATE FILE.

                CLC             ; ADD LNGTH OF FILE TO START OF PGM
                ADC TXTTAB      ; (NORMALLY, $801).
                TAX             ; SAVE LOW BYTE OF PRGEND IN (X).
                TYA             ; RETRIEVE HI BYTE OF LEN FROM (Y)
                ADC TXTTAB+1

                CMP MEMSIZ+1
                BCS TOTOOLRG    ; BRANCH IF NOT ENOUGH ROOM.
                                ; (GO ISSUE PRGM-TOO-LARGE MSG.)

                                ; PRGM IS SHORT ENOUGH TO BE
                                ; ACCOMODATED IN FREE MEMORY SPACE.
                                ;
                                ; SET ZERO-PAGE POINTERS.

                STA PRGEND+1    ; SET END OF PRGM POINTER.
                STA VARTAB+1    ; SET START OF VARIABLE SPACE.
                STX PRGEND      ; PRGEND: VAL IN TXTTAB + LENGTH.
                STX VARTAB      ; VARTAB: VAL IN TXTTAB + LENGTH.
                LDX TXTTAB
                LDY TXTTAB+1

                ifelse(eval(VERSION < 320),1,`
                JMP READREST
                ',`

                JSR LODINTFP    ; DESIGNATE WHERE IN FREE MEMORY TO
                                ; LOAD PRGM & THEN GO LOAD IT.
                JSR INITIOHK    ; POINT THE I/O HOOKS AT DOS.
                JMP (RLOCNTRY)  ; NORMALLY PTS TO SETLINKS ($D4F2)
                                ; ROUTINE IN BASIC WHICH SETS
                                ; IMPORTANT Z-PAGE POINTERS,
                                ; CLEARS OUT VARIABLES, RESETS STK
                                ; PTR & THEN ADJUSTS LINKS IN EACH
                                ; PROGRAM LINE.
                                ; EVENTUALLY, EXECUTION FLOWS INTO
                                ; BSCS WRMSTART (RESTART, $D43C).
                                ; IF THE LOAD CMD WAS CALLED VIA
                                ; THE RUN CMD, EXECUTION BRANCHES
                                ; BACK TO THE RUNFPINT ($A4DC) PART
                                ; OF THE RUN CMD (AFTER ADJUSTING
                                ; THE STACK).
                                ; IF LOAD WAS NOT CALLED FROM RUN,
                                ; THEN THE THE RESTART ($D43C)
                                ; PORTION OF BASIC EVENTUALLY
                                ; REQUESTS FURTHER PRGM OR KEYBRD
                                ; INPUT & ANOTHER CMD IS PARSED.
                ')


                                ; ---------------------------------
                                ; LOAD AN INTEGER PROGRAM
                                ; ---------------------------------

LODINTGR        LDA #1          ; CODE FOR INTEGER BASIC FILE TYPE.
                JSR SELCTBSC    ; CHK IF INTEGER BASIC IS ACTIVE.
                                ; IF NOT, SWITCH FROM FP TO INTEGER.
                JSR RDADRLEN    ; READ 1ST 2 BYTES OF FILE TO GET
                                ; LENGTH OF PRGM TO LOAD.
                SEC             ; CALC START OF PRGRM.
                LDA HIMEM       ; (HIMEM - LENADRBF.)
                SBC LENADRBF
                TAX
                LDA HIMEM+1
                SBC LENADRBF+1

                BCC TOTOOLRG    ; LENGTH > HIMEM SO ISSUE ERR MSG.
                TAY
                CPY LOMEM+1     ; CHK IF PRGM < = LOMEM.
                BCC TOTOOLRG    ; START OF PRGM TOO LOW, SO GO
                BEQ TOTOOLRG    ; ISSUE PRGM-TOO-LARGE ERROR MSG.

                STY INTPGMST+1  ; SET START-OF-PRGM POINTER.
                STX INTPGMST

                                ; =================================
                                ; GO DO THE ACTUAL LOAD.
                                ; (COMMON LOAD ROUTN FOR
                                ; FP OR INTEGER LOAD CMDS.)
                                ; =================================

LODINTFP
                ifelse(eval(VERSION < 320),1,`
                JMP READREST
                ',`
                STX CURIOBUF    ; DESIGNATE LOAD ADDR AS I/O BUF
                STY CURIOBUF+1  ; IN THE FM PARAMETER LIST.
                JMP CLOSEFM     ; USE FILE MANAGER TO LOAD PRGM.
                ')

                                ; =================================
                                ; COMMON CODE USED TO READ THE
                                ; BLOAD ADDRESS, BLOAD LENGTH
                                ; OR LOAD LENGTH FROM THE DISK.
                                ; =================================

RDADRLEN        LDA ADLENADR    ; GET ADR OF TWO-BYTE INPUT BUFFER
                STA CURIOBUF    ; (LENADRBF, $AA60) FRM RELOCATBL
                LDA ADLENADR+1  ; CONSTANTS TBL & DESIGNATE IT AS
                STA CURIOBUF+1  ; THE I/O BUF IN THE FM PARM LIST.
                LDA #0          ; PUT LENGTH TO READ = 2 BYTES IN
                STA LEN2RDWR+1  ; THE FM PARM LIST.
                LDA #2
                STA LEN2RDWR
                LDA #3          ; PUT READ OPCODE IN FM PARM LIST.
                STA OPCODEFM
                LDA #2          ; INDICATE WANT TO READ RNG OF BYTS
                STA SUBCODFM
                JSR FMDRIVER    ; GO READ IN THE ADDR (OR LENGTH).
                LDA LENADRBF+1  ; GET HI BYTE OF ADR (OR LENGTH)
                                ; JUST READ FROM DISK.
                STA LEN2RDWR+1  ; PUT VAL JUST RD IN PARM LIST IN
                                ; CASE JUST READ LENGTH (SO KNOW
                                ; HOW MUCH TO READ WHEN READ MAIN
                                ; BODY OF FILE).
                TAY             ; SAVE HI BYTE IN (Y).
                LDA LENADRBF    ; DO LIKEWISE WITH LOW BYTE.
                STA LEN2RDWR
                RTS

                ifelse(eval(VERSION < 320),1,`
READREST        STX   CURIOBUF
                STY   CURIOBUF+1
                JSR   FMDRIVER
                JMP   CMDCLOSE
                ')

                                ; =================================
                                ; CLOSE FILE & ISSUE A PROGRAM-
                                ; TOO-LARGE ERROR MESSAGE.
                                ; =================================

TOTOOLRG        JSR CMDCLOSE    ; CLOSE FILE.
                JMP TOOLARGE    ; ISSUE ERROR MSG.


                                ; =================================
                                ; SELECT DESIRED BASIC
                                ; =================================

                                ; CHK IF DESIRED BASIC IS UP OR NOT
                                ; (SWITCH BASIC IF NECESSARY).

SELCTBSC        CMP FILTYPFM    ; TYPE WANTED = TYPE FOUND?
                BEQ SELBSRTN    ; YES - BASIC WANTED IS ACTIVE.

                LDX NDX2CMD     ; SAVE INDEX TO PRESENT CMD IN CASE
                STX NEXTCMD     ; WE ARE USING INTEGER & MUST LOAD
                                ; INTEGER FILE CALLED "APPLESOFT"
                                ; IN ORDER TO LOAD A(RAM).
                LSR             ; SHIFT TYPE WANTED TO SEE WHICH
                                ; BASIC TO SWITCH INTO.
                BEQ SWTCH2FP    ; SWITCH FROM INTEGER TO APPLESOFT.
                JMP CMDINT      ; SWITCH FROM APPLESOFT TO INTEGER.


                                ; SWITCHING FROM INT TO APPLESOFT
                                ; SO COPY NAME OF FILE FROM PRIMARY
                                ; TO SECONDARY NAME BUF IN CASE WE
                                ; HAVE TO USE RAM-BASED APPLESOFT.

SWTCH2FP        LDX #29         ; 30 BYTES TO COPY (0 TO 29).
PRIM2SND        LDA PRIMFNBF,X  ; GET BYTE FROM PRIMARY.
                STA SCNDFNBF,X  ; COPY IT TO SECONDARY.
                DEX
                BPL PRIM2SND    ; BRANCH IF MORE BYTES TO COPY.

                JMP CMDFP       ; EXECUTE THE FP COMMAND.

SELBSRTN        RTS             ; DESIRED BASIC WAS ACTIVE.


                                ; =================================
                                ; RUN COMMAND HANDLER
                                ; =================================

CMDRUN
                ifelse(eval(VERSION >= 320),1,`
                LDA ACTBSFLG    ; CHK WHICH BASIC IS CURRENT.
                BEQ LOAD4RUN    ; BRANCH IF USING INTEGER BASIC.

                STA RUNTRUPT    ; SET THE RUN INTERCEPT FLAG TO
                                ; SIGNAL THAT WE ARE ABOUT TO
                                ; INTERRUPT THE RUN COMMAND TO DO
                                ; LOAD.  ($40=A(ROM), $80=A(RAM).)
                ')

LOAD4RUN        JSR CMDLOAD     ; GO LOAD THE PROGRAM.
                                ; ************* NOTE ************
                                ; * THE "JSR" IS ACTUALLY A
                                ; * PLACEBO BECAUSE AFTER FILE
                                ; * IS LOADED, EXECUTION GOES INTO
                                ; * BASIC & THEN RE-ENTERS DOS
                                ; * THRU THE DOS INTERCEPTS. ONCE
                                ; * DOS GETS ITS MITTS BACK INTO
                                ; * THINGS, THE RUNTRUPT FLAG IS
                                ; * TESTED & THEN EXECUTION FLOWS
                                ; * TO THE NEXT INSTRUCTION (AT
                                ; * RUNFPINT).  THE MACHINE DOES
                                ; * NOT GET LOST BECAUSE THE STK
                                ; * PTR GETS RESET ACCORDINDLY.
                                ; *******************************

RUNFPINT        JSR CRVIADOS    ; PRINT A <CR>.
                JSR INITIOHK    ; RESET THE I/O HKS TO PT TO DOS.
                JMP (RUNTRY)    ; GO EXECUTE THE PRGM.
                                ; (RUNTRY PTS TO RUNFPROM IF USING
                                ; A(ROM) OR POINTS TO FPRAMRUN
                                ; IF USING A(RAM).)


                                ; =======================================
                                ; INTEGER BASICS RUN CMD ENTRY POINT.
                                ; (RUNTRY PTS HERE WHEN INTEGER ACTIVE.)
                                ; =======================================

RUNINTGR        LDA LOMEM       ; CLEAR OUT ALL VARIABLES.
                STA INTVRLND    ; ZERO OUT INTEGER BASICS CURRENT
                LDA LOMEM+1     ; VARIABLE POINTER.
                STA INTVRLND+1
                JMP (CHAINTRY)  ; GO INTO INTEGER BASIC TO EXECUTE.


                                ; =================================
                                ; CHAIN COMMAND HANDLER
                                ; =================================

CMDCHAIN
                ifelse(eval(VERSION < 320),1,`
                JSR CMDLOAD
                ',`
                JSR OPENLOAD    ; LOAD THE INTEGER PRGM.
                ')
                JSR CRVIADOS    ; PRINT A <CR>.
                JSR INITIOHK    ; POINT I/O HOOKS AT DOS.
                JMP (CHAINTRY)  ; GO INTO INTEGER BASIC TO EXECUTE.


                                ; =================================
                                ; A(ROM)S RUN ENTRY POINT.
                                ; (RUNTRY POINTS HERE IF A(ROM)
                                ; BASIC IS ACTIVE.)
                                ; =================================

RUNFPROM        JSR SETZPTRS    ; CLEAR OUT ALL VARIABLES.
                ifelse(eval(VERSION >= 320),1,`
                STA PROMPT      ; ZERO OUT PROMPT & ON-ERROR FLAG.
                STA ERRFLG
                ')
                JMP NEWSTT      ; JUMP INTO BASIC TO EXECUTE PRGM.


                                ; =================================
                                ; A(RAM)S RUN ENTRY POINT.
                                ; (RUNTRY POINTS HERE IF A(RAM)
                                ; VERSION OF BASIC IS UP.)
                                ; =================================

FPRAMRUN        JSR CLRFPRAM    ; CLEAR ALL VARIABLES.
                ifelse(eval(VERSION >= 320),1,`
                STA PROMPT      ; ZERO OUT PROMPT & ON ERROR FLAG.
                STA ERRFLG
                ')
                JMP RUNFPRAM    ; GO RUN THE PROGRAM.

                                ; =================================
                                ; WRITE COMMAND HANDLER.
                                ; =================================

CMDWRITE        JSR COMRDWR     ; CALL COMMON READ/WRITE ROUTINE.
                                ; FIND NAMED BUF, ELSE A FREE BUF.
                                ; OPEN FILE IF NOT ALREADY OPEN.
                                ; POSN FILE PTR IF R- OR B-PARMS
                                ; WERE ISSUED.
                LDA #5          ; SET CONDITION 5.
                STA OPUTCOND

                JMP FINSHCMD    ; XIT WITH CONDITION 5 SET SO THAT
                                ; THE NEXT TIME A PRINT STATEMENT
                                ; IS ENCOUNTERED, EXECUTION WILL
                                ; FLOW VIA COUT & THE DOS HKS TO
                                ; SEND CHARS TO THE NAMED FILE.


                                ; =================================
                                ; READ COMMAND HANDLER.
                                ; =================================

CMDREAD         JSR COMRDWR     ; CALL COMMON READ/WRITE ROUTINE.
                                ; FIND NAMED BUF, ELSE FIND A FREE
                                ; BUFFER. OPEN FILE IF NOT ALREADY
                                ; OPEN.  POSN FILE PTR IF R- OR
                                ; B-PARMS WERE ISSUED WITH CMD.
                LDA #1          ; SET CONDNFLG TO SIGNAL READING.
                STA CONDNFLG
                JMP FINSHCMD    ; XIT WITH OPUTCOND=0 & CONDNFLG=1
                                ; EXECUTION EVENTUALLY FLOWS BACK
                                ; INTO APPLESOFT.  WHEN APPLESOFT
                                ; PICKS UP A SUBSEQUENT "INPUT" OR
                                ; "GET" STATEMENT, IT PRINTS A
                                ; PROMPT.  DOS INTERCEPTS OUTPUT
                                ; VIA OPUTINCP ($9EBD).  WHEN THE
                                ; SETTING OF CONDNFLG IS DETECTED,
                                ; THE MACHINE IS DIRECTED TO TAKE
                                ; DATA FROM THE DISK.


                                ; =================================
                                ; CODE COMMON TO READ/WRITE.
                                ; =================================

COMRDWR         JSR GETBUFF     ; LOCATE A DOS BUF WITH SAME NAME,
                                ; ELSE LOCATE A FREE BUF.
                BCC BUFS4RW     ; BRNCH IF MATCHING BUF WAS FOUND.
                JSR CMDOPEN     ; FILE NOT ALREADY OPN, SO OPEN IT
                JMP CKRBOPTN    ; GO CHK IF R- & B-PARMS ISSUED.

BUFS4RW         JSR BUFS2PRM    ; COPY ADDRS OF THE VARIOUS DOS
                                ; BUFFERS TO THE FM PARAMETER LIST

                                ; CHK IF R- OR B-PARAMETERS
                                ; WERE ISSUED WITH COMMAND.

CKRBOPTN        LDA CUMLOPTN    ; CHK IF R- OR B-PARMS ISSUED.
                AND #%00000110  ; (R=$04, B=$02.)
                BEQ RDWRRTN     ; NO - SKIP POSNING OF FILE PTR.

                                ; COPY B- & R-PARMS FROM OPTION
                                ; PARSED TABLE TO FM PARM LIST.

                LDX #3
CPYBPARM        LDA RECPRSD,X   ; GET VALUE OF PARAMETER.
                STA RECNMBFM,X  ; STORE IT IN PARM LIST.
                DEX             ; 4 BYTES TO COPY (3 TO 0).
                BPL CPYBPARM

                                ; CALL THE FILEMANAGER
                                ; WITH THE POSITION OPCODE.

BK2APND         LDA #$0A        ; OPCODE FOR POSITION.
                STA OPCODEFM    ; PUT IT IN THE FM PARAMETER LIST.
                JSR FMDRIVER    ; CALL FM TO DO THE POSN FUNCTION.
RDWRRTN         RTS


                                ; =================================
                                ; INIT COMMAND HANDLER.
                                ; =================================

CMDINIT         LDA #%01000000  ; CHK TO SEE IF V(OLUME) OPTION
                AND CUMLOPTN    ; WAS ISSUED WITH INIT COMMAND.
                ifelse(eval(VERSION < 320),1,`
                BEQ L24FE
                ',`
                BEQ VOL254      ; NO V-PARM ISSUED, SO USE A DFLT
                                ; VOLUME VALUE OF 254.
                ')
                LDA VOLPRSD     ; A VOL VAL WAS ISSUED, SO USE IT
                ifelse(eval(VERSION < 320),1,`
                BEQ L24FE
                ',`
                BNE OTHRVOL     ; (BUT ONLY IF IT IS NOT ZERO).
VOL254          LDA #$FE        ; USE VOL 254 AS DEFAULT VALUE.
                STA VOLPRSD
                ')
OTHRVOL         LDA ADOSTART+1  ; HI BYTE OF DOS LOAD ADDR FROM
                                ; DOSS MAIN VARIABLE TABLE.
                STA SUBCODFM
                LDA #11         ; OPCODE FOR INIT COMMAND.
                JSR HNDLCMD1    ; CALL FM COMMAND HANDLER TO DO
                                ; THE INIT COMMAND.
                JMP CMDSAVE     ; GO SAVE THE "HELLO" FILE & THEN
                                ; EXIT TO THE CALLER OF THE INIT
                                ; CMD.  (NORMALLY RTNS TO AFTRCMD
                                ; ($A17D) LOCATED IN THE COMMAND
                                ; PARSING & PROCESSING ROUTINES.)
                ifelse(eval(VERSION < 320),1,`
L24FE           JMP CKIFCTRL
                ')

                                ; =================================
                                ; CATALOG COMMAND HANDLER.
                                ; =================================

CMDCATLG        LDA #6          ; CATALOG OPCODE.
                JSR HNDLCMD1    ; CALL CMD HANDLER TO DO CATALOG.
                LDA VOLFM       ; GET VOLUME # FROM FM PARM LIST
                STA VOLPRSD     ; & PUT IT IN THE PARSED TABLE.
                RTS             ; EXIT TO CALLER OF CATALOG CMD.
                                ; (OFTEN RETURNS TO AFTRCMD ($A17D)
                                ; LOCATED IN THE CMD PARSING AND
                                ; PROCESSING ROUTINES.)


                                ; =================================
                                ; FP COMMAND HANDLER.
                                ; =================================

CMDFP           LDA #$4C        ; (A) = OPCODE FOR "JMP".
                JSR SETROM      ; TEST TO SEE IF LANGUAGE WANTED IS
                                ; ON CARD OR MOTHERBOARD.
                BEQ TODOSCLD    ; ROM VERSION OF FP WAS PRESENT ON
                                ; EITHER CARD OR MOTHERBOARD SO GO
                                ; DO A COLDSTART.

                                ; USING MACHINE WITH INTEGER IN ROM.
                                ;
                                ; ASSUME USING "SYSTEM MASTER" DISK
                                ; SO TRY TO RUN AN INTEGER PRGM
                                ; CALLED "APPLESOFT".  WHEN RUN, THE
                                ; PRGM CALLED "APPLESOFT" LOADS A
                                ; RAM OR DISK-BASED VERSION OF FP
                                ; BASIC THAT IS CONTAINED IN A BINARY
                                ; FILE CALLED "FPBASIC".  THIS LATTER
                                ; FILE IS ALSO HOUSED ON THE SYSTEM
                                ; MASTER DISK.

                LDA #0          ; SET ACTIVE BASIC FLAG TO DENOTE
                STA ACTBSFLG    ; USING INTEGER.
                LDY #30
                JSR BLNK1RST    ; BLANK OUT THE PRIMARY FILE NAME
                                ; BUFFER (30 BYTES LONG).

                                ; COPY THE NAME OF THE INTEGER FILE
                                ; CALLED "APPLESOFT" INTO THE PRIMARY
                                ; FILE NAME BUFFER.

                LDX #9          ; ONLY 9 CHARS IN NAME "APPLESOFT"
CPYAPPLE        LDA APLSFTXT-1,X ; GET CHARS OF NAME.
                STA PRIMFNBF-1,X ; STORE THEM IN PRIMARY NAME BUF
                DEX              ; REDUCE COUNTER.
                BNE CPYAPPLE    ; MORE CHARS TO COPY.

                LDA #$C0        ; SET CONDNFLG TO DESIGNATE USING
                STA CONDNFLG    ; RAM VERSION OF APPLESOFT.
                JMP CMDRUN      ; GO RUN FILE CALLED "APPLESOFT"
                                ; WHICH LOADS A RAM VERSION OF
                                ; FP BASIC CONTAINED IN A BINARY
                                ; FILE CALLED "FPBASIC".


                                ; =================================
                                ; INT COMMAND HANDLER.
                                ; =================================

CMDINT          LDA #$20        ; OPCODE FOR "JSR".
                JSR SETROM      ; TEST TO SEE IF LANGUAGE WANTED
                                ; IS ON CARD OR MOTHERBOARD.
                ifelse(eval(VERSION < 320),1,`
                BNE CMDFP
                ',`
                BEQ INTPRSNT    ; INTEGER BASIC IS PRESENT (EITHER
                                ; ON CARD OR MOTHERBOARD).

                                ; INTEGER BASIC NOT PRESENT
                                ; ON CARD OR MOTHERBOARD.

NOLNGINT        LDA #1          ; SET ERROR CODE FOR LANGUAGE-NOT-
                JMP ERRHNDLR    ; AVAILABLE MSG & GO EXIT.

                                ; INTEGER BASIC PRESENT ON DEVICE.

INTPRSNT        LDA #0          ; BECAUSE DESIRED BASIC IS PRESENT,
                STA RUNTRUPT    ; ZERO OUT THE RUN INTERCEPT FLAG
                                ; BECAUSE WE WONT BE LOADING A LANG
                ')
TODOSCLD        JMP DOSCOLD     ; GO INTO THE COLDSTART ROUTINE.


                                ; =================================
                                ; SELECT DESIRED BASIC
                                ; =================================

                                ; TEST CARD OR MOTHERBOARD TO INSURE
                                ; THAT DEVICE CONTAINING THE ROM
                                ; VERSION WE WANT IS SELECTED.
                                ; BASICCLD ($E000) CONTAINS A "JMP"
                                ; OR "JSR" INSTRUCTION IF DEALING
                                ; WITH FP OR INTEGER ROM RESPECTIVELY.

SETROM          CMP BASICCLD    ; TEST CARD OR MOTHERBOARD.
                                ; (IE.CHK WHICHEVER DEVICE IS UP.)
                BEQ DVICERTN    ; LANG WNTD ON PRESENT ROM DEVICE.

                                ; LANGUAGE WAS NOT ON DEVICE SELECTED
                                ; ABOVE, SO SPECIFICALLY TEST CARD
                                ; IN SLOT 0.  (P.S. COULD CHANGE ADDRS
                                ; IF WANT CARD IN DIFFERENT SLOT.)

                STA RRAMWXXXD2  ; READ ENABLE SLOT0.
                CMP BASICCLD    ; CHECK IDENTIFYING BYTE.
                BEQ DVICERTN    ; BRANCH IF ROM WANTED IS ON CARD.

                                ; ROM WANTED WAS NOT ON CARD.
                                ; WE MAY HAVE JUST TESTED CARD TWICE
                                ; SO NOW SPECIFICALLY TEST MOTHERBOARD.

                STA RROMWRAMD2  ; TEST MOTHERBOARD.
                CMP BASICCLD    ; CHECK IDENTIFYING BYTE.
DVICERTN        RTS             ; EXIT WITH THE SWITCHES POINTING
                                ; AT THE LAST DEVICE TESTED.IF THE
                                ; DESIRED LANGUAGE IS PRESENT, THE
                                ; SWITCHES ARE LEFT WITH THE
                                ; APPROPRIATE DEVICE SELECTED.


                                ; =================================
                                ; EXEC COMMAND HANDLER.
                                ; =================================

CMDEXEC         JSR CMDOPEN     ; GO OPEN THE FILE TO BE EXECED.
                LDA CURFNADR    ; GET ADDR OF CURRENT FILENAME BUF
                STA EXECBUFF    ; & DESIGNATE AS EXECS NAME BUF.
                LDA CURFNADR+1
                STA EXECBUFF+1
                LDA PRIMFNBF    ; SET EXEC FLAG TO A NON-ZERO VAL.
                STA EXECFLAG    ; (USE 1ST CHAR OF FILE NAME.)
                BNE POSNCHKR    ; ALWAYS - GO POSN FILE PTR IF
                                ; NECESSARY.
                                ; NOTE: ACTUAL EXECING OF STATMNTS
                                ; DOES NOT OCCUR UNTIL AFTER THE
                                ; COMPUTER RETURNS TO BASICS
                                ; RESTART ($D43C) ROUTINE.  WHEN
                                ; INPUT IS REQUESTED, EXECUTION
                                ; FLOWS VIA DOS HKS INTO OPUTINCP
                                ; ($9EBD).  HERE THE EXECFLAG IS
                                ; TESTED & DISCOVERED TO BE SET.
                                ; AS RESULT, THE READEXEC ($A682)
                                ; ROUTINE IS USED TO READ DATA FROM
                                ; THE EXEC FILE.  THE STATEMENTS
                                ; ARE INTERPRETED AS IF THEY WERE
                                ; ENCOUNTERED IN THE IMMED MODE.


                                ; =================================
                                ; POSITION COMMAND HANDLER
                                ; =================================

CMDPOSN         JSR GETBUFF     ; LOCATE BUF WITH SAME NAME, ELSE
                                ; LOCATE A FREE BUFFER.

                BCC BUFS4PSN    ; ALREADY OPEN -SKIP NEXT INSTRUC.
                JSR CMDOPEN     ; GO OPEN THE FILE.
                JMP POSNCHKR    ; BYPASS NEXT INSTRUC, BECAUSE JUST
                                ; OPENED FILE & PARM LIST ALREADY
                                ; CONTAINS ADRS OF D.IF DOS BUFS.

BUFS4PSN        JSR BUFS2PRM    ; GET ADR OF DOS BUFS FROM CHAIN
                                ; BUF & PUT THEM IN FM PARM LIST.

POSNCHKR        LDA CUMLOPTN    ; CHK TO SEE IF A NON-ZERO R-PARM
                AND #%00000100  ; WAS ISSUED WITH CMD.
                BEQ DONEPOSN    ; R-PARM WAS ZERO, SO GO EXIT
                                ; (IE. DONT MOVE FILE POINTER).

                                ; A NON-ZERO R-PARM WAS ISSUED, SO GO MOVE
                                ; THE FILE POINTER FORWARD BY READING
                                ; ONE BYTE AT A TIME.  WHEN A <CR> IS
                                ; ENCOUNTERED, REDUCE THE COUNT OF THE
                                ; RELATIVE FIELD POSITIONS LEFT TO MOVE.
                                ; WHEN THE COUNT EQUALS ZERO, WE ARE
                                ; DONE POSITIONING.

CKPSNDUN        LDA RECPRSD     ; CHECK COUNT.
                BNE POSNMORE
                LDX RECPRSD+1
                BEQ DONEPOSN    ; R-PRM HAS BEEN COUNTED DWN TO 0,
                                ; SO WE ARE DONE POSITIONING.
                DEC RECPRSD+1   ; REDUCE COUNT OF R-PARM (IE. # OF
POSNMORE        DEC RECPRSD     ; FIELDS MOVED FORWARD) FOR NEXT
                                ; TIME AROUND.
PSNFIELD        JSR RDTXTBYT    ; GO READ A TEXT FILE BYTE.
                BEQ ENDATERR    ; IF BYTE JUST READ = $00,THEN RAN
                                ; OUT OF DATA.  A ZERO BYTE CAN BE
                                ; OBTAINED FROM AN INCOMPLETELY
                                ; FILLED DATA SECTOR.  OR, IF THE
                                ; FILE ENDS ON A SECTOR BOUNDARY,
                                ; A $00 CAN ALSO BE ACQUIRED FROM
                                ; A ZEROED-OUT T/S LINK OR A
                                ; ZEROED-OUT DATA PAIR (TRK/SEC
                                ; VALUES) LISTED IN A T/S LIST.
                CMP #CR        ; WAS BYT A FIELD-DELIMITING <CR>?
                BNE PSNFIELD    ; NO -GO READ THE NEXT BYTE IN THE
                                ; SAME FIELD.
                BEQ CKPSNDUN    ; YES - GOT END-OF-FIELD MARKER SO
                                ; BRANCH BACK TO REDUCE THE FIELD
                                ; COUNT & SEE IF WERE DONE
                                ; POSITIONING YET.
DONEPOSN        RTS             ; EXIT - EITHER DONE POSITIONING,
                                ; ELSE R-PARM WAS 0 TO START WITH
                                ; & THERE4 NO POSITIONING NEEDED.
                                ; EXIT TO CALLER OF COMMAND.  OFTEN
                                ; RETURNS TO AFTRCMD ($A17D) LOCD
                                ; IN THE CMD PARSING & PROCESSING
                                ; ROUTINES.


                                ; =================================
                                ; WRITE-ONE-DATA-BYTE SUBROUTINE.
                                ; =================================

WRITEXT         JSR CKBSCRUN    ; CHK IF BASIC IS RUNNING A PRGM.
                ifelse(eval(VERSION >= 320),1,`
                BCS CLOSZERO    ; NOT RUNNING, SO GO CLOSE FILE,
                                ; RESET TO CONDITION 0 & THEN DO A
                                ; WARMSTART.  (REMEMBER, WRITE CMD
                                ; IS RESTRICTED TO DEFERRED MODE.)
                ')
                LDA ASAVED      ; RETRIEVE BYTE TO WRITE.
                STA ONEIOBUF    ; PUT IT IN FM PARM LIST.
                LDA #4          ; SET PARM LIST TO WRITE ONE BYTE.
                STA OPCODEFM
                LDA #1
                STA SUBCODFM
                JMP FMDRIVER    ; GO TO FM DRV TO WRITE DATA BYTE.


                                ; =================================
                                ; ROUTINE TO READ A DATA BYTE.
                                ; =================================

READTEXT        JSR CKBSCRUN    ; CHK IF BASIC IS RUNNING A PRGM.
                ifelse(eval(VERSION >= 320),1,`
                BCS CLOSZERO    ; BASIC NOT RUNNING SO GO CLOSE
                                ; FILE, RESET TO CONDITION 0 & DO A
                                ; WARMSTART.  (REMEMBER READ CMD
                                ; IS RESTRICTED TO DEFERRED MODE.)
                ')
                LDA #6          ; SET CONDN6 -IGNORE INPUT PROMPT

SETCOND         STA OPUTCOND
                JSR RDTXTBYT    ; GO READ TEXT FILE DATA BYTE.
                BNE NOTEND      ; IF BYTE READ <> 0, THEN HAVENT
                                ; HIT END-OF-FILE MARKER YET.

                                ; RAN OUT OF DATA.  PICKED UP A $00 BYTE
                                ; EITHER FROM PARTIALLY FULL DATA SECTOR,
                                ; A ZEROED-OUT T/S LINK OR A ZEROED-OUT
                                ; DATA PAIR (TRK/SEC VALUES LISTED IN A
                                ; T/S LIST).

                JSR CLOSEONE    ; RAN OUT OF DATA SO CLOSE FILE.
                LDA #3          ; USING CONDITION 3?
                CMP OPUTCOND    ; IE. HNDLING AN INPUT STATEMENT?
                ifelse(eval(VERSION < 320),1,`
                BEQ TOEXIT2
                ',`
                BEQ DONEPOSN    ; YES - JUST GO TO AN "RTS".
                ')

ENDATERR        LDA #5          ; NO - THERE4 GOT OUT-OF-DATA ERR.
                JMP ERRHNDLR    ; GO HANDLE ERROR.


NOTEND
                ifelse(eval(VERSION < 320),1,`
                STA ASAVED
TOEXIT2         JMP DOSEXIT
CKBSCRUN        LDA ACTBSFLG
                BEQ INTBASIC
                LDX CURLIN+1
                JMP L3FD5
                ',`
                CMP #$E0        ; LOWERCASE?
                BCC SAVIT       ; BRANCH IF UPPERCASE.
                AND #$7F        ; CONVERT LOWER TO UPPER IN ORDER
                                ; TO FOOL CAPTST ROUTINE ($FD7E)
                                ; IN MONITOR ROM.
SAVIT           STA ASAVED      ; SAVE CHAR READ.
                LDX XSAVED      ; GET INDEX TO INPUT BUFFER.
                BEQ TOEXIT      ; BRANCH IF 1ST CHAR.
                DEX             ; TURN HI BIT ON IN PREVIOUS CHAR
                LDA BUF200,X    ; STORED IN BUF200 TO CONVERT TO
                ORA #$80        ; LOWERCASE IF NECESSARY.
                STA BUF200,X
TOEXIT          JMP DOSEXIT     ; GO TO DOSS EXIT ROUTINE.


                                ; ==================================
                                ; CHECK IF BASIC IS RUNNING A PRGM.
                                ; ==================================

CKBSCRUN        PHA             ; SAVE (A) ON STK.
                LDA ACTBSFLG    ; WHICH BASIC IS UP?
                BEQ INTBASIC    ; BRANCH IF USING INTEGER.

                                ; USING APPLESOFT SO NOW CHECK IF
                                ; IN IMMEDIATE OR DEFERRED MODE.
                                ; (IF LINE NUMBER BYTES ARE
                                ; GREATER THAN OR EQUAL TO DECIMAL
                                ; 65280 ($FF IN HI BYTE), THEN THE
                                ; COMPUTER ASSUMES THAT WERE USING
                                ; THE IMMEDIATE MODE.)

                LDX CURLIN+1    ; CHK HI BYTE OF LINE #.
                INX             ; IF $FF --> $00, THEN # > = 65280
                BEQ IMEDMODE    ; BRANCH IF USING IMMEDIATE MODE.

                                ; FP APPEARS TO BE RUNNING A PRGM
                                ; BUT, MAYBE CURLIN+1 WAS ZAPPED
                                ; (POSSIBLY AS PART OF A PROTECTION
                                ; SCHEME) SO BETTER ALSO CHECK THE
                                ; PROMPT.

                LDX PROMPT
                CPX #HICHAR(`]')        ; USING AN APPLESOFT PROMPT?
                BEQ IMEDMODE    ; YES - SO MUST BE IN IMMED MODE.

RUNNING         PLA             ; GET SAVED (A) BACK FROM STK.
                CLC             ; SIGNAL PRGM IS RUNNING.
                RTS
                ')

INTBASIC        LDA RUNMODE     ; CHK INTGR BASICS RUN MODE FLG.
                ifelse(eval(VERSION < 320),1,`
                BMI PT2EXEC2
                ',`
                BMI RUNNING     ; IF NEG, INT BASIC IN DEFERRED.
                ')


                ifelse(eval(VERSION >= 320),1,`
IMEDMODE        PLA             ; GET SAVED (A) BACK FROM STK.
                SEC             ; SIGNAL IN IMMEDIATE MODE.
                RTS
                ')


                                ; ====================================
                                ; CLOSE FILE, SET CONDITION0, & EXIT.
                                ; ====================================

CLOSZERO        JSR CLOSEONE    ; CLOSE OPEN FILE.
                JSR RESTAT0     ; RESET TO CONDITION 0.
                JMP DOSEXIT     ; GO TO DOSS EXIT ROUTINE.


                                ; =================================
                                ; EXECS READ DATA ROUTINE.
                                ; =================================

READEXEC        JSR PT2EXEC     ; POINT THE A3L/H POINTER AT BUF
                                ; THAT WERE EXECING IN.
                JSR BUFS2PRM    ; COPY ADDRS OF THE VARIOUS DOS
                                ; BUFS FROM THE CHAIN BUF & PUT
                                ; THEM IN THE FM PARAMETER LIST.
                LDA #3          ; SET CONDITION 3 SO PROCESS DATA
                BNE SETCOND     ; INPUT FROM THE DISK.


                                ; =================================
                                ; READ A TEXT FILE BYTE.
                                ; =================================

RDTXTBYT        LDA #3          ; SET FM PRM LIST TO READ ONE BYTE.
                STA OPCODEFM
                LDA #1
                STA SUBCODFM
                JSR FMDRIVER    ; CALL FM DRIVER TO READ A BYTE.
                LDA ONEIOBUF    ; LOAD (A) WITH BYTE JUST READ.
                RTS


                                ; =================================
                                ; POINT THE A3L/H POINTER AT
                                ; BUFFER THAT WERE EXECING IN.
                                ; =================================

PT2EXEC         LDA EXECBUFF+1  ; GET ADR OF DOS BUF USING TO EXEC.
                STA A3H         ; PUT IT IN POINTER.
                LDA EXECBUFF
                STA A3L
PT2EXEC2        RTS


                                ; =================================
                                ; THE FILE MANAGER DRIVER.
                                ; =================================

FMDRIVER        JSR FILEMGR     ; CALL FM MANAGER TO DO FUNCTION.

                                ; RETURN HERE AFTER DOING THE FUNCTION.
                                ; (BECAUSE, USE STACK TO GET BACK TO
                                ; ORIGINAL CALLER OF FUNCTION.)
                                ; IF WE JUST DID A READ FUNCTION &
                                ; THE LAST BYTE READ WAS FROM A DATA
                                ; SECTOR, THEN ENTER WITH (C)=0.
                                ; (NOTE THAT IT MAKES NO DIFFERENCE
                                ; IF THAT DATA BYTE WAS A $00 OR NOT.)
                                ; HOWEVER, IF WE ARE DEALING WITH A
                                ; ZEROED-OUT T/S LINK OR A ZEROED-OUT
                                ; DATA-PAIR BYTE FROM A T/S LIST,
                                ; THEN ENTER WITH CARRY SET.


                ifelse(eval(VERSION < 320),1,`
                BCS AFTRFUNC2
                RTS
                ',`
AFTRFUNC        BCC FMDRVRTN    ; (C) = 0 = NO ERRORS.
                ')

                ifelse(eval(VERSION == 320 || VERSION == 321),1,`
                JSR GETBUFF
                BCS AFTRFUNC2
                LDA #0
                TAY
                STA (A3L),Y
                ')

AFTRFUNC2
                LDA RTNCODFM    ; GET RETURN CODE FRM FM PARM LIST
                CMP #5          ; "END-OF-DATA" ERROR?
                ifelse(eval(VERSION < 320),1,`
                BNE FMDRVRTN2

                ',`
                ifelse(eval(VERSION < 330),1,`
                BNE ERRHNDLR
                ',`
                BEQ TOAPPTCH    ; YES -NOT HANDLED LIKE OTHER ERRS
                                ; FILE ENDS AT A FULL DATA SEC SO
                                ; WE ENCOUNTERED A ZEROED-OUT T/S
                                ; LINK OR A ZEROED-OUT DATA PAIR
                                ; (TRK/SEC VALUES LISTED IN A T/S
                                ; LIST).
                JMP OTHRERR     ; ONLY TAKE IF GOT AN ERROR OTHER
                                ; THAN AN END-OF-DATA ERROR.
TOAPPTCH        JMP APNDPTCH    ; GO HANDLE END-OF-DATA ERROR.

                NOP
                ifelse(eval(VERSION >= 331),1,`
BK2FMDRV        JSR CKIFAPND    ; <---NOTE: APNDPTCH RETURN HERE!!
                                ; GO CHK IF THE APPEND FLAG IS ON.
                ',`
                ifelse(eval(VERSION == 330),1,`
                NOP
BK2FMDRV        NOP
                NOP
                ')
                ')
                ')
                ')

                LDX #0          ; ZERO-OUT THE ONE-DATA-BYTE BUF
                STX ONEIOBUF    ; IN THE FM PARAMETER LIST.  (ALSO
                                ; REFERRED TO AS THE LOW BYTE OF
                                ; CURIOBUF.)
FMDRVRTN        RTS             ; RETURN TO CALLER OF FM DRIVER.
                ifelse(eval(VERSION < 320),1,`
FMDRVRTN2       JMP ERRHNDLR
                ')

                                ; =================================
                                ; SELECTED ERROR PROCESSING.
                                ; =================================
                                ; NOTE: PROGRAMMERS WHO ACCESS DOS
                                ; FROM ASSEMBLY LANGUAGE PROGRAMS
                                ; SHOULD TAKE SPECIAL NOTE OF THE
                                ; THE FORMATTED DISASSEMBLY TITLED
                                ; "DISASSEMBLY OF ERRORS".


SYNTXERR        LDA #11
                BNE ERRHNDLR    ; ALWAYS.
NOBUFERR        LDA #12
                BNE ERRHNDLR    ; ALWAYS.
TOOLARGE        LDA #14
                BNE ERRHNDLR    ; ALWAYS.
TYPMISM         LDA #13

                ifelse(eval(VERSION < 320),1,`
                BNE ERRHNDLR    ; ALWAYS.
NOTBINARY       LDA #15
                ')

                                ; =================================
                                ; DOSS MAIN ERROR-HANDLER ROUTINE
                                ; =================================

ERRHNDLR        STA ASAVED      ; SAVE RETURN CODE FOR LATER USE.
                ifelse(eval(VERSION < 320),1,`
                JSR RESTAT0
                ',`
                JSR RESTATIN    ; RESET THE FOLLOWING FLAGS TO 0:
                                ; OPUTCOND, CONDNFLG & RUNTRUPT.
                ')
                LDA ACTBSFLG    ; CHK IF INT OR FP BASIC ACTIVE.
                BEQ WASINT      ; BRANCH IF USING INTEGER.
                                ; (ONERR FLAG NOT APPLIC TO INT.)
                LDA ERRFLG      ; CHK IF BASICS ONERR FLAG IS ON.
                ifelse(eval(VERSION < 320),1,`
                BMI ONERRACT2
                ',`
                BMI ONERRACT    ; YES - SKIP PRINTING OF ERROR MSG
                                ; BECAUSE WE EVENTUALLY WANT TO GO TO
                                ; OUR OWN CUSTOMIZED ERROR-HNDLING
                                ; ROUTINE.
                ')
WASINT          LDX #0          ; INITIALIZE INDEX TO TABLE OF
                                ; OFFSETS TO ERRORS.
                JSR PRDOSERR    ; GO PRINT <RTN>, BELL, <RTN>.
                LDX ASAVED      ; GET SAVED RETURN CODE.
                JSR PRDOSERR    ; GO PRINT THE ERROR MESSAGE.
                ifelse(eval(VERSION < 320),1,`
                LDX #$10
                ',`
                JSR CRVIADOS    ; PRINT A <CR>.
                ')
ONERRACT
                ifelse(eval(VERSION < 320),1,`
                JSR PRDOSERR
                ',`
                JSR INITIOHK    ; RESET I/O HKS TO POINT TO DOS.
                ')
ONERRACT2
                ifelse(eval(VERSION < 320),1,`
                JSR INITIOHK
                ',`
                JSR CKBSCRUN    ; CHK IF BASIC IS RUNNING A PRGM:
                                ; (C) = 0 IF RUNNING.
                                ; (C) = 1 IF IMMEDIATE.
                ')
                LDX ASAVED      ; GET SAVED RETURN CODE.
                LDA #3          ; SET (A) = 3 IN CASE FALL THRU TO
                                ; GO TO BASICS ERROR HANDLING
                                ; ROUTINE. THE MAGIC # OF 3 ALLOWS
                                ; BSCERHLR ($D865) TO CONDITION
                                ; (C) = 0 AND (Z) = 1 IN ORDER TO
                                ; COMPLY WITH THE BASIC ROUTINE
                                ; THAT IS RESPONSIBLE FOR PRINTING
                                ; BASICS ERROR MESSAGES.
                ifelse(eval(VERSION >= 320),1,`
                BCS DOWRM       ; BASIC IS NOT RUNNING.
                ')

TOBSCERR        JMP (ADBSCERR)  ; TO BASICS ERROR HANDLING ROUTINE
                                ; (BSCERHLR, $D865).

                ifelse(eval(VERSION >= 320),1,`
DOWRM           JMP (TOWRMVEC)  ; TO BASICS WARMSTART ROUTINE
                                ; (RESTART, $D43C).
                ')


                                ; =================================
                                ; PRINT THE DOS ERROR MESSAGE.
                                ; =================================

PRDOSERR        LDA OFF2ERR,X   ; USE ERROR CODE TO GET OFFSET TO
                                ; ERROR MESSAGE.
                TAX             ; (X) = OFFSET INTO THE TABLE
                                ; CONTAINING THE TEXT OF THE DOS
                                ; ERROR MESSAGES.
MORERMSG        STX TEMPBYT     ; SAVE OFFSET INTO TXT TABLE.
                LDA ERRTXTBL,X  ; GET CHAR OF ERROR MESSAGE.
                PHA             ; SAVE IT ON STACK.
                ORA #$80        ; TURN HI BIT ON TO SATISFY MONITOR
                JSR GODSPLY     ; GO PRINT VIA TRUE OUTPUT HANDLER
                LDX TEMPBYT     ; RESET OFFSET TO TABLE OF TEXT.
                INX             ; KICK INDEX UP FOR NXT CHR OF MSG
                PLA             ; GET ORIG CHAR BACK IN (A).
                BPL MORERMSG    ; BRANCH IF MORE CHRS IN MSG TO PRT
                RTS             ; ALL BUT LAST CHR IN MSG ARE POS.


                                ; =================================
                                ; PUT VOL, DRV, AND SLOT VALUES
                                ; PLUS THE ADR OF THE PRIMARY FILE
                                ; NAME BUFFER IN FM PARAMETER LIST
                                ; =================================

CPY2PARM        LDA VOLPRSD     ; FROM PARSED TABLE.
                STA VOLFM
                LDA DRVPRSD     ; FROM PARSED TABLE.
                STA DRVFM
                LDA SLOTPRSD    ; FROM PARSED TABLE.
                STA SLOTFM
                LDA ADRPFNBF    ; GET THE ADR OF THE PRIMARY FILE
                STA FNAMBUFM    ; NAME BUF FROM THE CONSTANTS TBL
                LDA ADRPFNBF+1  ; AND PUT IT IN THE FM PARM LIST.
                STA FNAMBUFM+1
                LDA A3L         ; SAVE ADR OF CURRENT DOS FILENAME
                STA CURFNADR    ; BUF IN TABLE OF DOS VARIABLES.
                LDA A3H
                STA CURFNADR+1
                RTS


                                ; ===================================
                                ; COPY NAME OF FILE FROM THE PRIMARY
                                ; FILENAME BUFFER TO THE APPROPRIATE
                                ; DOS NAME BUFFER LOCATED IN THE
                                ; CHAIN OF DOS BUFFERS.
                                ; THIS ASSIGNS (OR RE-ASSIGNS) A DOS
                                ; BUFFER TO THE FILE WE WANT TO OPEN.
                                ; THE HIGHEST NUMBERED (LOWEST IN
                                ; MEMORY) FREE DOS BUFFER IS USED.
                                ; ===================================

CPYPFN          LDY #29         ; 30 BYTES TO COPY (0 TO 29).
CPYPRIM         LDA PRIMFNBF,Y  ; GET CHAR FROM PRIMARY.
                STA (A3L),Y     ; STORE IT IN DOS NAME BUF.
                DEY             ; REDUCE COUNTER.
                BPL CPYPRIM     ; MORE CHARS TO COPY.
                RTS


                                ; ====================================
                                ; GET THE ADDRS OF THE VARIOUS DOS
                                ; BUFS FROM THE CURRENT DOS CHAIN
                                ; BUF & PUT THEM IN THE FM PARM LIST.
                                ; ====================================

BUFS2PRM        LDY #30         ; GET ADR OF FM WORK BUF, T/S LIST
ADRINPRM        LDA (A3L),Y     ; BUF, DATA SECTOR BUF & NEXT
                STA WRKBUFFM-30,Y
                                ; DOS FILE NAME BUF FROM CHAIN
                INY             ; PTRS BUF & PUT IN FM PARM LIST.
                CPY #38         ; (PS. ADDR OF NEXT DOS FILE NAME
                BNE ADRINPRM    ; BUF IS NOT USED BY DOS.)
                RTS


                                ; =================================
                                ; RESET CONDNFLG & OPUTCOND TO 0.
                                ; =================================

RESTAT0         LDY #0
                STY CONDNFLG
                STY OPUTCOND
                RTS


                                ; ==================================
                                ; LOCATE BUFFER WITH SAME NAME.
                                ; IF THAT FAILS, LOCATE A FREE BUF.
                                ; ==================================

GETBUFF         LDA #0          ; DEFAULT HI BYTE OF PTR TO 0.
                STA A5H         ; (IE. ASSUME NO FREE BUFS AVAIL.)
                JSR GETFNBF1    ; PT A3L/H AT 1ST DOS FILE NAME
                                ; BUFFER IN THE DOS BUFFER CHAIN.
                JMP FNCHAR1     ; GO GET 1ST CHR OF NAME FRM BUF.
GETFNLNK        JSR GETNXBUF    ; GET ADR OF NXT NAME BUF IN CHAIN
                                ; FROM CHAIN POINTERS BUF(WHICH IS
                                ; OFFSET 37 & 36 BYTES FROM 1ST
                                ; CHAR OF PRESENT FILE NAME BUF).
                BEQ NOFNMTCH    ; LINK ZEROED OUT=END OF BUF CHAIN
FNCHAR1         JSR GETFNBY1    ; GET 1ST CHR OF NAME FRM NAM BUF
                BNE NXFNBUF     ; TAKE BRANCH IF BUF NOT FREE.
                LDA A3L         ; BUF WAS FREE, THERE4 POINT THE
                STA A5L         ; A5L/H POINTERS AT THE FREE BUF.
                LDA A3H
                STA A5H
                BNE GETFNLNK    ; ALWAYS.

NXFNBUF         LDY #29         ; BUF WASNT FREE SO CMP NAME OF
CMPFNCHR        LDA (A3L),Y     ; OWNER WITH NAME OF FILE IN
                CMP PRIMFNBF,Y  ; PRIMARY FILE NAME BUF.  (START
                                ; WITH LAST CHAR FIRST.)
                BNE GETFNLNK    ; CHAR DIDNT MATCH, SO LOOK FOR
                                ; ANOTHER BUF THAT MIGHT HAS SAME
                                ; FILE NAME.
                DEY             ; THAT CHAR MATCHED.  HOW ABOUT
                                ; REST OF CHARS IN NAME?
                BPL CMPFNCHR    ; 30 CHARS IN NAME (IE. 0 TO 29).
                CLC             ; (C)=0 TO SIGNAL NAMES MATCHED.
                RTS

NOFNMTCH        SEC             ; LINK ZEROED OUT.
                RTS


                                ; ===================================
                                ; POINT THE A3L/H POINTER AT THE
                                ; FIRST DOS FILE NAME BUFFER IN
                                ; THE DOS BUFFER CHAIN.  (IE. LOWEST
                                ; NUMBERED BUFFER, BUT HIGHEST IN
                                ; MEMORY.)
                                ; ===================================

GETFNBF1        LDA ADOSFNB1    ; GET 1ST LINK TO CHAIN OF BUFS.
                LDX ADOSFNB1+1
                BNE SETNXPTR    ; ALWAYS.


                                ; =================================
                                ; GET ADR OF NXT FILENAME BUF IN
                                ; CHAIN FROM THE CURRENT CHAIN
                                ; POINTERS BUF (WHICH IS OFFSET
                                ; 37 & 36 BYTES FROM 1ST CHAR
                                ; IN PRESENT DOS FILE NAME BUF).
                                ; ---------------------------------

GETNXBUF        LDY #37         ; OFFSET TO CHAIN BUF.
                LDA (A3L),Y     ; PICK UP ADR OF NEXT NAME BUF.
                BEQ GETNXRTN    ; IF HI BYTE=$00, LINK ZEROED OUT.
                TAX             ; SAVE HI BYTE IN (X).
                DEY             ; OFFSET FOR LOW BYTE.
                LDA (A3L),Y     ; PUT ADR OF FILE NAME BUF IN PTR.
SETNXPTR        STX A3H         ; PUT HI BYTE IN POINTER.
                STA A3L         ; PUT LOW BYTE IN POINTER.
                TXA             ; GET HI BYTE BACK IN (A).
GETNXRTN        RTS


                                ; =================================
                                ; GET 1ST CHAR OF FILE NAME
                                ; FROM DOS FILE NAME BUFFER.
                                ; =================================

GETFNBY1        LDY #0          ; BUF IS FREE IF 1ST BYTE = $00.
                LDA (A3L),Y     ; ELSE 1ST BYTE = 1ST CHAR OF
                RTS             ; NAME OF FILE WHICH OWNS BUF.


                                ; =================================
                                ; CHECK IF THE CURRENT FILE NAME
                                ; BUFFER BELONGS TO AN EXEC FILE.
                                ; (AFTER ALL, WE DONT WANT TO
                                ; PREMATURELY CLOSE A FILE IF WE
                                ; ARE USING IT TO EXEC - WOULD BE
                                ; LIKE BURYING OURSELVES ALIVE).
                                ; =================================

CKEXCBUF        LDA EXECFLAG    ; CHK TO SEE IF EXECING.
                BEQ NOTEXCBF    ; BRANCH IF NOT EXECING.
                LDA EXECBUFF    ; WE ARE EXECING, THERE4 CHK IF BUF
                CMP A3L         ; BELONGS TO THE EXEC FILE.
                BNE CKEXCRTN    ; NO.
                LDA EXECBUFF+1  ; MAYBE - LOW BYTES MATCHED SO
                CMP A3H         ; CHK HI BYTES OF ADR.
                BEQ CKEXCRTN    ; YES, EXEC BUF = CURRENT BUF.
NOTEXCBF        DEX             ; NOT EXECING, SO REDUCE (X) TO
                                ; MAKE SURE THAT Z-FLAG IS OFF.
                                ; (PS. (X) WAS ORIG CONDITIONED TO
                                ; A LARGE NON-ZERO VAL ON ENTRY
                                ; TO GETFNBF1, THERE4, IF NOW DEX,
                                ; THEN INSURE Z-FLAG OFF.)
CKEXCRTN        RTS             ; EXIT WITH:
                                ; Z-FLAG = 1 IF EXECING.
                                ; = 0 IF NOT EXECING.


                                ; ======================================
                                ; CHK IF FILE TYPE WANTED = TYPE FOUND.
                                ; ======================================

CHKFTYPE        EOR FILTYPFM    ; TYPE FOUND (VIA OPEN FUNCTION).
                BEQ CKTYPRTN    ; BRNCH IF TYPE WANTED=TYPE FOUND.
                AND #%01111111  ; MAYBE MATCHED-DISREGARD LOCK BIT
                BEQ CKTYPRTN    ; BRANCH IF MATCHED.
                JSR CMDCLOSE    ; NAMED FILE IS WRONG TYPE, SO GO
                JMP TYPMISM     ; CLOSE FILE & EXIT WITH A TYPE-
                                ; MISMATCH ERROR MESSAGE.
CKTYPRTN        RTS             ; TYPE WANTED = TYPE FOUND.


                                ; =================================
                                ; BUILD THE DOS BUFFERS.
                                ; =================================

                                ; POINT A3L/H AT FILENAME FIELD
                                ; IN THE LOWEST NUMBERD (HIGHEST
                                ; IN MEMORY) DOS BUFFER.

BILDBUFS        SEC             ; IRREL, MIGHT AS WELL BE A "NOP".
                LDA ADOSFNB1    ; GET ADDR OF 1ST FILE NAME FIELD
                STA A3L         ; & PUT IT IN A3L/H POINTER.
                LDA ADOSFNB1+1
                STA A3H

                                ; GET # OF MAXFILES WANTED & STORE
                                ; IT IN THE COUNTER (TEMPBYT).

                LDA MXFILVAL
                STA TEMPBYT

                                ; FREE BUFFER BY ZEROING OUT THE
                                ; FIRST BYTE OF THE DOS BUFFERS
                                ; FILE NAME FIELD.

ZDOSBUFN        LDY #0
                TYA
                STA (A3L),Y


                                ; POINT LINK IN CHAIN POINTERS BUF
                                ; AT FM WORK AREA BUFFER.

                LDY #30         ; SET (Y) TO INDEX 1ST LINK IN
                                ; CHAIN POINTERS BUFFER.
                SEC
                LDA A3L         ; SUBT 45 FROM LOW BYTE OF ADDR
                SBC #45         ; OF NAME BUF TO CALC LOW BYTE OF
                STA (A3L),Y     ; ADDR OF FM WORK BUF & PUT IT IN
                PHA             ; THE CHAIN PTR BUF & ON THE STK.
                LDA A3H         ; SUBT (C) FROM HIGH BYTE OF ADR
                SBC #0          ; OF NAME BUF TO GET HI BYTE OF
                                ; FM WRK BUF ADR.
                INY             ; KICK UP (Y) TO INDEX ADDR OF HI
                                ; BYTE OF LINK IN CHAIN POINTERS.
                STA (A3L),Y     ; STORE HI BYTE OF ADR OF FM WRK
                                ; BUF IN THE LINK.
                                ; (NOTE:  ABOVE CALCS EFFECT (A)
                                ; BUT NOT A3L/H.)

                                ; POINT LINK IN CHAIN POINTERS BUFFER
                                ; AT T/S LIST SECTOR BUFFER.

                TAX             ; PUT HI BYTE OF ADDR OF FM WRK BUF
                DEX             ; IN (X) & KICK IT DOWN SO IT
                                ; INDEXES HI BYTE OF T/S LIST BUF.
                                ; (T/S LST BUF = $100 BYTES LONG.)
                PLA             ; GET LOW BYTE OF ADDR OF FM WRK
                PHA             ; BACK FROM STK.
                INY             ; KICK UP INDEX TO LINK IN CHAIN
                                ; POINTERS BUFFER.
                STA (A3L),Y     ; PUT LOW BYTE OF FM WRK BUF ADR
                                ; IN LINK BUFFERS POINTERS.
                TXA             ; GET HI BYTE T/S LIST BUF IN (A).
                INY             ; KICK UP INDEX IN CHAIN BUF.
                STA (A3L),Y     ; PUT HI BYTE OF LINK IN PTRS BUF.

                                ; POINT LINK IN CHAIN POINTERS BUF
                                ; AT DATA SECTOR BUFFER.

                TAX             ; PUT HI BYTE OF ADDR OF T/S LIST
                DEX             ; BUF IN (X) & KICK IT DOWN TO
                                ; CORRESPOND TO HI BYTE OF ADDR
                                ; OF DATA SEC BUF.
                PLA             ; GET LOW BYTE OF T/S LIST SEC BUF
                PHA             ; FROM STACK & USE IT FOR LOW BYTE
                                ; OF DATA SEC BUF (BECAUSE THEY ARE
                                ; EXACTLY 1 PAGE APART).
                INY             ; KICK UP INDEX TO CHAIN BUF.

                STA (A3L),Y     ; PUT LOW BYTE OF DATA SEC BUF
                                ; IN LINK.
                INY             ; KICK UP INDEX TO CHAIN BUF.
                TXA             ; GET HI BYTE OF ADR OF T/S LIST
                STA (A3L),Y     ; BUF & DESIGN8 AS HI BYT OF LINK.

                                ; REDUCE COUNTER FOR # OF BUFS TO BUILD.

                DEC TEMPBYT     ; IF COUNTER GOES TO 0, THEN JUST
                BEQ ZLNK2NXT    ; DID LAST BUF & SHOULD 0 OUT LNK.

                                ; NOT DONE ALL BUFS YET SO POINT
                                ; LINK IN CHAIN POINTERS BUFFER
                                ; AT NEXT FILE NAME FIELD.

                TAX             ; SET (X) = LOW BYTE OF ADR OF DATA
                                ; SECTOR BUFFER.
                PLA             ; GET LOW BYTE OF ADDR OF DATA
                                ; SECTOR BUF BACK OFF STK.
                SEC             ; SUBT 38 FROM LOW BYTE OF DATA SEC
                SBC #38         ; ADR TO INDEX NEXT NAME BUF.
                INY             ; KICK UP INDEX TO CHAIN BUF.
                STA (A3L),Y     ; STORE LOW BYTE OF ADR OF NEXT
                PHA             ; NAME BUF IN LINK & THEN SAVE
                                ; IT ON STK.
                TXA             ; GET HI BYTE OF ADR OF DATA SEC
                SBC #0          ; BUF FROM (X) & SUBT (C) (IN CASE
                                ; CROSS PAGE BOUNDARY) TO GET
                                ; (A) = HI BYTE OF NEXT NAME BUF.
                INY             ; KICK INDEX UP TO CHAIN PTRS BUF
                STA (A3L),Y     ; & STORE HI BYTE OF NEXT NAME BUF
                                ; IN LINK.

                                ; POINT A3L/H AT NEXT NAME BUF.

                STA A3H
                PLA
                STA A3L
                JMP ZDOSBUFN    ; GO BACK TO FREE NEXT NAME BUF
                                ; & BUILD MORE DOS BUFFERS.

                                ; NO MORE BUFS TO BUILD SO ZERO OUT
                                ; THE LINK THAT WOULD NORMALLY POINT
                                ; TO THE NEXT NAME BUFFER.

ZLNK2NXT        PHA             ; SAVE LOW BYTE OF ADR OF DATA BUF
                                ; ON STK.
                LDA #0          ; ZERO OUT LINK TO NEXT NAME BUF.
                INY
                STA (A3L),Y
                INY
                STA (A3L),Y

                                ; CHK WHICH BASIC IS ACTIVE.

                LDA ACTBSFLG    ; CHK IF ACTV BASIC IS FP OR INT.
                BEQ SETINTPT    ; BRANCH IF INTEGER.

                                ; USING APPLESOFT, SO INITIALIZE
                                ; MEMSIZ & FRETOP (STRING STORAGE)
                                ; TO A VALUE 1 BYTE GREATER THAN
                                ; HIGHEST MEMORY LOCATION AVAILABLE
                                ; TO BASIC PROGRAM.

                PLA
                STA MEMSIZ+1
                STA FRETOP+1
                PLA
                STA MEMSIZ
                STA FRETOP
                RTS             ; EXIT TO CALLER OF MAXFILES CMD.
                                ; (USUALLY EXITS TO AFTRCMD ($A17D)
                                ; LOCATED IN THE DOS CMD PARSING
                                ; AND PROCESSING ROUTINES.)

                                ; USING INTEGER, SO SET HIMEM AND
                                ; PROGRAM POINTER (INTPGMST).

SETINTPT        PLA
                STA HIMEM+1
                STA INTPGMST+1
                PLA
                STA HIMEM
                STA INTPGMST
                RTS             ; EXIT TO CALLER OF MAXFILES CMD.
                                ; (USUALLY EXITS TO AFTRCMD ($A17D)
                                ; LOCATED IN THE DOS CMD PARSING
                                ; AND PROCESSING ROUTINES.)

                                ; ===================================
                                ; INITIALIZE THE I/O HOOKS SO THAT
                                ; DOS INTERCEPTS ALL INPUT & OUTPUT.
                                ; ===================================

                                ; FOR INSTANCE, IF A ROUTINE ACCESSES
                                ; "COUT: JMP (CSW)" THEN EXECUTION
                                ; WILL ACTUALLY FLOW TO DOSS
                                ; OUTPUT ROUTINE (OPUTINCP, $9EBD).
                                ; SIMILARLY, ANY ROUTINE THAT REFERS
                                ; TO "RDKEY: JMP (KSW)" WILL ACTUALLY
                                ; JUMP TO DOSS INPUT ROUTINE
                                ; (INPTINCP, $9E81).
                                ;
                                ; THE TRUE (IE. NORMAL) HOOKS ARE SAVED,
                                ; EX.  KSW: KEYIN --> KSWTRUE: KEYIN.
                                ; CSW: COUT1 --> CSWTRUE: COUT1.
                                ; THEN THE INTERCEPTS ARE SET AS FOLLOWS:
                                ; ADINPTCP: INPTINCP --> KSW: INPTINCP.
                                ; ADOPUTCP: OPUTINCP --> CSW: OPUTINCP.

                                ; CHECK IF INPUT HOOK NEEDS TO BE RESET.
INITIOHK        LDA KSW+1
                CMP ADINPTCP+1
                BEQ CKOUTHK     ; INPUT HK ALREADY POINTS TO DOSS
                                ; INPUT HNDLER SO GO CHK OUTPUT HK

                                ; RESET INPUT HOOK TO POINT TO DOS.

                STA KSWTRUE+1   ; KSW: KEYIN --> KSWTRUE: KEYIN.
                LDA KSW
                STA KSWTRUE
                LDA ADINPTCP    ; ADINPTCP:INPTINCP-->KSW:INPTINCP
                STA KSW
                LDA ADINPTCP+1
                STA KSW+1

                                ; CHECK IF OUTPUT HOOK NEEDS TO BE RESET.

CKOUTHK         LDA CSW+1
                CMP ADOPUTCP+1
                BEQ SETHKRTN    ; OUTPUT HK ALREADY PTS TO DOSS
                                ; OUTPUT HANDLER, SO EXIT.

                                ; RESET OUTPUT HOOK TO POINT TO DOS.

                STA CSWTRUE+1   ; CSW: COUT1 --> CSWTRUE: COUT1.
                LDA CSW
                STA CSWTRUE
                LDA ADOPUTCP    ; ADOPUTCP:OPUTINCP-->CSW:OPUTINCP
                STA CSW
                LDA ADOPUTCP+1
                STA CSW+1
SETHKRTN        RTS



                                ; ========================================
                                ; DOS COMMAND TEXT TABLE ($A884 - $A908).
                                ; ========================================
                                ; THE NAMES OF THE DIFFERENT DOS COMMANDS
                                ; CAN READILY BE CHANGED BY ALTERING THE
                                ; INFORMATION IN THIS TABLE.  IF YOU
                                ; DECIDE TO MESS AROUND WITH THIS TABLE,
                                ; BE SURE TO:
                                ; - LET THE 1ST CMD CREATE A NEW FILE.
                                ; (FOR EXPLANATION, SEE FMXTNTRY
                                ; ROUTINE ($AAFD) IN LINEAR DISMBLY.)
                                ; - AVOID CREATING NEW DOS CMD NAMES
                                ; THAT DUPLICATE BASIC CMD NAMES.
                                ; - USE POSITIVE ASCII CHARS FOR ALL BUT
                                ; THE LAST CHAR OF EACH NAME.
                                ; - ENTER THE LAST CHAR IN EACH NAME IN
                                ; NEGATIVE ASCII FORM.
                                ; - SHIFT SUBSEQUENT NAMES TO KEEP ALL
                                ; CHARS CONTIGUOUS IF YOU CREATE
                                ; SHORTER NAMES.
                                ; - DONT EXPAND THE TABLE BEYOND $A908.
                                ; - END THE TABLE WITH A 0 BYTE.
                                ; ========================================

CMDTXTBL        LHASCII(`INIT')
                LHASCII(`LOAD')
                LHASCII(`SAVE')
                LHASCII(`RUN')
                LHASCII(`CHAIN')
                LHASCII(`DELETE')
                LHASCII(`LOCK')
                LHASCII(`UNLOCK')
                LHASCII(`CLOSE')
                LHASCII(`READ')
                LHASCII(`EXEC')
                LHASCII(`WRITE')
                LHASCII(`POSITION')
                LHASCII(`OPEN')
                LHASCII(`APPEND')
                LHASCII(`RENAME')
                LHASCII(`CATALOG')
                LHASCII(`MON')
                LHASCII(`NOMON')
                LHASCII(`PR#')
                LHASCII(`IN#')
                LHASCII(`MAXFILES')
                LHASCII(`FP')
                LHASCII(`INT')
                LHASCII(`BSAVE')
                LHASCII(`BLOAD')
                LHASCII(`BRUN')
                LHASCII(`VERIFY')
                ASM_DATA(0)         ; 0 BYTE DENOTES END OF TABLE.


                                ; ========================================
                                ; TABLE OF ATTRIBUTES & VALID OPTIONS
                                ; THAT ARE ASSOCIATED WITH EACH COMMAND.
                                ; ($A909 - $A940)
                                ; ========================================

                                ; NOTE:  ANY ALTERATIONS TO THIS TABLE
                                ; SHOULD BE DONE WITH A DEGREE OF CAUTION
                                ; BECAUSE SEVERAL PARAMETERS ACQUIRE
                                ; CERTAIN DEFAULT VALUES IN THE ACTUAL
                                ; COMMAND-HANDLING ROUTINES.  WITH JUST
                                ; A BIT OF INSPECTION OF THE HANDLING
                                ; ROUTINES HOWEVER, YOU SHOULD BE ABLE TO
                                ; SAFELY MAKE EXTENSIVE CHANGES IF YOU SO
                                ; DESIRE.
                                ;
                                ; THIS TABLE IS INDEXED BY A VALUE
                                ; CORRESPONDING TO THE PARSED DOS
                                ; COMMAND.  TWO BYTES OF INFORMATION
                                ; ARE ASSOC WITH @ CMD.  A SET BIT
                                ; DENOTES THE FOLLOWING:
                                ;
                                ; LO BYTE
                                ; 7  6  5  4  3  2  1  0
                                ; .  .  .  .  .  .  .  .
                                ; .  .  .  .  .  .  .  ...CMD CAN CREATE NEW FILE IF FILE
                                ; .  .  .  .  .  .  .     NOT FOUND.
                                ; .  .  .  .  .  .  ......CMD RESTRICTED TO DEFERRED MODE.
                                ; .  .  .  .  .  .........VALUE FOR MAXFILES REQUIRED.
                                ; .  .  .  .  ............VALUE FOR PR# OR IN# REQUIRED.
                                ; .  .  .  ...............2ND FILENAME NEEDED (RENAME CMD)
                                ; .  .  ..................FILE NAME APPLICABLE TO CMD
                                ; .  .                    (EXPECTED BUT NOT NECESSARILY
                                ; .  .                    REQUIRED.)
                                ; .  .................... DO DOS CMD EVEN IF NO NAME GIVEN
                                ; ....................... INTERPRET AS BASIC CMD IF NO NAM

                                ; HI BYTE
                                ; 7  6  5  4  3  2  1  0
                                ; .  .  .  .  .  .  .  .
                                ; .  .  .  .  .  .  .  ...A(DDRESS) PARAMETER ALLOWED.
                                ; .  .  .  .  .  .  ......B(YTE) PARAMETER ALLOWED.
                                ; .  .  .  .  .  .........R(ECORD) # OR R(EL FIELD POSN)
                                ; .  .  .  .  .           PARAMETER ALLOWED.
                                ; .  .  .  .  ............L(ENGTH) PARAMETER ALLOWED.
                                ; .  .  .  ...............S(LOT) PARAMETER ALLOWED.
                                ; .  .  ..................D(RIVE) PARAMETER ALLOWED.
                                ; .  .....................V(OLUME) PARAMETER ALLOWED.
                                ; ........................C, I, OR O PARAMETER ALLOWED.

CMDATTRB
                ifelse(eval(VERSION < 320),1,`
                ASM_ADDR(%0111000000100000) ; INIT
                ',`
                ASM_ADDR(%0111000000100001) ; INIT
                ')
                ASM_ADDR(%0111000010100000) ; LOAD
                ifelse(eval(VERSION < 320),1,`
                ASM_ADDR(%0111000010100000) ; SAVE
                ',`
                ASM_ADDR(%0111000010100001) ; SAVE
                ')
                ASM_ADDR(%0111000010100000) ; RUN
                ASM_ADDR(%0111000000100000) ; CHAIN
                ASM_ADDR(%0111000000100000) ; DELETE
                ASM_ADDR(%0111000000100000) ; LOCK
                ASM_ADDR(%0111000000100000) ; UNLOCK
                ASM_ADDR(%0000000001100000) ; CLOSE
                ifelse(eval(VERSION < 320),1,`
                ASM_ADDR(%0000011000100000) ; READ
                ',`
                ASM_ADDR(%0000011000100010) ; READ
                ')
                ASM_ADDR(%0111010000100000) ; EXEC
                ifelse(eval(VERSION < 320),1,`
                ASM_ADDR(%0000011000100000) ; WRITE
                ASM_ADDR(%0000010000100000) ; POSITION
                ASM_ADDR(%0111100000100000) ; OPEN
                ASM_ADDR(%0111100000100000) ; APPEND
                ',`
                ASM_ADDR(%0000011000100010) ; WRITE
                ASM_ADDR(%0000010000100010) ; POSITION
                ASM_ADDR(%0111100000100011) ; OPEN
                ASM_ADDR(%0111000000100010) ; APPEND
                ')
                ASM_ADDR(%0111000000110000) ; RENAME
                ASM_ADDR(%0111000001000000) ; CATALOG
                ASM_ADDR(%1000000001000000) ; MON
                ASM_ADDR(%1000000001000000) ; NOMON
                ASM_ADDR(%0000000000001000) ; PR#
                ASM_ADDR(%0000000000001000) ; IN#
                ASM_ADDR(%0000000000000100) ; MAXFILES
                ASM_ADDR(%0111000001000000) ; FP
                ASM_ADDR(%0000000001000000) ; INT
                ifelse(eval(VERSION < 320),1,`
                ASM_ADDR(%0111100100100000) ; BSAVE
                ',`
                ASM_ADDR(%0111100100100001) ; BSAVE
                ')
                ASM_ADDR(%0111000100100000) ; BLOAD
                ASM_ADDR(%0111000100100000) ; BRUN
                ASM_ADDR(%0111000000100000) ; VERIFY




                                ; =================================
                                ; OPTION CHARACTER SYMBOL TABLE.
                                ; ($A941 - $A94A)
                                ; THESE CHARACTERS ARE FREQUENTLY
                                ; CHANGED AS PART OF A PROTECTION
                                ; SCHEME.
                                ; =================================

OPTNTXT         HIASCII(`VDSLRBACIO')


                                ; =================================
                                ; OPTIONS ISSUED TABLE
                                ; ($A94B - $A954)
                                ; A SET BIT IN THE FOLLOWING BIT
                                ; POSITIONS (OF CUMLOPTN) DENOTE
                                ; THE OPTIONS THAT WERE ISSUED
                                ; WITH THE COMMAND.
                                ; =================================

OPTNISSD        ASM_DATA(%01000000)          ; V(OLUME) PARAMETER.
                ASM_DATA(%00100000)          ; D(RIVE) PARAMETER.
                ASM_DATA(%00010000)          ; S(LOT) PARAMETER.
                ASM_DATA(%00001000)          ; L(ENGTH) PARAMETER.
                ASM_DATA(%00000100)          ; R(ECORD # OR R(EL FIELD POSN).
                ASM_DATA(%00000010)          ; B(YTE) PARAMETER.
                ASM_DATA(%00000001)          ; A(DDRESS) PARAMETER.
                ASM_DATA(%11000000)          ; C(OMMAND).
                ASM_DATA(%10100000)          ; I(NPUT).
                ASM_DATA(%10010000)          ; O(UTPUT).

                                ; =================================
                                ; TABLE OF VALID RANGES ASSOCIATED
                                ; WITH EACH OPTION CHARACTER.
                                ; ($A955 - $A970)
                                ; (THESE VALUES ARE FREQUENTLY
                                ; CHANGED IN AN EFFORT TO
                                ; "ENHANCE" DOS OR IMPLEMENT
                                ; A PROTECTION SCHEME.  FOR
                                ; EXAMPLE, THE UPPER RANGE ASSOCIATED
                                ; WITH THE L-PARAMETER IS OFTEN
                                ; CHANGED TO ALLOW DOS TO HANDLE
                                ; LARGER FILES.
                                ; =================================

OPTNRNG
                ASM_DATA_W(0,$100-2)     ; ($A955 - $A958)  V: (0 - 254).
                ASM_DATA_W(1,2)          ; ($A959 - $A95C)  D: (1 - 2).
                ASM_DATA_W(1,$8-1)       ; ($A95D - $A960)  S: (1 - 7).
                ASM_DATA_W(1,$8000-1)    ; ($A961 - $A964)  L: (1 - 32767).
                ASM_DATA_W(0,$8000-1)    ; ($A965 - $A968)  R: (0 - 32767).
                ASM_DATA_W(0,$8000-1)    ; ($A969 - $A96C)  B: (0 - 32767).
                ifelse(eval(VERSION < 320),1,`
                ASM_DATA_W(0,$C000)      ; ($A96D - $A970)  A: (0 - 49152).
                ',`
                ASM_DATA_W(0,$10000-1)   ; ($A96D - $A970)  A: (0 - 65535).
                ')


                                ; ==================================
                                ; TEXT TABLE OF DOS ERROR MESSAGES.
                                ; ($A971 - $AA3E)
                                ; NOTE THAT ONLY THE LAST CHARACTER
                                ; OF EACH TEXT MESSAGE IS WRITTEN
                                ; IN NEGATIVE ASCII FORM.  (THESE
                                ; MESSAGES ARE FREQUENTLY ALTERED
                                ; IN COMMERCIAL PROGRAMS.)
                                ; ==================================

ERRTXTBL
ERR00           ASM_DATA($0D,$07)
                ifelse(eval(VERSION < 320),1,`
                LHASCII(`***DISK: ')
                ',`
                ASM_DATA($8D)
                ')


                ifelse(eval(VERSION < 320),1,`
ERR01
ERR02
ERR03
                LHASCII(`SYS')
                ',`
ERR01           LHASCII(`LANGUAGE NOT AVAILABLE')
ERR02
ERR03           LHASCII(`RANGE ERROR')
                ')

ERR04
                ifelse(eval(VERSION < 320),1,`
                LHASCII(`WRITE PROTECT')
                ',`
                LHASCII(`WRITE PROTECTED')
                ')


ERR05           LHASCII(`END OF DATA')
ERR06           LHASCII(`FILE NOT FOUND')
ERR07           LHASCII(`VOLUME MISMATCH')

ERR08
                ifelse(eval(VERSION < 320),1,`
                LHASCII(`DISK I/O')
                ',`
                LHASCII(`I/O ERROR')
                ')

ERR09           LHASCII(`DISK FULL')
ERR10           LHASCII(`FILE LOCKED')

                ifelse(eval(VERSION < 320),1,`
ERR11           LHASCII(`CMD SYNTAX')
ERR12           LHASCII(`NO FILE BUFFS AVAIL')
ERR13           LHASCII(`NOT BASIC PROGRAM')
                ',`
ERR11           LHASCII(`SYNTAX ERROR')
ERR12           LHASCII(`NO BUFFERS AVAILABLE')
ERR13           LHASCII(`FILE TYPE MISMATCH')
                ')

ERR14           LHASCII(`PROGRAM TOO LARGE')

                ifelse(eval(VERSION < 320),1,`
ERR15           LHASCII(`NOT BINARY FILE')
ERR16           LOASCII(` ERROR')
                ',`
ERR15           LHASCII(`NOT DIRECT COMMAND')
                ')

                ASM_DATA($8D)


                                ; =================================
                                ; TABLE OF OFFSETS TO THE ERROR
                                ; MSG TEXT TABLE ($AA3F - $AA4E).
                                ; (HACKERS OFTEN SWAP THESE BYTES
                                ; AROUND SO ERRONEOUS ERROR MSGS
                                ; WILL BE USED.)
                                ; =================================

OFF2ERR         ASM_DATA(ERR00-ERRTXTBL)
                ASM_DATA(ERR01-ERRTXTBL)
                ASM_DATA(ERR02-ERRTXTBL)
                ASM_DATA(ERR03-ERRTXTBL)
                ASM_DATA(ERR04-ERRTXTBL)
                ASM_DATA(ERR05-ERRTXTBL)
                ASM_DATA(ERR06-ERRTXTBL)
                ASM_DATA(ERR07-ERRTXTBL)
                ASM_DATA(ERR08-ERRTXTBL)
                ASM_DATA(ERR09-ERRTXTBL)
                ASM_DATA(ERR10-ERRTXTBL)
                ASM_DATA(ERR11-ERRTXTBL)
                ASM_DATA(ERR12-ERRTXTBL)
                ASM_DATA(ERR13-ERRTXTBL)
                ASM_DATA(ERR14-ERRTXTBL)
                ASM_DATA(ERR15-ERRTXTBL)
                ifelse(eval(VERSION < 320),1,`
                ASM_DATA(ERR16-ERRTXTBL)
                ')
                                ; =================================
                                ; DOS MAIN ROUTINE VARIABLES.
                                ; ($AA4F - $AA65)
                                ; =================================

CURFNADR        ASM_RES(2)            ; PTS TO CURRENT FILENAME BUF
                                ; (USUALLY PRIMFNBF, $AA75).
                                ; (NORMALLY LOADED FROM FNAMBUFM
                                ; $B5C3, IN FM PARM LIST).
CONDNFLG        ASM_RES(1)            ; STATUS FLAG:
                                ; $00 = WARMSTART, $01 = READ
                                ; $80 = COLDSTART, $C0 = A(RAM).
OPUTCOND        ASM_RES(1)            ; CHAR SWITCH OUTPUT CONDITION FLG
                                ; $00 = EVALUATE START OF INPUT
                                ; LINE.
                                ; $01 = GOT A DOS CTRL CHAR SO
                                ; COLLECT DOS COMMAND.
                                ; $02 = NOT A DOS CMD, SO JUST
                                ; PRT CHAR & RTN TO CALLER.
                                ; $03 = HANDLING INPUT OR GET
                                ; STATMENTS WHILE READING.
                                ; $04 = WRITING DATA TO DISK.
                                ; $05 = EVALUATE 1ST CHAR OF
                                ; DATA LINE READ FROM DSK.
                                ; $06 = IGNORE "?" PRMPT & RESET
                                ; TO CONDITION 0.
CSWTRUE         ASM_ADDR(COUT1) ; ADR OF TRUE OUTPUT HANDLER.
KSWTRUE         ASM_ADDR(KEYIN) ; ADR OF TRUE INPUT HANDLER.
MXFILVAL        ASM_DATA(3,3)       ; CURRENT # OF DOS BUFS IN CHAIN
                                ; (SECOND BYTE IS IRRELEVANT).
STKSAVED        ASM_RES(1)          ; ($AA59) 1ST STACK PTR SAV AREA.
                                ; (P.S.  DONT CONFUSE WITH THE
                                ; 2ND STK POINTER SAVE AREA KNOWN
                                ; AS "STKSAV" ($B39B).
XSAVED          ASM_RES(1)          ; (X) SAVE AREA.
YSAVED          ASM_RES(1)          ; (Y) SAVE ARE.
ASAVED          ASM_RES(1)          ; (A) SAVE AREA.
NDX2INBF        ASM_RES(1)          ; INDEX TO CMD LINE IN INPUT BUF.
CIOCUMUL        ASM_RES(1)          ; MON/NOMON FLAG.
                                ; CUMMULATIVE UPDATED RECORD OF
                                ; C/I/O ARGUMENTS:
                                ; C=$40, I=$20, O=$10, IO=$30,
                                ; CO=$50, CI=$60, CIO=$70.
NDX2CMD                         ; INDEX TO COMMAND.
                ifelse(eval(VERSION < 320),1,`
                ASM_DATA($20)      ; NEEDED FOR MASTER.CREATE TO WORK
                ',`
                ASM_RES(1)
                ')

LENADRBF        ASM_RES(2)          ; 2-BYTE BUF USED TO HOLD BLOAD ADR
                                ; & LENGTH READ FROM DISK.
                                ; (LEFT WITH LNGTH OF LAST BLOAD.)
NEXTCMD         ASM_RES(1)          ; CODE FOR PENDING COMMAND.
TEMPBYT         ASM_RES(1)          ; TEMPORARY STORAGE AREA.
NDX2OPTN        ASM_RES(1)          ; INDEX TO OPTION (USED TO INDEX
                                ; OPTNTXT, OPTNISSD & OPTNRNG).

CUMLOPTN        ASM_RES(1)          ; HOLDS CUMMULTIVE RECORD OF
                                ; OPTIONS PARSED ON CMD LINE.

                                ; =================================
                                ; OPTION PARSED VALUES TABLE.
                                ; ($AA66 - $AA74)
                                ; (CONTAINS A RECORD OF THE OPTION
                                ; VALUES THAT WERE ISSUED WITH THE
                                ; COMMAND.  SOME OF THESE BYTES
                                ; ARE DEFAULTED TO NON-ZERO VALS.)
                                ; =================================
                ifelse(eval(VERSION < 330),1,`
VOLPRSD
                ASM_RES(1)          ; PARSED VOLUME NUMBER.
                ASM_RES(1)
DRVPRSD         ASM_RES(1)          ; PARSED DRIVE NUMBER.
                ASM_RES(1)
SLOTPRSD        ASM_RES(1)          ; PARSED SLOT NUMBER.
                ASM_RES(1)
VOLPRSD2
LENPRSD         ASM_RES(1)          ; PARSED LENGTH VALUE.
                ASM_RES(1)
RECPRSD         ASM_RES(1)          ; PARSED RECORD OR RELATIVE FIELD
                                ; POSITION NUMBER.
                ASM_RES(1)
BYTPRSD         ASM_RES(1)
                ASM_RES(1)
ADRPRSD         ASM_RES(1)          ; PARSED ADDRESS PARAMETER.
                ASM_RES(1)
MONPRSD         ASM_RES(1)          ; PARSED MON/NOMON CHR CODE VALS.
                ',`


VOLPRSD         ASM_RES(2)          ; PARSED VOLUME NUMBER.
DRVPRSD         ASM_RES(2)          ; PARSED DRIVE NUMBER.
SLOTPRSD        ASM_RES(2)          ; PARSED SLOT NUMBER.
LENPRSD         ASM_RES(2)          ; PARSED LENGTH VALUE.
RECPRSD         ASM_RES(2)          ; PARSED RECORD OR RELATIVE FIELD
                                ; POSITION NUMBER.
BYTPRSD         ASM_RES(2)          ; PARSED BYTE VALUE.
ADRPRSD         ASM_RES(2)          ; PARSED ADDRESS PARAMETER.
MONPRSD         ASM_RES(1)          ; PARSED MON/NOMON CHR CODE VALS.
                ')


                                ; ==================================
                                ; NON-CHAIN FILE NAME BUFFERS.
                                ; (PS.  DONT CONFUSE WITH THE
                                ; VARIOUS FILENAME BUFS ASSOC
                                ; WITH THE CHAIN OF DOS BUFFERS.)
                                ; ==================================

                                ; PRIMARY FILE NAME BUFFER.

PRIMFNBF        ASM_RES(30)         ; ($AA75 - $AA92)

                                ; SECONDARY FILE NAME BUFFER.

SCNDFNBF        ASM_RES(30)         ; ($AA93 - $AAB0)


                                ; ====================================
                                ; MAIN ROUTINE CONSTANTS & VARIABLES.
                                ; ($AAB1 - $AAB7)
                                ; ====================================

MAXDFLT         ASM_DATA(3)            ; DEFAULT VALUE FOR # OF DOS BUFS. (mosher: was .RES 1)
                                ; (OFTEN ALTERED N COMCIAL PRGMS)
DCTRLCHR        ASM_DATA($84)          ; DOSS CTRL CHAR:
                                ; NORMALLY = CTRL-D, $84.
                                ; (OFTEN CHNGD IN COMCIAL PRGMS)
EXECFLAG        ASM_RES(1)            ; EXEC FLAG: $00 = NOT EXECING,
                                ; (ELSE CONTAINS 1ST CHAR OF NAME
                                ; OF EXEC FILE).
EXECBUFF        ASM_RES(2)            ; PTS TO EXEC FILES BUFFER.

ACTBSFLG                        ; ACTIVE BASIC FLAG (INT=$00,
                                ; A(ROM)=$40, A(RAM)=$80).
                ifelse(eval(VERSION >= 320),1,`
                ASM_RES(1)
                ')

RUNTRUPT        ASM_RES(1)            ; RUN INTERCEPTED FLAG:
                                ; $00 = RUN NOT INTECEPTED.
                                ; NON-ZERO = RUN INTERCEPTED TO DO
                                ; A LOAD.


                                ; =================================
                                ; TEXT OF THE WORD "APPLESOFT".
                                ; ($AAB8 - $AAC0)
                                ; =================================

APLSFTXT        HIASCII(`APPLESOFT')
