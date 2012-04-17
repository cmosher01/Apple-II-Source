include(`asm.m4h')
                ifelse(eval(VERSION == 13),1,`
SECYNIBS        =     $9A
GRP             =     $33
ADDRSIGNATURE   =     $B5
                ',`
SECYNIBS        =     $56
ADDRSIGNATURE   =     $96
                ')




BUFPTR          =     $26
SLOT            =     $2B
TMP             =     $3C
SECTOR          =     $3D
TRACK           =     $41

                ifelse(eval(VERSION == 13),1,`
A3L             =     $2A
                ',`
A3L             =     $40
                ')

STACKBASE       =     $0100
READBUF         =     $0300
                ifelse(eval(VERSION == 13),1,`
XLATE           =     $0880
                ',`
XLATE           =     READBUF+SECYNIBS
BOOT1PAGE       =     $08
BOOT2BASE       =     BOOT1PAGE << 8
                ')

SWITCHBASE      =     $C080
STEPOFFBASE     =     SWITCHBASE+$0
STEPONBASE      =     STEPOFFBASE+$1
MOTORON         =     SWITCHBASE+$9
DRV0EN          =     SWITCHBASE+$A
Q6OFF           =     SWITCHBASE+$C
Q7OFF           =     SWITCHBASE+$E

WAIT            =     $FCA8
PRERR           =     $FF2D
MONRTS          =     $FF58



                                        ; build translation table
DISK2           LDX   #$20
                LDY   #0

                ifelse(eval(VERSION == 13),1,`

LC604           LDA   #$03
                STA   TMP
                CLC
                DEY
                TYA
LC60B           BIT   TMP
                BEQ   LC604
                ROL   TMP
                BCC   LC60B
                CPY   #$D5              ; $D5 is reserved to indicate header
                BEQ   LC604
                DEX
                TXA
                STA   XLATE-$80,Y
                BNE   LC604

                ',`

LC604           LDX   #$03
LC606           STX   TMP
                TXA
                ASL
                BIT   TMP
                BEQ   LC61E
                ORA   TMP
                EOR   #%11111111
                AND   #%01111110
LC614           BCS   LC61E
                LSR
                BNE   LC614
                TYA
                STA   XLATE,X
                INY
LC61E           INX
                BPL   LC606

                ')





                JSR   MONRTS            ; calculate slot based on ROM code address
                TSX
                LDA   STACKBASE,X
                ifelse(eval(VERSION == 13),1,`
                PHA
                ')
                ASL
                ASL
                ASL
                ASL
                STA   SLOT



                TAX
                ifelse(eval(VERSION == 13),1,`
                LDA   #<(DENIB-1)
                PHA
                ')
                LDA   Q7OFF,X           ; set drive to "read" mode
                LDA   Q6OFF,X
                LDA   DRV0EN,X          ; engage drive 0
                LDA   MOTORON,X         ; turn drive motor on





                LDY   #$50              ; move arm to track 0
LC63D           LDA   STEPOFFBASE,X
                TYA
                AND   #%00000011
                ASL
                ORA   SLOT
                TAX
                LDA   STEPONBASE,X
                LDA   #$56
                ifdef(`NODELAY',`
                LDA   #0
                NOP
                ',`
                JSR   WAIT
                ')
                DEY
                BPL   LC63D




                                        ; set params for READSECT
                ifelse(eval(VERSION == 13),1,`
                LDA   #>READBUF
                STA   BUFPTR+1
                LDA   #<READBUF
                STA   BUFPTR
                STA   SECTOR
                ',`
                STA   BUFPTR
                STA   SECTOR            ; SECTOR 0
                STA   TRACK             ; TRACK 0
                LDA   #BOOT1PAGE
                STA   BUFPTR+1          ; buffer $800
                ')


                                        ; read SECTOR to (BUFPTR) buffer
READSECT        CLC
READSECT2       PHP

                                        ; look for D5 AA 96 (address) or D5 AA AD (data)
LC65E           LDA   Q6OFF,X
                BPL   LC65E
LC663           EOR   #$D5
                BNE   LC65E
LC667           LDA   Q6OFF,X
                BPL   LC667
                CMP   #$AA
                BNE   LC663
                NOP
LC671           LDA   Q6OFF,X
                BPL   LC671
                CMP   #ADDRSIGNATURE
                BEQ   ADDRHANDLER
                PLP
                BCC   READSECT
                EOR   #$AD
                BEQ   DATAHANDLER
                BNE   READSECT




                                        ; get T/S number from address header
ADDRHANDLER     LDY   #3
                ifelse(eval(VERSION == 13),1,`
                STY   A3L
                ',`
LC685           STA   A3L
                ')
LC687           LDA   Q6OFF,X
                BPL   LC687
                ROL
                STA   TMP
LC68F           LDA   Q6OFF,X
                BPL   LC68F
                AND   TMP
                DEY
                ifelse(eval(VERSION == 13),1,`
                BNE   LC687
                ',`
                BNE   LC685
                ')
                PLP
                CMP   SECTOR
                BNE   READSECT          ; branch if not the sector were looking for
                ifelse(eval(VERSION > 13),1,`
                LDA   A3L
                CMP   TRACK
                BNE   READSECT          ; branch if not the track were looking for
                ')
                BCS   READSECT2





DATAHANDLER     LDY   #SECYNIBS         ; read $56 nibblized bytes to $300
LC6A8           STY   TMP
LC6AA           LDY   Q6OFF,X
                BPL   LC6AA
                EOR   XLATE-$80,Y
                LDY   TMP
                DEY
                ifelse(eval(VERSION == 13),1,`
                STA   XLATE-$80,Y
                ',`
                STA   READBUF,Y
                ')
                BNE   LC6A8

LC6BA           STY   TMP               ; read $100 nibblized bytes to (BUFPTR)
LC6BC           LDY   Q6OFF,X
                BPL   LC6BC
                EOR   XLATE-$80,Y
                LDY   TMP
                STA   (BUFPTR),Y
                INY
                BNE   LC6BA

LC6CB           LDY   Q6OFF,X
                BPL   LC6CB
                EOR   XLATE-$80,Y
LC6D3           BNE   READSECT          ; branch if checksum is wrong

                ifelse(eval(VERSION == 13),1,`

                RTS

DENIB           TAY
LC6D2           LDX   #0
LC6D4           LDA   XLATE-$80,Y
                LSR
                ROL   READBUF+4*GRP,X
                LSR
                ROL   READBUF+3*GRP,X
                STA   TMP
                LDA   (BUFPTR),Y
                ASL
                ASL
                ASL
                ORA   TMP
                STA   (BUFPTR),Y
                INY
                INX
                CPX   #GRP
                BNE   LC6D4
                DEC   A3L
                BNE   LC6D2
                CPY   READBUF
                BNE   ERROR
                JMP   READBUF+1
ERROR           JMP   PRERR

                ASM_DATA($FF)

                ',`

                                        ; denibblize into (BUFPTR)
                LDY   #0
LC6D7           LDX   #SECYNIBS

LC6D9           DEX
                BMI   LC6D7
                LDA   (BUFPTR),Y
                LSR   READBUF,X
                ROL
                LSR   READBUF,X
                ROL
                STA   (BUFPTR),Y
                INY
                BNE   LC6D9
                INC   BUFPTR+1
                INC   SECTOR
                LDA   SECTOR
                CMP   BOOT2BASE
                LDX   SLOT
                BCC   LC6D3
                JMP   BOOT2BASE+1

                ')
