; SUBROUTINE IN APPLE ][ ROM @ $F669
;

ASTORE	=	$56
XSTORE	=	$57
YSTORE	=	$58
PSTORE	=	$59

STACK	=	$0100

EF669:
;
; Get low nibble of caller's page.
; For example, if called from code in $C3xx,
; return with $30 in A
;

                                ; save A,X,Y,P
        STY     YSTORE
        STX     XSTORE
        STA     ASTORE
        PHP
        PLA
        STA     PSTORE

        TSX
        INX
        INX
        LDA     STACK,X         ; get page we were called from

        ASL
        ASL
        ASL
        ASL                     ; shift low nibble to high nibble

        RTS



EF67E:
                                ; restore A,X,Y,P
        LDY     YSTORE
        LDX     XSTORE
        LDA     PSTORE
        PHA
        LDA     ASTORE
        PLP

        RTS
