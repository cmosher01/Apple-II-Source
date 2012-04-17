include(`asm.m4h')
include(`symbols.m4h')

PADDL0   =     $C064
PTRIG    =     $C070

;-----------------------------------------------------------------------
;
; PADDLES
;
; Handles the paddles.
;
;-----------------------------------------------------------------------





PREAD    LDA   PTRIG      ;TRIGGER PADDLES
         LDY   #$00       ;INIT COUNT
         NOP              ;COMPENSATE FOR 1ST COUNT
         NOP
PREAD2   LDA   PADDL0,X   ;COUNT Y-REG EVERY
         BPL   RTS2D      ;  12 USEC
         INY
         BNE   PREAD2     ;  EXIT AT 255 MAX
         DEY
RTS2D    RTS
