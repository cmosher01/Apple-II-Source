include(`asm.m4h')

X1        =    $F8
M1        =    $F9

FIX1      JSR  RTAR
FIX       LDA  X1
          BPL  UNDFL
          CMP  #$8E
          BNE  FIX1
          BIT  M1
          BPL  FIXRTS
          LDA  M1+2
          BEQ  FIXRTS
          INC  M1+1
          BNE  FIXRTS
          INC  M1
FIXRTS    RTS
UNDFL     LDA  #$0
          STA  M1
          STA  M1+1
          RTS

UF65E     ASM_RES(8,$FF)
