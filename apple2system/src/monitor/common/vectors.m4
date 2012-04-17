include(`asm.m4h')
include(`symbols.m4h')


M6502VEC ASM_ADDR(NMI)        ;NMI VECTOR

         ASM_ADDR(RESET`'ifelse(eval(VERSION `>= 2'),1,`2')) ;RESET VECTOR

         ASM_ADDR(IRQ)        ;IRQ VECTOR
