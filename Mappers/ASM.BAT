
TASM_x64 -65 -x %1.asm
ren %1.obj %1.int
FFC_x64 %1.int %1.bin
rem move %1.bin ..\plugdone\
del %1.int