
tasm -65 -x %1.asm
ren %1.obj %1.int
ffc %1.int %1.bin
rem move %1.bin ..\plugdone\
del %1.int