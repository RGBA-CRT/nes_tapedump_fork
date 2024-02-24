nesasm -S -raw TapeDump_RAM.asm >debug_RAM.txt
bincut2 -o RAM.bin -s 140 -l 2C0 TAPEDUMP_RAM.NES
nesasm -S TapeDump.asm >debug.txt
del TAPEDUMP_RAM.NES
del RAM.bin