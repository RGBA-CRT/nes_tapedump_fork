ffmpeg -i %1 -ar 22050 -ac 1 -acodec pcm_u8 %1_u8.wav
kcs_win64.exe -b2 -F10 -G2 %1_u8.wav %1.bin