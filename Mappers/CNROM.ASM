             .title        "CNROM Reader Plugin"

;06/10/00 ;Written by KH
;07/22/06 ;BootGod ;added open bus check for first bank as well
;04/28/07 ;BootGod ;added user var for diode setting
;07/30/07 ;BootGod ;Complete rewrite, 8K CHR plugin merged into this one, 
;CHR enable user val not needed anymore, fixes under-dumping of games like Mighty Bomb Jack (USA)
;09/14/09 ;BootGod ;Runs write bank routine before checking mirroring to ensure diode value
;is written in first.
;10/25/09 ;BootGod ;Added a special block type to return aux data, in this case, it returns
;1 byte containing the diode code used and more importantly, the code to enable an 8K CHR ROM.

;             ;plugin version and flags
;             .db p_ver
;             .db p_read | p_var1
             
             ;vectors for standard system calls
inesHDR      .equ 0060h	;$60-$6F is the iNES header for sending
inesPRG	     .equ inesHDR+4	;Num of 16K banks in iNES header
inesCHR	     .equ inesHDR+5	;Num of 8K banks in iNES header
send_hdr:    .equ 01FAh        ;Send iNES header as audio
send_ldr:    .equ 01FDh		;Send 5 sec tone (for CopyNES to use)

send_byte:   .equ 0200h
;baton:       .equ 0203h
;chk_vram:    .equ 0206h
;chk_wram:    .equ 0209h
wr_ppu:      .equ 020ch
;read_byte:   .equ 020fh
init_crc:    .equ 0212h
do_crc:      .equ 0215h
finish_crc:  .equ 0218h

crc0:        .equ 0080h
crc1:        .equ 0081h
crc2:        .equ 0082h
crc3:        .equ 0083h

crcs:        .equ 00d0h

addr1:       .equ 00e0h
addr1_lo:    .equ 00e0h
addr1_hi:    .equ 00e1h
addr2:       .equ 00e2h
addr2_lo:    .equ 00e2h
addr2_hi:    .equ 00e3h

numbanks:    .equ 00e4h
curbank:     .equ 00e5h
reg:         .equ 00e6h
goodbanks:   .equ 00e7h
banksleft:   .equ 00e8h
is8kchr:     .equ 00e9h

x_temp:      .equ 00eah
y_temp:      .equ 00ebh
prg_siz:     .equ 00ech
;chr_siz:     .equ 00edh

uv1enabled:  .equ 07ceh
diodeval:    .equ 07cfh


;plugin header data

             .org 0400h

;plugin code start here
	     jmp	all_dump	;All dumping start
	     jmp	hdr_dump	;send iNES header
	     jmp	prg_dump        ;send PRG only
	     jmp	chr_dump	;send CHR only
	     jmp	mpr_end		;wram?		;No WRAM!
	     jmp	mpr_end		;set PRG bank
	     jmp	mpr_end		;set CHR bank
	     jmp	mpr_end		;send CRCs???


all_dump:    ;Send ALL data!  (Fairly short for NROM)
	     jsr hdr_dump	;Determine header & send it!
	     jsr Send_PRG
             jsr Send_CHR
mpr_end:     rts            ;done

hdr_dump:    ;Determine info in the header and dump it.
	     jsr Chk_Mirroring   ;These three are needed for header info
	     jsr Chk_PRG_size    ;"	"	"
	     jsr Chk_CHR_size
	     jsr send_ldr	;5-second minimum lead-in
	     jsr send_hdr	;Send iNES header
	rts

prg_dump:    ;Send the PRG data only.
	     jsr Chk_PRG_size    ;"	"	"
	     jsr send_ldr	;5-second minimum lead-in
	     jsr Send_PRG
	     rts

chr_dump:    ;Send the CHR data only.
	     jsr Chk_CHR_size    ;"	"	"
	     jsr send_ldr	;5-second minimum lead-in
	     jsr Send_CHR
	     rts
;------------------------
	     
	     

Chk_Mirroring:
             lda #0
             sta is8kchr
             jsr wr_bank	;bank write is to make sure diode value is written before checking mirroring
             
;check mirroring (x = mirroring)
             ldx #01h		;Horiz.
             lda #020h
             jsr wr_ppu
             lda #055h
             sta 2007h
             lda #0aah
             sta 2007h
             lda #024h
             jsr wr_ppu
             lda 2007h
             lda 2007h
             cmp #055h
             bne got_mir
             lda 2007h
             cmp #0aah
             bne got_mir
             lda #020h
             jsr wr_ppu
             lda #0aah
             sta 2007h
             lda #024h
             jsr wr_ppu
             lda 2007h
             lda 2007h
             cmp #0aah
             bne got_mir
             dex		;Vertical
got_mir:     txa
             ora inesHDR+6
	     sta inesHDR+6	;Save mirroring in iNES header.
             rts
;------------------------

Chk_PRG_size:
;determine PRG ROM size (16K or 32K)
             lda #000h
             sta addr1_lo
             sta addr2_lo
             lda #080h
             sta addr1_hi
             lda #0c0h
             sta addr2_hi	;set addresses to $8000 and $C000
             
             ldx #040h
             ldy #0		;set up to check 16K

chk_prg:     lda (addr1),y
             cmp (addr2),y
             bne is32k
             iny
             bne chk_prg
             inc addr1_hi
             inc addr2_hi
             dex
             bne chk_prg

is16k:       lda #040h		;40h * 256bytes = 16K
             tax
             lda #1		;1 x 16K banks
             bne doneck_prg

is32k:       lda #080h		;80h * 256bytes = 32K
             tax
             lda #2		;2x 16k banks

doneck_prg:  sta inesPRG	;Num of PRG banks
	     stx prg_siz	;Store temporarily
	     rts
;------------------------

Send_PRG:    ldx prg_siz
	     ldy #0
             sty addr1_lo
             lda #080h
             sta addr1_hi	;set address to $8000, X = num blocks to send

dump_prg:    lda (addr1),y
             jsr send_byte
             iny
             bne dump_prg
             inc addr1_hi
             dex
             bne dump_prg
             rts
;------------------------
;------------------------

Chk_CHR_size:

		lda #0
		sta 02001h	;Turn off screen.
;determine CHR ROM size (8K or 16K or 32K)             
	      lda #2
             sta goodbanks	;clear flag byte (sets which banks should be dumped later)
             lda #0
             sta numbanks	;set num banks to 0

             lda #3
             sta curbank	;start with bank 4 and work down

             lda #4
             sta banksleft

chk_chr:     lda curbank
             jsr wr_bank	;set bank
             jsr crc_bank	;get crc
             beq is_ob		;if open bus, jump below

             ora goodbanks
             sta goodbanks	;flag as good bank

             lda curbank
             asl a
             asl a
             tay		;init index for bank# CRC
             ldx #3		;init index for reading last crc

save_crc:    lda crc0,x		;save the crc
             sta crcs,y
             iny
             dex
             bpl save_crc 	;was bne!!!

             inc numbanks	;inc num good banks

is_ob:       asl goodbanks	;shift flags over 1
             dec curbank
             dec banksleft
             bne chk_chr	;continue to next bank

             lda numbanks	;now that we've checked all 4 banks, if all 4 banks are good,
             cmp #4             ;we need to make sure it's not actually 16K mirrored
             bne chr_ready

             ldx #07h
             ldy #0fh

check_crcs:  lda crcs,x
             cmp crcs,y
             bne chr_ready
             dey
             dex
             bpl check_crcs	;was bne

             lda #6		;banks 0-1 matched 2-3, so fix vars (6 = 110b which will be 11b after corrective shift)
             sta goodbanks
             lda #2
             sta numbanks

chr_ready:   lda numbanks	;convert numbanks to num pages
             cmp #1		;check if is 8K CHR
             bne chr_shift
             sta is8kchr	;it is, so set flag to send special block later
chr_shift:   ;A should contain 1, 2, or 4 now...?
	     sta inesCHR	;save in header
	     rts
;---------------------------


Send_CHR:
;		jsr send_ldr	;5-second minimum lead-in

             lsr goodbanks
             lda #0
             sta curbank

next_chr:    lda curbank
             jsr wr_bank	;set bank
             inc curbank

             lsr goodbanks	;check if good bank
             bcc next_chr

             lda #0
             jsr wr_ppu		;set to PPU $0000
             lda 2007h

             ldx #32		;setup for 8K dump
             ldy #0

send_chr:    lda 2007h
             jsr send_byte
             iny
             bne send_chr
             dex
             bne send_chr
             dec numbanks
             bne next_chr
done:        rts		;return to BIOS
;------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------





;sub for setting CHR bank (compensates for bus conflicts)
wr_bank:     stx x_temp
             sty y_temp

             ldx uv1enabled	;check if user has enter value for D4/D5
             cpx #0ffh
             beq no_diodes
             ora diodeval	;if so, OR it with bank num

no_diodes:   sta reg		;store value to be written to reg

             lda #0
             sta addr1_lo
             lda #080h
             sta addr1_hi	;set address to $8000

             ldx #080h		;setup to search all 32K of PRG
             ldy #0

wrb_loop:    lda (addr1),y
             and #033h		;mask it to just the bits we need to match
             cmp reg		;check if current byte matches what we want to write
             beq found_reg
             iny
             bne wrb_loop
             inc addr1_hi
             bne wrb_loop

no_reg:      lda reg		;didn't find place to write, try it anyways
             sta 08000h
             jmp wrb_return

found_reg:   lda (addr1),y	;found value equal to reg, write it.
             sta (addr1),y

wrb_return:  ldx x_temp
             ldy y_temp

             rts

;sub for doing CRC on 8K CHR at $0000, returns 0 if was open bus, 1 if not
crc_bank:    lda #0
             jsr wr_ppu		;set to PPU $0000

             jsr init_crc
             lda 2007h

             ldx #020h		;checking 8K
             ldy #0

crc_loop:    lda 2007h
             stx x_temp
             jsr do_crc
             ldx x_temp
             iny
             bne crc_loop
             dex
             bne crc_loop

;             ldx #3		;check if open bus type 1
;ob1_check:   lda crc0,x
;             cmp openbus1,x
;             bne _ob2check
;             dex             ;This does only 3 loops!!
;             bne ob1_check
              lda crc0
              cmp #0
              bne crc_pass
              lda crc1
              cmp #0F0h		;$(F)F000 is the checksum for CHR open bus (?)
              bne crc_pass

is_ob1:      lda #0
             rts

;_ob2check:  ; ldx #3		;check if open bus type 2
;ob2_check:   lda crc0,x
;             cmp openbus2,x
;             bne crc_pass
;             dex
;             bne ob2_check
;
;is_ob2:      lda #0
;             rts
;
crc_pass:    lda #1
             rts

;openbus1:    .db 007h,053h,067h,0b6h
;openbus2:    .db 01ah,0c9h,05ch,078h

;             .fill 07c0h-*,0ffh   ;fill rest to get 1K of data
;
;             .db "Diode Code"
;             .fill 07ceh-*,0
;             .fill 07d0h-*,0ffh
;
;             .fill 0800h-*,0ffh
             .end
