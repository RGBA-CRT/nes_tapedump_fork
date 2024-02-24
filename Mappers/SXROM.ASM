             .title        "SxROM Reader Plugin"

;             .db "Nintendo SxROM / MMC1 carts"

;06/15/00 
;Written by KH
;Version 1.0
             
;07/20/06
;BootGod
;Added support for user vars to override PRG / CHR size detection. Code does not check for "valid" values, up to user to do it right.
;Sizes are in terms of 256 byte pages.
;lo | hi | size
;--------------
;20 | 00 |   8K
;40 | 00 |  16K
;80 | 00 |  32K
;00 | 01 |  64K
;00 | 02 | 128K
;00 | 04 | 256K
;00 | 08 | 512K

;01/23/09
;Kinopio
;Cleaned up the code and added support for SXROM and SUROM.
;This plugin should now handle all known S*ROM carts except for SFEXPROM.

;05/10/09
;BootGod
;Added support for 8K CHR. Seta thought they'd be funny and stick a CHR ROM in a SNROM board :/
;Was dumpable via user vars for CHR override, but auto-detection is always better!

;             ;PRG/CHR/WRAM min/max sizes supported
 ;            .db 32 >> 4,512 >> 4
  ;           .db 8 >> 3,128 >> 3
   ;          .db 8,32

             ;vectors for standard system calls
inesHDR      .equ 0060h	;$60-$6F is the iNES header for sending
inesPRG	     .equ inesHDR+4	;Num of 16K banks in iNES header
inesCHR	     .equ inesHDR+5	;Num of 8K banks in iNES header
send_hdr:    .equ 01FAh        ;Send iNES header as audio
send_ldr:    .equ 01FDh		;Send 5 sec tone (for CopyNES to use)

send_byte:   .equ 0200h
;baton:       .equ 0203h
;chk_vram:    .equ 0206h
dm_wram:    .equ 0209h  	;Dump WRAM in the RAM code!
wr_ppu:      .equ 020ch
;read_byte:   .equ 020fh
init_crc:    .equ 0212h
do_crc:      .equ 0215h
;finish_crc:  .equ 0218h

crc0:        .equ 0080h
crc1:        .equ 0081h
crc2:        .equ 0082h
crc3:        .equ 0083h

temp1_lo:    .equ 0088h
temp1_hi:    .equ 0089h

cbanks_left:  .equ 008ah

readaddr:    .equ 00e0h
readaddr_lo: .equ 00e0h
readaddr_hi: .equ 00e1h

banks_left:  .equ 00e2h
cur_bank:    .equ 00e3h

wram0:       .equ 00e4h
wram1:       .equ 00e5h

tempvar:     .equ 00e6h
temp_crc:    .equ 00e7h

uv1enabled:   .equ 07ceh
poverride_lo: .equ 07cfh
uv2enabled:   .equ 07deh
poverride_hi: .equ 07dfh
uv3enabled:   .equ 07eeh
coverride_lo: .equ 07efh
uv4enabled:   .equ 07feh
coverride_hi: .equ 07ffh


             .org 0400h
	     jmp	all_dump	;All dumping start
	     jmp	hdr_dump	;send iNES header
	     jmp	prg_dump        ;send PRG only
	     jmp	chr_dump	;send CHR only
	     jmp	SRAM_Check      ;Check for SRAM
	     jmp	mpr_end		;set PRG bank
	     jmp	mpr_end		;set CHR bank
	     jmp	mpr_end		;send CRCs???

all_dump:    ;Send ALL data!
	     jsr hdr_dump	;Determine header & send it!
	     jsr Send_PRG
             jsr Send_CHR
mpr_end:     rts            ;done

hdr_dump:    ;Determine info in the header and dump it.
	     jsr Reset_MMC1
	     jsr Chk_Mirroring   ;These three are needed for header info
	     jsr Detect_CHR_Size ;"	"	"
	     jsr Detect_PRG_Size ;"	"	"
	     jsr send_ldr	;5-second minimum lead-in
	     jsr send_hdr	;Send iNES header
	rts
	
prg_dump:    ;Send the PRG data only.
	     jsr Reset_MMC1
	     jsr Detect_PRG_Size    ;"	"	"
             jsr send_ldr
	     jsr Send_PRG
	     rts
	     
chr_dump:    ;Send CHR data only.
	     jsr Reset_MMC1
 	     jsr Detect_CHR_Size
             jsr Send_CHR
 	     rts
 	     
SRAM_Check:  ;Check presence of SRAM and dump it
	        jsr Reset_MMC1
	        jsr Detect_WRAM
	        lda inesHDR+6
                and #2		;Return presence for our WRAM dumper
                beq	no_dump_sram
;---- dump it, but try to protect the SRAM, okay?
        	lda #00h
                jsr wr_mmc3       ;turn RAM enable on (note: MMC1B/C only!!)
                lda #00h
                jsr wr_mmc1       ;select first WRAM bank (SUROM, SOROM)
                jsr     dm_wram		;Dump it via my code
                lda #10h
                jsr wr_mmc3       ;turn RAM OFF (note: MMC1B/C only!!)
;---------------------------
no_dump_sram:  rts



Chk_Mirroring:
;	             lda #04h          ;mapper controlled mirroring
;             jsr send_byte     ;send mirroring info
; Mirroring is ignored for MMC1 / SXROM
	jmp	Detect_WRAM
;------------------------------------






;plugin code start here
Send_PRG:
;	jsr	Reset_MMC1

;             lda uv1enabled       	;This code jumps all over the place, so no overriding!
;             cmp #0ffh
;             bne prg_override
;-----------------------------------------------------------------------
;dump PRG
;;----------------------------- user-controlled size
;prg_override:lda poverride_lo
;             jsr send_byte
;             lda poverride_hi
;             jsr send_byte
;
;             asl poverride_hi
;             lda poverride_lo
;             rol a
;             lda #0            ;???
;             rol a
;             ora poverride_hi
;             sta banks_left    ;was tay in old code
;             jmp blktypep

;-------------------------------------------------------- continue
blktypep:;    lda #01h
         ;    jsr send_byte     ;type is PRG

             lda #00h
             sta cur_bank      ;init bank variables
             jsr wr_mmc1
             

dmpp_loop1:
;	     jsr send_ldr
	     lda #00h
             sta readaddr_lo
             tay
             lda #80h
             sta readaddr_hi   ;readaddr=$8000h

             lda cur_bank
             cmp #10h          ;time to change 256K bank? Bank 0-15 first 256K, 16-31 last 256K
             bne no_switch
             lda #10h
             jsr wr_mmc1
no_switch:   lda cur_bank
             jsr wr_mmc3       ;select bank 32K at $8000h

dmpp_loop2:  lda (readaddr),y
             jsr send_byte
             iny
             bne dmpp_loop2    ;send a byte 256 times

             inc readaddr_hi
             bne dmpp_loop2    ;repeat 128 times, $8000h->$FFFFh, 128*256=32768
             inc cur_bank
             inc cur_bank      ;2*16K
             dec banks_left
             bne dmpp_loop1
             
        	rts
;------------------ Send PRG end -----------------



Send_CHR:
	     lda cbanks_left
	     beq done	  	;If zero banks, don't dump

		jsr send_ldr	;Send leader again for CHR-ROM
		lda #00h
        	sta cur_bank      ;init bank variables
        	sta $2001	;Screen off!
             
dmpc_loop1:  lda cur_bank
             jsr wr_mmc1
             ldx #20h
             lda #00h
             tay
             sta 2006h
             sta 2006h
             lda 2007h

dmpc_loop2:  lda 2007h
             jsr send_byte
             iny
             bne dmpc_loop2    ;send a byte 256 times
             
             dex
             bne dmpc_loop2    ;repeat 32 times, $0000h->$1FFFh, 32*256=8192
             inc cur_bank
             inc cur_bank      ;2*4K
             dec cbanks_left
             bne dmpc_loop1

done:	     rts
;------------------ Send CHR end -----------------













;*****************************************************
 

;calculate CRC of 32K PRG bank specified by accumulator

do_prg_crc:  jsr wr_mmc3
             lda #00h
             tay
             sta readaddr_lo
             lda #80h
             sta readaddr_hi   ;readaddr=$8000h
             jsr init_crc

crcp_loop:   lda (readaddr),y
             jsr do_crc
             iny
             bne crcp_loop     ;repeat 256 times
             inc readaddr_hi
             bne crcp_loop     ;repeat 128 times, 128*256=32768
;             jsr finish_crc
             
             rts
             
;calculate CRC of 8K CHR bank specified by accumulator

do_chr_crc:  jsr wr_mmc1
             lda #00h
             tay
             sta 2006h
             sta 2006h
             lda #20h
             sta tempvar
             jsr init_crc
             lda 2007h

crcc_loop:   lda 2007h
             jsr do_crc
             iny
             bne crcc_loop     ;repeat 256 times
             dec tempvar
             bne crcc_loop     ;repeat 32 times, 32*256=8192
;             jsr finish_crc

             rts

;store current CRC in temp_crc

store_crc:   ldx #3

st_loop:     lda crc0,x
             sta temp_crc,x
             dex
             bpl st_loop
             
             rts

;compare current CRC with temp_crc, if they match return 0 else return !0
    
comp_crc:    ldx #3

co_loop:     lda crc0,x
             cmp temp_crc,x
             bne no_comp
             dex
             bpl co_loop
             lda #0

no_comp:     rts

;************************


Detect_PRG_Size:
;-------------- PRG check
             lda #00h
             jsr do_prg_crc    ;CRC PRG bank 0
             jsr store_crc     ;store CRC of first 32K PRG (bank 0+1)
             
             lda #10h          ;detect from max to min size, 512->32K
             sta banks_left    ;512K PRG. 1 bank = 32K
             
             jsr chk_vram      ;if CHR is RAM check if 512K PRG (for SUROM and SXROM boards)
             bne detp_loop1    ;CHR isn't RAM, keep checking

             lda #10h
             jsr wr_mmc1       ;try and select next 256K bank
             lda #00h
             jsr do_prg_crc    ;CRC PRG bank 0 (16)
             jsr comp_crc      ;check if CRC of PRG bank 0 changed
             bne got_prg
                          
detp_loop1:  lsr banks_left    ;halve size
             lda banks_left
             cmp #01h
             beq got_prg       ;32K
             jsr do_prg_crc    ;CRC PRG bank; 08h, 04h, 02h
             jsr comp_crc      ;compare CRC with PRG bank 0
             bne got_prg       ;if not equal then PRG size found; 256K, 128K, 64K
             jmp detp_loop1    ;keep looping until size is found
;--------------------------- automatic size
got_prg:     lda banks_left
		asl	a	;iNES header uses 1 = 16k bank; this prog uses 1 = 32k banks
		sta	inesPRG
             ;sta tempvar
             ;lda #00h
             ;lsr tempvar
             ;ror a
             ;jsr send_byte
             ;lda tempvar
             ;jsr send_byte     ;send size

	     rts

Detect_CHR_Size:
;detect CHR size
	lda	#$00
	sta $2001	;Screen off!

	jsr chk_vram      ;check if CHR is RAM
             bne no_vram  ;Jump if not RAM but CHR-ROM!
             lda #$00
             sta cbanks_left	;Saves 0
             jmp got_chr    ;nothing to dump then
             
no_vram:;     lda uv3enabled                       ;Keep it all automatic!
;             cmp #0ffh
;             bne chr_override

             lda #00h
             jsr do_chr_crc    ;CRC CHR bank 0
             jsr store_crc     ;store CRC of first 8K CHR (bank 0+1)
             
             lda #10h          ;detect from max to min size, 128->16K
             sta cbanks_left    ;128K CHR. 1 bank = 8K
             
detc_loop1:  lda cbanks_left
             cmp #01h          ;Changed from 2 to 1 (16K -> 8K) -BootGod
             beq got_chr       ;8K
             jsr do_chr_crc    ;CRC CHR bank; 10h, 08h, 04h
             jsr comp_crc      ;compare CRC with CHR bank 0
             bne got_chr       ;if not equal then CHR size found; 128K, 64K, 32K
             lsr cbanks_left    ;halve size
             jmp detc_loop1    ;keep looping until size is found
             
;dump CHR
;
;chr_override:lda coverride_lo
;             jsr send_byte
;             lda coverride_hi
;             jsr send_byte
;
;             asl coverride_hi
;             asl coverride_hi
;             asl coverride_hi
;             lda coverride_lo
;             lsr a
;             lsr a
;             lsr a
;             lsr a
;             lsr a
;             ora coverride_hi
;             sta banks_left    ;was tay in old code
;
;             jmp blktypec

got_chr:     lda cbanks_left
		sta inesCHR	;This prog is equivalent to iNES header CHR size.
;             sta tempvar
;             lda #00h
;             lsr tempvar
;             ror a
;             lsr tempvar
;             ror a
;             lsr tempvar
;             ror a
;             jsr send_byte
;             lda tempvar
;             jsr send_byte     ;send size
;blktypec:    lda #02h
;             jsr send_byte     ;type is CHR
             
cchk_end:           rts

;**********************************************

;MMC1 write register functions

Reset_MMC1:
             lda #80h
             sta 0ffffh        ;reset MMC1
             lda #00h
             jsr wr_mmc0       ;8K CHR, 32K PRG (PRG/CHR bank = 16/4K)
             jsr wr_mmc1
             rts

wr_mmc0:     sta 09fffh             
             lsr a
             sta 09fffh
             lsr a
             sta 09fffh
             lsr a
             sta 09fffh
             lsr a
             sta 09fffh
             rts
             
wr_mmc1:     pha
             jsr wr_mmc2
             pla
             sta 0bfffh
             lsr a
             sta 0bfffh
             lsr a
             sta 0bfffh
             lsr a
             sta 0bfffh
             lsr a
             sta 0bfffh
             rts
             
wr_mmc2:     sta 0dfffh
             lsr a
             sta 0dfffh
             lsr a
             sta 0dfffh
             lsr a
             sta 0dfffh
             lsr a
             sta 0dfffh
             rts
             
wr_mmc3:     sta 0ffffh             
             lsr a
             sta 0ffffh
             lsr a
             sta 0ffffh
             lsr a
             sta 0ffffh
             lsr a
             sta 0ffffh
             rts
             
             
;-------------------------
chk_vram:    lda #0
             jsr wr_ppu
             lda #055h
             sta 2007h
             lda #0aah
             sta 2007h
             lda #0
             jsr wr_ppu
             lda 2007h
             lda 2007h
             cmp #55h
             bne no_ram5
             lda 2007h
             cmp #0aah
             bne no_ram5
             lda #0
             jsr wr_ppu
             lda #0aah
             sta 2007h
             lda #0
             jsr wr_ppu
             lda 2007h
             lda 2007h
             cmp #0aah
             
no_ram5:     rts


;-------------------------

Detect_WRAM:
;detect WRAM size
	     lda #00h
             jsr wr_mmc3       ;turn RAM enable on (note: MMC1B/C only!!)
             lda #00h
             jsr wr_mmc1       ;select first WRAM bank (SUROM, SOROM)
             jsr chk_wram
             beq yes_wram
             jmp wram_done          ;if no wram then nothing more to do

yes_wram:    lda #02h	;WRAM flag
	     ora inesHDR+6	;SRAM, Mirroring flags
	     sta inesHDR+6
wram_done:
	     lda #10h
             jsr wr_mmc3       ;turn RAM OFF again! (note: MMC1B/C only!!)
		rts

;*****************************


chk_wram:    lda 6000h
             sta temp1_hi
             lda 6080h
             sta temp1_lo
             lda #055h
             sta 6000h
             eor #0ffh
             sta 6080h

             ldy 6000h
             ldx 6080h
             lda temp1_hi
             sta 6000h
             lda temp1_lo
             sta 6080h
             cpy #055h
             bne no_ram
             cpx #0aah
             bne no_ram
             
             lda #0aah
             sta 6000h
             eor #0ffh
             sta 6080h
             ldy 6000h
             ldx 6080h
             lda temp1_hi
             sta 6000h
             lda temp1_lo
             sta 6080h
             cpy #0aah
             bne no_ram
             cpx #055h
             bne no_ram

no_ram:      rts

;             .fill 07c0h-*,0ffh   ;fill rest to get 1K of data
;
;             .db "PRG Pages (lo)"
;             .fill 07ceh-*,0
;             .fill 07d0h-*,0ffh
;
;             .db "PRG Pages (hi)"
;             .fill 07deh-*,0
;             .fill 07e0h-*,0ffh
;
;             .db "CHR Pages (lo)"
;             .fill 07eeh-*,0
;             .fill 07f0h-*,0ffh
;
;             .db "CHR Pages (hi)"
;             .fill 07feh-*,0
;             .fill 0800h-*,0ffh
             .end
