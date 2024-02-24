             .title        "NROM Reader Plugin"

;06/10/00 
;Written by KH
;Version 1.0
             
             ;vectors for standard system calls

inesHDR      .equ 0060h	;$60-$6F is the iNES header for sending
inesPRG	     .equ inesHDR+4	;Num of 16K banks in iNES header
inesCHR	     .equ inesHDR+5	;Num of 8K banks in iNES header
send_hdr:    .equ 01FAh        ;Send iNES header as audio
send_ldr:    .equ 01FDh		;Send 5 sec tone (for CopyNES to use)

send_byte:   .equ 0200h
;baton:       .equ 0203h
dm_wram:    .equ 0209h  	;Dump WRAM in the RAM code!
;wr_ppu:      .equ 020ch

temp1:       .equ 00e0h
temp1_lo:    .equ 00e0h
temp1_hi:    .equ 00e1h
temp2:       .equ 00e2h
temp2_lo:    .equ 00e2h
temp2_hi:    .equ 00e3h
temp3:       .equ 00e4h
temp3_lo:    .equ 00e4h
temp3_hi:    .equ 00e5h


             .org 0400h
	     jmp	all_dump	;All dumping start
	     jmp	hdr_dump	;send iNES header
	     jmp	prg_dump        ;send PRG only
	     jmp	Send_CHR	;send CHR only
	     jmp	SRAM_OK		;A basic SRAM dumper
	     jmp	mpr_end		;set PRG bank
	     jmp	mpr_end		;set CHR bank
	     jmp	mpr_end		;send CRCs???


all_dump:    ;Send ALL data!  (Fairly short for NROM)
	     jsr hdr_dump	;Determine header & send it!
	     jsr Send_PRG
             jsr Send_CHR
mpr_end:     rts            ;done

hdr_dump:    ;Determine info in the header and dump it.
	     jsr Chk_Mirroring   ;These two are needed for header info
	     jsr Chk_PRG_size    ;"	"	"
	     jsr send_ldr	;5-second minimum lead-in
	     jsr send_hdr	;Send iNES header
	rts
	
prg_dump:    ;Send the PRG data only.
	     jsr Chk_PRG_size    ;"	"	"
	     jsr send_ldr	;5-second minimum lead-in
	     jsr Send_PRG
	     rts

;---------------------
Chk_Mirroring:
;check mirroring

             ldx #01h		;This code had it backwards!
             
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
             bne horz_mir
             lda 2007h
             cmp #0aah
             bne horz_mir
             lda #020h
             jsr wr_ppu
             lda #0aah
             sta 2007h
             lda #024h
             jsr wr_ppu
             lda 2007h
             lda 2007h
             cmp #0aah
             bne horz_mir
             dex

horz_mir:    txa
	     ora inesHDR+6
	     sta inesHDR+6	;Save mirroring in iNES header.
	     rts
;---------------------
Chk_PRG_size:
             lda #000h       ;determine if this is a 16K or 32K game
             sta temp1_lo
             sta temp2_lo
             lda #080h
             sta temp1_hi
             lda #0c0h
             sta temp2_hi
             
             ldx #040h
             ldy #0

chk_loop:    lda (temp1),y
             cmp (temp2),y
             bne not_16k
             iny
             bne chk_loop
             inc temp1_hi
             inc temp2_hi
             dex
             bne chk_loop

;this is a 16K game

             ldx #040h
             lda #1		;1 x 16K banks
             bne next_part

not_16k:     ldx #080h          	;X register needs to be saved!
	     lda #2		;2x 16k banks


next_part:   sta inesPRG	;Num of PRG banks
             lda #001h
             sta inesCHR        ;One 8K CHR bank only
             rts
;----------------------
Send_PRG:    		;X register needs to contain PRG size!
             ldy #0
             sty temp1_lo
             lda #080h
             sta temp1_hi

dump_it:     lda (temp1),y
             jsr send_byte
             iny
             bne dump_it
             inc temp1_hi
             dex
             bne dump_it
             rts
;----------------------
Send_CHR:
             jsr send_ldr		;Pause between PRG and CHR dump

             ldx #020h
;             tax
;             jsr send_byte
;             lda #000h
;             jsr send_byte
;             lda #002h
;             jsr send_byte  ;send CHR header
             lda #000h
        	sta	$2001	;Screen off!
             sta 02006h
             lda #000h
             sta 02006h     ;set PPU address to 0000h
             lda 02007h     ;read garbage byte
             ldy #0
             
dump_chr:    lda 02007h
             jsr send_byte
             iny
             bne dump_chr

             dex
             bne dump_chr   ;dump 8K of CHR
	     rts
;---------------------------
wr_ppu:      sta 2006h
             lda #0
             sta 2006h
             rts
        ;----------
SRAM_OK:	;NROM is "Generic", so allow SRAM reading for the sake of it.
	jmp	dm_wram

             .end
