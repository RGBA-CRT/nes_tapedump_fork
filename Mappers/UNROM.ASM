             .title        "UNROM Reader Plugin"

;06/12/00
;Written by KH
;Version 1.0

;              ;plugin version and flags
;             .db p_ver
;             .db p_read | p_var1

;08/05/07
;Merged my UOROM plugin (from 02/19/06) into this one.
;Not sure why I didn't do this from the start!?

;11/11/07
;Added support for HVC-UN1ROM

;02/25/08
;Changed way UN1ROM is detected, Pro Wrestling bank setup was fooling it
;into detecting it as UN1ROM, when it's not. Detection is not bullet-proof,
;maybe it should be setup for a user var instead.

;11/01/09
;Crazy Climber got crazy and used 74HC08 in place of the typical 74HC32
;which effectively changes the swappable area from 8000h to C000h. Set UV1 = C0h
;for this type of config (size will be fixed to 128K). Not setting UV1 will default to 80h

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

init_crc:    .equ 0212h
do_crc:      .equ 0215h
finish_crc:  .equ 0218h

crc0:        .equ 0080h
crc1:        .equ 0081h
crc2:        .equ 0082h
crc3:        .equ 0083h

bank0crc:    .equ 00d0h
temp_crc:    .equ 00d4h
address:     .equ 00d8h
address_lo:  .equ 00d8h
address_hi:  .equ 00d9h
isun1rom:    .equ 00dah

;variables
temp1:       .equ 00e0h
temp1_lo:    .equ 00e0h
temp1_hi:    .equ 00e1h
temp2:       .equ 00e2h
temp2_lo:    .equ 00e2h
temp2_hi:    .equ 00e3h
temp3:       .equ 00e4h
temp3_lo:    .equ 00e4h
temp3_hi:    .equ 00e5h
temp4:       .equ 00e6h
temp4_lo:    .equ 00e6h
temp4_hi:    .equ 00e7h
temp5:       .equ 00e8h
temp5_lo:    .equ 00e8h
temp5_hi:    .equ 00e9h
x_temp:      .equ 00eah
y_temp:      .equ 00ebh

uv1enabled:  .equ 07ceh
swapaddr:    .equ 07cfh       ;*Probably* won't have this as an option in TapeDump.

;plugin header data

             .org 0400h
	     jmp	all_dump	;All dumping start
	     jmp	hdr_dump	;send iNES header
	     jmp	mpr_end        ;send PRG only		;This mapper is PRG only anyway
	     jmp	mpr_end		;send CHR only          ;No CHR
	     jmp	mpr_end		;wram?			;No WRAM
	     jmp	mpr_end		;set PRG bank
	     jmp	mpr_end		;set CHR bank
	     jmp	mpr_end		;send CRCs???

all_dump:    ;Send ALL data!  (Fairly short for UxROM)
	     jsr hdr_dump	;Determine header & send it!
	     jsr Send_PRG
mpr_end:     rts            ;done

hdr_dump:    ;Determine info in the header and dump it.
	     jsr Chk_Mirroring   ;These two are needed for header info
	     jsr Chk_PRG_size    ;"	"	"
	     jsr send_ldr	;5-second minimum lead-in
	     jsr send_hdr	;Send iNES header
	rts

;plugin code start here
Chk_Mirroring:
;check mirroring
             ldx #01h    	;was Backwards from iNES format!
             
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

Chk_PRG_size:
;clear UN1ROM flag
             lda #0
             sta isun1rom

;check if user var 1 is set
             lda uv1enabled
             cmp #0ffh
             bne is128k		;if uv1 is set, skip size detect checks

;set swap address to 8000h if it wasn't
             lda #080h
             sta swapaddr

;check for UN1ROM by comparing bank 0 to bank 3. Setting to bank 3 will be ineffective with
;UN1ROM because lower 2 bits are not used. So if crc's match, it is UN1ROM. This is assuming
;A normal UNROM game wouldn't have duplicate banks in these locations! Originally was set to
;check bank 0 and 1, but turned out a normal UNROM game did have dupe banks here.

chk_un1:     lda #0
             jsr wr_bank	;set to PRG bank 0
             jsr crc_pbank	;get crc of bank 0

             ldx #3
ld_pcrc:     lda temp_crc,x	;save for comparison
             sta bank0crc,x
             dex
             bpl ld_pcrc

             lda #3		;changed from bank 1 -> 3
             jsr wr_bank	;set to PRG bank 3
             jsr crc_pbank	;get crc of bank 3

             ldx #3		;compare bank 0 and 3
chk_crc:     lda bank0crc,x
             cmp temp_crc,x
             bne crc_fail
             dex
             bpl chk_crc

isun1:       lda #1
             sta isun1rom	;set UN1ROM flag     ;Mapper 94 ($5E) for senjou no Ookami
        ;---------
        lda     inesHDR+6
        ora	#0E0h		;don't need to AND since old map was 020h
	sta     inesHDR+6     	;Lower nybble goes to header
	lda	#050h
	sta     inesHDR+7      ;Upper nybble goes to header
	;----------------
             ldx #2
             jmp next_part	;set to 128K PRG

crc_fail:    lda #3		;crc failed, determine size of normal UxROM game
             jsr wr_bank
             jsr compbank
             beq is64k

             lda #7
             jsr wr_bank
             jsr compbank
             beq is128k

is256k:      ldx #4
             jmp next_part
is128k:      ldx #2
             jmp next_part
is64k:       ldx #1

next_part:	txa
		asl a
		asl a	;Change these to regular iNES size!
		sta inesPRG	;Num of PRG banks
	rts


Send_PRG:     ;X (from PRG size) needs to be preserved on entry!

             lda #0
             sta temp3_lo
             txa
             asl a
             asl a           ;8 or 16 banks
             sta temp3_hi
            
             
di_loop2:    lda isun1rom	;check un1rom flag
             cmp #1
             bne normal
             lda temp3_lo	;shift bank << 2 for UN1ROM
             asl a
             asl a
             jmp setbank

normal:      lda temp3_lo

setbank:     jsr wr_bank
             ldx #040h
             ldy #0
             sty temp1_lo
             lda swapaddr
             sta temp1_hi

dump_it:     lda (temp1),y
             jsr send_byte
             iny
             bne dump_it
             inc temp1_hi
             dex
             bne dump_it
             inc temp3_lo
             dec temp3_hi
             bne di_loop2

             rts
             
             
;======================================================
;======================================================

wr_bank:     stx x_temp
             sty y_temp
             sta temp4_lo   ;store desired bank
             lda #0
             sta temp5_lo
             lda swapaddr
             sta temp5_hi   ;start addr
             ldx #080h      ;# iterations
             ldy #0

wrb_loop:    lda (temp5),y
             and #1fh
             cmp temp4_lo   ;check current byte
             beq gotit
             iny
             bne wrb_loop
             inc temp5_hi
             bne wrb_loop   ;keep checking
             lda temp4_lo
             sta 08000h
             ldx x_temp
             ldy y_temp
             rts            ;if it wasn't found, bag it
             
gotit:       lda (temp5),y             
             sta (temp5),y  ;if we found it, do the switch
             ldx x_temp
             ldy y_temp
             rts

compbank:    lda #0
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
             bne diff
             iny
             bne chk_loop
             inc temp1_hi
             inc temp2_hi
             dex
             bne chk_loop

diff:        rts 

;CRC routine for 16K PRG bank @ $8000 (or $C000), stores crc in temp_crc

crc_pbank:   lda #0
             sta address_lo
             lda swapaddr
             sta address_hi	;set address to $8000 (or $C000)

             jsr init_crc
             ldy #0
             ldx #64		;init crc for 16K of data (64 pages = 16K)

crc_ploop:   lda (address),y	;crc generation loop
             stx x_temp
             jsr do_crc
             ldx x_temp
             iny
             bne crc_ploop
             inc address_hi
             dex
             bpl crc_ploop

             ldx #3
crc_psave:   lda crc0,x		;save crc in temp_crc
             sta temp_crc,x
             dex
             bpl crc_psave

             rts

;             .fill 07c0h-*,0ffh   ;fill rest to get 1K of data
;
;             .db "Swap Address"
;             .fill 07ceh-*,0
;             .fill 07d0h-*,0ffh
;
;             .fill 0800h-*,0ffh   ;fill rest to get 1K of data

             .end
