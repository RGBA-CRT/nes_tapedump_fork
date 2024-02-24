             .title        "VRC2 + VRC4 Reader Plugin"
;             .db "Konami VRC2 / VRC4 carts"
;           .db p_read | p_var1

;01/14/01 
;Written by KH
;Version 1.0

;.../09
;BootGod
;Thru user variables, supports all VRC2 & 4 variants. Valid values are: 2a,2b,4a,4b,4c,4d,4e
;Added write to 9004h (vrc4d prg swap control) to ensure 8000 is mapped to 8000 and not C000

             ;vectors for standard system calls

inesHDR      .equ 0060h	;$60-$6F is the iNES header for sending
inesPRG	     .equ inesHDR+4	;Num of 16K banks in iNES header
inesCHR	     .equ inesHDR+5	;Num of 8K banks in iNES header
send_hdr:    .equ 01FAh        ;Send iNES header as audio
send_ldr:    .equ 01FDh		;Send 5 sec tone (for CopyNES to use)

send_byte:   .equ 0200h
;baton:       .equ 0203h
;chk_vram:    .equ 0206h	;Unused.
dm_wram:    .equ 0209h  	;Dump WRAM in the RAM code!
wr_ppu:      .equ 020ch

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
romsiz:      .equ 00e7h
curr_bank:   .equ 00e8h
cromsiz:      .equ 00e9h

cb0_1:        .equ 00eah
cb1_0:        .equ 00ebh
cb1_1:        .equ 00ech
			;User vars... need to move these to some unused part of RAM...
uv1enabled:   .equ 06feh	;Like, the top of the mapper?
vrc_version:  .equ 06ffh

             .org 400h
	     jmp	all_dump	;All dumping start
	     jmp	hdr_dump	;send iNES header
	     jmp	prg_dump        ;send PRG only
	     jmp	chr_dump	;send CHR only
	     jmp	SRAM_Check	;Check for SRAM
	     jmp	mpr_end		;set PRG bank
	     jmp	mpr_end		;set CHR bank
	     jmp	mpr_end		;send CRCs???

all_dump:    ;Send ALL data!
	     jsr hdr_dump	;Determine header & send it!
	     jsr Send_PRG
             jsr Send_CHR
mpr_end:     rts            ;done

hdr_dump:    ;Determine info in the header and dump it.
	     jsr Check_VRC_Ver
	     jsr Chk_Mirroring   ;These three are needed for header info
	     jsr Detect_PRG_Size ;"	"	"
	     jsr Detect_CHR_Size ;"	"	"
	     jsr send_ldr	;5-second minimum lead-in
	     jsr send_hdr	;Send iNES header
	rts
	
prg_dump:    ;Send the PRG data only.
	     jsr Check_VRC_Ver
	     jsr Detect_PRG_Size    ;"	"	"
             jsr send_ldr
	     jsr Send_PRG
	     rts
	     
chr_dump:    ;Send CHR data only.
	     jsr Check_VRC_Ver
 	     jsr Detect_CHR_Size
             jsr Send_CHR
 	     rts
 	     
;-------------------
Detect_PRG_Size
	     lda #1
             sta romsiz
             ldy #004h
             jsr comp_bank
             beq gotit
             asl romsiz
             ldy #008h
             jsr comp_bank
             beq gotit
             asl romsiz
             ldy #010h
             jsr comp_bank
             beq gotit
             asl romsiz
             ldy #020h
             jsr comp_bank
             beq gotit
             asl romsiz
gotit:       lda romsiz
		asl	a	;iNES header uses 1 = 16k bank; this prog uses 1 = 32k banks
       	     sta inesPRG
	     rts
;---------------

Send_PRG:
	     lsr romsiz
             lda #0
             ror a
             sta temp3_hi
             lda romsiz
             asl a
             asl a
             asl a
             sta temp3_lo    ;# 8K banks

send_plp:    lda temp3_hi
             sta 08000h
             inc temp3_hi
             lda #0
             sta temp1_lo
             lda #080h
             sta temp1_hi
             ldx #020h
             ldy #0

send_plp2:   lda (temp1),y
             jsr send_byte
             iny
             bne send_plp2
             inc temp1_hi
             dex
             bne send_plp2    ;send 8K bank
             dec temp3_lo
             bne send_plp
             rts
;----------------------------------

Detect_CHR_Size:
	lda	#$00
	sta $2001	;Screen off!
;read CHR stuff
	     lda #1
             sta cromsiz
             ldy #020h
             jsr comp_bank2
             beq gotit2
             asl cromsiz
             ldy #040h
             jsr comp_bank2
             beq gotit2
             asl cromsiz
             ldy #080h
             jsr comp_bank2
             beq gotit2
             asl cromsiz
gotit2:      lda cromsiz
		asl a
		asl a	;CopyNES seems to count # of 32k banks!
	     sta inesCHR
	     rts
;----------------------------------------
Send_CHR:
	     lda cromsiz
             pha
             lsr cromsiz
             lda #0
             sta temp3_hi    ;start out at 0
             ror a
             pla
             asl a
             asl a
             asl a
             asl a
             asl a  ;1,2,4,8 == 20,40,80,00
             sta temp3_lo
             
             jsr send_ldr	;Send leader again

send_plp3:   lda temp3_hi
             ldx vrc_version
             cpx #2ah
             bne noshift
             asl a

noshift:     sta 0b000h
             lsr a
             lsr a
             lsr a
             lsr a
             ldx cb0_1
             sta (0b000h),x      ;current bank
             lda #00h
             jsr wr_ppu
             lda 2007h      ;set up PPU
             ldy #0
             ldx #4

send_plp4:   lda 2007h
             jsr send_byte
             iny
             bne send_plp4
             dex
             bne send_plp4
             inc temp3_hi
             dec temp3_lo
             bne send_plp3
done_chr:      rts

;==========================================================
;==========================================================

;y = bank to compare
;z=same
comp_bank:   ldx #0
             stx temp3_lo    ;lower pointer
             sty temp3_hi    ;upper pointer
             sty temp4       ;# blocks to compare

cb_loop:     lda #000h       ;init pointers
             sta temp1_lo
             sta temp2_lo
             lda #080h
             sta temp1_hi
             lda #0a0h
             sta temp2_hi    ;pointers 1,2 to 8000/a000
             
             lda temp3_lo
             sta 08000h
             lda temp3_hi
             sta 0a000h      ;write in current banks
             
             ldx #020h
             ldy #0

cb_loop2:    lda (temp1),y
             cmp (temp2),y
             bne diff
             iny
             bne cb_loop2
             inc temp1_hi
             inc temp2_hi
             dex
             bne cb_loop2
             inc temp3_lo
             inc temp3_hi
             dec temp4
             bne cb_loop

diff:        rts

;y = bank to compare
;z=same
;for CHR
comp_bank2:  ldx #0
             stx temp3_lo    ;lower pointer
             sty temp3_hi    ;upper pointer
             sty temp4       ;# blocks to compare

cc_loop:     lda temp3_lo
             ldx vrc_version
             cpx #2ah
             bne noshift2
             asl a

noshift2:    sta 0b000h
             lsr a
             lsr a
             lsr a
             lsr a
             ldx cb0_1
             sta (0b000h),x

             lda temp3_hi
             ldx vrc_version
             cpx #2ah
             bne noshift3
             asl a

noshift3:    ldx cb1_0
             sta (0b000h),x      ;write in current banks
             lsr a
             lsr a
             lsr a
             lsr a
             ldx cb1_1
             sta (0b000h),x

             ldx #004h
             lda #000h
             sta curr_bank   ;reset current bank

cc_loop2:    ldy #0
             lda curr_bank
             sta 2006h       ;pointer =000h
             sty 2006h
             lda 2007h       ;garbage read

ql:          lda 2007h
             sta 0700h,y
             iny
             bne ql          ;load 256 bytes for testing
             lda curr_bank
             clc
             adc #4
             inc curr_bank
             sta 2006h       ;pointer =400h
             sty 2006h
             lda 2007h       ;garbage read

cc_loop3:    lda 2007h
             cmp 0700h,y
             bne diff2
             iny
             bne cc_loop3
             dex
             bne cc_loop2
             inc temp3_lo
             inc temp3_hi
             dec temp4
             bne cc_loop

diff2:       rts
;------------------------------------
Check_VRC_Ver:
             lda #000h
             sta 09002h
;vrc version and variant must be defined, otherwise exit

;             lda uv1enabled	;Assume this is correct.
;             cmp #0ffh		;If it's incorrect, wouldn't dump anyway.
;             beq badvar
;             jmp goodvar
;
;badvar:      jmp done

goodvar:     lda vrc_version
             cmp #02ah
             bne check4b
             beq vrc2a4b
             
check4b:     cmp #04bh
             bne not_vrc2a

vrc2a4b:     lda #2
             sta cb0_1
             lda #1
             sta cb1_0
             lda #3
             sta cb1_1
             jmp vrc_ready

not_vrc2a:   lda vrc_version
             cmp #02bh
             bne not_vrc2b
             lda #1
             sta cb0_1
             lda #2
             sta cb1_0
             lda #3
             sta cb1_1
             jmp vrc_ready

not_vrc2b:   lda vrc_version
             cmp #04ah
             bne not_vrc4a
             lda #2
             sta cb0_1
             lda #4
             sta cb1_0
             lda #6
             sta cb1_1
             jmp vrc_ready

not_vrc4a:   lda vrc_version
             cmp #04ch
             bne not_vrc4c
             lda #40h
             sta cb0_1
             lda #80h
             sta cb1_0
             lda #0c0h
             sta cb1_1
             jmp vrc_ready

not_vrc4c:   lda vrc_version
             cmp #04dh
             bne not_vrc4d
             lda #8
             sta cb0_1
             lda #4
             sta cb1_0
             lda #0ch
             sta cb1_1
             lda #0
             sta 09004h
             jmp vrc_ready

not_vrc4d:   lda vrc_version
             cmp #04eh
             bne not_vrc4e
             lda #4
             sta cb0_1
             lda #8
             sta cb1_0
             lda #0ch
             sta cb1_1
             jmp vrc_ready

not_vrc4e:   pla
	     pla		;Cheat with the return address!
	     jmp mpr_end	;wasn't a valid version, exit

vrc_ready:   rts



Chk_Mirroring:
;	      lda #04h          ;mapper controlled mirroring
;             jsr send_byte     ;send mirroring info
; Mirroring is ignored for Konami VRC2/4 chips
;	jmp	Detect_WRAM
Detect_WRAM:          ;check for save-game RAM.
	     lda #001h
             sta 09002h

             jsr chk_wram
             bne chk_done
	lda #02h	;WRAM flag
	ora inesHDR+6	;SRAM, Mirroring flags
	sta inesHDR+6

chk_done:    lda #000h
             sta 09002h
nram3:	     rts
	     
SRAM_Check:	jsr Detect_WRAM
             lda inesHDR+6
             and #2		;Return presence for our WRAM dumper
        	beq	nram3   ;Quit if no SRAM
        	lda #001h
                sta 09002h
                jsr     dm_wram		;Dump it.
		jmp     chk_done		;Lock SRAM via the Mapper


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
;             .db "VRC 2ab,4a-e"
;             .fill 07ceh-*,0
;             .fill 07d0h-*,0ffh
;
;             .fill 0800h-*,0ffh   ;fill rest to get 1K of data

             .end
