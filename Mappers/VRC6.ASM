             .title        "VRC6 Reader Plugin"

;10/29/00 ;Written by KH ;Version 1.0
;07/26/07...09 ;Bugfix by BootGod - changed to mapper controlled mirroring

             ;vectors for standard system calls
inesHDR      .equ 0060h	;$60-$6F is the iNES header for sending
inesPRG	     .equ inesHDR+4	;Num of 16K banks in iNES header
inesCHR	     .equ inesHDR+5	;Num of 8K banks in iNES header
send_hdr:    .equ 01FAh        ;Send iNES header as audio
send_ldr:    .equ 01FDh		;Send 5 sec tone (for CopyNES to use)
                            
send_byte:   .equ 0200h
;baton:       .equ 0203h
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
temp4_lo:    .equ 00e6h
temp4_hi:    .equ 00e7h

temp_bank:   .equ 00ech
romsiz:      .equ 00edh
curr_bank:   .equ 00eeh


;plugin header data

             .org 0400h
;plugin code start here
	     jmp	all_dump	;All dumping start
	     jmp	hdr_dump	;send iNES header
	     jmp	prg_dump        ;send PRG only
	     jmp	chr_dump	;send CHR only
	     jmp	SRAM_Check	;Check for SRAM
	     jmp	mpr_end		;set PRG bank
	     jmp	mpr_end		;set CHR bank
	     jmp	mpr_end		;send CRCs???

all_dump:    ;Send ALL data!
;	     jsr Check_VRC_Ver
	     jsr hdr_dump	;Determine header & send it!
	     jsr Send_PRG
             jsr Send_CHR
mpr_end:     rts            ;done

hdr_dump:    ;Determine info in the header and dump it.
	     jsr Check_WRAM   ;These three are needed for header info
	     jsr Detect_PRG_Size ;"	"	"
	     jsr Detect_CHR_Size ;"	"	"
	     jsr send_ldr	;5-second minimum lead-in
	     jsr send_hdr	;Send iNES header
	rts
	
prg_dump:    ;Send the PRG data only.
;	     jsr Check_VRC_Ver
	     jsr Detect_PRG_Size    ;"	"	"
             jsr send_ldr
	     jsr Send_PRG
	     rts
	     
chr_dump:    ;Send CHR data only.
;	     jsr Check_VRC_Ver
 	     jsr Detect_CHR_Size
             jsr Send_CHR
 	     rts
 	     
;==============================================
Detect_PRG_Size:
             lda #0feh
             sta 0c000h      ;last 16K = last 16K on cart

             lda #1
             sta temp_bank
             lda #003h
             jsr test_bank
             beq got_it
             asl temp_bank
             lda #007h
             jsr test_bank
             beq got_it
             asl temp_bank

got_it:      lda temp_bank	;1,2,4 meaning $4,$8,$10 x 16k banks!
	     asl a
	     asl a
	     sta inesPRG        ;Used for the header... AND the dump routine.
	     rts
;-------------

Send_PRG:
             lda #0
             sta temp3_lo
 	     lda inesPRG      ;Get our header which contains needed size!
;             txa
;             asl a
;             asl a           ;4, 8 or 16 banks
             sta temp3_hi

di_loop2:    lda temp3_lo
             sta 08000h
             ldx #040h
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
             inc temp3_lo
             dec temp3_hi
             bne di_loop2
             rts
;------------------

Detect_CHR_Size:
             lda #1             ;1=32k
             sta romsiz
             ldy #020h          ;32k???
             jsr comp_bank2
             beq gotit2
             asl romsiz         ;2=64k
             ldy #040h   	;64k
             jsr comp_bank2
             beq gotit2
             asl romsiz         ;4=128k
             ldy #080h     	;128k
             jsr comp_bank2
             beq gotit2
             asl romsiz         ;8=256k
             
gotit2:      lda romsiz
		asl a
		asl a		;Mult by 4 to get CHR banks
		sta inesCHR
	     rts
;---------------------------

Send_CHR:    lda romsiz
             pha
             lsr romsiz
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

send_plp3:   lda temp3_hi
             sta 0d000h      ;current bank
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
             
             rts
             
;=========================
Check_WRAM:  lda #080h
             sta 0b003h
             jsr chk_wram
             bne chk_done
	     lda #02h		;WRAM flag
	     ora inesHDR+6	;SRAM, Mirroring flags
	     sta inesHDR+6
chk_done:    lda #000h
             sta 0b003h
nram3:		rts


SRAM_Check	;Check for SRAM
	     jsr Check_WRAM
             lda inesHDR+6
             and #2		;Return presence for our WRAM dumper
             beq	nram3   ;Quit if no SRAM
       	     lda #080h
             sta 0b003h
             jsr     dm_wram		;Dump it.
	     jmp     chk_done		;Lock SRAM via the Mapper



test_bank:   sta 08000h
             lda #0
             sta temp1_lo
             sta temp2_lo
             lda #080h
             sta temp1_hi
             lda #0c0h
             sta temp2_hi

             ldx #040h
             ldy #0

chk_loopq:   lda (temp1),y
             cmp (temp2),y
             bne not_it
             iny
             bne chk_loopq
             inc temp1_hi
             inc temp2_hi
             dex
             bne chk_loopq

not_it:      rts



;y = bank to compare
;z=same
;for CHR
comp_bank2:  ldx #0
             stx temp3_lo    ;lower pointer
             sty temp3_hi    ;upper pointer
             sty temp4       ;# blocks to compare

cc_loop:     lda temp3_lo
             sta 0d000h
             lda temp3_hi
             sta 0e000h      ;write in current banks
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
             adc #010h
             inc curr_bank
             sta 2006h       ;pointer =1000h
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

;
;             .fill 0800h-*,0ffh   ;fill rest to get 1K of data
;
             .end
             
