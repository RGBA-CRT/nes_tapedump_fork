             .title        "TxROM Reader Plugin"

;             ;plugin description
;             .db "Nintendo TxROM / MMC3 carts"

;This has been optimized a bit, 'cause space is tight (RTSes removed, etc...)

;plugin code start here

;06/11/00
;Written by KH
;Version 1.0

;12/17/05
;Bugfix by BootGod
;Fixed dumping of 32K PRG ROM

;If the PRG is 32K, the var romsiz = 1 (# of 32K banks), when it calcs the size to send
;back to client, it LSR's romsiz making it 0. After telling the client, it does a series of
;ASL's to get the # of 8K banks, but in this case, it's not doing anything because it's 0.
;So to fix, it checks if it's 0 and sets it to 4 if so (4 x 8K = 32K)

             ;vectors for standard system calls
inesHDR      .equ 0060h	;$60-$6F is the iNES header for sending
inesPRG	     .equ inesHDR+4	;Num of 16K banks in iNES header
inesCHR	     .equ inesHDR+5	;Num of 8K banks in iNES header
send_hdr:    .equ 01FAh        ;Send iNES header as audio
send_ldr:    .equ 01FDh		;Send 5 sec tone (for CopyNES to use)

send_byte:   .equ 0200h
;chk_vram:    .equ 0206h
dm_wram:    .equ 0209h  	;Dump WRAM in the RAM code!
wr_ppu:      .equ 020ch

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
romsiz:      .equ 00e7h
curr_bank:   .equ 00e8h
cromsiz:      .equ 00e9h

;plugin header data

             .org 0400h
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
             jmp Send_CHR  ;done
        ;    rts

hdr_dump:    ;Determine info in the header and dump it.
	     jsr Chk_Mirroring   ;These four are needed for header info
	     jsr Detect_SRAM
	     jsr Detect_PRG_Size ;"	"	"
	     jsr Detect_CHR_Size ;"	"	"
	     jsr send_ldr	;5-second minimum lead-in
	     jmp send_hdr	;Send iNES header
;	rts

prg_dump:    ;Send the PRG data only.
	     jsr Detect_PRG_Size    ;"	"	"
             jsr send_ldr
	     jmp Send_PRG
;	     rts

chr_dump:    ;Send CHR data only.
 	     jsr Detect_CHR_Size
             jmp Send_CHR
; 	     rts

;=========================================
Chk_Mirroring:
             lda #00h
             sta 0a000h

             ldx #02fh

ddloop:      txa
             jsr wr_ppu
             lda #0
             sta 2007h
             dex
             cpx #01fh
             bne ddloop   ;load first byte of 2000, 2400, 2800, 2c00 w/ 00h
             ldx #0       ;H mirroring
             lda #020h
             jsr wr_ppu
             lda #055h
             sta 2007h
             lda #028h
             jsr wr_ppu
             lda 2007h
             lda 2007h
             cmp #055h
             beq mirrord
             inx          ;V mirroring
             lda #024h
             jsr wr_ppu
             lda 2007h
             lda 2007h
             cmp #055h
             beq mirrord
             ldx #08h	;inx          ;4 screen
             bne got_mir
             
mirrord:     lda #01h
             sta 0a000h
             lda #024h
             jsr wr_ppu
             lda 2007h
             lda 2007h
             cmp #055h
             bne got_mir
             lda #00h
             sta 0a000h
             lda #028h
             jsr wr_ppu
             lda 2007h
             lda 2007h
             cmp #055h
             beq mirr_end   ;bne got_mir  ;test for MMC3 mirror control
;             ldx #04h	;If mapper-controlled mirroring, write nothing.
;	     rts	;Simply exit
got_mir:	     txa
             	; jsr send_byte
             	ora inesHDR+6
             	sta inesHDR+6	;Save in header.
mirr_end:         rts
;----------------------
Detect_PRG_Size:
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
gotit:       lda romsiz 	;Get size, save in header
	     asl a		;Multiply by 2
	     sta inesPRG
		rts



Send_PRG:
	     lsr romsiz
             lda #0
             ror a
             sta temp3_hi
;             jsr send_byte
;             lda romsiz
;             jsr send_byte
             lda romsiz
             asl a
             asl a
             asl a
             sta temp3_lo    ;# 8K banks

             cmp #0          ;if PRG is 32K, fix # 8K banks
             bne send_hdbyt
             lda #4
             sta temp3_lo

send_hdbyt:;    lda #01h
           ;  jsr send_byte

send_plp:    lda #6
             sta 08000h
             lda temp3_hi
             sta 08001h
             inc temp3_hi
             ldy #0
             sty temp1_lo
             lda #080h
             sta temp1_hi
             ldx #020h
;             ldy #0

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



Detect_CHR_Size:
;read CHR stuff
;try VRAM

	lda	#$00
	sta $2001	;Screen off!
             lda #01h
             sta 0a000h   ;I don't know why, but it doesn't work without this

             lda #82h
             sta 08000h
             lda #00h
             sta 08001h
             sta cromsiz	;Clear our CHR-ROM size here!

             jsr chk_vram    	;Is there VRAM?
             bne no_ram3        ;If not, dump CHR
             jmp gotit2		;Otherwise, jump to end.
no_ram3:

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
gotit2:      lda cromsiz 	;Get size, save in header
	     asl a		;Multiply by 4
	     asl a
	     sta inesCHR
	     rts

;-----------------------
Send_CHR:    lda cromsiz	;If C(hr)ROMSIZE is 0, finish!
	     beq no_chr		;bne cont_snd_chr
;	     rts ;jmp no_chr
cont_snd_chr:
             jsr send_ldr	;Send leader again
;	     lda cromsiz
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

send_plp3:   lda #082h
             sta 08000h
             lda temp3_hi
             sta 08001h      ;current bank
             lda #00h
             tay
             jsr wr_ppu
             lda 2007h      ;set up PPU
;             ldy #0
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
no_chr: rts



Detect_SRAM:
;check for save-game RAM
	      lda #080h
             sta 0a001h
             jsr chk_wram
             bne no_ram2
		; *** I am NOT going to dump WRAM! ***

	lda #02h	;WRAM flag
	ora inesHDR+6	;SRAM, Mirroring flags
	sta inesHDR+6

no_ram2:     lda #000h
             sta 0a001h
nram3: rts
	
SRAM_Check:
	     jsr Detect_SRAM
             lda inesHDR+6
             and #2		;Return presence for our WRAM dumper
        	beq	nram3
	      lda #0C0h
             sta 0a001h		;Have to activate SRAM for reading ONLY?
                jsr     dm_wram		;Dump it.
		jmp     no_ram2		;Lock SRAM via the Mapper


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
             lda #006h
             sta 08000h
             lda temp3_lo
             sta 08001h
             lda #007h
             sta 08000h
             lda temp3_hi
             sta 08001h      ;write in current banks
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

cc_loop:     lda #082h
             sta 08000h
             lda temp3_lo
             sta 08001h
             lda #083h
             sta 08000h
             lda temp3_hi
             sta 08001h      ;write in current banks
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


;--------------------

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
             
no_ram5:
mpr_end:     rts
;-------------------

        .end
