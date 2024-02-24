;Chris Covell's KCS Tape ROM Dumper!
;This code selects a mapper & loads the main program into the FC's RAM.

; INES header setup
        .inesprg    2
        .ineschr    1
        .inesmir    0
        .inesmap    0


RAM_Code = $0140		;This is really low in RAM!
inesHDR = $0060	;$60-$6F is the iNES header for sending

PPU2005V = $00FC
PPU2005H = $00FD
PPU2001 = $00FE
PPU2000 = $00FF

RDADD = $50		;Address for reading
RDADD_HI = $51
WRADD = $52     	;Address for writing
WRADD_HI = $53
LP_CN = $54		;One loop counter

Jump_to_Ram = $55	;if 1, proceed to RAM code.
NMI_PASS = $56		;Has NMI passed?
VBCOUNT = $57
MPR_Choice = $58	;Current mapper number
MPR_Num = $59           ;Mapper number for header.
MPR_Opt = $5A

CONT1		= $5D                  ;0-> A,B,SL,ST,U,D,L,R <-7
CONT1LAST	= $5E		;Reading of the last controller
CONT1STROBE	= $5F

SPRITEY	= $0200		;Sprite 0 location in RAM
SPRITEX = SPRITEY+3
SPRYDEST = $0E          ;Sprite 0 final Y resting place
SPRXDEST = $06		;Sprite 0 final X

        ZP

        CODE
        LIST


        .bank 0		;These are 8KB banks in NESASM!  Stupid, eh?
        .org $8000

Mapper_NROM:  incbin "Mappers\NROM.bin"
Mapper_SXROM: incbin "Mappers\SXROM.bin"
Mapper_UXROM: incbin "Mappers\UNROM.bin"
Mapper_CNROM: incbin "Mappers\CNROM.bin"
Mapper_TXROM: incbin "Mappers\TXROM1.bin"
Mapper_AXROM: incbin "Mappers\AXROM.bin"
Mapper_VRC4:  incbin "Mappers\VRC4.BIN"
Mapper_VRC6:  incbin "Mappers\VRC6.BIN"
Mapper_FDS:   incbin "Mappers\FDS.BIN"

Mapper_Add: .dw Mapper_NROM,Mapper_SXROM,Mapper_UXROM,Mapper_CNROM
	    .dw Mapper_TXROM,Mapper_AXROM
	.dw Mapper_VRC4,Mapper_VRC4,Mapper_VRC4,Mapper_VRC4
	.dw Mapper_VRC4,Mapper_VRC4,Mapper_VRC4
	.dw Mapper_VRC6,Mapper_VRC6,Mapper_FDS
Map_Add_End:

Map_Max = (Map_Add_End-Mapper_Add)/2

Map_Size = $0380	;It's easier to just load a set size in.


	;Mappers that have $8x set in their description byte
	;have optional code that gets executed after all is loaded.
		      ;123456789ABCDEF
Mapper_List:	.db 0,"0  NROM        "
		.db 1,"1  SXROM (MMC1)"
		.db 2,"2  UXROM       "
		.db 3,"3  CNROM       "
		.db 4,"4  TXROM (MMC3)"
		.db 7,"7  AXROM (RARE)"
	    .db 22,$81,"22 KONAMIVRC2A"
	    .db 23,$82,"23 KONAMIVRC2B"
	    .db 21,$83,"21 KONAMIVRC4A"
	    .db 25,$84,"25 KONAMIVRC4B"
	    .db 21,$85,"21 KONAMIVRC4C"
	    .db 25,$86,"25 KONAMIVRC4D"
	    .db 23,$87,"23 KONAMIVRC4E"
	       .db 24,"24 KONAMI VRC6A"
	       .db 26,"26 KONAMI VRC6B"
		.db 0,$88,"  FDS DISKS   "



Mp_Option_List: .dw Op_00-1,Op_VRC2a-1,Op_VRC2b-1,Op_VRC4a-1    ;80-83
		.dw Op_VRC4b-1,Op_VRC4c-1,Op_VRC4d-1,Op_VRC4e-1 ;84-87
		.dw Op_FDS-1                                    ;88

;	.bank 1
;	.org $A000
;	.bank 2
;	.org $C000
;=============================================================================
		; Program bank!  E000-FFFF
;=============================================================================
	.bank 3
	.org $E000

NES_Hard: .db $4E,$45,$53,$1A	;"NES \"
	  .db 0,0,0,0,0,0
	  .db $00		;Bit 4 has to be set for NO SRAM in CPU?
	  .db 0,0,0,0,0
OK_Msg:	.db "SWAP CARTRIDGES NOW.",0
;--------------0123456789ABCDEF0123456789ABCDEF
Baud_Msg: .db " A=CHG bps.  START=DUMP AS .NES "
	  .db "                                "
	  .db " B=SEND iNES Hdr, $FFD0+ (64byt)"
	  .db " LEFT = DUMP Save-RAM (SRAM)    "
	  .db " RIGHT= DUMP PRG.  SEL=DUMP CHR."
          .db "                                "
	  .db "  ",$FE,"=300bps  ",$FD,"=600bps  ",$FE,"=1200bps "
	  .db " ",$FD,"=2400 @ 44KHz  ",$FE,"=5200 @ 96Khz ",0
	  
FDS_Msg:                              .db "FDS "
	  .db "                                "
	  .db " B= SEND Disk Directory Listing "
 	  .db "                                "
	  .db " SEL= DUMP RAW Disk Files ONLY  ",0

Fancy_Palette:	incbin "Gfx\Soundwave.pal"	;Our fancy title screen palette
Sprite_Palette:	incbin "Gfx\sprites.pal"

Palette: .db $30,$10,$00,$0F	;Main palette, greys
	 .db $30,$31,$21,$11	;An extra palette
	 .db $30,$30,$30,$0F	;Half-used, white & black
	 .db $30,$27,$16,$0F    ;Used for bps display.

Attribs: .db 0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,$AA,0,0
	.db $FF,0,0,0,$FF,0,0,0
 	.db 0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0

Sprite_Init:	;Initial values for sprites
	incbin "Gfx\SpriteDefs.bin"
	
Fancy_Screen:   incbin "Gfx\Soundwave.nam"
;=============================== INITIALIZE RAM, VARS, ETC ===================
Reset_SUB
	cld
	sei
	lda	#$00
	sta	$2000
	sta	<PPU2000
	sta	$2001
	sta	<PPU2001

waitvb1:	lda	$2002		;Wait for VBlank
	bpl	waitvb1

	ldx	#$3F			;Stack will be 64 bytes only!
	txs				;(Should be enough...)

	jsr	Init_Clear_PPU		;Clear up 2 name tables!

	ldy	#$07
	sty	<$01
	ldy	#$00
	sty	<$00
	lda	#$00

clrram1:	sta	[$00],y	;Clear all RAM
	dey
	bne	clrram1
	dec	<$01
	bpl	clrram1

	ldx	#$00
	stx	<NMI_PASS
	stx	<MPR_Choice
	stx	<Jump_to_Ram

	ldx	#$00
.spr_df_lp:
	lda     Sprite_Init,X
	sta	SPRITEY,X		;Save sprite defaults in RAM
	inx
	cpx	#$80		;Have 128 bytes been copied?
	bne	.spr_df_lp

waitvb2:	lda	$2002		;Wait for VBlank
	bpl	waitvb2

	jsr	Write_Mpr_Text		;Writes the text for the default mapper
	
	
	lda	#%10010001    	;NMIs on, 2nd CHR bank, 2nd screen.
	sta	$2000
	sta	<PPU2000
	lda	#%00011110		;Sprites on,Screen on, no clipping.
	sta	$2001
	sta	<PPU2001

;=============================== LOOP & WAIT FOR COMMANDS ===================
.here	lda	<Jump_to_Ram
	beq	.here




.to_ram_code:

	lda	#$00    	;NMIs off!
	sta	$2000
	sta	$2001
;========================= SETUP SCREEN ===========
	jsr	Write_Dmp_Pal 		;Change palette

	ldy	#$00
	lda	#$20
	sta	$2006
	lda	#$66
	sta	$2006	;Point to screen

.write_txt:
	lda	OK_Msg,Y 	;Write short message
	beq	.txt_done
	sta	$2007
	iny
	bne	.write_txt
.txt_done:

	ldy	#$00
	lda	#$20
	sta	$2006
	lda	#$A0
	sta	$2006	;Point to screen

.write_tx2:
	lda	Baud_Msg,Y 	;Write instructions
	beq	.txt_done2
	sta	$2007
	iny
	bne	.write_tx2
.txt_done2:

	ldx	#$00           ;Write a test pattern to the screen
	lda	#$21
	sta     <WRADD+1
	sta	$2006
	lda     #$A8
	sta	<WRADD
	sta	$2006		;Point to PPU $21A8
.wrpattlp1:
	stx	$2007
	inx
	beq	.wrpatt_finish
	txa
	and	#$0F		;Check every 16 lines
	bne	.wrpattlp1
	lda	<WRADD
	clc
	adc	#$20		;Go down one line
	sta	<WRADD
	tay
	lda	<WRADD+1
	adc	#$00
	sta	<WRADD+1
	sta	$2006
	sty	$2006		;Adjust PPU
	bne     .wrpattlp1
.wrpatt_finish:

	ldy	#$00
	lda	#$20
	sta	$2006
	sty	$2006


;=============================== COPY CODE TO RAM ===========================
	;Copy code to RAM

	ldx	#$00
copy_ines:              	;Copy iNES header into RAM for sending
	lda NES_Hard,X
	sta <inesHDR,X
	inx
	cpx	#$10
	bne	copy_ines
	
	lda	<MPR_Num	;Mapper number
	asl	a
	asl	a
	asl	a
	asl	a		;Lower nybble goes to header
	sta     <inesHDR+6
	lda	<MPR_Num	;Mapper number
	and	#$F0		;Isolate top bits
	sta     <inesHDR+7      ;Upper nybble goes to header


	ldx	#$00
copy_low:
        lda	Relocate_Code,X        ;Copy .75 pages of ROM to RAM
	sta	RAM_Code,X
	inx
	cpx	#$C0
	bne     copy_low          ;$0140-$01FF
;---
	ldx	#$00
copycode:
	lda	Relocate_Code+$C0,X        ;Copy my code from ROM to RAM
	sta	RAM_Code+$C0,X        ;$0200-$02FF
	lda	Relocate_Code+$1C0,X
	sta	RAM_Code+$1C0,X       ;$0300-$03FF
;	lda	Code_Loc+$2C0,X
;	sta	RAM_Code+$2C0,X
;	lda	Code_Loc+$3C0,X
;	sta	RAM_Code+$3C0,X
;	lda	Code_Loc+$4C0,X
;	sta	RAM_Code+$4C0,X
;	lda	Code_Loc+$5C0,X
;	sta	RAM_Code+$5C0,X         ;Up to $7FF
	inx
	bne	copycode
	
	lda	#$FF
	ldx	#$00
.fillffs:
	sta	$0700,X          	;FILL $700-$7FF with $FFs?
	inx                             ;User functions use it.
	bne	.fillffs
;------------------------

	lda	<MPR_Choice 		;Which mapper number
	asl	a
	tax
	lda	Mapper_Add,X            ;Get mapper source
	sta	<RDADD	
	lda	Mapper_Add+1,X
	sta	<RDADD+1
	lda	#$00
	sta	<WRADD
	lda	#$04			;Save at $0400-0XXX
	sta	<WRADD+1

	ldy	#$00
	ldx     #LOW(Map_Size)
	lda	#HIGH(Map_Size)
	sta	<LP_CN

.cpy_map_lp:
	lda	[RDADD],Y	;Copy from selected mapper to $0400
	sta	[WRADD],Y
	inc	<RDADD
	bne	.no_upd_rd
	inc	<RDADD+1
.no_upd_rd:
	inc	<WRADD
	bne	.no_upd_wr
	inc	<WRADD+1
.no_upd_wr:
	dex
	bne     .cpy_map_lp
	dec	<LP_CN
	bpl     .cpy_map_lp
	
;-------------
	lda	<MPR_Opt	;Does the mapper have any extra options?
	beq	.no_ex_opts
	jsr	Jump_Options    ;This runs some extra code.
.no_ex_opts:
	jmp	RAM_Code	;Jump to RAM_Code address!


;********************************************************************

;********************************************


;------------------------------- SUBROUTINES ---------------

NMI_SUB
	php
	pha
	txa
	pha
	tya
	pha

;	lda	#$00
;	sta	$2000
;	sta	<PPU2000
;	sta	$2001
;	sta	<PPU2001

	lda	#$01
	sta	<NMI_PASS
	INC	<VBCOUNT
	jsr	Strobe_Cont
	jsr	Controls

	lda	#$2C	;Point to 2nd NAM table now!
	sta	$2006
	lda	#$00
	sta	$2006
	sta	$2005
	sta	$2005

;	lda	#$80    	;NMIs on
;	sta	$2000
;	sta	<PPU2000
;	lda	#%00001000		;Sprites off,Screen on, clipping.
;	sta	$2001
;	sta	<PPU2001
	lda	#$00
	sta	$2003		;Clear sprite read pointer?
	lda	#$02
	sta	$4014		;Init sprite DMA!

	jsr	Move_Sprites	;Slide our sprites down.

	pla
	tay
	pla
	tax
	pla
	plp
	rti

;----------------
IRQ_SUB
	rti

;---------------------
;----------------------------------------------------------------------------
Controls:
	lda	<CONT1STROBE
	and	#$01
	beq	.NotRPressed
			;Right Pressed!
;------------------
.NotRPressed
	lda	<CONT1STROBE
	and	#$02
	beq	.NotLPressed
			;Left Pressed
;---------
.NotLPressed
	lda	<CONT1STROBE
	and	#$04
	beq	.NotDPressed
			;Down Pressed
	inc     <MPR_Choice
	lda	<MPR_Choice
	cmp	#Map_Max	;Have we reached the end of the list?
	bne	.no_reset_mp
	lda	#$00
	sta	<MPR_Choice
.no_reset_mp:
	jsr	Write_Mpr_Text
;-----------
.NotDPressed                         ;No, I'm not depressed...
        lda	<CONT1STROBE
	and	#$08
	beq	.NotUPressed
			;Up Pressed
	lda	<MPR_Choice
	bne	.no_upset_mp 	;Have we gone past the beginning of the list?
	lda	#Map_Max
	sta	<MPR_Choice
.no_upset_mp:
	dec     <MPR_Choice
	jsr	Write_Mpr_Text
;--------------
.NotUPressed
	lda	<CONT1STROBE
	and	#$80
	beq	.NotAPressed
			;A Pressed
	lda	#$01		;Exit to our RAM code!
	sta	<Jump_to_Ram
;-----------------
.NotAPressed
	lda	<CONT1STROBE
	and	#$40
	beq	.NotBPressed
;			;B pressed
;----------------
.NotBPressed
	lda	<CONT1STROBE
	and	#$20
	beq	.NotSELPressed
			;Select Pressed
;------------------
.NotSELPressed
        lda	<CONT1STROBE
	and	#$10
	beq	.NotSTAPressed
				 ;Start pressed
;------------------
.NotSTAPressed
	rts






Jump_Options:                   ;Jumps to some extra code defined by a table
	asl	a		;Option number is in a
	tax
	lda	Mp_Option_List+1,X
	pha
	lda	Mp_Option_List,X	;This puts the jump address on the stack.
	pha
Op_00:	rts


Op_VRC2a: lda #$2a
	bne Op_VRC
Op_VRC2b: lda #$2b
	bne Op_VRC
Op_VRC4a: lda #$4a
	bne Op_VRC
Op_VRC4b: lda #$4b
	bne Op_VRC
Op_VRC4c: lda #$4c
	bne Op_VRC
Op_VRC4d: lda #$4d
	bne Op_VRC
Op_VRC4e: lda #$4e
Op_VRC:	sta $06ff
	lda #$00
	sta $06fe
	rts

Op_FDS:      ;Write a different menu if FDS disks are to be dumped.
	ldy	#$00
	lda	#$20
	sta	$2006
	lda	#$BC
	sta	$2006	;Point to screen
.write_fd:
	lda	FDS_Msg,Y 	;Write instructions
	beq	.fd_txt_done
	sta	$2007
	iny
	bne	.write_fd
.fd_txt_done:
	lda	#$46	;"F"
	sta	<inesHDR
	lda	#$44	;"D"
	sta	<inesHDR+1
	lda	#$02	;FDS Disks have 2 sides, on average...
	sta	<inesHDR+4
	rts
;--------------------------------------

Move_Sprites: 			;Move our sprites in a nice way

	lda	SPRITEY 	;Has it slid down all the way?
	cmp	#SPRYDEST
	beq	.next_test  	;If it has, test for horizontal
	jmp	Shift_Spr_Down
;;	jmp	MvSprEnd
.next_test:
        lda	SPRITEX 	;Has it slid over all the way?
	cmp	#SPRXDEST
	beq	.MvSprEnd
	jmp	Shift_Spr_Side
.MvSprEnd:
	rts

Shift_Spr_Down:
	ldx	#$00
.lp1
	lda     SPRITEY,X
	clc
	adc	#$02
	sta     SPRITEY,X
	inx
	inx
	inx
	inx
	cpx	#$80
	bne	.lp1

	rts
	
Shift_Spr_Side:
	ldx	#$00
.lp1
	lda     SPRITEX,X
	sec
	sbc	#$02
	sta     SPRITEX,X
	inx
	inx
	inx
	inx
	cpx	#$40
	bne	.lp1
.lp2
	lda     SPRITEX,X
	clc
	adc	#$02
	sta     SPRITEX,X
	inx
	inx
	inx
	inx
	cpx	#$80
	bne	.lp2

	rts

Draw_Fancy_Screen:
	lda	#LOW(Fancy_Screen)
	sta	<RDADD
	lda	#HIGH(Fancy_Screen)
	sta	<RDADD+1

;	clc
;	adc     <RDADD
;	sta	<RDADD
;	adc     <RDADD+1
;	sta	<RDADD+1	;Add read address

	ldy	#$00
	ldx	#$04	;$0400 bytes...

	lda	#$2C
	sta	$2006
	lda	#$00
	sta	$2006	;Point to 2nd screen

.write_txt:
	lda	[RDADD],Y 	;Write to NAM table
	sta	$2007
	iny
	bne	.write_txt
	inc	<RDADD+1
	dex
	bne	.write_txt
	rts
	
	
	
	
	
	


Write_Mpr_Text:   	;Writes mapper text to screen, saves mapper #.
	lda	<MPR_Choice
	sta	<RDADD
	lda	#$00
	sta	<RDADD+1
	sta	<MPR_Opt	;Clear our option.

	asl     <RDADD
	rol	<RDADD+1
	asl     <RDADD
	rol	<RDADD+1
	asl     <RDADD
	rol	<RDADD+1
	asl     <RDADD
	rol	<RDADD+1	;Multiply by 16
	lda	#LOW(Mapper_List)
	clc
	adc     <RDADD
	sta	<RDADD
	lda	#HIGH(Mapper_List)
	adc     <RDADD+1
	sta	<RDADD+1	;Add read address

	ldy	#$00
	lda	[RDADD],Y
	sta	<MPR_Num 	;Save mapper number for dumper
	iny
	
	lda	[RDADD],Y       ;Read next byte
;	and     #$80		;If high bit set, mapper has extra options!
	bpl	.no_special_opts
	and	#$7F
	sta	<MPR_Opt        ;Save our option number
	iny			;Go to next byte

.no_special_opts:
	lda	#$2F
	sta	$2006
	lda	#$29
	sta	$2006	;Point to screen
;	lda	#$20
;	sta	$2006
;	lda	#$48
;	sta	$2006	;Point to screen

.write_txt:
	lda	[RDADD],Y 	;Write mapper description
	sta	$2007
	iny
	cpy	#$10
	bne	.write_txt

	lda     <MPR_Opt	;Extra options?
	beq	.no_extra_spc
	lda	#$20	;Space character
	sta	$2007	;Write a space just to cover up ugly stuff.
.no_extra_spc:
;-------------------------- 2nd (MAIN) screen written, now write same to 1st screen.
	lda	#$20
	sta	$2006
	lda	#$48
	sta	$2006	;Point to 1st screen
	ldy	#$01	;Start at text OR option
	
	lda     <MPR_Opt	;Extra options?
	beq	.no_extra_spc2
	lda	#$20	;Space character
	sta	$2007	;Write a space just to cover up ugly stuff.
	iny		;skip option
.no_extra_spc2:

.write_txt2:
	lda	[RDADD],Y 	;Write mapper description
	sta	$2007
	iny
	cpy	#$10
	bne	.write_txt2
	rts






Strobe_Cont:
	lda	<CONT1
	sta	<CONT1LAST

checkcont1:	ldx	#$01
	stx	$4016
	dex
	stx	$4016			;Strobe controller.


       	LDY   #$08
.readcontlp:
	PHA
	LDA   $4016
	STA	<CONT1
	LSR     A
	ORA	<CONT1
	LSR     A
	PLA
	ROL     A
	DEY
	BNE   .readcontlp
	sta	<CONT1

	lda	<CONT1LAST
	eor	#$FF
	and	<CONT1
	sta	<CONT1STROBE		;Save strobe.
	rts
;=============================

Init_Clear_PPU:
	lda	#$20
	sta	$2006
	lda	#$00
	sta	$2006

	ldy	#$0B
	sty	$0001
	ldy	#$00
	sty	$0000
	lda	#$00    ;Space Character

cppu1	sta	$2007	;Clear 2 PPU NAM tables
	dey
	bne	cppu1
	dec	$0001
	bpl	cppu1

	lda	#$23		;Point to attribute table
	sta	$2006
	lda	#$C0
	sta	$2006

	ldy	#$00
.att_loop
	lda     Attribs,Y
	sta	$2007
	iny
	cpy	#$40
	bne	.att_loop
	
	jsr	Draw_Fancy_Screen	;Draw our 1st screen

	lda	#$3F		;Point to Palette
	sta	$2006
	ldx	#$00
	stx	$2006
.pal_loop:
	lda	Fancy_Palette,X
	sta	$2007
	inx
	cpx	#$20
	bne	.pal_loop
	rts

;-----------
Write_Dmp_Pal:
	lda	#$3F		;Point to Palette
	sta	$2006
	ldx	#$00
	stx	$2006
.pal_loop:
	lda	Palette,X
	sta	$2007
	inx
	cpx	#$10
	bne	.pal_loop
	rts

	
Relocate_Code:				;The code to be relocated!
	incbin "RAM.bin"
 ;	.end			;End of Relocated code!!!!!
;Code_Loc = Relocate_Code+RAM_Code

	.org $FFFA
	.dw NMI_SUB
	.dw Reset_SUB
	.dw IRQ_SUB
	
	.bank 4		;OK??
	.org $0000
	incbin "Gfx\FONT4k.CHR"
	incbin "Gfx\Soundwave.chr"