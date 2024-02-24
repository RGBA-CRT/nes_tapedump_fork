
;Chris Covell's Poor-Man's Dumper (Mark V?  VI?) -- Tape Saver!
;This code runs entirely in the FC's (tight) RAM.

PAL = 0		;Set PAL = 1 to assemble it for European (PAL) NES hardware.

;0000-007F:	My variables
;0080-00FF:	CopyNES mapper variables *****
;0100-013F:	Stack (should be OK?)
;0140-01FF:	My code (part 1!)
;0200-021A:	CopyNES routine call table ****
;021B-03FF:	My code (part 2!)
;0400-06FF:	CopyNES Mapper & Dumping code ****
;0700-07FF:	CopyNES Scratch RAM

;       CopyNES standard routines at .org 0400h
COPYNES  EQU $0400
all_dump EQU COPYNES	;All dumping start
hdr_dump EQU COPYNES+3	;send iNES header
prg_dump EQU COPYNES+6  ;send PRG only
chr_dump EQU COPYNES+9	;send CHR only
chk_wram EQU COPYNES+12	;Check, then I Dump SRAM/WRAM
;	     jmp	mpr_end		;set PRG bank   ;These 3 are unused for now.
;	     jmp	mpr_end		;set CHR bank
;	     jmp	mpr_end		;send CRCs???


DEFLEN EQU	$10	;$1000 (4K bytes for now)
DEFADD EQU	$C0	;$C000 starting point

inesHDR EQU	$60	;$60-$6F is the iNES header for sending
crc0:        equ $80    ;CRC for CopyNES (now just a 16-bit checksum)
crc1:        equ $81
crc2:        equ $82
crc3:        equ $83

	ZP
RESV_BLANK	ds 16	;Reserve bytes for non-use
PPU2005V	ds 1
PPU2005H	ds 1
PPU2001		ds 1
PPU2000		ds 1

CONT1		ds 1                  ;0-> A,B,SL,ST,U,D,L,R <-7
CONTROLLER2 	ds 1
CONT1LAST	ds 1		;Reading of the last controller
CONT2LAST	ds 1
CONT1STROBE	ds 1
CONT2STROBE	ds 1
NMI_PASS	ds 1		;Has NMI passed?
VBCOUNT		ds 1
UPDATEDELAY	ds 1
DEBUG		ds 1		;Used for debugging purposes
LOOP_CN1	ds 1		;A loop counter for various things
LOOP_CN2	ds 1
LOOP_CN3	ds 1
;---------------------
RDADDR          ds 2    ;The current address to read
RDLength	ds 2	;How many bytes to send
SNDSPEED	ds 1	;How fast to send data 0..4
MODE		ds 1	;Mode the program is in...
			;8 = normal, 300 baud.
			;4 = double, 600 baud.
			;2 = quadruple, 1200
SHOWADD		ds 1	;If 1, print address 1st.

PRGBANK		ds 1	;PRG Bank number for games with mappers
CHRBANK		ds 1	;CHR Bank number for games with mappers
;--
PPUADDR		ds 2	;Pointer for a PPU writing function
VBROUTINE	ds 1	;Enumerated list for routines triggered in VBlank
VBADDR		ds 2	;Storage of address for above routine

;------------------------- Set the frequencies for NTSC or PAL NES units
 IF (PAL=0)
HIFREQ	EQU	$46	;A loop setting for 2400hz in NTSC
LOFREQ	EQU	$91	;A loop setting for 1200hz in NTSC
SHIFREQ EQU      $20     ;Super-hi-frequency? (try 21hi,48lo... or 20hi,45lo)
SLOFREQ EQU	$45 ;(ok?)
SDHIFREQ EQU     $0c     ;Super-Duper-hi-frequency? (0chi,1dlo... or...)
SDLOFREQ EQU	$1d ;(ok?)
 ELSE
HIFREQ	EQU	$40	;A loop setting for 2400hz in PAL
LOFREQ	EQU	$85	;A loop setting for 1200hz in PAL
SHIFREQ EQU      $1E     ;Super-hi-frequency.  These probably need adjusting!
SLOFREQ EQU	$40 ;(ok? Or, $41?)
SDHIFREQ EQU     $0B     ;Super-Duper-hi-frequency.
SDLOFREQ EQU	$1B ;(ok?)
 ENDIF
 
WAVE_MIN_LO EQU $00     ;Defines the waveform shape for standard dump.
WAVE_MAX_LO EQU $7F
WAVE_MIN_SDHI EQU $13     ;Defines the waveform shape for the highest speed.
WAVE_MAX_SDHI EQU $56	;Due to the frequency response of the NES, high-frequency
			;waves are muted too much, so we need this...
;------------------------------------------------------------------------
        CODE
	LIST

	

;=============================
        .org $0140
RAM_Code_Start:                 ;Start of code; init global variables here!
;	lda	#$04
;	sta	<MODE           ;Put into 600 baud mode...
;	lda	#DEFADD		;Get default starting address
;	sta     <RDADDR+1
;	lda	#$00
;	sta	<RDADDR
;	sta	<PRGBANK	;Reset banks
;	sta	<CHRBANK
	lda	#$80
	sta	<CONT1		;Mask 1st press of A button

restart_pos:          		;Restart position (don't clear variables...)

	lda	#%00000000		;NO NMI,Sprite CHR
	sta	$2000
	sta	<PPU2000
	sta	$2001
	sta	<PPU2001

	lda	#$21		;Start with a nice light blue
	jsr     SetPal

	lda	#$00
	sta	<PPU2005H
	sta	<PPU2005V
;	lda	#%00000000		;NMI,Sprite CHR
	sta	$2000
	sta	<PPU2000
	
	sta	<SNDSPEED
	jsr	Change_Speed		;Set our baud rate

	lda	#%00001000		;Sprites off,Screen on, clipping.
	sta	$2001
	sta	<PPU2001
	lda	#$10
	sta	$4015			;Enable Digital Channel?
	lda	#$3F			;centreline?
	sta	$4011

	lda	<PPU2000
	sta	$2000		;Change PPU settings
	lda	<PPU2005H	;Update scrolling, at least
	sta	$2005
	lda	<PPU2005V
	sta	$2005

;@@@@@@@@@@@@@@ MAIN LOOP @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
waitvbA:
	lda	$2002		;Wait for VBlank to end
	bmi	waitvbA
	

waitvbB: 	;First, occupy DMA for about 10000+ cycles, for crash protection
        ;A slight delay and busywork.  Thanks *Blargg*!
	lda #$01
        ldy #24
sprdm_loop:	;sta $4011
	sta $4014
        eor #$10
        dey
        bne sprdm_loop


	lda	$2002		;Wait for VBlank to start again
	bpl	waitvbB
;---------------------------- This waits 1 VBlank, right? ----------------
	inc	<VBCOUNT	;Increase a VBlank counter
	jsr	Check_Controller
	jmp	waitvbA                ;Go back to main loop!
;@@@@@@@@@@@@@@ MAIN LOOP @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;----------------------------------------------------------------------------
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
;=============================================
Send_a_Byte: 	;Sends 8 bits serially, LSB first
	sta     <LOOP_CN2	;Save A temporarily
	txa     ;Save our X reg
	pha
	tya
	pha
	
	lda	#$08		;8 loops
	sta	<LOOP_CN3	;Counter 3!
	lda	<LOOP_CN2	;Get A again... bummer

	jsr	Send_0          ;"Start" bit
;	ldx	#$08        	;A and X are clobbered!
.shftlp:
	lsr	a
	bcs	.to1
	jsr	Send_0
	bcc	.to0
.to1:	jsr	Send_1
.to0:	dec	<LOOP_CN3 ;dex
	bne	.shftlp
	jsr	Send_1		;2 or more "Stop" bits
	jsr	Send_1

	pla
	tay
	pla
	tax
	lda	<LOOP_CN2	;Get A again... bummer
	rts
;------------

;@@@@@@@@@@@@@@

        .org $01FA              ;These are shared routines for the CopyNES dumper
send_hdr:    jmp Send_ines_hdr  ;Send iNES header as audio
send_ldr:    jmp Send_Leader	;Send 5 sec tone (for CopyNES to use)
send_byte:   jmp Send_a_Byte	;.equ 0200h
baton:	     jmp Nothing        ;Old LCD code...  --> remove it!!!
chk_vram:    jmp Nothing	;(for now) .equ 0206h --> Move into each mapper?
dm_wram:    jmp Dump_SRAM	; .equ 0209h   The code here simply dumps $6000+
wr_ppu:      jmp my_wr_ppu	;.equ 020ch

read_byte:   jmp Nothing	;.equ 020fh --> Remove it!
init_crc:    jmp my_init_crc	;.equ 0212h --> Change to simple checksum
do_crc:      jmp my_do_crc	;.equ 0215h
finish_crc:  jmp Nothing	;.equ 0218h	;This really is nothing!

;============ SUBROUTINES!! ============
;=============================================================================

Check_Controller:
	lda	<VBCOUNT
	asl	a
	asl	a
	asl	a
	and	#$80		;Isolate upper bit
	ora	<PPU2001	;Make it our BLUE emphasis bit.
	sta	$2001
;**********************
	jsr	Strobe_Cont	;Does the technical stuff.

	lda	<CONT1STROBE
	and	#$01
	beq	.NotRPressed
			;Right Pressed! - Dump PRG only
	jsr     scr_black

	jsr	prg_dump	;Jump to CopyNES dumping code

	lda	#$2A		;Green!
	jsr     SetPal
;------------------
.NotRPressed
	lda	<CONT1STROBE
	and	#$02
	beq	.NotLPressed
				;Left Pressed - Do a simple WRAM/SRAM dump
	jsr     chk_wram	;$040C in each CopyNES mapper
;---------
.NotLPressed
	lda	<CONT1STROBE
	and	#$04
	beq	.NotDPressed
			;Down Pressed
;-----------
.NotDPressed
        lda	<CONT1STROBE
	and	#$08
	beq	.NotUPressed
			;Up Pressed
;--------------
.NotUPressed
	lda	<CONT1STROBE
	and	#$80
	beq	.NotAPressed
			;A Pressed -- Cycle through baud rates!
	lda	#$00
	sta	<VBCOUNT 	;Reset our Colour Emphasis flash...

	inc <SNDSPEED
	lda <SNDSPEED
	cmp #5		;0..4 are allowed
	bne .no_rset_spd
	lda #$00
	sta <SNDSPEED
.no_rset_spd:
	jsr Change_Speed   ;Change baud variables
;	tax
	lda	SpdPal,X       ;Palette for Speed choice
	jsr	SetPal

;---------
.NotAPressed
	lda	<CONT1STROBE
	and	#$40
	beq	.NotBPressed
	
;			;B pressed  -- Send Header & Vectors as sanity check
	jsr     scr_black
	jsr	hdr_dump         ;in CopyNES code: determines header, sends it.
	ldx	#$D0
.veclp:	lda	$FF00,X		;Get $FFD0-FFFF
	jsr	Send_a_Byte
	inx
	bne    .veclp
	lda	#$2A		;Green!
	jsr     SetPal
;----------------
.NotBPressed
	lda	<CONT1STROBE
	and	#$20
	beq	.NotSELPressed
			;Select Pressed, send all 8K CHR to audio output!
	jsr     scr_black

	jsr	chr_dump	;Jump to CopyNES dumping code
	
	lda	#$2A		;Green!
	jsr     SetPal
;------------------
.NotSELPressed
        lda	<CONT1STROBE
	and	#$10
	beq	.NotSTAPressed
				 ;Start pressed!  Dump EVERYTHING.
	jsr     scr_black

	jsr	all_dump	;Jump to CopyNES dumping code

	lda	#$2A		;Green!
	jsr     SetPal


.NotSTAPressed
	rts


;------------------
Change_Speed;		;Set our baud rate
	ldx <SNDSPEED	;Get our sending rate
	lda mode_tbl,X  ;Get speed divisor
	sta <MODE
	lda HSpd_Tbl,X  ;Get high frequency variable
	sta HIFBYT+1    ;Modify the code
	lda LSpd_Tbl,X  ;Do the same for the low-freq variable
	sta LOFBYT+1
	lda Wavemax_tbl,X   ;Change waveform amplitude for higher freqs!
        sta Snd0_L1+1       ;Modify our "Send 0" subroutine!
        lda Wavemin_tbl,X
	sta Snd0_L2+1
	     rts
HSpd_Tbl: .db HIFREQ,HIFREQ,HIFREQ,SHIFREQ,SDHIFREQ
LSpd_Tbl: .db LOFREQ,LOFREQ,LOFREQ,SLOFREQ,SDLOFREQ
Wavemin_tbl: .db WAVE_MIN_LO,WAVE_MIN_LO,WAVE_MIN_LO,WAVE_MIN_LO,WAVE_MIN_SDHI
Wavemax_tbl: .db WAVE_MAX_LO,WAVE_MAX_LO,WAVE_MAX_LO,WAVE_MAX_LO,WAVE_MAX_SDHI
mode_tbl: .db 8,4,2,2,2
SpdPal:   .db $00,$10,$20,$27,$16


Delay_Hifreq:
HIFBYT:	ldx	#HIFREQ	;#$46 for NTSC High freq (2400 Hz) +- 15Hz :-{
.dlp1:	dex
	bne    .dlp1
	rts
	
Delay_Lofreq:
LOFBYT:	ldx	#LOFREQ	;#$91 for NTSC low freq (1200 Hz)
.dlp1:	dex
	bne    .dlp1
	rts
Nothing: rts

;**********************************************


Send_1:	pha
;	txa	;Not needed, as upper-level routines don't depend on X or Y anymore...?
;	pha
;	tya
;	pha
	
	ldy	<MODE		;Contains how many loops for a "1"

.lp1:	lda	#$7F
	sta	$4011
	jsr	Delay_Hifreq
	lda	#$00
	sta	$4011
	jsr	Delay_Hifreq
	dey
	bne	.lp1
	lda	#$3F			;centreline?
	sta	$4011

;	pla
;	tay
;	pla
;	tax
	pla
	rts

Send_0: pha
;	txa            	;Not needed
;	pha
;	tya
;	pha
	
	lda	<MODE		;Contains how many loops for a "1"
	lsr	a		;Cut in half for a "0"
	tay

Snd0_L1: lda	#$7F	;Change back to 7F!!
	sta	$4011
	jsr	Delay_Lofreq
Snd0_L2: lda	#$00
	sta	$4011
	jsr	Delay_Lofreq
	dey
	bne	Snd0_L1
	lda	#$3F			;centreline?
	sta	$4011                   ;This helps correct the waveforms, apparently!

;	pla
;	tay
;	pla
;	tax
	pla
	rts

Send_Leader:
	pha
	txa
	pha
	tya
	pha
	
	lda	#$40             ;$4000 loops = 7 secs or so.
	sta	<LOOP_CN1	;$3000 loops, over 5 seconds
	lda	#$00
	sta	<LOOP_CN3
.lp0:	;ldy	#$00

.lp1:	lda	#$7F
	sta	$4011
	jsr	Delay_Hifreq
	lda	#$00
	sta	$4011
	jsr	Delay_Hifreq
;	dey
	dec	<LOOP_CN3
	bne	.lp1
	dec	<LOOP_CN1
;	lda	<LOOP_CN1
	bne	.lp0 
	lda	#$3F			;centreline?      ;Remove, possibly?
	sta	$4011

	pla
	tay
	pla
	tax
	pla
	rts



;============================================================================
Send_ines_hdr		;Send 16-byte iNES header in audio stream.
	txa
	pha
	
	ldx	#$00
.lp:	lda	<inesHDR,X
	jsr	Send_a_Byte
	inx
	cpx	#$10
	bne	.lp
	
	pla
	tax
	rts


;=========================================
Dump_SRAM:	;Simply copy $6000-$7FFF for our prog.

	jsr	Send_Leader
	ldy	#$00
	sty     <RDADDR
	lda	#$60
	sta	<RDADDR+1
.cpy_sram_lp:
	lda	[RDADDR],Y	;Copy from selected mapper to $0400
	jsr	Send_a_Byte
	inc	<RDADDR
	bne	.no_upd_rd
	inc	<RDADDR+1
.no_upd_rd:
	lda     <RDADDR+1
	cmp	#$80		;Has reached $8000.
	bne     .cpy_sram_lp
	
	lda	#$2A    	;Green means OK
.dmped:	jsr     SetPal
	rts

;=============================
my_init_crc: lda #0
             sta <crc0
             sta <crc1
             sta <crc2
             sta <crc3
             rts

my_do_crc:   clc
	     adc <crc0        ;Just a simple 16-bit checksum
             sta <crc0
             lda <crc1
             adc #0
             sta <crc1
             rts
;
;Fill_Byte: 	;Fills PPU VRAM with byte A for Y,X loops
;	cpx     #$00
;	bne	.noadjY
;	dey		;This is a KLUDGE to account for X=0
;.noadjY:
;	sta	$2007
;	dex
;	bne	.noadjY
;	dey
;	bpl	.noadjY
;	rts
;@@@@@@@@@@@@@@@@@@

;---------------------------


SetPal:		;Updates the palette # in A
	ldx	#0		;Screen off
	stx	$2001
	ldx	#$3F
	stx	$2006
	ldx	#$00
	stx	$2006	;Point to Palette
StPlDrct: sta	$2007
	
	ldx #$20
	stx $2006
        ldx #$00
        stx $2006
;        ldx #$00
        stx $2005
        stx $2005

	ldx	<PPU2001	;Screen on?
	stx	$2001
	rts

	
my_wr_ppu:      sta $2006
             lda #0
             sta $2006
             rts
        ;----------

scr_black:
	lda	#$0F		;Black
	jsr     SetPal
	lda	#$00
	sta	$2001
	rts
;=======================================================
;
;Init_CHR:
;
;	lda	#$00
;	sta	$2006
;	sta	$2006		;Point to CHR table
;	ldy	#$24		;$2400 times, hopefully...
;	ldx	#$00
; ; 	lda	#$00
;	jsr     Fill_Byte   	;CHR fill *** & NAM & ATT fill too! ***
; ;--------------------------
;	lda	#$0F
;	sta	$2006
;	lda	#$D0
;	sta	$2006		;Point to CHR table
;	ldy	#$00		;8 times, hopefully...
;	ldx	#8
;  	lda	#$FF
;	jsr     Fill_Byte   	;2nd part of CHR fill
;;-----------------------------------
; 	lda	#$0F
;	sta	$2006
;	lda	#$E8
;	sta	$2006		;Point to CHR table
;	ldy	#$00		;24 times, hopefully...
;	ldx	#24
;  	lda	#$FF
;	jsr     Fill_Byte   	;last part of CHR fill
;	rts

;===================================================================
;(End of program)

;===================================================================
;========================= ORG $400 Bankswitch & dumping routines! ===========
;	.org $400		;Bankswitching routines go here
;Change_PRG_Bank:	jmp	prg_bank_code		;a small jump table
;Change_CHR_Bank:        jmp	chr_bank_code


	.org $3FD
	DB "Hi!"