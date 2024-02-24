             .title        "FDS Disk Reader Plugin"

;09/24/2011
;Written by Chris Covell
;Version 1.0

             ;vectors for standard system calls

inesHDR      .equ 0060h	;$60-$6F is the xixNxExSx FDS header for sending
send_hdr:    .equ 01FAh        ;Send ...FDS header as audio
send_ldr:    .equ 01FDh		;Send 5 sec tone (for CopyNES to use)
send_byte:   .equ 0200h

temp_buf:    .equ 00d0h		;Use $D0-$DF for our file header buffer!

temp1:       .equ 00e0h
temp1_lo:    .equ 00e0h
temp1_hi:    .equ 00e1h
temp2:       .equ 00e2h
temp2_lo:    .equ 00e2h
temp2_hi:    .equ 00e3h
temp3:       .equ 00e4h
temp3_lo:    .equ 00e4h
temp3_hi:    .equ 00e5h
dummy_lim:   .equ 00e6h		;The last saved limit for FDS file skipping
dummy_cur:   .equ 00e7h         ;The current file number for each round
dummy_flag:  .equ 00e8h		;If this flag is $80, loop, (skip) and re-read disk
total_written: .equ 00e9h	;Total disk contents dumped (should add up to 65500)
raw_dump:    .equ 00EBh		;80 = dump raw only (no filling up end of disk)
hdr_write:   .equ 00ECh		;0 at first entry, 1=write FDS header, 2+=do nothing.
bigfile:     .equ 00EDh		;A flag that a BIG file has been read (kludge)
; ----- FDS ADDRESSES! --------
StartXfer	.equ $e6e3
WaitForRdy	.equ $E64D
MilSecTimer	.equ $E153
CheckBlkType    .equ $E68F
XferByte        .equ $E7A3
XferDone	.equ $E778
EndOfBlkRead	.equ $E706
GetHCPwNWPchk	.equ $E3E7
GetNumofFiles	.equ $E484
GetDiskInfo	.equ $E32A

             .org 0400h
	     jmp	all_dump	;All dumping start (a proper .FDS file)
	     jmp	hdr_dump	;send FDS Disk listing
	     jmp	mpr_end        	;send PRG only
	     jmp	short_dump	;send xCxHxRx -- raw disk files only
	     jmp	mpr_end		;No SRAM dump...
;	     jmp	mpr_end		;set PRG bank   -- Unused for now, so remove!
;	     jmp	mpr_end		;set CHR bank
;	     jmp	mpr_end		;send CRCs???



;================================================================================
hdr_dump:    ;Determine info in the header and dump it.
	jsr	Test_Sanity	;Tests the presence & correct insertion of the RAM adaptor.
       ;***                      exits early if there's a problem
       
 	cli			;Enable interrupts. (Dangerous?)
	jsr	GetDiskInfo	;Call the disk info routine.
	.dw	$6000		;Parameter: Start of 32k RAM
	sei
;	cmp	#$00		;Not needed?
	bne     error_exit	;If A is non-zero, exit
;----- Got file list, determine size.

	jsr Reset_Pointers
	jsr Multiplyby9         ;Get the total size of the file list

	jsr send_ldr		;5-second minimum lead-in
	jsr Dump_Buffer	;Dump_Range          ;Dump from $6000-

	jmp good_exit
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;========================================
Test_Sanity	;Tests the presence & correct insertion of the RAM adaptor.
       ;***                      exits early if there's a problem
	lda     $E338
	cmp	#$46
	bne     to_error_exit 	;Check FDS RAM adaptor is inserted correctly
	lda	$E339
	cmp	#$E3
	bne     to_error_exit
	lda     $FFFC
	cmp	#$24
	bne     to_error_exit 	;Check FDS RAM adaptor is inserted correctly
	lda	$FFFD
	cmp	#$EE
	bne     to_error_exit
	jmp	FDS_HOOK                    ;Keep going if all OK

to_error_exit:
	pla
	pla		;Exits our test programs forcefully.
	ldy     #$15	;RED: Fatal Error (like no RAM adaptor)
	bne	good2	;Branch always
error_exit:
	ldy	#$38	;Yellow: Misc. Error (Bad FDS disk, drive not ready, etc.)
	bne	good2	;Branch always
good_exit:
	ldy	#$2A		;Green!  Finished/mostly OK
good2:
	lda	#0		;Screen off
	sta	$2001
	lda	#$3F
	sta	$2006
	lda	#$00
	sta	$2006	;Point to Palette
	sty	$2007

	lda	#%00001000
	sta	$2001

	pla 	;This skips the $FFD0-FFFF sending of my main prog.
	pla
	rts
;=============================




;================================================================================
short_dump:
	lda	#$01
	sta	hdr_write	;Once the header is read once, this'll equal 2 or 3.
				;So we don't dump the header in this mode!
	lda	#$80
	sta	raw_dump	;a raw dump.  This will send dumped files only.
	bmi	all_dump2	;Branch always...?
;================================================================================
all_dump:    ;Send ALL data!
	lda	#$00
	sta	raw_dump	;NOT a raw dump.  This will send 65500 full bytes.
        sta     hdr_write		;If 1, write header, otherwise, don't!
all_dump2:
	jsr	Test_Sanity	;Tests the presence & correct insertion of the RAM adaptor.
       ;***                      exits early if there's a problem
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      lda	#$00
        sta dummy_flag		;Flag to NOT advance the write pointer.
	sta dummy_lim           ;Upper limit for # of disk files
	sta dummy_cur           ;Current # of disk files
        sta total_written       ;Total bytes written out (has to add up to 65500... :^(
        sta total_written+1
        sta bigfile

alldump_loop:
	jsr Reset_Pointers

	cli			;Enable interrupts. (Dangerous?)
	jsr	My_GetDiskInfo 		;The whole subroutine that gets everything(?)
	.dw	$6000		;Start of 32k RAM
	sei

	cmp	#$00          ;(Cmp not needed?)    ;If routine returns 0, no errors in disk reading.
	beq	no_error_dumpall
	

	cmp	#$24                   ;If ErrorCode is $24 or higher, end of disk (basically)
	bcc     error_exit	;If Error code is $1-$23, exit (failed!)
;~~~~~~~~~~Error code was $24+ ~~~~~~ Try to dump everything up to end of last read file.
	lda	#$00
	sta     dummy_flag	;Make dummy flag 0, so this loop will terminate.
	
	

;--------------------- NOW DUMP OUR DATA AS AUDIO! ------------
no_error_dumpall:
     jsr XferDone	;Finish reading?
	sei

	jsr send_ldr		;5-second minimum lead-in
;.....................
	lda hdr_write           ;Get our header-writing flag
	cmp #$01		;It must be exactly 1!
	bne no_fwnes_header
	jsr send_hdr		;Send our header first.
no_fwnes_header:
;.....................


	bit	bigfile		;Was it one BIG file to read?
	bpl	no_write_extra_hdr	;If it was, inject the header here.
	jsr	inject_header
no_write_extra_hdr:


	jsr	Dump_Buffer	;Dump from $6000-> end of what was read.

	bit dummy_flag	;If flag is set, it was full last time, so need to loop.
	bmi alldump_loop
	

;::::::::::::::::::::::::::: Almost finished, now fill rest of the disk (or not)
	bit raw_dump	;if a raw dump, skip next part
	bmi skip_fillin
	;Now write remaining zero bytes to fill up 65500 bytes of a disk side.
	jsr fill_FDS_end
skip_fillin:
;:::::::::::::::::::::::::::
	jmp good_exit
;========================================











;======================= _My_ GetDiskInfo dumping routine =================
; Input parameter after call: File dumping buffer (prob. not needed.)
; Reads disk header files 01 and 02, then loops reading files 03 and 04.
; If a file is too large for remaining buffer space ($6000-$DFFF), sets a flag
; to dump starting with that file the next time round. Some files are at/near
; the 32KB size limit, so a special case routine has to handle rhat.
My_GetDiskInfo:
	LDA #$00
	JSR GetHCPwNWPchk	;get 1 16-bit pointer; put A in [$02]
	LDA $0101
	PHA
	JSR My_Get2
	PLA
	STA $0101
	TXA
	RTS
My_Get2: 		;$E346 in FDS BIOS.
	jsr     My_StartXfer
;	LDA $00;STA $0A;LDA $01;STA $0B
	LDA #$01
	jsr	ClyStoreA	;Store our block type here...
;	LDY #$00
	STY $02
	STY $03
;load next xx bytes off disk into RAM
ld_hd1:	JSR XferByte
	sta (temp2),y
	INY
;++++++++++++++++++ FWNES header provision.
	cpy #$15	;Is it reading the disk side #?
	bne not_disk_side
	sec 		;SEC!!  This should add 1 each time.
	adc hdr_write   ;The value in A is the side #.  It'll either be 0 or 1
	sta hdr_write   ;If it was 0, a later function will write the FWNES FDS header
			;If it was 1, then no header for disk side B
			;hdr_write gets increased every time this function passes,
			;so the header should get written only after the 1st pass.
not_disk_side:
;++++++++++++++++++
	CPY #$37
	BNE ld_hd1
	jsr Add_Y_temp2	;Add Y to temp2 pointer, clears Y
;get # of files
	JSR EndOfBlkRead
	jsr	TstDummy_ClearBuf

	lda	#$02
	JSR GetNumofFiles		;stores it in [$06]
	lda	#$02
	jsr	ClyStoreA	;Store our block type here...
	LDA $06
	jsr	StoreA		;Store # of files. (I'll probably ignore it, tho.)
;	;	BEQ $E3CB;	branch if # of files = 0
	jsr	TstDummy_ClearBuf
;==================================
Read_Files_Loop:		;OUR MAIN DUMPING LOOP!
	lda	#$00
	sta	bigfile		;Our kludgy "BIG FILE" flag.

	lda dummy_lim
	cmp dummy_cur           ;Current # of disk files
	bne no_switch_on	;If limit = current, allow writing.
	lda #$00
	sta dummy_flag		;Flag to NOT advance the write pointer.
no_switch_on:

;get info for next file
	LDA #$03
	sta temp_buf		;Save in 1st byte of 16.
	JSR CheckBlkType        ;Get block type 3 next... hopefully?
	ldy #$01

ld_hd2:	JSR XferByte
	sta temp_buf,y		;Save to our buffer
	INY
	CPY #$10
	BNE ld_hd2
	
	JSR EndOfBlkRead	;Go to end of block?

;------------------- Check if there's room in our buffer
	lda temp_buf+$0D  ;File size low byte
	clc
	adc #$11	;Our 1st buffer size + 1 byte (block 04)
	sta temp3_lo
	lda temp_buf+$0E  ;File size high byte
	adc #$00
	sta temp3_hi

	lda temp3_lo	;Add our current pointer
	clc
	adc temp2_lo
	sta temp3_lo
	lda temp3_hi	;Add our current pointer
	adc temp2_hi
	sta temp3_hi    ;Get our predicted size.
	sec
	sbc #$60	;(could run over to $0000...)
	cmp #$80	;Will it run into the BIOS?
	bmi safedump
	jmp unsafedump
safedump:		;It's safe to load in the type 04 file.
	ldy #$0		;First, copy the header over.
ld_hd3:	lda temp_buf,y		;load from our buffer
	sta (temp2),y           ;save to our large buf.
	INY
	CPY #$10
	BNE ld_hd3
	jsr Add_Y_temp2	;Add Y to temp2 pointer, clears Y
;------------------------------------------- Read file type 04!
	lda	#$04
	JSR CheckBlkType        ;Get block type 4 next... hopefully?
	lda	#$04
	jsr	ClyStoreA	;Store our block type here...
	   			;Now read back what we dumped.
dump_4bf:
	JSR XferByte		;Get 1 byte
	jsr StoreA              ;Store it

	lda  temp2_hi          ;Is pointer 2 = pointer 3?
	cmp  temp3_hi
	bne  dump_4bf
	lda  temp2_lo          ;Is pointer 2 = pointer 3?
	cmp  temp3_lo
	bne  dump_4bf          ;Read from disk until pointer matches predicted.

	JSR 	EndOfBlkRead	;Go to end of block?

	inc 	dummy_cur	;Increase our current # of files read.
	
	bit     bigfile		;Was it a single LARGE file?
	bmi	avg_sized_file	;If so, don't loop anymore.

	jsr	TstDummy_ClearBuf	;Tests the dummy flag, clears the buffer pointer

	jmp	Read_Files_Loop		;Keep looping until full!

	lda	#$BE
	jsr	StoreA
	lda	#$EF
	jsr	StoreA

unsafedump:
;--------- No space left in buffer.  However, if our buffer pointer is at $6000 right now,
;          it means that a VERY large file had been found the first pass around, right?
;          So first, test if the buffer is $6000 (kludge).
;          If it is, we will read our large file ONLY (no header or 04 byte) to the buffer.
;           and tell the dumping program afterwards to insert the header before dumping.


	;If the buffer is not at $6000, then it had just read in an average-sized file.

	lda temp2_lo       	;Low pointer byte
	bne avg_sized_file      ;Must be 0
	lda temp2_hi
	cmp #$60                ;Must be 60
	bne avg_sized_file      ;If not, try to loop around again.

;------------	First, subtract the header size ($11 bytes)
        lda temp3_lo	;Get our end of file pointer
	sec
	sbc #$11        ;Remove $11 bytes
	sta temp3_lo
	lda temp3_hi
	sbc #$00
	sta temp3_hi    ;Get our predicted size. (could even run higher than $DFFF but who cares?)

        bit	dummy_flag 	;Are we meant to skip this file anyway?  (ie: a read pass AFTER the file)
	bmi	no_set_bigfile 	;If so, then we read the file, and buffer will get cleared anyway.
	lda	#$80
	sta	bigfile		;Our kludgy "BIG FILE" flag.
no_set_bigfile:
        lda	#$04
	JSR CheckBlkType        ;Get block type 4 next... hopefully?

	ldy	#$00
	jmp	dump_4bf	;Continue dumping, but just 1 file.
	

;------------
avg_sized_file:
	lda dummy_cur           ;Current # of disk files
	sta dummy_lim           ;Save it as the limit for next time.
	lda #$80
	sta dummy_flag		;Flag to NOT advance the write pointer.
	lda #$00		;Will loop back to start for next time.
	sta dummy_cur           

	lda	#$00		;No errors, really...?
	tax

	RTS



















inject_header:
	ldy	#$00
injct:  ;lda	#$58
	lda temp_buf,Y
        jsr send_byte      ;Send audio
        iny
        cpy	#$10
        bne     injct
        lda	#$04
        jsr     send_byte      ;Send File Type 04 after our Type 03 header.
        
        ;Still need to add #$11 to Total Dumped.
        lda total_written
	clc
	adc #$11
        sta total_written       ;Total bytes written out (has to add up to 65500... :^(
        lda total_written+1
        adc #$00
        sta total_written+1

	lda	#$00
	sta	bigfile		;No more large files.
	rts





;----------------
My_StartXfer:
	JSR WaitForRdy
	LDY #$c5
	JSR MilSecTimer;0.197 sec delay
	LDY #$46
	JSR MilSecTimer;0.070 sec delay
	LDA #$01
	JSR CheckBlkType
	RTS

TstDummy_ClearBuf:	;Tests the dummy flag, clears the buffer pointer
	bit dummy_flag	;If flag is set, clear our write pointer!
	bpl no_reset_pointer04
	lda	#$00
	sta     temp2_lo
	lda	#$60
	sta 	temp2_hi
no_reset_pointer04:
	rts




;=========================
Multiplyby9:			;# of bytes to send is: Disk_count*9 + 13 + $6000
	lda	#$00
	sta     temp2_hi	;Start with 0000
	lda	$600A		;Get disk file count
	sta     temp2_lo
	asl     temp2_lo
	rol     temp2_hi
	asl     temp2_lo
	rol     temp2_hi
	asl     temp2_lo
	rol     temp2_hi	;Multiply by 8
	lda	$600A		;Get disk file count
	clc
	adc     temp2_lo
	sta     temp2_lo	;Multiply count by 9
	lda	temp2_hi
	adc	#$00
	sta     temp2_hi
	lda	#13		;Add constant info
	clc
	adc     temp2_lo
	sta     temp2_lo
	lda	temp2_hi
	adc	#$60		;plus $6000
	sta     temp2_hi
	rts
;*******************

;Dump_Range:		;Dumps data from (temp2) to (temp2+temp1)
;			;sends it out the audio port
;dump_lst:     lda (temp2),y	;Get 1 byte
;             jsr send_byte      ;Send...
;             inc temp2_lo       ;Adjust pointers...
;             bne no_add_src
;             inc temp2_hi
;no_add_src:
;             dec temp1_lo	;Decrement our counter
;             bne dump_lst        ;If not zero, keep dumping
;             dec temp1_hi	;Decrement our high byte of counter
;             bpl dump_lst	;Dump if positive.
;             rts
;


Dump_Buffer:		;An improved? routine that dumps from (temp1) up to our old buffer (temp2).
        lda #060h      	;Dump from $6000 in RAM
        sta temp1_hi
	lda	#$00                    ;Now read back what we dumped.
	sta     temp1_lo
	tay

dump_loop0:                    ;It's safest to test equality beforehand!
	lda  temp1_hi          ;Is pointer 1 = pointer 2?
	cmp  temp2_hi
	bne  dump_1bf
	lda  temp1_lo          ;Is pointer 1 = pointer 2?
	cmp  temp2_lo
	beq  dump_finished	;If both pairs of bytes are equal, finished dumping.

dump_1bf:     lda (temp1),y	;Get 1 byte
             jsr send_byte      ;Send...
             inc temp1_lo       ;Adjust pointers...
             bne dump_loop0  	;If lower byte is 0, increase high byte (naturally)
             inc temp1_hi
	     bne dump_loop0	;Should be an unconditional branch (never wraps around to 00)

dump_finished:
	lda total_written
	clc
	adc temp2_lo
        sta total_written       ;Total bytes written out (has to add up to 65500... :^(
        lda total_written+1
        adc #$00
        sta total_written+1
        lda temp2_hi
        sec
        sbc #$60		;Subtract the source offset.
        clc
        adc total_written+1
	sta total_written+1     ;Add it to our total.
	rts
;---------------------------------



;***********************
fill_FDS_end:
	lda #$00
fill_to_end:
	ldx  total_written+1          ;Is it #$FFDC yet?
	
	txa


	cpx  #$FF
	bne  fill_write1
	ldx  total_written
	cpx  #$DC
	beq  fill_finished
fill_write1:
		jsr send_byte
             inc total_written       ;Keep counting up total bytes written.
             bne no_add_wrt
             inc total_written+1
no_add_wrt:
	jmp  fill_to_end
fill_finished:
	rts
;====================

ClyStoreA:	ldy #$00
StoreA:		;Saves A into (temp2) then increases the pointer
	     sta (temp2),y	;Save 1 byte
             inc temp2_lo       ;Adjust pointers...
             bne no_add_dst1
             inc temp2_hi
no_add_dst1:
	 rts

Add_Y_temp2:	;Adds Y to pointer to make up for local loops using Y.
	tya
	ldy #$00	;We should clear Y too.
	clc
	adc	temp2_lo
	sta	temp2_lo
	lda	temp2_hi
	adc	#0
	sta	temp2_hi
	rts
;**********************
Reset_Pointers:		;Sets the dumping length to (A)00, address to $6000, and clears Y.
	lda	#$00                    ;Now read back what we dumped.
	sta     temp1_lo
        sta 	temp2_lo
	tay		;Reset Y-index
        lda #060h      	;Dump from $6000 in RAM
        sta temp2_hi
	sta temp1_hi
        rts

;---------------------------
wr_ppu:      sta 2006h
             lda #0
             sta 2006h
             rts
        ;----------
FDS_HOOK        	;Hooks in values the FDS RAM adaptor needs.
	lda	#$00    ;NMIs off?
	sta	$0100
;	lda	#$35    ;Reset vecs off
	sta	$0102
;	lda	#$53
	sta	$0103
	
	sta	$2001	;Screen off.

	lda	#$80    ;On IRQ, stay in BIOS
	sta	$0101


;;	lda     #$F7   	;Load some vectors.  Does this help any?
;	sta	$DFF6   ;These point to the FDS' RTI.
;	sta	$DFF8
;	sta	$DFFA
;	sta	$DFFE
;	lda     #$E1
;	sta	$DFF7
;	sta	$DFF9
;	sta	$DFFB
;	sta	$DFFF

	rts

:----------
mpr_end:     rts            ;done
             .end
