; RubeCron Flash Programmer

include 'clockmon.inc'

org 9000h         ; after reset, 0-7fff is rom, 8000-ffff is ram

; CODE STARTS HERE
;main program
    ld sp, stack   ; initialize the stack pointer
	di				; don't want interrupts during this process

	xor a
	out (MMU_PG0), a	; page0 = rom page 0
	inc a
	out (MMU_PG1), a	; page1 = rom page 1

page:	; ask user what page we will be writing to
	ld hl, page_str
	call in_prompt
	sub 30h			; ascii adjust
	cp 8
	jp nc, page		; check input
	ld d,a			; save bank number
	
erase_p: ; make sure the user knows what they are doing
	ld hl, confirm_str	; ask
	call in_prompt
	cp 'y'			; yes?
	jp z, erase		;   go!
	cp 'n'			; no?
	jp z, page		;   try again
	jp erase_p		; user didn't enter y or n

erase: ; erase the entire bank
	ld hl, erase_str
	call outstr	; "Erasing"

	xor a			; a=0
	ld h,a			; h=0
	ld l,a			; l=0

erase_loop: ; tell the chip to erase a sector
	xor a
	out (MMU_PG0),a	; 0 for erase command

	ld a, 0aah		; send
	ld (5555h),a	;  the
	ld a, 55h		;  sector
	ld (2aaah),a		;  erase
	ld a, 80h		;  command
	ld (5555h),a
	ld a, 0aah
	ld (5555h),a
	ld a, 55h
	ld (2aaah),a

	ld a,d
	out (MMU_PG0),a	; set page
	ld (hl), 30h
	
	ld c, '.'		; show progress
	call outc

erase_wait: ; wait for the sector erase to complete
	ld a,(hl)			; read the byte at hl
	and 80h			; mask out "DQ7"
	jp z, erase_wait		; DQ7 will be 0 during erase operation

	; erase done, start next sector
	ld a,h			; get current sector number
	add a, 10h			; increment it
	cp 40h			; last sector?
	ld h,a			; update sector number
	jp nz, erase_loop		; keep going if not.

; ready to write new data
	ld hl, send_str		; "Ready..."
	call outstr

romin_intel: ; read data from the console, write it to flash
	call crlf

romin_intel_start:
	call chrin
	cp ':'                 ; start code
	jp z, romin_intel_1
	cp CtrlC               ; user wants to quit?
	jp z, page		;   then go back to start
	jp romin_intel_start

romin_intel_1: ; get first part of header
	call in_hex        ; byte count
	ld b, a
	call in_hex        ; address high
	ld h, a
	call in_hex        ; address low
	ld l, a

	ld a,h			; check address
	cp 40h			; make sure it is in lower 32k
	jp c, romin_intel_2	; keep going if so

	ld hl, rangeerr_str	; out of range message
	call outstr
	jp romin_intel		; wait for next record

romin_intel_2: ; get second part of header
	ld a, b
	add h
	add l                   ; start of checksum
	ld e, a

	push de			; in_hex clobbers e
	call in_hex        ; record type
	pop de
	and a                   ; 0=data
	jp z, romin_intel_3
	cp 1                   ; 1=eof
	jp z, romin_intel_eof
	ld hl, unsupp_str	; unsupported message
	call outstr
	jp romin_intel 	; not supported, wait for another record

romin_intel_3: ; get and program data
	ld a, b		; check byte counter
	and a
	jp z, romin_intel_cksum    ; b=0, checksum comes next
	push de                  ; e will get clobbered by input
	call in_hex
	pop de
	push af                  ; save byte

	xor a
	out (MMU_PG0),a	; 0 for program command

	ld a, 0aah		; send
	ld (5555h),a		;   the
	ld a, 55h		;   program
	ld (2aaah),a		;   command
	ld a, 0a0h
	ld (5555h),a

	ld a,d
	out (MMU_PG0),a	; program in correct page

	pop af			; get the data byte again
	ld (hl),a			; program the byte
	ld c,a			; save for the wait that follows

program_wait: ; wait for the program operation to finish
	ld a,(hl)			; read current byte
	cp c			; check if programming has finished
	jp nz, program_wait	; loop until it finishes

	add e                   ; add to checksum
	ld e, a
	inc hl                   ; increment pointer
	dec b                   ; decrement counter
	jp romin_intel_3       ; next byte

romin_intel_cksum:
	push de                  ; input clobbers e
	call in_hex        ; get checksum
	pop de
	add e
	jp z, romin_intel          ; checksum is good, get next record
	ld hl, ckerr_str
	call outstr       ; show error on console
	jp romin_intel         ; get next record

romin_intel_eof: ; done!
	call in_hex        ; checksum

	ld hl, done_str		; tell the user
	call outstr	;   that we're done
	jp 8000h			;   and return to monitor

; hardware support routines
in_prompt: ; ask the user a question
	call outstr	; page string address in hl
	call chrin		; get the answer
	ld c,a			; echo it
	call outc
	ret			; answer in a and c at this point

; strings (null terminated)
crlf_str: db CR,LF,0
page_str: db CR,LF,"Flash page to program (0-7)? ",0
confirm_str: db CR,LF,"About to erase. Are you sure (y/n)? ",0
erase_str: db CR,LF,"Erasing",0
send_str: db CR,LF,"Ready. Send data now.",0
ckerr_str: db " cksum err",0
rangeerr_str: db " address >16K",0
unsupp_str: db " unsupported",0
done_str: db CR,LF,"EOF.",0

end: