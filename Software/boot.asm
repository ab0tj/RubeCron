; RubeCron Boot Routine
; meant to be stored in bank0 of the flash

.hexfile boot.hex
.binfile boot.bin
.download hex

.org 0         ; after reset, 0-7fff is rom, 8000-ffff is ram
startmem                equ 08000h
endmem                  equ 0ffffh

stack   equ endmem - 010h ; stack pointer

; i/o ports
banksel		equ 40h		; bank select port
i8155_cmdport	equ 00h		; 8155 command/status register
i8155_portc	equ 03h		; user switches are here
usb_data        equ 30h         ; usb module
usb_stat        equ 03h         ; 8155 port c
ide_ctl		equ 10h		; IDE control signals port

; constants
i8155_cmd	equ 40h		; stop timer, all ports input, interrupt disabled

; Bit Masks
usb_rx_rdy     equ 20h
usb_tx_rdy     equ 10h

; ascii
BS equ 08h
CR equ 0Dh
LF equ 0Ah
SPC equ 020h
CtrlC equ 03h


; CODE STARTS HERE
; set ide register to idle state
	xra a 			; a=0
	out ide_ctl
	
; initialize the 8155
	mvi a, i8155_cmd	; set command
	out i8155_cmdport	; send to 8155 chip

	in i8155_portc		; read port c
	cma			; 0=on (switch pulls to ground)
	ani 03h			; mask out just the bank switches
	
	lxi h, startmem		; boot code will go here

	ana a			; switches set to 0?
	jz programmer		; go to flash programming code
	
; place code in ram to switch banks and start execution from that bank
; can't run this from rom because it will disappear when switching banks
	mvi m, 0d3h		; 'out' opcode
	inx h			; hl++
	mvi m, banksel		; set bank to switch value
	inx h			; hl++
	mvi m, 0c3h		; 'jmp' opcode
	inx h			; hl++
	mvi m, 0		; low addr
	inx h			; hl++
	mvi m, 0		; high addr
	jmp startmem		; run code we just placed in ram

programmer: ; put flash programming code into ram and run it
	lxi b, programmer_end	; finish copying here
	lxi d, programmer_start	; start copying here

programmer_loop: ; copy the bytes
	ldax d			; get a byte
	mov m,a			; store in ram

	mov a,d			; done copying?
	cmp b
	jnz programmer_loop_inc	; no, increment pointers and keep copying
	mov a,e
	cmp c
	jz startmem		; yes, jump to the code we copied

programmer_loop_inc:
	inx d			; increment de
	inx h			; increment hl
	jmp programmer_loop	; go for another byte

; flash programming code follows
programmer_start:
	os equ startmem-.	; offset for jumps
        lxi sp, stack   	; initialize the stack pointer

prompt:	; ask user what bank we will be writing to
	lxi h, os+bank_str
	call os+usb_in_prompt
	sui 30h			; ascii adjust
	cpi 4
	jnc os+prompt		; check input
	mov d,a			; save bank number
	
erase_p: ; make sure the user knows what they are doing
	lxi h, os+confirm_str	; ask
	call os+usb_in_prompt
	cpi 'y'			; yes?
	jz os+erase		;   go!
	cpi 'n'			; no?
	jz os+prompt		;   try again
	jmp os+erase_p		; user didn't enter y or n

erase: ; erase the entire bank
	mov a,d			; d=bank number
	out banksel		; set bank

	lxi h, os+erase_str
	call os+usb_out_str	; "Erasing"

	xra a			; a=0
	mov h,a			; h=0
	mov l,a			; l=0

erase_loop: ; tell the chip to erase a sector
	mvi a, 0aah		; send
	sta 5555h		;  the
	mvi a, 55h		;  sector
	sta 2aaah		;  erase
	mvi a, 80h		;  command
	sta 5555h
	mvi a, 0aah
	sta 5555h
	mvi a, 55h
	sta 2aaah
	mvi m, 30h
	
	mvi c, '.'		; show progress
	call os+usb_out

erase_wait: ; wait for the sector erase to complete
	mov a,m			; read the byte at hl
	ani 80h			; mask out "DQ7"
	jz os+erase_wait	; DQ7 will be 0 during erase operation

	; erase done, start next sector
	mov a,h			; get current sector number
	adi 10h			; increment it
	mov h,a			; save it
	cpi 80h			; last sector?
	jnz os+erase_loop	; keep going if not.

; ready to write new data
	lxi h, os+send_str	; "Ready..."
	call os+usb_out_str

memin_intel: ; read data from the console, write it to flash
        call os+usb_out_crlf

memin_intel_start:
        call os+usb_in
        cpi ':'                 ; start code
        jz os+memin_intel_1
        cpi CtrlC               ; user wants to quit?
        jz os+prompt		;   then go back to start
        jmp os+memin_intel_start

memin_intel_1: ; get first part of header
        call os+usb_in_hex        ; byte count
        mov b, a
        call os+usb_in_hex        ; address high
        mov h, a
        call os+usb_in_hex        ; address low
        mov l, a

	mov a,h			; check address
	cpi 80h			; make sure it is in lower 32k
	jc os+memin_intel_2	; keep going if so

	lxi h, os+rangeerr_str	; out of range message
	call os+usb_out_str
	jmp os+memin_intel		; wait for next record

memin_intel_2: ; get second part of header
        mov a, b
        add h
        add l                   ; start of checksum
        mov e, a

        push d			; usb_in_hex clobbers e
        call os+usb_in_hex        ; record type
        pop d
        ana a                   ; 0=data
        jz os+memin_intel_3
        cpi 1                   ; 1=eof
        jz os+memin_intel_eof
	lxi h, os+unsupp_str	; unsupported message
	call os+usb_out_str
        jmp os+memin_intel 	; not supported, wait for another record

memin_intel_3: ; get and program data
        mov a, b		; check byte counter
        ana a
        jz os+memin_intel_cksum    ; b=0, checksum comes next
        push d                  ; e will get clobbered by input
        call os+usb_in_hex
        pop d
        push a                  ; save byte

	mvi a, 0aah		; send
	sta 5555h		;   the
	mvi a, 55h		;   program
	sta 2aaah		;   command
	mvi a, 0a0h
	sta 5555h

	pop a			; get the data byte again
	mov m,a			; program the byte
	mov c,a			; save for the wait that follows

program_wait: ; wait for the program operation to finish
	mov a,m			; read current byte
	cmp c			; check if programming has finished
	jnz os+program_wait	; loop until it finishes

        add e                   ; add to checksum
        mov e, a
        inx h                   ; increment pointer
        dcr b                   ; decrement counter
        jmp os+memin_intel_3       ; next byte

memin_intel_cksum:
        push d                  ; input clobbers e
        call os+usb_in_hex        ; get checksum
        pop d
        add e
        jz os+memin_intel          ; checksum is good, get next record
        lxi h, os+ckerr_str
        call os+usb_out_str       ; show error on console
        jmp os+memin_intel         ; get next record

memin_intel_eof: ; done!
        call os+usb_in_hex        ; checksum

	lxi h, os+done_str		; tell the user
	call os+usb_out_str	;   that we're done
	hlt			;   and halt

; hardware support routines
usb_in_prompt: ; ask the user a question
	call os+usb_out_str	; prompt string address in hl
	call os+usb_in		; get the answer
	mov c,a			; echo it
	call os+usb_out
	ret			; answer in a and c at this point

usb_in_hex: ; get a byte from the console in ascii hex
        call os+usb_in_hexnib     ; get high nibble
        ral
        ral
        ral
        ral                     ; shift to the high nibble
        ani 0f0h                ; mask off low nibble
        mov e, a                ; store the first nibble
        call os+usb_in_hexnib     ; get low nibble
        ora e                   ; combine high and low nibble
        ret                     ; return with byte in a

usb_in_hexnib: ; get a single hex digit from the console, return in a
        call os+usb_in            ; get digit from console
        cpi CtrlC               ; ctrl-z?
        jz os+prompt               ; bail if so
        cpi 061h                ; lowercase?
        jc os+hexnib1              ; nope
        sui 20h                 ; lower -> upper
hexnib1:
        mov c, a                ; save ascii version
        call os+hexbin            ; convert to bin
        out 0ffh                ; DEBUG
        cpi 010h                ; carry if 0-F
        jnc os+usb_in_hexnib      ; try again
        mov d, a                ; a will get clobbered by usb_out
        call os+usb_out           ; echo
        mov a, d
        ret

hexbin: ; convert ascii hex digit to binary nibble in a
        cpi 041h                ; >=A?
        jc os+hexbin1             ; <A.
        sui 7                   ; correct for A-F
hexbin1:
        sui 030h                ; get binary value
        ret

usb_out_crlf: ; newline without clobbering de
        mvi c, CR
        call os+usb_out
        mvi c, LF
        call os+usb_out
        ret

usb_out_str: ; send string at hl to the console
        mov a, m                ; get a byte to send
        ora a                   ; is it null?
        rz                      ; we're done if so
        mov c, a
        call os+usb_out           ; send this byte
        inx h                   ; increment the pointer
        jmp os+usb_out_str        ; loop until we find a null


usb_out: ; send byte in c to the console
        in usb_stat             ; check fifo status
        ani usb_tx_rdy
        jnz os+usb_out             ; loop until we can send this byte
        mov a, c
        out usb_data           ; send the byte
        ret

usb_in: ; get a byte from the console, store in a
        in usb_stat             ; check fifo status
        ani usb_rx_rdy
        jnz os+usb_in              ; loop until a byte comes in
        in usb_data            ; get the byte
        ret

; strings (null terminated)
crlf_str: db CR,LF,0
bank_str: db CR,LF,"Bank to program (0-3)? ",0
confirm_str: db CR,LF,"About to erase. Are you sure (y/n)? ",0
erase_str: db CR,LF,"Erasing",0
send_str: db CR,LF,"Ready. Send data now.",0
ckerr_str: db " cksum err",0
rangeerr_str: db " address >32K",0
unsupp_str: db " unsupported",0
done_str: db CR,LF,"EOF.",0

programmer_end equ .-1