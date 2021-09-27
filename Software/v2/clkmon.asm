; RubeCron Clock + Monitor

.hexfile rubemon.hex
.binfile rubemon.bin
.download hex

.org 0h         ; after reset, 0-7fff is rom, 8000-ffff is ram
startmem                equ 08000h
endmem                  equ 0ffffh

stack   equ endmem - 010h ; stack pointer

; i/o ports
i8155_cmd	equ 00h		; 8155 command/status register
i8155_tc1	equ 04h 	; 8155 low byte of timer count
i8155_tc2	equ 05h		; 8155 high byte of timer count
usb_data        equ 30h         ; usb module
usb_stat        equ 03h         ; 8155 port c
uart_data	equ 20h		; uart data port
uart_stat	equ 21h		; uart status port
sec_led		equ 50h		; seconds display
min_led		equ 51h		; minutes display
hrs_led		equ 52h		; hours display
day_led		equ 53h		; day display

; Bit Masks
usb_rx_rdy	equ 20h
usb_tx_rdy	equ 10h
uart_rx_rdy	equ 02h
uart_tx_rdy	equ 01h

; hardware constants
tmr_tclow	equ 10h		; 4.9152MHz/2/16/16=9600baud
tmr_tchi	equ 40h		; square wave output
tmr_tmmode	equ 0c0h	; all ports input, start timer
uart_reset      equ 40h	        ; reset byte
uart_mode       equ 4Eh	        ; mode byte
uart_cmd_byte   equ 27h	        ; control byte

; ascii
BS equ 08h
CR equ 0Dh
LF equ 0Ah
SPC equ 020h
CtrlC equ 03h

; variable storage
porttemp	equ stack+1	; scratch space for port commands
tm_hrs		equ porttemp+3	; current hour
tm_min		equ tm_hrs+1	; current minute
tm_sec		equ tm_min+1	; current second	
tm_day		equ tm_sec+1	; current day
tm_mo		equ tm_day+1	; current month
tm_yr		equ tm_mo+1	; current year
rmc_match	equ tm_yr+1	; pointer to position in $GPRMC
rmc_fld		equ rmc_match+2	; current parse field in rmc messgae
rmc_pos		equ rmc_fld+1	; current parse position in field

; CODE STARTS HERE
;main program
        lxi sp, stack   ; initialize the stack pointer

; initialize variables
	lxi h, gprmc_str	; store rmc string
	shld rmc_match		;   pointer
	xra a 			; a=0
	sta rmc_fld		; set to zero
	sta rmc_pos		; set to zero

; initialize hardware, show signon message
; clear clock display
	xra a			; a=0
	out day_led		; clear day
	out hrs_led		; clear hour
	out min_led		; clear minute
	out sec_led		; clear second

; initialize 8155 timer
	mvi a, tmr_tclow	; low byte
	out i8155_tc1		; write to 8155
	mvi a, tmr_tchi		; high byte
	out i8155_tc2		; write to 8155
	mvi a, tmr_tmmode	; start the timer
	out i8155_cmd		; write to 8155

; initialize uart
	xra a			; a=0
	out uart_stat		; send a
	out uart_stat		; dummy mode
	out uart_stat		; and command
	mvi a, uart_reset	; reset the
	out uart_stat		; uart
	mvi a, uart_mode	; set the
	out uart_stat		; uart mode
	mvi a, uart_cmd_byte	; send the
	out uart_stat		; uart command

; clear usb buffer
clrusb:
        in usb_stat             ; get status
        ani usb_rx_rdy          ; mask out the rx status
        jnz signon              ; 1=empty
        in usb_data
        jmp clrusb              ; loop until fifo is empty

; done with init
signon:
        lxi h, signon_str       ; show signon
        call usb_out_str

prompt:
        lxi sp, stack   ; just in case there should have been a ret
        lxi h, prompt_str
        call usb_out_str
        call usb_in
        mov c, a
        push a
        call usb_out           ; echo
        call usb_out_crlf

        pop a
        cpi 'm'                 ; modify memory
        jz memin
        cpi ':'			; intel hex
        jz memin_intel_1
        cpi 'd'                 ; dump memory
        jz memout
        cpi 'j'                 ; jump to memory address
        jz jump
        cpi 'i'                 ; input from i/o
        jz portin
        cpi 'o'                 ; output to i/o
        jz portout

        mvi c, '?'
        call usb_out
        jmp prompt              ; probably didn't understand cmd

memin:
        lxi h, hextype_str
        call usb_out_str       ; raw or intel?
        call usb_in
        mov c, a                ; save the input
        call usb_out           ; echo
        mov a, c
        cpi 'r'                 ; raw
        jz memin_raw
        cpi 'i'                 ; intel
        jz memin_intel
        jmp prompt

memin_raw:
        lxi h, start_str
        call usb_out_str       ; "start:"
        call usb_in_hexaddr
        call usb_out_crlf

        mov a, l
        ani 0fh                 ; calculate offset
        mov b, a
        mov a, l
        ani 0f0h                ; round down
        mov l, a

        call usb_out_mempos
memin_raw_1:
        mov a, l
        ani 0fh
        cmp b                   ; end of offset?
        jz memin_raw_2
        mvi c, ' '
        call usb_out
        mov d, m
        call usb_out_hex
        inx h
        jmp memin_raw_1

memin_raw_2:
        mvi c, ' '
        call usb_out
        call usb_in_hex
        mov m, a
        inx h
        inr b

        mov a, b
        cpi 010h                ; end of line?
        jnz memin_raw_2         ; nope

        call usb_out_crlf
        call usb_out_mempos
        mvi b, 0
        jmp memin_raw_2

memin_intel:
        call usb_out_crlf
memin_intel_start:
        call usb_in
        cpi ':'                 ; start code
        jz memin_intel_1
        cpi CtrlC               ; bail
        jz prompt
        jmp memin_intel_start
memin_intel_1:
        call usb_in_hex        ; byte count
        mov b, a
        call usb_in_hex        ; address high
        mov h, a
        call usb_in_hex        ; address low
        mov l, a

        mov a, b
        add h
        add l                   ; start of checksum
        mov e, a

        push d
        call usb_in_hex        ; record type
        pop d
        cpi 0                   ; data
        jz memin_intel_2
        cpi 1                   ; eof
        jz memin_intel_eof
        jmp memin_intel ; not supported, wait for another record
memin_intel_2:
        mov a, b
        ana a
        jz memin_intel_cksum    ; b=0, checksum comes next
        push d                  ; e will get clobbered by input
        call usb_in_hex
        pop d
        mov m, a                ; save byte
        add e                   ; add to checksum
        mov e, a
        inx h                   ; increment pointer
        dcr b                   ; decrement counter
        jmp memin_intel_2       ; next byte
memin_intel_cksum:
        push d                  ; input clobbers e
        call usb_in_hex        ; get checksum
        pop d
        add e
        jz memin_intel          ; checksum is good, get next record
        lxi h, ckerr_str
        call usb_out_str       ; show error on console
        jmp memin_intel         ; get next record
memin_intel_eof:
        call usb_in_hex        ; checksum
        jmp prompt              ; done

memout:
        lxi h, hextype_str      ; raw or intel?
        call usb_out_str
        call usb_in
        mov c, a                ; save the input
        call usb_out
        mov a, c
        cpi 'r'
        jz memout_raw
        cpi 'i'
        jz memout_intel
        jmp prompt

memout_prompt:
        lxi h, start_str        ; "start"
        call usb_out_str
        call usb_in_hexaddr
        push h
        lxi h, end_str          ; "end:"
        call usb_out_str
        call usb_in_hexaddr
        call usb_out_crlf
        xchg                    ; end addr in de
        pop h                   ; start addr in hl
        ret

memout_raw:
        call memout_prompt
        mov a, l
        ani 0f0h                ; round down
        mov l, a
        mov a, e
        ani 0f0h
        mov e, a
        dcx h

memout_raw_1:
        inx h
        mvi b, 0
        push d
        call usb_out_mempos
        mvi c, ' '
        call usb_out
memout_raw_2:
        mov d, m                ; spit out a byte
        call usb_out_hex
        mvi c, ' '
        call usb_out

        inx h                   ; increment the pointer
        inr b                   ; increment the byte counter
        mov a, b
        cpi 010h                ; time for a new line?
        jnz memout_raw_2        ; not yet

        call usb_out_crlf
        in usb_stat
        ani usb_rx_rdy
        jnz memout_raw_3
        in usb_data
        cpi CtrlC
        jz prompt
memout_raw_3:
        pop d
        dcx h
        mov a, h                ; see if we're at the end addr
        cmp d
        jc memout_raw_1
        mov a, l
        cmp e
        jc memout_raw_1
        jmp prompt              ; done

memout_intel:
        call memout_prompt
        mov a, e
        ana a                   ; clear carry
        sub l
        mov e, a
        mov a, d
        sbb h
        mov d, a                ; de=de-hl
memout_intel_1:
        mov a, d
        ora a
        jnz memout_intel_2      ; d>0
        mov a, e
        cpi 020h
        jnc memout_intel_2      ; e>20h
        mov b, e                ; b=e
        jmp memout_intel_3
memout_intel_2:
        mvi b, 020h             ; b=20h
memout_intel_3:
        ana a                   ; clear carry
        mov a, e
        sub b                   ; e=e-b
        mov e, a
        mov a, d
        sbi 0                   ; d=d-(borrow)
        mov d, a
        push d

        mvi c, ':'
        call usb_out           ; start code
        mov d, b
        call usb_out_hex       ; byte count
        mov d, h
        call usb_out_hex       ; address high
        mov d, l
        call usb_out_hex       ; address low
        mvi d, 0
        call usb_out_hex       ; record type

        mov a, b                ; checksum=b
        add h
        add l
        mov e, a                ; store checksum
memout_intel_4:
        mov a, b
        ana a
        jz memout_intel_5       ; end of line
        mov d, m                ; grab hex byte
        mov a, e
        add d
        mov e, a                ; checksum+=d
        call usb_out_hex       ; send to console
        dcr b                   ; decrement byte counter
        inx hl                  ; increment memory pointer
        jmp memout_intel_4
memout_intel_5:
        mov a, e
        cma                     ; compliment checksum
        inr a                   ; a++
        mov d, a
        call usb_out_hex       ; checksum
        call usb_out_crlf      ; newline

        in usb_stat
        ani usb_rx_rdy
        jnz memout_intel_6
        in usb_data
        cpi CtrlC
        jz prompt
memout_intel_6:
        pop d
        mov a, d
        ana a                   ; d=0
        jnz memout_intel_1      ; next line
        mov a, e
        ana a                   ; e=0
        jz memout_intel_7       ; done
        jmp memout_intel_1      ; next line
memout_intel_7:
        lxi h, hexeof_str
        call usb_out_str
        jmp prompt

jump: ; jump to user-specified memory address
        lxi h, addr_str
        call usb_out_str       ; "addr:"
        call usb_in_hexaddr
        push h
        lxi h, crlf_str
        call usb_out_str
        pop h
        pchl                    ; jump!

portin: ; input from i/o port, display result
;there is no "in r" instruction, so we'll have to put some code in memory
        lxi h, port_str         ; "port:"
        call usb_out_str
        lxi h, porttemp         ; use the temp space just above the stack
        mvi m, 0DBh             ; "in"
        inx h                   ; increment pointer
        call usb_in_hex
        mov m, a                ; port number
        call usb_out_crlf
        inx h
        mvi m, 0C9h             ; "ret"
        call porttemp           ; call input instruction
portin_1:
        mov d, a
        call usb_out_hex
        jmp prompt

portout: ; output specified value to i/o port
        lxi h, port_str         ; "port:"
        call usb_out_str
        call usb_in_hex
        mov b, a
        lxi h, byte_str         ; "byte:"
        call usb_out_str
        call usb_in_hex
        lxi h, porttemp
        mvi m, 0D3h             ; "out"
        inx h                   ; increment pointer
        mov m, b                ; port number
        inx h
        mvi m, 0C9h             ; "ret"
        call porttemp
        jmp prompt

; hardware support routines
usb_out_hex: ; send byte in d as ascii hex digits
        mov a, d
        rar
        rar
        rar
        rar                     ; get the high nibble
        call bin2hex
        mov c, a
        call usb_out           ; send first digit
        mov a, d
        call bin2hex
        mov c, a
        call usb_out           ; send second digit
        ret

usb_in_hexaddr: ; get an address from the console in hex, return in hl
        call usb_in_hex        ; high byte
        mov h, a
        call usb_in_hex        ; low byte
        mov l, a
        ret

usb_in_hex: ; get a byte from the console in ascii hex
        call usb_in_hexnib     ; get high nibble
        ral
        ral
        ral
        ral                     ; shift to the high nibble
        ani 0f0h                ; mask off low nibble
        mov e, a                ; store the first nibble
        call usb_in_hexnib     ; get low nibble
        ora e                   ; combine high and low nibble
        ret                     ; return with byte in a

usb_in_hexnib: ; get a single hex digit from the console, return in a
        call usb_in            ; get digit from console
        cpi CtrlC               ; ctrl-z?
        jz prompt               ; bail if so
        cpi 061h                ; lowercase?
        jc hexnib1              ; nope
        sui 20h                 ; lower -> upper
hexnib1:
        mov c, a                ; save ascii version
        call hex2bin            ; convert to bin
        out 0ffh                ; DEBUG
        cpi 010h                ; carry if 0-F
        jnc usb_in_hexnib      ; try again
        mov d, a                ; a will get clobbered by usb_out
        call usb_out           ; echo
        mov a, d
        ret

hex2bin: ; convert ascii hex digit to binary nibble in a
        cpi 041h                ; >=A?
        jc hex2bin1             ; <A.
        sui 7                   ; correct for A-F
hex2bin1:
        sui 030h                ; get binary value
        ret

bin2hex: ; convert low nibble of a to ascii hex digit
        ani 0fh                 ; mask off high nibble
        adi 90h
        daa
        aci 40h
        daa
        ret

usb_out_crlf: ; newline without clobbering de
        mvi c, CR
        call usb_out
        mvi c, LF
        call usb_out
        ret

usb_out_str: ; send string at hl to the console
        mov a, m                ; get a byte to send
        ora a                   ; is it null?
        rz                      ; we're done if so
        mov c, a
        call usb_out           ; send this byte
        inx h                   ; increment the pointer
        jmp usb_out_str        ; loop until we find a null

usb_out_mempos: ; address in hl, plus a colon
        mov d, h
        call usb_out_hex
        mov d, l
        call usb_out_hex
        mvi c, ':'
        call usb_out
        ret

usb_out: ; send byte in c to the console
	; and do clock stuff while we wait
	in uart_stat		; get uart status
	ani uart_rx_rdy		; mask off rx ready bit
	jz usb_out_1		; continue if nothing waiting
	call parse_nmea		; handle if there is
usb_out_1:
        in usb_stat             ; check fifo status
        ani usb_tx_rdy
        jnz usb_out             ; loop until we can send this byte
        mov a, c
        out usb_data           ; send the byte
        ret

usb_in: ; get a byte from the console, store in a
	; and do clock stuff while we wait
	in uart_stat		; get uart status
	ani uart_rx_rdy		; mask off rx ready bit
	jz usb_in_1		; check usb if nothing in uart
	call parse_nmea		; parse uart byte if there is
usb_in_1:
        in usb_stat             ; check fifo status
        ani usb_rx_rdy
        jnz usb_in              ; loop until a byte comes in
        in usb_data            ; get the byte
        ret

; clock routines
parse_nmea: ; handle an incoming nmea byte
	push b			; save
	push d			; the
	push h			; registers
	lhld rmc_match		; get current match position
	in uart_data		; get the byte from the uart
	mov c,a			; stash it in c
	cmp m			; does it match gprmc string?
	jz nmea_match		; handle if so
	mov a,m			; get char at pointer
	ora a			; is it zero?
	jz parse_rmc		; we're parsing rmc message if so
	lxi h, gprmc_str	; reset rmc string
	shld rmc_match		;   pointer
	jmp parse_done		; return to monitor

nmea_match: ; matched a char in gprmc string
	inx h			; increment pointer
	shld rmc_match		; save it
	jmp parse_done		; return to monitor

parse_rmc: ; parse a byte in the rmc message
	mov a,c			; is this
	cpi ','			;   a comma?
	jz parse_rmc_sep	; handle it if so
	lda rmc_fld		; get field number
	cpi 1			; time field
	jz parse_rmc_time	; handle time
	cpi 9			; date field
	jz parse_rmc_date	; handle date
	jmp parse_done		; return to monitor

parse_rmc_sep: ; handle nmea field seperator
	lda rmc_fld		; get current field number
	inr a			; increment it
	sta rmc_fld		; save it
	xra a			; a=0
	sta rmc_pos		; reset field position
	jmp parse_done		; return to monitor

parse_rmc_time: ; handle gprmc time field
	lda rmc_pos		; get field position
	inr a			; increment it
	sta rmc_pos		; save it
	cpi 1			; hrs 10's place
	jnz parse_rmc_t2	; not at position 1
	lxi d, tm_hrs		; put in hour value
	call set_high_bcd	; save the value
	jmp parse_done		; keep reading stream
parse_rmc_t2:
	cpi 2			; hrs 1's place
	jnz parse_rmc_t3	; not at position 2
	lxi d, tm_hrs		; put in hour value
	call set_low_bcd	; save the value
	out hrs_led		; send to display
	jmp parse_done		; keep reading stream
parse_rmc_t3:
	cpi 3			; min 10's place
	jnz parse_rmc_t4	; not at position 3
	lxi d, tm_min		; put in minute value
	call set_high_bcd	; save the value
	jmp parse_done		; keep reading stream
parse_rmc_t4:
	cpi 4			; min 1's place
	jnz parse_rmc_t5	; not at position 4
	lxi d, tm_min		; put in minute value
	call set_low_bcd	; save the value
	out min_led		; send to display
	jmp parse_done		; keep reading stream
parse_rmc_t5:
	cpi 5			; sec 10's place
	jnz parse_rmc_t6	; not at position 5
	lxi d, tm_sec		; put in second value
	call set_high_bcd	; save the value
	jmp parse_done		; keep reading stream
parse_rmc_t6:
	cpi 6			; sec 1's place
	jnz parse_done		; done with time field
	lxi d, tm_sec		; put in second value
	call set_low_bcd	; save the value
	out sec_led		; send to display
	jmp parse_done		; keep reading stream
	
parse_rmc_date:
	lda rmc_pos		; get field position
	inr a			; increment it
	sta rmc_pos		; save it back to memory
	cpi 1			; day 10's place
	jnz parse_rmc_d2	; not position 1
	lxi d, tm_day		; put in day value
	call set_high_bcd	; save the value
	jmp parse_done		; keep reading stream
parse_rmc_d2:
	cpi 2			; day 1's place
	jnz parse_rmc_d3	; not position 2
	lxi d, tm_day		; put in day value
	call set_low_bcd	; save the value
	out day_led		; send to display
	jmp parse_done		; keep reading stream
parse_rmc_d3:
	cpi 3			; month 10's place
	jnz parse_rmc_d4	; not position 3
	lxi d, tm_mo		; store in month value
	call set_high_bcd	; save the value
	jmp parse_done		; keep reading stream
parse_rmc_d4:
	cpi 4			; month 1's place
	jnz parse_rmc_d5	; not position 4
	lxi d, tm_mo		; put in month value
	call set_low_bcd	; save the value
	jmp parse_done		; keep reading stream
parse_rmc_d5:
	cpi 5			; year 10's place	
	jnz parse_rmc_d6	; not position 5
	lxi d, tm_yr		; put in year value
	call set_high_bcd	; save value
	jmp parse_done		; keep reading stream
parse_rmc_d6: ; only option left is position 6
	lxi d, tm_yr		; put in year value
	call set_low_bcd	; save value
	xra a			; a=0
	sta rmc_fld		; wait for next gprmc
	lxi h, gprmc_str	; reset gprmc pointer
	shld rmc_match		
	jmp parse_done		; keep reading stream

parse_done:
	pop h			; restore
	pop d			;  the
	pop b			;  registers
	ret			;  and return

set_high_bcd:
; ascii value in c, store in de
	mov a, c
	sbi 30h			; ascii adjust
	rlc			; put
	rlc			; in
	rlc			; high
	rlc			; nibble
	stax d			; put in address de
	ret			; done

set_low_bcd:
; ascii value in c, store in de
	ldax d			; get day value
	add c			; add current byte
	sui 30h			; ascii adjust
	stax d			; save in memory
	ret

; strings (null terminated)
crlf_str: db CR,LF,0
prompt_str: db CR,LF,'>',0
signon_str: db CR,LF,"RubeCron ROM Monitor",CR,LF,"by AB0TJ, 2017",CR,LF,0
hextype_str: db "(r)aw or (i)ntel:",0
start_str: db CR,LF,"start:",0
end_str: db CR,LF,SPC," end:",0
addr_str: db "addr:",0
hexeof_str: db ":00000001FF",CR,LF,0
ckerr_str: db " cksum err",0
port_str: db "port:",0
byte_str: db CR,LF,"byte:",0
gprmc_str: db "$GPRMC",0

end: