; RubeCron Binary Clock

.hexfile rubeclock.hex
.binfile rubeclock.bin
.download hex

.org 8000h         ; 0-7fff is rom, 8000-ffff is ram

stack   equ 0ffe0h ; stack pointer

; i/o ports
tmr_cmd	equ 0	; 8155 command/status register
tmr_tc1	equ 04h ; 8155 low byte of timer count
tmr_tc2	equ 05h	; 8155 high byte of timer count
usb_data        equ 30h        ; usb module
usb_stat        equ 03h        ; 8155 port c
uart_data	equ 20h	; uart data port
uart_stat	equ 21h	; uart status port
sec_led		equ 50h	; seconds display
min_led		equ 51h	; minutes display
hrs_led		equ 52h	; hours display
day_led		equ 53h	; day display

; Bit Masks
usb_rx_rdy	equ 20h
usb_tx_rdy	equ 10h
uart_rx_rdy	equ 02h
uart_tx_rdy	equ 01h

; hardware constants
tmr_tclow	equ 010h	; 9600*16*16*2 = 4.9152 MHz
tmr_tchi	equ 040h	; square wave output
tmr_tmmode	equ 0c0h	; command to start timer
uart_reset      equ 040h        ; reset byte
uart_mode       equ 04Eh        ; mode byte
uart_cmd_byte   equ 027h        ; control byte

; ascii
BS equ 08h
CR equ 0Dh
LF equ 0Ah
SPC equ 020h
CtrlC equ 03h

; CODE STARTS HERE
;main program
        lxi sp, stack   ; initialize the stack pointer

; clear display
	xra a			; a=0
	out day_led		; clear day
	out hrs_led		; clear hour
	out min_led		; clear minute
	out sec_led		; clear second

; initialize 8155 timer
	mvi a, tmr_tclow	; low byte
	out tmr_tc1		; write to 8155
	mvi a, tmr_tchi	; high byte
	out tmr_tc2		; write to 8155
	mvi a, tmr_tmmode	; start the timer
	out tmr_cmd		; write to 8155

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

; set gprmc pointer
	lxi h, gprmc

uart_in: ; get byte from uart and store it in c
	in uart_stat		; read uart status
	ani uart_rx_rdy		; mask out ready bit
	jz uart_in		; loop if not
	in uart_data		; get the byte
	mov c, a		; stash it in c

; watch for gprmc
	cmp m			; does this match the gprmc string?
	jz gprmc_match		; yes, handle match
	mov a, m		; get byte at pointer
	ora a			; 0 = parsing
	jz parsermc		; go to parsing code
	lxi h, gprmc		; no, reset to beginning of string
	jmp uart_in		; and keep looking
gprmc_match:
	out usb_data	; DEBUG
	inx h			; increment pointer
	jmp uart_in		; wait for next byte
parsermc:
	mov a, c
	cpi ','			; is this a comma?
	jz parsermc_sep		; yes, handle it
	lda gp_field		; where are we in parsing?
	cpi 1			; time
	jz parsermc_time	; go parse the time
	cpi 9			; date
	jz parsermc_date	; go parse the date
	jmp uart_in		; not date or time, keep looking

parsermc_time:
	mov a, c	; DEBUG
	out usb_data	; DEBUG
	lda gp_pos		; get field position
	inr a			; increment the value
	sta gp_pos		; save incremented value
	cpi 1			; hrs 10's place
	jnz parsermc_t2		; not at position 1
	lxi d, hrs		; put in hour value
	call set_high_bcd	; save the value
	jmp uart_in		; keep reading stream
parsermc_t2:
	cpi 2			; hrs 1's place
	jnz parsermc_t3		; not at position 2
	lxi d, hrs		; put in hour value
	call set_low_bcd	; save the value
	out hrs_led		; send to display
	jmp uart_in		; keep reading stream
parsermc_t3:
	cpi 3			; min 10's place
	jnz parsermc_t4		; not at position 3
	lxi d, min		; put in minute value
	call set_high_bcd	; save the value
	jmp uart_in		; keep reading stream
parsermc_t4:
	cpi 4			; min 1's place
	jnz parsermc_t5		; not at position 4
	lxi d, min		; put in minute value
	call set_low_bcd	; save the value
	out min_led		; send to display
	jmp uart_in		; keep reading stream
parsermc_t5:
	cpi 5			; sec 10's place
	jnz parsermc_t6		; not at position 5
	lxi d, sec		; put in second value
	call set_high_bcd	; save the value
	jmp uart_in		; keep reading stream
parsermc_t6:
	cpi 6			; sec 1's place
	jnz uart_in		; done with time field
	lxi d, sec		; put in second value
	call set_low_bcd	; save the value
	out sec_led		; send to display
	jmp uart_in		; keep reading stream

parsermc_date:
	mov a, c	; DEBUG
	out usb_data	; DEBUG
	lda gp_pos		; get field position
	inr a			; increment it
	sta gp_pos		; save it back to memory
	cpi 1			; day 10's place
	jnz parsermc_d2		; not position 1
	lxi d, day		; put in day value
	call set_high_bcd	; save the value
	jmp uart_in		; keep reading stream
parsermc_d2:
	cpi 2			; day 1's place
	jnz parsermc_d3		; not position 2
	lxi d, day		; put in day value
	call set_low_bcd	; save the value
	out day_led		; send to display
parsermc_d3:
	xra a			; a=0
	sta gp_field		; wait for next gprmc
	lxi h, gprmc		; reset gprmc pointer
	mvi a, CR
	out usb_data
	mvi a, LF
	out usb_data
	jmp uart_in		; keep reading stream

parsermc_sep:
	lda gp_field
	inr a			; increment field number
	sta gp_field		; save it
	xra a			; a=0
	sta gp_pos		; reset field counter
	jmp uart_in		; keep parsing

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

usb_out: ; send byte in c to the console
        in usb_stat             ; check fifo status
        ani usb_tx_rdy
        jnz usb_out             ; loop until we can send this byte
        mov a, c
        out usb_data           ; send the byte
	ret

gprmc:		db "$GPRMC",0

gp_field:	db 0
gp_pos		db 0
day:		db 0
hrs:		db 0
min:		db 0
sec:		db 0
