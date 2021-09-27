; RubeCron Monitor + Clock
; For Z80 RubeCron (r4)

MMU_BASE:   equ 040h
include 'mmu.inc'
MID_BASE:   equ 010h
include 'tp3465v.inc'
UART_BASE:  equ 020h
include 'uart.inc'
PIC_BASE:   equ 030h

include 'ascii.inc'

; at boot, RAM occupies the top 3 pages
startmem:   equ 04000h
endmem:     equ 0ffffh

; varable storage locations
;stack:      equ endmem - 17 ; top of stack
dma:        equ endmem - 16 ; current DMA address
temp:       equ endmem - 14 ; temporary space
tm_hrs:     equ endmem - 10	; current hour
tm_min:     equ endmem - 9	; current minute
tm_sec:     equ endmem - 8 	; current second	
tm_day:     equ endmem - 7	; current day
tm_mo:      equ endmem - 6 	; current month
tm_yr:      equ endmem - 5 	; current year
rmc_match:  equ endmem - 4	; pointer to position in $GPRMC
rmc_fld:    equ endmem - 2	; current parse field in rmc messgae
rmc_pos:    equ endmem - 1	; current parse position in field
sd_cap:     equ endmem      ; SD card capacity flag

; IO ports
sec_led:	equ 50h		; seconds display
min_led:    equ 51h		; minutes display
hrs_led:	equ 52h		; hours display
day_led:	equ 53h		; day display

    org 0F700H

stack:
start:
    ld sp, stack        ; Initialize stack pointer just below monitor
    di                  ; disable interrupts

; initialize variables
	ld hl, str_gprmc	; store rmc string
	ld (rmc_match), hl	;   pointer
	xor a 			    ; a=0
	ld (rmc_fld), a	    ; set to zero
	ld (rmc_pos), a	    ; set to zero

; clear clock display
	out (day_led), a    ; clear day
	out (hrs_led), a    ; clear hour
	out (min_led), a    ; clear minute
	out (sec_led), a    ; clear second

; initialize MMU
    ld a, 00100000b ; RAM @0000
    out (MMU_PG0), a
    ld a, 00100001b ; RAM @4000
    out (MMU_PG1), a
    ld a, 00100010b ; RAM @8000
    out (MMU_PG2), a
    ld a, 00100011b ; RAM @C000
    out (MMU_PG3), a
    ld a, 1
    out (MMU_EN), a

; initialize UART
    ld a, 10111010b         ; reset MRA pointer, disable TX and RX
    out (UART_CRA), a
    out (UART_CRB), a
    ld a, 00101010b         ; Flush RX FIFO
    out (UART_CRA), a
    out (UART_CRB), a
    ld a, 00111010b         ; Reset transmitter
    out (UART_CRA), a
    out (UART_CRB), a
    ld a, 00000100b         ; MR0A: Extended mode 2 baud rates, 1 byte interrupts
    out (UART_MRA), a
    ld a, 01000100b         ; MR0B: Extended mode 2 baud rates, 6 byte interrupts
    out (UART_MRB), a
    ld a, 10010011b         ; MR1A: RTS, no parity, 8 bits, 1 byte interrupts
    out (UART_MRA), a
    ld a, 00010011b         ; MR1B: No RTS, no parity, 8 bits, 6 byte interrupts
    out (UART_MRB), a
    ld a, 00010111b         ; MR2A: normal mode, CTS, 1 stop bit
    out (UART_MRA), a
    ld a, 00000111b         ; MR2B: normal mode, no CTS, 1 stop bit
    out (UART_MRB), a
    ld a, 01100110b         ; CSRA: 115200 baud
    out (UART_CSRA), a
    ld a, 10111011b         ; CSRB: 9600 baud
    out (UART_CSRB), a
    ld a, 00110000b         ; ACR: Select first baud rate option
    out (UART_ACR), a
    ld a, 00100000b         ; IMR: unmask RxB interrupt
    out (UART_IMR), a
    ld a, 10000101b         ; UARTA: Set RTS, Enable TX and RX
    out (UART_CRA), a
    ld a, 00000101b         ; UARTB: Enable TX and RX
    out (UART_CRB), a

; disable all GPS strings except GPRMC
    ld hl, str_nmea_msgs
    ld (temp), hl
    ld hl, str_nmea_cksums
    ld (temp+2), hl
    ld d, 6
nmea_dis_loop:
    ld a, d
    and a
    jp z, nmea_dis_end
    dec d
    ld hl, str_pubx
    call outstr_b
    ld hl, (temp)
    call outstr_b
    inc hl
    ld (temp), hl
    ld hl, str_nmea_dis
    call outstr_b
    ld hl, (temp+2)
    call outstr_b
    inc hl
    ld (temp+2), hl
    ld hl, str_crlf
    call outstr_b    
    jp nmea_dis_loop
nmea_dis_end:

; initialize PIC
    ld a, 0C3h      ; JP opcode
    ld (030h), a    ; RST6 calls here
    ld (066h), a    ; NMI calls here
    ld hl, int_handler
    ld (031h), hl
    ld hl, nmi
    ld (067h), hl

    ld a, 00010010b         ; ICW1
    out (PIC_BASE), a
    ld a, 0                 ; ICW2
    out (PIC_BASE+1), a
    ld a, 11111011b         ; OCW1: unmask UART interrupt
    out (PIC_BASE+1), a
    im 0
    call int_handler        ; handle any pending interrupts

; Show signon message
    ld hl, str_signon
    call outstr             ; Show signon message

prompt:
    ld sp, stack   ; just in case there should have been a ret
    ei
    ld hl, str_prompt
    call outstr
    call chrin
    ld d, a                 ; save for later
    ld c, a
    call outc               ; echo
    call crlf

    ld a, d
    cp 'm'                 ; modify memory
    jp z, memin
    cp ':'			        ; intel hex
    jp z, memin_intel_p
    cp 'd'                 ; dump memory
    jp z, memout
    cp 'j'                 ; jump to memory address
    jp z, jump
    cp 'i'                 ; input from i/o
    jp z, portin
    cp 'o'                 ; output to i/o
    jp z, portout
    cp 'b'                  ; boot from SD card
    jp z, boot
    cp 'g'                  ; dump GPS stream
    jp z, gps
    cp 'f'                  ; fill memory
    jp z, fill
    cp 'p'                  ; program flash
    jp z, program

    ld c, '?'
    call outc
    jp prompt              ; didn't understand cmd

memin:
    xor a
    ld (temp), a            ; clear error flag
    ld hl, str_hextype
    call outstr             ; raw or intel?
    call chrin
    ld c, a                ; save the input
    call outc              ; echo
    ld a, c
    cp 'r'                 ; raw
    jp z, memin_raw
    cp 'i'                 ; intel
    jp z, memin_intel
    jp prompt

memin_raw:
    ld hl, str_start
    call outstr       ; "start:"
    call in_hexaddr
    call crlf

    ld a, l
    and 0fh                 ; calculate offset
    ld b, a
    ld a, l
    and 0f0h                ; round down
    ld l, a

    call out_mempos
memin_raw_1:
    ld a, l
    and 0fh
    cp b                   ; end of offset?
    jp z, memin_raw_2
    ld c, ' '
    call outc
    ld d, (hl)
    call out_hex
    inc hl
    jp memin_raw_1

memin_raw_2:
    ld c, ' '
    call outc
    call in_hex
    ld (hl), a
    inc hl
    inc b

    ld a, b
    cp 010h                ; end of line?
    jp nz, memin_raw_2     ; nope

    call crlf
    call out_mempos
    ld b, 0
    jp memin_raw_2

memin_intel_p:
    xor a
    ld (temp), a            ; clear error flag
    jp memin_intel_1

memin_intel:
    call crlf
memin_intel_start:
    call chrin
    cp ':'                  ; wait for start code
    jp z, memin_intel_1
    cp CtrlC                ; bail if user sends Ctrl-C
    jp z, prompt
    jp memin_intel_start
memin_intel_1:
    call in_hex             ; byte count
    ld b, a
    call in_hex             ; address high
    ld h, a
    call in_hex             ; address low
    ld l, a

    ld a, b
    add a, h
    add a, l                ; start of checksum
    ld e, a

    push de
    call in_hex             ; record type
    pop de
    cp 0                    ; data
    jp z, memin_intel_2
    cp 1                    ; eof
    jp z, memin_intel_eof
    push hl
    ld hl, str_unsup
    call outstr
    xor a
    ld (temp), a
    pop hl
    jp memin_intel          ; not supported, wait for another record
memin_intel_2:
    ld a, b
    and a
    jp z, memin_intel_cksum ; b=0, checksum comes next
    push de                 ; e will get clobbered by input
    call in_hex
    pop de
    ld (hl), a              ; save byte
    add a, e                ; add to checksum
    ld e, a
    inc hl                  ; increment pointer
    dec b                   ; decrement counter
    jp memin_intel_2        ; next byte
memin_intel_cksum:
    push de                 ; input clobbers e
    call in_hex             ; get checksum
    pop de
    add a, e
    jp z, memin_intel       ; checksum is good, get next record
    ld hl, str_ckerr
    call outstr             ; show error on console
    ld a, 1                 ; set error flag
    ld (temp), a
    jp memin_intel          ; get next record
memin_intel_eof:
    call in_hex             ; checksum
    ld a, (temp)
    and a                   ; check if we had errors
    jp z, prompt            ; none - return to prompt
    ld hl, str_error
    call outstr             ; warn the user
    jp prompt

memout:
    ld hl, str_hextype      ; raw or intel?
    call outstr
    call chrin
    ld c, a                ; save the input
    call outc
    ld a, c
    cp 'r'
    jp z, memout_raw
    cp 'i'
    jp z, memout_intel
    jp prompt

memout_prompt:
    ld hl, str_start        ; "start"
    call outstr
    call in_hexaddr
    push hl
    ld hl, str_end          ; "end:"
    call outstr
    call in_hexaddr
    call crlf
    ex de, hl               ; end addr in de
    pop hl                  ; start addr in hl
    ret

memout_raw:
    call memout_prompt
    ld a, l
    and 0f0h                ; round down
    ld l, a
    ld a, e
    and 0f0h
    ld e, a
    dec hl

memout_raw_1:
    inc hl
    ld b, 0
    push de
    call out_mempos
    ld c, ' '
    call outc
memout_raw_2:
    ld d, (hl)                ; spit out a byte
    call out_hex
    ld c, ' '
    call outc

    inc hl                   ; increment the pointer
    inc b                    ; increment the byte counter
    ld a, b
    cp 010h                 ; time for a new line?
    jp nz, memout_raw_2        ; not yet

    call crlf
memout_raw_3:
    in a, (UART_SRA)        ; check fifo status
    bit 0, a                ; check bit 0 (RxRDYA)
    jp z, memout_raw_4
    in a, (UART_FIFOA)
    cp CtrlC
    jp z, prompt
    jp memout_raw_3
memout_raw_4:
    pop de
    dec hl
    ld a, h                ; see if we're at the end addr
    cp d
    jp c, memout_raw_1
    ld a, l
    cp e
    jp c, memout_raw_1
    jp prompt              ; done

memout_intel:
    call memout_prompt
    ld a, e
    and a                   ; clear carry
    sub l
    ld e, a
    ld a, d
    sbc a, h
    ld d, a                ; de=de-hl
memout_intel_1:
    ld a, d
    or a
    jp nz, memout_intel_2      ; d>0
    ld a, e
    cp 020h
    jp nc, memout_intel_2      ; e>20h
    ld b, e                ; b=e
    jp memout_intel_3
memout_intel_2:
    ld b, 020h             ; b=20h
memout_intel_3:
    and a                   ; clear carry
    ld a, e
    sub b                   ; e=e-b
    ld e, a
    ld a, d
    sbc a, 0                ; d=d-(borrow)
    ld d, a
    push de

    ld c, ':'
    call outc           ; start code
    ld d, b
    call out_hex       ; byte count
    ld d, h
    call out_hex       ; address high
    ld d, l
    call out_hex       ; address low
    ld d, 0
    call out_hex       ; record type

    ld a, b                ; checksum=b
    add a, h
    add a, l
    ld e, a                ; store checksum
memout_intel_4:
    ld a, b
    and a
    jp z, memout_intel_5       ; end of line
    ld d, (hl)                ; grab hex byte
    ld a, e
    add a, d
    ld e, a                ; checksum+=d
    call out_hex            ; send to console
    dec b                   ; decrement byte counter
    inc hl                  ; increment memory pointer
    jp memout_intel_4
memout_intel_5:
    ld a, e
    cpl                     ; compliment checksum
    inc a                   ; a++
    ld d, a
    call out_hex           ; checksum
    call crlf              ; newline
memout_intel_6:
    in a, (UART_SRA)        ; check fifo status
    bit 0, a                ; check bit 0 (RxRDYA)
    jp z, memout_intel_7
    in a, (UART_FIFOA)
    cp CtrlC
    jp z, prompt
    jp memout_intel_6
memout_intel_7:
    pop de
    ld a, d
    and a                   ; d=0
    jp nz, memout_intel_1      ; next line
    ld a, e
    and a                   ; e=0
    jp z, memout_intel_8       ; done
    jp memout_intel_1      ; next line
memout_intel_8:
    ld hl, str_hexeof
    call outstr
    jp prompt

jump: ; jump to user-specified memory address
    ld hl, str_addr
    call outstr             ; "addr:"
    call in_hexaddr
    call crlf
    jp (hl)                 ; jump!

portin: ; input from i/o port, display result
    ld hl, str_port         ; "port:"
    call outstr
    call in_hex
    ld c, a
    in a, (c)               ; read port
    ld d, a
    call crlf
    call out_hex
    jp prompt

portout: ; output specified value to i/o port
    ld hl, str_port         ; "port:"
    call outstr
    call in_hex
    ld b, a
    ld hl, str_byte         ; "byte:"
    call outstr
    call in_hex
    ld c, b
    out (c), a
    jp prompt

gps:
    ld hl, str_gps
    call outstr
gps_loop:
    in a, (UART_SRA)    ; check UARTA status
    bit 0, a
    jp z, gps_loop_1
    call chrin
    cp CtrlC            ; exit if user pressed Chrtl-C
    jp z, prompt
gps_loop_1:
    in a, (UART_SRB)
    bit 0, a            ; check bit 0 (RxRDYB)
    jp z, gps_loop         ; loop until a byte comes in
    in a, (UART_FIFOB)  ; get the byte
    ld c, a
    call outc
    jp gps_loop_1

fill:   ; fill memory with provided value
    ld hl, str_start
    call outstr
    call in_hexaddr     ; get start address
    push hl
    ld hl, str_end
    call outstr
    call in_hexaddr     ; get end address
    push hl
    ld hl, str_byte
    call outstr
    call in_hex         ; get fill byte
    ld c, a
    pop de              ; recall end addr
    pop hl              ; recall start addr
fill_loop:
    ld a, h
    cp d                ; done?
    jp nz, fill_next
    ld a, l
    cp e
    jp z, prompt        ; return if so
fill_next:
    ld (hl), c
    inc hl
    jp fill_loop

boot:   ; boot from SD card
CS: macro val
    ld a, val
    out (MID_CS), a
endm
    CS 0FFh
    ld a, 11111110b     ; Set CS0 as output
    out (MID_PD), a
    ld a, 00000100b     ; SKR: CLK/16 (250kHz on RubeCron)
    out (MID_SKR), a

    ld b, 10
    call sd_dummy       ; 80 SCK pulses with CS high
    CS 0FEh
    xor a               ; CMD0: go idle
    ld b, a
    ld c, a
    ld d, a
    ld e, a             ; arg=0
    ld h, 095h          ; checksum
    call sd_cmd
    ld a, e
    cp 1                ; ack?
    jp nz, sd_error

    ld b, 5
    call sd_dummy
sd_init_1:
    ld a, 55            ; Next cmd is ACMD
    ld bc, 0
    ld de, 0
    ld h, 065h
    call sd_cmd
    ld a, 41            ; ACMD41 (init)
    ld bc, 04000h
    ld de, 0
    ld h, 077h
    call sd_cmd
    ld a, e
    cp 5
    jp nz, sd_init_2
    ld a, 1             ; Old card - try sending CMD1 instead.
    ld bc, 0
    ld h, 0F9h
    call sd_cmd
    ld a, e
sd_init_2:
    and a
    jp nz, sd_init_1
    ld hl, str_init
    call outstr

; get card OCR
    ld a, 58
    ld bc, 0
    ld de, 0
    call sd_cmd
    ld bc, 4
    ld hl, temp
    call sd_rx
    ld a, (temp)
    bit 6, a
    jp z, sd_std_size
    ld a, 1
    ld (sd_cap), a
    jp sd_setblock
sd_std_size:
    xor a
    ld (sd_cap), a

sd_setblock:    ; set block size to 512 bytes
    ld a, 16
    ld bc, 0
    ld de, 512
    call sd_cmd
    CS 0FFh
    ld a, e
    and a
    jp nz, sd_error

; set SPI clock divider to high speed
    xor a   ; CKIN/1 (4MHz on RubeCron)
    out (MID_SKR), a

; load first 32 sectors into memory and jump there
    ld hl, 0FEh
    ld (dma), hl
    ld de, 0FFh
    push de
boot_load:
    ld bc, 0
    pop de
    inc e
    ld a, e
    cp 32
    jp z, boot_load_done
    push de
    call sd_getblock
    ld hl, (dma)
    ld de, 512
    add hl, de
    ld (dma), hl
    jp boot_load
boot_load_done:
    ld a, (0FEh)
    cp 055h
    jp nz, sd_not_bootable
    ld a, (0FFh)
    cp 0AAh
    jp nz, sd_not_bootable
    ld hl, str_boot
    call outstr
    di
    jp 0100h

sd_not_bootable:
    ld hl, str_nosdboot
    call outstr
    jp prompt

sd_error:
    ld d, e
    ld hl, str_sderr
    call outstr
    call out_hex
    CS 0FFh
    jp prompt

program:
    jp prompt              ; not yet implemented

nmi:
    push hl
    push de
    push bc
    push af
    call crlf
    ld hl, str_af
    call outstr
    pop hl
    call out_hexword
    ld hl, str_bc
    call outstr
    pop hl
    call out_hexword
    ld hl, str_de
    call outstr
    pop hl
    call out_hexword
    ld hl, str_hl
    call outstr
    pop hl
    call out_hexword
    ld hl, str_pc
    call outstr
    pop hl
    call out_hexword
    jp prompt
    
; clock routines
parse_nmea: ; handle an incoming nmea byte
	ld hl, (rmc_match)		; get current match position
	in a, (UART_FIFOB)		; get the byte from the uart
	ld c, a			; stash it in c
	cp (hl)			; does it match gprmc string?
	jp z, nmea_match	; handle if so
	ld a, (hl)			; get char at pointer
	or a			; is it zero?
	jp z, parse_rmc		; we're parsing rmc message if so
	ld hl, str_gprmc	; reset rmc string
	ld (rmc_match), hl 	;   pointer
    ret

nmea_match: ; matched a char in gprmc string
	inc hl			; increment pointer
	ld (rmc_match), hl	; save it
    ret

parse_rmc: ; parse a byte in the rmc message
	ld a, c			; is this
	cp ','			;   a comma?
	jp z, parse_rmc_sep	; handle it if so
	ld a, (rmc_fld)		; get field number
	cp 1			; time field
	jp z, parse_rmc_time	; handle time
	cp 9			; date field
	jp z, parse_rmc_date	; handle date
    ret

parse_rmc_sep: ; handle nmea field seperator
	ld a, (rmc_fld)	; get current field number
	inc a			; increment it
	ld (rmc_fld), a		; save it
	xor a			; a=0
	ld (rmc_pos), a		; reset field position
    ret

parse_rmc_time: ; handle gprmc time field
	ld a, (rmc_pos)	    ; get field position
	inc a			    ; increment it
	ld (rmc_pos), a		; save it
	cp 1			    ; hrs 10's place
	jp nz, parse_rmc_t2	; not at position 1
	ld de, tm_hrs		; put in hour value
	call set_high_bcd	; save the value
    ret
parse_rmc_t2:
	cp 2			    ; hrs 1's place
	jp nz, parse_rmc_t3	; not at position 2
	ld de, tm_hrs		; put in hour value
	call set_low_bcd	; save the value
	out (hrs_led), a		; send to display
    ret
parse_rmc_t3:
	cp 3			    ; min 10's place
	jp nz, parse_rmc_t4	; not at position 3
	ld de, tm_min		; put in minute value
	call set_high_bcd	; save the value
    ret
parse_rmc_t4:
	cp 4			    ; min 1's place
	jp nz, parse_rmc_t5	; not at position 4
	ld de, tm_min		; put in minute value
	call set_low_bcd	; save the value
	out (min_led), a		; send to display
    ret
parse_rmc_t5:
	cp 5			    ; sec 10's place
	jp nz, parse_rmc_t6	; not at position 5
	ld de, tm_sec		; put in second value
	call set_high_bcd	; save the value
    ret
parse_rmc_t6:
	cp 6		    	; sec 1's place
	ret nz      		; done with time field
	ld de, tm_sec		; put in second value
	call set_low_bcd	; save the value
	out (sec_led), a	; send to display
    ret
	
parse_rmc_date:
	ld a, (rmc_pos)		; get field position
	inc a			    ; increment it
	ld (rmc_pos), a		; save it back to memory
	cp 1			    ; day 10's place
	jp nz, parse_rmc_d2	; not position 1
	ld de, tm_day		; put in day value
	call set_high_bcd	; save the value
    ret
parse_rmc_d2:
	cp 2			    ; day 1's place
	jp nz, parse_rmc_d3	; not position 2
	ld de, tm_day		; put in day value
	call set_low_bcd	; save the value
	out (day_led), a	; send to display
    ret
parse_rmc_d3:
	cp 3			    ; month 10's place
	jp nz, parse_rmc_d4	; not position 3
	ld de, tm_mo		; store in month value
	call set_high_bcd	; save the value
    ret
parse_rmc_d4:
	cp 4			    ; month 1's place
	jp nz, parse_rmc_d5	; not position 4
	ld de, tm_mo		; put in month value
	call set_low_bcd	; save the value
    ret
parse_rmc_d5:
	cp 5			    ; year 10's place	
	jp nz, parse_rmc_d6	; not position 5
	ld de, tm_yr		; put in year value
	call set_high_bcd	; save value
    ret
parse_rmc_d6: ; only option left is position 6
	ld de, tm_yr		; put in year value
	call set_low_bcd	; save value
	xor a			; a=0
	ld (rmc_fld), a		; wait for next gprmc
	ld hl, str_gprmc	; reset gprmc pointer
	ld (rmc_match), hl		
    ret


set_high_bcd:
; ascii value in c, store in de
	ld a, c
	sbc a, 30h		; ascii adjust
	rlca    		; put
	rlca	    	; in
	rlca		    ; high
	rlca		    ; nibble
	ld (de), a		; put in address de
	ret			    ; done

set_low_bcd:
; ascii value in c, store in de
	ld a, (de)		; get day value
	add a, c		; add current byte
	sub 30h			; ascii adjust
	ld (de), a		; save in memory
	ret

; Hex functions
in_hexaddr: ; get an address from the console in hex, return in hl
    call in_hex             ; high byte
    ld h, a
    call in_hex             ; low byte
    ld l, a
    ret

in_hex: ; get a byte from the console in ascii hex
    call in_hexnib          ; get high nibble
    rla
    rla
    rla
    rla                     ; shift to the high nibble
    and 0f0h                ; mask off low nibble
    ld e, a                 ; store the first nibble
    call in_hexnib          ; get low nibble
    or e                    ; combine high and low nibble
    ret                     ; return with byte in a

in_hexnib: ; get a single hex digit from the console, return in a
    call chrin              ; get digit from console
    cp CtrlC                ; ctrl-c?
    jp z, prompt            ; bail if so
    cp 061h                 ; lowercase?
    jp c, hexnib1           ; nope
    sub 20h                 ; lower -> upper
hexnib1:
    ld c, a                 ; save ascii version
    call hex2bin            ; convert to bin
    cp 010h                 ; carry if 0-F
    jp nc, in_hexnib        ; try again
    ld d, a                 ; a will get clobbered by outc
    call outc               ; echo
    ld a, d
    ret

hex2bin: ; convert ascii hex digit to binary nibble in a
    cp 041h                 ; >=A?
    jp c, hex2bin1          ; <A.
    sub 7                   ; correct for A-F
hex2bin1:
    sub 030h                ; get binary value
    ret

bin2hex: ; convert low nibble of a to ascii hex digit
    and 0fh                 ; mask off high nibble
    add 90h
    daa
    adc a, 40h
    daa
    ret

out_mempos: ; address in hl, plus a colon
    ld d, h
    call out_hex
    ld d, l
    call out_hex
    ld c, ':'
    call outc
    ret

out_hex: ; send byte in d as ascii hex digits
    ld a, d
    rra
    rra
    rra
    rra                    ; get the high nibble
    call bin2hex
    ld c, a
    call outc           ; send first digit
    ld a, d
    call bin2hex
    ld c, a
    call outc           ; send second digit
    ret

out_hexword:
    ld d, h
    call out_hex
    ld d, l
    call out_hex
    ret

; UART functions
outc:   ; sent a single byte to UARTA
    in a, (UART_SRA)    ; get UART status
    bit 2, a            ; check bit 2 (TxRDYA)
    jp z, outc          ; loop if not ready
    ld a, c             ; get byte to send
    out (UART_FIFOA), a ; send to UART
    ret                 ; done

outc_b:   ; sent a single byte to UARTB
    in a, (UART_SRB)    ; get UART status
    bit 2, a            ; check bit 2 (TxRDYA)
    jp z, outc_b        ; loop if not ready
    ld a, c             ; get byte to send
    out (UART_FIFOB), a ; send to UART
    ret                 ; done

outstr_b:
    ld a, (hl)          ; get a byte to send
    or a                ; is it null?
    ret z               ; we're done if so
    ld c, a
    call outc_b         ; send this byte
    inc hl              ; increment the pointer
    jp outstr_b         ; loop until we find a null

outstr: ; send string at hl to the UART
    ld a, (hl)          ; get a byte to send
    or a                ; is it null?
    ret z               ; we're done if so
    ld c, a
    call outc           ; send this byte
    inc hl              ; increment the pointer
    jp outstr           ; loop until we find a null

crlf: ; send newline to UART
    ld c, CR
    call outc
    ld c, LF
    call outc
    ret

chrin: ; get a byte from the UART, store in a
    in a, (UART_SRA)    ; check fifo status
    bit 0, a            ; check bit 0 (RxRDYA)
    jp z, chrin         ; loop until a byte comes in
    in a, (UART_FIFOA)  ; get the byte
    ret

sd_getblock:    ; read a block specified by bcde (LBA) into the buffer
    ld a, (sd_cap)  ; we need to shift the block value left by 9 if card is not LBA
    bit 0, a
    call z, lba_adj
    CS 0FEh
    ld a, 17            ; CMD17: read block
    call sd_cmd
sd_getblock_wait:      ; wait for start token
    call mid_rx
    ld e, a
    cp 0FEh
    jp z, sd_getblock_read  ; start token
    cp 0FFh
    jp z, sd_getblock_wait  ; still waiting
    jp sd_getblock_done
sd_getblock_read:
    ld bc, 512
    ld hl, (dma)
    call sd_rx      ; get block
    call mid_rx
    call mid_rx     ; get crc and discard
sd_getblock_done:
    CS 0FFh
    ret

sd_dummy:
    call mid_rx
    dec b
    jp nz, sd_dummy     ; send dummy byte
    ret

sd_cmd:
    push af
    call mid_rx          ; send a dummy byte
    pop af
    set 6, a
    call mid_xfer        ; command | 0x40
    ld a, b
    call mid_xfer        ; arg>>24
    ld a, c
    call mid_xfer        ; arg>>16
    ld a, d
    call mid_xfer        ; arg>>8
    ld a, e
    call mid_xfer        ; arg
    ld a, h
    call mid_xfer        ; checksum
sd_cmd_wait:
    call mid_rx
    cp 0FFh
    jp z, sd_cmd_wait   ; loop until we get reply
    ld e, a
    ret

sd_rx:  ; recieve bytes from SD card into memory.
        ; number of bytes in bc, start address in hl
sd_rx_1:
    ld a, b
    and a
    jp nz, sd_rx_2
    ld a, c
    and a
    ret z
sd_rx_2:
    call mid_rx
    ld (hl), a
    inc hl
    dec bc
    jp sd_rx_1

lba_adj:    ; LBA -> byte addr
    ld b, c ; shift left by 8
    ld c, d
    ld d, e
    ld e, 0
    sla d   ; shift left by one
    rl c
    rl b
    ret

; MID functions
mid_rx:
    ld a, 0FFh          ; enter here to just receive a byte
mid_xfer:
    out (MID_FMB), a    ; enter here to exchange a byte
mid_xfer_wait:
    in a, (MID_ST)
    bit 7, a
    jp z, mid_xfer_wait
    in a, (MID_FMB)
    ret

; interrupt stuff
int_handler:    ; Jump here on RST6
    ex af, af'
    exx
int_handler_loop:
    in a, (UART_SRB)    ; check fifo status
    bit 0, a            ; check bit 0 (RxRDYA)
    jp z, int_handler_done    ; loop until a byte comes in
    call parse_nmea
    jp int_handler_loop
int_handler_done:
    ld a, 00100000b     ; non-specific EOI
    out (PIC_BASE), a
    exx
    ex af, af'
    ei
    ret

; strings (null terminated)
str_crlf: db CR,LF,0
str_prompt: db CR,LF,'>',0
str_signon: db CR,LF,"RubeCron ROM Monitor",CR,LF,"V2.0 by AB0TJ, 2021",CR,LF,0
str_hextype: db "(r)aw or (i)ntel:",0
str_start: db CR,LF,"start:",0
str_end: db CR,LF,"  end:",0
str_addr: db "addr:",0
str_hexeof: db ":00000001FF",CR,LF,0
str_ckerr: db " cksum err",0
str_unsup: db " unsupported", 0
str_port: db "port:",0
str_byte: db CR,LF,"byte:",0
str_gprmc: db "$GPRMC",0
str_error: db CR,LF,CR,LF,"WARNING: Errors detected.",CR,LF,0
str_gps: db "Dumping GPS stream. Ctrl-C to stop.",CR,LF,0
str_pubx: db "$PUBX,40,",0
str_nmea_dis: db ",0,0,0,0,0,0*",0
str_nmea_msgs: db "GLL",0,"GSV",0,"GSA",0,"GGA",0,"VTG",0,"ZDA",0
str_nmea_cksums: db "5C",0,"59",0,"4E",0,"5A",0,"5E",0,"44",0
str_init: db "SD card initialized. Loading bootloader...",CR,LF,0
str_boot: db "Done. Booting...",CR,LF,CR,LF,0
str_sderr: db "Error initializing SD card!",CR,LF,0
str_nosdboot: db "SD card is not bootable.",CR,LF,0
str_af: db "AF:",0
str_bc: db " BC:",0
str_de: db " DE:",0
str_hl: db " HL:",0
str_pc: db " PC:",0