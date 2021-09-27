; RubeCron Simple Monitor

.hexfile rubemon.hex
.binfile rubemon.bin
.download hex

.org 0h         ; after reset, 0-7fff is rom, 8000-ffff is ram
startmem                equ 08000h
endmem                  equ 0ffffh

stack   equ endmem - 010h ; stack pointer

; i/o ports
8155_cmd	equ 000h	; 8155 command/status register
usb_data        equ 030h        ; usb module
usb_stat        equ 003h        ; 8155 port c

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
;main program
        lxi sp, stack   ; initialize the stack pointer

; initialize hardware, show signon message

        call usb_out_crlf      ; start on a fresh line

; init memory
        lxi h, initmem_str
        call usb_out_str       ; show message

        lxi h, startmem         ; beginning of testable memory
        lxi d, endmem           ; end of testable memory

initmem_loop_1:
        mvi a, 0ffh
        mov m, a
        cmp m                   ; is this what we stored?
        jnz initmem_error       ; that's not right
        inx h                   ; next memory address
        mov a, h
        cmp d
        jc initmem_loop_1
        mov a, l
        cmp e
        jc initmem_loop_1       ; not at end of memory?
        lxi h, end              ; we are; time for next pass

initmem_loop_2:
        xra a                   ; zero
        mov m, a
        cmp m                   ; is this what we stored?
        jnz initmem_error       ; no...
        inx h                   ; next memory address
        mov a, h
        cmp d
        jc initmem_loop_2
        mov a, l
        cmp e
        jc initmem_loop_2       ; not at end of memory?
        jmp initmem_done        ; we are. move on.

initmem_error:
        lxi h, initmem_error_str
        call usb_out_str       ; tell the user
        mov d, h                ; high byte
        call usb_out_hex
        mov d, l                ; low byte
        call usb_out_hex
        call usb_out_crlf
initmem_done:

; initialize 8155
	xra a			; a=0 (set all ports to input)
	out 8155_cmd		; set command register 

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
        cpi ':'					; intel hex
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
        lxi h, stack+1          ; use the temp space just above the stack
        mvi m, 0DBh             ; "in"
        inx h                   ; increment pointer
        call usb_in_hex
        mov m, a                ; port number
        call usb_out_crlf
        inx h
        mvi m, 0C9h             ; "ret"
        call stack+1            ; call input instruction
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
        lxi h, stack+1
        mvi m, 0D3h             ; "out"
        inx h                   ; increment pointer
        mov m, b                ; port number
        inx h
        mvi m, 0C9h             ; "ret"
        call stack+1
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
        in usb_stat             ; check fifo status
        ani usb_tx_rdy
        jnz usb_out             ; loop until we can send this byte
        mov a, c
        out usb_data           ; send the byte
        ret

usb_in: ; get a byte from the console, store in a
        in usb_stat             ; check fifo status
        ani usb_rx_rdy
        jnz usb_in              ; loop until a byte comes in
        in usb_data            ; get the byte
        ret

; strings (null terminated)
crlf_str: db CR,LF,0
initmem_str: db "Initializing memory...",CR,LF,0
initmem_error_str: db "Memory error at ",0
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

end: