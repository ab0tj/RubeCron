; RubeCron Flash ID Check

org 9000h         ; 0-7fff is rom, 8000-ffff is ram

include 'clockmon.inc'

; CODE STARTS HERE
;main program
        ld sp, stack   ; initialize the stack pointer

        xor a
        out (MMU_PG0), a
        inc a
        out (MMU_PG1), a     ; ROM at bottom 32k

	ld a, 0aah	; enter Software ID mode
	ld (5555h), a
	ld a, 55h
	ld (2aaah), a
	ld a, 90h
	ld (5555h), a

	ld hl, manf_str
	call outstr

	ld a, (0)		; get manufacturer id
	ld d, a			; save in d
	call out_hex    	; send to console

	ld hl, dev_str
	call outstr
	
	ld a, (1)		; get device id
	ld d, a			; save in d
	call out_hex    	; send to console

	ld a, 0f0h		; exit id mode
	ld (0), a

	ld hl, key_str
	call outstr

        call chrin
	jp 8000h	; return to monitor

; strings (null terminated)
manf_str: db CR,LF,"Manufacturer ID: ",0
dev_str:  db CR,LF,"Device ID: ",0
key_str: db CR,LF,"Press any key to continue.",CR,LF,0
end: