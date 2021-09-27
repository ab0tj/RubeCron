
	TITLE	'BOOT LOADER MODULE FOR CP/M 3.0'
	; For RubeCron Z80 board. Originally by John Monahan

	; DEFINE LOGICAL VALUES:
TRUE		EQU	-1
FALSE		EQU	NOT TRUE

	; DETERMINE IF FOR BANK SELECT OR NOT:
BANKED		EQU	TRUE	;FULL BLOWN VERSION

	; DEFINE PUBLIC LABELS:
	PUBLIC	?INIT,?LDCCP,?RLCCP,?TIME
	PUBLIC	OUT$BLOCKS
	PUBLIC	SD$CAP

	; EXTERNALLY DEFINED ENTRY POINTS AND LABELS:
	EXTRN	?PMSG,?CONIN
	EXTRN	@CIVEC,@COVEC,@AIVEC,@AOVEC,@LOVEC
	EXTRN 	@CBNK,?BNKSL

	IF	BANKED
	EXTRN	BANKBUF		;128 BYTE BUFFER IN MOVE MODULE FOR USE
				; DURING COLD AND WARM BOOTS
	ENDIF

	EXTRN	@SEC,@MIN,@HOUR,@DATE	;FIELDS HOLDING CURRENT TIME AND DATE


	; INCLUDE Z-80 MACROS:
	MACLIB	Z80


	; SOME MISCELLANEOUS EQUATES:
BDOS		EQU	5
CR		EQU	13	;ASCII CARRIAGE RETURN
LF		EQU	10	;ASCII LINEFEED
;

	; IO PORTS
UART$BASE 	EQU 020H
UART$SRB    EQU UART$BASE + 9
UART$FIFOB  EQU UART$BASE + 11
PIC$BASE	EQU 030H
LED$SEC 	EQU 50h		; seconds display
LED$MIN     EQU 51h		; MINutes display
LED$HRS 	EQU 52h		; hours display
LED$DAY 	EQU 53h		; DAY display
;
;
	; WE CAN DO INITIALIZATION FROM BANKED MEMORY (IF WE HAVE IT):
	IF	BANKED
	DSEG	; INIT DONE FROM BANKED MEMORY
	ELSE
	CSEG	; INIT TO BE DONE FROM COMMON MEMORY
	ENDIF


	;;;;; ?INIT
	; HARDWARE INITIALIZATION OTHER THAN CHARACTER AND DISK I/O:
?INIT:
	; ASSIGN CONSOLE INPUT AND OUTPUT TO CRT:
	LXI	H,8000H		;SIGNIFIES DEVICE 0
	SHLD	@CIVEC		;CONSOLE INPUT VECTOR
	SHLD	@COVEC		;CONSOLE OUTPUT VECTOR

	; ASSIGN PRINTER TO LPT:
	LXI	H,4000H		;SIGNIFIES DEVICE 1
	SHLD	@LOVEC		;LIST OUTPUT VECTOR

	; ASSIGN AUX TO CRT1:
	LXI	H,02000H	;SIGNIFIES DEVICE 2
	SHLD	@AIVEC		;AUXILLIARY INPUT VECTOR
	SHLD	@AOVEC		;AUXILLIARY OUTPUT VECTOR

	;MVI A,0C3h      ; JP opcode
    ;STA 030H	    ; RST6 calls here
    ;LXI H,INTH		; Interrupt handler address
    ;SHLD 031H
	;IM0
    ;CALL INTH       ; handle any pending interrupts
	DI ;EI			; Let's get CPM working before enabling interrupts

	LDA 0FFFFH
	STA SD$CAP		; Copy SD flags from bootloader

	; PRINT THE SIGN-ON MESSAGE:
	LXI	H,SIGNON$MSG	;POINT TO IT
	JMP	?PMSG		;AND PRINT IT
	RET


	;;;;; OUT$BLOCKS
	; ROUTINE OUTPUTS SPECIFIED # BYTES TO SPECIFIED OUTPUT PORTS:
	IF	BANKED
	CSEG			;WE WANT THIS ROUTINE IN COMMON MEMORY
	ENDIF
OUT$BLOCKS:
	MOV	A,M		;GET A BYTE FROM THE BLOCK
	ORA	A		;END OF OUTPUT BLOCK ?
	RZ			;THEN DONE!!
	MOV	B,A		;ELSE PUT # BYTES TO SEND OUT IN [B]
	INX	H		;POINT TO PORT TO SEND TO
	MOV	C,M		;GET IT TO [C]
	INX	H		;POINT TO 1ST BYTE OF BLOCK TO SEND OUT
	OUTIR			;Z-80 BLOCK OUTPUT
	JR	OUT$BLOCKS


	;;;;; ?LDCCP
	; THIS ROUTINE IS ENTERED TO LOAD THE CCP.COM FILE INTO THE TPA BANK
	;  AT SYSTEM COLD START:
?LDCCP:
	; SET UP THE FCB FOR THE FILE OPERATION:
	XRA	A		;ZERO EXTENT
	STA	CCP$FCB+15
	LXI	H,0		;START AT BEGINNING OF FILE
	SHLD	FCB$NR

	; TRY TO OPEN THE CCP.COM FILE:
	LXI	D,CCP$FCB	;POINT TO FCB
	CALL	OPEN		;ATTEMPT THE OPEN OPERATION
	INR	A		;WAS IT ON THE DISK ?
	JRNZ	CCP$FOUND	;YES -- GO LOAD IT

	; WE ARRIVE HERE WHEN CCP.COM FILE WASN'T FOUND:
	LXI	H,CCP$MSG	;REPORT THE ERROR
	CALL	?PMSG
	CALL	?CONIN		;GET A RESPONSE
	JR	?LDCCP		;AND TRY AGAIN

	; FILE WAS OPENED OK -- READ IT IN:
CCP$FOUND:
	LXI	D,0100H		;LOAD AT BOTTOM OF TPA
	CALL	SETDMA		;BY SETTING THE NEXT DMA ADDRESS
	LXI	D,128		;SET MULTI SECTOR I/O COUNT
	CALL	SETMULTI	; TO ALLOW UP TO 16K BYTES IN ONE OPERATION
	LXI	D,CCP$FCB	;POINT TO THE FCB
	CALL	READ		;AND READ THE CCP IN

	; FOLLOWING CODE FOR BANKED SYSTEMS -- MOVES CCP IMAGE TO BANK 2
	;  FOR LATER RELOADING AT WARM STARTS:
	IF	BANKED
	LXI	H,0100H		;GET CCP IMAGE FROM START OF TPA
	MVI	B,25		;TRANSFER 25 LOGICAL SECTORS
	LDA	@CBNK		;GET CURRENT BANK
	PUSH	PSW		;AND SAVE IT
LD$1:
	PUSH	B		;SAVE SECTOR COUNT
	MVI	A,1		;SELECT TPA BANK
	CALL	?BNKSL
	LXI	B,128		;TRANSFER 128 BYTES TO TEMPORARY BUFFER
	LXI	D,BANKBUF	;TEMPORARY BUFFER ADDR IN [DE]
	PUSH	H		;SAVE SOURCE ADDRESS
	PUSH	D		;AND DESTINATION
	PUSH	B		;AND COUNT
	LDIR			;BLOCK MOVE SECTOR TO TEMPORARY BUFFER
	MVI	A,2		;SELECT BANK TO SAVE CCP IN
	CALL	?BNKSL
	POP	B		;GET BACK COUNT
	POP	H		;LAST DESTINATION WILL BE NEW SOURCE ADDR
	POP	D		;LAST SOURCE WILL BE NEW DESTINATION
	LDIR			;BLOCK MOVE SECTOR FROM BUFFER TO ALTERNATE
				; BANK
	XCHG			;NEXT ADDR WILL BE NEW SOURCE ADDR
	POP	B		;GET BACK SECTOR COUNT
	DJNZ	LD$1		;DROP SECTOR COUNT AND LOOP TILL DONE...
	POP	PSW		;WHEN DONE -- RESTORE ORIGINAL BANK
	JMP	?BNKSL
	ELSE

	; IF NON-BANKED WE RETURN THROUGH HERE:
	RET
	ENDIF


	;;;;; ?RLCCP
	; ROUTINE RELOADS CCP IMAGE FROM BANK 2 IF BANKED SYSTEM OR FROM THE
	;  DISK IF NON-BANKED VERSION:
?RLCCP:
	IF	BANKED
	; FOLLOWING CODE FOR BANKED VERSION:
	LXI	H,0100H		;GET CCP IMAGE FROM START OF ALTERNATE BUFFER
	MVI	B,25		;TRANSFER 25 LOGICAL SECTORS
	LDA	@CBNK		;GET CURRENT BANK
	PUSH	PSW		;AND SAVE IT
RL$1:
	PUSH	B		;SAVE SECTOR COUNT
	MVI	A,2		;SELECT ALTERNATE BANK
	CALL	?BNKSL
	LXI	B,128		;TRANSFER 128 BYTES TO TEMPORARY BUFFER
	LXI	D,BANKBUF	;TEMPORARY BUFFER ADDR IN [DE]
	PUSH	H		;SAVE SOURCE ADDRESS
	PUSH	D		;AND DESTINATION
	PUSH	B		;AND COUNT
	LDIR			;BLOCK MOVE SECTOR TO TEMPORARY BUFFER
	MVI	A,1		;PUT CCP TO TPA BANK
	CALL	?BNKSL
	POP	B		;GET BACK COUNT
	POP	H		;LAST DESTINATION WILL BE NEW SOURCE ADDR
	POP	D		;LAST SOURCE WILL BE NEW DESTINATION
	LDIR			;BLOCK MOVE SECTOR FROM BUFFER TO TPA BANK
	XCHG			;NEXT ADDR WILL BE NEW SOURCE ADDR
	POP	B		;GET BACK SECTOR COUNT
	DJNZ	RL$1		;DROP SECTOR COUNT AND LOOP TILL DONE...
	POP	PSW		;GET BACK LAST CURRENT BANK #
	JMP	?BNKSL		;SELECT IT AND RETURN
	ELSE

	; FOLLOWING CODE IS FOR NON-BANKED VERSIONS:
	JMP	?LDCCP		;JUST DO LOAD AS THOUGH COLD BOOT
	ENDIF


	;;;;; ?TIME
	; ROUTINE SETS/GETS TIME From clock interrupt memory locations
?TIME:	
	; SETTING OR GETTING TIME ?
	MOV	A,C
	ORA	A
	RNZ		; No need to set time as it is done by the GPS
	DI		; don't want values changing on us while we read the date
	PUSH B
	PUSH D
	PUSH H
; get time
	; hour, minute, second are already stored in BCD
	LDA TM$HRS
	STA @HOUR
	LDA TM$MIN
	STA @MIN
	LDA TM$SEC
	STA @SEC
;
; Calculate how many whole years elapsed from 31-12-1977 to now
; Stolen from: https://github.com/agn453/Z80-MBC2-master/blob/master/bios/BOOT.ASM
	lda TM$YR
	call BCD2BIN
    adi     22
    mov     c, a                ; C = elapsed_years = (TM$YR) + 22
    ;
    ; Convert it in days into HL (16 bit)
    xra     a                   ; A = 0
    mov     b, a                ; B = 0. BC = elapsed_years (16bit)
    lxi     d, 365              ; DE = 365
    call    Mult16              ; HL = elapsed_years * 365 = elapsed_years_days
    ;
    ; Calculate how many whole leap years elapsed from 31-12-1977 to now
    ;  (current year excluded) and add it to elapsed_years_days
    lda     TM$YR
    ora     a
    jrz     addleapyrs          ; If TM$YR = 00 -> A = 0
    dcr     a                   ; If TM$YR > 00 -> A = (RTRYEAR) - 1
    srlr    a                   ; / 2S
    srlr    a                   ; / 4
    inr     a                   ; If TM$YR > 00 -> A = (((TM$YR) - 1) / 4) + 1 =
                                ;  leap years from 31-12-1999 to now (current year excluded)
addleapyrs:
    adi     5                   ; Add leap years from 31-12-1977 to 31-12-1999
    mov     c, a                ; C = elapsed_leap_years = (((TM$YR) - 1) / 4) + 1 + 5
    xra     a                   ; A = 0
    mov     d, a                ; D = 0
    mov     b ,a                ; B = 0. BC = elapsed_leap_years (16bit)
    dad     b                   ; HL = elapsed_years_days + elapsed_leap_years
    ;
    ; Add days of current month
    lda     TM$DAY
    mov     c, a                ; BC = days_of_current_month (16bit)
    dad     b                   ; Add to HL days_of_current_month (BC)
    lda     TM$MO            	; A = current_month
    dcr     a                   ; A = Number_of_months_before_current
    jrz     checkCurrYear       ; Jump if Number_of_months_before_current = 0
    ;
    ; Add days of all previous months of current year, starting with January
    lxi     b, MONTHTAB         ; BC -> days_of_month (starting from Jan)
addDays:
    push    a                   ; Save A = Number_of_months_before_current
    ldax    b                   ; A = days_of_month (month pointed by BC)
    mov     e, a                ; DE = days of month (16bit)
    dad     d                   ; Add it to HL
    inx     b                   ; BC -> next month
    pop     a
    dcr     a                   ; There is an other month?
    jrnz    addDays             ; Jump if there is an other month to compute
    ;
    ; If current year is a leap year and current month is > February add one DAY
    ; to HL
checkCurrYear:
    lda     TM$MO
    cpi     3                   ; Current month < March?
    jrc     TIMEend             ; Jump if yes
    lda     TM$YR             ; No, check if current year is leap
    mov     b, a                ; A = B = current year
    srlr    a                   ; / 2
    srlr    a                   ; / 4
    slar    a                   ; * 2
    slar    a                   ; * 4
    cmp     b                   ; A = B if A is leap
    jrnz    TIMEend             ; Jump if not leap
    inx     h                   ; Add 1 to HL
TIMEend:
    ;
    ; All done, store days from 31-12-1977 to now into @DATE
    shld    @DATE               ; Store the date in days from CP/M epoch
    pop     h
    pop     d
    pop     b
	ei
    ret
    
MONTHTAB:
    DB      31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30  ; Only Jan-Nov needed
    
; Multiply 16-bit values (with 16-bit result)
; In: Multiply BC with DE
; Out: HL = result
;
Mult16: 
    mov     a,  b
    mvi     b, 16
Mult16Loop:
    dad     h
    slar    c
    ral
    jrnc    Mult16NoAdd
    dad     d
Mult16NoAdd:
    djnz    Mult16Loop
    ret    

BCD2BIN:	; convert packed BCD to binary value
	MOV C,A		; save A
	ANI 0FH		; mask off high nibble (left with ones place)
	MOV D,A		; save
	MOV A,C
	ANI 0F0H	; mask off low nibble
	SRLR C
	SRLR C
	SRLR C		; C = tens place / 5
	SRLR A		; A = (tens/10) * 8
	ADD C		; *10
	ADD D		; add ones place back in
	RET
;
;
	IF	BANKED
	CSEG
	ENDIF

INTH:	; interrupt handler
	RET	; disable for now
	EXAF
    EXX
INTH$LOOP:
    IN UART$SRB    ; check fifo status
    ANI 1            ; check bit 0 (RxRDYA)
    JZ INTH$DONE    ; done if no more bytes
    CALL PARSE$NMEA
    JMP INTH$LOOP
INTH$DONE:
    MVI A, 00100000B     ; non-specific EOI
    OUT PIC$BASE
    EXX
    EXAF
    EI
    RET

; clock routines
PARSE$NMEA: ; handle an incoming nmea byte
	LHLD RMC$MATCH		; get current match position
	IN UART$FIFOB		; get the byte from the uart
	MOV C,A				; stash it in c
	CMP M				; does it match gprmc string?
	JZ NMEA$MATCH		; handle if so
	MOV A,M				; get char at pointer
	ORA A				; is it zero?
	JZ PARSE$RMC		; we're parsing rmc message if so
	LXI H, STR$GPRMC	; reset rmc string
	SHLD RMC$MATCH	 	;   pointer
    RET

NMEA$MATCH: ; matched a char in gprmc string
	INX H				; increment pointer
	SHLD RMC$MATCH		; save it
    RET

PARSE$RMC: ; parse a byte in the rmc message
	MOV A,C				; is this
	CPI ','				;   a comma?
	JZ PARSE$RMC$SEP	; handle it if so
	LDA RMC$FLD			; get field number
	CPI 1				; time field
	JZ PARSE$RMC$TIME	; handle time
	CPI 9				; date field
	JZ PARSE$RMC$DATE	; handle date
    RET

PARSE$RMC$SEP: ; handle nmea field seperator
	LDA RMC$FLD			; get current field number
	INR A				; increment it
	STA RMC$FLD			; save it
	XRA A				; a=0
	STA RMC$POS			; reset field position
    RET

PARSE$RMC$TIME: ; handle gprmc time field
	LDA RMC$POS		    ; get field position
	INR A			    ; increment it
	STA RMC$POS			; save it
	CPI 1			    ; HRS 10's place
	JNZ PARSE$RMC$T2	; not at position 1
	LXI D,TM$HRS		; put in hour value
	CALL SET$HIGH$BCD	; save the value
    RET
PARSE$RMC$T2:
	CPI 2			    ; HRS 1's place
	JNZ PARSE$RMC$T3	; not at position 2
	LXI D,TM$HRS		; put in hour value
	CALL SET$LOW$BCD	; save the value
	OUT LED$HRS			; send to display
    RET
PARSE$RMC$T3:
	CPI 3			    ; MIN 10's place
	JNZ PARSE$RMC$T4	; not at position 3
	LXI D,TM$MIN		; put in MINute value
	CALL SET$HIGH$BCD	; save the value
    RET
PARSE$RMC$T4:
	CPI 4			    ; MIN 1's place
	JNZ PARSE$RMC$T5	; not at position 4
	LXI D,TM$MIN		; put in MINute value
	CALL SET$LOW$BCD	; save the value
	OUT LED$MIN			; send to display
    RET
PARSE$RMC$T5:
	CPI 5			    ; SEC 10's place
	JNZ PARSE$RMC$T6	; not at position 5
	LXI D,TM$SEC		; put in second value
	CALL SET$HIGH$BCD	; save the value
    RET
PARSE$RMC$T6:
	CPI 6		    	; SEC 1's place
	RNZ		      		; done with time field
	LXI D,TM$SEC		; put in second value
	CALL SET$LOW$BCD	; save the value
	OUT LED$SEC			; send to display
    ret
	
PARSE$RMC$DATE:
	LDA RMC$POS			; get field position
	INR A			    ; increment it
	STA RMC$POS			; save it back to memory
	CPI 1			    ; DAY 10's place
	JNZ PARSE$RMC$D2	; not position 1
	LXI D,TM$DAY		; put in DAY value
	CALL SET$HIGH$BCD	; save the value
    RET
PARSE$RMC$D2:
	CPI 2			    ; DAY 1's place
	JNZ PARSE$RMC$D3	; not position 2
	LXI D,TM$DAY		; put in DAY value
	CALL SET$LOW$BCD	; save the value
	OUT LED$DAY			; send to display
    RET
PARSE$RMC$D3:
	CPI 3			    ; month 10's place
	JNZ PARSE$RMC$D4	; not position 3
	LXI D,TM$MO			; store in month value
	CALL SET$HIGH$BCD	; save the value
    RET
PARSE$RMC$D4:
	CPI 4			    ; month 1's place
	JNZ PARSE$RMC$D5	; not position 4
	LXI D,TM$MO			; put in month value
	CALL SET$LOW$BCD	; save the value
    RET
PARSE$RMC$D5:
	CPI 5			    ; year 10's place	
	JNZ PARSE$RMC$D6	; not position 5
	LXI D,TM$YR			; put in year value
	CALL SET$HIGH$BCD	; save value
    RET
PARSE$RMC$D6: ; only option left is position 6
	LXI D,TM$YR			; put in year value
	CALL SET$LOW$BCD	; save value
	XRA A				; a=0
	STA RMC$FLD			; wait for next gprmc
	LXI H,STR$GPRMC		; reset gprmc pointer
	SHLD RMC$MATCH		
    RET

SET$HIGH$BCD:
; ascii value in c, store in de
	MOV A,C
	SBI 30H			; ascii adjust
	RLC				; put
	RLC				; in
	RLC				; high
	RLC				; nibble
	STAX D			; put in address de
	RET				; done

SET$LOW$BCD:
; ascii value in c, store in de
	LDAX D			; get value
	ADD C			; add current byte
	SUI 30H			; ascii adjust
	STAX D			; save in memory
	RET

	;;;;;
	; CP/M BDOS FUNCTION INTERFACES

	; OPEN FILE:
OPEN:
	MVI C,15 ! JMP BDOS		; OPEN FILE CONTROL BLOCK

	; SET DMA ADDRESS:
SETDMA:
	MVI C,26 ! JMP BDOS		; SET DATA TRANSFER ADDRESS

	; SET MULTI SECTOR I/O COUNT:
SETMULTI:
	MVI C,44 ! JMP BDOS		; SET RECORD COUNT

	; READ FILE RECORD:
READ:
	MVI C,20 ! JMP BDOS		; READ RECORDS

	; CCP NOT FOUND ERROR MESSAGE:
CCP$MSG:
	DB	CR,LF,'BIOS ERR ON A: NO CCP.COM FILE',0


	; FCB FOR CCP.COM FILE LOADING:
CCP$FCB:
	DB	1		;AUTO-SELECT DRIVE A
	DB	'CCP     COM'	;FILE NAME AND TYPE
	DB	0,0,0,0
	DS	16
FCB$NR:	DB	0,0,0

	; SYSTEM SIGN-ON MESSAGE:
SIGNON$MSG:
	DB	CR,LF,LF,'64K CP/M BANKED VERSION 3.0 (Alex Swedenburg 7/27/2021)'
	DB	CR,LF,'Drives A,B,C as 8MB hard drive images on SD card'
	DB	CR,LF,LF,0

SD$CAP DB 0	    ; SD card capacity flag

; GPS time storage
STR$GPRMC: DB '$GPRMC',0
TM$HRS: DB 1		; Current hour
TM$MIN: DB 1		; Current minute
TM$SEC: DB 1		; Current second
TM$DAY: DB 1		; Current day
TM$MO: DB 1			; Current month
TM$YR: DB 1			; Current year
RMC$MATCH: DW 1		; pointer to position in $GPRMC
RMC$FLD: DB 1		; current parse field in rmc messgae
RMC$POS: DB 1		; current parse position in field

END
