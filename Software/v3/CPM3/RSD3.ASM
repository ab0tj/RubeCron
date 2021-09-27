
TITLE	'CPM3 MODULE FOR RubeCron SD CARD.'

; Assume a >>>>>>>>> BANKED <<<<<<<<<< Version of CPM3
;
;	12/17/2010	V1.0		John Monahan (monahan@vitasoft.org)
;	07/30/2021  V1.1		Alex Swedenburg


FALSE	EQU	0
TRUE	EQU	1
;
LF	EQU	0AH
CR	EQU	0DH
BELL	EQU	07H
CLEAR	EQU	1AH			;SD Systems Video Board Clear Screen
TAB	EQU	09H
ESC	EQU	1BH
EOL	EQU	1CH			;SD Systems Video Board Clear to end of line


;	PORTS FOR RUBECRON BOARD

MID$BASE	EQU 010h
MID$FMB	    EQU MID$BASE + 01h
MID$CS      EQU MID$BASE + 0Ah
MID$ST      EQU MID$BASE + 0Eh
UART$BASE	EQU 020H
UART$SRA    EQU UART$BASE + 1	
UART$FIFOA	EQU UART$BASE + 3

SD$DELAY	EQU 15			; About 2 seconds @ 4MHz SPI clock

	; DEFINE PUBLIC LABELS:
	PUBLIC	DPH0,DPH1,DPH2		;DISK PARAMETER HEADERS

	; DEFINE EXTERNAL LABELS:
	EXTRN	@ADRV,@RDRV
	EXTRN	@DMA,@TRK,@SECT
	EXTRN	@CBNK
	EXTRN	@DBNK			;BANK FOR DMA OPERATION
	EXTRN	@ERMDE			;BDOS ERROR MODE
	EXTRN	?WBOOT			;WARM BOOT VECTOR
	EXTRN	?PMSG			;PRINT MESSAGE @<HL> UP TO 00, SAVES [BC] AND [DE]
	EXTRN	?PDERR			;PRINT BIOS DISK ERROR HEADER
	EXTRN	?CONIN,?CONO		;CONSOLE IN AND OUT
	EXTRN	?CONST			;CONSOLE STATUS
	EXTRN	?BNKSL			;SELECT PROCESSOR MEMORY BANK
	EXTRN	SD$CAP			; SD card capacity flag

	; INCLUDE CP/M 3.0 DISK DEFINITION MACROS:
	MACLIB CPM3

	; INCLUDE Z-80 MACRO LIBRARY:
	MACLIB Z80

	DSEG				;PUT IN OP SYS BANK IF BANKING

	; EXTENDED DISK PARAMETER HEADER FOR DRIVE 0: (A:)
	DW	WRITE$SECTOR		;FD SEC WRITE ROUTINE
	DW	READ$SECTOR		;FD SEC READ ROUTINE
	DW	FLOPPY$LOGIN$0		;FLOPPY DISK "A:" LOGIN PROCEDURE
	DW	FLOPPY$INIT$0		;FLOPPY DISK "A:" DRIVE INITIALIZATION ROUTINE
	DB	0			;RELATIVE DRIVE 0 ON THIS CONTROLLER
	DB	0			;MEDIA TYPE KNOWN 
					;HI BIT SET : DRIVE NEEDS RECALIBRATING

DPH0:	DPH	0,DISK$DPB
					;Bytes 0-24 used by DPH/CPM
	DW	512			;25, 128 Bytes per sector count
	DB	0			;27, Drive Hardware Select

	
	; EXTENDED DISK PARAMETER HEADER FOR DRIVE 1: (B:)
	DW	WRITE$SECTOR		;FD SEC WRITE ROUTINE
	DW	READ$SECTOR		;FD SEC READ ROUTINE
	DW	FLOPPY$LOGIN$1		;FLOPPY DISK "B:" LOGIN PROCEDURE
	DW	FLOPPY$INIT$1		;FLOPPY DISK "B:" INITIALIZATION ROUTINE
	DB	1			;RELATIVE DRIVE 1 ON THIS CONTROLLER
	DB	0			;MEDIA TYPE KNOWN
					;HI BIT SET : DRIVE NEEDS RECALIBRATING

DPH1:	DPH	0,DISK$DPB
					;Bytes 0-24 used by DPH/CPM
	DW	512			;25, 128 Bytes per sector count
	DB	1			;27, Drive Hardware Select

	; EXTENDED DISK PARAMETER HEADER FOR DRIVE 2: (C:)
	DW	WRITE$SECTOR		;FD SEC WRITE ROUTINE
	DW	READ$SECTOR		;FD SEC READ ROUTINE
	DW	FLOPPY$LOGIN$2		;FLOPPY DISK "C:" LOGIN PROCEDURE
	DW	FLOPPY$INIT$2		;FLOPPY DISK "C:" DRIVE INITIALIZATION ROUTINE
	DB	2			;RELATIVE DRIVE 2 ON THIS CONTROLLER
	DB	0			;MEDIA TYPE KNOWN
					;HI BIT SET : DRIVE NEEDS RECALIBRATING

DPH2:	DPH	0,DISK$DPB
					;Bytes 0-24 used by DPH/CPM
	DW	512			;25, 512 Bytes per sector count
	DB	2			;27, Drive Hardware Select


	; CPM3 XDPH offset definitions:-
					;offset constants for the above XDPH's 
BYTE$COUNT	equ	25		;# bytes/sector for this disk offset
DRIVE$SELECT	equ	27		;Drive Hardware Select (Relative Drive Offset does not work)
	

	; MAKE SURE DPB'S ARE IN COMMON MEMORY:

	CSEG

DISK$DPB:
	DPB	512,32,512,2048,1024,1,8000H	; 512b/sec, 32 SPT, 512 tracks, 2k alloc, 1k dir entries, 1 sys trk, non-removable = 8MB


	DSEG				;CAN SET BACK TO BANKED SEGMENT IF BANKING

	;;;;; INIT:			;Initilization routines for each floppy drive
	
FLOPPY$INIT$0:		
FLOPPY$INIT$1:
FLOPPY$INIT$2:
	RET
	
;-----------------LOGIN OF FLOPPY DISK -----------------------------------------
;
;	on entry:	[de] = pointer to XDPH of drive to be logged-in
;
;	on exit:	XDPH address on the stack at entry has been
;			replaced with a null address if the drive has
;			been determined to be an invalid one

FLOPPY$LOGIN$0:				;Restore Floppy Drive 0, to Track 0,
FLOPPY$LOGIN$1:	
FLOPPY$LOGIN$2:
	XRA	A
	RET				;Return Z as no error


CSL MACRO
	MVI A,0FEH
	OUT MID$CS
	ENDM

CSH MACRO
	MVI A,0FFH
	OUT MID$CS
	ENDM

;----------------------- SECTOR WRITE ROUTINE -------------------------------------------
;
;		ROUTINE WRITES 1 SECTOR TO THE DISK:
;
;	on entry:	[de] = XDPH address for current drive
;
;	on exit:	[a] =   0 --> successful write operation
;				1 --> unsuccessful write operation
;                               2 --> Disk is write protected
;
; Assumes valid track in (@TRK) & (@SECT), Address in (@DMA) from CPM 
;
;----------------------------------------------------------------------------------------

WRITE$SECTOR:
 	PUSH	D			;Save it just in case CPM uses it later
 	PUSHIX				;Save [IX]
 	PUSH	D			;[DE]->[IX]
 	POPIX				;get XDPH address for current requested drive to [ix]

	CALL SET$LBA		; Drive/Track/Sector -> LBA
	
 	JMP	ADJ$BANK1
	
 	CSEG
;=======================================
ADJ$BANK1:
 	LDA	@CBNK
 	PUSH	PSW
 	LDA	@DBNK
 	CALL	?BNKSL

SD$PUTBLOCK:		; write a block specified by bcde (LBA) from memory
	CSL
    MVI A,24            	; CMD24: write block
    CALL SD$CMD

	MVI A,0FEH
	CALL MID$XFER			;Send start token
TX:
	LHLD @DMA				;Get DMA address
	LXI D, 512				;Bytes/sector for this disk format
TX$1:
    MOV A,D
    ANA A
    JRNZ TX$2
    MOV A,E
    ANA A
    JRZ TX$DONE
TX$2:
	MOV A,M
    CALL MID$XFER
    INX H
    DCX D
    JR TX$1
TX$DONE:
	CALL MID$RX
	CPI 0FFH
	JRZ TX$DONE
	ANI 01FH
	MOV C,A

WR$WAIT:			; Card will send 0's until write completes
	CALL MID$RX
	ANA A
	JRZ WR$WAIT 

	CSH
	POP	PSW
	CALL	?BNKSL
	JMP	CHECK$WR
;=======================================
	DSEG
	
CHECK$WR:
	MOV A,C
	CPI 05H
	JRNZ	WRITE$ERROR
	XRA A
	POPIX				;Return with original IX
	POP	D			;and DE
	RET				;Ret Z

WRITE$ERROR:
 	LXI	H,DISK$WRITE$ERR	;"Sector Write Error"
 	JMP	SEC$ERROR

;---------------FLOPPY SECTOR READ ROUTINE ------------------------------------------------
;
;		ROUTINE READS 1 SECTOR TO THE DISK:
;
;	on entry:	[de] = XDPH address for current drive
;
;	on exit:	[a] =   0 --> successful read operation
;				1 --> unsuccessful read operation
;
; Assumes valid track in (@TRK) & (@SECT), Address in (@DMA). 
; If the disk is double sided then the ZFDC board assumes 1 to NSCTRS+1 on Side A 
; and NSCTRS+1 to (NSCTRS X 2)+1 on Side B
;
; Note: The XDPH table is used to get the current drive and sector byte count.  
; These are  IX+DRIVE$SELECT and IX+BYTE$COUNT from the above FLOPPY$LOGIN routine.
;--------------------------------------------------------------------------------------------

READ$SECTOR:
	PUSH D				;Save it just in case CPM uses it later
	PUSHIX				;Save [IX]
	PUSH	D			;[DE]->[IX]
	POPIX				;get XDPH address for current requested drive to [ix]
	
	CALL SET$LBA		; Get LBA address from drive/track/sector

	JMP	ADJ$BANK2
	
	CSEG
;=======================================
ADJ$BANK2:
	LDA	@CBNK			;get current bank
	PUSH	PSW
	LDA	@DBNK			;Get Destination Bank. MUST HAVE THIS CODE IN COMMON
	CALL	?BNKSL			;NOW DMA ADDRESS IS AT THE CORRECT BANK

SD$GETBLOCK:		; read a block specified by bcde (LBA) into memory
	CSL
    MVI A,17            	; CMD17: read block
    CALL SD$CMD
SD$GETBLOCK$WAIT:      		; wait for start token
    CALL MID$RX
    CPI 0FEh
    JRZ RX				  	; start token
    CPI 0FFh
    JZ SD$GETBLOCK$WAIT  	; still waiting
    JMP SD$GETBLOCK$ERROR	; error
RX:
	LHLD @DMA				;Get DMA address
	LXI D, 512				;Bytes/sector for this disk format
RX$1:
    MOV A,D
    ANA A
    JRNZ RX$2
    MOV A,E
    ANA A
    JZ RX$DONE
RX$2:
    CALL MID$RX
    MOV M,A
    INX H
    DCX D
    JR RX$1
RX$DONE:
	CALL MID$RX
	CALL MID$RX			; get CRC bytes and discard
	CSH
	MVI C,0FEH				; FE = no error
    JR RD$DONE

SD$GETBLOCK$ERROR:
	MVI C,A				; save error

RD$DONE:
	POP	PSW
	CALL	?BNKSL
	JMP	CHECK$RD
;=======================================
	DSEG
	
CHECK$RD:
	MOV A,C
	CPI 0FEH
	JNZ	READ$ERROR
	XRA A
	POPIX				;Return with original IX
	POP	D			;and DE
	RET				;Ret Z

READ$ERROR:
	LXI	H,DISK$READ$ERR	;"Sector Read Error"

SEC$ERROR:
	PUSH	PSW			;Save Returned Error Code
	CALL	?PMSG
	CALL	?PDERR			;Have CPM tell Drive, Track#, sector# etc.
	LXI	H,ERROR$CODE$MSG	;"Error Code Returned = "
	CALL	?PMSG
	POP	PSW			;Get Error Code returned by Board
	CALL	PACC			;Print error # 
	POPIX				;Return with origional IX
	POP	D			;and DE
	XRA	A
	INR	A			;Set to NZ & 1 for read error
	RET		
	
INVALID$DRIVE:
	LXI	H,INVALID$DRIVE$MSG
	CALL	?PMSG
	POPIX				;Return with origional IX
	POP	D			;and DE
	XRA	A
	INR	A			;Set to NZ & 1 for read error
	RET	


SET$LBA:	; Translates CPM drive, track, sector params into SD card LBA address (max 32MB)
	MVI E,0
	LDX	D,DRIVE$SELECT		;[D] <- (IX + DRIVE$SELECT). Drive #
	SLAR D					; DE = drive*512 tracks
	LHLD @TRK
	DAD D					; HL = drive offset + track
	DAD H
	DAD H
	DAD H
	DAD H
	DAD H					; Track * 32 sectors
	XCHG
	LHLD @SECT				; Get sector number
	DAD D					; Add to running total
	XCHG					; DE = sector #
	LXI B,0					; BCDE = sector #

	LDA SD$CAP  	; we need to shift the block value left by 9 if card is not LBA
    ANA 1
    RNZ
	MOV B,C ; shift left by 8
    MOV C,D
    MOV D,E
    MVI E,0
    SLAR D	  ; shift left
    RALR C		; by one
    RALR B		; more bit
	RET


;============== SUPPORT ROUTINES FOR HARDWARE =================================

;============== SUPPORT ROUTINES FOR HARDWARE =================================
;
CSEG

SD$DUMMY:
    CALL MID$RX
    DCR B
    JNZ SD$DUMMY     ; send dummy byte
    RET

SD$CMD:
    MOV L,A
    CALL MID$RX			; give the card time to do things
    MOV A,L
    ORI 40H
    CALL MID$XFER        ; command | 0x40
    MOV A,B
    CALL MID$XFER        ; arg>>24
    MOV A,C
    CALL MID$XFER        ; arg>>16
    MOV A,D
    CALL MID$XFER        ; arg>>8
    MOV A,E
    CALL MID$XFER        ; arg
    MOV A,H
    CALL MID$XFER        ; checksum
SD$CMD$WAIT:
    CALL MID$RX
    CPI 0FFh
    JZ SD$CMD$WAIT   ; loop until we get reply
    MOV E,A
    RET

MID$RX:			; Enter here if only receiving a byte
	MVI A,0FFH
MID$XFER:		; Enter here to send byte in A
    OUT MID$FMB
MID$XFER$WAIT:
    IN MID$ST
    ANI 80H
    JZ MID$XFER$WAIT
    IN MID$FMB
    RET

WAIT$FOR$ACK:				;Delay to wait for SD card to return data. There is a timeout of about 2 sec.
	PUSH	B
	PUSH	D
	LXI	B,0
	MVI	E,SD$DELAY		;Timeout, (about 2 seconds)
WAIT$1:	CALL MID$RX		; Read from SD card
	CPI 0FFH
	JZ	WAIT$2			;still idling
	IN MID$FMB			; Otherwise return with byte in A
	POP	D			;Balance up stack
	POP	B
	RET				;Return NZ if problem, Z if no problem
WAIT$2:	DCR	B
	JNZ	WAIT$1			;Try for ~2 seconds
	DCR	B			;Reset B to 0FFH
	DCR	C
	JNZ	WAIT$1
	DCR	B			;Reset B to 0FFH
	DCR	C
	DCR	E
	JNZ	WAIT$1
	XRA	A
	DCR	A
	POP	D			;Balance up stack
	POP	B
	RET				;Return NZ flag set if timeout AND 0FFH in [A]

DSEG

;Print the acccumulator value on CRT in HEX-ASCII. Routine used for Error reporting
PACC:	PUSH	PSW
	PUSH	B
	PUSH	PSW
	RRC
	RRC
	RRC
	RRC
	CALL	ZCONV
	POP	PSW
	CALl	ZCONV
	POP	B
	POP	PSW
	RET

ZCONV:	ANI	0FH			;HEX to ASCII
	ADI	90H
	DAA
	ACI	40H
	DAA
	MOV	C,A
	CALL	CO
	RET
	
; -------------- For Debugging purposes ------------------------------------
;
CI:	IN	0H			;Direct console input (used only for debugging)
	ANI	02H
	JZ	CI
	IN	1H			;return with character in A
	RET
;		
CO:	IN	0H			;console output (arrive with character in C)
	ANI	04H			;Note character is in C and A on return.
	JZ	CO
	MOV	A,C
	OUT	1H
	RET
	
	
	DSEG
;--------------------------------------------------------------------------------
;
DISK$WRITE$ERR		DB	CR,LF,'SD Card Sector Write Error.',0 
DISK$READ$ERR		DB	CR,LF,'SD Card Sector Read Error.',0 
TIMEOUT$MSG			DB	CR,LF,'SD Operation Timed Out.',0
INVALID$DRIVE$MSG	DB	CR,LF,'Invalid Drive.',0
DISK$WP$ERROR		DB	CR,LF,'This Disk is Write Protected.',0
ERROR$CODE$MSG		DB	CR,LF,'SD Card Error Code = ',0
HMSG				DB	'H.',CR,LF,0
TRACK$MSG			DB	CR,LF,'Track = ',0
SECTOR$MSG			DB	'H.   Sector = ',0
CRLF$MSG			DB	CR,LF,0

END
