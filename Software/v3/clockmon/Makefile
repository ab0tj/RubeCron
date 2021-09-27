C=z80asm
CFLAGS=-I ../include/

all: clockmon

clockmon: clockmon.asm ../include/uart.inc ../include/tp3465v.inc
	$(C) $(CFLAGS) clockmon.asm -o clockmon.bin --list=clockmon.lst --label=../include/clockmon.inc
	srec_cat clockmon.bin -binary -offset 0xF700 -o clockmon.hex -intel

clean:
	rm *.bin *.hex *.lst