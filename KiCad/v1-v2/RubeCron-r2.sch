EESchema Schematic File Version 4
LIBS:RubeCron-r2-cache
EELAYER 30 0
EELAYER END
$Descr A2 23386 16535
encoding utf-8
Sheet 1 1
Title ""
Date "28 jan 2017"
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L RubeCron-cache:8085 U1
U 1 1 587FC2B9
P 2250 2850
F 0 "U1" H 2250 2900 60  0000 C CNN
F 1 "8085" H 2250 2800 60  0000 C CNN
F 2 "" H 2250 2850 60  0000 C CNN
F 3 "" H 2250 2850 60  0000 C CNN
	1    2250 2850
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:8251A U10
U 1 1 587FC342
P 7850 4950
F 0 "U10" H 7850 5000 60  0000 C CNN
F 1 "8251A" H 7850 4900 60  0000 C CNN
F 2 "" H 7850 4950 60  0000 C CNN
F 3 "" H 7850 4950 60  0000 C CNN
	1    7850 4950
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:CRYSTAL X1
U 1 1 587FFB03
P 1050 2900
F 0 "X1" H 1050 3050 60  0000 C CNN
F 1 "4.9152MHz" H 1050 2750 60  0000 C CNN
F 2 "~" H 1050 2900 60  0000 C CNN
F 3 "~" H 1050 2900 60  0000 C CNN
	1    1050 2900
	0    -1   -1   0   
$EndComp
$Comp
L RubeCron-cache:74LS373 U4
U 1 1 58800039
P 4650 1700
F 0 "U4" H 4650 1700 60  0000 C CNN
F 1 "74LS373" H 4700 1350 60  0000 C CNN
F 2 "~" H 4650 1700 60  0000 C CNN
F 3 "~" H 4650 1700 60  0000 C CNN
	1    4650 1700
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:GND-RESCUE-RubeCron #PWR01
U 1 1 5880EA5A
P 1050 3650
F 0 "#PWR01" H 1050 3650 30  0001 C CNN
F 1 "GND-RESCUE-RubeCron" H 1050 3580 30  0001 C CNN
F 2 "" H 1050 3650 60  0000 C CNN
F 3 "" H 1050 3650 60  0000 C CNN
	1    1050 3650
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:GND-RESCUE-RubeCron #PWR02
U 1 1 5880F728
P 3950 2250
F 0 "#PWR02" H 3950 2250 30  0001 C CNN
F 1 "GND-RESCUE-RubeCron" H 3950 2180 30  0001 C CNN
F 2 "" H 3950 2250 60  0000 C CNN
F 3 "" H 3950 2250 60  0000 C CNN
	1    3950 2250
	1    0    0    -1  
$EndComp
Text Label 5450 1200 0    60   ~ 0
A0
Text Label 5450 1300 0    60   ~ 0
A1
Text Label 5450 1400 0    60   ~ 0
A2
Text Label 5450 1500 0    60   ~ 0
A3
Text Label 5450 1600 0    60   ~ 0
A4
Text Label 5450 1700 0    60   ~ 0
A5
Text Label 5450 1800 0    60   ~ 0
A6
Text Label 5450 1900 0    60   ~ 0
A7
Text Label 3100 4100 0    60   ~ 0
RESET
$Comp
L RubeCron-cache:628128 U9
U 1 1 588123F8
P 7250 2400
F 0 "U9" H 7300 2400 70  0000 C CNN
F 1 "628128" H 7550 1200 70  0000 C CNN
F 2 "" H 7250 2400 60  0000 C CNN
F 3 "" H 7250 2400 60  0000 C CNN
	1    7250 2400
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:27C010-RESCUE-RubeCron U11
U 1 1 58812A4D
P 9650 2200
F 0 "U11" H 9750 3200 70  0000 C CNN
F 1 "SST39SF010A" H 9650 800 70  0000 C CNN
F 2 "" H 9650 2200 60  0000 C CNN
F 3 "" H 9650 2200 60  0000 C CNN
	1    9650 2200
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:DIL18 P3
U 1 1 58812C02
P 13750 5000
F 0 "P3" H 13750 5550 70  0000 C CNN
F 1 "USB245R" H 13750 4450 70  0000 C CNN
F 2 "" H 13750 5000 60  0000 C CNN
F 3 "" H 13750 5000 60  0000 C CNN
	1    13750 5000
	1    0    0    -1  
$EndComp
Text Label 3100 4500 0    60   ~ 0
CLK
Text Label 12150 3000 2    60   ~ 0
CLK
Text Label 12150 3100 2    60   ~ 0
BAUD
NoConn ~ 3050 3900
NoConn ~ 3050 3200
Text Label 3100 3700 0    60   ~ 0
~WR
Text Label 3100 3500 0    60   ~ 0
IO/~M
$Comp
L RubeCron-cache:VCC #PWR03
U 1 1 58814E73
P 1450 1150
F 0 "#PWR03" H 1450 1250 30  0001 C CNN
F 1 "VCC" H 1450 1250 30  0000 C CNN
F 2 "" H 1450 1150 60  0000 C CNN
F 3 "" H 1450 1150 60  0000 C CNN
	1    1450 1150
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:GND-RESCUE-RubeCron #PWR04
U 1 1 58814FED
P 1450 2250
F 0 "#PWR04" H 1450 2250 30  0001 C CNN
F 1 "GND-RESCUE-RubeCron" H 1450 2180 30  0001 C CNN
F 2 "" H 1450 2250 60  0000 C CNN
F 3 "" H 1450 2250 60  0000 C CNN
	1    1450 2250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:R-RESCUE-RubeCron R1
U 1 1 5881506D
P 1250 4250
F 0 "R1" V 1330 4250 40  0000 C CNN
F 1 "100k" V 1257 4251 40  0000 C CNN
F 2 "~" V 1180 4250 30  0000 C CNN
F 3 "~" H 1250 4250 30  0000 C CNN
	1    1250 4250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:DIODE D1
U 1 1 5881508B
P 900 4250
F 0 "D1" H 900 4350 40  0000 C CNN
F 1 "1N4148" H 900 4150 40  0000 C CNN
F 2 "~" H 900 4250 60  0000 C CNN
F 3 "~" H 900 4250 60  0000 C CNN
	1    900  4250
	0    -1   -1   0   
$EndComp
$Comp
L RubeCron-cache:SW_PUSH SW1
U 1 1 58815838
P 900 4950
F 0 "SW1" H 1050 5060 50  0000 C CNN
F 1 "RESET" H 900 4870 50  0000 C CNN
F 2 "~" H 900 4950 60  0000 C CNN
F 3 "~" H 900 4950 60  0000 C CNN
	1    900  4950
	0    -1   -1   0   
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C2
U 1 1 58815851
P 1250 4950
F 0 "C2" H 1300 5050 50  0000 L CNN
F 1 "10uF" H 1300 4850 50  0000 L CNN
F 2 "~" H 1250 4950 60  0000 C CNN
F 3 "~" H 1250 4950 60  0000 C CNN
	1    1250 4950
	1    0    0    -1  
$EndComp
Text Label 10450 1300 0    60   ~ 0
D0
Text Label 10450 1400 0    60   ~ 0
D1
Text Label 10450 1500 0    60   ~ 0
D2
Text Label 10450 1600 0    60   ~ 0
D3
Text Label 10450 1700 0    60   ~ 0
D4
Text Label 10450 1800 0    60   ~ 0
D5
Text Label 10450 1900 0    60   ~ 0
D6
Text Label 10450 2000 0    60   ~ 0
D7
Text Label 8050 1350 0    60   ~ 0
D0
Text Label 8050 1450 0    60   ~ 0
D1
Text Label 8050 1550 0    60   ~ 0
D2
Text Label 8050 1650 0    60   ~ 0
D3
Text Label 8050 1750 0    60   ~ 0
D4
Text Label 8050 1850 0    60   ~ 0
D5
Text Label 8050 1950 0    60   ~ 0
D6
Text Label 8050 2050 0    60   ~ 0
D7
$Comp
L RubeCron-cache:74LS245 U5
U 1 1 58816669
P 4650 3000
F 0 "U5" H 4750 3575 60  0000 L BNN
F 1 "74LS245" H 4700 2425 60  0000 L TNN
F 2 "~" H 4650 3000 60  0000 C CNN
F 3 "~" H 4650 3000 60  0000 C CNN
	1    4650 3000
	1    0    0    -1  
$EndComp
Text Label 3150 1200 0    60   ~ 0
AD0
Text Label 3150 1300 0    60   ~ 0
AD1
Text Label 3150 1400 0    60   ~ 0
AD2
Text Label 3150 1500 0    60   ~ 0
AD3
Text Label 3150 1600 0    60   ~ 0
AD4
Text Label 3150 1700 0    60   ~ 0
AD5
Text Label 3150 1800 0    60   ~ 0
AD6
Text Label 3150 1900 0    60   ~ 0
AD7
Text Label 3850 2500 2    60   ~ 0
AD0
Text Label 3850 2600 2    60   ~ 0
AD1
Text Label 3850 2700 2    60   ~ 0
AD2
Text Label 3850 2800 2    60   ~ 0
AD3
Text Label 3850 2900 2    60   ~ 0
AD4
Text Label 3850 3000 2    60   ~ 0
AD5
Text Label 3850 3100 2    60   ~ 0
AD6
Text Label 3850 3200 2    60   ~ 0
AD7
Text Label 5450 2500 0    60   ~ 0
D0
Text Label 5450 2600 0    60   ~ 0
D1
Text Label 5450 2700 0    60   ~ 0
D2
Text Label 5450 2800 0    60   ~ 0
D3
Text Label 5450 2900 0    60   ~ 0
D4
Text Label 5450 3000 0    60   ~ 0
D5
Text Label 5450 3100 0    60   ~ 0
D6
Text Label 5450 3200 0    60   ~ 0
D7
$Comp
L RubeCron-cache:GND-RESCUE-RubeCron #PWR05
U 1 1 58825B87
P 3950 3550
F 0 "#PWR05" H 3950 3550 30  0001 C CNN
F 1 "GND-RESCUE-RubeCron" H 3950 3480 30  0001 C CNN
F 2 "" H 3950 3550 60  0000 C CNN
F 3 "" H 3950 3550 60  0000 C CNN
	1    3950 3550
	1    0    0    -1  
$EndComp
Text Label 12150 1400 2    60   ~ 0
D0
Text Label 12150 1500 2    60   ~ 0
D1
Text Label 12150 1600 2    60   ~ 0
D2
Text Label 12150 1700 2    60   ~ 0
D3
Text Label 12150 1800 2    60   ~ 0
D4
Text Label 12150 1900 2    60   ~ 0
D5
Text Label 12150 2000 2    60   ~ 0
D6
Text Label 12150 2100 2    60   ~ 0
D7
Text Label 12150 2300 2    60   ~ 0
ALE
$Comp
L RubeCron-cache:VCC #PWR06
U 1 1 588264E0
P 12200 2500
F 0 "#PWR06" H 12200 2600 30  0001 C CNN
F 1 "VCC" H 12200 2600 30  0000 C CNN
F 2 "" H 12200 2500 60  0000 C CNN
F 3 "" H 12200 2500 60  0000 C CNN
	1    12200 2500
	1    0    0    -1  
$EndComp
Text Label 12150 2600 2    60   ~ 0
~8155_CS
Text Label 12150 2700 2    60   ~ 0
~BRD
Text Label 12150 2800 2    60   ~ 0
~BWR
Text Label 12150 3300 2    60   ~ 0
RESET
Text Label 6450 1350 2    60   ~ 0
A0
Text Label 6450 1450 2    60   ~ 0
A1
Text Label 6450 1550 2    60   ~ 0
A2
Text Label 6450 1650 2    60   ~ 0
A3
Text Label 6450 1750 2    60   ~ 0
A4
Text Label 6450 1850 2    60   ~ 0
A5
Text Label 6450 1950 2    60   ~ 0
A6
Text Label 6450 2050 2    60   ~ 0
A7
Text Label 6450 2150 2    60   ~ 0
A8
Text Label 6450 2250 2    60   ~ 0
A9
Text Label 6450 2350 2    60   ~ 0
A10
Text Label 6450 2450 2    60   ~ 0
A11
Text Label 6450 2550 2    60   ~ 0
A12
Text Label 6450 2650 2    60   ~ 0
A13
Text Label 6450 2750 2    60   ~ 0
MEM_A14
Text Label 6450 2850 2    60   ~ 0
MEM_A15
Text Label 6450 2950 2    60   ~ 0
MEM_A16
Text Label 6450 3350 2    60   ~ 0
~BRD
Text Label 6450 3450 2    60   ~ 0
~BWR
Text Label 6450 3150 2    60   ~ 0
~RAM_CS
Text Label 8850 1300 2    60   ~ 0
A0
Text Label 8850 1400 2    60   ~ 0
A1
Text Label 8850 1500 2    60   ~ 0
A2
Text Label 8850 1600 2    60   ~ 0
A3
Text Label 8850 1700 2    60   ~ 0
A4
Text Label 8850 1800 2    60   ~ 0
A5
Text Label 8850 1900 2    60   ~ 0
A6
Text Label 8850 2000 2    60   ~ 0
A7
Text Label 8850 2100 2    60   ~ 0
A8
Text Label 8850 2200 2    60   ~ 0
A9
Text Label 8850 2300 2    60   ~ 0
A10
Text Label 8850 2400 2    60   ~ 0
A11
Text Label 8850 2500 2    60   ~ 0
A12
Text Label 8850 2600 2    60   ~ 0
A13
Text Label 8850 2700 2    60   ~ 0
MEM_A14
Text Label 8850 2800 2    60   ~ 0
MEM_A15
Text Label 8850 2900 2    60   ~ 0
MEM_A16
Text Label 6450 3250 2    60   ~ 0
VCC
NoConn ~ 8950 3200
Text Label 8850 3400 2    60   ~ 0
~ROM_CS
Text Label 8850 3500 2    60   ~ 0
~BRD
$Comp
L RubeCron-cache:CONN_3 K1
U 1 1 58828FCE
P 8000 3200
F 0 "K1" V 7950 3200 50  0000 C CNN
F 1 "FLASH WP" V 8050 3200 40  0000 C CNN
F 2 "" H 8000 3200 60  0000 C CNN
F 3 "" H 8000 3200 60  0000 C CNN
	1    8000 3200
	-1   0    0    1   
$EndComp
$Comp
L RubeCron-cache:VCC #PWR07
U 1 1 588290D3
P 8350 3050
F 0 "#PWR07" H 8350 3150 30  0001 C CNN
F 1 "VCC" H 8350 3150 30  0000 C CNN
F 2 "" H 8350 3050 60  0000 C CNN
F 3 "" H 8350 3050 60  0000 C CNN
	1    8350 3050
	1    0    0    -1  
$EndComp
Text Label 8450 3300 0    60   ~ 0
~BWR
Text Label 5100 6700 0    60   ~ 0
CD0
Text Label 5100 6800 0    60   ~ 0
CD1
Text Label 5100 6900 0    60   ~ 0
CD2
Text Label 5100 7000 0    60   ~ 0
CD3
Text Label 5100 7100 0    60   ~ 0
CD4
Text Label 5100 7200 0    60   ~ 0
CD5
Text Label 5100 7300 0    60   ~ 0
CD6
Text Label 5100 7400 0    60   ~ 0
CD7
Text Label 5100 8800 0    60   ~ 0
CD0
Text Label 5100 8900 0    60   ~ 0
CD1
Text Label 5100 9000 0    60   ~ 0
CD2
Text Label 5100 9100 0    60   ~ 0
CD3
Text Label 5100 9200 0    60   ~ 0
CD4
Text Label 5100 9300 0    60   ~ 0
CD5
Text Label 5100 9400 0    60   ~ 0
CD6
Text Label 5100 9500 0    60   ~ 0
CD7
Text Label 13300 4600 2    60   ~ 0
GND
Text Label 13300 4700 2    60   ~ 0
D2
Text Label 13300 4800 2    60   ~ 0
D7
Text Label 13300 4900 2    60   ~ 0
D5
Text Label 13300 5000 2    60   ~ 0
D3
Text Label 14200 4600 0    60   ~ 0
D0
Text Label 14200 4700 0    60   ~ 0
D4
Text Label 14200 4800 0    60   ~ 0
D1
Text Label 14200 5100 0    60   ~ 0
D6
Text Label 14200 5200 0    60   ~ 0
~USB_WR
Text Label 14200 5300 0    60   ~ 0
~USB_RD
Text Label 14200 5400 0    60   ~ 0
GND
NoConn ~ 13400 5400
$Comp
L RubeCron-cache:R-RESCUE-RubeCron R2
U 1 1 5882C8C2
P 12900 5100
F 0 "R2" V 12980 5100 40  0000 C CNN
F 1 "10k" V 12907 5101 40  0000 C CNN
F 2 "~" V 12830 5100 30  0000 C CNN
F 3 "~" H 12900 5100 30  0000 C CNN
	1    12900 5100
	0    -1   -1   0   
$EndComp
$Comp
L RubeCron-cache:VCC #PWR08
U 1 1 5882CB24
P 12650 5000
F 0 "#PWR08" H 12650 5100 30  0001 C CNN
F 1 "VCC" H 12650 5100 30  0000 C CNN
F 2 "" H 12650 5000 60  0000 C CNN
F 3 "" H 12650 5000 60  0000 C CNN
	1    12650 5000
	1    0    0    -1  
$EndComp
Text Label 7050 4100 2    60   ~ 0
D0
Text Label 7050 4200 2    60   ~ 0
D1
Text Label 7050 4300 2    60   ~ 0
D2
Text Label 7050 4400 2    60   ~ 0
D3
Text Label 7050 4500 2    60   ~ 0
D4
Text Label 7050 4600 2    60   ~ 0
D5
Text Label 7050 4700 2    60   ~ 0
D6
Text Label 7050 4800 2    60   ~ 0
D7
Text Label 7050 5000 2    60   ~ 0
A0
Text Label 7050 5300 2    60   ~ 0
~BRD
Text Label 7050 5400 2    60   ~ 0
~BWR
Text Label 7050 5200 2    60   ~ 0
~UART_CS
Text Label 7050 5600 2    60   ~ 0
RESET
Text Label 7050 5800 2    60   ~ 0
CLK
NoConn ~ 8550 4200
NoConn ~ 8550 4400
$Comp
L RubeCron-cache:GND-RESCUE-RubeCron #PWR09
U 1 1 5882E211
P 8650 4350
F 0 "#PWR09" H 8650 4350 30  0001 C CNN
F 1 "GND" H 8650 4280 30  0001 C CNN
F 2 "" H 8650 4350 60  0000 C CNN
F 3 "" H 8650 4350 60  0000 C CNN
	1    8650 4350
	1    0    0    -1  
$EndComp
NoConn ~ 8550 4700
NoConn ~ 8550 4800
NoConn ~ 8550 5200
NoConn ~ 8550 5500
Text Label 8650 4900 0    60   ~ 0
BAUD
Text Label 8650 5300 0    60   ~ 0
BAUD
$Comp
L RubeCron-cache:CONN_5 P2
U 1 1 5882E71D
P 9800 5350
F 0 "P2" V 9750 5350 50  0000 C CNN
F 1 "GPS" V 9850 5350 50  0000 C CNN
F 2 "" H 9800 5350 60  0000 C CNN
F 3 "" H 9800 5350 60  0000 C CNN
	1    9800 5350
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:VCC #PWR010
U 1 1 5882E738
P 9400 5100
F 0 "#PWR010" H 9400 5200 30  0001 C CNN
F 1 "VCC" H 9400 5200 30  0000 C CNN
F 2 "" H 9400 5100 60  0000 C CNN
F 3 "" H 9400 5100 60  0000 C CNN
	1    9400 5100
	1    0    0    -1  
$EndComp
Text Label 9300 5550 2    60   ~ 0
INT0
Text Label 3850 3800 2    60   ~ 0
AD0
Text Label 3850 3900 2    60   ~ 0
AD1
Text Label 3850 4000 2    60   ~ 0
AD2
Text Label 3850 4100 2    60   ~ 0
AD3
Text Label 3850 4200 2    60   ~ 0
AD4
Text Label 3850 4300 2    60   ~ 0
AD5
Text Label 3850 4400 2    60   ~ 0
AD6
Text Label 3850 4500 2    60   ~ 0
AD7
$Comp
L RubeCron-cache:74LS244 U6
U 1 1 5882FC63
P 4650 4300
F 0 "U6" H 4700 4100 60  0000 C CNN
F 1 "74LS244" H 4750 3900 60  0000 C CNN
F 2 "~" H 4650 4300 60  0000 C CNN
F 3 "~" H 4650 4300 60  0000 C CNN
	1    4650 4300
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:GND-RESCUE-RubeCron #PWR011
U 1 1 5882FEE1
P 3950 4850
F 0 "#PWR011" H 3950 4850 30  0001 C CNN
F 1 "GND-RESCUE-RubeCron" H 3950 4780 30  0001 C CNN
F 2 "" H 3950 4850 60  0000 C CNN
F 3 "" H 3950 4850 60  0000 C CNN
	1    3950 4850
	1    0    0    -1  
$EndComp
Text Label 5450 3800 0    60   ~ 0
CD0
Text Label 5450 3900 0    60   ~ 0
CD1
Text Label 5450 4000 0    60   ~ 0
CD2
Text Label 5450 4100 0    60   ~ 0
CD3
Text Label 5450 4200 0    60   ~ 0
CD4
Text Label 5450 4300 0    60   ~ 0
CD5
Text Label 5450 4400 0    60   ~ 0
CD6
Text Label 5450 4500 0    60   ~ 0
CD7
Text Label 3100 3000 0    60   ~ 0
ALE
Text Label 3100 2100 0    60   ~ 0
A8
Text Label 3100 2200 0    60   ~ 0
A9
Text Label 3100 2300 0    60   ~ 0
A10
Text Label 3100 2400 0    60   ~ 0
A11
Text Label 3100 2500 0    60   ~ 0
A12
Text Label 3100 2600 0    60   ~ 0
A13
Text Label 3100 2700 0    60   ~ 0
A14
Text Label 3100 2800 0    60   ~ 0
A15
$Comp
L RubeCron-cache:LED D14
U 1 1 58831A84
P 3550 6500
F 0 "D14" H 3550 6600 50  0000 C CNN
F 1 "LED" H 3550 6400 50  0000 C CNN
F 2 "~" H 3550 6500 60  0000 C CNN
F 3 "~" H 3550 6500 60  0000 C CNN
	1    3550 6500
	0    1    1    0   
$EndComp
$Comp
L RubeCron-cache:LED D10
U 1 1 58831A97
P 3250 6500
F 0 "D10" H 3250 6600 50  0000 C CNN
F 1 "LED" H 3250 6400 50  0000 C CNN
F 2 "~" H 3250 6500 60  0000 C CNN
F 3 "~" H 3250 6500 60  0000 C CNN
	1    3250 6500
	0    1    1    0   
$EndComp
$Comp
L RubeCron-cache:LED D6
U 1 1 58831A9D
P 2950 6500
F 0 "D6" H 2950 6600 50  0000 C CNN
F 1 "LED" H 2950 6400 50  0000 C CNN
F 2 "~" H 2950 6500 60  0000 C CNN
F 3 "~" H 2950 6500 60  0000 C CNN
	1    2950 6500
	0    1    1    0   
$EndComp
$Comp
L RubeCron-cache:LED D2
U 1 1 58831AA3
P 2650 6500
F 0 "D2" H 2650 6600 50  0000 C CNN
F 1 "LED" H 2650 6400 50  0000 C CNN
F 2 "~" H 2650 6500 60  0000 C CNN
F 3 "~" H 2650 6500 60  0000 C CNN
	1    2650 6500
	0    1    1    0   
$EndComp
$Comp
L RubeCron-cache:LED D15
U 1 1 58831AA9
P 3550 7600
F 0 "D15" H 3550 7700 50  0000 C CNN
F 1 "LED" H 3550 7500 50  0000 C CNN
F 2 "~" H 3550 7600 60  0000 C CNN
F 3 "~" H 3550 7600 60  0000 C CNN
	1    3550 7600
	0    -1   -1   0   
$EndComp
$Comp
L RubeCron-cache:LED D11
U 1 1 58831AEB
P 3250 7600
F 0 "D11" H 3250 7700 50  0000 C CNN
F 1 "LED" H 3250 7500 50  0000 C CNN
F 2 "~" H 3250 7600 60  0000 C CNN
F 3 "~" H 3250 7600 60  0000 C CNN
	1    3250 7600
	0    -1   -1   0   
$EndComp
$Comp
L RubeCron-cache:LED D7
U 1 1 58831AF1
P 2950 7600
F 0 "D7" H 2950 7700 50  0000 C CNN
F 1 "LED" H 2950 7500 50  0000 C CNN
F 2 "~" H 2950 7600 60  0000 C CNN
F 3 "~" H 2950 7600 60  0000 C CNN
	1    2950 7600
	0    -1   -1   0   
$EndComp
$Comp
L RubeCron-cache:LED D3
U 1 1 58831AF7
P 2650 7600
F 0 "D3" H 2650 7700 50  0000 C CNN
F 1 "LED" H 2650 7500 50  0000 C CNN
F 2 "~" H 2650 7600 60  0000 C CNN
F 3 "~" H 2650 7600 60  0000 C CNN
	1    2650 7600
	0    -1   -1   0   
$EndComp
$Comp
L RubeCron-cache:74HC563 U2
U 1 1 588324D6
P 4350 7200
F 0 "U2" H 4350 7850 60  0000 C CNN
F 1 "74HCT563" H 4420 6550 60  0000 C CNN
F 2 "" H 4350 7200 60  0000 C CNN
F 3 "" H 4350 7200 60  0000 C CNN
	1    4350 7200
	-1   0    0    -1  
$EndComp
$Comp
L RubeCron-cache:74HC563 U7
U 1 1 588324E5
P 6350 7200
F 0 "U7" H 6350 7850 60  0000 C CNN
F 1 "74HCT563" H 6420 6550 60  0000 C CNN
F 2 "" H 6350 7200 60  0000 C CNN
F 3 "" H 6350 7200 60  0000 C CNN
	1    6350 7200
	1    0    0    -1  
$EndComp
Text Label 5100 7600 0    60   ~ 0
GND
$Comp
L RubeCron-cache:74HC563 U3
U 1 1 5883263A
P 4350 9300
F 0 "U3" H 4350 9950 60  0000 C CNN
F 1 "74HCT563" H 4420 8650 60  0000 C CNN
F 2 "" H 4350 9300 60  0000 C CNN
F 3 "" H 4350 9300 60  0000 C CNN
	1    4350 9300
	-1   0    0    -1  
$EndComp
$Comp
L RubeCron-cache:74HC563 U8
U 1 1 58832640
P 6350 9300
F 0 "U8" H 6350 9950 60  0000 C CNN
F 1 "74HCT563" H 6420 8650 60  0000 C CNN
F 2 "" H 6350 9300 60  0000 C CNN
F 3 "" H 6350 9300 60  0000 C CNN
	1    6350 9300
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:LED D16
U 1 1 58832646
P 3550 8600
F 0 "D16" H 3550 8700 50  0000 C CNN
F 1 "LED" H 3550 8500 50  0000 C CNN
F 2 "~" H 3550 8600 60  0000 C CNN
F 3 "~" H 3550 8600 60  0000 C CNN
	1    3550 8600
	0    1    1    0   
$EndComp
$Comp
L RubeCron-cache:LED D12
U 1 1 5883264C
P 3250 8600
F 0 "D12" H 3250 8700 50  0000 C CNN
F 1 "LED" H 3250 8500 50  0000 C CNN
F 2 "~" H 3250 8600 60  0000 C CNN
F 3 "~" H 3250 8600 60  0000 C CNN
	1    3250 8600
	0    1    1    0   
$EndComp
$Comp
L RubeCron-cache:LED D8
U 1 1 58832652
P 2950 8600
F 0 "D8" H 2950 8700 50  0000 C CNN
F 1 "LED" H 2950 8500 50  0000 C CNN
F 2 "~" H 2950 8600 60  0000 C CNN
F 3 "~" H 2950 8600 60  0000 C CNN
	1    2950 8600
	0    1    1    0   
$EndComp
$Comp
L RubeCron-cache:LED D4
U 1 1 58832658
P 2650 8600
F 0 "D4" H 2650 8700 50  0000 C CNN
F 1 "LED" H 2650 8500 50  0000 C CNN
F 2 "~" H 2650 8600 60  0000 C CNN
F 3 "~" H 2650 8600 60  0000 C CNN
	1    2650 8600
	0    1    1    0   
$EndComp
$Comp
L RubeCron-cache:LED D17
U 1 1 5883265E
P 3550 9700
F 0 "D17" H 3550 9800 50  0000 C CNN
F 1 "LED" H 3550 9600 50  0000 C CNN
F 2 "~" H 3550 9700 60  0000 C CNN
F 3 "~" H 3550 9700 60  0000 C CNN
	1    3550 9700
	0    -1   -1   0   
$EndComp
$Comp
L RubeCron-cache:LED D13
U 1 1 58832664
P 3250 9700
F 0 "D13" H 3250 9800 50  0000 C CNN
F 1 "LED" H 3250 9600 50  0000 C CNN
F 2 "~" H 3250 9700 60  0000 C CNN
F 3 "~" H 3250 9700 60  0000 C CNN
	1    3250 9700
	0    -1   -1   0   
$EndComp
$Comp
L RubeCron-cache:LED D9
U 1 1 5883266A
P 2950 9700
F 0 "D9" H 2950 9800 50  0000 C CNN
F 1 "LED" H 2950 9600 50  0000 C CNN
F 2 "~" H 2950 9700 60  0000 C CNN
F 3 "~" H 2950 9700 60  0000 C CNN
	1    2950 9700
	0    -1   -1   0   
$EndComp
$Comp
L RubeCron-cache:LED D5
U 1 1 58832670
P 2650 9700
F 0 "D5" H 2650 9800 50  0000 C CNN
F 1 "LED" H 2650 9600 50  0000 C CNN
F 2 "~" H 2650 9700 60  0000 C CNN
F 3 "~" H 2650 9700 60  0000 C CNN
	1    2650 9700
	0    -1   -1   0   
$EndComp
$Comp
L RubeCron-cache:LED D18
U 1 1 5883269C
P 7150 6500
F 0 "D18" H 7150 6600 50  0000 C CNN
F 1 "LED" H 7150 6400 50  0000 C CNN
F 2 "~" H 7150 6500 60  0000 C CNN
F 3 "~" H 7150 6500 60  0000 C CNN
	1    7150 6500
	0    -1   1    0   
$EndComp
$Comp
L RubeCron-cache:LED D22
U 1 1 588326A2
P 7450 6500
F 0 "D22" H 7450 6600 50  0000 C CNN
F 1 "LED" H 7450 6400 50  0000 C CNN
F 2 "~" H 7450 6500 60  0000 C CNN
F 3 "~" H 7450 6500 60  0000 C CNN
	1    7450 6500
	0    -1   1    0   
$EndComp
$Comp
L RubeCron-cache:LED D26
U 1 1 588326A8
P 7750 6500
F 0 "D26" H 7750 6600 50  0000 C CNN
F 1 "LED" H 7750 6400 50  0000 C CNN
F 2 "~" H 7750 6500 60  0000 C CNN
F 3 "~" H 7750 6500 60  0000 C CNN
	1    7750 6500
	0    -1   1    0   
$EndComp
$Comp
L RubeCron-cache:LED D30
U 1 1 588326AE
P 8050 6500
F 0 "D30" H 8050 6600 50  0000 C CNN
F 1 "LED" H 8050 6400 50  0000 C CNN
F 2 "~" H 8050 6500 60  0000 C CNN
F 3 "~" H 8050 6500 60  0000 C CNN
	1    8050 6500
	0    -1   1    0   
$EndComp
$Comp
L RubeCron-cache:LED D19
U 1 1 588326B4
P 7150 7600
F 0 "D19" H 7150 7700 50  0000 C CNN
F 1 "LED" H 7150 7500 50  0000 C CNN
F 2 "~" H 7150 7600 60  0000 C CNN
F 3 "~" H 7150 7600 60  0000 C CNN
	1    7150 7600
	0    1    -1   0   
$EndComp
$Comp
L RubeCron-cache:LED D23
U 1 1 588326BA
P 7450 7600
F 0 "D23" H 7450 7700 50  0000 C CNN
F 1 "LED" H 7450 7500 50  0000 C CNN
F 2 "~" H 7450 7600 60  0000 C CNN
F 3 "~" H 7450 7600 60  0000 C CNN
	1    7450 7600
	0    1    -1   0   
$EndComp
$Comp
L RubeCron-cache:LED D27
U 1 1 588326C0
P 7750 7600
F 0 "D27" H 7750 7700 50  0000 C CNN
F 1 "LED" H 7750 7500 50  0000 C CNN
F 2 "~" H 7750 7600 60  0000 C CNN
F 3 "~" H 7750 7600 60  0000 C CNN
	1    7750 7600
	0    1    -1   0   
$EndComp
$Comp
L RubeCron-cache:LED D31
U 1 1 588326C6
P 8050 7600
F 0 "D31" H 8050 7700 50  0000 C CNN
F 1 "LED" H 8050 7500 50  0000 C CNN
F 2 "~" H 8050 7600 60  0000 C CNN
F 3 "~" H 8050 7600 60  0000 C CNN
	1    8050 7600
	0    1    -1   0   
$EndComp
$Comp
L RubeCron-cache:LED D20
U 1 1 588326DA
P 7150 8600
F 0 "D20" H 7150 8700 50  0000 C CNN
F 1 "LED" H 7150 8500 50  0000 C CNN
F 2 "~" H 7150 8600 60  0000 C CNN
F 3 "~" H 7150 8600 60  0000 C CNN
	1    7150 8600
	0    -1   1    0   
$EndComp
$Comp
L RubeCron-cache:LED D24
U 1 1 588326E0
P 7450 8600
F 0 "D24" H 7450 8700 50  0000 C CNN
F 1 "LED" H 7450 8500 50  0000 C CNN
F 2 "~" H 7450 8600 60  0000 C CNN
F 3 "~" H 7450 8600 60  0000 C CNN
	1    7450 8600
	0    -1   1    0   
$EndComp
$Comp
L RubeCron-cache:LED D28
U 1 1 588326E6
P 7750 8600
F 0 "D28" H 7750 8700 50  0000 C CNN
F 1 "LED" H 7750 8500 50  0000 C CNN
F 2 "~" H 7750 8600 60  0000 C CNN
F 3 "~" H 7750 8600 60  0000 C CNN
	1    7750 8600
	0    -1   1    0   
$EndComp
$Comp
L RubeCron-cache:LED D32
U 1 1 588326EC
P 8050 8600
F 0 "D32" H 8050 8700 50  0000 C CNN
F 1 "LED" H 8050 8500 50  0000 C CNN
F 2 "~" H 8050 8600 60  0000 C CNN
F 3 "~" H 8050 8600 60  0000 C CNN
	1    8050 8600
	0    -1   1    0   
$EndComp
$Comp
L RubeCron-cache:LED D21
U 1 1 588326F2
P 7150 9700
F 0 "D21" H 7150 9800 50  0000 C CNN
F 1 "LED" H 7150 9600 50  0000 C CNN
F 2 "~" H 7150 9700 60  0000 C CNN
F 3 "~" H 7150 9700 60  0000 C CNN
	1    7150 9700
	0    1    -1   0   
$EndComp
$Comp
L RubeCron-cache:LED D25
U 1 1 588326F8
P 7450 9700
F 0 "D25" H 7450 9800 50  0000 C CNN
F 1 "LED" H 7450 9600 50  0000 C CNN
F 2 "~" H 7450 9700 60  0000 C CNN
F 3 "~" H 7450 9700 60  0000 C CNN
	1    7450 9700
	0    1    -1   0   
$EndComp
$Comp
L RubeCron-cache:LED D29
U 1 1 588326FE
P 7750 9700
F 0 "D29" H 7750 9800 50  0000 C CNN
F 1 "LED" H 7750 9600 50  0000 C CNN
F 2 "~" H 7750 9700 60  0000 C CNN
F 3 "~" H 7750 9700 60  0000 C CNN
	1    7750 9700
	0    1    -1   0   
$EndComp
$Comp
L RubeCron-cache:LED D33
U 1 1 58832704
P 8050 9700
F 0 "D33" H 8050 9800 50  0000 C CNN
F 1 "LED" H 8050 9600 50  0000 C CNN
F 2 "~" H 8050 9700 60  0000 C CNN
F 3 "~" H 8050 9700 60  0000 C CNN
	1    8050 9700
	0    1    -1   0   
$EndComp
Text Label 5000 7700 0    60   ~ 0
DAY_LE
Text Label 5700 7700 2    60   ~ 0
HRS_LE
Text Label 5100 9700 0    60   ~ 0
GND
Text Label 5000 9800 0    60   ~ 0
MIN_LE
Text Label 5700 9800 2    60   ~ 0
SEC_LE
$Comp
L RubeCron-cache:RR9 RR1
U 1 1 58833209
P 1700 7000
F 0 "RR1" H 1750 7600 70  0000 C CNN
F 1 "470" V 1730 7000 70  0000 C CNN
F 2 "~" H 1700 7000 60  0000 C CNN
F 3 "~" H 1700 7000 60  0000 C CNN
	1    1700 7000
	-1   0    0    -1  
$EndComp
$Comp
L RubeCron-cache:VCC #PWR012
U 1 1 58833D77
P 2050 6400
F 0 "#PWR012" H 2050 6500 30  0001 C CNN
F 1 "VCC" H 2050 6500 30  0000 C CNN
F 2 "" H 2050 6400 60  0000 C CNN
F 3 "" H 2050 6400 60  0000 C CNN
	1    2050 6400
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:RR9 RR2
U 1 1 58833EFE
P 1700 9100
F 0 "RR2" H 1750 9700 70  0000 C CNN
F 1 "470" V 1730 9100 70  0000 C CNN
F 2 "~" H 1700 9100 60  0000 C CNN
F 3 "~" H 1700 9100 60  0000 C CNN
	1    1700 9100
	-1   0    0    -1  
$EndComp
$Comp
L RubeCron-cache:VCC #PWR013
U 1 1 58834B63
P 2050 8500
F 0 "#PWR013" H 2050 8600 30  0001 C CNN
F 1 "VCC" H 2050 8600 30  0000 C CNN
F 2 "" H 2050 8500 60  0000 C CNN
F 3 "" H 2050 8500 60  0000 C CNN
	1    2050 8500
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:RR9 RR3
U 1 1 58834D0E
P 8950 7000
F 0 "RR3" H 9000 7600 70  0000 C CNN
F 1 "470" V 8980 7000 70  0000 C CNN
F 2 "~" H 8950 7000 60  0000 C CNN
F 3 "~" H 8950 7000 60  0000 C CNN
	1    8950 7000
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:VCC #PWR014
U 1 1 58835A52
P 8600 6400
F 0 "#PWR014" H 8600 6500 30  0001 C CNN
F 1 "VCC" H 8600 6500 30  0000 C CNN
F 2 "" H 8600 6400 60  0000 C CNN
F 3 "" H 8600 6400 60  0000 C CNN
	1    8600 6400
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:RR9 RR4
U 1 1 58835C17
P 9000 9100
F 0 "RR4" H 9050 9700 70  0000 C CNN
F 1 "470" V 9030 9100 70  0000 C CNN
F 2 "~" H 9000 9100 60  0000 C CNN
F 3 "~" H 9000 9100 60  0000 C CNN
	1    9000 9100
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:VCC #PWR015
U 1 1 58836A52
P 8650 8500
F 0 "#PWR015" H 8650 8600 30  0001 C CNN
F 1 "VCC" H 8650 8600 30  0000 C CNN
F 2 "" H 8650 8500 60  0000 C CNN
F 3 "" H 8650 8500 60  0000 C CNN
	1    8650 8500
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:22V10 U12
U 1 1 58836C83
P 11100 7250
F 0 "U12" H 11100 7950 60  0000 C CNN
F 1 "22V10" H 11100 6550 60  0000 C CNN
F 2 "~" H 11100 7250 60  0000 C CNN
F 3 "~" H 11100 7250 60  0000 C CNN
	1    11100 7250
	1    0    0    -1  
$EndComp
Text Label 10200 6700 2    60   ~ 0
A0
Text Label 10200 6800 2    60   ~ 0
A1
Text Label 10200 6900 2    60   ~ 0
A2
Text Label 10200 7000 2    60   ~ 0
A3
Text Label 10200 7100 2    60   ~ 0
A4
Text Label 10200 7200 2    60   ~ 0
A5
Text Label 10200 7300 2    60   ~ 0
A6
Text Label 10200 7400 2    60   ~ 0
A7
Text Label 10200 7500 2    60   ~ 0
IO/~M
Text Label 10200 7600 2    60   ~ 0
~BRD
Text Label 10200 7700 2    60   ~ 0
~BWR
Text Label 12000 6800 0    60   ~ 0
~UART_CS
Text Label 12000 6900 0    60   ~ 0
~USB_WR
Text Label 12000 7000 0    60   ~ 0
~USB_RD
Text Label 12000 7100 0    60   ~ 0
DAY_LE
Text Label 12000 7200 0    60   ~ 0
HRS_LE
Text Label 12000 7300 0    60   ~ 0
MIN_LE
Text Label 12000 7400 0    60   ~ 0
SEC_LE
Text Label 12000 7500 0    60   ~ 0
~BNKSEL
Text Label 12000 6700 0    60   ~ 0
~8155_CS
$Comp
L RubeCron-cache:CP1-RESCUE-RubeCron C5
U 1 1 58850891
P 3050 11250
F 0 "C5" H 3100 11350 50  0000 L CNN
F 1 "470uF" H 3100 11150 50  0000 L CNN
F 2 "~" H 3050 11250 60  0000 C CNN
F 3 "~" H 3050 11250 60  0000 C CNN
	1    3050 11250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C6
U 1 1 588508B3
P 3350 11250
F 0 "C6" H 3350 11350 40  0000 L CNN
F 1 ".1uF" H 3356 11165 40  0000 L CNN
F 2 "~" H 3388 11100 30  0000 C CNN
F 3 "~" H 3350 11250 60  0000 C CNN
	1    3350 11250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C7
U 1 1 588508C0
P 3600 11250
F 0 "C7" H 3600 11350 40  0000 L CNN
F 1 ".1uF" H 3606 11165 40  0000 L CNN
F 2 "~" H 3638 11100 30  0000 C CNN
F 3 "~" H 3600 11250 60  0000 C CNN
	1    3600 11250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C8
U 1 1 588508C6
P 3850 11250
F 0 "C8" H 3850 11350 40  0000 L CNN
F 1 ".1uF" H 3856 11165 40  0000 L CNN
F 2 "~" H 3888 11100 30  0000 C CNN
F 3 "~" H 3850 11250 60  0000 C CNN
	1    3850 11250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C9
U 1 1 588508CC
P 4100 11250
F 0 "C9" H 4100 11350 40  0000 L CNN
F 1 ".1uF" H 4106 11165 40  0000 L CNN
F 2 "~" H 4138 11100 30  0000 C CNN
F 3 "~" H 4100 11250 60  0000 C CNN
	1    4100 11250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C10
U 1 1 588508D2
P 4350 11250
F 0 "C10" H 4350 11350 40  0000 L CNN
F 1 ".1uF" H 4356 11165 40  0000 L CNN
F 2 "~" H 4388 11100 30  0000 C CNN
F 3 "~" H 4350 11250 60  0000 C CNN
	1    4350 11250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C11
U 1 1 588508D8
P 4600 11250
F 0 "C11" H 4600 11350 40  0000 L CNN
F 1 ".1uF" H 4606 11165 40  0000 L CNN
F 2 "~" H 4638 11100 30  0000 C CNN
F 3 "~" H 4600 11250 60  0000 C CNN
	1    4600 11250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C12
U 1 1 588508DE
P 4850 11250
F 0 "C12" H 4850 11350 40  0000 L CNN
F 1 ".1uF" H 4856 11165 40  0000 L CNN
F 2 "~" H 4888 11100 30  0000 C CNN
F 3 "~" H 4850 11250 60  0000 C CNN
	1    4850 11250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C13
U 1 1 588508E4
P 5100 11250
F 0 "C13" H 5100 11350 40  0000 L CNN
F 1 ".1uF" H 5106 11165 40  0000 L CNN
F 2 "~" H 5138 11100 30  0000 C CNN
F 3 "~" H 5100 11250 60  0000 C CNN
	1    5100 11250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C14
U 1 1 588508EA
P 5350 11250
F 0 "C14" H 5350 11350 40  0000 L CNN
F 1 ".1uF" H 5356 11165 40  0000 L CNN
F 2 "~" H 5388 11100 30  0000 C CNN
F 3 "~" H 5350 11250 60  0000 C CNN
	1    5350 11250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C15
U 1 1 588508F0
P 5600 11250
F 0 "C15" H 5600 11350 40  0000 L CNN
F 1 ".1uF" H 5606 11165 40  0000 L CNN
F 2 "~" H 5638 11100 30  0000 C CNN
F 3 "~" H 5600 11250 60  0000 C CNN
	1    5600 11250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C16
U 1 1 588508F6
P 5850 11250
F 0 "C16" H 5850 11350 40  0000 L CNN
F 1 ".1uF" H 5856 11165 40  0000 L CNN
F 2 "~" H 5888 11100 30  0000 C CNN
F 3 "~" H 5850 11250 60  0000 C CNN
	1    5850 11250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C17
U 1 1 58850902
P 6100 11250
F 0 "C17" H 6100 11350 40  0000 L CNN
F 1 ".1uF" H 6106 11165 40  0000 L CNN
F 2 "~" H 6138 11100 30  0000 C CNN
F 3 "~" H 6100 11250 60  0000 C CNN
	1    6100 11250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C18
U 1 1 58850908
P 6350 11250
F 0 "C18" H 6350 11350 40  0000 L CNN
F 1 ".1uF" H 6356 11165 40  0000 L CNN
F 2 "~" H 6388 11100 30  0000 C CNN
F 3 "~" H 6350 11250 60  0000 C CNN
	1    6350 11250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C19
U 1 1 5885090E
P 6600 11250
F 0 "C19" H 6600 11350 40  0000 L CNN
F 1 ".1uF" H 6606 11165 40  0000 L CNN
F 2 "~" H 6638 11100 30  0000 C CNN
F 3 "~" H 6600 11250 60  0000 C CNN
	1    6600 11250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C20
U 1 1 58850914
P 6850 11250
F 0 "C20" H 6850 11350 40  0000 L CNN
F 1 ".1uF" H 6856 11165 40  0000 L CNN
F 2 "~" H 6888 11100 30  0000 C CNN
F 3 "~" H 6850 11250 60  0000 C CNN
	1    6850 11250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:VCC #PWR016
U 1 1 588547C5
P 6850 11000
F 0 "#PWR016" H 6850 11100 30  0001 C CNN
F 1 "VCC" H 6850 11100 30  0000 C CNN
F 2 "" H 6850 11000 60  0000 C CNN
F 3 "" H 6850 11000 60  0000 C CNN
	1    6850 11000
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:GND-RESCUE-RubeCron #PWR017
U 1 1 588547D4
P 6850 11500
F 0 "#PWR017" H 6850 11500 30  0001 C CNN
F 1 "GND" H 6850 11430 30  0001 C CNN
F 2 "" H 6850 11500 60  0000 C CNN
F 3 "" H 6850 11500 60  0000 C CNN
	1    6850 11500
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:PWR_FLAG #FLG018
U 1 1 58854C30
P 2800 11050
F 0 "#FLG018" H 2800 11145 30  0001 C CNN
F 1 "PWR_FLAG" H 2800 11230 30  0000 C CNN
F 2 "" H 2800 11050 60  0000 C CNN
F 3 "" H 2800 11050 60  0000 C CNN
	1    2800 11050
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:PWR_FLAG #FLG019
U 1 1 58854C3D
P 2800 11450
F 0 "#FLG019" H 2800 11545 30  0001 C CNN
F 1 "PWR_FLAG" H 2800 11630 30  0000 C CNN
F 2 "" H 2800 11450 60  0000 C CNN
F 3 "" H 2800 11450 60  0000 C CNN
	1    2800 11450
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:CONN_2 P1
U 1 1 58855096
P 2250 11250
F 0 "P1" V 2200 11250 40  0000 C CNN
F 1 "5VDC" V 2300 11250 40  0000 C CNN
F 2 "" H 2250 11250 60  0000 C CNN
F 3 "" H 2250 11250 60  0000 C CNN
	1    2250 11250
	-1   0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C3
U 1 1 5886543E
P 1500 4250
F 0 "C3" H 1500 4350 40  0000 L CNN
F 1 ".1uF" H 1506 4165 40  0000 L CNN
F 2 "~" H 1538 4100 30  0000 C CNN
F 3 "~" H 1500 4250 60  0000 C CNN
	1    1500 4250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C4
U 1 1 58865450
P 1600 4950
F 0 "C4" H 1600 5050 40  0000 L CNN
F 1 ".1uF" H 1606 4865 40  0000 L CNN
F 2 "~" H 1638 4800 30  0000 C CNN
F 3 "~" H 1600 4950 60  0000 C CNN
	1    1600 4950
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:VCC #PWR020
U 1 1 58865D27
P 1250 3950
F 0 "#PWR020" H 1250 4050 30  0001 C CNN
F 1 "VCC" H 1250 4050 30  0000 C CNN
F 2 "" H 1250 3950 60  0000 C CNN
F 3 "" H 1250 3950 60  0000 C CNN
	1    1250 3950
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:GND-RESCUE-RubeCron #PWR021
U 1 1 58865D36
P 1250 5300
F 0 "#PWR021" H 1250 5300 30  0001 C CNN
F 1 "GND-RESCUE-RubeCron" H 1250 5230 30  0001 C CNN
F 2 "" H 1250 5300 60  0000 C CNN
F 3 "" H 1250 5300 60  0000 C CNN
	1    1250 5300
	1    0    0    -1  
$EndComp
Text Label 1400 2000 2    60   ~ 0
INT0
$Comp
L RubeCron-cache:GND-RESCUE-RubeCron #PWR022
U 1 1 58867824
P 1300 1500
F 0 "#PWR022" H 1300 1500 30  0001 C CNN
F 1 "GND-RESCUE-RubeCron" H 1300 1430 30  0001 C CNN
F 2 "" H 1300 1500 60  0000 C CNN
F 3 "" H 1300 1500 60  0000 C CNN
	1    1300 1500
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C1
U 1 1 5880EA4B
P 1050 3400
F 0 "C1" H 1050 3500 40  0000 L CNN
F 1 "20pF" H 1056 3315 40  0000 L CNN
F 2 "~" H 1088 3250 30  0000 C CNN
F 3 "~" H 1050 3400 60  0000 C CNN
	1    1050 3400
	1    0    0    -1  
$EndComp
Text Notes 850  3200 3    60   ~ 0
Cap not req'd\nfor X>4MHz
Text Label 3100 3600 0    60   ~ 0
~RD
NoConn ~ 3050 3300
NoConn ~ 8600 7400
NoConn ~ 8650 9500
NoConn ~ 2050 9500
NoConn ~ 2050 7400
NoConn ~ 3050 4000
$Comp
L RubeCron-cache:8155 U14
U 1 1 588132F7
P 12950 2550
F 0 "U14" H 12950 2600 60  0000 C CNN
F 1 "8155" H 12950 2500 60  0000 C CNN
F 2 "" H 12950 2550 60  0000 C CNN
F 3 "" H 12950 2550 60  0000 C CNN
	1    12950 2550
	1    0    0    -1  
$EndComp
Text Label 12000 8300 0    60   ~ 0
~RAM_CS
Text Label 12000 8400 0    60   ~ 0
~ROM_CS
Text Label 12000 8600 0    60   ~ 0
MEM_A15
Text Label 12000 8700 0    60   ~ 0
MEM_A16
Text Label 12000 8800 0    60   ~ 0
~BRD
Text Label 12000 8900 0    60   ~ 0
~BWR
Text Label 10200 8300 2    60   ~ 0
~BNKSEL
Text Label 10200 8400 2    60   ~ 0
D0
Text Label 10200 8500 2    60   ~ 0
D1
Text Label 10200 8600 2    60   ~ 0
D2
Text Label 10200 8900 2    60   ~ 0
A15
Text Label 10200 9000 2    60   ~ 0
IO/~M
Text Label 10200 9100 2    60   ~ 0
~RD
Text Label 10200 9200 2    60   ~ 0
~WR
Text Label 10200 9300 2    60   ~ 0
RESET
$Comp
L RubeCron-cache:22V10 U13
U 1 1 5886F828
P 11100 8850
F 0 "U13" H 11100 9550 60  0000 C CNN
F 1 "22V10" H 11100 8150 60  0000 C CNN
F 2 "~" H 11100 8850 60  0000 C CNN
F 3 "~" H 11100 8850 60  0000 C CNN
	1    11100 8850
	1    0    0    -1  
$EndComp
NoConn ~ 3050 4300
NoConn ~ 10300 7800
NoConn ~ 10300 9400
NoConn ~ 11900 9200
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C21
U 1 1 5887B0B4
P 7100 11250
F 0 "C21" H 7100 11350 40  0000 L CNN
F 1 ".1uF" H 7106 11165 40  0000 L CNN
F 2 "~" H 7138 11100 30  0000 C CNN
F 3 "~" H 7100 11250 60  0000 C CNN
	1    7100 11250
	1    0    0    -1  
$EndComp
Text Label 14350 6750 0    60   ~ 0
IDE_A0
Text Label 14350 6850 0    60   ~ 0
IDE_A1
Text Label 14350 6950 0    60   ~ 0
IDE_A2
Text Label 13150 6850 2    60   ~ 0
D0
Text Label 13150 6950 2    60   ~ 0
D1
Text Label 13150 7050 2    60   ~ 0
D2
Text Label 13150 7150 2    60   ~ 0
D3
Text Label 13150 7250 2    60   ~ 0
D4
Text Label 13150 7350 2    60   ~ 0
D5
Text Label 13150 7450 2    60   ~ 0
D6
Text Label 13150 7550 2    60   ~ 0
D7
Text Label 13750 1400 0    60   ~ 0
IDE_D0
Text Label 13750 1500 0    60   ~ 0
IDE_D1
Text Label 13750 1600 0    60   ~ 0
IDE_D2
Text Label 13750 1700 0    60   ~ 0
IDE_D3
Text Label 13750 1800 0    60   ~ 0
IDE_D4
Text Label 13750 1900 0    60   ~ 0
IDE_D5
Text Label 13750 2000 0    60   ~ 0
IDE_D6
Text Label 13750 2100 0    60   ~ 0
IDE_D7
Text Label 13750 2300 0    60   ~ 0
IDE_D8
Text Label 13750 2400 0    60   ~ 0
IDE_D9
Text Label 13750 2500 0    60   ~ 0
IDE_D10
Text Label 13750 2600 0    60   ~ 0
IDE_D11
Text Label 13750 2700 0    60   ~ 0
IDE_D12
Text Label 13750 2800 0    60   ~ 0
IDE_D13
Text Label 13750 2900 0    60   ~ 0
IDE_D14
Text Label 13750 3000 0    60   ~ 0
IDE_D15
$Comp
L RubeCron-cache:DIPS_04 SW2
U 1 1 588816E7
P 15250 3350
F 0 "SW2" V 15000 3350 60  0000 C CNN
F 1 "USER" V 15500 3350 60  0000 C CNN
F 2 "" H 15250 3350 60  0000 C CNN
F 3 "" H 15250 3350 60  0000 C CNN
	1    15250 3350
	0    -1   -1   0   
$EndComp
$Comp
L RubeCron-cache:GND-RESCUE-RubeCron #PWR023
U 1 1 58882695
P 15450 3600
F 0 "#PWR023" H 15450 3600 30  0001 C CNN
F 1 "GND" H 15450 3530 30  0001 C CNN
F 2 "" H 15450 3600 60  0000 C CNN
F 3 "" H 15450 3600 60  0000 C CNN
	1    15450 3600
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:R-RESCUE-RubeCron R4
U 1 1 588828E2
P 14550 2850
F 0 "R4" V 14630 2850 40  0000 C CNN
F 1 "4.7k" V 14557 2851 40  0000 C CNN
F 2 "~" V 14480 2850 30  0000 C CNN
F 3 "~" H 14550 2850 30  0000 C CNN
	1    14550 2850
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:R-RESCUE-RubeCron R5
U 1 1 588828F4
P 14700 2850
F 0 "R5" V 14780 2850 40  0000 C CNN
F 1 "4.7k" V 14707 2851 40  0000 C CNN
F 2 "~" V 14630 2850 30  0000 C CNN
F 3 "~" H 14700 2850 30  0000 C CNN
	1    14700 2850
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:R-RESCUE-RubeCron R6
U 1 1 588828FA
P 14850 2850
F 0 "R6" V 14930 2850 40  0000 C CNN
F 1 "4.7k" V 14857 2851 40  0000 C CNN
F 2 "~" V 14780 2850 30  0000 C CNN
F 3 "~" H 14850 2850 30  0000 C CNN
	1    14850 2850
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:R-RESCUE-RubeCron R7
U 1 1 58882900
P 15000 2850
F 0 "R7" V 15080 2850 40  0000 C CNN
F 1 "4.7k" V 15007 2851 40  0000 C CNN
F 2 "~" V 14930 2850 30  0000 C CNN
F 3 "~" H 15000 2850 30  0000 C CNN
	1    15000 2850
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:VCC #PWR024
U 1 1 58883210
P 15000 2500
F 0 "#PWR024" H 15000 2600 30  0001 C CNN
F 1 "VCC" H 15000 2600 30  0000 C CNN
F 2 "" H 15000 2500 60  0000 C CNN
F 3 "" H 15000 2500 60  0000 C CNN
	1    15000 2500
	1    0    0    -1  
$EndComp
Text Label 14350 7050 0    60   ~ 0
~IDE_CS0
Text Label 14350 7150 0    60   ~ 0
~IDE_CS1
Text Label 14350 7250 0    60   ~ 0
~IDE_DIOW
Text Label 14350 7350 0    60   ~ 0
~IDE_DIOR
Text Label 14350 7450 0    60   ~ 0
~IDE_RST
$Comp
L RubeCron-cache:R-RESCUE-RubeCron R3
U 1 1 5888767F
P 19200 5350
F 0 "R3" V 19280 5350 40  0000 C CNN
F 1 "470" V 19207 5351 40  0000 C CNN
F 2 "~" V 19130 5350 30  0000 C CNN
F 3 "~" H 19200 5350 30  0000 C CNN
	1    19200 5350
	0    -1   -1   0   
$EndComp
$Comp
L RubeCron-cache:LED D34
U 1 1 588878F2
P 19750 5350
F 0 "D34" H 19750 5450 50  0000 C CNN
F 1 "CF ACT" H 19750 5250 50  0000 C CNN
F 2 "~" H 19750 5350 60  0000 C CNN
F 3 "~" H 19750 5350 60  0000 C CNN
	1    19750 5350
	-1   0    0    1   
$EndComp
$Comp
L RubeCron-cache:VCC #PWR025
U 1 1 58887B61
P 19950 5200
F 0 "#PWR025" H 19950 5300 30  0001 C CNN
F 1 "VCC" H 19950 5300 30  0000 C CNN
F 2 "" H 19950 5200 60  0000 C CNN
F 3 "" H 19950 5200 60  0000 C CNN
	1    19950 5200
	1    0    0    -1  
$EndComp
Wire Wire Line
	11900 7500 12000 7500
Wire Wire Line
	11900 7400 12000 7400
Wire Wire Line
	11900 7300 12000 7300
Wire Wire Line
	11900 7200 12000 7200
Wire Wire Line
	11900 7100 12000 7100
Wire Wire Line
	11900 7000 12000 7000
Wire Wire Line
	11900 6900 12000 6900
Wire Wire Line
	11900 6800 12000 6800
Wire Wire Line
	11900 6700 12000 6700
Wire Wire Line
	10300 7700 10200 7700
Wire Wire Line
	10300 7600 10200 7600
Wire Wire Line
	10300 7500 10200 7500
Wire Wire Line
	10300 7400 10200 7400
Wire Wire Line
	10300 7300 10200 7300
Wire Wire Line
	10300 7200 10200 7200
Wire Wire Line
	10300 7100 10200 7100
Wire Wire Line
	10300 7000 10200 7000
Wire Wire Line
	10300 6900 10200 6900
Wire Wire Line
	10300 6800 10200 6800
Wire Wire Line
	10300 6700 10200 6700
Wire Wire Line
	8650 8500 8650 8600
Wire Wire Line
	7150 10050 7150 9900
Wire Wire Line
	8400 10050 7150 10050
Wire Wire Line
	8400 9400 8400 10050
Wire Wire Line
	8650 9400 8400 9400
Wire Wire Line
	7450 10000 7450 9900
Wire Wire Line
	8350 10000 7450 10000
Wire Wire Line
	8350 9300 8350 10000
Wire Wire Line
	8650 9300 8350 9300
Wire Wire Line
	7750 9950 7750 9900
Wire Wire Line
	8300 9950 7750 9950
Wire Wire Line
	8300 9200 8300 9950
Wire Wire Line
	8650 9200 8300 9200
Wire Wire Line
	8250 9900 8050 9900
Wire Wire Line
	8250 9100 8250 9900
Wire Wire Line
	8650 9100 8250 9100
Wire Wire Line
	7150 8250 7150 8400
Wire Wire Line
	8400 8250 7150 8250
Wire Wire Line
	8400 8700 8400 8250
Wire Wire Line
	8650 8700 8400 8700
Wire Wire Line
	7450 8300 7450 8400
Wire Wire Line
	8350 8300 7450 8300
Wire Wire Line
	8350 8800 8350 8300
Wire Wire Line
	8650 8800 8350 8800
Wire Wire Line
	7750 8350 7750 8400
Wire Wire Line
	8300 8350 7750 8350
Wire Wire Line
	8300 8900 8300 8350
Wire Wire Line
	8650 8900 8300 8900
Wire Wire Line
	8250 9000 8650 9000
Wire Wire Line
	8250 8400 8250 9000
Wire Wire Line
	8050 8400 8250 8400
Wire Wire Line
	8600 6400 8600 6500
Wire Wire Line
	7150 7950 7150 7800
Wire Wire Line
	8400 7950 7150 7950
Wire Wire Line
	8400 7300 8400 7950
Wire Wire Line
	8600 7300 8400 7300
Wire Wire Line
	7450 7900 7450 7800
Wire Wire Line
	8350 7900 7450 7900
Wire Wire Line
	8350 7200 8350 7900
Wire Wire Line
	8600 7200 8350 7200
Wire Wire Line
	7750 7850 7750 7800
Wire Wire Line
	8300 7850 7750 7850
Wire Wire Line
	8300 7100 8300 7850
Wire Wire Line
	8600 7100 8300 7100
Wire Wire Line
	8250 7800 8050 7800
Wire Wire Line
	8250 7000 8250 7800
Wire Wire Line
	8600 7000 8250 7000
Wire Wire Line
	7150 6150 7150 6300
Wire Wire Line
	8400 6150 7150 6150
Wire Wire Line
	8400 6600 8400 6150
Wire Wire Line
	8600 6600 8400 6600
Wire Wire Line
	7450 6200 7450 6300
Wire Wire Line
	8350 6200 7450 6200
Wire Wire Line
	8350 6700 8350 6200
Wire Wire Line
	8600 6700 8350 6700
Wire Wire Line
	7750 6250 7750 6300
Wire Wire Line
	8300 6250 7750 6250
Wire Wire Line
	8300 6800 8300 6250
Wire Wire Line
	8600 6800 8300 6800
Wire Wire Line
	8250 6900 8600 6900
Wire Wire Line
	8250 6300 8250 6900
Wire Wire Line
	8050 6300 8250 6300
Wire Wire Line
	2050 8500 2050 8600
Wire Wire Line
	3550 10050 3550 9900
Wire Wire Line
	2300 10050 3550 10050
Wire Wire Line
	2300 9400 2300 10050
Wire Wire Line
	2050 9400 2300 9400
Wire Wire Line
	3250 10000 3250 9900
Wire Wire Line
	2350 10000 3250 10000
Wire Wire Line
	2350 9300 2350 10000
Wire Wire Line
	2050 9300 2350 9300
Wire Wire Line
	2950 9950 2950 9900
Wire Wire Line
	2400 9950 2950 9950
Wire Wire Line
	2400 9200 2400 9950
Wire Wire Line
	2050 9200 2400 9200
Wire Wire Line
	2450 9900 2650 9900
Wire Wire Line
	2450 9100 2450 9900
Wire Wire Line
	2050 9100 2450 9100
Wire Wire Line
	3550 8250 3550 8400
Wire Wire Line
	2300 8250 3550 8250
Wire Wire Line
	2300 8700 2300 8250
Wire Wire Line
	2050 8700 2300 8700
Wire Wire Line
	3250 8300 3250 8400
Wire Wire Line
	2350 8300 3250 8300
Wire Wire Line
	2350 8800 2350 8300
Wire Wire Line
	2050 8800 2350 8800
Wire Wire Line
	2950 8350 2950 8400
Wire Wire Line
	2400 8350 2950 8350
Wire Wire Line
	2400 8900 2400 8350
Wire Wire Line
	2050 8900 2400 8900
Wire Wire Line
	2450 8400 2650 8400
Wire Wire Line
	2450 9000 2450 8400
Wire Wire Line
	2050 9000 2450 9000
Wire Wire Line
	2050 6400 2050 6500
Wire Wire Line
	3550 6150 3550 6300
Wire Wire Line
	2300 6150 3550 6150
Wire Wire Line
	2300 6600 2300 6150
Wire Wire Line
	2050 6600 2300 6600
Wire Wire Line
	3250 6200 3250 6300
Wire Wire Line
	2350 6200 3250 6200
Wire Wire Line
	2350 6700 2350 6200
Wire Wire Line
	2050 6700 2350 6700
Wire Wire Line
	2950 6250 2950 6300
Wire Wire Line
	2400 6250 2950 6250
Wire Wire Line
	2400 6800 2400 6250
Wire Wire Line
	2050 6800 2400 6800
Wire Wire Line
	2450 6300 2650 6300
Wire Wire Line
	2450 6900 2450 6300
Wire Wire Line
	2050 6900 2450 6900
Wire Wire Line
	3550 7950 3550 7800
Wire Wire Line
	2300 7950 3550 7950
Wire Wire Line
	2300 7300 2300 7950
Wire Wire Line
	2050 7300 2300 7300
Wire Wire Line
	3250 7900 3250 7800
Wire Wire Line
	2350 7900 3250 7900
Wire Wire Line
	2350 7200 2350 7900
Wire Wire Line
	2050 7200 2350 7200
Wire Wire Line
	2950 7850 2950 7800
Wire Wire Line
	2400 7850 2950 7850
Wire Wire Line
	2400 7100 2400 7850
Wire Wire Line
	2050 7100 2400 7100
Wire Wire Line
	2450 7800 2650 7800
Wire Wire Line
	2450 7000 2450 7800
Wire Wire Line
	2050 7000 2450 7000
Wire Wire Line
	5750 9800 5700 9800
Wire Wire Line
	4950 9800 5000 9800
Wire Wire Line
	4950 9700 5750 9700
Wire Wire Line
	5750 7700 5700 7700
Wire Wire Line
	4950 7700 5000 7700
Wire Wire Line
	8050 9200 8050 9500
Wire Wire Line
	6950 9200 8050 9200
Wire Wire Line
	7750 9300 7750 9500
Wire Wire Line
	6950 9300 7750 9300
Wire Wire Line
	7450 9400 7450 9500
Wire Wire Line
	6950 9400 7450 9400
Wire Wire Line
	6950 9500 7150 9500
Wire Wire Line
	8050 9100 8050 8800
Wire Wire Line
	6950 9100 8050 9100
Wire Wire Line
	7750 9000 7750 8800
Wire Wire Line
	6950 9000 7750 9000
Wire Wire Line
	7450 8900 7450 8800
Wire Wire Line
	6950 8900 7450 8900
Wire Wire Line
	6950 8800 7150 8800
Wire Wire Line
	8050 7100 8050 7400
Wire Wire Line
	6950 7100 8050 7100
Wire Wire Line
	7750 7200 7750 7400
Wire Wire Line
	6950 7200 7750 7200
Wire Wire Line
	7450 7300 7450 7400
Wire Wire Line
	6950 7300 7450 7300
Wire Wire Line
	6950 7400 7150 7400
Wire Wire Line
	8050 7000 8050 6700
Wire Wire Line
	6950 7000 8050 7000
Wire Wire Line
	7750 6900 7750 6700
Wire Wire Line
	6950 6900 7750 6900
Wire Wire Line
	7450 6800 7450 6700
Wire Wire Line
	6950 6800 7450 6800
Wire Wire Line
	6950 6700 7150 6700
Wire Wire Line
	2650 9200 2650 9500
Wire Wire Line
	3750 9200 2650 9200
Wire Wire Line
	2950 9300 2950 9500
Wire Wire Line
	3750 9300 2950 9300
Wire Wire Line
	3250 9400 3250 9500
Wire Wire Line
	3750 9400 3250 9400
Wire Wire Line
	3750 9500 3550 9500
Wire Wire Line
	2650 9100 2650 8800
Wire Wire Line
	3750 9100 2650 9100
Wire Wire Line
	2950 9000 2950 8800
Wire Wire Line
	3750 9000 2950 9000
Wire Wire Line
	3250 8900 3250 8800
Wire Wire Line
	3750 8900 3250 8900
Wire Wire Line
	3750 8800 3550 8800
Wire Wire Line
	4950 7600 5750 7600
Wire Wire Line
	2650 7100 2650 7400
Wire Wire Line
	3750 7100 2650 7100
Wire Wire Line
	2950 7200 2950 7400
Wire Wire Line
	3750 7200 2950 7200
Wire Wire Line
	3250 7300 3250 7400
Wire Wire Line
	3750 7300 3250 7300
Wire Wire Line
	3750 7400 3550 7400
Wire Wire Line
	2650 7000 2650 6700
Wire Wire Line
	3750 7000 2650 7000
Wire Wire Line
	2950 6900 2950 6700
Wire Wire Line
	3750 6900 2950 6900
Wire Wire Line
	3250 6800 3250 6700
Wire Wire Line
	3750 6800 3250 6800
Wire Wire Line
	3750 6700 3550 6700
Wire Wire Line
	3050 2800 3100 2800
Wire Wire Line
	3050 2700 3100 2700
Wire Wire Line
	3050 2600 3100 2600
Wire Wire Line
	3050 2500 3100 2500
Wire Wire Line
	3050 2400 3100 2400
Wire Wire Line
	3050 2300 3100 2300
Wire Wire Line
	3050 2200 3100 2200
Wire Wire Line
	3050 2100 3100 2100
Wire Wire Line
	3600 3000 3600 2100
Wire Wire Line
	3050 3000 3600 3000
Wire Wire Line
	5350 4500 5450 4500
Wire Wire Line
	5350 4400 5450 4400
Wire Wire Line
	5350 4300 5450 4300
Wire Wire Line
	5350 4200 5450 4200
Wire Wire Line
	5350 4100 5450 4100
Wire Wire Line
	5350 4000 5450 4000
Wire Wire Line
	5350 3900 5450 3900
Wire Wire Line
	5350 3800 5450 3800
Wire Wire Line
	3950 4500 3850 4500
Wire Wire Line
	3950 4400 3850 4400
Wire Wire Line
	3950 4300 3850 4300
Wire Wire Line
	3950 4200 3850 4200
Wire Wire Line
	3950 4100 3850 4100
Wire Wire Line
	3950 4000 3850 4000
Wire Wire Line
	3950 3900 3850 3900
Wire Wire Line
	3950 3800 3850 3800
Wire Wire Line
	9050 4300 9050 5250
Wire Wire Line
	9050 5250 9400 5250
Wire Wire Line
	9400 5550 9300 5550
Wire Wire Line
	9000 5450 9400 5450
Wire Wire Line
	9000 4600 9000 5450
Wire Wire Line
	8550 4600 9000 4600
Wire Wire Line
	8950 5100 8550 5100
Wire Wire Line
	8950 5350 8950 5100
Wire Wire Line
	9400 5350 8950 5350
Wire Wire Line
	9400 5100 9400 5150
Wire Wire Line
	8550 5300 8650 5300
Wire Wire Line
	8550 4900 8650 4900
Connection ~ 8650 4300
Wire Wire Line
	8650 4100 8650 4300
Wire Wire Line
	8550 4100 8650 4100
Wire Wire Line
	8550 4300 8650 4300
Wire Wire Line
	7150 5800 7050 5800
Wire Wire Line
	7150 5600 7050 5600
Wire Wire Line
	7150 5200 7050 5200
Wire Wire Line
	7150 5400 7050 5400
Wire Wire Line
	7150 5300 7050 5300
Wire Wire Line
	7150 5000 7050 5000
Wire Wire Line
	7150 4800 7050 4800
Wire Wire Line
	7150 4700 7050 4700
Wire Wire Line
	7150 4600 7050 4600
Wire Wire Line
	7150 4500 7050 4500
Wire Wire Line
	7150 4400 7050 4400
Wire Wire Line
	7150 4300 7050 4300
Wire Wire Line
	7150 4200 7050 4200
Wire Wire Line
	7150 4100 7050 4100
Wire Wire Line
	13150 5100 13400 5100
Wire Wire Line
	12650 5300 13400 5300
Wire Wire Line
	13400 5300 13400 5200
Wire Wire Line
	14100 5400 14200 5400
Wire Wire Line
	14100 5300 14200 5300
Wire Wire Line
	14100 5200 14200 5200
Wire Wire Line
	14100 5100 14200 5100
Wire Wire Line
	14100 4800 14200 4800
Wire Wire Line
	14100 4700 14200 4700
Wire Wire Line
	14100 4600 14200 4600
Wire Wire Line
	13400 5000 13300 5000
Wire Wire Line
	13400 4900 13300 4900
Wire Wire Line
	13400 4800 13300 4800
Wire Wire Line
	13400 4700 13300 4700
Wire Wire Line
	13400 4600 13300 4600
Wire Wire Line
	5750 9500 4950 9500
Wire Wire Line
	4950 9400 5750 9400
Wire Wire Line
	5750 9300 4950 9300
Wire Wire Line
	4950 9200 5750 9200
Wire Wire Line
	5750 9100 4950 9100
Wire Wire Line
	4950 9000 5750 9000
Wire Wire Line
	5750 8900 4950 8900
Wire Wire Line
	4950 8800 5750 8800
Wire Wire Line
	5750 7400 4950 7400
Wire Wire Line
	4950 7300 5750 7300
Wire Wire Line
	5750 7200 4950 7200
Wire Wire Line
	4950 7100 5750 7100
Wire Wire Line
	5750 7000 4950 7000
Wire Wire Line
	4950 6900 5750 6900
Wire Wire Line
	5750 6800 4950 6800
Wire Wire Line
	4950 6700 5750 6700
Wire Wire Line
	8800 3100 8950 3100
Wire Wire Line
	8800 3200 8800 3100
Wire Wire Line
	8350 3200 8800 3200
Wire Wire Line
	8350 3300 8450 3300
Wire Wire Line
	8350 3050 8350 3100
Wire Wire Line
	8950 3500 8850 3500
Wire Wire Line
	8950 3400 8850 3400
Wire Wire Line
	6550 3250 6450 3250
Wire Wire Line
	8950 2900 8850 2900
Wire Wire Line
	8950 2800 8850 2800
Wire Wire Line
	8950 2700 8850 2700
Wire Wire Line
	8950 2600 8850 2600
Wire Wire Line
	8950 2500 8850 2500
Wire Wire Line
	8950 2400 8850 2400
Wire Wire Line
	8950 2300 8850 2300
Wire Wire Line
	8950 2200 8850 2200
Wire Wire Line
	8950 2100 8850 2100
Wire Wire Line
	8950 2000 8850 2000
Wire Wire Line
	8950 1900 8850 1900
Wire Wire Line
	8950 1800 8850 1800
Wire Wire Line
	8950 1700 8850 1700
Wire Wire Line
	8950 1600 8850 1600
Wire Wire Line
	8950 1500 8850 1500
Wire Wire Line
	8950 1400 8850 1400
Wire Wire Line
	8950 1300 8850 1300
Wire Wire Line
	6550 3150 6450 3150
Wire Wire Line
	6550 3450 6450 3450
Wire Wire Line
	6550 3350 6450 3350
Wire Wire Line
	6550 2950 6450 2950
Wire Wire Line
	6550 2850 6450 2850
Wire Wire Line
	6550 2750 6450 2750
Wire Wire Line
	6550 2650 6450 2650
Wire Wire Line
	6550 2550 6450 2550
Wire Wire Line
	6550 2450 6450 2450
Wire Wire Line
	6550 2350 6450 2350
Wire Wire Line
	6550 2250 6450 2250
Wire Wire Line
	6550 2150 6450 2150
Wire Wire Line
	6550 2050 6450 2050
Wire Wire Line
	6550 1950 6450 1950
Wire Wire Line
	6550 1850 6450 1850
Wire Wire Line
	6550 1750 6450 1750
Wire Wire Line
	6550 1650 6450 1650
Wire Wire Line
	6550 1550 6450 1550
Wire Wire Line
	6550 1450 6450 1450
Wire Wire Line
	6550 1350 6450 1350
Wire Wire Line
	12250 3300 12150 3300
Wire Wire Line
	12250 2800 12150 2800
Wire Wire Line
	12250 2700 12150 2700
Wire Wire Line
	12250 2600 12150 2600
Wire Wire Line
	12200 2500 12250 2500
Wire Wire Line
	12250 2300 12150 2300
Wire Wire Line
	12250 2100 12150 2100
Wire Wire Line
	12250 2000 12150 2000
Wire Wire Line
	12250 1900 12150 1900
Wire Wire Line
	12250 1800 12150 1800
Wire Wire Line
	12250 1700 12150 1700
Wire Wire Line
	12250 1600 12150 1600
Wire Wire Line
	12250 1500 12150 1500
Wire Wire Line
	12250 1400 12150 1400
Wire Wire Line
	3950 3500 3950 3550
Wire Wire Line
	5350 3200 5450 3200
Wire Wire Line
	5350 3100 5450 3100
Wire Wire Line
	5350 3000 5450 3000
Wire Wire Line
	5350 2900 5450 2900
Wire Wire Line
	5350 2800 5450 2800
Wire Wire Line
	5350 2700 5450 2700
Wire Wire Line
	5350 2600 5450 2600
Wire Wire Line
	5350 2500 5450 2500
Wire Wire Line
	3950 3200 3850 3200
Wire Wire Line
	3950 3100 3850 3100
Wire Wire Line
	3950 3000 3850 3000
Wire Wire Line
	3950 2900 3850 2900
Wire Wire Line
	3950 2800 3850 2800
Wire Wire Line
	3950 2700 3850 2700
Wire Wire Line
	3950 2600 3850 2600
Wire Wire Line
	3950 2500 3850 2500
Wire Wire Line
	3600 2100 3950 2100
Wire Wire Line
	7950 2050 8050 2050
Wire Wire Line
	7950 1950 8050 1950
Wire Wire Line
	7950 1850 8050 1850
Wire Wire Line
	7950 1750 8050 1750
Wire Wire Line
	7950 1650 8050 1650
Wire Wire Line
	7950 1550 8050 1550
Wire Wire Line
	7950 1450 8050 1450
Wire Wire Line
	7950 1350 8050 1350
Wire Wire Line
	10350 2000 10450 2000
Wire Wire Line
	10350 1900 10450 1900
Wire Wire Line
	10350 1800 10450 1800
Wire Wire Line
	10350 1700 10450 1700
Wire Wire Line
	10350 1600 10450 1600
Wire Wire Line
	10350 1500 10450 1500
Wire Wire Line
	10350 1400 10450 1400
Wire Wire Line
	10350 1300 10450 1300
Wire Wire Line
	1250 5150 1250 5250
Wire Wire Line
	900  5250 1250 5250
Connection ~ 900  4550
Connection ~ 1250 4550
Wire Wire Line
	700  4550 900  4550
Wire Wire Line
	1250 4500 1250 4550
Wire Wire Line
	900  4450 900  4550
Wire Wire Line
	900  4000 900  4050
Wire Wire Line
	900  4000 1250 4000
Wire Wire Line
	700  2400 1450 2400
Wire Wire Line
	700  4550 700  2400
Wire Wire Line
	1450 1150 1450 1200
Wire Wire Line
	3050 3500 3100 3500
Wire Wire Line
	3050 3700 3100 3700
Wire Wire Line
	12250 3100 12150 3100
Wire Wire Line
	12250 3000 12150 3000
Wire Wire Line
	3050 4500 3100 4500
Wire Wire Line
	3050 1200 3950 1200
Wire Wire Line
	3050 1300 3950 1300
Wire Wire Line
	3050 1400 3950 1400
Wire Wire Line
	3050 1500 3950 1500
Wire Wire Line
	3050 1600 3950 1600
Wire Wire Line
	3050 1700 3950 1700
Wire Wire Line
	3050 1800 3950 1800
Wire Wire Line
	3050 1900 3950 1900
Wire Wire Line
	3050 4100 3100 4100
Wire Wire Line
	5350 1900 5450 1900
Wire Wire Line
	5350 1800 5450 1800
Wire Wire Line
	5350 1700 5450 1700
Wire Wire Line
	5350 1600 5450 1600
Wire Wire Line
	5350 1500 5450 1500
Wire Wire Line
	5350 1400 5450 1400
Wire Wire Line
	5350 1300 5450 1300
Wire Wire Line
	5350 1200 5450 1200
Wire Wire Line
	3950 2200 3950 2250
Wire Wire Line
	1300 3200 1050 3200
Wire Wire Line
	1300 2700 1300 3200
Wire Wire Line
	1450 2700 1300 2700
Wire Wire Line
	1050 2600 1450 2600
Wire Wire Line
	6850 11050 6850 11000
Wire Wire Line
	6850 11450 6850 11500
Wire Wire Line
	2600 11350 2600 11450
Wire Wire Line
	1600 5250 1600 5150
Connection ~ 1250 5250
Wire Wire Line
	1600 4550 1600 4750
Wire Wire Line
	1500 4450 1500 4550
Connection ~ 1500 4550
Wire Wire Line
	1500 4000 1500 4050
Connection ~ 1250 4000
Wire Wire Line
	1250 4000 1250 3950
Wire Wire Line
	1450 2200 1450 2250
Wire Wire Line
	1450 2000 1400 2000
Wire Wire Line
	1300 1400 1450 1400
Wire Wire Line
	1300 1400 1300 1500
Wire Wire Line
	1050 3600 1050 3650
Wire Wire Line
	1450 3600 1450 2900
Wire Wire Line
	1050 3600 1450 3600
Wire Wire Line
	3050 3600 3750 3600
Wire Wire Line
	3750 3600 3750 3400
Wire Wire Line
	3750 3400 3950 3400
Wire Wire Line
	11900 8300 12000 8300
Wire Wire Line
	11900 8400 12000 8400
Wire Wire Line
	11900 8600 12000 8600
Wire Wire Line
	11900 8700 12000 8700
Wire Wire Line
	11900 8800 12000 8800
Wire Wire Line
	11900 8900 12000 8900
Wire Wire Line
	10300 8300 10200 8300
Wire Wire Line
	10300 8400 10200 8400
Wire Wire Line
	10300 8500 10200 8500
Wire Wire Line
	10300 8600 10200 8600
Wire Wire Line
	10300 8900 10200 8900
Wire Wire Line
	10300 9000 10200 9000
Wire Wire Line
	10300 9100 10200 9100
Wire Wire Line
	10300 9200 10200 9200
Wire Wire Line
	10300 9300 10200 9300
Wire Wire Line
	13650 3300 14700 3300
Wire Wire Line
	13650 3400 14850 3400
Wire Wire Line
	13650 3500 15000 3500
Connection ~ 6850 11050
Connection ~ 6850 11450
Wire Wire Line
	14250 6750 14350 6750
Wire Wire Line
	14250 6850 14350 6850
Wire Wire Line
	14250 6950 14350 6950
Wire Wire Line
	13250 6850 13150 6850
Wire Wire Line
	13250 6950 13150 6950
Wire Wire Line
	13250 7050 13150 7050
Wire Wire Line
	13250 7150 13150 7150
Wire Wire Line
	13250 7250 13150 7250
Wire Wire Line
	13250 7350 13150 7350
Wire Wire Line
	13250 7450 13150 7450
Wire Wire Line
	13250 7550 13150 7550
Wire Wire Line
	13650 1400 13750 1400
Wire Wire Line
	13650 1500 13750 1500
Wire Wire Line
	13650 1600 13750 1600
Wire Wire Line
	13650 1700 13750 1700
Wire Wire Line
	13650 1800 13750 1800
Wire Wire Line
	13650 1900 13750 1900
Wire Wire Line
	13650 2000 13750 2000
Wire Wire Line
	13650 2100 13750 2100
Wire Wire Line
	13650 2300 13750 2300
Wire Wire Line
	13650 2400 13750 2400
Wire Wire Line
	13650 2500 13750 2500
Wire Wire Line
	13650 2600 13750 2600
Wire Wire Line
	13650 2700 13750 2700
Wire Wire Line
	13650 2800 13750 2800
Wire Wire Line
	13650 2900 13750 2900
Wire Wire Line
	13650 3000 13750 3000
Wire Wire Line
	13650 3200 14550 3200
Wire Wire Line
	14550 3100 14550 3200
Connection ~ 14550 3200
Wire Wire Line
	14700 3100 14700 3300
Connection ~ 14700 3300
Wire Wire Line
	14850 3100 14850 3400
Connection ~ 14850 3400
Wire Wire Line
	15000 3100 15000 3500
Connection ~ 15000 3500
Wire Wire Line
	15000 2600 15000 2500
Wire Wire Line
	14100 4900 14350 4900
Wire Wire Line
	14350 4900 14350 3700
Wire Wire Line
	14350 3700 13650 3700
Wire Wire Line
	13650 3600 14400 3600
Wire Wire Line
	14400 3600 14400 5000
Wire Wire Line
	14400 5000 14100 5000
Wire Wire Line
	14250 7050 14350 7050
Wire Wire Line
	14250 7150 14350 7150
Wire Wire Line
	14250 7250 14350 7250
Wire Wire Line
	14250 7350 14350 7350
Wire Wire Line
	14250 7450 14350 7450
NoConn ~ 11900 9000
NoConn ~ 11900 9100
$Comp
L RubeCron-cache:GAL16V8 U15
U 1 1 5888BE32
P 13750 7250
F 0 "U15" H 13400 7900 50  0000 L CNN
F 1 "GAL16V8" H 13800 7900 50  0000 L CNN
F 2 "" H 13750 7250 50  0001 C CNN
F 3 "" H 13750 7250 50  0001 C CNN
	1    13750 7250
	1    0    0    -1  
$EndComp
Wire Wire Line
	11900 7600 12750 7600
Wire Wire Line
	12750 7600 12750 6750
Wire Wire Line
	12750 6750 13250 6750
$Comp
L RubeCron-cache:GND-RESCUE-RubeCron #PWR026
U 1 1 5888C0C3
P 13250 7700
F 0 "#PWR026" H 13250 7700 30  0001 C CNN
F 1 "GND-RESCUE-RubeCron" H 13250 7630 30  0001 C CNN
F 2 "" H 13250 7700 60  0000 C CNN
F 3 "" H 13250 7700 60  0000 C CNN
	1    13250 7700
	1    0    0    -1  
$EndComp
Wire Wire Line
	13250 7650 13250 7700
Wire Wire Line
	2600 11150 2600 11050
$Comp
L RubeCron-cache:CONN_1 P5
U 1 1 5888CF83
P 8150 11050
F 0 "P5" H 8230 11050 40  0000 L CNN
F 1 "CONN_1" H 8150 11105 30  0001 C CNN
F 2 "" H 8150 11050 60  0000 C CNN
F 3 "" H 8150 11050 60  0000 C CNN
	1    8150 11050
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:CONN_1 P6
U 1 1 5888CF90
P 8150 11150
F 0 "P6" H 8230 11150 40  0000 L CNN
F 1 "CONN_1" H 8150 11205 30  0001 C CNN
F 2 "" H 8150 11150 60  0000 C CNN
F 3 "" H 8150 11150 60  0000 C CNN
	1    8150 11150
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:CONN_1 P7
U 1 1 5888CF96
P 8150 11250
F 0 "P7" H 8230 11250 40  0000 L CNN
F 1 "CONN_1" H 8150 11305 30  0001 C CNN
F 2 "" H 8150 11250 60  0000 C CNN
F 3 "" H 8150 11250 60  0000 C CNN
	1    8150 11250
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:CONN_1 P8
U 1 1 5888CF9C
P 8150 11350
F 0 "P8" H 8230 11350 40  0000 L CNN
F 1 "CONN_1" H 8150 11405 30  0001 C CNN
F 2 "" H 8150 11350 60  0000 C CNN
F 3 "" H 8150 11350 60  0000 C CNN
	1    8150 11350
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:GND-RESCUE-RubeCron #PWR027
U 1 1 5888D6EE
P 8000 11450
F 0 "#PWR027" H 8000 11450 30  0001 C CNN
F 1 "GND" H 8000 11380 30  0001 C CNN
F 2 "" H 8000 11450 60  0000 C CNN
F 3 "" H 8000 11450 60  0000 C CNN
	1    8000 11450
	1    0    0    -1  
$EndComp
Text Notes 8100 11500 0    60   ~ 0
Mounting holes
Wire Wire Line
	1450 1900 1400 1900
Text Label 1400 1900 2    60   ~ 0
INT1
Text Notes 7250 3750 0    60   ~ 0
or compatible \n(eg AS6C1008)
Wire Wire Line
	10300 8800 10200 8800
Text Label 10200 8800 2    60   ~ 0
A14
Wire Wire Line
	10300 8700 10200 8700
Text Label 10200 8700 2    60   ~ 0
D3
Wire Wire Line
	11900 8500 12000 8500
Text Label 12000 8500 0    60   ~ 0
MEM_A14
$Comp
L RubeCron-cache:CF-CARD CON1
U 1 1 588CCB40
P 18150 5050
F 0 "CON1" H 17650 6600 50  0000 L BNN
F 1 "CF-CARD" H 17650 3450 50  0000 L BNN
F 2 "Compact Flash Connector" H 18180 5200 20  0001 C CNN
F 3 "" H 18150 5050 60  0000 C CNN
	1    18150 5050
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:GND-RESCUE-RubeCron #PWR028
U 1 1 588CDC9E
P 17450 4700
F 0 "#PWR028" H 17450 4700 30  0001 C CNN
F 1 "GND" H 17450 4630 30  0001 C CNN
F 2 "" H 17450 4700 60  0000 C CNN
F 3 "" H 17450 4700 60  0000 C CNN
	1    17450 4700
	1    0    0    -1  
$EndComp
Wire Wire Line
	18850 6250 18950 6250
Text Label 18950 6250 0    60   ~ 0
INT1
Wire Wire Line
	17450 3650 17350 3650
Text Label 17350 3650 2    60   ~ 0
IDE_A0
Wire Wire Line
	17450 3750 17350 3750
Text Label 17350 3750 2    60   ~ 0
IDE_A1
Wire Wire Line
	17450 3850 17350 3850
Text Label 17350 3850 2    60   ~ 0
IDE_A2
Wire Wire Line
	17450 4850 17350 4850
Text Label 17350 4850 2    60   ~ 0
~IDE_CS0
Wire Wire Line
	17450 4950 17350 4950
Text Label 17350 4950 2    60   ~ 0
~IDE_CS1
Wire Wire Line
	18850 3650 18950 3650
Text Label 18950 3650 0    60   ~ 0
IDE_D0
Wire Wire Line
	18850 3750 18950 3750
Text Label 18950 3750 0    60   ~ 0
IDE_D1
Wire Wire Line
	18850 3850 18950 3850
Text Label 18950 3850 0    60   ~ 0
IDE_D2
Wire Wire Line
	18850 3950 18950 3950
Text Label 18950 3950 0    60   ~ 0
IDE_D3
Wire Wire Line
	18850 4050 18950 4050
Text Label 18950 4050 0    60   ~ 0
IDE_D4
Wire Wire Line
	18850 4150 18950 4150
Text Label 18950 4150 0    60   ~ 0
IDE_D5
Wire Wire Line
	18850 4250 18950 4250
Text Label 18950 4250 0    60   ~ 0
IDE_D6
Wire Wire Line
	18850 4350 18950 4350
Text Label 18950 4350 0    60   ~ 0
IDE_D7
Wire Wire Line
	18850 4450 18950 4450
Text Label 18950 4450 0    60   ~ 0
IDE_D8
Wire Wire Line
	18850 4550 18950 4550
Text Label 18950 4550 0    60   ~ 0
IDE_D9
Wire Wire Line
	18850 4650 18950 4650
Text Label 18950 4650 0    60   ~ 0
IDE_D10
Wire Wire Line
	18850 4750 18950 4750
Text Label 18950 4750 0    60   ~ 0
IDE_D11
Wire Wire Line
	18850 4850 18950 4850
Text Label 18950 4850 0    60   ~ 0
IDE_D12
Wire Wire Line
	18850 4950 18950 4950
Text Label 18950 4950 0    60   ~ 0
IDE_D13
Wire Wire Line
	18850 5050 18950 5050
Text Label 18950 5050 0    60   ~ 0
IDE_D14
Wire Wire Line
	18850 5150 18950 5150
Text Label 18950 5150 0    60   ~ 0
IDE_D15
Wire Wire Line
	17450 6450 17350 6450
Text Label 17350 6450 2    60   ~ 0
~IDE_RST
Wire Wire Line
	17450 6050 17350 6050
Text Label 17350 6050 2    60   ~ 0
~IDE_DIOR
Wire Wire Line
	17450 6150 17350 6150
Text Label 17350 6150 2    60   ~ 0
~IDE_DIOW
Wire Wire Line
	18850 5350 18950 5350
Wire Wire Line
	19550 5350 19450 5350
NoConn ~ 18850 5650
NoConn ~ 18850 5750
NoConn ~ 18850 6050
NoConn ~ 18850 5850
NoConn ~ 18850 5950
NoConn ~ 18850 6350
NoConn ~ 18850 6450
$Comp
L RubeCron-cache:GND-RESCUE-RubeCron #PWR029
U 1 1 588D412C
P 17450 5600
F 0 "#PWR029" H 17450 5600 30  0001 C CNN
F 1 "GND" H 17450 5530 30  0001 C CNN
F 2 "" H 17450 5600 60  0000 C CNN
F 3 "" H 17450 5600 60  0000 C CNN
	1    17450 5600
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:R-RESCUE-RubeCron R8
U 1 1 588D466C
P 17150 5700
F 0 "R8" V 17230 5700 40  0000 C CNN
F 1 "1000" V 17157 5701 40  0000 C CNN
F 2 "~" V 17080 5700 30  0000 C CNN
F 3 "~" H 17150 5700 30  0000 C CNN
	1    17150 5700
	0    -1   -1   0   
$EndComp
$Comp
L RubeCron-cache:R-RESCUE-RubeCron R9
U 1 1 588D4679
P 17150 5850
F 0 "R9" V 17230 5850 40  0000 C CNN
F 1 "1000" V 17157 5851 40  0000 C CNN
F 2 "~" V 17080 5850 30  0000 C CNN
F 3 "~" H 17150 5850 30  0000 C CNN
	1    17150 5850
	0    -1   -1   0   
$EndComp
Wire Wire Line
	17400 5700 17400 5750
Wire Wire Line
	17400 5750 17450 5750
Wire Wire Line
	17400 5850 17450 5850
Wire Wire Line
	17450 5850 17450 5950
$Comp
L RubeCron-cache:VCC #PWR030
U 1 1 588D4E52
P 16900 5600
F 0 "#PWR030" H 16900 5700 30  0001 C CNN
F 1 "VCC" H 16900 5700 30  0000 C CNN
F 2 "" H 16900 5600 60  0000 C CNN
F 3 "" H 16900 5600 60  0000 C CNN
	1    16900 5600
	1    0    0    -1  
$EndComp
$Comp
L RubeCron-cache:R-RESCUE-RubeCron R10
U 1 1 588D5111
P 19200 5500
F 0 "R10" V 19280 5500 40  0000 C CNN
F 1 "1000" V 19207 5501 40  0000 C CNN
F 2 "~" V 19130 5500 30  0000 C CNN
F 3 "~" H 19200 5500 30  0000 C CNN
	1    19200 5500
	0    -1   -1   0   
$EndComp
Wire Wire Line
	18850 5450 18950 5450
Wire Wire Line
	18950 5450 18950 5500
Wire Wire Line
	19950 5500 19450 5500
$Comp
L RubeCron-cache:C-RESCUE-RubeCron C22
U 1 1 588D59A6
P 7350 11250
F 0 "C22" H 7350 11350 40  0000 L CNN
F 1 ".1uF" H 7356 11165 40  0000 L CNN
F 2 "~" H 7388 11100 30  0000 C CNN
F 3 "~" H 7350 11250 60  0000 C CNN
	1    7350 11250
	1    0    0    -1  
$EndComp
Wire Wire Line
	8650 4300 8650 4350
Wire Wire Line
	8650 4300 9050 4300
Wire Wire Line
	900  4550 1250 4550
Wire Wire Line
	900  4550 900  4650
Wire Wire Line
	1250 4550 1250 4750
Wire Wire Line
	1250 4550 1500 4550
Wire Wire Line
	1250 5250 1250 5300
Wire Wire Line
	1250 5250 1600 5250
Wire Wire Line
	1500 4550 1600 4550
Wire Wire Line
	1250 4000 1500 4000
Wire Wire Line
	14550 3200 15050 3200
Wire Wire Line
	14700 3300 15050 3300
Wire Wire Line
	14850 3400 15050 3400
Wire Wire Line
	15000 3500 15050 3500
Wire Wire Line
	3950 4700 3950 4850
Wire Wire Line
	12650 5000 12650 5300
Wire Wire Line
	1450 1400 1450 1800
Wire Wire Line
	19950 5200 19950 5500
Wire Wire Line
	17450 5450 17450 5600
Wire Wire Line
	16900 5600 16900 5850
Wire Wire Line
	6850 11050 7350 11050
Wire Wire Line
	6850 11450 7350 11450
Wire Wire Line
	14550 2600 15000 2600
Wire Wire Line
	15450 3200 15450 3600
Wire Wire Line
	17450 3950 17450 4700
Wire Wire Line
	2600 11450 6850 11450
Wire Wire Line
	2600 11050 6850 11050
Wire Wire Line
	8000 11050 8000 11150
Connection ~ 8000 11150
Wire Wire Line
	8000 11150 8000 11250
Connection ~ 8000 11250
Wire Wire Line
	8000 11250 8000 11350
Connection ~ 8000 11350
Wire Wire Line
	8000 11350 8000 11450
$EndSCHEMATC
