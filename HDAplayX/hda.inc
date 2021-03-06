
;--- stream descriptors: 32 bytes, starting at offset 80h (input)

STREAM struct 8
wCtl       dw ?  ;+0 15:5 rsvd, 4 DEIE desc err int, 3 FEIE fifo err int, 2 IOCE int on compl, 1 RUN, 0 SRST stream reset
bCtl2316   db ?  ;+2 23:20 stream number (0=reserved, 1-15 = stream 1-15)
bSts       db ?  ;+3
dwLinkPos  dd ?  ;+4
dwBufLen   dd ?  ;+8
wLastIdx   dw ?  ;+12
wFIFOmark  dw ?  ;+14
wFIFOsize  dw ?  ;+16
wFormat    dw ?  ;+18
qwBuffer   dq ?  ;+24
STREAM ends

;--- format:
;--- [15] stream type: 0=PCM, 1=non-PCM
;--- [14] sample base rate: 0=48 kHz, 1=44.1 kHz
;--- [13:11]: sample base rate multiple: 0=x1 or less,1=x2,2=x3,3=x4,4-7 reserved
;--- [10:8]: sample base rate divisor: 0=div 1,1=div 2,2=div 3,3=div 4,4=div 5,5=div 6,6=div 7,7=div 8
;--- [7]: reserved
;--- [6:4]: bits per sample: 0=8, 1=16, 2=20, 3=24, 4=32, 5-7 reserved
;--- [3:0]: number of channels: 0=1, 1=2, ... 15=16

;--- gctl: [0] CRST: writing 0 -> controller resets
;---                 writing 1 -> exit reset

;--- structure of HDA controller memory-mapped registers

HDAREGS struct 8
gcap     dw ?
vminor   db ?
vmajor   db ?
opayload dw ?
ipayload dw ?
gctl     dd ?
wakeen   dw ?
statests dw ?
gsts     dd ?	;+10h
         dd ?,?,?
intctl   dd ?	;+20h
         dd ?
intsts   dd ?
         dd ?
walclk   dd ?	;+30h
         dd ?
ssync    dd ?
         dd ?
corbbase dq ?	;+40h size of CORB 256*4 = 1024
corbwp   dw ? 
corbrp   dw ? 
corbctl  db ?
corbsts  db ? 
corbsize db ?	;2, 16 or 256 entries (bits 1:0)
         db ?
rirbbase dq ?	;+50h size of RIRB 256*8 = 2048
rirbwp   dw ? 
rirbric  dw ? 
rirbctl  db ?
rirbsts  db ?
rirbsize db ?
         db ?
ic       dd ?	;+60h immediate command register
ir       dd ?	;immediate response register
ics      dw ?	;immediate command status register
         org 80h
stream0  STREAM <>
stream1  STREAM <>
stream2  STREAM <>
stream3  STREAM <>
stream4  STREAM <>
stream5  STREAM <>
stream6  STREAM <>
stream7  STREAM <>
HDAREGS ends

;--- bits in ics
;--- bit 0 (immediate command busy): 1=busy (set to 1 by software to run command)
;--- bit 1 (immediate result valid): 1=valid new response. 
;---        reset by software (by writing 1 to it!) before a new response is expected.

ICS_ICB equ 1
ICS_IRV equ 2

;--- DMA position in current buffer
;--- must be aligned to 128-byte
;--- for stream descriptor 0: dd pos, rsvd
;--- for stream descriptor 1: dd pos, rsvd,
;--- ....

;--- structure of BDL entry (size 16 bytes)
;--- start of BDL itself must be 128-byte aligned

BDLENTRY struct
qwAddr dq ?
dwLen  dd ?
dwFlgs dd ?	;bit 0:1=IOC (interrupt on completion)
BDLENTRY ends

;--- CORB: buffer must start on a 128-byte boundary, size in CORBSIZE
;--- each entry is 32-bits
;--- corb read pointer (CORBRP): write 1 to [15] to reset this pointer to 0
;--- if reset is complete, [15] will be 1.
;--- RIRB: buffer must start on a 128-byte boundary, size in RIRBSIZE
;--- each entry is 64-bits: response, response_ex (0-3: codec, 4: 1=unsolicited response)

;--- codec communication.
;--- 0003: set amplifier gain/mute
;--- bit 15: set output amp
;--- bit 14: set input amp
;--- bit 13: set left amp
;--- bit 12: set right amp
;--- bit 11-8: index (usually 0)
;--- bit 7: mute
;--- bit 6-0: gain
;
;--- default configuration (F1C)
;--- 31:30 port connectivity
;--- 29:24 location
;--- 23:20 default device
;--- 19:16 connection type
;--- 15:12 color
;--- 11:8 misc
;--- 7:4 def. association
;--- 3:0 sequence

;--- widget types

WTYPE_AUDIOOUT	equ 0
WTYPE_AUDIOIN	equ 1
WTYPE_MIXER		equ 2
WTYPE_SELECTOR	equ 3
WTYPE_PIN		equ 4
WTYPE_POWER		equ 5
WTYPE_VOLKNOB	equ 6
WTYPE_BEEPGEN	equ 7

;--- pin default device types

DEFDEV_LINEOUT equ 0
DEFDEV_SPEAKER equ 1
DEFDEV_HEADPHONE equ 2


