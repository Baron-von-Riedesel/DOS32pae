
;*** definitions DOS Protected Mode Interface ***

RMCS    struct          ;real mode call structure
rEDI    dd ?            ;+0
rESI    dd ?            ;+4
rEBP    dd ?            ;+8
        dd ?            ;+12
rEBX    dd ?            ;+16
rEDX    dd ?            ;+20
rECX    dd ?            ;+24
rEAX    dd ?            ;+28
rFlags  dw ?            ;+32
rES     dw ?            ;+34
rDS     dw ?            ;+36
rFS     dw ?            ;+38    
rGS     dw ?            ;+40  
rCSIP   dd ?            ;+42
rSSSP   dd ?            ;+46
RMCS    ends

MEMINFO struct
maxBlock      dd ?            ;0	;in bytes
freeUnlocked  dd ?            ;4	;pages (max unlocked allocation)
maxLockable   dd ?            ;8	;pages (max locked allocation)
totalAdrSpace dd ?            ;12
unlocked      dd ?            ;16
freePhys      dd ?            ;20
totalPhys     dd ?            ;24
freeAdrSpace  dd ?            ;28
swapFile      dd ?            ;32
res           db 12 dup (?)
MEMINFO ends

EXCFRAME struct
_dpmieip	dd ?
_dpmics		dd ?
errorcode	dd ?
_eip		dd ?
_cs			dd ?
_eflags		dd ?
_esp		dd ?
_ss			dd ?
EXCFRAME ends

