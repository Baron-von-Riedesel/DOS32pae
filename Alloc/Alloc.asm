
;--- sample showing how huge amounts of memory can be allocated.
;--- XMS v3.5 is used here, physical memory is allocated from the pool beyond
;--- 4 GB and mapped at linear address 0x400000.
;--- the preferred load address of the image is 0x200000h.
;--- requirements: needs 8 GB of RAM!

    .386
    .model flat, stdcall
    option casemap:none
    option proc:private

lf  equ 10

?BLOCKSIZE equ 4092	;size in MB of block to allocate

;--- define a string
CStr macro text:vararg
local sym
    .const
sym db text,0
    .code
    exitm <offset sym>
endm

    include dpmi.inc

    .data

dwXMS  dd 0	;XMS entry point
xmshdl dw 0	;XMS handle for allocated EMB

    .code

    include printf.inc

ifdef _DEBUG
@dprintf macro text:req,args:vararg
local sym
	.const
sym db text,10,0
	.code
	pushad
ifnb <args>
	invoke printf, offset sym, args
else
	invoke printf, offset sym
endif
	popad
endm
else
@dprintf textequ <;>
endif

;--- allocate an EMB
;--- dwSize: size in MB

allocmem proc dwSize:dword

local rmcs:RMCS

;--- first find XMM entry point.

    @dprintf "allocmem: calling int 31h, ax=300h, bx=2Fh (ax=4300h)"
    lea edi,rmcs
    xor eax,eax
    mov rmcs.rAX,4300h
    mov rmcs.rFlags,3202h
    mov rmcs.rSSSP,0
    mov bx,2fh
    mov cx,0
    mov ax,0300h
    int 31h
    mov eax,rmcs.rEAX
    .if al != 80h
        invoke printf, CStr("no XMM found",lf)
        jmp error
    .endif

;--- get driver entry address

    @dprintf "allocmem: calling int 31h, ax=300h, bx=2Fh (ax=4310h)"
    mov rmcs.rAX,4310h
    mov bx,2fh
    mov cx,0
    mov ax,0300h
    int 31h

;--- copy XMS entry point to rmcs.CS:IP.
;--- the XMM will be called via a DPMI "call real-mode proc with RETF frame".

    mov ax,rmcs.rES
    mov dx,rmcs.rBX
    mov rmcs.rCS,ax
    mov rmcs.rIP,dx
    push ax
    push dx
    pop eax
    mov dwXMS, eax	;save the driver's address in global variable

;--- allocate (& lock) XMS memory
;--- use XMS v3.5 function 0C9h, to allocate the block
;--- beyond the 4 GB barrier.

    @dprintf "allocmem: calling int 31h, ax=301h (ax=0C900h)"
    mov rmcs.rAX,0C900h
	mov eax, dwSize
	shl eax,10	;convert size to kB
	add eax,3	;add 3 kB for page alignment
    mov rmcs.rEDX,eax
    mov cx,0
    mov ax,0301h
    int 31h
    .if rmcs.rAX != 1
        invoke printf, CStr("XMS memory allocation failed",lf)
        jmp error
    .endif
    mov ax,rmcs.rDX
    mov xmshdl,ax
    mov rmcs.rAX,0CC00h ;lock the block
    mov ax,0301h
    int 31h
    .if rmcs.rAX != 1
        invoke printf, CStr("XMS memory lock failed",lf)
        jmp error
    .endif
    mov eax,rmcs.rEBX
    mov edx,rmcs.rEDX
    push edx
    push eax
    invoke printf, CStr("allocmem: EMB physical address=%lX",lf), edx::eax
    pop eax
    pop edx

;--- align the physical block to page boundary

    add eax,1000h-1
    adc edx,0
    and ax,0F000h
    push edx
    push eax
    invoke printf, CStr("allocmem: aligned physical address=%lX",lf), edx::eax
    pop eax
    pop edx
    clc
    ret
error:
    stc
    ret
allocmem endp

;--- unlock and free EMB

freemem proc

local rmcs:RMCS

    lea edi,rmcs
    mov rmcs.rAX, 0D00h ;unlock block
    mov cx,xmshdl
    mov rmcs.rDX, cx
    mov rmcs.rFlags,3202h
    mov eax,dwXMS
    mov rmcs.rCSIP, eax
    mov rmcs.rSSSP,0
    mov cx,0
    mov ax,301h
    int 31h
    mov rmcs.rAX, 0A00h ;free block
    mov ax,301h
    int 31h
    ret

freemem endp

main proc c

local dwAddr:dword
local qwPhysMem:qword

    invoke printf, CStr("cs=%X ss=%X esp=%X ",lf), cs, ss, esp

;--- allocate address space

    mov ebx,400000h
    mov ecx,?BLOCKSIZE * 1024 * 1024	;size in bytes
    mov dx,0
    mov ax,504h
    int 31h
    .if CARRY?
        invoke printf, CStr("allocating address space failed",lf)
        jmp exit
    .endif
    mov dwAddr, ebx
    invoke printf, CStr("uncommitted memory at %X, handle=%X, size=%u MB",lf), ebx, esi, ?BLOCKSIZE

;--- allocate physical memory

    invoke allocmem, ?BLOCKSIZE
    jc exit
    mov dword ptr qwPhysMem+0, eax
    mov dword ptr qwPhysMem+4, edx

;--- map physical memory into allocated address space

    mov edi,edx
    mov edx,eax
    xor ebx,ebx
    mov ecx,?BLOCKSIZE * 1024 * 1024 / 4096	;size in 4k pages
    mov ax,518h
    int 31h
    .if CARRY?
        invoke printf, CStr("mapping physical memory failed",lf)
        jmp exit
    .endif
    invoke printf, CStr("physical memory block mapped",lf)

;--- clear newly allocated memory

if 0
    invoke printf, CStr("press a key to clear memory...")
    mov ah,1        ;read a key from keyboard with echo
    int 21h
    invoke printf, CStr(lf)
endif
    mov edi, dwAddr
    mov ecx, ?BLOCKSIZE * 1024 * 1024
    lea edx,[edi+ecx-1]
    shr ecx, 2
    mov eax, "BEAD"
    rep stosd
    invoke printf, CStr("block %X-%X cleared",lf), dwAddr, edx

exit:
    .if xmshdl
        call freemem
    .endif
    ret
main endp

    end main
