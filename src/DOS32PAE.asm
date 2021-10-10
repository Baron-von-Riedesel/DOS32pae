
;--- DOS stub program. It does:
;--- - check if cpu supports PAE feature
;--- - check that image is PE it can handle
;--- - allocates XMS memory and loads the image
;--- - initializes IDT
;--- - setup PAE paging tables (using unreal mode)
;--- - switch to 32-bit protected mode, PAE paging enabled
;--- - init segment registers to FLAT mode, calls PE entry point.
;---
;--- Note: requires at least JWasm v2.
;--- To create the binary enter:
;---  JWasm -mz Dos32pae.asm

    .model small
DGROUP group _TEXT
    .stack 5120
    .dosseg
    .586p
    option casemap:none

    include peimage.inc
    include dpmi.inc

?IRQ0TORM equ 1			;1=route IRQ 0 (clock) to real-mode
?IRQ1TORM equ 1			;1=route IRQ 1 (keyb) to real-mode

;--- define "conventional" memory region seen by the application.
;--- default is 00000-FFFFF (begin 0, size 256 pages).
;--- ?CONVBEG: if the begin isn't 0, physical page 0 will be mapped in
;--- somewhere else, because access to the IVT is necessary (int 31h, ax=300h).
;--- ?CONVSIZE: it's possible to reduce the size, if there's no
;--- need to access the BIOS in protected-mode.

?CONVBEG  equ 0			;start page of "conventional" memory
?CONVSIZE equ 256		;size of "conventional" memory in 4k-pages

?MPIC     equ 78h		;base master PIC
?SPIC     equ 70h		;base slave PIC
?I31301   equ 1			;1=support int 31h, ax=301h
?I31504   equ 1			;1=support int 31h, ax=504h (uncommitted memory only)
?I31518   equ 1			;1=support int 31h, ax=518h map physical memory
?DIRVIO   equ 1			;1=direct video output for int 41h, 0=use BIOS
?I31SETTG equ 1			;1=allow int 31h to alter task gate CS:EIP, 0=int 31h resets exception 8 to interrupt gate

ife ?CONVBEG
?PG0ADDR  equ 0
?IDTADDR  equ (?CONVBEG+?CONVSIZE)*4096 ;linear address of IDT
else
?PG0ADDR  equ (?CONVBEG+?CONVSIZE)*4096
?IDTADDR  equ ?PG0ADDR+1000h			;linear address of IDT
endif

;--- there's half a page free after the IDT (unlike in Dos64stb)
;--- use the first half of this half-page for a "kernel stack" in the main TSS
;--- and the second half for an alternative double-fault stack
;--- (former not really needed since we really only provide for ring-0 operation,
;--- but it doesn't really do any harm to have it there...)
?KSTKADR  equ ?IDTADDR+0C00h
?DFSTKADR equ ?IDTADDR+0FFCh

ifdef __JWASM__
    option MZ:sizeof IMAGE_DOS_HEADER   ;set min size of MZ header if jwasm's -mz option is used
endif

lf  equ 10

EMM struct  ;XMS block move struct
_size  dd ?
srchdl dw ?
srcofs dd ?
dsthdl dw ?
dstofs dd ?
EMM ends

TSS struct  ;Standard 386 TSS
_res	dw ?	;reserved
Blink	dw ?
ESP_r0	dd ?
SS_r0	dd ?	;only the bottom word may be filled
ESP_r1	dd ?
SS_r1	dd ?	;only the bottom word may be filled
ESP_r2	dd ?
SS_r2	dd ?	;only the bottom word may be filled
TSS_CR3 dd ?	;physical address page directory (=CR3)
TSS_EIP	dd ?
TSS_EFL	dd ?
TSS_EAX	dd ?
TSS_ECX	dd ?
TSS_EDX	dd ?
TSS_EBX	dd ?
TSS_ESP	dd ?
TSS_EBP	dd ?
TSS_ESI	dd ?
TSS_EDI	dd ?
TSS_ES	dd ?
TSS_CS	dd ?
TSS_SS	dd ?
TSS_DS	dd ?
TSS_FS	dd ?
TSS_GS	dd ?
TSS_LDT	dd ?
_res2	dw ?	;reserved
IOPB	dw ?
TSS ends

sizeTSS equ sizeof TSS


;--- a segment consisting of two EMS pages used as a buffer for page tables
;--- (and some other data) for copying to XM when Unreal is unavailable
PTbuflayout struct
;--- first EMS page, first 4k page:
curPT	dq 200h	dup(?)
;--- first EMS page, second 4k page:
curPD	dq 200h	dup(?)
;--- first EMS page, third 4k page:
PDPT	dq 4	dup(?)
pCurPT	dq ?	; physical address of swapped-in PT
pCurPD	dq ?	; physical address of swapped-in PD
pPDPT	dq ?	; physical address of swapped-in PDPT
align	1000h
;--- first EMS page, fourth 4k page (unused):
_unused	dq 200h	dup(?)

;--- second EMS page, first 4k page:
vcpiPT	dd 400h	dup(?)
;--- second EMS page, second 4k page:
vcpiPD	dd 400h dup(?)
;--- second EMS page, third 4k page:
blank	db 400h dup(?) ; 1k of zeros
embadrs	dq 180h	dup(?) ; store qwords representing EMB base physical addresses

PTbuflayout ends


;--- define a string
CStr macro text:vararg
local sym
    .const
sym db text,0
    .code
    exitm <offset sym>
endm

ifdef _DEBUG
dprintf proto c text:ptr byte, args:vararg
@dprintf macro text:req,args:vararg
local sym
CONST segment
sym db text,10,0
CONST ends
ifb <args>
	invoke dprintf, offset sym
else
	invoke dprintf, offset sym, args
endif
endm
else
@dprintf textequ <;>
endif

@wait macro	;debug help: wait for ESC key
local sm1
sm1:
    in al,64h       ;key from keyboard arrived?
    test al,1
    jz sm1
    in al,60h
    cmp al,81h      ;wait for ESC released
    jnz sm1
endm

@fatexit macro text
local sym
    mov dx,offset sym
    mov ah,9
    int 21h
    mov ah,4Ch
    int 21h
    .const
sym db text,13,10,'$'
    .code
endm

@lidt macro addr
if ?IDTADDR ge 1000000h	;is IDT linear address below 16M?
    db 66h
endif
    lidt addr
endm

@rep macro cmd	;if 32-bit string opcode is used in 16-bit
    db 67h
    rep cmd
endm

    .code

SEL_FLATCS equ 1*8
SEL_FLATDS equ 2*8
SEL_CODE16 equ 3*8
SEL_DATA16 equ 4*8
SEL_TSS    equ 5*8
SEL_DFTSS  equ 6*8
SEL_VCPICS equ 7*8

GDT dq 0                ; null descriptor
    dw -1,0,9A00h,00CFh ; flat code descriptor
    dw -1,0,9200h,00CFh ; flat data descriptor
    dw -1,0,9A00h,0000h ; 16-bit DGROUP code descriptor
    dw -1,0,9200h,0000h ; 16-bit DGROUP data descriptor
    dw sizeTSS-1,offset maintss,8900h,0040h ; main TSS
    dw sizeTSS-1,offset dflttss,8900h,0040h ; #DF TSS
sizeGDT equ $-GDT
    dq 3 dup(?)
sizeGDT_VCPI equ $-GDT

inVCPImode equ (word ptr cs:[GDTR] == sizeGDT_VCPI-1)

GDTR label fword        ; Global Descriptors Table Register
    dw sizeGDT-1        ; limit of GDT (size minus one)
    dd offset GDT       ; linear address of GDT
IDTR label fword        ; Interrupt Descriptor Table Register
    dw 256*8-1          ; limit of IDT (size minus one)
    dd ?IDTADDR         ; linear address of IDT
  
nullidt label fword
    dw 3FFh
    dd 0

    .data

;--- the main TSS will not be switched to, so it doesn't need the PDBR filled in
maintss	TSS <0,0,?KSTKADR,SEL_FLATDS,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0DFFFh> ; max possible IOPB offset to ensure NO IOPB!
;--- double-fault task gate TSS - will be switched to for #DF!
dflttss	TSS <0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,?DFSTKADR,0,0,0,SEL_FLATDS,SEL_FLATCS,SEL_FLATDS,SEL_FLATDS,SEL_FLATDS,SEL_FLATDS,0,0,0DFFFh> ; max possible IOPB offset to ensure NO IOPB!
;--- since this TSS needs a PDBR set (for switching), just store our CR3 here.
pPageDir equ dflttss.TSS_CR3

xmsaddr dd ?	;seg:offs address of XMS entry
PhysBase dd ?	;physical page image base
;pPageDir dd ?	;physical address page directory (=CR3)
wStkBot dw ?,?	;real-mode stack bottom, offset & segment
dwESP   dd ?	;protected-mode ESP
        dw SEL_FLATDS
retad   label fword
        dd offset start32
        dw SEL_FLATCS
dwCSIP  dd ?	;real mode CS:IP to be called
adjust  dd ?	;offset to adjust EMB to page boundary
fname   dd ?	;image's file name in environment block
nxtLinAddr dd ?
fhandle dw -1	;DOS file handle for image
wFlags  dw ?	;used to temporarily store real mode flags
xmsidx  dw 0
if ?MPIC ne 8
bPICM   db ?
endif
if ?SPIC ne 70h
bPICS   db ?
endif
vPIC	label word
vPICM	db 8    ;saved master PIC vector
vPICS	db 70h  ;saved slave PIC vector
PTbufseg dw -1  ;low mem buffer for page tables if Unreal Mode unavailable
emshdl	dw -1
V86pgs  dd ?
vcpiadr	label fword
	dd ?
	dw SEL_VCPICS

;--- don't move the saved IVT vectors to .data?
;--- this leaves the full _BSS and CONST space
;--- as extra real-mode stack space.
if ?MPIC ne 8
storedIntM label dword
        dd 8 dup (?)
endif
if ?SPIC ne 70h
storedIntS label dword
        dd 8 dup (?)
endif

;--- persistent EMM block used for setting up page tables without Unreal
emm3    EMM <>

;--- VCPI control structure
vcpiCR3	dd ?		; not the PAE CR3, a 32-bit one!
vcpiGDT dd offset GDTR
vcpiIDT	dd offset IDTR
vcpiLDT dw 0
vcpiTSS dw SEL_TSS
vcpiEIP dd ?
vcpiCS	dw SEL_CODE16

_DATA1 segment word "DATA"
DGROUP group _DATA1

MEMBLK struct 4
wHdl   dw ?
dwAddr dd ?
MEMBLK ends

;--- the table is put BEHIND the .data segment
;--- because the items in .const and .data?
;--- are used during init only - hence the xms handle
;--- table can grow dynamically.
xmshdltab label MEMBLK
xms1 MEMBLK <0,0>  ;handle of allocated XMS block
_DATA1 ends

	.const

intstruc struct
intno db ?
ofs   dw ?
intstruc ends

;--- table of pm interrupts that the stub wants to handle

inttab label intstruc
    intstruc <?MPIC+0, lowword offset clock>
    intstruc <?MPIC+1, lowword offset keyboard>
    intstruc <21h,     lowword offset int21>
    intstruc <31h,     lowword offset int31>
    intstruc <41h,     lowword offset int41>
endinttab equ $

    .data?

nthdr   IMAGE_NT_HEADERS <>
sechdr  IMAGE_SECTION_HEADER <>
emm     EMM <>
emm2    EMM <>

    .code

;--- 16bit start/exit code

CreateAddrSpace proto stdcall linaddr:dword, pages:dword, physpage:dword
MapPages        proto stdcall linaddr:dword, pages:dword, physpage:dword

    assume ds:DGROUP

start16 proc
    mov eax,cs
    mov ds,ax
    shl eax,4
    add dword ptr [GDTR+2], eax ; convert offset to linear address
    add [vcpiGDT], eax		; convert offset to linear address
    add [vcpiIDT], eax		; convert offset to linear address
    mov word ptr [GDT + SEL_CODE16 + 2], ax
    mov word ptr [GDT + SEL_DATA16 + 2], ax
    add word ptr [GDT + SEL_TSS + 2], ax
    add word ptr [GDT + SEL_DFTSS + 2], ax
    shr eax,16
    mov byte ptr [GDT + SEL_CODE16 + 4], al
    mov byte ptr [GDT + SEL_DATA16 + 4], al
    mov byte ptr [GDT + SEL_TSS + 4], al
    mov byte ptr [GDT + SEL_DFTSS + 4], al

    mov ax,ss
    mov dx,es
    sub ax,dx
    mov bx,sp
    shr bx,4
    add bx,ax
    mov ah,4Ah
    int 21h         ; free unused memory
    push cs
    pop es

    mov ax,ss
    mov dx,cs
    sub ax,dx
    shl ax,4
    add ax,sp
    push ds
    pop ss
    mov sp,ax       ; make a TINY model, CS=SS=DS=ES
    mov wStkBot+0,ax
    mov wStkBot+2,ss

    smsw ax
    test al,1
    jz @F
;--- not in RM - therefore can't use Unreal...
    xor ax,ax
    mov [PTbufseg],ax
;--- need VCPI to take control, so check for that...
    mov ah,0DEh
    int 67h
    mov word ptr [GDTR],sizeGDT_VCPI-1 ; 3 extra GDT entries needed!
    test ah,ah
    jz @F
    @fatexit "Mode is V86 and no VCPI. Need REAL mode to switch to PAE paging mode!"
@@:
    xor edx,edx
    mov eax,1   ; test if pae paging is supported
    cpuid
    bt edx,6
    jc @F
    @fatexit "PAE paging not supported."
@@:
    mov ax,4300h
    int 2fh         ;XMS host available?
    test al,80h
    jnz @F
    @fatexit "No XMS host detected."
@@:
    push es
    mov ax,4310h
    int 2fh
    mov word ptr [xmsaddr+0],bx
    mov word ptr [xmsaddr+2],es
    pop es

    mov ah,5        ;local enable A20
    call xmsaddr

;--- do VCPI setup work if necessary before transferring stuff into XMS mem

    .if inVCPImode
        mov ah,41h ; get page frame address
        int 67h
        test ah,ah
        jnz @@tryumb
        mov [PTbufseg],bx

;--- allocate two EMS pages (i.e. eight 4k pages)
;--- first EMS page for temporary storage of PDPT/PDs/PTs before copying to XM
;--- second one for VCPI PT and VCPI PD (latter with only one entry)
;--- lots of space "wasted" here (it could all be compressed into one EMS page)
;--- but this strategy is reusable for Dos64Stb (where four-level paging makes
;--- better use of the first EMS page)
        mov ebx,2
        mov ah,43h ; allocate pages
        int 67h
        test ah,ah
        jnz @@tryumb
        mov [emshdl],dx

        mov ax,4400h ; map handle page, to zeroth "physical" page
        xor bx,bx    ; map the first EMS page of this handle
        int 67h
        test ah,ah
        jnz @@tryumb

        mov ax,4401h ; map handle page, to first "physical" page
        mov bx,1     ; map the second EMS page of this handle
        int 67h
        test ah,ah
        jz @@PTbufallocated

@@tryumb:
;--- no EMS page frame - try an XMS UMB instead...
        mov ah,10h   ; request UMB
        mov dx,sizeof PTbuflayout SHR 4
        call xmsaddr
        cmp ax,1
        jne @@tryconv
        mov [PTbufseg],bx
        jz @@PTbufallocated

@@tryconv:
;--- agh, we need to allocate 28k of conventional memory! :(
        mov ah,48h   ; request UMB
        mov bx,sizeof PTbuflayout SHR 4
        int 21h
        mov [PTbufseg],ax
        jnc @@PTbufallocated

        @fatexit "Unable to allocate low-memory PT buffer"

@@PTbufallocated:
;--- clear the EMS pages
        mov es,[PTbufseg]
        xor di,di
        mov cx,sizeof PTbuflayout SHR 2
        xor eax,eax
        cld
        rep stosd

        mov ax,0DE01h                  ; get protected mode interface
        mov edi,PTbuflayout.vcpiPT     ; second EMS page in the page frame
        mov si,offset GDT+SEL_VCPICS   ; DS:SI points to descriptors
        int 67h
        mov dword ptr [vcpiadr],ebx
        sub edi,PTbuflayout.vcpiPT
        mov ecx,edi
        shr ecx,2                      ; convert to page count
        mov [V86pgs],ecx
;--- clear bits 9-11 of all the PT entries, as per the VCPI standard
@@:
        and byte ptr es:[ecx*4-3],0F1h ; byte index n*4 - 3, where n > 0
        loop @B

        .if !([PTbufseg] & 0FFh)
;--- get the physical addx of the PT we just filled in
;--- (but only if it's page-aligned!)
            mov di,[PTbufseg]
            add di,PTbuflayout.vcpiPT SHR 4 ; convert offset to segment delta
            ;shl edi,4      ; paragraphs to bytes
            ;shr edi,12     ; bytes to pages
            ;shl edi,2      ; page count to PT index
            shr di,6
            add di,PTbuflayout.vcpiPT
            mov ebx,[es:di] ; physical addx of VCPI PT
            add di,4
            mov ecx,[es:di] ; physical addx of empty PD (very next page)
            and cx,0F000h   ; get just the addx, no attrs
            mov di,PTbuflayout.vcpiPD
            mov [es:di],ebx ; put the PT addx at the beginning of the PD
            mov vcpiCR3,ecx ; address of the PD for VCPI switch
        .endif

        push ds
        pop es
    .endif

    push es
    mov ah,51h
    int 21h
    mov es,bx
    mov es,es:[002Ch]
    xor di,di
    xor al,al
    mov cx,-1
@@:
    repnz scasb
    cmp byte ptr es:[di],0
    jnz @B
    add di,3
    mov word ptr fname+0,di
    mov word ptr fname+2,es
    pop es
    push ds
    lds dx,fname
    mov ax,3D00h
    int 21h
    pop ds
    mov bp,CStr("cannot open file")
    jc @error
    mov fhandle,ax
    mov bx,ax
;--- load the file header
    sub sp,4096
    mov cx,sizeof IMAGE_DOS_HEADER
    mov dx,sp
    mov ah,3Fh
    int 21h
    cmp ax,cx
    mov bp, CStr("invalid file format")
    jnz @error
    movzx edx,dx
    cmp word ptr [edx].IMAGE_DOS_HEADER.e_magic,"ZM"
    mov bp, CStr("invalid file format (no MZ header)")
    jnz @error
    cmp word ptr [edx].IMAGE_DOS_HEADER.e_lfarlc,sizeof IMAGE_DOS_HEADER
    mov bp, CStr("invalid file format (MZ header too small)")
    jc @error
    mov cx,word ptr [edx].IMAGE_DOS_HEADER.e_lfanew+2
    mov dx,word ptr [edx].IMAGE_DOS_HEADER.e_lfanew+0
    mov ax,4200h
    int 21h
    mov dx,offset nthdr
    mov cx,sizeof nthdr
    mov ah,3Fh
    int 21h
    cmp ax,cx
    mov bp, CStr("invalid file format (cannot locate PE header)")
    jnz @error

    cmp dword ptr nthdr.Signature,"EP"
    mov bp, CStr("invalid file format (no PE header)")
    jnz @error
    cmp nthdr.FileHeader.Machine,IMAGE_FILE_MACHINE_I386
    mov bp, CStr("not a 32-bit binary")
    jnz @error
if 0
    test nthdr.FileHeader.Characteristics,IMAGE_FILE_RELOCS_STRIPPED
    mov bp, CStr("relocations stripped, cannot load")
    jnz @error
endif
    cmp nthdr.OptionalHeader.Subsystem,IMAGE_SUBSYSTEM_NATIVE
    mov bp, CStr("subsystem not native, cannot load")
    jnz @error
    cmp nthdr.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT*sizeof IMAGE_DATA_DIRECTORY].Size_,0
    mov bp, CStr("image contains imports, cannot load")
    jnz @error
    mov edx, nthdr.OptionalHeader.SizeOfImage
    add edx, dword ptr nthdr.OptionalHeader.SizeOfStackReserve
    mov bp, CStr("image + stack > 4GB")
    jc @error
    shr edx,12      ;convert to 4k pages

;--- calculate space for page tables (each PT covers 2 MB)
    mov eax,edx
    shr eax,9
    add eax,2       ;add 2 pages (max overhead)
    add edx,eax
;--- add space for IDT and page tables
;--- needed: 1 page  for IDT + PDPT
;---         max 4 pages for PD (4 * 512 * PDEs )
;--- total:  5 pages
    add edx, 5
    .if inVCPImode && ([PTbufseg] & 0FFh)
;--- PTbufseg not page-aligned, so need to move VCPI PT/PD (2 pages) into XM too
        add edx,2
    .endif
    inc edx     ;extra page since we need to align to page boundary
    shl edx, 2  ;convert to kB

    mov esi, edx

    mov ah,89h
    call xmsaddr
    cmp ax,1
    mov bp, CStr("XMS memory allocation failed")
    jnz @error
    mov xms1.wHdl,dx
    inc xmsidx
    mov emm.dsthdl,dx
    mov ah,0Ch      ;lock EMB 
    call xmsaddr
    cmp ax,1
    mov bp, CStr("cannot lock EMB")
    jnz @error
    push dx
    push bx
    pop edi
    .if ([PTbufseg] != 0) && ([PTbufseg] != -1)
        mov es,[PTbufseg]
        mov dword ptr [es:PTbuflayout.embadrs],edi
        push ds
        pop es
    .endif
    push edi
    call ClearRegion
    mov bp, CStr("cannot clear EMB")
    jc @error

;--- align to page boundary
    pop eax
    add eax,1000h-1
    and ax, 0f000h
    push eax
    sub ax,bx
    movzx eax,ax
    mov emm.dstofs,eax
    mov adjust, eax
    pop eax
    shr eax,12
    mov PhysBase, eax


;--- copy the PE header into extended memory

    mov ecx, sizeof nthdr
    mov emm.srchdl, 0
    mov word ptr emm.srcofs+0, offset nthdr
    mov word ptr emm.srcofs+2, ds
    mov si,offset emm
    call copy2x_ordie

;--- read image sections and copy them to extended memory

    mov di,sp
    mov bx,fhandle
    mov cx,nthdr.FileHeader.NumberOfSections
    .while cx
        push cx
        mov dx,offset sechdr
        mov cx,sizeof sechdr
        mov ah,3Fh
        int 21h
        cmp ax,cx
        mov bp, CStr("error reading section headers")
        jnz @error
        mov si,offset emm
        call copy2x_ordie
        call readsection
        pop cx
        dec cx
    .endw

    mov ah,3Eh
    int 21h
    mov fhandle,-1

;--- create IDT

    .if inVCPImode
;--- check if anyone has reprogrammed the PIC, and setup our IDT accordingly
        mov ax,0DE0Ah
        int 67h
        mov vPICM,bl
        mov vPICS,cl
        .if vPIC != 7008h
            mov [inttab.intno],bl                 ; clock vector (IRQ0)
            inc bl
            mov [inttab.intno+sizeof intstruc],bl ; kbd vector (IRQ1)
        .endif
    .endif

    mov ebx, _TEXT32
    shl ebx, 4
    add dword ptr [retad],ebx

    mov cx,32
    mov edx, offset exception
make_exc_gates:
    lea eax,[ebx+edx]
    stosw
    .if ecx == 24    ; 32-8 - i.e. this is the double-fault gate
        mov [dflttss.TSS_EIP],eax
        mov eax,SEL_DFTSS
        stosw
        mov ax,8500h ; Task Gate
    .else
        mov ax,SEL_FLATCS
        stosw
        mov ax,8E00h ; Interrupt Gate
    .endif
    stosd
    add edx,4
    loop make_exc_gates

    mov cx,256-32
    mov edx,offset swint
    mov si, 8F00h
    call make_int_gates

    mov di,sp
    lea di,[di+?MPIC*8]
    mov cx,8
    mov edx,offset Irq0007
    mov si, 8E00h
    call make_int_gates
    mov di,sp
    lea di,[di+?SPIC*8]
    mov cx,8
    mov edx,offset Irq080F
    call make_int_gates

;--- setup IRQ0, IRQ1, Int21, Int31, Int41

    mov si, offset inttab
    .while si < endinttab
        lodsb
        movzx di,al
        shl di,3
        add di,sp
        lodsw
        movzx eax,ax
        add eax, ebx
        mov [di+0],ax
        shr eax,16
        mov [di+6],ax
    .endw

;--- copy IDT to extended memory

    mov di, sp
    mov word ptr emm.srcofs+0,sp
    mov eax,nthdr.OptionalHeader.SizeOfImage
    add eax,nthdr.OptionalHeader.SizeOfStackReserve
    add eax,adjust
    mov emm.dstofs, eax

    mov si,offset emm
    mov ecx,800h
    call copy2x_ordie

    add sp,4096

;--- setup page directories and tables

    mov esi,nthdr.OptionalHeader.SizeOfImage
    add esi,nthdr.OptionalHeader.SizeOfStackReserve
    mov eax,nthdr.OptionalHeader.ImageBase
    add eax,esi
    mov nxtLinAddr, eax
    shr esi,12
    push esi        ;size of image+stack in pages
    add esi,PhysBase

;--- esi = next free physical page
;--- first page is used for IDT and page dir

    mov eax,esi
    shl eax,12
    add eax, 800h
    mov [pPageDir],eax
    push esi
    inc esi

    .if inVCPImode && ([PTbufseg] & 0FFh)
;--- PTbufseg not page-aligned, so need to move VCPI PT/PD (2 pages) into XM too
        mov es,[PTbufseg]
        mov word ptr emm.srcofs+2,es
        mov word ptr emm.srcofs+0,PTbuflayout.vcpiPT
        add emm.dstofs, 800h ; move to the next page

        mov eax,esi
        shl eax,12
        mov [es:PTbuflayout.vcpiPD],eax
        or [es:PTbuflayout.vcpiPD],11b
        add eax, 1000h
        mov [vcpiCR3],eax

        push ds
        pop es

        push esi
        mov si,offset emm
        mov ecx,2000h
        call copy2x_ordie
        pop esi
        add esi,2
    .endif

ife ?CONVBEG
    invoke CreateAddrSpace, ?CONVBEG*4096, ?CONVSIZE+1, esi
    add esi, eax
else
    invoke CreateAddrSpace, ?CONVBEG*4096, ?CONVSIZE+2, esi
    add esi, eax
    invoke MapPages, ?PG0ADDR, 1, 0
endif
    invoke MapPages, ?CONVBEG*4096, ?CONVSIZE, ?CONVBEG
    pop eax
    invoke MapPages, ?IDTADDR, 1, eax
    mov eax,[esp]
    invoke CreateAddrSpace, nthdr.OptionalHeader.ImageBase, eax, esi
    pop eax
    mov bp,CStr("Cannot map image in address space")
    jc @error
    invoke MapPages, nthdr.OptionalHeader.ImageBase, eax, PhysBase

;--- disable int 23h termination (myint23)
;--- or exit program (@@exit2)
;    mov dx,offset myint23
    mov dx,offset @@exit2
    mov ax,2523h
    int 21h

    cmp vPIC,7008h
    jne @F
    call setints
if ?MPIC ne 8
    in al,21h
    mov bPICM,al
endif
if ?SPIC ne 70h
    in al,0A1h
    mov bPICS,al
endif

@@:
    @dprintf "start16: switching to pm"

    cli
    cmp vPIC,7008h
    jne @F
    mov dx,?SPIC shl 8 or ?MPIC
    call setpic
@@:
    call switch2pm

;--- now load:
;--- esi = value of image's EIP
;--- ebx = image start linear address

    mov ebx,cs:nthdr.OptionalHeader.ImageBase
    mov ecx,cs:nthdr.OptionalHeader.SizeOfStackReserve
    add ecx,cs:nthdr.OptionalHeader.SizeOfImage
    add ecx,ebx
    mov esi,cs:nthdr.OptionalHeader.AddressOfEntryPoint
    add esi,ebx

    mov ax,SEL_FLATDS
    mov ss,ax
    mov esp,ecx
    mov ds,ax
    mov es,ax
    xor ax,ax
    mov fs,ax
    mov gs,ax
    .if !inVCPImode
        mov ax,SEL_TSS
        ltr ax
    .endif
    sti

;    call HandleRelocs

    jmp cs:[retad]

@error::
    mov si,bp
nextchar:
    lodsb
    and al,al
    jz done
    mov dl,al
    mov ah,2
    int 21h
    jmp nextchar
done:
    mov dx,offset @dotnewline
    mov ah,9
    int 21
    jmp @@exit
@dotnewline db '.',13,10,'$'

make_int_gates:
    lea eax, [ebx+edx]
    stosw
    mov ax,SEL_FLATCS
    stosw
    mov ax,si           ;int/trap gate
    stosd
    loop make_int_gates
    ret

start16 endp

;--- clear region, edi=addr, esi=size in kB

ClearRegion proc
;--- clear the whole block
    call EnableUnreal
    jc @F
    mov ecx,esi
    shl ecx,8  ;kb to dword
    xor eax,eax;clears CF
    @rep stosd
    push ds
    pop es
    ret

@@:
;--- no Unreal, need to use fiddly XMS helpers instead...
    push esi
    mov ecx,esi

    call FindXmsHdl
    mov [emm3.srchdl],0
    mov si,[PTbufseg]
    mov word ptr [emm3.srcofs],PTbuflayout.blank
    mov word ptr [emm3.srcofs+2],si

    mov si,offset emm3
@@:
    push ecx
    mov ecx,400h
    call copy2x
    pop ecx
    jc @F
    loop @B
@@:
    pop esi
    ret
ClearRegion endp

;--- create address space
;--- linaddr: start linear address space to create
;--- pages: no of pages to map
;--- physpage: start physical memory pages used for paging (physical address >> 12)

CreateAddrSpace proc stdcall uses es esi di linaddr:dword, pages:dword, physpage:dword

    @dprintf "CreateAddrSpace(%lX, %lX, %lX) enter", linaddr, pages, physpage
    call EnableUnreal
    setc dh
    movzx di,dh ; Use DI to track if Unreal is unavailable
    mov esi, physpage
    .while pages
        mov eax,linaddr
        shr eax,9
        push eax
        shr eax,9
        push eax

;--- 2 offsets pushed:
;--- 1. bits 0-11 offset in PT   (linaddr [12-20])
;--- 2. bits 0-11 offset in PD   (linaddr [21-29])
        shr eax,9           
;--- 3. bits 0-11 offset in PDPT (linaddr [30-31])
        and eax,0018h
        mov cx,2
        mov ebx,pPageDir
@@:
        .if di
            call CopyInTable
            jnc copytabok
            shl cx,2
            add sp,cx
            stc
            jmp exit
copytabok:
        .endif
        add ebx,eax
        mov edx,es:[ebx]
        .if (edx == 0)             ;does PDPTE/PDE exist?
            mov edx,esi
; the paging tables must be below 4GB - if this
; is to change, we cannot use unreal mode here anymore!
;            sub eax,eax
;            shld eax,edx,12
            shl edx,12
            cmp cx,2               ;don't set bit 1 for PDPTE
            adc dl,dl
            shl dl,1
            or dl,1
            mov es:[ebx+0],edx
;            mov es:[ebx+4],eax
            inc esi
        .endif
        pop eax
        and eax,0ff8h
        mov ebx,edx
;       and bx,0FC00h
        and bx,0F000h
        loop @B
        .if di
            call CopyInTable
            jc exit
        .endif
        add ebx,eax
        test dword ptr es:[ebx],1
        stc
        jnz exit
        add linaddr,1000h
        dec pages
    .endw
    .if di
;--- swap out the page tables
        mov cx,3 ; loop three times...
        mov es,[PTbufseg]
        mov di,PTbuflayout.pPDPT
@@:
        mov ebx,[es:di]
        dec cx   ; ... but do so with cx = 2,1,0!
        call CopyOutTable
        jc exit
        inc cx
        sub di,8
        loop @B
    .endif
    mov eax,esi
    sub eax,physpage
    @dprintf "CreateAddrSpace() ok, esi=%lX, eax=%lX", esi, eax
    clc
ifdef _DEBUG
    ret
exit:
    @dprintf "CreateAddrSpace() failed, linaddr=%lX, pages=%lX, ebx=%lX", linaddr, pages, ebx
    stc
else
exit:
endif
    ret

CreateAddrSpace endp

;--- map physical pages in existing address space
;--- linaddr: start linear address space 
;--- pages: no of pages to map
;--- physpage: start physical page to map (phys page=phys addr shr 12)
;--- note: physical address of paging tables must be 32-bit.

MapPages proc stdcall uses es linaddr:dword, pages:dword, physpage:dword

    @dprintf "MapPages(%lX, %lX, %lX) enter", linaddr, pages, physpage
    call EnableUnreal
    setc dh
    movzx di,dh ; Use DI to track if Unreal is unavailable
    .while pages
        mov eax,linaddr
        shr eax,9
        push eax
        shr eax,9
        push eax
        shr eax,9           

;--- 1. bits 0-11 offset in PT   (linaddr [12-20])
;--- 2. bits 0-11 offset in PD   (linaddr [21-29])
;--- 3. bits 0-4  offset in PDPT (linaddr [30-31])

        mov cx,2
        mov ebx,pPageDir
        and eax,0ff8h
@@:
        .if di
            call CopyInTable
            jnc copytabok
            shl cx,2
            add sp,cx
            stc
            jmp exit
copytabok:
        .endif
        add ebx,eax
        pop eax
        and eax,0ff8h
        mov ebx,es:[ebx]
        and bx,0F000h
        loop @B
        .if di
            call CopyInTable
            jc exit
        .endif
        add ebx,eax
        mov edx,physpage
        .if inVCPImode && (edx < cs:[V86pgs])
            ;push es
            mov es,[PTbufseg]
            add edx,PTbuflayout.vcpiPT SHR 2
            mov edx,es:[edx*4]
            xor eax,eax
            ;pop es
        .else
            sub eax,eax
            shld eax,edx,12
            shl edx,12
            or dl,11b
        .endif
        mov es:[ebx+0],edx
        mov es:[ebx+4],eax
        inc physpage
        add linaddr,1000h
        dec pages
    .endw
    .if di
;--- swap out the page tables
        mov cx,3 ; loop three times...
        mov es,[PTbufseg]
        mov di,PTbuflayout.pPDPT
@@:
        mov ebx,[es:di]
        dec cx   ; ... but do so with cx = 2,1,0!
        call CopyOutTable
        jc exit
        inc cx
        sub di,8
        loop @B
    .endif
    clc
exit:
    ret

MapPages endp

;--- copy page table from XMS to EMS/convmem so we can work on it
;--- EBX = physical addx of table
;--- CX  = paging level
;--- sets ES:EBX to pointer to copied-in page table
CopyInTable proc
    push eax
    push edi

    mov es,[PTbufseg]
    mov di,PTbuflayout.pCurPT shr 3
    add di,cx          ; the CXth entry on the list
    shl di,3

    cmp [es:di],ebx    ; is the physical addx the same one we've swapped in?
    setz al
    xchg [es:di],ebx
    .if (al == 0) && ebx
;--- if we're swapping in a new table, and there's one there now, swap it out
        call CopyOutTable
        jc @F
    .endif

    movzx ebx,cx
    shl bx,12          ; nth paging level corresponds to nth 4k page in buffer

    .if al == 0
        mov edi,[es:di]
        ;@dprintf "Copying level-%d PT In from %lX", cx, edi
        call FindXmsHdl
;--- FindXmsHdl filled in the destination, but here it's actually the source!
        xor di,di
        xchg di,[emm3.dsthdl] ; make destination handle zero (low mem)
        mov [emm3.srchdl],di
        mov edi,[emm3.dstofs]
        mov [emm3.srcofs],edi
;--- our destination is in the buffer segment
;--- TODO: allocate buffer segment in convmem if it's unset (i.e. zero)
;--- (currently never the case since we have already allocated it in EMS)
        mov word ptr [emm3.dstofs+2],es
        mov word ptr [emm3.dstofs],bx

        push ecx
        .if cx == 2
            mov ecx,20h   ; PDPT is only 32 bytes
        .else
            mov ecx,1000h ; copy full page
        .endif

        push esi
        mov si,offset emm3
        call copy2x

        pop esi
        pop ecx
    .else
        clc
    .endif
@@:
    pop edi
    pop eax
    ret
CopyInTable endp

;--- copy page table from EMS/convmem back out to XMS
;--- EBX = physical addx of table
;--- CX  = paging level
CopyOutTable proc
    push eax
    push edi
    push esi
    mov edi,ebx
    call FindXmsHdl
;--- FindXmsHdl filled in the destination; our source is in the buffer segment
    mov [emm3.srchdl],0 ; low mem

    push es
    mov es,[PTbufseg]
    movzx edi,cx
    shl di,12 ; nth paging level corresponds to nth 4k page in buffer
    mov word ptr [emm3.srcofs+2],es
    mov word ptr [emm3.srcofs],di
    mov eax,[es:edi]
    mov esi,[es:edi+4]
    ;@dprintf "Copying level-%d PT to %lX from %04X:%04X - starts with %08lX%08lX", cx,ebx,es,di,esi,eax
    pop es

    push ecx
    .if cx == 2
        mov ecx,20h   ; PDPTE is only 32 bytes
    .else
        mov ecx,1000h ; copy full page
    .endif

    mov si,offset emm3
    call copy2x

    pop ecx
    pop esi
    pop edi
    pop eax
    ret
CopyOutTable endp

EnableUnreal proc
    .if [PTbufseg] != -1
;--- if a PT buffer is in use, it means Unreal is unavailable
        stc
        ret
    .endif
    cli
    lgdt [GDTR]
    mov eax,cr0
    or al,1
    mov cr0,eax
    jmp @F
@@:
    push SEL_FLATDS
    pop es
    and al,0FEh
    mov cr0,eax
    jmp @F
@@:
    sti
    xor ax,ax ; clears carry
    mov es,ax
    ret
EnableUnreal endp

FindXmsHdl proc
;--- take physical addx in edi and fill in emm3 struct with the corresponding
;--- XMS handle and offset (or best guess thereat)
    push ebx
    push ebp
    push si
    push es
    mov es,[PTbufseg]

    xor ebp,ebp
    not ebp     ; track the smallest offset we find - so begin with FFFFFFFFh
    mov si,xmsidx
    shl si,3	;sizeof MEMBLK = 8
    .while si
;--- get EMB bases from EMS page
        mov ebx,dword ptr [es:PTbuflayout.embadrs-8+si]
        neg ebx
        add ebx,edi
        jnc @F  ; edi would be a negative offset in this block, so not right
        cmp ebx,ebp
        jnb @F  ; not the smallest offset we've found
;--- this is the handle with the smallest offset for edi that we've found,
;--- so it's a candidate for the handle that contains edi
        mov ebp,ebx
        mov [emm3.dstofs],ebx
        mov bx,[si+xmshdltab-sizeof MEMBLK].MEMBLK.wHdl
        mov [emm3.dsthdl],bx
        ;@dprintf "FindXmsHdl(): (%d/8)th hdl %X candidate for addr %lX (offs=%lX)", si, bx, edi, ebp
@@:
        sub si,sizeof MEMBLK
    .endw

    pop es
    pop si
    pop ebp
    pop ebx
    ret
FindXmsHdl endp


;--- switch to real-mode

switch2rm proc
    mov ax,SEL_DATA16
    mov fs,ax
    mov gs,ax
;--- disable paging & protected-mode
    mov eax,cr0
    and eax,7ffffffeh
    mov cr0, eax
    jmp far16 ptr @F
@@:
    mov eax,cr4
    and al,0DFh             ; reset bit 5, disable PAE paging
    mov cr4,eax

    .if inVCPImode
        mov eax,cs:[vcpiCR3]
        mov cr3,eax
        mov eax,cr0
        or eax,80000001h    ; re-enable paging with VCPI-provided settings
        mov cr0,eax
        jmp @F
@@:
        push SEL_FLATDS
        pop ds

        push ebx
        mov ebx,esp

;--- setup segments - make a tiny model...
        movzx eax,cs:[wStkBot+2]
        push eax ; GS
        push eax ; FS
        push eax ; DS
        push eax ; ES
        push eax ; SS
        push ebx ; ESP
        sub esp,4 ; EFLAGS
        push eax ; CS
        pushd offset @F ; EIP

        mov ax, 0DE0Ch ; switch to VM86
        call cs:vcpiadr
@@:
        pop ebx
    .else
        lidt cs:[nullidt]   ; IDTR=real-mode compatible values
    .endif
    ret
switch2rm endp

;--- switch to protected-mode

switch2pm proc
    .if inVCPImode
        mov ax,0DE0Ch
        push esi
        push cs:[wStkBot]
        mov cs:[wStkBot],sp
        mov esi,cs
        shl esi,4
        add esi,offset vcpiCR3
        mov cs:[vcpiEIP],offset @F
        int 67h
@@:
        mov ax, SEL_DATA16
        ;mov ds, ax
        ;mov es, ax
        mov ss, ax
        mov sp, ss:[wStkBot]
        pop ss:[wStkBot]
        pop esi
        mov eax,cr0
        and eax,7FFFFFFFh ; turn off paging while we reconfigure it
        mov cr0,eax
    .else
        lgdt cs:[GDTR]
        @lidt cs:[IDTR]
    .endif
    mov eax,cr4
    or ax,220h          ; enable PAE (bit 5) and OSFXSR (bit 9)
    mov cr4,eax
    mov eax,cs:pPageDir
    mov cr3,eax
;--- enable protected-mode + paging - also clear TS bit?
    mov eax,cr0
    or eax,80000001h
    mov cr0,eax
    ret
switch2pm endp

@popd macro x
	db 66h
	pop x
endm

;--- call real-mode thru DPMI functions ax=300h/301h.
;--- interrupts disabled, stack switched to 16-bit.
;--- SP-> CS:EIP, RMCS.

call_rmode proc

    call switch2rm
;    pop dword ptr cs:retad  ;not really needed. this proc is called by int31 only!
;    add sp,4   ;skip CS (must be flat cs)
    popad
    pop cs:wFlags
    pop es
    pop ds
    pop fs
    pop gs
    pop cs:[dwCSIP]
    lss sp,[esp]

	cmp ax,4B00h
	jnz @F
	int 3
@@:

    push cs:wFlags   ;make an IRET frame
    call cs:[dwCSIP]
    lss sp,dword ptr cs:wStkBot
    push gs
    push fs
    push ds
    push es
    pushf
    cli
    pushad
    movzx esi,cs:[wStkBot+2]
    shl esi,4
    movzx esp,sp
    add esi,esp
    call switch2pm
    lss esp,fword ptr cs:[dwESP]

    pushf
    btr word ptr [esp],14	;clear NT in eflags
    popf

    @popd gs
    @popd fs
    @popd es
    @popd ds
    mov edi,[esp]
    cld
    mov ecx,42/2
    rep movsw [edi],[esi]
    popad
    iretd

call_rmode endp

setints proc
    push 0
    pop es
if ?MPIC ne 8
    mov cx,8
    mov bx,?MPIC*4
    mov di,offset storedIntM
@@:
    mov eax, es:[bx]
    mov [di], eax
    add bx,4
    add di,4
    loop @B
endif
if ?SPIC ne 70h
    mov cx,8
    mov bx,?SPIC*4
    mov di,offset storedIntS
@@:
    mov eax, es:[bx]
    mov [di], eax
    add bx,4
    add di,4
    loop @B
endif
    push ds
    push es
    pop ds
if ?MPIC ne 8
    mov cx,8
    mov si,8*4
    mov di,?MPIC*4
    rep movsd
endif
if ?SPIC ne 70h
    mov cx,8
    mov si,70h*4
    mov di,?SPIC*4
    rep movsd
endif
    pop ds
    ret

setints endp

restoreints proc
    cld
    push 0
    pop es
if ?MPIC ne 8
    mov cx,8
    mov di,?MPIC*4
    mov si,offset storedIntM
    rep movsd
endif
if ?SPIC ne 70h
    mov cx,8
    mov di,?SPIC*4
    mov si,offset storedIntS
    rep movsd
endif
    ret
restoreints endp

setpic proc

;--- change IRQ 0-7 to ?MPIC
if ?MPIC ne 8
    mov al,10001b       ; ICW1: initialization
    out 20h,al
    mov al,dl           ; ICW2: IRQ 0-7: interrupts ?MPIC-?MPIC+7
    out 21h,al
    mov al,100b         ; ICW3: slave connected to IRQ2
    out 21h,al
    mov al,1            ; ICW4: Intel environment, manual EOI
    out 21h,al
    mov al,bPICM
    out 21h,al
endif
;--- change IRQ 8-F to ?SPIC
if ?SPIC ne 70h
    mov al,10001b       ; ICW1: initialization
    out 0A0h,al
    mov al,dh           ; ICW2: IRQ 8-15: interrupts ?SPIC-?SPIC+7
    out 0A1h,al
    mov al,2            ; ICW3:
    out 0A1h,al
    mov al,1            ; ICW4: Intel environment, manual EOI
    out 0A1h,al
    mov al,bPICS
    out 0A1h,al
endif
    .if inVCPImode
        push bx
        push cx
        movzx bx,dl
        movzx cx,dh
        mov ax,0DE0Bh
        call doint67
        pop cx
        pop bx
    .endif
    ret
setpic endp

;--- int 67h wrapper that just calls it in VM86 mode
;--- but goes through int 31h 0300h in protected mode
;--- might have been easier to put an int 67h gate in our IDT,
;--- but then what if someone installs something else over it...

doint67 proc
    push cs
    cmp word ptr [esp],_TEXT ;in VM86 mode?
    lea esp,[esp+2]          ;pop cs
    jne @F
    int 67h
    ret

@@:
    sub esp,14h
    pushad
    xor ecx,ecx
    mov [esp].RMCS.rFlags, 0202h
    mov dword ptr [esp].RMCS.rES,ecx
    mov dword ptr [esp].RMCS.rFS,ecx
    mov [esp].RMCS.rSSSP, ecx
    mov edi,esp
    mov bx,67h
    mov ax,0300h
    int 31h
    popad
    lea esp,[esp+14h]
    ret
doint67 endp

;--- switch back to real-mode and exit

backtoreal proc

    cli
    mov ax,SEL_DATA16   ; set SS, DS and ES to 16bit, 64k data
    mov ds,ax
    mov es,ax
    mov ss,ax
    movzx esp,wStkBot
    call switch2rm

@@exit2::
    mov ax,cs
    mov ds,ax           ; DS=DGROUP
    lss sp,dword ptr wStkBot

;--- restore PIC and IVT
    mov dx,vPIC
    cmp dx,7008h
    jne @@exit
    call setpic
    call restoreints

@@exit::
    sti
    mov bx,fhandle
    cmp bx,-1
    jz @F
    mov ah,3Eh
    int 21h
@@:
    mov dx,emshdl
    cmp dx,-1
    jz @F
    mov ah,45h          ;deallocate pages
    int 67h
@@:
    mov si,xmsidx
    shl si,3	;sizeof MEMBLK = 8
    .while si
        mov dx,[si+xmshdltab-sizeof MEMBLK].MEMBLK.wHdl
        mov ah,0dh          ;unlock handle
        call xmsaddr
        mov ah,0ah          ;free EMB
        call xmsaddr
        sub si,sizeof MEMBLK
    .endw
    cmp xmsaddr,0
    jz @F
    mov ah,6            ;local disable A20
    call xmsaddr
    mov ah,11h          ;release UMB
    mov dx,[PTbufseg]
    call xmsaddr
@@:
    mov ax,4c00h
    int 21h
backtoreal endp

;--- copy cx bytes to extended memory

copy2x proc
    mov [si].EMM._size,ecx
    push ecx
    push bx
    mov ah,0bh
    call xmsaddr
    pop bx
    pop ecx
    add [si].EMM.dstofs,ecx
    .if ax == 1
        clc
    .else
        stc
    .endif
    ret
copy2x endp

copy2x_ordie proc
    call copy2x
    mov bp, CStr("error copying to extended memory")
    jc @error
    ret
copy2x_ordie endp

;--- read a section and copy it to extended memory
;--- DI = 4 kB buffer
;--- BX = file handle
;--- sechdr = current section

readsection proc
    mov ax,4201h
    xor cx,cx
    xor dx,dx
    int 21h
    push dx
    push ax

    mov emm2.srchdl, 0
    mov word ptr emm2.srcofs+0, di
    mov word ptr emm2.srcofs+2, ds
    mov ax,xms1.wHdl
    mov emm2.dsthdl, ax
    mov eax, sechdr.VirtualAddress
    add eax, adjust
    mov emm2.dstofs, eax

    mov eax, sechdr.PointerToRawData
    push eax
    pop dx
    pop cx
    mov ax,4200h
    int 21h
    mov esi, sechdr.SizeOfRawData
    .while esi
        mov ecx,esi
        cmp ecx,1000h
        jb @F
        mov cx,1000h
@@:
        mov dx,di
        mov ah,3Fh
        int 21h
        cmp ax,cx
        mov bp, CStr("cannot read section data")
        jnz @error
        sub esi, ecx
        push si
        mov si,offset emm2
        call copy2x_ordie
        pop si
    .endw
    pop dx
    pop cx
    mov ax,4200h
    int 21h
    ret
readsection endp

savestate:
    push eax
    push ecx
    mov ax,[esp+8]
    push ds
    push es
    push fs
    push gs
    mov ecx, esp
    push SEL_DATA16
    pop ss
    mov sp,cs:[wStkBot]
    push ax
    jmp switch2rm

reststate_iretd:
    cli
    call switch2pm
    mov ax,SEL_FLATDS
    mov ss,ax
    mov esp,ecx
    pop gs
    pop fs
    pop es
    pop ds
    pop ecx
    pop eax
    add esp,2
    iretd

if ?IRQ0TORM
clock16 proc
    call savestate
    int 08h
    jmp reststate_iretd
clock16 endp
endif

if ?IRQ1TORM
keyboard16 proc
    call savestate
    int 09h
    jmp reststate_iretd
keyboard16 endp
endif

ife ?DIRVIO	;use BIOS for debug output
int4116 proc far
    cli
    mov bx,ax
    call savestate
    mov ax,bx
    xor bx,bx
    mov ah,0Eh
    cmp al,10
    jnz @F
    mov al,13
    int 10h
    mov ax,0E0Ah
@@:
    int 10h
reststate_retd::
    cli
    call switch2pm	;modifies flags!
    mov ax,SEL_FLATDS
    mov ss,ax
    mov esp,ecx
    pop gs
    pop fs
    pop es
    pop ds
    pop ecx
    pop eax
    add esp,2
    retd
int4116 endp
endif

if ?I31504
;--- allocate address space:
;--- EBX: linear address (or 0)
;--- ECX: size in bytes
;--- DX: attributes (bit 0 must be zero [=uncommitted])
;--- ebx must be page aligned

int31_50416 proc far

local linad:dword
local dwSize:dword
local adjLinAddr:byte
local handle:word

    push cs
    pop ds
    @dprintf "int31_504( ebx=%lX ecx=%lX dx=%X ) enter", ebx, ecx, dx
    test bx,0fffh
    jnz error
    cmp ebx,0
    setz adjLinAddr
    jnz @F
    mov ebx,nxtLinAddr
@@:

;--- alloc EMB for page tables
;--- ecx=200000h -> 200h PTEs -> 1 PT
    mov edx,ecx
    mov dwSize,ecx

    mov eax,ebx
    add eax,ecx
    jz @F
    jb error
@@:

    mov linad, ebx

    shr edx,21

    mov eax,edx
    shr eax,9
    add edx, eax

    inc edx		;number of PTs
    inc edx		;number of PTs

    inc edx		;add 1 for page alignment
    shl edx,2   ;convert 4k to 1k
    mov esi,edx
    mov ah,89h
    call xmsaddr
    cmp ax,1
    jnz error
    mov di,xmsidx
    shl di,3
    add di,offset xmshdltab
    mov [di].MEMBLK.wHdl,dx
    mov [di].MEMBLK.dwAddr,ebx
    mov handle,di
    inc xmsidx
    mov ah,0Ch      ;lock EMB 
    call xmsaddr
    cmp ax,1
    jnz error
    push dx
    push bx
    pop edi
    .if ([PTbufseg] != 0) && ([PTbufseg] != -1)
        push es
        mov es,[PTbufseg]
        movzx ebx,xmsidx
        mov dword ptr [es:PTbuflayout.embadrs-8+ebx*8],edi
        pop es
    .endif
    push edi
    @dprintf "int31_504() clear region edi=%lX, esi=%lX kB", edi, esi
    call ClearRegion    ;clear region edi, size esi kb
    pop eax
    jc error
    add eax,1000h-1
    ;and ax,0F000h
    shr eax,12
    mov esi, dwSize
    add esi, 1000h-1
    shr esi,12
    invoke CreateAddrSpace, linad, esi, eax
    jc error
    .if adjLinAddr
        shl esi,12
        add nxtLinAddr, esi
    .endif
    mov ebx,linad
    movzx esi,handle
    clc
    ret
error:
    stc
    ret
int31_50416 endp
endif

if ?I31518

;--- map physical memory
;--- esi=handle
;--- ebx=offset in block
;--- ecx=pages
;--- edi:edx=64-bit physical address

int31_51816 proc far
	push cs
	pop ds
	mov ax,si
	sub ax,offset xmshdltab
	jc error
	test al,7
	jnz error
	shr ax,3
	cmp ax,xmsidx
	jae error
	test bx,0fffh
	jnz error
	add ebx,[si].MEMBLK.dwAddr

;--- todo: check if ebx and ebx+ecx are within block

	shl edi,20
	shr edx,12
	or edx, edi
	invoke MapPages, ebx, ecx, edx
	clc
	ret
error:
    stc
    ret
int31_51816 endp
endif

;--- set/reset PIC from application program

int31_a0016 proc far
    cli
    mov dx,[cs:vPIC]
    cmp bl,0	;restore to standard?
    jz @F
;--- only change the vectors if nobody else changed them (in VCPI mode)
    .if dh == 70h
        mov dh,?SPIC
    .endif
    .if dl == 8
        mov dl,?MPIC
    .endif
@@:
    call setpic
    iretd
int31_a0016 endp

if 0
;--- handle base relocations

HandleRelocs proc
    mov edi, PhysBase
    mov esi, nthdr.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC*sizeof IMAGE_DATA_DIRECTORY].VirtualAddress
    mov ecx, nthdr.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC*sizeof IMAGE_DATA_DIRECTORY].Size_
    mov edx, edi
    sub edx, dword ptr nthdr.OptionalHeader.ImageBase
    add esi, edi    ;RVA->linear
    add ecx, esi    ;ecx=end of relocs (linear)
nextpage:
    cmp esi, ecx
    jnc reloc_done
    push ecx
    @lodsd              ;get RVA of page
    mov ebx, eax
    add ebx, edi        ;convert RVA to linear address
    @lodsd
    lea ecx, [esi+eax-8];ecx=end of relocs for this page
    xor eax, eax
nextreloc:
    @lodsw
    test ah,0F0h        ;must be < 1000h (size of a page)
    jz ignreloc
    and ah,0Fh
    add [eax+ebx], edx
ignreloc:
    cmp esi, ecx
    jb nextreloc
    pop ecx
    jmp nextpage
reloc_done:
    ret
HandleRelocs endp

endif

ifdef _DEBUG
    include dprintf.inc
endif

_TEXT ends

@jmp16 macro ofs
    db 66h,0eah
    dw offset ofs
    dw SEL_CODE16
endm
@call16 macro ofs
    db 09ah
    dd offset ofs
    dw SEL_CODE16
endm

_TEXT32 segment use32 para public 'CODE'

    assume ds:nothing, es:nothing, ss:nothing

start32 proc

    call esi
    mov ah,4Ch
    int 21h

start32 endp

;--- debug output helpers ( int 41h )

if ?DIRVIO	; direct video output - don't use BIOS

?VIOCOLS equ ?PG0ADDR+44Ah
?VIOPBEG equ ?PG0ADDR+44Eh
?VIOCSR  equ ?PG0ADDR+450h
?VIOPG   equ ?PG0ADDR+462h
?VIOCRT  equ ?PG0ADDR+463h
?VIOROWS equ ?PG0ADDR+484h

;--- set text mode cursor
set_cursor proc
    pushad
    push ds
    push SEL_FLATDS
    pop ds
    MOVZX esi, BYTE PTR ds:[?VIOPG]
    MOVZX ecx, BYTE PTR ds:[esi*2+?VIOCSR+1] ;get cursor pos ROW
    MOVZX eax, WORD PTR ds:[?VIOCOLS]
    MUL ecx
    MOVZX edx, BYTE PTR ds:[esi*2+?VIOCSR+0] ;get cursor pos COL
    ADD eax, edx
    movzx ecx,word ptr ds:[?VIOPBEG]
    shr ecx,1
    add ecx, eax
    mov dx,ds:[?VIOCRT]
    mov ah,ch
    mov al,0Eh
    out dx,ax
    inc al
    mov ah,cl
    out dx,ax
    pop ds
    popad
    ret
set_cursor endp

;--- scroll screen up one line
;--- ebp = linear address start of last line

scroll_screen proc
    CLD
    push esi
    mov edi,ebp
    movzx eax,word ptr ds:[?VIOCOLS]
    push eax
    MOV CL, ds:[?VIOROWS]
    lea esi, [edi+2*eax]
    mul cl
    mov ecx,eax
    rep movsw
    pop ecx
    mov ax,0720h
    rep stosw
    pop esi
    ret
scroll_screen endp

;--- interprets 10 (line feed)

WriteChr proc
    push ds
    push es
    pushad
    push SEL_FLATDS
    pop ds
    push ds
    pop es
    MOV edi,0B8000h
    CMP BYTE ptr ds:[?VIOCRT],0B4h
    JNZ @F
    XOR DI,DI
@@:
    movzx ebx, WORD PTR ds:[?VIOPBEG]   ;page start
    ADD edi, ebx
    MOVZX ebx, BYTE PTR ds:[?VIOPG]
    mov ebp, edi
    MOVZX ecx, BYTE PTR ds:[ebx*2+?VIOCSR+1] ;ROW
    MOVZX eax, WORD PTR ds:[?VIOCOLS]
    MUL ecx
    MOVZX edx, BYTE PTR ds:[ebx*2+?VIOCSR+0] ;COL
    ADD eax, edx
    MOV DH,CL
    LEA edi, [edi+eax*2]
    MOV AL, [esp+1Ch]
    CMP AL, 13
    JZ skipchar
    CMP AL, 10
    JZ newline
    MOV [edi], AL
    MOV byte ptr [edi+1], 07
    INC DL
    CMP DL, BYTE PTR ds:[?VIOCOLS]
    JB @F
newline:
    MOV DL, 00
    INC DH
    CMP DH, BYTE PTR ds:[?VIOROWS]
    JBE @F
    DEC DH
    CALL scroll_screen
@@:
    MOV ds:[ebx*2+?VIOCSR],DX
skipchar:
    popad
    pop es
    pop ds
    RET
WriteChr endp

else

WriteChr proc

    push ebx
    @call16 int4116
    pop ebx
    ret

WriteChr endp

endif

WriteStrng proc
    cld
@@:
    lodsb
    and al,al
    jz @F
    mov dl,al
    xor ax,ax
    int 41h
    jmp @B
@@:
    ret
WriteStrng endp

int41 proc
    cmp ax,0
    jnz @F
    mov al,dl
    call WriteChr
    iretd
@@:
    cmp ax,2
    jnz @F
    call WriteStrng
@@:
    iretd
int41 endp

WriteDW:
    push eax
    shr eax,16
    call WriteW
    pop eax
WriteW:
    push eax
    shr eax,8
    call WriteB
    pop eax
WriteB:     ;write Byte in al
    push eax
    shr eax,4
    call WriteNb
    pop eax
WriteNb:
    and al,0Fh
    add al,'0'
    cmp al,'9'
    jbe @F
    add al,7
@@:
    mov dl,al
    xor ax,ax
    int 41h
    ret

;--- default exception handler

;--- define string in _DATA, not CONST
CStr32 macro text:vararg
local sym
_DATA segment
sym db text,0
_DATA ends
    exitm <offset sym>
endm

exception:
excno = 0
    repeat 32
    push excno
    jmp @F
    excno = excno+1
    endm
@@:
;--- did we get here via a task gate?
    mov eax,cr0
    btr eax,3   ; TS flag
    mov cr0,eax ; reset TS before switching back to RM/VM86
    setc ch     ; but remember if it was the case!

    push SEL_DATA16
    pop ds
    xor esi,esi
    mov si, CStr32(lf,"Exception ")
    mov ax,2
    int 41h
    pop eax
    .if ch
;--- now reorder the stack if indeed we did come from a task gate
        pop ebx ; save errcode...
        push ds:[maintss.TSS_EFL]
        push ds:[maintss.TSS_CS]
        push ds:[maintss.TSS_EIP]
        push ebx
    .endif
    push eax
    call WriteB
if 1
    mov si, CStr32(" base=")
    mov ax,2
    int 41h
    mov eax, _TEXT32
    shl eax,4
    call WriteDW
endif
    mov si, CStr32(" errcode=")
    mov ax,2
    int 41h
    mov eax,[esp+4]
    call WriteDW
    mov si, CStr32(" cs:eip=")
    mov ax,2
    int 41h
    mov ax,[esp+12]
    call WriteW
    mov dl,':'
    xor ax,ax
    int 41h
    mov eax,[esp+8]
    call WriteDW
if 0
    mov si, CStr32(" edi=")
    mov ax,2
    int 41h
    mov eax, edi
    call WriteDW
endif
if 1
    cmp byte ptr [esp],0Eh
    jnz @F
    mov si, CStr32(" cr2=")
    mov ax,2
    int 41h
    mov eax,cr2
    call WriteDW
@@:
endif
if 1
    cmp byte ptr [esp],0Ah
    jnz @F ; TSS error
    test ch,ch
    jnz @F ; or task-switched
    mov si, CStr32(" esp=")
    mov ax,2
    int 41h
    lea eax,[esp+16]
    call WriteDW
    mov si, CStr32(" [esp]=")
    mov ax,2
    int 41h
    mov eax,[esp+16]
    call WriteDW
    mov al,' '
    call WriteChr
    mov eax,[esp+20]
    call WriteDW
    mov al,' '
    call WriteChr
    mov eax,[esp+24]
    call WriteDW
    mov al,' '
    call WriteChr
    mov eax,[esp+28]
    call WriteDW
@@:
endif
    mov dl,lf
    xor ax,ax
    int 41h
    .if ch
;--- clear NT before we die, if we came from a task gate
        pushf
        btr word ptr [esp],14
        popf
    .endif
    mov ax,4cffh
    int 21h
    align 4

;--- clock and keyboard interrupts

clock:
ife ?IRQ0TORM
    push ds
    push SEL_FLATDS
    pop ds
    inc dword ptr ds:[46Ch]
    pop ds
else
;--- route irq 0 (clock) to real-mode
    @jmp16 clock16
endif
Irq0007:
    push eax
Irq0007_1:
    mov al,20h
    out 20h,al
    pop eax
swint:
    iretd
Irq080F:
    push eax
    mov al,20h
    out 0A0h,al
    jmp Irq0007_1
keyboard:
ife ?IRQ1TORM
    push eax
    in al,60h
    pop eax
    jmp Irq0007
else
;--- route irq 1 (kbd) to real-mode
    @jmp16 keyboard16
endif
    align 4

;--- simple int 21h handler.
;--- handles ah=4Ch
;--- any other DOS function is transfered to real-mode

int21 proc
    cmp ah,4Ch
    jz int21_4c
    and byte ptr [esp+2*4],0FEh ;clear carry flag
    sub esp,14h
    pushad
    xor ecx,ecx
    mov [esp].RMCS.rFlags, 0202h
    mov dword ptr [esp].RMCS.rES,ecx
    mov dword ptr [esp].RMCS.rFS,ecx
    mov [esp].RMCS.rSSSP, ecx
    mov edi,esp
    mov bx,21h
    mov ax,0300h
    int 31h
    jc int21_carry
    mov al,byte ptr [esp].RMCS.rFlags
    mov byte ptr [esp+34h+2*4],al    ;set CF,ZF,...
    jmp @F
int21_carry:
    or  byte ptr [esp+34h+2*4],1    ;set carry flag
@@:
    popad
    lea esp,[esp+14h]
    iretd
int21_4c:
    @jmp16 backtoreal
int21 endp

int31 proc
    and byte ptr [esp+2*4],0FEh	;clear carry flag
    cmp ax,0300h	;simulate real-mode interrupt?
    jz int31_300
if ?I31301
    cmp ax,0301h	;call real-mode far proc with RETF frame?
    jz int31_301
endif
    cmp ax,0202h	;get exception vector?
    jz int31_202
    cmp ax,0203h	;set exception vector?
    jz int31_203
    cmp ax,0204h	;get interrupt vector?
    jz int31_204
    cmp ax,0205h	;set interrupt vector?
    jz int31_205
if ?I31504
    cmp ax,0504h	;allocate uncommitted memory 
    jz int31_504
endif
if ?I31518
    cmp ax,0518h	;allocate uncommitted memory 
    jz int31_518
endif
    cmp ax,0A00h	;vendor specifc API
    jz int31_a00
ret_with_carry:
    or byte ptr [esp+2*4],1 ;set carry flag
    iretd
    align 4
int31_300:
int31_301:
    pushad
    push ds
    push es
    push fs
    push gs
    mov esi,edi

al_i3130x textequ <byte ptr [esp+11*4]>

    mov ax,SEL_DATA16
    mov es,ax
    assume es:DGROUP

;--- the contents of the RMCS has to be copied
;--- to conventional memory. We use the DGROUP stack

    movzx ebx,bl
if ?I31301
    mov eax,[esi].RMCS.rCSIP
    cmp al_i3130x,0
    jnz @F
endif
    mov eax,[ebx*4 + ?PG0ADDR]
@@:
    mov bx,[wStkBot] 
    sub bx,34h
    mov edi,ebx
    mov [dwESP],esp
    cli
    cld
    movsd   ;copy 2Ah bytes (8 * std regs,flags,es,ds,fs,gs)
    movsd
    movsd
    movsd
    movsd
    movsd
    movsd
    movsd
    movsd
    movsd
    movsw
    add esi,4   ;skip CS:IP in source RMCS
    stosd       ;save CS:IP

    lodsd       ;load SS:SP
    and eax,eax ;is a real-mode stack set?
    jnz @F
    mov eax,dword ptr [wStkBot] ;if no, use the default stack
@@:
    stosd
    mov ax,SEL_DATA16
    mov ss,eax
    mov esp,ebx         ;clear highword ESP, ESP is used inside call_rmode
    assume es:nothing
    @jmp16 call_rmode

int31_203:
    cmp bl,20h
    jae ret_with_carry
int31_205:
    push eax
if ?I31SETTG
    .if bl==8
        push ds
        push SEL_DATA16
        pop ds
        assume ds:DGROUP
        mov [dflttss.TSS_EIP],edx
        mov word ptr [dflttss.TSS_CS],cx
;--- handler may IRET with NT, in which case we need the right PDBR in main TSS
        mov eax,cr3
        mov [maintss.TSS_CR3],eax
        pop ds
        assume ds:nothing
    .else
endif
    push edi

    movzx edi,bl
    lea edi,[edi*8+?IDTADDR]

    mov eax,edx
    cld

    stosw           ;lowword offset
    mov ax,cx
    stosw           ;CS
if ?I31SETTG
    add edi,2
else
;--- use these two lines to ensure #DF becomes an interrupt gate, if so desired
    mov ax,8E00h
    stosw
endif
    shr eax,16
    stosw           ;hiword offset

    pop edi
if ?I31SETTG
    .endif
endif
    pop eax
    iretd
int31_202:	;get exception vector BL in CX:EDX
    cmp bl,20h
    jae ret_with_carry
int31_204:	;get interrupt vector BL in CX:EDX
if ?I31SETTG
    .if bl==8
        push ds
        push SEL_DATA16
        pop ds
        assume ds:DGROUP
        mov edx,[dflttss.TSS_EIP]
        mov ecx,[dflttss.TSS_CS]
        pop ds
        assume ds:nothing
    .else
endif
    movzx ecx,bl
    lea ecx,[ecx*8+?IDTADDR]
    mov dx,[ecx+6]
    shl edx,16
    mov dx,[ecx+0]
    mov cx,[ecx+2]
if ?I31SETTG
    .endif
endif
    iretd

if ?I31504
int31_504:
;--- create a RMCS onto stack
	sub esp,2	;adjustment to make stack dword aligned
	push 0
	pushw DGROUP
	pushw offset int31_50416
	push 0
	push 0
	pushw 3202h
	pushad
	push edi
	lea edi,[esp+4]
	mov ax,301h
	int 31h
	pop edi
	jc @F
	test [esp].RMCS.rFlags,1
	jnz @F
	mov ebx,[esp].RMCS.rEBX
	mov esi,[esp].RMCS.rESI
	add esp,34h
	iretd
@@:
	add esp,34h
	jmp ret_with_carry
endif

if ?I31518
int31_518:
;--- create a RMCS onto stack
	sub esp,2	;adjustment to make stack dword aligned
	push 0
	pushw DGROUP
	pushw offset int31_51816
	push 0
	push 0
	pushw 3202h
	pushad
	push edi
	lea edi,[esp+4]
	mov ax,301h
	int 31h
	pop edi
	jc @F
	test [esp].RMCS.rFlags,1
	jnz @F
	add esp,34h
	iretd
@@:
	add esp,34h
	jmp ret_with_carry
endif

int31_a00:
	@jmp16 int31_a0016

int31 endp

_TEXT32 ends

    end start16
