
;--- sample demonstrating the use of the added dosext.obj module
;--- reads file c:\config.sys into buffer,
;--- then writes buffer contents to stdout.

    .386
    .model flat, stdcall
    option casemap:none
    option proc:private

lf  equ 10

;--- define a string
CStr macro text:vararg
local sym
    .const
sym db text,0
    .code
    exitm <offset sym>
endm

    include dpmi.inc

_InitExtender proto c

    .data

szFile db "c:\config.sys",0

    .data?

buff db 10000h dup (?)

    .code

    include printf.inc

main proc c

local dwSize:dword
local hFile:dword

    mov hFile,-1

;--- init DOS extender.
;--- after successful initialization, DOS functions
;--- 3Fh (read file), 40h (write file) and (71)6Ch (open file)
;--- are directly supported by int 21h.
    call _InitExtender
    .if !eax
        invoke printf, CStr("extender init error",lf)
        jmp exit
    .endif

;--- open file (LFN version)
    mov esi, offset szFile
    mov bx,3040h
    mov cx,0
    mov dx,1
    mov di,0
    mov ax,716ch
    int 21h
    .if CARRY?
        invoke printf, CStr("cannot open file '%s'",lf), esi
        jmp exit
    .endif
    mov ebx, eax
    mov hFile, eax

;--- read file
    mov ecx,sizeof buff
    mov edx,offset buff
    mov ax,3F00h
    int 21h
    .if CARRY?
        invoke printf, CStr("cannot read file '%s', handle=%X",lf), addr szFile, ebx
        jmp exit
    .endif
    mov dwSize, eax

    invoke printf, CStr("read %u bytes from file '%s'",lf), dwSize, addr szFile

;--- write file to stdout
    mov bx,1
    mov ecx,dwSize
    mov edx,offset buff
    mov ax,4000h
    int 21h

exit:
    .if hFile != -1
        ;--- close file
        mov ebx, hFile
        mov ah,3Eh
        int 21h
    .endif
    ret
main endp

    end main
