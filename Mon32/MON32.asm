
;--- this is a simple monitor program loaded by dos32pae.bin

    .386
    .model flat
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

    .data?

dwEsp   dd ?    ;stack pointer
address dd ?

;--- keyboard buffer
buffer db 20 dup (?)
lbuffer equ $ - offset buffer

    .code

    include printf.inc

;--- this is the entry point.
;--- it's called by the stub with registers:
;--- ebx: image base
;--- esp: bottom of (reserved) stack

main proc c

    invoke printf, CStr("Mon32 loaded at %lX, esp=%lX",lf), ebx, esp
    call set_exception_handlers
    invoke printf, CStr("cs=%X ss=%X ds=%X es=%X fs=%X gs=%x",lf), cs, ss, ds, es, fs, gs
nextcmd:
    invoke printf, CStr("(cmds: a,d,q): ")
ifdef ?WAITINPM ;wait in protected-mode ( to test interrupts )
@@:
    mov ax,flat:[41ah]
    cmp ax,flat:[41ch]
    jnz @F
    hlt
    jmp @B
@@:
endif
    mov ah,1        ;read a key from keyboard with echo
    int 21h
    lea ecx,[nextcmd]
    push ecx
    mov dwEsp, esp
    push eax
    invoke printf, CStr(lf)
    pop eax
    cmp al,'a'
    jz a_cmd
    cmp al,'d'
    jz d_cmd
    cmp al,'q'
    jz q_cmd
    cmp al,0dh      ;ENTER?
    jz newline
    mov ecx, eax
    invoke printf, CStr("unknown cmd: %c",lf), ecx
newline:
    ret
main endp

;--- get a line of characters
;--- ebx->buffer
;--- esi=size of buffer

getline proc
    xor edi, edi
    dec esi
nextkey:
    mov ah,1
    int 21h
    cmp al,0dh
    jz enter_pressed
    cmp al,08h
    jz backspace_pressed
    mov [ebx+edi],al
    inc edi
    cmp edi,esi
    jnz nextkey
    mov dl,0dh
    mov ah,2
    int 21h
    jmp enter_pressed
backspace_pressed:
    mov dl,20h
    mov ah,2
    int 21h
    cmp edi,0
    jz nextkey
    dec edi
    mov dl,08h
    mov ah,2
    int 21h
    jmp nextkey
enter_pressed:    
    mov byte ptr [ebx+edi],0
    ret
getline endp

;--- enter address (for d cmd)

a_cmd proc
    invoke printf, CStr("enter start address for d cmd: ")
    mov esi, lbuffer
    lea ebx, buffer
    call getline
    and edi,edi        ;at least 1 digit entered?
    jz done
    xor edi,edi
    xor esi,esi
    .while byte ptr [ebx+edi]
        mov al,byte ptr [ebx+edi]
        sub al,'0'
        jc error
        cmp al,9
        jbe @F
        sub al, 27h
        cmp al, 0fh
        ja error
@@:
        movzx eax,al
        shl esi,4
        add esi,eax
        inc edi
    .endw
    mov [address],esi
done:
    invoke printf, CStr(lf)
    ret
error:
    invoke printf, CStr(lf,"%s?",lf), addr buffer
    ret

a_cmd endp

;--- display memory dump

d_cmd proc
    mov edi,[address]
    mov ecx,8
nextline:
    push ecx
    invoke printf, CStr("%08lX: "), edi
    mov cl,16
    .while cl
        push ecx
        mov dl,[edi]
        movzx edx,dl
        invoke printf, CStr("%02X "), edx
        pop ecx
        inc edi
        dec cl
    .endw
    mov dl,' '
    mov ah,2
    int 21h
    sub edi,16
    mov cl,16
    .while cl
        mov dl,[edi]
        cmp dl,20h
        jnc @F
        mov dl,'.'
@@:
        mov ah,2
        int 21h
        inc edi
        dec cl
    .endw
    invoke printf, CStr(lf)
    pop ecx
    loop nextline
    mov [address],edi
    ret
d_cmd endp

;--- 'q': back to real-mode

q_cmd proc
    mov ax,4c00h
    int 21h
q_cmd endp

;--- set exception 0D+0E handlers so we 
;--- won't terminate unexpectedly

set_exception_handlers proc

    mov ecx,cs
    lea edx, exception0D
    mov bl,0Dh
    mov ax,0203h
    int 31h
    mov ecx,cs
    lea edx, exception0E
    mov bl,0Eh
    mov ax,0203h
    int 31h
    ret

set_exception_handlers endp

;--- handle protection and page faults

exception0D:
    sti
    mov edx,[esp+0]
    mov ecx,[esp+4]
    mov eax,[esp+8]
    invoke printf, CStr(lf,"protection fault, errcode=%X cs:eip=%X:%lX",lf), edx, eax, ecx
    mov esp, dwEsp
    ret
exception0E:
    sti
    mov edx,[esp+0]
    mov ecx,[esp+4]
    mov eax,[esp+8]
    mov ebx,cr2
    invoke printf, CStr(lf,"page fault, errcode=%X cs:eip=%X:%lX cr2=%lX",lf), edx, eax, ecx, ebx
    mov esp, dwEsp
    ret

    end main
