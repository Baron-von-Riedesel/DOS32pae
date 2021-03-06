
;--- translate cmdline at psp:80h to argc & argv

DELIM MACRO target
    cmp al,13           ;; Test for end-of-line character
    je  target
    or  al,al           ;; Either CR or Null
    je  target
ENDM

    .code

;--- DS=ES=SS=flat

_setargv proc c

    pop esi
    mov ebp,esp
    sub esp,100h
    mov edi,esp
    mov byte ptr [edi+1Dh],51h      ;AH
    mov word ptr [edi+20h],3202h    ;Flags
    mov dword ptr [edi+2Eh],0       ;SS:SP
    mov bx,21h
    mov cx,0
    mov ax,0300h
    int 31h
    push esi
    movzx esi,word ptr [edi+10h]	;BX
    shl esi,4

    xor eax,eax

    push eax                ; save this address
    xor edi,edi             ; init argc
    xor edx,edx             ; init size of mem block
    add esi,81H
    jmp arg110

;--- EDI holds argc
;--- EDX holds block size (not including null terminators)
arg100:
    push ebx
arg110:
@@:
    lodsb
    cmp al,' '
    je @B
    cmp al,9
    je @B
    DELIM arg400            ; exit if eol
    inc edi                 ; another argument
    mov ebx,0               ; init argument size
	mov cx,0920h
	cmp al,'"'
	jnz @F
	mov cx,2222h
	jmp quotedstr
@@:
    dec esi                 ; back up to reload character
quotedstr:
    push esi                ; save start of argument
@@:
    lodsb
    cmp al,cl               ; end argument?
    je arg100
    cmp al,ch
    je arg100               ; white space terminates argument
    DELIM arg399            ; exit if eol
    inc ebx
    inc edx
    jmp @B
arg399:
    push ebx                ; last argument
arg400:
;--- address & size of arguments are pushed
    mov ecx,edi             ; Store number of arguments
    add edx,edi             ; add in terminator bytes
    inc edi                 ; add one for NULL pointer
    inc edi                 ; add one for filename
    shl edi,2               ; each pointer needs 4 bytes
    add edx,edi             ; add space for pointers to space for chars

    add edx,4-1
    and dl,0fch
    sub ebp,edx
    mov eax,ebp

;argc = [ebp-4]
;argv = [ebp-8]

    mov [ebp-4],ecx
    mov [ebp-8],eax
    mov edx,ecx
    add edi,eax             ; edi -> behind vector table (strings)
    lea ebx,[edi-4]
    mov dword ptr [ebx],0
    sub ebx,4
    cmp edx,0
    jz arg500
@@:
    pop ecx                 ; size
    pop esi                 ; address
    mov [ebx],edi
    sub ebx,4
    rep movsb
    xor al,al
    stosb
    dec edx
    jnz @B
arg500:
    pop eax
    mov [ebx],eax       ; store address filename as first item
    mov eax,[ebp-4]
    mov edx,[ebp-8]
    inc eax
    ret
_setargv endp

