
;--- simple printf implementation for DOS 32-bit
;--- supports:
;--- %x  : word (dword pushed)
;--- %lx : dword 
;--- %u  : word (dword pushed)
;--- %lu : dword 
;--- %d  : sword (dword pushed)
;--- %ld : sdword 
;--- %s  : near32 string out
;--- %c  : character (dword pushed)

	.386

;--- convert long to string
;--- ltoa( long n, char * s, base n );

ltoa PROC c uses ebx esi edi number:dword, tbuffer:ptr byte, base:dword

	mov eax, number
	mov ebx, tbuffer
	mov edi, base
	mov ch,0
	cmp edi,-10
	jne @F
	mov edi,10
	and eax,eax
	jns @F
	neg eax
	mov ch,'-'
@@:
	add ebx,10
	mov BYTE PTR [ebx],0
	dec ebx
@@nextdigit:
	xor edx, edx
	div edi
	add dl,'0'
	cmp dl,'9'
	jbe @F
	add dl,7+20h
@@:
	mov [ebx],dl
	dec ebx
	and eax, eax
	jne @@nextdigit
	cmp ch,0
	je @F
	mov [ebx],ch
	dec ebx
@@:
	inc ebx
	mov eax,ebx
	ret

ltoa ENDP

printf PROC c uses ebx esi edi fmt:ptr sbyte, args:VARARG

local flag:byte
local longarg:byte
local size_:dword
local fillchr:dword
local szTmp[12]:byte

	lea edi,args
@@L335:
	mov esi,fmt
nextchar:
	lodsb
	or al,al
	je done
	cmp al,'%'
	je formatitem
	push eax
	call handle_char
	jmp nextchar
done:
	xor eax,eax
	ret 

formatitem:
	push offset @@L335
	xor edx,edx
	mov [longarg],dl
	mov bl,1
	mov cl,' '
	cmp BYTE PTR [esi],'-'
	jne @F
	dec bl
	inc esi
@@:
	mov [flag],bl
	cmp BYTE PTR [esi],'0'
	jne @F
	mov cl,'0'
	inc esi
@@:
	mov [fillchr],ecx
	mov ebx,edx

	.while ( byte ptr [esi] >= '0' && byte ptr [esi] <= '9' )
		lodsb
		sub al,'0'
		movzx eax,al
		imul ecx,ebx,10		;ecx = ebx * 10
		add eax,ecx
		mov ebx,eax
	.endw

	mov [size_],ebx
	cmp BYTE PTR [esi],'l'
	jne @F
	mov [longarg],1
	inc esi
@@:
	lodsb
	mov [fmt],esi
	cmp al,'x'
	je handle_x
	cmp al,'X'
	je handle_x
	cmp al,'d'
	je handle_d
	cmp al,'u'
	je handle_u
	cmp al,'s'
	je handle_s
	cmp al,'c'
	je handle_c
	and al,al
	jnz @F
	pop eax
	jmp done
handle_c:
	mov eax,[edi]
	add edi, 4
@@:
	push eax
	call handle_char
	retn

handle_s:
	mov esi,[edi]
	add edi,4
	jmp print_string
handle_d:
handle_i:
	mov ebx,-10
	jmp @F
handle_u:
	mov ebx, 10
	jmp @F
handle_x:
	mov ebx, 16
@@:
	mov eax,[edi]
	add edi,4
	cmp [longarg],0
	jne @F
	movzx eax,ax	;unsigned short
@@:
	lea esi, szTmp
	invoke ltoa, eax, esi, ebx
	mov esi, eax

print_string:		;print string ESI, size EAX
	mov eax, esi
	.while byte ptr [esi]
		inc esi
	.endw
	sub esi, eax
	xchg eax, esi
	mov ebx,size_
	sub ebx,eax
	.if flag == 1
		.while sdword ptr ebx > 0
			push [fillchr]
			call handle_char	;print leading filler chars
			dec ebx
		.endw
	.endif

	.while byte ptr [esi]
		lodsb
		push eax
		call handle_char	;print char of string
	.endw

	.while sdword ptr ebx > 0
		push [fillchr]
		call handle_char	;print trailing spaces
		dec ebx
	.endw
	retn

handle_char:
	pop ecx
	pop edx
	cmp dl,10
	jnz @F
	mov dl,13
	mov ah,2
	int 21h
	mov dl,10
@@:
	mov ah,2
	int 21h
	jmp ecx

	align 4

printf ENDP


