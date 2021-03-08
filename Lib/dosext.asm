
;--- extend int 21h for file io

	.386
	.model flat
	option proc:private
	option casemap:none

	include dpmi.inc

?TLBSIZE equ 2000h

PUSHADS struct
rEDI	dd ?
rESI	dd ?
rEBP	dd ?
		dd ?
rEBX	dd ?
rEDX	dd ?
rECX	dd ?
rEAX	dd ?
PUSHADS ends

@dprintf macro fstr, args:vararg
local sym
ifdef _DEBUG
	ifnb <args>
		invoke printf, CStr(fstr), args
	else
		invoke printf, CStr(fstr)
	endif
endif
endm

	.data

pTLB dd -1
oldint21 df 0

RDWRS struct
regs PUSHADS <>
rmcs RMCS <>
	dw ?
RDWRS ends

	.code

ifdef _DEBUG
CStr macro text:vararg
local sym
	.const
sym db text,0
	.code
	exitm <offset sym>
endm
	include printf.inc
endif

memcpy proc stdcall uses esi edi dst:ptr, src:ptr, size_:dword
	mov edi,dst
	mov esi,src
	mov ecx,size_
	rep movsb
	ret
memcpy endp

strlen proc stdcall pStr:ptr
	mov eax,pStr
	.while byte ptr [eax]
		inc eax
	.endw
	sub eax, pStr
	ret
strlen endp

;*** 21h AH=3Fh -> read file BX, ECX bytes into DS:EDX

int213F proc
	sub esp,sizeof RMCS + 2
	pushad
	lea edi,[esp].RDWRS.rmcs
	mov [edi].RMCS.rFlags,3202h
	mov [edi].RMCS.rSSSP,0
	mov eax,pTLB
	shr eax,4
	mov [edi].RMCS.rDS,ax
	mov word ptr [edi].RMCS.rEDX,0
	mov [edi].RMCS.rEBX,ebx

	xor esi,esi 	;esi = count bytes read
	mov ebp, [esp].PUSHADS.rEDX ;ebp = destination
nextblock:
	mov word ptr [edi].RMCS.rEAX,3F00h
	mov ecx,[esp].PUSHADS.rECX
	sub ecx,esi
	cmp ecx,?TLBSIZE
	jb @F
	mov ecx,?TLBSIZE
@@:
	mov word ptr [edi].RMCS.rECX, cx
	mov bl,21h
	mov cx,0
	mov ax,0300h
	int 31h
	jc error
	test [edi].RMCS.rFlags,1
	stc
	jnz error

	movzx eax,word ptr [edi].RMCS.rEAX
	push eax
	invoke memcpy, ebp, pTLB, eax
	pop eax
	add ebp,eax
	add esi,eax

	cmp ax,?TLBSIZE		 ;if  AX <> BP we're done
	jz nextblock
	clc
	mov eax,esi
error:
	mov [esp].PUSHADS.rEAX,eax
	popad
	lea esp,[esp+sizeof RMCS+2]
	ret
	align 4
int213F endp

;*** 21h AH=40h -> write file BX, ECX bytes into DS:EDX

int2140 proc
	sub esp,sizeof RMCS + 2
	pushad
	lea edi,[esp].RDWRS.rmcs
	mov [edi].RMCS.rFlags,3202h
	mov [edi].RMCS.rSSSP,0
	mov eax,pTLB
	shr eax,4
	mov [edi].RMCS.rDS,ax
	mov word ptr [edi].RMCS.rEDX,0
	mov [edi].RMCS.rEBX,ebx

	xor esi,esi 	;esi = count bytes written
	mov ebp, [esp].PUSHADS.rEDX ;ebp = destination
nextblock:
	mov word ptr [edi].RMCS.rEAX,4000h
	mov ecx,[esp].PUSHADS.rECX
	sub ecx,esi
	cmp ecx,?TLBSIZE
	jb @F
	mov ecx,?TLBSIZE
@@:
	mov word ptr [edi].RMCS.rECX, cx

	invoke memcpy, pTLB, ebp, ecx

	mov bl,21h
	mov cx,0
	mov ax,0300h
	int 31h
	jc error
	test [edi].RMCS.rFlags,1
	stc
	jnz error

	add ebp,eax
	add esi,eax

	cmp ax,?TLBSIZE		 ;if  AX <> BP we're done
	jz nextblock
	clc
	mov eax,esi
error:
	mov [esp].PUSHADS.rEAX,eax
	popad
	lea esp,[esp+sizeof RMCS+2]
	ret
	align 4
int2140 endp

;--- 21h AH=6Ch and AX=716Ch -> open file
;--- DS:SI -> filename

int216C proc
int216C endp	;fall thru

int21716C proc
	sub esp,sizeof RMCS + 2 - sizeof PUSHADS
	pushad
	mov edi,esp
	mov [edi].RMCS.rFlags,3203h
	mov [edi].RMCS.rSSSP,0
	mov eax,pTLB
	shr eax,4
	mov [edi].RMCS.rDS,ax
	mov word ptr [edi].RMCS.rESI,0
	invoke strlen, esi
	inc eax
	invoke memcpy, pTLB, esi, eax
	mov bx,21h
	xor ecx,ecx
	mov ax,0300h
	int 31h
	jc error
	mov ax,word ptr [edi].RMCS.rEAX
	test [edi].RMCS.rFlags, 1
	stc
	jnz error
	clc
error:
	mov [edi].RMCS.rESI, esi
	mov word ptr [edi].RMCS.rEAX,ax
	popad
	lea esp,[esp + sizeof RMCS + 2 - sizeof PUSHADS]
	ret
	align 4

int21716C endp

EXECRM struct
environ dw ?
cmdline dd ?
fcb1	dd ?
fcb2	dd ?
res1	dd ?
res2	dd ?
EXECRM ends

EXECPM struct
cmdline dq ?
fcb1	dq ?
fcb2	dq ?
EXECPM ends

int214B00 proc

local rmcs:RMCS

	pushad
	@dprintf <"int214B enter, esp=%X",10>, esp
	lea edi,rmcs
	mov [edi].RMCS.rEAX,eax
	mov [edi].RMCS.rFlags,3202h
	mov [edi].RMCS.rSSSP,0
	mov eax,pTLB
	shr eax,4
	mov [edi].RMCS.rDS,ax
	mov [edi].RMCS.rES,ax
	mov word ptr [edi].RMCS.rEBX,0
	mov word ptr [edi].RMCS.rEDX,100h
	invoke strlen, edx
	inc eax
	mov esi,pTLB
	lea ecx,[esi+100h]
	invoke memcpy, ecx, edx, eax
	mov [esi].EXECRM.environ,0
	mov word ptr [esi].EXECRM.cmdline+0,80h
	mov ax,[edi].RMCS.rDS
	mov word ptr [esi].EXECRM.cmdline+2,ax
	mov word ptr [esi].EXECRM.fcb1+0,sizeof EXECRM
	mov word ptr [esi].EXECRM.fcb1+2,ax
	mov word ptr [esi].EXECRM.fcb2+0,sizeof EXECRM
	mov word ptr [esi].EXECRM.fcb2+2,ax

	xor ecx,ecx
	mov edx,dword ptr [ebx].EXECPM.fcb1
@@:
	mov eax,[edx+ecx*4]
	mov [esi+ecx*4+sizeof EXECRM],eax
	inc ecx
	cmp ecx,8
	jb @B

	lea ecx,[esi+80h]
	mov edx,dword ptr [ebx].EXECPM.cmdline
@@:
	mov al,[edx]
	mov [ecx],al
	inc ecx
	inc edx
	cmp al,13
	jnz @B

;--- restore PIC to standard setting
	cli
	mov bl,0
	mov ax,0A00h
	int 31h

	mov bx,21h
	xor ecx,ecx
	mov ax,0300h
	int 31h
	jc error
	mov ax,word ptr [edi].RMCS.rEAX
	test [edi].RMCS.rFlags, 1
	stc
	jnz error
	@dprintf <"int214B exit, esp=%X",10>, esp
	clc
error:
	mov word ptr [esp].PUSHADS.rEAX,ax

;--- restore PIC to dos32pae setting
	pushfd
	mov bl,1
	mov ax,0A00h
	int 31h
	popfd

	popad
	ret
	align 4

int214B00 endp

myint21 proc
	cmp ah,3Fh
	jz f3F
	cmp ah,40h
	jz f40
	cmp ah,6Ch
	jz f6C
	cmp ax,716Ch
	jz f716C
	cmp ax,4B00h
	jz f4B00
	jmp cs:[oldint21]
f3F:
	call int213F
	jmp done
f40:
	call int2140
	jmp done
f6C:
f716C:
	call int21716C
	jmp done
f4B00:
	call int214B00
	jmp done
done:
	push eax
	lahf
	mov [esp+3*4],ah
	pop eax
	iretd
myint21 endp


_InitExtender proc c public uses ebx edi

local rmcs:RMCS

;--- allocate TLB
	mov rmcs.rSSSP,0
	mov rmcs.rFlags,3202h
	mov word ptr rmcs.rEAX,4800h
	mov word ptr rmcs.rEBX,?TLBSIZE shr 4
	lea edi,rmcs
	mov bx,21h
	xor cx,cx
	mov ax,0300h
	int 31h
	jc error
	test rmcs.rFlags,1
	jnz error
	movzx eax,word ptr rmcs.rEAX
	shl eax, 4
	mov pTLB, eax
	@dprintf <"milestone 1",10>

;--- get interrupt vector 21h
	mov bl,21h
	mov ax,0204h
	int 31h
	jc error
	mov dword ptr oldint21+0,edx
	mov word ptr oldint21+4,cx
	@dprintf <"milestone 2",10>

;--- set interrupt vector 21h
	mov ecx,cs
	mov edx,offset myint21
	mov bl,21h
	mov ax,0205h
	int 31h
	jc error
	@dprintf <"milestone 3",10>
	mov eax,1
	ret
error:
	xor eax,eax
	ret
_InitExtender endp

	end

