; FORTH


;[BITS 32]

; when it's 64 bit, this will be 8
%define RWIDTH 	4
%define TRU	0xFFFFFFFF

; tmp stack
T_STACK 	dd 0		; stack ptr
T_RTACK		dd 0		; rstack ptr

times 4096 dd 0
N_STACK	dd 0xba1ba1		; max 1024 elements in stack
times 4096 dd 0
N_RTACK	dd 0xba1ba1


; misc buffers
wbuff2 times 1600 db 0
ebuff2 times 1024 db ' '
xbuff2 times 1024 db 0
ybuff2 times 1023 db 'y'
db 0
zbuff2 times 1023 db 'z'
db 0
fatbuff times 1024 db 0

; word buffer
db 0
B_WORD		times 64 db 0	; last word
B_CNT		dw 0		; key counter
B_IDX		dw 0		; start of word in buffer

interpret_lit	dd 0		; is lit?

; kludges
ch_semi 	db "semi", 0
ch_tick 	db "tick", 0
ch_comma 	db "comma", 0



; vars
;V_BASE		db 10		; base for numbers
;V_LATEST	db 0		; latest dictionary entry
;V_HERE		db 0		; curr pos
;V_STATE	db 0		; curr state 0= compile 1= interpret
V_ZERO		dd 0		; misc zero word
F_IMMED 	equ 0x80	; flag immediate word
F_HIDDEN 	equ 0x20	; flag hidden word
F_INTERP	equ 0x10	; docol def NEVER PASSED
F_LENMASK	equ 0x1f	; flag length mask
%define LINK 	0	 	; current link
%define OLDLINK 0		; old link


; #################################
; 	macros
; #################################


; NEXT word
%macro NEXT 0
	lodsd
	jmp [eax]
%endmacro


%macro PS 1
	sub dword [T_STACK], 4
	mov ebp, [T_STACK]
	mov dword [ebp], %1
%endmacro

%macro PP 1
	mov ebp, [T_STACK]
	mov %1, dword [ebp]
	add dword [T_STACK], 4
%endmacro

; push return stack
%macro PSR 1
	sub dword [T_RTACK], 4
	mov ebp, [T_RTACK]
	mov dword [ebp], %1
%endmacro

; pop return stack
%macro PPR 1
	mov ebp, [T_RTACK]
	mov %1, dword [ebp]
	add dword [T_RTACK], 4
%endmacro

; interpreter

DOCOL:
	PSR esi
	mov esi, eax
	add esi, 4
	NEXT
	
; name, len, label, flags
%macro defword 3
	align 4
	%defstr name %1
	%strlen nlen name
	%undef OLDLINK
	%xdefine OLDLINK LINK
	%undef LINK
	%xdefine LINK name_%2
	name_%2:
		dd OLDLINK
		db %3 + nlen	; flags + name length
		db name
		times (32-nlen) db 0
	%2: dd DOCOL
%endmacro


; name, label, flags
%macro defcode 3
	align 4
	%defstr name %1
	%strlen nlen name
	%undef OLDLINK
	%xdefine OLDLINK LINK
	%undef LINK
	%xdefine LINK name_%2
	name_%2:
		dd OLDLINK
		db %3 + nlen	; flags + name length
		db name
		times (32-nlen) db 0
	%2:
		dd code_%2
	code_%2:
%endmacro


; (pushes the address onto stack) name, label, flags, value
%macro defvar 4
	defcode %1, %2, %3
	PS V_%2
	NEXT
	V_%2: dd %4
%endmacro

; (pushes the value onto stack) name, label, flags, value
%macro defconst 4
	defcode %1, %2, %3
	PS %4
	NEXT
%endmacro


; #################################
; 	variables and constants
; #################################

defconst version, version, 0, 1
      
; The address of the top of the return stack.
defconst R0, RZ, 0, V_ZERO
  
; Pointer to DOCOL.
defconst DOCOL, __DOCOL, 0, DOCOL
   
; The IMMEDIATE flag's actual value.
defconst F_IMMED, __F_IMMED, 0, F_IMMED

; The HIDDEN flag's actual value.
defconst F_HIDDEN, __F_HIDDEN, 0, F_HIDDEN

; interpreted word
defconst F_INTERP, __F_INTERP, 0, F_INTERP
  
;The length mask in the flags/len byte.
defconst F_LENMASK, __F_LENMASK, 0, F_LENMASK

defconst T, TRUE, 0, 0xFFFFFFFF

NILV dd NILV, NILV
defconst NIL, NIL, 0, NILV


defconst vga, VGA_ADDR, 0, vga_framebuff

; variables

defvar STATE, STATE, 0, 0		; 1 compiling, 0 executing
defvar LATEST, LATEST, 0, 0
defvar HERE, HERE, 0, 0
defvar BASE, BASE, 0, 10
defvar W_ADDR, W_ADDR, 0, 0		; address of the current reading buffer
defvar W_LATEST, W_LATEST, 0, 0		; last end of word
defvar B_LATEST, B_LATEST, 0, 0
defvar SCL, SCL, 0, 4			; base 3 scale
defvar CBLK, CBLK, 0, 0			; current block



defcode exit, exit, 0
	PPR esi
	NEXT
	
; compilation only, fetches the next dword, skips and pushes it on stack
defcode lit, lit, 0
	lodsd
	PS eax
	NEXT
	
defcode drop, drop, 0
	PP eax
	NEXT

defcode swap, swap, 0
	PP edx
	PP ebx
	PS edx
	PS ebx
	NEXT

defcode dup, dup, 0
	PP edx
	PS edx
	PS edx
	NEXT

defcode over, over, 0
	mov eax, [T_STACK]
	mov edx, [eax+4]
	PS edx
	NEXT

defcode over1, over1, 0
	mov eax, [T_STACK]
	mov edx, [eax+8]
	PS edx
	NEXT	
	
defcode 2over, twover, 0
	mov eax, [T_STACK]
	mov edx, [eax+8]
	PS edx
	NEXT

defcode rot, rot, 0
	PP eax
	PP ebx
	PP ecx
	PS eax
	PS ecx
	PS ebx
	NEXT

defcode -rot, nrot, 0
	PP eax
	PP ebx
	PP ecx
	PS ebx
	PS eax
	PS ecx
	NEXT
		
defcode 2drop, ddrop, 0
	PP eax 
	PP eax
	NEXT

defcode 2dup, ddup, 0
	mov ecx, [T_STACK]
	mov eax, [ecx]
	mov ebx, [ecx+4]
	PS ebx
	PS eax
	NEXT

defcode 2swap, dswap, 0
	PP eax
	PP ebx
	PP ecx
	PP edx
	PS ebx
	PS eax
	PS edx
	PS ecx
	NEXT

defcode dup?, qdup, 0			
	mov ecx, [T_STACK]
	mov eax, [ecx]
	test eax, eax
	jz .1 
	PS eax
.1: 	NEXT

defcode nth, nth, 0
	PP eax
	imul eax, 4
	mov ecx, [T_STACK]
	mov ebx, [ecx+eax]
	PS ebx
	NEXT

; return stack	 ---------------------------------------

; push to return stack
defcode >r, toret, 0
	PP eax
	PSR eax
	NEXT
	
; pops from return stack
defcode r>, retto, 0
	PPR eax
	PS eax
	NEXT
	
defcode r@, rfetch, 0
	mov eax, dword [T_RTACK]
	PS eax
	NEXT
	
; rsp fetch 1st
defcode I, rfetch1, 0			
	mov eax, [T_RTACK]
	mov ebx, [eax]
	PS ebx
	NEXT
	
defcode N, rfetch2, 0
	mov eax, [T_RTACK]
	mov ebx, [eax+4]
	PS ebx
	NEXT
	
defcode J, rfetch3, 0
	mov eax, [T_RTACK]
	mov ebx, [eax+8]
	PS ebx
	NEXT
	
defcode K, rfetch4, 0
	mov eax, [T_RTACK]
	mov ebx, [eax+12]
	PS ebx
	NEXT
	
defcode X, rfetch5, 0
	mov eax, [T_RTACK]
	mov ebx, [eax+16]
	PS ebx
	NEXT
	
defcode Y, rfetch6, 0
	mov eax, [T_RTACK]
	mov ebx, [eax+20]
	PS ebx
	NEXT

defcode rswap, rswap, 0
	mov eax, [T_RTACK]
	mov ebx, [eax]
	mov ecx, [eax+4]
	mov [eax], ecx
	mov [eax+4], ebx
	NEXT
	
defcode I+, Iplus, 0
	mov eax, [T_RTACK]
	inc dword [eax]
	NEXT

defcode I-, Isubs, 0
	mov eax, [T_RTACK]
	dec dword [eax]
	NEXT
	
defcode N+, Nplus, 0
	mov eax, [T_RTACK]
	inc dword [eax+4]
	NEXT
	
defcode N-, Nsubs, 0
	mov eax, [T_RTACK]
	dec dword [eax+4]
	NEXT
	
defcode J+, Jplus, 0
	mov eax, [T_RTACK]
	inc dword [eax+8]
	NEXT

defcode K+, Kplus, 0
	mov eax, [T_RTACK]
	inc dword [eax+12]
	NEXT


; rsp store				
defcode r!, rstore, 0
	PP eax
	mov [T_RTACK], eax
	NEXT

defcode rdrop, rdrop, 0
	add dword [T_RTACK], 4
	NEXT

; data stack	 ---------------------------------------	

; stack address
defcode d@, dspfetch, 0
	mov eax, [T_STACK]
	PS eax
	NEXT

; store stack address (for context changes)
defcode d!, dspstore, 0	
	PP eax
	mov [T_STACK], eax
	NEXT
	
; shift right
defcode >>, shr, 0				; UNTESTED
	PP ecx
	PP eax
	shr eax, cl
	PS eax
	NEXT

; shift left		
defcode <<, shl, 0				; UNTESTED
	PP ecx
	PP eax
	shl eax, cl
	PS eax
	NEXT

defcode exec, execute, 0
	PP eax
	jmp [eax]
	NEXT
	


; memory access	 ---------------------------------------

defcode 0!, zstore, 0
	PP eax
	mov dword [eax], 0
	NEXT

; (val addr -- )
defcode !, store, 0
	PP ebx
	PP eax
	mov [ebx], eax
	NEXT

defcode @, fetch, 0
	PP ebx
	mov eax, [ebx]
	PS eax
	NEXT

defcode +!, addstore, 0
	PP ebx
	PP eax
	add [ebx], eax
	NEXT
	
defcode -!, substore, 0
        PP ebx       
        PP eax       
        sub [ebx], eax   
        NEXT

defcode l!, storebyte_l, 0
        PP ebx       
        PP eax       
        mov [ebx], al    
        NEXT

defcode l@, fetchbyte_l, 0
        PP ebx       
        xor eax, eax
        mov al, [ebx]    
        PS eax      
        NEXT
	
defcode h!, storebyte_h, 0
        PP ebx       
        PP eax       
        mov [ebx], ah    
        NEXT

defcode h@, fetchbyte_h, 0
        PP ebx       
        xor eax, eax
        mov ah, [ebx]    
        PS eax      
        NEXT

defcode w!, storeword, 0
        PP ebx       
        PP eax       
        mov [ebx], ax    
        NEXT

defcode w@, fetchword, 0
        PP ebx       
        xor eax, eax
        mov ax, [ebx]    
        PS eax      
        NEXT
	
; 1st 4 bits
defcode x4, rx4, 0
	xor eax, eax
	xor ecx, ecx
	PP edx
	shl edx, 28 
	mov ecx, 4
.l	shl edx, 1
	rcl eax, 1
	loop .l
	PS eax
	NEXT
	
defcode xl, rxl, 0
	xor eax, eax
	PP ecx
	mov al, cl
	PS eax
	NEXT
	
defcode xh, rxh, 0
	xor eax, eax
	PP ecx
	mov al, ch
	PS eax
	NEXT
	
defcode xx, rxw, 0
	xor eax, eax
	PP ecx
	mov ax, cx
	PS eax
	NEXT

defcode xxh, rxxh, 0
	PP ecx
	shr ecx, 16
	PS ecx
	NEXT
	

; function: cmove
;   Block copy.
; Stack:
;   &s &d n --
; Params:
;   &s - Source Address
;   &d - Destination Address
;   n  - Number of bytes to copy
defcode cmove, cmove, 0				; UNTESTED
	push esi     
        PP ecx       
        PP esi       
        PP edi       
        rep movsb      
	pop esi
        NEXT

; copies dwords edi esi ecx -- 	
defcode dmove, dmove, 0
	push esi     
        PP ecx       
        PP esi       
        PP edi       
        rep movsd      
	pop esi
        NEXT

	
; lists	 	---------------------------------------

defcode car, car, 0
	PP ecx
	mov eax, [ecx]
	PS eax
	NEXT
	
defcode cdr, cdr, 0
	PP ecx
	mov eax, [ecx+4]
	PS eax
	NEXT
	
; compiler	---------------------------------------

; (addr len -- faddr)
defcode find, find, 0
	PP ecx		; len
	PP edi		; addr
	call _find
	PS eax		; pointer
	NEXT
	
_find:
	push esi
	mov edx, V_LATEST
	
	; kludges (doesnt find properly if in "")
	cmp cl, 1
	jne .fi1			; dont even bother since its not a single byte
	
	cmp byte [edi], ';'
	jne .0
	mov ecx, 4
	mov edi, ch_semi
	
.0	cmp byte [edi], 39
	jne .1
	mov ecx, 4
	mov edi, ch_tick
	
.1	cmp byte [edi], ','
	jne .fi1
	mov ecx, 5
	mov edi, ch_comma
	

.fi1	test edx, edx			; null ptr?
	je .f404	
	xor eax, eax
	mov al, [edx + RWIDTH]
	and al, F_HIDDEN|F_LENMASK	; al is length
	cmp al, cl			; same len?
	jne .next

	; compare strings
	push ecx			; save length
	push edi			; save addr
	lea esi,[edx+5]			; dictionary pointer
	rep cmpsb
	pop edi
	pop ecx
	jne .next
	
	; ok strings =
	pop esi
	mov eax, edx
	ret
	
	; loop
.next 	mov edx, [edx]
	jmp .fi1
	
.f404	pop esi				; not found
	xor eax, eax
	ret
	

; Code Field Address (addr -- cfa)
defcode >cfa, tcfa, 0
	PP edi
	call _tcfa
	PS edi
	NEXT
_tcfa:
	add edi, 37
ret
	
	
; DFA(after docol) (addr -- dfa)
defword >dfa, tdfa, 0
	dd tcfa, incr4, exit
	

; starts word compiling (addr len -- )
defcode create, create, 0
	PP ecx
	PP ebx
	mov edx, 32		; tcfa padding
	sub edx, ecx
	
	; link new word
	mov edi, [V_HERE]	; address of the header
	mov eax, [V_LATEST]	; link ptr
	stosd			; store it

	; length byte and word string
	mov al, cl		; store length
	stosb
	push esi
	mov esi, ebx
	rep movsb		; store string
	pop esi

	add edi, edx		; offset
	
	; update latest and here
	mov eax, [V_HERE]
	mov [V_LATEST], eax
	mov [V_HERE], edi	
NEXT

; appends address to HERE and adds 4(compiles?)
defcode comma, comma, 0
	PP eax
	call _comma
	NEXT
_comma:
	mov edi, [V_HERE]
	stosd
	mov [V_HERE], edi 
ret	


	
; goes into compile mode
defcode [, lbrac, F_IMMED
	mov [V_STATE], dword 0
	NEXT

; goes to interp mode
defcode ], rbrac, 0
	mov [V_STATE], dword 1
	NEXT
	
; colon
defword : , colon, 0
	dd bword, create, __DOCOL, comma
	dd LATEST, fetch, hidden
	dd rbrac	; goes into compile mode
	dd exit


; map to semicolon
defword semi, semicolon, F_IMMED
	dd lit, exit, comma
	dd LATEST, fetch, hidden
	dd lbrac	; back to immediate mode
	dd exit
	
; makes the word immediate
defcode IMMED, immediate, 0
	mov edi, [V_LATEST]
	add edi, 4		; point to the len flag
	xor [edi], byte F_IMMED	; make it immediate
	NEXT

; hides the word (addr -- )
defcode hidden, hidden, 0
	PP edi	
	add edi, RWIDTH
	xor [edi], byte F_HIDDEN
	NEXT
	
; hides the word after this
defword hide, hide, 0
	dd code_bword
	dd code_find
	dd code_hidden
	dd exit

defword tick, tick, 0
	dd bword, find, tcfa
	dd exit
	
defword [comp], wcompile, F_IMMED
	dd bword, find, tcfa, comma, exit
	
; prints the name of a >cfa
defcode nme, nme, 0
	PP ebx
	sub ebx, 32	; points to string
	PS ebx
	NEXT


defcode branch, branch, 0
	add esi, [esi]
	NEXT

; zero branch
defcode 0branch, zbranch, 0
	PP eax
	test eax, eax
	jz code_branch
	add esi, 4
NEXT

; zero branch
defcode 1branch, obranch, 0
	PP eax
	test eax, eax
	jnz code_branch
	add esi, 4
NEXT

defcode skip, skip, 0
	add esi, 4
	NEXT


defword if, if, F_IMMED
	dd lit, zbranch, comma, HERE, fetch, lit, 0, comma
	dd exit
	
defword else, else, F_IMMED
	dd lit, branch, comma, HERE, fetch, lit, 0 , comma
	dd swap, dup, HERE, fetch, swap, msub, swap, store
	dd exit
	
defword unless, unless, F_IMMED
	dd lit, obranch, comma, HERE, fetch, lit, 0, comma
	dd exit

defword then, then, F_IMMED
	dd dup, HERE, fetch, swap, msub, swap, store
	dd exit
	
defword begin, begin, F_IMMED
	dd HERE, fetch, exit
	
; begin until
defword until, until, F_IMMED
	dd lit, zbranch, comma, HERE, fetch, msub, comma, exit

defword again, again, F_IMMED
	dd lit, branch, comma, HERE, fetch, msub, comma, exit
	
	
; numbers	 ---------------------------------------


defcode 1+, incr, 0
	mov ecx, [T_STACK]
	inc dword [ecx]
	NEXT

defcode 1-, decr, 0
	mov ecx, [T_STACK]
	dec dword [ecx]
	NEXT

defcode 4+, incr4, 0
	mov ecx, [T_STACK]
	add dword [ecx], 4
	NEXT
	
defcode 4-, decr4, 0
	mov ecx, [T_STACK]
	sub dword [ecx], 4
	NEXT
	
defcode +, madd, 0
        PP eax     
	mov ecx, [T_STACK]
	add dword [ecx], eax
	NEXT	
	
defcode -, msub, 0
        PP eax     
	mov ecx, [T_STACK]
	sub dword [ecx], eax
	NEXT	

defcode *, mmul, 0
        PP eax
        PP ebx
	xor edx, edx
	cdq
        imul eax, ebx
        PS eax      
        NEXT
	
defcode /mod, divmod, 0
	xor edx, edx
	PP ebx
	PP eax
	cmp ebx, 0
	jne .cont
	mov eax, 0
	jmp .end
.cont	cdq
	idiv ebx
.end	PS edx
	PS eax
	NEXT
	
defword / , mdiv, 0
	dd divmod, swap, drop, exit
	
defword */ , mdivmul, 0
	dd rot, mmul, swap, mdiv, exit
	
defword % , mmod, 0
	dd divmod, drop, exit
	
; exponentiate num to (base exp -- res)
defcode exp, mexp, 0
	PP ecx
	PP eax
	call _exp
	PS eax
	NEXT
_exp:
	cmp ecx, 0
	jne .nex
	mov eax, 1
	ret
.nex	dec ecx		
	cmp ecx, 0
	jne .mul
	ret		; same number
.mul	imul eax, eax
	loop .mul
ret

	
defcode =, equ, 0
	PP eax
	PP ebx
	cmp eax, ebx
	sete al
	movzx eax, al
	PS eax
	NEXT
	
defcode <>, neq, 0
	PP eax
	PP ebx
	cmp eax, ebx
	setne al
	movzx eax, al
	PS eax
	NEXT

defcode <, lt, 0
	PP eax
	PP ebx
	cmp ebx, eax
	setl al
	movzx eax, al
	PS eax
	NEXT

defcode >, gt, 0
        PP eax
        PP ebx
        cmp ebx, eax
        setg al
        movzx eax, al
        PS eax
        NEXT

defcode <=, le, 0
        PP eax
        PP ebx
        cmp ebx, eax
        setle al
        movzx eax, al
        PS eax
        NEXT

defcode >=, ge, 0
        PP eax
        PP ebx
        cmp ebx, eax
        setge al
        movzx eax, al
        PS eax
        NEXT

defcode 0=, zequ, 0
        PP eax
        test eax, eax
        setz al
        movzx eax, al
        PS eax
        NEXT	

defcode 0<>, znequ, 0
        PP eax
        test eax, eax
        setnz al
        movzx eax, al
        PS eax
        NEXT

defcode 0<, ltz, 0
        PP eax
        test eax, eax
        setl al
        movzx eax, al
        PS eax
        NEXT

defcode 0>, gtz, 0
        PP eax
        test eax, eax
        setg al
        movzx eax, al
        PS eax
        NEXT

defcode 0<=, lez, 0
        PP eax
        test eax, eax
        setle al
        movzx eax, al
        PS eax
        NEXT

defcode 0>=, gez, 0
        PP eax
        test eax, eax
        setge al
        movzx eax, al
        PS eax
        NEXT
	
defcode and, and, 0
	PP eax
	mov ecx, [T_STACK]
	and [ecx], eax
	NEXT

defcode or, or, 0
	PP eax
	mov ecx, [T_STACK]
	or [ecx], eax
	NEXT

defcode xor, xor, 0
	PP eax
	mov ecx, [T_STACK]
	xor [ecx], eax
	NEXT
	
defcode not, not, 0
	mov ecx, [T_STACK]
	not dword [ecx]
	NEXT
	
defcode nneg, nneg, 0
	mov ecx, [T_STACK]
	neg dword [ecx]
	NEXT
	
; makes eax nearer to ebx by ecx (minimize) (n1 n2 n+- -- )
defcode aprox, approximate, 0
	PP ecx
	PP ebx
	PP eax
	cmp eax, ebx
	je .end
	cmp eax, ebx
	jg .sub
	add eax, ecx
	jmp .end
.sub	sub eax, ecx
.end	PS eax
	NEXT
	

; IO	 ---------------------------------------

; buffer key
defcode lkey, lkey, 0
	xor eax, eax
	call kybrd_get_last_key
	cmp ax, KEY_UNKNOWN
	jne .ok
	PS 0
	jmp .end
.ok	PS eax
.end	NEXT
	
defcode keydis, keydis, 0
	call kybrd_discard_last_key
	NEXT
	
; buffer word ( -- addr len)
defcode word, bword, 0	
	call _bword
	NEXT

_bword:
	xor ecx, ecx
	mov eax, [V_W_LATEST]
	cmp eax, 0
	jne .cont
	mov eax, [V_W_ADDR]
	mov [V_W_LATEST], eax
.cont	call nchr
	call nspc
	call _storeword
	add eax, ecx
	mov [V_W_LATEST], eax
	cmp ecx, 0			; eof
	jne .end
	PP ecx
	PP ecx
	jmp _bword
.end 	ret

; assume we are in a space
nchr:
	cmp byte [eax], 0
	je _wordNIL
	cmp byte [eax], 32
	jg .end
	inc eax
	jmp nchr
.end 	ret

nspc:	
	cmp byte [eax+ecx], 0
	je _wordNIL
.n	cmp byte [eax+ecx], 32
	jle .end
	inc ecx
	jmp nspc
.end 	ret
	


_storeword:
	push esi
	PS eax
.nex	mov esi, eax
	mov edi, B_WORD
	push ecx
	rep movsb
	pop ecx
	mov byte [B_WORD+ecx], 0
.n	PS ecx
	pop esi
	ret
	
; end of file
_wordNIL:
	mov byte [B_WORD], 'n'
	mov byte [B_WORD+1], 'i'
	mov byte [B_WORD+2], 'l'
	mov byte [B_WORD+3], 0
	PS B_WORD
	PS 0xFFFFFFFF			; NIL and T have max length
	mov eax, [V_W_ADDR]
	mov [V_W_LATEST], eax
	pop edx				; skips return and back to main loop
.end 	ret



defcode emit, emit, 0
	PP ebx
	call stdio_put_ch
	NEXT
	
; (addr len -- num errors)
defcode number, number, 0
	PP ecx
	PP edi
	call _num
	PS eax				; parsed num
	PS ecx				; error count
	NEXT

dotpos dd 0	; position	
inilen dd 0	; initial length
_num:
	mov dword [inilen], 0
	mov dword [dotpos], 0
	xor eax, eax
	xor ebx, ebx	
	test ecx, ecx			; zero length num?
	je .nu5	
	
	mov dword [inilen], ecx		; store the length 
	
	mov edx, [V_BASE]
	mov bl, [edi] 			; 1st char
	inc edi
	push eax			; 0 on stack
	cmp bl, '-'			; negative?
	jnz .nu2
	pop eax	
	push ebx			; sign on stack
	dec ecx				; dec length
	jnz .nu1
	pop ebx				; string is only '-'.
	mov ecx, 1
	ret

	; read loop
.nu1:	imul eax, edx			; eax *= base
	mov bl, [edi]			; next char
	inc edi	

	cmp bl, '.'
	jne .nu2
	mov [dotpos], ecx
	mov bl, [edi]
	inc edi
	dec ecx
	
	; parse 0-9, a-z to number 0-35
.nu2:	sub bl,'0'			; < '0'?
	jb .nu4
	cmp bl,10			; <= '9'?
	jb .nu3
	sub bl,17			; < 'A'? (17 is 'A'-'0')
	jb .nu4
	add bl,10	

.nu3:	cmp bl, dl			; > base?
	jge .nu4
	add eax, ebx
	dec ecx
	jnz .nu1			; loop

	; negate if 1st char was -
.nu4:	pop ebx
	test ebx, ebx
	jz .nu5
	neg eax
	
.nu5:	cmp byte [dotpos], 0
	je .end
	call numdec
.end 	ret	

numdec2:
	push eax
	mov eax, [dotpos]
	call stdio_put_dec
	call space
	mov eax, [inilen]
	call stdio_put_dec
	call space
	pop eax
ret
nmsg db "in num dec", 0
numdec:
	push ecx
	mov ecx, [inilen]
	mov ebx, [dotpos]
	sub ecx, ebx		; actual dotpos
	mov ebx, [inilen]
	sub ebx, ecx
	mov ecx, [V_SCL]
	inc ecx
	sub ecx, ebx
	cmp ecx, 0
	jle .neg
.lp	imul eax, 10
	loop .lp
.end	pop ecx
	ret
.neg	push edx
	xor edx, edx
	neg ecx
	mov ebx, 10
.nlp 	idiv ebx
	loop .nlp
	pop edx
	jmp .end

scl_msg db "out of scale", 0
sclerr_msg db "scale error 3long5me", 0


; strings	---------------------------------------


; stores it in HERE ( -- addr len)
defcode mkstr , mkstr, F_IMMED
	push edi
	push esi
	mov edi, [V_HERE]
	mov esi, [V_W_LATEST]
	;inc esi			; initial space
	xor ecx, ecx
	PS edi
	movsb
.lp	movsb
	inc ecx
	cmp byte [esi], '"'
	jne .lp
	;movsb			; end loop	
	inc esi
	xor eax, eax
	stosb			; close string
	add edi, 4		; padding 1
	add ecx, 4
	mov [V_HERE], edi
	mov [V_W_LATEST], esi
	pop esi
	pop edi
	;PS ecx
	NEXT

defword [str], compstr, F_IMMED
	dd lit, branch, comma, HERE, fetch, lit, 0, comma
	dd mkstr, toret
	dd HERE, fetch, over, msub, swap, store
	dd retto, lit, lit, comma, comma
	dd exit

; unsigned print
defcode U. , Udot, 0
	PP eax
	call stdio_put_dec
	NEXT
	
defcode C. , bytedot, 0
	PP edx
	call stdio_put_hex8
	NEXT
	
defcode W. , worddot, 0
	PP edx
	call stdio_put_hex16
	NEXT

defcode D. , dworddot, 0
	PP edx
	call stdio_put_hex
	NEXT
	
defcode B.old , bindotold, 0
	PP eax
	call stdio_put_bits
	clc
	NEXT



; prints a zero terminated string
defcode prn, prn, 0
	PP ebx
	call stdio_puts
	NEXT

; (addr len -- ) prints len characters	
defcode prnc, prnc, 0
	PP ecx
	PP edx
	cmp ecx, 0
	je .end
.prn	mov bl, [edx]
	inc edx
	call stdio_put_ch
	loop .prn
.end	NEXT

	
; test

defcode pp, pp, 0
	PS 0xdead777
	NEXT

; gets the immed flag (addr -- flag)
defcode flag, flag, 0
	PP eax
	add eax, 4
	xor ebx, ebx
	mov bl, [eax]
	and bl, F_IMMED
	PS ebx
	NEXT
	
defword fflag, fflag, 0
	dd bword, find, flag, exit

	
defword nex, nex, 0
	dd incr4, dup, fetch, exit

defword p, pname, 0
	dd nme, prn, exit
	
defcode nop, nop, 0
	NEXT
	

	
; #################################
; 	start
; #################################	


defcode quit, quit, 0
	dd branch, -(RWIDTH*2)	; loop indefinitely
	

; debug

; single word interpreter
defcode interpret, interp, 0
	call _bword
terp:	PP ecx			; len
	PP edi			; addr
	call _isNIL
	jne .run
	mov dword [lastNIL], 1	; it was NIL so exit in the next zbranch
	NEXT
.run	mov dword [lastNIL], 0	; so we can exit the branch
	call _find
	cmp edx, 0
	je .num
	cmp edx, name_lit	; blaze it?
	jne .1
	mov byte [prev_lit], 2
.1	mov edi, edx
	cmp byte [V_STATE], 1
	je .comp
	jmp .interp
.num	call _num
	cmp ecx, 0		; no errors?
	jne .err
	PS eax
	cmp byte [V_STATE], 1
	jne .nl
	call comp_num
.nl	NEXT
.err	call space
	mov ebx, B_WORD
	call stdio_puts
	mov bl, '?'
	call stdio_put_ch
	call space
	mov edx, edi
	call stdio_put_hex
	mov dword [lastNIL], 1	; error takes you out of the loop
	PPR esi
	NEXT
.comp:
	mov eax, edi
	cmp byte [prev_lit], 0	; not lit?
	je .2
	dec byte [prev_lit]
	jmp .3			; lit so compile anyways
.2	mov al, [edi+4]		; immed?
	and al, F_IMMED
	jnz .interp
.3	call _tcfa
	mov eax, edi
	mov edi, [V_HERE]
	stosd
	mov [V_HERE], edi
	NEXT
.interp:			; this one differs in that it never returns
	call _tcfa		; so its up to the next word to return and call NEXT
	mov eax, edi
	jmp [eax]
	
; needs addr len
defcode eval, evalword, 0
	jmp terp
	NEXT
	
defcode !!, debug, 0
	mov eax, esi
	PSR eax
	jmp [ret_addr]
	NEXT
	
defcode next, next2, 0
	PPR esi
	NEXT
	
; ecx pit delay
defcode wait, _wait, 0
	PP ebx
	call pit_delayforth
	NEXT

defconst TICKS, pticks, 0, pit_ticksf

	
	
; compile number
comp_num:
	mov edi, [V_HERE]
	mov eax, lit
	stosd
	PP eax
	stosd
	mov [V_HERE], edi
ret

; is word nil?
defcode wNIL?, wnilq, 0
	PP ecx
	PP edi
	xor eax, eax
	call _isNIL
	sete al
	PS eax
	NEXT

lastNIL dd 0
defconst lastNIL?, lNILq, 0, lastNIL
	
; leaves the flag untouched so we can do shit afterwards	
_isNIL:
	push ecx
	push edi
	push esi
	mov esi, s_NIL
	;mov edi, B_WORD
	mov ecx, 3
	repz cmpsb
	pop esi
	pop edi
	pop ecx
ret

; runs forth word in eax
run_word :
mov esi, run_end
jmp [eax]



run_end:
pop ecx
ret






	
	


