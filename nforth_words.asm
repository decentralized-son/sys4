%include "gstdio.asm"
; %include "usb/usb.asm"


%define RAM_SIZE_LOC	0x6FF8
%define KERNEL_SIZE_LOC	0x6FFC

; misc strings
RAMTxt		db "RAM: ", 0
MbTxt		db " Mb", 0
HDTxt		db "HD: ", 0
GbTxt		db " Gb", 0
NoHDTxt		db "Couldn't detect", 0
KernelTxt	db "Kernel: ", 0
KbTxt		db " Kb", 0


defconst xb, xbuff, 0, xbuff2
defconst yb, ybuff, 0, ybuff2
defconst zb, zbuff, 0, zbuff2
defconst wb, wbuff, 0, wbuff2
defconst eb, ebuff, 0, ebuff2

defconst idt_err, idterr, 0, idt_lasterr 

; SYSTEM INFO 	------------------------------------------------

defcode sys-ram, sysram, 0
	; RAM  (calculated in Real Mode)
	mov ebx, RAMTxt
	call stdio_puts
	mov eax, DWORD [RAM_SIZE_LOC]
	mov ebx, 1024*1024			; get Mb
	xor edx, edx
	div ebx					; edx:eax / ebx  ==> Quotient in EAX, Remainder in EDX
	call space
	call stdio_put_dec
	mov ebx, MbTxt
	call stdio_puts
	NEXT

	
; character words	------------------------------------------------

defcode [CR], ch_newline, 0
	PS 0x0A
	NEXT

defcode [BL], ch_space, 0
	PS ' '
	NEXT
	
defcode CR, e_return, 0
	push ebx
	mov bl, 0x0A
	call stdio_put_ch
	pop ebx
	NEXT
	
defcode SPC, e_space, 0
	mov bl, ' '
	call stdio_put_ch
	NEXT
	
defcode CLR, e_clrscr, 0
	call stdio_clrscr
	NEXT
	
	

; memory and hdd 	------------------------------------------------

; (addr len -- ) dump bytes in hex
defcode dumpch, dumpchar, 0
	PP ecx
	PP edx
.lp	mov bl, [edx]
	call stdio_put_ch
	inc edx
	cmp byte [stdio_cur_x], 79
	jle .e
	mov word [stdio_cur_x], 0
	inc word [stdio_cur_y]
.e	loop .lp
	NEXT

; (addr len -- ) dump bytes in hex
defcode dump8, dumpbyte, 0
	PP ecx
	PP ebx
.lp	mov dh, [ebx]
	call stdio_put_hex8
	inc ebx
	loop .lp
	NEXT




; HDD stuff

; ata lba hd write (lbaLo sec addr -- )
; lowlevel hd write (memaddr sectcnt [lbaHI=0] lbaLO)
; (lbalo sec addr - f)
defcode hdwr, hdwrite, 0
	PP edi
	PP ecx
	PP eax
	call nhdd_write
	NEXT

	
; ata lba hd read (lbaLo sec addr -- )
; lowlevel hd read (memaddr sectcnt [lbaHI=0] lbaLO )
; (lbalo sec addr - f)
defcode hdrd, hdread, 0
	PP edi
	PP ecx
	PP eax
	call nhdd_read
	NEXT

	
; editor block and edit word
editor_mode dd 0
editor_cblk dd 0	
editor_tmpb dd 0				; misc use, temporal block
defconst EDIT?, EDITQ, 0, editor_mode
defconst BBLK, BBLK, 0, B_BLK
defconst BTMP, BTMP, 0, editor_tmpb

defconst bsze, bsze,0, 512
defconst ssze, ssze, 0, 1
	
	

defword rdblk, rdblk, 0
	dd ssze, wbuff, hdread
	dd exit
	
	
defword dumpprep, dumpprep, 0
	dd wbuff, bsze, e_clrscr
	dd exit
	
; (blknum -- )
defword list, list, 0
	dd rdblk, dumpprep, dumpchar
	dd exit
	
	
; (blknum -- ) switches to edit mode, block edit 
defword edit, edit, 0
	dd lit, 1, EDITQ, store		; changes mode
	dd dup, list
	; dd lit, 0, dup, xpos, ypos	; set cursor at beginning
	dd exit
	
; (addr -- ) finalizes the buffer by adding 0 at byte 1024
defword bfin, bfin, 0
	dd bsze, madd, lit, 0, swap, store 
	dd exit
	
; (blknum -- f) saves block
defword bsav, bsav, 0
	dd wbuff, bfin
	dd ssze, wbuff, hdwrite
	dd exit
	
; (blknum -- f)
defword bclr, bclr, 0
	dd ssze, ebuff, hdwrite
	dd exit
	
; loads a buffer (addr -- )
defword loadb, loadb, 0
	dd W_LATEST, fetch, BTMP, store		; older buffer on btmp
	dd W_LATEST, store			; store new buffer
	dd interp
	dd lNILq, fetch, zbranch, -16
	dd lit, 0, lNILq, store
	dd BTMP, fetch, W_LATEST, store
	dd exit

; (blknum -- ) loads and executes a block from buffer wbuff
defword load, load, 0
	dd dup, CBLK, store, rdblk
	dd wbuff, loadb, exit
	
; converts block to meg, max 2000 per meg
; 400mb offset to avoid screwing windows boot
defword mmeg, mmeg, 0
	dd lit, 400, madd, lit, 2000, mmul, madd, exit
	
; list block # of mb (blk mb -- )
defword mlist, mlist, 0
	dd mmeg, list, exit
	
; edits block of mb (blk(max 2k) mb# -- blknum)
defword medit, medit, 0
	dd mmeg, edit, exit

; (blk mb -- )
defword mload, mload, 0
	dd mmeg, load, exit
	
defword mclr, mclr, 0
	dd mmeg, bclr, exit
	
	
	
; dumps wb 
defword bd, bdump, 0
	dd lit, 1023, e_clrscr, dumpchar
	dd exit

defword d8, dump8, 0
	dd bsze, e_clrscr, dumpbyte, exit

defword d82, dump82, 0
	dd bsze, madd, bsze, e_clrscr, dumpbyte, exit
	
; debug

defconst xpos, xpos, 0, stdio_cur_x
defconst ypos, ypos, 0, stdio_cur_y

defconst vga_font, vga_font, 0, stdio_font
defconst vga_bg, vga_bg, 0, clr_bg
defconst vga_fg, vga_fg, 0, clr_fg

defconst TAB_SZE, tab_size, 0, STDIO_TAB
	
; draw line (x y 8col -- )
defcode pix, pix_addr, 0
	PP ebx
	PP eax
	call pixel_addr
	PS eax
	NEXT
	
defconst xres, xres, 0, 1024
defconst yres, yres, 0, 768

; PCI ---

; reads dword (bus slot func offset -- dword)
defcode pci_r8, pci_rd, 0
	PP edx
	PP ecx
	PP ebx
	PP eax
	call pci_config_read_dword
	PS eax
	NEXT
	
; reads dword (bus slot func offset -- dword)
defcode pci_r4, pci_rw, 0
	PP edx
	PP ecx
	PP ebx
	PP eax
	call pci_config_read_word
	PS eax
	NEXT
	
; reads dword (bus slot func offset -- dword)
defcode pci_r2, pci_rb, 0
	PP edx
	PP ecx
	PP ebx
	PP eax
	call pci_config_read_byte
	PS eax
	NEXT



; load FORTH.f in sys4_files
%include "sys4_files/scr.4" ; includes init_buff 
defconst ibuff, ibuff, 0, init_buff
defword forth_init, forth_I, 0 
	dd ibuff, loadb, exit



	
; last word of the CODE dictionary NO MORE WORDS IN CODE AFTER THIS
defword DICEND, dicend, 0
	dd exit


	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

