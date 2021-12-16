.model small
.stack 100h
.data
	; file names
	input_file 			db 0, 50h dup(0)
	output_file 		db 0, 50h dup(0)
	; file handles
	input_handle		dw 0000h
	output_handle		dw 0000h
	; general for buffers
	buffer_cap			dw 200h
	; input file buffer
	input_buffer 		db 200h dup(0)
	input_buffer_size	dw 0000h
	ib_iter				dw 0000h	; input buffer iterator
	; output_buffer
	output_buffer 		db 200h dup(0)
	ob_iter				dw 0000h	; output buffer iterator
	; reading byte
	rdbyte				db 00h
	; instruction strings
	str_pushf			db "PUSHF", 0Dh, 0Ah
	str_popf			db "POPF", 0Dh, 0Ah
	str_call			db "CALL "
	str_add				db "ADD "
	str_cmp				db "CMP "
	str_nul				db "?", 0Dh, 0Ah
	str_ins				db 100h dup(0), 0h
	; str ins iterator
	ins_iter			db 00h
	; register strings
	word_reg			db "AXCXDXBX"
	str_reg_adr_sum		db "BX+SIBX+DIBP+SIBP+DI"
	str_reg_adr			db "SIDIBPBX"	
	str_reg				db "AXCXDXBXSPBPSIDI"
	; instruction opcodes
	OP_PUSHF			db 9Ch
	OP_POPF				db 9Dh
	OP_CALL1			db 0E8h
	OP_CALL2			db 0FFh
	OP_CALL3			db 09Ah
	OP_CALL4			db 0FFh
	; utility 
	endl				db 0Dh, 0Ah, '$'
	; error messages
	msg_file_open		db "Couldn't open file", 0Dh, 0Ah, '$'
	msg_file_read		db "Couldn't read file", 0Dh, 0Ah, '$'
	msg_file_close		db "Couldn't close file", 0Dh, 0Ah, '$'
	msg_file_write		db "Couldn't write to file", 0Dh, 0Ah, '$'
	; log messages
	msg_ibuffer_updated	db "update_input_buffer has been called", 0Dh, 0Ah, '$'
	msg_obuffer_dump	db "flush_output_buffer has been called", 0Dh, 0Ah, '$'
	msg_debug			db "debug text!", 0Dh, 0Ah, '$'
	; printing registers as hex
	hex_word_buf		db 4 dup(0), '$' 
	hex_byte_buf		db 2 dup(0), '$'
	; instruction components
	op 					db 0h
	addrb				db 0h
	s 					db 0h
	d 					db 0h
	w 					db 0h
	_mod 				db 0h
	reg 				db 0h
	rm 					db 0h
	adr					db 0h, 0h
	posl				db 0h, 0h
	segreg				db 0h, 0h
	; sign symbol
	sign				db ' '
.code
start:
	mov dx, @data
	mov ds, dx
	
	call read_params
	call open_input_file
	jc exit
	call open_output_file
	jc exit
	call update_input_buffer
	
	xor cx, cx
	inc cx
	l003: ; infinite loop
		clc
		push cx
		
		call get_byte ; opcode
		jc b003
		mov al, rdbyte
		call convert_al_to_hex_str
		
		mov ah, 09h
		lea dx, hex_byte_buf
		int 21h
		mov ah, 09h
		lea dx, endl
		int 21h
		
		mov al, rdbyte
		call parse_pushf
		jnc write_str
		
		call parse_popf
		jnc write_str
	
		call parse_call
		jnc write_str
		
	
		call write_str_nul
		write_str:
			call write_ins_str
		next:
		pop cx
		inc cx ; increment cx to make loop infinite
	loop l003
	b003:
	
	cmp ob_iter, 0
	je close
	call flush_output_buffer
	close:
	call close_input_file
	call close_output_file
exit:
    mov ax, 4c00h
    int 21h

; stores program arguments and their lengths 
; in 'input_file' & 'output_file'
; STATUS: works
read_params:
	xor bx, bx
	xor cx, cx
	mov cl, es:[80h]
	jcxz exit
	l001:
		mov al, es:[bx + 82h]
		cmp al, ' '
		je b001
		cmp al, 0Dh
		je b001
		mov [input_file + bx + 1h], al
		inc bx
	loop l001
	b001:
	mov [input_file], bl
	mov si, bx
	inc si
	xor bx, bx
	l002:
		mov al, es:[bx + si + 82h]
		cmp al, ' '
		je b002
		cmp al, 0Dh
		je b002
		mov [output_file + bx + 1h], al
		inc bx
	loop l002
	b002:
	mov [output_file], bl
	ret

; writes str_nul to str_ins
; STATUS: works
write_str_nul:
	mov [str_ins + 0], '?'
	mov [str_ins + 1], 0Dh
	mov [str_ins + 2], 0Ah
	mov [str_ins + 3], 0h
	ret
	
; processes call instruction, writes the instruction string
; to str_ins, expects opcode to be in AL
; in case of failure sets CF
; STATUS: not tested
parse_call:
	clc
	cmp al, OP_CALL1
	jne test_call_3
		call parse_call_1
		jc parse_call_exit
		ret
	test_call_3:
		cmp al, OP_CALL3
		jne test_call_24
		call parse_call_3
		jc parse_call_exit
		ret
	test_call_24:
		cmp al, OP_CALL2
		jne parse_call_exit
		call parse_call_24
		jc parse_call_exit
		ret
	parse_call_exit:
	stc
	ret
	
; 1110 1000 adr.j.b. adr.v.b.
; set CF on failure
; STATUS: works
parse_call_1:
	clc
	
	call get_byte
	jc pc1_ex
	mov al, rdbyte
	mov [adr + 0], al
	
	call get_byte
	jc pc1_ex
	mov al, rdbyte
	mov [adr + 1], al
	
	jmp pc2_cont
	pc1_ex:
	stc
	ret
	pc2_cont:
	mov al, [adr + 0]
	mov ah, [adr + 1]
	
	add ax, 3
	
	call convert_ax_to_hex_str
	mov al, [hex_word_buf + 0]
	mov ah, [hex_word_buf + 1]
	mov bl, [hex_word_buf + 2]
	mov bh, [hex_word_buf + 3]
	mov [str_ins +  0], 'C'
	mov [str_ins +  1], 'A'
	mov [str_ins +  2], 'L'
	mov [str_ins +  3], 'L'
	mov [str_ins +  4], ' '
	mov [str_ins +  5], al
	mov [str_ins +  6], ah
	mov [str_ins +  7], bl
	mov [str_ins +  8], bh
	mov [str_ins +  9], 0Dh
	mov [str_ins + 10], 0Ah
	mov [str_ins + 11], 0h
	ret
; 1111 1111 mod 010 r/m [posl.j.b. [posl.v.b.]]
; 1111 1111 mod 011 r/m [poslinkis]
; set CF on failure
; STATUS: not tested
parse_call_24:
	; read addresing byte
	call get_byte
	jne pc_cont0 
		pc_ex:
		stc
		ret
	pc_cont0:
	mov al, rdbyte
	; look if reg is 010 or 011 else exit
	call parse_modregrm
	cmp reg, 010b
	je pc_cont1
	cmp reg, 011b
	je pc_cont1
	jmp pc_ex
	pc_cont1:
	; reg is okay, so this is some sort of call
	; write "CALL "
	mov [str_ins + 0], 'C'
	mov [str_ins + 1], 'A'
	mov [str_ins + 2], 'L'
	mov [str_ins + 3], 'L'
	mov [str_ins + 4], ' '
	mov ins_iter, 5
	; special case mod = 11b and reg = 010b
	cmp _mod, 11b
	jne pc_cont4
	cmp reg, 010b
	jne pc_cont4
		; call str_reg	
		xor bx, bx
		mov bl, rm
		shl bx, 1 ; * 2
		mov al, [str_reg + bx + 0]
		mov ah, [str_reg + bx + 1]
		mov [str_ins + 5], al
		mov [str_ins + 6], ah
		mov [str_ins + 7], 0Dh
		mov [str_ins + 8], 0Ah
		mov [str_ins + 9], 0h
		add ins_iter, 5
		ret
	pc_cont4:
	; write far if needed
	cmp reg, 011b
	jne pc_cont2
		mov [str_ins + 5], 'F'
		mov [str_ins + 6], 'A'
		mov [str_ins + 7], 'R'
		add ins_iter, 3
	pc_cont2:
	; write open bracket
	xor bx, bx
	mov bl, ins_iter
	mov [str_ins + bx + 0], '['
	inc ins_iter
	; special case if addresing byte = 16 or 1E read 2 extra bytes
	cmp rdbyte, 16h
	je pc_cont10
	cmp rdbyte, 1eh
	je pc_cont10
	jmp pc_cont7
	pc_cont10:
		; read 2 bytes
		call get_byte
		jc pc_ex3
		mov al, rdbyte
		push ax
		call get_byte
		jc pc_ex3
		pop ax
		mov ah, rdbyte
		
		jmp pc_cont9
		pc_ex3:
			stc
			ret
		pc_cont9:
		
		call convert_ax_to_hex_str
		mov al, [hex_word_buf + 0]
		mov ah, [hex_word_buf + 1]
		mov dl, [hex_word_buf + 2]
		mov dh, [hex_word_buf + 3]
		
		xor bx, bx
		mov bl, ins_iter
		mov [str_ins +  6], al
		mov [str_ins +  7], ah
		mov [str_ins +  8], dl
		mov [str_ins +  9], dh
		add ins_iter, 4
		jmp pc_cont8
	pc_cont7:
	
	; write single or double register as index
	cmp rm, 4
	jb double
		xor bx, bx
		mov bl, rm
		sub bl, 4
		shl bl, 1
		mov al, [str_reg_adr + bx + 0]
		mov ah, [str_reg_adr + bx + 1]
		
		xor bx, bx
		mov bl, ins_iter
		mov [str_ins + bx + 0], al
		mov [str_ins + bx + 1], ah
		add ins_iter, 2
		
		cmp _mod, 01b
		je pc_cont6
		cmp _mod, 10b
		je pc_cont6
		jmp pc_cont3
	double:
		mov cx, 0005h
		xor ax, ax
		mov al, rm
		mul cx ; ax *= 5
		mov si, ax ; bx = ax
		
		xor bx, bx
		mov bl, ins_iter
		l008:
			mov al, [str_reg_adr_sum + si]
			mov [str_ins + bx], al
			inc si
			inc bx
		loop l008
		add ins_iter, 5
		
		cmp _mod, 01b
		je pc_cont6
		cmp _mod, 10b
		je pc_cont6
		
		pc_cont8:
		; same as pc_cont3
		xor bx, bx
		mov bl, ins_iter
		mov [str_ins + bx + 0], ']'
		mov [str_ins + bx + 1], 0Dh
		mov [str_ins + bx + 2], 0Ah
		mov [str_ins + bx + 3], 0h
		ret
		pc_cont6:
		; read mod bytes and add them!
		call get_byte 
		jnc pc_cont5
			pc_ex2:
			stc
			ret
		pc_cont5:
		
		mov al, rdbyte
		cmp _mod, 01b
		je pc_write_1
			; here goes writing 2 bytes and reading one extra
			push ax
			call get_byte
			jc pc_ex2
			pop ax
			mov ah, rdbyte
			
			; adjust ax
			mov sign, '+'
			cmp ax, 8000h
			jb do_nothing2
				mov bx, ax
				xor ax, ax
				sub ax, bx
				mov sign, '-'
			do_nothing2:
			
			call convert_ax_to_hex_str
			
			mov al, [hex_word_buf + 0]
			mov ah, [hex_word_buf + 1]
			mov dl, [hex_word_buf + 2]
			mov dh, [hex_word_buf + 3]
			
			xor bx, bx
			mov bl, ins_iter
			mov cl, sign
			mov [str_ins + bx + 0], cl
			mov [str_ins + bx + 1], al
			mov [str_ins + bx + 2], ah
			mov [str_ins + bx + 3], dl
			mov [str_ins + bx + 4], dh
			add ins_iter, 5
			jmp pc_cont3
		jmp pc_cont3
		pc_write_1:
			; here goes writting 1 byte
			; if(AL > 0x7f) AL = 0x100 - AL
			mov sign, '+'
			cmp al, 7fh
			jbe pc_do_nothing1
				; here we adjust al
				mov bl, al
				xor al, al
				sub al, bl ; idk if this works?
				mov sign, '-'
			pc_do_nothing1:
			
			call convert_al_to_hex_str			
			mov al, [hex_byte_buf + 0]
			mov ah, [hex_byte_buf + 1]
			
			xor bx, bx
			mov bl, ins_iter
			mov dl, sign
			mov [str_ins + bx + 0], dl
			mov [str_ins + bx + 1], al
			mov [str_ins + bx + 2], ah
			add ins_iter, 3
	pc_cont3:
		xor bx, bx
		mov bl, ins_iter
		mov [str_ins + bx + 0], ']'
		mov [str_ins + bx + 1], 0Dh
		mov [str_ins + bx + 2], 0Ah
		mov [str_ins + bx + 3], 0h
		; no need to edit ins_iter, it's gonna get reset anyways
	ret
	
; 1001 1010 adr.j.b. adr.v.b. seg.reg.j.b. seg.reg.v.b.
; set CF on failure
; STATUS: works
parse_call_3:
	clc
	; read 4 bytes
	call get_byte
	jc pc3_ex
	mov al, rdbyte
	mov [adr + 0], al
	
	call get_byte
	jc pc3_ex
	mov al, rdbyte
	mov [adr + 1], al
	
	call get_byte
	jc pc3_ex
	mov al, rdbyte
	mov [segreg + 0], al
	
	call get_byte
	jc pc3_ex
	mov al, rdbyte
	mov [segreg + 1], al
	
	jmp pc3_cont
	pc3_ex:
		stc
		ret
	pc3_cont:
	
	mov al, [segreg + 0]
	mov ah, [segreg + 1]
	call convert_ax_to_hex_str
	
	mov al, [hex_word_buf + 0]
	mov ah, [hex_word_buf + 1]
	mov bl, [hex_word_buf + 2]
	mov bh, [hex_word_buf + 3]
	
	; I know it's ugly but it's less code than doing loops
	mov [str_ins +  0], 'C'
	mov [str_ins +  1], 'A'
	mov [str_ins +  2], 'L'
	mov [str_ins +  3], 'L'
	mov [str_ins +  4], ' '
	mov [str_ins +  5], al
	mov [str_ins +  6], ah
	mov [str_ins +  7], bl
	mov [str_ins +  8], bh
	mov [str_ins +  9], ':'
	; convert adr
	mov al, [adr + 0]
	mov ah, [adr + 1]
	call convert_ax_to_hex_str
	mov al, [hex_word_buf + 0]
	mov ah, [hex_word_buf + 1]
	mov bl, [hex_word_buf + 2]
	mov bh, [hex_word_buf + 3]
	
	mov [str_ins + 10], al
	mov [str_ins + 11], ah
	mov [str_ins + 12], bl
	mov [str_ins + 13], bh
	mov [str_ins + 14], 0Dh
	mov [str_ins + 15], 0Ah
	mov [str_ins + 16], 0h
	ret
	
; expects addresing byte to be in AL
; sets _mod reg and r/m variables
parse_modregrm:
; 18 0001 1000
	mov rm, al
	and rm, 00000111b
	mov reg, al
	shr reg, 3
	and reg, 00000111b
	mov _mod, al
	shr _mod, 6
	and _mod, 00000011b	
	ret
	
; checks al, if opcode matched writes to 
; ins_str the string of the instruction
; in case of failure sets CF
; STATUS: works
parse_pushf:
	clc
	cmp al, OP_PUSHF
	jne ret001
	; if opcode matched copy str_pushf to ins_str
	mov cx, 7
	xor bx, bx
	l005:
		mov dl, [str_pushf + bx]
		mov [str_ins + bx], dl
		inc bx
	loop l005
	mov [str_ins + bx], 0h
	ret
	ret001:
	stc
	ret

; checks al, if opcode matched OP_POPF, writes to 
; ins_str the string of the instruction POPF
; in case of failure sets CF
; STATUS: works
parse_popf:
	clc
	cmp al, OP_POPF
	jne ret002
	; if opcode matched copy str_pushf to ins_str
	mov cx, 6
	xor bx, bx
	l007:
		mov dl, [str_popf + bx]
		mov [str_ins + bx], dl
		inc bx
	loop l007
	mov [str_ins + bx], 0h
	ret
	ret002:
	stc
	ret
	
; converts AL register to a hex string and stores
; result in hex_byte_buf
; STATUS: works
convert_al_to_hex_str:
	push ax
	and al, 0Fh
	cmp al, 09h
	jbe digit
		sub al, 0Ah
		add al, 'A'
	jmp write_to_hex_buf1
	digit:
		add al, '0'
	write_to_hex_buf1:
	mov [hex_byte_buf + 1], al
	pop ax
	push ax
	shr al, 4
	cmp al, 09h
	jbe digit01
		sub al, 0Ah
		add al, 'A'
	jmp write_to_hex_buf2
	digit01:
		add al, '0'
	write_to_hex_buf2:
	mov [hex_byte_buf + 0], al
	pop ax
	ret

; converts AX register to a hex string and stores
; result in hex_word_buf
; STATUS: works
convert_ax_to_hex_str:
	push ax dx
	call convert_al_to_hex_str
	mov dl, [hex_byte_buf + 0]
	mov [hex_word_buf + 2 + 0], dl
	mov dl, [hex_byte_buf + 1]
	mov [hex_word_buf + 2 + 1], dl
	shr ax, 8
	call convert_al_to_hex_str
	mov dl, [hex_byte_buf + 0]
	mov [hex_word_buf + 0 + 0], dl
	mov dl, [hex_byte_buf + 1]
	mov [hex_word_buf + 0 + 1], dl
	pop dx ax
	ret

; open file with name 'input_file'
; store handle in 'input_handle'
; in case of error sets CF
; STATUS: works
open_input_file:
	clc
	mov ax, 3d00h
	lea dx, input_file + 1
	int 21h
	jc file_open_err
	mov input_handle, ax
	ret
	file_open_err:
		mov ah, 09h
		lea dx, msg_file_open
		int 21h
		ret

; closes input file using handle 'input_handle'
; STATUS: works
close_input_file:
	clc
	mov ax, 3e00h
	mov bx, input_handle
	int 21h
	jc file_close_error
	ret
	file_close_error:
		mov ah, 09h
		lea dx, msg_file_close
		int 21h
		ret
		
; open file with name 'output_file'
; store handle in 'output_handle'
; STATUS: works
open_output_file:
	clc
	mov ax, 3d01h
	lea dx, output_file + 1
	int 21h
	jc file_open_err2
	mov output_handle, ax
	ret
	file_open_err2:
		mov ah, 09h
		lea dx, msg_file_open
		int 21h
		ret

; closes output file using handle 'output_handle'
; STATUS: works
close_output_file:
	clc
	mov ax, 3e00h
	mov bx, output_handle
	int 21h
	jc file_close_error2
	ret
	file_close_error2:
		mov ah, 09h
		lea dx, msg_file_close
		int 21h
		ret

; update internal buffer, resets 'ib_iter'
; and updates 'buffer_size'
; STATUS: works
update_input_buffer:
	mov ah, 09h
	lea dx, msg_ibuffer_updated
	int 21h

	mov ax, 3f00h
	mov bx, input_handle
	mov cx, buffer_cap
	lea dx, input_buffer
	int 21h
	jc file_read_err
		mov input_buffer_size, ax
		mov ib_iter, 0
		ret
	file_read_err:
		mov ah, 09h
		lea dx, msg_file_read
		int 21h
		ret

; stores byte from input_file in 'byte'
; uses 'input_buffer' to minimize interupt calls
; if no more bytes are available to read sets CF
; STATUS: works
get_byte:
	clc
	mov ax, input_buffer_size
	cmp ib_iter, ax
	jne read_byte
		mov ax, buffer_cap
		cmp input_buffer_size, ax
		je _update_buffer
			stc
			ret
		_update_buffer:
			call update_input_buffer
	read_byte:
		clc		; cmp sets CF if op1 < op2
		mov bx, ib_iter
		mov al, [input_buffer + bx]
		mov rdbyte, al
		inc ib_iter
		ret
; writes 'output_buffer' contents to 'output_handle'
; reset ob_iter
; STATUS: works
flush_output_buffer:
	mov ah, 09h
	lea dx, msg_obuffer_dump
	int 21h
	mov ax, 4000h
	mov bx, output_handle
	mov cx, ob_iter			; dump everything to iterator 
	lea dx, output_buffer
	int 21h
	mov ob_iter, 0h
	ret

; writes 'str_ins' to file with 'output_handle', 
; uses 'output_buffer' to minimize interupt calls 
; ins_str should have 0 at the end
; STATUS: works
write_ins_str:
	xor bx, bx
	mov cx, 1h
	l004:	; infinite loop
		; if ob_iter == buffer_cap flush output buffer
		mov ax, ob_iter
		cmp buffer_cap, ax
		jne dont_flush
			call flush_output_buffer
		dont_flush:
		
		mov al, [str_ins + bx]
		cmp al, 0
		je exit_wis	; end proc if ins_str ended
		
		mov si, ob_iter
		mov [output_buffer + si], al
		inc ob_iter
		inc bx
		inc cx 
	loop l004
	exit_wis:
	ret

end start