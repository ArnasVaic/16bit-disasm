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
	str_reg				db "ALCLDLBLAHCHDHBHAXCXDXBXSPBPSIDI"
	; instruction opcodes
	OP_PUSHF			db 9Ch
	OP_POPF				db 9Dh
	OP_CALL1			db 0E8h
	OP_CALL2			db 0FFh
	OP_CALL3			db 09Ah
	OP_CALL4			db 0FFh
	OP_CMP1				db 38h
	OP_CMP2				db 80h
	OP_CMP3				db 3Ch
	OP_ADD1				db 00h
	OP_ADD2				db 80h
	OP_ADD3				db 04h
	; instruction masks
	MASK_CMP1			db 0FCh
	MASK_CMP2			db 0FCh
	MASK_CMP3			db 0FEh
	MASK_ADD1			db 0FCh
	MASK_ADD2			db 0FCh
	MASK_ADD3			db 0FEh
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
	dir					db 0h
	w					db 0h
	_mod 				db 0h
	reg 				db 0h
	rm 					db 0h
	adr					db 0h, 0h
	posl				db 0h, 0h
	segreg				db 0h, 0h
	; sign symbol
	sign				db ' '
	;
	safe_op				db 00h
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
		mov op, al
		mov safe_op, al
		
		mov al, safe_op
		call parse_pushf
		jnc write_str
		
		mov al, safe_op
		call parse_popf
		jnc write_str
	
		mov al, safe_op
		call parse_call
		jnc write_str
		
		mov al, safe_op
		call parse_add
		jnc write_str
		
		mov al, safe_op
		call parse_cmp
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
	
; parses add instruction
; STATUS: not tested	
parse_add:
	clc
	mov op, al
		and al, MASK_ADD1
		cmp al, OP_ADD1
		jne test_add_2
		call parse_add1
		jc add_exit
		ret
	test_add_2:
		mov al, op
		and al, MASK_ADD2
		cmp al, OP_ADD2
		jne test_add_3
		call parse_add2
		jc add_exit
		ret
	test_add_3:
		mov al, op
		and al, MASK_ADD3
		cmp al, OP_ADD3
		jne add_exit
		call parse_add3
		jc add_exit
		ret
	add_exit:
	stc
	ret
	
; 0000 00dw mod reg r/m [poslinkis]
; STATUS: works
parse_add1:
	clc
	call parse_dw
	; read addresing byte
	call get_byte
	jnc cont024
	exit008:
		stc
		ret
	cont024:
	; write "cmp "
	mov [str_ins + 0], 'A'
	mov [str_ins + 1], 'D'
	mov [str_ins + 2], 'D'
	mov [str_ins + 3], ' '
	mov ins_iter, 4
	; parse modregrm
	mov al, rdbyte
	call parse_modregrm
	; special case mod=00b rm=110b
	cmp _mod, 00h
	jne cont026
	cmp rm, 110b
	jne cont026
		; read another 2 bytes
		call get_byte
		jc exit008
		mov al, rdbyte
		push ax
		call get_byte
		jc exit008
		pop ax
		mov ah, rdbyte
		; convert
		call convert_ax_to_hex_str
		cmp dir, 0
		jne first_reg2
			call append_immediate_offset_str
			call append_comma
			call append_reg_str
		jmp cont025
		first_reg2:
			call append_reg_str
			call append_comma
			call append_immediate_offset_str
		cont025:
		call append_endl0
		ret
	cont026:
	
	cmp dir, 0
	jne first_reg3
		call append_mod_rm_str
		call append_comma
		call append_mod_reg_str
		jc exit009
		jmp cont027
		ret
	first_reg3:
		call append_mod_reg_str
		call append_comma
		call append_mod_rm_str
		jc exit009
	cont027:
	call append_endl0
	ret
	exit009:
	stc
	ret
; 1000 00sw mod 000 r/m [poslinkis] bet.op.j.b. [bet.op.v.b]
; STATUS: not tested
parse_add2:
	clc
	call get_byte
	jc exit010
	mov al, rdbyte
	call parse_modregrm
	cmp reg, 000b
	je cont028 ;cont017
		exit010:
		stc
		ret
	cont028:
	;write "add "
	mov [str_ins + 0], 'A'
	mov [str_ins + 1], 'D'
	mov [str_ins + 2], 'D'
	mov [str_ins + 3], ' '
	mov ins_iter, 4
	; append first operand
	call append_mod_rm_str
	jc exit010
	call append_comma
	; things get interasting
	call parse_sw
	cmp w, 0h
	jne cont029 ;cont018
		; read one byte as second operand
		call get_byte
		jc exit010
		mov al, rdbyte
		call append_byte_operand
		jmp cont030;cont019
	cont029:
		; do interasting things if s = 1
		cmp s, 1
		jne const031;cont020
			call get_byte
			jc exit010
			mov al, rdbyte
			call append_sign_byte_offset
			jmp cont030
		const031:
			; read word as second operand
			call get_word
			jc  exit010
			call append_word_operand
	cont030:
	call append_endl0
	ret
	
; 0000 010w bet.op.j.b [bet.op.v.b., jei w = 1]
; STATUS: works?
parse_add3:
	clc	
	call parse_dw
	; write "cmp AL"(or AX)
	mov [str_ins + 0], 'A'
	mov [str_ins + 1], 'D'
	mov [str_ins + 2], 'D'
	mov [str_ins + 3], ' '
	mov [str_ins + 4], 'A'
	mov al, 'L'
	cmp w, 1
	jne cont021
		mov al, 'X'
	cont021:
	mov [str_ins + 5], al 
	mov [str_ins + 6], ','
	mov [str_ins + 7], ' ' 
	mov ins_iter, 8
	
	cmp w, 1
	jne cont022
		; append word
		call get_word
		jc exit007
		call append_word_operand
		jmp cont023
	cont022:
		; append byte
		call get_byte
		jc exit007
		mov al, rdbyte
		call append_byte_operand
	cont023:
	call append_endl0
	ret
	exit007:
	stc
	ret

; parses cmp instruction
; sets CF on failure
; STATUS: works
parse_cmp:
	clc
	mov op, al
		and al, MASK_CMP1
		cmp al, OP_CMP1
		jne test_cmp_2
		call parse_cmp1
		jc cmp_exit
		ret
	test_cmp_2:
		mov al, op
		and al, MASK_CMP2
		cmp al, OP_CMP2
		jne test_cmp_3
		call parse_cmp2
		jc cmp_exit
		ret
	test_cmp_3:
		mov al, op
		and al, MASK_CMP3
		cmp al, OP_CMP3
		jne cmp_exit
		call parse_cmp3
		jc cmp_exit
		ret
	cmp_exit:
	stc
	ret

; 0011 10dw mod reg r/m [poslinkis]
; CF set on failure
; STATUS: works
parse_cmp1:
	clc
	call parse_dw
	; read addresing byte
	call get_byte
	jnc pcmp_cont01
	pcmp_ex1:
		stc
		ret
	pcmp_cont01:
	; write "cmp "
	mov [str_ins + 0], 'C'
	mov [str_ins + 1], 'M'
	mov [str_ins + 2], 'P'
	mov [str_ins + 3], ' '
	mov ins_iter, 4
	; parse modregrm
	mov al, rdbyte
	call parse_modregrm
	; special case mod=00b rm=110b
	cmp _mod, 00h
	jne pcm_cont00
	cmp rm, 110b
	jne pcm_cont00
		; read another 2 bytes
		call get_byte
		jc pcmp_ex1
		mov al, rdbyte
		push ax
		call get_byte
		jc pcmp_ex1
		pop ax
		mov ah, rdbyte
		; convert
		call convert_ax_to_hex_str
		cmp dir, 0
		jne first_reg
			call append_immediate_offset_str
			call append_comma
			call append_reg_str
		jmp pcm_cont02
		first_reg:
			call append_reg_str
			call append_comma
			call append_immediate_offset_str
		pcm_cont02:
		call append_endl0
		ret
	pcm_cont00:
	
	cmp dir, 0
	jne first_reg1
		call append_mod_rm_str
		call append_comma
		call append_mod_reg_str
		jc exit005
		jmp cont016
		ret
	first_reg1:
		call append_mod_reg_str
		call append_comma
		call append_mod_rm_str
		jc exit005
	cont016:
	call append_endl0
	ret
	exit005:
	stc
	ret

; appends a string to 'str_ins' based on rm field (mod = 11b)
; STATUS: works
append_11_rm_str:
	mov al, reg ; save reg value
	push ax
	mov dl, rm
	mov reg, dl
	call append_mod_reg_str
	pop ax
	mov reg, al ; restore reg value
	ret

; appends a string to 'str_ins' based on mod/rm fields
; STATUS: works
append_mod_rm_str:
	cmp _mod, 11b
	jne cont010
		; if mod 11 just rm->reg & call append_mod_reg_str
		call append_11_rm_str
		ret
	cont010:
	; special case: mod 00 rm 110
	cmp _mod, 00b
	jne cont011
	cmp rm, 110b
	jne cont011
		call get_word
		jnc cont014
			stc
			ret
		cont014:
		call convert_ax_to_hex_str
		call append_immediate_offset_str
		ret
	cont011:
	; append '['
	xor bx, bx
	mov bl, ins_iter
	mov [str_ins + bx], '['
	inc ins_iter
	; append register offset, ex: BX+SI, BP+DI, DI, BP, etc...
	cmp rm, 4
	jae single_reg
		
		xor ax, ax
		mov al, rm
		mov cx, 0005h
		mul cx
		mov si, ax
		
		xor bx, bx
		mov bl, ins_iter
		
		l009:
			mov al, [str_reg_adr_sum + si]
			mov [str_ins + bx], al
			inc si
			inc bx
		loop l009
		add ins_iter, 5
		
		jmp cont012
	single_reg:
		xor bx, bx
		mov bl, rm
		sub bl, 4
		shl bx, 1 ; * 2
	
		mov al, [str_reg_adr + bx + 0]
		mov ah, [str_reg_adr + bx + 1]
	
		xor bx, bx
		mov bl, ins_iter
		mov [str_ins + bx + 0], al
		mov [str_ins + bx + 1], ah
		add ins_iter, 2
	cont012:
	; check if there is offset
	cmp _mod, 00b
	je cont013
		; write byte or word offset
		cmp _mod, 01h
		je byte_offset
			; read/write word
			call get_word
			jnc cont015
				exit004:
				stc 
				ret
			cont015:
			call append_sign_word_offset
		jmp cont013
		byte_offset:
			call get_byte
			jc exit004
			mov al, rdbyte
			call append_sign_byte_offset
	cont013:
	; write ']'
	xor bx, bx
	mov bl, ins_iter
	mov [str_ins + bx], ']'
	inc ins_iter
	ret
	


; appends a byte offset from al register with a sign to 'ins_str'
; may adjust al (if al > 0x7f then al = 0x100 - al)
; format: "+XX" or "-XX"
; STATUS: works
append_sign_byte_offset:
	mov sign, '+'
	cmp al, 80h
	jb cont004
		mov bl, al
		mov al, 0
		sub al, bl
		mov sign, '-'
	cont004:
	
	call convert_al_to_hex_str
	
	xor bx, bx
	mov bl, ins_iter
	
	mov al, [hex_byte_buf + 0]
	mov ah, [hex_byte_buf + 1]
	mov dl, sign
	
	mov [str_ins + bx + 0], dl
	mov [str_ins + bx + 1], al
	mov [str_ins + bx + 2], ah
	add ins_iter, 3
	ret
	
; appends a word offset from ax register with a sign to 'ins_str'
; may adjust ax (if al > 0x7fff then al = 0x1000 - al)
; format: "+XXXX" or "-XXXX"
; STATUS: works
append_sign_word_offset:
	mov sign, '+'
	cmp ax, 8000h
	jb cont006
		mov bx, ax
		xor ax, ax
		sub ax, bx
		mov sign, '-'
	cont006:
	
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
	ret

; appends a string to 'str_ins' dependant on mod/reg fields
; STATUS: works
append_mod_reg_str:
	; calculate offset = 2(8w + reg)
	xor ax, ax
	mov al, w
	mov bx, 8
	mul bx
	add al, reg
	shl ax, 1
	; put register string in AX
	mov si, ax
	mov al, [str_reg + si + 0]
	mov ah, [str_reg + si + 1]
	; write to ins_str
	xor bx, bx
	mov bl, ins_iter
	mov [str_ins + bx + 0], al
	mov [str_ins + bx + 1], ah
	add ins_iter, 2
	ret
	
; proccesses byte in 'op'
; parses direction and word flags
; STATUS: works
parse_dw:
	mov al, op
	mov dir, al
	shr dir, 1
	and dir, 01h
	mov w, al
	and w, 01h
	ret
	
; appends "\r\n\0" to 'str_ins'
; STATUS: works
append_endl0:
	xor bx, bx
	mov bl, ins_iter
	mov [str_ins + bx + 0], 0Dh
	mov [str_ins + bx + 1], 0Ah
	mov [str_ins + bx + 2], 0h
	add ins_iter, 3
	ret
	
; appends "[word offset]" to 'str_ins'
; word offset stored in hex_word_buf
; STATUS: works
append_immediate_offset_str:
	xor bx, bx
	mov bl, ins_iter
	mov al, [hex_word_buf + 0]
	mov ah, [hex_word_buf + 1]
	mov dl, [hex_word_buf + 2]
	mov dh, [hex_word_buf + 3]
	mov [str_ins + bx + 0], '['
	mov [str_ins + bx + 1], al
	mov [str_ins + bx + 2], ah
	mov [str_ins + bx + 3], dl
	mov [str_ins + bx + 4], dh
	mov [str_ins + bx + 5], ']'
	add ins_iter, 6
	ret
	
append_comma:
	xor bx, bx
	mov bl, ins_iter
	mov [str_ins + bx + 0], ','
	mov [str_ins + bx + 1], ' '
	add ins_iter, 2
	ret
	
append_reg_str:
	xor ax, ax
	xor bx, bx
	
	mov ax, 0008h
	mov bl, w
	mul bx
	
	mov bx, ax
	add bl, reg
	shl bx, 1
	
	mov al, [str_reg + bx + 0]
	mov ah, [str_reg + bx + 1]
	
	mov bl, ins_iter
	mov [str_ins + bx + 0], al
	mov [str_ins + bx + 1], ah
	add ins_iter, 2
	ret
	
; 1000 00sw mod 111 r/m [poslinkis] bet.op.j.b. [bet.op.v.b.]
; STATUS: doesnt work
parse_cmp2:
	clc
	; add & cmp has an opcode collision so no need to call get_byte because it is already in rdbyte
	mov al, rdbyte	
	call parse_modregrm
	cmp reg, 111b
	je cont017
		exit006:
		stc
		ret
	cont017:
	;write "cmp "
	mov [str_ins + 0], 'C'
	mov [str_ins + 1], 'M'
	mov [str_ins + 2], 'P'
	mov [str_ins + 3], ' '
	mov ins_iter, 4
	; append first operand
	call append_mod_rm_str
	jc exit006
	call append_comma
	; things get interasting
	call parse_sw
	cmp w, 0h
	jne cont018
		; read one byte as second operand
		call get_byte
		jc exit006
		mov al, rdbyte
		call append_byte_operand
		jmp cont019
	cont018:
		; do interasting things if s = 1
		cmp s, 1
		jne cont020
			call get_byte
			jc exit006
			mov al, rdbyte
			call append_sign_byte_offset
			jmp cont019
		cont020:
			; read word as second operand
			call get_word
			jc  exit006
			call append_word_operand
	cont019:
	call append_endl0
	ret
	
; expects opcode in 'op'
; parses s & w fields
; STATUS: works
parse_sw:
	mov dl, op
	mov s, dl
	shr s, 1
	and s, 01h
	
	mov w, dl
	and w, 01h
	ret
	
; appends to str_ins a word sized operand stored in ax
; STATUS: works
append_word_operand:
	push ax
	call convert_ax_to_hex_str
	mov al, [hex_word_buf + 0]
	mov ah, [hex_word_buf + 1]
	mov dl, [hex_word_buf + 2]
	mov dh, [hex_word_buf + 3]
	xor bx, bx
	mov bl, ins_iter
	mov [str_ins + bx + 0], al
	mov [str_ins + bx + 1], ah
	mov [str_ins + bx + 2], dl
	mov [str_ins + bx + 3], dh
	add ins_iter, 4
	pop ax
	ret
	
; appends to str_ins a byte sized operand stored in al
; STATUS: works
append_byte_operand:
	push ax
	call convert_al_to_hex_str
	mov al, [hex_byte_buf + 0]
	mov ah, [hex_byte_buf + 1]
	xor bx, bx
	mov bl, ins_iter
	mov [str_ins + bx + 0], al
	mov [str_ins + bx + 1], ah
	add ins_iter, 2
	pop ax
	ret

; 0011 110w bet.op.j.b. [bet.op.v.b., w = 1]
; STATUS: works
parse_cmp3:
	clc	
	call parse_dw
	; write "cmp AL"(or AX)
	mov [str_ins + 0], 'C'
	mov [str_ins + 1], 'M'
	mov [str_ins + 2], 'P'
	mov [str_ins + 3], ' '
	mov [str_ins + 4], 'A'
	mov al, 'L'
	cmp w, 1
	jne cont007
		mov al, 'X'
	cont007:
	mov [str_ins + 5], al 
	mov [str_ins + 6], ','
	mov [str_ins + 7], ' ' 
	mov ins_iter, 8
	
	cmp w, 1
	jne cont008
		; append word
		call get_word
		jc exit002
		call append_word_operand
		jmp cont009
	cont008:
		; append byte
		call get_byte
		jc exit002
		mov al, rdbyte
		call append_byte_operand
	cont009:
	call append_endl0
	ret
	exit002:
	stc
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
; STATUS: works!
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
		mov al, [str_reg + bx + 16]
		mov ah, [str_reg + bx + 17]
		mov [str_ins + 5], al
		mov [str_ins + 6], ah
		mov [str_ins + 7], 0Dh
		mov [str_ins + 8], 0Ah
		mov [str_ins + 9], 0h
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
		mov [str_ins + bx + 0], al
		mov [str_ins + bx + 1], ah
		mov [str_ins + bx + 2], dl
		mov [str_ins + bx + 3], dh
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
			cmp al, 80h
			jb pc_do_nothing1
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
		
; calls get_byte 2 times
; sets CF on failure
; stores result in AX
; STATUS: not tested	
get_word:
	clc
	call get_byte
	jc exit003
	mov al, rdbyte
	push ax
	call get_byte
	jc exit003
	pop ax
	mov ah, rdbyte
	ret
	exit003:
	stc
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

; writes byte stored in AL to 'output_handle'
; used 'output_buffer' to minimize interupt calls
; STATUS: works
write_byte:
	try_again:
	mov bx, ob_iter
	cmp buffer_cap, bx
	je flush
		mov [output_buffer + bx], al
		inc ob_iter
		ret
	flush:
		push ax
		call flush_output_buffer
		pop ax
		jmp try_again
	ret

; alternative to write_ins_str
; STATUS: works
write_ins_str:
	mov cx, 1
	xor bx, bx
	l010:
		mov al, [str_ins + bx]
		cmp al, 0h
		je b004
		
		push bx cx 
		call write_byte
		pop cx bx
		
		inc cx
		inc bx
	loop l010
	b004:
	ret

end start