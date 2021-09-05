done:
	mov r15, print_newline_exit
	mov rdi, rax
	jmp print_any
; Prints the given message of the given length
; RDI: Pointer <- message
; RSI: Word <- length
print:
	mov r8, rdi ; Save args
	mov r9, rsi
	mov rax, 1
	mov rdi, 1
	mov rsi, r8
	mov rdx, r9
	syscall
	jmp r15
; Prints the given char
; RDI: Native Char <- char
print_char:
	mov r8, rdi
	mov [ rbp ], rdi
	mov rax, 1
	mov rdi, 1
	mov rsi, rbp
	mov rdx, 1
	syscall
	jmp r15
; Prints first argument
; RDI: Any
print_any:
	mov r8, 0b111
	and r8, rdi
	cmp r8, 0 ; Int
	jmp print_int
; cmp r8, 1 ; Pair
; je print_pair
; cmp r8, 0b111 ; Vector
; je print_vector
; mov r8, 0xFF
; and r8, rdi
; cmp r8, 0b1110 ; True
; je print_true
; cmp r8, 0b110 ; False
; je print_false
; cmp r8, 0b10110 ; Empty
; je print_empty
; cmp r8, 0b11110 ; Void
; je print_void
; cmp r8, 0b101110 ; Char
; je print_char
; cmp r8, 0b111110 ; Error
; je print_error
; Prints an Int
; RDI: Int
print_int:
	mov [ rbp - 8], r15
	sar rdi, 3
	cmp rdi, 0
	jge print_positive
print_negative:
	mov rdi, negative_sign
	mov rsi, negative_sign_len
	mov r15, print_negative_cont
	mov [ rbp ], rdi
	sub rbp, 16
	jmp print
print_negative_cont:
	add rbp, 16
	mov rdi, [ rbp ]
	neg rdi
print_positive:
	xor rdx, rdx
	mov r8, 10
	mov rax, rdi
	div r8
	add rdx, '0'
	mov [ rbp ], rax
	sub rbp, 16
	mov rdi, rdx
	mov r15, print_positive_cont
	jmp print_char
print_positive_cont:
	add rbp, 16
	mov rax, [ rbp ]
	cmp rax, 0
	jne print_positive
	mov r15, [ rbp - 8 ]
	jmp r15
print_newline_exit:
	mov rdi, nl
	mov r15, exit
	jmp print_char
exit:
	mov rax, 60
	mov rdi, 0
	syscall

section .data

negative_sign_len: equ 1
negative_sign: db '-'
nl: equ 0x0a
