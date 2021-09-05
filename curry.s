global start
extern printf

section .text

start:
	; init heap
	mov rax, 9 ; mmap
	mov rdi, 0 ; page aligned
	mov rsi, 8388608 ; 8 MiB
	mov rdx, 3 ; rw
	mov r10, 34 ; map type = memory
	mov r8, -1 ; not a fd
	mov r9, 0 ; offset
	syscall
	mov rbp, rsp
	sub rbp, 8370000; Why 8370000 and not 8388608? Because 8388608 segfaults and
	                ; 8370000 doesn't, that's why.

section .data
main$4: dq 0
add10$3: dq 0
add$2: dq 0
section .text
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov QWORD [ add$2 + 0 ] , func$23
mov RCX , 10
mov QWORD [ RBP + 0 ] , cont$29
mov RAX , RBP
mov RBX , 8
add RBP , RBX
mov RBX , QWORD [ add$2 + 0 ]
mov RDI , add$2
mov RSI , RAX
mov RDX , RCX
jmp RBX
func$20:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RBX , RSI
mov RCX , RDX
mov RDX , QWORD [ RAX + 8 ]
mov RAX , RDX
add RAX , RCX
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
func$23:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RCX , RSI
mov RAX , RDX
mov QWORD [ RBP + 0 ] , func$20
mov QWORD [ RBP + 8 ] , RAX
mov RBX , RBP
mov RAX , 16
add RBP , RAX
mov RAX , QWORD [ RCX + 0 ]
mov RDI , RCX
mov RDX , RBX
jmp RAX
cont$26:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RAX , RDX
mov QWORD [ main$4 + 0 ] , RAX
mov RAX , QWORD [ main$4 + 0 ]
mov RBX , QWORD [ halt + 0 ]
mov RDI , halt
mov RDX , RAX
jmp RBX
cont$29:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RAX , RDX
mov QWORD [ add10$3 + 0 ] , RAX
mov RAX , QWORD [ add10$3 + 0 ]
mov RDX , 20
mov QWORD [ RBP + 0 ] , cont$26
mov RBX , RBP
mov RCX , 8
add RBP , RCX
mov RCX , QWORD [ RAX + 0 ]
mov RDI , RAX
mov RSI , RBX
mov RDX , RDX
jmp RCX
done:
	mov rax, rdx ; Arg register is rdx
print_int:
	mov r8, 10 ; we need to div by 10
	mov r10, msg ; r10 becomes pointer into msg that we traverse backwards
	add r10, len
	xor r11, r11 ; r11 becomes the counter for the string length
	cmp rax, 0
	jl rax_negative
rax_positive:
print_rax_positive:
	xor rdx, rdx
	dec r10 
	inc r11
	div r8
	add rdx, '0'
	mov [r10], dl
	cmp rax, 0
	jne print_rax_positive
	jmp print
rax_negative:
	neg rax
print_rax_negative:
	xor rdx, rdx
	dec r10 
	inc r11
	div r8
	add rdx, '0'
	mov [r10], dl
	cmp rax, 0
	jne print_rax_negative
	dec r10
	inc r11
	mov BYTE [r10], '-'
	jmp print
print:
	mov rax, 1
	mov rdi, 1
	mov rsi, r10
	mov rdx, r11
	syscall
print_newline:
	mov rax, 1
	mov rdi, 1
	mov rsi, nl
	mov rdx, 1
	syscall
exit:
	mov rax, 60
	mov rdi, 0
	syscall
error:
	mov rdi, rax
	mov rax, 60
	syscall

section .data

len: equ 20
msg: times len dw 0
trueLen: equ 5
true: db 'true', 0
falseLen: equ 6
false: db 'false', 0
emptyLen: equ 3
empty: db '[]', 0
nl: db 0x0a
halt: dq done

