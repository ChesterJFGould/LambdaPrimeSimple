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
main$26: dq 0
section .text
mov RSP , RBP
mov R10 , 112
add RBP , R10
mov RAX , 10
mov R10 , 10
mov QWORD [ RSP + 104 ] , R10
mov R10 , 10
mov QWORD [ RSP + 96 ] , R10
mov R10 , 10
mov QWORD [ RSP + 88 ] , R10
mov R10 , 10
mov QWORD [ RSP + 80 ] , R10
mov R10 , 10
mov QWORD [ RSP + 72 ] , R10
mov R10 , 10
mov QWORD [ RSP + 64 ] , R10
mov R10 , 10
mov QWORD [ RSP + 56 ] , R10
mov R10 , 10
mov QWORD [ RSP + 48 ] , R10
mov R10 , 10
mov QWORD [ RSP + 40 ] , R10
mov R10 , 10
mov QWORD [ RSP + 32 ] , R10
mov R10 , 10
mov QWORD [ RSP + 24 ] , R10
mov R10 , 10
mov QWORD [ RSP + 16 ] , R10
mov R10 , 10
mov QWORD [ RSP + 8 ] , R10
mov R10 , 10
mov QWORD [ RSP + 0 ] , R10
mov R15 , 10
mov R14 , 10
mov R13 , 10
mov R12 , 10
mov R9 , 10
mov R8 , 10
mov RDI , 10
mov RSI , 10
mov RDX , 10
mov RCX , 10
mov RBX , 10
mov RAX , RAX
mov R10 , QWORD [ RSP + 104 ]
add RAX , R10
mov RAX , RAX
mov R10 , QWORD [ RSP + 96 ]
add RAX , R10
mov RAX , RAX
mov R10 , QWORD [ RSP + 88 ]
add RAX , R10
mov RAX , RAX
mov R10 , QWORD [ RSP + 80 ]
add RAX , R10
mov RAX , RAX
mov R10 , QWORD [ RSP + 72 ]
add RAX , R10
mov RAX , RAX
mov R10 , QWORD [ RSP + 64 ]
add RAX , R10
mov RAX , RAX
mov R10 , QWORD [ RSP + 56 ]
add RAX , R10
mov RAX , RAX
mov R10 , QWORD [ RSP + 48 ]
add RAX , R10
mov RAX , RAX
mov R10 , QWORD [ RSP + 40 ]
add RAX , R10
mov RAX , RAX
mov R10 , QWORD [ RSP + 32 ]
add RAX , R10
mov RAX , RAX
mov R10 , QWORD [ RSP + 24 ]
add RAX , R10
mov RAX , RAX
mov R10 , QWORD [ RSP + 16 ]
add RAX , R10
mov RAX , RAX
mov R10 , QWORD [ RSP + 8 ]
add RAX , R10
mov RAX , RAX
mov R10 , QWORD [ RSP + 0 ]
add RAX , R10
mov RAX , RAX
add RAX , R15
mov RAX , RAX
add RAX , R14
mov RAX , RAX
add RAX , R13
mov RAX , RAX
add RAX , R12
mov RAX , RAX
add RAX , R9
mov RAX , RAX
add RAX , R8
mov RAX , RAX
add RAX , RDI
mov RAX , RAX
add RAX , RSI
mov RAX , RAX
add RAX , RDX
mov RAX , RAX
add RAX , RCX
mov RAX , RAX
add RAX , RBX
mov QWORD [ main$26 + 0 ] , RAX
mov RBX , main$26
mov RAX , QWORD [ RBX + 0 ]
mov RBX , QWORD [ halt + 0 ]
mov RDI , halt
mov RDX , RAX
jmp RBX
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
nl: db 0x0a
halt: dq done

