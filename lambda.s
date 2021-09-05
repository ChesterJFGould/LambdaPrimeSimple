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
main$2: dq 0
section .text
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov QWORD [ RBP + 0 ] , func$15
mov RDX , RBP
mov RAX , 8
add RBP , RAX
mov QWORD [ RBP + 0 ] , func$18
mov RCX , RBP
mov RAX , 8
add RBP , RAX
mov QWORD [ RBP + 0 ] , cont$21
mov RAX , RBP
mov RBX , 8
add RBP , RBX
mov RBX , QWORD [ RDX + 0 ]
mov RDI , RDX
mov RSI , RAX
mov RDX , RCX
jmp RBX
func$15:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RCX , RSI
mov RDX , RDX
mov RAX , 3
mov RBX , QWORD [ RDX + 0 ]
mov RDI , RDX
mov RSI , RCX
mov RDX , RAX
jmp RBX
func$18:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RCX , RSI
mov RAX , RDX
mov RBX , 6
mov RAX , RAX
add RAX , RBX
mov RBX , QWORD [ RCX + 0 ]
mov RDI , RCX
mov RDX , RAX
jmp RBX
cont$21:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RAX , RDX
mov QWORD [ main$2 + 0 ] , RAX
mov RAX , QWORD [ main$2 + 0 ]
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

