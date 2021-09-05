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
	sub rbp, 8288608; Why 8288608 and not 8388608? Because 8388608 segfaults and
	                ; 8288608 doesn't, that's why.

section .data
main$2: dq 0
fib$0: dq 0
section .text
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov QWORD [ fib$0 + 0 ] , func$35
mov RCX , 25
mov QWORD [ RBP + 0 ] , cont$38
mov RAX , RBP
mov RBX , 8
add RBP , RBX
mov RBX , QWORD [ fib$0 + 0 ]
mov RDI , fib$0
mov RSI , RAX
mov RDX , RCX
jmp RBX
cont$27:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RSI , RDI
mov RBX , RDX
mov RCX , QWORD [ RSI + 8 ]
mov RAX , QWORD [ RSI + 16 ]
mov RAX , RAX
add RAX , RBX
mov RBX , QWORD [ RCX + 0 ]
mov RDI , RCX
mov RDX , RAX
jmp RBX
cont$32:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RBX , RDI
mov RDX , RDX
mov RSI , QWORD [ RBX + 8 ]
mov RAX , QWORD [ RBX + 16 ]
mov RBX , 2
mov RCX , RAX
sub RCX , RBX
mov QWORD [ RBP + 0 ] , cont$27
mov QWORD [ RBP + 8 ] , RSI
mov QWORD [ RBP + 16 ] , RDX
mov RAX , RBP
mov RBX , 24
add RBP , RBX
mov RBX , QWORD [ fib$0 + 0 ]
mov RDI , fib$0
mov RSI , RAX
mov RDX , RCX
jmp RBX
func$35:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RSI , RSI
mov RAX , RDX
mov RBX , 0
cmp RAX , RBX
je consequence$44
mov RBX , 1
cmp RAX , RBX
je consequence$43
mov RBX , 1
mov RCX , RAX
sub RCX , RBX
mov QWORD [ RBP + 0 ] , cont$32
mov QWORD [ RBP + 8 ] , RSI
mov QWORD [ RBP + 16 ] , RAX
mov RAX , RBP
mov RBX , 24
add RBP , RBX
mov RBX , QWORD [ fib$0 + 0 ]
mov RDI , fib$0
mov RSI , RAX
mov RDX , RCX
jmp RBX
consequence$43:
mov RAX , 1
mov RBX , QWORD [ RSI + 0 ]
mov RDI , RSI
mov RDX , RAX
jmp RBX
consequence$44:
mov RAX , 0
mov RBX , QWORD [ RSI + 0 ]
mov RDI , RSI
mov RDX , RAX
jmp RBX
cont$38:
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
trueLen: equ 5
true: db 'true', 0
falseLen: equ 6
false: db 'false', 0
emptyLen: equ 3
empty: db '[]', 0
nl: db 0x0a
halt: dq done

