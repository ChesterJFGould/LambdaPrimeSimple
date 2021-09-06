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
main$9: dq 0
zero$0: dq 0
zero?$2: dq 0
even?$3: dq 0
odd?$4: dq 0
boolToInt$8: dq 0
section .text
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , 0
mov QWORD [ zero$0 + 0 ] , RAX
mov QWORD [ zero?$2 + 0 ] , func$50
mov QWORD [ even?$3 + 0 ] , func$59
mov QWORD [ odd?$4 + 0 ] , func$68
mov QWORD [ boolToInt$8 + 0 ] , func$72
mov RAX , boolToInt$8
mov RCX , even?$3
mov RDX , 110
mov QWORD [ RBP + 0 ] , cont$79
mov QWORD [ RBP + 8 ] , RAX
mov RAX , RBP
mov RBX , 16
add RBP , RBX
mov RBX , QWORD [ RCX + 0 ]
mov RDI , RCX
mov RSI , RAX
mov RDX , RDX
jmp RBX
func$50:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RSI , RSI
mov RBX , RDX
mov RCX , zero$0
mov RAX , QWORD [ RCX + 0 ]
cmp RBX , RAX
je consequence$85
mov RBX , 0
mov RAX , QWORD [ RSI + 0 ]
mov RDI , RSI
mov RDX , RBX
jmp RAX
consequence$85:
mov RBX , 1
mov RAX , QWORD [ RSI + 0 ]
mov RDI , RSI
mov RDX , RBX
jmp RAX
cont$56:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RCX , RDI
mov RBX , RDX
mov RDX , QWORD [ RCX + 8 ]
mov RAX , QWORD [ RCX + 16 ]
mov RCX , 1
cmp RBX , RCX
je consequence$86
mov RBX , odd?$4
mov RCX , 1
mov RAX , RAX
sub RAX , RCX
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RSI , RDX
mov RDX , RAX
jmp RCX
consequence$86:
mov RBX , 1
mov RAX , QWORD [ RDX + 0 ]
mov RDI , RDX
mov RDX , RBX
jmp RAX
func$59:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RAX , RSI
mov RCX , RDX
mov RDX , zero?$2
mov QWORD [ RBP + 0 ] , cont$56
mov QWORD [ RBP + 8 ] , RAX
mov QWORD [ RBP + 16 ] , RCX
mov RAX , RBP
mov RBX , 24
add RBP , RBX
mov RBX , QWORD [ RDX + 0 ]
mov RDI , RDX
mov RSI , RAX
mov RDX , RCX
jmp RBX
cont$65:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RCX , RDI
mov RBX , RDX
mov RDX , QWORD [ RCX + 8 ]
mov RAX , QWORD [ RCX + 16 ]
mov RCX , 1
cmp RBX , RCX
je consequence$87
mov RBX , even?$3
mov RCX , 1
mov RAX , RAX
sub RAX , RCX
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RSI , RDX
mov RDX , RAX
jmp RCX
consequence$87:
mov RBX , 0
mov RAX , QWORD [ RDX + 0 ]
mov RDI , RDX
mov RDX , RBX
jmp RAX
func$68:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RAX , RSI
mov RCX , RDX
mov RDX , zero?$2
mov QWORD [ RBP + 0 ] , cont$65
mov QWORD [ RBP + 8 ] , RAX
mov QWORD [ RBP + 16 ] , RCX
mov RAX , RBP
mov RBX , 24
add RBP , RBX
mov RBX , QWORD [ RDX + 0 ]
mov RDI , RDX
mov RSI , RAX
mov RDX , RCX
jmp RBX
func$72:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RBX , RSI
mov RCX , RDX
mov RAX , 1
cmp RCX , RAX
je consequence$88
mov RAX , 0
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
consequence$88:
mov RAX , 1
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$75:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RAX , RDX
mov QWORD [ main$9 + 0 ] , RAX
mov RBX , main$9
mov RAX , QWORD [ RBX + 0 ]
mov RBX , QWORD [ halt + 0 ]
mov RDI , halt
mov RDX , RAX
jmp RBX
cont$79:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RDX , RDX
mov RCX , QWORD [ RAX + 8 ]
mov QWORD [ RBP + 0 ] , cont$75
mov RAX , RBP
mov RBX , 8
add RBP , RBX
mov RBX , QWORD [ RCX + 0 ]
mov RDI , RCX
mov RSI , RAX
mov RDX , RDX
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

