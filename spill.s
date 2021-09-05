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
main$26: dq 0
section .text
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov QWORD [ RBP + 0 ] , cont$507
mov RBX , RBP
mov RAX , 8
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$132:
mov RSP , RBP
mov R10 , 120
add RBP , R10
mov R10 , RDI
mov QWORD [ RSP + 0 ] , R10
mov RAX , RDX
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 8 ]
mov QWORD [ RSP + 112 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 16 ]
mov QWORD [ RSP + 104 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 24 ]
mov QWORD [ RSP + 96 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 32 ]
mov QWORD [ RSP + 88 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 40 ]
mov QWORD [ RSP + 80 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 48 ]
mov QWORD [ RSP + 72 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 56 ]
mov QWORD [ RSP + 64 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 64 ]
mov QWORD [ RSP + 56 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 72 ]
mov QWORD [ RSP + 48 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 80 ]
mov QWORD [ RSP + 40 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 88 ]
mov QWORD [ RSP + 32 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 96 ]
mov QWORD [ RSP + 24 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 104 ]
mov QWORD [ RSP + 16 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 112 ]
mov QWORD [ RSP + 8 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R15 , QWORD [ R10 + 120 ]
mov R10 , QWORD [ RSP + 0 ]
mov R14 , QWORD [ R10 + 128 ]
mov R10 , QWORD [ RSP + 0 ]
mov R13 , QWORD [ R10 + 136 ]
mov R10 , QWORD [ RSP + 0 ]
mov R12 , QWORD [ R10 + 144 ]
mov R10 , QWORD [ RSP + 0 ]
mov R9 , QWORD [ R10 + 152 ]
mov R10 , QWORD [ RSP + 0 ]
mov R8 , QWORD [ R10 + 160 ]
mov R10 , QWORD [ RSP + 0 ]
mov RDI , QWORD [ R10 + 168 ]
mov R10 , QWORD [ RSP + 0 ]
mov RSI , QWORD [ R10 + 176 ]
mov R10 , QWORD [ RSP + 0 ]
mov RDX , QWORD [ R10 + 184 ]
mov R10 , QWORD [ RSP + 0 ]
mov RCX , QWORD [ R10 + 192 ]
mov R10 , QWORD [ RSP + 0 ]
mov RBX , QWORD [ R10 + 200 ]
mov R10 , QWORD [ RSP + 112 ]
mov R10 , R10
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 104 ]
mov R11 , QWORD [ RSP + 0 ]
add R11 , R10
mov QWORD [ RSP + 0 ] , R11
mov R10 , QWORD [ RSP + 0 ]
mov R10 , R10
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 96 ]
mov R11 , QWORD [ RSP + 0 ]
add R11 , R10
mov QWORD [ RSP + 0 ] , R11
mov R10 , QWORD [ RSP + 0 ]
mov R10 , R10
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 88 ]
mov R11 , QWORD [ RSP + 0 ]
add R11 , R10
mov QWORD [ RSP + 0 ] , R11
mov R10 , QWORD [ RSP + 0 ]
mov R10 , R10
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 80 ]
mov R11 , QWORD [ RSP + 0 ]
add R11 , R10
mov QWORD [ RSP + 0 ] , R11
mov R10 , QWORD [ RSP + 0 ]
mov R10 , R10
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 72 ]
mov R11 , QWORD [ RSP + 0 ]
add R11 , R10
mov QWORD [ RSP + 0 ] , R11
mov R10 , QWORD [ RSP + 0 ]
mov R10 , R10
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 64 ]
mov R11 , QWORD [ RSP + 0 ]
add R11 , R10
mov QWORD [ RSP + 0 ] , R11
mov R10 , QWORD [ RSP + 0 ]
mov R10 , R10
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 56 ]
mov R11 , QWORD [ RSP + 0 ]
add R11 , R10
mov QWORD [ RSP + 0 ] , R11
mov R10 , QWORD [ RSP + 0 ]
mov R10 , R10
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 48 ]
mov R11 , QWORD [ RSP + 0 ]
add R11 , R10
mov QWORD [ RSP + 0 ] , R11
mov R10 , QWORD [ RSP + 0 ]
mov R10 , R10
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 40 ]
mov R11 , QWORD [ RSP + 0 ]
add R11 , R10
mov QWORD [ RSP + 0 ] , R11
mov R10 , QWORD [ RSP + 0 ]
mov R10 , R10
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 32 ]
mov R11 , QWORD [ RSP + 0 ]
add R11 , R10
mov QWORD [ RSP + 0 ] , R11
mov R10 , QWORD [ RSP + 0 ]
mov R10 , R10
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 24 ]
mov R11 , QWORD [ RSP + 0 ]
add R11 , R10
mov QWORD [ RSP + 0 ] , R11
mov R10 , QWORD [ RSP + 0 ]
mov R10 , R10
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 16 ]
mov R11 , QWORD [ RSP + 0 ]
add R11 , R10
mov QWORD [ RSP + 0 ] , R11
mov R10 , QWORD [ RSP + 0 ]
mov R10 , R10
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 8 ]
mov R11 , QWORD [ RSP + 0 ]
add R11 , R10
mov QWORD [ RSP + 0 ] , R11
mov R10 , QWORD [ RSP + 0 ]
mov R10 , R10
mov QWORD [ RSP + 0 ] , R10
mov R11 , QWORD [ RSP + 0 ]
add R11 , R15
mov QWORD [ RSP + 0 ] , R11
mov R10 , QWORD [ RSP + 0 ]
mov R15 , R10
add R15 , R14
mov R14 , R15
add R14 , R13
mov R13 , R14
add R13 , R12
mov R12 , R13
add R12 , R9
mov R9 , R12
add R9 , R8
mov R8 , R9
add R8 , RDI
mov RDI , R8
add RDI , RSI
mov RSI , RDI
add RSI , RDX
mov RDX , RSI
add RDX , RCX
mov RCX , RDX
add RCX , RBX
mov RBX , RCX
add RBX , RAX
mov QWORD [ main$26 + 0 ] , RBX
mov RAX , QWORD [ main$26 + 0 ]
mov RBX , QWORD [ halt + 0 ]
mov RDI , halt
mov RDX , RAX
jmp RBX
cont$159:
mov RSP , RBP
mov R10 , 112
add RBP , R10
mov R10 , RDI
mov QWORD [ RSP + 64 ] , R10
mov RAX , RDX
mov R10 , QWORD [ RSP + 64 ]
mov R10 , QWORD [ R10 + 8 ]
mov QWORD [ RSP + 104 ] , R10
mov R10 , QWORD [ RSP + 64 ]
mov R10 , QWORD [ R10 + 16 ]
mov QWORD [ RSP + 96 ] , R10
mov R10 , QWORD [ RSP + 64 ]
mov R10 , QWORD [ R10 + 24 ]
mov QWORD [ RSP + 88 ] , R10
mov R10 , QWORD [ RSP + 64 ]
mov R10 , QWORD [ R10 + 32 ]
mov QWORD [ RSP + 80 ] , R10
mov R10 , QWORD [ RSP + 64 ]
mov R10 , QWORD [ R10 + 40 ]
mov QWORD [ RSP + 72 ] , R10
mov R10 , QWORD [ RSP + 64 ]
mov R10 , QWORD [ R10 + 48 ]
mov QWORD [ RSP + 56 ] , R10
mov R10 , QWORD [ RSP + 64 ]
mov R10 , QWORD [ R10 + 56 ]
mov QWORD [ RSP + 48 ] , R10
mov R10 , QWORD [ RSP + 64 ]
mov R10 , QWORD [ R10 + 64 ]
mov QWORD [ RSP + 40 ] , R10
mov R10 , QWORD [ RSP + 64 ]
mov R10 , QWORD [ R10 + 72 ]
mov QWORD [ RSP + 32 ] , R10
mov R10 , QWORD [ RSP + 64 ]
mov R10 , QWORD [ R10 + 80 ]
mov QWORD [ RSP + 24 ] , R10
mov R10 , QWORD [ RSP + 64 ]
mov R10 , QWORD [ R10 + 88 ]
mov QWORD [ RSP + 16 ] , R10
mov R10 , QWORD [ RSP + 64 ]
mov R10 , QWORD [ R10 + 96 ]
mov QWORD [ RSP + 8 ] , R10
mov R10 , QWORD [ RSP + 64 ]
mov R10 , QWORD [ R10 + 104 ]
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 64 ]
mov R15 , QWORD [ R10 + 112 ]
mov R10 , QWORD [ RSP + 64 ]
mov R14 , QWORD [ R10 + 120 ]
mov R10 , QWORD [ RSP + 64 ]
mov R13 , QWORD [ R10 + 128 ]
mov R10 , QWORD [ RSP + 64 ]
mov R12 , QWORD [ R10 + 136 ]
mov R10 , QWORD [ RSP + 64 ]
mov R9 , QWORD [ R10 + 144 ]
mov R10 , QWORD [ RSP + 64 ]
mov R8 , QWORD [ R10 + 152 ]
mov R10 , QWORD [ RSP + 64 ]
mov RDI , QWORD [ R10 + 160 ]
mov R10 , QWORD [ RSP + 64 ]
mov RSI , QWORD [ R10 + 168 ]
mov R10 , QWORD [ RSP + 64 ]
mov RDX , QWORD [ R10 + 176 ]
mov R10 , QWORD [ RSP + 64 ]
mov RCX , QWORD [ R10 + 184 ]
mov R10 , QWORD [ RSP + 64 ]
mov RBX , QWORD [ R10 + 192 ]
mov QWORD [ RBP + 0 ] , cont$132
mov R11 , QWORD [ RSP + 104 ]
mov QWORD [ RBP + 8 ] , R11
mov R11 , QWORD [ RSP + 96 ]
mov QWORD [ RBP + 16 ] , R11
mov R11 , QWORD [ RSP + 88 ]
mov QWORD [ RBP + 24 ] , R11
mov R11 , QWORD [ RSP + 80 ]
mov QWORD [ RBP + 32 ] , R11
mov R11 , QWORD [ RSP + 72 ]
mov QWORD [ RBP + 40 ] , R11
mov R11 , QWORD [ RSP + 56 ]
mov QWORD [ RBP + 48 ] , R11
mov R11 , QWORD [ RSP + 48 ]
mov QWORD [ RBP + 56 ] , R11
mov R11 , QWORD [ RSP + 40 ]
mov QWORD [ RBP + 64 ] , R11
mov R11 , QWORD [ RSP + 32 ]
mov QWORD [ RBP + 72 ] , R11
mov R11 , QWORD [ RSP + 24 ]
mov QWORD [ RBP + 80 ] , R11
mov R11 , QWORD [ RSP + 16 ]
mov QWORD [ RBP + 88 ] , R11
mov R11 , QWORD [ RSP + 8 ]
mov QWORD [ RBP + 96 ] , R11
mov R11 , QWORD [ RSP + 0 ]
mov QWORD [ RBP + 104 ] , R11
mov QWORD [ RBP + 112 ] , R15
mov QWORD [ RBP + 120 ] , R14
mov QWORD [ RBP + 128 ] , R13
mov QWORD [ RBP + 136 ] , R12
mov QWORD [ RBP + 144 ] , R9
mov QWORD [ RBP + 152 ] , R8
mov QWORD [ RBP + 160 ] , RDI
mov QWORD [ RBP + 168 ] , RSI
mov QWORD [ RBP + 176 ] , RDX
mov QWORD [ RBP + 184 ] , RCX
mov QWORD [ RBP + 192 ] , RBX
mov QWORD [ RBP + 200 ] , RAX
mov RBX , RBP
mov RAX , 208
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$185:
mov RSP , RBP
mov R10 , 104
add RBP , R10
mov R10 , RDI
mov QWORD [ RSP + 56 ] , R10
mov RAX , RDX
mov R10 , QWORD [ RSP + 56 ]
mov R10 , QWORD [ R10 + 8 ]
mov QWORD [ RSP + 96 ] , R10
mov R10 , QWORD [ RSP + 56 ]
mov R10 , QWORD [ R10 + 16 ]
mov QWORD [ RSP + 88 ] , R10
mov R10 , QWORD [ RSP + 56 ]
mov R10 , QWORD [ R10 + 24 ]
mov QWORD [ RSP + 80 ] , R10
mov R10 , QWORD [ RSP + 56 ]
mov R10 , QWORD [ R10 + 32 ]
mov QWORD [ RSP + 72 ] , R10
mov R10 , QWORD [ RSP + 56 ]
mov R10 , QWORD [ R10 + 40 ]
mov QWORD [ RSP + 64 ] , R10
mov R10 , QWORD [ RSP + 56 ]
mov R10 , QWORD [ R10 + 48 ]
mov QWORD [ RSP + 48 ] , R10
mov R10 , QWORD [ RSP + 56 ]
mov R10 , QWORD [ R10 + 56 ]
mov QWORD [ RSP + 40 ] , R10
mov R10 , QWORD [ RSP + 56 ]
mov R10 , QWORD [ R10 + 64 ]
mov QWORD [ RSP + 32 ] , R10
mov R10 , QWORD [ RSP + 56 ]
mov R10 , QWORD [ R10 + 72 ]
mov QWORD [ RSP + 24 ] , R10
mov R10 , QWORD [ RSP + 56 ]
mov R10 , QWORD [ R10 + 80 ]
mov QWORD [ RSP + 16 ] , R10
mov R10 , QWORD [ RSP + 56 ]
mov R10 , QWORD [ R10 + 88 ]
mov QWORD [ RSP + 8 ] , R10
mov R10 , QWORD [ RSP + 56 ]
mov R10 , QWORD [ R10 + 96 ]
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 56 ]
mov R15 , QWORD [ R10 + 104 ]
mov R10 , QWORD [ RSP + 56 ]
mov R14 , QWORD [ R10 + 112 ]
mov R10 , QWORD [ RSP + 56 ]
mov R13 , QWORD [ R10 + 120 ]
mov R10 , QWORD [ RSP + 56 ]
mov R12 , QWORD [ R10 + 128 ]
mov R10 , QWORD [ RSP + 56 ]
mov R9 , QWORD [ R10 + 136 ]
mov R10 , QWORD [ RSP + 56 ]
mov R8 , QWORD [ R10 + 144 ]
mov R10 , QWORD [ RSP + 56 ]
mov RDI , QWORD [ R10 + 152 ]
mov R10 , QWORD [ RSP + 56 ]
mov RSI , QWORD [ R10 + 160 ]
mov R10 , QWORD [ RSP + 56 ]
mov RDX , QWORD [ R10 + 168 ]
mov R10 , QWORD [ RSP + 56 ]
mov RCX , QWORD [ R10 + 176 ]
mov R10 , QWORD [ RSP + 56 ]
mov RBX , QWORD [ R10 + 184 ]
mov QWORD [ RBP + 0 ] , cont$159
mov R11 , QWORD [ RSP + 96 ]
mov QWORD [ RBP + 8 ] , R11
mov R11 , QWORD [ RSP + 88 ]
mov QWORD [ RBP + 16 ] , R11
mov R11 , QWORD [ RSP + 80 ]
mov QWORD [ RBP + 24 ] , R11
mov R11 , QWORD [ RSP + 72 ]
mov QWORD [ RBP + 32 ] , R11
mov R11 , QWORD [ RSP + 64 ]
mov QWORD [ RBP + 40 ] , R11
mov R11 , QWORD [ RSP + 48 ]
mov QWORD [ RBP + 48 ] , R11
mov R11 , QWORD [ RSP + 40 ]
mov QWORD [ RBP + 56 ] , R11
mov R11 , QWORD [ RSP + 32 ]
mov QWORD [ RBP + 64 ] , R11
mov R11 , QWORD [ RSP + 24 ]
mov QWORD [ RBP + 72 ] , R11
mov R11 , QWORD [ RSP + 16 ]
mov QWORD [ RBP + 80 ] , R11
mov R11 , QWORD [ RSP + 8 ]
mov QWORD [ RBP + 88 ] , R11
mov R11 , QWORD [ RSP + 0 ]
mov QWORD [ RBP + 96 ] , R11
mov QWORD [ RBP + 104 ] , R15
mov QWORD [ RBP + 112 ] , R14
mov QWORD [ RBP + 120 ] , R13
mov QWORD [ RBP + 128 ] , R12
mov QWORD [ RBP + 136 ] , R9
mov QWORD [ RBP + 144 ] , R8
mov QWORD [ RBP + 152 ] , RDI
mov QWORD [ RBP + 160 ] , RSI
mov QWORD [ RBP + 168 ] , RDX
mov QWORD [ RBP + 176 ] , RCX
mov QWORD [ RBP + 184 ] , RBX
mov QWORD [ RBP + 192 ] , RAX
mov RBX , RBP
mov RAX , 200
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$210:
mov RSP , RBP
mov R10 , 96
add RBP , R10
mov R10 , RDI
mov QWORD [ RSP + 48 ] , R10
mov RAX , RDX
mov R10 , QWORD [ RSP + 48 ]
mov R10 , QWORD [ R10 + 8 ]
mov QWORD [ RSP + 88 ] , R10
mov R10 , QWORD [ RSP + 48 ]
mov R10 , QWORD [ R10 + 16 ]
mov QWORD [ RSP + 80 ] , R10
mov R10 , QWORD [ RSP + 48 ]
mov R10 , QWORD [ R10 + 24 ]
mov QWORD [ RSP + 72 ] , R10
mov R10 , QWORD [ RSP + 48 ]
mov R10 , QWORD [ R10 + 32 ]
mov QWORD [ RSP + 64 ] , R10
mov R10 , QWORD [ RSP + 48 ]
mov R10 , QWORD [ R10 + 40 ]
mov QWORD [ RSP + 56 ] , R10
mov R10 , QWORD [ RSP + 48 ]
mov R10 , QWORD [ R10 + 48 ]
mov QWORD [ RSP + 40 ] , R10
mov R10 , QWORD [ RSP + 48 ]
mov R10 , QWORD [ R10 + 56 ]
mov QWORD [ RSP + 32 ] , R10
mov R10 , QWORD [ RSP + 48 ]
mov R10 , QWORD [ R10 + 64 ]
mov QWORD [ RSP + 24 ] , R10
mov R10 , QWORD [ RSP + 48 ]
mov R10 , QWORD [ R10 + 72 ]
mov QWORD [ RSP + 16 ] , R10
mov R10 , QWORD [ RSP + 48 ]
mov R10 , QWORD [ R10 + 80 ]
mov QWORD [ RSP + 8 ] , R10
mov R10 , QWORD [ RSP + 48 ]
mov R10 , QWORD [ R10 + 88 ]
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 48 ]
mov R15 , QWORD [ R10 + 96 ]
mov R10 , QWORD [ RSP + 48 ]
mov R14 , QWORD [ R10 + 104 ]
mov R10 , QWORD [ RSP + 48 ]
mov R13 , QWORD [ R10 + 112 ]
mov R10 , QWORD [ RSP + 48 ]
mov R12 , QWORD [ R10 + 120 ]
mov R10 , QWORD [ RSP + 48 ]
mov R9 , QWORD [ R10 + 128 ]
mov R10 , QWORD [ RSP + 48 ]
mov R8 , QWORD [ R10 + 136 ]
mov R10 , QWORD [ RSP + 48 ]
mov RDI , QWORD [ R10 + 144 ]
mov R10 , QWORD [ RSP + 48 ]
mov RSI , QWORD [ R10 + 152 ]
mov R10 , QWORD [ RSP + 48 ]
mov RDX , QWORD [ R10 + 160 ]
mov R10 , QWORD [ RSP + 48 ]
mov RCX , QWORD [ R10 + 168 ]
mov R10 , QWORD [ RSP + 48 ]
mov RBX , QWORD [ R10 + 176 ]
mov QWORD [ RBP + 0 ] , cont$185
mov R11 , QWORD [ RSP + 88 ]
mov QWORD [ RBP + 8 ] , R11
mov R11 , QWORD [ RSP + 80 ]
mov QWORD [ RBP + 16 ] , R11
mov R11 , QWORD [ RSP + 72 ]
mov QWORD [ RBP + 24 ] , R11
mov R11 , QWORD [ RSP + 64 ]
mov QWORD [ RBP + 32 ] , R11
mov R11 , QWORD [ RSP + 56 ]
mov QWORD [ RBP + 40 ] , R11
mov R11 , QWORD [ RSP + 40 ]
mov QWORD [ RBP + 48 ] , R11
mov R11 , QWORD [ RSP + 32 ]
mov QWORD [ RBP + 56 ] , R11
mov R11 , QWORD [ RSP + 24 ]
mov QWORD [ RBP + 64 ] , R11
mov R11 , QWORD [ RSP + 16 ]
mov QWORD [ RBP + 72 ] , R11
mov R11 , QWORD [ RSP + 8 ]
mov QWORD [ RBP + 80 ] , R11
mov R11 , QWORD [ RSP + 0 ]
mov QWORD [ RBP + 88 ] , R11
mov QWORD [ RBP + 96 ] , R15
mov QWORD [ RBP + 104 ] , R14
mov QWORD [ RBP + 112 ] , R13
mov QWORD [ RBP + 120 ] , R12
mov QWORD [ RBP + 128 ] , R9
mov QWORD [ RBP + 136 ] , R8
mov QWORD [ RBP + 144 ] , RDI
mov QWORD [ RBP + 152 ] , RSI
mov QWORD [ RBP + 160 ] , RDX
mov QWORD [ RBP + 168 ] , RCX
mov QWORD [ RBP + 176 ] , RBX
mov QWORD [ RBP + 184 ] , RAX
mov RBX , RBP
mov RAX , 192
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$234:
mov RSP , RBP
mov R10 , 88
add RBP , R10
mov R10 , RDI
mov QWORD [ RSP + 40 ] , R10
mov RAX , RDX
mov R10 , QWORD [ RSP + 40 ]
mov R10 , QWORD [ R10 + 8 ]
mov QWORD [ RSP + 80 ] , R10
mov R10 , QWORD [ RSP + 40 ]
mov R10 , QWORD [ R10 + 16 ]
mov QWORD [ RSP + 72 ] , R10
mov R10 , QWORD [ RSP + 40 ]
mov R10 , QWORD [ R10 + 24 ]
mov QWORD [ RSP + 64 ] , R10
mov R10 , QWORD [ RSP + 40 ]
mov R10 , QWORD [ R10 + 32 ]
mov QWORD [ RSP + 56 ] , R10
mov R10 , QWORD [ RSP + 40 ]
mov R10 , QWORD [ R10 + 40 ]
mov QWORD [ RSP + 48 ] , R10
mov R10 , QWORD [ RSP + 40 ]
mov R10 , QWORD [ R10 + 48 ]
mov QWORD [ RSP + 32 ] , R10
mov R10 , QWORD [ RSP + 40 ]
mov R10 , QWORD [ R10 + 56 ]
mov QWORD [ RSP + 24 ] , R10
mov R10 , QWORD [ RSP + 40 ]
mov R10 , QWORD [ R10 + 64 ]
mov QWORD [ RSP + 16 ] , R10
mov R10 , QWORD [ RSP + 40 ]
mov R10 , QWORD [ R10 + 72 ]
mov QWORD [ RSP + 8 ] , R10
mov R10 , QWORD [ RSP + 40 ]
mov R10 , QWORD [ R10 + 80 ]
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 40 ]
mov R15 , QWORD [ R10 + 88 ]
mov R10 , QWORD [ RSP + 40 ]
mov R14 , QWORD [ R10 + 96 ]
mov R10 , QWORD [ RSP + 40 ]
mov R13 , QWORD [ R10 + 104 ]
mov R10 , QWORD [ RSP + 40 ]
mov R12 , QWORD [ R10 + 112 ]
mov R10 , QWORD [ RSP + 40 ]
mov R9 , QWORD [ R10 + 120 ]
mov R10 , QWORD [ RSP + 40 ]
mov R8 , QWORD [ R10 + 128 ]
mov R10 , QWORD [ RSP + 40 ]
mov RDI , QWORD [ R10 + 136 ]
mov R10 , QWORD [ RSP + 40 ]
mov RSI , QWORD [ R10 + 144 ]
mov R10 , QWORD [ RSP + 40 ]
mov RDX , QWORD [ R10 + 152 ]
mov R10 , QWORD [ RSP + 40 ]
mov RCX , QWORD [ R10 + 160 ]
mov R10 , QWORD [ RSP + 40 ]
mov RBX , QWORD [ R10 + 168 ]
mov QWORD [ RBP + 0 ] , cont$210
mov R11 , QWORD [ RSP + 80 ]
mov QWORD [ RBP + 8 ] , R11
mov R11 , QWORD [ RSP + 72 ]
mov QWORD [ RBP + 16 ] , R11
mov R11 , QWORD [ RSP + 64 ]
mov QWORD [ RBP + 24 ] , R11
mov R11 , QWORD [ RSP + 56 ]
mov QWORD [ RBP + 32 ] , R11
mov R11 , QWORD [ RSP + 48 ]
mov QWORD [ RBP + 40 ] , R11
mov R11 , QWORD [ RSP + 32 ]
mov QWORD [ RBP + 48 ] , R11
mov R11 , QWORD [ RSP + 24 ]
mov QWORD [ RBP + 56 ] , R11
mov R11 , QWORD [ RSP + 16 ]
mov QWORD [ RBP + 64 ] , R11
mov R11 , QWORD [ RSP + 8 ]
mov QWORD [ RBP + 72 ] , R11
mov R11 , QWORD [ RSP + 0 ]
mov QWORD [ RBP + 80 ] , R11
mov QWORD [ RBP + 88 ] , R15
mov QWORD [ RBP + 96 ] , R14
mov QWORD [ RBP + 104 ] , R13
mov QWORD [ RBP + 112 ] , R12
mov QWORD [ RBP + 120 ] , R9
mov QWORD [ RBP + 128 ] , R8
mov QWORD [ RBP + 136 ] , RDI
mov QWORD [ RBP + 144 ] , RSI
mov QWORD [ RBP + 152 ] , RDX
mov QWORD [ RBP + 160 ] , RCX
mov QWORD [ RBP + 168 ] , RBX
mov QWORD [ RBP + 176 ] , RAX
mov RBX , RBP
mov RAX , 184
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$257:
mov RSP , RBP
mov R10 , 80
add RBP , R10
mov R10 , RDI
mov QWORD [ RSP + 32 ] , R10
mov RAX , RDX
mov R10 , QWORD [ RSP + 32 ]
mov R10 , QWORD [ R10 + 8 ]
mov QWORD [ RSP + 72 ] , R10
mov R10 , QWORD [ RSP + 32 ]
mov R10 , QWORD [ R10 + 16 ]
mov QWORD [ RSP + 64 ] , R10
mov R10 , QWORD [ RSP + 32 ]
mov R10 , QWORD [ R10 + 24 ]
mov QWORD [ RSP + 56 ] , R10
mov R10 , QWORD [ RSP + 32 ]
mov R10 , QWORD [ R10 + 32 ]
mov QWORD [ RSP + 48 ] , R10
mov R10 , QWORD [ RSP + 32 ]
mov R10 , QWORD [ R10 + 40 ]
mov QWORD [ RSP + 40 ] , R10
mov R10 , QWORD [ RSP + 32 ]
mov R10 , QWORD [ R10 + 48 ]
mov QWORD [ RSP + 24 ] , R10
mov R10 , QWORD [ RSP + 32 ]
mov R10 , QWORD [ R10 + 56 ]
mov QWORD [ RSP + 16 ] , R10
mov R10 , QWORD [ RSP + 32 ]
mov R10 , QWORD [ R10 + 64 ]
mov QWORD [ RSP + 8 ] , R10
mov R10 , QWORD [ RSP + 32 ]
mov R10 , QWORD [ R10 + 72 ]
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 32 ]
mov R15 , QWORD [ R10 + 80 ]
mov R10 , QWORD [ RSP + 32 ]
mov R14 , QWORD [ R10 + 88 ]
mov R10 , QWORD [ RSP + 32 ]
mov R13 , QWORD [ R10 + 96 ]
mov R10 , QWORD [ RSP + 32 ]
mov R12 , QWORD [ R10 + 104 ]
mov R10 , QWORD [ RSP + 32 ]
mov R9 , QWORD [ R10 + 112 ]
mov R10 , QWORD [ RSP + 32 ]
mov R8 , QWORD [ R10 + 120 ]
mov R10 , QWORD [ RSP + 32 ]
mov RDI , QWORD [ R10 + 128 ]
mov R10 , QWORD [ RSP + 32 ]
mov RSI , QWORD [ R10 + 136 ]
mov R10 , QWORD [ RSP + 32 ]
mov RDX , QWORD [ R10 + 144 ]
mov R10 , QWORD [ RSP + 32 ]
mov RCX , QWORD [ R10 + 152 ]
mov R10 , QWORD [ RSP + 32 ]
mov RBX , QWORD [ R10 + 160 ]
mov QWORD [ RBP + 0 ] , cont$234
mov R11 , QWORD [ RSP + 72 ]
mov QWORD [ RBP + 8 ] , R11
mov R11 , QWORD [ RSP + 64 ]
mov QWORD [ RBP + 16 ] , R11
mov R11 , QWORD [ RSP + 56 ]
mov QWORD [ RBP + 24 ] , R11
mov R11 , QWORD [ RSP + 48 ]
mov QWORD [ RBP + 32 ] , R11
mov R11 , QWORD [ RSP + 40 ]
mov QWORD [ RBP + 40 ] , R11
mov R11 , QWORD [ RSP + 24 ]
mov QWORD [ RBP + 48 ] , R11
mov R11 , QWORD [ RSP + 16 ]
mov QWORD [ RBP + 56 ] , R11
mov R11 , QWORD [ RSP + 8 ]
mov QWORD [ RBP + 64 ] , R11
mov R11 , QWORD [ RSP + 0 ]
mov QWORD [ RBP + 72 ] , R11
mov QWORD [ RBP + 80 ] , R15
mov QWORD [ RBP + 88 ] , R14
mov QWORD [ RBP + 96 ] , R13
mov QWORD [ RBP + 104 ] , R12
mov QWORD [ RBP + 112 ] , R9
mov QWORD [ RBP + 120 ] , R8
mov QWORD [ RBP + 128 ] , RDI
mov QWORD [ RBP + 136 ] , RSI
mov QWORD [ RBP + 144 ] , RDX
mov QWORD [ RBP + 152 ] , RCX
mov QWORD [ RBP + 160 ] , RBX
mov QWORD [ RBP + 168 ] , RAX
mov RBX , RBP
mov RAX , 176
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$279:
mov RSP , RBP
mov R10 , 72
add RBP , R10
mov R10 , RDI
mov QWORD [ RSP + 24 ] , R10
mov RAX , RDX
mov R10 , QWORD [ RSP + 24 ]
mov R10 , QWORD [ R10 + 8 ]
mov QWORD [ RSP + 64 ] , R10
mov R10 , QWORD [ RSP + 24 ]
mov R10 , QWORD [ R10 + 16 ]
mov QWORD [ RSP + 56 ] , R10
mov R10 , QWORD [ RSP + 24 ]
mov R10 , QWORD [ R10 + 24 ]
mov QWORD [ RSP + 48 ] , R10
mov R10 , QWORD [ RSP + 24 ]
mov R10 , QWORD [ R10 + 32 ]
mov QWORD [ RSP + 40 ] , R10
mov R10 , QWORD [ RSP + 24 ]
mov R10 , QWORD [ R10 + 40 ]
mov QWORD [ RSP + 32 ] , R10
mov R10 , QWORD [ RSP + 24 ]
mov R10 , QWORD [ R10 + 48 ]
mov QWORD [ RSP + 16 ] , R10
mov R10 , QWORD [ RSP + 24 ]
mov R10 , QWORD [ R10 + 56 ]
mov QWORD [ RSP + 8 ] , R10
mov R10 , QWORD [ RSP + 24 ]
mov R10 , QWORD [ R10 + 64 ]
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 24 ]
mov R15 , QWORD [ R10 + 72 ]
mov R10 , QWORD [ RSP + 24 ]
mov R14 , QWORD [ R10 + 80 ]
mov R10 , QWORD [ RSP + 24 ]
mov R13 , QWORD [ R10 + 88 ]
mov R10 , QWORD [ RSP + 24 ]
mov R12 , QWORD [ R10 + 96 ]
mov R10 , QWORD [ RSP + 24 ]
mov R9 , QWORD [ R10 + 104 ]
mov R10 , QWORD [ RSP + 24 ]
mov R8 , QWORD [ R10 + 112 ]
mov R10 , QWORD [ RSP + 24 ]
mov RDI , QWORD [ R10 + 120 ]
mov R10 , QWORD [ RSP + 24 ]
mov RSI , QWORD [ R10 + 128 ]
mov R10 , QWORD [ RSP + 24 ]
mov RDX , QWORD [ R10 + 136 ]
mov R10 , QWORD [ RSP + 24 ]
mov RCX , QWORD [ R10 + 144 ]
mov R10 , QWORD [ RSP + 24 ]
mov RBX , QWORD [ R10 + 152 ]
mov QWORD [ RBP + 0 ] , cont$257
mov R11 , QWORD [ RSP + 64 ]
mov QWORD [ RBP + 8 ] , R11
mov R11 , QWORD [ RSP + 56 ]
mov QWORD [ RBP + 16 ] , R11
mov R11 , QWORD [ RSP + 48 ]
mov QWORD [ RBP + 24 ] , R11
mov R11 , QWORD [ RSP + 40 ]
mov QWORD [ RBP + 32 ] , R11
mov R11 , QWORD [ RSP + 32 ]
mov QWORD [ RBP + 40 ] , R11
mov R11 , QWORD [ RSP + 16 ]
mov QWORD [ RBP + 48 ] , R11
mov R11 , QWORD [ RSP + 8 ]
mov QWORD [ RBP + 56 ] , R11
mov R11 , QWORD [ RSP + 0 ]
mov QWORD [ RBP + 64 ] , R11
mov QWORD [ RBP + 72 ] , R15
mov QWORD [ RBP + 80 ] , R14
mov QWORD [ RBP + 88 ] , R13
mov QWORD [ RBP + 96 ] , R12
mov QWORD [ RBP + 104 ] , R9
mov QWORD [ RBP + 112 ] , R8
mov QWORD [ RBP + 120 ] , RDI
mov QWORD [ RBP + 128 ] , RSI
mov QWORD [ RBP + 136 ] , RDX
mov QWORD [ RBP + 144 ] , RCX
mov QWORD [ RBP + 152 ] , RBX
mov QWORD [ RBP + 160 ] , RAX
mov RBX , RBP
mov RAX , 168
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$300:
mov RSP , RBP
mov R10 , 64
add RBP , R10
mov R10 , RDI
mov QWORD [ RSP + 16 ] , R10
mov RAX , RDX
mov R10 , QWORD [ RSP + 16 ]
mov R10 , QWORD [ R10 + 8 ]
mov QWORD [ RSP + 56 ] , R10
mov R10 , QWORD [ RSP + 16 ]
mov R10 , QWORD [ R10 + 16 ]
mov QWORD [ RSP + 48 ] , R10
mov R10 , QWORD [ RSP + 16 ]
mov R10 , QWORD [ R10 + 24 ]
mov QWORD [ RSP + 40 ] , R10
mov R10 , QWORD [ RSP + 16 ]
mov R10 , QWORD [ R10 + 32 ]
mov QWORD [ RSP + 32 ] , R10
mov R10 , QWORD [ RSP + 16 ]
mov R10 , QWORD [ R10 + 40 ]
mov QWORD [ RSP + 24 ] , R10
mov R10 , QWORD [ RSP + 16 ]
mov R10 , QWORD [ R10 + 48 ]
mov QWORD [ RSP + 8 ] , R10
mov R10 , QWORD [ RSP + 16 ]
mov R10 , QWORD [ R10 + 56 ]
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 16 ]
mov R15 , QWORD [ R10 + 64 ]
mov R10 , QWORD [ RSP + 16 ]
mov R14 , QWORD [ R10 + 72 ]
mov R10 , QWORD [ RSP + 16 ]
mov R13 , QWORD [ R10 + 80 ]
mov R10 , QWORD [ RSP + 16 ]
mov R12 , QWORD [ R10 + 88 ]
mov R10 , QWORD [ RSP + 16 ]
mov R9 , QWORD [ R10 + 96 ]
mov R10 , QWORD [ RSP + 16 ]
mov R8 , QWORD [ R10 + 104 ]
mov R10 , QWORD [ RSP + 16 ]
mov RDI , QWORD [ R10 + 112 ]
mov R10 , QWORD [ RSP + 16 ]
mov RSI , QWORD [ R10 + 120 ]
mov R10 , QWORD [ RSP + 16 ]
mov RDX , QWORD [ R10 + 128 ]
mov R10 , QWORD [ RSP + 16 ]
mov RCX , QWORD [ R10 + 136 ]
mov R10 , QWORD [ RSP + 16 ]
mov RBX , QWORD [ R10 + 144 ]
mov QWORD [ RBP + 0 ] , cont$279
mov R11 , QWORD [ RSP + 56 ]
mov QWORD [ RBP + 8 ] , R11
mov R11 , QWORD [ RSP + 48 ]
mov QWORD [ RBP + 16 ] , R11
mov R11 , QWORD [ RSP + 40 ]
mov QWORD [ RBP + 24 ] , R11
mov R11 , QWORD [ RSP + 32 ]
mov QWORD [ RBP + 32 ] , R11
mov R11 , QWORD [ RSP + 24 ]
mov QWORD [ RBP + 40 ] , R11
mov R11 , QWORD [ RSP + 8 ]
mov QWORD [ RBP + 48 ] , R11
mov R11 , QWORD [ RSP + 0 ]
mov QWORD [ RBP + 56 ] , R11
mov QWORD [ RBP + 64 ] , R15
mov QWORD [ RBP + 72 ] , R14
mov QWORD [ RBP + 80 ] , R13
mov QWORD [ RBP + 88 ] , R12
mov QWORD [ RBP + 96 ] , R9
mov QWORD [ RBP + 104 ] , R8
mov QWORD [ RBP + 112 ] , RDI
mov QWORD [ RBP + 120 ] , RSI
mov QWORD [ RBP + 128 ] , RDX
mov QWORD [ RBP + 136 ] , RCX
mov QWORD [ RBP + 144 ] , RBX
mov QWORD [ RBP + 152 ] , RAX
mov RBX , RBP
mov RAX , 160
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$320:
mov RSP , RBP
mov R10 , 56
add RBP , R10
mov R10 , RDI
mov QWORD [ RSP + 8 ] , R10
mov RAX , RDX
mov R10 , QWORD [ RSP + 8 ]
mov R10 , QWORD [ R10 + 8 ]
mov QWORD [ RSP + 48 ] , R10
mov R10 , QWORD [ RSP + 8 ]
mov R10 , QWORD [ R10 + 16 ]
mov QWORD [ RSP + 40 ] , R10
mov R10 , QWORD [ RSP + 8 ]
mov R10 , QWORD [ R10 + 24 ]
mov QWORD [ RSP + 32 ] , R10
mov R10 , QWORD [ RSP + 8 ]
mov R10 , QWORD [ R10 + 32 ]
mov QWORD [ RSP + 24 ] , R10
mov R10 , QWORD [ RSP + 8 ]
mov R10 , QWORD [ R10 + 40 ]
mov QWORD [ RSP + 16 ] , R10
mov R10 , QWORD [ RSP + 8 ]
mov R10 , QWORD [ R10 + 48 ]
mov QWORD [ RSP + 0 ] , R10
mov R10 , QWORD [ RSP + 8 ]
mov R15 , QWORD [ R10 + 56 ]
mov R10 , QWORD [ RSP + 8 ]
mov R14 , QWORD [ R10 + 64 ]
mov R10 , QWORD [ RSP + 8 ]
mov R13 , QWORD [ R10 + 72 ]
mov R10 , QWORD [ RSP + 8 ]
mov R12 , QWORD [ R10 + 80 ]
mov R10 , QWORD [ RSP + 8 ]
mov R9 , QWORD [ R10 + 88 ]
mov R10 , QWORD [ RSP + 8 ]
mov R8 , QWORD [ R10 + 96 ]
mov R10 , QWORD [ RSP + 8 ]
mov RDI , QWORD [ R10 + 104 ]
mov R10 , QWORD [ RSP + 8 ]
mov RSI , QWORD [ R10 + 112 ]
mov R10 , QWORD [ RSP + 8 ]
mov RDX , QWORD [ R10 + 120 ]
mov R10 , QWORD [ RSP + 8 ]
mov RCX , QWORD [ R10 + 128 ]
mov R10 , QWORD [ RSP + 8 ]
mov RBX , QWORD [ R10 + 136 ]
mov QWORD [ RBP + 0 ] , cont$300
mov R11 , QWORD [ RSP + 48 ]
mov QWORD [ RBP + 8 ] , R11
mov R11 , QWORD [ RSP + 40 ]
mov QWORD [ RBP + 16 ] , R11
mov R11 , QWORD [ RSP + 32 ]
mov QWORD [ RBP + 24 ] , R11
mov R11 , QWORD [ RSP + 24 ]
mov QWORD [ RBP + 32 ] , R11
mov R11 , QWORD [ RSP + 16 ]
mov QWORD [ RBP + 40 ] , R11
mov R11 , QWORD [ RSP + 0 ]
mov QWORD [ RBP + 48 ] , R11
mov QWORD [ RBP + 56 ] , R15
mov QWORD [ RBP + 64 ] , R14
mov QWORD [ RBP + 72 ] , R13
mov QWORD [ RBP + 80 ] , R12
mov QWORD [ RBP + 88 ] , R9
mov QWORD [ RBP + 96 ] , R8
mov QWORD [ RBP + 104 ] , RDI
mov QWORD [ RBP + 112 ] , RSI
mov QWORD [ RBP + 120 ] , RDX
mov QWORD [ RBP + 128 ] , RCX
mov QWORD [ RBP + 136 ] , RBX
mov QWORD [ RBP + 144 ] , RAX
mov RBX , RBP
mov RAX , 152
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$339:
mov RSP , RBP
mov R10 , 48
add RBP , R10
mov R10 , RDI
mov QWORD [ RSP + 0 ] , R10
mov RAX , RDX
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 8 ]
mov QWORD [ RSP + 40 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 16 ]
mov QWORD [ RSP + 32 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 24 ]
mov QWORD [ RSP + 24 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 32 ]
mov QWORD [ RSP + 16 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R10 , QWORD [ R10 + 40 ]
mov QWORD [ RSP + 8 ] , R10
mov R10 , QWORD [ RSP + 0 ]
mov R15 , QWORD [ R10 + 48 ]
mov R10 , QWORD [ RSP + 0 ]
mov R14 , QWORD [ R10 + 56 ]
mov R10 , QWORD [ RSP + 0 ]
mov R13 , QWORD [ R10 + 64 ]
mov R10 , QWORD [ RSP + 0 ]
mov R12 , QWORD [ R10 + 72 ]
mov R10 , QWORD [ RSP + 0 ]
mov R9 , QWORD [ R10 + 80 ]
mov R10 , QWORD [ RSP + 0 ]
mov R8 , QWORD [ R10 + 88 ]
mov R10 , QWORD [ RSP + 0 ]
mov RDI , QWORD [ R10 + 96 ]
mov R10 , QWORD [ RSP + 0 ]
mov RSI , QWORD [ R10 + 104 ]
mov R10 , QWORD [ RSP + 0 ]
mov RDX , QWORD [ R10 + 112 ]
mov R10 , QWORD [ RSP + 0 ]
mov RCX , QWORD [ R10 + 120 ]
mov R10 , QWORD [ RSP + 0 ]
mov RBX , QWORD [ R10 + 128 ]
mov QWORD [ RBP + 0 ] , cont$320
mov R11 , QWORD [ RSP + 40 ]
mov QWORD [ RBP + 8 ] , R11
mov R11 , QWORD [ RSP + 32 ]
mov QWORD [ RBP + 16 ] , R11
mov R11 , QWORD [ RSP + 24 ]
mov QWORD [ RBP + 24 ] , R11
mov R11 , QWORD [ RSP + 16 ]
mov QWORD [ RBP + 32 ] , R11
mov R11 , QWORD [ RSP + 8 ]
mov QWORD [ RBP + 40 ] , R11
mov QWORD [ RBP + 48 ] , R15
mov QWORD [ RBP + 56 ] , R14
mov QWORD [ RBP + 64 ] , R13
mov QWORD [ RBP + 72 ] , R12
mov QWORD [ RBP + 80 ] , R9
mov QWORD [ RBP + 88 ] , R8
mov QWORD [ RBP + 96 ] , RDI
mov QWORD [ RBP + 104 ] , RSI
mov QWORD [ RBP + 112 ] , RDX
mov QWORD [ RBP + 120 ] , RCX
mov QWORD [ RBP + 128 ] , RBX
mov QWORD [ RBP + 136 ] , RAX
mov RBX , RBP
mov RAX , 144
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$357:
mov RSP , RBP
mov R10 , 40
add RBP , R10
mov R15 , RDI
mov RAX , RDX
mov R10 , QWORD [ R15 + 8 ]
mov QWORD [ RSP + 32 ] , R10
mov R10 , QWORD [ R15 + 16 ]
mov QWORD [ RSP + 24 ] , R10
mov R10 , QWORD [ R15 + 24 ]
mov QWORD [ RSP + 16 ] , R10
mov R10 , QWORD [ R15 + 32 ]
mov QWORD [ RSP + 8 ] , R10
mov R10 , QWORD [ R15 + 40 ]
mov QWORD [ RSP + 0 ] , R10
mov R14 , QWORD [ R15 + 48 ]
mov R13 , QWORD [ R15 + 56 ]
mov R12 , QWORD [ R15 + 64 ]
mov R9 , QWORD [ R15 + 72 ]
mov R8 , QWORD [ R15 + 80 ]
mov RDI , QWORD [ R15 + 88 ]
mov RSI , QWORD [ R15 + 96 ]
mov RDX , QWORD [ R15 + 104 ]
mov RCX , QWORD [ R15 + 112 ]
mov RBX , QWORD [ R15 + 120 ]
mov QWORD [ RBP + 0 ] , cont$339
mov R11 , QWORD [ RSP + 32 ]
mov QWORD [ RBP + 8 ] , R11
mov R11 , QWORD [ RSP + 24 ]
mov QWORD [ RBP + 16 ] , R11
mov R11 , QWORD [ RSP + 16 ]
mov QWORD [ RBP + 24 ] , R11
mov R11 , QWORD [ RSP + 8 ]
mov QWORD [ RBP + 32 ] , R11
mov R11 , QWORD [ RSP + 0 ]
mov QWORD [ RBP + 40 ] , R11
mov QWORD [ RBP + 48 ] , R14
mov QWORD [ RBP + 56 ] , R13
mov QWORD [ RBP + 64 ] , R12
mov QWORD [ RBP + 72 ] , R9
mov QWORD [ RBP + 80 ] , R8
mov QWORD [ RBP + 88 ] , RDI
mov QWORD [ RBP + 96 ] , RSI
mov QWORD [ RBP + 104 ] , RDX
mov QWORD [ RBP + 112 ] , RCX
mov QWORD [ RBP + 120 ] , RBX
mov QWORD [ RBP + 128 ] , RAX
mov RBX , RBP
mov RAX , 136
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$374:
mov RSP , RBP
mov R10 , 32
add RBP , R10
mov R14 , RDI
mov RAX , RDX
mov R10 , QWORD [ R14 + 8 ]
mov QWORD [ RSP + 24 ] , R10
mov R10 , QWORD [ R14 + 16 ]
mov QWORD [ RSP + 16 ] , R10
mov R10 , QWORD [ R14 + 24 ]
mov QWORD [ RSP + 8 ] , R10
mov R10 , QWORD [ R14 + 32 ]
mov QWORD [ RSP + 0 ] , R10
mov R15 , QWORD [ R14 + 40 ]
mov R13 , QWORD [ R14 + 48 ]
mov R12 , QWORD [ R14 + 56 ]
mov R9 , QWORD [ R14 + 64 ]
mov R8 , QWORD [ R14 + 72 ]
mov RDI , QWORD [ R14 + 80 ]
mov RSI , QWORD [ R14 + 88 ]
mov RDX , QWORD [ R14 + 96 ]
mov RCX , QWORD [ R14 + 104 ]
mov RBX , QWORD [ R14 + 112 ]
mov QWORD [ RBP + 0 ] , cont$357
mov R11 , QWORD [ RSP + 24 ]
mov QWORD [ RBP + 8 ] , R11
mov R11 , QWORD [ RSP + 16 ]
mov QWORD [ RBP + 16 ] , R11
mov R11 , QWORD [ RSP + 8 ]
mov QWORD [ RBP + 24 ] , R11
mov R11 , QWORD [ RSP + 0 ]
mov QWORD [ RBP + 32 ] , R11
mov QWORD [ RBP + 40 ] , R15
mov QWORD [ RBP + 48 ] , R13
mov QWORD [ RBP + 56 ] , R12
mov QWORD [ RBP + 64 ] , R9
mov QWORD [ RBP + 72 ] , R8
mov QWORD [ RBP + 80 ] , RDI
mov QWORD [ RBP + 88 ] , RSI
mov QWORD [ RBP + 96 ] , RDX
mov QWORD [ RBP + 104 ] , RCX
mov QWORD [ RBP + 112 ] , RBX
mov QWORD [ RBP + 120 ] , RAX
mov RBX , RBP
mov RAX , 128
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$390:
mov RSP , RBP
mov R10 , 24
add RBP , R10
mov R13 , RDI
mov RAX , RDX
mov R10 , QWORD [ R13 + 8 ]
mov QWORD [ RSP + 16 ] , R10
mov R10 , QWORD [ R13 + 16 ]
mov QWORD [ RSP + 8 ] , R10
mov R10 , QWORD [ R13 + 24 ]
mov QWORD [ RSP + 0 ] , R10
mov R15 , QWORD [ R13 + 32 ]
mov R14 , QWORD [ R13 + 40 ]
mov R12 , QWORD [ R13 + 48 ]
mov R9 , QWORD [ R13 + 56 ]
mov R8 , QWORD [ R13 + 64 ]
mov RDI , QWORD [ R13 + 72 ]
mov RSI , QWORD [ R13 + 80 ]
mov RDX , QWORD [ R13 + 88 ]
mov RCX , QWORD [ R13 + 96 ]
mov RBX , QWORD [ R13 + 104 ]
mov QWORD [ RBP + 0 ] , cont$374
mov R11 , QWORD [ RSP + 16 ]
mov QWORD [ RBP + 8 ] , R11
mov R11 , QWORD [ RSP + 8 ]
mov QWORD [ RBP + 16 ] , R11
mov R11 , QWORD [ RSP + 0 ]
mov QWORD [ RBP + 24 ] , R11
mov QWORD [ RBP + 32 ] , R15
mov QWORD [ RBP + 40 ] , R14
mov QWORD [ RBP + 48 ] , R12
mov QWORD [ RBP + 56 ] , R9
mov QWORD [ RBP + 64 ] , R8
mov QWORD [ RBP + 72 ] , RDI
mov QWORD [ RBP + 80 ] , RSI
mov QWORD [ RBP + 88 ] , RDX
mov QWORD [ RBP + 96 ] , RCX
mov QWORD [ RBP + 104 ] , RBX
mov QWORD [ RBP + 112 ] , RAX
mov RBX , RBP
mov RAX , 120
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$405:
mov RSP , RBP
mov R10 , 16
add RBP , R10
mov R12 , RDI
mov RAX , RDX
mov R10 , QWORD [ R12 + 8 ]
mov QWORD [ RSP + 8 ] , R10
mov R10 , QWORD [ R12 + 16 ]
mov QWORD [ RSP + 0 ] , R10
mov R15 , QWORD [ R12 + 24 ]
mov R14 , QWORD [ R12 + 32 ]
mov R13 , QWORD [ R12 + 40 ]
mov R9 , QWORD [ R12 + 48 ]
mov R8 , QWORD [ R12 + 56 ]
mov RDI , QWORD [ R12 + 64 ]
mov RSI , QWORD [ R12 + 72 ]
mov RDX , QWORD [ R12 + 80 ]
mov RCX , QWORD [ R12 + 88 ]
mov RBX , QWORD [ R12 + 96 ]
mov QWORD [ RBP + 0 ] , cont$390
mov R11 , QWORD [ RSP + 8 ]
mov QWORD [ RBP + 8 ] , R11
mov R11 , QWORD [ RSP + 0 ]
mov QWORD [ RBP + 16 ] , R11
mov QWORD [ RBP + 24 ] , R15
mov QWORD [ RBP + 32 ] , R14
mov QWORD [ RBP + 40 ] , R13
mov QWORD [ RBP + 48 ] , R9
mov QWORD [ RBP + 56 ] , R8
mov QWORD [ RBP + 64 ] , RDI
mov QWORD [ RBP + 72 ] , RSI
mov QWORD [ RBP + 80 ] , RDX
mov QWORD [ RBP + 88 ] , RCX
mov QWORD [ RBP + 96 ] , RBX
mov QWORD [ RBP + 104 ] , RAX
mov RBX , RBP
mov RAX , 112
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$419:
mov RSP , RBP
mov R10 , 8
add RBP , R10
mov R9 , RDI
mov RAX , RDX
mov R10 , QWORD [ R9 + 8 ]
mov QWORD [ RSP + 0 ] , R10
mov R15 , QWORD [ R9 + 16 ]
mov R14 , QWORD [ R9 + 24 ]
mov R13 , QWORD [ R9 + 32 ]
mov R12 , QWORD [ R9 + 40 ]
mov R8 , QWORD [ R9 + 48 ]
mov RDI , QWORD [ R9 + 56 ]
mov RSI , QWORD [ R9 + 64 ]
mov RDX , QWORD [ R9 + 72 ]
mov RCX , QWORD [ R9 + 80 ]
mov RBX , QWORD [ R9 + 88 ]
mov QWORD [ RBP + 0 ] , cont$405
mov R11 , QWORD [ RSP + 0 ]
mov QWORD [ RBP + 8 ] , R11
mov QWORD [ RBP + 16 ] , R15
mov QWORD [ RBP + 24 ] , R14
mov QWORD [ RBP + 32 ] , R13
mov QWORD [ RBP + 40 ] , R12
mov QWORD [ RBP + 48 ] , R8
mov QWORD [ RBP + 56 ] , RDI
mov QWORD [ RBP + 64 ] , RSI
mov QWORD [ RBP + 72 ] , RDX
mov QWORD [ RBP + 80 ] , RCX
mov QWORD [ RBP + 88 ] , RBX
mov QWORD [ RBP + 96 ] , RAX
mov RBX , RBP
mov RAX , 104
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$432:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov R8 , RDI
mov RAX , RDX
mov R15 , QWORD [ R8 + 8 ]
mov R14 , QWORD [ R8 + 16 ]
mov R13 , QWORD [ R8 + 24 ]
mov R12 , QWORD [ R8 + 32 ]
mov R9 , QWORD [ R8 + 40 ]
mov RDI , QWORD [ R8 + 48 ]
mov RSI , QWORD [ R8 + 56 ]
mov RDX , QWORD [ R8 + 64 ]
mov RCX , QWORD [ R8 + 72 ]
mov RBX , QWORD [ R8 + 80 ]
mov QWORD [ RBP + 0 ] , cont$419
mov QWORD [ RBP + 8 ] , R15
mov QWORD [ RBP + 16 ] , R14
mov QWORD [ RBP + 24 ] , R13
mov QWORD [ RBP + 32 ] , R12
mov QWORD [ RBP + 40 ] , R9
mov QWORD [ RBP + 48 ] , RDI
mov QWORD [ RBP + 56 ] , RSI
mov QWORD [ RBP + 64 ] , RDX
mov QWORD [ RBP + 72 ] , RCX
mov QWORD [ RBP + 80 ] , RBX
mov QWORD [ RBP + 88 ] , RAX
mov RBX , RBP
mov RAX , 96
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$444:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RDI , RDI
mov RAX , RDX
mov R14 , QWORD [ RDI + 8 ]
mov R13 , QWORD [ RDI + 16 ]
mov R12 , QWORD [ RDI + 24 ]
mov R9 , QWORD [ RDI + 32 ]
mov R8 , QWORD [ RDI + 40 ]
mov RSI , QWORD [ RDI + 48 ]
mov RDX , QWORD [ RDI + 56 ]
mov RCX , QWORD [ RDI + 64 ]
mov RBX , QWORD [ RDI + 72 ]
mov QWORD [ RBP + 0 ] , cont$432
mov QWORD [ RBP + 8 ] , R14
mov QWORD [ RBP + 16 ] , R13
mov QWORD [ RBP + 24 ] , R12
mov QWORD [ RBP + 32 ] , R9
mov QWORD [ RBP + 40 ] , R8
mov QWORD [ RBP + 48 ] , RSI
mov QWORD [ RBP + 56 ] , RDX
mov QWORD [ RBP + 64 ] , RCX
mov QWORD [ RBP + 72 ] , RBX
mov QWORD [ RBP + 80 ] , RAX
mov RBX , RBP
mov RAX , 88
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$455:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RSI , RDI
mov RAX , RDX
mov R13 , QWORD [ RSI + 8 ]
mov R12 , QWORD [ RSI + 16 ]
mov R9 , QWORD [ RSI + 24 ]
mov R8 , QWORD [ RSI + 32 ]
mov RDI , QWORD [ RSI + 40 ]
mov RDX , QWORD [ RSI + 48 ]
mov RCX , QWORD [ RSI + 56 ]
mov RBX , QWORD [ RSI + 64 ]
mov QWORD [ RBP + 0 ] , cont$444
mov QWORD [ RBP + 8 ] , R13
mov QWORD [ RBP + 16 ] , R12
mov QWORD [ RBP + 24 ] , R9
mov QWORD [ RBP + 32 ] , R8
mov QWORD [ RBP + 40 ] , RDI
mov QWORD [ RBP + 48 ] , RDX
mov QWORD [ RBP + 56 ] , RCX
mov QWORD [ RBP + 64 ] , RBX
mov QWORD [ RBP + 72 ] , RAX
mov RBX , RBP
mov RAX , 80
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$465:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RSI , RDI
mov RAX , RDX
mov R12 , QWORD [ RSI + 8 ]
mov R9 , QWORD [ RSI + 16 ]
mov R8 , QWORD [ RSI + 24 ]
mov RDI , QWORD [ RSI + 32 ]
mov RDX , QWORD [ RSI + 40 ]
mov RCX , QWORD [ RSI + 48 ]
mov RBX , QWORD [ RSI + 56 ]
mov QWORD [ RBP + 0 ] , cont$455
mov QWORD [ RBP + 8 ] , R12
mov QWORD [ RBP + 16 ] , R9
mov QWORD [ RBP + 24 ] , R8
mov QWORD [ RBP + 32 ] , RDI
mov QWORD [ RBP + 40 ] , RDX
mov QWORD [ RBP + 48 ] , RCX
mov QWORD [ RBP + 56 ] , RBX
mov QWORD [ RBP + 64 ] , RAX
mov RBX , RBP
mov RAX , 72
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$474:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RCX , RDI
mov RAX , RDX
mov R9 , QWORD [ RCX + 8 ]
mov R8 , QWORD [ RCX + 16 ]
mov RDI , QWORD [ RCX + 24 ]
mov RSI , QWORD [ RCX + 32 ]
mov RDX , QWORD [ RCX + 40 ]
mov RBX , QWORD [ RCX + 48 ]
mov QWORD [ RBP + 0 ] , cont$465
mov QWORD [ RBP + 8 ] , R9
mov QWORD [ RBP + 16 ] , R8
mov QWORD [ RBP + 24 ] , RDI
mov QWORD [ RBP + 32 ] , RSI
mov QWORD [ RBP + 40 ] , RDX
mov QWORD [ RBP + 48 ] , RBX
mov QWORD [ RBP + 56 ] , RAX
mov RBX , RBP
mov RAX , 64
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$482:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RBX , RDI
mov RAX , RDX
mov R8 , QWORD [ RBX + 8 ]
mov RDI , QWORD [ RBX + 16 ]
mov RSI , QWORD [ RBX + 24 ]
mov RDX , QWORD [ RBX + 32 ]
mov RCX , QWORD [ RBX + 40 ]
mov QWORD [ RBP + 0 ] , cont$474
mov QWORD [ RBP + 8 ] , R8
mov QWORD [ RBP + 16 ] , RDI
mov QWORD [ RBP + 24 ] , RSI
mov QWORD [ RBP + 32 ] , RDX
mov QWORD [ RBP + 40 ] , RCX
mov QWORD [ RBP + 48 ] , RAX
mov RBX , RBP
mov RAX , 56
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$489:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RBX , RDX
mov RDI , QWORD [ RAX + 8 ]
mov RSI , QWORD [ RAX + 16 ]
mov RDX , QWORD [ RAX + 24 ]
mov RCX , QWORD [ RAX + 32 ]
mov QWORD [ RBP + 0 ] , cont$482
mov QWORD [ RBP + 8 ] , RDI
mov QWORD [ RBP + 16 ] , RSI
mov QWORD [ RBP + 24 ] , RDX
mov QWORD [ RBP + 32 ] , RCX
mov QWORD [ RBP + 40 ] , RBX
mov RBX , RBP
mov RAX , 48
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$495:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RBX , RDX
mov RSI , QWORD [ RAX + 8 ]
mov RDX , QWORD [ RAX + 16 ]
mov RCX , QWORD [ RAX + 24 ]
mov QWORD [ RBP + 0 ] , cont$489
mov QWORD [ RBP + 8 ] , RSI
mov QWORD [ RBP + 16 ] , RDX
mov QWORD [ RBP + 24 ] , RCX
mov QWORD [ RBP + 32 ] , RBX
mov RBX , RBP
mov RAX , 40
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$500:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RBX , RDX
mov RDX , QWORD [ RAX + 8 ]
mov RCX , QWORD [ RAX + 16 ]
mov QWORD [ RBP + 0 ] , cont$495
mov QWORD [ RBP + 8 ] , RDX
mov QWORD [ RBP + 16 ] , RCX
mov QWORD [ RBP + 24 ] , RBX
mov RBX , RBP
mov RAX , 32
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$504:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RBX , RDX
mov RCX , QWORD [ RAX + 8 ]
mov QWORD [ RBP + 0 ] , cont$500
mov QWORD [ RBP + 8 ] , RCX
mov QWORD [ RBP + 16 ] , RBX
mov RBX , RBP
mov RAX , 24
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
jmp RCX
cont$507:
mov RSP , RBP
mov R10 , 0
add RBP , R10
mov RAX , RDI
mov RAX , RDX
mov QWORD [ RBP + 0 ] , cont$504
mov QWORD [ RBP + 8 ] , RAX
mov RBX , RBP
mov RAX , 16
add RBP , RAX
mov RAX , 10
mov RCX , QWORD [ RBX + 0 ]
mov RDI , RBX
mov RDX , RAX
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

