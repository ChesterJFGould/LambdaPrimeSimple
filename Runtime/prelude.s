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
