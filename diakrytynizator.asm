SYS_WRITE   equ 1
SYS_EXIT    equ 60
SYS_READ    equ 0
STDOUT      equ 1
STDIN      equ 0
SYS_BRK  equ 12
BUFFER_SIZE  equ 4096
MODULO_VALUE equ 0x10ff80

; There should be 3 bytes of room in buffer, if utf-8 bytes don't allign to the end of the buffer.
BUFFER_SIZE_TO_READ  equ 4093     

section   .text
   global    _start
_start:   
   pop rax                          ; Get number of arguments + 1.
   dec rax                          ; Make it real number of arguments.
   mov r12, rax                     ; Copy number of args to preserved register.
   
   pop rsi                          ; Discard program name.
   
   call allocate_memory_for_coeffs

   ; Copy begin and end of allocated memory to preserved registers
   mov r13, rdx                      

   xor rbx, rbx                      ; Use rbx as loop counter.

put_coeff_in_memory:   
   cmp rbx, r12
   jz parse_input
   pop r10
   call atoi
   mov [r13 + rbx * 8], rax
   inc rbx
   jmp put_coeff_in_memory

parse_input:
   call read_buffer
   cmp r14, 0         ; If read returned 0, end
   je exit
   call parse_buffer
   jmp exit

; PROCEDURES
; Takes number of arguments in rax and returns end of block in rax and first address in rdx
; Uses rdx, r8
allocate_memory_for_coeffs:
   mov r8, 8
   mul r8
   mov r8, rax            ; Now there is number of bytes to be reserved in r8.

   mov	rax, SYS_BRK
   xor	rdi, rdi
   syscall
   
   mov rdx, rax              ; Copy first address to rdx

   add   rax, r8	          ; Add number of bytes to be reserved.
   mov   rdi, rax
   mov   rax, SYS_BRK
   syscall
   ret

; Takes address of string in r10 and returns integer in rax.
atoi:
   push rcx
   push rbx
   push r10
   xor rax, rax
   mov cl, 10

atoi_next:
   xor rbx, rbx

   mov bl, [r10]
   inc r10

   cmp bl, '0'                        ; Check if bl contains digit
   jb atoi_end
   cmp bl, '9'
   ja atoi_end

   sub bl, '0'
   mul cl
   add rax, rbx
   jmp atoi_next

atoi_end:
   pop r10
   pop rbx
   pop rcx
   ret
   

print_message:
   mov       rax, SYS_WRITE          ; system call for write
   mov       rdi, STDOUT             ; file handle 1 is stdout
   mov       rsi, WRITE_BUFFER            ; address of string to output
   mov       rdx, r14               ; number of bytes
   syscall                          
   ret

read_buffer:
   mov rax, SYS_READ
   mov rdi, STDIN
   mov rsi, READ_BUFFER
   mov rdx, BUFFER_SIZE_TO_READ
   syscall
   mov r14, rax     ; Move number of bytes read to preserved register
   ret

; in rax there should be number of bytes read
; r8b - first utf8 byte
; r9b - second utf8 byte
; r10b - third utf8 byte
; r11b - fourth utf8 byte
parse_buffer:
   push rdx
   push rcx
   push rbx
   push r8
   xor rcx, rcx ; Use rcx as counter.
   mov rbx, READ_BUFFER
parse_buffer_loop:
   cmp rcx, r14         ; In r14 there is number of bytes in buffer.
   je parse_buffer_check_end ; Exit when all bytes are parsed.

   ;Read first byte from buffer.
   xor r8, r8           ; Clear buffer after last loop.
   mov r8b, [rbx]       ; Get value from READ_BUFFER.
   inc rbx              ; Move pointer to the next byte.

   cmp r8b, 0x80        ; Check if value is smaller then 0x80, then we know it's ascii character
   jb put_ascii_char_in_buffer_without_transform

   ; Read next byte from buffer.
   xor r9, r9           
   mov r9b, [rbx]
   inc rbx     
   
   ; Check if it's 2 byte character
   ; TODO comment this
   mov dl, r8b
   and dl, 0xe0   ; and with 11100000
   cmp dl, 0xc0   ; check if equals 11000000
   je transform_two_byte_char

   xor r10, r10
   mov r10b, [rbx]
   inc rbx     

   ; Check if it's 3 byte character
   ; TODO comment this
   mov dl, r8b
   and dl, 0xf0
   cmp dl, 0xe0
   je transform_three_byte_char

   xor r11, r11
   mov r11b, [rbx]
   inc rbx     

   ; Check if it's 4 byte character
   push rbx  ; Preserve register
   push rax  ; Preserve register
   xor bx, bx
   xor ax, ax
   mov ax, 1

   mov dl, r8b
   and dl, 0xf8
   cmp dl, 0xf0
   cmove bx, ax      ; If condition is satisfied, remember logical value.

   cmp r8b, 0xf4
   cmovle dx, ax     ; Again if condition is satisfied, remember logical value.
   pop rax           ; I don't use ax anymore, so I can restore value.

   and bx, dx      ; do logical and
   cmp bx, 1
   pop rbx 
   je transform_four_byte_char

   ; If none of this cases is true then error
   call exit

parse_buffer_check_end:
   call print_message
   call read_buffer
   mov r14, rax
   cmp r14, 0
   jne parse_buffer_loop

parse_buffer_exit:
   pop r8
   pop rbx
   pop rcx
   pop rdx
   ret


put_ascii_char_in_buffer_without_transform:
   mov [WRITE_BUFFER + rcx], r8b
   inc rcx
   jmp parse_buffer_loop

transform_two_byte_char:
   ; Encode bytes from r8b and r9b into unicode char in r8
   and r8b, 0x1f
   and r9b, 0x3f

   shl r8, 6

   or r8, r9

   call apply_polynomial

   ; Decode bytes from r8 into utf-8 bytes in r8b and r9b
   mov r9, r8
   shr r8, 6
   and r8b, 0x1f

   or r8b, 0xc0
   and r9b, 0x3f
   or r9b, 0x80


   mov [WRITE_BUFFER + rcx], r8b
   inc rcx
   mov [WRITE_BUFFER + rcx], r9b
   inc rcx
   jmp parse_buffer_loop
   

transform_three_byte_char:
   and r8b, 0x0f 
   and r9b, 0x3f 
   and r10b, 0x3f 
   
   shl r8, 12
   shl r9, 6

   or r8, r9
   or r8, r10

   call apply_polynomial

   mov r9, r8
   mov r10, r8

   shr r8, 12
   shr r9, 6

   and r8b, 0x0f
   and r9b, 0x3f
   and r10b, 0x3f

   or r8b, 0xe0
   or r9b, 0x80
   or r10b, 0x80

   mov [WRITE_BUFFER + rcx], r8b
   inc rcx
   mov [WRITE_BUFFER + rcx], r9b
   inc rcx
   mov [WRITE_BUFFER + rcx], r10b
   inc rcx
   jmp parse_buffer_loop

   ret

transform_four_byte_char:
   and r8b, 0x07
   and r9b, 0x3f 
   and r10b, 0x3f 
   and r11b, 0x3f 
   
   shl r8, 18
   shl r9, 12
   shl r10, 6

   or r8, r9
   or r8, r10
   or r8, r11

   call apply_polynomial

   mov r9, r8
   mov r10, r8
   mov r11, r8

   shr r8, 18
   shr r9, 12
   shr r10, 6

   and r8b, 0x07
   and r9b, 0x3f
   and r10b, 0x3f
   and r11b, 0x3f

   or r8b, 0xf0
   or r9b, 0x80
   or r10b, 0x80
   or r11b, 0x80

   mov [WRITE_BUFFER + rcx], r8b
   inc rcx
   mov [WRITE_BUFFER + rcx], r9b
   inc rcx
   mov [WRITE_BUFFER + rcx], r10b
   inc rcx
   mov [WRITE_BUFFER + rcx], r11b
   inc rcx
   jmp parse_buffer_loop

   ret

; get unicode value from r8 and put result also in r8
apply_polynomial:
   push rdx
   push rcx
   push rbx
   push r9
   push rax
   sub r8, 0x80

   cmp r12, 0
   je apply_polynomial_exit

   mov rbx, r8    ; Now in rbx there will be unicode value of character to the 1 power.
   mov r9, r8 ; Now in r9 there will be unicode value of character to the kth power.
   xor r8, r8 ; Make r8 zero so we can accumulate result there.

   add r8, [r13] ; Add a_0 to the result
   cmp r12, 1
   je apply_polynomial_exit

   mov rdx, r13     ; Now in rdx there will be current address of coefficent.
   add rdx, 8

   mov rax, r9
   mul QWORD [rdx]       ; multiply times coefficent

   add r8, rax      ; Add a_n*(x-0x80) to result

   mov rcx, r12 ; Now in rcx there will be number of coefficents left.
   sub rcx, 2   ; Two coefficents where already applied, so we skip them.
apply_polynomial_loop:
   cmp rcx, 0
   je apply_polynomial_exit

   mov rax, r9
   mul rbx       ; change unicode**k -> unicode**(k+1)
   mov r9, rax
   mul QWORD [rdx]       ; multiply times coefficent
   add r8, rax     ; Add a_n * (x - 0x80)**k to result

   dec rcx
   add rdx, 8

   jmp apply_polynomial_loop


apply_polynomial_exit:
   pop rax
   pop r9
   pop rbx
   pop rcx
   pop rdx
   add r8, 0x80
   ret
      

exit:
   mov       rax, SYS_EXIT           ; system call for exit
   xor       rdi, rdi                ; exit code 0
   syscall                           ; invoke operating system to exit


section   .data
   message:      dq       0
   minus_flag:   db       0
   x:   db        0
   char_val:     db       0
   unicode_value:     dd       0
   READ_BUFFER TIMES BUFFER_SIZE db 0
   WRITE_BUFFER TIMES BUFFER_SIZE db 0
