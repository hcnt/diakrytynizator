SYS_WRITE   equ 1
SYS_EXIT    equ 60
SYS_READ    equ 0
STDOUT      equ 1
STDIN      equ 0
SYS_BRK  equ 12
BUFFER_SIZE  equ 4096
MODULO_VALUE equ 0x10ff80

; There should be 4 bytes of room in buffer, if utf-8 bytes don't allign to the end of the buffer.
BUFFER_SIZE_WITH_ROOM  equ 4092

section   .text
   global    _start
_start:   
   pop rax                          ; Get number of arguments + 1.
   dec rax                          ; Make it real number of arguments.
   mov r12, rax                     ; Copy number of args to preserved register.
   
   pop rsi                          ; Discard program name.
   
   xor rbx, rbx                      ; Use rbx as loop counter.
   mov r13, rsp       ; Copy value of stack pointer

put_coeff_in_memory:   
   cmp rbx, r12
   jz parse_input
   ; pop r10
   mov r10, [r13]
   call atoi
   mov [r13], rax
   add r13, 8
   inc rbx
   jmp put_coeff_in_memory

; r8b - first utf8 byte
; r9b - second utf8 byte
; r10b - third utf8 byte
; r11b - fourth utf8 byte
parse_input:
   sub r13, 8  ; Make r13 point to last coeff in memory
   xor r14, r14 ; Use r14 as counter in READ_BUFFER
   xor rcx, rcx ; Use rcx as counter in WRITE_BUFFER
parse_buffer_loop:
   cmp rcx, BUFFER_SIZE_WITH_ROOM
   jle parse_buffer_utf8_to_unicode
   call print_message
   xor rcx, rcx

parse_buffer_utf8_to_unicode:
   cmp r14, 0
   jne parse_buffer_continue_0
   call parse_buffer_check_end
parse_buffer_continue_0:

   ;Read first byte from buffer.
   xor r8, r8           ; Clear buffer after last loop.
   mov r8b, [rbx]       ; Get value from READ_BUFFER.
   inc rbx              ; Move pointer to the next byte.
   dec r14

   cmp r8b, 0x80        ; Check if value is smaller then 0x80, then we know it's ascii character
   jb encode_utf8_one_byte

   cmp r8b, 0xf4        ; If it's bigger than 0xf4 then it's error
   ja parse_buffer_error

   cmp r14, 0
   jne parse_buffer_continue_1
   call parse_buffer_check_end
parse_buffer_continue_1:
   ; Read next byte from buffer.
   xor r9, r9           
   mov r9b, [rbx]
   inc rbx     
   dec r14

   ; Compute lower and higher bounds for second byte 
   push rax
   push r9
   mov ax, 0x80
   mov dx, 0xbf

   mov r9w, 0xa0
   mov r10w, 0x9f
   mov r11w, 0x90
   mov r15w, 0x8f

   cmp r8b, 0xe0
   cmove ax, r9w

   ; cmp r8b, 0xed
   ; cmove dx, r10w

   cmp r8b, 0xf0
   cmove ax, r11w

   cmp r8b, 0xf4
   cmove dx, r15w

   pop r9
   ; Check if second byte is correct
   cmp r9b, al
   jb parse_buffer_error
   cmp r9b, dl
   ja parse_buffer_error

   pop rax

   ; Check if it's 2 byte character
   mov dl, r8b
   and dl, 0xe0   ; and with 11100000
   cmp dl, 0xc0   ; check if equals 11000000
   je decode_utf8_two_bytes

   cmp r14, 0
   jne parse_buffer_continue_2
   call parse_buffer_check_end
parse_buffer_continue_2:

   ; Read next byte
   xor r10, r10
   mov r10b, [rbx]
   inc rbx     
   dec r14

   ; Check if it's correct
   mov dl, r10b
   and dl, 0xc0
   cmp dl, 0x80
   jne parse_buffer_error

   ; Check if it's 3 byte character
   mov dl, r8b
   and dl, 0xf0
   cmp dl, 0xe0
   je decode_utf8_three_bytes

   cmp r14, 0
   jne parse_buffer_continue_3
   call parse_buffer_check_end
parse_buffer_continue_3:

   ; Read next byte
   xor r11, r11
   mov r11b, [rbx]
   inc rbx     
   dec r14

   ; Check if it's correct
   mov dl, r11b
   and dl, 0xc0
   cmp dl, 0x80
   jne parse_buffer_error

   ; Check if it's 4 byte character
   mov dl, r8b
   and dl, 0xf8
   cmp dl, 0xf0
   je decode_utf8_four_bytes

   ; If none of this cases is true then error
parse_buffer_error:
   call print_message
   mov rdi, 1
   jmp exit


parse_buffer_check_end:
   push rcx
   call read_buffer
   pop rcx
   mov r14, rax
   cmp r14, 0
   je parse_buffer_exit
   ret

parse_buffer_exit:
   call print_message
   xor rdi, rdi
   jmp exit

;----------------------------------------

decode_utf8_two_bytes:
   and r8b, 0x1f
   and r9b, 0x3f
   cmp r8b, 2
   jb parse_buffer_error

   shl r8, 6
   or r8, r9
   jmp parse_buffer_apply_polynomial_and_unicode_to_utf8

decode_utf8_three_bytes:
   and r8b, 0x0f 
   and r9b, 0x3f 
   and r10b, 0x3f 
   
   shl r8, 12
   shl r9, 6

   or r8, r9
   or r8, r10
   jmp parse_buffer_apply_polynomial_and_unicode_to_utf8


decode_utf8_four_bytes:
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
   jmp parse_buffer_apply_polynomial_and_unicode_to_utf8


parse_buffer_apply_polynomial_and_unicode_to_utf8:
   call apply_polynomial
   cmp r8, 0x80
   jb encode_utf8_one_byte
   cmp r8, 0x0800
   jb encode_utf8_two_bytes
   cmp r8, 0x010000
   jb encode_utf8_three_bytes
   cmp r8, 0x0110000
   jb encode_utf8_four_bytes


encode_utf8_one_byte:
   mov [WRITE_BUFFER + rcx], r8b
   inc rcx
   jmp parse_buffer_loop


encode_utf8_two_bytes:
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
   
encode_utf8_three_bytes:
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

encode_utf8_four_bytes:
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

; get unicode value from r8 and put result also in r8
; CANT DESTROY: rcx, r14, r12, rbx
apply_polynomial:
   push rcx
   push rbx
   sub r8, 0x80
   mov r11, MODULO_VALUE 

   cmp r12, 0
   je apply_polynomial_exit

   mov rbx, r8    ; Now in rbx there will be unicode value of character to the 1 power.
   xor r8, r8 ; Make r8 zero so we can accumulate result there.
   xor rax, rax


   mov r15, r13     ; Now in r15 there will be current address of coefficent.

   mov rcx, r12 ; Now in rcx there will be number of coefficents left.

apply_polynomial_loop:
   cmp rcx, 1          ; When only a_0 is left, end loop
   je apply_polynomial_exit

   add rax, [r15]
   xor rdx, rdx
   mul rbx      
   call modulo

   dec rcx
   sub r15, 8

   jmp apply_polynomial_loop

apply_polynomial_exit:
   add rax, [r15]  

   call modulo
   mov r8, rax

   add r8, 0x80

   pop rbx
   pop rcx

   ret

; Takes address of string in r10 and returns integer in rax.
atoi:
   xor rax, rax
   mov rcx, 10
   mov r11, MODULO_VALUE 

atoi_next:
   xor r8, r8

   mov r8b, [r10]
   inc r10

   cmp r8b, '0'                        ; Check if bl contains digit
   jb atoi_end
   cmp r8b, '9'
   ja atoi_end

   sub r8b, '0'
   xor rdx, rdx
   mul rcx
   add rax, r8

   ;Modulo
   xor rdx, rdx
   div r11                   ; Divide the result
   mov rax, rdx               ; Copy reminder to the result.

   jmp atoi_next

atoi_end:
   ret
   

print_message:
   mov       rax, SYS_WRITE          ; system call for write
   mov       rdi, STDOUT             ; file handle 1 is stdout
   mov       rsi, WRITE_BUFFER            ; address of string to output
   mov       rdx, rcx               ; number of bytes
   syscall                          
   ret

read_buffer:
   mov rax, SYS_READ
   mov rdi, STDIN
   mov rsi, READ_BUFFER
   mov rdx, BUFFER_SIZE
   syscall
   mov r14, rax     ; Move number of bytes read to preserved register
   mov rbx, READ_BUFFER
   ret

modulo:
   push rdx                  ; Save current address of coefficent (it would be destroyed by div)
   xor rdx, rdx             
   div r11                  
   mov rax, rdx               ; Copy reminder to the result.
   pop rdx
   ret

; Exit code has to be in rdi
exit:
   mov       rax, SYS_EXIT           ; system call for exit
   syscall                           ; invoke operating system to exit


section   .data
   READ_BUFFER TIMES BUFFER_SIZE db 0
   WRITE_BUFFER TIMES BUFFER_SIZE db 0
