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
; USED REGISTERS:
; rax, r12, rsi, rcx, r13  
_start:   
   pop rax                          ; Get number of arguments + 1.
   dec rax                          ; Make it real number of arguments.

   cmp rax, 0                       ; Number of coeffs has to be more than 0.
   je exit_1

   mov r12, rax            ; Copy number of args to preserved register.
   mov rcx, rax                      ; Use rcx as loop counter for putting coeffs on stack.
 
   pop rsi                          ; Discard program name.
   
   mov r13, rsp       ; Copy value of stack pointer

; Replace pointers to strings on stack to their actual value.
; In r13 there will be address pointing after the last coefficent.
put_coeffs_on_stack:   
   mov r10, [r13]               
   call atoi
   mov [r13], eax
   add r13, 8
   loop put_coeffs_on_stack

; r8b - first utf8 byte
; r9b - second utf8 byte
; r10b - third utf8 byte
; r11b - fourth utf8 byte
; r12 - first unsafe adrdress in WRITE_BUFFER 
; (if pointer is at this position at the start of loop, we have to flush the buffer.)
; r14 - number of bytes to read in read_buffer
; r15 - WRITE_BUFFER pointer to next empty cell.
parse_input:
   mov [NUM_COEFFS], r12 
   sub r13, 8  ; Make r13 point to last coefficent in memory.
   xor r14, r14 ; Use r14 as counter in READ_BUFFER
   mov r15, WRITE_BUFFER ; Use r15 as counter in WRITE_BUFFER
   mov r12, WRITE_BUFFER ; Use r12 as last address in write buffer
   add r12, BUFFER_SIZE_WITH_ROOM
parse_buffer_loop:
   cmp r15, r12     ; Check if WRITE_BUFFER has to be flushed.
   jle parse_buffer_utf8_to_unicode
   call print_write_buffer
   mov r15, WRITE_BUFFER   ; Reset r15 to the beggining of WRITE_BUFFER

parse_buffer_utf8_to_unicode:
   cmp r14, 0                   ; Check if READ_BUFFER must be refilled or there is eof.
   jne parse_buffer_continue_0
   call parse_buffer_check_end  ; Refill or exit if empty.
parse_buffer_continue_0:

   ;Read first byte from buffer.
   xor r8d, r8d         ; Clear buffer after last loop.
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
   xor r9d, r9d           
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
   mov cl, 0x8f

   cmp r8b, 0xe0
   cmove ax, r9w

   cmp r8b, 0xf0
   cmove ax, r11w

   cmp r8b, 0xf4
   cmove dx, cx

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
   xor r10d, r10d
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
   xor r11d, r11d
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
   call print_write_buffer
   jmp exit_1


parse_buffer_check_end:
   call read_buffer
   cmp rax, 0             ; Check is syscall returned 0
   je parse_buffer_exit
   mov rbx, READ_BUFFER   ; Reset rbx to the beginning of read buffer
   mov r14, rax           ; Move number of bytes read to preserved register.
   ret

parse_buffer_exit:
   call print_write_buffer
   jmp exit_0

;----------------------------------------

decode_utf8_two_bytes:
   and r8b, 0x1f
   and r9b, 0x3f
   cmp r8b, 2
   jb parse_buffer_error

   shl r8d, 6
   or r8d, r9d
   jmp parse_buffer_apply_polynomial_and_unicode_to_utf8

decode_utf8_three_bytes:
   and r8b, 0x0f 
   and r9b, 0x3f 
   and r10b, 0x3f 
   
   shl r8d, 12
   shl r9d, 6

   or r8d, r9d
   or r8d, r10d
   jmp parse_buffer_apply_polynomial_and_unicode_to_utf8


decode_utf8_four_bytes:
   and r8b, 0x07
   and r9b, 0x3f 
   and r10b, 0x3f 
   and r11b, 0x3f 
   
   shl r8d, 18
   shl r9d, 12
   shl r10d, 6

   or r8d, r9d
   or r8d, r10d
   or r8d, r11d


parse_buffer_apply_polynomial_and_unicode_to_utf8:
   call apply_polynomial
   mov r8d, eax
   cmp r8d, 0x80
   jb encode_utf8_one_byte
   cmp r8d, 0x0800
   jb encode_utf8_two_bytes
   cmp r8d, 0x010000
   jb encode_utf8_three_bytes
   cmp r8d, 0x0110000
   jb encode_utf8_four_bytes


encode_utf8_one_byte:
   mov [r15], r8b
   inc r15
   jmp parse_buffer_loop


encode_utf8_two_bytes:
   mov r9d, r8d
   shr r8d, 6
   and r8b, 0x1f

   or r8b, 0xc0
   and r9b, 0x3f
   or r9b, 0x80


   mov [r15], r8b
   inc r15
   mov [r15], r9b
   inc r15
   jmp parse_buffer_loop
   
encode_utf8_three_bytes:
   mov r9d, r8d
   mov r10d, r8d

   shr r8d, 12
   shr r9d, 6

   and r8b, 0x0f
   and r9b, 0x3f
   and r10b, 0x3f

   or r8b, 0xe0
   or r9b, 0x80
   or r10b, 0x80

   mov [r15], r8b
   inc r15
   mov [r15], r9b
   inc r15
   mov [r15], r10b
   inc r15
   jmp parse_buffer_loop

encode_utf8_four_bytes:
   mov r9d, r8d
   mov r10d, r8d
   mov r11d, r8d

   shr r8d, 18
   shr r9d, 12
   shr r10d, 6

   and r8b, 0x07
   and r9b, 0x3f
   and r10b, 0x3f
   and r11b, 0x3f

   or r8b, 0xf0
   or r9b, 0x80
   or r10b, 0x80
   or r11b, 0x80

   mov [r15], r8b
   inc r15
   mov [r15], r9b
   inc r15
   mov [r15], r10b
   inc r15
   mov [r15], r11b
   inc r15
   jmp parse_buffer_loop

; Get unicode value from r8d and put result of polynomial evaluation in eax.
apply_polynomial:
   sub r8d, 0x80
   mov r11, MODULO_VALUE 
   mov r9, [NUM_COEFFS] ; Now in r9 there will be number of coefficents left.

   cmp r9, 0            ; If there are none - exit.
   je apply_polynomial_exit

   xor eax, eax
   mov rcx, r13     ; Now in rcx there will be current address of coefficent.


apply_polynomial_loop:
   cmp r9, 1          ; When only a_0 is left, end loop
   je apply_polynomial_exit

   ; Compute value * (eax + a_k)
   add eax, [rcx]
   xor edx, edx
   mul r8d    

   ;Compute modulo
   div r11d                  
   mov eax, edx              

   dec r9
   sub rcx, 8

   jmp apply_polynomial_loop

apply_polynomial_exit:
   add eax, [rcx]  

   ;Cmopute modulo.
   xor edx, edx             
   div r11d                  
   mov eax, edx              

   add eax, 0x80

   ret

; Takes address of string in r10 and returns integer in eax.
; Modified registers: 
; r8 - Used as buffer for digit.
; r9 - Used to store number 10 
; r10 - Address of string is being iterated.
; r11 - Used to store MODULO_VALUE
; rdx - Is being modified by mul and div instructions.
atoi:
   xor eax, eax
   xor r9d, r9d
   mov r9b, 10
   mov r11d, MODULO_VALUE 

atoi_next:
   xor r8d, r8d

; Copy digit from memory to register.
   mov r8b, [r10]
   inc r10

 ; Check if r8b contains digit.
   cmp r8b, '0'                       
   jb atoi_end
   cmp r8b, '9'
   ja atoi_end

   sub r8b, '0' ; Change r8b value from ascii character to digit value.
   xor edx, edx

 ; Compute rax * 10 + digit
   mul r9d      
   add eax, r8d

   ; Compute modulo.
   div r11d                   ; Divide the result
   mov eax, edx               ; Copy reminder to the result.

   jmp atoi_next

atoi_end:
   cmp r8b, 0                  ; Check if not digit character was \0, if not then it's not a correct number.
   jne exit_1
   ret
   

; Print r15 bytes from WRITE_BUFFER, using system call write().
print_write_buffer:
   mov       rax, SYS_WRITE         
   mov       rdi, STDOUT            
   mov       rsi, WRITE_BUFFER           

   sub       r15, WRITE_BUFFER      ; r15 was address, so we have to subtract beginning of array to get number of bytes.
   mov       rdx, r15             

   syscall                          
   ret

; Invoke system call read().
; Result will be in variable READ_BUFFER.
; r14 - number of bytes read.
read_buffer:
   mov rax, SYS_READ
   mov rdi, STDIN
   mov rsi, READ_BUFFER
   mov rdx, BUFFER_SIZE
   syscall
   mov r14, rax     ; Move number of bytes read to preserved register
   ret

; Exit with status code 0.
exit_0:
   xor rdi, rdi
   mov       rax, SYS_EXIT          
   syscall                          

; Exit with status code 1.
exit_1:
   mov rdi, 1
   mov       rax, SYS_EXIT          
   syscall                          

section   .bss
   READ_BUFFER resb BUFFER_SIZE
   WRITE_BUFFER resb BUFFER_SIZE
   NUM_COEFFS resq 1
