SYS_WRITE equ 1
SYS_EXIT equ 60
SYS_READ equ 0
STDOUT equ 1
STDIN equ 0
BUFFER_SIZE equ 4096
; There should be 4 bytes of room in buffer, if utf-8 bytes don't allign to the end of the buffer.
BUFFER_SIZE_WITH_ROOM equ 4092

MODULO_VALUE equ 0x10ff80
CONTINUATION_BYTE_LOWER_BOUND equ 0x80
CONTINUATION_BYTE_HIGHER_BOUND equ 0xbf
FIRST_NON_ACII_CHAR equ 0x80
HIGHEST_VALUE_FOR_FIRST_BYTE_UTF8 equ 0xf4

FIRST_UNICODE_VALUE_FOR_TWO_BYTE_ENCODING equ 0x80
FIRST_UNICODE_VALUE_FOR_THREE_BYTE_ENCODING equ 0x0800
FIRST_UNICODE_VALUE_FOR_FOUR_BYTE_ENCODING equ 0x010000


section .text
         global _start
_start:
         pop rax                       ; Get number of arguments + 1.
         dec rax                       ; Make it real number of arguments.

         test rax, rax                 ; Number of coeffs has to be more than 0.
         je exit_1

         mov r12, rax                  ; Copy number of args to preserved register.
         mov rcx, rax                  ; Use rcx as loop counter for putting coeffs on stack.

         pop rsi                       ; Discard program name.

         mov r13, rsp                  ; Copy value of stack pointer

; Replace pointers to strings on stack to their actual value.
; In r13 there will be address pointing after the last coefficent.
put_coeffs_on_stack:
         mov r10, [r13]
         call atoi
         mov [r13], eax
         add r13, 8
         loop put_coeffs_on_stack

; If r14 is 0 then there are no bytes to read from buffer,
; and we have to check if there is more to read.
; Argument is needed to ensure that every label is different.
%macro check_if_buffer_ended 1
         test r14, r14
         jne parse_buffer_continue_%1
         call parse_buffer_check_end
parse_buffer_continue_%1:
%endmacro

%macro read_byte_from_buffer 1
         xor %1d, %1d
         mov %1b, [rbx]
         inc rbx
         dec r14
%endmacro

; I am not using constants for bitmasks,
; because their purpose is determined by value, so they don't need a name.
%macro check_if_continuation_byte_is_correct 1
         mov dl, %1b
         and dl, 11000000b
         cmp dl, 10000000b
         jne parse_buffer_error
%endmacro

; rax - Used to perform logical operations. (scratch)
; rbx - Address pointing to next byte to read from READ_BUFFER.
; rcx - Used to perform logical operations. (scratch)
; rdx - Used to perform logical operations. (scratch)
; rsi - Used to perform logical operations. (scratch)
; rbp - Stores number of polynomial coefficents.
; r8b - First utf8 byte.
; r9b - Second utf8 byte.
; r10b - Third utf8 byte.
; r11b - Fourth utf8 byte.
; r12 - First unsafe adrdress in WRITE_BUFFER.
; (if pointer is at this position at the start of loop, we have to flush the buffer.)
; r13 - Address pointing to the last coeff in memory.
; r14 - Number of bytes to read in read_buffer.
; r15 - WRITE_BUFFER pointer to next empty cell.
parse_input:
         mov rbp, r12
         sub r13, 8                    ; Make r13 point to last coefficent in memory.
         xor r14, r14                  ; At the start there is nothing in READ_BUFFER.
         mov r15, WRITE_BUFFER
         mov r12, WRITE_BUFFER
         add r12, BUFFER_SIZE_WITH_ROOM
parse_buffer_loop:
         cmp r15, r12                  ; Check if WRITE_BUFFER has to be flushed.
         jle parse_buffer_utf8_to_unicode
         call print_write_buffer
         mov r15, WRITE_BUFFER         ; Reset r15 to the beggining of WRITE_BUFFER

parse_buffer_utf8_to_unicode:
         check_if_buffer_ended 0
         read_byte_from_buffer r8

         cmp r8b, FIRST_NON_ACII_CHAR  ; Check if value is smaller then 0x80, then we know it's ascii character
         jb encode_utf8_one_byte

         cmp r8b, HIGHEST_VALUE_FOR_FIRST_BYTE_UTF8
         ja parse_buffer_error

         check_if_buffer_ended 1
         read_byte_from_buffer r9

; Compute lower and higher bounds for second byte
         mov ax, CONTINUATION_BYTE_LOWER_BOUND
         mov dx, CONTINUATION_BYTE_HIGHER_BOUND

         mov si, 0xa0
         mov r10w, 0x90
         mov cl, 0x8f

         cmp r8b, 0xe0
         cmove ax, si

         cmp r8b, 0xf0
         cmove ax, r10w

         cmp r8b, 0xf4
         cmove dx, cx

; Check if second byte is correct
         cmp r9b, al
         jb parse_buffer_error
         cmp r9b, dl
         ja parse_buffer_error

; Check if it's 2 byte character
         mov dl, r8b
         and dl, 11100000b
         cmp dl, 11000000b
         je decode_utf8_two_bytes

         check_if_buffer_ended 2
         read_byte_from_buffer r10
         check_if_continuation_byte_is_correct r10

; Check if it's 3 byte character
         mov dl, r8b
         and dl, 11110000b
         cmp dl, 11100000b
         je decode_utf8_three_bytes

         check_if_buffer_ended 3
         read_byte_from_buffer r11
         check_if_continuation_byte_is_correct r11

; Check if it's 4 byte character
         mov dl, r8b
         and dl, 11111000b
         cmp dl, 11110000b
         je decode_utf8_four_bytes

; If none of this cases is true then error
parse_buffer_error:
         call print_write_buffer
         jmp exit_1

parse_buffer_check_end:
         call read_buffer
         test rax, rax                 ; Check is syscall returned 0
         je parse_buffer_exit
         mov rbx, READ_BUFFER          ; Reset rbx to the beginning of read buffer
         mov r14, rax                  ; Move number of bytes read to preserved register.
         ret

parse_buffer_exit:
         call print_write_buffer
         jmp exit_0

; ---------------------------------------

decode_utf8_two_bytes:
         and r8b, 00011111b
         and r9b, 00111111b

; If value of the first byte is 0 or 1
; then this unicode char can be coded on one byte, so it's incorrect.
         cmp r8b, 2
         jb parse_buffer_error

         shl r8d, 6
         or r8d, r9d
         jmp parse_buffer_apply_polynomial_and_unicode_to_utf8

decode_utf8_three_bytes:
         and r8b, 00001111b
         and r9b, 00111111b
         and r10b, 00111111b

         shl r8d, 12
         shl r9d, 6

         or r8d, r9d
         or r8d, r10d
         jmp parse_buffer_apply_polynomial_and_unicode_to_utf8

decode_utf8_four_bytes:
         and r8b, 00000111b
         and r9b, 00111111b
         and r10b, 00111111b
         and r11b, 00111111b

         shl r8d, 18
         shl r9d, 12
         shl r10d, 6

         or r8d, r9d
         or r8d, r10d
         or r8d, r11d

parse_buffer_apply_polynomial_and_unicode_to_utf8:
         call apply_polynomial
         mov r8d, eax
         cmp r8d, FIRST_UNICODE_VALUE_FOR_TWO_BYTE_ENCODING
         jb encode_utf8_one_byte
         cmp r8d, FIRST_UNICODE_VALUE_FOR_THREE_BYTE_ENCODING
         jb encode_utf8_two_bytes
         cmp r8d, FIRST_UNICODE_VALUE_FOR_FOUR_BYTE_ENCODING
         jb encode_utf8_three_bytes
         jmp encode_utf8_four_bytes

encode_utf8_one_byte:
         mov [r15], r8b
         inc r15
         jmp parse_buffer_loop

encode_utf8_two_bytes:
         mov r9d, r8d
         shr r8d, 6

; Get only bits that code unicode values.
         and r8b, 00011111b
         and r9b, 00111111b

; Add utf8 "headers" to bytes.
         or r8b, 11000000b
         or r9b, 10000000b

; Put bytes to WRITE_BUFFER
         mov [r15], r8b
         mov [r15 + 1], r9b
         add r15, 2
         jmp parse_buffer_loop

; Rest of encoding functions is analogous to first one.
encode_utf8_three_bytes:
         mov r9d, r8d
         mov r10d, r8d

         shr r8d, 12
         shr r9d, 6

         and r8b, 00001111b
         and r9b, 00111111b
         and r10b, 00111111b

         or r8b, 11100000b
         or r9b, 10000000b
         or r10b, 10000000b

         mov [r15], r8b
         mov [r15 + 1], r9b
         mov [r15 + 2], r10b
         add r15, 3
         jmp parse_buffer_loop

encode_utf8_four_bytes:
         mov r9d, r8d
         mov r10d, r8d
         mov r11d, r8d

         shr r8d, 18
         shr r9d, 12
         shr r10d, 6

         and r8b, 00000111b
         and r9b, 00111111b
         and r10b, 00111111b
         and r11b, 00111111b

         or r8b, 11110000b
         or r9b, 10000000b
         or r10b, 10000000b
         or r11b, 10000000b

         mov [r15], r8b
         mov [r15 + 1], r9b
         mov [r15 + 2], r10b
         mov [r15 + 3], r11b
         add r15, 4
         jmp parse_buffer_loop

; Get unicode value from r8d and put result of polynomial evaluation in eax.
; Resulting polynomial is of form (w(r8d - 0x80) % MODULO_VALUE) + 0x80.
; Used registers:
; r9, rcx, rdx
apply_polynomial:
         sub r8d, 0x80
         mov r11, MODULO_VALUE
         mov r9, rbp                   ; Now in r9 there will be number of coefficents left.

         test r9, r9                   ; If there are none - exit.
         je apply_polynomial_exit

         xor eax, eax
         mov rcx, r13                  ; Now in rcx there will be current address of coefficent.

apply_polynomial_loop:
         cmp r9, 1                     ; When only a_0 is left, end loop
         je apply_polynomial_exit

; Compute value * (eax + a_k)
         add eax, [rcx]
         xor rdx, rdx
         mul r8

; Compute modulo
         call modulo

         dec r9
         sub rcx, 8

         jmp apply_polynomial_loop

apply_polynomial_exit:
         add eax, [rcx]
         call modulo
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

         sub r8b, '0'                  ; Change r8b value from ascii character to digit value.
         xor edx, edx

; Compute rax * 10 + digit
         mul r9
         add eax, r8d

         call modulo

         jmp atoi_next

atoi_end:
         test r8b, r8b                 ; Check if not digit character was \0, if not then it's not a correct number.
         jne exit_1
         ret

; Takes value in rax and computes value % MODULO_VALUE in eax
; Uses rax, rdi, rdx
; Code taken from compiling modulo function in gcc with -O3 flag
modulo:
         mov rdi, rax
         mov rdx, 0x787c03a5c11c4499
         mul rdx
         shr rdx, 0x13
         imul rdx, rdx, 0x10ff80
         sub rdi, rdx
         mov eax, edi
         ret

; Print r15 bytes from WRITE_BUFFER, using system call write().
print_write_buffer:
         mov rax, SYS_WRITE
         mov rdi, STDOUT
         mov rsi, WRITE_BUFFER

         sub r15, WRITE_BUFFER         ; r15 was address, so we have to subtract beginning of array to get number of bytes.
         mov rdx, r15

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
         mov r14, rax                  ; Move number of bytes read to preserved register
         ret

; Exit with status code 0.
exit_0:
         xor rdi, rdi
         mov rax, SYS_EXIT
         syscall

; Exit with status code 1.
exit_1:
         xor rdi, rdi
         inc dil
         mov rax, SYS_EXIT
         syscall

section .bss
         READ_BUFFER resb BUFFER_SIZE
         WRITE_BUFFER resb BUFFER_SIZE
