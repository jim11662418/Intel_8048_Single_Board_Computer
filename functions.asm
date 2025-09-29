            PAGE 0              ; suppress page headings in ASW listing file

; RAM:  00-07   register bank 0
;       08-17   stack
;       18-1F   register bank 1
;       20-7F   data RAM

            cpu 8048
            
; 664 cycles*1.5 탎ec/cycle=996 탎ec
; uses R0 and R1
_1mSec:     mov R0,#2
            mov R1,#164
            djnz R1,$
            djnz R0,$-4
            ret

; 6660 cycles*1.5 탎ec/cycle=9990 탎ec
; uses R0 and R1
_10mSec:    mov R0,#13
            mov R1,#255
            djnz R1,$
            djnz R0,$-4
            ret
            
; 66664 cycles*1.5 탎ec/cycle=99996 탎ec
; uses R0 and R1
_100mSec:   mov R0,#130
            mov R1,#255
            djnz R1,$
            djnz R0,$-4
            mov R0,#50
            djnz R0,$
            ret
            
; 666666 cycles*1.5 탎ec/cycle=999999 탎ec
; uses R0 and R1
_1Sec:      mov R0,#200
            mov R1,#0
            nop
            mov R2,#4
            nop
            djnz R2,$
            djnz R1,$-5
            djnz R0,$-10
            mov R0,#30
            djnz R0,$
            ret            
            
RX0         reg R0
AEX         reg R2            

; The following routines are written as subroutines. R0 and R1 are
; used as data pointers, R2 (AEX) is used as an extension of the accumulator
; and R3 is used as a loop counter.

; double add
dadd:       dec RX0                 ;get low byte and add to a
            add a,@RX0
            inc RX0                 ;get hi byte and add to AEX
            xch a,AEX
            addc a,@RX0
            xch a,AEX
            ret

; double subtract
dsub:       dec RX0                 ;get low byte and sub fRX0m a
            cpl a
            add a,@RX0
            cpl a
            inc RX0                 ;get hi byte and sub fRX0m AEX
            xch a,AEX
            cpl a
            addc a,@RX0
            cpl a
            xch a,AEX
            ret

; double load
dld:        dec RX0                 ;get low byte and place in a
            mov a,@RX0
            inc RX0                 ;get hi byte and place in AEX
            xch a,AEX
            mov a,@RX0
            xch a,AEX
            ret

; double store
dst:        dec RX0                 ;move a into low byte
            mov @RX0,a
            inc RX0                 ;move AEX into high byte
            xch a,AEX
            mov @RX0,a
            xch a,AEX
            ret
            
; double exchange
dex:        dec RX0                 ;exchange A and low byte
            xch a,@RX0
            inc RX0                 ;exchange AEX and high byte
            xch a,AEX
            xch a,@RX0
            xch a,AEX
            ret
            
; double left logical shift
llsh:       rlc a                       ;shift a
            xch a,R2                    ;shift R2
            rlc a
            xch a,R2
            ret
            
; double right logical shift
rlsh:       xch a,R2 ;shift R2
            rrc a
            xch a,R2
            rrc a                       ; shift a
            ret
            
; double right arithmetic shift
rash:       clr c                       ;set carry
            cpl c
            xch a,R2                    ;if R2[7]<>1 then
            jb7 $+3
            clr c                       ;clear carry
            rrc a                       ;shift c into R2
            xch a,R2
            rrc a                       ;shift a
            ret
            
; single precision binary multiply
; this routine assumes a one-byte multiplier and a one-byte multiplicand.
; the product, therefore, is two bytes long. the algorithm follows these steps:
;   1. the registers are arranged as follows:
;       ACC - 0
;       R1  - multiplier
;       R2  - multiplicand
;       R3  - loop counter (= 8)
;
;   the accumulator and register R1 are treated as a register pair when they are
;   shifted right (see step 2).             
;
;   2. the accumulator and R1 are shifted right one place, thus the lsb of the
;   multiplier goes into the carry. 
;
;   3. the multiplicand is added to the accumulator if the carry bit is a 'one'. no
;   action if the carry is a 'zero'. 
;
;   4. decrement the loop counter and loop (return to step 2) until it reaches zero.
;
;   5. shift the result right one last time just before exiting the routine
;
; the result will be found in the accumulator (ms byte) and R1 (ls byte). 

; binary multiply
bmpy:       mov R3,#08h                 ;set counter to 8
            clr a                       ;clear a
            clr c                       ;clear carry bit
bmpy1:      rrc a                       ;double shift right acc & R1
            xch a,R1                    ;into carry
            rrc a
            xch a,R1
            jnc bmpy3                   ;if carry=1 add, otherwise don't
            add a,R2                    ;add multiplicand to accumulator
bmpy3:      djnz R3,bmpy1               ;decrement counter and loop if 0
            rrc a                       ;do a final right shift at the
            xch a,R1                    ;end of the routine
            rrc a
            xch a,R1
            
;------------------------------------------------------------------------            
; convert the lower case character in A to upper case 
; uses R3           
;------------------------------------------------------------------------
toupper:    mov R3,A            ; save character in R3
            mov A,#'a'
            call compare
            jnc toupper1       ; jump if less than 61H (already upper case)
            mov A,#'z'+1
            call compare
            jc toupper1        ; jump if greater than 7AH
            mov A,#20H
            cpl A
            inc A
            add A,R3           ; subtract 20H to convert to uppercase
            ret
            
toupper1:   mov A,R3
            ret            
        
;------------------------------------------------------------------------
; print the string in page 3 of program memory pointed to by R0.
; the string must be terminated by zero.
; uses R0
;------------------------------------------------------------------------            
txtout:     mov A,R0
            movp3 A,@A          ; move to A from page 3 of program memory
            anl A,#07FH
            jz txtdone
            call putch
            inc R0
            jmp txtout
txtdone:    ret       

;------------------------------------------------------------------------            
; compare the value in R3 to the value in the Accumulator.
; returns with zero flag set if the value in R3 is equal to the value in the Accumulator.
; returns with carry flag set if the value in R3 is equal to or greater than the value in the Accumulator.
; returns with carry flag cleared if the value in R3 is less than the value in the Accumulator.
;------------------------------------------------------------------------
compare:    cpl A
            inc A
            add A,R3
            ret
            
;------------------------------------------------------------------------
; print carriage return and line feed
;------------------------------------------------------------------------            
newline:    mov A,#CR
            call putch
            mov A,#LF
            jmp putch
            
;------------------------------------------------------------------------
; print a space
;------------------------------------------------------------------------            
space:      mov A,#' '
            jmp putch
            
            
;------------------------------------------------------------------------            
; prints the contents of the accumulator as two hex digits
;------------------------------------------------------------------------
printhex:   mov R0,A            ; save the value on A in R0
            rr A
            rr A
            rr A
            rr A
            call hex2ascii
            call putch          ; print the most significant digit
            mov A,R0            ; recall the value from R0
            call hex2ascii
            call putch          ; print the least significant digit
            ret

; returns the ASCII value for the hex nibble in A
hex2ascii:  anl A,#0FH
            movp3 A,@A
            ret

            org 0200H
;------------------------------------------------------------------------
; sends the character in A out from the serial output (P2.7)
; uses A, R6 and R7.
; 9600 bps N-8-1
;------------------------------------------------------------------------
putch:      anl P2,#7FH             ; make serial output low to send the start bit
            mov R6,#8               ; load R6 with the number of bits to send
            mov R7,#30              ; load the number of cycles to delay into R7
            djnz R7,$               ; delay 60 cycles
            nop

            ;send bits 0-7
putch1:     jb0 putch2              ; jump if the bit to send is "1"
            anl P2,#7FH             ; else, send "0"
            jmp putch3              ; skip the next part
putch2:     orl P2,#80H             ; send "1"
            jmp putch3              ; makes the timing equal for both "0" and "1" bits
putch3:     rr A                    ; rotate the next bit into position
            mov R7,#29              ; load the number of cycles to delay into R7
            djnz R7,$               ; delay 58 cycles
            djnz R6,putch1          ; loop to send all 8 bits

            ;send the stop bit
            nop
            nop
            orl P2,#80H             ; make serial output high to send the stop bit
            mov R7,#18              ; load the number of cycles to delay into R7
            djnz R7,$               ; delay 36 cycles (1/2 bit time)
            ret

;------------------------------------------------------------------------
; waits for a character from the serial input (T0).
; returns the character in A.
; uses A, R6 and R7.
; 9600 bps N-8-1
;------------------------------------------------------------------------
getch:      jt0 getch               ; wait here for the start bit
            clr A                   ; start with A cleared
            mov R6,#8               ; load the number of bits to receive into R6
            mov R7,#18              ; load the number of cycles to delay into R7
            djnz R7,$               ; delay 36 cycles (1/2 bit time)

            ;get bits 0-7
getch1:     mov R7,#29              ; load the number of cycles to delay into R7
            djnz R7,$               ; delay 58 cycles (1 bit time)
            jnt0 getch2             ; jump if the serial input is zero
            orl A,#01H              ; else, set the bit of the recieved character
            jmp getch3              ; skip the next part
getch2:     anl A,#0FEH             ; clear the bit of the received character
            jmp getch3              ; makes the timing equal for both "0" and "1" bits
getch3:     rr A                    ; rotate the bits in the received character right
            djnz R6,getch1          ; loop to receive all 8 bits

            ;stop bit
            mov R7,#18              ; load the number of cycles to delay into R7
            djnz R7,$               ; delay 36 cycles (1/2 bit time) for the stop bit
            ret

;------------------------------------------------------------------------
; waits for a character from the serial input (T0).
; echos the character bit by bit (output on P2.7).
; returns the character in A.
; uses A, R6 and R7.
; 9600 bps N-8-1
;------------------------------------------------------------------------
getche:     jt0 getche              ; wait here for the start bit
            clr A                   ; start with A cleared
            mov R6,#8               ; load the number of bits to receive into R6
            mov R7,#14              ; load the number of cycles to delay into R7
            djnz R7,$               ; delay 28 cycles
            anl P2,#7FH             ; make serial output low to send the start bit
            mov R7,#2               ; load the number of cycles to delay into R7
            djnz R7,$               ; delay 4 cycles
            nop

            ;get bits 0-7
getche1:    mov R7,#28              ; load the number of cycles to delay into R7
            djnz R7,$               ; delay 56 cycles
            jnt0 getche2            ; jump if the serial input is zero
            orl P2,#80H             ; send "1"
            orl A,#01H              ; else, set the bit of the recieved character
            jmp getche3             ; skip the next part
getche2:    anl P2,#7FH             ; else, send "0"
            anl A,#0FEH             ; clear the bit of the received character
            jmp getche3             ; makes the timing equal for both "0" and "1" bits
getche3:    rr A                    ; rotate the bits in the received character right
            djnz R6,getche1         ; loop to receive all 8 bits

            ;stop bit
            mov R7,#29              ; load the number of cycles to delay into R7
            djnz R7,$               ; delay 58 cycles
            orl P2,#80H             ; make serial output high to send the stop bit
            mov R7,#29              ; load the number of cycles to delay into R7
            djnz R7,$               ; delay 58 cycles
            ret
            
;------------------------------------------------------------------------              
; get two hex digits from the serial port. echo the digits. 
; return with carry set if ESCAPE, RETURN or SPACE 
; else, return the two hex digits as the corresponding byte in the accumulator.
; in addition to A, uses R3, R4, R6 and R7
;------------------------------------------------------------------------            
get2hex:    call get1hex        ; get the most significant hex digit
            jc get2hex1         
            rl A
            rl A
            rl A
            rl A
            mov R4,A            ; save it in R1
            call get1hex        ; get the least signficant digit
            jc get2hex1
            orl A,R4            ; combine the two digits into A
get2hex1:   ret
            
; get a hex digit from serial port. echo the digit.       
; return with carry set if ESCAPE, RETURN or SPACE     
; else, return the hex digit as a nibble in A 
get1hex:    call getch
            call toupper        ; convert the character to upper case
            mov R3,A            ; save the character in R3
            mov A,#ESC
            call compare
            jz gethex2
            mov A,#0DH
            call compare
            jz gethex2
            mov A,#' '
            call compare
            jz gethex2
            mov A,#'0'          
            call compare
            jnc get1hex         ; jump if the character in R3 is less than '0'
            mov A,#'9'+1
            call compare
            jc get1hex1         ; jump if the character in R3 is equal to or greater than ':'
            mov A,R3            ; else, recall the character from R3
            call putch          ; echo the character
            anl A,#0FH
            clr c               ; clear carry
            ret                 ; return with digit 0-9
            
get1hex1:   mov A,#'A'
            call compare
            jnc get1hex         ; jump if the character in R0 is less than 'A'
            mov A,#'F'+1
            call compare
            jc get1hex          ; jump if the character in R0 is equal to or greater than 'G'
            mov A,R3            ; else, recall the character from R3
            call putch          ; echo the character
            anl A,#0FH
            add A,#9
            ret                 ; return with digit A-F
            
gethex2:    mov A,R3            ; retrieve the character from R3
            clr c
            cpl c               ; return with carry set
            ret
            
            
; interrupt handling
; this interrupt routine assumes single level interrupt. the purpose is to store the status of
; the machine at the time the interrupt occurs by storing contents of all registers, 
; accumulator, and the status word. at the end of the interrupt the state of the machine
; is restored and interrupts are enabled again.             

intrpt:     sel rb1                 ;save working registers
            mov @R0,a               ;R0 in alternate register
                                    ;bank contains sacc
                                    ;pointer for saving
                                    ;accumulator
; service interrupt here

            ;mov R0,sacc            ;restore sacc
            mov a,@R0               ;restore accumulator
            retr                    ;restore working registers
                                    ;restore psw and
                                    ;re-enable interrupts

            end