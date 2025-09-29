            PAGE 0              ; suppress page headings in ASW listing file
            
; switch between memory bank 0 and memory bank 1            
; serial I/O at 9600 bps
            
            include "bitfuncs.inc"

            cpu 8048

CR          equ 0DH
LF          equ 0AH
ESC         equ 1BH

; RAM:  00-07     register bank 0
;       08-17     stack
;       18-1F     register bank 1
;       20-7F     data RAM
;
; ROM:  0000-07FF memory bank 0
;       0800-0FFF memory bank 1  

ram:        equ 20H                 ; usable RAM starts at 20H

            org 0000H               ; memory bank 0
reset:      jmp entry

            org 0003H               ; external interrupt
extisr:     retr

            org 0007H               ; timer overflow interrupt
timerisr:   retr

            org 0010H
entry:      dis I                   ; disable external interrupt
            dis TCNTI               ; disable timer overflow interrupt
            orl P2,#80H             ; set serial output high (mark)

start:      mov R0,#lo(titletxt)
loop:       mov A,R0
            movp3 A,@A              ; move to A from page 3 of program memory
            anl A,#0FFH
            jz next
            
; If a SEL MB instruction is issued before a CALL, it affects only the subroutine called. The return restores PC bit 11
; to its previous value (see NOTE below). A SEL MB issued before a jump instruction modifies PC bit 11 permanently.
; NOTE: While PC bit 11 is restored on returning from a CALL, the 'designate bank' internal flip-flop (DBF) is not. 
; This means you must reset the DBF with another SEL before issuing another jump instruction.            
            sel MB1                 ; select memory bank 1
            call putch              ; call function in memory bank 1
            sel MB0                 ; select memory bank 0 (see above)
            inc R0
            jmp loop
            
next:       jmp $            
  
            org 0300H               ; page 3 of program memory bank 0
titletxt:   db  CR,LF
            db  "Memory Bank switch test",CR,LF
            db  "Assembled on ",DATE," at ",TIME,CR,LF,0
  
            org 0800H               ; memory bank 1
;------------------------------------------------------------------------
; sends the character in A out from the serial output (P2.7)
; uses A, R6 and R7.
;------------------------------------------------------------------------
putch:      anl P2,#7FH             ; make serial output low to send the start bit
            mov R6,#8               ; load R6 with the number of bits to send
            mov R7,#(58-2)/2        ; load the number of cycles to delay into R7
            call delay              ; delay 58 cycles
            nop

            ;send bits 0-7
putch1:     jb0 putch2              ; jump if the bit to send is "1"
            anl P2,#7FH             ; else, send "0"
            jmp putch3              ; skip the next part
putch2:     orl P2,#80H             ; send "1"
            jmp putch3              ; makes the timing equal for both "0" and "1" bits
putch3:     rr A                    ; rotate the next bit into position
            mov R7,#(56-2)/2        ; load the number of cycles to delay into R7
            call delay              ; delay 56 cycles
            djnz R6,putch1          ; loop to send all 8 bits

            ;send the stop bit
            nop
            nop
            orl P2,#80H             ; make serial output high to send the stop bit
            mov R7,#(34-2)/2        ; load the number of cycles to delay into R7
            call delay              ; delay 34 cycles (1/2 bit time)
            ret

;------------------------------------------------------------------------
; waits for a character from the serial input (T0).
; returns the character in A. 
; uses A, R6 and R7.
;------------------------------------------------------------------------
getch:      jt0 getch               ; wait here for the start bit
            clr A                   ; start with A cleared
            mov R6,#8               ; load the number of bits to receive into R6
            mov R7,#(30-2)/2        ; load the number of cycles to delay into R7
            call delay              ; delay 30 cycles (1/2 bit time)

            ;get bits 0-7
getch1:     mov R7,#(56-2)/2        ; load the number of cycles to delay into R7
            call delay              ; delay 56 cycles (1 bit time)
            jnt0 getch2             ; jump if the serial input is zero
            orl A,#01H              ; else, set the bit of the recieved character
            jmp getch3              ; skip the next part
getch2:     anl A,#0FEH             ; clear the bit of the received character
            jmp getch3              ; makes the timing equal for both "0" and "1" bits
getch3:     rr A                    ; rotate the bits in the received character right
            djnz R6,getch1          ; loop to receive all 8 bits

            ;stop bit
            mov R7,#(34-2)/2        ; load the number of cycles to delay into R7
            call delay              ; delay 34 cycles (1/2 bit time) for the stop bit
            ret
            
;------------------------------------------------------------------------
; waits for a character from the serial input (T0).
; echos the character bit by bit (output on P2.7).
; returns the character in A.
; uses A, R6 and R7.
;------------------------------------------------------------------------
getche:     jt0 getche              ; wait here for the start bit
            clr A                   ; start with A cleared
            mov R6,#8               ; load the number of bits to receive into R6
            mov R7,#(26-2)/2        ; load the number of cycles to delay into R7
            call delay              ; delay 26 cycles (1/2 bit time)
            anl P2,#7FH             ; make serial output low to send the start bit
            mov R7,#(4-2)/2         ; load the number of cycles to delay into R7
            call delay              ; delay 4 cycles

            ;get bits 0-7
getche1:    mov R7,#(54-2)/2        ; load the number of cycles to delay into R7
            call delay              ; delay 54 cycles (1 bit time)
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
            mov R7,#(56-2)/2        ; load the number of cycles to delay into R7
            call delay              ; delay 56 cycles (1 bit time)
            orl P2,#80H             ; make serial output high to send the stop bit    
            mov R7,#(56-2)/2        ; load the number of cycles to delay into R7
            call delay              ; delay 56 cycles (1 bit time) for the stop bit            
            ret            

;------------------------------------------------------------------------
; delay the number of cycles in R7
; mov R7,#(cycles-2)/2      ; load R7 with number of cycles to delay
; call delay                ; call the delay function
;------------------------------------------------------------------------
delay:      djnz R7,delay
            ret

            end