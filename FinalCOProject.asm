.MODEL SMALL
.STACK 100H

mixcolumns macro constant state result
    local product_loop
    local inner_loop
    local outer_loop  
    ; Outer loop (i = 0 to 3) - iterates over rows of A
    mov si, 0           ; SI = 0 (row index for A)
outer_loop:;Kol wa7da bt7adar lely ba3daha (outerloop btfady el di lel columns, inner loop btfady el ax lel sum)
    ; Inner loop (j = 0 to 3) - iterates over columns of B
    xor di, di           ; DI = 0 (column index for B)
inner_loop:
    ; Zero out the sum for Result[i][j]
    xor ax, ax           ; Clear AX (accumulator for sum)

    ; k loop (k = 0 to 3) - dot product of row i of A and column j of B
    xor dx, dx           ; DX = 0 (k index)
product_loop:
    ; Load galoisFieldMatrix[i][k]
    lea ax, constant ;load address of first element of galoisFieldMatrix to ax
    mov bx, si                ;load i to bx
    shl bx, 2                 ;multiply by 4(=shift by 2 to the left)
    add bx, dx                ;add k
    add bx, ax                ;add address of galoisFieldMatrix[0][0] (base)
    mov al, [bx]              ;load galoisFieldMatrix[i][k]

    ; Load state[k][j]
    lea cx, state             ;load address of firt element of state to ax
    mov bx, dx                ;load k to bx
    shl bx, 2                 ;multibly by 4(=shift by 2 to the left)
    add bx, di                ;add j
    add bx, cx                ;add address of state[0][0] (base)
    mov bl, [bx]              ;load state[k][j]
       
    call MULX
    
    ; Add result to current sum for state[i][j]
    push si
    shl si,2
    add si, di
    xor result[si],bl
    pop si
    ; Increment k
    inc dx
    cmp dx, 4
    jl product_loop      ; If k < 4, continue loop

    ; Increment column index (j)
    inc di
    cmp di, 4
    jl inner_loop        ; If j < 4, continue loop

    ; Increment row index (i)
    inc si
    cmp si, 4
    jl outer_loop        ; If i < 4, continue loop
    
    XOR AX,AX
    XOR CX,CX
    XOR DX,DX
    XOR BX,BX
    MOV SI,0
    MOV DI,0
endm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


AddRoundKey macro state
    local XORING 
    mov ah,0
    mov cx,16
    mov al,0FFh
    mov si,0
    XORING:mov ah,state[si]
    xor ah,al
    mov state[si],ah
    inc si
    loop XORING
    xor ax,ax
    xor cx,cx
    xor bx,bx
    xor dx,dx
    mov si,0
    mov di,0
endm



;;;;;;;;;;;



SubBytes MACRO state S_BOX
    Local loopstart
mov cx, 16               ; Loop counter for 16 bytes
    mov si, offset state     ; SI points to the input
    mov di, offset S_BOX      ; DI points to the SBox
    mov bx, 0                ; Clear BX for indexing in Sbox

loopstart:
    ; Load input byte into AL
    mov al, [si]             ; Load input byte
    xor ah, ah               ; Clear AH to make AX valid for index(use al only)
    
    ; Index into SBox
    mov bl, al               ; Move input byte into BL
    mov al, [di + bx]        ; AL = SBox[input[si]]
    
    ; Store result back into input array
    mov [si], al

    ; Increment pointers
    inc si                   ; Move to next input byte
    loop loopstart
    
    XOR AX,AX
    XOR CX,CX
    XOR DX,DX
    XOR BX,BX
    MOV SI,0
    MOV DI,0           ; Repeat for all input bytes
endm


;;;;;;;;;;;;;;;;;;;;



ShiftRows MACRO state
    ; Shift each row of the 4x4 state matrix (state is assumed to be 4x4)
    ; For example, we can directly swap bytes for now.  
    MOV SI, OFFSET state
    ; Row 1: No shift
    ; Row 1: No change

    ; Row 2: Shift left by 1
    mov al, [si+5]
    mov ah, [si+6]
    mov dl, [si+7]
    mov dh, [si+4]
    mov [si+4], al
    mov [si+5], ah
    mov [si+6], dl
    mov [si+7], dh

    ; Row 3: Shift left by 2
    mov al, [si+8]
    mov ah, [si+9]
    mov dl, [si+10]
    mov dh, [si+11]
    mov [si+8], dl
    mov [si+9], dh
    mov [si+10], al
    mov [si+11], ah

    ; Row 4: Shift left by 3
    mov al, [si+15]
    mov ah, [si+12]
    mov dl, [si+13]
    mov dh, [si+14]
    mov [si+12], al
    mov [si+13], ah
    mov [si+14], dl
    mov [si+15], dh
    XOR AX,AX
    XOR CX,CX
    XOR DX,DX
    XOR BX,BX
    MOV SI,0
    MOV DI,0 
endm



;;;;;;;;;;;;;;;  



.DATA
    state DB 16 DUP(0)          ; Array to hold 16 bytes (128 bits). 
    
    HEXA DB '0123456789ABCDEF'  ; Lookup table for hexadecimal digits. 
    
    PROMPT1 DB 'Enter 128-bit hex input (16 bytes): $'
    
    PROMPT2 DB 13,10,'Output: $'
    
    TOTAL DB 0 ;A VARIABLE FOR READ NUMBER 
    
    MULTIPLIER DB 100 ; used for readNumber 
    
    ;COUNT DW 0   ;USED IN THE PRINTHEXA PROC FOR THE LOOP
    
    ;ROWCOUNT DB 0  ;USED IN THE PRINTHEXA TO INDCATE WHEN THE ROW ENDS AND STARTS A NEW ROW 
                           
                          
    result db 16 dup(?) ; returned from mix coulumns then transferred back to the state
    
    
    constant db 2h, 3h, 1h,1h
             db 1h, 2h, 3h, 1h
             db 1h, 1h, 2h, 3h
             db 3h, 1h, 1h, 2h


    ; Full S-Box for AES (256 values)
    S_BOX DB 63h, 7Ch, 77h, 7Bh, 0F2h, 6Bh, 6Fh, 0c5h, 30h, 01h, 67h, 2Bh, 0FEh, 0D7h, 0ABh, 76h
        db 0CAh, 82h, 0C9h, 7Dh, 0FAh, 59h, 47h, 0F0h, 0ADh, 0D4h, 0A2h, 0AFh, 9Ch, 0A4h, 72h, 0C0h
        db 0B7h, 0FDh, 93h, 26h, 36h, 3Fh, 0F7h, 0CCh, 34h, 0A5h, 0E5h, 0F1h, 71h, 0D8h, 31h, 15h
        db 04h, 0C7h, 23h, 0C3h, 18h, 96h, 05h, 9Ah, 07h, 12h, 80h, 0E2h, 0EBh, 27h, 0B2h, 75h
        db 09h, 83h, 2Ch, 1Ah, 1Bh, 6Eh, 5Ah, 0A0h, 52h, 3Bh, 0D6h, 0B3h, 29h, 0E3h, 2Fh, 84h
        db 53h, 0D1h, 00h, 0EDh, 20h, 0FCh, 0B1h, 5Bh, 6Ah, 0CBh, 0BEh, 39h, 4Ah, 4Ch, 58h, 0CFh
        db 0D0h, 0EFh, 0AAh, 0FBh, 43h, 4Dh, 33h, 85h, 45h, 0F9h, 02h, 7Fh, 50h, 3Ch, 9Fh, 0A8h
        db 51h, 0A3h, 40h, 8Fh, 92h, 9Dh, 38h, 0F5h, 0BCh, 0B6h, 0DAh, 21h, 10h, 0FFh, 0F3h, 0D2h
        db 0cdh, 0ch, 13h, 0ECh, 5Fh, 97h, 44h, 17h, 0C4h, 0A7h, 7Eh, 3Dh, 64h, 5Dh, 19h, 73h
        db 60h, 81h, 4Fh, 0DCh, 22h, 2Ah, 90h, 88h, 46h, 0EEh, 0B8h, 14h, 0DEh, 5Eh, 0Bh, 0DBh
        db 0E0h, 32h, 3Ah, 0Ah, 49h, 06h, 24h, 5Ch, 0C2h, 0D3h, 0ACh, 62h, 91h, 95h, 0E4h, 79h
        db 0E7h, 0C8h, 37h, 6Dh, 8Dh, 0D5h, 4Eh, 0A9h, 6Ch, 56h, 0F4h, 0EAh, 65h, 7Ah, 0AEh, 08h
        db 0BAh, 78h, 25h, 2Eh, 1Ch, 0A6h, 0B4h, 0C6h, 0E8h, 0DDh, 74h, 1Fh, 4Bh, 0BDh, 8Bh, 8Ah
        db 70h, 3Eh, 0B5h, 66h, 48h, 03h, 0F6h, 0Eh, 61h, 35h, 57h, 0B9h, 86h, 0C1h, 1Dh, 9Eh
        db 0E1h, 0F8h, 98h, 11h, 69h, 0D9h, 8Eh, 94h, 9Bh, 1Eh, 87h, 0E9h, 0CEh, 55h, 28h, 0DFh
        db 8Ch, 0A1h, 89h, 0Dh, 0BFh, 0E6h, 42h, 68h, 41h, 99h, 2Dh, 0Fh, 0B0h, 54h, 0BBh, 16h
    

.CODE 

;Note:MUL2 And MUL3 must use bl register(see mixcolumns)
 
MUL2 proc
    SHL bl, 1
    JNC End
    XOR bl, 1Bh 
    End: 
    ret   
    
MUL2 endp



 ; bl is the state , al is the galeo
MUL3 proc
    MOV ah, bl
    CALL MUL2
    XOR bl,ah 
    ret
endp


MULX proc

    CMP al,1     ;Esme3na al? Because it is the one I was using in mixcolumns
    JZ Endy

    CMP al,2
    JZ Multiply2


    CALL MUL3
    JMP Endy
                     
                     
    Multiply2: 
    CALL MUL2
                                                      
    Endy:
    ret
MULX endp


readNumber PROC 
    MOV CX,16
    MOV SI,OFFSET state
    ;OUTER LOOP
    OUTER:   ; The iteration handles 1 byte at a time(2 hexa characters) because memory is byte addressable.
    MOV AH,01H ;This line and the following one mean we are waiting for key press
    int 21h
    sub al,'0';The output from this line will be decimal equivalent of ascii character if it's a number and a large number indicating nothing if it's character.
    CMP AL, 9
    JBE valid_digit ;JBE: jump if less than equal. In other words, this condition is true if al has a number(not a character a,b,c,d,e,f)
    sub al,39  ;-87+48, 48 to return back the '0' subtracted and -87 to get the hexa decimal of the asci letter
    valid_digit:
    MOV TOTAL, AL               ; Store the first nibble in TOTAL.
    SHL TOTAL, 4                ; Shift left 4 bits to make space for the second nibble. 
    ;read the second input digit
    MOV AH,01H
    int 21h
    sub al,'0'
    CMP AL, 9
    JBE valid_digit2
    sub al,39
    valid_digit2:
    OR TOTAL, AL                ; Combine the second nibble into TOTAL.

    ; Store the byte into the state array
    MOV AL, TOTAL               ; Move TOTAL into AL (intermediate register).
    MOV [SI], AL                ; Store AL into the state array.
    INC SI                      ; Move to the next byte in the array.

    ; Clear TOTAL for the next byte
    XOR AL, AL                  ; Clear AL register.
    MOV TOTAL, AL               ; Store the zero value into TOTAL.
    LOOP OUTER              ; Repeat until 16 bytes are read.
     XOR AX,AX
    XOR CX,CX
    XOR DX,DX
    XOR BX,BX
    MOV SI,0
    MOV DI,0
    ret
readNumber ENDP


PRINTHEXA PROC 
     MOV SI,OFFSET state
     MOV DI,16  
     
     START:
     MOV AX,0
     MOV CX,0
     MOV DX,0
     MOV BX,0
     mov dl,[si];mov what is in memory in si(byte ely 3aleha el dor fel input bta3na) in dl
     rol dx,12
     mov cl,dl ; put the higher 4 bits in cl
     mov dl,0
     rol dx,4  ; put the lower 4 bits in dl
     mov al,dl ; put the lower 4 bits in al
     mov ch,0
     mov bl,cl ;put the higher 4 bits in bl 
     mov dl,[HEXA + bx]   ; get the assci for the higer 4 bits and put in dl
     push ax ;to keep track of ax, to not change it when i mov ah,02H later on
     mov ah,02h ;printed the dl, the higher 4 bits in ascii form
     int 21h
     pop ax          
     mov bl,al   ;put the lower 4 bits in bl 
     mov dl,[HEXA + bx]     ; get the assci for the lower 4 bits and put in dl
     mov ah,02h
     int 21h         ;print the lower 4 ascii
     ;INC COUNT
     ;inc ROWCOUNT 
     inc si
     MOV Dx," "
     MOV AH,02H
     INT 21H
     mov ax,0
     dec DI
     JNZ START
      XOR AX,AX
    XOR CX,CX
    XOR DX,DX
    XOR BX,BX
    MOV SI,0
    MOV DI,0    
    RET 
    PRINTHEXA ENDP

ResToState proc ;transfers the result to the state
    mov si,15
    loopy:
    mov al,result[si]
    mov state[si],al
    ;mov al,0
    ;xor result[si],al
    dec si
    jge loopy
    call resToZero  ; to make sure the result array is zero for the following pass
  
 XOR AX,AX
    XOR CX,CX
    XOR DX,DX
    XOR BX,BX
    MOV SI,0
    MOV DI,0
   ret
endp

resToZero proc
    push si
    push ax
    mov si,15
    tozero:
    mov ax,0
    mov result[si],al
    ;cmp si,0
    dec si
    jge tozero
    pop ax 
    pop si
    ret
endp        

; Main procedure
MAIN PROC
    MOV AX, @DATA               ; Initialize the data segment.
    MOV DS, AX
    Mov si,0

    MOV DX, OFFSET PROMPT1
    MOV AH, 09H
    INT 21H

    CALL readNumber

   
    mov dx,0
    mov ax,0 


    ;Apply SubBytes and ShiftRows to the state array
    AddRoundKey state
    mov bx,09h
    
    loopStar:
    push bx
    xor bx,bx
    SubBytes state S_BOX 
    ShiftRows state   
    
    mixcolumns constant state result
    call ResToState
    
    AddRoundKey state   

    pop bx
    dec bx
    cmp bx,0
    jnz loopStar
    SubBytes state S_BOX
    ShiftRows state 
    AddRoundKey state  
    
    MOV DX, OFFSET PROMPT2
    MOV AH, 09H
    INT 21H
    CALL PRINTHEXA

    MOV AH, 4CH                 ; Terminate the program.
    INT 21H
       
MAIN ENDP

END MAIN