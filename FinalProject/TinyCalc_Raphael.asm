TITLE TinyCalc.asm
; Program Description: A simple calculator with: Addition, Subtraction, Multiplication, 
;                      Division, and Modulo. Additionally, maintains running total, and 
;                      at end of session, report is displayed with number of operations 
;                      performed per operation type, total and average of all operations.
; Author: Sybil Raphael
; Creation Date: November 29, 2024
 
INCLUDE Irvine32.inc
INCLUDELIB Irvine32.lib



.data
titleMsg BYTE " ----- Tiny Calculator ----- ", 0                        ; title message
menuMsg BYTE "Select an operation (Aa/Ss/Mm/Dd/%): ", 0                 ; menu message
op1Msg BYTE "Enter the first operand (or 'M' for memory): ", 0          ; message for first operand
op2Msg BYTE "Enter the second operand (or 'M' for memory): ", 0         ; message for second operand
answerMsg BYTE "Answer: ", 0                                            ; message for answer to operation
continueMsg BYTE "Continue? (Yy/Nn): ", 0                               ; message to continue 
invalidMsg BYTE "Invalid input. Try again.", 0                          ; error, invlaild input message
invalidOperationMsg BYTE 0Dh, 0Ah, "Invalid operation. Try again.", 0   ; error, invlaild operation message               
overFlowMsg BYTE 0Dh, 0Ah, "ERROR - Overflow.", 0                       ; error, overflow message
DBZMsg BYTE "Error: Division by zero.", 0                               ; error, division by zero message
reportTitle BYTE " ----- Calculator Report ----- ", 0                   ; report title message
addReport BYTE "Total Additions: ", 0                                   ; displays addition report
subReport BYTE "Total Subtractions: ", 0                                ; displays subtraction report
mulReport BYTE "Total Multiplications: ", 0                             ; displays multiplication report
divReport BYTE "Total Divisions: ", 0                                   ; displays divisions report
modReport BYTE "Total Modulos: ", 0                                     ; displays modulos report
opReport BYTE "Total Operations: ", 0                                   ; displays operations report
runTotReport BYTE "Running Total : ", 0                                 ; displays running total report
avgReport BYTE "Total Operations Average: ", 0                          ; displays total operations average report

operation BYTE ?                                                        ; variable to hold operation type (+,-,*,/,%)
op1 DWORD ?                                                             ; variable to store operand 1
op2 DWORD ?                                                             ; variable to store operand 2
op1Buffer BYTE 20 DUP(0)                                                ; buffer for the first operand (max 20 chars)
op2Buffer BYTE 20 DUP(0)                                                ; buffer for the second operand (max 20 chars)
memoryResult DWORD ?                                                    ; variable to store result in memory
equalS  BYTE " = ", 0                                                   ; = sign
addS BYTE " + ", 0                                                      ; + sign
subS  BYTE " - ", 0                                                     ; - sign
mulS  BYTE " * ", 0                                                     ; * sign
divS  BYTE " / ", 0                                                     ; / sign
modS  BYTE " % ", 0                                                     ; % sign
addCount DWORD 0                                                        ; variable to store count of add operation
subCount DWORD 0                                                        ; variable to store count of sub operation
mulCount DWORD 0                                                        ; variable to store count of mul operation
divCount DWORD 0                                                        ; variable to store count of div operation
modCount DWORD 0                                                        ; variable to store count of mod operation
totalCount DWORD 0                                                      ; variable to store total count of operations
runningTotal DWORD 0                                                    ; variable to store runningTotal
avgOps DWORD 0                                                          ; variable to store avgerage operations



.code
main PROC
;-----------------------------------------------------------------
;Description: Calls procedures that allows user to perform operations.
;Receives: user input for menu 
;Returns: displays calculation results  
;-----------------------------------------------------------------  
mov memoryResult, 0                                                     ; initialize memoryResult
mov runningTotal, 0                                                     ; initialize runningTotal

mov edx, OFFSET titleMsg                                                ; offset, points to titleMsg 
call WriteString                                                        ; displays titleMsg  
call Crlf                                                               ; displays new line 

StartLoop:
    call GetOperationType                                               ; calls GetOperationType
    call GetOperands                                                    ; calls GetOperands
    call ProcessOperation                                               ; calls ProcessOperation
    call AskContinue                                                    ; calls AskContinue
    cmp al, 'N'                                                         ; compares AL to 'N'
    je EndProgram                                                       ; if 'N', jumps to EndProgram
    cmp al, 'n'                                                         ; compares AL to 'n'
    je EndProgram                                                       ; if 'n', jumps to EndProgram
    jmp StartLoop                                                       ; loops to start

EndProgram:
    call Crlf
    call DisplayReport                                                  ; calls DisplayReport
    exit                                                                ; exits program
main ENDP                                                               ; end of main procedure


                                      
GetOperationType PROC
;-----------------------------------------------------------------
;Description: gets operation type from user 
;Receives: Aa/Ss/Mm/Dd/%
;Returns: nothing 
;----------------------------------------------------------------- 
GetOpType:
    mov edx, OFFSET menuMsg                                             ; offset, points to menuMsg 
    call WriteString                                                    ; displays menuMsg
    call ReadChar                                                       ; reads char entered by user 
    call WriteChar                                                      ; displays char 
    
    cmp al, 'A'                                                         ; compares operation to 'A'
    je valid                                                            ; if equal, jumps to valid
    cmp al, 'a'                                                         ; compares operation to 'a'
    je valid                                                            ; if equal, jumps to valid
    cmp al, 'S'                                                         ; compares operation to 'S'
    je valid                                                            ; if equal, jumps to valid
    cmp al, 's'                                                         ; compares operation to 's'
    je valid                                                            ; if equal, jumps to valid
    cmp al, 'M'                                                         ; compares operation to 'M'
    je valid                                                            ; if equal, jumps to valid
    cmp al, 'm'                                                         ; compares operation to 'm'
    je valid                                                            ; if equal, jumps to valid
    cmp al, 'D'                                                         ; compares operation to 'D'
    je valid                                                            ; if equal, jumps to valid
    cmp al, 'd'                                                         ; compares operation to 'd'
    je valid                                                            ; if equal, jumps to valid
    cmp al, '%'                                                         ; compares operation to '%'
    je valid                                                            ; if equal, jumps to valid

    mov edx, OFFSET invalidOperationMsg                                 ; off
    call WriteString                                                    ;
    call Crlf                                                           ;
    jmp GetOpType                                                       ;

valid:
    mov operation, al                                                   ; AL = operation 
    call Crlf                                                           ; displays new line 
    ret
GetOperationType ENDP                                                   ; end of GetOperationType procedure



GetOperands PROC
;-----------------------------------------------------------------
;Description: gets operands from user 
;Receives: integers 
;Returns: nothing 
;-----------------------------------------------------------------
Oppy1:
    mov edx, OFFSET op1Msg                                              ; offset, points to op1Msg
    call WriteString                                                    ; displays op1Msg
    mov edx, OFFSET op1Buffer                                           ; offset, points to op1Buffer
    mov ecx, 10                                                         ; ECX = 10
    call ReadString                                                     ; reads op1
    lea esi, op1Buffer

    mov al, [esi]                                                       ; AL = address of inputBuffer
    cmp al, 'M'                                                         ; compares AL to 'M'
    je load1                                                            ; if equal, jumps to load1
    cmp al, 'm'                                                         ; compares AL to 'm'
    je load1                                                            ; if equal, jumps to load1

    call ParseOperand                                                   ; calls ParseOperand
    cmp ax, 0                                                           ; compares AX to 0
    jne valid1                                                          ; if not equal, jumps to valid1
    cmp BYTE PTR [esi], '0'                                             ; compares to '0'
    je valid1                                                           ; if equal, jumps to valid1

    mov edx, OFFSET invalidMsg                                          ; offset, points to invalidMsg
    call WriteString                                                    ; displays invalidMsg
    mov eax, 0                                                          ; clears result (invalidInput)
    jmp Oppy1                                                           ; jumps to Oppy1

valid1:
    mov op1, eax                                                        ; EAX = op1
    jmp Oppy2                                                           ; jumps to Oppy2

load1:
    mov eax, memoryResult                                               ; EAX = memoryResult
    mov op1, eax                                                        ; op1 = EAX 
    jmp Oppy2                                                           ; jumps to Oppy2

Oppy2:
    mov edx, OFFSET op2Msg                                              ; offset, points to op2Msg
    call WriteString                                                    ; displays op2Msg
    mov edx, OFFSET op2Buffer                                           ; offset, points to op2Buffer
    mov ecx, 10                                                         ; ECX = 10
    call ReadString                                                     ; reads op2
    lea esi, op2Buffer                                                  ; loads efficient address of op2Buffer

    mov al, [esi]                                                       ; AL = address of buffer
    cmp al, 'M'                                                         ; compares AL to 'M'
    je load2                                                            ; if equal, jumps to load1
    cmp al, 'm'                                                         ; compares AL to 'm'
    je load2                                                            ; if equal, jumps to load1

    call ParseOperand                                                   ; calls ParseOperand
    cmp ax, 0                                                           ; compares AX to 0
    jne valid2                                                          ; if not equal, jumps to valid2
    cmp BYTE PTR [esi], '0'                                             ; compares to '0'
    je valid2                                                           ; if equal, jumps to valid2

valid2:
    mov op2, eax                                                        ; op2 = EAX
    jmp done                                                            ; jumps to done

load2:
    mov eax, memoryResult                                               ; EAX = memoryResult
    mov op2, eax                                                        ; op2 = EAX 

done:
    ret                                      
GetOperands ENDP                                                        ; end of GetOperands procedure



ParseOperand PROC 
;-----------------------------------------------------------------
;Description: checks if input 'M' is number, if so, loads memory 
;             result into EAX, otherwise, converts string to integer
;             (uses IMUL for string-to-integer conversion)
;Receives: integers 
;Returns: nothing 
;----------------------------------------------------------------- 
xor ecx, ecx                                                            ; clears regs
xor edx, edx                                                            ; clears regs

mov al, BYTE PTR [esi]                                                  ; AL = address
cmp al, '+'                                                             ; compares AL to '+'
je NextChar                                                             ; if NOT equal, jumps to NectChar
cmp al, '-'                                                             ; compares AL to '-'
jne ParseLoop                                                           ; if NOT equal, jumps to ParseLoop
inc esi                                                                 ; increments ESI 
mov edx, 1                                                              ; moves -1 to EBX for negative
                                       

ParseLoop:
    mov al, [esi]                                                       ; AL = address 
    cmp al, 0                                                           ; compares AL to 0
    je sign                                                             ; if equal, jumps to sign
    sub al, '0'                                                         ; subtracts '0'
    cmp al, '9'                                                         ; compares AL to '9'
    ja InvalidInput                                                     ; if above, jumps to InvalidInput
    imul ecx, ecx, 10                                                   ; multiplies result in ECX by 10
    movzx ebx, al                                                       ; zero-extend AL into EBX
    add ecx, ebx                                                        ; adds digit to answer in ECX

NextChar:
    inc esi                                                             ; increments ESI
    jmp ParseLoop                                                       ; jumps to ParseLoop

sign:
    cmp edx, 1                                                          ; compares EDX to 1
    jne Done                                                            ; jumps to Done
    neg ecx                                                             ; incorporates sign (positive/negative)
    jmp Done                                                            ; jumps to Done

Done:
    mov eax, ecx                                                        ; EAX = ECX
    ret

InvalidInput:
    mov edx, OFFSET invalidMsg                                          ; offset, points to invalidMsg
    call WriteString                                                    ; displays invalidMsg
    mov eax, 0                                                          ; clears result (invalidInput)
    call Crlf 
    jmp Done                                                            ; jumps to Done
ParseOperand ENDP                                                       ; end of ParseOperand procedure



ProcessOperation PROC
;-----------------------------------------------------------------
;Description: processes operation based on user selection
;Receives: user input  
;Returns: nothing 
;----------------------------------------------------------------- 
cmp operation, 'A'                                                      ; compares operation to 'A'
je DoAddition                                                           ; if equal, jumps to DoAddition
cmp operation, 'a'                                                      ; compares operation to 'a'
je DoAddition                                                           ; if equal, jumps to DoAddition

cmp operation, 'S'                                                      ; compares operation to 'S'
je DoSubtraction                                                        ; if equal, jumps to DoSubtraction
cmp operation, 's'                                                      ; compares operation to 's'
je DoSubtraction                                                        ; if equal, jumps to DoSubtraction

cmp operation, 'M'                                                      ; compares operation to 'M'
je DoMultiplication                                                     ; if equal, jumps to DoMultiplication
cmp operation, 'm'                                                      ; compares operation to 'm'
je DoMultiplication                                                     ; if equal, jumps to DoMultiplication

cmp operation, 'D'                                                      ; compares operation to 'D'
je DoDivision                                                           ; if equal, jumps to DoDivision
cmp operation, 'd'                                                      ; compares operation to 'd'
je DoDivision                                                           ; if equal, jumps to DoDivision

cmp operation, '%'                                                      ; compares operation to '%''
je DoModulo                                                             ; if equal, jumps to DoModulo


DoAddition:
    call Addition                                                       ; calls Addition procedure
    ret

DoSubtraction:
    call Subtraction                                                    ; calls Subtraction procedure
    ret

DoMultiplication:
    push op1                                                            ; passes op1 as parameter
    push op2                                                            ; passes op2 as parameter
    call Multiplication                                                 ; calls Multiplication procedure
    add esp, 8                                                          
    ret

DoDivision: 
    push op1                                                            ; passes op1 as parameter
    push op2                                                            ; passes op2 as parameter
    call Division                                                       ; calls Division procedure
    add esp, 8                                                          
    ret


DoModulo:   
    push op1                                                            ; passes op1 as parameter
    push op2                                                            ; passes op2 as parameter
    call Modulo                                                         ; calls Modulo procedure
    add esp, 8                                                          
    ret
ProcessOperation ENDP                                                   ; end of ProcessOperation procedure



Addition PROC 
;-----------------------------------------------------------------
;Description: adds first and second operand
;Receives: nothing  
;Returns: nothing 
;----------------------------------------------------------------- 
mov eax, op1                                                            ; EAX = first operand 
mov ebx, op2                                                            ; adds first with second operand

add eax, ebx                                                    
jo Overflow                                                             ; jumps to overflow 
mov memoryResult, eax                                                   ; EAX = memoryResult
add runningTotal, eax                                                   ; adds runningTotal
jmp stop

stop:
    mov edx, OFFSET answerMsg                                           ; offset, points to answerMsg 
    call WriteString                                                    ; displays answerMsg
    mov eax, op1                                                        ; EAX = op1
    call WriteInt
    mov edx, OFFSET addS                                                ; includes +
    call WriteString
    mov eax, op2                                                        ; EAX = op2
    call WriteInt
    mov edx, OFFSET equalS                                              ; includes =
    call WriteString
    mov eax, memoryResult
    inc addCount                                                        ; increments addCount
    inc totalCount                                                      ; increments totalCount
    call WriteInt
    call Crlf 
    ret

Overflow:
    mov edx, OFFSET overFlowMsg                                         ; offset, points to overFlowMsg 
    call WriteString                                                    ; displays overFlowMsg  
    call Crlf                                                           ; displays new line 
    dec addCount                                                        
    dec totalCount 
    ret
Addition ENDP                                                           ; end of Addition procedure



Subtraction PROC 
;-----------------------------------------------------------------
;Description: subtracts first and second operand
;Receives: nothing 
;Returns: nothing 
;----------------------------------------------------------------- 
mov eax, op1                                                            ; EAX = first operand 
mov ebx, op2                                                            ; adds first with second operand

sub eax, ebx
jo Overflow                                                             ; jumps to overflow 

mov memoryResult, eax                                                   ; EAX = memoryResult
add runningTotal, eax                                                   ; adds  runningTotal
jmp stop

stop:
    mov edx, OFFSET answerMsg                                           ; offset, points to answerMsg 
    call WriteString                                                    ; displays answerMsg
    mov eax, op1                                                        ; EAX = op1
    call WriteInt
    mov edx, OFFSET subS                                                ; includes -
    call WriteString
    mov eax, op2                                                        ; EAX = op2
    call WriteInt
    mov edx, OFFSET equalS                                              ; includes =
    call WriteString
    mov eax, memoryResult
    inc subCount                                                        ; increments subCount
    inc totalCount                                                      ; increments totalCount
    call WriteInt
    call Crlf 
    ret

Overflow:
    mov edx, OFFSET overFlowMsg                                         ; offset, points to overFlowMsg 
    call WriteString                                                    ; displays overFlowMsg  
    call Crlf                                                           ; displays new line 
    dec subCount                                                          
    dec totalCount 
    ret
Subtraction ENDP                                                        ; end of Subtraction procedure



Multiplication PROC
;-----------------------------------------------------------------
;Description: multiplies first and second operand
;Receives: nothing 
;Returns: nothing 
;-----------------------------------------------------------------
push ebp                                                                 ; stack epilogue 
mov ebp, esp                                                             

push eax                                                                 ; saves regs 
push edx

mov eax, [ebp+12]                                                        ; op1
imul eax, [ebp+8]                                                        ; op2
jo Overflow

mov memoryResult, eax
add runningTotal, eax
jmp stop 

Overflow:
    mov edx, OFFSET overFlowMsg                                          ; offset, points to overFlowMsg 
    call WriteString                                                     ; displays overFlowMsg  
    call Crlf                                                            ; displays new line 
    dec mulCount                                                         
    dec totalCount 
    jmp finish

stop:
    mov edx, OFFSET answerMsg                                            ; offset, points to answerMsg 
    call WriteString                                                     ; displays answerMsg
    mov eax, [ebp+12]                                                    ; op1
    call WriteInt
    mov edx, OFFSET mulS                                                 ; includes *
    call WriteString
    mov eax, [ebp+8]                                                     ; op2 
    call WriteInt
    mov edx, OFFSET equalS                                               ; includes =
    call WriteString
    mov eax, memoryResult
    inc mulCount                                                         ; inc mulCount
    inc totalCount                                                       ; inc totalCount
    call WriteInt
    call Crlf                                                            ; displays new line 

finish:
    pop edx                                                              ; restores regs
    pop eax
    pop ebp 
    ret
Multiplication ENDP                                                      ; end of Multiplication procedure



Division PROC
;-----------------------------------------------------------------
;Description: divides first and second operand
;Receives: nothing 
;Returns: nothing 
;-----------------------------------------------------------------
push ebp                                                                 ; stack epilogue 
mov ebp, esp                                                             

push eax                                                                 ; saves regs
push ebx
push edx

mov eax, [ebp+12]                                                        ; op1
mov ebx, [ebp+8]                                                         ; op2

cmp ebx, 0 
je divByZero

mov ecx, eax                 
add ecx, 1 
jo Overflow

cdq
idiv ebx
mov memoryResult, eax
add runningTotal, eax
jmp stop 

divByZero:
    mov edx, OFFSET DBZMsg                                               ; offset, points to DBZMsg 
    call WriteString                                                     ; displays DBZMsg  
    call Crlf                                                            ; displays new line 
    dec divCount                                                         
    dec totalCount 
    jmp finish
    
Overflow:
    mov edx, OFFSET overFlowMsg                                          ; offset, points to overFlowMsg 
    call WriteString                                                     ; displays overFlowMsg  
    call Crlf                                                            ; displays new line 
    dec divCount                                                         
    dec totalCount  
    jmp finish

stop:
    mov edx, OFFSET answerMsg                                            ; offset, points to answerMsg 
    call WriteString                                                     ; displays answerMsg
    mov eax, [ebp+12]                                                    ; op1
    call WriteInt
    mov edx, OFFSET divS                                                 ; includes /
    call WriteString
    mov eax, [ebp+8]                                                     ; op2
    call WriteInt
    mov edx, OFFSET equalS                                               ; includes = 
    call WriteString
    mov eax, memoryResult
    inc divCount                                                         ; inc divCount
    inc totalCount                                                       ; inc totalCount
    call WriteInt
    call Crlf                                                            ; displays new line 
    
finish:
    pop edx                                                              ; restores regs 
    pop ebx
    pop eax
    pop ebp  
    ret
Division ENDP                                                            ; end of Division procedure



Modulo PROC
;-----------------------------------------------------------------
;Description: divison that returns remainder of first and 
;             second operand
;Receives: nothing 
;Returns: nothing 
;-----------------------------------------------------------------
push ebp                                                                 ; stack epilogue 
mov ebp, esp                                                             

push eax                                                                 ; saves regs 
push ebx
push edx

mov eax, [ebp+12]                                                        ; op1
mov ebx, [ebp+8]                                                         ; op2
sub edx, edx
idiv ebx
jo Overflow

mov memoryResult, edx
add runningTotal, edx
jmp stop 

Overflow:
    mov edx, OFFSET overFlowMsg                                          ; offset, points to overFlowMsg 
    call WriteString                                                     ; displays overFlowMsg  
    call Crlf                                                            ; displays new line
    dec modCount                                                         
    dec totalCount 
    jmp finish

stop:
    mov edx, OFFSET answerMsg                                            ; offset, points to answerMsg 
    call WriteString                                                     ; displays answerMsg
    mov eax, [ebp+12]                                                    ; op1
    call WriteInt
    mov edx, OFFSET modS                                                 ; includes %
    call WriteString
    mov eax, [ebp+8]                                                     ; op2
    call WriteInt
    mov edx, OFFSET equalS                                               ; includes =
    call WriteString
    mov eax, memoryResult
    inc modCount                                                         ; inc modCount
    inc totalCount                                                       ; inc totalCount
    call WriteInt
    call Crlf                                                            ; displays new line 
    
finish:
    pop edx                                                              ; restores regs 
    pop ebx
    pop eax
    pop ebp 
    ret
ret
Modulo ENDP                                                              ; end of Modulo procedure



AskContinue PROC
;-----------------------------------------------------------------
;Description: asks user if they want to continue
;Receives: nothing 
;Returns: nothing 
;-----------------------------------------------------------------
mov edx, OFFSET continueMsg                                              ; offset, points to continueMsg
call WriteString                                                         ; displays continueMsg
call ReadChar                                                            ; reads char entered by user 
call Crlf 
ret
AskContinue ENDP                                                         ; end of AskContinue procedure



DisplayReport PROC
;-----------------------------------------------------------------
;Description: displays final report
;Receives: nothing 
;Returns: nothing 
;-----------------------------------------------------------------
mov edx, OFFSET reportTitle                                              ; offset, points to reportTitle
call WriteString                                                         ; displays reportTitle
call Crlf 

mov edx, OFFSET addReport                                                ; offset, points to addReport
call WriteString                                                         ; displays addReport
mov eax, addCount 
call WriteDec 
call Crlf

mov edx, OFFSET subReport                                                ; offset, points to subReport
call WriteString                                                         ; displays subReport
mov eax, subCount 
call WriteDec 
call Crlf

mov edx, OFFSET mulReport                                                ; offset, points to mulReport
call WriteString                                                         ; displays mulReport
mov eax, mulCount 
call WriteDec 
call Crlf

mov edx, OFFSET divReport                                                ; offset, points to divReport
call WriteString                                                         ; displays divReport
mov eax, divCount 
call WriteDec 
call Crlf

mov edx, OFFSET modReport                                                ; offset, points to modReport
call WriteString                                                         ; displays modReport
mov eax, modCount 
call WriteDec 
call Crlf

mov edx, OFFSET opReport                                                 ; offset, points to opReport
call WriteString                                                         ; displays opReport
mov eax, totalCount 
call WriteDec 
call Crlf

mov edx, OFFSET runTotReport                                             ; offset, points to runTotReport
call WriteString                                                         ; displays runTotReport
mov eax, runningTotal 
call WriteInt 
call Crlf

mov edx, OFFSET avgReport                                                ; offset, points to avgReport
call WriteString                                                         ; displays avgReport
mov eax, runningTotal
cdq
idiv DWORD PTR [totalCount]
mov avgOps, eax 
call WriteInt 
call Crlf 
ret
DisplayReport ENDP                                                       ; end of DisplayReport procedure

end MAIN                                                                 ; end of source code
