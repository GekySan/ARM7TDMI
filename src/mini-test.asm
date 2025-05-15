start:
    MOV   R0, #0
    ADDS  R1, R0, #0
    MVN   R2, #0x0F
    ADDS  R3, R2, #0x10
    SBCS  R4, R1, #1
    RSBS  R5, R4, #0
    MOV   R6, #0x40000000
    ADDS  R7, R6, R6
    ADDS  R8, R7, R7
    MULS  R9, R6, R6
    MOV   R10, #1
    ADCS  R10, R10, #0
    B     start
