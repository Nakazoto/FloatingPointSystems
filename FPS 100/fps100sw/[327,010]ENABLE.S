"
"****** ENABLE = ROUTINE TO ENABLE INTERRUPTS = REL 0.0, AUG 79 **
"
"
        $TITLE ENABLE
        $ENTRY ENABLE
"
"     --- ABSTRACT ---
"  THIS ROUTINE FINDS THE BIT MASK TO ENABLE THE INTERRUPT OF AN IO
"  DEVICE IN THE SYSTEM.  THE MASK IS FOUND BY SEARCHING THROUGH THE
"  SYSTEM CONFIGURATION TABLE LOOKING FOR THE DEVICE ADDRESS PASSED
"  AS A PARAMETER.  WHEN FOUND, THE BIT MASK IS TAKEN FROM THE TABLE.
"  IF NOT FOUND, A MASK IS RETURNED WHICH ENABLES NOTHING.
"
$COMMON /CONFIG/ DEV1(5) /T, DEV2(5) /T, DEV3(5) /T, DEV4(5) /T,
                 DEV5(5) /T, DEV6(5) /T, DEV7(5) /T, DEV8(5) /T,
                 DEV9(5) /T, DEV10(5) /T, DEV11(5) /T, DEV12(5) /T,
                 DEV13(5) /T, DEV14(5) /T, DEV15(5) /T
$COMMON /ISRMAP/ DUMMY(120.) /I
"
"
" THIS IS A CONFIGURATION TABLE FOR THE I/O DEVICES ON THE FPS-100.
"
" THEIR ORDER IS THE SAME AS THAT OF THEIR BITS IN THE IMASK REGISTER.
"
" THE FORMAT OF EACH ENTRY IN THIS TABLE IS:
"
"   WORD 1:  PRIORITY MASK (TO MASK OUT LOWER PRIORITY DEVICES)
"   WORD 2:  BIT MASK (THIS DEVICE'S BIT ON)
"   WORD 3:  POINTER TO SERVICE ROUTINE'S ENTRY IN OVERLAY MAP
"   WORD 4:  DEVICE ORDER NUMBER  (EXP)
"            PHYSICAL DEVICE ADDRESS (LMAN)
"   WORD 5:  SAVE AREA FOR OLD IMASK
"
W = 8.  "WIDTH OF OVERLAY TABLE ENTRY
"
"
"
"
"
"  VALUE  ASSIGNMENTS
        R0    $EQU 0            "
        R1    $EQU 1            "
        R2    $EQU 2            "
        R3    $EQU 3            "
        R4    $EQU 4            "
        X0    $EQU 0            "
        X1    $EQU 1            "
        X2    $EQU 2            "
        X3    $EQU 3            "
"
"
" ON ENTRY:
"       R0 = DEVICE ADDR
" ON EXIT:
"       R4 = BIT MASK
"
"
ENABLE: LDSPI R1;DB=DEV1+4              "R1 = FIRST DEVICE ADDR IN CONFIG TABLE
        LDSPI R2;DB=15.                 "R2 = NUMBER OF ENTRYS
        LDSPI R3;DB=5.                  "R3 = LENGTH OF ENTRY
"
LOOP:   MOV R1,R1;SETMA                 "FETCH DEVICE ADDR
        STATMA;ADD R3,R1                "SETUP FOR NEXT ENTRY
        STATMA                          "
        LDSPI R4;DB=MD                  "R4 = DEVICE ADDR
        SUB# R4,R0                      "TEST FOR SAME ADDR
        BEQ FOUND                       "YES
"
        DEC R2                          "TEST COUNT
        BNE LOOP                        "LOOP
        SUB R4,R4                       "CLEAR R4 ERROR
        RETURN;COM R4                   "DONE
"
FOUND:  SUB R3,R1                       "RESET R1
        DEC R1                          "MASK BIT IN ENTRY+1
        DEC R1;SETMA                    "FETCH BIT MASK
        STATMA                          "WAIT ON MD
        STATMA                          "
        LDSPI R4;DB=MD                  "R4 = BIT MASK
        RETURN;COM R4                   "DONE
"
"
        $END
