"****** TASK5 = TASK FOR TEST 5.0 = REL 3.0, JUN 79
"
        $TASK 3 /M
        $TITLE TASK3
        $ENTRY TASK3
        $EXT FPUT,TWAIT
"
$INSERT FHOSTC
"
"
"  LOCAL VARIABLES
"
        R0    $EQU 0            "
        R1    $EQU 1            "
        R2    $EQU 2            "
        R3    $EQU 3            "
        X0    $EQU 0            "
        X1    $EQU 1            "
        X2    $EQU 2            "
        X3    $EQU 3            "
"
        ID1 = 3                 "TASK ID NUMBER
        TIME1 = 61               "TIME TO WAIT
        FLAG1 = 17702            "
"
TASK3:  CLR R3                  "CLEAR RUN COUNT
        CLR R2                  "CLEAR RUN COUNT
        LDSPI R1;DB=ID1         "R1 = TASK ID
"
LOOP:   DPX(X1)<EXDAT1          "DPX(1) = MESSAGE EXCHANGE
        DPX(X0)<TIME1           "TIME TO WAIT
        TRAP;DB=@TWAIT;LDTMA      "WAIT FOR MESSAGE
"
"  NOW YOU ARE RUNNING SO INCREMENT FLAG
"
        LDMA;DB=FLAG1
        INC R3;STATMA;MI<DB;DB=SPFN "WRITE FLAG
        MOV R3,R2                "INC RUN FLAG
        JSR FPUT                 "SEND MESSAGE
"
"
        JMP LOOP                "AND LOOP
"
        $END
