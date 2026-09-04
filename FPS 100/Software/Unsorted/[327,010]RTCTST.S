"****** RTCTST = RTC TEST (TEST 5) FOR MINI = REL 1.0  , NOV 79 ***************
"
        $TITLE TASK1
        $ENTRY TASK1
        $EXT FPUT,FGET,RTCANS,RTCREQ
"
"
"
"  LOCAL VARIABLES
"
        R0    $EQU 0            "
        R1    $EQU 1            "
        R2    $EQU 2            "
        R3    $EQU 3            "
        R7    $EQU 7            "
        X0    $EQU 0            "
        X1    $EQU 1            "
        X2    $EQU 2            "
        X3    $EQU 3            "
"
"   DEFINE TIME-INTERVAL MESSAGES
"
        $INTEGER  MSG1,MSG2,MSG3
"
        $COMIO TESTC 0
        $COMMON /TESTC/ MSG1(10.),MSG2(10.),MSG3(10.)
"
"
TASK1:  LDSPI R1;DB=1           "CALL FGET (1)
        JSR FGET                "
"
        DPX(X0)<MSG1+2          "SEND FIRST TIME MESSAGE
        JSR RTCREQ              "
        DPX(X0)<MSG2+2          "SEND SECOND TIME MESSAGE
        JSR RTCREQ              "
        DPX(X0)<MSG3+2          "SEND THIRD TIME MESSAGE
        JSR RTCREQ
"
"  NOW LOOP WAITING FOR TIMEOUTS, SENDING FPUTS BACK TO HOST
"
LOOP:   LDSPI R0;DB=1           "WAIT FOR MESSAGE
        JSR RTCANS              "
"
        LDSPI R7;DPX(X0)<DPX(X3) "MOVE MESSAGE ADDR TO R7 & DPX(0)
        JSR RTCREQ              "SEND MESSAGE BACK TO RTC DRIVER
"
        DEC# R7;SETMA           "FETCH MESSAGE COUNT
        DECMA                   "FETCH MESSAGE NUMBER
        INCMA                   "SETUP TO INC COUNT
"
        LDSPI R2;DB=MD;STATMA   "R2 = MESSAGE COUNT
        LDSPI R1;DB=MD          "R1 = MESSAGE #
"
        INC  R2;STATMA;MI<DB;   "WRITE OUT NEW COUNT
        DB=SPFN                 "
"
        JSR FPUT                "SEND HOST MESSAGE
"
        JMP LOOP                "LOOP FOR MORE
"
"
        $END
