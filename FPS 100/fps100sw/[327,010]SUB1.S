"****** SUB1 = HASI TEST ROUTINE = REL 1.0  , NOV 79 *************************
$TITLE SUBR1
$ENTRY SUBR1,13.
$COMIO PASS 1
$COMMON /PASS/ IT(17.)  /I
$DATA IT(1) 0
"
SUBR1:  LDMA; DB=IT                    "GET COUNT OF TIMES THRU
        INCMA; MI<SPFN; MOV 0,0        "SP0 INTO COMMON
        INCMA; MI<SPFN; MOV 1,1        "SP1 INTO COMMON
        INCMA; MI<SPFN; MOV 2,2        "SP2 INTO COMMON
        LDSPI 1; DB=13.                "LOOP COUNTER
        CLR 2                          "SP2 = 0
        LDSPI 0; DB=3; LDSPD           "SPD POINTER
LOOP:   INCMA; MI<SPFN; ADD 2,0        "SP(N) INTO COMMON
        DEC 1
        BGT LOOP;                      "CHECK IF DONE ALL SPADS
         INC 0; DB=SPFN; LDSPD         "BUMP SPD POINTER
"OUT OF LOOP.
"SPD=16.=0
"
        LDSPI 0; DB=MD                 "COUNT OF TIMES THRU
        INC 0; DPX<SPFN                "INCREMENT
        LDSPI 0; DB=IT
        MOV 0,0; SETMA; MI<DPX;        "STORE COUNT
         RETURN
$END
