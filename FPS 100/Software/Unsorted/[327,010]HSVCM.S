"/****** HSVCM = HOST COMMUNICATION (RTS100) = REL 1.0  , NOV 79 **************
"
"               CHANGED  79-AUG-31    13:15
"
"       FOR USE WITH FPS-100 SUPERVISOR
"         JOSEPH E. RAMUS        MAY 79
"
"       TAB STOP SETTINGS FOR EDITOR:    9   33  60
"
"         SPAD REGISTER NAMES DEFINED IN MINDEF
"           R0 R1 R2 R3 R4 R5 R6 R7
"
"         INDEXES FOR DATA PAD X DEFINED IN MINDEF
"           X0 X1 X2 X3
"
"       VDATM    IS STORAGE FOR DATUM VALUES
"
$TITLE HSVC
$ENTRY WATSWR
$ENTRY SENDER
$ENTRY FPUT
$ENTRY FGET
$ENTRY FTST
$ENTRY WATLIT
"
"               $INSERT MINDEF
$NOLIST
$INSERT MINDEF
$LIST
"
"
$INTEGER  VDATM
$COMMON  /FHOSTC/  VDATM(7)
"
"/****** HSVC = HOST COMMUNICATION SERVICES = REL 1.0  , NOV 79 ***************
"
HSVC:    NOP
"
"
"/****** WATSWR = WAIT FOR HOST TO WRITE SWR = REL 1.0  , NOV 79 **************
"
"
"         SUBROUTINE WATSWR
"           WAIT FOR HOST TO WRITE SWR
"           USES R0 & R1
WATSWR: LDSPI R1; DB=4000;      "MASK FOR SWR BIT
          BR WATSL
"
"    -=- -=- -=- -=- -=- -=- -=- -=- -=- -=- -=- -=- -=- -=- -=-
"/****** WATLIT = WAIT FOR HOST TO READ LITES = REL 1.0  , NOV 79 *************
"
"         SUBROUTINE WATLIT
"           WAIT FOR HOST TO READ LITES
"           USES RO & R1
WATLIT: LDSPI R1; DB=2000       "MASK FOR LITES BIT
"             WAIT FOR THE BIT TO GO OFF
WATSL:  LDDA; DB=APST3            "DEVICE ADDRESS OF APSTAT3
        IN; DB=INBS;            "READ APSTAT3
          LDSPI R0                "PUT IN R0
        AND# R1,R0              "PICK OUT THE DESIRED FLAG BIT
        BNE WATSL               "WAIT SOME MORE UNTIL BIT IS OFF
        RETURN
"
"/****** SENDER = SEND MESSAGE TO HOST = REL 1.0  , NOV 79 ********************
"
"           SUBROUTINE SENDER (ARG1, ARG2)
"           DPX(X0) CONTAINS THE FIRST MESSAGE WORD
"           DPX(X1) CONTAINS THE SECOND MESSAGE WORD IF REQUIRED
"           DPX(X2) CONTAINS THE THIRD MESSAGE WORD IF REQUIRED
"
SENDER: JSR WATLIT              "WAIT FOR HOST TO READ LITES
        DBLLIT; DB=DPX(X0)      "FIRST WORD
        IOFF                    "DISABLE INTERRUPTS FOR A MOMENT
        INTEN;                  "IRP HOST (CTRL BIT 5)
          LDSPI R1; DB=DPX(X0)    "GET FIRST ARG FOR TEST
        MOV R1,R1
        BNE SENDR5              "ONE WORD MESSAGE (DATUM ONLY)
        JSR WATLIT
        DBLLIT; DB=DPX(X1)      "SECOND WORD - APEX VALUE
        JSR WATLIT
        DBLLIT; DB=DPX(X2)      "THIRD WORD - ERROR FLAG
SENDR5: ION; RETURN              "ACTIVATE INTERRUPTS
"
"/****** FPUT = SEND A DATUM TO HOST = REL 1.0  , NOV 79 **********************
"
"         SUBROUTINE FPUT (DEST, DATUM)
"         R0 IS USED IN THIS ROUTINE
"         R1 HAS DEST VALUE  (3 BITS)
"         R2 HAS DATUM VALUE  (13 BITS)
"         DPX(X0) IS ALSO USED
FPUT:   LDSPI R0; DB=7
        AND R0,R1               "MAKE SURE ONLY 3 BITS
        BEQ FPUT5;              "EXIT IF DEST = 0
          MOVL R2,R2              "SHIFT DATUM LEFT
        MOVL R2,R2
        MOVL R2,R2
        OR R1,R2;               "INSERT DEST VALUE
          DPX(X0)<SPFN            "STORE ARG FOR SENDER
        JMP SENDER              "SENDER WILL RETURN TO CALLR
FPUT5:  RETURN
"
"/****** FGET = GET A DATUM FROM HOST = REL 1.0  , NOV 79 *********************
"           SUBROUTINE FGET (SORS, FLAG, DATUM)
"              DATA PADS USED:  X0, X1, X2, X3
"           R0 IS USED IN THIS ROUTINE
"           R1 HAS SORS VALUE
"                 =0 NOT ALLOWED.  RETURN WITH FLAG = -1.
"                 =1 TO 7  RETURN DATUM WHEN AVAILABLE.
"                  ELSE ERROR RETURN.
"           R2 RETURNS FLAG
"                           +1 IF SORS EXCEEDS 3 BITS
"                            0 IF A DATUM IS RETURNED
"                           -1 IF SORS = 0
"           R3 RETURNS DATUM WHEN AVAILABLE
"
FGET:   MOV R1,R1               "TO TEST SORS
        BEQ FG120;              "GO IF SORS = 0
          LDSPI R2; DB=177770   "BIT MASK
        AND R1,R2               "ERASE LOWER 3 BITS
        LDSPI R3; DB=VDATM-1;   "BASE ADDR FOR DATUM VALUE
          BEQ FG200               "GO IF SORS IN RANGE 1 TO 7
        LDSPI R2; DB=1;         "FLAG = +1
          RETURN                  "ILLEGAL SORS VALUE
FG120:  LDSPI R2; DB=177777;    "FLAG = -1
          RETURN                  "BECAUSE SORS = 0
"
"            WAIT FOR A DATUM VALUE TO ARRIVE
FG200:  ADD# R1,R3; SETMA       "FETCH DATUM VALUE
        MOV R2,R2
        BNE FDATUM              "FOUND DATUM VALUE
        LDSPI R2; DB=MD;        "LOAD DATUM VALUE
          BR FG200                "CONTINUE THE LOOP
"
"         BEGIN CRITICAL SECTION.  READ & CLEAR DATUM VALUE.
FDATUM: IOFF                    ""IRPS OFF
        ADD R1,R3; SETMA        ""FETCH DATUM VALUE
        LDSPI R2; DB=ZERO       ""SET FLAG = 0
        STATMA; MI<ZERO         ""CLEAR MEMORY
        LDSPI R3; DB=MD         ""LOAD DATUM VALUE
"            END CRITICAL SECTION.
        ION                     ""IRPS ON
        MOVRR R3,R3             "SHIFT RIGHT TO DELETE 3 BITS
        MOVR  R3,R3;
          RETURN
"
"
"/****** FTST = TEST IF DATUM IS AVAILABLE = REL 1.0  , NOV 79 ****************
"           SUBROUTINE FTST (SORS, FLAG, DATUM)
"              DATA PADS USED:  X0, X1, X2, X3
"           R0 IS USED IN THIS ROUTINE
"           R1 HAS SORS VALUE
"                 =0  RETURN DATUM FOR LOWEST NUMBER THAT IS AVAILABLE.
"                 =1 TO 7  RETURN DATUM IF AVAILABLE.
"                  ELSE ERROR RETURN.
"           R2 RETURNS FLAG
"                           +1 IF SORS EXCEEDS 3 BITS
"                            0 IF A DATUM IS RETURNED
"                           -1 IF NO DATUM IS RETURNED (NOT AVAILABLE)
"           R3 RETURNS DATUM IF AVAILABLE
"
FTST:   MOV R1,R1               "TO TEST SORS
        BEQ FT400;              "GO IF SORS = 0
          LDSPI R2; DB=177770     "BIT MASK
        AND R1,R2               "ERASE LOWER 3 BITS
        BEQ FT600;              "GO IF SORS IN RANGE 1 TO 7
          LDSPI R3; DB=VDATM-1  "BASE ADDRESS
        LDSPI R2; DB=1;         "SET FLAG = +1
          RETURN                  "ILLEGAL SORS VALUE
"
"            SEARCH ALL SLOTS FOR A VALUE
FT400:  LDSPI R4; DB=7          "LOOP COUNTER
        DB=VDATM;
          LDSPI R3; LDMA          "FETCH FIRST VALUE
        INC R1;                 "SET R1 = 1 FOR FIRST SLOT
          BR TWATE
"
"              SORS IN RANGE 1 TO 7.  EXAMINE ONLY ONE SLOT.
FT600:  ADD R1,R3; SETMA        "FETCH VALUE
        LDSPI R4; DB=1          "LOOP COUNTER
"            MEMORY FETCH IN PROGRESS
"            R4 IS LOOP COUNTER
"            R1 IS SORS VALUE
"            R3 IS MEMORY ADDRESS FOR FETCH
TWATE:  NOP                     "WAIT FOR MEMORY
        LDSPI R0; DB=MD "LOAD VALUE
        MOV R0,R0
        BNE FT640;              "GO IF GOT A VALUE
          INC R3; SETMA           "FETCH NEXT VALUE
        DEC R4
        INC R1;                 "NEXT SORS NUMBER
          BGT TWATE               "CONTINUE LOOP
"                        NO VALUE AVAILABLE
        LDSPI R2; DB=177777;    "FLAG = -1
          RETURN                  "NO VALUE AVAILABLE
"
"                     FOUND A VALUE
FT640:  LDSPI R3; DB=VDATM-1    "BASE ADDRESS FOR DATUM VALUE
        JMP FDATUM              "READ & CLEAR DATUM VALUE
"
        $END
