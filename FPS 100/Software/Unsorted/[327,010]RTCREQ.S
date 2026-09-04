"****** RTCREQ = RTC REQUEST HANDLER = REL 1.0  , NOV 79 **********************
"
        $TITLE RTCREQ
        $ENTRY RTCREQ,0
        $ENTRY RTCANS,0
        $EXT INSERT, ENABLE, DELETE, EMPTY
"
$NOLIST
$INSERT MINCOM
$INSERT MINDEF
$LIST
"
"       RTC QUEUE DEFINITIONS
"
        $INTEGER RTCQ,RTCRQ
"
        $COMMON /RTCCOM/ RTCQ(2),RTCRQ(2)
"
"   DEFINE PARAMETERS
"
        BITMSK = 001
        RATMSK = 017
        RUN    = 401
"
"
"  USES R0 - R5, DPX(0) - DPX(3)
"
"  PARAMETERS -
"
"  ON ENTRY:  R0 = 0, IF USER WISHES TO JUST TEST FOR A BUFFER
"                     AVAILABLE
"                = 1, IF THE USER WISHES TO WAIT FOR A BUFFER
"                     AVAILABLE.
"
"  ON RETURN: R0 = 0, IF NO BUFFER IS AVAILABLE
"                = 1, IF BUFFER IS AVAILABLE.
"
"         DPX(3) = THE ADDRESS OF THE AVAILABLE BUFFER.
"
RTCANS: MOV R0,R5               "SAVE TEST VALUE IN R5
        LDSPI R4;DB=RTCRQ;      "R4 = ADDRESS OF RTC1RQ
        DPX(X2)<DB              "DPX(2) = ADDRESS OF RTC RETURN QUEUE
"
TEST:   JSR EMPTY               "TEST FOR RETURN QUEUE EMPTY
        BNE FOUND               "JUMP IF NOT EMPTY
        MOV R5,R0               "TEST IF GOING TO LOOP
        BNE TEST                "IF <> 0 , LOOP
        RETURN                  "DONE, R0 = 0 (NO BUFFER AVAIL)
"
FOUND:  IOFF                    "DISABLE INTERRUPTS
        JSR DELETE              "REMOVE BUFFER FROM QUEUE
        ION                     "ENABLE INTERRUPTS
        LDSPI R0;DB=1           "R0 = 1 (BUFFER AVAIL)
        RETURN                  "DONE, BUFFER ADDRESS IN DPX(3)
"
"
"
"  PARAMETERS -
"
"  ON ENTRY:
"        DPX(0) = MESSAGE ADDRESS
"
"  ON RETURN:
"        R0 = 0, IF NO ERRORS
"
"
RTCREQ: LDSPI R0;DB=DPX(X0);    "R0 = MESSAGE ADDR
        DPX(X3)<DB              "DPX(3) = NEW MESSAGE TO QUEUE
"
        LDSPI R4;DB=RTCQ;       "R4 = ADDR OF RTC QUEUE #1
        DPX(X2)<DB              "DPX(2) = QUEUE HEADER
"
        IOFF                    "DISABLE INTERRUPTS
        JSR INSERT              "PUT ELEMENT AT END OF QUEUE
        ION                     "ENABLE INTERRUPTS
                                "R3 = LAST ELEMENT IN QUEUE
        SUB# R3,R4              "TEST FOR QUEUE EMPTY
        BEQ START               "YES, START I/O
"
        RETURN;CLR R0           "DONE, NO ERRORS
"
START:  LDSPI R0;DB=RTCCTL      "R0 = RTC ADDR
        JSR ENABLE              "GET BIT MASK
"
        LDSPI R1;DB=DPX(X3)     "R1 = MESSAGE ADDRESS
        LDSPI R3;DB=3           "R3 = RTC TICK FIELD OFFSET
        LDDA;DB=RTCCST          "SETUP DEVICE ADDR
        ADD R1,R3;SETMA         "FETCH RTC TICK COUNT
        INCMA                   "FETCH DIRECTION
        INCMA                   "FETCH MODE
        OUT;DB=MD;INCMA         "SET TICKS, FETCH RATE
"
        LDSPI R1;DB=MD;STATMA   "R1 = DIRECTION
        LDSPI R2;DB=MD;STATMA   "R2 = MODE
        LDSPI R3;DB=MD          "R3 = RATE
        LDSPI R0;DB=BITMSK      "R0 = BIT MASK
"
        ANDL R0,R1              "MASK DIRECTION BIT
        AND R0,R2               "MASK MODE BIT
        LDSPI R0;DB=RATMSK      "R0 = RATE MASK
        AND R0,R3               "MASK RATE
        ORL R2,R1               "BUILD CONTROL VALUE
        MOVL R1,R1;INCMA        "MAKE ROOM FOR RATE
        MOVL R1,R1;STATMA       "FETCH REPEAT COUNT
        MOVL R1,R1;STATMA       "
        ORL R3,R1               "ADD IN RATE
        LDSPI R0;DB=RUN         "R0 = RUN BITS
        LDDA;DB=RTCCTL          "SETUP DEVICE ADDR
        OR R0,R1;OUT;DB=SPFN    "START CLOCK
        LDDA;DB=RTCCST          "SETUP DEVICE ADDR
        OUT;DB=MD               "SET REPEAT COUNT
"
        LDDA;DB=IMASK           "SETUP DEVICE ADDR
        LDSPI R0;DB=INBS;IN     "R0 = CURRENT IMASK VALUE
        AND R0,R4;OUT;DB=SPFN   "ENABLE RTC INTERRUPT
"
        RETURN;CLR R0           "DONE, NO ERRORS
"
        $END
