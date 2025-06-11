"
"****** IOQUE = I/O DRIVER QUEUE SUPPORT ROUTINES = REL 1.0  , NOV 79 *********
"
"****** INSERT = INSERT INTO QUEUE = REL 1.0  , NOV 79 ************************
"
$TITLE INSERT
$ENTRY INSERT,0
"
"
" INSERTS NEW BETWEEN LAST AND NEXT
"
"
" THIS ASSUMES SUPERVISOR MODE AND INTERRUPTS OFF
"
"
" PARAMETERS:
"   (IN) DPX(2)=ADDR OF NEXT
"   (IN) DPX(3)=ADDR OF NEW
"
"
" SPAD 0 AND SPFN ARE LEFT WITH ERROR CODE
" (0=OK, NON-0=ERROR: NEW ALREADY LINKED ELSEWHERE)
"
"
" SCRATCH: SP 0-3, DPX 2-3
"
"
" TIMING: 11 CYCLES IF SUCCESSFUL, 7 OTHERWISE
"
"
NEXT = 1
NEW  = 2
LAST = 3
"
"
INSERT: LDSPI NEW; DB=DPX(3);          "ADDR OF NEW
          LDMA                         "GET RLINK(NEW)
        LDSPI NEXT; DB=DPX(2); LDMA    "ADDR OF NEXT
        STATMA
        LDSPI 0; DB=MD                 "RLINK(NEW)
                                       "COMPARE NEW AND RLINK(NEW)
        SUB NEW,0                      "SP0  = 0 OR NOT
                                       "IF NEW DOESN'T POINT TO
        BNE ERROR;                     "SELF (UNLINKED) THEN ERROR
          INCMA                        "GET LAST=LLINK(NEXT)
        INC NEXT; SETMA; MI<DPX(3)     "LLINK(NEXT)=NEW
        MOV NEW,NEW; SETMA; MI<DPX(2)  "RLINK(NEW)=NEXT
        INCMA; MI<MD; LDSPI LAST       "LLINK(NEW)=LAST
        MOV LAST,LAST; SETMA; MI<DPX(3) "RLINK(LAST)=NEW
ERROR:  MOV 0,0;                       "SET SPFN
          RETURN
$END
"****** EMPTY = CHECKS FOR EMPTY QUEUE = REL 1.0  , NOV 79 ********************
"
$TITLE EMPTY
$ENTRY EMPTY,0
"
"
"
" THIS SEES IF THERE IS A SUCCESSOR ELEMENT.
" IF GIVEN A HEADER, THIS SEES IF THERE ARE ANY ELEMENTS.
"
"
" PARAMETERS:
"  (IN)   DPX(2)=ADDR OF Q HEADER OR ELEMENT, I.E. LAST
"  (OUT)  DPX(3)=ADDR OF NEXT
"  (OUT)  SP0 AND SPFN = 0 IF EMPTY (NO SUCCESSOR)
"
"
" THIS ASSUMES SUP. MODE AND INTERRUPTS OFF
"
"
" SCRATCH:  SP 0-1, DPX 2-3
"
" TIMING: 5 CYCLES
"
"
NEXT = 0
LAST = 1
"
"
EMPTY:  LDMA; DB=DPX(2); LDSPI LAST    "GET NEXT=RLINK(LAST)
        STATMA
        STATMA
        LDSPI NEXT; DPX(3)<MD          "NEXT
                                       "IF LAST=NEXT, THEN EMPTY,
        SUB LAST,NEXT;                 " AND SP0 =0.
          RETURN                       " OTHERWISE NON-0.
$END
"****** DELETE = DELETE FROM QUEUE = REL 1.0  , NOV 79 ************************
"
$TITLE DELETE
$ENTRY DELETE,0
"
"
"
" DELETES ELEMENT FROM QUEUE
"
"
" PARAMETERS:
"   (IN)  DPX(3)=ADDR OF ELEMENT TO BE DELETED
"   (OUT) DPX(2)=ADDR OF NEXT ELEMENT (DELETED'S SUCCESSOR)
"
"
"
" SPAD 0 IS UNTOUCHED
"
"
" THIS ASSUMES SUP. MODE AND INTERRUPTS OFF
"
"
"
" SCRATCH: SP 1-3, DPX 2-3
"
"
" TIMING: 7 CYCLES
"
"
NEXT = 1
OUT = 2
LAST = 3
"
"
DELETE: LDMA; DB=DPX(3); LDSPI OUT     "GET NEXT=RLINK(OUT)
        INCMA                          "GET LAST=LLINK(OUT)
        STATMA; MI<DPX(3)              "LLINK(OUT)=OUT
        DPX(2)<MD; LDSPI NEXT; LDMA    "NEXT
        INCMA; MI<MD; LDSPI LAST       "LLINK(NEXT)=LAST
        MOV LAST,LAST; SETMA; MI<DPX(2) "RLINK(LAST)=NEXT
        MOV OUT,OUT; SETMA; MI<DPX(3); "RLINK(OUT)=OUT
          RETURN
$END
"
"****** ENABLE = ROUTINE TO ENABLE INTERRUPTS = REL 1.0  , NOV 79 *************
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
