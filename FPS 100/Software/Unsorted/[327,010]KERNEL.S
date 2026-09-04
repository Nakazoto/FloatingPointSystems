"****** EXTASK = EXECUTE TASK MODULE = REL 0.0  , AUG 79 **********************
"
$TITLE EXTASK
$ENTRY EXTASK,0
$EXT EMPTY
$EXT CHKPT
$EXT IDLE
$EXT MXSAVE
$EXT MNREST
$EXT MXREST
$EXT PSMNGR
$INSERT COMSYS
"
"
" EXECUTE TASK MODULE
"
" PROCEDURE:
"   SELECTS HIGHEST PRIORITY READY TASK.
"   MAKES TASK RESIDENT (IF NOT ALREADY).
"   SWITCHES CONTEXT IF NECESSARY.
"   LOADS NEW TASK'S MINIMUM STATE AND RUNS IT.
"
" THIS ASSUMES SUP. MODE, SMA, AND INTS. OFF.
"
" PARAMETERS:
"   (OUT) SMA AND SPAD 0 = END OF MIN SAVE AREA
"
" SCRATCH: SP 0-1,5-7  DPX 2-3
"
" ROUTINES CALLED: EMPTY, CHKPT, IDLE, MXSAVE, MXREST, MNREST, PSMNGR
"
" TIMING:
"   48 CYCLES IF NEW TASK IS SAME AS PREVIOUS TASK (MINIMUM TIME):
"      24 IN EXTASK
"       5 IN EMPTY
"      15 IN PSMNGR
"       4 IN CHKPT
"   377-389 CYCLES  (MAXIMUM TIME)  IF NEW TASK NOT SAME AS
"   PREVIOUS TASK, AND NEW TASK HAS FULL CONTEXT, AND ANOTHER
"   TASK WITH FULL CONTEXT HAD PREVIOUSLY EXECUTED:
"      40  IN EXTASK
"       5  IN EMPTY
"      15  IN PSMNGR
"     145-148 IN MXSAVE
"     168-177 IN MXREST
"       4  IN CHKPT
"   DEPENDING UPON CONDITIONS, ANYWHERE IN BETWEEN.
"
"   NOTE: ABOVE TIMINGS ALL ASSUME THAT THE NEW TASK IS
"   ALREADY PS RESIDENT, AND IS ONE SEGMENT.
"   OTHERWISE, ADD MORE PSMNGR OVERHEAD.
"
STAT = 0
FULLC = 1
HERE = 0
LAST = 5
NEW = 6
CUR = 7
"
"
EXTASK: DPX(2)<RDYQUE                  "READY QUEUE
                                       "PARAM FOR EMPTY:
                                       "DPX(2)=Q HEAD
        JSR EMPTY                      "GET HIGHEST PRI TCB
        BNE MAKRES;                    "ARE THERE ANY READY TASKS?
         LDSPI R0; DB=NEWTSK
        JSR CHKPT                      "IF NOT, TRY CHECKPOINT
        JSR IDLE                       "IF THAT FAILS, IDLE
        BR EXTASK+1;                   "THEN TRY READY Q AGAIN
         DPX(2)<RDYQUE
" GOT HERE WHEN HAVE HIGHEST PRIORITY TASK.
"
" EMPTY RETURNED WITH ITS TCB ADDR IN DPX(3).
"
MAKRES: MOV R0,R0; SETMA; MI<DPX(3)    "SET NEW TASK
        LDSPI R0; DB=DPX(3)            "TCB ADDR
        LDSPI R1; DB=OVLPTR            "OVERLAY MAP POINTER OFFSET
                                       "GET ADDR OF 1ST ENTRY
        ADD R0,R1; SETMA               "IN OVERLAY MAP FOR THIS TASK
        INCMA                          "GET # OF ENTRIES
        STATMA
        DPX(3)<MD;                     "1ST ENTRY ADDR
         STATMA
                                       "PARAMS FOR PSMNGR:
                                       "DPX(3)=PTR INTO OVERLAY TABLE
                                       "DPX(2)=# OF ENTRIES
        DPX(2)<MD;                     "COUNT
         JSR PSMNGR                    "MAKE TASK RESIDENT
" GOT HERE WHEN TASK IS RESIDENT
"
        LDMA; DB=CURTSK                "GET CURRENT (OLD) TASK
        LDMA; DB=NEWTSK                "GET NEW TASK
        LDSPI STAT; DB=STATUS;         "TCB STATUS OFFSET
         STATMA
        LDSPI CUR; DB=MD               "CURRENT TASK'S TCB ADDR
        MOV CUR,CUR;                   "CURTSK = 0?
         STATMA
                                       "IF SO, THIS IS THE FIRST TASK.
        BEQ CKFULL;                    "SEE IF HAS FULL CONTEXT.
         LDSPI NEW; DB=MD              "NEW TASK'S TCB ADDR
        SUB# CUR,NEW                   "OTHERWISE, NEWTSK=CURTSK?
        BNE CKFULL+1;                  "IF NOT, SEE IF FULL CONTEXT
         ADD NEW,STAT; SETMA           "GET NEWTSK STATUS
        JMP LOADST                     "IF SO, DON'T SAVE/REST CONTEXT
CKFULL: ADD NEW,STAT; SETMA            "GET NEWTSK STATUS
        LDSPI FULLC; DB=CXTBIT;        "TASK CONTEXT BIT MASK
         STATMA
                                       "GET ADDR OF
        LDMA; DB=LSTFUL                "LAST TASK WITH FULL CONTEXT
        LDSPI STAT; DB=MD;             "NEWTSK STATUS
         STATMA
        AND# STAT,FULLC;               "NEWTSK HAS FULL CONTEXT?
         STATMA
        BEQ SETCUR;                    "BRANCH IF NOT.
                                       "TCB ADDR OF
         LDSPI LAST; DB=MD             "LAST FULL CONTEXT TASK
        MOV LAST,LAST                  "WAS THERE A FULL CONTEXT TASK?
        BEQ SETLST;                    "BRANCH IF NOT.
         LDSPI HERE; DB=MINTCB+1       "BEGIN. OF MAX SAVE AREA OFFSET
                                       "OTHERWISE, IS NEW TASK
        SUB# NEW,LAST                  "SAME AS IT?
        BEQ SETCUR;                    "IF SO, DON'T SAVE/REST CONTEXT.
                                       "OTHERWISE, SET SMA TO
                                       "BEGINNING OF CONTEXT SAVE AREA
         ADD LAST,HERE; SETMA          "IN LAST TASK WITH FULL CONTEXT
        JSR MXSAVE                     "SAVE ITS CONTEXT NOW.
SETLST: LDMA; DB=LSTFUL
        STATMA; MI<SPFN; MOV NEW,NEW   "SET LSTFUL = NEWTSK
        LDSPI HERE; DB=MAXTCB          "END OF MAX SAVE AREA
                                       "SET SMA TO END OF
        ADD  NEW,HERE; SETMA           "CONTEXT AREA IN NEW TASK
        JSR MXREST                     "RESTORE ITS CONTEXT
SETCUR: LDMA; DB=CURTSK
        STATMA; MI<SPFN; MOV NEW,NEW   "SET CURTSK = NEWTSK
LOADST: JSR CHKPT                      "CHECK FOR PENDING INTS.
        LDSPI HERE; DB=MINTCB          "END OF MIN SAVE AREA
                                       "SET SMA AND SP0 TO END OF
        ADD NEW,HERE; SETMA            "MIN SAVE AREA IN NEW TASK
        JMP MNREST                     "LOAD STATE OF NEW CURRENT TASK
" MIN. RESTORE SHOULD DO:
"    RETURN; EXINT
" WHICH WOULD START TASK RUNNING (AGAIN)
"
$NOLIST
$INSERT SYSDEF
$LIST
$END
"****** PSMNGR = PS MANAGER = REL 0.0  , AUG 79 *******************************
"
$TITLE PSMNGR
$ENTRY PSMNGR,0
$EXT MOVER
$INSERT COMSYS
"
"
" MANAGES PS MEMORY.
"
"
" PROCEDURE:
"   CHECK IF EACH OVERLAY SEGMENT WHICH SHOULD BE RESIDENT
"   IS CURRENTLY RESIDENT.  IF NOT, FIND ITS PS PARTITION(S),
"   MARK WHATEVER OVERLAY(S) USED TO BE IN THOSE PARTITIONS
"   (IF ANY) AS NON-RESIDENT, AND MARK THE NEW SEGMENT RESIDENT.
"   MARK THE PARTITION(S) AS CONTAINING THE NEW SEGMENT.
"   CALL THE CODE MOVER TO TRANSFER THE SEGMENT FROM MD TO PS.
"   CHECK ALL SEGMENTS AGAIN, IN CASE THE PROCESS WAS
"   INTERRUPTED AND CONTENTS OF PS CHANGED IN THE MEANTIME.
"
"
" THIS ASSUMES SUP MODE, SMA, INTS OFF (UPON ENTRY)
"
" PARAMETERS:
"   (IN)  DPX(3)=ADDR OF FIRST OVERLAY TABLE ENTRY FOR THIS TASK OR ISR
"   (IN)  DPX(2)=NUMBER OF (CONSECUTIVE) ENTRIES
"
"   NOTE: UPON EXIT, DPX(3) IS LEFT BASICALLY UNCHANGED, POINTING
"   WITHIN THE FIRST OVTAB ENTRY (ALTHOUGH AT THE RES WORD),
"   WHICH IS ASSUMED BY THE IO MODULE.
"
" SCRATCH: SP 0-6, DPX 0-3   (DO NOT USE SP 7)
"
" ROUTINES CALLED: MOVER
"
" TIMING:
"
"   15 CYCLES FOR 1-SEGMENT TASK (OR ISR) WHICH IS ALREADY RESIDENT.
"
"   9 + 6*N CYCLES FOR N-SEGMENT TASK IN WHICH ALL SEGMENTS
"      WHICH SHOULD BE RESIDENT ARE RESIDENT.
"
"   FOR TASKS WITH ONE OR MORE SEGMENTS WHICH SHOULD BE
"      RESIDENT BUT AREN'T, THE OVERHEAD IS HIGHLY VARIABLE,
"      DEPENDING UPON MANY FACTORS INVOLVED.
"
" THIS ASSUMES THAT EACH OVERLAY TABLE ENTRY LOOKS AS FOLLOWS:
"
" WORD 1:  OVERLAY SEG NUMBER
" WORD 2:  MD ADDR
" WORD 3:  PS ADDR
" WORD 4:  LENGTH (IN PS WORDS)
" WORD 5:  TASK ID OR TCB ADDR
" WORD 6:  RESIDENCY:  EXP=CURRENTLY RES
"                      LM =SHOULD BE RES
" WORD 7:  POINTER TO FIRST PS PARTITION TABLE ENTRY FOR THIS SEGMENT
" WORD 8:  NUMBER OF PS PARTITION ENTRIES
"
"
WIDTH = 8.
RES = 6
"
"
"
SEGENT = 0
SEGCNT = 1
WIDE = 2
SHOULD = 3
IS = 4
CNT = 5
PPTR = 2
PCNT = 3
TSKID = 4
OLDSEG = 5
OLDTID = 6
"
"
PSMNGR: LDSPI SEGCNT; DB=DPX(2)        "# OF OVERLAY SEGS
        LDSPI SEGENT; DB=DPX(3)        "ADDR OF 1ST SEG ENTRY
        LDSPI R2; DB=RES-1             "RESIDENCY WORD OFFSET
        ADD R2,SEGENT; SETMA;          "GET 1ST SEG'S RES WORD
         DPX(3)<SPFN;                  "ADDR OF 1ST SEG'S RES WORD
         BR .+2                        "SKIP NEXT INSTR
" THIS RE-INITIALIZES TO THE BEGINNING OF THE TASK'S
" AREA IN THE OVERLAY MAP.
"
"
LOOP3:  LDMA; DB=DPX(3);               "GET 1ST SEG'S RES WORD
         LDSPI SEGENT                  "ITS ADDR
        MOV SEGCNT,CNT;                "RE-INIT COUNT OF SEGS
         STATMA
        LDSPI WIDE; DB=WIDTH;          "WIDTH OF ENTRY IN OVERLAY TABLE
         STATMA
        LDSPI R6; DB=512.
        LDSPI SHOULD; DB=MD            "SHOULD-BE-RES BIT OF RES WORD
" THIS LOOP CHECKS EACH SEGMENT FOR
" SHOULD-BE-RESIDENT VS. CURRENTLY-RESIDENT.
"
" THE INDENTATION OF THE FOLLOWING LOOP IS TO INDICATE ITS
" 2-COLUMN STRUCTURE.
"
LOOP1:        LDSPE IS; DB=MD          "CURRENTLY-RES BIT OF RES WORD
              ADD R6,IS                "(ADJUST FOR LDSPE)
        ADD WIDE,SEGENT; SETMA         "GET NEXT SEG'S RES WORD
        STATMA;
              SUB# SHOULD,IS           "SHOULD = IS ?
        STATMA;
              BNE OUT;                 "IF NOT MATCH, GET OUT
              DEC CNT                  "DEC SEG COUNT
        LDSPI SHOULD; DB=MD;           "SHOULD-BE-RES BIT OF NEXT
                                       "SEG'S RES WORD
              BGT LOOP1                "LOOP IF NOT CHECKED ALL SEGS
" GOT HERE IF, FOR ALL SEGS OF THIS TASK, SHOULD-BE-RES MATCHES IS-RES.
"
        RETURN
" GOT HERE WITH SEG WHICH SHOULD BE RES BUT ISN'T.
"
OUT:    SUB WIDE,SEGENT                "(OVERSHOT IN LOOP)
                                       "GET PTR TO 1ST ENTRY IN
        INC# SEGENT; SETMA             "PS PART TABLE FOR THIS SEG
                                       "GET COUNT OF PS PARTITIONS
        INCMA                          "IN THIS SEG
        DEC# SEGENT; SETMA             "GET SEG'S TASK ID
        LDSPI PPTR; DB=MD;             "ADDR OF 1ST PARTITION ENTRY
         STATMA
        LDSPI PCNT; DB=MD;             "COUNT OF PS PARTITIONS
         STATMA
        LDSPI TSKID; DB=MD             "SEG'S TASK ID
        DEC PPTR                       "OFFSET FIRST INC IN LOOP
" THIS LOOP CHECKS ALL THE PS PARTITIONS FOR A GIVEN OVERLAY SEGMENT,
" MARKING WHAT USED TO BE THERE AS NON-RESIDENT, POINTING TO THE
" NEW SEGMENT, AND MARKING IT RESIDENT.
"
LOOP2:  INC PPTR; SETMA                "GET NEXT PS PART TABLE ENTRY
        STATMA; MI<SPFN;               "NOW MARK IT TO NEW SEG
         MOV SEGENT,SEGENT
        STATMA
        LDSPI OLDSEG; DB=MD            "OLD SEG IN THIS PS PARTITION
        MOV OLDSEG,OLDSEG              "IF THERE WAS NO OLD SEG,
        BEQ CKDONE                     "THEN GO TO ALT LOOP END
        DEC# OLDSEG; SETMA             "GET OLD SEG'S TASK ID
        SUB# SEGENT,OLDSEG;            "NEW SEG = OLD SEG ?
         STATMA
        BEQ CKDONE;                    "IF SO, GO TO ALT LOOP END
         DPX<ZERO;
         STATMA
        LDSPI OLDTID; DB=MD            "OLD TASK ID
        SUB TSKID,OLDTID               "OLD SEG AND NEW SEG IN SAME TASK?
        BEQ .+2;                       "BRANCH IF SO (AND MARK OLD SEG
                                       "AS SHOULDN'T BE RES AND ISN'T)
         DEC PCNT                      "DEC PARTITION COUNTER
        DPX<1;                         "OTHERWISE, MARK OLD SEG AS
         WRTLMN                        "SHOULD BE RES BUT ISN'T
        MOV OLDSEG,OLDSEG; SETMA; MI<DPX; "MARK OLD SEG'S RES ENTRY
         BGT LOOP2                     "LOOP IF NOT DONE ALL PARTITIONS
        DPX<1; BR GO
"GOT HERE IF PARTITION HAD NO SEG OR HAD SAME SEG AS NEW SEG.
"THIS IS THE ALTERNATE LOOP END.
"
CKDONE: DEC PCNT;                      "DEC PARTITION COUNTER
         DPX<1
        BGT LOOP2+1;                   "LOOP IF NOT DONE ALL PARTITIONS
         INC PPTR; SETMA               "(1ST INSTR OF LOOP2)
GO:     MOV SEGENT,SEGENT; SETMA; MI<DPX "MARK NEW SEG SHOULD BE RES AND IS
"
" NOW READY TO LOAD SEG FROM MD TO PS.
"
"
        DEC SEGENT
                                       "GET POINTER TO NEXT AREA
        LDMA; DB=SYSPTR                "IN STACKED SYSTEM SAVE AREA
        DEC SEGENT; SETMA              "GET # OF PS WORDS
        DECMA                          "GET BEGINNING PS ADDR
        DECMA;                         "GET BEGINNING MD ADDR
         DPX(1)<MD                     "SAVE SYSPTR
        DPX(2)<MD;                     "# OF PS WORDS
         STATMA
        DPX(0)<MD                      "PS ADDR
                                       "SET SMA=SYSPTR IN CASE
        LDMA; DB=DPX(1)                "MOVER IS INTERRUPTED
                                       "PARAMS FOR MOVER:
                                       "DPX(0)=PS ADDR
                                       "DPX(1)=MD ADDR
                                       "DPX(2)=# OF PS WORDS
        DPX(1)<MD;                     "MD ADDR
         JSR MOVER                     "MOVE CODE TO PS
"BACK HERE AFTER CODE WAS MOVED.
"IF IT WAS INTERRUPTED, THE STUFF IN PS COULD HAVE BEEN CLOBBERED.
"HENCE WE START CHECKING ALL THE SEGS OVER AGAIN.
"
        JMP LOOP3
"
$NOLIST
$INSERT SYSDEF
$LIST
"
$END
"****** MOVER = CODE MOVER = REL 0.0  , AUG 79 ********************************
"
$TITLE MOVER
$ENTRY MOVER,0
"
"
" MOVES CODE FROM MD TO PS.
" (2 WORDS OF MD = 1 WORD OF PS)
"
"
" THIS PUTS ITSELF INTO USER MODE, USER MA, INTERRUPTS ENABLED
" FOR THE DURATION, THEN SWITCHES BACK TO SUP MODE, SMA, INTS OFF.
"
"
" PARAMETERS:
"  (IN)  DPX(0)=PS ADDRESS
"  (IN)  DPX(1)=MD ADDRESS
"  (IN)  DPX(2)=NUMBER OF PS WORDS
"
" SCRATCH:  SP 0, DPX 0-2, TMA   (DO NOT USE DPX(3).)
"
" TIMING: 5 CYCLES PER PS WORD MOVED.
"
"
" NOTE: THIS ASSUMES THAT IN THE INSTRUCTIONS WHICH INCREMENT MA
"       AND LOAD PS, BOTH OF THESE WILL HAPPEN IN THE SAME CYCLE
"       OF THE 2 CYCLES IT TAKES TO EXECUTE, AND THAT FURTHERMORE
"       THEY HAPPEN IN THE FIRST CYCLE OF THE 2 CYCLES.
"
"       THE LOOP IS A 3-INSTRUCTION, 5-CYCLE LOOP, EXPRESSED
"       CONCEPTUALLY AS FOLLOWS:
"
" 1.  LOOP: INCMA         |  LPSRT; DB=MD
" 2.         ---          |     -----
" 3.        INCMA         |  DEC CTR; INCTMA
" 4.        LPSLT; DB=MD  |  BGT LOOP
" 5.         ------       |     -----
"
"
"       SINCE THE FIRST COLUMN STORES INTO PS, WE MUST DROP OUT
"       OF THE LOOP ONE TIME EARLY.
"
"
"
CTR = 0
"
MOVER:  SELMA;                         "SET TO USER MA
         LDTMA; DB=DPX(0)              "PS ADDR
        LDMA; DB=DPX(1);               "GET 1ST 1/2 OF 1ST PS WORD FROM MD
         CLRMOD                        "SET TO USER MODE
        ION                            "TURN INTS ON
        LDSPI CTR; DB=DPX(2)           "NUMBER OF PS WORDS
        INCMA;                         "GET 2ND 1/2 OF 1ST PS WORD FROM MD
         DEC CTR                       "DEC COUNTER BY 1
        LPSLT; DB=MD;                  "STORE 1ST 1/2 OF 1ST PS WORD
         BEQ OUT                       "SKIP LOOP IF ONLY 1 PS WORD
LOOP:   INCMA;                         "GET 1ST 1/2 OF NTH PS WORD FROM MD
         LPSRT; DB=MD                  "STORE 2ND 1/2 OF (N-1)TH PS WORD
        INCMA;                         "GET 2ND 1/2 OF NTH PS WORD FROM MD
         INCTMA;                       "INC POINTER INTO PS
         DEC CTR                       "DEC WORD COUNTER
        LPSLT; DB=MD;                  "STORE 1ST 1/2 OF NTH PS WORD
         BGT LOOP                      "BRANCH UNTIL THIS IS LAST WORD
OUT:    LPSRT; DB=MD                   "STORE 2ND 1/2 OF LAST PS WORD
        ENTINT;                        "BACK TO SUP MODE, INTS ON, SMA
         RETURN
$END
"****** IO = I/O INTERRUPT HANDLER = REL 0.0  , AUG 79 ************************
"
$TITLE IO
$ENTRY IO,0
$EXT RMVCLK
$EXT PSMNGR
$EXT RTCINT
$EXT INTEXT
$INSERT COMSYS
"
$COMMON /CONFIG/ DEV1(5) /T, DEV2(5) /T, DEV3(5) /T, DEV4(5) /T,
                 DEV5(5) /T, DEV6(5) /T, DEV7(5) /T, DEV8(5) /T,
                 DEV9(5) /T, DEV10(5) /T, DEV11(5) /T, DEV12(5) /T,
                 DEV13(5) /T, DEV14(5) /T, DEV15(5) /T
"
"
" HANDLES I/O INTERRUPT
"
"
" PROCEDURE:
"   MASKS OUT FURTHER INTERRUPTS FROM INTERRUPTING DEVICE
"   AND LOWER PRIORITY DEVICES.  MAKES SURE THE APPROPRIATE
"   INTERRUPT SERVICE ROUTINE (ISR) IS RESIDENT, AND JSR'S
"   TO IT.  UPON RETURN, RE-ADJUSTS IMASK REGISTER AND EXITS
"   THROUGH COMMON INTERRUPT EXIT ROUTINE.
"
"   THE RTC IS HANDLED SEPARATELY.
"
"
" THIS ASSUMES SUP. MODE, SMA, AND INTS. OFF.
"
" PARAMETERS:
"  (IN)  S-PAD 7=ADDR OF CURRENT TASK'S TCB
"
" SCRATCH:  SP 0-4 AND 7, DPX 0-3, DA
"
" ROUTINES CALLED:  RTCINT,  PSMNGR, INTEXT, RMVCLK
"
" TIMING:
"
"   58 CYCLES PLUS THE ISR, ASSUMING THAT THE ISR IS
"      ALREADY RESIDENT AND THE DEVICE IS NOT THE RTC:
"      43 IN IO
"      15 IN PSMNGR
"       ? IN ISR
"
"
" THIS ASSUMES THE DEVICE CONFIGURATION TABLE IS FORMATTED AS
" FOLLOWS FOR EACH ENTRY:
"
"   WORD 1:  PRIORITY MASK (TO MASK OUT LOWER PRIORITY DEVICES)
"   WORD 2:  BIT MASK (THIS DEVICE'S BIT ON)
"   WORD 3:  POINTER TO SERVICE ROUTINE'S ENTRY IN OVERLAY MAP
"   WORD 4:  DEVICE ORDER NUMBER  (EXP)
"            PHYSICAL DEVICE ADDRESS (LMAN)
"   WORD 5:  SAVE AREA FOR OLD IMASK
"
"
W = 5       "WIDTH OF DEVICE CONFIGURATION TABLE ENTRY
MSAVE = 5   "IMASK SAVE AREA WORD WITHIN 5 WORD DEVICE TABLE ENTRY
PRIMSK = 1  "PRI MASK         "     "    "   "    "      "     "
PSADDR = 3  "PS ADDR WORD WITHIN 8 WORD OVERLAY TABLE ENTRY
RES = 6     "RES WORD        "   "   "     "       "    "
"
RTC = 5  "REAL TIME CLOCK DEVICE NUMBER
"
"
IO:     IOINTA; DB=INBS; LDSPI R1      "DEVICE NUMBER
        LDSPI R2; DB=RTC               "RTC VALUE
        LDDA; DB=IMASK;                "DA=I/O INT MASK
         SUB R1,R2                     "CHECK FOR RTC
        BNE NOTRTC;                    "BRANCH IF NOT RTC
         MOVL R1,R3                    "DEV # *2
        JSR RTCINT                     "IF RTC, TAKE CARE OF IT
        BEQ .+2                        "BRANCH IF CLOCK Q WAS EMPTY
                                       "PARAM FOR RMVCLK:
                                       "DPX(1)=TCB ADDR (FROM RTCINT)
        JSR RMVCLK                     "OTHERWISE CHANGE CLOCK Q
        JMP INTEXT                     "EXIT
NOTRTC: BNE .+2;                       "CHECK FOR NO DEV ANSWERED
         MOVL R3,R3;                   "DEV # *4
         DPX(2)<1
        JMP NONE                       "GO AWAY IF NO DEVICE
        ADD R1,R3                      "DEV # *5 (INDEX INTO DEV TABLE)
        LDSPI R7; DB=CONFIG-W          "DEVICE TABLE ADDR
        ADD R3,R7; SETMA               "GET PRI MASK
        INCMA;                         "GET BIT MASK
         IN; DPX<INBS; LDSPI R1        "CURRENT IMASK (OLD)
        INCMA                          "GET PTR TO OVERLAY TABLE ENTRY
        LDSPI R2; DB=MD;               "PRI MASK
         STATMA
        LDSPI R3; DB=MD;               "BIT MASK
         STATMA
        OR R2,R3;                      "MAKE SURE ABOUT CURRENT DEV
         DPX(3)<MD                     "OVTAB ENTRY PTR
                                       "MASK OUT INTERRUPTS FROM
        OR R1,R3; DB=SPFN; OUT         "CURRENT AND LOWER PRI DEVICES
        LDSPI R4; DB=MSAVE-PRIMSK
        ADD# R7,R4; SETMA; MI<DPX      "SAVE OLD IMASK
                                       "PARAMS FOR PSMNGR:
                                       "DPX(3)=PTR INTO OVERLAY TABLE
                                       "DPX(2)=NUMBER OF ENTRIES (=1)
        JSR PSMNGR                     "MAKE SURE ISR IS RESIDENT
"BACK FROM PS MANAGER...
"
        LDMA; DB=SYSPTR                "GET PTR INTO SYSTEM SAVE AREA
        LDSPI R3; DB=PSADDR-RES
        INC# R7; SETMA                 "GET BIT MASK
        INCMA;                         "(PUSH)
                                       "(DPX(3) LEFT BY PSMNGR:)
         LDSPI R4; DB=DPX(3)           "RES WORD WITHIN OVTAB ENTRY
        INCMA;                         "GET DEV ORDER #
                                       "AND PHYS DEV ADDR
         DPX<MD                        "SYSPTR
        LDSPI R0; DB=MD;               "BIT MASK
         STATMA                        "(REPEAT OF INCMA)
        ADD R3,R4; SETMA               "GET PS ADDR FROM OVTAB
        LDSPE R1; DB=MD;               "DEV ORDER #
         STATMA
        LDSPI R2; DB=MD;               "PHYS DEV ADDR
         STATMA
        LDTMA; DB=MD;                  "ISR'S PS ADDR
         CLRMOD                        "SET TO USER MODE
        LDSPI R4; DB=512.
        LDMA; DB=DPX;                  "SET SMA = SYSPTR
         ADD R4,R1                     "(ADJUST FOR LDSPE)
        SELMA                          "SET TO USER MA
                                       "PARAMS FOR ISR:
                                       "SP0=BIT MASK
                                       "SP1=DEV ORDER #
                                       "SP2=PHYS DEV ADDR
        JSRT                           "JSR TO ISR
" BACK FROM INTERRUPT SERVICING...
"
"
" THE ISR RETURNED THE BIT MASK EITHER ON OR OFF TO DISABLE/ENABLE
" FURTHER INTERRUPTS FROM THIS DEVICE.
"
" SET CURRENT IMASK = (CURRENT IMASK 'AND' 'NOT' PRI MASK) 'OR'
"                          RETURNED BIT MASK 'OR' OLD IMASK.
"
        ENTINT;                        "SUP MODE, SMA, INTS OFF
         LDDA; DB=IMASK                "DA=IMASK
        MOV R7,R7; SETMA               "GET PRI MASK
        IN; DB=INBS; LDSPI R1;         "CURRENT IMASK
         INCMA                         "(PUSH)
        LDSPI R4; DB=MSAVE-PRIMSK;
         STATMA
        LDSPI R2; DB=MD                "PRI MASK
        ADD R7,R4; SETMA               "GET OLD IMASK
        COM R2;                        "COMPLEMENT PRI MASK
         STATMA
        AND R1,R2                      "R2=CURRENT 'AND' 'NOT' PRI MASK
        OR R0,R2;                      "R2=RETURNED BIT MASK 'OR' ABOVE R2
         STATMA
        LDSPI R3; DB=MD                "OLD IMASK
        OR R3,R2;                      "R2=OLD 'OR' ABOVE R2
         DB=SPFN; OUT                  "SET NEW IMASK
OUT:    JMP INTEXT                     "EXIT
"
"
" GOT HERE IF THERE WAS NO INTERRUPTING DEVICE
" (SPURIOUS I/O INTERRUPT)
"
NONE:   LDMA; DB=LOGIOD                "GET COUNT OF FAKE I/O INTS.
        STATMA
        STATMA
        LDSPI R0; DB=MD                "COUNT
        INC R0; STATMA; MI<SPFN;       "INC AND STORE
         BR OUT                        "EXIT
$NOLIST
$INSERT SYSDEF
$LIST
$END
"****** INTEXT = INTERRUPT EXIT ROUTINE = REL 0.0  , AUG 79 *******************
"
$TITLE INTEXT
$ENTRY INTEXT,0
$EXT EXTASK
$EXT MNREST
$INSERT COMSYS
"
"
" COMMON INTERRUPT EXIT.
"
"
" PROCEDURE:
"   FINDS OUT IF ORIGINALLY CAME FROM AN ISR, THE PS MANAGER, OR
"   A TASK.  IF AN ISR OR THE PS MANAGER, RETURN TO IT.
"   OTHERWISE, GO TO EXTASK (PICK THE HIGHEST PRIORITY TASK
"   AND RUN IT).
"
"
" THIS ASSUMES SUP. MODE, SMA, AND INTS OFF.
"
" PARAMETERS:
"   (OUT) SP0=SYSPTR-1 (FOR MNREST)
"
"
" SCRATCH: SP 0-1
"
" ROUTINES CALLED: EXTASK, MNREST
"
" TIMING: 8 CYCLES.
"
"
"
"  FIND OUT WHERE WE CAME FROM...
"
INTEXT: LDMA; DB=SYSPTR                "GET SYSPTR
        LDMA; DB=SYSAVE                "GET BEGINNING OF SYS SAVE AREA
        STATMA
        LDSPI 0;  DB=MD;               "SYSPTR
         STATMA
        LDSPI 1;  DB=MD                "SYSAVE
        SUB# 0,1                       "SYSPTR = SYSAVE ?
        BNE .+2;                       "BRANCH IF NOT
                                       "SET SMA AND SP0 = SYSPTR-1
         DEC 0;  SETMA                 "FOR MNREST
" GOT HERE IF SYSPTR=SYSAVE, I.E. CAME FROM USER TASK
"
        JMP EXTASK                     "PICK AND RUN HIGHEST PRI TASK
" GOT HERE IF CAME FROM ANOTHER ISR OR PSMNGR.
" RESTORE STATE AND RETURN.
"
        JMP MNREST
$END
"****** RTCINT = RTC INTERRUPT HANDLER = REL 0.0  , AUG 79 ********************
"
$TITLE RTCINT
$ENTRY RTCINT,0
$EXT EMPTY
$EXT DELETE
$EXT RESUME
$INSERT COMSYS
"
"
" HANDLES INTERRUPT FROM REAL TIME CLOCK
"
"
" PROCEDURE:
"   GETS FIRST ELEMENT FROM CLOCK QUEUE (A TCB ADDRESS).
"   DELETES THE TASK FROM THE EXCHANGE WHERE IT HAD BEEN WAITING.
"   RETURNS A TIMEOUT MESSAGE TO IT.
"   PUTS IT BACK INTO THE READY QUEUE.
"
"   (WHEN DELETING THE TASK FROM THE EXCHANGE WHERE IT HAD BEEN
"   WAITING, THE EXCHANGE TYPE MIGHT NOW NEED TO BE CHANGED TO
"   'EMPTY'.  FIRST WE HAVE TO FIND OUT WHICH EXCHANGE IT IS.
"   THIS IS DONE BY COMPARING THE TCB'S LINKS BEFORE THE DELETION:
"   IF THE RIGHT LINK AND LEFT LINK ARE POINTING TO THE SAME
"   THING, IT MUST BE THE EXCHANGE, AND THIS MUST BE THE ONLY
"   TCB ON IT.  THEREFORE IT WILL BE EMPTY AFTER DELETION.
"   IF THE TCB'S LINKS WERE NOT THE SAME, IT DOESN'T MATTER
"   WHICH EXCHANGE IT WAS, BECAUSE THERE ARE OTHER TCB'S ON IT,
"   SO IT WON'T BE EMPTY AFTER DELETION.)
"
"
" THIS ASSUMES SUP. MODE AND INTS. OFF.
"
" PARAMETERS:
"  (OUT) DPX(1)=TCB ADDR
"  (OUT) SP0 AND SPFN = 0 IF CLOCK QUEUE WAS EMPTY,
"                       TCB ADDR OTHERWISE.
"
" SCRATCH: SP 0-4, DPX 0-3
"
" ROUTINES CALLED:  EMPTY, DELETE, RESUME
"
" TIMING:  60-62 CYCLES + 4/ELE SEARCHED
"                18-20 IN RTCINT
"                5 IN EMPTY
"                7 IN DELETE
"                30 + 4/ELE IN RESUME
"
"
DP = 1
EXCH = 2
EXCH2 = 3
SP = 3
TCB = 4
"
"
RTCINT: DPX(2)<CLKQUE                  "CLOCK Q ADDR
                                       "PARAM TO EMPTY:
                                       "DPX(2)=Q HEADER
        JSR EMPTY                      "SEE IF CLOCK Q EMPTY
                                       "RETURNS FROM EMPTY WITH:
                                       "DPX(3)=ADDR OF FIRST ELEMENT IN Q
        BNE OK;                        "BRANCH IF CLOCK Q NOT EMPTY
         LDSPI TCB; DB=DPX(3)          "TCB'S RIGHT CLOCK LINK
" GOT HERE IF THERE WAS NOTHING IN CLOCK QUEUE.
"
        CLR R0;                        "SET SPFN
         RETURN
"
"
OK:     LDSPI R1; DB=RCLOCK            "OFFSET
        SUB R1,TCB; DPX(3)<SPFN;       "TCB ADDR
         LDMA                          "GET RLINK(TCB)
        INCMA;                         "GET LLINK(TCB)
         DPX(1)<DPX(3)                 "TCB ADDR
        LDSPI DP; DB=DPX0;             "TCB'S DPX0 OFFSET
         STATMA
        LDSPI EXCH; DB=MD;             "RLINK
         STATMA
        LDSPI EXCH2; DB=MD             "LLINK
        DPX<TIMOUT;                    "TIMOUT MSG ADDR
         SUB EXCH,EXCH2                "RLINK=LLINK?
        BNE NOTEX;                     "IF SO, IT'S EXCH, AND WILL
                                       "BE EMPTY AFTER DELETING TCB.
         LDSPI SP; DB=SPFUNC           "TCB'S SPFN OFFSET
        LDSPI R0; DB=TYPE              "SO IF IT'S AN EXCH,
        ADD EXCH,R0; SETMA; MI<ZERO    "SET EXCH TYPE TO EMPTY
                                       "PUT TIMEOUT MSG ADDR
NOTEX:  ADD TCB,DP; SETMA; MI<DPX      "INTO TCB'S DPX0 LOC
        DPX<TIMBIT                     "TIMEOUT MSG TYPE
        ADD TCB,SP; SETMA; MI<DPX      "INTO TCB'S SPFN
        DECMA; MI<DPX                  "AND INTO TCB'S SP0
                                       "PARAM FOR DELETE:
                                       "DPX(3)=ADDR OF ELE TO BE DELETED
                                       "TAKE TCB OFF EXCH
        JSR DELETE                     "WHERE HAD BEEN WAITING
                                       "PARAM FOR RESUME:
                                       "DPX(1)=TCB ADDR
        JSR RESUME                     "PUT BACK INTO READY QUEUE
        MOV TCB,R0; DPX(1)<SPFN;       "TCB ADDR (FOR RMVCLK)
         RETURN
$NOLIST
$INSERT SYSDEF
$LIST
$END
"****** CHKPT = CHECKPOINT = REL 0.0  , AUG 79 ********************************
"
$TITLE CHKPT
$ENTRY CHKPT,0
$EXT IO
$INSERT COMSYS
"
"
" CHECKPOINT
"
" PROCEDURE:
"   SEE IF THERE'S AN INTERRUPT WAITING.
"   IF SO, CLEAR SRS AND GO HANDLE IT.
"
" THIS ASSUMES SUP. MODE AND INTS OFF.
"
" PARAMETERS:
"  (OUT) SPAD 1 = DEVICE INTERRUPT NUMBER (0-15)
"  (OUT) SPAD 7 = CURTSK  (FOR IO EXIT ONLY)
"
" SCRATCH: SP 0-2,7  DA
"
" ROUTINES CALLED: IO
"
" TIMING:  4 CYCLES IF NO DEVICE TRYING TO INTERRUPT.
"          8 CYCLES IF INTERRUPTS WERE PENDING.
"
"
CHKPT:  IOINTA; DB=INBS; LDSPI R1      "DEV INT NUMBER
        MOV R1,R1;                     "DID A DEV ANSWER?
         LDMA; DB=CURTSK               "GET CURTSK
        BEQ NOINT;                     "IF NOT, JUST RETURN
         LDSPI R2; DB=176077;          "APSTAT3 MASK FOR SRA BITS
         STATMA
        LDDA; DB=APST3                 "APSTAT3 DEV ADDR
        IN; DB=INBS; LDSPI R0;         "GET APSTAT3
         INCMA                         "(JUST A PUSHER)
        AND R2,R0; DB=SPFN; OUT        "SET SRA=0
        LDSPI R7; DB=MD                "CURTSK'S TCB ADDR
        JMP IO+1
"GOT HERE IF NO INTERRUPTS TO BE SERVICED
"
NOINT:  RETURN
"
$NOLIST
$INSERT SYSDEF
$LIST
$END
"****** IDLE = IDLING ROUTINE = REL 0.0  , AUG 79 *****************************
"
$TITLE IDLE
$ENTRY IDLE,0
"
"
" THIS IS CALLED WHEN THERE ARE NO TASKS READY TO BE RUN
" AND NO I/O INTERRUPTS PENDING.
"
"
" THE USER CAN CHANGE THIS ROUTINE IF DESIRED, OR MAKE IT INTO
" JUST A RETURN STATEMENT.  CURRENTLY, IT KEEPS A COUNT (IN PS)
" OF THE NUMBER OF TIMES WE GO THROUGH HERE.
"
" THIS ASSUMES SUP. MODE, SMA, INTERRUPTS OFF.
"
"
" SCRATCH: SPAD 0, DPX(2)
"
" TIMING: 6 CYCLES
"
"
"
IDLE:   RPSL COUNT; DPX(2)<DB;         "FETCH COUNT
         BR .+2
COUNT:  $VAL 0, 0, 0, 0                "COUNT IS KEPT HERE
        LDSPI 0; DB=DPX(2)
        INC 0; DPX(2)<SPFN             "INCREMENT
        LPSL COUNT; DB=DPX(2);         "STORE BACK INTO PS
         RETURN
$END
"****** TRAP = TRAP HANDLER = REL 0.0  , AUG 79 *******************************
"
$TITLE TRAP
$ENTRY TRAP,0
$EXT INTEXT
"
"
" HANDLES TRAP INTERRUPT
"
"
" PROCEDURE:
"   LOADS TMA AND DPX 0-3 FROM CURRENT TASK'S TCB SAVE AREA
"   INTO THE PHYSICAL REGISTERS.
"   DOES JSRT TO SVC.
"   STORES DPX 0-3, SP 0, AND SPFN BACK INTO TCB.
"
" NOTE:  THIS WILL (PURPOSELY) CLOBBER THE USER'S
"        DPX 0-3, AND SPAD 0.
"
" THIS ASSUMES SUP. MODE AND INTS. OFF
"
" PARAMETERS:
"   (IN)   SP 7=ADDR OF CURRENT TASK'S TCB
"
" SCRATCH: SP 0,1,7,   DPX 0-3
"
" NOTE:  WE GET HERE AFTER AN INTERRUPT OCCURS,
"        THE STATE SAVE HAS BEEN DONE, THE CURRENT
"        TASK'S TCB ADDRESS IS PUT INTO SPAD 7,
"        AND IT HAS BEEN DETERMINED THAT THE
"        INTERRUPT WAS A TRAP.
"
"        IT IS ASSUMED THAT THE USER'S LAST INSTRUCTION WAS:
"
"             TRAP; DB=@XXX; LDTMA
"
"        WHERE XXX IS AN SVC NAME  (ADDR).
"
" TIMING: 20 CYCLES PLUS SVC
"
"
"
TCB = 7
"
TRAP:   LDSPI R0; DB=TMAREG            "TCB'S TMA OFFSET
        ADD TCB,R0; SETMA              "GET TMA VALUE (SVC ADDR)
        LDSPI R0; DB=DPX0;             "DPX(0) 0FFSET
         STATMA
        ADD TCB,R0; SETMA              "GET DPX(0) FROM TCB
        INCMA;                         "GET DPX(1)
         LDTMA; DB=MD                  "TMA=SVC ADDR
        INCMA                          "GET DPX(2)
        INCMA;                         "GET DPX(3)
         DPX(0)<MD                     "DPX(0) PARAM FOR SVC
        DPX(1)<MD;                     "DPX(1) PARAM
         STATMA
        DPX(2)<MD;                     "DPX(2) PARAM
         STATMA
        DPX(3)<MD;                     "DPX(3) PARAM
         JSRT                          "JSR TO SVC VIA TMA
"GOT HERE WHEN SVC IS DONE
"
        LDSPI R1; DB=DPX0              "DPX(0) OFFSET
        ADD TCB,R1; SETMA; MI<DPX(0)   "STORE DPX(0) INTO TCB
        INCMA; MI<DPX(1)               "STORE DPX(1)
        INCMA; MI<DPX(2)               "STORE DPX(2)
        INCMA; MI<DPX(3)               "STORE DPX(3)
        LDSPI R1; DB=SPFUNC            "SPFN OFFSET
        MOV R0,R0; DPX<SPFN
        ADD TCB,R1; SETMA; MI<DPX      "STORE SPFN
        DECMA; MI<DPX                  "STORE SPAD 0
        JMP INTEXT
$NOLIST
$INSERT SYSDEF
$LIST
$END
"****** FATAL = FATAL/EXCEPTION INTERRUPT HANDLER = REL 0.0  , AUG 79 *********
"
$TITLE FATAL
$ENTRY FATAL,0
$ENTRY FPE,0
$EXT DELETE
$EXT INSERT
$EXT INTEXT
$INSERT COMSYS
"
"
" FATAL OR FPE INTERRUPT HANDLING
"
" CURRENTLY, BOTH INTERRUPTS RESULT IN ABNORMAL TERMINATION
" OF THE TASK CAUSING THE INTERRUPT.
"
" THE USER MAY DESIRE TO DO OTHER FORMS OF HANDLING HERE.
" NOTE THAT THIS IS IN SUPERVISOR MODE, I.E. MEMORY FETCHES
" MUST BE PUSHED.  IF SWITCHING TO USER MODE FOR THE
" DURATION, REMEMBER TO SWITCH BACK BEFORE EXITING TO INTEXT.
"
" PARAMETERS:
"  (IN)  S-PAD 7=ADDR OF CURRENT TASK'S TCB
"
" ROUTINES CALLED: DELETE, INSERT
"
" SCRATCH: SP 7, DPX 2-3
"
" TIMING:  24 CYCLES FOR FATAL OR FPE.
"             6 IN FATAL OR FPE
"             7 IN DELETE
"             11 IN INSERT
"
"
TCB = 7
"
FPE:    JMP ABTERM
"
"
FATAL:  JMP ABTERM
"
"
ABTERM: MOV TCB,TCB; DPX(3)<SPFN       "TCB ADDR
                                       "PARAM FOR DELETE:
                                       "DPX(3)=ELEMENT TO BE DELETED
        JSR DELETE                     "DELETE TCB FROM READY QUEUE
        DPX(2)<MORGUE                  "ABNORMAL TERM EXCH ADDR
                                       "PARAMS FOR INSERT:
                                       "DPX(2)=NEXT=Q HEAD=EXCH
                                       "DPX(3)=NEW=TCB ADDR
        JSR INSERT                     "ADD TCB TO MORGUE QUEUE
"
" EXIT
"
        JMP INTEXT
$END
"****** PRIQ = PRIORITY INSERT INTO QUEUE = REL 0.0  , AUG 79 *****************
"
$TITLE PRIQ
$ENTRY PRIQ,0
$EXT INSERT
"
"
" DOES PRIORITY INSERT INTO QUEUE, BEFORE FIRST ELEMENT
" OF LOWER PRIORITY.
" ASSUMES QUEUE HEADER TO HAVE PRIORITY 0.
"
" THIS ASSUMES SUP. MODE AND INTERRUPTS OFF.
"
" PARAMETERS:
"  (IN)  DPX(2)=QUEUE HEADER
"  (IN)  DPX(3)=NEW ELEMENT
"  (OUT) SP0 AND SPFN = 0 IF OK
"                     = NON-0 IF NEW ALREADY LINKED ELSEWHERE
"
"
" SCRATCH: SP 0-3, DPX 0-3
"
" ROUTINES CALLED: INSERT
"
" TIMING: 20 CYCLES IF NO ERROR AND QUEUE EMPTY
"             9 CYCLES IN PRIQ
"            11 CYCLES IN INSERT
"         21 CYCLES + 4 CYCLES PER ELEMENT SEARCHED
"            IF NO ERROR AND QUEUE NOT EMPTY
"         16 CYCLES IF ERROR AND QUEUE EMPTY
"             9 CYCLES IN PRIQ
"             7 CYCLES IN INSERT
"
"
" (TABBING IN CODE REFLECTS COLUMNS OF LOOP.)
"
"
TEMP1 = 1
NEWPRI = 1
TEMP2 = 2
NXTPRI = 2
HEAD  = 3
"
"
PRIQ:   LDSPI TEMP2; DB=2              "OFFSET TO PRIORITY
        LDMA; DB=DPX(2);               "GET ELE.1=RLINK(HEAD)
         LDSPI HEAD                    "HEAD
        LDSPI TEMP1; DB=DPX(3);        "NEW
         STATMA
        ADD TEMP1,TEMP2; SETMA         "GET PRI(NEW)
        LDMA; DB=MD;                   "GET ELE.2=RLINK(ELE.1)
         DPX(1)<DB;                    "SAVE ADDR ELE.1
         LDSPI TEMP2                   "ELE.1
        INCMA;
         SUB TEMP2,HEAD                "ELE.1=HEAD?
        BEQ OUT;                       "IF SO, QUEUE EMPTY
                                       "OTHERWISE
         INCMA;                        "GET PRI(ELE.1)
         LDSPI NEWPRI; DB=MD           "PRI(NEW)
        LDMA; DB=MD;                   "GET ELE.3=RLINK(ELE.2)
         DPX<DB                        "SAVE ADDR ELE.2
        INCMA;
              DPX(2)<DPX(1)            "SAVE ADDR ELE.1
        INCMA;                         "GET PRI(ELE.2)
              LDSPI NXTPRI; DB=MD      "PRI(ELE.1)
        DPX(1)<DPX;                    "SAVE ADDR ELE.2
              SUB NEWPRI,NXTPRI        "COMPARE PRIORITIES
LOOP:   LDMA; DB=MD;                   "GET ELE.N+3=RLINK(ELE.N+2)
         DPX<DB;                       "SAVE ADDR ELE.N+2
                                       "BRANCH IF
                    BLT OUT            "PRI(ELE.N)<PRI(NEW)
        INCMA;
              DPX(2)<DPX(1)            "SAVE ADDR ELE.N+1
        INCMA;                         "GET PRI(ELE.N+2)
              LDSPI NXTPRI; DB=MD      "PRI(ELE.N+1)
        DPX(1)<DPX;                    "SAVE ADDR ELE.N+2
              SUB NEWPRI,NXTPRI;       "TEST PRI(ELE.N+1)<PRI(NEW)
                    BR LOOP
                                       "DPX(2) CONTAINS ADDR OF NEXT ELE,
OUT:    JSR INSERT                     "BEFORE WHICH NEW WILL BE INSERTED
        RETURN
$END
"****** INSERT = INSERT INTO QUEUE = REL 0.0  , AUG 79 ************************
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
"****** DEQ = DEQUEUE = REL 0.0  , AUG 79 *************************************
"
$TITLE DEQ
$ENTRY DEQ,0
"
"
" UNQUEUES FIRST ELEMENT AFTER QUEUE HEADER.
"
"
" THIS ASSUMES SUP. MODE AND INTERRUPTS OFF.
"
"
" PARAMETERS:
"  (IN)   DPX(2)=QUEUE HEADER  (LAST)
"  (OUT)  DPX(3)=ADDR OF DEQUEUED ELEMENT, IF ANY  (OUT)
"                OTHERWISE QUEUE HEADER
"  (OUT)  SP0=0 IF QUEUE WAS EMPTY,
"               NON-0 IF ELEMENT WAS DEQUEUED.
"
"
" SCRATCH:  SP 0-3, DPX 2-3
"
"
" TIMING: 10 CYCLES IF ELEMENT DEQUEUED
"          7 CYCLES IF QUEUE WAS EMPTY
"
"
NEXT = 1
OUT = 2
LAST = 3
"
"
DEQ:    LDSPI LAST; DB=DPX(2);         "LAST
          LDMA                         "GET OUT=RLINK(LAST)
        STATMA; LDSPI 0; DB=1
        STATMA
        LDSPI OUT; DPX(3)<MD;          "OUT
          LDMA                         "GET NEXT=RLINK(OUT)
                                       "(NO STATMA)
        SUB# LAST,OUT                  "IF SAME, THEN EMPTY
        BEQ EMPTY;                     "IF EMPTY, FORGET IT.
          MOV OUT,OUT; SETMA; MI<DPX(3) "RLINK(OUT)=OUT
        INCMA; MI<DPX(3)               "LLINK(OUT)=OUT
                                       "(NO STATMA)
        LDSPI NEXT; DB=MD              "NEXT
        MOV LAST,LAST; SETMA; MI<MD    "RLINK(LAST)=NEXT
        INC NEXT; SETMA; MI<DPX(2);    "LLINK(NEXT)=LAST
          RETURN
EMPTY:  CLR 0; RETURN
$END
"****** EMPTY = CHECKS FOR EMPTY QUEUE = REL 0.0  , AUG 79 ********************
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
"****** DELETE = DELETE FROM QUEUE = REL 0.0  , AUG 79 ************************
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
 