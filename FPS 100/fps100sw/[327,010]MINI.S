"****** SYSCOM = SYSTEM COMMON INITIALIZATIONS = REL 1.0  , NOV 79 ************
"
$TITLE SYSCOM
"
$COMMON /DOT/ DTABLK(33.)  /I
$INSERT MINCOM
"
"
"
$DATA RDYQUE(1) RDYQUE
$DATA RDYQUE(2) RDYQUE
$DATA SYSAVE SSAREA
$DATA SYSPTR SSAREA
$DATA SYSTOP SSAREA+SYSIZE-1
$DATA LOGITS 0
$DATA LOGIOD 0
"
$END
"****** CONFIG = I/O DEVICE CONFIGURATION TABLE = REL 1.0  , NOV 79 ***********
"
$TITLE CONFIG
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
$DATA DEV1(1)    0,0,    040000
$DATA DEV1(2)    0,0,    040000
$DATA DEV1(3)    0,0,    ISRMAP+0
$DATA DEV1(4)    0,0,    0
$DATA DEV1(5)    0,0,    0
"
$DATA DEV2(1)    0,0,    020000
$DATA DEV2(2)    0,0,    020000
$DATA DEV2(3)    0,0,    ISRMAP+(W*1)
$DATA DEV2(4)    0,0,    0
$DATA DEV2(5)    0,0,    0
"
$DATA DEV3(1)    0,0,    010000
$DATA DEV3(2)    0,0,    010000
$DATA DEV3(3)    0,0,    ISRMAP+(W*2)
$DATA DEV3(4)    0,0,    0
$DATA DEV3(5)    0,0,    0
"
$DATA DEV4(1)    0,0,    004000
$DATA DEV4(2)    0,0,    004000
$DATA DEV4(3)    0,0,    ISRMAP+(W*3)
$DATA DEV4(4)    0,0,    0
$DATA DEV4(5)    0,0,    0
"
" REAL TIME CLOCK
"
$DATA DEV5(1)    0,0,    002000
$DATA DEV5(2)    0,0,    002000
$DATA DEV5(3)    0,0,    ISRMAP+(W*4)
$DATA DEV5(4)    0,0,    372
$DATA DEV5(5)    0,0,    0
"
" HOST (DMA)
"
$DATA DEV6(1)    0,0,    001000
$DATA DEV6(2)    0,0,    001000
$DATA DEV6(3)    0,0,    ISRMAP+(W*5)
$DATA DEV6(4)    0,0,    0
$DATA DEV6(5)    0,0,    0
"
" HOST (NON-DMA INTERRUPT)
"
$DATA DEV7(1)    0,0,    000400
$DATA DEV7(2)    0,0,    000400
$DATA DEV7(3)    0,0,    ISRMAP+(W*6)
$DATA DEV7(4)    0,0,    0
$DATA DEV7(5)    0,0,    0
"
" IOP16 (DMA)
"
$DATA DEV8(1)    0,0,    000200
$DATA DEV8(2)    0,0,    000200
$DATA DEV8(3)    0,0,    ISRMAP+(W*8.)
$DATA DEV8(4)    1,0,    10
$DATA DEV8(5)    0,0,    0
"
" IOP16 (NON-DMA INTERRUPT)
"
$DATA DEV9(1)    0,0,    000100
$DATA DEV9(2)    0,0,    000100
$DATA DEV9(3)    0,0,    ISRMAP+(W*8.)
$DATA DEV9(4)    1,0,    10
$DATA DEV9(5)    0,0,    0
"
" IOP38 (DMA)
"
$DATA DEV10(1)    0,0,   000040
$DATA DEV10(2)    0,0,   000040
$DATA DEV10(3)    0,0,   ISRMAP+(W*8.)
$DATA DEV10(4)    1,0,   20
$DATA DEV10(5)    0,0,   0
"
" IOP38 (NON-DMA INTERRUPT)
"
$DATA DEV11(1)    0,0,   000020
$DATA DEV11(2)    0,0,   000020
$DATA DEV11(3)    0,0,   ISRMAP+(W*8.)
$DATA DEV11(4)    1,0,   20
$DATA DEV11(5)    0,0,   0
"
" IOP38 (DMA)
"
$DATA DEV12(1)    0,0,   000010
$DATA DEV12(2)    0,0,   000010
$DATA DEV12(3)    0,0,   ISRMAP+(W*8.)
$DATA DEV12(4)    2,0,   40
$DATA DEV12(5)    0,0,   0
"
" IOP38 (NON-DMA INTERRUPT)
"
$DATA DEV13(1)    0,0,   000004
$DATA DEV13(2)    0,0,   000004
$DATA DEV13(3)    0,0,   ISRMAP+(W*8.)
$DATA DEV13(4)    2,0,   40
$DATA DEV13(5)    0,0,   0
"
" GPIOP
"
$DATA DEV14(1)    0,0,   000002
$DATA DEV14(2)    0,0,   000002
$DATA DEV14(3)    0,0,   ISRMAP+(W*13.)
$DATA DEV14(4)    1,0,   100
$DATA DEV14(5)    0,0,   0
"
" GPIOP
"
$DATA DEV15(1)    0,0,   000001
$DATA DEV15(2)    0,0,   000001
$DATA DEV15(3)    0,0,   ISRMAP+(W*13.)
$DATA DEV15(4)    2,0,   200
$DATA DEV15(5)    0,0,   0
"
$END
"****** SAVRST = SAVE/RESTORE SEQUENCE = REL 1.0  , NOV 79 ********************
"
        $TITLE SAVRST
        $ENTRY MNSAVE,0
        $ENTRY MNREST,0
        $EXT TRAP
        $EXT FATAL
        $EXT FPE
        $EXT IO
        $EXT INTEXT
$INSERT MINCOM
"
"
" STATE SAVE/RESTORE SEQUENCE
"
"
        "THIS SEQUENCE IS DESIGNED TO SAVE AND RESTORE THE STATE OF A
        "FPS-100 FOLLOWING A HARDWARE INTERRUPT.  THE PROGRAM ASSUMES
        "THE FOLLOWING UPON ENTRANCE:
        "MODE IS SET TO SUPERVISOR
        "MEMORY SELECT IS SET TO SMA
        "UNIVERSAL INTERRUPTS ARE INHIBITED
        "ON INTERRUPT, THE FOLLOWING DATA IS STORED IN APSTAT2 BITS 0-2:
        "
        "BIT 0   APMO     MODE OF MACHINE BEFORE INTERRUPT
        "    1  PINTU     PREVIOUS INTU = UNIVERSAL INTERRUPT ENABLE/DISABLE
        "    2   PMAS     PREVIOUS MA/SMA SELECT
        "
        "STATISTICS:
        "LANGUAGE:      FPS-100 ASSEMBLER
        "EQUIPMENT:     FPS-100 (FAST MEMORY)
        "HISTORY:
        "ORIGINAL:      MARCH 78   G. J. THROOP
        "TEST VERSION:  FEBRUARY 79 G. J. THROOP
        "SUPER-100 VERSION: MARCH 79, S. CAMLEY
        "MINI-100 VERSION:  AUG 79, S. CAMLEY
        "
        "
        " TIMINGS (APPROXIMATE):
        "   MINIMUM STATE SAVE: 119-127 CYCLES
        "   MINIMUM STATE RESTORE: 93 CYCLES
        "
        "
        "
        "MINIMUM STATE IS:
        "
        "MD FIFO(1), MD FIFO(2), MD FIFO(3), DATA PAD BUS MANTISSA BITS 0-1
        "DPX WRITE BUFFER, APSTATUS, DPX(DPA) THROUGH DPX(DPA+3), DA,
        "SPD, SP(0), SPFN, SP(1)-SP(7), APSTAT2, TMA, TMREG, FFT STATUS BITS,
        "MA, APSTAT3, SRS(15)-SRS(0)
        "
        "THE ORDER OF SOME OF THE CODE IN THE MINIMUM STATE SAVE/RESTORE
        "SEQUENCE IS CRITICAL AND CAUTION SHOULD BE USED WHEN ALTERING IT.
        "
"       """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
       "                                                               "
       "              SAVE THE MINIMUM STATE OF THE SYSTEM             "
       "                                                               "
       """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
"
"
MNSAVE: MI<MD;                          "1. SAVE FIFO(1)
         STATMA
        INCMA; MI<MD                    "2. SAVE FIFO(2)
        INCMA; MI<MD                    "3. SAVE FIFO(3)
        INCMA;                          "4. HOLE (CURRENTLY UNUSED).
         RDPI                            "TO SAVE DPX WRITE BUFFER
        MI<DPX; INCMA                   "5. SAVE DPX WRITE BUFFER
        PN2DBL;
         RAPS; MI<DB; INCMA             "6. SAVE APSTAT1
        MI<DPX; INCMA                   "7. SAVE DPX
        MI<DPX(1); INCMA                "8. SAVE DPX(1)
        MI<DPX(2); INCMA                "9. SAVE DPX(2)
        MI<DPX(3); INCMA                "10. SAVE DPX(3)
        DPX<SPFN                        "TEMP STORE FOR SPFN
        PN2DBL;
         RDA; MI<DB; INCMA              "11. SAVE DEVICE ADDRESS
        PN2DBL;
         RSPD; MI<DB; INCMA             "12. SAVE SPD
        MOV 0,0; MI<SPFN; INCMA         "13. SAVE SP 0
        MI<DPX; INCMA                   "14. SAVE SPFN
        MOV 1,1; MI<SPFN; INCMA         "15. SAVE SP 1
        MOV 2,2; MI<SPFN; INCMA         "16. SAVE SP 2
        MOV 3,3; MI<SPFN; INCMA         "17. SAVE SP 3
        MOV 4,4; MI<SPFN; INCMA         "18. SAVE SP 4
        MOV 5,5; MI<SPFN; INCMA         "19. SAVE SP 5
        MOV 6,6; MI<SPFN; INCMA         "20. SAVE SP 6
        MOV 7,7; MI<SPFN; INCMA         "21. SAVE SP 7
        "THE FOLLOWING CODE SWAPS FIFO(1) AND FIFO(3) SO
        "THAT IN RESTORE FOLLOWING INTERRUPT THE FIFO IS
        "FILLED SO THAT FIFO(1) IS OUT FIRST AND FIFO(3)
        "IS OUT LAST.
        RMA; LDSPNL 0                   "READ SMA TO SP 0
        LDSPI 1; DB=1.-21.              "BIAS TO FIFO(1) *****
        ADD# 0,1; SETMA                 "GET FIFO(1)
        INCMA                           "BUMP SMA
        INCMA                           "GET FIFO(3)
        STATMA;
         MI<MD                          "STORE FIFO(1)
        DECMA;                          "BUMP SMA
         LDAPS; DB=ZERO                 "TO READ TMA
        DECMA; MI<MD                    "STORE FIFO(3)
        MOV 0,0; SETMA;                 "RESTORE SMA
         DPX<TM                         "SAVE TMREG
        "GO BACK TO STORING STATE OF MACHINE
        LDDA; DB=377                    "SET DEVICE ADDRESS TO APSTAT2
        IN; DB=INBS; MI<DB; INCMA;      "22. SAVE APSTAT2
         LDSPI 0                        "APSTAT2 TO TEMP STORE
        "APSTAT HAS BEEN CLEARED SO TM REFERENCES ARE NOT MESSED UP
        "BY  FFT MODE.
        "NOW SAVE TMREG AND TMA.
        "IN RESTORE, THE FFT STATUS BITS MUST BE RESET BEFORE THE FINAL READ.
        PN2DBL;
         RTMA; MI<DB; INCMA             "23. SAVE TMA
        INCMA; MI<DPX                   "24. SAVE TMREG
        RAPS; PN2DBL;
         INCMA; MI<DB                   "25. SAVE FFT STATUS BITS
       """""""""""""""""""""""""""""""""""""""""""""""""""""
       "                                                   "
       "  SAVE MA, APSTAT3, AND FULL SRS                   "
       "                                                   "
       """""""""""""""""""""""""""""""""""""""""""""""""""""
        NOP                             " (FOR ROMA)
        INCMA; MI<DB; PN2DBL; ROMA      "26. SAVE USER MA
" APSTAT3 CONTAINS THE CURRENT SRA.
"
        LDDA; DB=376                    "SET DA TO APSTAT3
        IN; DB=INBS; LDSPI 1;           "GET APSTAT3
         INCMA; MI<DB                   "27.  SAVE APSTAT3
        LDSPI 2; DB=001700              "SRA BITS ON
        OR# 1,2; DB=SPFN; OUT           "SET SRA = 15 (TOP)
        LDSPI 2; DB=16.                 "LOOP COUNTER
RDSTK:  REXIT;                          "READ TOP OF STACK
         PN2DBL;                        "AND WRITE TO DPBS
         MI<DB; INCMA                   "28-43. SRS(15)-SRS(0)
        DEC 2                           "DECREMENT LOOP COUNTER
        SETEX RDSTK;                    "SET RETURN ADDRESS TO RDSTK
         BEQ DNMIN                      "BRANCH IF DONE
        RETURN                          "POP STACK
"
"
"
" THE ABOVE LEFT SRA AT 0.
"
"
"
""  MINIMUM SAVE IS DONE.  NOW SET SYSPTR.
"
" IF    SYSAVE < SMA <= SYSTOP
" THEN SET    SYSPTR = SMA + 1
" OTHERWISE, HALT
"
"
"
DNMIN:  RMA; LDSPNL 1                   "READ SMA
        LDMA; DB=SYSTOP                 "GET TOP OF SYSTEM SAVE AREA
        LDSPI 2; DB=SSAREA;            "SYSAVE
         STATMA
        SUB# 2,1;                      "SYSAVE<SMA ?
         STATMA
        BGT .+2;                       "BRANCH IF SO
         LDSPI 2; DB=MD                "SYSTOP
" GOT HERE IF SMA NOT IN SYSTEM SAVE AREA,
" I.E. HAVE CLOBBERED SOME OTHER AREA.
"
OOPS:   DBLLIT; DB=78.;                "SYSTEM DOWN
         HALT
"
"
        SUB# 2,1                       "SMA<=SYSTOP ?
        BGT OOPS;                      "BRANCH IF NOT
         LDMA; DB=SYSPTR               "PRE-SET SMA FOR STORE
        INC 1; STATMA; MI<SPFN         "SET SYSPTR=SMA+1
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "                                                                "
        "                  TEST FOR TYPE OF INTERRUPT                    "
        "                                                                "
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        LDSPI 7; DB=SSAREA             "TASK'S SAVE AREA
        MOVR 0,1                        "CHECK FOR TRAP INT
        BZC .+2                         "SKIP IF NOT
        JMP TRAP
        MOVR 1,1                        "CHECK FOR FATAL INT
        BZC .+2                         "SKIP IF NOT
        JMP FATAL
        MOVR 1,1                        "CHECK FOR FLOATING PT EXCEPTION
        BZC .+2;                        "SKIP IF NOT
         LDMA; DB=LOGITS                "GET FALSE INT COUNTER
        JMP FPE
        MOVR 1,1;                       "CHECK FOR I/O INT
         STATMA
        BZC .+2;                        "SKIP IF NOT
         STATMA
        JMP IO
"GOT HERE IF FALSE INTERRUPT
"
        LDSPI 2; DB=MD                  "FALSE INT COUNTER
        INC 2; MI<SPFN; STATMA          "INCREMENT AND STORE
        JMP INTEXT
"         """""""""""""""""""""""""""""""""""""""""""""""""""""""""
         "                                                       "
         "                   RESTORE MINIMUM STATE               "
         "                                                       "
         """""""""""""""""""""""""""""""""""""""""""""""""""""""""
" WHEN WE ENTER HERE, SMA AND SP 0 HAVE BEEN SET TO THE END OF THE
" MIN SAVE AREA, WHICH ALSO HAPPENED TO INITIATE A READ.
" THIS PRE-FETCHES THE FIRST ELEMENT OF THE SUBROUTINE RETURN
" STACK ( 43. SRS(0) ).
"
" UNFORTUNATELY, WE HAVE TO DO SOME SYSTEM BOOKKEEPING BEFORE
" THE MIN STATE COUNTDOWN.
"
"
"
" IF    SYSAVE+MNSIZE <= SYSPTR <= SYSTOP+1
" THEN SET   SYSPTR = SYSPTR-MNSIZE
" OTHERWISE, HALT
MNREST: LDMA; DB=SYSPTR                "GET SYSPTR
        LDSPI 2; DB=SSAREA+MNSIZE;
         STATMA
        LDMA; DB=SYSTOP                "GET TOP OF SAVE AREA
        LDSPI 1; DB=MD;                "SYSPTR
         STATMA
        SUB# 2,1;                      "SYSAVE+MNSIZE <= SYSPTR ?
         STATMA
        BGE .+2;                       "BRANCH IF SO
         LDSPI 2; DB=MD                "SYSTOP
" GOT HERE IF SYSPTR OUT OF BOUNDS...
" SYSTEM IS MESSED UP.
"
KILL:   DBLLIT; DB=79.;
         HALT
"
"
        INC 2;                         "SYSTOP+1
         LDAPS; DB=ZERO
        SUB# 2,1                       "SYSPTR<=SYSTOP+1 ?
        BGT KILL;                      "BRANCH IF NOT
         LDSPI 2; DB=MNSIZE
        LDMA; DB=SYSPTR                "PRE-SET SMA FOR STORE
        STATMA; SUB 2,1; MI<SPFN       "SET SYSPTR=SYSPTR-MNSIZE
"" NOW START GETTING THE ITEMS TO RESTORE...
"
        MOV 0,0; SETMA                  "43. GET SRS(0)
        LDDA; DB=376                    "SET DA TO APSTAT3
        DB=ZERO; OUT                    "SET SRA=0
        LDSPI 1; DB=15.;                "COUNT=16-1 TO DROP OUT EARLY
         STATMA
        STATMA
LDSTK:  LDTMA; DB=MD;                   "SET TMA TO OLD STACK ADDRESS
         DECMA;                         "42-28. GET SRS(I)
         DEC 1                          "DECREMENT LOOP COUNTER
        STATMA;                         "PUSH MD
         SETEXT;                        "WRITE ON TOP OF STACK
         BEQ DNSRS                      "BRANCH IF DONE LOOP
        STATMA;                         "PUSH MD
         JSR LDSTK                      "PUSH STACK DOWN
" DROPPED OUT OF LOOP ONE TIME EARLY BECAUSE THE LAST JSR
" WOULD HAVE CAUSED THE SRS OVERFLOW INTERRUPT.
"
DNSRS:  DECMA                           "27. GET OLD APSTAT3
        DB=001700; OUT                  "SET SRA=15
        LDTMA; DB=MD;                   "SRS(15)
         DECMA                          "26. GET USER MA
        SETEXT;                         "WRITE SRS(15) ON STACK
         DECMA                          "25. GET FFT STATUS BITS
        DECMA;                          "24. GET TMREG FOR TMA=177776
         DB=MD; OUT                     "RESTORE APSTAT3
        DECMA;                          "23. GET TMA
         DB=MD; LDOMA                   "RESTORE USER MA
        DECMA;                          "22. GET APSTAT2
         DPX<MD                         "TEMP STORE FFT STATUS BITS
        LDDA; DB=5                      "SET DEVICE ADDRESS TO TMRAM
        LDTMA; DB=177776                "GET OLD TM VALUE FROM TMRAM
        DECMA;                          "21. GET SP 7
         OUT; DB=MD                     "RESTORE OLD VALUE TO TMA=177776
        LDDA; DB=377                    "SET DA TO APSTAT2
        DB=DPX; LDAPS                   "RESTORE FFT STATUS BITS
        DECMA;                          "20. GET SP 6
         DB=MD; LDTMA                   "RESTORE TMA
        DECMA;                          "19. GET SP 5
         DB=MD; OUT                     "RESTORE APSTAT2
        DECMA;                          "18. GET SP 4
         DB=MD; LDSPI 7                 "RESTORE SP 7
        DECMA;                          "17. GET SP 3
         DB=MD; LDSPI 6                 "RESTORE SP 6
        DECMA;                          "16. GET SP 2
         DB=MD; LDSPI 5                 "RESTORE SP 5
        DECMA;                          "15. GET SP 1
         DB=MD; LDSPI 4                 "RESTORE SP 4
        DECMA;                          "14. GET SPFN
         DB=MD; LDSPI 3                 "RESTORE SP 3
        DECMA;                          "13. GET SP 0
         DB=MD; LDSPI 2                 "RESTORE SP 2
        DECMA;                          "12. GET SPD
         DB=MD; LDSPI 1                 "RESTORE SP 1
        DECMA;                          "11. GET DA
         DB=MD; LDSPI 0                 "SPFN INTO SP 0
        DECMA;                          "10. GET DPX(3)
         DB=MD; LDSPI 0                 "RESTORE SP 0 AND SPFN
        DECMA;                          "9. GET DPX(2)
         DB=MD; LDSPD                   "RESTORE SPD
        DECMA;                          "8. GET DPX(1)
         DB=MD; LDDA                    "RESTORE DA
        DECMA;                          "7. GET DPX(0)
         DB=MD; DPX(3)<DB               "RESTORE DPX(3)
        DECMA;                          "6. GET APSTATUS
         DB=MD; DPX(2)<DB               "RESTORE DPX(2)
        DECMA;                          "5. GET DPX WRITE BUFFER
         DB=MD; DPX(1)<DB               "RESTORE DPX(1)
        DECMA;                          "4. (PUSH HOLE)
         DB=MD; DPX(0)<DB               "RESTORE DPX(0)
        DECMA;                          "3. GET MD FIFO 1
         DB=MD; LDAPS                   "RESTORE APSTATUS
        DECMA;                          "2. GET MD FIFO 2
         DPX<MD; WDPI                   "RESTORE DPX WRITE BUFFER
        DECMA                           "1. GET MD FIFO 3
        EXINT;                          "RESTORE PREVIOUS STATE
                                        "AND DPBS MANT. BITS 0-1
         RETURN                         "EXIT
$END
"****** OVHNDL = THE OVERLAY HANDLER = REL 1.0  , NOV 79 **********************
"
        $TITLE OVHNDL
        $BOX
        OVHNDL --- ABSTRACT
        THE OVERLAY HANDLER CONSISTS OF TWO PARTS.  THE FIRST BEGINS
        AT THE OVHNDL ENTRY POINT AND IS ONLY ENTERED BY THE HOST
        DURING AN APRUN CALL.  HERE THE OVERLAY NUMBERS ACCUMULATED
        DURING HOST SIDE APOVLD CALLS ARE USED TO LOAD THEIR RESPECTIVE
        OVERLAYS FROM MD INTO PS.  A SUBROUTINE ENTRY POINT IS ALSO
        SPECIFIED AND IS GIVEN CONTROL AFTER THE OVERLAYS ARE LOADED.
        THE SECOND PORTION OF THE OVERLAY HANDLER HAS TWO ENTRY
        POINTS:  APOVLD AND OVLD, FOR APFTN AND APAL RESPECTIVELY.
        THIS ROUTINE IS GIVEN A SINGLE OVERLAY ID NUMBER
        WHICH CORRESPONDS TO AN OVERLAY IN MD WHICH IT WILL LOAD
        INTO PS.
        $ENDBOX
"       THIS RUNS IN USER MODE, WITH USER MA AND INTS ON.
        $PAGE
        $ENTRY OVHNDL
        $ENTRY APOVLD
        $ENTRY OVLD
        $EXT OVLAY1
        $EXT OVLAYM
        $EXT WATLIT
$COMMON /DOT/ DTABLK(33.)  /I
        ENTLOC = DTABLK+0       "ADDR. OF PS ENTRY OF THE CALLED SUB
        OVCNT  = DTABLK+1       "ADDR. OF NUMBER OF OVERLAYS TO LOAD
        OVIDS  = DTABLK+2       "OVERLAY ID NUMBERS START HERE
"SPADS:
        OVID = 10
        $PAGE
        $BOX
        *** OVHNDL - THE HOST ENTRY POINT ***
        HERE WE LOAD ALL THE REQUESTED OVERLAYS THAT HAVE ACCUMULATED
        ON THE HOST SIDE SINCE THE LAST APRUN CALL.  THEN WE TRANSFER
        CONTROL TO THE GIVEN USER SUBROUTINE ENTRY POINT.
        THE CALL:    PLACE IN MD:
        MD(DTABLK  ) = PS ENTRY POINT OF THE CALLED SUBROUTINE
        MD(DTABLK+1) = THE NUMBER OF OVERLAYS TO LOAD
        MD(DTABLK+2) = THE BEGINNING OF THE OVERLAY ID NUMBERS
        $ENDBOX
OVHNDL: TRAP; DB=@OVLAYM; LDTMA        "MAKE ALL REQUESTED SEGS RESIDENT
        LDMA; DB=DTABLK                "GET PS ENTRY
        NOP
        NOP
        LDTMA; DB=MD                   "PS ENTRY POINT
        JSRT                           "GO TO IT
        JSR WATLIT                     "WAIT FOR LAST FPUT DONE
        IOFF
        HALT; DBLLIT; DB=ZERO          "TASK/SUBR IS DONE
        $BOX
        APOVLD --- ABSTRACT ---
        THIS ROUTINE SEARCHES THE OVERLAY MAP (CREATED BY APLOAD) FOR
        THE ENTRY CORRESPONDING TO A REQUESTED OVERLAY ID NUMBER.
        USING INFORMATION IN THE MAP, IT THEN LOADS THE REQUESTED
        OVERLAY INTO PS MEMORY AND THEN RETURNS.  IF THE REQUESTED
        OVERLAY IS NOT FOUND, THE SUBROUTINE SIMPLY RETURNS.
        THE CALL --- FROM APFTN ----
        CALL APOVLD (OVID)
                WHERE OVID IS AN INTEGER SPECIFYING THE OVERLAY ID
                NUMBER.
        THE CALL --- FROM APAL ---
        S-PAD 10 =  SET TO THE OVERLAY ID NUMBER
        JSR OVLD
                OVLD RETURNS WITH ALL S-PADS INTACT, BUT CLOBBERS
                TMA AND DPX(0).
        $ENDBOX
        $BOX
        *** APOVLD - APFTN ENTRY POINT ***
        PICK UP THE REQUESTED OVERLAY ID NUMBER FROM THE .LOCAL DATA
        BLOCK.  THIS WAS SET UP BY APLOAD AND STARTS AT 'DTABLK' IN MD.
        $ENDBOX
APOVLD: LDMA; DB=DTABLK                         "FETCH ADDRESS OF OV ID
        NOP
        NOP
        LDMA; DB=MD                             "FETCH OVERLAY ID NUMBER
        NOP
        NOP
        DPX(0)<MD; BR .+2              "OVERLAY SEGMENT #
        $BOX
        *** OVLD - APAL ENTRY POINT ***
        $ENDBOX
OVLD:   MOV OVID,OVID; DPX(0)<SPFN     "OVERLAY SEGMENT #
        TRAP; DB=@OVLAY1; LDTMA        "MAKE IT RESIDENT
        RETURN
$END
"****** OVLAY = OVERLAY SVC'S = REL 1.0  , NOV 79 *****************************
"
$TITLE OVLAY
$ENTRY OVLAYM, 0
$ENTRY OVLAY1, 0
$ENTRY OVLAY2, 0
$EXT INTEXT
$EXT PSMNGR
"
"
" OVLAYM ENTRY POINT
"
"
" SVC FOR MULTIPLE OVERLAYS FROM HOST REQUEST
"
"
" PROCEDURE:
"  GET OVERLAY SEGMENT NUMBERS FROM AN ARRAY IN MD
"  PUT THERE BY THE HOST.  MARK EACH ONE AS
"  SHOULD-BE-RESIDENT.  AFTER MARKING THE LAST ONE,
"  CALL THE PS MANAGER TO MAKE THEM ALL RESIDENT.
"
"
"
" THIS ASSUMES SUP MODE, INTS OFF, SMA.
"
" PARAMETERS:
"   (IN)  SPAD 7 = ADDR OF TASK'S SAVE AREA
"
" SCRATCH: SP 5-7, DPX 0
"
" ROUTINES CALLED: OVLAY1, OVLAY2
"
"
" MD ALLOCATIONS:
"
$COMMON /DOT/ DTABLK(33.)  /I
"
ENTLOC = DTABLK +0
OVCNT  = DTABLK +1
OVIDS  = DTABLK +2  "ARRAY OF OVERLAY ID'S (SEGMENT #'S)
"
$INSERT MINCOM
"
" S-PADS:
"
PTR = 5
COUNT = 6
" TASK = 7
"
OVLAYM: LDMA; DB=OVCNT                 "COUNT OF ACCUMULATED SEGS
        LDSPI PTR; DB=OVIDS;           "POINTER INTO ARRAY
         LDMA                          "GET FIRST SEG #
        STATMA
        LDSPI COUNT; DB=MD             "COUNT
        DEC COUNT                      "TO DROP OUT OF LOOP 1 EARLY
        BEQ OUT1;                      "SKIP LOOP IF ONLY 1 SEG
         STATMA
" THE SPACING IN THE LOOP INDICATES ITS 2-COLUMN STRUCTURE.
"
LOOPM:            DPX<MD;              "NTH SEG #
                  JSR OVLAY2           "MARK IT SHOULD-BE-RES
        INC PTR; SETMA                 "GET N+1TH SEG #
        STATMA;
                  DEC COUNT
        STATMA;
                  BGT LOOPM            "BRANCH FOR MORE SEGS
" GOT HERE TO DO LAST (OR ONLY) OVERLAY SEG
"
OUT1:   DPX<MD;                        "LAST SEG #
                                       "MARK IT SHOULD-BE-RES
         JMP OVLAY1                    "AND THEN MAKE ALL SEGS RES
 $PAGE
" OVLAY1 AND OVLAY2 ENTRY POINTS
"
"
" HANDLES OVERLAY CALLS FROM APFTN AND APAL TASKS
"
"
" PROCEDURE:
"  FINDS OVERLAY SEGMENT IN TASK'S OVERLAY MAP.
"  MARKS SEG AS SHOULD-BE-RESIDENT.
"  GO TO PS MANAGER TO MAKE IT RESIDENT, UNLESS CAME
"  FROM OVLAYM, IN WHICH CASE JUST RETURN UNTIL HAVE
"  LAST SEG.
"  THEN MAKE ALL MARKED SEGS RESIDENT.
"
"
" THIS ASSUMES SUP MODE, INTS OFF, SMA.
"
" PARAMETERS:
"  (IN)  SPAD 7 = TASK'S SAVE AREA
"  (IN)  DPX(0) = SEGMENT #
"
" SCRATCH:  SP 0-7, DPX 0-3
"
" ROUTINES CALLED: PSMNGR, INTEXT
"
" THIS ASSUMES THE 8-WORD OVERLAY MAP ENTRY, AS SPECIFIED ELSEWHERE.
"
WIDTH = 8.
RES = 6
"
" SPADS:
"
OV = 0
WIDE = 1
SEGID = 2
SEGNUM = 3
SEGCNT = 4
TASK = 7
FLAG = 2
"SPADS 5 AND 6 CAN'T BE USED, AS OVLAYM EXPECTS THEM TO BE INTACT
"AFTER CALLING THIS IN A LOOP.
"
"
"
"
" OVLAY1 IS FOR THE SINGLE (OR LAST) SEGMENT.
" OVLAY2 IS FOR MULTIPLE SEGMENTS FROM OVLAYM.
"
"
OVLAY1: DPX(1)<1; BR .+2               "SET ENTRY FLAG
OVLAY2: DPX(1)<ZERO                    "CLEAR ENTRY FLAG
        LDMA; DB=RDYQUE
        LDSPI OV; DB=OVLPTR;           "TCB OFFSET FOR OVMAP PTR
         STATMA
        STATMA
        LDSPI TASK; DB=MD
        ADD TASK,OV; SETMA             "GET ADDR OF 1ST OVMAP ENTRY
        INCMA                          "GET COUNT OF SEGS IN TASK
        LDSPI SEGNUM; DB=DPX;          "DESIRED SEG #
         STATMA
        DPX(3)<MD;                     "ADDR OF 1ST OVMAP ENTRY
         LDSPI OV                      "(1ST SEG) FOR THIS TASK
        MOV OV,OV; SETMA               "GET 1ST SEG #
        DPX(2)<MD; LDSPI SEGCNT        "SEG COUNT
        LDSPI WIDE; DB=WIDTH;          "TABLE WIDTH
         STATMA
        STATMA
        LDSPI SEGID; DB=MD             "1ST SEG #
" THE SPACING IN THIS LOOP REFLECTS ITS 2-COLUMN STRUCTURE.
"
LOOP:   ADD WIDE,OV; SETMA             "GET N+1TH SEG #
        STATMA;
                  SUB# SEGID,SEGNUM    "COMPARE NTH TO DESIRED SEG #
        STATMA;
                  BEQ MATCH;           "BRANCH IF SAME
                  DEC SEGCNT
        LDSPI SEGID; DB=MD;            "N+1TH SEG #
                  BGT LOOP             "TEST IF CHECKED ALL SEGS
"GOT HERE IF NEVER FOUND THE DESIRED OVERLAY SEGMENT
"
        DBLLIT; DB=77.;
         HALT
"
" GOT HERE IF FOUND THE DESIRED SEGMENT IN THE OVERLAY MAP
"
MATCH:  LDSPI R1; DB=WIDTH-RES+1
        SUB R1,OV; SETMA               "GET SEG'S RES WORD
        LDSPI FLAG; DB=DPX(1);         "ENTRY FLAG
         STATMA
        STATMA; DPX<1
        DPX<MD;                        "RES WORD:
         WRTEXP                        "PREVIOUS EXP, SET LMAN=1
        STATMA; MI<DPX;                "SET SHOULD-BE-RES BIT
         MOV FLAG,FLAG
        BNE .+2                        "CHECK WHICH ENTRY CAME FROM
" GOT HERE IF CAME FROM OVLAY2, I.E. CAME FROM OVLAYM.
" GO BACK FOR MORE SEG #'S TO MARK AS SHOULD-BE-RES.
"
        RETURN
" GOT HERE WHEN ALL DESIRED OVERLAY SEGMENTS ARE MARKED AS
" SHOULD-BE-RESIDENT.
"
                                       "PARAMS FOR PSMNGR:
                                       "DPX(3)=ADDR OF TASK'S 1ST SEG'S
                                       "       ENTRY IN OVERLAY MAP
                                       "DPX(2)=# OF SEGS IN TASK
        JSR PSMNGR                     "MAKE DESIRED SEGS RESIDENT
"NOTE:  WE GO TO INTEXT INSTEAD OF RETURNING
"TO THE TRAP HANDLER, BECAUSE IT WOULD CLOBBER DPX 0-3 AND SP 0
"IN THE TASK'S SAVE AREA.
"
        JMP INTEXT                     "EXIT
"
$NOLIST
$INSERT MINDEF
$LIST
$END
"****** PSMNGR = PS MANAGER = REL 1.0  , NOV 79 *******************************
"
$TITLE PSMNGR
$ENTRY PSMNGR,0
$EXT MOVER
$INSERT MINCOM
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
        STATMA
        LDSPI PPTR; DB=MD;             "ADDR OF 1ST PARTITION ENTRY
         STATMA
        LDSPI PCNT; DB=MD              "COUNT OF PS PARTITIONS
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
        SUB# SEGENT,OLDSEG             "NEW SEG = OLD SEG ?
        BEQ CKDONE;                    "IF SO, GO TO ALT LOOP END
         DPX<ZERO
        DEC PCNT                       "DEC PARTITION COUNTER
        MOV OLDSEG,OLDSEG; SETMA; MI<DPX; "MARK OLD SEG'S RES ENTRY
                                       "AS SHOULDN'T-BE-RES AND ISN'T
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
$INSERT MINDEF
$LIST
"
$END
"****** MOVER = CODE MOVER = REL 1.0  , NOV 79 ********************************
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
"****** TRAP = TRAP HANDLER = REL 1.0  , NOV 79 *******************************
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
"   STORES DPX 0-3, SP 0, AND SPFN BACK INTO SAVE AREA.
"
" NOTE:  THIS WILL (PURPOSELY) CLOBBER THE USER'S
"        DPX 0-3, AND SPAD 0.
"
" THIS ASSUMES SUP. MODE AND INTS. OFF
"
" PARAMETERS:
"   (IN)   SP 7=ADDR OF TASK'S SAVE AREA
"
" SCRATCH: SP 0,1,7,   DPX 0-3
"
" NOTE:  WE GET HERE AFTER AN INTERRUPT OCCURS,
"        THE STATE SAVE HAS BEEN DONE, THE CURRENT
"        TASK'S SAVE AREA IS PUT INTO SPAD 7,
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
$INSERT MINDEF
$LIST
$END
"****** IO = I/O INTERRUPT HANDLER = REL 1.0  , NOV 79 ************************
"
$TITLE IO
$ENTRY IO,0
$EXT PSMNGR
$EXT INTEXT
$INSERT MINCOM
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
"
"
" THIS ASSUMES SUP. MODE, SMA, AND INTS. OFF.
"
" PARAMETERS:
"  (IN)  S-PAD 7=ADDR OF TASK'S SAVE AREA
"
" SCRATCH:  SP 0-4 AND 7, DPX 0-3, DA
"
" ROUTINES CALLED:    PSMNGR, INTEXT
"
" TIMING:
"
"   56 CYCLES PLUS THE ISR, ASSUMING THAT THE ISR IS
"      ALREADY RESIDENT:
"      41 IN IO
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
"
"
IO:     IOINTA; DB=INBS; LDSPI R1      "DEVICE NUMBER
        MOVL R1,R3                     "DEV # *2
        BNE .+2;                       "CHECK FOR NO DEV ANSWERED
         MOVL R3,R3;                   "DEV # *4
         DPX(2)<1
        JMP NONE                       "GO AWAY IF NO DEVICE
        ADD R1,R3;                     "DEV # *5 (INDEX INTO DEV TABLE)
         LDDA; DB=IMASK                "DA=I/O INT MASK
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
$INSERT MINDEF
$LIST
$END
"****** INTEXT = INTERRUPT EXIT ROUTINE = REL 1.0  , NOV 79 *******************
"
$TITLE INTEXT
$ENTRY INTEXT,0
$EXT MNREST
$INSERT MINCOM
"
"
" COMMON INTERRUPT EXIT.
"
"
" PROCEDURE:
"   PRE-SETS SMA FOR MNREST.
"
"
" THIS ASSUMES SUP. MODE, SMA, AND INTS OFF.
"
" PARAMETERS:
"   (OUT) SP0=SYSPTR-1
"
"
" SCRATCH: SP 0
"
" ROUTINES CALLED:  MNREST
"
" TIMING: 6 CYCLES.
"
"
"
"
INTEXT: LDMA; DB=SYSPTR                "GET SYSPTR
        STATMA
        STATMA
        LDSPI 0;  DB=MD                "SYSPTR
        DEC 0; SETMA                   "SET SMA AND SP0 = SYSPTR-1
" RESTORE STATE AND RETURN.
"
        JMP MNREST
$END
"****** FATAL = FATAL/EXCEPTION INTERRUPT HANDLER = REL 1.0  , NOV 79 *********
"
$TITLE FATAL
$ENTRY FATAL,0
$ENTRY FPE,0
$INSERT MINCOM
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
" DURATION, REMEMBER TO SWITCH BACK BEFORE EXITING TO INTEXT
" (IF EXIT IS DESIRED).
"
" PARAMETERS:
"  (IN)  S-PAD 7=ADDR OF TASK'S SAVE AREA
"
"
"
"
"
TCB = 7
"
FPE:    DBLLIT; DB=76.;
         HALT
"
"
FATAL:  DBLLIT; DB=1;
         HALT
"
"
$END
 