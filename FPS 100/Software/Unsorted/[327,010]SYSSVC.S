"****** SAVRST = SAVE/RESTORE SEQUENCE = REL 0.0  , AUG 79 ********************
"
        $TITLE SAVRST
        $ENTRY MNSAVE,0
        $ENTRY MNREST,0
        $ENTRY MXSAVE,0
        $ENTRY MXREST,0
        $EXT TRAP
        $EXT FATAL
        $EXT FPE
        $EXT IO
        $EXT INTEXT
$INSERT COMSYS
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
        "
        "
        " TIMINGS (APPROXIMATE):
        "    MINIMUM SAVE  123-130 CYCLES
        "    MAXIMUM SAVE  145-148 CYCLES (ABOVE MINI)
        "    MINIMUM RESTORE  92-96 CYCLES
        "    MAXIMUM RESTORE  168-177 CYCLES (ABOVE MINI)
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
        "THE REMAINDER OF THE CODE STORES AND RESTORES VARIOUS UNITS
        "SUCH AS THE REMIAINDER OF THE S-PADS, DATA-PADS, FLOATING ADDER, ETC.
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
" IF    SMA = SYSAVE   OR IF    SYSTOP < SMA <= SYSTOP+MNSIZE
" THEN   HALT
" OTHERWISE  SET   SYSPTR = SYSAVE
"
"
"
DNMIN:  RMA; LDSPNL 1                   "READ SMA
        LDMA; DB=SYSTOP                 "GET TOP OF SYSTEM SAVE AREA
        LDMA; DB=SYSAVE                 "GET BEGINNING OF AREA
        LDSPI 4; DB=MNSIZE;             "MIN SAVE AREA SIZE
         STATMA
        LDSPI 2; DB=MD                  "SYSTOP
        SUB# 1,2;                       "SMA <= SYSTOP ?
         STATMA
        BGE OK;                         "BRANCH IF SO
         LDSPI 3; DPX<MD                "SYSAVE
"GOT HERE IF SMA > SYSTOP
"
        ADD 2,4                         "SYSTOP + MNSIZE
        SUB# 4,1                        "SMA <= SYSTOP+MNSIZE ?
        BGT NOTIN;                      "BRANCH IF NOT
         LDMA; DB=SYSPTR                "PRE-SET SMA
"GOT HERE IF CLOBBERED JUST PAST SYSTOP,
"OR CLOBBERED BEGINNING OF SYSAVE.
"
" FATAL SYSTEM ERROR.
" SOLUTION: MAKE SYSTEM SAVE AREA BIGGER (INCREASE SYSIZE).
"
OOPS:   HALT
"GOT HERE IF SMA <= SYSTOP
"
OK:     SUB# 1,3                        "SYSAVE < SMA ?
        BGT NOTIN;                      "BRANCH IF SMA < SYSAVE
         LDMA; DB=SYSPTR                "PRE-SET MA FOR STORE
"GOT HERE IF SYSAVE <= SMA <= SYSTOP
"
        INC 1; DPX<SPFN;                "SET SYSPTR=SMA+1
         BEQ OOPS                       "DIE IF OLD SMA=SYSAVE
"
"
NOTIN:  STATMA; MI<DPX                  "OTHERWISE, SET SYSPTR=SYSAVE
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "                                                                "
        "                  TEST FOR TYPE OF INTERRUPT                    "
        "                                                                "
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        LDMA; DB=CURTSK                 "GET CURTSK TCB ADDR
        STATMA
        STATMA
        LDSPI 7; DB=MD                  "CURTSK
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
         "            SAVE MAX STATE (CONTEXT)                   "
         "                                                       "
         """""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ENTER HERE WITH SMA POINTING TO BEGINNING OF MAX SAVE AREA IN TCB.
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "                                                                  "
        "                           SAVE S PADS                            "
        "                                                                  "
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
MXSAVE: LDSPI 1; DB=8.                  "NUMBER OF SPADS TO BE SAVED
        LDSPI 2; DB=8.                  "STARTING SPAD
        DECMA;                          "TO OFFSET FIRST INCMA IN LOOP
         MOV 2,2; DB=SPFN; LDSPD        "SPD=8
SLOOP:  RSPFN; PN2DBL; DPX<DB           "SP(SPD) TO DPX
        INCMA; MI<DPX;                  "44-51. STORE SP(SPD)
         DEC 1                          "DEC COUNTER
        BGT SLOOP;                      "LOOP IF NOT DONE
         INC 2; DB=SPFN; LDSPD          "SPD=SPD+1
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "                                                               "
        "          SAVE DPYW, DPY(0)-DPY(3), REMAINING DATA PADS        "
        "                                                               "
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        RDPI                            "TO SAVE DPY WRITE BUFFER
        INCMA; MI<DPY                   "52. SAVE DPY WRITE BUFFER
        INCMA; MI<DPY; INCDPA           "53. SAVE DPY(0)
        INCMA; MI<DPY; INCDPA           "54. SAVE DPY(1)
        INCMA; MI<DPY; INCDPA           "55. SAVE DPY(2)
        INCMA; MI<DPY; INCDPA           "56. SAVE DPY(3)
        "WE NEED TO SAVE DATA PADS IN THE OPPOSITE ORDER
        "FROM WHICH THEY ARE READ OUT.  DO THIS BY
        "INCREMENTING DPA ON STORE, AND DECREMENTING DPA
        "ON RESTORE.
        LDSPI 1; DB=28.                 "NUMBER OF DATA PADS TO BE SAVED
NXTDP:  INCMA; MI<DPX;                  "STORE DPX(DPA)
         DEC 1                          "DECREMENT COUNTER
        INCMA; MI<DPY;                  "STORE DPY(DPA)
         INCDPA;                        "INCREMENT DPA
         BGT NXTDP                      "BRANCH IF NOT DONE
        RDPA; PN2DBL; MI<DB; INCMA      "113. SAVE DPA
       """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
       "                                                               "
       "               SAVE ADDER                                      "
       "                                                               "
       """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "IT IS NOW NECESSARY TO SAVE THE STATE OF THE FLOATING ADDER.
        "WHEN AN INTERRUPT IS ACTIVE, THE STATUS REGISTER IS PUSHED BY THE
        "FLOATING ADDER.
        "THE LAST FLOATING ADDER INSTRUCTION BEFORE INTERRUPT IS
        "SAVED IN I/O DEVICE 376.  THE OPCODES FPA1,FPA2
        "PUSH THE FLOATING ADDER ARGUMENTS A1 AND A2 THROUGH THE
        "PIPELINE UNCHANGED.  UPON RESTORE, THE FOLLOWING SEQUENCE
        "MUST BE FOLLOWED:  APSTATUS MUST BE RESET TO PROPER VALUE.
        "                   FA MUST BE RESTORED AND POSSIBLE
        "                   OVERFLOWS AND UNDERFLOWS MUST BE RESTORED.
        "                   THE LAST FLOATING ADDER INSTRUCTION
        "                   BEFORE INTERRUPT MUST BE RECONSTRUCTED
        "                   EXACTLY.  THIS IS DONE BY READING THE
        "                   ADDER OPCODE FROM IOD 376 AND STORING
        "                   THIS OPCODE IN BITS 14-17 OF AN
        "                   INSTRUCTION IN PROGRAM SOURCE WITH
        "                   ADDRESS FAOLD.  WITH OLD A1 AND OLD
        "                   A2 RECALLED INTO DPX AND DPY, THE
        "                   EXECUTION OF THIS CONSTRUCETED INSTRUC-
        "                   TION RESTORES EXACTLY THE STATE OF THE
        "                   FLOATING ADDER.
        FPA2;                           "PUSH OLD A2; PUSH APSTAT TO OLD FA VALU
         DPX<FA                         "SAVE OLD FA
        FPA1;                           "PUSH OLD A1
         BFPE FAERR                     "BRANCH IF FLOATING POINT ERROR IN OLD F
        INCMA;MI<FA;                    "114. SAVE OLD A2
         FADD
        INCMA; MI<FA                    "115. SAVE OLD A1
        INCMA; MI<ZERO                  "116. SAVE ZERO FOR A1
        INCMA; MI<DPX;                  "117. SAVE OLD FA
         BR PASSER                      "SKIP OVERFLOW-UNDERFLOW DETECTION
FAERR:  INCMA; MI<FA;                   "114. SAVE OLD A2
         FADD;
         DB=DPX                         "DPX = OLD FA
        BDBZ UNF;                       "BRANCH IF OLD FA UNDERFLOWED
         INCMA; MI<FA                   "115. SAVE OLD A1
        INCMA; MI<DPX                   "116. SAVE SETUP FOR OVERFLOW
        INCMA; MI<DPX;                  "117. SAVE SETUP FOR OVERFLOW
         BR PASSER
UNF:    RPSF UNFC; DPX<DB               "GET CONSTANT TO GENERATE UNDERFLOW
        INCMA; MI<DPX                   "116. SAVE SETUP FOR UNDERFLOW
        INCMA; MI<ZERO                  "117. SAVE SETUP FOR UNDERFLOW
PASSER: RMA; LDSPNL 1                   "CURRENT SMA
        LDSPI 2; DB=27.-117.            "BIAS TO APSTAT3 *****
        ADD 1,2; SETMA                  "GET APSTAT3
        MOV 1,1; SETMA                  "RESTORE CURRENT SMA
        LDSPI 3; DB=77;                 "MASK FOR BITS 10-15 OF APSTAT3
         STATMA
        LDSPI 1; DB=MD                  "APSTAT3
        MOV 1,2
        ANDR 3,2                        "GET FADD/FADD1 FIELD
        MOVRR 2,2                       "SHIFT RIGHT THREE PLACES
        BEQ FADD1                       "BRANCH IF ONE ARG FADD FUNCTION
        MOV 2,2; DPX<SPFN               "FADD OP TO DPX
        RPSF FADARG; DPY<DB             "GET ARGUMENTS FOR FADD OP
        LDSPI 2; DB=33;                 "EXPONENT TO FLOAT FADD OP
         BR SETFOP                      "SET FADD OPERATION
FADD1:  AND 1,3; DPX<SPFN               "FADD1 OP TO DPXP
        RPSF FAD1ARG; DPY<DB            "GET FADD1 ARG
        LDSPI 2; DB=30                  "EXPONENT TO FLOAT FADD1 OP
SETFOP: FADD ZERO,MDPX; MOV 2,2         "FLOAT FADD OP
        FADD;
         DPX<ZERO                       "CLEAR DPX FOR WRTEX
        FADD DPY,FA                     "FADD OP + FADD ARGUMENTS
        FADD; LDSPI 2; DB=15            "EXPONENT TO SCALE FADD INSTRUCTION
        FSCLT FA; MOV 2,2               "SHIFT FADD INSTRUCTION RIGHT
        FADD;
         DPX<4; WRTEXP
        DPX<FA; WRTMAN                  "FADD INSTRUCTION TO DPX
        INCMA; MI<DPX;                  "118. SAVE OLD FADD INSTR.
         BR SAVEFM                      "BRANCH OVER CONSTANTS
FADARG:      $VAL 0,37,172400,0         "A1 = DPX, A2 = MD
FAD1ARG:     $VAL 0,37,170400,0         "A2 = MD
UNFC:        $VAL 0,0,0,1               "CONSTANT TO FORCE UNDERFLOW
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "                                                             "
        "                   SAVE MULTIPLIER                           "
        "                                                             "
        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "THE FOLLOWING IS CODE TO SAVE THE STATE OF THE FLOATING
        "MULTIPLIER.  THE POSSIBILITY OF A FLOATING POINT ERROR
        "MUST BE DETECTED.  IF THERE IS NO FLOATING POINT ERROR,
        "THE STATE WILL BE RESTORED BY FMUL 1.0,FM.  IF THERE
        "IS A FLOATING POINT ERROR, THE STATE IS RESTORED BY
        "FMUL FM,FM, WHICH WILL GENERATE THE APPROPRIATE
        "OVERFLOW OR UNDERFLOW.  THE NUMBERS COME OUT OF
        "THE MULTIPLIER IN THE SEQUENCE IN WHICH THEY MUST
        "BE RESTORED, SO STORAGE IN MEMORY IS INVERTED SO
        "THAT THE NUMBERS ARE IN MEMORY IN THE PROPER SEQUENCE
        "FOR RESTORE.
        "NOTE THAT UNDERFLOW IS NOT PROPERLY GENERATED IN THE FOLLOWING
        "CODE.  IF UNDERFLOW IS DETECTED, THE PROPER RESTORE IS
        "FMUL UNFC, UNFC WHERE UNFC IS THE UNDERFLOW CONSTANT  ABOVE.
SAVEFM: RMA; LDSPNL 1                   "READ SMA
        LDSPI 2; DB=124.-118.           "BIAS FOR FM STORAGE  *****
        LDAPS; DB=ZERO                  "CLEAR STAT1 FOR F.P.E. TEST
        LDTMA; DB=ZERO                  "GET 1.0 FOR MULTIPLIER
        DPX<FM;                         "SAVE FM0
         ADD 1,2; SETMA;MI<FM;          "124. SAVE FM0
         FMUL;                          "PUSH STAT FM0, GET FM1
         INCTMA                         "PUSH TM (SINCE THERE IS NO STATTMA)
        BFPE SETFM0;                    "BRANCH IF F.P.E. IN FM0
         LDAPS; DB=ZERO                 "CLEAR APSTAT1 FOR FM1 TEST
        DPX<TM                          "SAVE 1.0 FOR MULTIPLIER OF FM0
SETFM0: DECMA; MI<DPX                   "123. SAVE MULTIPLIER FOR FM0
        DPX<FM;                         "SAVE FM1
         DECMA; MI<FM;                  "122. SAVE FM1
         FMUL                           "PUSH STAT FM1, GET FM2
        BFPE SETFM1;                    "BRANCH IF F.P.E. IN FM1
         LDAPS; DB=ZERO                 "CLEAR APSTAT1 FOR FM2 TEST
        DPX<TM                          "SAVE 1.0 FOR MULTIPLIER OF FM1
SETFM1: DECMA; MI<DPX                   "121. SAVE MULTIPLIER FOR FM1
        LDAPS; DB=ZERO                  "CLEAR STAT FOR FM2 TEST
        DPX<FM;                         "SAVE FM2
         DECMA; MI<FM;                  "120. SAVE FM2
         FMUL                           "PUSH STAT FM2
        BFPE SETFM2                     "BRANCH IF F.P.E. IN FM2
        DPX<TM                          "SAVE 1.0 FOR MULTIPLIER OF FM2
SETFM2: DECMA; MI<DPX                   "119. SAVE MULTIPLIER OF FM2
        MOV 2,2; SETMA                  "RESTORE SMA
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "                                                              "
        "                 SAVE FLAGS                                   "
        "                                                              "
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
TSTF0:  BFL0 STF0                       "BRANCH IF FLAG0 = 1
        INCMA; MI<ZERO;                 "125. SET FLAG0 = 0
         BR TSTF1;
         CFL0                           "CLEAR FLAG0
STF0:   INCMA; MI<TM;                   "125. SET FLAG0 = 1
         CFL0                           "CLEAR FLAG0
TSTF1:  BFL1 STF1                       "BRANCH IF FLAG1 = 1
        INCMA; MI<ZERO;                 "126. SET FLAG1 = ZERO
         CFL1;                          "CLEAR FLAG1
         BR TSTF2                       "BRANCH TO TEST FLAG2
STF1:   INCMA; MI<TM;                   "126. SET FLAG1 = 1
         CFL1                           "CLEAR FLAG1
TSTF2:  BFL2 STF2                       "BRANCH IF FLAG2 = 1
        INCMA; MI<ZERO;                 "127. SET FLAG2 = ZERO
         CFL2;                          "CLEAR FLAG2
         BR TSTF3                       "BRANCH TO TEST FLAG3
STF2:   INCMA; MI<TM;                   "127. SET FLAG2 = 1
         CFL2                           "CLEAR FLAG2
TSTF3:  BFL3 STF3                       "BRANCH IF FLAG3 = 1
        INCMA; MI<ZERO;                 "128. SET FLAG3 = ZERO
         CFL3;                          "CLEAR FLAG3
         BR EXIT                        "BRANCH TO END OF FLAG TESTS
STF3:   INCMA; MI<TM;                   "128. SET FLAG3 = 1
         CFL3                           "CLEAR FLAG 3
EXIT:   RETURN
"      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "                                                                 "
      "               RESTORE MAX STATE (CONTEXT)                       "
      "                                                                 "
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ENTER HERE WITH SMA POINTING TO END OF MAX SAVE AREA.
" THIS INITIATED A READ, WHICH PRE-FETCHED    128. OLD FLAG 3
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "                                                                "
        "                   RESTORE FLAGS                                "
        "                                                                "
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
MXREST: CFL3                            "CLEAR FLAG3
        DECMA;                          "127. GET OLD FLAG2
         CFL2                           "CLEAR FLAG2
        DECMA;                          "126. GET OLD FLAG1
         CFL1                           "CLEAR FLAG1
        DECMA;                          "125. GET OLD FLAG0
         CFL0;                          "CLEAR FLAG0
         DPX<MD                         "OLD FLAG3 TO DPBS
        BDBZ SET2;                      "BRANCH IF OLD FLAG3 = 0
         STATMA                         "PUSH OLD FLAG1
        DPX<MD                          "OLD FLAG2 TO DPBS
        DPX<DPX;                        "OLD FLAG2 TO DPBS
         SFL3                           "SET FLAG3
SET2:   BDBZ SET1;                      "BRANCH IF OLD FLAG2 = 0
         STATMA;                        "PUSH OLD FLAG0
         DPX<MD                         "OLD FLAG1 TO DPBS
        DPX<DPX;                        "OLD FLAG1 TO DPBS
         SFL2                           "SET FLAG2
SET1:   BDBZ SET0;                      "BRANCH IF OLD FLAG1 = 0
         DPX<MD                         "OLD FLAG0 TO DPBS
        DPX<DPX;                        "OLD FLAG0 TO DPBS
         SFL1                           "SET FLAG1
SET0:   BDBZ RSTFM                      "BRANCH IF OLD FLAG0 = 0
        SFL0                            "SET FLAG0
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "                                                                "
        "                    RESTORE MULTIPLIER                          "
        "                                                                "
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
RSTFM:  DECMA;                          "124. GET FM01
         LDAPS; DB=ZERO
        DECMA                           "123. GET FM02
        DECMA                           "122. GET FM11
        DECMA;                          "121. GET FM12
         DPX<MD                         "SAVE FM01
        DECMA;                          "120. GET FM21
         FMUL DPX,MD                    "GENERATE FM0
        DECMA;                          "119. GET FM22
         DPX<MD                         "SAVE FM11
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "                                                                  "
        "                     RESTORE ADDER                                "
        "                                                                  "
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
RSTFA:  DECMA;                          "118. GET OLD FADD INSTR
                FMUL DPX,MD             "GENERATE FM1
        DECMA;                          "117. GET OLD FA
                DPX<MD                  "SAVE FM21
        DECMA;                          "116. GET A1=ZERO
                FMUL DPX,MD             "GENERATE FM2
        STATMA;
         LPSL FAOLD; DB=MD              "OLD ADDER INSTR INTO FAOLD
        DPX<MD;                         "TEMP STORE FOR OLD FA
         LDSPE 0;                       "GET EXP OF OLD FA.
         FADD ZERO,MD                   "NORMALIZE OLD FA
        STATMA
        DPY<MD;                         "TEMP STORE FOR ZERO
         BDBZ SCLFA;                    "BRANCH IF FA POS AND UNNORMALIZED
         FADD
        DPX(1)<FA                       "TEMP STORE FOR NORMALIZED FA
        LDSPE 1; DB=DPX(1)              "GET EXP OF NORMALIZED FA
        SUB 0,1                         "COMPARE NORMALIZED AND UNNORMALIZED
        BNE SCLFA                       "BRANCH IF FA NEG AND UNNORMALIZED
        FADD DPX,DPY;                   "GENERATE OVERFLOW
         BR ENTA
        "UNNORMALIZE OLD FA
SCLFA:  INC 0; FSCLT FA                 "UNNORMALIZE OLD FA
ENTA:   DECMA                           "115. GET OLD A1
        DECMA                           "114. GET OLD A2
        STATMA                          "PUSH MD
        STATMA;                         "PUSH MD
         DPX<MD;                        "TEMP STORE FOR OLD A1
         LDSPE 0                        "GET SPFN FOR ADDER OP
        INC 0                           "FSCLT EXP IS ONE TOO LOW
                                        "BEFORE NEXT INSTRUCTION IS
                                        "EXECUTED, OLD SPFN MUST BE
                                        "RESTORED TO SPAD 0
FAOLD:  FADD DPX,MD; MOV 0,0            "GENERATE OLD FLOATING ADDER STATE
       """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
       "                                                               "
       "         RESTORE DATA PADS, DPY(0)-DPY(3), DPYW                "
       "                                                               "
       """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
DNLD:   LDSPI 1; DB=28.                 "NUMBER OF DATA PADS
        DECMA                           "113. GET DPA
        DECMA                           "112. GET DPY(DPA+31)
        DECMA                           "111. GET DPX(DPA+31)
        LDDPA; DB=MD                    "SET DPA
        DECDPA;                         "DPA-1=DPA+31
         STATMA
NXRST:  DPY<MD;                         "RESTORE DPY(I)
         DECMA                          "GET DPY(I-1)
        DPX<MD;                         "RESTORE DPX(I)
         DECMA;                         "GET DPX(I-1)
         DEC 1
        STATMA;                         "PUSH MD
         BGT NXRST;                     "BRANCH IF NOT DONE
         DECDPA
        DPY<MD;                         "RESTORE DPY(3)
         DECMA;                         "54. GET DPY(1)
         DECDPA
        DPY<MD;                         "RESTORE DPY(2)
         DECMA;                         "53. GET DPY(0)
         DECDPA
        DECMA                           "52. GET DPYW
        DPY<MD;                         "RESTORE DPY(1)
         STATMA;
         DECDPA
        DPY<MD;                         "RESTORE DPY(0)
         STATMA
        WDPI; DPY<MD                    "RESTORE DPYW
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "                                                              "
        "                 RESTORE S PADS                               "
        "                                                              "
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        LDSPI 2; DB=15.+1               "STARTING SPAD
        LDSPI 1; DB=8.+1                "NUMBER OF SPADS
RLOOP:  DECMA;                          "51-44. GET CONTENTS OF NEXT S PAD
         DEC 1                          "DECREMENT COUNTER
        BEQ DNMAX;                      "BRANCH IF DONE
         STATMA
        STATMA                          "PUSH MD
        LDSPD; DEC 2; DB=SPFN           "DECREMENT SPD
        LDSPI 3; DB=MD;                 "LOAD SP(SPD)
         BR RLOOP
DNMAX:  RETURN
"        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "                                                              "
        "                    RESTORE MINIMUM STATE                     "
        "                                                              "
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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
" IF    SYSAVE < SYSPTR <= SYSTOP+1
" THEN SET   SYSPTR = SYSPTR-MNSIZE
" OTHERWISE SET  SYSPTR = SYSAVE
MNREST: LDMA; DB=SYSPTR;                "GET SYSPTR
         LDSPI 5                        "SAVE ADDR
        LDMA; DB=SYSAVE                 "GET SYSAVE
        LDMA; DB=SYSTOP                 "GET SYSTOP
        LDSPI 1; DB=MD;                 "SYSPTR
         STATMA
        LDSPI 2; DPX<MD                 "SYSAVE
        SUB# 1,2;                       "SYSAVE<SYSPTR ?
         STATMA
        BGE SET;                        "BRANCH IF NOT
         LDSPI 3; DB=MD                 "SYSTOP
        INC 3                           "SYSTOP+1
        SUB# 3,1                        "SYSPTR<=SYSTOP+1 ?
        BGT SET;                        "BRANCH IF NOT
         LDSPI 4; DB=MNSIZE             "MIN SAVE SIZE
"GOT HERE IF SYSPTR WAS WITHIN SYSTEM AREA
"
        SUB 4,1; DPX<SPFN               "SYSPTR-MNSIZE
"GOT HERE TO SET SYSPTR
"
SET:    MOV 5,5; SETMA; MI<DPX          "SET SYSPTR
        LDAPS; DB=ZERO
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
"****** WAIT = WAIT FOR MESSAGE/ANSWER = REL 0.0  , AUG 79 ********************
"
$TITLE WAIT
$ENTRY WAIT,0
$ENTRY TWAIT,0
$ENTRY WAITA,0
$ENTRY TWAITA,0
$EXT DELETE
$EXT DEQ
$EXT EMPTY
$EXT RECEIV
$EXT INSERT
$EXT RESUME
$EXT ADDCLK
$INSERT COMSYS
"
"
" WAITS FOR MESSAGE OR ANSWER.
"
" PROCEDURE:
"   IF WAITING FOR ANSWER, GET EXCHANGE FROM CURRENT TASK'S
"   TCB:  ANSKEY.
"   DELETE CURRENT TASK FROM READY QUEUE AND CLEAR ITS READY BIT.
"   SEE IF THERE IS A MESSAGE WAITING AT THE EXCHANGE.
"   IF SO, DEQUEUE IT.  IF THE EXCHANGE'S QUEUE IS NOW
"   EMPTY, MARK EXCHANGE TYPE.  RECEIVE MESSAGE.
"   IF THERE WAS NO MESSAGE WAITING AT THE EXCHANGE,
"   SEE IF THIS IS A TIMED WAIT.
"   IF NOT, QUEUE THE TASK.
"   IF SO, SEE IF TIME LIMIT = 0.
"   IF SO, RETURN TIMEOUT MESSAGE AND RESUME THE TASK.
"   OTHERWISE, ADD TO CLOCK QUEUE, AND QUEUE THE TASK
"   AT THE EXCHANGE.
"
" NOTE:  ONE CANNOT JSR TO WAIT  OR WAITA.
"        ONE CAN JSR TO TWAIT OR TWAITA ONLY WITH A TIME LIMIT=0.
"        OTHERWISE MUST TRAP TO WAIT, TWAIT, WAITA, TWAITA.
"
"
" THIS ASSUMES SUP. MODE AND INTS. OFF.
"
" PARAMETERS:
"  (IN)  DPX(0)=TIME LIMIT  (TWAIT OR TWAITA)
"  (IN)  DPX(1)=EXCH ADDR   (TWAIT OR WAIT)
"  (OUT) DPX(0)=MSG ADDR
"  (OUT) SP0 AND SPFN=MSG TYPE
"
" SCRATCH: SP 0-6, DPX 0-3
"
" ROUTINES CALLED: DELETE, DEQ, EMPTY, RECEIV, INSERT, RESUME, ADDCLK
"
" TIMING:
" FOR WAITA OR TWAITA, ADD 4 CYCLES TO WAIT OR TWAIT BELOW.
"
" IF THERE WAS A MESSAGE AT THE EXCHANGE:
" 110-114 CYCLES + 4/ELE SEARCHED IN RDYQUE.
"    22-23 IN WAIT OR TWAIT
"    7 IN DELETE
"    10 IN DEQ
"    5 IN EMPTY
"    66-69 + 4/ELE IN RECEIV
" IF THERE WAS NO MSG AND NO TIME LIMIT:
" 39 CYCLES.
"    21 IN WAIT
"    7 IN DELETE
"    11 IN INSERT
" IF THERE WAS NO MSG, WITH TIME LIMIT = 0:
" 59 CYCLES + 4/ELE SEARCHED IN RDYQUE.
"    22 IN TWAIT
"    7 IN DELETE
"    30 + 4/ELE IN RESUME
" IF THERE WAS NO MSG, WITH NON-ZERO TIME LIMIT:
" 43+? CYCLES.
"    25 IN TWAIT
"    7 IN DELETE
"    11 IN INSERT
"    ? IN ADDCLK
"
"
FLAG = 0
EXCH = 1
EXTYPE = 4
TCB = 5
MSG = 6
"
"
TWAITA: LDSPI FLAG;
         LDMA; DB=CURTSK;              "GET TCB OF CURRENT TASK
         BR .+2
WAITA:  CLR FLAG;
         LDMA; DB=CURTSK               "GET TCB OF CURRENT TASK
        LDSPI R1; DB=ANSKEY;           "ANSWER EXCH OFFSET IN TCB
         STATMA
        LDSPI R4; DB=STATUS;           "TCB STATUS OFFSET
         STATMA
        LDSPI TCB; DPX(3)<MD           "TCB ADDR
        ADD TCB,R1; SETMA              "GET ANSWER EXCH ADDR
        LDSPI R6; DB=RDYOFF;           "MASK WITH RDYBIT OFF
         STATMA
        STATMA
        DPX(1)<MD;                     "EXCH ADDR
         BR JOIN
TWAIT:  LDSPI FLAG;
         LDMA; DB=CURTSK;              "GET TCB OF CURRENT TASK
         BR .+2
WAIT:   CLR FLAG;
         LDMA; DB=CURTSK               "GET TCB OF CURRENT TASK
        LDSPI R4; DB=STATUS;           "TCB STATUS OFFSET
         STATMA
        LDSPI R6; DB=RDYOFF;           "MASK WITH RDYBIT OFF
         STATMA
        LDSPI TCB; DPX(3)<MD           "TCB ADDR
                                       "PARAM TO DELETE:
                                       "DPX(3)=ELE TO BE DELETED
JOIN:   JSR DELETE                     "DELETE TCB FROM RDYQUE
        ADD TCB,R4; SETMA              "GET TCB STATUS
        LDSPI EXCH; DPX(2)<DPX(1);     "EXCH ADDR
         STATMA
        LDSPI EXTYPE; DB=TYPE;         "TYPE OFFSET
         STATMA
        LDSPI R3; DB=MD                "TCB STATUS
        AND# R3,R6; STATMA; MI<SPFN    "TURN READY BIT OFF
        ADD EXCH,EXTYPE; SETMA;        "GET EXCH TYPE
         DPX(1)<DPX(3)                 "TCB ADDR
        LDSPI R3; DB=MSGBIT;
         STATMA
        LDSPI R1; DB=DPX;              "TIME LIMIT
         STATMA
        LDSPI R2; DB=MD                "EXCH TYPE
        AND# R3,R2                     "CHECK MSGBIT IN EXCH TYPE
        BEQ NOMSG;                     "BRANCH IF NO MSG AT EXCH
         MOV FLAG,FLAG                 "SET SPFN (FOR NOMSG)
"GOT HERE TO DEQUEUE MSG
"
                                       "PARAM TO DEQ:
                                       "DPX(2)=Q HEADER=EXCH ADDR
        JSR DEQ                        "DEQUEUE MSG FROM EXCH
        DPX<DPX(3);                    "DEQ RETURNS MSG ADDR
                                       "IN DPX(3)
         LDSPI MSG                     "SAVE IT
                                       "PARAM TO EMPTY:
                                       "DPX(2)=Q HEADER=EXCH ADDR
        JSR EMPTY                      "SEE IF Q EMPTY NOW
        BNE NEMPTY                     "BRANCH IF Q NOT EMPTY
        MOV EXTYPE,EXTYPE; SETMA; MI<ZERO "OTHERWISE, SET EXCH=EMPTY
                                       "PARAMS TO RECEIV:
                                       "DPX(0)=MSG ADDR
                                       "DPX(1)=TCB ADDR
NEMPTY: JSR RECEIV                     "RECEIVE MSG
        RETURN
"
" GOT HERE IF NO MSG'S QUEUED
"
NOMSG:  BEQ QTASK;                     "BRANCH IF NOT TIMED WAIT
         MOV R1,R1                     "SPFN=TIME LIMIT
"GOT HERE IF THIS WAS TIMED WAIT
"
TIMED:  BNE NOT0;                      "BRANCH IF TIMLIM NOT=0
         LDSPI R6; DB=DPX(2)           "SAVE EXCH ADDR
"GOT HERE IF TIMLIM=0...RETURN TIMEOUT MSG
"
                                       "PARAM FOR RESUME:
                                       "DPX(1)=TCB ADDR
        JSR RESUME
        DPX(0)<TIMOUT                  "ADDR OF TIMOUT MSG
        LDSPI 0; DB=TIMBIT             "SP0=MSG TYPE
        MOV 0,0;                       "SAME WITH SPFN
         RETURN
"GOT HERE IF TIMLIM NOT=0
"
                                       "PARAMS TO ADDCLK:
                                       "DPX(0)=TIME LIMIT
                                       "DPX(1)=TCB ADDR
NOT0:   JSR ADDCLK                     "ADD TCB TO CLOCK Q
        MOV TCB,TCB; DPX(3)<SPFN       "RESTORE TCB ADDR
        MOV R6,R6; DPX(2)<SPFN         "RESTORE EXCH ADDR
"GOT HERE TO QUEUE THE TASK AT THE EXCHANGE
"
"IF WE GOT HERE, IT COULDN'T HAVE BEEN BY JSR, ONLY BY TRAP
"
QTASK:  DPX<TSKBIT
        MOV EXTYPE,EXTYPE; SETMA; MI<DPX "EXCH TYPE=TASKS Q'ED
                                       "PARAMS TO INSERT:
                                       "DPX(2)=NEXT=Q HEAD=EXCH ADDR
                                       "DPX(3)=NEW=TCB ADDR
        JSR INSERT                     "INSERT TASK INTO EXCH Q
        RETURN
$NOLIST
$INSERT SYSDEF
$LIST
$END
"****** ANSWER = SEND ANSWER = REL 0.0  , AUG 79 ******************************
"
$TITLE ANSWER
$ENTRY ANSWER,0
$ENTRY MSGANS,0
$EXT SENDA
$INSERT COMSYS
"
"
" SENDS ANSWER
"
" PROCEDURE:
"   'ANSWER' ANSWERS THE LAST MSG RECEIVED BY THE CURRENT
"   TASK.  IF 0, THERE WAS NO LAST MSG RECEIVED...ERROR.
"   'MSGANS' HAS ADDR OF MSG AS A PARAMETER.
"
"   MAKE SURE MSG IS NOT LINKED ELSEWHERE.
"   GET ADDR OF ANS EXCH FROM MSG.
"   SET ANSWER BIT IN MSG TYPE.
"   SEND ANSWER.
"
"
" THIS ASSUMES SUP. MODE AND INTS. OFF.
"
"
" PARAMETERS:
"  (MSGANS ONLY)
"  (IN)  DPX(0)=MSG ADDR
"
" SCRATCH: SP 0-4, DPX 0-2
"
" ROUTINES CALLED: SENDA
"
" TIMING:
" IF ANSWER IS SIMPLY QUEUED AT EXCHANGE:
" 41 CYCLES FOR ANSWER.
"    19 IN ANSWER
"    22 IN SENDA
" 35 CYCLES FOR MSGANS.
"    13 IN MSGANS
"    22 IN SENDA
" IF ANSWER IS RECEIVED BY WAITING TASK:
" 113-117 CYCLES + 4/ELE SEARCHED IN RDYQUE, FOR ANSWER.
"    19 IN ANSWER
"    94-98 + 4/ELE IN SENDA
" 107-111 CYCLES + 4/ELE SEARCHED IN RDYQUE, FOR MSGANS.
"    13 IN MSGANS
"    94-98 + 4/ELE IN SENDA
"
"
MTYPE = 0
MSG = 2
TCB = 3
"
"
ANSWER: LDMA; DB=CURTSK                "GET CURRENT TASK'S TCB ADDR
        LDSPI R1; DB=LSTMSG;           "LAST-MSG-REC'D OFFSET
         STATMA
        LDSPI MTYPE; DB=TYPE;          "TYPE OFFSET
         STATMA
        LDSPI TCB; DB=MD               "CURTSK'S TCB
        ADD TCB,R1; SETMA              "GET TCB'S LAST-MSG-REC'D
        LDSPI R4; DB=ANSBIT;           "MSG TYPE ANSWER BIT
         STATMA
        STATMA
        DPX<MD; LDSPI MSG;             "ADDR OF LAST-MSG-REC'D
         LDMA                          "GET RLINK(MSG)
        MOV MSG,MSG;                   "SET SPFN
         STATMA
        BNE JOIN;                      "BRANCH IF THERE WAS A LSTMSG
         ADD MSG,MTYPE; SETMA          "GET MSG TYPE
        JMP ERR1                       "IF NO LAST MSG...ERROR
MSGANS: LDSPI MSG; DB=DPX;             "MSG ADDR
         LDMA                          "GET RLINK(MSG)
        LDSPI MTYPE; DB=TYPE;          "TYPE OFFSET
         STATMA
        LDSPI R4; DB=ANSBIT            "MSG TYPE ANSWER BIT
        ADD MSG,MTYPE; SETMA           "GET MSG TYPE
JOIN:   LDSPI R3; DB=MD                "RLINK(MSG)
        SUB MSG,R3                     "RLINK(MSG)=MSG? (UNLINKED)
        BNE ERR2;                      "BRANCH IF MSG LINKED ELSEWHERE
         LDSPI R1; DB=ANSKEY;          "ANS EXCH OFFSET
         STATMA
        ADD MSG,R1; SETMA              "GET MSG'S ANS EXCH
        LDSPI R1; DB=MD;               "MSG TYPE
         STATMA
        OR R1,R4; DPX(2)<SPFN          "SET MSG'S ANSBIT
        MOV MTYPE,MTYPE; SETMA; MI<DPX(2) "STORE BACK
                                       "PARAMS TO SENDA:
                                       "DPX(0)=MSG ADDR
        DPX(1)<MD;                     "DPX(1)=EXCH ADDR
         JSR SENDA                     "SEND ANSWER
        RETURN
" GOT HERE IF NO LAST MSG RECEIVED.
"
ERR1:   LDSPI 0; DB=ERRMSG; BR OUT     "RETURN ERROR CODE
" GOT HERE IF MSG IS LINKED ELSEWHERE
"
ERR2:   LDSPI 0; DB=ERRBSY             "RETURN ERROR CODE
OUT:    MOV 0,0; RETURN
"
$NOLIST
$INSERT SYSDEF
$LIST
$END
"****** SEND = SEND MESSAGE = REL 0.0  , AUG 79 *******************************
"
$TITLE SEND
$ENTRY SEND,0
$ENTRY SENDA,0
$EXT INSERT
$EXT DEQ
$EXT EMPTY
$EXT RECEIV
$INSERT COMSYS
"
"
" SENDS MESSAGE (OR ANSWER) TO EXCHANGE.
" IF A TASK IS WAITING, IT RECEIVES THE MESSAGE.
"
"
" SEND MESSAGE PROCEDURE:
"   IF MSG LINKED ELSEWHERE, ERROR.
"   OTHERWISE:
"   SET MSG'S BUSY BIT.
"   COPY CURRENT TASK'S PRIORITY INTO MSG PRIORITY.
"   COPY MSG'S ANSWER EXCH INTO CURRENT TASK'S TCB.
"   FOLLOW SEND ANSWER PROCEDURE (BELOW).
" SEND ANSWER PROCEDURE:
"   CHECK EXCH'S TSKBIT.
"   IF 0, NO TASKS ARE QUEUED AT EXCH,
"   THEREFORE QUEUE THE MSG AT THE EXCH,
"   AND SET EXCH'S MSGBIT.
"   IF TSKBIT IS SET,   TASKS ARE QUEUED AT EXCH,
"   THEREFORE DEQUEUE A TASK.  IF EXCH IS NOW EMPTY,
"   CLEAR EXCH'S MSGBIT AND TSKBIT.  CAUSE THE MSG
"   TO BE REC'D BY THE TASK.
"
"
" THIS ASSUMES SUP. MODE AND INTS. OFF.
"
"
" PARAMETERS:
"   (IN)  DPX(0)=MSG ADDR
"   (IN)  DPX(1)=EXCH ADDR
"
" SCRATCH: SP 0-4, DPX 0-3
"
" ROUTINES CALLED: INSERT, DEQ, EMPTY, RECEIV
"
" TIMING:
" IF MSG IS SIMPLY QUEUED:
" 37 CYCLES FOR SEND.
"     26 IN SEND
"     11 IN INSERT
" 22 CYCLES FOR SENDA.
"     11 IN SENDA
"     11 IN INSERT
" IF TASK WAS WAITING FOR MSG:
" 109-113 CYCLES + 4 CYCLES/ELEMENT SEARCHED IN READY QUEUE, FOR SEND.
"     28-29 IN SEND
"     10 IN DEQ
"     5 IN EMPTY
"     66-69 + 4/ELE IN RECEIV
" 94-98 CYLES + 4 CYCLES/ELEMENT SEARCHED IN READY QUEUE, FOR SENDA.
"     13-14 IN SENDA
"     10 IN DEQ
"     5 IN EMPTY
"     66-69 + 4/ELE IN RECEIV
"
"
" NOTES ON EXCH TYPE:
"    MSGBIT TSKBIT
"      1      1       ILLEGAL
"      1      0       MESSAGES QUEUED AT EXCH
"      0      1       TASKS QUEUED AT EXCH
"      0      0       NOTHING QUEUED AT EXCH
"
"
PRI = 0
EXCH = 1
TCB = 2
MSG = 3
EXTYPE = 4
"
"
SEND:   LDMA; DB=DPX;                  "GET RLINK(MSG)
         LDSPI MSG                     "MSG ADDR
        LDMA; DB=CURTSK                "GET TCB ADDR
        LDSPI R1; DB=TYPE;             "TYPE OFFSET
         STATMA
        LDSPI R0; DB=MD                "RLINK(MSG)
        SUB# MSG,R0;                   "COMPARE MSG TO RLINK(MSG)
         DPX(2)<BSYBIT;
         STATMA
        BEQ .+2;                       "IF MSG IS UNLINKED, OK.
         LDSPI TCB; DB=MD              "TCB ADDR
        JMP ERR                        "OTHERWISE MSG QUEUED ELSEWHERE
        ADD MSG,R1; SETMA              "GET MSG TYPE
        LDSPI PRI; DB=RPRI;            "PRIORITY OFFSET
         STATMA
        ADD# TCB,PRI; SETMA            "GET TCB PRI
        MOV R1,R1; SETMA; MI<MD        "PRIME THE DPBS BUFFER
        STATMA; MI<DPX(2); WRTEXP      "SET MSG'S BUSY BIT
        ADD MSG,PRI; SETMA; MI<MD      "TASK PRI INTO MSG PRI
        LDSPI R0; DB=ANSKEY            "ANSWER EXCH OFFSET
        ADD# MSG,R0; SETMA             "GET MSG'S ANS EXCH
        LDSPI EXCH; DPX(2)<DPX(1);     "EXCH ADDR
         STATMA
        LDSPI EXTYPE; DB=TYPE;         "TYPE OFFSET
         STATMA
        ADD TCB,R0; SETMA; MI<MD;      "ANS EXCH INTO TCB
         BR JOIN
"ALTERNATE ENTRY POINT
"
SENDA:  LDSPI EXCH; DPX(2)<DPX(1)      "EXCH ADDR
        LDSPI EXTYPE; DB=TYPE          "TYPE OFFSET
JOIN:   ADD EXCH,EXTYPE; SETMA         "GET EXCH TYPE
        LDSPI R1; DB=TSKBIT;           "EXCH'S TSKBIT MASK
         STATMA
        DPX(3)<MSGBIT;                 "EXCH'S MSGBIT MASK
         STATMA
        LDSPI R3; DB=MD                "EXCH TYPE
        AND# R3,R1                     "CHECK TSKBIT
        BNE GETSK                      "BRANCH IF SET
" GOT HERE IF NO TASKS QUEUED AT EXCH
"
        MOV EXTYPE,EXTYPE; SETMA;      "SET EXCH TYPE:
                          MI<DPX(3)    "MSGBIT ON, TSKBIT OFF
                                       "PARAMS FOR INSERT:
                                       "DPX(2)=ADDR OF NEXT
                                       "DPX(3)=ADDR OF NEW
        DPX(3)<DPX;                    "NEW=MSG ADDR
                                       "NEXT=Q HEADER=EXCH ADDR
         JSR INSERT                    "QUEUE THE MSG
        RETURN
" GOT HERE IF TASKS ARE QUEUED AT EXCH
"
                                       "PARAMS FOR DEQ:
                                       "DPX(2)=Q HEADER=EXCH ADDR
GETSK:  JSR DEQ                        "DEQUEUE A TASK
                                       "DEQ RETURNS WITH TCB IN DPX(3)
                                       "EXCH ADDR STILL IN DPX(2)
                                       "PARAMS FOR EMPTY:
                                       "DPX(2)=Q HEADER=EXCH ADDR
        DPX(1)<DPX(3);                 "SAVE TCB IN SAFE PLACE
         JSR EMPTY                     "SEE IF QUEUE EMPTY
                                       "RETURNS WITH SPFN=0 IF EMPTY
        BNE NEMPTY                     "BRANCH IF NOT EMPTY
" GOT HERE IF QUEUE EMPTY AFTER DEQUEUING TASK
"
        MOV EXTYPE,EXTYPE; SETMA; MI<ZERO "SET TYPE=NOTHING QUEUED
                                       "PARAMS FOR RECEIV:
                                       "DPX(0)=MSG ADDR
                                       "DPX(1)=TCB ADDR
" GOT HERE WITH PARAMS FOR RECEIV INTACT
"
NEMPTY: JSR RECEIV                     "TASK RECEIVES MSG
        RETURN
" GOT HERE IF MSG WAS LINKED ELSEWHERE
"
ERR:    LDSPI R0; DB=ERRBSY
        MOV R0,R0; RETURN              "RETURN ERROR CODE
"
$NOLIST
$INSERT SYSDEF
$LIST
$END
"****** RECEIV = RECEIVE MESSAGE = REL 0.0  , AUG 79 **************************
"
$TITLE RECEIV
$ENTRY RECEIV,0
$EXT RESUME
$EXT RMVCLK
"
"
" TASK RECEIVES MESSAGE.
" THIS RETURNS THE ADDRESS OF THE MESSAGE IN DPX(0),
" AND IN THE DPX0 LOCATION OF THE TCB SAVE AREA.
" THE TCB'S SPFN AND SP0 LOCATIONS ARE FILLED WITH
" THE MESSAGE TYPE.
" THE TASK IS PRIORITY-INSERTED INTO THE READY QUEUE.
"
" SIDE EFFECTS:
" IF THE TASK IS SLAVED, THE MESSAGE PRIORITY
" IS STORED INTO THE TASK'S RUN PRIORITY.
" IF RECEIVING AN ANSWER,
" THE MESSAGE'S BUSY BIT IS CLEARED.
" IF THE MESSAGE WAS NORMAL (AS OPPOSED TO AN
" ANSWER OR INTERRUPT MESSAGE) THEN THE MESSAGE
" ADDRESS IS ALSO PUT INTO THE LAST-MSG-REC'D
" WORD IN THE TASK'S TCB.
"
" NOTE:  BOTH THE MESSAGE AND THE TASK ARE ASSUMED
"        UNLINKED BY THE TIME WE GET HERE.
"
"
" THIS ASSUMES SUP. MODE AND INTERRUPTS OFF.
"
"
" PARAMETERS:
"  (IN)   DPX(0)=ADDR OF MSG
"  (IN)   DPX(1)=ADDR OF TCB
"  (OUT)  DPX(0)=ADDR OF MSG
"  (OUT)  SP0 AND SPFN=TYPE FIELD OF MESSAGE
"
"
" SCRATCH: SP 0-6, DPX 0-2
"
" ROUTINES CALLED: RESUME, RMVCLK
"
" TIMING: 66-69 CYCLES + 4 CYCLES/ELEMENT SEARCHED IN READY QUEUE.
"            30 CYCLES + 4 CYCLES/ELEMENT IN RESUME
"             9 CYCLES IN RMVCLK
"            27-30 CYCLES IN RECEIV (AS PER BELOW)
"               27 CYCLES IF TASK NOT SLAVED
"                  AND MESSAGE IS UNTYPED
"               +1 IF TASK IS SLAVED
"               +1 IF MSG IS NEITHER REG (UNTYPED) NOR ANSWER
"               +2 IF MSG IS ANSWER
"
"
TCB = 6
MSG = 2
PRI = 3
MTYPE = 4
SAVEM = 5
"
"
RECEIV: LDSPI MSG; DB=DPX              "ADDR OF MSG
        LDSPI TCB; DB=DPX(1)           "ADDR OF TCB
        LDSPI R3; DB=STATUS            "TCB STATUS OFFSET
        ADD TCB,R3; SETMA              "GET TCB STATUS
        LDSPI PRI; DB=RPRI;            "PRIORITY OFFSET
         STATMA
        ADD PRI,MSG; SETMA             "GET MSG PRI
        LDSPI R4; DB=MD                "TCB STAT
        LDSPI R0; DB=SLVBIT            "MASK FOR SLAVE BIT
        AND R0,R4;                     "MASK ALL BUT SLAVE BIT IN TCB STAT
         STATMA
        BEQ NOTSLV;                    "IF 0, NOT SLAVED
         INC MSG; SETMA                "GET MSG TYPE
        ADD TCB,PRI; SETMA; MI<MD      "MSG PRI TO TCB RPRI
NOTSLV: LDSPI R3; DB=ANSBIT;           "MSG TYPE ANS BIT
         STATMA
        LDSPI R0; DB=LSTMSG;           "TCB OFFSET FOR LAST-MSG-REC'D
         STATMA
        LDSPI MTYPE; DPX(2)<MD         "MSG TYPE
        MOV MTYPE,MTYPE                "SET SPFN
        BEQ REG;                       "BRANCH IF MSG UNTYPED
         LDSPI SAVEM; DB=DPX           "SAVE MSG ADDR
" GOT HERE IF NOT REGULAR MESSAGE...COULD BE ANSWER, INTERRUPT, ETC.
"
NOTREG: AND MTYPE,R3                   "TEST IF ANSWER
        BEQ ADD2Q;                     "BRANCH IF NOT ANSWER
         MOV MSG,MSG; SETMA; MI<DPX(2) "PRIME THE DPBS BUFFER
" GOT HERE IF ANSWER
"
ANS:    STATMA; MI<ZERO; WRTEXP;       "CLEAR MSG'S BSYBIT
         BR ADD2Q                      "AND GO
" GOT HERE IF REGULAR MESSAGE
"
REG:    ADD TCB,R0; SETMA; MI<DPX      "MSG ADDR TO TCB'S LSTMSG
" GOT HERE TO ADD TASK TO READY QUEUE
"
ADD2Q:  LDSPI R0; DB=DPX0              "TCB OFFSET FOR DPX0
        ADD TCB,R0; SETMA; MI<DPX      "TCB'S DPX0=MSG ADDR
        LDSPI R0; DB=SPFUNC            "TCB OFFSET FOR SPFN
        ADD TCB,R0; SETMA; MI<DPX(2)   "TCB'S SPFN=MSG TYPE
        DECMA; MI<DPX(2)               "TCB'S SP0=MSG TYPE
                                       "PARAM FOR RESUME:
                                       "DPX(1)=TCB ADDR
        JSR RESUME                     "ADD TASK TO RDYQUE
        MOV TCB,TCB; DPX(1)<SPFN       "RESTORE TCB ADDR
        MOV MTYPE,R6                   "SAVE MSG TYPE
                                       "PARAM FOR RMVCLK:
                                       "DPX(1)=TCB ADDR
        JSR RMVCLK                     "REMOVE TCB FROM CLOCK Q
        MOV SAVEM,SAVEM; DPX<SPFN      "RESTORE MSG ADDR
        MOV R6,R0;                     "MSG TYPE
         RETURN
"
$NOLIST
$INSERT SYSDEF
$LIST
"
$END
"****** RESUME = RESUME TASK = REL 0.0  , AUG 79 ******************************
"
$TITLE RESUME
$ENTRY RESUME,0
$EXT PRIQ
$INSERT COMSYS
"
"
" PUTS TASK BACK INTO READY QUEUE
"
"
" THIS ASSUMES SUP. MODE AND INTS. OFF
"
" PARAMETERS:
"   (IN)  DPX(1)=TCB ADDR
"
" SCRATCH: SP 0-3, DPX 1-3
"
" ROUTINES CALLED: PRIQ
"
" TIMING: 30 CYCLES + 4 CYCLES PER ELEMENT SEARCHED IN READY QUEUE
"              9 CYCLES IN RESUME
"             21 CYCLES + 4/ELE IN PRIQ
"
"
TSTAT = 0
TCB = 3
"
"
RESUME: LDSPI TCB; DPX(3)<DPX(1)       "TCB ADDR
        LDSPI TSTAT; DB=STATUS         "TCB STATUS OFFSET
        ADD TCB,TSTAT; SETMA           "GET TASK STATUS
        LDSPI R2; DB=RDYBIT;           "STATUS READY BIT
         STATMA
        DPX(2)<RDYQUE;                 "Q HEADER=READY QUEUE
         STATMA
        LDSPI R1; DB=MD                "TASK STATUS
        OR R2,R1; STATMA; MI<SPFN      "STORE INTO TCB, RDYBIT SET
                                       "PARAMS FOR PRIQ:
                                       "DPX(2)=Q HEADER
                                       "DPX(3)=NEW ELEMENT
        JSR PRIQ                       "PRIORITY INSERT TCB INTO RDYQUE
        RETURN
$NOLIST
$INSERT SYSDEF
$LIST
$END
"****** RUNPRI = SET RUN PRIORITY = REL 0.0  , AUG 79 *************************
"
$TITLE RUNPRI
$ENTRY RUNPRI,0
$ENTRY SETPRI,0
$EXT DELETE
$EXT RESUME
$INSERT COMSYS
"
"
" SETS RUN PRIORITY FOR A TASK.
" (CURRENT TASK FOR RUNPRI, TASK PARAMETER FOR SETPRI)
"
"
" PROCEDURE:
"   IF NEW PRIORITY IS LESS THAN OR EQUAL TO ZERO,
"   USE TASK'S DEFAULT PRIORITY.
"   OTHERWISE, USE NEW PRIORITY.
"   IF TASK IS IN READY QUEUE, PUT IN NEW PRIORITY ORDER.
"
" THIS ASSUMES SUP. MODE AND INTS OFF.
"
" PARAMETERS:
"  (IN)   DPX(0)=NEW RUN PRIORITY
"  (IN)   DPX(1)=TCB ADDR (SETPRI ONLY)
"
" SCRATCH:  SP 0-4, DPX 0,1,3
"
" ROUTINES CALLED: DELETE, RESUME
"
" TIMING:
"        51 CYCLES + 4 CYCLES/ELEMENT SEARCHED FOR RUNPRI
"            14 IN RUNPRI
"            7 IN DELETE
"            30 + 4/ELE IN RESUME
"        17 CYCLES FOR SETPRI IF TASK NOT IN READY QUEUE
"        56 CYCLES + 4/ELE FOR SETPRI IF TASK IS IN READY QUEUE
"            19 IN SETPRI
"            7 IN DELETE
"            30 + 4/ELE IN RESUME
"
"
FLAG = 0
RDY = 0
NPRI = 1
PRI = 2
TCB = 3
DFPRI = 4
ST = 4
"
"
RUNPRI: LDMA; DB=CURTSK;               "GET CURRENT TASK'S TCB
         LDSPI FLAG                    "FLAG NOT=0
        LDSPI NPRI; DB=DPX;            "NEW RUN PRI
         STATMA
        LDSPI PRI; DB=RPRI;            "TCB'S PRI OFFSET
         STATMA
        LDSPI TCB; DPX(1)<MD;          "TCB ADDR
         BR JOIN
SETPRI: CLR FLAG
        LDSPI TCB; DB=DPX(1)           "TCB ADDR
        LDSPI NPRI; DB=DPX             "NEW RUN PRI
        LDSPI PRI; DB=RPRI             "TCB'S PRI OFFSET
JOIN:   LDSPI DFPRI; DB=DPRI           "TCB'S DEFAULT PRI OFFSET
        ADD TCB,DFPRI; SETMA           "GET DEFAULT PRI
        MOV NPRI,NPRI;                 "SET SPFN
         STATMA
        BGT .+2;                       "CHECK IF NEW PRI>0
         LDSPI ST; DB=STATUS;          "TCB STATUS OFFSET
         STATMA
        ADD TCB,PRI; SETMA; MI<MD;     "IF NOT, USE DEFAULT PRI
         BR .+2
        ADD TCB,PRI; SETMA; MI<DPX     "IF SO, USE NEW PRI
        MOV FLAG,FLAG                  "CHECK IF CAME FROM RUNPRI
        BNE MOVE;                      "IF SO, TCB IS IN READY Q
         ADD TCB,ST; SETMA             "GET TCB STATUS
"GOT HERE IF DON'T KNOW IF TCB IS IN READY QUEUE
"(COULD BE WAITING AT EXCHANGE, ITS PRIORITY BEING
" CHANGED BY ANOTHER TASK.)
"
        LDSPI RDY; DB=RDYBIT;          "TASK READY BIT
         STATMA
        STATMA
        LDSPI ST; DB=MD                "TCB STATUS
        AND# RDY,ST                    "IS TASK'S RDYBIT ON?
        BEQ DONE                       "IF OFF, TASK ISN'T IN RDYQUE
"GOT HERE IF TASK IS IN READY QUEUE.
"MOVE IT ACCORDING TO PRIORITY.
"
                                       "PARAM FOR DELETE:
MOVE:   DPX(3)<DPX(1);                 "DPX(3)=ADDR OF ELEMENT TO DELETE
         JSR DELETE                    "REMOVE TASK FROM READY Q
                                       "PARAM FOR RESUME:
                                       "DPX(1)=TCB ADDR
        JSR RESUME                     "ADD TO RDYQUE IN NEW ORDER
DONE:   RETURN
$NOLIST
$INSERT SYSDEF
$LIST
$END
"****** SETFPE = ENABLE/DISABLE F.P. EXCEPTION INTERRUPT = REL 0.0  , AUG 79 **
"
$TITLE SETFPE
$ENTRY SETFPE,0
$INSERT COMSYS
"
"
" THIS ENABLES OR DISABLES THE INTERRUPT ON FLOATING POINT EXCEPTION.
"
"
" THIS ASSUMES SUP. MODE AND INTS OFF.
"
" PARAMETERS:
"  (IN)  DPX(0)=0 FOR DISABLE, NON-0 FOR ENABLE
"
" SCRATCH:  SP 0-3, DPX 0, DA
"
" TIMING: 12 CYCLES TO ENABLE OR DISABLE
"
"
TCB = 1
"
SETFPE: LDMA; DB=CURTSK                "GET CURRENT TASK'S TCB
        LDDA; DB=APST2                 "DA=APSTAT2
        LDSPI R2; DB=DPX;              "PARAM FOR SET OR CLEAR
         STATMA
        LDSPI R0; DB=STAT2;            "TCB OFFSET FOR APSTAT2
         STATMA
        LDSPI TCB; DB=MD               "CURRENT TASK'S TCB
        ADD TCB,R0; SETMA              "GET TCB'S APSTAT2
        LDSPI R1; DB=INBS; IN;         "CURRENT APSTAT2
         INCMA                         "(JUST A PUSHER)
        MOV R2,R2;                     "SET SPFN
         STATMA
        BEQ CLEAR;                     "BRANCH TO DISABLE
         LDSPI R3; DB=MD               "TCB'S APSTAT2
"GOT HERE TO ENABLE
"
SET:    LDSPI R2; DB=FPEON
        OR R2,R3; STATMA; MI<SPFN      "SET FPE IN TCB'S APSTAT2
        OR R2,R1; DB=SPFN; OUT;        "SET FPE IN CURRENT APSTAT2
         RETURN
"GOT HERE TO DISABLE
"
CLEAR:  LDSPI R2; DB=FPEOFF
        AND R2,R3; STATMA; MI<SPFN     "CLEAR FPE IN TCB'S APSTAT2
        AND R2,R1; DB=SPFN; OUT;       "CLEAR FPE IN CURRENT APSTAT2
         RETURN
$NOLIST
$INSERT SYSDEF
$LIST
$END
"****** OVHNDL = THE OVERLAY HANDLER = REL 0.0  , AUG 79 **********************
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
        RETURN                         "RETURN TO APEX TASK
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
"****** OVLAY = OVERLAY SVC'S = REL 0.0  , AUG 79 *****************************
"
$TITLE OVLAY
$ENTRY OVLAYM, 0
$ENTRY OVLAY1, 0
$ENTRY OVLAY2, 0
$EXT EXTASK
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
"   (IN)  SPAD 7 = ADDR OF CURRENT TASK'S TCB
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
"  (IN)  SPAD 7 = CURRENT TASK'S TCB
"  (IN)  DPX(0) = SEGMENT #
"
" SCRATCH:  SP 0-7, DPX 0-3
"
" ROUTINES CALLED: PSMNGR, EXTASK
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
" OVLAY1 IS FOR THE SINGLE (OR LAST) SEGMENT.
" OVLAY2 IS FOR MULTIPLE SEGMENTS FROM OVLAYM.
"
"
OVLAY1: DPX(1)<1; BR .+2               "SET ENTRY FLAG
OVLAY2: DPX(1)<ZERO                    "CLEAR ENTRY FLAG
        LDSPI OV; DB=OVLPTR            "TCB OFFSET FOR OVMAP PTR
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
        JMP EXTASK
"NOTE: HERE AND ELSEWHERE WE GO TO EXTASK INSTEAD OF RETURNING
"TO THE TRAP HANDLER, BECAUSE IT WOULD CLOBBER DPX 0-3 AND SP 0
"IN THE TASK'S TCB.
"
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
        JMP EXTASK                     "EXIT
"
$NOLIST
$INSERT SYSDEF
$LIST
$END
 