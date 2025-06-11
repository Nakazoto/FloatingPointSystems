"/****** FUNC.S = FUNCTION INTERPRETER TASK = REL 0.0  , JUL 79 ***************
"
"               CHANGED    79-AUG-29            13:05
"
"       FOR USE WITH FPS-100 SUPERVISOR
"         JOSEPH E. RAMUS        MAR 79
"
"       TAB STOP SETTINGS FOR EDITOR:    9   33  60
"
"         SPAD REGISTER NAMES DEFINED IN SYSDEF
"           R0 R1 R2 R3 R4 R5 R6 R7
"
"         INDEXES FOR DATA PAD X DEFINED IN SYSDEF
"           X0 X1 X2 X3
"
"
"                               LOGIC FLOWCHART
"
"  $""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""$
"  $    IF:  DEPOSIT,  EXAM,  OR OTHER                              $
"  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"         V                         V                        V
"       DEPOSIT                   EXAM                     OTHER
"         V                         V                        V
"         V                         V                        V
"     $"""""""""""""""""""""""$   $""""""""""""""$           V
"     $  INC MA IF REQUIRED.  $   $  SELECT.     $           V
"     $  SELECT.              $   $  GET VALUE.  $           V
"     $  STORE SWR VALUE.     $   """"""""""""""""           V
"     """""""""""""""""""""""""                V             V
"          V                                $""""""""""""""""""""""""$
"     $"""""""""""""""""""""""$             $                        $
"     $  INC TMA OR DPA       $             $  INC MA, TMA, OR DPA   $
"     $    IF REQUIRED        $             $    IF REQUIRED         $
"     """""""""""""""""""""""""             """"""""""""""""""""""""""
"          V                                     V
"          V                                     V
"     $""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""$
"     $   IF:  UNIMPLEMENTED FUNCTION                                $
"     $                                                              $
"     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"          NO  V                                YES  V
"              V                                     V
"     $"""""""""""""""""""""""$             $""""""""""""""""""""""""$
"     $  CTRL BIT 5 IRP       $             $  SET ERROR FLAG        $
"     $  RETURN VALUES        $             $  RETURN VALUES         $
"     """""""""""""""""""""""""             """"""""""""""""""""""""""
""
""
""
$TASK 776
$TITLE FUNC
$ENTRY FUNC,0
$EXT SENDER
$EXT WAIT
"
"               $INSERT  SYSDEF
$NOLIST
$INSERT SYSDEF
$LIST
"
$INSERT FHOSTC
"
        $COMMON /TCB777/ TCB(148.)
"/****** FUNC = FUNCTION INTERPRETER TASK = REL 0.0  , JUL 79 *****************
"
FUNC:   NOP
"
"         WAIT FOR MESSAGE FROM HOST INTERRUPT ROUTINE
"
          DPX(X1)<EXFUNC        "MESSAGE EXCHANGE
          TRAP; DB=@WAIT; LDTMA       "WAIT FOR MESSAGE
"
"         SUPERVISOR STATE RESTORE WILL LOAD TMA, SPADS, ETC. FROM TCB
"
"                               READ MESSAGE VALUES
        LDMA; DB=APMSG+8.       "FETCH SWR VALUE
        INCMA                   "FETCH FUNC VALUE
        LDSPI R4; DB=60         "FOR WORD FIELD
        LDSPI R5; DB=MD         "LOAD SWR VALUE
        LDSPI R1; DB=MD         "LOAD FUNC VALUE
        ANDRR R1,R4             "MASK & SHIFT BY 2
        MOVRR R4,R4             "SHIFT 4  (SAVE R4 FOR LATER USE)
        LDSPI R2; DB=1000        "MASK FOR DEPOSIT FIELD
        AND# R1,R2
        BNE  FDEPS;             "DEPOSIT SPECIFIED
          MOVL  R2,R2               "TO TEST FOR EXAM
        AND# R1,R2
        BNE  FEXAM                "EXAM SPECIFIED
"                    OTHER FUNCTIONS
        JSR INCADD              "INC ADDRESS REGISTER IF SPECIFIED
"                  R2 RETURNS A FLAG
        MOV R2,R2
        BEQ FINC
        JMP NOFUNC              "UNIMPLEMENTED FUNCTION
FINC:   JMP OKOK                "ALL OK
"
FEXAM:  JMP FEXAMS              "BECAUSE BRANCH IS OUT OF RANGE
"
"                           DEPOSIT
FDEPS:  LDSPI R7; DB=300         "FOR INC FIELD --  DEPOSIT
        AND R1,R7               "R7 GETS INC VALUE
        LDSPI R6; DB=200;
          BEQ FDEPS2              "NO INCREMENT
        SUB R6,R7               "COMPUTE R7-R6
        BGE FDEPS2              "NOT INC MA
        JSR ICRMA               "INCREMENT MA
FDEPS2: JSR FSELEC              "LOOK AT SELECT FIELD
"                  FLAG AND OTHER VALUES
"             R0  +  UNIMPLEMENTED SELECT VALUE
"                 0  DEPOSIT INTO 38 BITS, SWR TO MD (MA IS SET)
"                 -  DEPOSIT 16 BITS INTO TCB IN MD  (MA IS SET)
"             R6  0  IF CAN DEPOSIT INTO 38 BIT WORD IN MD
"                 -  IF DEPOSIT NOT ALLOWED
"                 +  NOT USED
"             R4  HAS VALUE FROM WORD FIELD
"             R7  HAS VALUE FROM INC  FIELD
        MOV R7,R7               "TEST FLAG COMPUTED ABOVE
        BLT FDEPS3              "NOT TMA OR DPA
        JSR INCADD              "INCREMENT ADDRESS REG
FDEPS3: MOV R4,R4;              "FLAG FROM WORD FIELD
        DPX(X3)<ZERO
        BNE NODEP               "DEPOSIT NOT IMPLEMENTED
"                  DEPOSIT 16 BITS  INTO TCB
        MOV R5,R5; DB=SPFN;
          STATMA                  "STORE SWR VALUE
        JMP OKOK
"
NODEP:  JMP NOFUNC
"
"                      EXAM
FEXAMS: JSR FSELEC              "LOOK AT SELECT FIELD --  EXAMINE
        JSR INCADD              "INCREMENT ADDRESS REG IF NEEDED
"             R4  HAS VALUE FROM WORD FIELD
        DPX(X0)<ZERO
        DPX(X1)<DPX(X3)         "VALUE FROM TCB
        DPX(X2)<ZERO            "ERROR FLAG
        JSR SENDER
        JMP FUNC                "WAIT FOR ANOTHER MESSAGE
"
"/****** INCADD = INCREMENT ADDRESS REGISTER IF NEEDED = REL 0.0  , JUL 79 ****
"
INCADD: LDSPI R2; DB=300         "FOR INC FIELD
        AND R1,R2
        LDSPI R3; DB=200;
          BEQ INCDUN              "NO INCREMENT
        SUB R3,R2                 "COMPUTE R2-R3
        BEQ ICRDPA;             "INC DPA
          MOV  R2,R2
        BLT ICRMA              "INC MA
ICRTMA: LDMA; DB=TCB+TMAREG;     "INC TMA
          BR ICRG
ICRDPA: LDMA; DB=TCB+DPA;    "FETCH DPA VALUE
          BR ICRG
ICRMA:  LDMA; DB=TCB+MAREG      "FETCH MA VALUE
ICRG:   DECMA                   "PREPARE FOR INCMA
        SPMDAV;                  "WAIT
          LDSPI R2;DB=MD           "PUT IN SPAD
        INC R2;                  "INC VALUE AND INSERT IN MD
          INCMA; MI<SPFN
        CLR R2; RETURN          "SET FLAG FOR ADDRESS INCREMENTED
INCDUN: RETURN;
          LDSPI R2; DB=1        "SET FLAG FOR NOT INCREMENTED
"
"/****** FSELEC = DECIDE REGISTER SELECT  = REL 0.0  , JUL 79 *****************
"
"       SUBROUTINE TO PROCESS THE REGISTER SELECT FIELD
"       PRESERVE SPAD R1
"       R6 RETURNS A FLAG
"           0 IF DEPOSIT IN MD
"           - IF DEPOSIT NOT ALLOWED
"       SPAD R0  RETURNS A FLAG:
"         +  IF UNIMPLEMENTED SELECT VALUE
"         0  MEANS DPX(X3) HAS A 38 BIT VALUE.
"             LATER CODE WILL SELECT  EXP, HMAN, LMAN.
"         -  MEANS DPX(X3) HAS INTEGER VALUE
"
FSELEC: LDSPI R2; DB=17          "MASK FOR SELECT FIELD
        LDSPI R0; DB=123234      "SET FLAG TO NEG VALUE
        MOV R0,R6               "SET FLAG FOR DEPOSIT NOT ALLOWED
        AND R1,R2                 "R2 HAS SELECT VALUE
        BEQ  FSEL00;  DEC R2;    " SELECT = 00
          DPX(X3)<ZERO             "CLEAR
        BEQ  FSEL01;  DEC R2     " SELECT = 01
        BEQ  FSEL02;  DEC R2     " SELECT = 02
        BEQ  FSEL03;  DEC R2     " SELECT = 03
        BEQ  FSEL04;  DEC R2     " SELECT = 04
        BEQ  FSEL05;  DEC R2     " SELECT = 05
        BEQ  FSEL06;  DEC R2     " SELECT = 06
        BEQ  FSEL07;  DEC R2     " SELECT = 07
        JMP NOFUNC              "JUMP AROUND LOCAL CODE
"
FSEL00:  JMP NOFUNC              "PSA NOT READILY ACCESSIBLE IN TCB
"
FSEL01: LDMA; DB=TCB+SPD;       "FETCH SPD VALUE --  SPD REG
          BR FSLX
"
FSEL02: LDMA; DB=TCB+MAREG;     "FETCH MA VALUE --  MA REG
          BR FSLX
"
FSEL03: LDMA; DB=TCB+TMAREG;     "FETCH TMA VALUE --  TMA REG
          BR FSLX
"
FSEL04: LDMA; DB=TCB+DPA;    "FETCH DPA VALUE --  DPA REG
          BR FSLX
"
FSEL05: LDMA; DB=TCB+SPFUNC;      "FETCH SPFN VALUE --  SPFN
          BR FSLX
"
FSEL06: LDMA; DB=TCB+STAT1;    "FETCH STATUS VALUE --  AP STATUS
          BR FSLX
"
FSEL07: LDMA; DB=TCB+DA         "FETCH DA VALUE --  DA REG
"
FSLX:   SPMDAV;                 "WAIT FOR MD
          DPX(X3)<MD;  RETURN
"
"                   ALL IS OK
OKOK:   DPX(X2)<ZERO;           "THIRD ARG
          BR FCEX
"             UNIMPLEMENTED FUNCTION
NOFUNC: DPX(X2)<2               "THIRD ARG - ERROR FLAG
FCEX:   CLR R0; DPX(X0)<SPFN    "FIRST ARG FOR SENDER
        DPX(X1)<DPX(X0)         "SECOND ARG FOR SENDER
        JSR SENDER
        JMP FUNC                "WAIT FOR ANOTHER MESSAGE
        $END
 