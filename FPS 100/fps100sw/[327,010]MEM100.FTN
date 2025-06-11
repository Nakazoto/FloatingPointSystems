C****** MEM100 = MEMORY TEST FOR ARRAY PROCESSER = REL 5.0  , NOV 79 **********
C****** MEM100 = MEMORY TEST FOR ARRAY PROCESSER -CONTROLLER= REL 5.0  , NOV 79
C%.I 15 MITRA
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : MEM100                                                   *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THIS PROGRAM TESTS THE MEMORIES OF THE ARRAY PROCESSOR   *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C:
C
C
C==
C-------COMMON STORAGE:
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C       ITTI   = TERMINAL INPUT FORTRAN UNIT NUMBER
C       ITTO   = TERMINAL OUTPUT FORTRAN UNIT NUMBER
C       FUNIT  = ECHO FILE OUTPUT FORTRAN UNIT NUMBER
C
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C------ LOCAL STORAGE:
C
        INTEGER STESTS(2),FIRST,TESTN
C:
C%.I 71 NOVA
C
C====== INITIALIZE VARIABLES
C
        ON=1
        OFF=0
        FIRST=1
        ERRORS=0
        PASSES=0
        NTESTS=16
C
C====== SET CONSOLE LUNS
C
        N=IOCNTL(1,0,0,0)
        CALL FMGR(0)
C
C====== GET OPERATOR COMMANDS
C
10      CALL SETUP(FIRST)
        FIRST=0
        TIP=OFF
C
C====== RUN TESTS
C
20      DO 60 TESTN=1,NTESTS
C
C====== SET STAT TO INDICATE START OF TEST
C
        STAT=0
C
C====== RESET RESTART VARIABLE
C
        RSTART=OFF
C
        IF(TESTS(1).EQ.0.AND.TESTS(2).EQ.0) GOTO 30
C
C====== CHECK FOR SELECTED TESTS
C
        J=2-(TESTN/16)
        IF(IAND16(TESTS(J),ILSH16(1,MOD(TESTN,16))).EQ.0) GOTO 60
C
C====== RUN SELECTED TEST
C
30      CALL TEST(TESTN)
C
C====== SAVE THE REQUESTED TEST NUMBERS FOR COMPARISON AFTER RETURNING
C       FROM SETUP SO THAT THE PROGRAM CAN START THE TEST SEQUENCE OVER IF
C       ANY NEW TESTS WERE SELECTED
C
        STESTS(1)=TESTS(1)
        STESTS(2)=TESTS(2)
C
C====== GET OPERATOR INTERVENTION FLAG
C
        CALL TERM(N)
        IF(N.NE.0) TIP=ON
C
        IF(NERR.EQ.ON) ERRORS=ERRORS+1
C
C====== TEST FOR ERROR LIMIT
C
        IF(ERRORS.LT.ERRLIM.OR.ERRLIM.EQ.0) GOTO 35
        WRITE(ITTO,110)
        IF(ECHO.NE.0) WRITE(FUNIT,110)
110     FORMAT(1X,11HERROR LIMIT)
        ERRORS=0
        GOTO 40
C
C====== TEST FOR OPERATOR INTERVENTION
C
35      IF(TIP.NE.OFF) GOTO 40
C
C====== TEST FOR HALT FLAG SET
C
        IF(HALT.EQ.ON) GOTO 40
C
C====== TEST FOR WAIT ON ERROR FLAG
C
        IF(WAIT.EQ.OFF.OR.NERR.EQ.OFF) GOTO 50
40      CALL SETUP(0)
50      HALT=OFF
C
C====== SEE IF ANY DIFFERENT TESTS WERE SELECTED
C
        IF(TESTS(1).NE.STESTS(1)) GOTO 20
        IF(TESTS(2).NE.STESTS(2)) GOTO 20
        IF(RSTART.EQ.ON) GOTO 20
C
C====== TEST FOR CONTINUATION OF TEST
C
        IF(STAT.NE.2) GOTO 30
        IF(NERR.EQ.ON.AND.LOOP.EQ.ON) GOTO 30
C
60      CONTINUE
C
C====== PRINT PASS MESSAGE AND TEST FOR PASS LIMIT AND ERROR LIMIT
C
        PASSES=PASSES+1
        IF(BETWN.GT.PASLIM) BETWN=PASLIM
        IF(MOD(PASSES,BETWN).NE.0) GOTO 80
        WRITE(ITTO,70) PASSES,ERRORS
        IF(ECHO.NE.0) WRITE(FUNIT,70) PASSES,ERRORS
70      FORMAT(1X,16HMEM100 - PASSES=,I6,8H ERRORS=,I6)
C
80      IF(MOD(PASSES,PASLIM).NE.0) GOTO 20
C
C====== PASS LIMIT REACHED
C
        ERRORS=0
        PASSES=0
        WRITE(ITTO,90)
        IF(ECHO.NE.0) WRITE(FUNIT,90)
90      FORMAT(1X,10HPASS LIMIT)
        GOTO 10
        END
C****** TEST = TEST ROUTINE CALLING PROGRAM = REL 5.0  , NOV 79 ***************
        SUBROUTINE TEST(NTEST)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TEST                                                     *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : NTEST  =>TEST ROUTINE NUMBER TO CALL                     *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : CALL THE DIFFERENT AP MEMORY TEST ROUTINES               *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C%.I 201 NOVA
C
C====== COMPUTED GOTO TO CALL TESTS
C
        GOTO(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160)
     +  ,NTEST
C
C====== PROGRAM SOURCE TEST  - PANEL
C
C%.R 209 NOVA
10      CALL PSVTST(NTEST)
C%.E 209 NOVA
C         LOAD PS BOOT AFTER LOW MEMORY TEST
        CALL LOADBT
C         MAKE SURE THAT A FILE REWIND IS DONE
C         SO THAT A AP LOAD IS FORCED
        CALL FMGR(0)
        RETURN
C
C====== BRANCH TEST
C
C%.R 222 NOVA
20      CALL BRTST(NTEST)
C%.E 222 NOVA
        RETURN
C
C====== DATA PAD X TEST
C
C%.R 229 NOVA
30      CALL DPXTS(NTEST)
C%.E 229 NOVA
        RETURN
C
C====== DATA PAD Y TEST
C
C%.R 236 NOVA
40      CALL DPYTS(NTEST)
C%.E 236 NOVA
        RETURN
C
C====== PROGRAM SOURCE - MICRO CODE
C
C%.R 243 NOVA
50      CALL PSTST(NTEST)
C%.E 243 NOVA
        RETURN
C
C====== SPAD TEST
C
C%.R 250 NOVA
60      CALL SPTST(NTEST)
C%.E 250 NOVA
        RETURN
C
C====== MAIN DATA TEST
C
C%.R 257 NOVA
70      CALL MDTST(NTEST)
C%.E 257 NOVA
        RETURN
C
C====== MAIN DATA PARITY TEST PART 1
C
80      CONTINUE
C%.R 265 NOVA
        CALL MDPT1(NTEST)
C%.E 265 NOVA
        RETURN
C
C====== MAIN DATA PARITY TEST PART 2
C
90      CONTINUE
C%.R 273 NOVA
        CALL MDPT2(NTEST)
C%.E 273 NOVA
        RETURN
C
C====== MAIN DATA ADDRESS TAG
C
100     CONTINUE
C%.R 281 NOVA
        CALL MTST0(NTEST)
C%.E 281 NOVA
        RETURN
C
C====== MAIN DATA RANDOM PATTERNS
C
110     CONTINUE
C%.R 289 NOVA
        CALL MTST1(NTEST)
C%.E 289 NOVA
        RETURN
C
C====== TABLE MEMORY RAM TEST
C
C%.R 296 NOVA
120     CALL TMTST(NTEST)
C%.E 296 NOVA
        RETURN
C
C====== TM RAM ADDRESS TAG
C
C%.R 303 NOVA
130     CALL TMBAD(NTEST)
C%.E 303 NOVA
        RETURN
C
C====== TM RAM RANDOM PATTERN
C
C%.R 310 NOVA
140     CALL TMRDM(NTEST)
C%.E 310 NOVA
        RETURN
C
C====== TM ROM CHECKSUM TEST
C
C%.R 317 NOVA
150     CALL TMCKS(NTEST)
C%.E 317 NOVA
        RETURN
C
C====== TM ROM NOISE TEST
C
C%.R 324 NOVA
160     CALL TMNSE(NTEST)
C%.E 324 NOVA
        RETURN
C
        END
C****** PSVTST = PROGRAM SOURCE FRONT PANEL TEST DRIVER = REL 5.0  , NOV 79 ***
C%.I 331 NOVA
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PSVTST                                                   *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THIS IS THE DRIVER FOR THE PROGRAM SOURCE VIRTUAL FRONT  *
C  *                  PANEL TEST                                               *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE PSVTST(NTEST)
C:
C
C------ COMMON STORAGE:
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C:
C
C====== TEST FOR A CONTINUE AFTER AN ERROR
C
        IF(STAT.NE.0) GOTO 40
C
C====== TEST FOR ANY PS SELECTED
C
        IF(PSLO.LE.255.AND.(PSLO.NE.0.OR.PSHI.NE.0)) GOTO 10
        STAT=2
        RETURN
C
C====== TEST FOR NAMES PRINT
C
10      IF(NAMES.EQ.OFF) GOTO 30
        WRITE(ITTO,20)NTEST
        IF(ECHO.NE.0) WRITE(FUNIT,20)NTEST
20      FORMAT(1X,1H(,I2,2H) ,31HPROGRAM SOURCE FRONT PANEL TEST)
C
C====== MAKE SURE THE AP IS STOPPED
C
30      CALL APRSET
C
C======DO THE TEST
C
40      CALL PSPNL
C
C====== CALL ERROR CHECK ROUTINE
C
50      CALL PSVER
C
        RETURN
        END
C****** PSPNL = DO FRONT PANEL TEST OF PROGRM SOURCE = REL 5.0  , NOV 79 ******
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PSPNL                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : STAT => IF STAT IS ZERO THEN THE TEST IS STARTED         *
C  *                  ELSE THE TEST IS CONTINUED FROM THE LAST ERROR STOP      *
C  *                                                                           *
C  *    EXIT        : ERR => ERROR FLAG SET WHEN AN ERROR OCCURS               *
C  *                                                                           *
C  *    FUNCTION    : TO DO THE VFP TEST OF PROGRAM SOURCE (256 LOCATIONS)     *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE PSPNL
C:
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
        INTEGER START,RET,IEND
C:
C
C====== CHECK FOR A CONTINUE OF TEST
C
        IF(STAT.NE.0) GOTO 130
C
C====== TEST FIRST 16 WORDS .. ZEROES
C
        IF(PSLO.GT.15) GOTO 40
        START=0
        IEND=15
        IF(PSLO.GT.START) START=PSLO
        IF(PSHI.LT.IEND) IEND=PSHI
        NUM=IEND-START+1
        RET=1
        CALL FILL(TABLE,1,64,0)
        GOTO 130
C
C====== TEST FIRST 16 WORDS ALL ONES
C
10      RET=2
        STAT=0
        CALL FILL(TABLE,1,64,IP16(-1))
        GOTO 130
C
C====== TEST FIRST 16 WORDS .. RANDOM NUMBERS
C
20      RET=3
        STAT=0
        IY=7
        DO 30 I=1,64
30      TABLE(I)=IGRN16(IX,IY)
        GOTO 130
C
C====== TEST FIRST HALF OF PS (DMA)  0'S
C
40      CALL LOADBT
        IF(PSLO.GT.135.OR.PSHI.LT.16) GOTO 80
        START=16
        IEND=135
        IF(PSLO.GT.START) START=PSLO
        IF(PSHI.LT.IEND) IEND=PSHI
        NUM=IEND-START+1
        STAT=0
        RET=4
        CALL FILL(TABLE,1,480,0)
        GOTO 130
C
C====== TEST FIRST HALF OF PS (DMA) 1'S
C
50      RET=5
        STAT=0
        CALL FILL(TABLE,1,480,IP16(-1))
        GOTO 130
C
C====== TEST FIRST HALF OF PS (DMA) RANDOM
C
60      RET=6
        STAT=0
        DO 70 I=1,480
70      TABLE(I)=IGRN16(IX,IY)
        GOTO 130
C
C====== TEST SECOND HALF OF PS (DMA) 0'S
C
80      RET=7
        IF(PSLO.GT.255.OR.PSHI.LT.136) GOTO 120
        START=136
        IEND=255
        IF(PSLO.GT.START)START=PSLO
        IF(PSHI.LT.IEND)IEND=PSHI
        NUM=IEND-START+1
        STAT=0
        CALL FILL(TABLE,1,480,0)
        GOTO 130
C
C====== TEST SECOND HALF OF PS (DMA) 1'S
C
90      RET=8
        STAT=0
        CALL FILL(TABLE,1,480,IP16(-1))
        GOTO 130
C
C====== TEST OF SECOND HALF OF PS (DMA) RANDOM
C
100     RET=9
        STAT=0
        DO 110 I=1,480
110     TABLE(I)=IGRN16(IX,IY)
        GOTO 130
C
C====== IEND OF TEST
C
120     STAT=2
        RETURN
C
C====== CALL TEST CONTROLLER
C
130     CALL PSVFP(TABLE,START,NUM,STAT,NERR,RET,TIP,LOOP)
        IF(NERR.EQ.ON) RETURN
        GOTO (10,20,40,50,60,80,90,100,120),RET
        END
C****** PSVFP = DO VIRTUAL FRON PANNEL PS TEST = REL 5.0  , NOV 79 ************
        SUBROUTINE PSVFP(TABLE,START,NUM,STAT,NERR,RET,TIP,LOOP)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PSVFP                                                    *
C  *    REV         : O                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : TABLE => BUFFER TO TEST MEMORY                           *
C  *                  START => START LOCATION TO TEST MEMORY                   *
C  *                  NUM   => NUMBER OF LOCATIONS TO TEST                     *
C  *                  STAT  => TEST IN PROGRESS FLAG (0=START 1=CONTINUE)      *
C  *                  RET   => ERROR CODE NUMBER                               *
C  *                                                                           *
C  *    EXIT        : ERR   =>SET TO 'ON' WHEN ERROR IS DETECTED               *
C  *                 IEXP   =>EXPECTED ERROR DATA BUFFER                       *
C  *                  ACT   =>ACTUAL READ DATA BUFFER                          *
C  *                                                                           *
C  *    FUNCTION    : TEST PROGRAM SOURCE THROUGH FRONT PANNEL                 *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C:
        COMMON /PANEL/ IEXP(4),ACT(4),PSAD,CODE
        INTEGER IEXP,ACT,PSAD,CODE
C
        INTEGER ADDR,START,TABLE(1024),NUM,STAT,PS,COUNT,NERR,OFF,ON,RET,
     +  TIP,LOOP
C%.I 547 NOVA
        DATA    PS/9/,OFF/0/,ON/1/
C:
C
C====== CHECK FOR A CONTINUE
C
        IF(STAT.NE.0) GOTO 60
        STAT=1
        ADDR=START
C
C====== TEST FOR DMA OR PANNEL
C
        IF(START.GT.16) GOTO 20
C
C====== PANEL BUFFER TO AP
C
        DO 10 I=1,NUM
        J=I*4-3
        CALL APDEP(TABLE(J),PS,ADDR)
10      ADDR=ADDR+1
        GOTO 30
C
C====== DMA TO PS
C
20      CALL LOADPS(TABLE,START,NUM)
C
C====== GET DATA BACK FROM AP
C
30      ADDR=START
        DO 40 I=1,NUM
        J=I*4-3+NUM*4
        CALL APEXAM(TABLE(J),PS,ADDR)
40      ADDR=ADDR+1
C
C====== ERROR CHECK
C
        ADDR=START
        COUNT=1
50      J=COUNT*4-3
        K=J+NUM*4
        IF(NCOMP(TABLE,J,J+3,TABLE,K).NE.0) GOTO 70
60      NERR=OFF
        CALL TERM(TIP)
        IF(TIP.NE.OFF) RETURN
        COUNT=COUNT+1
        ADDR=ADDR+1
        IF(COUNT.GT.NUM) RETURN
        GOTO 50
C
C====== PROCESS ERROR
C
70      NERR=ON
        CALL MOVE(TABLE,J,J+3,IEXP,1)
        CALL MOVE(TABLE,K,K+3,ACT,1)
C
C====== TEST FOR LOOP ON ERROR
C
        IF(LOOP.EQ.ON) GOTO 80
        PSAD=ADDR
        CODE=RET
        RETURN
C
C====== LOOP
C
80      CALL APDEP(IEXP,PS,ADDR)
        CALL APEXAM(ACT,PS,ADDR)
        CALL TERM(TIP)
        IF(TIP.EQ.OFF) GOTO 80
        RETURN
        END
C****** PSVER = PROGRAM SOURCE TEST ERROR PROCESSOR = REL 5.0  , NOV 79 *******
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PSVER                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : ERROR INFORMATION IS READ FROM COMMON BLOCK /PANEL/      *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : INTERPRETS THE ERROR CODE AND PRINTS EXPECTED AND        *
C  *                  ACTUAL DATA FROM THE AP                                  *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE PSVER
C:
C
C------COMMON STORAGE:
C
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
        COMMON /PANEL/IEXP(4),ACT(4),PSAD,CODE
        INTEGER       IEXP,   ACT   ,PSAD,CODE
C
C------LOCAL STORAGE:
C
        INTEGER ADDR(6)
C%.I 685 NOVA
        INTEGER LINE(27),BLANK
        DATA BLANK/1H /
C
C
C:
        CALL FILL(LINE,1,27,BLANK)
C
C====== TEST FOR ERROR CONDITION
C
        IF(NERR.EQ.OFF) RETURN
        IF(TD.EQ.ON) RETURN
C
C====== PRINT MESSAGE
C
        WRITE(ITTO,10)CODE
        IF(ECHO.NE.0) WRITE(FUNIT,10)CODE
10      FORMAT(1X,37HMEM100 - PS FRONT PANEL TEST - ERROR=,I6)
C
        CALL I2ASCI(6,PSAD,ADDR,IRADX,0)
C
C====== PRINT ADDRESS AND EXPECTED
C
        DO 20 I=1,4
        J=I*7-6
20      CALL I2ASCI(6,IEXP(I),LINE(J),IRADX,0)
C
        WRITE(ITTO,30)ADDR,LINE
        IF(ECHO.NE.0) WRITE(FUNIT,30)ADDR,LINE
30      FORMAT(1X,9HADDRESS= ,6A1,3X,2HE ,27A1)
C
C====== PRINT ACTUAL
C
        DO 40 I=1,4
        J=I*7-6
40      CALL I2ASCI(6,ACT(I),LINE(J),IRADX,0)
C
        WRITE(ITTO,50)LINE
        IF(ECHO.NE.0) WRITE(FUNIT,50)LINE
50      FORMAT(1X,18X,2HA ,27A1)
C
        RETURN
        END
C****** BRTST = DO BRANCH TEST = REL 5.0  , NOV 79 ****************************
C%.I 766 NOVA
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : BRTST                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : STAT => IF STAT IS ZERO THEN THE TEST IS STARTED         *
C  *                  ELSE THE TEST IS CONTINUED FROM THE LAST ERROR STOP      *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : CONTROLLER FOR BRANCH AND CONDITIONAL BRANCH TESTS       *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE BRTST(NTEST)
C:
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
        INTEGER ITTI,ITTO,FUNIT
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
        INTEGER START,RET,IEND
C:
C
C====== CHECK FOR A CONTINUE OF TEST
C
        IF(STAT.NE.0) GOTO 130
C
C====== TEST FOR NAMES PRINT
C
        IF(NAMES.EQ.OFF) GOTO 20
        WRITE(ITTO,10)NTEST
        IF(ECHO.NE.0) WRITE(FUNIT,10)NTEST
10      FORMAT(1X,1H(,I2,2H) ,11HBRANCH TEST)
C
C====== MAKE SURE THE AP IS STOPPED
C
20      CALL APRSET
C
C====== UNCONDITIONAL BRANCH TEST
C
        RET=1
        CALL FMGR(1)
        GOTO 130
C
C====== BRANCH ON FLAGS TEST
C
30      RET=2
        CALL APRSET
        STAT=0
        CALL FMGR(2)
        GOTO 130
C
C====== BRANCH ON DATA PAD BUS CONDITIONS
C
40      RET=3
        CALL APRSET
        STAT=0
        CALL FMGR(3)
        GOTO 130
C
C======  BRANCH ON SPAD STATUS BITS
C
50      CONTINUE
        STAT=0
        RET=4
        CALL APRSET
        CALL FMGR(4)
        GOTO 130
C
C====== BRANCH ON SPAD OPERATIONS
C
60      RET=5
        CALL APRSET
        STAT=0
        CALL FMGR(5)
        GOTO 130
C
C====== BRANCH ON FLOATING POINT STATUS BITS
C
70      RET=6
        CALL APRSET
        STAT=0
        CALL FMGR(6)
        GOTO 130
C
C====== BRANCH ON FLOATING POINT OPERATIONS
C
80      RET=7
        CALL APRSET
        STAT=0
        CALL FMGR(7)
        GOTO 130
C
C====== BRANCH ON I/O FLAGS
C
90      RET=8
        CALL APRSET
        STAT=0
        CALL FMGR(8)
        GOTO 130
C
C====== BRANCH ON INT. REQ. FLAGS
C
100     RET=9
        CALL APRSET
        STAT=0
        CALL FMGR(9)
        GOTO 130
C
C====== BRANCH ON FFT FLAGS
C
110     RET=10
        CALL APRSET
        STAT=0
        CALL FMGR(10)
        GOTO 130
C
C====== END OF TEST
C
120     STAT=2
        RETURN
C
C====== CALL TEST
C
130     CONTINUE
        CALL RUNAP(16,STAT,0,8192)
140     CALL TSTRUN(I)
        IF(I.EQ.1) GOTO 150
        CALL TERM(TIP)
        IF(TIP.EQ.OFF) GOTO 140
C
C====== OPERATOR INTERVENTION
C
        CALL APRSET
        RETURN
C
C====== CALL ERROR CHECK ROUTINE
C
150     CALL BRERR
        STAT=1
        IF(NERR.EQ.ON) RETURN
        GOTO (30,40,50,60,70,80,90,100,110,120),RET
        END
C****** BRERR = BRANCH TEST ERROR PROCESSOR = REL 5.0  , NOV 79 ***************
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : BRERR                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : INTERPRETS THE ERROR CODE AND PRINTS EXPECTED AND        *
C  *                  ACTUAL DATA FROM THE AP                                  *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE BRERR
C:
C
C------COMMON STORAGE:
C
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C------LOCAL STORAGE:
C
C
C
C:
C
C====== TEST FOR ERROR CONDITION
C
        NERR=OFF
        CALL ILITES(LITE)
        IF(LITE.EQ.-1) RETURN
        NERR=ON
        IF(TD.EQ.ON) RETURN
C
C====== PRINT MESSAGE
C
        WRITE(ITTO,10)LITE
        IF(ECHO.NE.0) WRITE(FUNIT,10)LITE
10      FORMAT(1X,29HMEM100 - BRANCH TEST - ERROR=,I6)
C
C====== TEST FOR FATAL BRANCH DISPLACEMENT ERROR
C
        IF(LITE.GT.9) RETURN
        WRITE(ITTO,20)
        IF(ECHO.NE.0) WRITE(FUNIT,20)
20      FORMAT(1X,41HFATAL BRANCH ERROR - TEST CANNOT CONTINUE)
        TIP=ON
        RETURN
        END
C****** DPXTS = DATA PAD X MOVING INVERSE TEST DRIVER = REL 5.0  , NOV 79 *****
C%.I 1025 NOVA
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : DPXTS                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : STAT => STAT IS INTERPRETTED AND THE MICROCODE           *
C  *                          PROGRAM IS LOADED AND STARTED IF STAT=0          *
C  *                          THE THE AP IS CONTINUED IF STAT=1                *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THIS IS THE DRIVER FOR THE DATA PAD MOVING INVERSE       *
C  *                  PATTERN TEST                                             *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE DPXTS(NTEST)
C:
C
C------ COMMON STORAGE:
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C
C------ LOCAL STORAGE:
C
C%.I 1051 NOVA
        INTEGER CONT,SP(16),SPAD
        DATA CONT/8192/,SP/16*0/,SPAD/6/
C:
C
C====== TEST FOR A CONTINUE AFTER AN ERROR
C
        IF(STAT.EQ.0) GOTO 10
        IF(LOOP.EQ.ON) CALL APDEP(0,SPAD,15)
        CALL APOUT(CONT,2)
        GOTO 40
C
C====== TEST FOR NAMES PRINT
C
10      IF(NAMES.EQ.OFF) GOTO 30
        WRITE(ITTO,20)NTEST
        IF(ECHO.NE.0) WRITE(FUNIT,20)NTEST
20      FORMAT(1X,'(',I2,') DATA PAD X MOVING INVERSE TEST')
C
C====== MAKE SURE THE AP IS STOPPED
C
30      CALL APRSET
C
C====== LOAD MD TEST MICRO CODE
C
        CALL FMGR(11)
C
C====== START THE AP
C
        SP(16)=LOOP
        CALL SPLDGO(SP,16,16,4095)
C
C====== WAIT FOR THE AP TO HALT OR OPERATOR INTERVENTION
C
40      CALL TSTRUN(I)
        IF(I.EQ.1) GOTO 50
        CALL TERM(TIP)
        IF(TIP.EQ.OFF) GOTO 40
C
C====== THE OPERATOR HAS INTERRUPTED STOP THE AP AND RETURN
C
        CALL APRSET
        RETURN
C
C====== IF THE AP HALTS CALL THE ERROR CHECK ROUTINE
C
50      CALL DPERR(2)
C
        RETURN
        END
C****** DPYTS = DATA PAD Y MOVING INVERSE PATTERN TEST DRIVE= REL 5.0  , NOV 79
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : DPYTS                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : STAT => STAT IS INTERPRETTED AND THE MICROCODE           *
C  *                          PROGRAM IS LOADED AND STARTED IF STAT=0          *
C  *                          THE THE AP IS CONTINUED IF STAT=1                *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THIS IS THE DRIVER FOR THE DATA PAD Y MOVING INVERSE     *
C  *                  PATTERN TEST                                             *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE DPYTS(NTEST)
C:
C
C------ COMMON STORAGE:
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C
C------ LOCAL STORAGE:
C
C%.I 1167 NOVA
        INTEGER CONT,SP(16),SPAD
        DATA CONT/8192/,SP/16*0/,SPAD/6/
C:
C
C====== TEST FOR A CONTINUE AFTER AN ERROR
C
        IF(STAT.EQ.0) GOTO 10
        IF(LOOP.EQ.ON) CALL APDEP(0,SPAD,15)
        CALL APOUT(CONT,2)
        GOTO 40
C
C====== TEST FOR NAMES PRINT
C
10      IF(NAMES.EQ.OFF) GOTO 30
        WRITE(ITTO,20)NTEST
        IF(ECHO.NE.0) WRITE(FUNIT,20)NTEST
20      FORMAT(1X,'(',I2,') DATA PAD Y MOVING INVERSE TEST')
C
C====== MAKE SURE THE AP IS STOPPED
C
30      CALL APRSET
C
C====== LOAD MD TEST MICRO CODE
C
        CALL FMGR(12)
C
C====== START THE AP
C
        SP(16)=LOOP
        CALL SPLDGO(SP,16,16,4095)
C
C====== WAIT FOR THE AP TO HALT OR OPERATOR INTERVENTION
C
40      CALL TSTRUN(I)
        IF(I.EQ.1) GOTO 50
        CALL TERM(TIP)
        IF(TIP.EQ.OFF) GOTO 40
C
C====== THE OPERATOR HAS INTERRUPTED STOP THE AP AND RETURN
C
        CALL APRSET
        RETURN
C
C====== IF THE AP HALTS CALL THE ERROR CHECK ROUTINE
C
50      CALL DPERR(1)
C
        RETURN
        END
C****** DPERR = DATA PAD TEST ERROR PROCESSOR = REL 5.0  , NOV 79 *************
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : DPERR                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : ERROR CODE IS READ FROM THE AP LITES REGISTER            *
C  *                                                                           *
C  *    EXIT        : ERR   =>ERROR FLAG                                       *
C  *                                                                           *
C  *    FUNCTION    : INTERPRETS THE ERROR CODE IN THE LITES REGISTER          *
C  *                  FROM THE AP AND PRINTS ERROR MESSAGES AND MEMORY         *
C  *                  AND REGISTER INFORMATION                                 *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE DPERR(ERRTYP)
C:
C
C------COMMON STORAGE:
C
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C------LOCAL STORAGE:
C
C%.I 1283 NOVA
        INTEGER IEXP(3),ACT(3),LINE(20),DP,DPA,DPX,DPY,BLANK,
     +          ADDR(6),ERRTYP
        DATA    DPA/5/,DPX/12/,DPY/13/,BLANK/1H /
C
C
C:
        STAT=1
        NERR=OFF
C
C====== GET TEST RETURN CODE (-1) IS GOOD RETURN
C
        CALL ILITES(LITE)
C
        IF(LITE.NE.-1) GOTO 10
C
C====== PROPER RETURN CODE - SET STAT TO INDICATE TEST COMPLETE
C
        STAT=2
        RETURN
C
C====== AN ERROR HAS BEEN DETECTED - PRINT ERROR INFORMATION
C
10      NERR=ON
C
C====== TEST FOR NO ERROR PRINTOUT
C
        IF(TD.EQ.ON) RETURN
C
C====== GET DPA AND SUBTRACT 4 TO GET DPX AND DPY (-4) FOR EXP AND ACT
C
        CALL APEXAM(DP,DPA,0)
C
C====== GET DPX AND DPY
C
        CALL APEXAM(IEXP,DPX,DP-4)
        CALL APEXAM(ACT,DPY,DP-4)
C
C====== PRINT  ERROR MESSAGE
C
        GOTO(20,40),ERRTYP
20      WRITE(ITTO,30)LITE
        IF(ECHO.NE.0) WRITE(FUNIT,30)LITE
30      FORMAT(1X,'MEM100 - DATA PAD X MOVING INVERSE TEST - ERROR=',I6)
        GOTO 60
C
C
40      WRITE(ITTO,50)LITE
        IF(ECHO.NE.0) WRITE(FUNIT,50)LITE
50      FORMAT(1X,'MEM100 - DATA PAD Y MOVING INVERSE TEST - ERROR=',I6)
        GOTO 60
C
C====== PRINT EXPECTED AND ACTUAL INFORMATION
C
60      CALL FILL(LINE,1,20,BLANK)
C
        CALL I2ASCI(6,DP,ADDR,IRADX,0)
        DO 70 I=1,3
        J=I*7-6
70      CALL I2ASCI(6,IEXP(I),LINE(J),IRADX,0)
C
        WRITE(ITTO,80)ADDR,LINE
        IF(ECHO.NE.0) WRITE(FUNIT,80)ADDR,LINE
80      FORMAT(1X,9HADDRESS= ,6A1,4X,2HE ,20A1)
C
        DO 90 I=1,3
        J=I*7-6
90      CALL I2ASCI(6,ACT(I),LINE(J),IRADX,0)
        WRITE(ITTO,100) LINE
        IF(ECHO.NE.0) WRITE(FUNIT,100) ADDR,LINE
100     FORMAT(1X,19X,2HA ,20A1)
        RETURN
        END
C****** PSTST = PROGRAM SOURCE MOVING INVERSE PATTERN TEST D= REL 5.0  , NOV 79
C%.I 1400 NOVA
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PSTST                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : STAT => STAT IS INTERPRETTED AND THE MICROCODE           *
C  *                          PROGRAM IS LOADED AND STARTED IF STAT=0          *
C  *                          THE THE AP IS CONTINUED IF STAT=1                *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THIS IS THE DRIVER FOR THE PROGRAM SOURCE MOVING INVERSE *
C  *                  PATTERN TEST                                             *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE PSTST(NTEST)
C:
C
C------ COMMON STORAGE:
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C
C------ LOCAL STORAGE:
C
C%.I 1422 NOVA
        INTEGER SP(16),CONT,PAGE,SPAD
        DATA CONT/8192/,SPAD/6/
C:
C
C====== TEST FOR A CONTINUE AFTER AN ERROR
C
        IF(STAT.EQ.0) GOTO 10
        IF(LOOP.EQ.ON) CALL APDEP(0,SPAD,15)
        CALL APOUT(CONT,2)
        GOTO 60
C
C====== TEST FOR ANY PS SELECTED
C
10      IF(PSHI.GE.256) GOTO 20
        STAT=2
        RETURN
C
C====== TEST FOR NAMES PRINT
C
20      IF(NAMES.EQ.OFF) GOTO 40
        WRITE(ITTO,30)NTEST
        IF(ECHO.NE.0) WRITE(FUNIT,30)NTEST
30      FORMAT(1X,1H(,I2,2H) ,34HPROGRAM SOURCE MOVING INVERSE TEST)
C
C====== MAKE SURE THE AP IS STOPPED
C
40      CALL APRSET
C
C====== LOAD PS TEST MICRO CODE
C
        CALL FMGR(13)
50      CONTINUE
C
C====== SET UP CALLING PARAMETERS
C
        SP(2)=PSHI
        SP(3)=PSLO
        IF(PSLO.LT.256) SP(3)=256
        SP(16)=LOOP
C
C====== START THE AP
C
        CALL SPLDGO(SP,16,16,4095)
C
C====== WAIT FOR THE AP TO HALT OR OPERATOR INTERVENTION
C
60      CALL TSTRUN(I)
        IF(I.EQ.1) GOTO 70
        CALL TERM(TIP)
        IF(TIP.EQ.OFF) GOTO 60
C
C====== THE OPERATOR HAS INTERRUPTED STOP THE AP AND RETURN
C
        CALL APRSET
        RETURN
C
C====== IF THE AP HALTS CALL ERROR CHECK ROUTINE
C
70      CALL PSERR
C
        RETURN
        END
C****** PSERR = PROGRAM SOURCE TEST ERROR PROCESSOR = REL 5.0  , NOV 79 *******
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PSERR                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : ERROR CODE IS READ FROM THE AP LITES REGISTER            *
C  *                                                                           *
C  *    EXIT        : ERR   =>ERROR FLAG                                       *
C  *                                                                           *
C  *    FUNCTION    : INTERPRETS THE ERROR CODE IN THE LITES REGISTER          *
C  *                  FROM THE AP AND PRINTS ERROR MESSAGES AND MEMORY         *
C  *                  AND REGISTER INFORMATION                                 *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE PSERR
C:
C
C------COMMON STORAGE:
C
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C------LOCAL STORAGE:
C
C%.I 1551 NOVA
        INTEGER TMA,DPA,DPX,DPY,PSA,ADDR(6),MINCD,MAXCD,PS,DP,SPAD
        INTEGER IEXP(4),ACT(4),EXP1(3),EXP2(3),ACT1(3),ACT2(3),LINE(27)
        INTEGER BLANK
        DATA    TMA/4/,DPA/5/,DPX/12/,DPY/13/,PSA/1/,SPAD/6/
        DATA    BLANK/1H /  ,MINCD/1/,MAXCD/8/
C
C
C:
        STAT=1
        NERR=OFF
        CALL FILL(LINE,1,27,BLANK)
C
C====== GET TEST RETURN CODE (-1) IS GOOD RETURN
C
        CALL ILITES(LITE)
C
        IF(LITE.NE.-1) GOTO 10
C
C====== PROPER RETURN CODE - SET STAT TO INDICATE TEST COMPLETE
C
        STAT=2
        RETURN
C
C====== AN ERROR HAS BEEN DETECTED - PRINT ERROR INFORMATION
C
10      NERR=ON
C
C====== TEST FOR NO ERROR PRINTOUT
C
        IF(TD.EQ.ON) RETURN
C
C====== CHECK FOR VALID RETURN CODE
C
        IF(LITE.GE.MINCD.AND.LITE.LE.MAXCD) GOTO 30
C
C====== INVALID CODE - UNKNOWN ERROR
C
        CALL APEXAM(PS,PSA,0)
        CALL I2ASCI(6,PS,ADDR,IRADX,0)
C
        WRITE(ITTO,20)LITE,ADDR
        IF(ECHO.NE.0) WRITE(FUNIT,20)LITE,ADDR
20      FORMAT(1X,19HINVALID ERROR CODE=,I6/
     +  1X,11HAP STOP AT ,6A1)
        RETURN
C
C====== GET FAILING ADDRESS
C
30      CALL APEXAM(PS,TMA,0)
C
C====== GET DPA FOR READING EXP AND ACT
C
        CALL APEXAM(DP,DPA,0)
C
C====== GET EXPECTED AND ACTUAL DATA ACCORDING TO ERROR CODE
C
        IF(LITE.GT.2) GOTO 40
        IDPA=DP+1
        IDPE=DP-3
        GOTO 70
40      IF(LITE.GT.4) GOTO 50
        IDPA=DP+2
        IDPE=DP-2
        GOTO 70
50      IF(LITE.GT.6) GOTO 60
        IDPA=DP+1
        IDPE=DP-2
        GOTO 70
60      IDPA=DP+2
        IDPE=DP-3
C
C====== GET ACTUAL DATA
C
70      CALL APEXAM(ACT1,DPY,IDPA)
        CALL APEXAM(ACT2,DPX,IDPA)
C
C====== GET EXPECTED DATA
C
        CALL APEXAM(EXP2,DPX,IDPE)
        CALL APEXAM(EXP1,DPY,IDPE)
C
C====== CREATE 64 BIT EXP AND ACT
C
80      IEXP(4)=EXP2(3)
        IEXP(3)=EXP2(2)+ILSH16(EXP2(1),12)
        IEXP(2)=EXP1(3)
        IEXP(1)=EXP1(2)+ILSH16(EXP1(1),12)
C
        ACT(4)=ACT2(3)
        ACT(3)=ACT2(2)+ILSH16(ACT2(1),12)
        ACT(2)=ACT1(3)
        ACT(1)=ACT1(2)+ILSH16(ACT1(1),12)
C
C====== PRINT MESSAGE
C
        WRITE(ITTO,90)LITE
        IF(ECHO.NE.0) WRITE(FUNIT,90)LITE
90      FORMAT(1X,40HMEM100 - PS MOVING INVERSE TEST - ERROR=,I6)
C
        CALL I2ASCI(6,PS,ADDR,IRADX,0)
C
C====== PRINT ADDRESS AND EXPECTED
C
        DO 100 I=1,4
        J=I*7-6
100     CALL I2ASCI(6,IEXP(I),LINE(J),IRADX,0)
C
        WRITE(ITTO,110)ADDR,LINE
        IF(ECHO.NE.0) WRITE(FUNIT,110)ADDR,LINE
110     FORMAT(1X,9HADDRESS= ,6A1,3X,2HE ,27A1)
C
C====== PRINT ACTUAL
C
        DO 120 I=1,4
        J=I*7-6
120     CALL I2ASCI(6,ACT(I),LINE(J),IRADX,0)
C
        WRITE(ITTO,130)LINE
        IF(ECHO.NE.0) WRITE(FUNIT,130)LINE
130     FORMAT(1X,18X,2HA ,27A1)
C
        RETURN
        END
C****** SPTST = SPAD EXERCISE TEST = REL 5.0  , NOV 79 ************************
C%.I 1722 NOVA
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : SPTST                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : STAT => IF STAT IS ZERO THEN THE TEST IS STARTED         *
C  *                  ELSE THE TEST IS CONTINUED FROM THE LAST ERROR STOP      *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : CONTROLLER FOR SPAD EXERCISE TESTS                       *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE SPTST(NTEST)
C:
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
        INTEGER ITTI,ITTO,FUNIT
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
        INTEGER START,RET,IEND
C:
C
C====== CHECK FOR A CONTINUE OF TEST
C
        IF(STAT.NE.0) GOTO 190
C
C====== TEST FOR NAMES PRINT
C
        IF(NAMES.EQ.OFF) GOTO 20
        WRITE(ITTO,10)NTEST
        IF(ECHO.NE.0) WRITE(FUNIT,10)NTEST
10      FORMAT(1X,'(',I2,') SPAD EXCERCISE TEST')
C
C====== MAKE SURE THE AP IS STOPPED
C
20      CALL APRSET
C
C====== SPAD(0)
C
        RET=1
C               DPXLP PUTS THE LOOP FLAG IN DPX(0)
        CALL DPXLP
        CALL FMGR(14)
        GOTO 190
C
C====== SPAD(1)
C
30      RET=2
        CALL DPXLP
        CALL APRSET
        STAT=0
        CALL FMGR(15)
        GOTO 190
C
C====== SPAD(2)
C
40      RET=3
        CALL DPXLP
        CALL APRSET
        STAT=0
        CALL FMGR(16)
        GOTO 190
C
C====== SPAD(3)
C
50      CONTINUE
        STAT=0
        RET=4
        CALL DPXLP
        CALL APRSET
        CALL FMGR(17)
        GOTO 190
C
C====== SPAD(4)
C
60      RET=5
        CALL DPXLP
        CALL APRSET
        STAT=0
        CALL FMGR(18)
        GOTO 190
C
C====== SPAD(5)
C
70      RET=6
        CALL DPXLP
        CALL APRSET
        STAT=0
        CALL FMGR(19)
        GOTO 190
C
C====== SPAD(6)
C
80      RET=7
        CALL DPXLP
        CALL APRSET
        STAT=0
        CALL FMGR(20)
        GOTO 190
C
C====== SPAD(7)
C
90      RET=8
        CALL DPXLP
        CALL APRSET
        STAT=0
        CALL FMGR(21)
        GOTO 190
C
C====== SPAD(10)
C
100     RET=9
        CALL DPXLP
        CALL APRSET
        STAT=0
        CALL FMGR(22)
        GOTO 190
C
C====== SPAD(11)
C
110     RET=10
        CALL DPXLP
        CALL APRSET
        STAT=0
        CALL FMGR(23)
        GOTO 190
C
C====== SPAD(12)
C
120     RET=11
        CALL DPXLP
        CALL APRSET
        STAT=0
        CALL FMGR(24)
        GOTO 190
C
C====== SPAD(13)
C
130     RET=12
        CALL DPXLP
        CALL APRSET
        STAT=0
        CALL FMGR(25)
        GOTO 190
C
C====== SPAD(14)
C
140     RET=13
        CALL DPXLP
        CALL APRSET
        STAT=0
        CALL FMGR(26)
        GOTO 190
C
C====== SPAD(15)
C
150     RET=14
        CALL DPXLP
        CALL APRSET
        STAT=0
        CALL FMGR(27)
        GOTO 190
C
C====== SPAD(16)
C
160     RET=15
        CALL DPXLP
        CALL APRSET
        STAT=0
        CALL FMGR(28)
        GOTO 190
C
C====== SPAD(17)
C
170     RET=16
        CALL DPXLP
        CALL APRSET
        STAT=0
        CALL FMGR(29)
        GOTO 190
C
C====== END OF TEST
C
180     STAT=2
        RETURN
C
C====== CALL TEST
C
190     CONTINUE
        CALL RUNAP(16,STAT,0,8192)
200     CALL TSTRUN(I)
        IF(I.EQ.1) GOTO 210
        CALL TERM(TIP)
        IF(TIP.EQ.OFF) GOTO 200
C
C====== OPERATOR INTERVENTION
C
        CALL APRSET
        RETURN
C
C====== CALL ERROR CHECK ROUTINE
C
210     CALL SPERR(RET)
        STAT=1
        IF(NERR.EQ.ON) RETURN
        GOTO(30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180)
     +  ,RET
        END
C****** SPERR = SPAD TESTS ERROR PROCESSOR = REL 5.0  , NOV 79 ****************
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : SPERR                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : INTERPRETS THE ERROR CODE AND PRINTS EXPECTED AND        *
C  *                  ACTUAL DATA FROM THE AP FOR THE SPAD TESTS.              *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE SPERR(RET)
C:
C
C------COMMON STORAGE:
C
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C------LOCAL STORAGE:
C
C%.I 1969 NOVA
        INTEGER CODE(3),DPA,DPX,TMA,DPY,IEXP,ACT,EXPD(6),ACTD(6),ISPD(2)
        INTEGER NSPD(2),LCODE(3),RET
C
        DATA DPA/5/,DPX/12/,DPY/13/
C
C
C:
C
C====== TEST FOR ERROR CONDITION
C
        NERR=OFF
        CALL APEXAM(IDPA,DPA,0)
        CALL APEXAM(CODE,DPX,IDPA-4)
        IF(IP16(CODE(3)).EQ.IP16(-1)) RETURN
        NERR=ON
        IF(TD.EQ.ON) RETURN
        LITE=CODE(3)
C
C====== GET ERROR TYPE NUMBER
C
        N=IRSH16(IAND16(LITE,IP16(-64)),6)
        IF(N.LT.1.OR.N.GT.6) GOTO 160
C
C====== PRINT ERROR CODE
C
        CALL I2ASCI(3,LITE,LCODE,8,0)
        WRITE(ITTO,10)LCODE
        IF(ECHO.NE.0) WRITE(FUNIT,10)LCODE
10      FORMAT(1X,'MEM100 - SPAD EXERCISE TEST - ERROR= ',3A1)
C
C====== GET SPAD NUMBER
C
        ISP=RET-1
C
C====== GOTO ERROR PROCESSOR
C
        GOTO(20,70,80,90,110,140),N
C
C====== SPAD WONT CLEAR ERROR
C
20      CONTINUE
        IEXP=0
C
C====== ACTUAL DATA IS IN TMA
C
        CALL APEXAM(ACT,TMA,0)
C
C====== FAILING SPAD
C
        NSP=IAND16(LITE,15)
C
30      CALL I2ASCI(2,ISP,ISPD,IRADX,0)
        CALL I2ASCI(2,NSP,NSPD,IRADX,0)
        CALL I2ASCI(6,IEXP,EXPD,IRADX,0)
        CALL I2ASCI(6,ACT,ACTD,IRADX,0)
C
        WRITE(ITTO,40)ISPD
        IF(ECHO.NE.0) WRITE(FUNIT,40)ISPD
40      FORMAT(1X,'TARGET SPAD  = ',2A1/)
        WRITE(ITTO,50)NSPD
        IF(ECHO.NE.0) WRITE(FUNIT,50)NSPD
50      FORMAT(1X,'FAILING SPAD = ',2A1)
        WRITE(ITTO,60)EXPD,ACTD
        IF(ECHO.NE.0) WRITE(FUNIT,60)EXPD,ACTD
60      FORMAT(1X,'E ',6A1/
     +         1X,'A ',6A1//)
        RETURN
C
C====== INCREMENT/DECREMENT ERROR
C
70      CONTINUE
        CALL APEXAM(CODE,DPX,IDPA-3)
        IEXP=CODE(3)
        CALL APEXAM(CODE,DPY,IDPA-3)
        ACT=CODE(3)
C
        CALL I2ASCI(1,ISP,ISPD,IRADX,0)
        CALL I2ASCI(6,IEXP,EXPD,IRADX,0)
        CALL I2ASCI(6,ACT,ACTD,IRADX,0)
C
        WRITE(ITTO,40)ISPD
        IF(ECHO.NE.0) WRITE(FUNIT,40)ISPD
        WRITE(ITTO,60)EXPD,ACTD
        IF(ECHO.NE.0) WRITE(FUNIT,60)EXPD,ACTD
C
        RETURN
C
C====== UNUSED SPAD CHANGED
C
80      CONTINUE
C
C       SAME MESAGE AS 100
C
        GOTO 20
C
C====== COUNTER REGISTER ERROR
C
90      CONTINUE
C
C  COUNTER REGISTER = TARGET + 10 OCTAL
        NSP=ISP+8
        CALL APEXAM(CODE,DPY,IDPA-4)
        ACT=CODE(3)
        CALL APEXAM(IEXP,TMA,0)
C
        CALL I2ASCI(2,ISP,ISPD,IRADX,0)
        CALL I2ASCI(2,NSP,NSPD,IRADX,0)
        CALL I2ASCI(6,IEXP,EXPD,IRADX,0)
        CALL I2ASCI(6,ACT,ACTD,IRADX,0)
C
        WRITE(ITTO,40)ISPD
        IF(ECHO.NE.0) WRITE(FUNIT,40)ISPD
        WRITE(ITTO,100)NSPD
        IF(ECHO.NE.0) WRITE(FUNIT,100)NSPD
100     FORMAT(1X,'COUNTER SPAD = ',2A1)
C
        WRITE(ITTO,60)EXPD,ACTD
        IF(ECHO.NE.0) WRITE(FUNIT,60)EXPD,ACTD
        RETURN
C
C====== INC TARGET / DEC INVERSE  SUMMATION ERROR
C
110     CONTINUE
C
C  INVERSE SPAD = COMPLEMENT OF TARGET
        NSP=IAND16(INOT16(ISP),15)
C
        CALL APEXAM(CODE,DPX,IDPA-2)
        IEXP=CODE(3)
        CALL APEXAM(CODE,DPY,IDPA-2)
        ACT=CODE(3)
C
        CALL I2ASCI(2,ISP,ISPD,IRADX,0)
        CALL I2ASCI(2,NSP,NSPD,IRADX,0)
        CALL I2ASCI(6,IEXP,EXPD,IRADX,0)
        CALL I2ASCI(6,ACT,ACTD,IRADX,0)
C
        WRITE(ITTO,40)ISPD
        IF(ECHO.NE.0) WRITE(FUNIT,40)ISPD
        WRITE(ITTO,120)NSPD
        IF(ECHO.NE.0) WRITE(FUNIT,120)NSPD
120     FORMAT(1X,'INVERSE SPAD = ',2A1)
        WRITE(ITTO,130)ACTD,EXPD
        IF(ECHO.NE.0) WRITE(FUNIT,130)ACTD,EXPD
130     FORMAT(1X,'TARGET SPAD  = ',6A1/
     +         1X,'INVERSE SPAD = ',6A1/)
        RETURN
C
C====== INVERSE REG NOT EQ TO ZERO
C
140     CONTINUE
        IEXP=0
        CALL APEXAM(ACT,TMA,0)
        NSP=IAND16(INOT16(ISP),15)
C
        CALL I2ASCI(2,ISP,ISPD,IRADX,0)
        CALL I2ASCI(2,NSP,NSPD,IRADX,0)
        CALL I2ASCI(6,IEXP,EXPD,IRADX,0)
        CALL I2ASCI(6,ACT,ACTD,IRADX,0)
C
        WRITE(ITTO,40)ISPD
        IF(ECHO.NE.0) WRITE(FUNIT,40)ISPD
        WRITE(ITTO,120)NSPD
        IF(ECHO.NE.0) WRITE(FUNIT,120)NSPD
        WRITE(ITTO,150)EXPD,ACTD
        IF(ECHO.NE.0) WRITE(FUNIT,150)EXPD,ACTD
150     FORMAT(1X,'EXPECTED INVERSE = ',6A1/
     +         1X,'ACTUAL INVERSE   = ',6A1/)
        RETURN
C
C====== INVALID ERROR CODE
C
160     CONTINUE
        CALL I2ASCI(6,LITE,EXPD,8,0)
        WRITE(ITTO,170)EXPD
        IF(ECHO.NE.0) WRITE(FUNIT,170)EXPD
170     FORMAT(1X,'INVALID ERROR CODE= ',6A1)
        RETURN
        END
C****** DPXLP = SET DPX(0) TO LOOP FLAG = REL 5.0  , NOV 79 *******************
        SUBROUTINE DPXLP
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : DPXLP                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : SET DPX(0) TO LOOP FLAG FOR THE SPAD TESTS TO READ       *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C------ LOCAL STORAGE:
C
C%.I 2179 NOVA
        INTEGER DPX,DPA,LPFLG(3)
        DATA DPX/12/,DPA/5/
C
C====== SET DPX(0) TO LOOP
C
        CALL APEXAM(IDPA,DPA,0)
        LPFLG(1)=0
        LPFLG(2)=0
        LPFLG(3)=LOOP
        CALL APDEP(LPFLG,DPX,IDPA)
        RETURN
        END
C****** MDTST = MAIN DATA MOVING INVERSE PATTERN TEST DRIVER= REL 5.0  , NOV 79
C%.I 2244 NOVA
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : MDTST                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : STAT => STAT IS INTERPRETTED AND THE MICROCODE           *
C  *                          PROGRAM IS LOADED AND STARTED IF STAT=0          *
C  *                          THE THE AP IS CONTINUED IF STAT=1                *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THIS IS THE DRIVER FOR THE MAIN DATA MOVING INVERSE      *
C  *                  PATTERN TEST                                             *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE MDTST(NTEST)
C:
C
C------ COMMON STORAGE:
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C
C------ LOCAL STORAGE:
C
C%.I 2258 NOVA
        INTEGER SP(16),CONT,PAGE,PAT(4),INV(4),DPX,DPY,DP,DPA,SPAD
        DATA CONT/8192/,DPX/12/,DPY/13/,DP/5/,SPAD/6/
        DATA INV/1023,4095,-1,0/,PAT/0,0,0,0/
C:
C
C====== TEST FOR A CONTINUE AFTER AN ERROR
C
        IF(STAT.EQ.0) GOTO 10
        CALL APPSEL(0,0,PAGE)
        IF(LOOP.EQ.ON) CALL APDEP(0,SPAD,15)
        CALL APOUT(CONT,2)
        GOTO 70
C
C====== TEST FOR ANY MD SELECTED
C
10      DO 20 I=1,16
        IF(MDHI(I).NE.0.OR.MDLO(I).NE.0) GOTO 30
20      CONTINUE
        STAT=2
        RETURN
C
C====== TEST FOR NAMES PRINT
C
30      IF(NAMES.EQ.OFF) GOTO 50
        WRITE(ITTO,40)NTEST
        IF(ECHO.NE.0) WRITE(FUNIT,40)NTEST
40      FORMAT(1X,1H(,I2,2H) ,29HMAIN DATA MOVING INVERSE TEST)
C
C====== MAKE SURE THE AP IS STOPPED
C
50      CALL APRSET
C
C====== LOAD MD TEST MICRO CODE
C
        CALL FMGR(30)
C
C====== TEST ALL PAGES
C
        PAGE=0
60      IF (MDHI(PAGE+1).EQ.0.AND.MDLO(PAGE+1).EQ.0) GOTO 90
        CALL APRSET
C
C====== SET UP CALLING PARAMETERS
C
        SP(1)=PARITY
        SP(2)=MDHI(PAGE+1)
        SP(3)=MDLO(PAGE+1)
        SP(16)=LOOP
C
C====== LOAD PATTERNS TO DATA PADS
C
        DPA=4
        CALL APDEP(DPA,DP,0)
C
        CALL APDEP(PAT,DPX,DPA-3)
        CALL APDEP(INV,DPX,DPA-2)
C
C====== SET UP PAGE
C
        CALL APPSEL(0,0,PAGE)
C
C====== START THE AP
C
        CALL SPLDGO(SP,16,16,4095)
C
C====== WAIT FOR THE AP TO HALT OR OPERATOR INTERVENTION
C
70      CALL TSTRUN(I)
        IF(I.EQ.1) GOTO 80
        CALL TERM(TIP)
        IF(TIP.EQ.OFF) GOTO 70
C
C====== THE OPERATOR HAS INTERRUPTED STOP THE AP AND RETURN
C
        CALL APRSET
        RETURN
C
C====== IF THE AP HALTS CALL ERROR CHECK ROUTINE
C
80      CALL MDERR(3)
C
C====== IF AN ERROR - RETURN FOR POSSIBLE OPERATOR INPUT
C       ELSE CONTINUE WITH TEST
C
        IF(NERR.EQ.ON) RETURN
90      PAGE=PAGE+1
        IF(PAGE.LE.15) GOTO 60
C
        STAT=2
        RETURN
        END
C****** MDPT1 = MAIN DATA PARITY TEST PART 1 (MOVING INVERS= REL 5.0  , NOV 79
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : MDPT1                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : STAT => STAT IS INTERPRETTED AND THE MICROCODE           *
C  *                          PROGRAM IS LOADED AND STARTED IF STAT=0          *
C  *                          THE THE AP IS CONTINUED IF STAT=1                *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THIS IS THE DRIVER FOR THE MAIN DATA MOVING INVERSE      *
C  *                  PATTERN TEST TO TOGGLE THE PARITY BITS.                  *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE MDPT1(NTEST)
C:
C
C------ COMMON STORAGE:
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C
C------ LOCAL STORAGE:
C
C%.I 2416 NOVA
        INTEGER SP(16),CONT,PAGE,PAT(4),INV(4),DPX,DPY,DP,DPA,SPAD
        DATA CONT/8192/,DPX/12/,DPY/13/,DP/5/,SPAD/6/
        DATA INV/1,1,1,0/,PAT/0,0,0,0/
C:
C
C====== TEST FOR A CONTINUE AFTER AN ERROR
C
        IF(STAT.EQ.0) GOTO 10
        CALL APPSEL(0,0,PAGE)
        IF(LOOP.EQ.ON) CALL APDEP(0,SPAD,15)
        CALL APOUT(CONT,2)
        GOTO 80
C
C====== MAKE SURE THE PARITY OPTION IS SET
C
10      IF(PARITY.EQ.ON) GOTO 20
        STAT=2
        RETURN
C
C====== TEST FOR ANY MD SELECTED
C
20      DO 30 I=1,16
        IF(MDHI(I).NE.0.OR.MDLO(I).NE.0) GOTO 40
30      CONTINUE
        STAT=2
        RETURN
C
C====== TEST FOR NAMES PRINT
C
40      IF(NAMES.EQ.OFF) GOTO 60
        WRITE(ITTO,50)NTEST
        IF(ECHO.NE.0) WRITE(FUNIT,50)NTEST
50      FORMAT(1X,1H(,I2,2H) ,23HMAIN DATA PARITY PART 1)
C
C====== MAKE SURE THE AP IS STOPPED
C
60      CALL APRSET
C
C====== LOAD MD TEST MICRO CODE
C
        CALL FMGR(30)
C
C====== TEST ALL PAGES
C
        PAGE=0
70      IF (MDHI(PAGE+1).EQ.0.AND.MDLO(PAGE+1).EQ.0) GOTO 100
        CALL APRSET
C
C====== SET UP CALLING PARAMETERS
C
        SP(2)=MDHI(PAGE+1)
        SP(3)=MDLO(PAGE+1)
        SP(16)=LOOP
C
C====== LOAD PATTERNS TO DATA PADS
C
        DPA=4
        CALL APDEP(DPA,DP,0)
C
        CALL APDEP(PAT,DPX,DPA-3)
        CALL APDEP(INV,DPX,DPA-2)
C
C====== SET UP PAGE
C
        CALL APPSEL(0,0,PAGE)
C
C====== START THE AP
C
        CALL SPLDGO(SP,16,16,4095)
C
C====== WAIT FOR THE AP TO HALT OR OPERATOR INTERVENTION
C
80      CALL TSTRUN(I)
        IF(I.EQ.1) GOTO 90
        CALL TERM(TIP)
        IF(TIP.EQ.OFF) GOTO 80
C
C====== THE OPERATOR HAS INTERRUPTED STOP THE AP AND RETURN
C
        CALL APRSET
        RETURN
C
C====== IF THE AP HALTS CALL ERROR CHECK ROUTINE
C
90      CALL MDERR(1)
C
C====== IF AN ERROR - RETURN FOR POSSIBLE OPERATOR INPUT
C       ELSE CONTINUE WITH TEST
C
        IF(NERR.EQ.ON) RETURN
100     PAGE=PAGE+1
        IF(PAGE.LE.15) GOTO 70
C
        STAT=2
        RETURN
        END
C****** MDPT2 = MAIN DATA PARITY TEST PART 2 (SHIFTING BIT)= REL 5.0  , NOV 79
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : MDPT2                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : STAT => STAT IS INTERPRETTED AND THE MICROCODE           *
C  *                          PROGRAM IS LOADED AND STARTED IF STAT=0          *
C  *                          THE THE AP IS CONTINUED IF STAT=1                *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THIS IS THE DRIVER FOR THE MAIN DATA SHIFTNG BIT         *
C  *                  PATTERN TEST TO CHECK THAT EVERY BIT IN MEMORY CAN       *
C  *                  GENERATE PARITY.                                         *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE MDPT2(NTEST)
C:
C
C------ COMMON STORAGE:
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C
C------ LOCAL STORAGE:
C
C%.I 2579 NOVA
        INTEGER SP(16),CONT,PAGE,DPX,DPY,DP,DPA,SPAD
        DATA CONT/8192/,DPX/12/,DPY/13/,DP/5/,SPAD/6/
C:
C
C====== TEST FOR A CONTINUE AFTER AN ERROR
C
        IF(STAT.EQ.0) GOTO 10
        CALL APPSEL(0,0,PAGE)
        IF(LOOP.EQ.ON) CALL APDEP(0,SPAD,15)
        CALL APOUT(CONT,2)
        GOTO 80
C
C====== MAKE SURE THE PARITY OPTION IS SET
C
10      IF(PARITY.EQ.ON) GOTO 20
        STAT=2
        RETURN
C
C====== TEST FOR ANY MD SELECTED
C
20      DO 30 I=1,16
        IF(MDHI(I).NE.0.OR.MDLO(I).NE.0) GOTO 40
30      CONTINUE
        STAT=2
        RETURN
C
C====== TEST FOR NAMES PRINT
C
40      IF(NAMES.EQ.OFF) GOTO 60
        WRITE(ITTO,50)NTEST
        IF(ECHO.NE.0) WRITE(FUNIT,50)NTEST
50      FORMAT(1X,1H(,I2,2H) ,23HMAIN DATA PARITY PART 2)
C
C====== MAKE SURE THE AP IS STOPPED
C
60      CALL APRSET
C
C====== LOAD MD TEST MICRO CODE
C
        CALL FMGR(31)
C
C====== TEST ALL PAGES
C
        PAGE=0
70      IF (MDHI(PAGE+1).EQ.0.AND.MDLO(PAGE+1).EQ.0) GOTO 100
        CALL APRSET
C
C====== SET UP CALLING PARAMETERS
C
        SP(2)=MDHI(PAGE+1)
        SP(3)=MDLO(PAGE+1)
        SP(16)=LOOP
C
C====== SET UP PAGE
C
        CALL APPSEL(0,0,PAGE)
C
C====== START THE AP
C
        CALL SPLDGO(SP,16,16,4095)
C
C====== WAIT FOR THE AP TO HALT OR OPERATOR INTERVENTION
C
80      CALL TSTRUN(I)
        IF(I.EQ.1) GOTO 90
        CALL TERM(TIP)
        IF(TIP.EQ.OFF) GOTO 80
C
C====== THE OPERATOR HAS INTERRUPTED STOP THE AP AND RETURN
C
        CALL APRSET
        RETURN
C
C====== IF THE AP HALTS CALL ERROR CHECK ROUTINE
C
90      CALL MDERR(2)
C
C====== IF AN ERROR - RETURN FOR POSSIBLE OPERATOR INPUT
C       ELSE CONTINUE WITH TEST
C
        IF(NERR.EQ.ON) RETURN
100     PAGE=PAGE+1
        IF(PAGE.LE.15) GOTO 70
C
        STAT=2
        RETURN
        END
C****** MTST0 = MAIN DATA ADDRESS TAG PATTERN TEST DRIVER = REL 5.0  , NOV 79 *
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : MTST0                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       :                                                          *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THIS IS THE DRIVER FOR THE MAIN DATA ADDRESS TAG         *
C  *                  PATTERN TEST                                             *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE MTST0(NTEST)
C:
C
C------ COMMON STORAGE:
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C
C------ LOCAL STORAGE:
C
C%.I 2731 NOVA
        INTEGER SP(16),CONT,PAGE,DPX,DPY,DP,DPA
        DATA CONT/8192/,DPX/12/,DPY/13/,DP/5/
C:
C
C====== TEST FOR A CONTINUE AFTER AN ERROR
C
        IF(STAT.EQ.0) GOTO 10
        CALL APPSEL(0,0,PAGE)
        CALL APOUT(CONT,2)
        GOTO 70
C
C====== TEST FOR ANY MD SELECTED
C
10      DO 20 I=1,16
        IF(MDHI(I).NE.0.OR.MDLO(I).NE.0) GOTO 30
20      CONTINUE
        STAT=2
        RETURN
C
C====== TEST FOR NAMES PRINT
C
30      IF(NAMES.EQ.OFF) GOTO 50
        WRITE(ITTO,40)NTEST
        IF(ECHO.NE.0) WRITE(FUNIT,40)NTEST
40      FORMAT(1X,1H(,I2,2H) ,29HMAIN DATA ADDRESS TAG    TEST)
C
C====== MAKE SURE THE AP IS STOPPED
C
50      CALL APRSET
C
C====== LOAD MD TEST MICRO CODE (BADD)
C
        CALL FMGR(32)
C
C====== TEST ALL PAGES
C
        PAGE=0
60      IF (MDHI(PAGE+1).EQ.0.AND.MDLO(PAGE+1).EQ.0) GOTO 90
        CALL APRSET
C
C====== SET UP CALLING PARAMETERS
C
        SP(1)=MDLO(PAGE+1)
        SP(2)=MDHI(PAGE+1)-MDLO(PAGE+1)+1
        SP(16)=LOOP
C
C====== SET UP PAGE
C
        CALL APPSEL(0,0,PAGE)
C
C====== START THE AP
C
        CALL SPLDGO(SP,16,16,4095)
C
C====== WAIT FOR THE AP TO HALT OR OPERATOR INTERVENTION
C
70      CALL TSTRUN(I)
        IF(I.EQ.1) GOTO 80
        CALL TERM(TIP)
        IF(TIP.EQ.OFF) GOTO 70
C
C====== THE OPERATOR HAS INTERRUPTED STOP THE AP AND RETURN
C
        CALL APRSET
        RETURN
C
C====== IF THE AP HALTS CALL ERROR CHECK ROUTINE
C
80      CALL MDERR(4)
C
C====== IF AN ERROR - RETURN FOR POSSIBLE OPERATOR INPUT
C       ELSE CONTINUE WITH TEST
C
        IF(NERR.EQ.ON) RETURN
90      PAGE=PAGE+1
        IF(PAGE.LE.15) GOTO 60
C
        STAT=2
        RETURN
        END
C****** MTST1 = MAIN DATA RANDOM DATA PATTERN TEST DRIVER = REL 5.0  , NOV 79 *
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : MTST1                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       :                                                          *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THIS IS THE DRIVER FOR THE MAIN DATA RANDOM DATA         *
C  *                  PATTERN TEST                                             *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE MTST1(NTEST)
C:
C
C------ COMMON STORAGE:
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C
C------ LOCAL STORAGE:
C
C%.I 2876 NOVA
        INTEGER SP(16),CONT,PAGE,DPX,DPY,DP,DPA
        DATA CONT/8192/,DPX/12/,DPY/13/,DP/5/
C:
C
C====== TEST FOR A CONTINUE AFTER AN ERROR
C
        IF(STAT.EQ.0) GOTO 10
        CALL APPSEL(0,0,PAGE)
        CALL APOUT(CONT,2)
        GOTO 70
C
C====== TEST FOR ANY MD SELECTED
C
10      DO 20 I=1,16
        IF(MDHI(I).NE.0.OR.MDLO(I).NE.0) GOTO 30
20      CONTINUE
        STAT=2
        RETURN
C
C====== TEST FOR NAMES PRINT
C
30      IF(NAMES.EQ.OFF) GOTO 50
        WRITE(ITTO,40)NTEST
        IF(ECHO.NE.0) WRITE(FUNIT,40)NTEST
40      FORMAT(1X,1H(,I2,2H) ,29HMAIN DATA RANDOM DATA TEST   )
C
C====== MAKE SURE THE AP IS STOPPED
C
50      CALL APRSET
C
C====== LOAD MD TEST MICRO CODE (RMTST)
C
        CALL FMGR(33)
C
C====== TEST ALL PAGES
C
        PAGE=0
60      IF (MDHI(PAGE+1).EQ.0.AND.MDLO(PAGE+1).EQ.0) GOTO 90
        CALL APRSET
C
C====== SET UP CALLING PARAMETERS
C
        SP(1)=MDLO(PAGE+1)
        SP(2)=MDHI(PAGE+1)-MDLO(PAGE+1)+1
        SP(16)=LOOP
C
C====== SET UP PAGE
C
        CALL APPSEL(0,0,PAGE)
C
C====== START THE AP
C
        CALL SPLDGO(SP,16,16,4095)
C
C====== WAIT FOR THE AP TO HALT OR OPERATOR INTERVENTION
C
70      CALL TSTRUN(I)
        IF(I.EQ.1) GOTO 80
        CALL TERM(TIP)
        IF(TIP.EQ.OFF) GOTO 70
C
C====== THE OPERATOR HAS INTERRUPTED STOP THE AP AND RETURN
C
        CALL APRSET
        RETURN
C
C====== IF THE AP HALTS CALL ERROR CHECK ROUTINE
C
80      CALL MDERR(5)
C
C====== IF AN ERROR - RETURN FOR POSSIBLE OPERATOR INPUT
C       ELSE CONTINUE WITH TEST
C
        IF(NERR.EQ.ON) RETURN
90      PAGE=PAGE+1
        IF(PAGE.LE.15) GOTO 60
C
        STAT=2
        RETURN
        END
C****** MDERR = MAIN DATA TEST ERROR PROCESSOR = REL 5.0  , NOV 79 ************
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : MDERR                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : ERROR CODE IS READ FROM THE AP LITES REGISTER            *
C  *                                                                           *
C  *    EXIT        : ERR   =>ERROR FLAG                                       *
C  *                                                                           *
C  *    FUNCTION    : INTERPRETS THE ERROR CODE IN THE LITES REGISTER          *
C  *                  FROM THE AP AND PRINTS ERROR MESSAGES AND MEMORY         *
C  *                  AND REGISTER INFORMATION                                 *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE MDERR(ERRTYP)
C:
C
C------COMMON STORAGE:
C
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C------LOCAL STORAGE:
C
C%.I 3023 NOVA
        INTEGER PRTYBT,IEXP(3),ACT(3),LINE(20),DP,DPA,DPX,DPY,BLANK,
     +          ADDR(6),SPAD,ERRTYP,EXPON,MDA(4),MAE(4)
        DATA    MA/3/,DPA/5/,DPX/12/,DPY/13/,BLANK/1H /,ME/30/,SPAD/6/
C
C
C:
        STAT=1
        NERR=OFF
C
C====== GET TEST RETURN CODE (-1) IS GOOD RETURN
C
        CALL ILITES(LITE)
C
        IF(LITE.NE.-1) GOTO 10
C
C====== PROPER RETURN CODE - SET STAT TO INDICATE TEST COMPLETE
C
        STAT=2
        RETURN
C
C====== AN ERROR HAS BEEN DETECTED - PRINT ERROR INFORMATION
C
10      NERR=ON
C
C====== TEST FOR NO ERROR PRINTOUT
C
        IF(TD.EQ.ON) RETURN
C
C====== GET EXPECTED AND ACTUAL DATA AND ADDRESS
C       MD ADDRESS REG (MA)
C
        CALL APEXAM(MDA,MA,0)
C
C====== MD EXTENSION (MAE)
C
        CALL APEXAM(MAE,ME,0)
C
C====== GET DPA AND SUBTRACT 4 TO GET DPX AND DPY (-4) FOR EXP AND ACT
C
        CALL APEXAM(DP,DPA,0)
        DP=DP-4
C
C====== GET DPX AND DPY
C
        CALL APEXAM(IEXP,DPX,DP)
        CALL APEXAM(ACT,DPY,DP)
C
C====== PRINT PROPPER ERROR MESSAGE
C       CODE 1 - 4 : DATA ERROR
C       CODE 8 - 11: PARITY ERROR
C
C
C====== ERRTYP = 1 FOR MD PARITY PART 1
C       ERRTYP = 2 FOR MD PARITY PART 2
C       ERRTYP = 3 FOR MD MOVING INVERSE
C       ERRTYP = 4 FOR MD ADDRESS TAG TEST
C       ERRTYP = 5 FOR MD RANDOM PATTERN
C
        GOTO (20,20,40,60,80),ERRTYP
20      WRITE(ITTO,30)ERRTYP,LITE
        IF(ECHO.NE.0) WRITE(FUNIT,30)ERRTYP,LITE
30      FORMAT(1X,28HMEM100 - MD PARITY TEST PART,I2,9H - ERROR=,I6)
        GOTO 100
C
C
40      WRITE(ITTO,50)LITE
        IF(ECHO.NE.0) WRITE(FUNIT,50)LITE
50      FORMAT(1X,47HMEM100 - MAIN DATA MOVING INVERSE TEST - ERROR=,I6)
        GOTO 100
C
C
60      WRITE(ITTO,70)LITE
        IF(ECHO.NE.0) WRITE(FUNIT,70)LITE
70      FORMAT(1X,39HMEM100 - MAIN DATA ADDRESS TAG - ERROR=,I6)
        GOTO 100
C
C
80      WRITE(ITTO,90)LITE
        IF(ECHO.NE.0) WRITE(FUNIT,90)LITE
90      FORMAT(1X,43HMEM100 - MAIN DATA RANDOM PATTERNS - ERROR=,I6)
C
C
100     IF(LITE.GT.4) GOTO 120
        WRITE(ITTO,110)
        IF(ECHO.NE.0) WRITE(FUNIT,110)
110     FORMAT(1X,10HDATA ERROR)
        GOTO 140
120     WRITE(ITTO,130)
130     FORMAT(1X,12HPARITY ERROR)
C
C====== PRINT EXPECTED AND ACTUAL INFORMATION
C
140     CALL FILL(LINE,1,20,BLANK)
C
        CALL I2ASCI(6,MDA,ADDR,IRADX,0)
        DO 150 I=1,3
        J=I*7-6
150     CALL I2ASCI(6,IEXP(I),LINE(J),IRADX,0)
C
        WRITE(ITTO,160)ADDR,LINE
        IF(ECHO.NE.0) WRITE(FUNIT,160)ADDR,LINE
160     FORMAT(1X,9HADDRESS= ,6A1,4X,2HE ,20A1)
C
        CALL I2ASCI(6,MAE,ADDR,IRADX,0)
        DO 170 I=1,3
        J=I*7-6
170     CALL I2ASCI(6,ACT(I),LINE(J),IRADX,0)
        WRITE(ITTO,180) ADDR,LINE
        IF(ECHO.NE.0) WRITE(FUNIT,180) ADDR,LINE
180     FORMAT(1X,4HMAE=,5X,6A1,4X,2HA ,20A1)
C
C====== RETURN IF NOT A PARITY ERROR, ELSE PRINT PARITY REGISTERS
C
        IF(LITE.LE.4.OR.LITE.GT.11) RETURN
C
C====== GET PARITY REGISTERS STORED IN SPADS
C
        CALL APGSP(LMN,9)
        CALL APGSP(HMN,10)
        CALL APGSP(EXPON,11)
        CALL APGSP(MADR,12)
        CALL APGSP(MDCA,13)
C
        MD00=IAND16(IRSH16(EXPON,12),1)
        MD01=IAND16(IRSH16(EXPON,11),1)
        MD04=IAND16(IRSH16(EXPON,10),1)
        EXPON=IAND16(EXPON,1023)
        MAE(1)=IAND16(MDCA,15)
        MDCA=IRSH16(MDCA,4)
C
        WRITE(ITTO,190)
        IF(ECHO.NE.0) WRITE(FUNIT,190)
190     FORMAT(1X,22HPARITY REGISTER VALUES)
C
        CALL I2ASCI(6,MADR,ADDR,IRADX,0)
        CALL I2ASCI(6,EXPON,LINE(1),IRADX,0)
        CALL I2ASCI(6,HMN,LINE(8),IRADX,0)
        CALL I2ASCI(6,LMN,LINE(15),IRADX,0)
C
        WRITE(ITTO,200) ADDR,LINE
        IF(ECHO.NE.0) WRITE(FUNIT,200)ADDR,LINE
200     FORMAT(1X,9HADDRESS= ,6A1,6H DATA ,20A1)
C
        CALL I2ASCI(6,MAE,LINE(1),IRADX,0)
        CALL I2ASCI(6,MDCA,LINE(8),IRADX,0)
C
        WRITE(ITTO,210)(LINE(J),J=1,14)
        IF(ECHO.NE.0) WRITE(FUNIT,210)(LINE(J),J=1,14)
210     FORMAT(1X,5HMAE= ,7A1,12H MDCA BITS= ,7A1)
C
        WRITE(ITTO,220)MD00,MD01,MD04
        IF(ECHO.NE.0) WRITE(FUNIT,220)MD00,MD01,MD04
220     FORMAT(1X,17HPARITY BITS = EXP,I2/
     +            15X,3HHMN,I2/
     +            15X,3HLMN,I2//)
C
C====== RESET AP TO CLEAR PARITY BIT AND RETURN
C
        CALL APRSET
C
        RETURN
        END
C****** TMTST = TM RAM MOVING INVERSE PATTERN TEST DRIVER = REL 5.0  , NOV 79 *
C%.I 3245 NOVA
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TMTST                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : STAT => STAT IS INTERPRETTED AND THE MICROCODE           *
C  *                          PROGRAM IS LOADED AND STARTED IF STAT=0          *
C  *                          THE THE AP IS CONTINUED IF STAT=1                *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THIS IS THE DRIVER FOR THE   TM  RAM MOVING INVERSE      *
C  *                  PATTERN TEST                                             *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE TMTST(NTEST)
C:
C
C------ COMMON STORAGE:
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C
C------ LOCAL STORAGE:
C
C%.I 3252 NOVA
        INTEGER SP(16),CONT,PAGE,SPAD
        DATA CONT/8192/,SPAD/6/
C:
C
C====== TEST FOR A CONTINUE AFTER AN ERROR
C
        IF(STAT.EQ.0) GOTO 10
        IF(LOOP.EQ.ON) CALL APDEP(0,SPAD,15)
        CALL APOUT(CONT,2)
        GOTO 50
C
C====== TEST FOR ANY TMRAM
C
10      IF(TMLO.NE.0.OR.TMHI.NE.0) GOTO 20
        STAT=2
        RETURN
C
C====== TEST FOR NAMES PRINT
C
20      IF(NAMES.EQ.OFF) GOTO 40
        WRITE(ITTO,30)NTEST
        IF(ECHO.NE.0) WRITE(FUNIT,30)NTEST
30      FORMAT(1X,1H(,I2,2H) ,26HTM RAM MOVING INVERSE TEST)
C
C====== MAKE SURE THE AP IS STOPPED
C
40      CALL APRSET
C
C====== LOAD MD TEST MICRO CODE
C
        CALL FMGR(34)
C
C====== SET UP CALLING PARAMETERS
C
        SP(2)=TMHI
        SP(3)=TMLO
        SP(16)=LOOP
C
C====== START THE AP
C
        CALL SPLDGO(SP,16,16,4095)
C
C====== WAIT FOR THE AP TO HALT OR OPERATOR INTERVENTION
C
50      CALL TSTRUN(I)
        IF(I.EQ.1) GOTO 60
        CALL TERM(TIP)
        IF(TIP.EQ.OFF) GOTO 50
C
C====== THE OPERATOR HAS INTERRUPTED STOP THE AP AND RETURN
C
        CALL APRSET
        RETURN
C
C====== IF THE AP HALTS CALL THE ERROR CHECK ROUTINE
C
60      CALL TMERR(1)
C
        RETURN
        END
C****** TMBAD = TM RAM BASIC ADDRESS TEST = REL 5.0  , NOV 79 *****************
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TMBAD                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    :  DRIIVER FOR TMRAM ADDRESS TAG TEST                      *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE TMBAD(NTEST)
C:
C
C------ COMMON STORAGE:
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C
C------ LOCAL STORAGE:
C
C%.I 3375 NOVA
        INTEGER SP(16),CONT,PAGE
        DATA CONT/8192/
C:
C
C
C====== TEST FOR A CONTINUE AFTER AN ERROR
C
        IF(STAT.EQ.0) GOTO 10
        CALL APOUT(CONT,2)
        GOTO 50
C
C====== TEST FOR ANY MEMORY SELECTED
C
10      IF(TMHI.NE.0.OR.TMLO.NE.0) GOTO 20
        STAT=2
        RETURN
C
C====== TEST FOR NAMES PRINT
C
20      IF(NAMES.EQ.OFF) GOTO 40
        WRITE(ITTO,30)NTEST
        IF(ECHO.NE.0) WRITE(FUNIT,30)NTEST
30      FORMAT(1X,'(',I2,') TM RAM ADDRESS TAG TEST')
C
C====== MAKE SURE THE AP IS STOPPED
C
40      CALL APRSET
C
C====== LOAD TM TEST MICRO CODE  (TMBADD)
C
        CALL FMGR(35)
C
C====== SET UP CALLING PARAMETERS
C
        SP(1)=TMHI-TMLO+1
        SP(2)=TMLO
        SP(3)=0
        SP(16)=LOOP
C
C====== START THE AP
C
        CALL SPLDGO(SP,16,16,4095)
C
C====== WAIT FOR THE AP TO HALT OR OPERATOR INTERVENTION
C
50      CALL TSTRUN(I)
        IF(I.EQ.1) GOTO 60
        CALL TERM(TIP)
        IF(TIP.EQ.OFF) GOTO 50
C
C====== THE OPERATOR HAS INTERRUPTED STOP THE AP AND RETURN
C
        CALL APRSET
        RETURN
C
C====== IF THE AP HALTS CALL THE ERROR CHECK ROUTINE
C
60      CALL TMERR(2)
C
        RETURN
        END
C****** TMRDM = TM RAM RANDOM TEST = REL 5.0  , NOV 79 ************************
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TMRDM                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : DRIVER FOR TMRAM RANDOM PATTERNS TEST                    *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE TMRDM(NTEST)
C:
C
C------ COMMON STORAGE:
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C
C------ LOCAL STORAGE:
C
C%.I 3499 NOVA
        INTEGER SP(16),CONT,PAGE,IXRN
        DATA CONT/8192/
C:
C
C
        IXRN=ERRORS+PASSES+ROM+LUN+PASLIM+NAMES+MDHI(2)
        IXRN=IXRN+STAT+SP(3)+TABLE(40)+PSHI+TMHI+APNUM
C
C====== TEST FOR A CONTINUE AFTER AN ERROR
C
        IF(STAT.EQ.0) GOTO 10
        CALL APOUT(CONT,2)
        GOTO 50
C
C====== TEST FOR ANY TMRAM
C
10      IF(TMLO.NE.0.OR.TMHI.NE.0) GOTO 20
        STAT=2
        RETURN
C
C====== TEST FOR NAMES PRINT
C
20      IF(NAMES.EQ.OFF) GOTO 40
        WRITE(ITTO,30)NTEST
        IF(ECHO.NE.0) WRITE(FUNIT,30)NTEST
30      FORMAT(1X,'(',I2,') TM RAM RANDOM DATA TEST')
C
C====== MAKE SURE THE AP IS STOPPED
C
40      CALL APRSET
C
C====== LOAD TM TEST MICRO CODE  (TRMTST)
C
        CALL FMGR(36)
C
C====== SET UP CALLING PARAMETERS
C
        SP(1)=TMHI-TMLO+1
        SP(2)=TMLO
        SP(3)=IXRN
        SP(16)=LOOP
C
C====== START THE AP
C
        CALL SPLDGO(SP,16,16,4095)
C
C====== WAIT FOR THE AP TO HALT OR OPERATOR INTERVENTION
C
50      CALL TSTRUN(I)
        IF(I.EQ.1) GOTO 60
        CALL TERM(TIP)
        IF(TIP.EQ.OFF) GOTO 50
C
C====== THE OPERATOR HAS INTERRUPTED STOP THE AP AND RETURN
C
        CALL APRSET
        RETURN
C
C====== IF THE AP HALTS CALL THE ERROR CHECK ROUTINE
C
60      CALL TMERR(3)
C
        RETURN
        END
C****** TMCKS = TM ROM DATA CHECKSUMS = REL 5.0  , NOV 79 *********************
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TMCKS                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : DRIVER FOR TM ROM DATA CHECKSUM TEST                     *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE TMCKS(NTEST)
C:
C
C------ COMMON STORAGE:
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C
C------ LOCAL STORAGE:
C
C%.I 3626 NOVA
        INTEGER IROMSZ(50),IR45(45),IR25(25)
        INTEGER SP(16),CONT,PAGE,ADDR,ROW,COL
C
C------ DATA ASSIGNMENTS
C
        DATA CONT/8192/
C
        DATA IROMSZ(1),IROMSZ(2),IROMSZ(3) /0,15627,15627/
        DATA IROMSZ(4),IROMSZ(5),IROMSZ(6) /-28499,22865,6493/
        DATA IROMSZ(7),IROMSZ(8),IROMSZ(9) /26692,0,0/
        DATA IROMSZ(10),IROMSZ(11),IROMSZ(12)/30518,15627,26807/
        DATA IROMSZ(13),IROMSZ(14),IROMSZ(15)/-28499,-20626,22865/
        DATA IROMSZ(16),IROMSZ(17),IROMSZ(18)/8463,29823,0/
C
C------ DATA ROM 2.5 K ROM TEST
C
        DATA IR25(1),IR25(2),IR25(3) /0,-8239,-27189/
        DATA IR25(4),IR25(5),IR25(6) /2638,2454,0/
        DATA IR25(7),IR25(8),IR25(9) /-15328,-2624,-1878/
        DATA IR25(10),IR25(11),IR25(12) /-857,-24746,-21002/
        DATA IR25(13),IR25(14),IR25(15) /-4461,-1063,-1315/
        DATA IR25(16),IR25(17),IR25(18) /-512,-15995,-1754/
        DATA IR25(19),IR25(20),IR25(21) /1896,-2906,-10260/
        DATA IR25(22),IR25(23),IR25(24) /-16680,-11865,-13264/
        DATA IR25(25) /-14395/
C
C------ DATA ROM 4.5 K ROM TEST
C
        DATA IR45(1),IR45(2),IR45(3) /0,-8239,-7235/
        DATA IR45(4),IR45(5),IR45(6) /4350,-61,0/
        DATA IR45(7),IR45(8),IR45(9) /-8192,18575,-59/
        DATA IR45(10),IR45(11),IR45(12) /3638,0,-11680/
        DATA IR45(13),IR45(14),IR45(15) /-4117,-183,81/
        DATA IR45(16),IR45(17),IR45(18) /0,-18992,-1103/
        DATA IR45(19),IR45(20),IR45(21) /-2015,-1384,0/
        DATA IR45(22),IR45(23),IR45(24) /-28160,488,-81/
        DATA IR45(25),IR45(26),IR45(27) /-605,20139,-13781/
        DATA IR45(28),IR45(29),IR45(30) /-9611,-790,621/
        DATA IR45(31),IR45(32),IR45(33) /-512,-15746,-576/
        DATA IR45(34),IR45(35),IR45(36) /-408,-2462,-512/
        DATA IR45(37),IR45(38),IR45(39) /-16344,-2658,12/
        DATA IR45(40),IR45(41),IR45(42) /-3425,-10260,-16680/
        DATA IR45(43),IR45(44),IR45(45) /-11865,-13264,-14395/
C
C
C:
C
C====== TEST FOR ANY ROM
C
        IF(ROM.NE.0) GOTO 5
        STAT=2
        RETURN
C
C
C====== TEST FOR NAMES PRINT
C
5       IF(NAMES.EQ.OFF) GOTO 20
        WRITE(ITTO,10)NTEST
        IF(ECHO.NE.0) WRITE(FUNIT,10)NTEST
10      FORMAT(1X,1H(,I2,2H) ,26HTM ROM DATA CHECKSUMS TEST)
C
C====== MAKE SURE THE AP IS STOPPED
C
20      CALL APRSET
C
C====== DMA THE ROM TEST TABLES INTO THE AP
C
        CALL APPUT(IR25,1000,25,1)
        CALL APPUT(IR45,2000,45,1)
        CALL APPUT(IROMSZ,0,50,1)
C
C====== LOAD TM TEST MICRO CODE (ROMDAT)
C
        CALL FMGR(37)
C
C====== SET UP CALLING PARAMETERS
C
        SP(1)=2559
        IF(ROM.EQ.4) SP(1)=4607
        SP(2)=0
        SP(3)=512
        SP(4)=0
C
C====== START THE AP
C
        CALL SPLDGO(SP,16,16,4095)
C
C====== WAIT FOR THE AP TO HALT OR OPERATOR INTERVENTION
C
30      CALL TSTRUN(I)
        IF(I.EQ.1) GOTO 40
        CALL TERM(TIP)
        IF(TIP.EQ.OFF) GOTO 30
C
C====== THE OPERATOR HAS INTERRUPTED STOP THE AP AND RETURN
C
        CALL APRSET
        RETURN
C
C====== IF THE AP HALTS TEST FOR AN ERROR
C
40      CALL ILITES(LITE)
        STAT=2
        NERR=OFF
        IF(LITE.EQ.-1) RETURN
C
C====== AN ERROR HAS BEEN DETECTED, PRINT ERROR INFORMATION
C
        NERR=ON
        CALL APGSP(NNERR,4)
        CALL APGSP(ADDR,1)
        ROW=(ADDR/5)+1
        COL=(ADDR-((ADDR/5)*5))+1
C
        WRITE(ITTO,50) NNERR,ROW,COL
        IF(ECHO.NE.0) WRITE(FUNIT,50) NNERR,ROW,COL
50      FORMAT(1X,'MEM100 - TM ROM CHECKSUM ERRORS = ',I2/
     +         1X,'FIRST FAILURE=',4X,'ROW ',I2,4X,'COLUMN ',I2)
        RETURN
C
        END
C****** TMNSE = TM ROM NOISE TEST= REL 5.0  , NOV 79 **************************
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TMNSE                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : DRIVER FOR TM ROM NOISE TEST                             *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE TMNSE(NTEST)
C:
C
C------ COMMON STORAGE:
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C
C------ LOCAL STORAGE:
C
C%.I 3810 NOVA
        INTEGER SP(16),CONT,PAGE
        DATA CONT/8192/
C:
C
C====== TEST FOR ANY ROM
C
        IF(ROM.NE.0) GOTO 5
        STAT=2
        RETURN
C====== INITIALIZE SPAD PARAMETERS
C
C
5       ISEED  = 10
        ICOUNT=4096
C
C
C====== TEST FOR A CONTINUE AFTER AN ERROR
C
        IF(STAT.EQ.0) GOTO 10
        CALL APOUT(CONT,2)
        GOTO 40
C
C====== TEST FOR NAMES PRINT
C
10      IF(NAMES.EQ.OFF) GOTO 30
        WRITE(ITTO,20)NTEST
        IF(ECHO.NE.0) WRITE(FUNIT,20)NTEST
20      FORMAT(1X,'(',I2,') TM ROM DATA NOISE TEST')
C
C====== MAKE SURE THE AP IS STOPPED
C
30      CALL APRSET
C
C====== LOAD TM TEST MICRO CODE (ROMNSE)
C
        CALL FMGR(38)
C
C====== SET UP CALLING PARAMETERS
C
        SP(1)=2048
        IF(ROM.EQ.4) SP(1)=4096
        SP(2)=ISEED
        SP(3)=ICOUNT
        SP(16)=LOOP
C
C====== START THE AP
C
        CALL SPLDGO(SP,16,16,4095)
C
C====== WAIT FOR THE AP TO HALT OR OPERATOR INTERVENTION
C
40      CALL TSTRUN(I)
        IF(I.EQ.1) GOTO 50
        CALL TERM(TIP)
        IF(TIP.EQ.OFF) GOTO 40
C
C====== THE OPERATOR HAS INTERRUPTED STOP THE AP AND RETURN
C
        CALL APRSET
        RETURN
C
C====== IF THE AP HALTS CALL THE ERROR CHECK ROUTINE
C
50      CALL TMERR(4)
C
C====== GET REPEAT COUNT
C
        CALL APGSP(ISEED,1)
        CALL APGSP (ICOUNT,2)
        IF (ICOUNT.GT.0) GO TO 30
C
        RETURN
        END
C****** TMERR = TM RAM TEST ERROR PROCESSOR = REL 5.0  , NOV 79 ***************
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TMERR                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : ERROR CODE IS READ FROM THE AP LITES REGISTER            *
C  *                                                                           *
C  *    EXIT        : ERR   =>ERROR FLAG                                       *
C  *                                                                           *
C  *    FUNCTION    : INTERPRETS THE ERROR CODE IN THE LITES REGISTER          *
C  *                  FROM THE AP AND PRINTS ERROR MESSAGES AND MEMORY         *
C  *                  AND REGISTER INFORMATION                                 *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE TMERR(ERRTYP)
C:
C
C------COMMON STORAGE:
C
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C------LOCAL STORAGE:
C
C%.I 3950 NOVA
        INTEGER IEXP(3),ACT(3),LINE(20),DP,DPA,DPX,DPY,BLANK,
     +          ADDR(6),TMA,TMAD,SPAD,ERRTYP
        DATA    TMAD/4/,DPA/5/,DPX/12/,DPY/13/,BLANK/1H /,SPAD/6/
C
C
C:
        STAT=1
        NERR=OFF
C
C====== GET TEST RETURN CODE (-1) IS GOOD RETURN
C
        CALL ILITES(LITE)
C
        IF(LITE.NE.-1) GOTO 10
C
C====== PROPER RETURN CODE - SET STAT TO INDICATE TEST COMPLETE
C
        STAT=2
        RETURN
C
C====== AN ERROR HAS BEEN DETECTED - PRINT ERROR INFORMATION
C
10      NERR=ON
C
C====== TEST FOR NO ERROR PRINTOUT
C
        IF(TD.EQ.ON) RETURN
C
C====== GET EXPECTED AND ACTUAL DATA AND ADDRESS
C       TM ADDRESS REG (TMA)
C
        CALL APEXAM(TMA,TMAD,0)
C
C====== GET DPA AND SUBTRACT 4 TO GET DPX AND DPY (-4) FOR EXP AND ACT
C
        CALL APEXAM(DP,DPA,0)
        DP=DP-4
C
C====== GET DPX AND DPY
C
        CALL APEXAM(IEXP,DPX,DP)
        CALL APEXAM(ACT,DPY,DP)
C
C====== PRINT  ERROR MESSAGE
C
        GOTO(20,40,60,80),ERRTYP
20      WRITE(ITTO,30)LITE
        IF(ECHO.NE.0) WRITE(FUNIT,30)LITE
30      FORMAT(1X,44HMEM100 - TM RAM MOVING INVERSE TEST - ERROR=,I6)
        GOTO 100
C
C
40      WRITE(ITTO,50)LITE
        IF(ECHO.NE.0) WRITE(FUNIT,50)LITE
50      FORMAT(1X,36HMEM100 - TM RAM ADDRESS TAG - ERROR=,I6)
        GOTO 100
C
C
60      WRITE(ITTO,70)LITE
        IF(ECHO.NE.0) WRITE(FUNIT,70)LITE
70      FORMAT(1X,40HMEM100 - TM RAM RANDOM PATTERNS - ERROR=,I6)
        GOTO 100
C
C
80      WRITE(ITTO,90)LITE
        IF(ECHO.NE.0) WRITE(FUNIT,90)LITE
90      FORMAT(1X,35HMEM100 - TM ROM NOISE TEST - ERROR=,I6)
C
C====== PRINT EXPECTED AND ACTUAL INFORMATION
C
100     CALL FILL(LINE,1,20,BLANK)
C
        CALL I2ASCI(6,TMA,ADDR,IRADX,0)
        DO 110 I=1,3
        J=I*7-6
110     CALL I2ASCI(6,IEXP(I),LINE(J),IRADX,0)
C
        WRITE(ITTO,120)ADDR,LINE
        IF(ECHO.NE.0) WRITE(FUNIT,120)ADDR,LINE
120     FORMAT(1X,9HADDRESS= ,6A1,4X,2HE ,20A1)
C
        DO 130 I=1,3
        J=I*7-6
130     CALL I2ASCI(6,ACT(I),LINE(J),IRADX,0)
        WRITE(ITTO,140) LINE
        IF(ECHO.NE.0) WRITE(FUNIT,140) ADDR,LINE
140     FORMAT(1X,19X,2HA ,20A1)
        RETURN
        END
C****** FMGR = MICROCODE FILE MANAGER ROUTINE = REL 5.0  , NOV 79 *************
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : FMGR                                                     *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : NFIL     => INPUT TEST NUMBER                            *
C  *                                                                           *
C  *    EXIT        :             OUTPUTS TEST FILE TO AP IF NOT               *
C  *                              ALREADY RESIDENT                             *
C  *                                                                           *
C  *    FUNCTION    : SCANS DOWN THE TEST FILE AND LOADS THE TEST NUMBER       *
C  *                  CORESPONDING TO NFILE. IF THE TEST IS ALREADY AP         *
C  *                  RESEDENT THEN NO FILE ACCESSING TAKES PLACE              *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE FMGR(NFILE)
C
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C%.R 4070 NOVA PRIME
        INTEGER START,OFILE,NAME(5),NLEN,LUNF,SKIP
        DATA START/1/,OFILE/0/,NAME/'ME','MD','AT','.D','AT'/
        DATA NLEN/10/,LUNF/2/
C%.E 4070 NOVA PRIME
C
C====== IF FIRST TIME THROUGH ROUTINE THEN THE FILES HAVE TO BE OPENED
C
        IF(START.NE.1) GOTO 10
        N=IOCNTL(7,NAME,NLEN,LUNF)
        IF(N.NE.0) CALL SETUP(-2)
        START=0
C
C====== SEE IF FILE IS ALREADY IN AP
C
10      IF(NFILE.EQ.OFILE) RETURN
C
C====== SEE IF FILE HAS TO BE REWOUND BEFORE GETTING TEST
C
        IF(NFILE.GT.OFILE) GOTO 20
        OFILE=0
        N=IOCNTL(6,0,0,LUNF)
        IF(NFILE.EQ.0) RETURN
C
C====== CHECK TO SEE IF ANY RECORDS HAVE TO BE SKIPPED BEFORE READING
C       DESIRED TEST IN
C
20      SKIP=(NFILE-OFILE)-1
        IF(SKIP.EQ.0) GOTO 70
C
C====== SKIP "SKIP" TESTS
C
        DO 60 I=1,SKIP
C
C====== GET TEST LENGTH TO SKIP
C
         READ(LUNF,30) A
30       FORMAT(F7.0)
         LENGTH=IPFIX(A)
         DO 50 N=1,LENGTH
          READ(LUNF,40)J
40        FORMAT(A1)
50       CONTINUE
60      CONTINUE
C
C====== READ TEST AND PUT INTO AP
C
70      READ(LUNF,30)A
        LENGTH=IPFIX(A)
        LOADPT=16
80      IF(LENGTH.GE.256) LOOP=256
        IF(LENGTH.LT.256) LOOP=LENGTH
        K=LOOP*4
C
C====== LOAD AP
C
        CALL TLOAD(TABLE,K,LUNF,4)
        CALL LOADPS(TABLE,LOADPT,LOOP)
        LENGTH=LENGTH-LOOP
        LOADPT=LOADPT+256
        IF(LENGTH.GT.0) GOTO 80
C
        OFILE=NFILE
        RETURN
        END
C****** SETUP5 = PROVIDE LINKAGE WITH PARAMETER ROUTINE = REL 5.0  , NOV 79 ***
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : SETUP                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : FLAG  => IF FLAG =  1, INITIALIZE TABLE TO DEFAULTS      *
C  *                                      0, ENTER INPUT MODE                  *
C  *                                     -2, PRINT FILE ERROR MESSAGE AND EXIT *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : SETUP HAS THE TABLE OF AVAILABLE COMMANDS TO THE         *
C  *                  CONTROLLER.  IT CALLS CSI WHICH PARSES AND INTERPRETS THE*
C  *                  COMMANDS AND SETS THE VALUES IN IPAR THAT CORRESPOND TO  *
C  *                  THE COMMANDS IN THE TABLE                                *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE SETUP(FLAG)
C:
C------PARAMETERS:
C
        INTEGER FLAG
C
C       FLAG= ONE CAUSES A CALL TO CSI TO INITIALIZE ALL VALUES
C       IN THE TABLE
C
C           = ZERO TO GO INTO INPUT MODE
C
C           = -2 TO PRINT FILE ERROR MESAGE AND EXIT
C
C
C
C------COMMON STORAGE:
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C
C------LOCAL STORAGE:
C
C%.I 4214 NOVA
        INTEGER XTB(15,12)
C
C         PASLIM
C
        DATA XTB( 1, 1),XTB( 1, 2),XTB( 1, 3)   /1HP,1HA,1HS/
        DATA XTB( 1, 4),XTB( 1, 5),XTB( 1, 6)   /1HL,1HI,1HM/
        DATA XTB( 1, 7),XTB( 1, 8),XTB( 1, 9)   /     2,     1,     1/
        DATA XTB( 1,10),XTB( 1,11),XTB( 1,12)   /     0,     0,    10/
C
C         ERRLIM
C
        DATA XTB( 2, 1),XTB( 2, 2),XTB( 2, 3)   /1HE,1HR,1HR/
        DATA XTB( 2, 4),XTB( 2, 5),XTB( 2, 6)   /1HL,1HI,1HM/
        DATA XTB( 2, 7),XTB( 2, 8),XTB( 2, 9)   /     2,     2,     0/
        DATA XTB( 2,10),XTB( 2,11),XTB( 2,12)   /     0,     0,    10/
C
C         BETWN
C
        DATA XTB( 3, 1),XTB( 3, 2),XTB( 3, 3)   /1HB,1HE,1HT/
        DATA XTB( 3, 4),XTB( 3, 5),XTB( 3, 6)   /1HW,1HN,1H /
        DATA XTB( 3, 7),XTB( 3, 8),XTB( 3, 9)   /     2,     3,     1/
        DATA XTB( 3,10),XTB( 3,11),XTB( 3,12)   /     0,     0,    10/
C
C         NAMES
C
        DATA XTB( 4, 1),XTB( 4, 2),XTB( 4, 3)   /1HN,1HA,1HM/
        DATA XTB( 4, 4),XTB( 4, 5),XTB( 4, 6)   /1HE,1HS,1H /
        DATA XTB( 4, 7),XTB( 4, 8),XTB( 4, 9)   /     3,     4,     1/
        DATA XTB( 4,10),XTB( 4,11),XTB( 4,12)   /     1,     0,     0/
C
C         TESTS
C
        DATA XTB( 5, 1),XTB( 5, 2),XTB( 5, 3)   /1HT,1HE,1HS/
        DATA XTB( 5, 4),XTB( 5, 5),XTB( 5, 6)   /1HT,1HS,1H /
        DATA XTB( 5, 7),XTB( 5, 8),XTB( 5, 9)   /     4,     6,     0/
        DATA XTB( 5,10),XTB( 5,11),XTB( 5,12)   /     0,    15,     0/
C
C         L
C
        DATA XTB( 6, 1),XTB( 6, 2),XTB( 6, 3)   /1HL,1H ,1H /
        DATA XTB( 6, 4),XTB( 6, 5),XTB( 6, 6)   /1H ,1H ,1H /
        DATA XTB( 6, 7),XTB( 6, 8),XTB( 6, 9)   /     3,     7,     0/
        DATA XTB( 6,10),XTB( 6,11),XTB( 6,12)   /     0,     0,     0/
C
C         W
C
        DATA XTB( 7, 1),XTB( 7, 2),XTB( 7, 3)   /1HW,1H ,1H /
        DATA XTB( 7, 4),XTB( 7, 5),XTB( 7, 6)   /1H ,1H ,1H /
        DATA XTB( 7, 7),XTB( 7, 8),XTB( 7, 9)   /     3,     8,     0/
        DATA XTB( 7,10),XTB( 7,11),XTB( 7,12)   /     0,     0,     0/
C
C         D
C
        DATA XTB( 8, 1),XTB( 8, 2),XTB( 8, 3)   /1HD,1H ,1H /
        DATA XTB( 8, 4),XTB( 8, 5),XTB( 8, 6)   /1H ,1H ,1H /
        DATA XTB( 8, 7),XTB( 8, 8),XTB( 8, 9)   /     3,     9,     0/
        DATA XTB( 8,10),XTB( 8,11),XTB( 8,12)   /     0,     0,     0/
C
C         H
C
        DATA XTB( 9, 1),XTB( 9, 2),XTB( 9, 3)   /1HH,1H ,1H /
        DATA XTB( 9, 4),XTB( 9, 5),XTB( 9, 6)   /1H ,1H ,1H /
        DATA XTB( 9, 7),XTB( 9, 8),XTB( 9, 9)   /     3,    10,     0/
        DATA XTB( 9,10),XTB( 9,11),XTB( 9,12)   /     0,     0,     0/
C
C         RSTART
C
        DATA XTB(10, 1),XTB(10, 2),XTB(10, 3)   /1HR,1HS,1HT/
        DATA XTB(10, 4),XTB(10, 5),XTB(10, 6)   /1HA,1HR,1HT/
        DATA XTB(10, 7),XTB(10, 8),XTB(10, 9)   /     5,     0,     1/
        DATA XTB(10,10),XTB(10,11),XTB(10,12)   /     0,     0,     0/
C
C         SET
C
        DATA XTB(11, 1),XTB(11, 2),XTB(11, 3)   /1HS,1HE,1HT/
        DATA XTB(11, 4),XTB(11, 5),XTB(11, 6)   /1H ,1H ,1H /
        DATA XTB(11, 7),XTB(11, 8),XTB(11, 9)   /     5,     0,     2/
        DATA XTB(11,10),XTB(11,11),XTB(11,12)   /     0,     0,     0/
C
C         DISPLA
C
        DATA XTB(12, 1),XTB(12, 2),XTB(12, 3)   /1HD,1HI,1HS/
        DATA XTB(12, 4),XTB(12, 5),XTB(12, 6)   /1HP,1HL,1HA/
        DATA XTB(12, 7),XTB(12, 8),XTB(12, 9)   /     5,     0,     3/
        DATA XTB(12,10),XTB(12,11),XTB(12,12)   /     0,     0,     0/
C
C         DUMP
C
        DATA XTB(13, 1),XTB(13, 2),XTB(13, 3)   /1HD,1HU,1HM/
        DATA XTB(13, 4),XTB(13, 5),XTB(13, 6)   /1HP,1H ,1H /
        DATA XTB(13, 7),XTB(13, 8),XTB(13, 9)   /     5,     0,     4/
        DATA XTB(13,10),XTB(13,11),XTB(13,12)   /     0,     0,     0/
C
C         WRITE
C
        DATA XTB(14, 1),XTB(14, 2),XTB(14, 3)   /1HW,1HR,1HI/
        DATA XTB(14, 4),XTB(14, 5),XTB(14, 6)   /1HT,1HE,1H /
        DATA XTB(14, 7),XTB(14, 8),XTB(14, 9)   /     5,     0,     5/
        DATA XTB(14,10),XTB(14,11),XTB(14,12)   /     0,     0,     0/
C
C         APNUM
C
        DATA XTB(15, 1),XTB(15, 2),XTB(15, 3)   /1HA,1HP,1HN/
        DATA XTB(15, 4),XTB(15, 5),XTB(15, 6)   /1HU,1HM,1H /
        DATA XTB(15, 7),XTB(15, 8),XTB(15, 9)   /     5,     0,     6/
        DATA XTB(15,10),XTB(15,11),XTB(15,12)   /     0,     0,     0/
C
C
C:
C
C====== SET UP NUMBER OF POSIBLE TESTS
C
        XTB(5,11)=NTESTS
C
C====== IF FLAG = -2 PRINT MESSAGE AND EXIT
C
        IF(FLAG.NE.-2) GOTO 20
        WRITE(ITTO,10)
10      FORMAT(25H ****** FILE ERROR ******/13H TEST ABORTED)
        CALL EXIT
C
C====== PRINT TEST HEADER
C
20      WRITE(ITTO,30)
        IF(ECHO.NE.0) WRITE(FUNIT,30)
30      FORMAT(1X,36H *** MEM100  - REL 0.0 - SEP. 79 ***/)
C
C====== INITIALIZE PARAMETERS IF FLAG IS SET
C
        IF(FLAG.EQ.1) CALL CSI(XTB,15,IPAR,11,1)
        IF(FLAG.EQ.1) WRITE(ITTO,15)
15      FORMAT(1X,'PLEASE ASSIGN AN AP')
C
C====== SET NZMD TO MAKE SURE THE BOOTSTRAP IS LOADED
C
        NZMD=1
C
C====== INPUT MODE FOR CHANGING/VEWING PARAMETERS
C
        CALL CSI(XTB,15,IPAR,11,0)
C
        RETURN
        END
C****** IMCMD5 = IMMEDIATE COMMAND PROCESSOR FOR CSI = REL 5.0  , NOV 79 ******
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : IMCMD                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : INDEX   =>SUBROUTINE NUMBER TO EXECUTE                   *
C  *                  INBUF   =>INPUT LINE FROM INPUT ROUTINE                  *
C  *                  IPOS    =>CHARACTER POINTER INTO INBUF FOR IRDTK SUBROUTI*
C  *                  IR      =>PROGRAM I/O RADIX                              *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THIS PROGRAM CAN BE CALLED BY CSI TO PREFORM             *
C  *                  IMMEDIATE COMMANDS.                                      *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE IMCMD(INDEX,INBUF,IPOS,IR)
C:
C
C------COMMON STORAGE
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C
C:
C%.I 4397 NOVA
        INTEGER INBUF(9999),E,BLANK
        DATA E/1HE/,BLANK/1H /
C
C====== GO TO THE ROUTINE INDICATED BY THE INDEX NUMBER FOR PROCESSING
C
        GOTO (10,20,30,40,50,60),INDEX
C
C====== PROCESS RE-START COMMAND
C
10      RSTART=1
        INBUF(IPOS)=BLANK
        INBUF(IPOS+1)=E
        INBUF(IPOS+2)=BLANK
        RETURN
C
C====== SET MEMORY PARAMETERS
C
20      CALL MEMSET(INBUF,IPOS)
        RETURN
C
C====== DISPLAY MEMORY PARAMETERS
C
30      CALL MEMDIS
        RETURN
C
C====== DUMP MEMORY CONTENTS
C
40      CALL MEMDMP(INBUF,IPOS,IR)
        RETURN
C
C====== WRITE MEMORY PARAMETERS TO PARAMETER FILE
C
50      CALL MEMWRT
        RETURN
C
C====== ASSIGN AP
C
60      CALL PARAP(INBUF,IPOS)
        RETURN
C
        END
C****** MEMSET = SET MEMORY PARAMETERS FOR MEM100 = REL 5.0  , NOV 79 *********
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : MEMSET                                                   *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *    ENTRY       : INBUF => COMMAND INPUT STRING FROM CSI                   *
C  *                  IPOS  => POSITION OF NEXT TOKEN                          *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : SET MEMORY SIZES AND PARAMETERS FOR USE BY MEM100        *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE MEMSET(INBUF,IPOS)
C:
C
C------COMMON STORAGE:
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C
C------LOCAL STORAGE:
C
C%.I 4503 NOVA
       INTEGER INBUF(80),IPOS,STR(80),IZ(48)
C
C
       DATA IZ/'P','A','R','I','T','Y',
     +         'N','O','P','A','R','I',
     +         'F','A','S','T',' ',' ',
     +         'S','T','A','N','D','A',
     +         'P','S',' ',' ',' ',' ',
     +         'T','M','R','O','M',' ',
     +         'T','M','R','A','M',' ',
     +         'M','D',' ',' ',' ',' '
     + /
C
C
C:
C
C====== GET TOKEN
C
        N=IRDTK(INBUF,IPOS,STR,IRADX,NUM,IMIN)
C
C====== SEARCH FOR FUNCTION
C
        DO 10 J=1,8
        L=J*6-5
        IF(NCOMP(STR,1,6,IZ,L).EQ.0) GOTO(20,30,40,50,60,70,80,90),J
10      CONTINUE
C
C====== SET PARITY FLAG
C
20      PARITY=ON
        RETURN
C
C====== RESET PARITY FLAG
C
30      PARITY=OFF
        RETURN
C
C====== SET MEMORY SPEED - FAST
C
40      SPEED=1
        RETURN
C
C====== SET MEMORY SPEED - STANDARD
C
50      SPEED=0
        RETURN
C
C====== SET PS LIMITS
C
60      IF(IRDTK(INBUF,IPOS,STR,IRADX,NUM,IMIN).NE.2) GOTO 100
        PSLO=NUM
        IF(IRDTK(INBUF,IPOS,STR,IRADX,NUM,IMIN).NE.2) GOTO 100
        PSHI=NUM
        IF(ICMP16(PSHI,4095).EQ.1) GOTO 65
        IF(ICMP16(PSHI,PSLO).NE.-1) RETURN
65      PSLO=0
        PSHI=0
        GOTO 100
C
C====== SET ROM SIZE
C
70      IF(IRDTK(INBUF,IPOS,STR,IRADX,NUM,IMIN).NE.2) GOTO 100
        IF(NUM.NE.2.AND.NUM.NE.4.AND.NUM.NE.0) GOTO 100
        ROM=NUM
        RETURN
C
C====== SET TMRAM LIMITS
C
80      IF(IRDTK(INBUF,IPOS,STR,IRADX,NUM,IMIN).NE.2) GOTO 100
        TMLO=NUM
        IF(IRDTK(INBUF,IPOS,STR,IRADX,NUM,IMIN).NE.2) GOTO 100
        TMHI=NUM
        IF(ICMP16(TMHI,TMLO).NE.-1) RETURN
        TMHI=0
        TMLO=0
        GOTO 100
C
C====== SET MAIN DATA LIMITS
C
90      CONTINUE
C
C====== PAGE
C
        IF(IRDTK(INBUF,IPOS,STR,IRADX,NUM,IMIN).NE.2) GOTO 100
        IPAGE=NUM+1
        IF(IPAGE.GT.16) GOTO 100
C
C====== MD - LOW
C
        IF(IRDTK(INBUF,IPOS,STR,IRADX,NUM,IMIN).NE.2) GOTO 100
        MDLO(IPAGE)=NUM
C
C====== MD - HIGH
C
        IF(IRDTK(INBUF,IPOS,STR,IRADX,NUM,IMIN).NE.2) GOTO 100
        MDHI(IPAGE)=NUM
        IF(ICMP16(MDHI(IPAGE),MDLO(IPAGE)).NE.-1) RETURN
        MDHI(IPAGE)=0
        MDLO(IPAGE)=0
        GOTO 100
C
C====== ERROR IN IRDTK
C
100     WRITE(ITTO,110)
110     FORMAT(1X,13HNUMERIC ERROR)
        IF(ECHO.NE.0) WRITE(FUNIT,110)
        RETURN
        END
C****** MEMDIS = DISPLAY MEMORY PARAMETERS = REL 5.0  , NOV 79 ****************
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : MEMDIS                                                   *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : DISPLAY MEMORY PARAMETERS ON CONSOLE                     *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE MEMDIS
C:
C
C------COMMON STORAGE:
C
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C
C------LOCAL STORAGE
C
        INTEGER LOW(6),HIGH(6),PAGE(2)
C
C
C:
C
C====== DISPLAY PS PARAMETERS
C
        CALL I2ASCI(6,PSLO,LOW,IRADX,0)
        CALL I2ASCI(6,PSHI,HIGH,IRADX,0)
        WRITE(ITTO,10)LOW,HIGH
        IF(ECHO.NE.0) WRITE(FUNIT,10)LOW,HIGH
10      FORMAT(1X,21HPROGRAM SOURCE   LOW=,6A1,7H  HIGH=,6A1)
C
C====== MAIN DATA
C
        WRITE(ITTO,20)
        IF(ECHO.NE.0) WRITE(FUNIT,20)
20      FORMAT(//1X,9HMAIN DATA/)
C
C====== SPEED
C
        IF(SPEED.EQ.1) GOTO 40
        WRITE(ITTO,30)
        IF(ECHO.NE.0) WRITE(FUNIT,30)
30      FORMAT(1X,8HSTANDARD)
        GOTO 60
40      WRITE(ITTO,50)
        IF(ECHO.NE.0) WRITE(FUNIT,50)
50      FORMAT(1X,4HFAST)
C
C====== PARITY
C
60      IF(PARITY.EQ.ON) GOTO 80
        WRITE(ITTO,70)
        IF(ECHO.NE.0) WRITE(FUNIT,70)
70      FORMAT(1X,9HNO PARITY)
        GOTO 100
80      WRITE(ITTO,90)
        IF(ECHO.NE.0) WRITE(FUNIT,90)
90      FORMAT(1X,6HPARITY)
C
C====== SIZES
C
100     DO 120 I=1,16
        IPAGE=I-1
        IF(MDLO(I).EQ.0.AND.MDHI(I).EQ.0) GOTO 120
        CALL I2ASCI(2,IPAGE,PAGE,IRADX,0)
        CALL I2ASCI(6,MDLO(I),LOW,IRADX,0)
        CALL I2ASCI(6,MDHI(I),HIGH,IRADX,0)
        WRITE(ITTO,110)PAGE,LOW,HIGH
        IF(ECHO.NE.0) WRITE(FUNIT,110)PAGE,LOW,HIGH
110     FORMAT(1X,2HMD,6X,5HPAGE=,2A1,2X,4HLOW=,6A1,2X,5HHIGH=,6A1)
120     CONTINUE
C
C====== TMRAM
C
        IF(TMLO.EQ.0.AND.TMHI.EQ.0) GOTO 140
        CALL I2ASCI(6,TMLO,LOW,IRADX,0)
        CALL I2ASCI(6,TMHI,HIGH,IRADX,0)
        WRITE(ITTO,130)LOW,HIGH
        IF(ECHO.NE.0) WRITE(FUNIT,130)LOW,HIGH
130     FORMAT(//1X,21HTABLE MEMORY RAM LOW=,6A1,7H  HIGH=,6A1)
        GOTO 160
140     WRITE(ITTO,150)
        IF(ECHO.NE.0) WRITE(FUNIT,150)
150     FORMAT(//1X,8HNO TMRAM)
C
C====== TMROM SIZE
C
160     CONTINUE
C
        WRITE(ITTO,170)ROM
        IF(ECHO.NE.0) WRITE(FUNIT,170)ROM
170     FORMAT(1X,9HROM SIZE=,I2,1HK//)
        RETURN
        END
C****** PARAP = READ PARAMETER FILE FOR MEMORY SIZES = REL 5.0  , NOV 79 ******
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PARAP                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : READ PARAMETER FILE FROM DISK INTO MEMORY SIZE           *
C  *                  ARRAY                                                    *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE PARAP(INBUF,IPOS)
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C       ITTI   = TERMINAL INPUT FORTRAN UNIT NUMBER
C       ITTO   = TERMINAL OUTPUT FORTRAN UNIT NUMBER
C       FUNIT  = ECHO FILE OUTPUT FORTRAN UNIT NUMBER
C
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C------LOCAL STORAGE:
C
C%.I 4814 NOVA
        INTEGER AP,PARFIL(3),NUM(9),LUNP,LENF,INBUF(80),IPOS,STR(6)
        INTEGER Y,BLANK
C%.R    4887    PRIME
        DATA AP/-1/,PARFIL/2HME,2HMP,2H00/,LUNP/3/,LENF/6/,Y/2HY /
C%.E    4887    PRIME
        DATA NUM/2H01,2H02,2H03,2H04,2H05,2H06,2H07,2H08,2H09/
        DATA BLANK/1H /
C
C
C
C====== ASSIGN AP
C
        IF(IRDTK(INBUF,IPOS,STR,IRADX,INUM,M).NE.2) GOTO 60
        IF(IOCNTL(4,0,NZMD,INUM).NE.0) GOTO 30
        WRITE(ITTO,10)INUM
        IF(ECHO.NE.0) WRITE(FUNIT,10)INUM
10      FORMAT(1X,2HAP,I5,3X,8HASSIGNED)
        APNUM=INUM
C
C====== FILL MEMORY PARAMETERS TO ZERO
C
        CALL FILL(MDHI,1,16,0)
        CALL FILL(MDLO,1,16,0)
        PARITY=OFF
        SPEED=0
        TMHI=0
        TMLO=0
        PSLO=0
        PSHI=0
        ROM=0
C
C====== ASK IF THERE IS A PARAMETER FILE
C
        WRITE(ITTO,12)APNUM
        IF(ECHO.NE.0) WRITE(FUNIT,12)APNUM
12      FORMAT(1X,'DOES A MEMORY PARAMETER FILE EXIST FOR AP',I2,' ?')
        READ(ITTI,13)N
13      FORMAT(A1)
        IF(ECHO.NE.0) WRITE(FUNIT,14)N
14      FORMAT(1X,A1)
        IF(N.NE.Y) GOTO 45
C
C====== OPEN AP PARAMETER FILE
C
        PARFIL(3)=NUM(APNUM)
C
        IF(IOCNTL(7,PARFIL,LENF,LUNP).NE.0) GOTO 40
        AP=APNUM
C
C====== GET PARAMETERS FROM FILE
C
        READ(LUNP,20)(TABLE(I),I=1,80)
20      FORMAT(80A1)
        I1=1
        DO 1000 I=1,16
C   IBNHX CONVERTS ASCII HEX TO AN INTEGER WORD
        MDHI(I)=IBNHX(TABLE,I1)
        I1=I1+5
1000    CONTINUE
C
        READ(LUNP,20)(TABLE(I),I=1,80)
        I1=1
        DO 1010 I=1,16
        MDLO(I)=IBNHX(TABLE,I1)
        I1=I1+5
1010    CONTINUE
        READ(LUNP,20)(TABLE(I),I=1,35)
        PARITY=IBNHX(TABLE,1)
        SPEED= IBNHX(TABLE,6)
        TMHI=  IBNHX(TABLE,11)
        TMLO=  IBNHX(TABLE,16)
        ROM=   IBNHX(TABLE,21)
        PSLO=  IBNHX(TABLE,26)
        PSHI=  IBNHX(TABLE,31)
        N=IOCNTL(9,0,0,LUNP)
30      RETURN
C
C====== PRINT NO PARMETER FILE WARNING
C
40      WRITE(ITTO,50)APNUM
        IF(ECHO.NE.0) WRITE(FUNIT,50)APNUM
50      FORMAT(1X,21HPARAMETER FILE FOR AP,I2,10H NOT FOUND)
45      WRITE(ITTO,55)
        IF(ECHO.NE.0) WRITE(FUNIT,55)
55      FORMAT( 1X,37HMEMORY SIZES MUST BE ENTERED MANUALLY)
        N=IOCNTL(9,0,0,LUNP)
        RETURN
C
C====== PRINT ERROR MESSAGE
C
60      WRITE(ITTO,70)
        IF(ECHO.NE.0) WRITE(FUNIT,70)
70      FORMAT(1X,13HNUMERIC ERROR)
        RETURN
        END
C****** MEMWRT = WRITE MEMORY PARAMETERS TO FILE = REL 5.0  , NOV 79 **********
        SUBROUTINE MEMWRT
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : MEMWRT                                                   *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : WRITE MEMORY PARAMETER FILE TO FILE FOR SETTING          *
C  *                  THE STANDARD PARAMETERS FOR MEM100                       *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
C
C       ITTI   = TERMINAL INPUT FORTRAN UNIT NUMBER
C       ITTO   = TERMINAL OUTPUT FORTRAN UNIT NUMBER
C       FUNIT  = ECHO FILE OUTPUT FORTRAN UNIT NUMBER
C
C
C
C               TOTAL CONTAINS THE VARIABLES SET BY THE OPERATOR
C               FROM SETUP
C
        COMMON /TOTAL/ IPAR(11)
C
        INTEGER PASLIM,ERRLIM,BETWN,TESTS(2),WAIT,TD,HALT ,RSTART
C
        EQUIVALENCE     (IPAR( 1),PASLIM),(IPAR( 2),ERRLIM),
     +                  (IPAR( 3),BETWN ),(IPAR( 4),NAMES ),
     +                  (IPAR( 5),TESTS ),(IPAR( 7),LOOP  ),
     +                  (IPAR( 8),WAIT  ),(IPAR( 9),TD    ),
     +                  (IPAR(10),HALT  ),(IPAR(11),RSTART)
C
C
C               TOTL CONTAINS OTHER VARIABLES PASSED
C               FROM SETUP
C
C
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +              (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +              (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C               MEM IS THE COMMON VARIABLES USED BY MEM100
C
        COMMON /MEM/ STAT,NERR,MDHI(16),MDLO(16),PARITY,SPEED,TMHI,TMLO,
     +               ROM,TIP,ON,OFF,ERRORS,PASSES,NTESTS,PSLO,PSHI,
     +               TABLE(1024)
        INTEGER STAT,NERR,PARITY,SPEED,TMHI,TMLO,ROM,TIP,PASSES,ERRORS,
     +          ON,OFF,PSLO,PSHI,TABLE
C
C
C------LOCAL STORAGE:
C
C%.I 4971 NOVA
        INTEGER AP,PARFIL(3),NUM(9),LUNP,LENF,Y,BLANK
C%.R    5045    PRIME
        DATA PARFIL/2HME,2HMP,2H00/,LUNP/3/,LENF/6/,Y/2HY /
C%.E    5045    PRIME
        DATA BLANK/1H /
        DATA NUM/2H01,2H02,2H03,2H04,2H05,2H06,2H07,2H08,2H09/
C
C
C====== GIVE WARNING
C
        WRITE(ITTO,10)APNUM
        IF(ECHO.NE.0) WRITE(FUNIT,10)APNUM
10      FORMAT(1X,42HTHIS WILL CHANGE THE PARAMETER FILE FOR AP,I3/
     $         1X,18HTYPE Y TO CONTINUE)
C
        READ(ITTI,20)N
20      FORMAT(A1)
C
        IF(ECHO.NE.0) WRITE(FUNIT,30)N
30      FORMAT(1X,A1)
C
        IF(N.NE.Y) RETURN
C
C====== OPEN AP PARAMETER FILE
C
        PARFIL(3)=NUM(APNUM)
C
        IF(IOCNTL(8,PARFIL,LENF,LUNP).NE.0) CALL SETUP(-2)
C
C====== PUT PARAMETERS TO FILE
C
        CALL FILL(TABLE,1,80,BLANK)
        I1=1
        DO 1000 I=1,16
        CALL I2ASCI(4,MDHI(I),TABLE(I1),16,0)
        I1=I1+5
1000    CONTINUE
        WRITE(LUNP,40)(TABLE(I),I=1,80)
C%.R 4975 NOVA
40      FORMAT(80A1)
C%.E 4975 NOVA
        I1=1
        DO 1010 I=1,16
        CALL I2ASCI(4,MDLO(I),TABLE(I1),16,0)
        I1=I1+5
1010    CONTINUE
        WRITE(LUNP,40)(TABLE(I),I=1,80)
        CALL I2ASCI(4,PARITY,TABLE(1),16,0)
        CALL I2ASCI(4,SPEED,TABLE(6),16,0)
        CALL I2ASCI(4,TMHI,TABLE(11),16,0)
        CALL I2ASCI(4,TMLO,TABLE(16),16,0)
        CALL I2ASCI(4,ROM,TABLE(21),16,0)
        CALL I2ASCI(4,PSLO,TABLE(26),16,0)
        CALL I2ASCI(4,PSHI,TABLE(31),16,0)
        WRITE(LUNP,40)(TABLE(I),I=1,35)
        N=IOCNTL(9,0,0,LUNP)
C
        RETURN
        END
C****** FILL = FILL ARRAY WITH SPECIFIED VALUE = REL 5.0  , NOV 79 ************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : FILL                                                     *
C  *    REV         : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : LOADS AN ARRAY WITH A WORD                               *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
C#
C:
        SUBROUTINE FILL(JCARD,J,JLAST,NCH)
        DIMENSION JCARD(9999)
C
C====== FILL JCARD J THROUGH JCARD ,JLAST WITH NCH
C
        DO 10 JNOW=J,JLAST
10      JCARD(JNOW)=NCH
        RETURN
        END
C****** TLOAD = BUILD TABLE OF MD/PS WORDS FROM DISK = REL 5.0  , NOV 79 ******
        SUBROUTINE TLOAD(TABLE,NLOC,LUN,NINSTR)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TLOAD                                                    *
C  *    REV         : 2                                                        *
C  *    VERSION     : 4                                                        *
C  *    DATE        : AUG 78                                                   *
C  *    HOST SYSTEM : ALL                                                      *
C  *                                                                           *
C  *    ENTRY       : TABLE = THE TABLE TO CONTAIN THE PS OR MD WORDS, EITHER  *
C  *                          4 (FOR PS) OR 3 (FOR MD) TAABLE LOCATIONS FOR    *
C  *                          INSTRUCTION.                                     *
C  *                  NLOC  = THE NUMBER OF LOCATIONS IN THE TABLE TO FILL     *
C  *                          (FOR PS 4 TIME THE NUMBER OF PS INSTRUCTIONS)    *
C  *                          (FOR MD 3 TIMES THE NUMBER OF MD LOCATIONS)      *
C  *                  LUN   = THE LOGICAL UNIT NUMBER OF THE FILE TO READ THE  *
C  *                          INSTRUCTIONS FROM                                *
C  *                  NINSTR= NUMBER OF WORDS PER MEMORY LOCATION (PS=4,MD=3)  *
C  *                                                                           *
C  *    EXIT        : TABLE = NOW HOLDS THE DATA                               *
C  *                                                                           *
C  *    FUNCTION    : TO BUILD A TABLE OF MD/PS INSTRUCTIONS THAT CAN BE       *
C  *                  DOWNLOADED INTO MD OR PS                                 *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C
        INTEGER TABLE(9999),NLOC,LUN,NINSTR
        K=1
        DO 60 N=1,NLOC,NINSTR
        IF(NINSTR.EQ.4) GOTO 20
C
C====== GET THE INSTRUCTION
C
        READ(LUN,10)A,B,C
10      FORMAT(3F7.0)
        GOTO 40
20      READ(LUN,30)A,B,C,D
30      FORMAT(4F7.0)
C
C====== CONVERT FROM A STRING INTO AN INTEGER AND PUT INTO TABLE
C
40      TABLE(K)=IPFIX(A)
        TABLE(K+1)=IPFIX(B)
        TABLE(K+2)=IPFIX(C)
        IF(NINSTR.EQ.3) GOTO 50
        TABLE(K+3)=IPFIX(D)
50      K=K+NINSTR
60      CONTINUE
        RETURN
C
        END
C****** MOVE = MOVE ONE ARRAY TO ANOTHER = REL 5.0  , NOV 79 ******************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : MOVE                                                     *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : TRANSFER CONTENTS OF ONE ARRAY TO ANOTHER                *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
C#
C:
        SUBROUTINE MOVE(JCARD,J,JLAST,KCARD,K)
        DIMENSION JCARD(9999), KCARD(9999)
C
C====== MOVE JCARD TO KCARD
C
        DO 10 JNOW=J,JLAST
        KNOW=K+JNOW-J
10      KCARD(KNOW)=JCARD(JNOW)
        RETURN
        END
C****** ASST5 = PRINT AVAILABLE COMMANDS                    = REL 5.0  , NOV 79
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ASSIST                                                   *
C  *    REV         : 0                                                        *
C  *    DATE        : SEP. 79                                                  *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : PRINT OUT AVAILABLE OPERATOR COMMANDS                    *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C
        SUBROUTINE ASSIST(ITTO,LFIL,IECHO)
C
C
        WRITE(ITTO,10)
        IF(IECHO.NE.0) WRITE(LFIL,10)
C
10      FORMAT (
     +  40H APNUM=   - CLOSE CURRENT AP, OPEN AP(N)                    /,
     +  48H CLRFLG   - <R> SETS ALL FLAGS TO DEFAULT VALUES            /,
     +  47H CLRALL   - RESETS PAREMETERS TO DEFAULT VALUES             /,
     +  36H ECHO     - OPEN FILE TO ECHO OUTPUT                        /,
     +  )
C
        WRITE(ITTO,20)
        IF(IECHO.NE.0) WRITE(LFIL,20)
C
20      FORMAT (
     +  27H -ECHO    - CLOSE ECHO FILE                                 /,
     +  31H RUN      - <E> EXECUTE PROGRAM                             /,
     +  44H HELP     - <?> <A> PRINT AVAILABLE COMMANDS                /,
     +  30H RADIX=   - SET I/O RADIX TO N                              /,
     +  )
C
        WRITE(ITTO,30)
        IF(IECHO.NE.0) WRITE(LFIL,30)
C
30      FORMAT (
     +  40H STAT     - PRINT CURRENT PROGRAM STATUS                    /,
     +  41H DISPLAY  - PRINT MEMORY PARAMETER VALUES                   /,
     +  47H DUMP     - PRINT SELECTED AP MEMORY TO CONSOLE             /,
     +  55H PASLIM=  - SET PASSES TO RUN BEFORE RETURNING TO INPUT     /,
     +  )
C
        WRITE(ITTO,40)
        IF(IECHO.NE.0) WRITE(LFIL,40)
C
40      FORMAT (
     +  52H ERRLIM=  - MAXIMUM ERRORS BEFORE RETURNING TO INPUT        /,
     +  51H BETWN=   - NUMBER OF PASSES BETWEEN PASS PRINTOUTS         /,
     +  49H NAMES    - FLAG TO ENABLE PRINTING OF TEST NAMES           /,
     +  31H TESTS=   - SELECT TESTS TO RUN                             /,
     +  )
C
        WRITE(ITTO,50)
        IF(IECHO.NE.0) WRITE(LFIL,50)
C
50      FORMAT (
     +  40H RSTART   - RESTART TESTS FROM BEGINNING                    /,
     +  30H L        - LOOP ON ERROR FLAG                              /,
     +  41H W        - RETURN TO INPUT ON ERROR FLAG                   /,
     +  35H D        - DISABLE ERROR PRINT OUT                         /,
     +  )
C
        WRITE(ITTO,60)
        IF(IECHO.NE.0) WRITE(LFIL,60)
C
60      FORMAT (
     +  56H H        - RETURN TO INPUT AFTER TEST (RESET EACH TIME)    /,
     +  33H SET      - SET MEMORY PARAMETERS                           /,
     +  40H WRITE    - WRITE TO PARAMETER FILE                        /)
C
        RETURN
        END
C****** APSTOP = DUMMY APEX APSTOP = REL 5.0  , NOV 79 ************************
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : APSTOP                                                   *
C  *    REV         : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *                                                                           *
C  *    ENTRY       : APSTOP NUMBER (NOT USED)                                 *
C  *                                                                           *
C  *    EXIT        : NONE                                                     *
C  *                                                                           *
C  *    FUNCTION    : TO DISABLE THE PAUSE X FROM APEX                         *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE APSTOP(I)
        RETURN
        END
C****** IBNHX = CONVERT ASCII HEX TO BINARY WORD = REL 5.0  , NOV 79 **********
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : IBNHX                                                    *
C  *    REV         : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *                                                                           *
C  *    ENTRY       : LINE   INPUT ARRAY OF HEX CHARACTERS                     *
C  *                  I      ARRAY POSITION OF HEX CHARACTERS                  *
C  *                                                                           *
C  *    EXIT        : BINARY WORD                                              *
C  *                                                                           *
C  *    FUNCTION    : CONVERT 4 ASCII HEX DIGITS TO BINARY                     *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        FUNCTION IBNHX(LINE,I)
C
C%.I 5247 NOVA
        INTEGER LINE(9999),NUMB(16)
C
C%.R 5250 NOVA
        DATA NUMB /1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,
     +            1H9,1HA,1HB,1HC,1HD,1HE,1HF/
C%.E 5250 NOVA
C
        IBNHX=0
        II=I
C
C====== CONVERT HEX TO BINARY
C
        DO 30 N=1,4
C
C====== SCAN FOR HEX CHARACTER
C
         DO 10 J=1,16
         IF(LINE(II).EQ.NUMB(J)) GOTO 20
10       CONTINUE
        IBNHX=0
        RETURN
C
C====== NUMBER IS 1 LESS THAN CHARACTER POSITION
C
20      J=J-1
        IBNHX=ILSH16(IBNHX,4)+J
        II=II+1
30      CONTINUE
        RETURN
        END
C%.I 5348 NOVA
