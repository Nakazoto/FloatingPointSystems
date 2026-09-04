C****** UFT100 = FPS100 UNIQUE FEATURES TEST = REL 5.0  , NOV 79 **************
C****** UFT100 = FPS100 UNIQUE FEATURES TEST = REL 5.0  , NOV 79 **************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : UFT100                                                   *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : TEST THE FPS100 UNIQUE FEATURES BY INTERPRETING TEST TABL*
C  *                  THAT HAVE A COMBINATION OF FRONT PANEL OPERATIONS AND    *
C  *                  AP RUNABLE MICRO-CODE                                    *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C                       BLOCK DESCRIPTION
C                       -----------------
C
C
C
C       PROC. = TYPE PROCESSOR: 2=TYPE2
C                               5=TYPE5
C                               C=COMMON TO BOTH
C
C    PROC.   NAME    DESCRIPTION
C----------------------------------------------------
C
C       C    TYPE    PART OF THE TABLE NAME
C       C    CASE    SECOND PART OF NAME
C       C    ADDR    LOCATION IN TSLIB OF TABLE
C       C    TSTSZ   TOTAL LOCATIONS IN TABLE
C       5    UTPKG   UTILITY PACKAGE NUMBER
C       5    USECD   USE CODE
C                      IF USE CODE IS NON-ZERO THEN OPFIELD IS USED
C
C       5    OPFLD   FOUR WORD ARRAY WHOSE FUNCTION IS DETERMINED BY USECD
C                      USECD=0: NOP NO OPFLD
C                      USECD=1: OPFLD IS A DATA MASK
C                      USECD=2: OVERWRITE PROGRAM SOURCE WITH OPFLD
C                      USECD=3: USE OPFLD FOR PLOOP LOC. VALUE WHEN LOOPING
C
C       C    PATWD   NUMBER OF WORDS PER PATTERN
C       C    RLOAD   FLAG TO DETERMINE HOW MANY TIMES SETUPS WILL BE DONE
C       C    ERFLG   EXPECTED VALUE(TYPE2),ERROR INDICATOR(TYPE5)
C       C    NSTUP   NUMBER OF WRITE SETUPS
C       C    NSTAD   LOCATION IN TSLIB OF SETUPS
C       2    WPNOP   WRITE OPERATION
C       5    RUNOP   NUMBER OF 'RUN' OPERATIONS
C       5    RUNAD   LOCATION IN TSLIB OF RUN OPS.
C       C    NRDSU   NUMBER OF READ SETUPS
C       C    NRDAD   LOCATION IN TSLIB OF READ SETUPS
C       2    RPNOP   READ OPERATION
C       2    MASK    DATA MASK (ANDED WITH TEST DATA BEFORE COMPARE)
C       C    ERFMT   ERROR PRINT FORMAT
C       5    GLPLC   LOCATION IN PROGRAM SOURCE TO PUT A NOP
C                    SO THE TEST WILL CONTINUALY RUN ALL PATTERNS
C       5    PLPLC   LOACTION IN PROGRAM SOURCE TO PUT EITHER A NOP
C                    OR OPFLD VALUE SO THE TEST WILL CONTINUALY RUN
C                    ONE PATTERN
C       5    ERRCK   PANNEL OP TO READ ERFLG FROM AP
C       5    EXPOP   ARRAY TO HOLD PANNEL OPS TO READ EXPECTED DATA
C                    BACK FROM THE AP FOR DISPLAY
C       5    ACTOP   ARRAY TO HOLD PANNEL OPS TO READ ACTUAL DATA
C                    BACK FROM THE AP FOR DISPLAY
C       C    ERRSZ   NUMBER OF WORDS IN ERROR TEXT
C       C    TXTAD   LOCATION IN TSLIB OF TEXT
C       C    MPFB    MOST PROBABLE FAILING BOARD NUMBER
C       5    CNTOP   NUMBER OF 'CONTINUE' OPS
C       5    CNTAD   LOCATION IN TSLIB OF CONTINUE OPS
C       5    PCPOP   NUMBER OF 'PLOOP DEPENDENT' CONTINUE OPS
C       5    PCPAD   LOCATION IN TSLIB OF PCPOP'S
C       5    NEROP   NUMBER OF ERROR DISPLAY OPS
C       5    NERAD   LOCATION IN TSLIB OF ERROR DISPLAY OPS
C       2    MSB     FLAG INDICATING NOT TO RUN PATTERNS WHEN SET
C       C    NPATS   NUMBER OF PATTERNS TO RUN
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
        COMMON /FILE/ TSTBL(10),LASTLB(10),LPOINT,TPOINT
        INTEGER       TSTBL,LASTLB,LPOINT,TPOINT
C
C
C==
C------LOCAL STORAGE:
C
        INTEGER TSTPT,INFLAG,EOL,FIRST
        LOGICAL CODE
C
C#
C:
C
C====== INITIALIZE VARIABLES
C
        CALL FILL(IPAR,1,44,0)
C
        IRADX=8
        APNUM=0
        ECHO=0
        FREE=.TRUE.
        CUTIL=0
C
        CALL INITFL
C
C====== PROCESS TESTS
C
10      FIRST=1
        TIP=.FALSE.
C
C====== FISRT IS SET WHEN SETUP IS CALLED TO INITIALIZE
C       VARIABLES TO THEIR DEFAULT VALUES
C       FIRST IS RESET TO ZERO AFTERWARDS FOR NORMAL OPERATION
C
20      CALL SETUP(FIRST)
        FIRST=0
C
C====== SEE IF TESTS HAVE BEEN SELECTED,DELETED, OR RESTORED
C
        CALL LOADTS
C
C====== CHECK FOR RESTART COMMAND
C
        IF(RESTRT.NE.0) TIP=.FALSE.
        RESTRT=0
C
C====== TEST 'TEST IN PROGRESS' FLAG
C
        IF(TIP) GOTO 40
C
        PASS=1
        ERROR=0
C
C====== RESET INLIB POINTERS
C
30      INFLAG=1
        TSTPT=1
        EOL=0
C
C====== CHECK FOR MANUAL TEST MODE OR AUTOMATIC MODE
C
40      IF(SKIP.NE.0) TIP=.FALSE.
        SKIP=0
        IF(FREE) GOTO 70
C
C====== MANUAL MODE (RUN OPERATOR SELECTED TESTS)
C       IF THERE IS A TEST IN PROGRESS (FROM WAIT ON ERROR OR OPERATOR
C       INTERVENTION) THEN SKIP TO 'RUN TEST' RIGHT AWAY
C
        IF(TIP) GOTO 60
C
C====== TEST FOR END OF TEST TABLE
C
50      IF(TSTPT.GT.TSTEND) GOTO 100
C
C====== GET TYPE CASE AND ADDRESS FOR RNTST
C
        ITYPE=INLIB(TSTPT,1)
        CASE=INLIB(TSTPT,2)
        ADDR=INLIB(TSTPT,3)
        TSTPT=TSTPT+1
C
C====== RNTST IS NOW EXECUTED WHICH SELECTS THE CORRECT
C       PROCESSOR FOR THE TABLE
C
60      CALL RNTST
C
C====== TIP IS SET EITHER ON OPERATIOR INTERVENTION
C       OR WHEN AN ERROR OCCURS AND 'WAIT ON ERROR'
C       HAS BEEN SET
C
        IF(TIP) GOTO 20
        GOTO 50
C
C====== AUTOMATIC MODE
C       CHECK FOR TEST IN PROGRESS AS ABOVE
C
70      IF(TIP) GOTO 90
C
C====== GET TYPE CASE AND ADDR
C
80      CALL GETTST(INFLAG,EOL)
        INFLAG=0
C
C====== CHECK FOR END OF PASS
C
        IF(EOL.EQ.1) GOTO 100
C
90      CALL RNTST
C
C====== TEST FLAGS FOR OPERATOR INPUT
C
        IF(TIP) GOTO 20
        GOTO 80
C
C====== PASS CONTROL
C
100     CALL PPASS(CODE)
C
C====== CODE SET INDICATES THAT THE PASS LIMIT HAS BEEN REACHED
C
        IF(CODE) GOTO 20
        GOTO 30
        END
C****** RNTST = SELECT TYPE PROCESSOR AND RUN TEST= REL 5.0  , NOV 79 *********
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : RNTST                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *    ENTRY       : TYPE                                                     *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : CHECKS TYPE OF TEST AND IF IN RANGE CALLS APPROPRIATE    *
C  *                  TYPE PROCESSOR.                                          *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE RNTST
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C
C------LOCAL VARIABLES
C
       INTEGER NUMB(6)
C
C#
C:
C
C====== CHECK TYPE AND CALL APPROPRIATE PROCESSOR
C
        IF(ITYPE.LT.0) RETURN
        CALL FNDVLD(TSLIB,ADDR,INTFLG,I)
        IF(I.NE.0) RETURN
C
C====== PRINT TEST NAMES
C
        IF(NAMES.EQ.0) GOTO 20
        CALL TNUM(ITYPE,CASE,NUMBER)
        CALL I2ASCI(6,CASE,NUMB,8,1)
        WRITE(ITTO,10)NUMBER,ITYPE,(NUMB(M),M=4,6)
10      FORMAT(1X,1H(,I3,1H),6H TEST=,I2,1H.,3A1)
          IF(ECHO.NE.0) WRITE(FUNIT,10)NUMBER,ITYPE,(NUMB(M),M=4,6)
C
20      IF(ITYPE.EQ.2) GOTO 40
        IF(ITYPE.EQ.5) GOTO 50
        IF(ITYPE.EQ.6) RETURN
C
C====== ERROR: TYPE MESSAGE
C
        WRITE(ITTO,30)ITYPE,(NUMB(M),M=4,6)
30      FORMAT(1X,I2,1H.,3A1,15H - INVALID TYPE)
                IF(ECHO.NE.0) WRITE(FUNIT,30)ITYPE,(NUMB(M),M=4,6)
        RETURN
C
C====== TYPE 2 PROCESSOR
C
40      CALL TYPE2
        RETURN
C
C====== TYPE 5 PROCESSOR
C
50      CALL TYPE5
        RETURN
        END
C****** FNDVLD = DETERMINE IF TEST IS ACTIVE = REL 5.0  , NOV 79 **************
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : FNDVLD                                                   *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *    ENTRY       : TSLIB   TEST TABLE ARRAY                                 *
C  *                  ADDR    ADDRESS OF TEST IN TSLIB                         *
C  *                  INTFLG  INTERRUPT BOARD OPTION FLAG                      *
C  *                                                                           *
C  *    EXIT        : I       0 FOR ACTIVE                                     *
C  *                          1 FOR DISABLED                                   *
C  *                                                                           *
C  *    FUNCTION    : CHECKS TO SEE IF A TEST REQUIRES THAT THE INTERRUPT/RTC  *
C  *                  BOARD IS PRESENT.  IF NOT THEN THE TEST IS DESIGNATED    *
C  *                  INACTIVE                                                 *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE FNDVLD(TSLIB,ADDR,INTFLG,I)
C
        INTEGER TSLIB(9999),ADDR,INTFLG,I
C
        I=0
C
C====== GET NUMBER OF 'ACTIVITY' FLAGS
C
        N=ADDR+2
        IACTFL=IRSH16(TSLIB(N),13)
C
C====== TEST FOR INTERRUPT OPTION (IF IT EXISTS DONT BOTHER TESTING FLAGS)
C
        IF(INTFLG.EQ.1) RETURN
C
C====== IF NO FLAGS RETURN
C
        IF(IACTFL.EQ.0) RETURN
C
C====== LOOP THROUGH FLAGS TESTING FOR INTERRUPT FLAG VALUE (54)
C
        K=ADDR+3
        KK=K+IACTFL
C
        DO 10 J=K,KK
        IF(IAND16(TSLIB(J),255).EQ.54) I=1
10      CONTINUE
C
        RETURN
        END
C****** INTRV = DETECT OPERATOR INTERVENTION                = REL 5.0  , NOV 79
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : INTRV                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : TIP (OPERATOR INTERVENTION/TEST IN PROGRESS FLAG) IS SET *
C  *                  IF THE USER REQUESTS A BREAK.  OTHERWISE IT IS RESET.    *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
C#
C:
        SUBROUTINE INTRV(TIP)
        LOGICAL TIP
C
C====== TERM SENDS BACK I AS NON ZERO IF THE MSB
C       OF THE SENSE SWITCH ON THE HOST IS SET
C
        CALL TERM(I)
C
        TIP=I.NE.0
C
        RETURN
        END
C****** PPASS = PRINT PASS INFORMATION = REL 5.0  , NOV 79 ********************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : PPASS                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *    EXIT        : CODE IS RETURNED TRUE IF PASS OR ERROR LIMIT HAS BEEN    *
C  *                  REACHED                                                  *
C  *                                                                           *
C  *    FUNCTION    : PRINT OUT THE NUMBER OF ERRORS DETECTED AND THE NUMBER OF*
C  *                  PASSES COMPLETED.                                        *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE PPASS(CODE)
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C
C------LOCAL STORAGE:
C
        LOGICAL CODE
C#
C:
C
C====== CODE IS SET TO TRUE IF PASS LIMIT HAS BEEN REACHED
C       IT IS INITIALY SET TO FALSE
C
        CODE=.FALSE.
C
C====== TEST FOR TYPE OUT DISABLED IF IT IS THEN RETURN
C
        IF(TD.NE.0) RETURN
C
C====== TEST FOR INFINITE PASSES BETWEEN PRINT OUT (BETWN=0)
C
        IF(BETWN.EQ.0) RETURN
C
C====== TEST TO SEE IF IT IS TIME TO PRINT PASS INFO.
C
        IF(MOD(PASS,BETWN).NE.0) GOTO 20
          WRITE(ITTO,10)ERROR,PASS
10        FORMAT(1X,17H UFT100 -,ERRORS=,I6, 8H PASSES=,I6)
            IF(ECHO.NE.0) WRITE(FUNIT,10)ERROR,PASS
20      PASS=PASS+1
C
C====== IF PSLIM IS ZERO THEN PRINT INFINITE PASSES
C
        IF(PSLIM.EQ.0) GOTO 60
C
C====== CHECK FOR PASS LIMIT REACHED
C
        IF(PASS.LT.PSLIM) GOTO 40
C
        WRITE(ITTO,30)
30      FORMAT(19H PASS LIMIT REACHED)
          IF(ECHO.NE.0) WRITE(FUNIT,30)
        CODE=.TRUE.
C
C====== CHECK FOR ERROR LIMIT
C
40      IF(ERLIM.EQ.0) GOTO 60
        IF(ERROR.LT.ERLIM) GOTO 60
        WRITE(ITTO,50)
50      FORMAT(12H ERROR LIMIT)
          IF(ECHO.NE.0) WRITE(FUNIT,50)
        CODE=.TRUE.
60      RETURN
        END
C****** INITFL = OPEN FILES, INITIALIZE BUFFERS= REL 5.0  , NOV 79 ************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : INITFL                                                   *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THE TEST FILES ARE OPENED, THE UTILITY FILE IS READ,     *
C  *                  A DIRECTORY OF THE UTILITY FILE IS CREATED, THE TABLE OF *
C  *                  CONTENTS OF THE TEST FILE IS READ, AND THE TEST BUFFER IS*
C  *                  INITIALIZED.                                             *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE INITFL
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C          COMMON SYSTEM HAS TWO VARIABLES AND APPLY TO THE
C          FPS100 SYSTEM APEX
C
C          SUPER SET TO ZERO TELLS APEX TO RUN IN 'AP120B' MODE
C          START SET TO ONE TELLS APEX THAT THE PS BOOT IS AT
C                LOCATION 1
C
        COMMON /SYSTEM/ SUPER,START,BOOT
        INTEGER SUPER,START,BOOT
C
C
C==
C
C------ LOCAL STORAGE:
C
        INTEGER TSFL(4),UTFL(4),TCFL(4),LINE(50)
C%.R 850 NOVA
        DATA TSFL/2HTS,2HFL,2H.D,2HAT/
        DATA UTFL/2HUT,2HFL,2H.D,2HAT/
        DATA TCFL/2HTC,2HFL,2H.D,2HAT/
C%.E NOVA
C
C#
C:
        SUPER=0
        START=0
        BOOT=1
C%.R 858 NOVA
        LUN1=11
        LUN2=12
        LUN3=13
C%.E NOVA
        ENBUF=1700
C
C====== OPEN TEST FILES
C
        IF(IOCNTL(1,0,0,IVL).NE.0) CALL SETUP(-2)
        IF(IOCNTL(7,TSFL,8,LUN1).NE.0) CALL SETUP(-2)
        IF(IOCNTL(7,UTFL,8,LUN2).NE.0) CALL SETUP(-2)
        IF(IOCNTL(7,TCFL,8,LUN3).NE.0) CALL SETUP(-2)
C
C====== GET UTILITY FILE
C
        READ(LUN2,10) UTLEND
10      FORMAT(I5)
        NN=1
20      READ(LUN2,30)LINE
30      FORMAT(50A1)
        DO 40 I=1,50,5
         II=I
        TSLIB(NN)=IBNHX(LINE,II)
        NN=NN+1
        IF(NN.GT.UTLEND) GOTO 50
40      CONTINUE
        GOTO 20
50      CONTINUE
C
C====== LOAD TABLE OF CONTENTS OF TEST FILE
C
        READ(LUN3,10) TOCEND
        READ(LUN3,60) ((TOC(I,J),J=1,4),I=1,TOCEND)
60      FORMAT(4I7)
C
C====== BUILD UTILITY FILE TOC
C
        N=1
        I=1
70      UTLIB(I,1)=TSLIB(N+1)
        UTLIB(I,2)=N
        N=N+TSLIB(N+2)
        I=I+1
        IF(N.LT.UTLEND) GOTO 70
        UTLBEN=I-1
        RETURN
        END
C****** LOADTS = SETUP SELECTED TEST INFORMATION = REL 5.0  , NOV 79 **********
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : LOADTS                                                   *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THE TEST BUFFER IN IPAR IS SEARCHED TO SEE IF TESTS HAVE *
C  *                  BEEN SELECTED, THE FREE FLAG IS SET DEPENDING ON TESTS   *
C  *                  BEING SELECTED. THEN THE DELETE AND RESTORE BUFFERS IN   *
C  *                  IPAR ARE SEARCHED AND THE SELECTED TESTS ARE THEN        *
C  *                  DISABLED OR ENABLED.                                     *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE LOADTS
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
        COMMON /FILE/ TSTBL(10),LASTLB(10),LPOINT,TPOINT
        INTEGER       TSTBL,LASTLB,LPOINT,TPOINT
C
C
C
C==
C
C------LOCAL STORAGE:
C
        INTEGER SKP,LINE(50)
C#
        DATA LASTLB/10*0/
C:
C
C====== INITIALIZE TEST TABLE
C
        CALL FILL(TSTBL,1,10,0)
C
C====== SELECT TESTS
C
        L=0
        DO 10 J=1,TOCEND
          ITEMP=43-J/16
          JJ=J
          K=IAND16(IPAR(ITEMP),ILSH16(1,MOD(JJ,16)))
          IF(K.EQ.0) GOTO 10
          L=L+1
          IF(L.GT.10) GOTO 20
          TSTBL(L)=J
10      CONTINUE
C
C====== SET 'FREE' FOR MANUAL/AUTO
C
20      FREE=L.EQ.0
C
C====== CHECK FOR CHANGE IN REQUESTED TESTS
C
        DO 30 J=1,10
          IF(TSTBL(J).NE.LASTLB(J)) GOTO 40
30      CONTINUE
        GOTO 50
C
C====== SET LAST TESTS TO CURRENT TESTS
C
40      CALL MOVE(TSTBL,1,10,LASTLB,1)
C
C====== IF TESTS ARE SELECTED MAKE SURE 'TEST IN PROGRES'
C       FLAG IS RESET
C
        TIP=.FALSE.
C
C====== SELECT DELETES
C
50      DO 60 J=1,TOCEND
          ITEMP=23-J/16
          JJ=J
          K=IAND16(IPAR(ITEMP),ILSH16(1,MOD(JJ,16)))
          IF(K.NE.0) TOC(J,1)=IOR16(TOC(JJ,1),IPFIX(-32768.))
          IF(K.NE.0) TIP=.FALSE.
60      CONTINUE
C
C====== CLEAR DELETE FIELD
C
        CALL FILL(IPAR,14,23,0)
C
C====== SELECT RESTORES
C
        DO 70 J=1,TOCEND
          ITEMP=33-J/16
          JJ=J
          K=IAND16(IPAR(ITEMP),ILSH16(1,MOD(JJ,16)))
          IF(K.NE.0) TOC(J,1)=IAND16(TOC(JJ,1),32767)
          IF(K.NE.0) TIP=.FALSE.
70      CONTINUE
C
C====== CLEAR RESTORE FIELD
C
        CALL FILL(IPAR,24,33,0)
C
C====== CHECK FOR TESTS TO LOAD
C
        IF(FREE) RETURN
C
C====== LOAD SELECTED TESTS
C
        LIBEND=0
        LOAD=UTLEND+1
C
C====== REWIND FILE AND START LOADING
C
        N=IOCNTL(6,0,0,LUN1)
C
        IPOINT=1
        TSTEND=0
        DO 150 J=1,10
          IF(TSTBL(J).EQ.0) GOTO 160
          ITEM=TSTBL(J)
C
C====== GET NUMBER OF RECORDS TO SKIP TO FIND DESIRED ONE
C
          SKP=TOC(ITEM,3)-IPOINT
          IPOINT=TOC(ITEM+1,3)
C
          IF(SKP.EQ.0) GOTO 100
          DO 90 N=1,SKP
            READ(LUN1,80)JUNK
80          FORMAT(A1)
90        CONTINUE
C
C====== NOW TEST CAN BE LOADED
C
100       NLOAD=LOAD+TOC(ITEM,4)-1
          IF(NLOAD.GT.ENBUF) GOTO 160
C
        NN=LOAD
110     READ(LUN1,120)LINE
120     FORMAT(50A1)
        DO 130 I=1,50,5
          II=I
        TSLIB(NN)=IBNHX(LINE,II)
        NN=NN+1
        IF(NN.GT.NLOAD) GOTO 140
130     CONTINUE
        GOTO 110
140     CONTINUE
C
          LIBEND=LIBEND+1
          INLIB(LIBEND,1)=TOC(ITEM,1)
          INLIB(LIBEND,2)=TOC(ITEM,2)
          INLIB(LIBEND,3)=LOAD
C
          LOAD=LOAD+TOC(ITEM,4)
          TSTEND=TSTEND+1
C
150     CONTINUE
160     RETURN
        END
C****** GETTST = GET TYPE, CASE, ADDR FOR TEST CONTROLLER = REL 5.0  , NOV 79 *
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : GETTST                                                   *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *    ENTRY       : INFLAG (REWIND TEST FILE)                                *
C  *                                                                           *
C  *    EXIT        : EOL (END OF LIBRARY/PASS FLAG)                           *
C  *                                                                           *
C  *    FUNCTION    : THIS PROGRAM BUILDS A TEST BUFFER FROM THE TEST FILE ON T*
C  *                  DISK.  IT PASSES THE TYPE CASE AND ADDRESS BACK TO THE   *
C  *                  CALLING PROGRAM..  WHEN THE MEMORY BUFFER IS EXHUASTED TH*
C  *                  NEXT SET OF TESTS IS LOADED.  WHEN THE END OF THE DISK   *
C  *                  FILE HAS BEEN REACHED THE EOL FLAG IS SET TO INDICATE    *
C  *                  A COMPLETE PASS.                                         *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE GETTST(INFLAG,EOL)
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
        COMMON /FILE/ TSTBL(10),LASTLB(10),LPOINT,TPOINT
        INTEGER       TSTBL,LASTLB,LPOINT,TPOINT
C
C
C
C==
C
C------LOCAL STORAGE:
C
        INTEGER INFLAG,LOAD,EOL,LINE(50)
C
C#
C:
C
C====== EOL IS END OF LIB. INDICATOR
C
        EOL=0
C
C====== TEST INFLAG FOR A LIBRARY REWIND
C
        IF(INFLAG.EQ.0) GOTO 20
C
10      LIBEND=0
        LPOINT=1
        TPOINT=1
        N=IOCNTL(6,0,0,LUN1)
C
C====== CHECK TO SEE IF AT END OF INLIB (GET MORE TESTS OFF DISK)
C       OR IF AT END OF TABLE OF CONTENTS (REWIND TEST FILE AND START OVER)
C
20      IF(LPOINT.GT.LIBEND.AND.TPOINT.GT.TOCEND) GOTO 80
        IF(LPOINT.LE.LIBEND) GOTO 100
C
        LIBEND=0
C
        LOAD=UTLEND+1
C
C====== TEST FOR INLIB FULL
C
30      IF(LIBEND.EQ.25) GOTO 90
C
C====== TEST FOR TEST BUFFER FULL
C
        IF(LOAD+TOC(TPOINT,4) .GE. ENBUF) GOTO 90
        NLOAD=LOAD+TOC(TPOINT,4)-1
C
C====== GET TEST DATA FROM FILE
C
        NN=LOAD
40      READ(LUN1,50)LINE
50      FORMAT(50A1)
        DO 60 I=1,50,5
         II=I
        TSLIB(NN)=IBNHX(LINE,II)
        NN=NN+1
        IF(NN.GT.NLOAD) GOTO 70
60      CONTINUE
        GOTO 40
70      CONTINUE
C
C====== BUILD 'INLIB'
C
        LIBEND=LIBEND+1
        INLIB(LIBEND,1)=TOC(TPOINT,1)
        INLIB(LIBEND,2)=TOC(TPOINT,2)
        INLIB(LIBEND,3)=LOAD
C
        LOAD=TOC(TPOINT,4)+LOAD
        TPOINT=TPOINT+1
C
C====== TEST FOR END OF LIBRARY (EOL)
C
        IF(TPOINT .GT. TOCEND) GOTO 90
C
        GOTO 30
C
C====== END OF LIB.
C
80      EOL=1
        RETURN
C
C====== DONE LOADING FROM FILE
C
90      TSTEND=LOAD -1
        LPOINT=1
C
C====== GET TYPE,CASE,AND ADDER FOR CONTROLLER
C
100     ITYPE=INLIB(LPOINT,1)
        CASE=INLIB(LPOINT,2)
        ADDR=INLIB(LPOINT,3)
C
        LPOINT=LPOINT+1
C
        RETURN
        END
C****** TYPE2 = TYPE TWO TABLE PROCESSOR = REL 5.0  , NOV 79 ******************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : TYPE2                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *    ENTRY       : TYPE CASE ADDR                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THIS SUBROUTINE RUNS TYPE TWO TABLES.  DECD2 IS CALLED   *
C  *                  TO DECODE THE TABLE.  TYPE2 CALLS THE INTERPRETER TO     *
C  *                  EXECUTE THE PANEL CODE AND MICRO-CODE, THEN THE RESULTS  *
C  *                  OBTAINED FROM THE LITES REG IS COMPARED AGAINST THE INPUT*
C  *                  PATTERN (OR EXPECTED DATA DEPENDING ON THE MSB FLAG BEING*
C  *                  SET IN THE RLOAD WORD).  IF AN ERROR IS INDICATED THE    *
C  *                  ERROR MESSAGE PRINTER (EMH) IS CALLED.                   *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE TYPE2
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C
C------LOCAL STORAGE:
        INTEGER PLP,PATT,PATERN(36)
        LOGICAL ERR2
        DATA PATERN/ -1,0,21845,-21846,1,-2,2,-3,4,-5,8,-9,16,-17,32,
     +              -33,64,-65,128,-129,256,-257,512,-513,1024,-1025,
     +             2048,-2049,4096,-4097,8192,-8193,16384,-16385,
     +                0,32767 /
C
C#
C:
C
C====== THIS SETS A -32768 BECAUSE THIS NUMBER CANNOT BE DATA'ED ON
C       ALL COMPUTERS
C
        PATERN(35)=IPFIX(-32768.)
C
C====== DONT RE-DECODE TABLE IF RETURNING FROM:
C       OPERATOR INTERVENTION, WAIT ON ERROR, OR HALT FLAG
C
        ERR2=.FALSE.
        IF(TIP) GOTO 20
C
C====== EXTRACT INFORMATION FROM TABLE
C
10      CALL DECD2
C
C====== DO READ SETUPS DEPENDING ON 'RLOAD' FLAG
C
20      TIP=.FALSE.
        CALL DORLD
C
C====== TEST IF PATTERNS ARE REQUIRED
C
        IF(MSB.NE.0) GOTO 30
        PATT=PATERN(PTPTR)
C
C====== DO WRITE PANEL OPERATION
C
        CALL WRTOP(WPNOP,PATT)
C
C====== READ SETUPS
C
30      CALL DOWOP(NRDSU,NRDAD)
C
C====== READ PANEL OPERATION - RESULT TO ILITES
C
        CALL RDOP(RPNOP)
        ILITES=IAND16(ILITES,MASK)
        PATT=IAND16(PATT,MASK)
        IF(MSB.NE.0) PATT=ERFLG
C
40      IF(PATT.EQ.ILITES) GOTO 50
C
C====== PUT EXPECTED AND ACTUAL DATA FOR ERROR PRINT
C
        ERR2=.TRUE.
        EXPDT(1)=PATT
        ACTDT(1)=ILITES
        CALL EMH
        ERROR=ERROR+1
C
C====== TEST FOR "WAIT ON ERROR"
C
        IF(WAIT.NE.0) TIP=.TRUE.
        IF(TIP) RETURN
C
C====== CHECK FOR I/O RESET FLAG AND HALT EACH PASS FLAG
C
50      IF(IORST.NE.0) CALL APRSET
          CALL INTRV(TIP)
        IF(HALT.NE.0) TIP=.TRUE.
        HALT=0
        IF(TIP) RETURN
C
C====== TEST FOR 'L' FLAG SET
C
        IF(PLOOP.NE.0.AND.ERR2) GOTO 20
          PTPTR=PTPTR+1
C
C====== TEST FOR ALL PATTERNS RUN
C
        IF(MSB.NE.0) GOTO 60
        IF(PTPTR.GT.NPATS) GOTO 60
        GOTO 20
C
C====== CHECK FOR OPERATOR INTERVENTION
C
60      CALL INTRV(TIP)
        IF(TIP) RETURN
C
        IF(GLOOP.NE.0.AND.ERR2) GOTO 10
        RETURN
        END
C****** DECD2 = DECODE TYPE 2 TABLES = REL 5.0  , NOV 79 **********************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : DECD2                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *    ENTRY       : ADDR                                                     *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : DECD2 EXTRACTS THE TABLE INFORMATION FOR THE TYPE2       *
C  *                  PROCESSOR.  IT SCANS THE TABLE PUTTING THE INFORMATION   *
C  *                  IN THE TBLVAR COMMON BLOCK.                              *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE DECD2
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C#
C:
C
C====== SET A TEMPORARY VARIABLE TO POINT AFTER TYPE,CASE, AND SIZE
C
        N=ADDR+2
                IACTFL=IRSH16(TSLIB(N),13)
                N=N+IACTFL+1
C
C====== INITIALIZE COMMON TABLE
C
        CALL FILL(IRAY,4,45,0)
C
C====== SET STANDARD NUMBER OF PATTERNS AND ERROR FORMAT
C
        PTPTR=1
        NPATS=35
        ERFMT=3
        PATWD=1
C
        RLOAD=IAND16(TSLIB(N),32767)
        MSB=IAND16(TSLIB(N),IPFIX(-32768.))
        N=N+1
C
        IF(MSB.EQ.0) GOTO 10
C
C====== EXPECTED DATA FOLLOWS IF MSB .NE. 0
C
        ERFLG=TSLIB(N)
        N=N+1
        NPATS=0
C
C====== GET NUMBER OF SETUPS
C
10      NSTUP=TSLIB(N)
        NSTAD=N+1
        N=N+NSTUP+1
C
C====== GET WRITE PANEL OP
C
        WPNOP=TSLIB(N)
        N=N+1
C
C====== READ SETUPS
C
        NRDSU=TSLIB(N)
        NRDAD=N+1
        N=N+NRDSU+1
C
C====== READ PANEL OP
C
        RPNOP=TSLIB(N)
        N=N+1
C
C====== DATA MASK
C
        MASK=TSLIB(N)
        N=N+1
C
C====== ERROR TEXT LENGTH AND LOCATION
C
        ERRSZ=TSLIB(N)
        TXTAD=N+1
        N=N+ERRSZ+1
C
C====== MOST PROBABLE FAILING BOARD
C
        MPFB=TSLIB(N)
C
        RETURN
        END
C****** SETUP = PROVIDE LINKAGE WITH PARAMETER ROUTINE = REL 5.0  , NOV 79 ****
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : SETUP                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *    ENTRY       : FLAG (SETS VARIABLES TO DEFAULT WHEN TRUE)               *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : SETUP HAS THE TABLE OF AVAILABLE COMMANDS TO THE         *
C  *                  CONTROLLER.  IT CALLS CSI WHICH PARSES AND INTERPRETS THE*
C  *                  COMMANDS AND SETS THE VALUES IN IPAR THAT CORRESPOND TO  *
C  *                  THE COMMANDS IN THE TABLE                                *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
C
        SUBROUTINE SETUP(FLAG)
C
C
C-------PARAMETERS:
C
        INTEGER FLAG
C
C       FLAG    = NON-ZERO CAUSES A CALL TO INPARM TO INITIALIZE
C                 ALL VALUES IN THE TABLE.
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C------LOCAL STORAGE:
C
        INTEGER TABLE(19,12)
C
C      SBREAK
C
        DATA TABLE( 1, 1),TABLE( 1, 2),TABLE( 1, 3)   /1HS,1HB,1HR/
        DATA TABLE( 1, 4),TABLE( 1, 5),TABLE( 1, 6)   /1HE,1HA,1HK/
        DATA TABLE( 1, 7),TABLE( 1, 8),TABLE( 1, 9)   /3,1,0/
        DATA TABLE( 1,10),TABLE( 1,11),TABLE( 1,12)   /0,0,0/
C
C      ERRLIM
C
        DATA TABLE( 2, 1),TABLE( 2, 2),TABLE( 2, 3)   /1HE,1HR,1HR/
        DATA TABLE( 2, 4),TABLE( 2, 5),TABLE( 2, 6)   /1HL,1HI,1HM/
        DATA TABLE( 2, 7),TABLE( 2, 8),TABLE( 2, 9)   /2,2,64/
        DATA TABLE( 2,10),TABLE( 2,11),TABLE( 2,12)   /0,0,0/
C
C      GLOOP
C
        DATA TABLE( 3, 1),TABLE( 3, 2),TABLE( 3, 3)   /1HG,1HL,1HO/
        DATA TABLE( 3, 4),TABLE( 3, 5),TABLE( 3, 6)   /1HO,1HP,1H /
        DATA TABLE( 3, 7),TABLE( 3, 8),TABLE( 3, 9)   /1,3,0/
        DATA TABLE( 3,10),TABLE( 3,11),TABLE( 3,12)   /0,1,0/
C
C      PASLIM
C
        DATA TABLE( 4, 1),TABLE( 4, 2),TABLE( 4, 3)   /1HP,1HA,1HS/
        DATA TABLE( 4, 4),TABLE( 4, 5),TABLE( 4, 6)   /1HL,1HI,1HM/
        DATA TABLE( 4, 7),TABLE( 4, 8),TABLE( 4, 9)   /2,4, 1/
        DATA TABLE( 4,10),TABLE( 4,11),TABLE( 4,12)   /0,0,10/
C
C      BRKADD
C
        DATA TABLE( 5, 1),TABLE( 5, 2),TABLE( 5, 3)   /1HB,1HR,1HK/
        DATA TABLE( 5, 4),TABLE( 5, 5),TABLE( 5, 6)   /1HA,1HD,1HD/
        DATA TABLE( 5, 7),TABLE( 5, 8),TABLE( 5, 9)   /2,5,0/
        DATA TABLE( 5,10),TABLE( 5,11),TABLE( 5,12)   /0,0,0/
C
C      SKIP
C
        DATA TABLE( 6, 1),TABLE( 6, 2),TABLE( 6, 3)   /1HS,1HK,1HI/
        DATA TABLE( 6, 4),TABLE( 6, 5),TABLE( 6, 6)   /1HP,1H ,1H /
        DATA TABLE( 6, 7),TABLE( 6, 8),TABLE( 6, 9)   /1,6,0/
        DATA TABLE( 6,10),TABLE( 6,11),TABLE( 6,12)   /0,1,0/
C
C      DMACNT
C
        DATA TABLE( 7, 1),TABLE( 7, 2),TABLE( 7, 3)   /1HD,1HM,1HA/
        DATA TABLE( 7, 4),TABLE( 7, 5),TABLE( 7, 6)   /1HC,1HN,1HT/
        DATA TABLE( 7, 7),TABLE( 7, 8),TABLE( 7, 9)   /2,7,64/
        DATA TABLE( 7,10),TABLE( 7,11),TABLE( 7,12)   /0,0,0/
C
C      D
C
        DATA TABLE( 8, 1),TABLE( 8, 2),TABLE( 8, 3)   /1HD,1H ,1H /
        DATA TABLE( 8, 4),TABLE( 8, 5),TABLE( 8, 6)   /1H ,1H ,1H /
        DATA TABLE( 8, 7),TABLE( 8, 8),TABLE( 8, 9)   /1,8,0/
        DATA TABLE( 8,10),TABLE( 8,11),TABLE( 8,12)   /0,1,0/
C
C      H
C
        DATA TABLE( 9, 1),TABLE( 9, 2),TABLE( 9, 3)   /1HH,1H ,1H /
        DATA TABLE( 9, 4),TABLE( 9, 5),TABLE( 9, 6)   /1H ,1H ,1H /
        DATA TABLE( 9, 7),TABLE( 9, 8),TABLE( 9, 9)   /1,9,0/
        DATA TABLE( 9,10),TABLE( 9,11),TABLE( 9,12)   /0,1,0/
C
C      I
C
        DATA TABLE(10, 1),TABLE(10, 2),TABLE(10, 3)   /1HI,1H ,1H /
        DATA TABLE(10, 4),TABLE(10, 5),TABLE(10, 6)   /1H ,1H ,1H /
        DATA TABLE(10, 7),TABLE(10, 8),TABLE(10, 9)   /1,10,0/
        DATA TABLE(10,10),TABLE(10,11),TABLE(10,12)   /0,1,0/
C
C      L
C
        DATA TABLE(11, 1),TABLE(11, 2),TABLE(11, 3)   /1HL,1H ,1H /
        DATA TABLE(11, 4),TABLE(11, 5),TABLE(11, 6)   /1H ,1H ,1H /
        DATA TABLE(11, 7),TABLE(11, 8),TABLE(11, 9)   /1,11,0/
        DATA TABLE(11,10),TABLE(11,11),TABLE(11,12)   /0,1,0/
C
C      W
C
        DATA TABLE(12, 1),TABLE(12, 2),TABLE(12, 3)   /1HW,1H ,1H /
        DATA TABLE(12, 4),TABLE(12, 5),TABLE(12, 6)   /1H ,1H ,1H /
        DATA TABLE(12, 7),TABLE(12, 8),TABLE(12, 9)   /1,12,0/
        DATA TABLE(12,10),TABLE(12,11),TABLE(12,12)   /0,1,0/
C
C      BETWN
C
        DATA TABLE(13, 1),TABLE(13, 2),TABLE(13, 3)   /1HB,1HE,1HT/
        DATA TABLE(13, 4),TABLE(13, 5),TABLE(13, 6)   /1HW,1HN,1H /
        DATA TABLE(13, 7),TABLE(13, 8),TABLE(13, 9)   /2,13,1/
        DATA TABLE(13,10),TABLE(13,11),TABLE(13,12)   /0,0,10/
C
C      DELETE
C
        DATA TABLE(14, 1),TABLE(14, 2),TABLE(14, 3)   /1HD,1HE,1HL/
        DATA TABLE(14, 4),TABLE(14, 5),TABLE(14, 6)   /1HE,1HT,1HE/
        DATA TABLE(14, 7),TABLE(14, 8),TABLE(14, 9)   /4,23,0/
        DATA TABLE(14,10),TABLE(14,11),TABLE(14,12)   /1,159,0/
C
C      RESTORE
C
        DATA TABLE(15, 1),TABLE(15, 2),TABLE(15, 3)   /1HR,1HE,1HS/
        DATA TABLE(15, 4),TABLE(15, 5),TABLE(15, 6)   /1HT,1HO,1HR/
        DATA TABLE(15, 7),TABLE(15, 8),TABLE(15, 9)   /4,33,0/
        DATA TABLE(15,10),TABLE(15,11),TABLE(15,12)   /1,159,0/
C
C      TESTS
C
        DATA TABLE(16, 1),TABLE(16, 2),TABLE(16, 3)   /1HT,1HE,1HS/
        DATA TABLE(16, 4),TABLE(16, 5),TABLE(16, 6)   /1HT,1HS,1H /
        DATA TABLE(16, 7),TABLE(16, 8),TABLE(16, 9)   /4,43,0/
        DATA TABLE(16,10),TABLE(16,11),TABLE(16,12)   /1,159,0/
C
C      NAMES
C
        DATA TABLE(17, 1),TABLE(17, 2),TABLE(17, 3)   /1HN,1HA,1HM/
        DATA TABLE(17, 4),TABLE(17, 5),TABLE(17, 6)   /1HE,1HS,1H /
        DATA TABLE(17, 7),TABLE(17, 8),TABLE(17, 9)   /3,44,1/
        DATA TABLE(17,10),TABLE(17,11),TABLE(17,12)   /-1,0,0/
C
C      INTFLG
C
       DATA TABLE(18, 1),TABLE(18, 2),TABLE(18, 3)   /1HI,1HN,1HT/
       DATA TABLE(18, 4),TABLE(18, 5),TABLE(18, 6)   /1HF,1HL,1HG/
       DATA TABLE(18, 7),TABLE(18, 8),TABLE(18, 9)   /3,45,1/
       DATA TABLE(18,10),TABLE(18,11),TABLE(18,12)   /-1,0,0/
C
C      RSTART
C
       DATA TABLE(19, 1),TABLE(19, 2),TABLE(19, 3)  /1HR,1HS,1HT/
       DATA TABLE(19, 4),TABLE(19, 5),TABLE(19, 6)  /1HA,1HR,1HT/
       DATA TABLE(19, 7),TABLE(19, 8),TABLE(19, 9)  /3,46,0/
       DATA TABLE(19,10),TABLE(19,11),TABLE(19,12)  /-1,0,0/
C
C#
C:
C
C====== SET UP NUMBER OF POSIBLE TESTS
C
        TABLE(14,11)=TOCEND
        TABLE(15,11)=TOCEND
        TABLE(16,11)=TOCEND
C
C====== IF FLAG = -2 CLOSE THE FILES AND EXIT
C
        IF(FLAG.NE.-2) GOTO 20
        WRITE(ITTO,10)
10      FORMAT(25H ****** FILE ERROR ******/13H TEST ABORTED)
        CALL EXIT
C
C====== PRINT THE TEST HEADER
C
20      WRITE(ITTO,30)
        IF(ECHO.EQ.1)WRITE(FUNIT,30)
30      FORMAT(1X,36H *** UFT100 - REL 0.0 - JUN. 79  ***/)
C
C====== INITIALIZE PARAMETERS IF FLAG IS SET
C
        IF (FLAG .EQ. 1) CALL CSI(TABLE,19,IPAR,46,1)
C
C====== ALLOW PARAMETERS TO BE VIEWED/CHANGED
C
        CALL CSI(TABLE,19,IPAR,46,0)
C
        RETURN
        END
C****** ASSIST = HELP DISPLAY - TEST DIRECTORY = REL 5.0  , NOV 79 ************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : ASSIST                                                   *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THIS PROGRAM IS CALLED BY OPERATOR INPUT WHENEVER        *
C  *                  "HELP","A", OR "?" IS TYPED IN BY THE OPERATOR.  WHEN    *
C  *                  REQUESTED , IF THE OPERATOR TYPES IN A ZERO THE TEST     *
C  *                  DIRECTORY IS PRINTED OUT.  IF A ONE IS ENTERED THEN THE  *
C  *                  LIST OF AVAILABLE COMMANDS IS PRINTED OUT.               *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE ASSIST(I,J,ECHO)
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
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C------LOCAL STORAGE:
C
        INTEGER AST,BLANK,ECHO,NUMB(6)
          DATA AST/1H*/,BLANK/1H /
C
C#
C:
        WRITE(ITTO,10)
          IF(ECHO.NE.0) WRITE(FUNIT,10)
10      FORMAT(26H TYPE 0 FOR TEST DIRECTORY/
     +         24H      1 FOR HELP DISPLAY)
C
        READ(ITTI,20)N
20      FORMAT(I1)
          IF(ECHO.NE.0) WRITE(FUNIT,30)N
30        FORMAT(1X,I2)
C
        IF(N.EQ.0) GOTO 110
C
        WRITE(ITTO,40)
        IF(ECHO.NE.0) WRITE(FUNIT,40)
C
40      FORMAT (
     +  39H APNUM=   - CLOSE CURRENT AP OPEN AP(N)                     /
     +  44H CLRFLG   - SETS ALL FLAGS TO DEFAULT VALUES                /
     +  44H CLRALL   - RESETS PROGRAM TO DEFAULT VALUES                /
     +  36H ECHO     - OPEN FILE TO ECHO OUTPUT                        /
     +  )
C
        WRITE(ITTO,50)
        IF(ECHO.NE.0) WRITE(FUNIT,50)
C
50      FORMAT (
     +  27H -ECHO    - CLOSE ECHO FILE                                 /
     +  31H RUN      - <E> EXECUTE PROGRAM                             /
     +  47H HELP     - <A> <?> PRINT AVAILABLE COMMANDS                /
     +  30H RADIX=   - SET I/O RADIX TO N                              /
     +  )
C
        WRITE(ITTO,60)
        IF(ECHO.NE.0) WRITE(FUNIT,60)
C
60      FORMAT (
     +  40H STAT     - PRINT CURRENT PROGRAM STATUS                    /
     +  43H STOP     - <QUIT> EXIT TO OPERATING SYSTEM                 /
     +  40H BETWN=   - SET PASSES BETWEEN PRINTOUTS                    /
     +  52H ERRLIM=  - MAXIMUM ERRORS BEFORE RETURNING TO INPUT        /
     +  )
C
        WRITE(ITTO,70)
        IF(ECHO.NE.0) WRITE(FUNIT,70)
C
70      FORMAT (
     +  51H PASLIM=  - PASSES TO RUN BEFORE RETURNING TO INPUT         /
     +  41H SBREAK   - FLAG TO ENABLE PS BREAK POINT                   /
     +  46H GLOOP    - FLAG TO ENABLE LOOP ON ENTIRE TEST              /
     +  37H BRKADD=  - AP PS ADDRESS TO BREAK ON                       /
     +  )
C
        WRITE(ITTO,80)
        IF(ECHO.NE.0) WRITE(FUNIT,80)
C
80      FORMAT (
     +  49H SKIP     - FLAG TO SKIP CURRENTLY EXECUTING TEST           /
     +  30H DMACNT=  - WAIT COUNT FOR DMA                              /
     +  28H D        - TYPE OUT DISABLE                                /
     +  27H H        - HALT AFTER TEST                                 /
     +  )
C
        WRITE(ITTO,90)
        IF(ECHO.NE.0) WRITE(FUNIT,90)
C
90      FORMAT (
     +  37H I        - I/O RESET AFTER EACH TEST                       /
     +  35H L        - LOOP ON ERROR CONDITION                         /
     +  46H W        - RETURN TO INPUT ON ERROR CONDITION              /
     +  28H DELETE=  - INACTIVATE TESTS                                /
     +  )
C
        WRITE(ITTO,100)
        IF(ECHO.NE.0) WRITE(FUNIT,100)
C
100     FORMAT (
     +  29H RESTORE= - RE-ACTIVATE TESTS                               /
     +  31H TESTS=   - SELECT TESTS TO RUN                             /
     +  46H NAMES    - FLAG TO ENABLE PRINTING TEST NAMES              /
     +  55H INTFLG   - FLAG TO ENABLE/DISABLE RTC/INTERRUPT TESTS      /
     +  55H RSTART   - FLAG TO BEGIN EXECUTING TESTS FROM START        /
     +  )
        RETURN
C
C====== TESTS DIRECTORY
C
110     WRITE(ITTO,120)
          IF(ECHO.NE.0) WRITE(FUNIT,120)
120     FORMAT(32H NUMBER     TYPE CASE   INACTIVE)
C
        DO 140 N=1,TOCEND
          ITYPE=IAND16(TOC(N,1),32767)
          ICASE=TOC(N,2)
          INAC=BLANK
C
          IF(ITYPE.NE.TOC(N,1)) INAC=AST
C
        CALL I2ASCI(6,ICASE,NUMB,8,1)
          WRITE(ITTO,130) N,ITYPE,(NUMB(M),M=4,6),INAC
            IF(ECHO.NE.0) WRITE(FUNIT,130) N,ITYPE,(NUMB(M),M=4,6),INAC
130       FORMAT(3X,I3,7X,I1,3X,3A1,9X,A1)
140     CONTINUE
        RETURN
        END
C****** TYPE5 = TYPE 5 TEST PROCESSOR = REL 5.0  , NOV 79 *********************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : TYPE5                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *    ENTRY       : TYPE CASE ADDR                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THIS ROUTINE EXECUTES TYPE5 TABLES.  DECD5 IS CALLED TO  *
C  *                  EXTRACT THE INFORMATION FROM THE TABLE.  THE ACTUAL AND  *
C  *                  EXPECTED DATA IS COMPARED IN THE AP AND PASSES AN ERROR  *
C  *                  INDICATOR FLAG BACK WHICH THE HOST CHECKS  AGAINST  THE  *
C  *                  ACCEPTABLE VALUE SUPPLIED IN THE TABLE                   *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE TYPE5
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C
        COMMON /APRG/ SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +                 RESET,IFSTAT,AMASK,APMAE,MAE
C
        INTEGER SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +          RESET,IFSTAT,AMASK,APMAE,MAE
C
C------LOCAL STORAGE:
C
        INTEGER GSAVE(4)
C
C#
C:
C
C====== DONT RE-DECODE TABLE IF RETURNING FROM:
C       OPERATOR INTERVENTION, WAIT ON ERROR, OR HALT FLAG
C
        IF(TIP) GOTO 20
C
C====== EXTRACT TABLE
C
10      CALL DECD5
C
C====== LOAD REQUESTED TYPE 6 UTILITY
C
        CALL UTYLD(IERR)
          IF(IERR.NE.0) RETURN
C
C====== DO SETUPS
C
20      TIP=.FALSE.
        RLOAD=1
        CALL DORLD
C
C====== SAVE THE OP AT PSA=PLPLC & GLPLC
C
        CALL SVPLP(PSAVE,PLPLC)
        CALL SVPLP(GSAVE,GLPLC)
C
C====== DO RUNOPS
C
        CALL DOWOP(RUNOP,RUNAD)
C
C====== WAIT FOR AP HALT OR OPERATOR INTERVENTION
C
30      CALL WAITA
        IF(TIP) RETURN
C
C====== DO READ SETUPS
C
        CALL DOWOP(NRDSU,NRDAD)
C
C====== GET ERROR CHECK VALUE
C
        CALL RDOP(ERRCK)
C
C====== SEE IF THE RESULT  IS TO BE MASKED
C
        IF(USECD.NE.1) GOTO 40
        ILITES=IAND16(ILITES,OPFLD(4))
C
C====== TEST FOR ERROR CONDITION
C
40      IF(ILITES.NE.ERFLG) GOTO 50
C
C====== TEST FOR "I/O RESET" FLAG ON
C
        IF(IORST.NE.0) CALL APRSET
C
C====== CHECK FOR "HALT EACH PASS" FLAG SET
C
        IF(HALT.NE.0) TIP=.TRUE.
        HALT=0
        IF(TIP) RETURN
C
        RETURN
C
C====== ERROR HANLDING SECTION
C
50      IF(PATWD.EQ.0) GOTO 60
C
C====== GET EXPECTED / ACTUAL DATA
C
        CALL GETEXP
        CALL GETACT
60      PTPTR=0
C
        ERROR=ERROR+1
        CALL EMH
C
C====== TEST FOR OPERATOR INTERVENTION
C
        CALL INTRV(TIP)
C
C====== TEST FOR WAIT ON ERROR FLAG
C
        IF(WAIT.NE.0) TIP=.TRUE.
C
C====== RETURN NORMAL OP-CODE TO PS @ PSA=PLPLC & GLPLC
C
        CALL PLDPS(PSAVE,PLPLC)
        CALL PLDPS(GSAVE,PLPLC)
        IF(TIP) RETURN
C
C====== TEST FOR LOOPING CONDITIONS
C
        IF(GLOOP.NE.0) GOTO 80
        IF(PLOOP.NE.0) GOTO 80
        IF(PATWD.EQ.0) RETURN
C
C====== DO NON-LOOP CONT. OPS
C
        CALL DOWOP(PCPOP,PCPAD)
70      CALL DOWOP(CNTOP,CNTAD)
        GOTO 30
C
C====== TEST FOR TYPE-OUT DISABLED IF NOT DONT GO INTO AP LOOP
C
80      IF(TD.EQ.0) GOTO 70
C
C====== PUT PLOOP-OP AND/OR GLOOP-OP INTO AP
C
        IF(PLOOP.NE.0) CALL PLDPS(PLPOP,PLPLC)
        IF(GLOOP.NE.0) CALL PLDPS(PLPOP,GLPLC)
C
C====== THE AP IS IN AN INFINITE LOOP
C       THE ONLY WAY OUT IS OPERATOR TYPE-IN
C
90      CALL DOWOP(CNTOP,CNTAD)
        CALL WAITA
        IF(.NOT.TIP) GOTO 90
        CALL PLDPS(PSAVE,PLPLC)
        CALL PLDPS(GSAVE,GLPLC)
        CALL APRSET
        RETURN
C
        END
C****** WAITA = WAIT FOR AP HALT OR OPERATOR TYPE-IN = REL 5.0  , NOV 79 ******
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : WAITA                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *    EXIT        : TIP (OPERATOR INTERVENTION FLAG)                         *
C  *                                                                           *
C  *    FUNCTION    : THIS ROUTINE TESTS IF THE AP IS HALTED OR IF THE         *
C  *                  OPERATOR HAS REQUESTED A BREAK.  RETURNS TIP AS          *
C  *                  TRUE IF OPERATOR HAS TYPED IN. IT LOOPS UNTIL EITHER     *
C  *                  CASE OCCURS                                              *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE WAITA
C==
C
C
C-------COMMON STORAGE:
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C
        COMMON /APRG/ SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +                 RESET,IFSTAT,AMASK,APMAE,MAE
C
        INTEGER SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +          RESET,IFSTAT,AMASK,APMAE,MAE
C#
C:
C
C====== TEST FOR OPERATOR TYPE-IN
C
10      CALL INTRV(TIP)
        IF(TIP) RETURN
C
C====== TEST FOR AP HALT
C
        CALL APIN(N,FN)
        IF(N.LT.0) RETURN
        GOTO 10
C
        END
C****** DECD5 = DECODE TYPE 5 INFORMATION = REL 5.0  , NOV 79 *****************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : DECD5                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *    ENTRY       : ADDR                                                     *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THE TABLE IS SCANED AND THE INFORMATION IS EXTRACTED AND *
C  *                  PUT INTO A COMMON AREA FOR TYPE5 TO OPERATE ON.          *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE DECD5
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
        COMMON /APRG/ SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +                 RESET,IFSTAT,AMASK,APMAE,MAE
C
        INTEGER SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +          RESET,IFSTAT,AMASK,APMAE,MAE
C#
C:
C
C====== SET A TEMPORARY VARIABLE TO POINT AFTER TYPE,CASE, AND ADDR
C
        N=ADDR+2
                IACTFL=IRSH16(TSLIB(N),13)
                N=N+IACTFL+1
C
C====== INITIALIZE COMMON TABLE
C
        CALL FILL(IRAY,4,45,0)
C
C====== UTILITY PACKAGE NUMBER
C
        UTPKG=TSLIB(N)
        N=N+1
C
C====== USE-CODE
C
        USECD=TSLIB(N)
        N=N+1
C
        IF(USECD.EQ.0) GOTO 20
        IF(USECD.NE.1) GOTO 10
C
C====== GET RESULT MASK
C
        OPFLD(4)=TSLIB(N)
        N=N+1
        GOTO 20
C
C====== GET 4 WORD OPFLD
C
10      OPFLD(1)=TSLIB(N)
        OPFLD(2)=TSLIB(N+1)
        OPFLD(3)=TSLIB(N+2)
        OPFLD(4)=TSLIB(N+3)
        N=N+4
C
C====== PATTERNS PER WORD (0 MEANS NO PATTERNS)
C
20      PATWD=TSLIB(N)
        N=N+1
C
C====== WRITE SETUPS
C
        NSTUP=TSLIB(N)
        NSTAD=N+1
        N=N+NSTUP+1
C
C====== RUNOPS
C
        RUNOP=TSLIB(N)
        RUNAD=N+1
        N=N+RUNOP+1
C
C====== READ SETUPS
C
        NRDSU=TSLIB(N)
        NRDAD=N+1
        N=N+NRDSU+1
C
C====== NON-LOOP OPS / REGULAR CONT. OPS
C
        PCPOP=IRSH16(TSLIB(N),8)
        CNTOP=IAND16(TSLIB(N),255)
        PCPAD=N+1
        N=N+PCPOP+1
        CNTAD=N
        N=N+CNTOP
C
C====== ERROR FORMAT
C
        ERFMT=TSLIB(N)
        N=N+1
C
C====== ERROR FLAG
C
        ERFLG=TSLIB(N)
        N=N+1
C
C====== GLOOP LOC.
C
        GLPLC=TSLIB(N)
        N=N+1
C
C====== PLOOP LOC.
C
        PLPLC=TSLIB(N)
        N=N+1
C
C====== ERROR CHECK OP.
C
        ERRCK=TSLIB(N)
        N=N+1
C
C====== EXPECTED ERROR OPS
C
        IF(PATWD.EQ.0) GOTO 50
        DO 30 I=1,PATWD
          EXPOP(I)=TSLIB(N)
          N=N+1
30      CONTINUE
C
C====== ACTUAL ERROR OPS
C
        DO 40 I=1,PATWD
          ACTOP(I)=TSLIB(N)
          N=N+1
40      CONTINUE
C
C====== ERROR OPS.
C
50      NEROP=TSLIB(N)
        NERAD=N+1
        N=N+NEROP+1
C
C====== TEXT LOCATIONS
C
        ERRSZ=TSLIB(N)
        TXTAD=N+1
        N=N+ERRSZ+1
C
C====== MOST PROBABLE FAILING BOARD
C
        MPFB=TSLIB(N)
C
        RETURN
        END
C****** FILL = FILL ARRAY WITH SPECIFIED VALUE = REL 5.0  , NOV 79 ************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : FILL                                                     *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
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
        DIMENSION JCARD(80)
C
C====== FILL JCARD J THROUGH JCARD ,JLAST WITH NCH
C
        DO 10 JNOW=J,JLAST
10      JCARD(JNOW)=NCH
        RETURN
        END
C****** MOVE = MOVE ONE ARRAY TO ANOTHER = REL 5.0  , NOV 79 ******************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : MOVE                                                     *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
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
        DIMENSION JCARD(80), KCARD(80)
C
C====== MOVE JCARD TO KCARD
C
        DO 10 JNOW=J,JLAST
        KNOW=K+JNOW-J
10      KCARD(KNOW)=JCARD(JNOW)
        RETURN
        END
C****** SVPLP = LOAD P.S. FROM AP TO HOST = REL 5.0  , NOV 79 *****************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : SVPLP                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : LOAD ONE WORD OF AP PROGRAM SOURCE INTO HOST.            *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE SVPLP(SAVE,PSA)
C
C
C
        COMMON /APRG/ SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +                 RESET,IFSTAT,AMASK,APMAE,MAE
C
        INTEGER SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +          RESET,IFSTAT,AMASK,APMAE,MAE
C
C------LOCAL STORAGE:
C
        INTEGER SAVE(4),PSA,ITMA,ISWR
C#
C:
C
C====== SAVE SWR & TMA
C
        CALL APIN(ISWR,SWR)
        CALL APEXAM(ITMA,4,0)
C
C====== LOAD OP @ PSA INTO HOST
C
        CALL APEXAM(SAVE,9,PSA)
C
C====== RESTORE SWR & TMA
C
        CALL APDEP(ITMA,4,0)
        CALL APOUT(ISWR,SWR)
C
        RETURN
        END
C****** PLDPS = PUT P.S. INTO AP AT PSA = REL 5.0  , NOV 79 *******************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : PLDPS                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : PUTS 4 16 BIT HOST WORDS INTO AP PROGRAM SOURCE AT       *
C  *                  SPECIFIED LOCATION.                                      *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE PLDPS(SAVE,PSA)
C
        COMMON /APRG/ SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +                 RESET,IFSTAT,AMASK,APMAE,MAE
C
        INTEGER SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +          RESET,IFSTAT,AMASK,APMAE,MAE
C
C------LOCAL STORAGE:
C
        INTEGER SAVE(4),PSA,ITMA,ISWR
C#
C:
C
C====== SAVE SWR & TMA
C
        CALL APIN(ISWR,SWR)
        CALL APEXAM(ITMA,4,0)
C
C====== PUT SAVE INTO AP @ PSA
C
        CALL APDEP(SAVE,9,PSA)
C
C====== RESTORE SWR & TMA
C
        CALL APDEP(ITMA,4,0)
        CALL APOUT(ISWR,SWR)
C
        RETURN
        END
C****** GETEXP = GET EXPECTED VALUE FROM AP = REL 5.0  , NOV 79 ***************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : GETEXP                                                   *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : LOAD EXECPECTED DATA VALUE FOR TYPE5                     *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE GETEXP
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C#
C:
C
C====== RDOP WILL USE EXPECTED OPS TO GET DATA FROM AP
C
        DO 10 I=1,PATWD
          CALL RDOP(EXPOP(I))
          EXPDT(I)=ILITES
10      CONTINUE
        RETURN
        END
C****** GETACT = GET ACTUAL DATA FROM AP = REL 5.0  , NOV 79 ******************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : GETACT                                                   *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : LOADS ACTUAL DATA VALUE FROM AP FOR TYPE5                *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE GETACT
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C#
C:
C
C====== RDOP READS AP USING ACTOP'S
C
        DO 10 I=1,PATWD
          CALL RDOP(ACTOP(I))
          ACTDT(I)=ILITES
10      CONTINUE
        RETURN
        END
C****** DORLD = DO SETUPS DEPENDING ON RLOAD = REL 5.0  , NOV 79 **************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : DORLD                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : IF RLOAD IS ZERO THEN THE PROGRAM DOES NOTHING.          *
C  *                  IF RLOAD IS ONE THEN THE INTERPRETER IS CALLED TO        *
C  *                  DO THE SETUPS AND RLOAD IS THEN SET TO ZERO              *
C  *                  IF RLOAD IS ANY OTHER VALUE THEN THE INTERPRETER IS CALLE*
C  *                  AND RLOAD IS UNAFECTED.                                  *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE DORLD
C==
C-------COMMON STORAGE:
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER ITTI,ITTO,FUNIT
C
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C#
C:
C
C====== TEST RLOAD TO SEE IF SETUPS ARE REQUIRED
C
        IF(RLOAD.EQ.0) RETURN
C
        CALL DOWOP(NSTUP,NSTAD)
C
        IF(RLOAD.EQ.1) RLOAD=0
        RETURN
        END
C****** TYPE6 = TYPE 6 TABLE PROCESSOR = REL 5.0  , NOV 79 ********************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : TYPE6                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : TYPE6 TESTS TO SEE IF THE REQUESTED UTILITY TABLE IS AP  *
C  *                  RESEDENT, IF NOT IT IS LOADED BY DOWOP                   *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE TYPE6
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C
C------LOCAL STORAGE:
C
        INTEGER TCUTIL,TY6CN,TY6AD
C#
C:
C
C====== SET TEMPORARY VARIABLE TO POINT TO CUTIL
C
        N=ADDR+3
C
        TCUTIL=TSLIB(N)
        N=N+2
C
C====== TEST FOR CUTIL CHANGE (CUTIL = CURRENTLY LOADED UTILITY)
C
        IF(TCUTIL.EQ.0) GOTO 10
          CUTIL=TCUTIL
C
C====== DO PANEL PROGRAM
C
10      TY6CN=TSLIB(N)
        TY6AD=N+1
C
        CALL DOWOP(TY6CN,TY6AD)
C
        RETURN
        END
C****** UTYLD = UTILITY LOADER FOR TYPE5 = REL 5.0  , NOV 79 ******************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : UTYLD                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THIS PROGRAM LOADS THE REQUESTED UTILITY PACKAGE(TYPE6)  *
C  *                  IT ALSO TAKES CARE OF INTERPRETING THE USE-CODE AND      *
C  *                  DEPENDING ON ITS VALUE TAKES THE PROPER ACTION WITH      *
C  *                  OPFLD(USE CODE  OPERAND FIELD)                           *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE UTYLD(IERR)
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C
C------LOCAL STORAGE:
C
        INTEGER IADDR
C#
C:
        IERR=0
C
C====== CLEAR PLOOP OP
C
        CALL FILL(PLPOP,1,4,0)
C
C====== USE CODE OF 3: USER DEFINED PLOOP OP
C
        IF(USECD.EQ.3) CALL MOVE(OPFLD,1,4,PLPOP,1)
C
C====== TEST FOR NO UTILITY NEEDED
C
        IF(UTPKG.EQ.0) RETURN
C
C====== USE CODE OF 2: FILL P.S. WITH OPFLD
C
        IF(USECD.NE.2) GOTO 20
C
C====== APDEP (9) SELECTS PROGRAM SOURCE
C
        DO 10 I=1,4096
          N=I-1
          CALL APDEP(OPFLD,9,N)
10      CONTINUE
        CUTIL=1
        RETURN
C
C====== TEST FOR UTILITY ALREADY LOADED
C
20      IF(UTPKG.EQ.CUTIL) RETURN
C
C====== TEST FOR USER BOMB P.S.
C
        IF(UTPKG.NE.1) GOTO 30
        CUTIL=1
        RETURN
C
C====== LOAD UTILITY
C
30      IADDR=ADDR
        CALL GETUTL(UTPKG)
        IF(ADDR.NE.0) GOTO 50
          WRITE(ITTO,40)UTPKG
            IF(ECHO.NE.0) WRITE(FUNIT,40)UTPKG
40        FORMAT(15H UTILITY NUMBER,I3,10H NOT FOUND)
        IERR=1
        RETURN
C
50      CALL TYPE6
        ADDR=IADDR
C
        RETURN
        END
C****** GETUTL = GET ADDRESS OF UTILITY IN TSLIB = REL 5.0  , NOV 79 **********
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : GETUTL                                                   *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : LOOKS UP THE TYPE6 TABLE IN UTLIB(DIRECTORY OF TYPE6)    *
C  *                  AND IF IT IS FOUND PASSES BACK ITS ADDRESS IN TSLIB      *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE GETUTL(ICASE)
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C#
C:
C
C====== INITIALIZE ADDR
C
        ADDR=0
C
C====== SCAN UTILITY DIRECTORY FOR MATCH OF CASE
C
        DO 10 I=1,UTLBEN
          IF(UTLIB(I,1).EQ.ICASE) GOTO 20
10      CONTINUE
        RETURN
C
C====== FOUND
C
20      ADDR=UTLIB(I,2)
        RETURN
        END
C****** EMH = PRINT ERROR MESSAGES FOR TESTS = REL 5.0  , NOV 79 **************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : EMH                                                      *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : PRINTS ERROR TEXT AND INFORMATION WHEN CALLED BY THE TYPE*
C  *                  PROCESSORS.                                              *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
        SUBROUTINE EMH
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C
C------LOCAL STORAGE:
        INTEGER IMPOP,IMPAD,IERRSZ,ITXTAD
C#
C:
C
C====== TEST FOR TYPE-OUT DISABLED
C
        IF(TD.NE.0) RETURN
C
C====== GET IMPLIED ERROR OPS (IMBEDDED IN ERROR TEXT)
C
        IMPAD=TXTAD
        IMPOP=0
C
        N=TXTAD+ERRSZ
        DO 10 I=TXTAD,N
          IF(TSLIB(I).GE.0) GOTO 20
          IMPOP=IMPOP+1
10      CONTINUE
        I=N
C
20      IERRSZ=ERRSZ-IMPOP
        ITXTAD=I
C
C====== PRINT TEST NAME AND TEXT
C
        NTXTAD=ITXTAD+IERRSZ-1
C
        CALL PERMS(ITYPE,CASE,TSLIB,ITXTAD,NTXTAD)
C
C====== PRINT EXPECTED AND ACTUAL DATA
C
        CALL PEXAC
C
C====== PRINT IMPLIED ERROR OPS
C
        CALL ERDIS(IMPOP,IMPAD)
C
C====== PRINT EXPLICIT ERROR OPS
C
        CALL ERDIS(NEROP,NERAD)
C
C====== PRINT MOST PROBABLE FAILING BOARD
C
        IF(MPFB.EQ.0) RETURN
        WRITE(ITTO,30)MPFB
        IF(ECHO.NE.0) WRITE(FUNIT,30)MPFB
30      FORMAT(29H MOST PROBABLE FAILING BOARD=,I5)
        RETURN
        END
C****** PERMS = PRINT ERROR TEXT FOR EMH = REL 5.0  , NOV 79 ******************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : PERMS                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : PRINT ERROR TEXT IN THE TEST TABLES                      *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE PERMS(ITYPE,CASE,TSLIB,ITXTAD,NTXTAD)
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
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        EQUIVALENCE     (ENTRY(1),IRADX),(ENTRY(2),APNUM),
     +                  (ENTRY(3),ECHO ),(ENTRY(4),LUN  ),
     +                  (ENTRY(5),NZMD ),(ENTRY(6),QUITAT)
C
C
C
C------LOCAL STORAGE:
C
        INTEGER ITYPE,CASE,TSLIB(9999),ITXTAD,NTXTAD,LINE(80),EOL,
     +          NUMB(6)
C#
C:
C
C====== DETERMINE MAXIMUM LINE LENGTH
C
        JTXTAD=NTXTAD
        IF(NTXTAD-ITXTAD+1.GT.40) JTXTAD=ITXTAD+39
C
C====== UNPACK TEXT LINE
C
        CALL UNPK(TSLIB,ITXTAD,JTXTAD,LINE,1)
        CALL IBLNK(EOL)
C
C====== PRINT FIRST LINE OF TEXT
C
        N=(JTXTAD-ITXTAD+1)*2
        J=1
        K=ISCAN(LINE,J,N,EOL)
        IF(K.EQ.0) K=N
        CALL I2ASCI(6,CASE,NUMB,8,1)
        WRITE(ITTO,10)ITYPE,(NUMB(M),M=4,6),(LINE(L),L=J,K)
        IF(ECHO.NE.0)WRITE(FUNIT,10)ITYPE,(NUMB(M),M=4,6),
     +  (LINE(L),L=J,K)
10      FORMAT(//6H TEST=,I2,1H.,3A1,3H - ,80A1)
C
C====== PRINT REMAINING LINES (IF ANY)
C
20      J=K+1
        IF(J.GE.N) RETURN
        K=ISCAN(LINE,J,N,EOL)
          IF(K.EQ.J) GOTO 20
          IF(K.EQ.0) K=N
        WRITE(ITTO,30)(LINE(L),L=J,K)
          IF(ECHO.NE.0) WRITE(FUNIT,30)(LINE(L),L=J,K)
30      FORMAT(1X,80A1)
          GOTO 20
        END
C****** ISCAN = SCAN FOR WORD IN ARRAY = REL 5.0  , NOV 79 ********************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : ISCAN                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : SCAN ARRAY FOR FIRST OCCURANCE OF WORD                   *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
C#
C:
        FUNCTION ISCAN(IRAY,I,J,ICHR)
        DIMENSION IRAY(9999)
        IF(I.GT.J) GOTO 30
        DO 10 K=I,J
        IF(IRAY(K).EQ.ICHR) GOTO 20
10      CONTINUE
        ISCAN=0
        RETURN
20      ISCAN=K
        RETURN
C
30      CONTINUE
        DO 40 K=J,I
        L=J-K+I
        IF(IRAY(L).EQ.ICHR) GOTO 50
40      CONTINUE
        ISCAN=0
        RETURN
50      ISCAN=L
        RETURN
        END
C****** PEXAC = PRINT EXPECTED AND ACTUAL DATA = REL 5.0  , NOV 79 ************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : PEXAC                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : PRINT THE EXPECED AND ACTUAL ARRAYS FOR ERROR PRINTS     *
C  *                  CALLED FROM EMH                                          *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE PEXAC
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C------LOCAL STORAGE:
        INTEGER BLANK,PRINT(30),NUM(6)
        DATA BLANK /1H /
C#
C:
        IF(PATWD.EQ.0) RETURN
C
        CALL FILL(PRINT,1,30,BLANK)
C
C====== DO EXPECTED DATA
C
        IPOINT=1
        DO 10 I=1,PATWD
          CALL I2ASCI(6,EXPDT(I),NUM,IRADX,0)
          CALL MOVE(NUM,1,6,PRINT,IPOINT)
          IPOINT=IPOINT+7
10      CONTINUE
C
        WRITE(ITTO,20)PRINT
          IF(ECHO.NE.0) WRITE(FUNIT,20)PRINT
20      FORMAT(/6H EXP= ,30A1)
C
C====== GET ACTUAL DATA
C
        IPOINT=1
        DO 30 I=1,PATWD
          CALL I2ASCI(6,ACTDT(I),NUM,IRADX,0)
          CALL MOVE(NUM,1,6,PRINT,IPOINT)
          IPOINT=IPOINT+7
30      CONTINUE
C
        WRITE(ITTO,40)PRINT
          IF(ECHO.NE.0) WRITE(FUNIT,40)PRINT
40      FORMAT(6H ACT= ,30A1/)
C
        RETURN
        END
C****** ERDIS = DISPLAY "ERROR OPS" = REL 5.0  , NOV 79 ***********************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : ERDIS                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : GETS VALUES FOR ERROR DISPLAY PANEL OPS AND DISPLAYS     *
C  *                  THE RESULTS ALONG WITH THE REGISTER NAME                 *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE ERDIS(IEROP,IERAD)
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C
C------LOCAL STORAGE:
        INTEGER OPS(20,2),NUM(6)
        INTEGER REGSTR(366)
        DATA REGSTR/
     +              2HMD,2H R,2HEG,2H E,2HXP,2H  ,
     +              2HMD,2H R,2HEG,2H H,2HMA,2HN ,
     +              2HMD,2H R,2HEG,2H L,2HMA,2HN ,
     +              2H@W,2HC ,2H  ,2H  ,2H  ,2H  ,
     +              2H@H,2HMA,2H  ,2H  ,2H  ,2H  ,
     +              2H@C,2HTR,2HL ,2H  ,2H  ,2H  ,
     +              2H@A,2HPM,2HA ,2H  ,2H  ,2H  ,
     +              2H@F,2HOR,2HMA,2HT ,2H  ,2H  ,
     +              2HPS,2HA ,2H  ,2H  ,2H  ,2H  ,
     +              2HDP,2HYL,2H  ,2H  ,2H  ,2H  ,
     +              2HDP,2HYH,2H  ,2H  ,2H  ,2H  ,
     +              2HMD,2HL ,2H  ,2H  ,2H  ,2H  ,
     +              2HMD,2HH ,2H  ,2H  ,2H  ,2H  ,
     +              2HMD,2HE ,2H  ,2H  ,2H  ,2H  ,
     +              2HTM,2HL ,2H  ,2H  ,2H  ,2H  ,
     +              2HTM,2HH ,2H  ,2H  ,2H  ,2H  ,
     +              2HTM,2HE ,2H  ,2H  ,2H  ,2H  ,
     +              2HSW,2HR ,2H  ,2H  ,2H  ,2H  ,
     +              2HFN,2H  ,2H  ,2H  ,2H  ,2H  ,
     +              2HLI,2HTE,2HS ,2H  ,2H  ,2H  ,
     +              2HAP,2HMA,2H  ,2H  ,2H  ,2H  ,
     +              2HHM,2HA ,2H  ,2H  ,2H  ,2H  ,
     +              2HWC,2H  ,2H  ,2H  ,2H  ,2H  ,
     +              2HCT,2HRL,2H  ,2H  ,2H  ,2H  ,
     +              2HFM,2HT ,2HHM,2HAN,2H  ,2H  ,
     +              2HFM,2HT ,2HLM,2HAN,2H  ,2H  ,
     +              2HIF,2H S,2HTA,2HT ,2H  ,2H  ,
     +              2HMA,2HSK,2H  ,2H  ,2H  ,2H  ,
     +              2HAP,2HMA,2HE ,2H  ,2H  ,2H  ,
     +              2HMA,2HE ,2H  ,2H  ,2H  ,2H  ,
     +              2HSP,2HD ,2H  ,2H  ,2H  ,2H  ,
     +              2HMA,2H  ,2H  ,2H  ,2H  ,2H  ,
     +              2HMD,2HL ,2HPA,2HRI,2HTY,2H  ,
     +              2HMD,2HH ,2HPA,2HRI,2HTY,2H  ,
     +              2HMD,2HE ,2HPA,2HRI,2HTY,2H  ,
     +              2HTM,2HE ,2HRE,2HG ,2H  ,2H  ,
     +              2HTM,2HH ,2HRE,2HG ,2H  ,2H  ,
     +              2HTM,2HL ,2HRE,2HG ,2H  ,2H  ,
     +              2HST,2HAT,2H2 ,2H  ,2H  ,2H  ,
     +              2HST,2HAT,2H3 ,2H  ,2H  ,2H  ,
     +              2HPE,2HAD,2H  ,2H  ,2H  ,2H  ,
     +              2HMD,2HCA,2H, ,2HMA,2HE ,2H  ,
     +              2HCC,2HTR,2H  ,2H  ,2H  ,2H  ,
     +              2HCC,2HST,2H  ,2H  ,2H  ,2H  ,
     +              2HCC,2HTL,2H  ,2H  ,2H  ,2H  ,
     +              2HIM,2HAS,2HK ,2H  ,2H  ,2H  ,
     +              2HTM,2HA ,2H  ,2H  ,2H  ,2H  ,
     +              2HDP,2HA ,2H  ,2H  ,2H  ,2H  ,
     +              2HSP,2HAD,2H(S,2HPD,2H) ,2H  ,
     +              2HSP,2HFN,2H  ,2H  ,2H  ,2H  ,
     +              2HST,2HAT,2HUS,2H  ,2H  ,2H  ,
     +              2HDA,2H  ,2H  ,2H  ,2H  ,2H  ,
     +              2HPS,2H0 ,2H  ,2H  ,2H  ,2H  ,
     +              2HPS,2H1 ,2H  ,2H  ,2H  ,2H  ,
     +              2HPS,2H2 ,2H  ,2H  ,2H  ,2H  ,
     +              2HPS,2H3 ,2H  ,2H  ,2H  ,2H  ,
     +              2H(D,2HA),2H  ,2H  ,2H  ,2H  ,
     +              2HDP,2HXE,2H  ,2H  ,2H  ,2H  ,
     +              2HDP,2HXH,2H  ,2H  ,2H  ,2H  ,
     +              2HDP,2HXL,2H  ,2H  ,2H  ,2H  ,
     +              2HDP,2HYE,2H  ,2H  ,2H  ,2H
     +  /
C#
C:
C
C====== TEST FOR NO ERROR DISPLAY
C
        IF(IEROP.EQ.0) RETURN
C
        JERAD=IERAD+IEROP-1
        J=0
C
C====== BUILD TABLE OF REGISTER NAME LOCATION AND VALUE
C
        DO 10 I=IERAD,JERAD
          J=J+1
          CALL DEROP(TSLIB(I),OPS(J,1),OPS(J,2))
10      CONTINUE
C
C====== PRINT ERROR INFORMATION
C
        DO 30 I=1,J
          CALL I2ASCI(6,OPS(I,1),NUM,IRADX,0)
          K=OPS(I,2)
C
C====== TEST FOR INVALID DISPLAY
C
            IF(K.LE.0) GOTO 30
          L=K+5
          WRITE(ITTO,20)(REGSTR(N),N=K,L),NUM
            IF(ECHO.NE.0) WRITE(FUNIT,20)(REGSTR(N),N=K,L),NUM
20        FORMAT(1X,6A2,2X,6A1)
30      CONTINUE
C
        RETURN
        END
C****** DEROP = GET DISPLAY VALUES FROM AP = REL 5.0  , NOV 79 ****************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : DEROP                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : CALLED FROM ERDIS.  GETS REGISTER VALUES BY APPLYING THE *
C  *                  ERROR DISPLAY PANEL OPS TO THE AP AND READING THE RESULTS*
C  *                  BACK TO ERDIS.                                           *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE DEROP(IOP,IVAL,MESAGE)
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
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
        COMMON /APRG/ SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +                 RESET,IFSTAT,AMASK,APMAE,MAE
C
        INTEGER SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +          RESET,IFSTAT,AMASK,APMAE,MAE
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C------LOCAL STORAGE:
C
        INTEGER TROP,ROP,NUM(6)
        INTEGER DANMS(40),NMLNK(94)
        DATA DANMS/
     +          24,175,
     +          5,85,
     +          0,19,
     +          1,25,
     +          2,31,
     +          3,37,
     +          4,43,
     +          25,169,
     +          26,163,
     +          27,193,
     +          28,199,
     +          29,205,
     +          30,241,
     +          31,247,
     +          255,229,
     +          254,235,
     +          248,253,
     +          249,259,
     +          250,265,
     +          251,271/
C
        DATA NMLNK/
     +          1024,49,
     +          1079,223,
     +          1063,217,
     +          1047,211,
     +          1025,181,
     +          1026,187,
     +          1027,277,
     +          1028,283,
     +          1029,289,
     +          1030,301,
     +          1031,307,
     +          1032,313,
     +          1048,319,
     +          1064,325,
     +          1080,331,
     +          1033,337,
     +          1082,13,
     +          1066,7,
     +          1082,1,
     +          1083,355,
     +          1067,349,
     +          1051,343,
     +          1084,55,
     +          1068,61,
     +          1052,361,
     +          1085,67,
     +          1069,73,
     +          1053,79,
     +          1086,295,
     +          1087,85,
     +          1071,91,
     +          1055,97,
     +          1049,97,
     +          1065,91,
     +          1,103,
     +          2,109,
     +          3,115,
     +          4,121,
     +          5,127,
     +          6,133,
     +          7,139,
     +          8,145,
     +          9,151,
     +          11,157,
     +          12,163,
     +          13,169,
     +          14,175/
C#
C:
C
C====== CLEAR UNNEEDED BITS
C
        TROP=IAND16(IOP,32575)
C
C====== TEST FO DEVICE ADDRESS REFERENCE
C
        IF(TROP.EQ.1081) GOTO 30
C
C====== REGULAR REGISTER DISPLAY OP
C
        DO 10 I=1,94,2
          IF(NMLNK(I).EQ.TROP) MESAGE=NMLNK(I+1)
          IF(NMLNK(I).EQ.TROP) GOTO 60
10      CONTINUE
C
        CALL I2ASCI(6,IOP,NUM,8,0)
        WRITE(ITTO,20)NUM
          IF(ECHO.NE.0) WRITE(FUNIT,20)NUM
20      FORMAT(25H ILLEGAL ERROR DISPLAY OP/
     +         8H ERROP= ,6A1)
        MESAGE=-1
        RETURN
C
C====== DEVICE ADDRESS REFERENCE
C       GET VALUE OF DA IN THE AP
C
30      CALL APOUT(1031,FN)
        CALL APIN(ILITES,LITES)
C
C====== SEARCH FOR DA VALUE IN TABLE
C
        DO 40 I=1,40,2
          IF(DANMS(I).EQ.ILITES) MESAGE=DANMS(I+1)
          IF(DANMS(I).EQ.ILITES) GOTO 60
40      CONTINUE
C
        CALL I2ASCI(6,ILITES,NUM,8,0)
        WRITE(ITTO,50)NUM
          IF(ECHO.NE.0) WRITE(FUNIT,50)NUM
50      FORMAT(23H ILLEGAL DEVICE ADDRESS/
     +         5H DA= ,6A1)
        MESAGE=-1
        RETURN
C
C====== GET VALUE FROM THE AP
C
60      ROP=IAND16(IOP,32767)
        CALL RDOP(ROP)
        IVAL=ILITES
        RETURN
        END
C****** WRTOP = DO WRITE PANEL OPERATIONS                   = REL 5.0  , NOV 79
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : WRTOP                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : TAKES PANEL OP FROM TEST TABLE AND SENDS IT TO THE AP    *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE WRTOP(WOP,WDATA)
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C
        COMMON /APRG/ SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +                 RESET,IFSTAT,AMASK,APMAE,MAE
C
        INTEGER SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +          RESET,IFSTAT,AMASK,APMAE,MAE
C
C------LOCAL STORAGE:
C
        INTEGER WOP,WDATA
C#
C:
C
C====== CHECK FOR A NO-OPERATION
C
        IF(WOP.EQ.0) RETURN
C
        IF(IAND16(WOP,2048).EQ.0) GOTO 10
C
C====== PARAMETER (CONTROLLER COMMON) INDIRECT DEPOSIT TO AP
C
        WDATA=ISTIND(WDATA)
        WOP=NAND16(WOP,2048)
        GOTO 20
C
10      IF(IAND16(WOP,4096).EQ.0) GOTO 20
C
C====== TEST TABLE INDIRECT DEPOSIT TO AP
C
        WDATA=TSLIB(ADDR+WDATA)
        WOP=NAND16(WOP,4096)
C
C====== PROCESS PANEL WRITE OP
C
20      IF(IAND16(WOP,512).EQ.0) GOTO 30
C
C====== FN / SWR OP
C
        CALL APOUT(WDATA,SWR)
        CALL APOUT(WOP,FN)
        RETURN
C
C====== OUTPUT TO SELECTED AP REG
C
30      CALL APOUT(WDATA,WOP)
        RETURN
        END
C****** RDOP = DO PANEL READ OPERATION = REL 5.0  , NOV 79 ********************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : RDOP                                                     *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : TAKES PANEL READ OP AND SENDS IT TO THE AP.              *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE RDOP(ROP)
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C
        COMMON /APRG/ SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +                 RESET,IFSTAT,AMASK,APMAE,MAE
C
        INTEGER SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +          RESET,IFSTAT,AMASK,APMAE,MAE
C
C------LOCAL STORAGE:
C
        INTEGER ROP
C#
C:
C
C====== CHECK FOR NO-OPERATION
C
        IF(ROP.EQ.0) RETURN
C
        IF(IAND16(ROP,1024).EQ.0) GOTO 10
C
C====== FN / LITES OPERATION
C
        CALL APOUT(ROP,FN)
        CALL APIN(ILITES,LITES)
        RETURN
C
C====== READ SELECTED REGISTER
C
10      CALL APIN(ILITES,ROP)
        RETURN
        END
C****** BDATA = BLOCK DATA SUBROUTINE = REL 5.0  , NOV 79 *********************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : BDATA                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : INITIALIZE COMMON VAIABLES                               *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
C#
        BLOCK DATA
C
        COMMON /APRG/ SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +                 RESET,IFSTAT,AMASK,APMAE,MAE
C
        INTEGER SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +          RESET,IFSTAT,AMASK,APMAE,MAE
C
        DATA SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +       RESET,IFSTAT,AMASK,APMAE,MAE
     +      /  1, 2,    3,   4,  5, 6,   7,   8,   9,
     +          10,    11,  12,   13, 14/
C
C
C
C:
        END
C****** DOWOP = PANEL CODE INTERPRETER = REL 5.0  , NOV 79 ********************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : DOWOP                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : THIS PROGRAM TAKES THE PANEL CODE AND                    *
C  *                  INTERPRETS IT.  THERE ARE 4 GROUPS OF THINGS THAT CAN BE *
C  *                  DONE:                                                    *
C  *                  1. DO A WRITE OPERATION TO THE AP                        *
C  *                  2. DO A READ OPERATION FROM THE AP                       *
C  *                  3. LOAD AN AP MEMORY (LDSEL)                             *
C  *                  4. SPECIAL OPERATIONS SUCH AS START THE AP, DO DMA'S ETC.*
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE DOWOP(INST,IADD)
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C
        COMMON /APRG/ SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +                 RESET,IFSTAT,AMASK,APMAE,MAE
C
        INTEGER SWR,FN,LITES,APMA,HMA,WC,CTRL,FMTH,FMTL,
     +          RESET,IFSTAT,AMASK,APMAE,MAE
C
C------LOCAL STORAGE:
C
        INTEGER STACK(25),TEMP,ROP,FUNCT
C
C#
C:
C
C====== INITIALIZE STACK POINTER FOR CHAIN
C
        IPOINT=1
C
C====== SAVE SWITCH REG. FOR LATER RESTORATION
C
        CALL APIN(NSWR,SWR)
C
C====== TRANSFER PARAMETERS TO TEMPORARY VARIABLES
C
        NINST=INST
        NADD=IADD
C
10      IF(NINST.LE.0) GOTO 220
C
C====== GET FUNCTION TO PERFORM
C
        FUNCT=TSLIB(NADD)
        NADD=NADD+1
        NINST=NINST-1
C
C====== TEST FOR MEMORY LOAD FUNCTION
C
        IF(IAND16(FUNCT,IPFIX(-32768.)).NE.0) GOTO 20
C
C====== TEST FOR SPECIAL FUNCTION GROUP
C
        IF(IAND16(FUNCT,16384).NE.0) GOTO 70
C
C====== TEST FOR AP READ (RDI)
C
        IF(IAND16(FUNCT,1024).NE.0) GOTO 180
C
C====== WRITE AP FUNCTION
C
        CALL WRTOP(FUNCT,TSLIB(NADD))
        NADD=NADD+1
        NINST=NINST-1
        GOTO 10
C
C====== MEMORY LOAD FUNCTION
C       STRIP FUNCTION BIT (MSB) AND SHIFT TO GET SUB FUNCTION
C
20      TEMP=IRSH16(IAND16(FUNCT,24576),13)+1
        GOTO (30,40,50,60),TEMP
C
C====== S-PAD LOAD
C
30      CALL SPLD(FUNCT,NADD,NINST,TSLIB)
        GOTO 10
C
C====== DATA PAD LOAD
C
40      CALL DPLD(FUNCT,NADD,NINST,TSLIB)
        GOTO 10
C
C====== MAIN DATA LOAD
C
50      CALL MDLD(FUNCT,NADD,NINST,TSLIB)
        GOTO 10
C
C====== PROGRAM SOURCE LOAD
C
60      CALL PSLD(FUNCT,NADD,NINST,TSLIB)
        GOTO 10
C
C====== SPECIAL FUNCTION GROUP
C       MASK AND SHIFT TO GET SUB FUNCTION
C
70      TEMP=IRSH16(IAND16(FUNCT,14336),11)+1
C
C====== MASK OFF AND GET ARGUMENT OF FUNCTION
C
        FUNCT=IAND16(FUNCT,2047)
        IF(TEMP.LT.1.OR.TEMP.GT.6) GOTO 230
C
        GOTO (80,100,110,140,150,160,170),TEMP
C
C====== STEP FUNCTION
C
80      DO 90 J=1,FUNCT
          CALL APOUT(4096,FN)
90      CONTINUE
        GOTO 10
C
C====== FN CONTINUE OR START FUNCTION
C       FN CONTINUE = 8192
C       FN CONTINUE + BREAK ENABLED = 8448
C
100     IFNCNT=8192
        IF(BREAK.NE.0) IFNCNT=8448
C
        CALL WAITA
        IF(TIP) RETURN
        CALL RUNAP(FUNCT,0,BRKADD,IFNCNT)
        GOTO 10
C
C====== CHAIN...DO TYPE6 AS A SUBROUTINE
C       FIRST SEE IF IT IS ALREADY RESEDENT
C
110     IF(FUNCT.EQ.CUTIL) GOTO 10
C
C====== SAVE CURRENT POINTERS, LOOK FOR TYPE6 TABLE, AND RUN IT
C
        CALL PUSH(STACK,IPOINT,NADD)
        CALL PUSH(STACK,IPOINT,NINST)
        CALL PUSH(STACK,IPOINT,ITYPE)
        CALL PUSH(STACK,IPOINT,CASE)
        CALL PUSH(STACK,IPOINT,ADDR)
C
        CALL GETUTL(FUNCT)
        IF(ADDR.EQ.0) GOTO 120
        ADDR=ADDR+3
        IF(TSLIB(ADDR).NE.0) CUTIL=TSLIB(ADDR)
        ADDR=ADDR+2
        NINST=TSLIB(ADDR)
        NADD=ADDR+1
        GOTO 10
C
120     WRITE(ITTO,130)FUNCT
          IF(ECHO.NE.0) WRITE(FUNIT,130)FUNCT
130     FORMAT(8H UTILITY,I3,10H NOT FOUND)
        RETURN
C
C====== DMAGO FUNCTION - INITIATE DMA
C
140     CALL DMAGO(ADDR,NADD,NINST,TSLIB)
        CALL WAITD(DMACNT,TIP)
        IF(TIP) RETURN
        GOTO 10
C
C====== PSDMA - DMA PROGRAM SOURCE TO AP
C       SAVE DEVICE ADDRESS BECAUSE LOADPS CHANGES IT
C
150     CALL APEXAM(IDA,8,0)
        CALL PSDM(FUNCT,NADD,NINST,TSLIB)
          CALL WAITD(DMACNT,TIP)
C
C====== RESTORE DEVICE ADDRESS
C
        CALL APDEP(IDA,8,0)
          IF(TIP) RETURN
        GOTO 10
C
C====== APWAIT - WAIT FOR AP TO HALT
C
160     CALL WAITA
        IF(TIP) RETURN
        GOTO 10
C
C====== DWAIT - WAIT FOR DMA
C
170     CALL WAITD(DMACNT,TIP)
        IF(TIP) RETURN
        GOTO 10
C
C====== RDI - READ AP FUNCTION
C
180     TEMP=IAND16(FUNCT,512)
        ROP=FUNCT
        IF(TEMP.NE.0) GOTO 190
C
        ROP=IAND16(ROP,-1025)
C
190     ROP=IAND16(ROP,15871)
        CALL RDOP(ROP)
C
        IF(IAND16(FUNCT,4096).NE.0) GOTO 200
        IF(IAND16(FUNCT,2048).NE.0) GOTO 210
        GOTO 10
C
C====== TEST TABLE STORE
C
200     TEMP=TSLIB(NADD+ADDR)
        NADD=NADD+1
        NINST=NINST-1
        TSLIB(TEMP)=ILITES
        GOTO 10
C
C====== PARAMETER (CONTROLLER COMMON) STORE
C
210     CALL ISTDEP(ILITES,TSLIB(ADDR+NADD))
        NADD=NADD+1
        NINST=NINST-1
C
        GOTO 10
C
C====== EXIT -
C       FIRST TEST IF THE PROGRAM IS IN A CHAIN
C       IF IT IS POP THE STACK AND CONTINUE
C       IF NOT, RETURN
C
220     IF(IPOINT.LE.1) CALL APOUT(NSWR,SWR)
        IF(IPOINT.LE.1) RETURN
C
C====== RETURN FROM CHAIN
C
        CALL POP(STACK,IPOINT,ADDR)
        CALL POP(STACK,IPOINT,CASE)
        CALL POP(STACK,IPOINT,ITYPE)
        CALL POP(STACK,IPOINT,NINST)
        CALL POP(STACK,IPOINT,NADD)
C
        GOTO 10
C
C====== INVALID PANEL CODE
C
230     IERLOC=NADD-ADDR-1
        WRITE(ITTO,240)IERLOC
          IF(ECHO.NE.0) WRITE(FUNIT,240)IERLOC
240     FORMAT(23H INVALID PANEL CODE AT ,I5,10H. RELETIVE)
        RETURN
        END
C****** PSLD = LOAD PS        = REL 5.0  , NOV 79 *****************************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : PSLD                                                     *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : LOAD PS        FROM THE TABLES THROUGH THE FRONT PANEL   *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE PSLD(IFN,NADD,NINST,TSLIB)
C
C
C------LOCAL STORAGE:
C
        INTEGER TSLIB(9999),WORD(4),PSA,PS
C
C#
C:
        PS=9
C
C====== WORDS TO TRANSFER
C
        NWORD=IAND16(IFN,4095)
C
C====== MEMORY ADDRESS TO SEND TO
C
        PSA=TSLIB(NADD)
        NADD=NADD+1
        NINST=NINST-1
C
C====== SET TRANSFER INHIBIT WORD
C
        ITRANI=IAND16(IFN,4096)
C
        DO 10 I=1,NWORD
C
          WORD(1)=TSLIB(NADD)
          WORD(2)=TSLIB(NADD+1)
          WORD(3)=TSLIB(NADD+2)
          WORD(4)=TSLIB(NADD+3)
C
          NADD=NADD+4
          NINST=NINST-4
C
          IF(ITRANI.NE.0) GOTO 10
C
          CALL APDEP(WORD,PS,PSA)
          PSA=PSA+1
10      CONTINUE
C
        RETURN
        END
C****** SPLD = LOAD S-PAD     = REL 5.0  , NOV 79 *****************************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : SPLD                                                     *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : LOAD S-PAD     FROM THE TABLES THROUGH THE FRONT PANEL   *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE SPLD(IFN,NADD,NINST,TSLIB)
C
C
C------LOCAL STORAGE:
C
        INTEGER TSLIB(9999),WORD(3),SPD,SP
C
C#
C:
        SP=6
C
C====== WORDS TO TRANSFER
C
        NWORD=IAND16(IFN,4095)
C
C====== MEMORY ADDRESS TO SEND TO
C
        SPD=TSLIB(NADD)
        NADD=NADD+1
        NINST=NINST-1
C
C====== SET TRANSFER INHIBIT WORD
C
        ITRANI=IAND16(IFN,4096)
C
        DO 10 I=1,NWORD
C
          WORD(1)=TSLIB(NADD)
C
          NADD=NADD+1
          NINST=NINST-1
C
          IF(ITRANI.NE.0) GOTO 10
C
          CALL APDEP(WORD,SP,SPD)
C
C====== MAKE SURE SPD IS SET
C
          CALL APDEP(SPD,2,0)
          SPD=SPD+1
10      CONTINUE
C
        RETURN
        END
C****** DPLD = LOAD DATA PADS = REL 5.0  , NOV 79 *****************************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : DPLD                                                     *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : LOAD DATA PADS FROM THE TABLES THROUGH THE FRONT PANEL   *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE DPLD(IFN,NADD,NINST,TSLIB)
C
C
C------LOCAL STORAGE:
C
        INTEGER TSLIB(9999),WORD(3),DPA,DP
C
C#
C:
        DPX=12
        DPY=13
C
        DP=DPX
        IF(IAND16(IFN,2048).NE.0) DP=DPY
C
C====== WORDS TO TRANSFER
C
        NWORD=IAND16(IFN,4095)
C
C====== MEMORY ADDRESS TO SEND TO
C
        DPA=TSLIB(NADD)
        NADD=NADD+1
        NINST=NINST-1
C
C====== SET TRANSFER INHIBIT WORD
C
        ITRANI=IAND16(IFN,4096)
C
        DO 10 I=1,NWORD
C
          WORD(1)=TSLIB(NADD)
          WORD(2)=TSLIB(NADD+1)
          WORD(3)=TSLIB(NADD+2)
C
          NADD=NADD+3
          NINST=NINST-3
C
          IF(ITRANI.NE.0) GOTO 10
C
          CALL APDEP(WORD,DP,DPA)
          DPA=DPA+1
10      CONTINUE
C
        RETURN
        END
C****** MDLD = LOAD MAIN DATA = REL 5.0  , NOV 79 *****************************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : MDLD                                                     *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : LOAD MAIN DATA FROM THE TABLES THROUGH THE FRONT PANEL   *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE MDLD(IFN,NADD,NINST,TSLIB)
C
C
C------LOCAL STORAGE:
C
        INTEGER TSLIB(9999),WORD(3),MA,MD
C
C#
C:
        MD=14
C
C====== WORDS TO TRANSFER
C
        NWORD=IAND16(IFN,4095)
C
C====== MEMORY ADDRESS TO SEND TO
C
        MA=TSLIB(NADD)
        NADD=NADD+1
        NINST=NINST-1
C
C====== SET TRANSFER INHIBIT WORD
C
        ITRANI=IAND16(IFN,4096)
C
        DO 10 I=1,NWORD
C
          WORD(1)=TSLIB(NADD)
          WORD(2)=TSLIB(NADD+1)
          WORD(3)=TSLIB(NADD+2)
C
          NADD=NADD+3
          NINST=NINST-3
C
          IF(ITRANI.NE.0) GOTO 10
C
          CALL APDEP(WORD,MD,MA)
          MA=MA+1
10      CONTINUE
C
        RETURN
        END
C****** DMAGO = DMA TO AP MAIN DATA                         = REL 5.0  , NOV 79
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : DMAGO                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : DMA WORDS FROM TEST TABLES TO MAIN DATA                  *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
        SUBROUTINE DMAGO(ADDR,NADD,NINST,TSLIB)
C
C
C------LOCAL STORAGE:
C
        INTEGER TSLIB(9999),ADDR,WC,APMA,CTRL,HMA
C
C#
C:
C
C====== WORD COUNT
C
        WC=TSLIB(NADD)
C
C====== HMA - HOST MEMORY ADDRESS
C
        HMA=TSLIB(NADD+1)+ADDR
C
C====== APMA - AP MEMORY ADDRESS
C
        APMA=TSLIB(NADD+2)
C
C====== CTRL - CONTROL REGISTER
C
        CTRL=TSLIB(NADD+3)
C
        NADD=NADD+4
        NINST=NINST-4
C
C====== DO DMA
C
        CALL RUNDMA(TSLIB(HMA),APMA,WC,CTRL)
C
        RETURN
        END
C****** PSDM = DMA PROGRAM SOURCE TO AP = REL 5.0  , NOV 79 *******************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : PSDM                                                     *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : DMA PROGRAM SOURCE MICRO-CODE TO AP THROUGH THE BOOTSTRAP*
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
        SUBROUTINE PSDM(FUNCT,NADD,NINST,TSLIB)
C
C
C------LOCAL STORAGE:
C
        INTEGER FUNCT,TSLIB(9999),PSA,HOST
C
C#
C:
C
C====== GET NUMBER OF PS WORDS TO TRANSFER
C
        NPS=IAND16(FUNCT,1023)
C
C====== SAVE SPD BECAUSE IT IS DISTURBED BELOW
C
        CALL APEXAM(ISPD,2,0)
C
C====== NOW THE NUMBER OF WORDS TO TRANSFER IS PUT IN SPAD 1
C       FOR THE BOOT STRAP IN THE AP
C
        CALL APDEP(NPS,6,1)
C
C====== RESTORE SPD
C
        CALL APDEP(ISPD,2,0)
C
C====== CALCULATE HOST WORDS
C
        NWORD=NPS*4
C
C====== GET AP ADDRESS
C
        PSA=TSLIB(NADD)
        NADD=NADD+1
        NINST=NINST-1
C
C====== HOST ADDRESS
C
        HOST=NADD
C
        NADD=NADD+NWORD
        NINST=NINST-NWORD
C
        CALL LOADPS(TSLIB(HOST),PSA,NPS)
C
        RETURN
        END
C****** WAITD = WAIT FOR DMA COMPLETE = REL 5.0  , NOV 79 *********************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : WAITD                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *    ENTRY       : DMACNT (HOW LONG TO WAIT FOR DMA)                        *
C  *                                                                           *
C  *    EXIT        : TIP (OPERATOR INTERVENTION FLAG)                         *
C  *                                                                           *
C  *    FUNCTION    : WAIT FOR THE DMA OPERATION UNTIL IT IS DONE OR THE       *
C  *                  OPERATOR INTERRUPTS.                                     *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
        SUBROUTINE WAITD(DMACNT,TIP)
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
        COMMON /TOTL/ ENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER ENTRY,IRADX,APNUM,ECHO,LUN,NZMD
C
C
C
C------LOCAL STORAGE:
C
        INTEGER DMACNT
        LOGICAL TIP
C
C#
C:
        NCOUNT=0
C
C====== CHECK FOR OPERATOR TYPE IN
C
10      CALL INTRV(TIP)
        IF(TIP) RETURN
C
C====== TEST FOR DMA DONE
C
        CALL TSTDMA(I)
        IF(I.EQ.1) RETURN
C
C====== TEST FOR DMA LOOP COUNT EXCEDED
C
        NCOUNT=NCOUNT+1
        IF(NCOUNT.LT.DMACNT) GOTO 10
C
C====== PRINT ERROR MESSAGE
C
        WRITE(ITTO,20)
          IF(ECHO.NE.0) WRITE(FUNIT,20)
20      FORMAT(20H DMA TIME OUT ERROR )
        TIP=.TRUE.
        RETURN
        END
C****** PUSH = PUSH VALUES ONTO STACK = REL 5.0  , NOV 79 *********************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : PUSH                                                     *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : PUSH A VALUE ONTO A STACK                                *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
C
        SUBROUTINE PUSH(IRAY,IPOINT,IREC)
        INTEGER IRAY(9999)
C
C#
C:
        IPOINT=IPOINT+1
        IRAY(IPOINT)=IREC
        RETURN
        END
C****** POP = POP VALUES FROM STACK = REL 5.0  , NOV 79 ***********************
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : POP                                                      *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : POP A VALUE FROM A STACK                                 *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
C
        SUBROUTINE POP(IRAY,IPOINT,IREC)
        INTEGER IRAY(9999)
C
C#
C:
        IF(IPOINT.LE.0) GOTO 10
        IREC=IRAY(IPOINT)
        IPOINT=IPOINT-1
10      RETURN
        END
C****** ISTIND = DO ISTAT INDIRECT GET FOR WRTOP ROUTINE = REL 5.0  , NOV 79 **
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : ISTIND                                                   *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : IF A TEST REQUIRES DATA FROM THE CONTROLLER THIS ROUTINE *
C  *                  FINDS WHERE THE DATA IS STORED AND PASSES THE VALUE OF TH*
C  *                  LOCATION  TO WRTOP WHICH WRITES IT TO THE AP.            *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        INTEGER FUNCTION ISTIND(IDATA)
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C
C------LOCAL STORAGE:
C
        INTEGER IPARN(13),IPARS(13),IRAYN(3),IRAYS(3)
C
        DATA IPARN/11,8,9,12,10,2,13,1,2,4,7,6,5/
        DATA IPARS/1,2,3,4,5,7,11,12,13,14,15,16,46/
C
        DATA IRAYN/12,25,24/
        DATA IRAYS/8,18,19 /
C
C#
C:
C
C====== LOOK FOR VALUE IN IPAR
C
        DO 10 I=1,13
          IF(IDATA.NE.IPARS(I)) GOTO 10
          J=IPARN(I)
          ISTIND=IPAR(J)
          RETURN
10      CONTINUE
C
C====== LOOK FOR VALUE IN IRAY
C
        DO 20 I=1,3
          IF(IDATA.NE.IRAYS(I)) GOTO 20
          J=IRAYN(I)
          ISTIND=IRAY(J)
          RETURN
20      CONTINUE
        WRITE(ITTO,30) IDATA
          IF(ECHO.NE.0) WRITE(FUNIT,30)IDATA
30      FORMAT(1X,I10,23H INVALID PARAMETER READ)
        RETURN
        END
C****** ISTDEP = PUT TEST TABLE DATA INTO CONTROLLER = REL 5.0  , NOV 79 ******
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : ISTDEP                                                   *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : PUTS DATA COMMING BACK FROM AP INTO CONTROLLER COMMON    *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE ISTDEP ( LIGHTS,IDATA)
C
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
        COMMON /TOTAL/ IPAR(46)
C
        INTEGER BREAK,ERLIM,GLOOP,PSLIM,BRKADD,SKIP ,DMACNT,
     +          TD,HALT,IORST,PLOOP,WAIT,BETWN,DELET,RESTO,TESTS
C
        EQUIVALENCE     (IPAR( 1),BREAK ),(IPAR( 2),ERLIM ),
     +                  (IPAR( 3),GLOOP ),(IPAR( 4),PSLIM ),
     +                  (IPAR( 5),BRKADD),(IPAR( 6),SKIP  ),
     +                  (IPAR( 7),DMACNT),(IPAR( 8),TD    )
        EQUIVALENCE
     +                  (IPAR( 9),HALT  ),(IPAR(10),IORST ),
     +                  (IPAR(11),PLOOP ),(IPAR(12),WAIT  ),
     +                  (IPAR(13),BETWN ),(IPAR(23),DELET )
        EQUIVALENCE
     +                  (IPAR(33),RESTO ),(IPAR(43),TESTS ),
     +                  (IPAR(44),NAMES ),(IPAR(45),INTFLG),
     +                  (IPAR(46),RESTRT)
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
C               TBLVAR IS A BLOCK OF VARIABLES FOR HOLDING
C               TEST TABLE INFORMATION
C
C
        COMMON /TBLVAR/ IRAY(45)
        INTEGER ITYPE,CASE,ADDR,TSTSZ,UTPKG,USECD,OPFLD(4),PATWD,
     +          RLOAD,ERFLG,NSTUP,NSTAD,WPNOP,RUNOP,RUNAD,NRDSU,NRDAD,
     +          RPNOP,MASK,ERFMT,GLPLC,PLPLC,ERRCK,EXPOP(4),ACTOP(4),
     +          ERRSZ,TXTAD,MPFB,CNTOP,CNTAD,PCPOP,PCPAD,NEROP,NERAD
        INTEGER
     +          MSB,NPATS
C
        EQUIVALENCE (IRAY(1),ITYPE)  ,(IRAY(2),CASE)  ,(IRAY(3),ADDR),
     +              (IRAY(4),TSTSZ) ,(IRAY(5),UTPKG) ,(IRAY(6),USECD),
     +              (IRAY(7),OPFLD) ,(IRAY(11),PATWD),(IRAY(12),RLOAD),
     +              (IRAY(13),ERFLG),(IRAY(14),NSTUP),(IRAY(15),NSTAD)
        EQUIVALENCE
     +              (IRAY(16),WPNOP),(IRAY(17),RUNOP),(IRAY(18),RUNAD),
     +              (IRAY(19),NRDSU),(IRAY(20),NRDAD),(IRAY(21),RPNOP),
     +              (IRAY(22),MASK) ,(IRAY(23),ERFMT),(IRAY(24),GLPLC)
        EQUIVALENCE
     +              (IRAY(25),PLPLC),(IRAY(26),ERRCK),(IRAY(27),EXPOP),
     +              (IRAY(31),ACTOP),(IRAY(35),ERRSZ),(IRAY(36),TXTAD),
     +              (IRAY(37),MPFB) ,(IRAY(38),CNTOP),(IRAY(39),CNTAD)
        EQUIVALENCE
     +              (IRAY(40),PCPOP),(IRAY(41),PCPAD),(IRAY(42),NEROP),
     +              (IRAY(43),NERAD),(IRAY(44),MSB)  ,(IRAY(45),NPATS)
C
C
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C
C
C               COM CONTAINS COMMON VARIABLES AND FLAGS
C               USED THROUGHOUT THE PROGRAM
C
C
        COMMON /COM/ FREE,TIP,ILITES,ISWR,PTPTR,EXPDT(4),ACTDT(4),
     +               ERROR,PASS,LUN1,LUN2,LUN3,CUTIL,PSAVE(4),PLPOP(4)
        INTEGER ILITES,ISWR,PTPTR,EXPDT,ACTDT,ERROR,PASS,
     +          CUTIL,PSAVE,PLPOP
        LOGICAL FREE,TIP
C
C
C==
C
C------LOCAL STORAGE:
C
        INTEGER IPARN(13),IPARS(13),IRAYN(3),IRAYS(3)
C
        DATA IPARN/11,8,9,12,10,2,13,1,2,4,7,6,5/
        DATA IPARS/1,2,3,4,5,7,11,12,13,14,15,16,46/
C
        DATA IRAYN/12,25,24/
        DATA IRAYS/8,18,19 /
C
C#
C:
C
C====== LOOK FOR VALUE IN IPAR
C
        DO 10 I=1,13
          IF(IDATA.NE.IPARS(I)) GOTO 10
          J=IPARN(I)
          IPAR(J)=LIGHTS
          RETURN
10      CONTINUE
C
C====== LOOK FOR VALUE IN IRAY
C
        DO 20 I=1,3
          IF(IDATA.NE.IRAYS(I)) GOTO 20
          J=IRAYN(I)
          IRAY(J)=LIGHTS
          RETURN
20      CONTINUE
        WRITE(ITTO,30) IDATA
          IF(ECHO.NE.0) WRITE(FUNIT,30)IDATA
30      FORMAT(1X,I10,26H INVALID PARAMETER DEPOSIT)
        RETURN
        END
C****** UNPK = UNPACK A2 TO A1 = REL 5.0  , NOV 79 ****************************
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : UNPK                                                     *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *    ENTRY       : JCARD - INPUT ARRAY (A2)                                 *
C  *                  I     - START ARRAY POSITION TO UNPACK FROM              *
C  *                  J     - END ARRAY POSITION TO UNPACK TO                  *
C  *                                                                           *
C  *    EXIT        : KCARD - OUTPUT ARRAY (A1)                                *
C  *                  K     - START POSITION TO UNPACK INTO                    *
C  *                                                                           *
C  *    FUNCTION    : UNPACK A2 TO A1                                          *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE UNPK(JCARD,I,J,KCARD,K)
        INTEGER JCARD(9999),KCARD(9999),BLANK
C
C====== GET BLANK MASK CHARACTER FOR HOST
C
        CALL IBLNK(BLANK)
        IF(IAND16(BLANK,-256).EQ.0) IFLAG=0
        IF(IAND16(BLANK,255).EQ.0) IFLAG=1
C
C====== UNPACK ARRAY
C
        KK=K-1
        DO 20 L=I,J
C
           IF(IFLAG.NE.0) GOTO 10
           KK=KK+1
           KCARD(KK)=IOR16(ILSH16(JCARD(L),8),BLANK)
           KK=KK+1
           KCARD(KK)=IOR16(IAND16(JCARD(L),-256),BLANK)
           GOTO 20
C
10         KK=KK+1
           KCARD(KK)=IOR16(IAND16(JCARD(L),255),BLANK)
           KK=KK+1
           KCARD(KK)=IOR16(IRSH16(JCARD(L),8),BLANK)
C
20      CONTINUE
        RETURN
        END
C****** TNUM = FIND TEST NUMBER IN TABLE  OF CONTENTS       = REL 5.0  , NOV 79
C:
C  *****************************************************************************
C  *                                                                           *
C  *    NAME        : TNUM                                                     *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
C  *                                                                           *
C  *    ENTRY       : TYPE ,CASE                                               *
C  *                                                                           *
C  *    EXIT        : TEST NUMBER                                              *
C  *                                                                           *
C  *    FUNCTION    : RETURN TEST NUMBER                                       *
C  *                                                                           *
C  *                                                                           *
C  *****************************************************************************
C
C
        SUBROUTINE TNUM(ITYPE,CASE,NUMBER)
C==
C-------COMMON STORAGE:
C
C               LIB IS A BLOCK WHICH CONTAINS THE TEST TABLES
C               THEMSELVS PLUS THE TABLE OF CONTENTS ARRAY
C               AND LENGTHS
C
C
        COMMON /LIB/ TSLIB(1700),INLIB(25,3),TOC(150,4),LIBEND,TSTEND,
     +               LIBSIZ,TOCEND,ENBUF,UTLEND,UTLIB(10,3),UTLBEN
        INTEGER TSLIB,INLIB,LIBEND,TSTEND,LIBSIZ,TOC,TOCEND,
     +          UTLEND,ENBUF,UTLIB,UTLBEN
C
C==
C
C------LOCAL VARIABLES
C
       INTEGER NUMBER,ITYPE,CASE
C
C#
C:
C
C====== SCAN TABLE OF CONTENTS
C
        DO 10 NUMBER=1,TOCEND
C
C====== FIND CASE
C
        IF(TOC(NUMBER,2).NE.CASE) GOTO 10
C
C====== CHECK FOR TYPE
C
        IF(TOC(NUMBER,1).NE.ITYPE) GOTO 10
        RETURN
10      CONTINUE
        NUMBER=0
        RETURN
        END
C****** APSTOP = DUMMY APEX APSTOP = REL 5.0  , NOV 79 ************************
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : APSTOP                                                   *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
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
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11 -- ECLIPSE - RDOS                           *
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
        INTEGER LINE(9999),NUMB(16)
C
        DATA NUMB /1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,
     +            1H9,1HA,1HB,1HC,1HD,1HE,1HF/
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
C%.R 6326 NOVA
         IF(LINE(II).EQ.NUMB(J)) GOTO 20
C%.E NOVA
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
C%.R 6338 NOVA
C****** IBLNK = DETERMINE HOST A1 TYPE = REL 5.0  , NOV 79 ********************
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : IBLNK                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *    HOST SYSTEM : PDP11 - RT11                                             *
C  *                                                                           *
C  *                                                                           *
C  *    EXIT        : BLANK - HOST A1 CHARACTER WITH A ZERO IN THE CHARACTER   *
C  *                          POSITION.                                        *
C  *                                                                           *
C  *    FUNCTION    : DETERMINES THE HOST JUSTIFICATION OF A1 CHARACTERS       *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        SUBROUTINE IBLNK(BLANK)
C
        INTEGER BLANK,KEY
        DATA KEY/1H /
C
C====== SET BLANK TO INDICATE HOST THAT RIGHT JUSTIFIES
C       A1 CHARACTERS IN WORD
C
        BLANK=IAND16(KEY,-256)
C
        RETURN
        END
C%.E NOVA
C****** IMCMD = IMMEDIATE COMMAND EXECUTION FOR CSI = REL 5.0  , NOV 79 *******
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : IMCMD                                                    *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 30 MAY 79                                                *
C  *                                                                           *
C  *    ENTRY       : INDEX  SUBROUTINE NUMBER TO EXECUTE                      *
C  *                  INBUF  INPUT LINE FROM INPUT ROUTINE                     *
C  *                  IPOS   CHARACTER POINTER INTO INBUF FOR IRDTK SUBROUTINE *
C  *                  IRADX  I/O RADIX                                         *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : A PROGRAM INCLUDED IN DIAGNOSTIC THAT CAN                *
C  *                  BE CALLED BY CSI TO PREFORM IMMEDIATE COMMANDS           *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
       SUBROUTINE IMCMD(INDEX,INBUF,IPOS,IRADX)
       RETURN
       END
