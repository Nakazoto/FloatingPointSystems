C****** FFT100                                             == REL 5.0  , NOV 79
C****** FFT100 = FFT TEST MAINLINE                          = REL 5.0  , NOV 79
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : FFT100>MAINLINE                                          *
C  *    DATE        : NOV  1, 1979                                             *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : N/A                                                      *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : FFT100 RUNS AP MICRO-CODE WHICH PERFORMS FORWARD         *
C  *                  AND INVERSE FAST FOURIER TRANSFORMS.  THE MAINLINE       *
C  *                  PORTION OF FFT100 CALLS SCFIFT (DGNHSR SUBROUTINE)       *
C  *                  WHICH LOADS THE AP MICRO-CODE INTO THE ARRAY             *
C  *                  PROCESSOR.  THE MAINLINE ALSO DETERMINES THE SIZE        *
C  *                  OF THE FFT TO BE DONE.  IF THE USER SPECIFIES A          *
C  *                  CERTAIN SIZE, IT WILL BE CHECKED TO DETERMINE IF         *
C  *                  IT IS VALID.  IF IT IS NOT, IT WILL BE SET TO A          *
C  *                  DEFAULT SIZE.  IF THE USER DOES NOT INITIALLY            *
C  *                  SET AN FFT SIZE, THE PROGRAM WILL USE THE MAIN           *
C  *                  DATA SIZING RESULTS TO SET THE FFT SIZE.                 *
C  *                                                                           *
C  *    GENERALITIES: FFT100 IS BASICALLY A VERIFIER TEST.  FFT100 SHOULD      *
C  *                  BE RUN AFTER APTEST (TESTS INTERFACE REGISTERS AND       *
C  *                  MEMORIES), APPATH (TESTS DATA PATHS), AND APARTH         *
C  *                  (TESTS AP ARITHMETIC HARDWARE) HAVE BEEN RUN             *
C  *                  SUCCESSFULLY.                                            *
C  *                                                                           *
C: *****************************************************************************
C%.I    33      MITRA
        INTEGER ITTI,ITTO,LFIL,ISTAT,IRTURN,ICHNGE,WATERR,
     +  LOOPER,TYPDIS,IORST,HALT,FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,
     +  IERLIM,IPSLIM,ERRCNT,PASCNT,IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ,
     +  PSIZE,BASLOW,MDSIZ,IAPNUM,IECHO,IRADX,IERRS,IPSIZE,NPAGES
        INTEGER
     +  BASE,SIZFFT,JPAGE,I,K,LPCNT,INO1(6),INO2(6),ERRCHK
        INTEGER STATBF,STATNM,STRAD
        REAL RMDSIZ,FMIN,FMAX,RMDHI,RMDLO,RBASE,RFSIZE
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /FIFT/   BASE,SIZFFT,JPAGE
        COMMON /STCNTL/ STATBF(60),STATNM(21),STRAD(20)
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),IDUM  )
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),FFTERR) , (ISTAT( 7),FFTBAS) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IPSLIM) , (ISTAT(10),ERRCNT) , (ISTAT(11),PASCNT)
        EQUIVALENCE
     +  (ISTAT(12), IPAGE) , (ISTAT(13),   IXN) , (ISTAT(14),   IYN),
     +  (ISTAT(15),FFTSIZ) , (ISTAT(16),BASLOW) , (ISTAT(17), IERRS),
     +  (ISTAT(18), MDSIZ) , (ISTAT(19),IERLIM) , (ISTAT(20), ISTEP)
C.
C..
C...
C
        FMIN=8.0
C
C====== INITIALIZE CONSOLE DEVICE LOGICAL UNIT NUMBERS
C
        IF(IOCNTL(1,0,0,0).NE.0) CALL EXIT
C
C====== WRITE HEADER
C
        WRITE(ITTO,2000)
C
C====== PROCESS COMMANDS
C
        WRITE(ITTO,2010)
C
        NAP=-1
        CALL SETUP(1)
20      CALL SETUP(0)
        IF(IAPNUM.EQ.NAP) GOTO 25
        CALL MEMSIZ
        NAP=IAPNUM
25      CONTINUE
        IF(IPSLIM.NE.0.AND.IPSLIM.LT.PSSFLG) PSSFLG=IPSLIM
C
C====== PRINT ERROR FILE HEADER IF SPECIFIED
C
        IF(IECHO.NE.0) WRITE(LFIL,2020)
C
C====== INITIALIZE AP
C
        CALL APRSET
        CALL APXSET
C
        JPAGE=NPAGES-1
C
C====== LOAD MICROCODE INTO AP
C
        CALL SCFIFT(BASE,4,IXN,IYN)
        CALL APWR
C
C>
C====== RESET COUNTERS
C
        PASCNT=0
        ERRCNT=0
        LPCNT=0
C
        GO TO 40
C
30      LPCNT=0
        CALL I2ASCI(6,ERRCNT,INO1,10,1)
        CALL I2ASCI(6,PASCNT,INO2,10,1)
        WRITE(ITTO,2030) INO1,INO2
        IF(IECHO.NE.0) WRITE(LFIL,2030) INO1,INO2
C
C====== THIS IS THE RE-ENTRY POINT FOR FFT100
C
40      IF(PSSFLG.EQ.0) PSSFLG=1
        IF(LPCNT.EQ.PSSFLG) GO TO 30
        IF(IPSLIM.EQ.0) GO TO 50
        IF(IPSLIM.EQ.PASCNT) GO TO 20
C
50      LPCNT=LPCNT+1
        PASCNT=PASCNT+1
C
C====== IF NOT FIXED PAGE INCREMENT PAGE NUMBER
C
        JPAGE=JPAGE+1
        IF(JPAGE.GE.NPAGES) JPAGE=0
        IF(IPAGE.LT.16) JPAGE=IPAGE
C
C====== GET USER MDHI OR HIGHEST ADDRESS AVAILABLE ON THIS PAGE
C
        RMDHI=PFLOAT(MDSIZ)
        IF(MDSIZ.EQ.0) RMDHI=RMDSIZ(JPAGE+1)-1.0
        IF(RMDHI.LT.FMIN) RMDHI=FMIN
C
C====== COMPUTE MAXIMUM ALLOWABLE FFT SIZE
C
        FMAX=RMDHI+1.0
        IF(FMAX.GT.16384.0) FMAX=16384.0
C
C====== COMPUTE RANDOM BASE OR FIX AT USER DEFINED VALUE
C
        RMDLO=PFLOAT(BASLOW)
        IF(RMDLO+FMIN.GT.RMDHI) RMDLO=RMDHI-FMIN
        RBASE=AMOD(PFLOAT(IXN),RMDHI+1.0)
        IF(RBASE.GT.(RMDHI-FMIN)) RBASE=RMDHI-FMIN
        IF(RBASE.LT.RMDLO.OR.FFTBAS.NE.0) RBASE=RMDLO
C
C====== GENERATE NEW RANDOM NUMBERS AND CHECK FOR USER DEFINED FFT SIZE
C
        IXN=IGRN16(IXN,IYN)
        IF(FFTSIZ.EQ.0) GO TO 90
C
C====== MUST BE USER DEFINED FFT SIZE
C
        RFSIZE=PFLOAT(FFTSIZ)*2.0
70      IF((RFSIZE+RBASE).LE.RMDHI) GO TO 80
C
C====== USER PARAMETERS ARE INCORRECT - TRY TO ADJUST BASE
C
        RBASE=RBASE*0.5
        IF(RBASE.GT.1.0) GO TO 70
C
C====== SITUATION HOPELESS - SET FOR MAX SIZES AND RUN ANYWAY
C
        RBASE=0.0
        RFSIZE=RMDHI+1.0
C
80      IF(RFSIZE.GT.FMAX) RFSIZE=FMAX
        GO TO 110
C
C====== COMPUTE FFT SIZE QUASI-RANDOMLY
C
90      K=IRSH16(IAND16(IXN,1920),7)
        RFSIZE=FMIN
        A=(RMDHI+1.0)-RBASE
C
        DO 100 I=1,K
        RFSIZE=RFSIZE*2.0
        IF(RFSIZE.LT.A) GO TO 100
        RFSIZE = RFSIZE * 0.5
        GO TO 110
100       CONTINUE
C
C====== SET COMPUTED FFT SIZE TO TRUE FFT SIZE
C
110     IF(RFSIZE.GT.FMAX) RFSIZE=FMAX
        RFSIZE=RFSIZE*0.5
C
C====== SIZE AND BASE ARE ACCEPTABLE - SET PAGE AND RUN MICROCODE
C
        SIZFFT=IPFIX(RFSIZE)
        BASE=IPFIX(RBASE)
        CALL OMAE(JPAGE)
C
120     CALL LDGO(IXN,IYN)
        IERRS=0
        CALL APWTRN
C
C====== CHECK FOR ERRORS
C
        I=ERRCHK(I)
C
C====== IF SCOPE LOOP REQUESTED RE-RUN THIS PASS
C
        IF(IRTURN.NE.0) GO TO 120
C
        GO TO 40
C
C====== FORMAT STATEMENTS
C
2000    FORMAT(36H  ***   FFT100                   ***//)
2010    FORMAT(20H PLEASE ASSIGN AN AP)
2020    FORMAT(36H1 ***   FFT100                   ***//,
     +        26H        ERROR LOGGING FILE ,////)
2030    FORMAT(17H FFT100 - ERRORS=,6A1,9H, PASSES=,6A1)
C
        END
C
C
C****** IMCMD4 = IMMEDIATE EXECUTION COMMANDS               = REL 5.0  , NOV 79
        SUBROUTINE IMCMD(INX,INBUF,IPOS,IRADX)
        GO TO (10,20) ,INX
C
10      CALL MEMSIZ
        RETURN
C
20      CALL MEMDMP(INBUF,IPOS,IRADX)
        RETURN
C
        END
C****** SETUP4 = SETS UP DATA FOR CSI                       = REL 5.0  , NOV 79
C
        SUBROUTINE SETUP(INIT)
C
        INTEGER XTB(19,12),INIT,XTLEN
        COMMON /ERORR/  IERROR
        COMMON /TOTAL/  ISTAT(40)
C
        DATA XTB( 1, 1),XTB( 1, 2),XTB( 1, 3)   /1HL,1HO,1HM/
        DATA XTB( 1, 4),XTB( 1, 5),XTB( 1, 6)   /1HE,1HM,1H /
        DATA XTB( 1, 7),XTB( 1, 8),XTB( 1, 9)   /     2,    16,     0/
        DATA XTB( 1,10),XTB( 1,11),XTB( 1,12)   /     0,     0,     0/
C
        DATA XTB( 2, 1),XTB( 2, 2),XTB( 2, 3)   /1HB,1HE,1HT/
        DATA XTB( 2, 4),XTB( 2, 5),XTB( 2, 6)   /1HW,1HN,1H /
        DATA XTB( 2, 7),XTB( 2, 8),XTB( 2, 9)   /     2,     8,    64/
        DATA XTB( 2,10),XTB( 2,11),XTB( 2,12)   /     0,     0,    10/
C
        DATA XTB( 3, 1),XTB( 3, 2),XTB( 3, 3)   /1HP,1HA,1HS/
        DATA XTB( 3, 4),XTB( 3, 5),XTB( 3, 6)   /1HL,1HI,1HM/
        DATA XTB( 3, 7),XTB( 3, 8),XTB( 3, 9)   /     2,     9,    64/
        DATA XTB( 3,10),XTB( 3,11),XTB( 3,12)   /     0,     0,    10/
C
        DATA XTB( 4, 1),XTB( 4, 2),XTB( 4, 3)   /1HP,1HA,1HG/
        DATA XTB( 4, 4),XTB( 4, 5),XTB( 4, 6)   /1HE,1H ,1H /
        DATA XTB( 4, 7),XTB( 4, 8),XTB( 4, 9)   /     2,    12,    20/
        DATA XTB( 4,10),XTB( 4,11),XTB( 4,12)   /     0,     0,    10/
C
        DATA XTB( 5, 1),XTB( 5, 2),XTB( 5, 3)   /1HX,1H ,1H /
        DATA XTB( 5, 4),XTB( 5, 5),XTB( 5, 6)   /1H ,1H ,1H /
        DATA XTB( 5, 7),XTB( 5, 8),XTB( 5, 9)   /     2,    13,     0/
        DATA XTB( 5,10),XTB( 5,11),XTB( 5,12)   /     0,     0,     0/
C
        DATA XTB( 6, 1),XTB( 6, 2),XTB( 6, 3)   /1HY,1H ,1H /
        DATA XTB( 6, 4),XTB( 6, 5),XTB( 6, 6)   /1H ,1H ,1H /
        DATA XTB( 6, 7),XTB( 6, 8),XTB( 6, 9)   /     2,    14,     0/
        DATA XTB( 6,10),XTB( 6,11),XTB( 6,12)   /     0,     0,     0/
C
        DATA XTB( 7, 1),XTB( 7, 2),XTB( 7, 3)   /1HF,1HF,1HT/
        DATA XTB( 7, 4),XTB( 7, 5),XTB( 7, 6)   /1HS,1HZ,1H /
        DATA XTB( 7, 7),XTB( 7, 8),XTB( 7, 9)   /     2,    15,     0/
        DATA XTB( 7,10),XTB( 7,11),XTB( 7,12)   /     0,     0,     0/
C
        DATA XTB( 8, 1),XTB( 8, 2),XTB( 8, 3)   /1HW,1H ,1H /
        DATA XTB( 8, 4),XTB( 8, 5),XTB( 8, 6)   /1H ,1H ,1H /
        DATA XTB( 8, 7),XTB( 8, 8),XTB( 8, 9)   /     3,     1,     0/
        DATA XTB( 8,10),XTB( 8,11),XTB( 8,12)   /     0,     0,     0/
C
        DATA XTB( 9, 1),XTB( 9, 2),XTB( 9, 3)   /1HL,1H ,1H /
        DATA XTB( 9, 4),XTB( 9, 5),XTB( 9, 6)   /1H ,1H ,1H /
        DATA XTB( 9, 7),XTB( 9, 8),XTB( 9, 9)   /     3,     2,     0/
        DATA XTB( 9,10),XTB( 9,11),XTB( 9,12)   /     0,     0,     0/
C
        DATA XTB(10, 1),XTB(10, 2),XTB(10, 3)   /1HD,1H ,1H /
        DATA XTB(10, 4),XTB(10, 5),XTB(10, 6)   /1H ,1H ,1H /
        DATA XTB(10, 7),XTB(10, 8),XTB(10, 9)   /     3,     3,     0/
        DATA XTB(10,10),XTB(10,11),XTB(10,12)   /     0,     0,     0/
C
        DATA XTB(11, 1),XTB(11, 2),XTB(11, 3)   /1HI,1H ,1H /
        DATA XTB(11, 4),XTB(11, 5),XTB(11, 6)   /1H ,1H ,1H /
        DATA XTB(11, 7),XTB(11, 8),XTB(11, 9)   /     3,     4,     0/
        DATA XTB(11,10),XTB(11,11),XTB(11,12)   /     0,     0,     0/
C
        DATA XTB(12, 1),XTB(12, 2),XTB(12, 3)   /1HH,1H ,1H /
        DATA XTB(12, 4),XTB(12, 5),XTB(12, 6)   /1H ,1H ,1H /
        DATA XTB(12, 7),XTB(12, 8),XTB(12, 9)   /     3,     5,     0/
        DATA XTB(12,10),XTB(12,11),XTB(12,12)   /     0,     0,     0/
C
        DATA XTB(13, 1),XTB(13, 2),XTB(13, 3)   /1HS,1H ,1H /
        DATA XTB(13, 4),XTB(13, 5),XTB(13, 6)   /1H ,1H ,1H /
        DATA XTB(13, 7),XTB(13, 8),XTB(13, 9)   /     3,     6,     0/
        DATA XTB(13,10),XTB(13,11),XTB(13,12)   /     0,     0,     0/
C
        DATA XTB(14, 1),XTB(14, 2),XTB(14, 3)   /1HF,1HI,1HX/
        DATA XTB(14, 4),XTB(14, 5),XTB(14, 6)   /1HB,1H ,1H /
        DATA XTB(14, 7),XTB(14, 8),XTB(14, 9)   /     3,     7,     0/
        DATA XTB(14,10),XTB(14,11),XTB(14,12)   /     0,     0,     0/
C
        DATA XTB(15, 1),XTB(15, 2),XTB(15, 3)   /1HM,1HE,1HM/
        DATA XTB(15, 4),XTB(15, 5),XTB(15, 6)   /1HS,1HI,1HZ/
        DATA XTB(15, 7),XTB(15, 8),XTB(15, 9)   /     5,     0,     1/
        DATA XTB(15,10),XTB(15,11),XTB(15,12)   /     0,     0,     0/
C
        DATA XTB(16, 1),XTB(16, 2),XTB(16, 3)   /1HD,1HU,1HM/
        DATA XTB(16, 4),XTB(16, 5),XTB(16, 6)   /1HP,1H ,1H /
        DATA XTB(16, 7),XTB(16, 8),XTB(16, 9)   /     5,     0,     2/
        DATA XTB(16,10),XTB(16,11),XTB(16,12)   /     0,     0,     0/
C
        DATA XTB(17, 1),XTB(17, 2),XTB(17, 3)   /1HH,1HI,1HM/
        DATA XTB(17, 4),XTB(17, 5),XTB(17, 6)   /1HE,1HM,1H /
        DATA XTB(17, 7),XTB(17, 8),XTB(17, 9)   /     2,    18,     0/
        DATA XTB(17,10),XTB(17,11),XTB(17,12)   /     0,     0,     0/
C
        DATA XTB(18, 1),XTB(18, 2),XTB(18, 3)   /1HE,1HR,1HR/
        DATA XTB(18, 4),XTB(18, 5),XTB(18, 6)   /1HL,1HI,1HM/
        DATA XTB(18, 7),XTB(18, 8),XTB(18, 9)   /     2,    19,    10/
        DATA XTB(18,10),XTB(18,11),XTB(18,12)   /     0,     0,    10/
C
        DATA XTB(19, 1),XTB(19, 2),XTB(19, 3)   /1HS,1HT,1HE/
        DATA XTB(19, 4),XTB(19, 5),XTB(19, 6)   /1HP,1H ,1H /
        DATA XTB(19, 7),XTB(19, 8),XTB(19, 9)   /     3,    20,     1/
        DATA XTB(19,10),XTB(19,11),XTB(19,12)   /     0,     0,     0/
C
C.
C..
C...
        IF(INIT.NE.0) GO TO 20
C
        IERROR=0
        CALL CSI(XTB,19,ISTAT,40,0)
C
C====== STEP MODE?
C
        CALL APMODE(0)
        IF(ISTAT(38).EQ.1) CALL APMODE(1)
C
        GO TO 30
C
20      CALL CSI(XTB,19,ISTAT,40,1)
C
30      CONTINUE
        RETURN
        END
C****** ASST4 = LISTS AVAILABLE COMMANDS FOR THIS TEST      = REL 5.0  , NOV 79
C
        SUBROUTINE ASSIST(ITTO,LFIL,IECHO)
C
C.
C..
C...
C
        WRITE(ITTO,10)
        IF(IECHO.NE.0) WRITE(LFIL,10)
C
10      FORMAT (
     +  41H APNUM=N  - CLOSE CURRENT AP, OPEN AP (N)                   /,
     +  44H CLRFLG   - SETS ALL FLAGS TO DEFAULT VALUES                /,
     +  44H CLRALL   - RESETS PROGRAM TO DEFAULT VALUES                /,
     +  42H ECHO     - PROMPTS USER FOR ECHO FILENAME                  /,
     +  )
C
        WRITE(ITTO,12)
        IF(IECHO.NE.0) WRITE(LFIL,12)
C
12      FORMAT (
     +  28H -ECHO    - CLOSES ECHO FILE                                /,
     +  27H RUN      - EXECUTE PROGRAM                                 /,
     +  36H HELP     - PRINT AVAILABLE COMMANDS                        /,
     +  38H RADIX=N  - SET I-O RADIX VALUE TO (N)                      /,
     +  )
C
        WRITE(ITTO,14)
        IF(IECHO.NE.0) WRITE(LFIL,14)
C
14      FORMAT (
     +  40H STAT     - PRINT CURRENT PROGRAM STATUS                    /,
     +  43H STOP     - <QUIT> EXIT TO OPERATING SYSTEM                 /,
     +  40H BETWN=N  - SET PASSES BETWEEN PRINTOUTS                    /,
     +  49H PASLIM=N - PASSES TO RUN BEFORE RETURNING TO CSI           /,
     +  )
C
        WRITE(ITTO,16)
        IF(IECHO.NE.0) WRITE(LFIL,16)
C
16      FORMAT (
     +  48H PAGE=N   - NUMBER OF PAGES OF MD MEMORY TO TEST            /,
     +  43H X=N      - SET "X" RANDOM NUMBER PARAMETER                 /,
     +  43H Y=N      - SET "Y" RANDOM NUMBER PARAMETER                 /,
     +  34H D        - DISABLE ERROR PRINTOUT                          /,
     +  )
C
        WRITE(ITTO,18)
        IF(IECHO.NE.0) WRITE(LFIL,18)
C
18      FORMAT (
     +  37H H        - HALT AFTER EACH TEST CASE                       /,
     +  38H I        - RESET AFTER EACH TEST CASE                      /,
     +  25H L        - LOOP ON ERROR                                   /,
     +  )
C
        WRITE(ITTO,20)
        IF(IECHO.NE.0) WRITE(LFIL,20)
C
20      FORMAT (
     +  34H W        - RETURN TO CSI ON ERROR                          /,
     +  37H FIXB     - SET BASE ADDRESS TO LOMEM                       /,
     +  24H FFTSZ=N  - SET FFT SIZE                                    /,
     +  43H /*       - /* CAN BE FOLLOWED BY A COMMENT                 /,
     +  )
C
        WRITE(ITTO,22)
        IF(IECHO.NE.0) WRITE(LFIL,22)
C
22      FORMAT(
     +  34H LOMEM=N  - SET THE LOW MD ADDRESS                          /,
     +  35H HIMEM=N  - SET THE HIGH MD ADDRESS                        /,
     +  32H MEMSIZ   - SETS PS AND MD SIZES                            /,
     +  39H DUMP XX  - DUMPS SPECIFIED MEMORY (XX)                     /,
     +  )
C
        WRITE(ITTO,24)
        IF(IECHO.NE.0) WRITE(LFIL,24)
C
24      FORMAT(
     +  44H STEP     - EXECUTES IN STEP MODE (DEFAULT)                 /,
     +  37H -STEP    - EXECUTES IN CHAINED MODE                        /,
     +  )
        RETURN
        END
C****** LPTST4 = LOOP TEST FOR USER INPUT                   = REL 5.0  , NOV 79
        SUBROUTINE LPTST(IRTURN)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : DGCOM>LPTST                                              *
C  *    DATE        : NOV  1, 1979                                             *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : N/A                                                      *
C  *                                                                           *
C  *    EXIT        : IRTURN <= 1  -  LOOPING IS REQUIRED                      *
C  *                            0  -  LOOPING NOT REQUIRED                     *
C  *                                                                           *
C  *    FUNCTION    : LPTST TESTS THE FOLLOWING FLAGS AND YIELDS THE           *
C  *                  FOLLOWING RESULTS:                                       *
C  *                  FLAG        RESULT                                       *
C  *                  ======      =========================================    *
C  *                  LOOPER      0 - SET IRTURN TO 0                          *
C  *                              1 - SET IRTURN TO 1                          *
C  *                  HALT        0 - DO NOTHING                               *
C  *                              1 - SET 'HALT' TO 0 AND GO TO CSI (DGCOM     *
C  *                                  SUBROUTINE)                              *
C  *                  IORST       0 - DO NOTHING                               *
C  *                              1 - RESET THE ARRAY PROCESSOR                *
C  *                  WATERR      0 - DO NOTHING                               *
C  *                              1 - IF AN ERROR HAS OCCURRED, GO TO CSI      *
C  *                                  (DGCOM SUBROUTINE)                       *
C  *                  -                                                        *
C  *                  LPTST CHECKS TO SEE IF THE NUMBER OF ERRORS DETECTED     *
C  *                  HAS REACHED THE ERROR LIMIT.  IF IT HAS, THE PROGRAM     *
C  *                  RETURNS TO THE COMMAND STRING INTEPRETER (CSI) FOR       *
C  *                  ANOTHER USER COMMAND.  LASTLY, LPTST CHECKS TO SEE       *
C  *                  IF THE MOST SIGNIFICANT BIT IN THE SWITCH REGISTER       *
C  *                  IS SET.  IF IT IS, THE ROUTINE PRINTS OUT A MESSAGE      *
C  *                  INDICATING THAT IT IS SET AND THEN RETURNS TO            *
C  *                  CSI (DGCOM SUBROUTINE).                                  *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER ITTI,ITTO,LFIL,ISTAT,IDUMMY,ICHNGE,WATERR,
     +  LOOPER,TYPDIS,IORST,HALT,FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,
     +  IERLIM,IPSLIM,ERRCNT,PASCNT,IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ,
     +  PSIZE,BASLOW,MDSIZ,IAPNUM,IECHO,IERRS,IMSIZ,IPSIZE,NPAGES
        INTEGER
     +  IERROR,IRTURN,IN
C
        REAL RMDSIZ
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /ERORR/  IERROR
C
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IDUMMY,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),IDUM  )
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),FFTERR) , (ISTAT( 7),FFTBAS) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IPSLIM) , (ISTAT(10),ERRCNT) , (ISTAT(11),PASCNT)
        EQUIVALENCE
     +  (ISTAT(12), IPAGE) , (ISTAT(13),   IXN) , (ISTAT(14),   IYN),
     +  (ISTAT(15),FFTSIZ) , (ISTAT(16),BASLOW) , (ISTAT(17), IERRS),
     +  (ISTAT(18), MDSIZ) , (ISTAT(19),IERLIM) , (ISTAT(20), ISTEP)
C.
C..
C...
C
        PSIZE=ISTAT(36)
        ICHNGE=0
C
C====== IS HALT FLAG SET ?
C
        IF(HALT.EQ.0) GO TO 70
        HALT=0
C
C====== CALL COMMAND STRING INTERPRETER
C
10      CALL SETUP(0)
        PSIZE=ISTAT(36)
C
C====== IS RESET FLAG ( I ) SET ?
C
20      IF(IORST.EQ.0) GO TO 30
        CALL APRSET
C
C====== MSB IN SWITCH REGISTER SET ?
C
30      CALL SREAD(IN)
        IN=IRSH16(IN,15)
        IF(IN) 40,50,40
C
C====== REPORT MSB BIT ON
C
40      WRITE(ITTO,2000)
        IF(IECHO.NE.0) WRITE(LFIL,2000)
        GO TO 10
C
C====== IS THERE A CHARACTER AT TERMINAL ?
C
50      CALL TERM(IN)
        IF(IN.EQ.1) GO TO 10
C
C====== IS LOOP FLAG ON ?
C
        IF(LOOPER.EQ.0) GO TO 60
        IRTURN=1
        RETURN
C
C====== LOOP FLAG IS NOT ON
C
60      IRTURN=0
        RETURN
C
C====== HAVE REACHED ERROR LIMIT ?
C
70      IF(IERRS.EQ.0) GO TO 20
        IERROR=IERROR+1
        IF(IERLIM.EQ.IERROR) GO TO 10
C
C====== IS WAIT ON ERROR FLAG SET ?
C
        IF(WATERR.EQ.0) GO TO 20
        GO TO 10
C
2000    FORMAT(7H MSB ON)
        END
C
C
C****** ERRCHK = FFT100 ERROR CHECK                         = REL 5.0  , NOV 79
        INTEGER FUNCTION ERRCHK(ERROR)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : FFT100>ERRCHK                                            *
C  *    DATE        : NOV  1, 1979                                             *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : N/A                                                      *
C  *                                                                           *
C  *    EXIT        : ERROR <= FLAG WHICH IS SET TO A '1' IF AN ERROR          *
C  *                           IS DETECTED.                                    *
C  *                                                                           *
C  *    FUNCTION    : ERRCHK CHECKS S-PAD REGISTER 15 FOR AN ERROR.  IF AN     *
C  *                  ERROR HAS OCCURRED, THE AP MICRO-CODE PROGRAM WILL       *
C  *                  SET A FLAG IN S-PAD REGISTER 15.  IF ERRCHK DETECTS      *
C  *                  AN ERROR, THE EXPECTED, ACTUAL, BASE, PAGE, LOCATION,    *
C  *                  SIZE, AND RANDOM NUMBER SEEDS ARE RETRIEVED FROM DATA    *
C  *                  PADS AND PRINTED OUT IN THE ERROR MESSAGE.               *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER ITTI,ITTO,LFIL,ISTAT,IRTURN,ICHNGE,WATERR,
     +  LOOPER,TYPDIS,IORST,HALT,FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,
     +  IERLIM,IPSLIM,ERRCNT,PASCNT,IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ,
     +  PSIZE,BASLOW,MDSIZ,IAPNUM,IECHO,IRADX,IERRS,IPSIZE,NPAGES
        INTEGER
     +  BASE,SIZFFT,JPAGE,I,OUT1(6),OUT2(6),OUT3(6),OUT4(6),OUT5(6),
     +  ERROR,OUT6(6),OUT7(6),OUT8(6),OUT9(6)
C
        REAL RMDSIZ
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /FIFT/   BASE,SIZFFT,JPAGE
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),IDUM  )
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),FFTERR) , (ISTAT( 7),FFTBAS) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IPSLIM) , (ISTAT(10),ERRCNT) , (ISTAT(11),PASCNT)
        EQUIVALENCE
     +  (ISTAT(12), IPAGE) , (ISTAT(13),   IXN) , (ISTAT(14),   IYN),
     +  (ISTAT(15),FFTSIZ) , (ISTAT(16),BASLOW) , (ISTAT(17), IERRS),
     +  (ISTAT(18), MDSIZ) , (ISTAT(19),IERLIM) , (ISTAT(20), ISTEP)
C.
C..
C...
C
10      IERRS=0
        IF (TYPDIS .EQ.0) GO TO 20
C
C====== TYPEOUT DISABLED, RETURN QUICKLY
C
        CALL LPTST(IRTURN)
        RETURN
C
C====== CHECK IF MICROCODE FOUND ERROR
C
20      CALL APCHK(I)
C>
        ERRCHK=0
        ERROR=0
        IF (I.NE.0) GO TO 40
C
C====== NO ERROR FOUND, RETURN WITH GOOD NEWS
C
30      CALL LPTST(IRTURN)
        RETURN
C
C====== HERE IF ERROR
C
40      IERRS=1
        ERRCNT=ERRCNT+1
        ERRCHK=1
        ERROR=1
C
C====== TYPE ERROR STATISTICS
C       GET LOC,SIZE,ACTUAL
C       GET MA
C
        CALL RREG(I,1026)
C>
        CALL I2ASCI(6,I,OUT1,IRADX,1)
        CALL I2ASCI(6,SIZFFT,OUT2,IRADX,1)
C
C====== GET DPX(-4) HIGH
C
        CALL RREG(I,1067)
C>
        CALL I2ASCI(6,I,OUT3,IRADX,0)
C
C====== GET DPX(-4) LOW
C
        CALL RREG(I,1083)
C>
        CALL I2ASCI(6,I,OUT4,IRADX,0)
        CALL I2ASCI(6,BASE,OUT5,IRADX,1)
C
C====== NOW GET ACTUAL, RANDOM #S
C       GET MD HIGH
C
        CALL RREG(I,1069)
C>
        CALL I2ASCI(6,I,OUT6,IRADX,0)
C
C====== AND MD LOW
C
        CALL RREG(I,1085)
C>
        CALL I2ASCI(6,I,OUT7,IRADX,0)
C
C====== GET DPA, DECR. IT TO GET RAND NUMBERS
C
        CALL RREG(I,1028)
C>
        I=I-1
        CALL WREG(I,516)
C
C====== GET DPX(-5)
C
        CALL RREG(I,1083)
C>
        CALL I2ASCI(6,I,OUT8,IRADX,0)
C
C====== GET DPY(-5)
C
        CALL RREG(I,1212)
C>
        CALL I2ASCI(6,I,OUT9,IRADX,0)
        WRITE(ITTO,2000) OUT5,JPAGE,OUT3,OUT4,OUT6,OUT7,
     +  OUT1,OUT2,OUT8,OUT9
        IF(IECHO.NE.0) WRITE(LFIL,2000) OUT5,JPAGE,OUT3,OUT4,OUT6,OUT7,
     +  OUT1,OUT2,OUT8,OUT9
        IF (FFTERR .EQ. 0) GO TO 30
C
        CALL LPTST
        IERRS=0
C
C====== CONTINUE AP TO GET NEXT ERROR OR END
C
        CALL SREAD(I)
        CALL WREG(I,8192)
        CALL APWTRN
C>
        GO TO 10
C
C====== FORMAT STATEMENTS
C
2000    FORMAT(6H BASE=,6A1,5X,6H PAGE=,I2,/,5X,3H E ,
     +  6A1,3X,6A1,/,5X,3H A ,6A1,3X,6A1,/,6H LOC =,6A1,5X,6H SIZE=,
     +  6A1,/,6H RNDX=,6A1,5X,6H RNDY=,6A1,//)
C
        END
C
C
C****** LDGO = LOAD S-PADS AND GO                           = REL 5.0  , NOV 79
        SUBROUTINE LDGO(IX,IY)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : FFT100>LDGO                                              *
C  *    DATE        : NOV  1, 1979                                             *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : IX => RANDOM S-PAD ARGUMENT                              *
C  *                  IY => RANDOM S-PAD ARGUMENT                              *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : LDGO LOADS S-PAD REGISTER 0 THROUGH S-PAD REGISTER 3     *
C  *                  AND THEN STARTS THE AP MICRO-CODE RUNNING. S-PAD         *
C  *                  REGISTER 0 IS LOADED WITH THE BASE VALUE, S-PAD          *
C  *                  REGISTER 1 IS LOADED WITH THE FFT SIZE (SIZFFT),         *
C  *                  S-PAD REGISTER 2 IS LOADED WITH A RANDOM NUMBER (IX),    *
C  *                  AND S-PAD REGISTER 3 IS ALSO LOADED WITH A RANDOM        *
C  *                  NUMBER (IY).                                             *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER IX,IY,BASE,SIZFFT,JPAGE,I
C
        COMMON /FIFT/   BASE,SIZFFT,JPAGE
C
C.
C..
C...
C
        CALL APRSET
C
C====== OUTPUT SPAD ARGS, SP(0)=BASE
C
        CALL WREG(0,513)
        CALL WREG(BASE,517)
C
C====== SP(1)=SIZE
C
        CALL WREG(1,513)
        CALL WREG(SIZFFT,517)
C
C====== SP(2)=IX
C
        CALL WREG(2,513)
        CALL WREG(IX,517)
C
C====== SP(3)=IY
C
        CALL WREG(3,513)
        CALL WREG(IY,517)
C
C====== NOW START AP RUNNING WITH TRIGGER IN SWR
C
C%.R    463     IBM
        CALL WREG(15,515)
        CALL WREG(8,512)
C%.E    463     IBM
        CALL SREAD(I)
        CALL WREG(I,8192)
C>
        RETURN
        END
C
C
C****** APWTRN = WAIT FOR AP TO STOP RUNNING                = REL 5.0  , NOV 79
        SUBROUTINE APWTRN
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : FFT100>APWTRN                                            *
C  *    DATE        : NOV  1, 1979                                             *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : N/A                                                      *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : APWTRN CHECKS THE FUNCTION REGISTER FOR THE 'AP RUNNING' *
C  *                  BIT.  IF THE AP IS RUNNING, THE PROGRAM WILL LOOP AND    *
C  *                  CONTINUE CHECKING THAT BIT UNTIL THE AP STOPS RUNNING.   *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER IN,I
C
C.
C..
C...
C
10      CALL IFN(IN)
C>
        CALL LPTST(I)
        IF(IRSH16(IN,15) .EQ. 0)GOTO 10
        RETURN
        END
