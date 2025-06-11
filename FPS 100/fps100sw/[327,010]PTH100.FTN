C****** PTH100.4K                                          == REL 5.0  , NOV 79
C****** PTH100 = AP DATA PATH TEST                          = REL 5.0  , NOV 79
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PTH100>MAINLINE                                          *
C  *    DATE        : NOV  5,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : N/A                                                      *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : PTH100 IS A TABLE DRIVEN DIAGNOSTIC.  AP MICROCODE IS    *
C  *                  LOADED INTO PROGRAM SOURCE AND EXECUTED.  VARIOUS        *
C  *                  BIT PATTERNS ARE USED FOR THE VARIOUS DATA PATH          *
C  *                  INSTRUCTIONS.  A PORTION OF THE TABLE WILL TEST          *
C  *                  TMRAM (IT IS ONLY USED FOR MACHINES THAT HAVE            *
C  *                  RAM TABLE MEMORY).  PTH100 HAS THE CAPABILITY            *
C  *                  OF ALSO TESTING TWO IOP'S.  THE PLUG ROUTINE             *
C  *                  (IN PTH100) PLUGS APPROPRIATE VALUES INTO THE            *
C  *                  TABLE IN ORDER TO TEST EITHER IOP1, IOP2, OR THE         *
C  *                  HOST INTERFACE AND DATA PATHS.                           *
C  *                  IF AN ERROR OCCURS IN TESTING ONE OF THE DATA PATHS,     *
C  *                  AN ERROR MESSAGE IS GENERATED THAT CONTAINS THE          *
C  *                  ACTUAL DATA VALUE, THE EXPECTED DATA VALUE, THE          *
C  *                  TABLE ENTRY VALUE (POINTS TO THE LOCATION IN THE         *
C  *                  TABLE WHERE THE ERROR OCCURRED), AND AN ERROR CODE       *
C  *                  VALUE.  THE ERROR CODE VALUE INDICATES WHICH DATA        *
C  *                  PATH WAS UNDER TEST WHEN AN ERROR OCCURRED.  THE         *
C  *                  FOLLOWING IS A LIST OF THE ERROR CODES:                  *
C  *                  ERROR CODE       UNIQUE PATH(S) UNDER TEST               *
C  *                  ==========       =========================               *
C  *                      64           PS LEFT HALF TO DPBS                    *
C  *                      65           PS RIGHT HALF (FP VALUE) TO DPBS        *
C  *                      66           DPBS TO PS LEFT HALF                    *
C  *                      67           DPBS TO PS RIGHT HALF                   *
C  *                      68           DPBS (LOW MANTISSA PORTION) TO SPAD     *
C  *                      69           DPBS (EXPONENT PORTION) TO SPAD         *
C  *                      70           DPBS TO I/O BUS TO WC                   *
C  *                      71           DPBS TO I/O BUS TO HMA                  *
C  *                      72           DPBS TO I/O BUS TO CTRL                 *
C  *                      73           DPBS TO I/O BUS TO APMA                 *
C  *                      74           DPBS TO I/O BUS TO FMT                  *
C  *                      77           MD TO FMT                               *
C  *                  CODE 78 IS EXECUTED AFTER CODE 85                        *
C  *                      79           DPX TO A2                               *
C  *                                   FA TO DPY                               *
C  *                      80           DPY TO A1                               *
C  *                                   FA TO DPX                               *
C  *                      81           DPY TO A2                               *
C  *                                   FA TO MD                                *
C  *                      82           DPY TO A1                               *
C  *                                   FA TO A2                                *
C  *                                   A2 TO MD                                *
C  *                      83           DPY TO M2                               *
C  *                                   FM TO DPX                               *
C  *                      84           DPY TO M1                               *
C  *                                   FM TO DPY                               *
C  *                      85           DPX TO M2                               *
C  *                                   FM TO MD                                *
C  *                      78           DPX TO A1                               *
C  *                                   FA TO M2                                *
C  *                                   FM TO DPY                               *
C  *                      86           DPX TO M1                               *
C  *                                   FM TO A1                                *
C  *                      87           MD TO M2                                *
C  *                                   FM TO M1                                *
C  *                      88           MD TO A2                                *
C  *                      89           TM TO DPBS                              *
C  *                      90           TM TO A1                                *
C  *                      91           TM TO M1                                *
C  *                      92           VAL TO DPBS (MANTISSA)                  *
C  *                                   SPFN TO MA                              *
C  *                      93           SPFN TO DPBS (MANTISSA)                 *
C  *                      94           VAL TO DPBS (EXPONENT)                  *
C  *                                   SPFN TO DPBS (EXPONENT)                 *
C  *                      95           SPFN TO A2 (EXPONENT)                   *
C  *                      96           SPFN TO A2 (MANTISSA)                   *
C  *                      97           NOT IMPLIMENTED                         *
C  *                      98           VAL TO EXIT                             *
C  *                                   EXIT TO PNLBUS                          *
C  *                                   PNLBUS TO SPAD                          *
C  *                      99           VAL+PSA TO EXIT                         *
C  *                                   (RELATIVE JSR)                          *
C  *                     100           SPFN (DPBS) TO TMA                      *
C  *                                   TMA TO EXIT                             *
C  *                     101           VAL TO PSA (JMPA)                       *
C  *                     102           VAL+PSA TO PSA (JMP)                    *
C  *                     103           EXIT TO PSA (RETURN JUMP)               *
C  *                     104           TMA TO PSA (JMPT)                       *
C  *                     105           CODE 70 FOR IOP1 (OPTION)               *
C  *                     106           CODE 71 FOR IOP1 (OPTION)               *
C  *                     107           CODE 72 FOR IOP1 (OPTION)               *
C  *                     108           CODE 73 FOR IOP1 (OPTION)               *
C  *                     109           CODE 70 FOR IOP2 (OPTION)               *
C  *                     110           CODE 71 FOR IOP2 (OPTION)               *
C  *                     111           CODE 72 FOR IOP2 (OPTION)               *
C  *                     112           CODE 73 FOR IOP2 (OPTION)               *
C  *                     113           CODE 70 FOR IOP3 (OPTION)               *
C  *                     114           CODE 71 FOR IOP3 (OPTION)               *
C  *                     115           CODE 72 FOR IOP3 (OPTION)               *
C  *                     116           CODE 73 FOR IOP3 (OPTION)               *
C  *                     200           TABLE MEMORY RAM (OPTION)               *
C  *                     201           TABLE MEMORY RAM (OPTION)               *
C  *                     203           TABLE MEMORY RAM (OPTION)               *
C  *                                                                           *
C  *                                                                           *
C  *    GENERALITIES: BEFORE PTH100 CAN BE USED AS AN EFFECTIVE DIAGNOSTIC,    *
C  *                  APTEST SHOULD BE RUN TO TEST THE INTERFACE REGISTERS     *
C  *                  AND THE MEMORIES.                                        *
C: *****************************************************************************
C%.I    103     MITRA
        INTEGER INCNT,INLOC,ITBLOC,ICLOC,IOTMSK,IOTCNT,IOTFMT,INFMT,
     +  ISLOC,INMSK,ITTI,ITTO,LFIL,ISTAT,IRTURN,ICHNGE,WATERR,
     +  LOOPER,TYPDIS,IORST,HALT,FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,
     +  IERLIM,IPSLIM,ERRCNT,PASCNT,IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ
        INTEGER
     +  PSIZE,BASLOW,MDSIZ,IAPNUM,IECHO,IERRS,IMSIZ,IPSIZE,NPAGES,
     +  ITBL,IT20,IT21,IT30,IT31,IHANS,IVTMP,I,LPCNT,ICRCNT,NEG,INO1(6)
        INTEGER INO2(6)
C
C
        INTEGER IDDPA,IDDPXE,IDDPXH,IDDPXL,IDDPYE,IDDPYH,IDDPYL,
     +  IDTMA,IDEP,ICONT,IEDPXE,IEDPXH,IEDPXL,IDEPSP,IDPSPA,IETMA,IEXAM,
     +  ISTART,IDPS0,IDPS1,IDPS2,IDPS3,IEXSP,IEDPYE,IEDPYH,IEDPYL,
     +  IX,IY,IZ,IS,IT
        INTEGER
     +  STATBF,STATNM,STRAD,IERROR
C
C
        REAL RMDSIZ
C
        COMMON /APPTH/  INCNT,INLOC,ITBLOC,ICLOC,IOTMSK,IOTCNT,IOTFMT,
     1  INFMT,ISLOC,INMSK
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /TABLE/  ITBL(1561),IT20,IT21,IT30,IT31,IHANS(4),
     1  IVTMP(4)
C
C
        COMMON /ERORR/  IERROR
C
        COMMON /RTRAN/  IX(3),IY(3),IZ(3),IS,IT
C
        COMMON /APCOM/  IDDPA,IDDPXE,IDDPXH,IDDPXL,IDDPYE,IDDPYH,IDDPYL,
     1  IDTMA,IDEP,ICONT,IEDPXE,IEDPXH,IEDPXL,IDEPSP,IDPSPA,IETMA,IEXAM,
     2  ISTART,IDPS0,IDPS1,IDPS2,IDPS3,IEXSP,IEDPYE,IEDPYH,IEDPYL
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
C
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),  IOP1) , (ISTAT( 7),  IOP2) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IERLIM) , (ISTAT(10),IPSLIM) , (ISTAT(11),ERRCNT)
        EQUIVALENCE
     +  (ISTAT(12),PASCNT) , (ISTAT(13), IERRS) , (ISTAT(14),TESTFG),
     +  (ISTAT(15),  TM4K) , (ISTAT(16),  IOP3) , (ISTAT(17),IWRTST),
     +  (ISTAT(18), ISTEP)
C.
C..
C...
C
C====== SET TM4K=0 FOR MACHINES WITH 2.5K OF TMROM
C           TM4K=1 FOR MACHINES WITH 4.5K OF TMROM
C
        TM4K=1
C
C====== CHECK INTERNAL DATA PATHS OF AP
C       PTH100 IS TABLE DRIVEN. UCODE IS LOADED INTO PS AND
C       EXECUTED. DIFFERENT BIT PATTERNS ARE EXECUTED FOR THE
C       VARIOUS DATA PATH INSTRUCTIONS.
C       NEG EQUALS 100000 OCTAL
C
C
C
        NEG=INOT16(32767)
C
C====== PUT -32768 (100000 OCTAL) INTO TABLE WHERE NECC.
C
        ITBL(338)=NEG
        ITBL(521)=NEG
        ITBL(797)=NEG
        ITBL(871)=NEG
        ITBL(922)=NEG
        ITBL(1052)=NEG
C
C====== CLEAR DATA TABLE
C
        DO 10 I=77,84,1
10      ITBL(I)=0
C
C====== PUT 1.0 IN TO TABLE
C
        ITBL(82)=32
        ITBL(83)=5120
C
C====== IROMSZ IS USED FOR SELECTING THE START OF TMRAM
C
        IROMSZ=8192
        IF(TM4K.NE.0) CALL ADJTBL
C
C====== INITIALIZE CONSOLE DEVICE LOGICAL UNIT NUMBERS
C
        IF(IOCNTL(1,0,0,0) .NE.0) CALL EXIT
C
C====== WRITE HEADER
C
        WRITE(ITTO,2000)
C
C====== PROCESS COMMANDS
C
        WRITE(ITTO,2010)
C
        CALL SETUP(1)
30      CALL SETUP(0)
        IF(IPSLIM.NE.0.AND.IPSLIM.LT.PSSFLG) PSSFLG=IPSLIM
C
C====== PRINT ERROR FILE HEADER IF SPECIFIED
C
        IF(IECHO.NE.0) WRITE(LFIL,2020)
C
C====== INITIALIZE AP
C
        CALL APXSET
C
C>
C====== RESET COUNTERS
C
        PASCNT=0
        ERRCNT=0
        LPCNT=0
C
        GO TO 50
C
40      LPCNT=0
        CALL I2ASCI(6,ERRCNT,INO1,10,1)
        CALL I2ASCI(6,PASCNT,INO2,10,1)
        WRITE(ITTO,2030) INO1,INO2
        IF(IECHO.NE.0) WRITE(LFIL,2030) INO1,INO2
C
C====== THIS IS THE RE-ENTRY POINT FOR PTH100
C
50      IF(PSSFLG.EQ.0) PSSFLG=1
        IF(LPCNT.EQ.PSSFLG) GO TO 40
        IF(IPSLIM.EQ.0) GO TO 60
        IF(IPSLIM.EQ.PASCNT) GO TO 30
C
60      LPCNT=LPCNT+1
        PASCNT=PASCNT+1
        CALL APRSET
C
C====== CHECK FOR TMRAM TEST FLAG ( T=200 )
C
        IF(TESTFG.NE.0) CALL TMRAMS(IROMSZ)
C
C====== SEE IF WE HAVE IOP1
C
70      IF(IOP1 .EQ. 0) GO TO 80
C
C====== PATCH FOR IOP1 AND RUN THE TEST
C
        CALL PLUG(1)
        CALL DPAT(96)
C
C====== SEE IF IOP2 IS SET
C
80      IF(IOP2 .EQ. 0) GO TO 85
C
C====== PATCH FOR IOP2 AND RUN THE TEST
C
        CALL PLUG(2)
        CALL DPAT(96)
C
C====== SEE IF IOP3 IS SET
C
85      IF(IOP3.EQ.0) GO TO 90
C
C====== PATCH FOR IOP3 AND RUN THE TEST
C
        CALL PLUG(3)
        CALL DPAT(96)
C
C====== LASTLY, TEST THE HOST INTERFACE
C
90      CALL PLUG(0)
        CALL DPAT(96)
        IF(TYPDIS.NE.0) GO TO 70
        GO TO 50
C
C====== FORMAT STATEMENTS
C
2000    FORMAT(35H  ***   PTH100.4K               ***//)
2010    FORMAT(20H PLEASE ASSIGN AN AP)
2020    FORMAT(35H1 ***   PTH100.4K               ***//,
     +        26H        ERROR LOGGING FILE ,////)
2030    FORMAT(17H PTH100 - ERRORS=,6A1,9H, PASSES=,6A1)
C
        END
C****** IMCMD2 = IMMEDIATE EXECUTION COMMANDS               = REL 5.0  , NOV 79
        SUBROUTINE IMCMD(INX,INBUF,IPOS,IRADX)
C
        GO TO (10,20),INX
C
C====== DUMP
C
10      CALL MEMDMP(INBUF,IPOS,IRADX)
        RETURN
C
C====== MEMSIZ
C
20      CALL MEMSIZ
        RETURN
        END
C****** LPTST2 = LOOP TEST FOR USER INPUT                   = REL 5.0  , NOV 79
        SUBROUTINE LPTST(IRTURN)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : DGCOM>LPTST                                              *
C  *    DATE        : NOV  11,1979                                             *
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
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
C
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),  IOP1) , (ISTAT( 7),  IOP2) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IERLIM) , (ISTAT(10),IPSLIM) , (ISTAT(11),ERRCNT)
        EQUIVALENCE
     +  (ISTAT(12),PASCNT) , (ISTAT(13), IERRS) , (ISTAT(14),TESTFG),
     +  (ISTAT(15),  TM4K) , (ISTAT(16),  IOP3) , (ISTAT(17),IWRTST),
     +  (ISTAT(18), ISTEP)
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
C>
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
C****** ASST2 = LISTS AVAILABLE COMMANDS FOR THIS TEST      = REL 5.0  , NOV 79
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
     +  45H ECHO     - PROMPTS USER TO OPEN AN ECHO FILE               /,
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
     +  47H BETWN=N  - SET PASSES BETWEEN HEADER PRINTOUTS             /,
     +  50H ERRLIM=N - MAXIMUM ERRORS BEFORE RETURNING TO CSI          /,
     +  )
C
        WRITE(ITTO,16)
        IF(IECHO.NE.0) WRITE(LFIL,16)
C
16      FORMAT (
     +  25H TIMLIM=N - NOT SUPPORTED                                   /,
     +  49H PASLIM=N - PASSES TO RUN BEFORE RETURNING TO CSI           /,
     +  49H NAMES    - INDICATES THE COMPLETION OF EACH TEST           /,
     +  34H D        - DISABLE ERROR PRINTOUT                          /,
     +  37H H        - HALT AFTER EACH TEST CASE                       /,
     +  )
C
        WRITE(ITTO,18)
        IF(IECHO.NE.0) WRITE(LFIL,18)
C
18      FORMAT (
     +  38H I        - RESET AFTER EACH TEST CASE                      /,
     +  25H L        - LOOP ON ERROR                                   /,
     +  34H W        - RETURN TO CSI ON ERROR                          /,
     +  34H TMRAM    - TESTS TABLE MEMORY RAM                          /,
     +  )
C
        WRITE(ITTO,20)
        IF(IECHO.NE.0) WRITE(LFIL,20)
C
20      FORMAT (
     +  22H IOP1     - TESTS IOP1                                      /,
     +  22H IOP2     - TESTS IOP2                                      /,
     +  22H IOP3     - TESTS IOP3                                      /,
     +  43H /*       - /* CAN BE FOLLOWED BY A COMMENT                 /,
     +  )
C
        WRITE(ITTO,22)
        IF(IECHO.NE.0) WRITE(LFIL,22)
C
22      FORMAT (
     +  44H MEMSIZ   - DISPLAYS PS AND MD SIZES                        /,
     +  47H DUMP XX  - DUMPS SPECIFIED MEMORY (XX)                     /,
     +  51H STEP     - EXECUTES IN STEP MODE (DEFAULT)                 /,
     +  44H -STEP    - EXECUTES IN CHAINED MODE                        /,
     +  )
        RETURN
        END
C****** SETUP2 = SETS UP DATA FOR CSI                       = REL 5.0  , NOV 79
C
        SUBROUTINE SETUP(INIT)
C
        INTEGER XTB(16,12),INIT,XTLEN
        COMMON /ERORR/ IERROR
        COMMON /TOTAL/ ISTAT(40)
C
C
        DATA XTB( 1, 1),XTB( 1, 2),XTB( 1, 3)   /1HB,1HE,1HT/
        DATA XTB( 1, 4),XTB( 1, 5),XTB( 1, 6)   /1HW,1HN,1H /
        DATA XTB( 1, 7),XTB( 1, 8),XTB( 1, 9)   /     2,     8,     1/
        DATA XTB( 1,10),XTB( 1,11),XTB( 1,12)   /     0,     0,    10/
C
        DATA XTB( 2, 1),XTB( 2, 2),XTB( 2, 3)   /1HE,1HR,1HR/
        DATA XTB( 2, 4),XTB( 2, 5),XTB( 2, 6)   /1HL,1HI,1HM/
        DATA XTB( 2, 7),XTB( 2, 8),XTB( 2, 9)   /     2,     9,    64/
        DATA XTB( 2,10),XTB( 2,11),XTB( 2,12)   /     0,     0,    10/
C
        DATA XTB( 3, 1),XTB( 3, 2),XTB( 3, 3)   /1HP,1HA,1HS/
        DATA XTB( 3, 4),XTB( 3, 5),XTB( 3, 6)   /1HL,1HI,1HM/
        DATA XTB( 3, 7),XTB( 3, 8),XTB( 3, 9)   /     2,    10,     1/
        DATA XTB( 3,10),XTB( 3,11),XTB( 3,12)   /     0,     0,    10/
C
        DATA XTB( 4, 1),XTB( 4, 2),XTB( 4, 3)   /1HW,1H ,1H /
        DATA XTB( 4, 4),XTB( 4, 5),XTB( 4, 6)   /1H ,1H ,1H /
        DATA XTB( 4, 7),XTB( 4, 8),XTB( 4, 9)   /     3,     1,     0/
        DATA XTB( 4,10),XTB( 4,11),XTB( 4,12)   /     0,     0,     0/
C
        DATA XTB( 5, 1),XTB( 5, 2),XTB( 5, 3)   /1HL,1H ,1H /
        DATA XTB( 5, 4),XTB( 5, 5),XTB( 5, 6)   /1H ,1H ,1H /
        DATA XTB( 5, 7),XTB( 5, 8),XTB( 5, 9)   /     3,     2,     0/
        DATA XTB( 5,10),XTB( 5,11),XTB( 5,12)   /     0,     0,     0/
C
        DATA XTB( 6, 1),XTB( 6, 2),XTB( 6, 3)   /1HD,1H ,1H /
        DATA XTB( 6, 4),XTB( 6, 5),XTB( 6, 6)   /1H ,1H ,1H /
        DATA XTB( 6, 7),XTB( 6, 8),XTB( 6, 9)   /     3,     3,     0/
        DATA XTB( 6,10),XTB( 6,11),XTB( 6,12)   /     0,     0,     0/
C
        DATA XTB( 7, 1),XTB( 7, 2),XTB( 7, 3)   /1HI,1H ,1H /
        DATA XTB( 7, 4),XTB( 7, 5),XTB( 7, 6)   /1H ,1H ,1H /
        DATA XTB( 7, 7),XTB( 7, 8),XTB( 7, 9)   /     3,     4,     0/
        DATA XTB( 7,10),XTB( 7,11),XTB( 7,12)   /     0,     0,     0/
C
        DATA XTB( 8, 1),XTB( 8, 2),XTB( 8, 3)   /1HH,1H ,1H /
        DATA XTB( 8, 4),XTB( 8, 5),XTB( 8, 6)   /1H ,1H ,1H /
        DATA XTB( 8, 7),XTB( 8, 8),XTB( 8, 9)   /     3,     5,     0/
        DATA XTB( 8,10),XTB( 8,11),XTB( 8,12)   /     0,     0,     0/
C
        DATA XTB( 9, 1),XTB( 9, 2),XTB( 9, 3)   /1HI,1HO,1HP/
        DATA XTB( 9, 4),XTB( 9, 5),XTB( 9, 6)   /1H1,1H ,1H /
        DATA XTB( 9, 7),XTB( 9, 8),XTB( 9, 9)   /     3,     6,     0/
        DATA XTB( 9,10),XTB( 9,11),XTB( 9,12)   /     0,     0,     0/
C
        DATA XTB(10, 1),XTB(10, 2),XTB(10, 3)   /1HI,1HO,1HP/
        DATA XTB(10, 4),XTB(10, 5),XTB(10, 6)   /1H2,1H ,1H /
        DATA XTB(10, 7),XTB(10, 8),XTB(10, 9)   /     3,     7,     0/
        DATA XTB(10,10),XTB(10,11),XTB(10,12)   /     0,     0,     0/
C
        DATA XTB(11, 1),XTB(11, 2),XTB(11, 3)   /1HI,1HO,1HP/
        DATA XTB(11, 4),XTB(11, 5),XTB(11, 6)   /1H3,1H ,1H /
        DATA XTB(11, 7),XTB(11, 8),XTB(11, 9)   /     3,    16,     0/
        DATA XTB(11,10),XTB(11,11),XTB(11,12)   /     0,     0,     0/
C
        DATA XTB(12, 1),XTB(12, 2),XTB(12, 3)   /1HT,1HM,1HR/
        DATA XTB(12, 4),XTB(12, 5),XTB(12, 6)   /1HA,1HM,1H /
        DATA XTB(12, 7),XTB(12, 8),XTB(12, 9)   /     3,    14,     0/
        DATA XTB(12,10),XTB(12,11),XTB(12,12)   /     0,     0,     0/
C
        DATA XTB(13, 1),XTB(13, 2),XTB(13, 3)   /1HD,1HU,1HM/
        DATA XTB(13, 4),XTB(13, 5),XTB(13, 6)   /1HP,1H ,1H /
        DATA XTB(13, 7),XTB(13, 8),XTB(13, 9)   /     5,     0,     1/
        DATA XTB(13,10),XTB(13,11),XTB(13,12)   /     0,     0,     0/
C
        DATA XTB(14, 1),XTB(14, 2),XTB(14, 3)   /1HN,1HA,1HM/
        DATA XTB(14, 4),XTB(14, 5),XTB(14, 6)   /1HE,1HS,1H /
        DATA XTB(14, 7),XTB(14, 8),XTB(14, 9)   /     3,    17,     1/
        DATA XTB(14,10),XTB(14,11),XTB(14,12)   /     0,     0,     0/
C
        DATA XTB(15, 1),XTB(15, 2),XTB(15, 3)   /1HM,1HE,1HM/
        DATA XTB(15, 4),XTB(15, 5),XTB(15, 6)   /1HS,1HI,1HZ/
        DATA XTB(15, 7),XTB(15, 8),XTB(15, 9)   /     5,     0,     2/
        DATA XTB(15,10),XTB(15,11),XTB(15,12)   /     0,     0,     0/
C
        DATA XTB(16, 1),XTB(16, 2),XTB(16, 3)   /1HS,1HT,1HE/
        DATA XTB(16, 4),XTB(16, 5),XTB(16, 6)   /1HP,1H ,1H /
        DATA XTB(16, 7),XTB(16, 8),XTB(16, 9)   /     3,    18,     1/
        DATA XTB(16,10),XTB(16,11),XTB(16,12)   /     0,     0,     0/
C
C
C.
C..
C...
        IF(INIT.NE.0) GO TO 20
C
        IERROR=0
        CALL CSI(XTB,16,ISTAT,40,0)
C
C====== STEP MODE?
C
        CALL APMODE(0)
        IF(ISTAT(38).EQ.1) CALL APMODE(1)
C
        GO TO 30
C
20      CALL CSI(XTB,16,ISTAT,40,1)
C
30      CONTINUE
        RETURN
        END
C****** ITLS = TRIPLE LEFT SHIFT                            = REL 5.0  , NOV 79
        SUBROUTINE ITLS(IA,IB,IC,N)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : DGCOM>ITLS                                               *
C  *    DATE        : NOV  11,1979                                             *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : IA => HIGH WORD                                          *
C  *                  IB => MIDDLE WORD                                        *
C  *                  IC => LOW WORD                                           *
C  *                  N  => SHIFT COUNT                                        *
C  *                                                                           *
C  *    EXIT        : IA <= SHIFTED HIGH WORD                                  *
C  *                  IB <= SHIFTED MIDDLE WORD                                *
C  *                  IC <= SHIFTED LOW WORD                                   *
C  *                                                                           *
C  *    FUNCTION    : ITLS TAKES 'IA, IB, IC' AS A TRIPLE LENGTH WORD AND      *
C  *                  SHIFTS IT LEFT BY 'N' PLACES.  THE ROUTINE FILLS WITH    *
C  *                  ONES SHIFTING IN FROM THE RIGHT.                         *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER IA,IB,IC,N,K,KK
C
C.
C..
C...
C
          IF (N.LE.0) RETURN
          IF (N.GE.16.AND.N.LE.31) GO TO 10
          IF (N.GE.32) GO TO 20
C
C====== SHIFT 1-15 PLACES LEFT
C
          K=16-N
          IA=IOR16(ILSH16(IA,N),IRSH16(IB,K))
          IB=IOR16(ILSH16(IB,N),IRSH16(IC,K))
          IC=IOR16(ILSH16(IC,N),IRSH16(INOT16(0),K))
          RETURN
C
C====== PLACES LEFT
C
10        K=N-16
          KK=32-N
          IA=IOR16(ILSH16(IB,K),IRSH16(IC,KK))
          IB=IOR16(ILSH16(IC,K),IRSH16(INOT16(0),KK))
          IC=INOT16(0)
          RETURN
C
C====== OR MORE PLACES LEFT
C
20        K=N-32
          IA=IOR16(ILSH16(IC,K),IRSH16(INOT16(0),48-N))
          IB=INOT16(0)
          IC=IB
          RETURN
        END
C
C
C****** ADJTBL = ADJUST TABLE TO TEST 4K TM                 = REL 5.0  , NOV 79
        SUBROUTINE ADJTBL
C
        COMMON /TABLE/  ITBL(1561),ITEMP(12)
C
C.
C..
C...
        ITBL(6) =-9
        ITBL(9) =-39
        ITBL(12)=-157
        ITBL(15)=-631
        ITBL(18)=-2526
        ITBL(21)=-10106
        ITBL(23)=2047
        ITBL(24)=25112
        ITBL(26)=2045
        ITBL(27)=-30599
        ITBL(29)=2038
        ITBL(30)=9065
        ITBL(32)=2008
        ITBL(33)=-23052
        ITBL(35)=1892
        ITBL(36)=6900
C
C
        RETURN
        END
C****** PLUG = PLUG IN ALTERNATE PTH100 TABLE DATA          = REL 5.0  , NOV 79
        SUBROUTINE PLUG(IWHCH)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PTH100>PLUG                                              *
C  *    DATE        : NOV  5,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : IWHCH => 0 - PATCHES TO TEST THE HOST INTERFACE          *
C  *                           1 - PATCHES TO TEST IOP1                        *
C  *                           2 - PATCHES TO TEST IOP2                        *
C  *                           3 - PATCHES TO TEST IOP3                        *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : PLUG IS USED TO ALTER THE TABLE WHICH DRIVES PTH100      *
C  *                  IN ORDER TO TEST IOP1, IOP2, OR THE HOST INTERFACE.      *
C  *                                                                           *
C  *    GENERALITIES: THE PLUGIN TABLE IS A 33 ELEMENT ARRAY.  IT IS GROUPED   *
C  *                  INTO 8 GROUPS OF 4 ELEMENTS.  EACH GROUP HAS THE         *
C  *                  FOLLOWING FORMAT:                                        *
C  *                  WORD #1 - SUBSCRIPT FOR ITBL.  THIS IS THE ITBL ELEMENT  *
C  *                            WHICH WILL BE ALTERED.                         *
C  *                  WORD #2 - VALUE FOR ITBL(WORD #1) IF THE HOST INTERFACE  *
C  *                            IS TO BE TESTED.                               *
C  *                  WORD #3 - VALUE FOR ITBL(WORD #1) IF THE IOP1 IS TO      *
C  *                            BE TESTED.                                     *
C  *                  WORD #4 - VALUE FOR ITBL(WORD #1) IF THE IOP2 IS TO      *
C  *                            BE TESTED.                                     *
C  *                  -                                                        *
C  *                  ELEMENT 33 OF THE PLUGIN ARRAY INDICATES THAT ALL THE    *
C  *                   APPROPRIATE LOCATIONS HAVE BEEN ALTERED AND A RETURN    *
C  *                  IS ISSUED.                                               *
C  *                                                                           *
C: *****************************************************************************
        INTEGER ITBL,IWHCH,PLUGIN(33),I,PTR,WHERE
C
        COMMON /TABLE/  ITBL(1561),ITEMP(12)
C
C.
C..
C...
C
        DATA PLUGIN/
     +  1471,     8,        16,       24,
     +  1496,     105,      109,      113,
     +  1499,     9,        17,       25,
     +  1524,     106,      110,      114,
     +  1527,     10,       18,       26,
     +  1544,     107,      111,      115,
     +  1547,     11,       19,       27,
     +  1559,     108,      112,      116,
     +  -1/
C
C====== IF IOP, RUN ONLY THE IOP TESTS
C
        IF(IWHCH.EQ.0) GO TO 3
        ITBL(97)=-1
        ITBL(98)=1468
        GO TO 5
3       ITBL(97)=37
        ITBL(98)=2
C
C====== START AT BEGINNING OF ARRAY
C
5       PTR=1
C
C====== LOOP HERE
C
10      WHERE = PLUGIN(PTR)
        IF (WHERE .EQ. -1) RETURN
C
C====== PATCH THE WORD
C
        I=PTR+IWHCH
        ITBL(WHERE) = PLUGIN(I)
C
C====== INCREMENT TO THE NEXT LOCATION
C       CONSTANT CAN BE CHANGED TO ALLOW MORE POSSIBLE SETS
C       OF PATCHES BY CHANGING IT TO (N+1) TO ALLOW N POSSIBLE
C       PATCHES.
C
        PTR = PTR+4
        GO TO 10
        END
C****** PNLLD = LOAD MICRO-CODE THROUGH THE PANEL           = REL 5.0  , NOV 79
        SUBROUTINE PNLLD
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PTH100>PNLLD                                             *
C  *    DATE        : NOV  5,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : THE TABLE AND THE TABLE POINTERS ARE PASSED THROUGH      *
C  *                  COMMONS /TABLE/ AND /APPTH/                              *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : PNLLD LOADS PROGRAM SOURCE INTO THE AP VIA PANEL         *
C  *                  OPERATIONS.  12 PROGRAM SOURCE WORDS ARE TRANSFERRED     *
C  *                  EACH TIME THIS ROUTINE IS CALLED.  THE VALUES USED       *
C  *                  IN THE TRANSFER ARE TAKEN FROM LOCATION 41 TO            *
C  *                  LOCATION 88 IN THE TABLE.  DPAT (PTH100 SUBROUTINE)      *
C  *                  LOADS THESE 48 LOCATIONS WITH THE PROPER VALUES          *
C  *                  NEEDED TO RUN EACH DATA PATH TEST.                       *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER ITBL,IT20,IT21,IT30,IT31,IHANS,IVTMP,INCNT,INLOC,ITBLOC,
     +  ICLOC,IOTMSK,IOTCNT,IOTFMT,INFMT,ISLOC,INMSK,ITTI,ITTO,LFIL,
     +  RJUSFY,ISTAT,IRTURN,ICHNGE,WATERR,LOOPER,TYPDIS,IORST,HALT,
     +  FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,IERLIM,IPSLIM,ERRCNT,PASCNT
        INTEGER
     +  IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ,PSIZE,BASLOW,MDSIZ,IAPNUM,
     +  IECHO,IERRS,IMSIZ,IPSIZE,NPAGES,IDDPA,IDDPXE,IDDPXH,IDDPXL
        INTEGER IDDPYE,IDDPYH,IDDPYL,IDTMA,IDEP,ICONT,IEDPXE,IEDPXH,
     +  IEDPXL,IDEPSP,IDPSPA,IETMA,IEXAM,ISTART,IDPS0,IDPS1,IDPS2,IDPS3,
     +  IEXSP,IEDPYE,IEDPYH,IEDPYL,ITMA,IN,ITMP(4)
C
        REAL RMDSIZ
C
        COMMON /TABLE/  ITBL(1561),IT20,IT21,IT30,IT31,IHANS(4),
     1  IVTMP(4)
C
        COMMON /APPTH/  INCNT,INLOC,JTBLOC,ICLOC,IOTMSK,IOTCNT,IOTFMT,
     1  INFMT,ISLOC,INMSK
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /APCOM/  IDDPA,IDDPXE,IDDPXH,IDDPXL,IDDPYE,IDDPYH,IDDPYL,
     1  IDTMA,IDEP,ICONT,IEDPXE,IEDPXH,IEDPXL,IDEPSP,IDPSPA,IETMA,IEXAM,
     2  ISTART,IDPS0,IDPS1,IDPS2,IDPS3,IEXSP,IEDPYE,IEDPYH,IEDPYL
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
C
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),  IOP1) , (ISTAT( 7),  IOP2) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IERLIM) , (ISTAT(10),IPSLIM) , (ISTAT(11),ERRCNT)
        EQUIVALENCE
     +  (ISTAT(12),PASCNT) , (ISTAT(13), IERRS) , (ISTAT(14),TESTFG),
     +  (ISTAT(15),  TM4K) , (ISTAT(16),  IOP3) , (ISTAT(17),IWRTST),
     +  (ISTAT(18), ISTEP)
C.
C..
C...
C
C====== SAVE TMA
C
        CALL RREG(ITMA,1027)
C
C>
C====== GET LOCATION IN TABLE TO START LOAD MICROCODE FROM
C
        ITBLOC=JTBLOC
10      ITBLOC=ITBLOC+1
C
C====== CODE=0 END=1 START=2
C
        IF(ITBL(ITBLOC-1)-1)40,20,30
C
C====== SET PSA TO STARTING ADDRESS
C
20      ITMP(1)=IP16(ITBL(ITBLOC+1))
        CALL WREG(ITMP(1),IDEP)
C>
        RETURN
C
C====== SET TMA TO ORIGINAL VALUE
C
30      CALL WREG(ITMA,IDTMA)
        ITBLOC=ITBLOC+1
C
C====== SET PSA TO ADDRESS
C
        ITMP(1)=IP16(ITBL(ITBLOC))
        CALL WREG(ITMP(1),IDEP)
C
C====== SET TRIGGER POINT IN SWITCH REGISTER
C
        CALL SREAD(IN)
C
C====== START AP RUNNING
C
        CALL WREG(IN,8192)
C>
        RETURN
C
C====== LOAD MICROCODE INTO AP
C
40      ITEMP1=-ITBL(ITBLOC)
C
C====== ITEMP1 IS NUMBER OF WORDS OF PS TO BE LOADED
C
        IF(ITEMP1 .EQ. 0)RETURN
C
C====== SET TMA TO DEPOSIT INTO PS
C
        ITMP(3)=IP16(ITBL(ITBLOC+1))
        CALL WREG(ITMP(3),IDTMA)
C>
        ITBLOC=ITBLOC+2
C
C====== PUT THE HIGH SIXTEEN BITS INTO PROGRAM SOURCE
C       THAT IS Q1
C
50      CONTINUE
        ITMP(1)=IP16(ITBL(ITBLOC+1))
        ITMP(2)=IP16(ITBL(ITBLOC+2))
        ITMP(3)=IP16(ITBL(ITBLOC+3))
        ITMP(4)=IP16(ITBL(ITBLOC+4))
        CALL WREG(ITMP(1),IDPS0)
C
C====== PUT THE SECOND SIXTEEN BITS INTO PROGRAM SOURCE
C       THAT IS Q2
C
        CALL WREG(ITMP(2),IDPS1)
C
C====== PUT THE THIRD SIXTEEN BITS INTO PROGRAM SOURCE
C       THAT IS Q3
C
        CALL WREG(ITMP(3),IDPS2)
C
C====== PUT THE FOURTH SIXTEEN BITS INTO PROGRAM SOURCE
C       DEPOSIT Q4 AND INCREAMENT TMA BY ONE
C
        CALL WREG(ITMP(4),IDPS3)
C>
        ITBLOC=ITBLOC+4
        ITEMP1=ITEMP1+1
C
C====== SEE IF ALL MICROCODE HAS BEEN LOADED
C       IF NOT GO AND LOAD NEXT WORD
C
        IF(ITEMP1 .NE. 0)GOTO 50
        ITBLOC=ITBLOC+1
        GOTO 10
        END
C****** RUNIT = RUN AP MICRO-CODE                           = REL 5.0  , NOV 79
        SUBROUTINE RUNIT
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PTH100>RUNIT                                             *
C  *    DATE        : NOV  5,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : ALL PARAMETERS ARE PASSED IN COMMONS.                    *
C  *                  ICLOC - BEGINNING TABLE LOCATION                         *
C  *                  ITBL  - PTH100 TABLE                                     *
C  *                  IOTFMT- OUTPUT FORMAT                                    *
C  *                  IOTCNT- OUTPUT BIT COUNT                                 *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : RUNIT LOADS AP MICRO-CODE VIA PNLLD (PTH100 SUBROUTINE). *
C  *                  THE MICRO-CODE IS EXECUTED AND RUNIT TRANSFERS THE DATA  *
C  *                  RESULTS BACK TO THE HOST FOR EVALUATION.  DATA IN THE    *
C  *                  TABLE DETERMINES THINGS AS THE NUMBER OF PANEL OPERATIONS*
C  *                  NEEDED TO SET UP THE TEST, THE NUMBER OF PANEL OPERATIONS*
C  *                  TO READ ANSWERS FROM THE AP, AND THE ERROR CODES USED    *
C  *                  FOR ERROR MESSAGES.  THE ERROR MESSAGES ARE GENERATED    *
C  *                  IN RUNIT.                                                *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER ITBL,IT20,IT21,IT30,IT31,IHANS,IVTMP,INCNT,INLOC,ITBLOC,
     +  ICLOC,IOTMSK,IOTCNT,IOTFMT,INFMT,ISLOC,INMSK,ITTI,ITTO,LFIL,
     +  RJUSFY,ISTAT,IRTURN,ICHNGE,WATERR,LOOPER,TYPDIS,IORST,HALT,
     +  FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,IERLIM,IPSLIM,ERRCNT,PASCNT
        INTEGER
     +  IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ,PSIZE,BASLOW,MDSIZ,IAPNUM,
     +  IECHO,IRADX,IERRS,IMSIZ,IPSIZE,NPAGES,INO1(6),INO2(6),INO3(6,3)
        INTEGER ITEMP1,ITEMP2,ITEMP3,ITEMP0,IACNT,N,J,I
C
        REAL RMDSIZ
C
        COMMON /TABLE/  ITBL(1561),IT20,IT21,IT30,IT31,IHANS(4),
     1  IVTMP(4)
C
        COMMON /APPTH/  INCNT,INLOC,ITBLOC,ICLOC,IOTMSK,IOTCNT,IOTFMT,
     1  INFMT,ISLOC,INMSK
C
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
C
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),  IOP1) , (ISTAT( 7),  IOP2) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IERLIM) , (ISTAT(10),IPSLIM) , (ISTAT(11),ERRCNT)
        EQUIVALENCE
     +  (ISTAT(12),PASCNT) , (ISTAT(13), IERRS) , (ISTAT(14),TESTFG),
     +  (ISTAT(15),  TM4K) , (ISTAT(16),  IOP3) , (ISTAT(17),IWRTST),
     +  (ISTAT(18), ISTEP)
C.
C..
C...
C
C====== LOAD MICROCODE AND START RUNNING
C
10      CALL PNLLD
        IERRS=0
C
C====== GET LOCATION TO START IN TABLE
C
        IT20=ICLOC+1
C
C====== GET NUMBER OF PANEL OPS TO SET UP SO ANSWERS CAN BE READ BACK
C
        ITEMP1=-ITBL(IT20)
C
C====== SEE IF NUMBER IS ZERO
C
        IF(ITEMP1 .EQ. 0)GOTO 100
20      IT20=IT20+1
C
C====== PUT NUMBER INTO SWITCH REGISTER TO BE DEPOSITED
C
        CALL OSWR(IP16(ITBL(IT20)))
C>
        IT20=IT20+1
        ITEMP0=ITBL(IT20)
C
C====== SEE IF PANEL OP IS A OFN,OCTRL,OAPMA,IFMTL,IFMTH
C
        IF(ITEMP0 - 25142)90,30,50
C
C====== IF EQUAL 25142 PUT VALUE IN SWITCH REGISTER INTO OAPMA
C
30      CALL ISWR(ITEMP0)
        CALL OAPMA(ITEMP0)
C
C====== INCREMENT POINT TO NEXT PANEL OPERATION
C
40      CONTINUE
C>
        ITEMP1=ITEMP1+1
C
C====== SEE IF THROUGH WITH PANEL OPERATIONS
C
        IF(ITEMP1 .EQ. 0)GOTO 100
        GOTO 20
C
C====== SEE IF PANEL OPERATION IS A OFN,OCTRL,IFMTH,IFMTL
C
50      IF(ITEMP0 - 25910)60,70,80
C
C====== IF EQUAL TO 25910 THEN OPERATION IS A IFMTL OPERATION.
C       IF EQUAL TO 25398 THEN OPERATION IS IFMTH.
C       IF NOT EQUAL TO ANY OF THE ABOVE, IT IS AN OFN OPERATION.
C
C%.R    645     IBM
60      CALL IFMTH(ITEMP0)
        GOTO 40
70      CALL IFMTL(ITEMP0)
C%.E    645     IBM
        GOTO 40
C
C====== DEPOSIT VALUE IN SWITCH REGISTER INTO CONTROL REGISTER
C
80      CALL ISWR(ITEMP0)
        CALL OCTRL(ITEMP0)
        GOTO 40
C
C====== DEPOSIT VALUES IN SWITCH REGISTER ACCORDING TO FUNCTION SETTING
C
90      CALL OFN(ITEMP0)
        GOTO 40
C
C====== GET NUMBER OF PANEL OPERATION TO GET ANSWER BACK
C
100     IT20=IT20+1
        ITEMP1=-ITBL(IT20)
C
C====== SEE IF NUMBER IS ZERO
C
        IF(ITEMP1 .EQ. 0)GOTO 190
C
C====== SET ANSWER POINT FOR ANSWERS
C
        IT30=4
110     IT20=IT20+1
        ITEMP0=ITBL(IT20)
C
C====== SEE WHAT OPERATIONS ARE REQUIRED
C
        IF(ITEMP0-25142)180,120,140
C
C====== IF EQUAL TO 25142 DO AND OAPMA OPERATION
C
120     CALL OAPMA(ITEMP0)
130     CONTINUE
C>
          IT30=IT30-1
C
C====== PUT ANSWER INTO ARRAY IHANS
C
        IHANS(IT30)=ITEMP0
        ITEMP1=ITEMP1+1
C
C====== SEE IF THROUGH WITH PANEL OPERATIONS
C
        IF(ITEMP1 .EQ. 0)GOTO 190
        GOTO 110
140     IF(ITEMP0-25910)150,160,170
C
C====== SEE IF AN IFMTH OPERATION
C
C%.R    704     IBM
150     CALL IFMTH(ITEMP0)
C%.E    704     IBM
C
C====== SEE IF AN IFMTL OPERATION
C
        GOTO 130
C%.R    711     IBM
160     CALL IFMTL(ITEMP0)
C%.E    711     IBM
        GOTO 130
C
C====== SEE IF A CONTROL REGISTER PANEL OPERATION
C
170     CALL OCTRL(ITEMP0)
        GOTO 130
C
C====== SEE IF AN EXAM OF SOME LOCATION IN AP
C
180     IZ00=ITEMP0
        CALL RREG(ITEMP0,IZ00)
        GOTO 130
C
C====== SEE IF TYPE OUT DISABLED
C       IF DISABLED DO NOT CHECK ANSWERS
C
190     IF(TYPDIS .EQ. 1 )GOTO 390
C
C====== MASK IHANS WITH MASK
C
        IHANS(3)=IAND16(INOT16(IOTMSK),IHANS(3))
        ITEMP0=IHANS(1)
        ITEMP1=IHANS(2)
C
C====== CHECK OUTPUT FORMAT
C
        ITEMP3=IOTFMT
        IF(IOTFMT .NE. 0)GOTO 200
C
C====== GET FOUR BITS FROM TEMP0 AND MAKE HIGH FOUR BITS OF TEMP1
C
        ITEMP1=ILSH16(ITEMP1,4)
        CALL IDRS(ITEMP0,ITEMP1,4)
C
C====== CHECK FORMAT AGAIN
C
200     IF(ITEMP3 .LE. 1)GOTO 250
        IHANS(1)=ITEMP0
        ITEMP0=ITEMP1
        ITEMP1=IHANS(3)
C
C====== SEE IF FORMAT GREATER THAN 128
C
        IF(IAND16(ITEMP3,128) .EQ. 0)GOTO 220
        ITEMP3=ITEMP3-256
C
C====== ITEMP3 IS SHIFT COUNTER
C
210     ITEMP1=IRSH16(ITEMP1,1)
        ITEMP3=ITEMP3+1
C
C====== SEE IF DONE SHIFTING
C
        IF(ITEMP3 .EQ. 0)GOTO 240
        GOTO 210
C
C====== TEMP3 IS SHIFT COUNT
C
220     ITEMP3=-ITEMP3
230     CALL IDLS(ITEMP0,ITEMP1,1)
        ITEMP3=ITEMP3+1
C
C====== SEE IF DONE
C
        IF(ITEMP3 .EQ. 0)GOTO 240
        GOTO 230
C
C====== PUT AWAY ANSWERS
C
240     IHANS(2)=ITEMP0
        IHANS(3)=ITEMP1
        GOTO 260
250     IHANS(1)=ITEMP0
        IHANS(2)=ITEMP1
260     ITEMP1=IRSH16(IOTCNT,4)
        IACNT=ITEMP1
        IT30=3
        IT31=INLOC+3
C
C====== COMPARE RESULT( IHANS ) VS EXPECTED ( ITBL )
C
        ITXX=IP16(ITBL(IT31))
        IF(IHANS(IT30) - ITXX)300,270,300
C
C====== SEE IF MORE THAN ONE WORD EXPECTED
C
270     ITEMP1=-ITEMP1
C
C====== IF ONE WORD EXPECTED GO CHECK THAT TOO
C
        IF(ITEMP1 .EQ. 0)GOTO 390
280     IT30=IT30-1
        IT31=IT31-1
C
C====== EXPECTED VS ACTUAL
C
        ITXX=IP16(ITBL(IT31))
        IF(IHANS(IT30) - ITXX)300,290,300
290     ITEMP1=ITEMP1+1
C
C====== SEE IF DONE
C
        IF(ITEMP1)280,390,280
C
C====== AN ERROR HAS OCCURRED-PRINT OUT ACTUAL EXPECTED AND TEST CODE
C
300     IERRS=IERRS+1
        ERRCNT=ERRCNT+1
C
        ITEMP2=INLOC+3-IACNT
C
C====== GET FIRST WORD TO BE WRITTEN OUT
C
        CALL I2ASCI(6,ITBL(ITEMP2),INO2,IRADX,0)
        ITEMP0=-IACNT
C
C====== SEE HOW MANY MORE WORDS ARE TO BE WRITTEN OUT
C
        IF(ITEMP0 .EQ. 0)GOTO 330
        N=-ITEMP0
        I=0
310     ITEMP1=ITBL(ITEMP2+1)
        I=I+1
C
C====== GET SECOND AND THIRD WORD IF NECC.
C
        CALL I2ASCI(6,ITEMP1,INO3(1,I),IRADX,0)
        ITEMP2=ITEMP2+1
        ITEMP0=ITEMP0+1
C
C====== SEE IF ALL WORDS ARE WRITTEN
C
        IF(ITEMP0 .EQ. 0)GOTO 320
        GOTO 310
320     IXXX=IT20+1
        WRITE(ITTO,2000)ITBL(IXXX),INO2,((INO3(J,I),J=1,6),I=1,N)
        IF(IECHO.NE.0) WRITE(LFIL,2000)ITBL(IXXX),INO2,
     +                 ((INO3(J,I),J=1,6),I=1,N)
        GOTO 340
330     IXXX=IT20+1
        WRITE(ITTO,2000) ITBL(IXXX),INO2
        IF(IECHO.NE.0) WRITE(LFIL,2000) ITBL(IXXX),INO2
340       ITEMP2=3-IACNT
C
C====== NOW GET THE ACTUAL NUMBER FROM AP
C
        ITEMP1=IHANS(ITEMP2)
C
C====== FIRST PART
C
        CALL I2ASCI(6,ITEMP1,INO1,IRADX,0)
C
C====== SEE IF ONLY ONE WORD EXPECTED
C
        IF(-IACNT .EQ. 0)GOTO 380
        ITEMP0=IACNT
        N=ITEMP0
        I=0
350     ITEMP1=IHANS(ITEMP2+1)
C
C====== GET SECOND AND THIRD WORDS IF NECC.
C
        I=I+1
        CALL I2ASCI(6,ITEMP1,INO3(1,I),IRADX,0)
        ITEMP2=ITEMP2+1
        ITEMP0=ITEMP0-1
C
C====== SEE IF NO MORE WORDS ARE NEEDED
C
        IF(ITEMP0 .EQ. 0)GOTO 360
        GOTO 350
360     WRITE(ITTO,2010) INO1,((INO3(J,I),J=1,6),I=1,N)
        IF(IECHO.NE.0) WRITE(LFIL,2010) INO1,((INO3(J,I),
     +                 J=1,6),I=1,N)
370     WRITE(ITTO,2020) ISLOC
        IF(IECHO.NE.0) WRITE(LFIL,2020) ISLOC
C
        GO TO 390
C
380     WRITE(ITTO,2010) INO1
        IF(IECHO.NE.0) WRITE(LFIL,2010) INO1
        GO TO 370
C
C====== SEE IF LOOPING IS DESIRED
C
390     CALL LPTST(IRTURN)
C
        IF(IRTURN .EQ. 1)GOTO 10
        RETURN
C
C====== FORMAT STATEMENTS
C
2000    FORMAT(12H ERROR CODE=,I4,/,5X,3H E ,6A1,
     +  10(3X,6A1))
2010    FORMAT(5X,3H A ,10(6A1,3X))
2020    FORMAT(13H J-TABLE LOC=,I4,//)
C
        END
C****** SVAL = DETERMINE # OF BITS TO SEND                  = REL 5.0  , NOV 79
        SUBROUTINE SVAL
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PTH100>SVAL                                              *
C  *    DATE        : NOV  5,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : PARAMETERS USED IN THIS ROUTINE ARE PASSED               *
C  *                  IN COMMON BLOCKS.                                        *
C  *                  INCNT - NUMBER OF BITS TO SET FOR A DATA WORD            *
C  *                  INFMT - TRANSFER FORMAT                                  *
C  *                  INMSK - MASKING VALUE USED BEFORE TRANSFER               *
C  *                                                                           *
C  *    EXIT        : WHEN DATA HAS BEEN CREATED, IT IS LOADED INTO THE        *
C  *                  APPROPRIATE TABLE LOCATIONS.                             *
C  *                                                                           *
C  *    FUNCTION    : SVAL IS USED TO CREATE THE "ALL ONES" DATA PATTERNS      *
C  *                  FOR THE DIFFERENT PATH TESTS IN THE AP.  THIS            *
C  *                  ROUTINE HAS THE CAPABILITY TO ALTER THE NUMBER OF        *
C  *                  ONES LOADED INTO THE DATA WORD (DEPENDING ON THE         *
C  *                  WIDTH OF THE DATA PATH UNDER TEST).                      *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER ITBL,IT20,IT21,IT30,IT31,IHANS,IVTMP,INCNT,INLOC,ITBLOC,
     +  ICLOC,IOTMSK,IOTCNT,IOTFMT,INFMT,ISLOC,INMSK,ITTI,ITTO,LFIL,
     +  RJUSFY,ISTAT,IRTURN,ICHNGE,WATERR,LOOPER,TYPDIS,IORST,HALT,
     +  FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,IERLIM,IPSLIM,ERRCNT,PASCNT
        INTEGER
     +  IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ,PSIZE,BASLOW,MDSIZ,IAPNUM,
     +  IECHO,IRADX,IERRS,IMSIZ,IPSIZE,NPAGES,ITEMP0,ITEMP1,ITEMP2
C
        REAL RMDSIZ
C
        COMMON /TABLE/  ITBL(1561),IT20,IT21,IT30,IT31,IHANS(4),
     1  IVTMP(4)
C
        COMMON /APPTH/  INCNT,INLOC,ITBLOC,ICLOC,IOTMSK,IOTCNT,IOTFMT,
     1  INFMT,ISLOC,INMSK
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
C
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),  IOP1) , (ISTAT( 7),  IOP2) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IERLIM) , (ISTAT(10),IPSLIM) , (ISTAT(11),ERRCNT)
        EQUIVALENCE
     +  (ISTAT(12),PASCNT) , (ISTAT(13), IERRS) , (ISTAT(14),TESTFG),
     +  (ISTAT(15),  TM4K) , (ISTAT(16),  IOP3) , (ISTAT(17),IWRTST),
     +  (ISTAT(18), ISTEP)
C.
C..
C...
C
C====== SET ALL BITS ON UP TO INCNT
C
        ITEMP2=1
        ITEMP1=0
        ITEMP0=0
        CALL ITLS(ITEMP0,ITEMP1,ITEMP2,INCNT)
C
C====== SEE IF EQUAL TO ZERO - IF NOT SAVE
C
        IF(ITEMP0 .NE. 0)ITBL(INLOC+1)=ITEMP0
        IF(ITEMP1 .NE. 0)ITBL(INLOC+2)=ITEMP1
C
C====== SAVE AND MASK;EXPONENT
C
        ITBL(INLOC+3)=IAND16(ITEMP2,INOT16(INMSK))
C
C====== SEE WHAT FORMAT IS DESIRED
C
        IF(INFMT-1)20,30,10
C
C====== INFMT=2SET = 2
C
10      ITBL(INLOC+3)=2
C
C====== FORMAT = 1
C
20      RETURN
C
C====== FORMAT = 0 NORMALIZE
C
30      ITBL(INLOC+2)=INOT16(1025)
        RETURN
        END
C****** CVAL = CLEAR TABLE VALUE                            = REL 5.0  , NOV 79
        SUBROUTINE CVAL
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PTH100>CVAL                                              *
C  *    DATE        : NOV  5,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : PARAMETERS USED IN CVAL ARE PASSED IN COMMONS.           *
C  *                  INCNT - NUMBER OF BITS TO BE CLEARED IN DATA WORD        *
C  *                  INLOC - LOCATION OF INPUT ARGUEMENT LIST IN ITBL.        *
C  *                                                                           *
C  *    EXIT        : DATA IN ITBL IS ALTERED IN THE APPROPRIATE LOCATIONS.    *
C  *                                                                           *
C  *    FUNCTION    : THE DATA CREATED IN CVAL IS USED FOR THE "ALL ZERO"      *
C  *                  DATA USED IN THE DATA PATH TESTS.  THE REQUIRED          *
C  *                  NUMBER OF WORDS TO BE CLEARED DEPENDS ON THE WIDTH       *
C  *                  OF THE DATA PATH.                                        *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER INCNT,INLOC,ITBLOC,ICLOC,IOTMSK,IOTCNT,IOTFMT,INFMT,
     +  ISLOC,INMSK,ITTI,ITTO,LFIL,ISTAT,IRTURN,ICHNGE,WATERR,
     +  LOOPER,TYPDIS,IORST,HALT,FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,
     +  IERLIM,IPSLIM,ERRCNT,PASCNT,IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ
        INTEGER
     +  PSIZE,BASLOW,MDSIZ,IAPNUM,IECHO,IERRS,IMSIZ,IPSIZE,NPAGES,
     +  ITBL,IT20,IT21,IT30,IT31,IHANS,IVTMP,ITEMP2,IWCT,ITEMP1
C
        REAL RMDSIZ
C
        COMMON /APPTH/  INCNT,INLOC,ITBLOC,ICLOC,IOTMSK,IOTCNT,IOTFMT,
     1  INFMT,ISLOC,INMSK
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /TABLE/  ITBL(1561),IT20,IT21,IT30,IT31,IHANS(4),
     1  IVTMP(4)
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
C
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),  IOP1) , (ISTAT( 7),  IOP2) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IERLIM) , (ISTAT(10),IPSLIM) , (ISTAT(11),ERRCNT)
        EQUIVALENCE
     +  (ISTAT(12),PASCNT) , (ISTAT(13), IERRS) , (ISTAT(14),TESTFG),
     +  (ISTAT(15),  TM4K) , (ISTAT(16),  IOP3) , (ISTAT(17),IWRTST),
     +  (ISTAT(18), ISTEP)
C.
C..
C...
        ITEMP2=INLOC
C
C====== SAVE TABLE FROM LOCATION INLOC TO INLOC + 3
C
        CALL SAVE(INLOC)
C
C====== GET NUMBER OF WORDS THAT ARE TO BE SENT TO AP
C
        IWCT=INCNT/16
C
C====== CLEAR ONE WORD AT LEAST
C
        ITBL(INLOC+3)=0
        ITEMP1=-IWCT
C
C====== SEE IF DONE CLEARING VALUES
C
10      IF(ITEMP1 .EQ. 0)RETURN
C
C====== CLEAR NEXT ONE OR TWO WORDS IF NECCESARY
C
        ITBL(ITEMP2+2)=0
        ITEMP2=ITEMP2-1
        ITEMP1=ITEMP1+1
        GOTO 10
        END
C****** DPAT = DRIVES PTH100 FROM TABLE                     = REL 5.0  , NOV 79
        SUBROUTINE DPAT(ITLOC)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PTH100>DPAT                                              *
C  *    DATE        : NOV  5,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : ITLOC => POINTER FOR THE TABLE (ITBL).                   *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : DPAT SETS UP THE AP BEFORE THE MICRO-CODE IS SENT.       *
C  *                  THESE PARAMETERS ARE TAKEN FROM THE TABLE (ITBL).        *
C  *                  THE FOLLOWING PARAMETERS ARE TAKEN FROM THE TABLE        *
C  *                  IN DPAT:                                                 *
C  *                  1)  LOCATIONS TO LOAD THE AP MICRO-CODE IN THE TABLE     *
C  *                      FOR EACH DATA PATH TEST.                             *
C  *                  2)  THE NUMBER OF PANEL DEPOSIT SETUPS                   *
C  *                  3)  THE NUMBER OF AP MICRO-CODE INSTRUCTIONS             *
C  *                  4)  THE NUMBER OF INPUT ARGUEMENTS                       *
C  *                  5)  THE LOCATION OF THE INPUT ARGUEMENTS IN THE TABLE    *
C  *                  6)  THE BIT WIDTH OF THE INPUT ARGUEMENT                 *
C  *                  7)  MASKING VALUES                                       *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER ITLOC,ITBL,IT20,IT21,IT30,IT31,IHANS,IVTMP,INCNT,INLOC,
     +  ITBLOC,ICLOC,IOTMSK,IOTCNT,IOTFMT,INFMT,ISLOC,INMSK,ITTI,ITTO,
     +  LFIL,ISTAT,IRTURN,ICHNGE,WATERR,LOOPER,TYPDIS,IORST,
     +  HALT,FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,IERLIM,IPSLIM,ERRCNT
        INTEGER
     +  PASCNT,IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ,PSIZE,BASLOW,MDSIZ,
     +  IAPNUM,IECHO,IRADX,IERRS,IMSIZ,IPSIZE,NPAGES,ITEMP0,ITEMP1
        INTEGER ITEMP2,ITEMP3,NVALI
C
        REAL RMDSIZ
C
        COMMON /TABLE/  ITBL(1561),IT20,IT21,IT30,IT31,IHANS(4),
     1  IVTMP(4)
C
        COMMON /APPTH/  INCNT,INLOC,ITBLOC,ICLOC,IOTMSK,IOTCNT,IOTFMT,
     1  INFMT,ISLOC,INMSK
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
C
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),  IOP1) , (ISTAT( 7),  IOP2) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IERLIM) , (ISTAT(10),IPSLIM) , (ISTAT(11),ERRCNT)
        EQUIVALENCE
     +  (ISTAT(12),PASCNT) , (ISTAT(13), IERRS) , (ISTAT(14),TESTFG),
     +  (ISTAT(15),  TM4K) , (ISTAT(16),  IOP3) , (ISTAT(17),IWRTST),
     +  (ISTAT(18), ISTEP)
C.
C..
C...
C
C====== GET POINTER FROM PARAMETER
C
        IT20=ITLOC
10      IT20=IT20+1
        ISLOC=IT20+1
        ITEMP0=ITBL(IT20)
        IF(ITEMP0 .EQ. 0)RETURN
C
C====== ITBLOC=INDEX TO TABLE OF UCODE TO BE EXECUTED.
C       UCODE WILL BE LOADED INTO PS VIA PNLLD
C
        ITBLOC=ITEMP0
        ITEMP0=ITEMP0+1
        IF(ITEMP0 .NE. 0)GOTO 20
        IT20=IT20+1
        IT20=ITBL(IT20)
        GOTO 10
20      IT20=IT20+1
        ITEMP1=-ITBL(IT20)
C
C====== ITEMP1=NUMBER OF SET UP PANEL OPERATIONS
C
        IF(ITEMP1 .EQ. 0)GOTO 50
30      IT20=IT20+1
        ITEMP0=ITBL(IT20)
        CALL OSWR(ITEMP0)
C>
        IT20=IT20+1
        ITEMP0=ITBL(IT20)
        IF(ITEMP0-25142)40,100,120
40      CALL OFN(ITEMP0)
        GOTO 110
50      IT20=IT20+1
        ITEMP1=-ITBL(IT20)
C
C====== NUMBER OF VARIABLE APA INSTRUCTIONS TO BE LOADED INTO PS
C
        IF(ITEMP1 .EQ. 0)GOTO 80
        IT20=IT20+1
        IT21=ITBL(IT20)-1
C
C====== IT21 IS INDEX FOR PS VARIABLE INSTRUCTIONS TO BE STORED INTO
C       ITBL.
C
60      ITEMP3=-4
70      IT20=IT20+1
        IT21=IT21+1
        ITBL(IT21)=ITBL(IT20)
C
C====== STORE VARIABLE INSTRUCTIONS
C
        ITEMP3=ITEMP3+1
        IF(ITEMP3 .NE. 0)GOTO 70
        ITEMP1=ITEMP1+1
        IF(ITEMP1 .NE. 0)GOTO 60
80      IT20=IT20+1
        NVALI=ITBL(IT20)
C
C====== NUMBER OF PS INPUTS
C
        IF(NVALI .EQ. 0)GOTO 90
        IT20=IT20+1
        INLOC=ITBL(IT20)
C
C====== LOCATION FOR PS INPUT
C
        IT20=IT20+1
C
C====== BIT COUNT FOR INPUT
C
        INCNT=IAND16(IP16(ITBL(IT20)),255)-1
C
C====== INFMT IS FORMAT ON INPUT
C
        INFMT=IAND16(ISWAP(IP16(ITBL(IT20))),255)
        IT20=IT20+1
C
C====== INPUT MASK
C
        INMSK=IP16(ITBL(IT20))
        IT20=IT20+1
C
C====== BIT ON OUTPUT
C
        IOTCNT=IAND16(IP16(ITBL(IT20)),255)-1
C
C====== FORMAT ON OUTPUT
C
        IOTFMT=IAND16(ISWAP(IP16(ITBL(IT20))),255)
        IT20=IT20+1
        IOTMSK=IP16(ITBL(IT20))
C
C====== INCNT=NUMBER OF BITS FOR INPUT ARG.
C       INMSK=MASK FOR LOW 16 BITS
C       INFMT= 0 UNPACKED, 1=PACKED, N=FOR LEFT SHIFT OF HARD. ANSWER
C       IOTCNT,IOTFMT,IOTMSK FOR OUTPUT ARG
C
90      ICLOC=IT20
C
C====== SAVE TABLE POINTER
C
        ITEMP0=NVALI
        IF(ITEMP0 .EQ. 0)GOTO 160
C
C====== SET HAS TAKEN PLACE NOW GO RUNIT
C
        CALL BCHCK(IT20)
C>
        IT20=IT20+1
        GOTO 10
C
C====== DO PANEL OPERS IF NECESSARY
C
100     CALL ISWR(ITEMP0)
        CALL OAPMA(ITEMP0)
110     CONTINUE
C>
        ITEMP1=ITEMP1+1
        IF(ITEMP1 .EQ. 0)GOTO 50
        GOTO 30
120     IF(ITEMP0-25910)130,140,150
C%.R    1307    IBM
130     CALL IFMTH(ITEMP0)
        GOTO 110
140     CALL IFMTL(ITEMP0)
C%.E    1370    IBM
        GOTO 110
150     CALL ISWR(ITEMP0)
        CALL OCTRL(ITEMP0)
        GOTO 110
C
C====== TMTEST IF NECCARY ON SOME TESTS
C
160     CALL TMTST(IT20)
        IT20=IT20+1
        GOTO 10
        END
C****** TMTST = TABLE MEMORY TEST                           = REL 5.0  , NOV 79
        SUBROUTINE TMTST(ITEMP2)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PTH100>TMTST                                             *
C  *    DATE        : NOV  5,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : N/A                                                      *
C  *                                                                           *
C  *    EXIT        : ITEMP2 <= TABLE POINTER                                  *
C  *                                                                           *
C  *    FUNCTION    : TMTST TESTS TABLE MEMORY ROM.  THE TABLE MEMORY          *
C  *                  ADDRESS IS SET TO ZERO, THE TABLE POINTER IS             *
C  *                  INITIALIZED AT ZERO, AND THEN TABLE MEMORY               *
C  *                  VALUES ARE COMPARED TO THE VALUES IN THE                 *
C  *                  PTH100 TABLE.  A SLIDING BIT IS USED TO ADDRESS          *
C  *                  THE TABLE MEMORY LOCATION TO BE CHECKED, THERE-          *
C  *                  FOR THE FOLLOWING TABLE MEMORY LOCATIONS ARE             *
C  *                  CHECKED: 0, 1, 2, 4, 8, 16, 32, 64, 128, 256,            *
C  *                  512, AND 1024.                                           *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER ITEMP2,INCNT,INLOC,ITBLOC,ICLOC,IOTMSK,IOTCNT,IOTFMT,
     +  INFMT,ISLOC,INMSK,ITTI,ITTO,LFIL,ISTAT,IRTURN,ICHNGE,
     +  WATERR,LOOPER,TYPDIS,IORST,HALT,FFTERR,DMPSIM,WRTST,FFTBAS,
     +  PSSFLG,IERLIM,IPSLIM,ERRCNT,PASCNT,IPAGE,TESTFG,IXN,IYN,SIMULS
        INTEGER
     +  FFTSIZ,PSIZE,BASLOW,MDSIZ,IAPNUM,IECHO,IERRS,IMSIZ,IPSIZE,
     +  NPAGES,ITBL,IT20,IT21,IT30,IT31,IMMM,IDTMA,ITMCNT,ITMA,ITEMP1
C
        REAL RMDSIZ
C
        COMMON /APPTH/  INCNT,INLOC,ITBLOC,ICLOC,IOTMSK,IOTCNT,IOTFMT,
     1  INFMT,ISLOC,INMSK
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /TABLE/  ITBL(1561),IT20,IT21,IT30,IT31,IMMM(8)
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
C
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),  IOP1) , (ISTAT( 7),  IOP2) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IERLIM) , (ISTAT(10),IPSLIM) , (ISTAT(11),ERRCNT)
        EQUIVALENCE
     +  (ISTAT(12),PASCNT) , (ISTAT(13), IERRS) , (ISTAT(14),TESTFG),
     +  (ISTAT(15),  TM4K) , (ISTAT(16),  IOP3) , (ISTAT(17),IWRTST),
     +  (ISTAT(18), ISTEP)
C.
C..
C...
        ITMCNT=0
        ITMA=0
C
C====== SET TMA TO ITMA
C
10      CALL OSWR(ITMA)
        CALL OFN(515)
        CALL APOUT(1039,2)
C
C>
C====== PUT ITMCNT IN TABLE(DATA)AREA
C
        INLOC=3*ITMCNT
C
C====== RUNIT
C
        CALL RUNIT
C
C====== COMPUTE NEXT ADDRESS
C
        ITEMP1=ILSH16(1,ITMCNT)
        ITMCNT=ITMCNT+1
        ITMA=ITEMP1
        IF(2048-ITEMP1)10,30,10
30      ITEMP2=IT20
        ITEMP3=IT20+1
        IF(IWRTST.NE.0) WRITE(ITTO,40) ITBL(ITEMP3)
        IF(IWRTST.NE.0.AND.IECHO.NE.0) WRITE(LFIL,40) ITBL(ITEMP3)
40      FORMAT(11H TEST CODE ,I3,11H COMPLETED.)
        RETURN
        END
C****** RVAL = RESTORES PREVIOUS VALUES IN TABLE            = REL 5.0  , NOV 79
        SUBROUTINE RVAL
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PTH100>RVAL                                              *
C  *    DATE        : NOV  5,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : PARAMETERS ARE PASSED BY COMMON TO THIS ROUTINE.         *
C  *                  IVTMP - AN ARRAY WHICH CONTAINS THE VALUES TO BE         *
C  *                          RESTORED.                                        *
C  *                  INLOC - BEGINNING ITBL LOCATION TO BE CHANGED.           *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : RVAL RESTORED THE FOUR VALUES IN THE ARRAY IVTMP         *
C  *                  AND PLACES THESE VALUES IN THE PTH100 TABLE              *
C  *                  BEGINNING AT LOCATION INLOC.                             *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER ITBL,IT20,IT21,IT30,IT31,IHANS,IVTMP,INCNT,INLOC,ITBLOC,
     +  ICLOC,IOTMSK,IOTCNT,IOTFMT,INFMT,ISLOC,INMSK,ITTI,ITTO,LFIL,
     +  RJUSFY,ISTAT,IRTURN,ICHNGE,WATERR,LOOPER,TYPDIS,IORST,HALT,
     +  FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,IERLIM,IPSLIM,ERRCNT,PASCNT
        INTEGER
     +  IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ,PSIZE,BASLOW,MDSIZ,IAPNUM,
     +  IECHO,IRADX,IERRS,IMSIZ,IPSIZE,NPAGES,I,ITEMP2
C
        REAL RMDSIZ
C
        COMMON /TABLE/  ITBL(1561),IT20,IT21,IT30,IT31,IHANS(4),
     1  IVTMP(4)
C
        COMMON /APPTH/  INCNT,INLOC,ITBLOC,ICLOC,IOTMSK,IOTCNT,IOTFMT,
     1  INFMT,ISLOC,INMSK
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
C
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),  IOP1) , (ISTAT( 7),  IOP2) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IERLIM) , (ISTAT(10),IPSLIM) , (ISTAT(11),ERRCNT)
        EQUIVALENCE
     +  (ISTAT(12),PASCNT) , (ISTAT(13), IERRS) , (ISTAT(14),TESTFG),
     +  (ISTAT(15),  TM4K) , (ISTAT(16),  IOP3) , (ISTAT(17),IWRTST),
     +  (ISTAT(18), ISTEP)
C.
C..
C...
        ITEMP2=INLOC
C
C====== INLOC IS INDEX IN TABLE TO RESTORE
C
        DO 10 I=1,4
C
C====== VTMP HAS STORED VALUES TO BE RESTORED
C
        ITBL(ITEMP2)=IVTMP(I)
10      ITEMP2=ITEMP2+1
        RETURN
        END
C****** BCHCK = SET UP AND START THE AP                     = REL 5.0  , NOV 79
        SUBROUTINE BCHCK(ITEMP2)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PTH100>BCHCK                                             *
C  *    DATE        : NOV  5,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : N/A                                                      *
C  *                                                                           *
C  *    EXIT        : ITEMP2 <= TABLE POINTER                                  *
C  *                                                                           *
C  *    FUNCTION    :BCHCK CALLS THE FOLLOING ROUTINES TO SET DATA PATTERNS:   *
C  *                  CVAL - SETS UP THE TEST FOR ALL ZEROS                    *
C  *                  SVAL - SETS UP THE TEST FOR ALL ONES                     *
C  *                  BVAL - SET UP THE TEST FOR A SLIDING BIT                 *
C  *                  EACH OF THESE DATA PATTERNS ARE USED AS DATA             *
C  *                  WHEN THE AP MICRO-CODE IS EXECUTED (BY A CALL            *
C  *                  TO THE RUNIT SUBROUTINE).                                *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER INCNT,INLOC,ITBLOC,ICLOC,IOTMSK,IOTCNT,IOTFMT,INFMT,
     +  ISLOC,INMSK,ITTI,ITTO,LFIL,ISTAT,IRTURN,ICHNGE,WATERR,
     +  LOOPER,TYPDIS,IORST,HALT,FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,
     +  IERLIM,IPSLIM,ERRCNT,PASCNT,IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ
        INTEGER
     +  PSIZE,BASLOW,MDSIZ,IAPNUM,IECHO,IERRS,IMSIZ,IPSIZE,NPAGES,
     +  ITBL,IT20,IT21,IT30,IT31,IHANS,IVTMP,ITEMP1,ITEMP2,IBCNTR
C
        REAL RMDSIZ
C
        COMMON /APPTH/  INCNT,INLOC,ITBLOC,ICLOC,IOTMSK,IOTCNT,IOTFMT,
     1  INFMT,ISLOC,INMSK
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /TABLE/  ITBL(1561),IT20,IT21,IT30,IT31,IHANS(4),
     1  IVTMP(4)
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
C
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),  IOP1) , (ISTAT( 7),  IOP2) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IERLIM) , (ISTAT(10),IPSLIM) , (ISTAT(11),ERRCNT)
        EQUIVALENCE
     +  (ISTAT(12),PASCNT) , (ISTAT(13), IERRS) , (ISTAT(14),TESTFG),
     +  (ISTAT(15),  TM4K) , (ISTAT(16),  IOP3) , (ISTAT(17),IWRTST),
     +  (ISTAT(18), ISTEP)
C.
C..
C...
C
C====== RUN AP WITH DATA ZERO
C
        CALL CVAL
        CALL RUNIT
C
C====== RUN IT WITH ALL BITS
C
        CALL SVAL
        CALL RUNIT
C
C====== NOW RUN IT WITH SLIDING BIT
C
        IBCNTR= INCNT+1
        ITEMP1=0
10      CALL RVAL
C
C====== SET WHICH BIT IS ON
C
        CALL BVAL(ITEMP1)
        CALL RUNIT
        ITEMP1=ITEMP1+1
        IBCNTR=IBCNTR-1
        IF(IBCNTR .NE. 0)GOTO 10
C
C====== IF DONE RESTORE VALUE
C
        CALL RVAL
        ITEMP2=IT20
        ITEMP3=IT20+1
        IF(IWRTST.NE.0) WRITE(ITTO,20) ITBL(ITEMP3)
        IF(IWRTST.NE.0.AND.IECHO.NE.0) WRITE(LFIL,20) ITBL(ITEMP3)
20      FORMAT(11H TEST CODE ,I3,11H COMPLETED.)
        RETURN
        END
C****** BVAL = SET A DATA BIT                               = REL 5.0  , NOV 79
        SUBROUTINE BVAL(IT1)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PTH100>BVAL                                              *
C  *    DATE        : NOV  5,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : ITI => VARIABLE THAT DETERMINES WHICH BIT IS TO BE       *
C  *                         SET IN THE DATA WORD                              *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : BVAL IS USED TO CREATE DATA FOR DATA PATH TESTS.         *
C  *                  BVAL IS USED FOR THE SLIDING BIT PATTERNS BECAUSE        *
C  *                  IT SETS A BIT IN A DATA WORD DEPENDING ON THE INPUT      *
C  *                  PARAMETER, ITI.                                          *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER INCNT,INLOC,ITBLOC,ICLOC,IOTMSK,IOTCNT,IOTFMT,INFMT,
     +  ISLOC,INMSK,ITTI,ITTO,LFIL,ISTAT,IRTURN,ICHNGE,WATERR,
     +  LOOPER,TYPDIS,IORST,HALT,FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,
     +  IERLIM,IPSLIM,ERRCNT,PASCNT,IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ
        INTEGER
     +  PSIZE,BASLOW,MDSIZ,IAPNUM,IECHO,IERRS,IMSIZ,IPSIZE,NPAGES,
     +  ITBL,IT20,IT21,IT30,IT31,IHANS,IVTMP,IT1,ITEMP2,ITEMP0,ITEMP1
        INTEGER ITEMP3,ITEMP
C
        REAL RMDSIZ
C
        COMMON /APPTH/  INCNT,INLOC,ITBLOC,ICLOC,IOTMSK,IOTCNT,IOTFMT,
     1  INFMT,ISLOC,INMSK
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /TABLE/  ITBL(1561),IT20,IT21,IT30,IT31,IHANS(4),
     1  IVTMP(4)
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
C
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),  IOP1) , (ISTAT( 7),  IOP2) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IERLIM) , (ISTAT(10),IPSLIM) , (ISTAT(11),ERRCNT)
        EQUIVALENCE
     +  (ISTAT(12),PASCNT) , (ISTAT(13), IERRS) , (ISTAT(14),TESTFG),
     +  (ISTAT(15),  TM4K) , (ISTAT(16),  IOP3) , (ISTAT(17),IWRTST),
     +  (ISTAT(18), ISTEP)
C.
C..
C...
C
C====== INLOC HAS VALUE OF INDEX IN TABLE
C
        ITEMP2=INLOC
        ITEMP0=-IAND16(15,IT1)
        ITEMP1=IRSH16(IT1,4)
        ITEMP3=1
        IF(ITEMP0 .EQ. 0)GOTO 20
10      ITEMP3=ILSH16(ITEMP3,1)
        ITEMP0=ITEMP0+1
        IF(ITEMP0 .NE. 0)GOTO 10
20      ITEMP0=INOT16(INMSK)
        IF(ITEMP1 .NE. 0)GOTO 30
        ITEMP3=IAND16(ITEMP3,ITEMP0)
30      ITEMP2=ITEMP2+3
        ITEMP2=ITEMP2-ITEMP1
        ITEMP0=ITBL(ITEMP2)
        ITEMP3=ITEMP3-IAND16(ITEMP3,ITEMP0)+ITEMP0
        ITBL(ITEMP2)=ITEMP3
        ITEMP2=INLOC
        IF(INFMT .EQ. 1)GOTO 40
        RETURN
40      ITEMP=IAND16(ITBL(ITEMP2+2),2048)
        IF(ITEMP .EQ. 0)RETURN
        ITBL(ITEMP2+2)=IAND16(INOT16(1025),ITBL(ITEMP2+2))
        RETURN
        END
C****** SAVE = SAVE DATA AREA OF TABLE                      = REL 5.0  , NOV 79
        SUBROUTINE SAVE(ITEMP2)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PTH100>SAVE                                              *
C  *    DATE        : NOV  5,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : ITEMP2 => POINTER FOR THE INITIAL ITBL LOCATION          *
C  *                            TO BE SAVED.                                   *
C  *                                                                           *
C  *    EXIT        : THE 4 ELEMENT ARRAY, IVTMP, IS LOADED WITH THE           *
C  *                  DATA TO BE SAVED.                                        *
C  *                                                                           *
C  *    FUNCTION    : SAVE PLACES FOUR VALUES FROM ITBL INTO THE 4             *
C  *                  ELEMENT ARRAY, IVTMP.  THE INPUT PARAMETER,              *
C  *                  ITEMP2, DETERMINES THE BEGINNING ARRAY LOCATION          *
C  *                  FOR ITBL.                                                *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER ITEMP2,ITBL,IT20,IT21,IT30,IT31,IHANS,IVTMP
C
        COMMON /TABLE/  ITBL(1561),IT20,IT21,IT30,IT31,IHANS(4),
     1  IVTMP(4)
C
C.
C..
C...
C
        IVTMP(1)=ITBL(ITEMP2)
        IVTMP(2)=ITBL(ITEMP2+1)
        IVTMP(3)=ITBL(ITEMP2+2)
        IVTMP(4)=ITBL(ITEMP2+3)
        RETURN
        END
C****** TMRAMS = SET UP TABLE TO TEST TMRAM                 = REL 5.0  , NOV 79
        SUBROUTINE TMRAMS(ITMADR)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PTH100>TMRAMS                                            *
C  *    DATE        : NOV  5,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : ITMADR => TABLE MEMORY SIZE                              *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    :   TMRAMS SETS THE THREE TABLE MEMORY RAM TESTS TO        *
C  *                  THE CORRECT TABLE MEMORY SIZE.  TMRAMS ALTERS TWO        *
C  *                  TABLE LOCATIONS IN ORDER TO REPLACE THE REGULAR          *
C  *                  TABLE MEMORY PATH TESTS (TEST CODE 89, 90, 91)           *
C  *                  WITH THE THREE TABLE MEMORY RAM TESTS (TEST CODE         *
C  *                  200, 201, 202).                                          *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER J,ITEMP
C
        COMMON /TABLE/  J(1561),ITEMP(12)
C
C.
C..
C...
C
C%.I    1716    IBM
        J(1341)=ITMADR
        J(1369)=ITMADR
        J(1417)=ITMADR
        J(1397)=INOT16(32767)
        J(889)=-1
        J(890)=1336
C
C====== THIS WILL RESUME AT TEST 92
C
        RETURN
        END
C****** APTHBK = PTH100 BLOCK DATA TABLE                    = REL 5.0  , NOV 79
        BLOCK DATA
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : PTH100>APTHBK                                            *
C  *    DATE        : NOV  5,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : N/A                                                      *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : PTH100 IS A TABLE DRIVEN TEST.  APTHBK CONTAINS          *
C  *                  THE DATA USED TO SET PARAMETERS, DATA FOR AP MICRO-      *
C  *                  CODE, AND DATA FOR CONTROLLING THE FLOW OF THE           *
C  *                  PROGRAM.                                                 *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER IX,IY,IZ,IS,IT,J,ITEMP,ITTI,ITTO,LFIL,ISTAT,
     +  IRTURN,ICHNGE,WATERR,LOOPER,TYPDIS,IORST,HALT,FFTERR,DMPSIM,
     +  WRTST,FFTBAS,PSSFLG,IERLIM,IPSLIM,ERRCNT,PASCNT,IPAGE,TESTFG,
     +  IXN,IYN,SIMULS,FFTSIZ,PSIZE,BASLOW,MDSIZ,IAPNUM,IECHO,IRADX
        INTEGER
     +  IERRS,IMSIZ,IPSIZE,NPAGES,STATBF,STATNM,STRAD,IDDPA,IDDPXE,
     +  IDDPXH,IDDPXL,IDDPYE,IDDPYH,IDDPYL,IDTMA,IDEP,ICONT,IEDPXE
        INTEGER IEDPXH,IEDPXL,IDEPSP,IDPSPA,IETMA,IEXAM,ISTART,IDPS0,
     +  IDPS1,IDPS2,IDPS3,IEXSP,IEDPYE,IEDPYH,IEDPYL
C
        REAL RMDSIZ
C
        COMMON /RTRAN/  IX(3),IY(3),IZ(3),IS,IT
C
        COMMON /TABLE/  J(1561),ITEMP(12)
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /APCOM/  IDDPA,IDDPXE,IDDPXH,IDDPXL,IDDPYE,IDDPYH,IDDPYL,
     1  IDTMA,IDEP,ICONT,IEDPXE,IEDPXH,IEDPXL,IDEPSP,IDPSPA,IETMA,IEXAM,
     2  ISTART,IDPS0,IDPS1,IDPS2,IDPS3,IEXSP,IEDPYE,IEDPYH,IEDPYL
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
C
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),  IOP1) , (ISTAT( 7),  IOP2) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IERLIM) , (ISTAT(10),IPSLIM) , (ISTAT(11),ERRCNT)
        EQUIVALENCE
     +  (ISTAT(12),PASCNT) , (ISTAT(13), IERRS) , (ISTAT(14),TESTFG),
     +  (ISTAT(15),  TM4K) , (ISTAT(16),  IOP3) , (ISTAT(17),IWRTST),
     +  (ISTAT(18), ISTEP)
C.
C..
C...
C
        DATA IX(1),IX(2),IX(3),IY(1),IY(2),IY(3),IZ(1),IZ(2),IZ(3),
     +  IS,IT/0,0,0,0,0,0,0,0,0,253,256/
C
        DATA IDDPA,IDDPXE,IDDPXH,IDDPXL,IDDPYE,IDDPYH,IDDPYL,IDTMA,
     +  IDEP,ICONT,IEDPXE,IEDPXH,IEDPXL,IDEPSP,IDPSPA,IETMA,IEXAM,
     +  ISTART,IDPS0,IDPS1,IDPS2,IDPS3,IEXSP,IEDPYE,IEDPYH,IEDPYL/
     +  516,539,555,571,540,556,572,515,512,8192,1051,1067,1083,517,
     +  513,1027,1024,16384,520,536,552,760,1029,1052,1068,1084/
C
C
C
C
C                               T A B L E   J
C
C
C                       TABLE FORMAT
C
C       J(I)            = MICROCODE CONTROL BLOCK LOC IN TABLE J
C                       = IF -1 CONTINUE TEST AT LOC IN J(I+1)
C                       = IF 0 TERMINATE TABLE EXECUTION
C       I=I+1
C       J(I)            = NUMBER OF PANEL DEPOSITS
C       J(I+1)          = VALUE -> SWR
C       J(I+2)          = VALUE -> FN
C          .               .
C          .               .
C       I=I+(J(I)*2)+1
C       J(I)            = NUMBER OF MICROCODE INSTRUCTIONS TO LOAD TO
C                         MICROCODE CONTROL BLOCK
C       I=I+1
C       J(I)          = LOC IN CODE BLOCK FOR MICROCODE
C       J(I+1)          = PS WORD 0
C       J(I+2)          = PS WORD 1
C       J(I+3)          = PS WORD 2
C       J(I+4)          = PS WORD 3
C          .               .
C          .               .
C       I=I+(J(I-1)*4)+1
C       J(I)            = NUMBER OF PROGRAM SOURCE INPUT ARGUMENTS
C       I=I+1
C       J(I)            = SIZE OF INPUT ARGUMENT IN BITS
C       I=I+1
C       J(I)            = MASK FOR LOW 16 BITS
C       I=I+1
C       J(I)            = SIZE OF RESULT IN BITS ( LOW BYTE )
C                       = IF HIGH BYTE = 0 - RESULT UNPACKED
C                       = IF HIGH BYTE = 1 - RESULT PACKED
C                       = IF HIGH BYTE > 1 - VAR LEFT SHIFT HDWARE RESULT
C       I=I+1
C       J(I)            = MASK FOR LOW 16 BITS OF RESULT
C       I=I+1
C       J(I)            = NUMBER OF PANEL DEPOSITS FOR READ SETUP
C       J(I+1)          = VALUE -> SWR
C       J(I+2)          = VALUE -> FN
C          .               .
C          .               .
C       I=I+(J(I)*2)+1
C       J(I)            = NUMBER OF PANEL FUNCTION OPS TO READ RESULT
C       I=I+1
C       J(I)            = VALUE -> FN
C       J(I+1)            = VALUE -> FN
C          .               .
C          .               .
C       I=I+J(I-1)
C       J(I)            = ERROR CODE
C          .               .
C          .               .
C       I=I+1
C       J(I)            = CONTROL BLOCK NUMBER (BEGIN NEXT TEST)
C
C
C
C====== DATA EXPECTED IN TMROM LOC 0
C
        DATA J(   1)/      32/
        DATA J(   2)/    5120/
        DATA J(   3)/       0/
C
C====== DATA EXPECTED IN TMROM LOC 1
C
        DATA J(   4)/      32/
        DATA J(   5)/    2047/
        DATA J(   6)/     -39/
C
C====== DATA EXPECTED IN TMROM LOC 2
C
        DATA J(   7)/      32/
        DATA J(   8)/    2047/
        DATA J(   9)/    -158/
C
C====== DATA EXPECTED IN TMROM LOC 4
C
        DATA J(  10)/      32/
        DATA J(  11)/    2047/
        DATA J(  12)/    -632/
C
C====== DATA EXPECTED IN TMROM LOC 8
C
        DATA J(  13)/      32/
        DATA J(  14)/    2047/
        DATA J(  15)/   -2527/
C
C====== DATA EXPECTED IN TMROM LOC 16
C
        DATA J(  16)/      32/
        DATA J(  17)/    2047/
        DATA J(  18)/  -10106/
C
C====== DATA EXPECTED IN TMROM LOC 32
C
        DATA J(  19)/      32/
        DATA J(  20)/    2047/
        DATA J(  21)/   25112/
C
C====== DATA EXPECTED IN TMROM LOC 64
C
        DATA J(  22)/      32/
        DATA J(  23)/    2045/
        DATA J(  24)/  -30599/
C
C====== DATA EXPECTED IN TMROM LOC 128
C
        DATA J(  25)/      32/
        DATA J(  26)/    2038/
        DATA J(  27)/    9065/
C
C====== DATA EXPECTED IN TMROM LOC 256
C
        DATA J(  28)/      32/
        DATA J(  29)/    2008/
        DATA J(  30)/  -23052/
C
C====== DATA EXPECTED IN TMROM LOC 512
C
        DATA J(  31)/      32/
        DATA J(  32)/    1892/
        DATA J(  33)/    6900/
C
C====== DATA EXPECTED IN TMROM LOC 1024
C
        DATA J(  34)/      32/
        DATA J(  35)/    1448/
        DATA J(  36)/   10138/
C
C====== PTH100 CONTROL BLOCK 1 (FOUR WORDS)
C
C       WORD 1: MODE CONTROL FOR PNLLD SUBROUTINE
C
C               MODE = 0        LOAD CODE BLOCK
C               MODE = 1        SET PSA TO START ADDRESS (STEP MODE)
C               MODE = 2        SET PSA AND START MICROCODE
C
C       WORD 2: NUMBER OF LOCATIONS TO LOAD IN PROGRAM SOURCE
C
C       WORD 3: NOP
C
C       WORD 4: NOP
C
        DATA J(  37)/       0/
        DATA J(  38)/      12/
        DATA J(  39)/       0/
        DATA J(  40)/       0/
C
C====== PROGRAM SOURCE LOAD BLOCK
C
        DATA J(  41)/       0/
        DATA J(  42)/       0/
        DATA J(  43)/       0/
        DATA J(  44)/       0/
C
        DATA J(  45)/       0/
        DATA J(  46)/       0/
        DATA J(  47)/       0/
        DATA J(  48)/       0/
C
        DATA J(  49)/       0/
        DATA J(  50)/       0/
        DATA J(  51)/       0/
        DATA J(  52)/       0/
C
        DATA J(  53)/       0/
        DATA J(  54)/       0/
        DATA J(  55)/       0/
        DATA J(  56)/       0/
C
        DATA J(  57)/       0/
        DATA J(  58)/       0/
        DATA J(  59)/       0/
        DATA J(  60)/       0/
C
        DATA J(  61)/       0/
        DATA J(  62)/       0/
        DATA J(  63)/       0/
        DATA J(  64)/       0/
C
        DATA J(  65)/       0/
        DATA J(  66)/       0/
        DATA J(  67)/       0/
        DATA J(  68)/       0/
C
        DATA J(  69)/       0/
        DATA J(  70)/       0/
        DATA J(  71)/       0/
        DATA J(  72)/       0/
C
        DATA J(  73)/       3/
        DATA J(  74)/   -4096/
        DATA J(  75)/       0/
        DATA J(  76)/       0/
C
C====== DATA STORED IN PROGRAM SOURCE
C
        DATA J(  77)/       0/
        DATA J(  78)/       0/
        DATA J(  79)/       0/
        DATA J(  80)/       0/
C
        DATA J(  81)/       0/
        DATA J(  82)/      32/
        DATA J(  83)/    5120/
        DATA J(  84)/       0/
C
        DATA J(  85)/       0/
        DATA J(  86)/      32/
        DATA J(  87)/    5120/
        DATA J(  88)/       0/
C
C====== MODE CONTROL
C
        DATA J(  89)/       2/
        DATA J(  90)/       0/
        DATA J(  91)/       0/
        DATA J(  92)/       0/
C
        DATA J(  93)/       1/
        DATA J(  94)/       0/
        DATA J(  95)/       0/
        DATA J(  96)/       0/
C
C
C ******************************************************************************
C
C
C *                         T E S T  C O D E   64
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(  97)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J(  98)/       2/
C
        DATA J(  99)/       0/
        DATA J( 100)/     514/
C
        DATA J( 101)/       9/
        DATA J( 102)/     515/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 103)/       8/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 104)/      41/
C
C
C                     CLR 0.
C
        DATA J( 105)/     512/
        DATA J( 106)/       0/
        DATA J( 107)/       0/
        DATA J( 108)/       0/
C
C
C                     RPSLT; MI<DB; SETMA
C
        DATA J( 109)/    4816/
        DATA J( 110)/       0/
        DATA J( 111)/       0/
        DATA J( 112)/     240/
C
C
C                     NOP
C
        DATA J( 113)/       0/
        DATA J( 114)/       0/
        DATA J( 115)/       0/
        DATA J( 116)/       0/
C
C
C                     NOP
C
        DATA J( 117)/       0/
        DATA J( 118)/       0/
        DATA J( 119)/       0/
        DATA J( 120)/       0/
C
C
C                     NOP
C
        DATA J( 121)/       0/
        DATA J( 122)/       0/
        DATA J( 123)/       0/
        DATA J( 124)/       0/
C
C
C                     NOP
C
        DATA J( 125)/       0/
        DATA J( 126)/       0/
        DATA J( 127)/       0/
        DATA J( 128)/       0/
C
C
C                     NOP
C
        DATA J( 129)/       0/
        DATA J( 130)/       0/
        DATA J( 131)/       0/
        DATA J( 132)/       0/
C
C
C                     NOP
C
        DATA J( 133)/       0/
        DATA J( 134)/       0/
        DATA J( 135)/       0/
        DATA J( 136)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 137)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 138)/      75/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 139)/      32/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 140)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J( 141)/      32/
C
C====== MASK FOR RESULT
C
        DATA J( 142)/       0/
C
C====== NUMBER OF PANEL DEPOSITS FOR SETUP
C
        DATA J( 143)/       1/
C
        DATA J( 144)/       0/
        DATA J( 145)/     514/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 146)/       3/
C
        DATA J( 147)/    1085/
        DATA J( 148)/    1069/
        DATA J( 149)/    1053/
C
C====== ERROR CODE
C
        DATA J( 150)/      64/
C
C ******************************************************************************
C *                         T E S T  C O D E   65
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 151)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J( 152)/       2/
C
        DATA J( 153)/       0/
        DATA J( 154)/     514/
C
        DATA J( 155)/       9/
        DATA J( 156)/     515/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 157)/       1/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 158)/      45/
C
C
C                     RPSFT; MI<DB; SETMA
C
        DATA J( 159)/    4820/
        DATA J( 160)/       0/
        DATA J( 161)/       0/
        DATA J( 162)/     240/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 163)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 164)/      77/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 165)/      38/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 166)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J( 167)/      38/
C
C====== MASK FOR RESULT
C
        DATA J( 168)/       0/
C
C====== NUMBER OF PANEL DEPOSITS FOR SETUP
C
        DATA J( 169)/       1/
C
        DATA J( 170)/       0/
        DATA J( 171)/     514/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 172)/       3/
C
        DATA J( 173)/    1085/
        DATA J( 174)/    1069/
        DATA J( 175)/    1053/
C
C====== ERROR CODE
C
        DATA J( 176)/      65/
C
C ******************************************************************************
C *                         T E S T  C O D E   66
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 177)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J( 178)/       1/
C
        DATA J( 179)/       0/
        DATA J( 180)/     514/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 181)/       2/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 182)/      41/
C
C
C                     RPSLA 9.; DPX(-4.)<DB
C
        DATA J( 183)/    4800/
        DATA J( 184)/       0/
        DATA J( 185)/   16384/
        DATA J( 186)/       9/
C
C
C                     LPSLA 16.; DB=DPX(-4.)
C
        DATA J( 187)/    4832/
        DATA J( 188)/       0/
        DATA J( 189)/    1536/
        DATA J( 190)/      16/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 191)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 192)/      75/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 193)/      32/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 194)/       0/
C
C====== PACKED BIT WIDTH OF RESULT
C
        DATA J( 195)/     288/
C
C====== MASK FOR RESULT
C
        DATA J( 196)/       0/
C
C====== NUMBER OF PANEL DEPOSITS FOR SETUP
C
        DATA J( 197)/       1/
C
        DATA J( 198)/      16/
        DATA J( 199)/     515/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 200)/       2/
C
        DATA J( 201)/    1048/
        DATA J( 202)/    1032/
C
C====== ERROR CODE
C
        DATA J( 203)/      66/
C
C ******************************************************************************
C *                         T E S T  C O D E   67
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 204)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J( 205)/       1/
C
        DATA J( 206)/       0/
        DATA J( 207)/     514/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 208)/       2/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 209)/      41/
C
C
C                     RPSFA 9.; DPY(-2.)<DB
C
        DATA J( 210)/    4804/
        DATA J( 211)/       0/
        DATA J( 212)/    4098/
        DATA J( 213)/       9/
C
C
C                     LPSRA 16.; DB=DPY(-2.)
C
        DATA J( 214)/    4836/
        DATA J( 215)/       0/
        DATA J( 216)/    2064/
        DATA J( 217)/      16/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 218)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 219)/      77/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 220)/      32/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 221)/       0/
C
C====== PACKED BIT WIDTH OF RESULT
C
        DATA J( 222)/     288/
C
C====== MASK FOR RESULT
C
        DATA J( 223)/       0/
C
C====== NUMBER OF PANEL DEPOSITS FOR SETUP
C
        DATA J( 224)/       1/
C
        DATA J( 225)/      16/
        DATA J( 226)/     515/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 227)/       2/
C
        DATA J( 228)/    1080/
        DATA J( 229)/    1064/
C
C====== ERROR CODE
C
        DATA J( 230)/      67/
C
C ******************************************************************************
C *                         T E S T  C O D E   68
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 231)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J( 232)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 233)/       2/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 234)/      41/
C
C
C                     RPSFA 9.; DPX(-1.)<DB
C
        DATA J( 235)/    4804/
        DATA J( 236)/       0/
        DATA J( 237)/   16387/
        DATA J( 238)/       9/
C
C
C                     LDSPT 0.; DB=DPX(-1.)
C
        DATA J( 239)/     960/
        DATA J( 240)/       0/
        DATA J( 241)/    1728/
        DATA J( 242)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 243)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 244)/      76/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 245)/      10/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 246)/   -1017/
C
C====== LOW BYTE=BIT WIDTH OF RESULT - HIGH BYTE=NUMBER OF LEFT SHIFTS
C
        DATA J( 247)/     775/
C
C====== MASK FOR RESULT
C
        DATA J( 248)/    -128/
C
C====== NO SETUPS
C
        DATA J( 249)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 250)/       1/
C
        DATA J( 251)/    1029/
C
C====== ERROR CODE
C
        DATA J( 252)/      68/
C
C ******************************************************************************
C *                         T E S T  C O D E   69
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 253)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J( 254)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 255)/       2/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 256)/      41/
C
C
C                     RPSFA 9.; DPY(-1.)<DB
C
        DATA J( 257)/    4804/
        DATA J( 258)/       0/
        DATA J( 259)/    4099/
        DATA J( 260)/       9/
C
C
C                     LDSPE 15.; DB=DPY(-1.)
C
        DATA J( 261)/     892/
        DATA J( 262)/       0/
        DATA J( 263)/    2072/
        DATA J( 264)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 265)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 266)/      76/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 267)/      20/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 268)/    4095/
C
C====== LOW BYTE=BIT WIDTH OF RESULT - HIGH BYTE=NUMBER OF LEFT SHIFTS
C
        DATA J( 269)/    3081/
C
C====== MASK FOR RESULT
C
        DATA J( 270)/    -512/
C
C====== NO SETUPS
C
        DATA J( 271)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 272)/       1/
C
        DATA J( 273)/    1029/
C
C====== ERROR CODE
C
        DATA J( 274)/      69/
C
C ******************************************************************************
C *                         T E S T  C O D E   70
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 275)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J( 276)/       1/
C
C%.R    2117    PRIME MITRA
C%.R    2598    IBM
        DATA J( 277)/       0/
C%.E    2598    IBM
C%.E    2117    PRIME MITRA
        DATA J( 278)/     519/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 279)/       2/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 280)/      41/
C
C
C                     RPSFA 9.; OUT
C
        DATA J( 281)/    4807/
        DATA J( 282)/  -16384/
        DATA J( 283)/       0/
        DATA J( 284)/       9/
C
C
C                     IN; DB=INBS; DPX(-4.)<DB
C
        DATA J( 285)/       3/
        DATA J( 286)/  -14336/
        DATA J( 287)/   16896/
        DATA J( 288)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 289)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 290)/      77/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 291)/      16/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 292)/       0/
C
C====== PACKED BIT WIDTH OF RESULT
C
        DATA J( 293)/     272/
C
C====== MASK FOR RESULT
C
        DATA J( 294)/       0/
C
C====== NO SETUPS
C
        DATA J( 295)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 296)/       1/
C
        DATA J( 297)/    1083/
C
C====== ERROR CODE
C
C%.R    2176    PRIME MITRA
C%.R    2666    IBM
        DATA J( 298)/      70/
C%.E    2666    IBM
C%.E    2176    PRIME MITRA
C
C ******************************************************************************
C *                         T E S T  C O D E   71
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 299)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J( 300)/       1/
C
C%.R    2184    PRIME MITRA
C%.R    2685    IBM
        DATA J( 301)/       1/
C%.E    2685    IBM
C%.E    2184    PRIME MITRA
        DATA J( 302)/     519/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 303)/       2/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 304)/      41/
C
C
C                     RPSFA 9.; OUT
C
        DATA J( 305)/    4807/
        DATA J( 306)/  -16384/
        DATA J( 307)/       0/
        DATA J( 308)/       9/
C
C
C                     IN; DB=INBS; DPY(-4.)<DB
C
        DATA J( 309)/       3/
        DATA J( 310)/  -14336/
        DATA J( 311)/    4608/
        DATA J( 312)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 313)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 314)/      77/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 315)/      16/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 316)/       1/
C
C====== PACKED BIT WIDTH OF RESULT
C
        DATA J( 317)/     272/
C
C====== MASK FOR RESULT
C
        DATA J( 318)/       1/
C
C====== NO SETUPS
C
        DATA J( 319)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 320)/       1/
C
        DATA J( 321)/    1084/
C
C====== ERROR CODE
C
C%.R    2243    PRIME MITRA
C%.R    2753    IBM
        DATA J( 322)/      71/
C%.E    2753    IBM
C%.E    2243    PRIME MITRA
C
C ******************************************************************************
C *                         T E S T  C O D E   72
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 323)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J( 324)/       1/
C
        DATA J( 325)/       2/
        DATA J( 326)/     519/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 327)/       1/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 328)/      41/
C
C
C                     RPSFA 9.; OUT
C
        DATA J( 329)/    4807/
        DATA J( 330)/  -16384/
        DATA J( 331)/       0/
        DATA J( 332)/       9/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 333)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 334)/      77/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 335)/      16/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 336)/  -24831/
C
C====== PACKED BIT WIDTH OF RESULT
C
        DATA J( 337)/     272/
C
C====== MASK FOR RESULT
C
        DATA J( 338)/  -30976/
C
C====== NO SETUPS
C
        DATA J( 339)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 340)/       1/
C
        DATA J( 341)/    1084/
C
C====== ERROR CODE
C
        DATA J( 342)/      72/
C
C ******************************************************************************
C *                         T E S T  C O D E   73
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 343)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J( 344)/       1/
C
        DATA J( 345)/       3/
        DATA J( 346)/     519/
C
C
C====== NO MICROCODE
C
        DATA J( 347)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 348)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 349)/      77/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 350)/      16/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 351)/       0/
C
C====== PACKED BIT WIDTH OF RESULT
C
        DATA J( 352)/     272/
C
C====== MASK FOR RESULT
C
        DATA J( 353)/       0/
C
C====== NO SETUPS
C
        DATA J( 354)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 355)/       1/
C
        DATA J( 356)/    1084/
C
C====== ERROR CODE
C
        DATA J( 357)/      73/
C
C ******************************************************************************
C *                         T E S T  C O D E   74
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
C%.R    2824    DEC10
C%.R    2896    IBM
        DATA J( 358)/      37/
C%.E    2896    IBM
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
C%.R    2902    IBM
        DATA J( 359)/       2/
C%.E    2902    IBM
C%.E    2824    DEC10
C
        DATA J( 360)/      40/
        DATA J( 361)/   26165/
C
        DATA J( 362)/       4/
        DATA J( 363)/     519/
C
C
C====== NO MICROCODE
C
        DATA J( 364)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 365)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 366)/      77/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 367)/      32/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 368)/       0/
C
C====== PACKED BIT WIDTH OF RESULT
C
        DATA J( 369)/     288/
C
C====== MASK FOR RESULT
C
        DATA J( 370)/       0/
C
C====== NUMBER OF PANEL DEPOSITS FOR SETUP
C
        DATA J( 371)/       2/
C
        DATA J( 372)/       0/
        DATA J( 373)/   25910/
C
        DATA J( 374)/       0/
        DATA J( 375)/   25398/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 376)/       2/
C
        DATA J( 377)/   25910/
        DATA J( 378)/   25398/
C
C====== ERROR CODE
C
        DATA J( 379)/      74/
C
C ******************************************************************************
C
C *==== JUMP OUT OF CURRENT TEST SEQUENCE
C
        DATA J( 380)/      -1/
C
C====== LOC IN TABLE J TO RESUME EXECUTION
C
        DATA J( 381)/     425/
C
C *                         T E S T  C O D E   75
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 382)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J( 383)/       2/
C
        DATA J( 384)/       8/
        DATA J( 385)/   26165/
C
        DATA J( 386)/      16/
        DATA J( 387)/     515/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 388)/       1/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 389)/      41/
C
C
C                     LPSR .+0.; IN; DB=INBS
C
        DATA J( 390)/    4847/
        DATA J( 391)/  -14336/
        DATA J( 392)/     512/
        DATA J( 393)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 394)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 395)/   25398/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 396)/      32/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 397)/       0/
C
C====== PACKED BIT WIDTH OF RESULT
C
        DATA J( 398)/     288/
C
C====== MASK FOR RESULT
C
        DATA J( 399)/       0/
C
C====== NO SETUPS
C
        DATA J( 400)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 401)/       2/
C
        DATA J( 402)/    1080/
        DATA J( 403)/    1064/
C
C====== ERROR CODE
C
        DATA J( 404)/      75/
C
C ******************************************************************************
C *                         T E S T  C O D E   76
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 405)/   22314/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J( 406)/       2/
C
        DATA J( 407)/      80/
        DATA J( 408)/   26165/
C
        DATA J( 409)/       0/
        DATA J( 410)/   25142/
C
C
C====== NO MICROCODE
C
        DATA J( 411)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 412)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 413)/   25398/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 414)/      32/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 415)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J( 416)/      32/
C
C====== MASK FOR RESULT
C
        DATA J( 417)/       0/
C
C====== NUMBER OF PANEL DEPOSITS FOR SETUP
C
        DATA J( 418)/       1/
C
        DATA J( 419)/       0/
        DATA J( 420)/     514/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 421)/       3/
C
        DATA J( 422)/    1085/
        DATA J( 423)/    1069/
        DATA J( 424)/    1053/
C
C====== ERROR CODE
C
        DATA J( 425)/      76/
C
C ******************************************************************************
C *                         T E S T  C O D E   77
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 426)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J( 427)/       4/
C
        DATA J( 428)/       9/
        DATA J( 429)/     515/
C
        DATA J( 430)/       0/
        DATA J( 431)/     514/
C
        DATA J( 432)/     104/
        DATA J( 433)/   26165/
C
        DATA J( 434)/       0/
        DATA J( 435)/   25142/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 436)/       2/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 437)/      41/
C
C
C                     CLR 0.
C
        DATA J( 438)/     512/
        DATA J( 439)/       0/
        DATA J( 440)/       0/
        DATA J( 441)/       0/
C
C
C                     RPSFT; MI<DB; SETMA
C
        DATA J( 442)/    4820/
        DATA J( 443)/       0/
        DATA J( 444)/       0/
        DATA J( 445)/     240/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 446)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 447)/      77/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 448)/      32/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 449)/       0/
C
C====== PACKED BIT WIDTH OF RESULT
C
        DATA J( 450)/     288/
C
C====== MASK FOR RESULT
C
        DATA J( 451)/       0/
C
C====== NUMBER OF PANEL DEPOSITS FOR SETUP
C
        DATA J( 452)/       5/
C
        DATA J( 453)/       0/
        DATA J( 454)/   25142/
C
        DATA J( 455)/       0/
        DATA J( 456)/   25910/
C
        DATA J( 457)/       0/
        DATA J( 458)/   25398/
C
        DATA J( 459)/       0/
        DATA J( 460)/   25910/
C
        DATA J( 461)/       0/
        DATA J( 462)/   25398/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 463)/       2/
C
        DATA J( 464)/   25910/
        DATA J( 465)/   25398/
C
C====== ERROR CODE
C
        DATA J( 466)/      77/
C
C ******************************************************************************
C *                         T E S T  C O D E   79
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 467)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J( 468)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 469)/       5/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 470)/      41/
C
C
C                     RPSFA 10.; DPX(-4.)<DB
C
        DATA J( 471)/    4804/
        DATA J( 472)/       0/
        DATA J( 473)/   16384/
        DATA J( 474)/      10/
C
C
C                     FADD ZERO,DPX(-4.)
C
        DATA J( 475)/       1/
        DATA J( 476)/  -11264/
        DATA J( 477)/       0/
        DATA J( 478)/       0/
C
C
C                     FADD ZERO,ZERO
C
        DATA J( 479)/       1/
        DATA J( 480)/   -9728/
        DATA J( 481)/       0/
        DATA J( 482)/       0/
C
C
C                     DPY(-4.)<FA
C
        DATA J( 483)/       0/
        DATA J( 484)/       0/
        DATA J( 485)/    8192/
        DATA J( 486)/       0/
C
C
C                     HALT
C
        DATA J( 487)/       3/
        DATA J( 488)/   -4096/
        DATA J( 489)/       0/
        DATA J( 490)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 491)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 492)/      81/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 493)/     294/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 494)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J( 495)/      38/
C
C====== MASK FOR RESULT
C
        DATA J( 496)/       0/
C
C====== NO SETUPS
C
        DATA J( 497)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 498)/       3/
C
        DATA J( 499)/    1084/
        DATA J( 500)/    1068/
        DATA J( 501)/    1052/
C
C====== ERROR CODE
C
        DATA J( 502)/      79/
C
C ******************************************************************************
C *                         T E S T  C O D E   80
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 503)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J( 504)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 505)/       4/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 506)/      41/
C
C
C                     RPSFA 10.; DPY(-4.)<DB
C
        DATA J( 507)/    4804/
        DATA J( 508)/       0/
        DATA J( 509)/    4096/
        DATA J( 510)/      10/
C
C
C                     FADD DPY(-4.),ZERO
C
        DATA J( 511)/       1/
        DATA J( 512)/  -17920/
        DATA J( 513)/       0/
        DATA J( 514)/       0/
C
C
C                     FADD ZERO,ZERO
C
        DATA J( 515)/       1/
        DATA J( 516)/   -9728/
        DATA J( 517)/       0/
        DATA J( 518)/       0/
C
C
C                     DPX(-4)<FA    (J(521)=100000 OCTAL IN MAINLINE)
C
        DATA J( 519)/       0/
        DATA J( 520)/       0/
        DATA J( 521)/      -1/
        DATA J( 522)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 523)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 524)/      81/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 525)/     294/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 526)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J( 527)/      38/
C
C====== MASK FOR RESULT
C
        DATA J( 528)/       0/
C
C====== NO SETUPS
C
        DATA J( 529)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 530)/       3/
C
        DATA J( 531)/    1083/
        DATA J( 532)/    1067/
        DATA J( 533)/    1051/
C
C====== ERROR CODE
C
        DATA J( 534)/      80/
C
C ******************************************************************************
C *                         T E S T  C O D E   81
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 535)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J( 536)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 537)/       3/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 538)/      45/
C
C
C                     FADD ZERO,DPY(-4.)
C
        DATA J( 539)/       1/
        DATA J( 540)/  -10752/
        DATA J( 541)/       0/
        DATA J( 542)/       0/
C
C
C                     FADD ZERO,ZERO
C
        DATA J( 543)/       1/
        DATA J( 544)/   -9728/
        DATA J( 545)/       0/
        DATA J( 546)/       0/
C
C
C                     CLR 0.; MI<FA; SETMA
C
        DATA J( 547)/     512/
        DATA J( 548)/       0/
        DATA J( 549)/       0/
        DATA J( 550)/     112/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 551)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 552)/      81/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 553)/     294/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 554)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J( 555)/      38/
C
C====== MASK FOR RESULT
C
        DATA J( 556)/       0/
C
C====== NUMBER OF PANEL DEPOSITS FOR SETUP
C
        DATA J( 557)/       1/
C
        DATA J( 558)/       0/
        DATA J( 559)/     514/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 560)/       3/
C
        DATA J( 561)/    1085/
        DATA J( 562)/    1069/
        DATA J( 563)/    1053/
C
C====== ERROR CODE
C
        DATA J( 564)/      81/
C
C ******************************************************************************
C *                         T E S T  C O D E   82
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 565)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J( 566)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 567)/       4/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 568)/      53/
C
C
C                     FADD ZERO,FA
C
        DATA J( 569)/       1/
        DATA J( 570)/  -11776/
        DATA J( 571)/       0/
        DATA J( 572)/       0/
C
C
C                     FADD ZERO,ZERO
C
        DATA J( 573)/       1/
        DATA J( 574)/   -9728/
        DATA J( 575)/       0/
        DATA J( 576)/       0/
C
C
C                     CLR 0.; MI<FA; SETMA
C
        DATA J( 577)/     512/
        DATA J( 578)/       0/
        DATA J( 579)/       0/
        DATA J( 580)/     112/
C
C
C                     HALT
C
        DATA J( 581)/       3/
        DATA J( 582)/   -4096/
        DATA J( 583)/       0/
        DATA J( 584)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 585)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 586)/      81/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 587)/     294/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 588)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J( 589)/      38/
C
C====== MASK FOR RESULT
C
        DATA J( 590)/       0/
C
C====== NUMBER OF PANEL DEPOSITS FOR SETUP
C
        DATA J( 591)/       1/
C
        DATA J( 592)/       0/
        DATA J( 593)/     514/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 594)/       3/
C
        DATA J( 595)/    1085/
        DATA J( 596)/    1069/
        DATA J( 597)/    1053/
C
C====== ERROR CODE
C
        DATA J( 598)/      82/
C
C ******************************************************************************
C *                         T E S T  C O D E   83
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 599)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J( 600)/       1/
C
        DATA J( 601)/       0/
        DATA J( 602)/     515/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 603)/       7/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 604)/      41/
C
C
C                     RPSFA 11.; DPX(0.)<DB
C
        DATA J( 605)/    4804/
        DATA J( 606)/       0/
        DATA J( 607)/   16388/
        DATA J( 608)/      11/
C
C
C                     RPSFA 10.; DPY(0.)<DB
C
        DATA J( 609)/    4804/
        DATA J( 610)/       0/
        DATA J( 611)/    4100/
        DATA J( 612)/      10/
C
C
C                     FMUL TM,DPY(0.)
C
        DATA J( 613)/       0/
        DATA J( 614)/       0/
        DATA J( 615)/      32/
        DATA J( 616)/    7680/
C
C
C                     FMUL FM,FA
C
        DATA J( 617)/       0/
        DATA J( 618)/       0/
        DATA J( 619)/       0/
        DATA J( 620)/    4096/
C
C
C                     FMUL FM,FA
C
        DATA J( 621)/       0/
        DATA J( 622)/       0/
        DATA J( 623)/       0/
        DATA J( 624)/    4096/
C
C
C                     DPX(-4.)<FM
C
        DATA J( 625)/       0/
        DATA J( 626)/       0/
        DATA J( 627)/  -16384/
        DATA J( 628)/       0/
C
C
C                     HALT
C
        DATA J( 629)/       3/
        DATA J( 630)/   -4096/
        DATA J( 631)/       0/
        DATA J( 632)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 633)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 634)/      81/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 635)/     294/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 636)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J( 637)/      38/
C
C====== MASK FOR RESULT
C
        DATA J( 638)/       0/
C
C====== NO SETUPS
C
        DATA J( 639)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 640)/       3/
C
        DATA J( 641)/    1083/
        DATA J( 642)/    1067/
        DATA J( 643)/    1051/
C
C====== ERROR CODE
C
        DATA J( 644)/      83/
C
C ******************************************************************************
C *                         T E S T  C O D E   84
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 645)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J( 646)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 647)/       4/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 648)/      49/
C
C
C                     FMUL DPY(0.),DPX(0.)
C
        DATA J( 649)/       0/
        DATA J( 650)/       0/
        DATA J( 651)/     288/
        DATA J( 652)/    6400/
C
C
C                     FMUL FM,FA
C
        DATA J( 653)/       0/
        DATA J( 654)/       0/
        DATA J( 655)/       0/
        DATA J( 656)/    4096/
C
C
C                     FMUL FM,FA
C
        DATA J( 657)/       0/
        DATA J( 658)/       0/
        DATA J( 659)/       0/
        DATA J( 660)/    4096/
C
C
C                     DPY(-4.)<FM
C
        DATA J( 661)/       0/
        DATA J( 662)/       0/
        DATA J( 663)/   12288/
        DATA J( 664)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 665)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 666)/      81/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 667)/     294/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 668)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J( 669)/      38/
C
C====== MASK FOR RESULT
C
        DATA J( 670)/       0/
C
C====== NO SETUPS
C
        DATA J( 671)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 672)/       3/
C
        DATA J( 673)/    1084/
        DATA J( 674)/    1068/
        DATA J( 675)/    1052/
C
C====== ERROR CODE
C
        DATA J( 676)/      84/
C
C ******************************************************************************
C *                         T E S T  C O D E   85
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 677)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J( 678)/       1/
C
        DATA J( 679)/       0/
        DATA J( 680)/     514/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 681)/       6/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 682)/      41/
C
C
C                     RPSFA 11.; DPY(0.)<DB
C
        DATA J( 683)/    4804/
        DATA J( 684)/       0/
        DATA J( 685)/    4100/
        DATA J( 686)/      11/
C
C
C                     RPSFA 10.; DPX(0.)<DB
C
        DATA J( 687)/    4804/
        DATA J( 688)/       0/
        DATA J( 689)/   16388/
        DATA J( 690)/      10/
C
C
C                     FMUL DPY(0.),DPX(0.)
C
        DATA J( 691)/       0/
        DATA J( 692)/       0/
        DATA J( 693)/     288/
        DATA J( 694)/    6400/
C
C
C                     FMUL FM,FA
C
        DATA J( 695)/       0/
        DATA J( 696)/       0/
        DATA J( 697)/       0/
        DATA J( 698)/    4096/
C
C
C                     FMUL FM,FA
C
        DATA J( 699)/       0/
        DATA J( 700)/       0/
        DATA J( 701)/       0/
        DATA J( 702)/    4096/
C
C
C                     CLR 0.; MI<FM; SETMA
C
        DATA J( 703)/     512/
        DATA J( 704)/       0/
        DATA J( 705)/       0/
        DATA J( 706)/     176/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 707)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 708)/      81/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 709)/     294/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 710)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J( 711)/      38/
C
C====== MASK FOR RESULT
C
        DATA J( 712)/       0/
C
C====== NUMBER OF PANEL DEPOSITS FOR SETUP
C
        DATA J( 713)/       1/
C
        DATA J( 714)/       0/
        DATA J( 715)/     514/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 716)/       3/
C
        DATA J( 717)/    1085/
        DATA J( 718)/    1069/
        DATA J( 719)/    1053/
C
C====== ERROR CODE
C
        DATA J( 720)/      85/
C
C ******************************************************************************
C *                         T E S T  C O D E   78
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 721)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J( 722)/       1/
C
        DATA J( 723)/       0/
        DATA J( 724)/     515/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 725)/       7/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 726)/      41/
C
C
C                     RPSFA 10.; DPX(-4.)<DB
C
        DATA J( 727)/    4804/
        DATA J( 728)/       0/
        DATA J( 729)/   16384/
        DATA J( 730)/      10/
C
C
C                     FADD DPX(-4.),ZERO
C
        DATA J( 731)/       1/
        DATA J( 732)/  -22016/
        DATA J( 733)/       0/
        DATA J( 734)/       0/
C
C
C                     FADD ZERO,ZERO
C
        DATA J( 735)/       1/
        DATA J( 736)/   -9728/
        DATA J( 737)/       0/
        DATA J( 738)/       0/
C
C
C                     FMUL TM,FA
C
        DATA J( 739)/       0/
        DATA J( 740)/       0/
        DATA J( 741)/       0/
        DATA J( 742)/    7168/
C
C
C                     FMUL FM,FA
C
        DATA J( 743)/       0/
        DATA J( 744)/       0/
        DATA J( 745)/       0/
        DATA J( 746)/    4096/
C
C
C                     FMUL FM,FA
C
        DATA J( 747)/       0/
        DATA J( 748)/       0/
        DATA J( 749)/       0/
        DATA J( 750)/    4096/
C
C
C                     DPY(-4.)<FM
C
        DATA J( 751)/       0/
        DATA J( 752)/       0/
        DATA J( 753)/   12288/
        DATA J( 754)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 755)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 756)/      81/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 757)/     294/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 758)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J( 759)/      38/
C
C====== MASK FOR RESULT
C
        DATA J( 760)/       0/
C
C====== NO SETUPS
C
        DATA J( 761)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 762)/       3/
C
        DATA J( 763)/    1084/
        DATA J( 764)/    1068/
        DATA J( 765)/    1052/
C
C====== ERROR CODE
C
        DATA J( 766)/      78/
C
C ******************************************************************************
C *                         T E S T  C O D E   86
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 767)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J( 768)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 769)/       7/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 770)/      41/
C
C
C                     RPSFA 10.; DPX(0.)<DB
C
        DATA J( 771)/    4804/
        DATA J( 772)/       0/
        DATA J( 773)/   16388/
        DATA J( 774)/      10/
C
C
C                     FMUL DPX(0.),DPY(0.)
C
        DATA J( 775)/       0/
        DATA J( 776)/       0/
        DATA J( 777)/     288/
        DATA J( 778)/    5632/
C
C
C                     FMUL FM,FA
C
        DATA J( 779)/       0/
        DATA J( 780)/       0/
        DATA J( 781)/       0/
        DATA J( 782)/    4096/
C
C
C                     FMUL FM,FA
C
        DATA J( 783)/       0/
        DATA J( 784)/       0/
        DATA J( 785)/       0/
        DATA J( 786)/    4096/
C
C
C                     FADD FM,ZERO
C
        DATA J( 787)/       1/
        DATA J( 788)/  -26112/
        DATA J( 789)/       0/
        DATA J( 790)/       0/
C
C
C                     FADD ZERO,ZERO
C
        DATA J( 791)/       1/
        DATA J( 792)/   -9728/
        DATA J( 793)/       0/
        DATA J( 794)/       0/
C
C
C                     DPX(-4)<FA    (J(797)=100000 OCTAL IN MAINLINE)
C
        DATA J( 795)/       0/
        DATA J( 796)/       0/
        DATA J( 797)/      -1/
        DATA J( 798)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 799)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 800)/      81/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 801)/     294/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 802)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J( 803)/      38/
C
C====== MASK FOR RESULT
C
        DATA J( 804)/       0/
C
C====== NO SETUPS
C
        DATA J( 805)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 806)/       3/
C
        DATA J( 807)/    1083/
        DATA J( 808)/    1067/
        DATA J( 809)/    1051/
C
C====== ERROR CODE
C
        DATA J( 810)/      86/
C
C ******************************************************************************
C *                         T E S T  C O D E   87
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 811)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J( 812)/       2/
C
        DATA J( 813)/       0/
        DATA J( 814)/    2048/
C
        DATA J( 815)/      10/
        DATA J( 816)/     515/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 817)/       6/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 818)/      41/
C
C
C                     RPSFT; MI<DB; SETMA
C
        DATA J( 819)/    4820/
        DATA J( 820)/       0/
        DATA J( 821)/       0/
        DATA J( 822)/     240/
C
C
C                     CLR 0.; SETMA
C
        DATA J( 823)/     512/
        DATA J( 824)/       0/
        DATA J( 825)/       0/
        DATA J( 826)/      48/
C
C
C                     SPMDAV; FMUL DPY(0.),MD
C
        DATA J( 827)/       3/
        DATA J( 828)/  -24576/
        DATA J( 829)/      32/
        DATA J( 830)/    6912/
C
C
C                     FMUL FM,FA
C
        DATA J( 831)/       0/
        DATA J( 832)/       0/
        DATA J( 833)/       0/
        DATA J( 834)/    4096/
C
C
C                     FMUL FM,FA
C
        DATA J( 835)/       0/
        DATA J( 836)/       0/
        DATA J( 837)/       0/
        DATA J( 838)/    4096/
C
C
C                     DPY(-4.)<FM
C
        DATA J( 839)/       0/
        DATA J( 840)/       0/
        DATA J( 841)/   12288/
        DATA J( 842)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 843)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 844)/      81/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 845)/     294/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 846)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J( 847)/      38/
C
C====== MASK FOR RESULT
C
        DATA J( 848)/       0/
C
C====== NO SETUPS
C
        DATA J( 849)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 850)/       3/
C
        DATA J( 851)/    1084/
        DATA J( 852)/    1068/
        DATA J( 853)/    1052/
C
C====== ERROR CODE
C
        DATA J( 854)/      87/
C
C ******************************************************************************
C *                         T E S T  C O D E   88
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 855)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J( 856)/       1/
C
        DATA J( 857)/      10/
        DATA J( 858)/     515/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 859)/       4/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 860)/      49/
C
C
C                     SPMDA; FADD ZERO,MD
C
        DATA J( 861)/    4225/
        DATA J( 862)/  -10240/
        DATA J( 863)/       0/
        DATA J( 864)/       0/
C
C
C                     FADD ZERO,ZERO
C
        DATA J( 865)/       1/
        DATA J( 866)/   -9728/
        DATA J( 867)/       0/
        DATA J( 868)/       0/
C
C
C                     DPX(-4)<FA    (J(871)=100000 OCTAL IN MAINLINE)
C
        DATA J( 869)/       0/
        DATA J( 870)/       0/
        DATA J( 871)/      -1/
        DATA J( 872)/       0/
C
C
C                     HALT
C
        DATA J( 873)/       3/
        DATA J( 874)/   -4096/
        DATA J( 875)/       0/
        DATA J( 876)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 877)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 878)/      81/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 879)/     294/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 880)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J( 881)/      38/
C
C====== MASK FOR RESULT
C
        DATA J( 882)/       0/
C
C====== NO SETUPS
C
        DATA J( 883)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 884)/       3/
C
        DATA J( 885)/    1083/
        DATA J( 886)/    1067/
        DATA J( 887)/    1051/
C
C====== ERROR CODE
C
        DATA J( 888)/      88/
C
C ******************************************************************************
C *                         T E S T  C O D E   89
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 889)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J( 890)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 891)/       2/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 892)/      41/
C
C
C                    DB=TM; DPX(-4.)<DB
C
        DATA J( 893)/       0/
        DATA J( 894)/       0/
        DATA J( 895)/   19968/
        DATA J( 896)/       0/
C
C
C                     HALT
C
        DATA J( 897)/       3/
        DATA J( 898)/   -4096/
        DATA J( 899)/       0/
        DATA J( 900)/       0/
C
C====== NO INPUT ARGUMENTS
C
        DATA J( 901)/       0/
C
C====== NO SETUPS
C
        DATA J( 902)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 903)/       3/
C
        DATA J( 904)/    1083/
        DATA J( 905)/    1067/
        DATA J( 906)/    1051/
C
C====== ERROR CODE
C
        DATA J( 907)/      89/
C
C ******************************************************************************
C *                         T E S T  C O D E   90
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 908)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J( 909)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 910)/       4/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 911)/      41/
C
C
C                     FADD TM,ZERO
C
        DATA J( 912)/       1/
        DATA J( 913)/  -13824/
        DATA J( 914)/       0/
        DATA J( 915)/       0/
C
C
C                     FADD ZERO,ZERO
C
        DATA J( 916)/       1/
        DATA J( 917)/   -9728/
        DATA J( 918)/       0/
        DATA J( 919)/       0/
C
C
C                     DPX(-4)<FA    (J(922)=100000 OCTAL IN MAINLINE)
C
        DATA J( 920)/       0/
        DATA J( 921)/       0/
        DATA J( 922)/      -1/
        DATA J( 923)/       0/
C
C
C                     HALT
C
        DATA J( 924)/       3/
        DATA J( 925)/   -4096/
        DATA J( 926)/       0/
        DATA J( 927)/       0/
C
C====== NO INPUT ARGUMENTS
C
        DATA J( 928)/       0/
C
C====== NO SETUPS
C
        DATA J( 929)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 930)/       3/
C
        DATA J( 931)/    1083/
        DATA J( 932)/    1067/
        DATA J( 933)/    1051/
C
C====== ERROR CODE
C
        DATA J( 934)/      90/
C
C ******************************************************************************
C *                         T E S T  C O D E   91
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 935)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J( 936)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 937)/       6/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 938)/      41/
C
C
C                     RPSFA 10.; DPX(0.)<DB
C
        DATA J( 939)/    4804/
        DATA J( 940)/       0/
        DATA J( 941)/   16388/
        DATA J( 942)/      10/
C
C
C                     FMUL TM,DPX(0.)
C
        DATA J( 943)/       0/
        DATA J( 944)/       0/
        DATA J( 945)/     256/
        DATA J( 946)/    7424/
C
C
C                     FMUL FM,FA
C
        DATA J( 947)/       0/
        DATA J( 948)/       0/
        DATA J( 949)/       0/
        DATA J( 950)/    4096/
C
C
C                     FMUL FM,FA
C
        DATA J( 951)/       0/
        DATA J( 952)/       0/
        DATA J( 953)/       0/
        DATA J( 954)/    4096/
C
C
C                     DPY(-4.)<FM
C
        DATA J( 955)/       0/
        DATA J( 956)/       0/
        DATA J( 957)/   12288/
        DATA J( 958)/       0/
C
C
C                     HALT
C
        DATA J( 959)/       3/
        DATA J( 960)/   -4096/
        DATA J( 961)/       0/
        DATA J( 962)/       0/
C
C====== NO INPUT ARGUMENTS
C
        DATA J( 963)/       0/
C
C====== NO SETUPS
C
        DATA J( 964)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 965)/       3/
C
        DATA J( 966)/    1084/
        DATA J( 967)/    1068/
        DATA J( 968)/    1052/
C
C====== ERROR CODE
C
        DATA J( 969)/      91/
C
C ******************************************************************************
C *                         T E S T  C O D E   92
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 970)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J( 971)/       1/
C
        DATA J( 972)/       0/
        DATA J( 973)/    2048/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J( 974)/       3/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J( 975)/      41/
C
C
C                     LDSPI 0.; DB=0.
C
        DATA J( 976)/     896/
        DATA J( 977)/       0/
        DATA J( 978)/    1024/
        DATA J( 979)/       0/
C
C
C                     MOV 0.,0.; SETMA
C
        DATA J( 980)/   16384/
        DATA J( 981)/       0/
        DATA J( 982)/       0/
        DATA J( 983)/      48/
C
C
C                     HALT
C
        DATA J( 984)/       3/
        DATA J( 985)/   -4096/
        DATA J( 986)/       0/
        DATA J( 987)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J( 988)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J( 989)/      41/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J( 990)/      16/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J( 991)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J( 992)/      16/
C
C====== MASK FOR RESULT
C
        DATA J( 993)/       0/
C
C====== NO SETUPS
C
        DATA J( 994)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J( 995)/       1/
C
        DATA J( 996)/    1026/
C
C====== ERROR CODE
C
        DATA J( 997)/      92/
C
C ******************************************************************************
C *                         T E S T  C O D E   93
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J( 998)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J( 999)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1000)/       1/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1001)/      45/
C
C
C                     MOV 0.,0.; DB=SPFN; DPX(-4.)<DB
C
        DATA J(1002)/   16384/
        DATA J(1003)/       0/
        DATA J(1004)/   19456/
        DATA J(1005)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1006)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1007)/      41/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1008)/      16/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1009)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J(1010)/      16/
C
C====== MASK FOR RESULT
C
        DATA J(1011)/       0/
C
C====== NO SETUPS
C
        DATA J(1012)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1013)/       1/
C
        DATA J(1014)/    1083/
C
C====== ERROR CODE
C
        DATA J(1015)/      93/
C
C ******************************************************************************
C *                         T E S T  C O D E   94
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1016)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J(1017)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1018)/       1/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1019)/      41/
C
C
C                     LDSPE 0.; DB=0.
C
        DATA J(1020)/     832/
        DATA J(1021)/       0/
        DATA J(1022)/    1024/
        DATA J(1023)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1024)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1025)/      41/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1026)/      10/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1027)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J(1028)/      10/
C
C====== MASK FOR RESULT
C
        DATA J(1029)/       0/
C
C====== NO SETUPS
C
        DATA J(1030)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1031)/       1/
C
        DATA J(1032)/    1051/
C
C====== ERROR CODE
C
        DATA J(1033)/      94/
C
C ******************************************************************************
C *                         T E S T  C O D E   95
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1034)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J(1035)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1036)/       5/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1037)/      45/
C
C
C                     RPSFA 10.; DPX(0.)<DB
C
        DATA J(1038)/    4804/
        DATA J(1039)/       0/
        DATA J(1040)/   16388/
        DATA J(1041)/      10/
C
C
C                     FADD ZERO,MDPX(0.); MOV 0,0
C
        DATA J(1042)/   16385/
        DATA J(1043)/   -9216/
        DATA J(1044)/     256/
        DATA J(1045)/       0/
C
C
C                     FADD ZERO,ZERO
C
        DATA J(1046)/       1/
        DATA J(1047)/   -9728/
        DATA J(1048)/       0/
        DATA J(1049)/       0/
C
C
C                     DPX(-4)<FA    (J(1052)=100000 OCTAL IN MAINLINE)
C
        DATA J(1050)/       0/
        DATA J(1051)/       0/
        DATA J(1052)/      -1/
        DATA J(1053)/       0/
C
C
C                     HALT
C
        DATA J(1054)/       3/
        DATA J(1055)/   -4096/
        DATA J(1056)/       0/
        DATA J(1057)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1058)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1059)/      41/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1060)/      10/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1061)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J(1062)/      10/
C
C====== MASK FOR RESULT
C
        DATA J(1063)/       0/
C
C====== NO SETUPS
C
        DATA J(1064)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1065)/       1/
C
        DATA J(1066)/    1051/
C
C====== ERROR CODE
C
        DATA J(1067)/      95/
C
C ******************************************************************************
C *                         T E S T  C O D E   96
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1068)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J(1069)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1070)/       1/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1071)/      49/
C
C
C                     FADD ZERO,EDPX(0.); MOV 0,0
C
        DATA J(1072)/   16385/
        DATA J(1073)/   -8704/
        DATA J(1074)/     256/
        DATA J(1075)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1076)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1077)/      41/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1078)/     514/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1079)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J(1080)/   -2558/
C
C====== MASK FOR RESULT
C
        DATA J(1081)/       0/
C
C====== NO SETUPS
C
        DATA J(1082)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1083)/       1/
C
        DATA J(1084)/    1067/
C
C====== ERROR CODE
C
        DATA J(1085)/      96/
C
C ******************************************************************************
C *                         T E S T  C O D E   98
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1086)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J(1087)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1088)/       3/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1089)/      41/
C
C
C                     SETEXA 0.
C
        DATA J(1090)/    4868/
        DATA J(1091)/       0/
        DATA J(1092)/       0/
        DATA J(1093)/       0/
C
C
C                     LDSPNL 0.; REXIT
C
        DATA J(1094)/     771/
        DATA J(1095)/  -20480/
        DATA J(1096)/       0/
        DATA J(1097)/       0/
C
C
C                     HALT
C
        DATA J(1098)/       3/
        DATA J(1099)/   -4096/
        DATA J(1100)/       0/
        DATA J(1101)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1102)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1103)/      41/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1104)/      12/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1105)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J(1106)/      12/
C
C====== MASK FOR RESULT
C
        DATA J(1107)/       0/
C
C====== NO SETUPS
C
        DATA J(1108)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1109)/       1/
C
        DATA J(1110)/    1029/
C
C====== ERROR CODE
C
        DATA J(1111)/      98/
C
C ******************************************************************************
C *                         T E S T  C O D E   99
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1112)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J(1113)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1114)/       1/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1115)/      41/
C
C
C                     SETEX .+0.
C
        DATA J(1116)/    4876/
        DATA J(1117)/       0/
        DATA J(1118)/       0/
        DATA J(1119)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1120)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1121)/      41/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1122)/      12/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1123)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J(1124)/      12/
C
C====== MASK FOR RESULT
C
        DATA J(1125)/       0/
C
C====== NO SETUPS
C
        DATA J(1126)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1127)/       1/
C
        DATA J(1128)/    1029/
C
C====== ERROR CODE
C
        DATA J(1129)/      99/
C
C ******************************************************************************
C *                         T E S T  C O D E  100
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1130)/      37/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J(1131)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1132)/       4/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1133)/      41/
C
C
C                     LDTMA; DB=0.
C
        DATA J(1134)/       3/
        DATA J(1135)/  -31232/
        DATA J(1136)/    1024/
        DATA J(1137)/       0/
C
C
C                     SETEXT
C
        DATA J(1138)/    4884/
        DATA J(1139)/       0/
        DATA J(1140)/       0/
        DATA J(1141)/       0/
C
C
C                     LDSPNL 0.; REXIT
C
        DATA J(1142)/     771/
        DATA J(1143)/  -20480/
        DATA J(1144)/       0/
        DATA J(1145)/       0/
C
C
C                     HALT
C
        DATA J(1146)/       3/
        DATA J(1147)/   -4096/
        DATA J(1148)/       0/
        DATA J(1149)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1150)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1151)/      41/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1152)/      12/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1153)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J(1154)/      12/
C
C====== MASK FOR RESULT
C
        DATA J(1155)/       0/
C
C====== NO SETUPS
C
        DATA J(1156)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1157)/       1/
C
        DATA J(1158)/    1029/
C
C====== ERROR CODE
C
        DATA J(1159)/     100/
C
C ******************************************************************************
C *                         T E S T  C O D E  101
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1160)/    1278/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J(1161)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1162)/       1/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1163)/    1282/
C
C
C                     JMPA 0.
C
        DATA J(1164)/    4608/
        DATA J(1165)/       0/
        DATA J(1166)/       0/
        DATA J(1167)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1168)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1169)/    1282/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1170)/      12/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1171)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J(1172)/      12/
C
C====== MASK FOR RESULT
C
        DATA J(1173)/       0/
C
C====== NUMBER OF PANEL DEPOSITS FOR SETUP
C
        DATA J(1174)/       1/
C
        DATA J(1175)/       0/
        DATA J(1176)/    4096/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1177)/       1/
C
        DATA J(1178)/    1024/
C
C====== ERROR CODE
C
        DATA J(1179)/     101/
C
C ******************************************************************************
C *                         T E S T  C O D E  102
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1180)/    1278/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J(1181)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1182)/       1/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1183)/    1282/
C
C
C                     JMP .+0.
C
        DATA J(1184)/    4616/
        DATA J(1185)/       0/
        DATA J(1186)/       0/
        DATA J(1187)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1188)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1189)/    1282/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1190)/      12/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1191)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J(1192)/      12/
C
C====== MASK FOR RESULT
C
        DATA J(1193)/       0/
C
C====== NUMBER OF PANEL DEPOSITS FOR SETUP
C
        DATA J(1194)/       1/
C
        DATA J(1195)/       0/
        DATA J(1196)/    4096/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1197)/       1/
C
        DATA J(1198)/    1024/
C
C====== ERROR CODE
C
        DATA J(1199)/     102/
C
C ******************************************************************************
C *                         T E S T  C O D E  103
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1200)/    1278/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J(1201)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1202)/       2/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1203)/    1282/
C
C
C                     SETEXA 0.
C
        DATA J(1204)/    4868/
        DATA J(1205)/       0/
        DATA J(1206)/       0/
        DATA J(1207)/       0/
C
C
C                     RETURN
C
        DATA J(1208)/       0/
        DATA J(1209)/     224/
        DATA J(1210)/       0/
        DATA J(1211)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1212)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1213)/    1282/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1214)/      12/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1215)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J(1216)/      12/
C
C====== MASK FOR RESULT
C
        DATA J(1217)/       0/
C
C====== NUMBER OF PANEL DEPOSITS FOR SETUP
C
        DATA J(1218)/       2/
C
        DATA J(1219)/       0/
        DATA J(1220)/    4096/
C
        DATA J(1221)/       0/
        DATA J(1222)/    4096/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1223)/       1/
C
        DATA J(1224)/    1024/
C
C====== ERROR CODE
C
        DATA J(1225)/     103/
C
C ******************************************************************************
C *                         T E S T  C O D E  104
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1226)/    1278/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J(1227)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1228)/       2/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1229)/    1282/
C
C
C                     LDTMA; DB=0.
C
        DATA J(1230)/       3/
        DATA J(1231)/  -31232/
        DATA J(1232)/    1024/
        DATA J(1233)/       0/
C
C
C                     JMPT
C
        DATA J(1234)/    4624/
        DATA J(1235)/       0/
        DATA J(1236)/       0/
        DATA J(1237)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1238)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1239)/    1282/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1240)/      12/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1241)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J(1242)/      12/
C
C====== MASK FOR RESULT
C
        DATA J(1243)/       0/
C
C====== NUMBER OF PANEL DEPOSITS FOR SETUP
C
        DATA J(1244)/       2/
C
        DATA J(1245)/       0/
        DATA J(1246)/    4096/
C
        DATA J(1247)/       0/
        DATA J(1248)/    4096/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1249)/       1/
C
        DATA J(1250)/    1024/
C
C====== ERROR CODE
C
        DATA J(1251)/     104/
C
C ******************************************************************************
C *                         T E S T  C O D E  105
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1252)/       0/
C
C====== NO PANEL DEPOSIT SETUPS
C
        DATA J(1253)/       0/
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1254)/       1/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1255)/    1282/
C
C
C                     BR .+0.
C
        DATA J(1256)/       0/
        DATA J(1257)/      80/
        DATA J(1258)/       0/
        DATA J(1259)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1260)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1261)/    1280/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1262)/       4/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1263)/     -16/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J(1264)/       4/
C
C====== MASK FOR RESULT
C
        DATA J(1265)/     -16/
C
C====== NUMBER OF PANEL DEPOSITS FOR SETUP
C
        DATA J(1266)/       1/
C
        DATA J(1267)/       0/
        DATA J(1268)/    4096/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1269)/       1/
C
        DATA J(1270)/    1024/
C
C====== ERROR CODE
C
        DATA J(1271)/     105/
C
C ******************************************************************************
C
C====== DUMMY DATA AREA
C
        DATA J(1272)/       0/
        DATA J(1273)/       0/
        DATA J(1274)/       0/
        DATA J(1275)/       0/
        DATA J(1276)/       0/
        DATA J(1277)/       0/
C
C====== CONTROL BLOCK 2 ( SAME FORMAT AS CONTROL BLOCK 1 )
C
        DATA J(1278)/       0/
        DATA J(1279)/       4/
        DATA J(1280)/       0/
        DATA J(1281)/       0/
C
C====== PROGRAM SOURCE IS LOADED FROM THIS AREA
C
        DATA J(1282)/       0/
        DATA J(1283)/       0/
        DATA J(1284)/       0/
        DATA J(1285)/       0/
C
        DATA J(1286)/       0/
        DATA J(1287)/       0/
        DATA J(1288)/       0/
        DATA J(1289)/       0/
C
        DATA J(1290)/       0/
        DATA J(1291)/       0/
        DATA J(1292)/       0/
        DATA J(1293)/       0/
C
        DATA J(1294)/       0/
        DATA J(1295)/       0/
        DATA J(1296)/       0/
        DATA J(1297)/       0/
C
C====== MODE CONTROL FOR PNLLD SUBROUTINE
C
C       MODE = 1        CAUSES STEP OPERATION
C
        DATA J(1298)/       1/
        DATA J(1299)/       0/
        DATA J(1300)/       0/
        DATA J(1301)/       0/
C
        DATA J(1302)/       0/
        DATA J(1303)/       0/
C
C *****             T A B L E  M E M O R Y  R A M  T E S T S
C
C                           T E S T  C O D E  200
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1337)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J(1338)/       2/
C
        DATA J(1339)/       5/
        DATA J(1340)/     519/
C
        DATA J(1341)/    4096/
        DATA J(1342)/     515/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1343)/       2/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1344)/      41/
C
C
C                     RPSFA 9.; OUT
C
        DATA J(1345)/    4807/
        DATA J(1346)/  -16384/
        DATA J(1347)/       0/
        DATA J(1348)/       9/
C
C
C                     HALT
C
        DATA J(1349)/       3/
        DATA J(1350)/   -4096/
        DATA J(1351)/       0/
        DATA J(1352)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1353)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1354)/      77/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1355)/      38/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1356)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J(1357)/      38/
C
C====== MASK FOR RESULT
C
        DATA J(1358)/       0/
C
C====== NUMBER OF PANEL DEPOSITS FOR SETUP
C
        DATA J(1359)/       1/
C
        DATA J(1360)/       0/
        DATA J(1361)/    1087/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1362)/       3/
C
        DATA J(1363)/    1087/
        DATA J(1364)/    1071/
        DATA J(1365)/    1055/
C
C====== ERROR CODE
C
        DATA J(1366)/     200/
C
C ******************************************************************************
C *                         T E S T  C O D E   201
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1367)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J(1368)/       2/
C
        DATA J(1369)/    4096/
        DATA J(1370)/     515/
C
        DATA J(1371)/       5/
        DATA J(1372)/     519/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1373)/       7/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1374)/      41/
C
C
C                     RPSFA 10.; OUT
C
        DATA J(1375)/    4807/
        DATA J(1376)/  -16384/
        DATA J(1377)/       0/
        DATA J(1378)/      10/
C
C
C                     NOP
C
        DATA J(1379)/       0/
        DATA J(1380)/       0/
        DATA J(1381)/       0/
        DATA J(1382)/       0/
C
C
C                     NOP
C
        DATA J(1383)/       0/
        DATA J(1384)/       0/
        DATA J(1385)/       0/
        DATA J(1386)/       0/
C
C
C                     FADD TM,ZERO
C
        DATA J(1387)/       1/
        DATA J(1388)/  -13824/
        DATA J(1389)/       0/
        DATA J(1390)/       0/
C
C
C                     FADD ZERO,ZERO
C
        DATA J(1391)/       1/
        DATA J(1392)/   -9728/
        DATA J(1393)/       0/
        DATA J(1394)/       0/
C
C
C                     DB=TM; DPX(3.)<FM; DPY(-4.)<FM
C
        DATA J(1395)/       0/
        DATA J(1396)/       0/
        DATA J(1397)/      -1/
        DATA J(1398)/       0/
C
C
C                     HALT
C
        DATA J(1399)/       3/
        DATA J(1400)/   -4096/
        DATA J(1401)/       0/
        DATA J(1402)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1403)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1404)/      81/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1405)/     294/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1406)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J(1407)/      38/
C
C====== MASK FOR RESULT
C
        DATA J(1408)/       0/
C
C====== NO SETUPS
C
        DATA J(1409)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1410)/       3/
C
        DATA J(1411)/    1083/
        DATA J(1412)/    1067/
        DATA J(1413)/    1051/
C
C====== ERROR CODE
C
        DATA J(1414)/     201/
C
C ******************************************************************************
C *                         T E S T  C O D E   202
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1415)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J(1416)/       2/
C
        DATA J(1417)/    4096/
        DATA J(1418)/     515/
C
        DATA J(1419)/       5/
        DATA J(1420)/     519/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1421)/       8/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1422)/      41/
C
C
C                     RPSFA 10.; OUT
C
        DATA J(1423)/    4807/
        DATA J(1424)/  -16384/
        DATA J(1425)/       0/
        DATA J(1426)/      10/
C
C
C                     RPSFA 11.; DPX(-4.)<DB
C
        DATA J(1427)/    4804/
        DATA J(1428)/       0/
        DATA J(1429)/   16384/
        DATA J(1430)/      11/
C
C
C                     NOP
C
        DATA J(1431)/       0/
        DATA J(1432)/       0/
        DATA J(1433)/       0/
        DATA J(1434)/       0/
C
C
C                     FMUL TM,DPX(-4.)
C
        DATA J(1435)/       0/
        DATA J(1436)/       0/
        DATA J(1437)/       0/
        DATA J(1438)/    7424/
C
C
C                     FMUL FM,FA
C
        DATA J(1439)/       0/
        DATA J(1440)/       0/
        DATA J(1441)/       0/
        DATA J(1442)/    4096/
C
C
C                     FMUL FM,FA
C
        DATA J(1443)/       0/
        DATA J(1444)/       0/
        DATA J(1445)/       0/
        DATA J(1446)/    4096/
C
C
C                     DB=0.; DPX(-4.)<DB; DPY(-4.)<FM
C
        DATA J(1447)/       0/
        DATA J(1448)/       0/
        DATA J(1449)/   30000/
        DATA J(1450)/       0/
C
C
C                     HALT; DPY(-4.)<FM
C
        DATA J(1451)/       3/
        DATA J(1452)/   -4096/
        DATA J(1453)/   12288/
        DATA J(1454)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1455)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1456)/      81/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1457)/     294/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1458)/       0/
C
C====== UNPACKED BIT WIDTH OF RESULT
C
        DATA J(1459)/      38/
C
C====== MASK FOR RESULT
C
        DATA J(1460)/       0/
C
C====== NO SETUPS
C
        DATA J(1461)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1462)/       3/
C
        DATA J(1463)/    1084/
        DATA J(1464)/    1068/
        DATA J(1465)/    1052/
C
C====== ERROR CODE
C
        DATA J(1466)/     202/
C
C ******************************************************************************
C
C *==== JUMP OUT OF CURRENT TEST SEQUENCE
C
        DATA J(1467)/      -1/
C
C====== LOC IN TABLE J TO RESUME EXECUTION
C
        DATA J(1468)/     969/
C
C ***************************************************************************
C *                     T E S T   C O D E   105-109-113
C *
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1469)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J(1470)/       1/
C
        DATA J(1471)/       0/
        DATA J(1472)/     519/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1473)/       3/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1474)/      41/
C
C
C                     RPSFA 9.; OUT
C
        DATA J(1475)/    4807/
        DATA J(1476)/  -16384/
        DATA J(1477)/       0/
        DATA J(1478)/       9/
C
C
C                     NOP
C
        DATA J(1479)/       0/
        DATA J(1480)/       0/
        DATA J(1481)/       0/
        DATA J(1482)/       0/
C
C                     IN; DB=INBS; DPX(-4.)<DB
C
        DATA J(1483)/       3/
        DATA J(1484)/  -14336/
        DATA J(1485)/   16896/
        DATA J(1486)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1487)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1488)/      77/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1489)/      16/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1490)/       0/
C
C====== PACKED BIT WIDTH OF RESULT
C
        DATA J(1491)/     272/
C
C====== MASK FOR RESULT
C
        DATA J(1492)/       0/
C
C====== NO SETUPS
C
        DATA J(1493)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1494)/       1/
C
        DATA J(1495)/    1083/
C
C====== ERROR CODE
C
        DATA J(1496)/      70/
C
C ******************************************************************************
C *                         T E S T  C O D E   106-110-114
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1497)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J(1498)/       1/
C
        DATA J(1499)/       1/
        DATA J(1500)/     519/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1501)/       3/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1502)/      41/
C
C
C                     RPSFA 9.; OUT
C
        DATA J(1503)/    4807/
        DATA J(1504)/  -16384/
        DATA J(1505)/       0/
        DATA J(1506)/       9/
C
C                       NOP
C
        DATA J(1507)/       0/
        DATA J(1508)/       0/
        DATA J(1509)/       0/
        DATA J(1510)/       0/
C
C
C                     IN; DB=INBS; DPY(-4.)<DB
C
        DATA J(1511)/       3/
        DATA J(1512)/  -14336/
        DATA J(1513)/    4608/
        DATA J(1514)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1515)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1516)/      77/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1517)/      16/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1518)/       1/
C
C====== PACKED BIT WIDTH OF RESULT
C
        DATA J(1519)/     272/
C
C====== MASK FOR RESULT
C
        DATA J(1520)/       1/
C
C====== NO SETUPS
C
        DATA J(1521)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1522)/       1/
C
        DATA J(1523)/    1084/
C
C====== ERROR CODE
C
        DATA J(1524)/      71/
C
C ******************************************************************************
C *                         T E S T  C O D E   107-111-115
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1525)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J(1526)/       1/
C
        DATA J(1527)/       2/
        DATA J(1528)/     519/
C
C
C====== NUMBER OF MICROCODE INSTRUCTIONS
C
        DATA J(1529)/       1/
C
C====== LOC TO INSERT MICROCODE IN TABLE J
C
        DATA J(1530)/      41/
C
C
C                     RPSFA 9.; OUT
C
        DATA J(1531)/    4807/
        DATA J(1532)/  -16384/
        DATA J(1533)/       0/
        DATA J(1534)/       9/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1535)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1536)/      77/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1537)/      16/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1538)/  -24831/
C
C====== PACKED BIT WIDTH OF RESULT
C
        DATA J(1539)/     272/
C
C====== MASK FOR RESULT
C
        DATA J(1540)/  -30976/
C
C====== NO SETUPS
C
        DATA J(1541)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1542)/       1/
C
        DATA J(1543)/    1084/
C
C====== ERROR CODE
C
        DATA J(1544)/      72/
C
C ******************************************************************************
C *                         T E S T  C O D E   108-112-116
C
C ******************************************************************************
C
C====== LOCATION OF MICROCODE CONTROL BLOCK IN TABLE J
C
        DATA J(1545)/      37/
C
C====== NUMBER OF PANEL DEPOSIT SETUPS
C
        DATA J(1546)/       1/
C
        DATA J(1547)/       3/
        DATA J(1548)/     519/
C
C
C====== NO MICROCODE
C
        DATA J(1549)/       0/
C
C====== NUMBER OF INPUT ARGUMENTS
C
        DATA J(1550)/       1/
C
C====== LOC OF INPUT ARGUMENT IN TABLE J
C
        DATA J(1551)/      77/
C
C====== BIT WIDTH OF INPUT ARGUMENT
C
        DATA J(1552)/      16/
C
C====== MASK FOR LOW I6 BITS
C
        DATA J(1553)/       0/
C
C====== PACKED BIT WIDTH OF RESULT
C
        DATA J(1554)/     272/
C
C====== MASK FOR RESULT
C
        DATA J(1555)/       0/
C
C====== NO SETUPS
C
        DATA J(1556)/       0/
C
C====== NUMBER OF PANEL OPS TO GET ANSWER
C
        DATA J(1557)/       1/
C
        DATA J(1558)/    1084/
C
C====== ERROR CODE
C
        DATA J(1559)/      73/
C
C ******************************************************************************
C *                         E N D    O F   I O P   T E S T S
C
C ******************************************************************************
C
C====== END OF PASS FOR IOP TEST
C
        DATA J(1560)/       0/
C
C====== GO BACK TO CONTROLER
C
        DATA J(1561)/       0/
C
        END
