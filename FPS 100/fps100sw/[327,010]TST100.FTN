C****** TST100                                             == REL 5.0  , NOV 79
C****** TST100 = AP INTERFACE & MEMORY TEST                 = REL 5.0  , NOV 79
C
C
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TST100>MAINLINE                                          *
C  *    DATE        : NOV  4,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : N/A                                                      *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : TEST ONE  - TESTS THE WC, HMA, CTL, APMA, SWR, FN, MASK, *
C  *                              APMAE, AND MAE REGISTERS.  WRITES ALL ZEROS, *
C  *                              ALL ONES, SLIDING ZERO, AND SLIDING ONE TO   *
C  *                              EACH REGISTER FOLLOWED BY A READ AND COMPARE.*
C  *                              INOUT (TST100 SUBROUTINE) TRANSFERS DATA VIA *
C  *                              DMA.                                         *
C  *                  TEST TWO  - TESTS PSA, SPD, MA, TMA, DPA, SPAD, APSTATUS,*
C  *                              DA, PS, DPX EXP, DPX HMAN, DPX LMAN, DPY EXP,*
C  *                              DPY HMAN, DPY LMAN, MD EXP, MD HMAN, AND MD  *
C  *                              LMAN.  ALL ONES, ALL ZEROS, SLIDING ONE, AND *
C  *                              SLIDING ZERO ARE TRANSFERRED BY PANEL  FUNC- *
C  *                              TIONS TO THE AP.  THESE VALUES ARE PANELED   *
C  *                              BACK AND COMPARED.  THE REGISTERS ARE TESTED *
C  *                              BEFORE THE MEMORIES ARE TESTED.              *
C  *                  TEST THREE- TESTS SPAD, DATA-PAD, MD, AND PS.  WRITES    *
C  *                              THE ADDRESS OF MEMORY INTO THE SPECIFIED     *
C  *                              MEMORY AND CHECKS RESULTS.  8 LOCATIONS OF   *
C  *                              MAIN DATA (MD) ARE TESTED.  THE REST ARE TESTE
C  *                              IN TEST 11 & 12.                             *
C  *                  TEST FOUR - TEST THE SAME MEMORIES AS TEST 3.  RANDOM    *
C  *                              NUMBERS ARE WRITTEN, READ BACK, AND COMPARED *
C  *                              UTILIZING THE SPECIFIED MEMORY.              *
C  *                  TEST FIVE - DMA TEST (HOST TO AP).  UP TO 50   INTEGERS  *
C  *                              ARE SENT VIA DMA TO THE AP AND THEN PANELED  *
C  *                              BACK FOR CHECKING.                           *
C  *                  TEST SIX  - DMA TEST (AP TO HOST).  THE INTEGERS SENT VIA*
C  *                              DMA IN TEST 5 ARE NOW SENT VIA DMA FROM THE  *
C  *                              AP TO THE HOST.  THE SAME VALUES IN THE AP   *
C  *                              ARE PANELED BACK TO THE HOST FOR COMPARISONS.*
C  *                  TEST 7 & 8- HOST FORMAT CONVERSION TESTS.  FLOATING      *
C  *                              POINT NUMBERS ARE SENT VIA DMA USING THE     *
C  *                              FORMAT REGISTERS.  TEST 7 TRANSFERS VALUES   *
C  *                              FROM THE HOST TO THE AP AND TEST 8 TRANS-    *
C  *                              FERS VALUES FROM THE AP TO THE HOST.  THE    *
C  *                              HOST FORMAT TABLES (IN TST100) ARE USED FOR  *
C  *                              BOTH THE VALUES TO BE TRANSFERRED AND ALSO   *
C  *                              FOR COMPARISONS.                             *
C  *                              PARAMETERS:                                  *
C  *                              IMDSTT  -  CONTROL FOR HOST TO AP DMA        *
C  *                              IMDSTR  -  CONTROL FOR AP TO HOST DMA        *
C  *                              ITWC    -  WORDS TO TRANSFER (AP TO HOST)    *
C  *                              ITWC2   -  WORDS TO TRANSFER (HOST TO AP)    *
C  *                              IFPTBL  -  POINTER TO HOSFMT TABLE           *
C  *                              IANSTB  -  POINTER TO APFMT TABLE            *
C  *                  TEST 9&10 - SAME AS TEST 7 & 8 USING IBM FORMAT FOR DMA  *
C  *                              TRANSFERS.                                   *
C  *                                                                           *
C: *****************************************************************************
        INTEGER ITTI,ITTO,LFIL,ITABLC,MASK,IADRSS,ICODE,ICNT,
     +  FMTNAM,ISTAT,IRTURN,ICHNGE,WATERR,LOOPER,TYPDIS,IORST,HALT,
     +  FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,IERLIM,IPSLIM,ERRCNT,PASCNT,
     +  IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ,PSIZE,BASLOW,MDSIZ,IAPNUM
        INTEGER
     +  IECHO,IERRS,IMSIZ,IPSIZE,NPAGES,IHANS,IFSTAT,ITABL1,ITABL2,
     +  ITBL2A,ITABL3,ITABL4,HOSFMT,APFMT,ITEST6,INCHAR,JCODE(50)
        INTEGER INO1(6),INO2(6),INO3(6),INO4(6),INO5(6),INO6(6),
     +  ITESTN,I,M,J,IN,IN1,INT1,IYN2,IXN1,IYN1,IXN3,IYN3,IXN2,JKODE,
     +  MCODE,IPNLTP,NEG,DMASIZ,IBASE,DMAHI,DMAADR,LPCNT
        INTEGER STATBF,STATNM,STRAD
C
        REAL RMDSIZ,HIADDR,LOADDR,HILIM
C
        COMMON /PRNT/ ITST
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /APTST/  ITABLC,MASK,IADRSS,IDPST,ICODE,ICNT,FMTNAM(20)
C
        COMMON /FTST/   IHANS(100),IFSTAT(20)
C
        COMMON /ITABLE/ ITABL1(27),ITABL2(54),ITBL2A(9),ITABL3(70),
     1  ITABL4(15),HOSFMT(84),APFMT(126)
C
        COMMON /ERSTAT/ MCADD,MIXN,IHDFLG,JPAGE
C
        COMMON /ERORR/  IERROR
C
        COMMON /TOTAL/  ISTAT(40)
C
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
        EQUIVALENCE (ISTAT(1),WATERR),(ISTAT(2),LOOPER),
     +  (ISTAT(3),TYPDIS),(ISTAT(4),IORST),(ISTAT(5),HALT),
     +  (ISTAT(6),FFTERR),(ISTAT(7),DMPSIM),(ISTAT(8),WRTST),
     +  (ISTAT(9),FFTBAS),(ISTAT(21),PSSFLG),(ISTAT(22),IERLIM)
        EQUIVALENCE
     +  (ISTAT(23),IPSLIM),(ISTAT(24),ERRCNT),(ISTAT(25),PASCNT),
     +  (ISTAT(26),IPAGE),(ISTAT(27),TESTFG),(ISTAT(28),IXN),
     +  (ISTAT(38), ISTEP)
        EQUIVALENCE (ISTAT(29),IYN),(ISTAT(30),SIMULS),
     +  (ISTAT(31),FFTSIZ),(ISTAT(32),BASLOW),(ISTAT(33),IERRS),
     +  (ISTAT(34),IMSIZ),(ISTAT(35),IHIGH),(IFSTAT(1),IHANSP),
     +  (IFSTAT(2),IANSP),(IFSTAT(3),IANSC),(IFSTAT(4),IANSTB)
        EQUIVALENCE (IFSTAT(5),IFPTBL),(IFSTAT(6),ITWC),
     +  (IFSTAT(7),ITWC2),(IFSTAT(8),IMDSTT),(IFSTAT(9),IMDSTR),
     +  (IFSTAT(10),IEMDE),(IFSTAT(11),IEMDH),(IFSTAT(12),IEMDLO),
     +  (IFSTAT(13),IDMDE),(IFSTAT(14),IDMDH),(IFSTAT(15),IDMDL)
        EQUIVALENCE
     +  (IFSTAT(16),MA),(IFSTAT(17),IDEPMA)
C
C
C.
C..
C...
C====== INIT FORMAT TEST FLAG - SET TO 9 IF NO HOST FORMAT TEST REQUIRED
C
        ITEST6=7
C
C====== INIT VARIABLE "NEG" FOR REPRESENTATION OF -32768
C
        NEG=INOT16(32767)
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
        WRITE(ITTO,2020)
C
        CALL SETUP(1)
10      CALL SETUP(0)
        IF(IPSLIM.NE.0.AND.IPSLIM.LT.PSSFLG) PSSFLG=IPSLIM
        PSIZE=ISTAT(36)
C
        CALL APRSET
        CALL APPSEL(0,0,0)
C
C>
        IF(IECHO .NE. 0) WRITE(LFIL,2010)
        CALL SETSIZ(ITABL3,PSIZE)
C
C====== SET FLAG THAT INDICATES THAT MEMORY SIZE SPECIFIED.
C       USE IN T11&12 TO SIZE EACH BANK OF MEMORY.
C
        HILIM=PFLOAT(IHIGH)
C
C
        PASCNT=0
        ERRCNT=0
        LPCNT=0
C
        GO TO 30
C
20      LPCNT=0
        CALL I2ASCI(6,ERRCNT,INO1,10,1)
        CALL I2ASCI(6,PASCNT,INO2,10,1)
        WRITE(ITTO,2030) INO1,INO2
        IF(IECHO .NE. 0) WRITE(LFIL,2030) INO1,INO2
C
C====== THIS IS THE RE-ENTRY POINT FOR TST100
C
30      IF(PSSFLG.EQ.0) PSSFLG=1
        IF(LPCNT.EQ.PSSFLG) GO TO 20
        IF(IPSLIM.EQ.0) GO TO 40
        IF(IPSLIM.LE.PASCNT) GO TO 10
C
40      LPCNT=LPCNT+1
        PASCNT=PASCNT+1
C
C
C
C-----------------------------------------------------------------------
C
C               TEST ONE
C
C       TESTS INTERFACE AND DMA REGISTERS BY WRITING TO EACH
C       INDIVIDUAL REGISTER
C
C-----------------------------------------------------------------------
C
        IF(TESTFG.EQ.0.OR.IAND16(TESTFG,2).NE.0) GO TO 105
        GO TO 200
105     CONTINUE
        IF(WRTST .NE. 0)WRITE(ITTO,2040)
        IF(IECHO .NE. 0 .AND. WRTST .NE. 0) WRITE(LFIL,2040)
         IERRS=0
        ITST=1
        DO 140 I=1,25,3
C
C====== BYPASS TESTING MASK,APMAE,AND MAE REGISTERS IF 1 PAGE OF MEMORY
C
        IF(I.LT.19) GO TO 110
        IF(IPAGE.EQ.0) GO TO 140
110     CONTINUE
C
C====== I IS LOCATION IN TABLE1
C       MASK IS THE MASK USED TO COMPARE THE INPUT WITH OUTPUT
C
        MASK=ITABL1(I+1)
C
C====== M IS THE NUMBER OF THE REGISTER TO WRITE TO
C
        M=ITABL1(I)
C
C====== MCODE IS TEST CODE
C
        MCODE=ITABL1(I+2)
C
C====== TEST WITH ALL ZEROS
C
        CALL INOUT(I,M,0,MASK,MCODE)
C
C====== TEST WITH ALL ONES
C
         CALL INOUT(I,M,INOT16(0),MASK,MCODE)
C
C====== SLIDING BIT TEST
C
        DO 120 J=1,16,1
          CALL INOUT(I,M,INOT16(ILSH16(1,J-1)),MASK,MCODE)
120       CONTINUE
C
        DO 130 J=1,16,1
          CALL INOUT(I,M,ILSH16(1,J-1),MASK,MCODE)
130       CONTINUE
140       CONTINUE
C
        CALL APPSEL(0,0,0)
C
C-----------------------------------------------------------------------
C
C               TEST TWO
C
C       TEST TWO TESTS REGISTERS WRITTEN TO BY THE REGISTER SELECT
C       FIELD I.E. DPA,TMA,MA, ETC.
C
C-----------------------------------------------------------------------
C
200     IF(TESTFG.EQ.0.OR.IAND16(TESTFG,4).NE.0) GO TO 205
        GO TO 300
205     CONTINUE
        IF(WRTST .NE. 0)WRITE(ITTO,2050)
        IF(IECHO .NE. 0 .AND. WRTST .NE. 0) WRITE(LFIL,2050)
        ITST=2
        DO 210 ITABLC=1,52,3
C
C====== MASK IS MASK TO USE WHEN WRITING TO AP
C
        MASK=IP16(ITABL2(ITABLC+1))
C
C====== RESET AP
C
        CALL OFN(2048)
C
C====== MCODE IS TEST CODE NUMBER
C
        MCODE=ITABL2(ITABLC+2)
C
C====== IPNLTP IS REGISTER SELECT VALUE
C
        IPNLTP=ITABL2(ITABLC)
C
C====== TEST ALL ZEROS
C
        CALL PNLFN(0,MCODE,IPNLTP)
C
C====== TEST ALL ONES
C
        CALL PNLFN(IAND16(INOT16(0),MASK),MCODE,IPNLTP)
C
C====== SLIDING BIT TEST
C
        DO 210 J=1,16,1
        CALL PNLFN(IAND16(ILSH16(1,J-1),MASK),MCODE,IPNLTP)
210     CONTINUE
C
C====== SET MA TO ZERO
C
        CALL OSWR(0)
        CALL OFN(IDEPMA)
C
C>
        DO 220 ITABLC=1,7,3
C
C====== MASK IS MASKING VALUE IN COMPARISION ROUTINE
C
        MASK=IP16(ITBL2A(ITABLC+1))
C
C====== MCODE IS TEST CODE NUMBER
C
        MCODE=ITBL2A(ITABLC+2)
C
C====== IPNLTP IS REGISTER SELECT VALUE
C
        IPNLTP=ITBL2A(ITABLC)
C
C====== TEST ALL ZEROS
C
        CALL PNLFA(0,MCODE,IPNLTP)
C
C====== TEST ALL ONES
C
        CALL PNLFA(IAND16(INOT16(0),MASK),MCODE,IPNLTP)
C
C====== SLIDING BIT TEST
C
        DO 220 J=1,16,1
        CALL PNLFA(IAND16(ILSH16(1,J-1),MASK),MCODE,IPNLTP)
220     CONTINUE
C
C-----------------------------------------------------------------------
C
C               TEST THREE
C
C       BASIC ADRESS TEST FOR SPAD,DATA-PAD,MD AND PS
C
C       WRITES ADDRESS OF MEMORY INTO MEMORY AND CHECKS RESULTS
C
C       ONLY 8 LOCATIONS ARE CHECKED IN MAIN DATA REST IS CHECKED IN
C       TEST 11
C
C-----------------------------------------------------------------------
C
C       RESET AP
C
300     IF(TESTFG.EQ.0.OR.IAND16(TESTFG,8).NE.0) GO TO 305
        GO TO 400
305     CONTINUE
        IF(WRTST .NE. 0)WRITE(ITTO,2060)
        IF(IECHO .NE. 0 .AND. WRTST .NE. 0) WRITE(LFIL,2060)
        CALL OFN(2048)
        DO 350 I=1,66,5
C
C====== MASKING VALUE
C
        MASK=ITABL3(I)
C
C====== REGISTER ADDRESS CODE FOR FN REGISTER
C
        IADRSS=ITABL3(I+2)
C
C====== REGISTER SELECT VALUE
C
        IDPST=ITABL3(I+3)
C
C====== CODE IS TEST CODE NUMBER
C
        ICODE=ITABL3(I+4)
C
C====== ICNT IS THE NUMBER OF REGISTERS OR LOCATIONS IN THE
C       DIFFERENT MEMORIES.
C
        ICNT=ITABL3(I+1)
C
C====== PUT 0 IN LOC 0,1 IN LOC 1, ETC.
C
        IBASE=BASLOW
        IF(IBASE.LE.0.OR.IBASE.GE.ICNT) IBASE=1
        IF(IHIGH.GE.IBASE.AND.IHIGH.LT.ICNT) ICNT=IHIGH
310     DO 320 J=IBASE,ICNT,1
        ITMP1=J-1
        CALL WREG(ITMP1,IADRSS)
        ITMP=IDPST+512
        CALL OFN(ITMP)
C>
320     CONTINUE
C
C====== READ VALUES BACK AND COMPARE THEM
C
        DO 330 J=IBASE,ICNT,1
        ITMP=J-1
        CALL WREG(ITMP,IADRSS)
        IERRS=0
C
C====== GET VALUE IN MEMORY
C
        ITMP=IDPST+1024
        CALL RREG(IN,ITMP)
C
C====== GET ADDRESS OF MEMORY
C
        CALL ISWR(IN1)
C
C>
C====== SEE IF TYPE OUT DISABLED
C
         IF(TYPDIS .NE. 0)GO TO 340
C
C====== COMPARE TO SEE IF EQUAL AND CHECK FOR ERROR
C
        IF(IAND16(IN1-IN,MASK) .EQ. 0)GO TO 330
C
C====== ERROR HAS OCCURRED AND TYPE OUT NOT DISABLED SO GIVE ERROR
C       AND WRITE RESULTS.
C
        ERRCNT=ERRCNT+1
        IERRS=IERRS+1
        CALL I2ASCI(6,IN1,INO1,IRADX,0)
        CALL I2ASCI(6,IN,INO2,IRADX,0)
        CALL I2ASCI(6,ICODE,INO3,10,1)
        WRITE(ITTO,2070) INO3,INO1,INO2
        IF(IECHO .NE. 0) WRITE(LFIL,2070) INO3,INO1,INO2
C
C====== CHECK FOR LOOPING
C
        CALL LPTST(IRTURN)
        IF(ICHNGE .EQ. 1)CALL SETSIZ(ITABL3,PSIZE)
        IF (IRTURN .EQ. 1)GO TO 310
330     CONTINUE
340     CALL LPTST(IRTURN)
        IF(ICHNGE .EQ. 1)CALL SETSIZ(ITABL3,PSIZE)
        IF(IRTURN .EQ. 1)GO TO 310
350     CONTINUE
C
C-----------------------------------------------------------------------
C
C               TEST FOUR
C
C       SAME AS TEST THREE EXCEPT RANDOM NUMBERS ARE WRITTEN INSTEAD
C       OF ADDRESSES
C
C-----------------------------------------------------------------------
C
400     IF(TESTFG.EQ.0.OR.IAND16(TESTFG,16).NE.0) GO TO 405
        GO TO 500
405     CONTINUE
        IF(WRTST .NE. 0)WRITE(ITTO,2080)
        IF(IECHO .NE. 0 .AND. WRTST .NE. 0) WRITE(LFIL,2080)
        IYN2=IYN
        IXN2=IXN
        DO 450 I=1,66,5
C
C====== RESET AP
C
410     CALL OFN(2048)
C
C====== MASK FOR COMPARING ANSWERS
C
        MASK=ITABL3(I)
C
C====== ICNT NUMBER OF LOCATIONS OR REGISTERS
C
        ICNT=ITABL3(I+1)
C
C====== IADRSS IS MEMORY OR REGISTER
C
        IADRSS=ITABL3(I+2)
C
C====== IDPST IS REGISTER SELECT VALUE
C
        IDPST=ITABL3(I+3)
C
C====== CODE - TEST CODE VALUE
C
        ICODE=ITABL3(I+4)
C
C====== SAVE RANDOM NUMBERS FOR INDIVIDUAL RESTART VALUES
C
        IXN1=IXN
        IYN1=IYN
C
C====== PUT RANDOM NUMBERS IN TO MEMORIES
C
        IBASE=BASLOW
        IF(IBASE.LE.0.OR.IBASE.GE.ICNT) IBASE=1
        IF(IHIGH.GE.IBASE.AND.IHIGH.LT.ICNT) ICNT=IHIGH
        DO 420 J=IBASE,ICNT,1
        ITMP2=J-1
        CALL WREG(ITMP2,IADRSS)
        ITMP=IGRN16(IXN,IYN)
        ITMP1=IDPST+512
        CALL WREG(ITMP,ITMP1)
C>
420     CONTINUE
C
C====== RESTORE RANDOM NUMBER STARTING VALUE TO REGENERATE SEQUENCE
C
        IXN=IXN1
        IYN=IYN1
C
C====== READ BACK FROM MEMORIES THE VALUES
C
        DO 430 J=IBASE,ICNT,1
        ITMP1=J-1
        CALL WREG(ITMP1,IADRSS)
        ITMP=IDPST+1024
        CALL RREG(IN,ITMP)
C>
        IERRS=0
C
C====== COMPARE VALUE TO REGENRATED VALUE
C       SEE IF TYPE OUT DISABLED
C
        IF(TYPDIS .NE. 0)GO TO 440
C
C====== COMPARE ACTUAL AND EXECTED
C
        IF(IAND16(MASK,IGRN16(IXN,IYN)-IN) .EQ. 0)GO TO 430
        IERRS=IERRS+1
        ERRCNT=ERRCNT+1
C
C====== ERROR HAS OCCURRED AND TYPE OUT NOT DISABLED
C       SO WRITE OUT ERROR AND CODE
C
        IXN=IAND16(IXN,MASK)
        IN=IAND16(IN,MASK)
        CALL I2ASCI(6,IXN,INO1,IRADX,0)
        CALL I2ASCI(6,IN,INO2,IRADX,0)
        CALL I2ASCI(6,ICODE,INO3,10,1)
        IJ=J-1
        CALL I2ASCI(6,IJ,INO4,IRADX,0)
        CALL I2ASCI(6,IXN2,INO5,IRADX,0)
        CALL I2ASCI(6,IYN2,INO6,IRADX,0)
        WRITE(ITTO,2090) INO3,INO1,INO2,INO5,INO6,INO4
        IF(IECHO .NE. 0) WRITE(LFIL,2090) INO3,INO1,INO2,INO5,INO6,INO4
C
C====== CHECK FOR LOOPING
C
        CALL LPTST(IRTURN)
        IF(ICHNGE .EQ. 1)CALL SETSIZ(ITABL3,PSIZE)
        IF(IRTURN .EQ. 1)GO TO 410
430     CONTINUE
440     IERRS=0
        CALL LPTST(IRTURN)
        IF(ICHNGE .EQ. 1)CALL SETSIZ(ITABL3,PSIZE)
        IF(IRTURN .NE. 1)GO TO 450
C
C====== IF LOOPING RESET RANDOM NUMBER PARAMETERS
C
        IXN=IXN1
        IYN=IYN1
        GO TO 410
450     CONTINUE
C
C-----------------------------------------------------------------------
C
C               TEST FIVE
C
C       16 BIT INTEGER DMA TEST FROM HOST TO AP
C
C       UP TO 50 INTEGERS ARE SENT VIA DMA TO THE AP AND THEN
C       PANELED BACK FOR CHECKING
C
C-----------------------------------------------------------------------
C
500     IF(TESTFG.EQ.0.OR.IAND16(TESTFG,32).NE.0.OR.
     +    IAND16(TESTFG,64).NE.0) GO TO 505
        GO TO 700
505     CONTINUE
        IF(WRTST .NE. 0)WRITE(ITTO,2110)
        IF(IECHO .NE. 0 .AND. WRTST .NE. 0) WRITE(LFIL,2110)
C
C====== PUT RANDOM NUMBERS INTO BUFFER TO TRANSFER TO AP
C
        DO 520 I=1,50
        JCODE(I)=IGRN16(IXN,IYN)
520     CONTINUE
C
C====== START DMA TRANSFER
C
530     IBASE=BASLOW
        IF(IBASE.EQ.0) IBASE=1
        DMAHI=IHIGH
        IF(DMAHI.EQ.0) DMASIZ=50
        DMASIZ=IADD16(ISUB16(DMAHI,IBASE),1)
        IF(DMASIZ.LE.0.OR.DMASIZ.GT.50) DMASIZ=50
        CALL APPUT(JCODE,IBASE,DMASIZ,1)
        CALL APWD
C
C>
C====== READ DATA BACK THROUGH PANEL AND CHECK
C
        DMAADR=IBASE
        IERRS=0
        DO 550 J=1,DMASIZ
C
C====== GET ACTUAL VALUE TRANSFERRED  TO AP
C
        CALL WREG(DMAADR,IDEPMA)
        CALL RREG(IN,1085)
C
C>
C====== CHECK TO SEE IF EQUAL TO VALUE EXPECTED
C
        IF(IAND16(JCODE(J),INOT16(0)).EQ. IN)GO TO 540
        ERRCNT=ERRCNT+1
C
C====== NOT SAME - ERROR - SEE IF TYPE OUT DISABLED
C
        IF(TYPDIS .NE. 0)GO TO 540
C
C====== ERROR HAS OCCURRED AND TYPE OUT NOT DISABLED SO TYPE OUT
C       ERROR AND CODE NUMBER
C
        IERRS=IERRS+1
        CALL I2ASCI(6,DMAADR,INO1,IRADX,0)
        CALL I2ASCI(6,JCODE(J),INO2,IRADX,0)
        CALL I2ASCI(6,IN,INO3,IRADX,0)
        WRITE(ITTO,2120)INO2,INO3,INO1
        IF(IECHO .NE. 0) WRITE(LFIL,2120) INO2,INO3,INO1
C
C====== SEE IF LOOPING IS DESIRED
C
540     DMAADR=DMAADR+1
550     CONTINUE
        CALL LPTST(IRTURN)
        IF(IRTURN .EQ. 1)GO TO 530
C
C-----------------------------------------------------------------------
C
C               TEST SIX
C
C       16 BIT INTEGER DMA TEST FROM AP TO HOST
C
C       THE UP TO 50 INTEGERS SENT VIA DMA IN TEST FIVE ARE NOW
C       SENT VIA DMA FROM THE AP TO THE HOST AND CHECKED
C
C-----------------------------------------------------------------------
C
C600    CONTINUE
        IF(WRTST .NE. 0)WRITE(ITTO,2130)
        IF(IECHO .NE. 0 .AND. WRTST .NE. 0) WRITE(LFIL,2130)
        DO 610 I=1,50
610     JCODE(I)=0
C
C====== START TRANSFER FROM AP
C
620     IBASE=BASLOW
        IF(IBASE.EQ.0) IBASE=1
        DMAHI=IHIGH
        IF(DMAHI.EQ.0) DMASIZ=50
        DMASIZ=ISUB16(DMAHI,IBASE)+1
        IF(DMASIZ.LE.0.OR.DMASIZ.GT.50) DMASIZ=50
        CALL APGET(JCODE,IBASE,DMASIZ,1)
        CALL APWD
C
C>
C====== READ NUMBER FROM AP THROUGH FRONT PANEL AND COMPARE WITH
C       NUMBER TRANSFER FROM AP THROUGH DMA TRANSFER
C
        DMAADR=IBASE
        IERRS=0
        DO 640 I=1,DMASIZ
C
C====== GET NUMBER THROUGH FRONT PANEL (EXPECTED)
C
        CALL WREG(DMAADR,IDEPMA)
        CALL RREG(IN,IEMDLO)
C
C>
C====== CHECK TO SEE IF EQUAL TO TRANSFERRED NUMBER
C
        IF(IAND16(JCODE(I),INOT16(0)) .EQ.IN)GO TO 630
        ERRCNT=ERRCNT+1
C
C====== ERROR HAS OCCURRED - SEE IF TYPE OUT DISABLED
C
        IF(TYPDIS .NE. 0)GO TO 630
C
C====== TYPEOUT NOT DISABLED AND ERROR HAS OCCURRED SO TYPE THEM OUT
C
        IERRS=IERRS+1
        CALL I2ASCI(6,DMAADR,INO1,IRADX,0)
        CALL I2ASCI(6,IN,INO2,IRADX,0)
        CALL I2ASCI(6,JCODE(I),INO3,IRADX,0)
        WRITE(ITTO,2140) INO2,INO3,INO1
        IF(IECHO .NE. 0) WRITE(LFIL,2140)  INO2,INO3,INO1
C
C====== SEE IF LOOPING IS DESIRED
C
630     DMAADR=DMAADR+1
640     CONTINUE
        CALL LPTST(IRTURN)
        IF(IRTURN .EQ. 1)GO TO 620
C
C-----------------------------------------------------------------------
C
C               TESTS SEVEN AND EIGHT
C
C       HOST FORMAT CONVERSION TESTS
C
C       TEST SEVEN SENDS HOST FLOATING POINT VALUES TO AP
C       TEST EIGHT SENDS AP FLOATING POINT VALUES TO HOST
C
C       PROPER FORMAT CONVERSION IS CHECKED WITH THE HOSFMT TABLE
C       IF PARAMETER ITEST6 IS EQUAL TO 9 THIS TEST IS NOT PERFORMED
C
C       PARAMETERS:
C                       IMDSTT  -       CONTROL FOR HOST TO AP DMA
C                       IMDSTR  -       CONTROL FOR AP TO HOST DMA
C                       ITWC    -       WORDS TO TRANSFER AP TO HOST
C                       ITWC2   -       WORDS TO TRANSFER HOST TO AP
C                       IFPTBL  -       POINTER TO HOSFMT TABLE
C                       IANSTB  -       POINTER TO APFMT TABLE
C
C-----------------------------------------------------------------------
C
700     IF(TESTFG.EQ.0.OR.IAND16(TESTFG,128).NE.0.OR.
     +    IAND16(TESTFG,256).NE.0) GO TO 705
        GO TO 900
705     CONTINUE
        IF(ITEST6 .EQ. 9)GO TO 900
        IF(WRTST .NE. 0)WRITE(ITTO,2150)
        IF(IECHO .NE. 0 .AND. WRTST .NE. 0) WRITE(LFIL,2150)
C
C====== IFPTBL POINTS TO LOCATION IN HOSFMT TABLE
C
        IFPTBL=1
C
C====== ITWC IS THE NUMBER OF WORDS TO TRANSFER
C       FROM AP TO HOST
C
        ITWC=8
C
C====== IMDSTT IS VALUE PUT IN CONTROL REGISTER FOR TRANSFER FROM HOST.
C       IMDSTT IS FORMAT 2, AND FROM HOST TO AP
C       FORMAT 2 IS HOST FORMAT.
C
        IMDSTT=197
C
C====== IANSTB IS POINTER TO LOCATION TO START IN APFMT TABLE
C
        IANSTB=1
C
C====== ITWC2 IS NUMBER OF WORDS TO TRANSFER TO AP
C
        ITWC2=19
C
C====== IMDSTR IS CONTROL REGISTER SETTING
C       FOR TRANFERRES FROM AP TO HOST WITH FORMAT 2
C
        IMDSTR=229
C
C====== SETUP DONE - START FLOATING POINT AND FORMAT TEST
C
        CALL FTEST
C
C-----------------------------------------------------------------------
C
C               TESTS NINE AND TEN
C
C       TEST NINE SENDS IBM FLOATING POINT VALUES VIA DMA TO THE AP
C       TEST TEN SENDS AP FLOATING POINT VALUES VIA DMA TO THE HOST
C
C       PARAMETERS:
C                       USES SAME VARIABLES AS TESTS SEVEN AND EIGHT
C                       WITH DIFFERENT VALUES
C
C-----------------------------------------------------------------------
C
900     IF(TESTFG.EQ.0.OR.IAND16(TESTFG,512).NE.0.OR.
     +    IAND16(TESTFG,1024).NE.0) GO TO 905
        GO TO 30
905     CONTINUE
         IF(WRTST .NE. 0)WRITE(ITTO,2160)
        IF(IECHO .NE. 0 .AND. WRTST .NE. 0) WRITE(LFIL,2160)
        IFPTBL=39
        ITWC=12
C
C====== FORMAT 3 OR IBM FORMAT
C
        IMDSTT=199
        IANSTB=58
        ITWC2=23
C
C====== FORMAT 3 OR IBM FORMAT
C
        IMDSTR=231
        CALL FTEST
C
        GO TO 30
C
C====== FORMAT STATEMENTS
C
2000    FORMAT(35H  ***   TST100                  ***//)
2010    FORMAT(35H1 ***   TST100                  ***//,
     +         28H          ERROR LOGGING FILE,////)
2020    FORMAT(20H PLEASE ASSIGN AN AP)
2030    FORMAT(17H TST100 - ERRORS=,6A1,9H, PASSES=,6A1)
2040    FORMAT(42H TEST ONE:             INTERFACE REGISTERS )
2050    FORMAT(41H TEST TWO:             INTERNAL REGISTERS )
2060    FORMAT(45H TEST THREE:           MEMORY (BASIC ADDRESS) )
2070    FORMAT(/,23H TST100 - BASIC ADDRESS,/,12H ERROR CODE=,6A1,/,
     +  5X,3H E ,6A1,/,5X,3H A ,6A1)
2080    FORMAT(47H TEST FOUR:            MEMORY (RANDOM PATTERNS) )
2090    FORMAT(/,22H TST100 - RND PATTERNS,/,12H ERROR CODE=,6A1,/,
     +  5X,3H E ,6A1,/,5X,3H A ,6A1,/,6H X,Y= ,6A1,3X,6A1,4X,
     +  10H ADDRESS= ,6A1)
2110    FORMAT(38H TEST FIVE:            DMA: HOST TO AP )
2120    FORMAT(/,25H TST100 - DMA: HOST TO AP,/,5X,3H E ,6A1,/,5X,
     +  3H A ,6A1,/,13H MD ADDRESS= ,6A1)
2130    FORMAT(38H TEST SIX:             DMA: AP TO HOST )
2140    FORMAT(/,25H TST100 - DMA: AP TO HOST,/,5X,3H E ,6A1,/,5X,
     +  3H A ,6A1,/,17H HOST ARRAY LOC= ,6A1)
2150    FORMAT(38H TEST SEVEN AND EIGHT: HOST FORMAT DMA )
2160    FORMAT(37H TEST NINE AND TEN:    IBM FORMAT DMA )
        END
C
C
C****** IMCMD1 = IMMEDIATE EXECUTION COMMANDS               = REL 5.0  , NOV 79
        SUBROUTINE IMCMD(INX,INBUF,IPOS,IRADX)
C
C
        GO TO (10,20) ,INX
C
10      CALL MEMSIZ
        RETURN
C
C====== DUMP ROUTINES
C
20      CALL MEMDMP(INBUF,IPOS,IRADX)
        RETURN
C
        END
C****** ASST1 = LISTS AVAILABLE COMMANDS FOR THIS TEST      = REL 5.0  , NOV 79
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
     +  43H ECHO     - PROMPTS USER FOR ECHO FILE NAME                 /,
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
     +  36H STOP     - EXIT TO OPERATING SYSTEM                        /,
     +  40H BETWN=N  - SET PASSES BETWEEN PRINTOUTS                    /,
     +  50H ERRLIM=N - MAXIMUM ERRORS BEFORE RETURNING TO CSI          /,
     +  )
C
        WRITE(ITTO,16)
        IF(IECHO.NE.0) WRITE(LFIL,16)
C
16      FORMAT (
     +  49H PASLIM=N - PASSES TO RUN BEFORE RETURNING TO CSI           /,
     +  45H LOMEM=N  - SET LOWEST MEMORY ADDRESS TO TEST               /,
     +  46H HIMEM=N  - SET HIGHEST MEMORY ADDRESS TO TEST              /,
     +  )
C
        WRITE(ITTO,18)
        IF(IECHO.NE.0) WRITE(LFIL,18)
C
18      FORMAT (
     +  28H NAMES    - PRINT TEST NAMES                                /,
     +  46H PSSIZE=N - SETS PROGRAM SOURCE SIZE TO TEST                /,
     +  34H TESTS=N,M- SET WHICH TESTS TO RUN                          /,
     +  43H X=N      - SET "X" RANDOM NUMBER PARAMETER                 /,
     +  )
C
        WRITE(ITTO,20)
        IF(IECHO.NE.0) WRITE(LFIL,20)
C
20      FORMAT (
     +  43H Y=N      - SET "Y" RANDOM NUMBER PARAMETER                 /,
     +  34H D        - DISABLE ERROR PRINTOUT                          /,
     +  37H H        - HALT AFTER EACH TEST CASE                       /,
     +  38H I        - RESET AFTER EACH TEST CASE                      /,
     +  )
C
        WRITE(ITTO,22)
        IF(IECHO.NE.0) WRITE(LFIL,22)
C
22      FORMAT (
     +  25H L        - LOOP ON ERROR                                   /,
     +  34H W        - RETURN TO CSI ON ERROR                          /,
     +  44H PAGE     - MORE THAN ONE PAGE OF MAIN DATA                 /,
     +  43H /*       - /* CAN BE FOLLOWED BY A COMMENT                 /,
     +  )
C
        WRITE(ITTO,24)
        IF(IECHO.NE.0) WRITE(LFIL,24)
C
24      FORMAT (
     +  32H MEMSIZ   - SETS PS AND MD SIZES                            /,
     +  39H DUMP XX  - DUMPS SPECIFIED MEMORY (XX)                     /,
     +  43H STEP     - EXECUTES IN STEP MODE (DEFAULT)                 /,
     +  36H -STEP    - EXECUTES IN CHAINED MODE                        /,
     +  )
C
C
        RETURN
        END
C****** SETUP1 = SETS UP DATA FOR CSI                       = REL 5.0  , NOV 79
        SUBROUTINE SETUP(INIT)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TST100>SETUP                                             *
C  *    DATE        : NOV  4,1979                                             *
C  *    HOST SYSTEM : PRIME                                                    *
C  *                                                                           *
C  *    ENTRY       : INIT => 1 - DEFAULT PARAMETERS ARE USED IN CSI           *
C  *                          0 - ALLOWS USER INPUT IN CSI                     *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : SETUP TRANSFERS THE EXTERNAL COMMAND TABLE TO THE        *
C  *                  COMMAND STRING INTERPRETTER (CSI), A SUBROUTINE          *
C  *                  IN DGCOM.  CSI USES THESE TABLES FOR IDENTIFYING         *
C  *                  USER COMMANDS.  PARAMETERS PASSED FROM SETUP TO          *
C  *                  CSI CONTAIN INFORMATION ABOUT THE EXTERNAL TABLE,        *
C  *                  SUCH AS THE LENTH OF THE TABLE AND THE INITIALIZATION    *
C  *                  FLAG.                                                    *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C
        INTEGER  XTB(19,12),INIT,XTLEN
        COMMON /ERORR/ IERROR
        COMMON /TOTAL/  ISTAT(40)
C
        DATA XTB( 1, 1),XTB( 1, 2),XTB( 1, 3)   /1HB,1HE,1HT/
        DATA XTB( 1, 4),XTB( 1, 5),XTB( 1, 6)   /1HW,1HN,1H /
        DATA XTB( 1, 7),XTB( 1, 8),XTB( 1, 9)   /     2,    21,     1/
        DATA XTB( 1,10),XTB( 1,11),XTB( 1,12)   /     0,     0,    10/
C
        DATA XTB( 2, 1),XTB( 2, 2),XTB( 2, 3)   /1HE,1HR,1HR/
        DATA XTB( 2, 4),XTB( 2, 5),XTB( 2, 6)   /1HL,1HI,1HM/
        DATA XTB( 2, 7),XTB( 2, 8),XTB( 2, 9)   /     2,    22,    64/
        DATA XTB( 2,10),XTB( 2,11),XTB( 2,12)   /     0,     0,    10/
C
        DATA XTB( 3, 1),XTB( 3, 2),XTB( 3, 3)   /1HP,1HA,1HS/
        DATA XTB( 3, 4),XTB( 3, 5),XTB( 3, 6)   /1HL,1HI,1HM/
        DATA XTB( 3, 7),XTB( 3, 8),XTB( 3, 9)   /     2,    23,     1/
        DATA XTB( 3,10),XTB( 3,11),XTB( 3,12)   /     0,     0,    10/
C
        DATA XTB( 4, 1),XTB( 4, 2),XTB( 4, 3)   /1HL,1HO,1HM/
        DATA XTB( 4, 4),XTB( 4, 5),XTB( 4, 6)   /1HE,1HM,1H /
        DATA XTB( 4, 7),XTB( 4, 8),XTB( 4, 9)   /     2,    32,     0/
        DATA XTB( 4,10),XTB( 4,11),XTB( 4,12)   /     0,     0,     0/
C
        DATA XTB( 5, 1),XTB( 5, 2),XTB( 5, 3)   /1HH,1HI,1HM/
        DATA XTB( 5, 4),XTB( 5, 5),XTB( 5, 6)   /1HE,1HM,1H /
        DATA XTB( 5, 7),XTB( 5, 8),XTB( 5, 9)   /     2,    35,     0/
        DATA XTB( 5,10),XTB( 5,11),XTB( 5,12)   /     0,     0,     0/
C
        DATA XTB( 6, 1),XTB( 6, 2),XTB( 6, 3)   /1HP,1HS,1HS/
        DATA XTB( 6, 4),XTB( 6, 5),XTB( 6, 6)   /1HI,1HZ,1HE/
        DATA XTB( 6, 7),XTB( 6, 8),XTB( 6, 9)   /     2,    36,   256/
        DATA XTB( 6,10),XTB( 6,11),XTB( 6,12)   /     0,     0,     0/
C
        DATA XTB( 7, 1),XTB( 7, 2),XTB( 7, 3)   /1HX,1H ,1H /
        DATA XTB( 7, 4),XTB( 7, 5),XTB( 7, 6)   /1H ,1H ,1H /
        DATA XTB( 7, 7),XTB( 7, 8),XTB( 7, 9)   /     2,    28,     0/
        DATA XTB( 7,10),XTB( 7,11),XTB( 7,12)   /     0,     0,     0/
C
        DATA XTB( 8, 1),XTB( 8, 2),XTB( 8, 3)   /1HY,1H ,1H /
        DATA XTB( 8, 4),XTB( 8, 5),XTB( 8, 6)   /1H ,1H ,1H /
        DATA XTB( 8, 7),XTB( 8, 8),XTB( 8, 9)   /     2,    29,     0/
        DATA XTB( 8,10),XTB( 8,11),XTB( 8,12)   /     0,     0,     0/
C
        DATA XTB( 9, 1),XTB( 9, 2),XTB( 9, 3)   /1HN,1HA,1HM/
        DATA XTB( 9, 4),XTB( 9, 5),XTB( 9, 6)   /1HE,1HS,1H /
        DATA XTB( 9, 7),XTB( 9, 8),XTB( 9, 9)   /     3,     8,     1/
        DATA XTB( 9,10),XTB( 9,11),XTB( 9,12)   /     0,     0,     0/
C
        DATA XTB(10, 1),XTB(10, 2),XTB(10, 3)   /1HT,1HE,1HS/
        DATA XTB(10, 4),XTB(10, 5),XTB(10, 6)   /1HT,1HS,1H /
        DATA XTB(10, 7),XTB(10, 8),XTB(10, 9)   /     4,    27,     0/
        DATA XTB(10,10),XTB(10,11),XTB(10,12)   /     0,    10,    10/
C
        DATA XTB(11, 1),XTB(11, 2),XTB(11, 3)   /1HD,1H ,1H /
        DATA XTB(11, 4),XTB(11, 5),XTB(11, 6)   /1H ,1H ,1H /
        DATA XTB(11, 7),XTB(11, 8),XTB(11, 9)   /     3,     3,     0/
        DATA XTB(11,10),XTB(11,11),XTB(11,12)   /     0,     0,     0/
C
        DATA XTB(12, 1),XTB(12, 2),XTB(12, 3)   /1HH,1H ,1H /
        DATA XTB(12, 4),XTB(12, 5),XTB(12, 6)   /1H ,1H ,1H /
        DATA XTB(12, 7),XTB(12, 8),XTB(12, 9)   /     3,     5,     0/
        DATA XTB(12,10),XTB(12,11),XTB(12,12)   /     0,     0,     0/
C
        DATA XTB(13, 1),XTB(13, 2),XTB(13, 3)   /1HI,1H ,1H /
        DATA XTB(13, 4),XTB(13, 5),XTB(13, 6)   /1H ,1H ,1H /
        DATA XTB(13, 7),XTB(13, 8),XTB(13, 9)   /     3,     4,     0/
        DATA XTB(13,10),XTB(13,11),XTB(13,12)   /     0,     0,     0/
C
        DATA XTB(14, 1),XTB(14, 2),XTB(14, 3)   /1HL,1H ,1H /
        DATA XTB(14, 4),XTB(14, 5),XTB(14, 6)   /1H ,1H ,1H /
        DATA XTB(14, 7),XTB(14, 8),XTB(14, 9)   /     3,     2,     0/
        DATA XTB(14,10),XTB(14,11),XTB(14,12)   /     0,     0,     0/
C
        DATA XTB(15, 1),XTB(15, 2),XTB(15, 3)   /1HW,1H ,1H /
        DATA XTB(15, 4),XTB(15, 5),XTB(15, 6)   /1H ,1H ,1H /
        DATA XTB(15, 7),XTB(15, 8),XTB(15, 9)   /     3,     1,     0/
        DATA XTB(15,10),XTB(15,11),XTB(15,12)   /     0,     0,     0/
C
        DATA XTB(16, 1),XTB(16, 2),XTB(16, 3)   /1HP,1HA,1HG/
        DATA XTB(16, 4),XTB(16, 5),XTB(16, 6)   /1HE,1H ,1H /
        DATA XTB(16, 7),XTB(16, 8),XTB(16, 9)   /     3,    26,     0/
        DATA XTB(16,10),XTB(16,11),XTB(16,12)   /     0,     0,     0/
C
        DATA XTB(17, 1),XTB(17, 2),XTB(17, 3)   /1HM,1HE,1HM/
        DATA XTB(17, 4),XTB(17, 5),XTB(17, 6)   /1HS,1HI,1HZ/
        DATA XTB(17, 7),XTB(17, 8),XTB(17, 9)   /     5,     0,     1/
        DATA XTB(17,10),XTB(17,11),XTB(17,12)   /     0,     0,     0/
C
        DATA XTB(18, 1),XTB(18, 2),XTB(18, 3)   /1HD,1HU,1HM/
        DATA XTB(18, 4),XTB(18, 5),XTB(18, 6)   /1HP,1H ,1H /
        DATA XTB(18, 7),XTB(18, 8),XTB(18, 9)   /     5,     0,     2/
        DATA XTB(18,10),XTB(18,11),XTB(18,12)   /     0,     0,     0/
C
        DATA XTB(19, 1),XTB(19, 2),XTB(19, 3)   /1HS,1HT,1HE/
        DATA XTB(19, 4),XTB(19, 5),XTB(19, 6)   /1HP,1H ,1H /
        DATA XTB(19, 7),XTB(19, 8),XTB(19, 9)   /     3,    38,     1/
        DATA XTB(19,10),XTB(19,11),XTB(19,12)   /     0,     0,     0/
C
C.
C..
C...
        IF(INIT.NE.0) GO TO 20
C
        IERROR=0
        CALL CSI(XTB,19,ISTAT,40,0)
        GO TO 30
C
20      CALL CSI(XTB,19,ISTAT,40,1)
C
C====== STEP MODE?
C
        CALL APMODE(0)
        IF(ISTAT(38).EQ.1) CALL APMODE(1)
C
30      CONTINUE
        RETURN
        END
C****** LPTST1 = LOOP TEST FOR USER INPUT                   = REL 5.0  , NOV 79
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
        COMMON /TOTAL/  ISTAT(40)
C
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IDUMMY,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
        EQUIVALENCE (ISTAT(1),WATERR),(ISTAT(2),LOOPER),
     +  (ISTAT(3),TYPDIS),(ISTAT(4),IORST),(ISTAT(5),HALT),
     +  (ISTAT(6),FFTERR),(ISTAT(7),DMPSIM),(ISTAT(8),WRTST),
     +  (ISTAT(9),FFTBAS),(ISTAT(21),PSSFLG),(ISTAT(22),IERLIM)
        EQUIVALENCE
     +  (ISTAT(23),IPSLIM),(ISTAT(24),ERRCNT),(ISTAT(25),PASCNT),
     +  (ISTAT(26),IPAGE),(ISTAT(27),TESTFG),(ISTAT(28),IXN),
     +  (ISTAT(38), ISTEP)
        EQUIVALENCE (ISTAT(29),IYN),(ISTAT(30),SIMULS),
     +  (ISTAT(31),FFTSIZ),(ISTAT(32),BASLOW),(ISTAT(33),IERRS),
     +  (ISTAT(34),IMSIZ),(ISTAT(35),IHIGH)
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
C****** FTEST = TEST FLOATING POINT FORMATS                 = REL 5.0  , NOV 79
        SUBROUTINE FTEST
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TST100>FTEST                                             *
C  *    DATE        : NOV  4,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : ALL PARAMETERS ARE PASSED IN COMMON /FTST/               *
C  *                  PARAMETERS: ITWC   -  WORDS SENT TO AP                   *
C  *                              ITWC2  -  WORDS SENT TO HOST                 *
C  *                              IANSTB -  POINTER TO IAPTBL2 TABLE           *
C  *                              IFPTBL -  POINTER TO HOSFMT TABLE            *
C  *                              IMDSTT -  FORMAT FOR CONTROL REGISTER        *
C  *                              IMDSTR -  FORMAT FOR CONTROL REGISTER        *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : FTEST IS A TABLE DRIVEN TEST.  FLOATING POINT DMA        *
C  *                  OPERATIONS ARE DONE TO TEST THE HOST AND THE IBM         *
C  *                  FORMATTERS.  VALUES FROM THE HOST FORMAT TABLE           *
C  *                  ARE TRANSFERRED VIA DMA TO THE AP.  THESE VALUES         *
C  *                  IN THE AP ARE PANELED BACK TO THE HOST AND COMPARED      *
C  *                  TO THE VALUES IN THE AP FORMAT TABLE.  NEXT, THE         *
C  *                  VALUES IN THE AP FORMAT TABLES ARE PANELED TO THE        *
C  *                  AP.  THESE VALUES ARE TRANSFERRED TO THE HOST VIA        *
C  *                  DMA AND COMPARED TO THE VALUES IN THE HOST FORMAT        *
C  *                  TABLES.  CMP (TST100 SUBROUTINE) DOES THE ACTUAL         *
C  *                  DATA COMPARES FOR THE FORMAT TEST.                       *
C  *                                                                           *
C  *    GENERALITIES: FTEST IS USED TO TEST BOTH THE HOST AND IBM FORMATS.     *
C  *                                                                           *
C: *****************************************************************************
        INTEGER ITTI,ITTO,LFIL,ITABLC,MASK,IADRSS,ICODE,ICNT,
     +  FMTNAM,ISTAT,IRTURN,ICHNGE,WATERR,LOOPER,TYPDIS,IORST,HALT,
     +  FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,IERLIM,IPSLIM,ERRCNT,PASCNT,
     +  IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ,PSIZE,BASLOW,MDSIZ,IAPNUM
        INTEGER
     +  IECHO,IERRS,IMSIZ,IPSIZE,NPAGES,IHANS,IFSTAT,ITABL1,ITABL2,
     +  ITBL2A,ITABL3,ITABL4,HOSFMT,APFMT,IHANS1,JANSTB,ITWC2T
C
        REAL RMDSIZ
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /APTST/  ITABLC,MASK,IADRSS,IDPST,ICODE,ICNT,FMTNAM(20)
C
C
        COMMON /FTST/   IHANS(100),IFSTAT(20)
C
        COMMON /ITABLE/ ITABL1(27),ITABL2(54),ITBL2A(9),ITABL3(70),
     1  ITABL4(15),HOSFMT(84),APFMT(126)
C
        COMMON /TOTAL/  ISTAT(40)
C
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
        EQUIVALENCE (ISTAT(1),WATERR),(ISTAT(2),LOOPER),
     +  (ISTAT(3),TYPDIS),(ISTAT(4),IORST),(ISTAT(5),HALT),
     +  (ISTAT(6),FFTERR),(ISTAT(7),DMPSIM),(ISTAT(8),WRTST),
     +  (ISTAT(9),FFTBAS),(ISTAT(21),PSSFLG),(ISTAT(22),IERLIM)
        EQUIVALENCE
     +  (ISTAT(23),IPSLIM),(ISTAT(24),ERRCNT),(ISTAT(25),PASCNT),
     +  (ISTAT(26),IPAGE),(ISTAT(27),TESTFG),(ISTAT(28),IXN),
     +  (ISTAT(38), ISTEP)
        EQUIVALENCE (ISTAT(29),IYN),(ISTAT(30),SIMULS),
     +  (ISTAT(31),FFTSIZ),(ISTAT(32),BASLOW),(ISTAT(33),IERRS),
     +  (ISTAT(34),IMSIZ),(ISTAT(35),IHIGH),(IFSTAT(1),IHANSP),
     +  (IFSTAT(2),IANSP),(IFSTAT(3),IANSC),(IFSTAT(4),IANSTB)
        EQUIVALENCE (IFSTAT(5),IFPTBL),(IFSTAT(6),ITWC),
     +  (IFSTAT(7),ITWC2),(IFSTAT(8),IMDSTT),(IFSTAT(9),IMDSTR),
     +  (IFSTAT(10),IEMDE),(IFSTAT(11),IEMDH),(IFSTAT(12),IEMDLO),
     +  (IFSTAT(13),IDMDE),(IFSTAT(14),IDMDH),(IFSTAT(15),IDMDL)
        EQUIVALENCE
     +  (IFSTAT(16),MA),(IFSTAT(17),IDEPMA)
C
C
C.
C..
C...
        PSIZE=ISTAT(36)
C====== IHANSP POINTS TO ANSWERS GOTTEN BACK FROM AP
C
         IHANSP=1
10      IHANS1=IHANSP
C
C====== SET NUMBER OF ITEMS TO TRANSFER
C
        IANSC=ITWC
C
C====== IFPTBL=POINTER TO HOST VALUE
C       HOST TO AP
C       START TRANSFER
C
        CALL RUNDMA(HOSFMT(IFPTBL),0,IANSC,IMDSTT)
C
C====== WAIT UNTIL TRANSFER IS COMPLETE
C
            CALL APWD
C
C====== IANSP IS THE POINTER TO THE EXECTED ANSWERS
C
        IANSP=IANSTB
        MA=0
C
C====== SET MA = 0
C
20      CALL WREG(MA,IDEPMA)
C
C====== GET EXPONENT , HIGH MANTISSA , LOW MANTISSA (ACTUAL)
C
        CALL RREG(IHANS(IHANS1),IEMDE)
        CALL RREG(IHANS(IHANS1+1),IEMDH)
        CALL RREG(IHANS(IHANS1+2),IEMDLO)
C
C>
C====== CMP COMPARES EXPECTED WITH ACTUALS
C
        CALL CMP(3)
C
C====== CMP DETERMINES IF LOOPING IS DESIRED
C       RETURNS A 1 IN IRTURN IF LOOPING IS DESIRERED
C
        IF(IRTURN .EQ. 1)GO TO 10
C
C====== INCREAMENT MA TO NEXT ADRRESS IN MD
C
        MA=MA+1
C
C====== INCREMENT POINT TO NEXT LOCATION IN EXPECTED TABLE
C
        IANSP=IANSP+3
C
C====== DECREMENT COUNTER
C
        IANSC=IANSC-1
C
C====== HAVE ALL FORMAT CASES BEEN CHECKED ?
C
        IF(IANSC .NE. 0)GO TO 20
C
C====== CHECK TO SEE IF LOOPING IS DESIRED
C
        CALL LPTST(IRTURN)
        IF(IRTURN .EQ. 1)GO TO 10
C
C====== IANSTB POINTS AT TABLE CONTAINING EXPECTED VALUE TO
C       BE PUT INTO AP THROUGH FRONT PANEL.
C       THESE ARE THEN TRANSFERRED BACK WITH A DMA.
C
30      JANSTB=IANSTB
C
C====== ITWC2 CONTAIN NUMBER OF WORDS TO TRANSFER
C
        IANSC=ITWC2
C
C====== SET MA = 0
C
        MA=0
40      CALL WREG(MA,IDEPMA)
C
C====== PUT HIGH AND LOW MANTISSA AND EXPONENT INTO MD
C
        CALL WREG(APFMT(JANSTB),IDMDE)
        CALL WREG(APFMT(JANSTB+1),IDMDH)
        CALL WREG(APFMT(JANSTB+2),IDMDL)
C
C>
C====== INCREASE POINTER TO NEXT VALUES TO SEND TO AP
C
        JANSTB=JANSTB+3
C
C====== INCREASE MA TO NEXT ADDRESS IN MD
C
        MA=MA+1
C
C====== DECREMENT COUNTER AND CHECK TO SEE IF DONE - PUT VALUES IN AP
C
        IANSC=IANSC-1
        IF(IANSC .NE. 0)GO TO 40
C
C====== ITWC2 IS WORD COUNT
C
        ITWC2T=ITWC2
C
C====== START TRANSFER BY DMA
C
        CALL RUNDMA(IHANS(1),0,ITWC2T,IMDSTR)
C
C====== WAIT UNTIL TRANSFER IS COMPLETE.
C
            CALL APWD
C>
        IANSP=IFPTBL
C
C====== SETUP PARAMETERS FOR CMP
C
        IANSC=ITWC2
C
C====== COMPARE HOST FORMAT VERSUS TABULATED HOST FORMAT
C
50      CALL CMP(2)
C
C====== IS LOOPING DESIRED?
C
        IF(IRTURN .EQ. 1)GO TO 30
C
C====== DO NEXT CHECK
C
        IANSP=IANSP+2
        IHANSP=IHANSP+2
        IANSC=IANSC-1
C
C====== IS TEST DONE
C
        IF(IANSC .NE. 0)GO TO 50
C
C====== IS LOOPING DESIRED
C
        CALL LPTST(IRTURN)
        IF(IRTURN .EQ. 1)GO TO 30
        RETURN
        END
C
C
C****** CMP = COMPARE FLOATING POINT                        = REL 5.0  , NOV 79
        SUBROUTINE CMP(NU)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TST100>CMP                                               *
C  *    DATE        : NOV  4,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : NU => NUMBER OF COMPARISONS.  FTEST WILL SPECIFY NU.     *
C  *                  ALL THE TABLE POINTERS ARE PASSED IN /FTST/              *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : COMPARES THE ACTUAL VALUES GENERATED BY THE FLOATING     *
C  *                  POINT DMA TRANSFERS TO EITHER THE HOST FORMAT TABLE      *
C  *                  OR THE AP FORMAT TABLE.  IF NU IS 3, A TRANSFER FROM     *
C  *                  THE HOST TO THE AP IS INDICATED. IF NU IS A 1 OR A 2,    *
C  *                  A TRANSFER FROM THE AP TO THE HOST IS INDICATED.         *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER NU,ITTI,ITTO,LFIL,ITABLC,MASK,IADRSS,ICODE,ICNT,
     +  FMTNAM,ISTAT,IRTURN,ICHNGE,WATERR,LOOPER,TYPDIS,IORST,HALT,
     +  FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,IERLIM,IPSLIM,ERRCNT,PASCNT,
     +  IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ,PSIZE,BASLOW,MDSIZ,IAPNUM
        INTEGER
     +  IECHO,IERRS,IMSIZ,IPSIZE,NPAGES,IHANS,IFSTAT,ITABL1,ITABL2,
     +  ITBL2A,ITABL3,ITABL4,HOSFMT,APFMT,ICC1,ICC2,ISTART,ITHNSP
        INTEGER IFINSH,ITANSP,ITEMP,IEND,N,J,K,I,INO,INO1,IN1,INO3
C
        REAL RMDSIZ
C
        DIMENSION INO(6,3),INO1(6),IN1(6,3),INO3(6)
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /APTST/  ITABLC,MASK,IADRSS,IDPST,ICODE,ICNT,FMTNAM(20)
C
C
        COMMON /FTST/   IHANS(100),IFSTAT(20)
C
        COMMON /ITABLE/ ITABL1(27),ITABL2(54),ITBL2A(9),ITABL3(70),
     1  ITABL4(15),HOSFMT(84),APFMT(126)
C
        COMMON /TOTAL/  ISTAT(40)
C
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
        EQUIVALENCE (ISTAT(1),WATERR),(ISTAT(2),LOOPER),
     +  (ISTAT(3),TYPDIS),(ISTAT(4),IORST),(ISTAT(5),HALT),
     +  (ISTAT(6),FFTERR),(ISTAT(7),DMPSIM),(ISTAT(8),WRTST),
     +  (ISTAT(9),FFTBAS),(ISTAT(21),PSSFLG),(ISTAT(22),IERLIM)
        EQUIVALENCE
     +  (ISTAT(23),IPSLIM),(ISTAT(24),ERRCNT),(ISTAT(25),PASCNT),
     +  (ISTAT(26),IPAGE),(ISTAT(27),TESTFG),(ISTAT(28),IXN),
     +  (ISTAT(38), ISTEP)
        EQUIVALENCE (ISTAT(29),IYN),(ISTAT(30),SIMULS),
     +  (ISTAT(31),FFTSIZ),(ISTAT(32),BASLOW),(ISTAT(33),IERRS),
     +  (ISTAT(34),IMSIZ),(ISTAT(35),IHIGH),(IFSTAT(1),IHANSP),
     +  (IFSTAT(2),IANSP),(IFSTAT(3),IANSC),(IFSTAT(4),IANSTB)
        EQUIVALENCE (IFSTAT(5),IFPTBL),(IFSTAT(6),ITWC),
     +  (IFSTAT(7),ITWC2),(IFSTAT(8),IMDSTT),(IFSTAT(9),IMDSTR),
     +  (IFSTAT(10),IEMDE),(IFSTAT(11),IEMDH),(IFSTAT(12),IEMDLO),
     +  (IFSTAT(13),IDMDE),(IFSTAT(14),IDMDH),(IFSTAT(15),IDMDL)
        EQUIVALENCE
     +  (IFSTAT(16),MA),(IFSTAT(17),IDEPMA)
C
C
C.
C..
C...
        PSIZE=ISTAT(36)
C====== SETUP TO COMPARE AND SET ERROR FLAG TO SHOW NO ERRORS
C
        ICC1=NU
        ICC2=NU
        IERRS=0
C
C====== GET POINTER
C
        ITHNSP=IHANSP
        ISTART=1
C
C====== SEE IF TEST7/8 OR TEST9/10
C
        IF(IANSP .GT. 38)ISTART=11
        IFINSH=ISTART+9
C
C====== SET POINTER TO HOSFMT TABLE
C
        ITANSP=IANSP
C
C====== TWO OR THREE COMPARISONS
C
10      IF(ICC2 .EQ. 3)GO TO 20
C
C====== IF THREE COMPARISION, THEN CHECK FROM HOST TO AP.
C       COMPARE EXPECTED WITH ACTUAL.
C
        ITEMP=ISUB16(HOSFMT(ITANSP),IHANS(ITHNSP))
        IF(NU.EQ.1) ITEMP=HOSFMT(ITANSP)-IHANS(ITHNSP)
        GO TO 30
C
C====== IF TWO COMPARISION, THEN CHECK FROM AP TO HOST.
C
20      ITEMP=ISUB16(APFMT(ITANSP),IHANS(ITHNSP))
C
C====== SEE IF EQUAL
C
30      IF(ITEMP .NE. 0)GO TO 40
C
C====== POINTER+1
C
        ITANSP=ITANSP+1
        ITHNSP=ITHNSP+1
C
C====== NUMBER OF COMPARISION REMAINING
C
        ICC1=ICC1-1
C
C====== SEE IF DONE
C
        IF(ICC1 .NE. 0)GO TO 10
        IRTURN=0
        RETURN
C
C====== ERROR HAS OCCURRED
C       SEE IF TYPEOUT DISABLED
C
40      ERRCNT=ERRCNT+1
        IF(TYPDIS .NE. 0)GO TO 110
C
C====== ERROR AND TYPE OUT NOT DISABLED.
C       SETUP TO TYPE OUT ERROR AND CODE.
C
        IERRS=1
        ITANSP=ITANSP+1
C
C====== COMPUTE POINTER TO HOSFMT TABLE
C
        IEND=IANSP+ICC2-1
        N=1
        DO 80 J=IANSP,IEND,1
C
C====== CHECK NUMBER OF COMPARISION TO COMPUTE NUMBER OF NUMBERS TO PR
C
        IF(ICC2 .EQ. 3)GO TO 50
         INO1(N)=HOSFMT(J)
C
C====== CONVERT NUMBERS TO OCTAL
C
        CALL I2ASCI(6,HOSFMT(J),INO3,IRADX,0)
        GO TO 60
50       INO1(N)=APFMT(J)
        CALL I2ASCI(6,APFMT(J),INO3,IRADX,0)
60      DO 70 K=1,6
        INO(K,N)=INO1(K)
70      IN1(K,N)=INO3(K)
80      N=N+1
        WRITE(ITTO,2000)(FMTNAM(I),I=ISTART,IFINSH)
        IF(IECHO.NE.0)  WRITE(LFIL,2000)(FMTNAM(I),I=ISTART,IFINSH)
        IF(ICC2.EQ.3) WRITE(ITTO,2010) ((IN1(K,N),K=1,6),N=1,ICC2)
        IF(ICC2.EQ.3.AND.IECHO.NE.0) WRITE(LFIL,2010) ((IN1(K,N),
     +  K=1,6),N=1,ICC2)
        IF(ICC2.NE.3) WRITE(ITTO,2020) ((IN1(K,N),K=1,6),N=1,ICC2)
        IF(ICC2.NE.3.AND.IECHO.NE.0) WRITE(LFIL,2020) ((IN1(K,N),
     +  K=1,6),N=1,ICC2)
C
C====== COMPUTE SET UP FOR ACTUAL
C
        IEND=IHANSP+ICC2-1
C
C====== CONVERT TO OCTAL
C
        N=1
        DO 100 J=IHANSP,IEND
        CALL I2ASCI(6,IHANS(J),INO1,IRADX,0)
        DO 90 K=1,6
90      IN1(K,N)=INO1(K)
100     N=N+1
        WRITE(ITTO,2030)((IN1(K,N),K=1,6),N=1,ICC2)
        IF(IECHO .NE. 0) WRITE(LFIL,2030)((IN1(K,N),K=1,6),N=1,ICC2)
C
C====== CONVERT LOCATION IN TABLE TO OCTAL TO PRINT IT OUT
C
        WRITE(ITTO,2040)IANSP
        IF(IECHO .NE. 0) WRITE(LFIL,2040)IANSP
C
C====== SEE IF LOOPING IS REQUIRED
C
110     CALL LPTST(IRTURN)
        RETURN
C
C
2000    FORMAT( 10A1)
2010    FORMAT(11H HOST TO AP,/,5X,3H  E,3(3X,6A1))
2020    FORMAT(11H AP TO HOST,/,5X,3H  E,3(3X,6A1))
2030    FORMAT(5X,3H  A,3(3X,6A1))
2040    FORMAT(16H TABLE POINTER= ,I3,//)
C
C
        END
C
C****** INOUT = CHECK REGISTERS                             = REL 5.0  , NOV 79
        SUBROUTINE INOUT(I,M,JOUTPT,MASK,ICODE)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TST100>INOUT                                             *
C  *    DATE        : NOV  4,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : I => POSITION IN TABLE1                                  *
C  *                  M => SELECTED REGISTER                                   *
C  *                  JOUTPT => OUTPUT VALUE                                   *
C  *                  MASK => MASK TO USE IN COMPARISON                        *
C  *                  ICODE => CODE FOR TEST NUMBER                            *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : THE VALUE PASSED AS JOUTPT IS OUTPUT TO THE              *
C  *                  SPECIFIED INTERFACE REGISTER.  THE VALUE IS THEN         *
C  *                  READ FROM THE INTERFACE REGISTER.  THESE TWO             *
C  *                  VALUES ARE PASSED TO T01EQ (TST100 SUBROUTINE)           *
C  *                  WHERE THE COMPARE TAKES PLACE.                           *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER I,M,JOUTPT,MASK,ICODE,IOUTPT,INPUT,IRTURN
C
C.
C..
C...
C
10      IOUTPT=IAND16(JOUTPT,MASK)
C
C====== SUBROUTINE USED BY TEST1=WRITE AND READ SELECTED INTERFACE REG
C
        CALL APOUT(IOUTPT,M)
        CALL APIN(INPUT,M)
C>
        INPUT=IAND16(INPUT,MASK)
C
C====== T01EQ COMPARES ACTUAL WITH EXPECTED
C       INPUT VALUE RECEIVED FROM AP
C       IOUTPT VALUE SENT TO AP
C       ICODE IS CODE FOR TEST
C       MEANS NO MASKING IN COMPARING
C
        CALL T01EQ(INPUT,IOUTPT,ICODE,0)
        CALL LPTST(IRTURN)
        IF (IRTURN .EQ. 1)GO TO 10
        RETURN
        END
C
C
C****** PNLFN = TEST REGISTERS THROUGH PANEL                = REL 5.0  , NOV 79
        SUBROUTINE PNLFN(JOUT,ICODE,IPNLTP)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TST100>PNLFN                                             *
C  *    DATE        : NOV  4,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : JOUT => VALUE TO OUTPUT TO AP                            *
C  *                  ICODE => TEST CODE NUMBER                                *
C  *                  IPNLTP => REGISTER SELECT VALUE                          *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : WRITES TO THE SELECTED REGISTER VIA A PANEL OPERATION    *
C  *                  THEN READS THE SAME REGISTER VIA A PANEL OPERATION AND   *
C  *                  USES T01EQ (TST100 SUBROUTINE) TO COMPARE THE VALUES.    *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER JOUT,ICODE,IPNLTP,IN,IRTURN
C
C
C.
C..
C...
C====== PUT VALUE INTO AP REGISTER
C
10      ITMP=IPNLTP+512
        CALL WREG(JOUT,ITMP)
C
C====== GET VALUE BACK FROM AP REGISTER
C
        ITMP=IPNLTP+1024
        CALL RREG(IN,ITMP)
C
C>
C====== SEE IF VALUES ARE EQUAL UNDER MASKING CONDITIONS
C       IN IS VALUE GOTTEN FROM AP
C       JOUT VALUES SENT
C       ICODE CODE FOR TEST
C       MEANS UNDER MASKING CONDITIONS
C
        CALL T01EQ(IN,JOUT,ICODE,1)
C
C====== SEE IF LOOPING DESIRED
C
        CALL LPTST(IRTURN)
        IF(IRTURN .EQ. 1)GO TO 10
        RETURN
        END
C
C
C****** PNLFA = TEST MEMORIES THROUGH PANEL                 = REL 5.0  , NOV 79
        SUBROUTINE PNLFA(JOUT,ICODE,IPNLTP)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TST100>PNLFA                                             *
C  *    DATE        : NOV  4,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : JOUT => VALUE TO SEND TO THE AP                          *
C  *                  ICODE => TEST CODE NUMBER                                *
C  *                  IPNLTP => REGISTER SELECT VALUE                          *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : UTILIZED BY TEST 2 OF TST100 TO DEPOSIT AND EXAMINE      *
C  *                  MAIN DATA LOCATIONS.  THESE TRANSFERS ARE DONE BY        *
C  *                  PANEL OPERATIONS.                                        *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER JOUT,ICODE,IPNLTP,IN,IRTURN
C
C.
C..
C...
C====== SEND VALUE OUT
C
10      ITMP=IPNLTP+512
        CALL WREG(JOUT,ITMP)
C
C====== SET MA TO 0
C
        CALL WREG(0,514)
C
C====== GET VALUE BACK
C
        ITMP=IPNLTP+1024
        CALL RREG(IN,ITMP)
C
C>
C====== COMPARE EXPECTED AND ACUAL
C       IN IS ACTUAL
C       JOUT IS EXPECTED
C       ICODE IS CODE FOR TEST
C       SAYS TO DO THIS UNDER MASKING CONDITIONS
C
        CALL T01EQ(IN,JOUT,ICODE,1)
C
C====== CHECK FOR LOOPING
C
        CALL LPTST(IRTURN)
        IF(IRTURN .EQ. 1)GO TO 10
        RETURN
        END
C
C
C****** T01EQ = MASKED COMPARE                              = REL 5.0  , NOV 79
        SUBROUTINE T01EQ(INPT,JOUTPT,ICODE,I)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TST100>T01EQ                                             *
C  *    DATE        : NOV  4,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : INPT => ACTUAL VALUE                                     *
C  *                  JOUTPT => EXPECTED VALUE                                 *
C  *                  ICODE => CODE FOR TEST                                   *
C  *                  I => 1 FOR MASKING;  0 FOR NO MASKING                    *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : COMPARES INPT WITH JOUTPT UNDER THE GIVEN MASKING        *
C  *                  CONDITIONS.  IF THEY ARE NOT EQUAL, THE ARE PRINTED      *
C  *                  OUT ALONG WITH THE APPROPRIATE ERROR CODE.               *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER INPT,JOUTPT,ICODE,I,ITTI,ITTO,LFIL,ITABLC,MASK,
     +  IADRSS,MCODE,ICNT,FMTNAM,ISTAT,IRTURN,ICHNGE,WATERR,LOOPER,
     +  TYPDIS,IORST,HALT,FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,IERLIM,
     +  IPSLIM,ERRCNT,PASCNT,IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ,PSIZE
        INTEGER
     +  BASLOW,MDSIZ,IAPNUM,IECHO,IERRS,IMSIZ,IPSIZE,NPAGES,IOUTPT,
     +  INPUT,IOTPUT,LCODE
C
        REAL RMDSIZ
C
        DIMENSION INPUT(6),IOTPUT(6),LCODE(6)
C
        COMMON /PRNT/ ITST
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /APTST/  ITABLC,MASK,IADRSS,IDPST,MCODE,ICNT,FMTNAM(20)
C
C
        COMMON /TOTAL/  ISTAT(40)
C
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
        EQUIVALENCE (ISTAT(1),WATERR),(ISTAT(2),LOOPER),
     +  (ISTAT(3),TYPDIS),(ISTAT(4),IORST),(ISTAT(5),HALT),
     +  (ISTAT(6),FFTERR),(ISTAT(7),DMPSIM),(ISTAT(8),WRTST),
     +  (ISTAT(9),FFTBAS),(ISTAT(21),PSSFLG),(ISTAT(22),IERLIM)
        EQUIVALENCE
     +  (ISTAT(23),IPSLIM),(ISTAT(24),ERRCNT),(ISTAT(25),PASCNT),
     +  (ISTAT(26),IPAGE),(ISTAT(27),TESTFG),(ISTAT(28),IXN),
     +  (ISTAT(38), ISTEP)
        EQUIVALENCE (ISTAT(29),IYN),(ISTAT(30),SIMULS),
     +  (ISTAT(31),FFTSIZ),(ISTAT(32),BASLOW),(ISTAT(33),IERRS),
     +  (ISTAT(34),IMSIZ),(ISTAT(35),IHIGH)
C
C.
C..
C...
        PSIZE=ISTAT(36)
C====== T01EQ CHECK TO SEE IF INPT=JOUTPT
C       SET ERROR FLAG TO SHOW NO ERRORS HAVE OCCURRED
C
         IERRS=0
        IOUTPT=JOUTPT
C
C====== CHECK TO SEE IF MASKING IS DESIRED
C
        IF (I .EQ. 0 )GO TO 10
C
C====== SEE IF ILLEGAL BITS ARE ON
C
        IF(IAND16(INOT16(MASK),INPT) .NE. 0 ) GO TO 20
        IOUTPT=IAND16(MASK,IOUTPT)
C
C====== CHECK TO SEE IF THE EXPECTED IS EQUAL TO ACTUAL
C
10      IF(INPT .EQ. IOUTPT) RETURN
        ERRCNT=ERRCNT+1
C
C====== ERROR HAS OCCURED - CHECK TYPE OUT FOR DISABLE
C
20      IF (TYPDIS .EQ. 1) RETURN
C
C====== SET ERROR FLAG
C       AND WRITE ERROR AND CODE NUMBER OUT
C
        IERRS=IERRS+1
        CALL I2ASCI(6,INPT,INPUT,IRADX,0)
        CALL I2ASCI(6,IOUTPT,IOTPUT,IRADX,0)
        CALL I2ASCI(6,ICODE,LCODE,10,1)
        WRITE(ITTO,2000) ITST,LCODE,IOTPUT,INPUT
        IF(IECHO .NE. 0) WRITE(LFIL,2000) ITST,LCODE,IOTPUT,INPUT
C
         RETURN
C
C
2000    FORMAT(/,15H TST100 - TEST ,I1,/,12H ERROR CODE=,6A1,/,5X,
     +   3H E ,6A1,/,5X,3H A ,6A1)
C
C
        END
C
C****** SETSIZ = SETS PS SIZES IN DATA TABLE                = REL 5.0  , NOV 79
        SUBROUTINE SETSIZ(TABLE,PSSIZ)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TST100>SETSIZ                                            *
C  *    DATE        : NOV  4,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : TABLE => DATA TABLE                                      *
C  *                  PSSIZ => PROGRAM SOURCE SIZE                             *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : TO PLACE PROGRAM SOURCE VALUES GATHERED FROM SIZING      *
C  *                  ROUTINES OR CSI INTO DATA TABLES.                        *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER TABLE(70),PSSIZ
C
C.
C..
C...
C====== PUT PROGRAM SOURCE SIZE INTO TABLE
C
        IF(PSSIZ.EQ.0) PSSIZ=1
        DO 10 I=37,52,5
          TABLE(I)=PSSIZ
10        CONTINUE
C
        RETURN
        END
C
C
C****** TABLEC = PDP-11 TABLEC                              = REL 5.0  , NOV 79
        SUBROUTINE TABLEC
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TST100>TABLEC                                            *
C  *    DATE        : NOV  4,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : N/A                                                      *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : TABLEC PUTS -32768 INTO THE TABLES IF NECESSARY          *
C  *                                                                           *
C  *    GENERALITIES: SOME HOSTS WILL NOT NEED THIS                            *
C  *                                                                           *
C: *****************************************************************************
        INTEGER ITABL1,ITABL2,ITBL2A,ITABL3,ITABL4,HOSFMT,APFMT,
     +  ICNTL(5)
C
        COMMON /ITABLE/ ITABL1(27),ITABL2(54),ITBL2A(9),ITABL3(70),
     1  ITABL4(15),HOSFMT(84),APFMT(126)
C
C.
C..
C...
C
        DATA ICNTL /14,22,24,28,38/
C
        DO 10 I=1,5
        J=ICNTL(I)
          HOSFMT(J)=INOT16(32767)
10        CONTINUE
        RETURN
        END
C
C
C****** APTBLK = AP BLOCK DATA                              = REL 5.0  , NOV 79
        BLOCK DATA
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : TST100>APTBLK                                            *
C  *    DATE        : NOV  4,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : N/A                                                      *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : CONTAINS DATA TO DRIVE TESTS 1,2,&3 IN TST100.           *
C  *                  INITIALIZING DATA FOR THE ISTAT ARRAY AND FOR            *
C  *                  REGISTER OPERATIONS ARE ALSO INCLUDED IN THE             *
C  *                  BLOCK DATA.  TEST POINTERS AND VALUES FOR ERROR          *
C  *                  OUTPUTS AND STAT COMMANDS CAN ALSO BE FOUND.             *
C  *                  FINALLY, THE HOST FORMAT TABLES AND THE AP               *
C  *                  FORMAT TABLES ARE INCLUDED IN THE BLOCK DATA             *
C  *                  SECTION.                                                 *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
        INTEGER ITABLC,MASK,IADRSS,ICODE,ICNT,IE,IK,ISTAT,IRTURN,ICHNGE
        INTEGER
     +  NPAGES,IHANS,IFSTAT,I,J,KX,L,M,N,K
C
        REAL RMDSIZ
C
        COMMON /APTST/  ITABLC,MASK,IADRSS,IDPST,ICODE,ICNT,IE(10),
     1  IK(10)
C
C
        COMMON /FTST/   IHANS(100),IFSTAT(20)
C
        COMMON /ITABLE/ I(27),J(54),KX(9),L(70),M(15),N(84),K(126)
C
        COMMON /TOTAL/  ISTAT(40)
C
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
C
        EQUIVALENCE (IFSTAT(1),IHANSP),(IFSTAT(2),IANSP),
     +  (IFSTAT(3),IANSC),(IFSTAT(4),IANSTB),(IFSTAT(5),IFPTBL),
     +  (IFSTAT(6),ITWC),(IFSTAT(7),ITWC2),(IFSTAT(8),IMDSTT),
     +  (IFSTAT(9),IMDSTR),(IFSTAT(10),IEMDE),(IFSTAT(11),IEMDH)
        EQUIVALENCE
     +  (IFSTAT(12),IEMDLO),(IFSTAT(13),IDMDE),(IFSTAT(14),IDMDH),
     +  (IFSTAT(15),IDMDL),(IFSTAT(16),MA),(IFSTAT(17),IDEPMA)
C
C
C====== TABLE 1
C
C       TABLE 1 IS 9 SETS OF THREE ENTRIES
C
C       THE FIRST IS THE REGISTER NUMBER TO TEST
C       THE SECOND IS THE MASK TO USE
C       THE THIRD IS THE ERROR CODE
C       THIS SEQUENCE IS REPEATED 9 TIMES
C
C
        DATA I/
     +  6,-1,0,   5,-2,1,   7,25854,2,   4,-1,3,   1,-1,4,
     +  2,4095,5,   12,63,64,   13,15,65,   14,15,66 /
C
C====== TABLE 2
C
C       TABLE 2 IS 18 SETS OF 3 WITH THE SAME ARRANGEMENT AS TABLE 2
C
        DATA J/
     +  0,4095,6,   1,15,7,   2,-1,8,   3,-1,9,   4,63,10,   5,-1,11,
     +  6,-193,12,   7,255,13,   8,-1,14,   24,-1,15,   40,-1,32622,
     +  56,-1,16,   27,1023,17,   43,4095,18,   59,-1,19,   28,1023,20,
     +  44,4095,21,   60,-1,22 /
C
C       TABLE 2A
C
C       TABLE 2A IS 3 SETS OF 3 WITH THE SAME ARRANGEMENT AS TABLE 1
C
        DATA KX/
     +  29,1023,23,   45,4095,24,   61,-1,25 /
C
C====== TABLE 3
C
C       TABLE 3 IS 14 SETS OF 5 ENTRIES
C
C       THE FIRST IS THE MASK TO USE FOR COMPARE OPERATIONS
C       THE SECOND IS THE COUNTER FOR LOCATIONS OR REGISTERS
C       THE THIRD IS THE FUNCTION REGISTER VALUE
C       THE FORTH IS THE REGISTER SELECT VALUE
C       THE FIFTH IS THE ERROR CODE VALUE
C       THIS SEQUENCE IS REPEATED 14 TIMES
C
        DATA L/
     +  -1,16,513,5,26,   1023,32,516,27,27,   4095,32,516,43,28,
     +  -1,32,516,59,29,   1023,32,516,28,30,   4095,32,516,44,31,
     +  -1,32,516,60,32,   -1,512,515,8,33,   -1,512,515,24,34,
     +  -1,512,515,40,35,   -1,512,515,56,36,   1023,50,562,29,37,
     +  4095,50,562,45,38,   -1,50,562,61,39 /
C
C====== IK CONTAINS THE TEXT " TEST 7-8 "
C
        DATA IE/ 1H ,1HT,1HE,1HS,1HT,1H ,1H7,1H-,1H8,1H  /
C
C====== IK CONTAINS THE TEXT " TEST 9-10"
C
        DATA IK/ 1H ,1HT,1HE,1HS,1HT,1H ,1H9,1H-,1H1,1H0 /
C
C====== INITIALIZING DATA FOR REGISTER OPERATIONS
C
        DATA IDMDE ,IDMDH ,IDMDL ,IEMDE ,IEMDH ,IEMDLO,IDEPMA /
     +       541   ,557   ,573   ,1053  ,1069  ,1085  ,562    /
C
C====== FORMAT TABLE DATA
C   *******
C
C TABLE RAYTB AND IBMTB TOGETHER
C RAYTB GOES FROM LOCATION 1 TO LOCATION 38
C IBMTB GOES FROM LOCATION 39 TO LOCATION 84
C
C
C THIS TABLE IS OF FLOATING POINT NUMBER TO BE DMA TO AP 120B
C EACH  WORD IS REPRESENTED BY 32 BITS
C
C  0.0
        DATA N(1),N(2)/O000000,O000000/
C  1.0
        DATA N(3),N(4)/O040200,O000000/
C  -1.0
        DATA N(5),N(6)/O140200,O000000/
C  BITS 1
        DATA N(7),N(8)/O052525,O125252/
C  BITS 2
        DATA N(9),N(10)/O125252,O052525/
C  PMAX
        DATA N(11),N(12)/O077777,O177777/
C  NMAX
        DATA N(13),N(14)/O177777,O177777/
C  MIN
        DATA N(15),N(16)/O000000,O000001/
C  -1.0
        DATA N(17),N(18)/O140200,O000000/
C  -.99999
        DATA N(19),N(20)/O140177,O177776/
C  -.99999
        DATA N(21),N(22)/O140177,O177776/
C  NMAX (OVFL)
        DATA N(23),N(24)/O177777,O177777/
C  PMAX (OVFL)
        DATA N(25),N(26)/O077777,O177777/
C  NMAX (OVFL)
        DATA N(27),N(28)/O177777,O177777/
C  NEG MIN
        DATA N(29),N(30)/O100000,O000000/
C  UNFL
        DATA N(31),N(32)/O000000,O000000/
C  UNFLP
        DATA N(33),N(34)/O000000,O000000/
C  PMAX
        DATA N(35),N(36)/O077777,O177777/
C  NMAX
        DATA N(37),N(38)/O177777,O177777/
C
C  END OF RAYTB
C
C  BEGINNING OF IBMTB
C
C  0.0
        DATA N(39),N(40)/O000000,O000000/
C  1.0
        DATA N(41),N(42)/O040420,O000000/
C  -1.0
        DATA N(43),N(44)/O140420,O000000/
C  1/2 + BITS
        DATA N(45),N(46)/O040252,O125252/
C  1/4 + BITS
        DATA N(47),N(48)/O040125,O052525/
C  1/8 + BITS
        DATA N(49),N(50)/O040052,O125252/
C  1/16 + BITS
        DATA N(51),N(52)/O040025,O052525/
C  BITS 1
        DATA N(53),N(54)/O052525,O125252/
C  BITS 2
        DATA N(55),N(56)/O125252,O052525/
C  PMAX
        DATA N(57),N(58)/O077777,O177777/
C  NMAX
        DATA N(59),N(60)/O177777,O177777/
C  MIN
        DATA N(61),N(62)/O000020,O000000/
C  -1.0
        DATA N(63),N(64)/O140420,O000000/
C
        DATA N(65),N(66)/O140377,O177776/
C  -.9999
        DATA N(67),N(68)/O140377,O177776/
C  NOVF
        DATA N(69),N(70)/O177777,O177777/
C  POVF
        DATA N(71),N(72)/O077777,O177777/
C  NMIN
        DATA N(73),N(74)/O100020,O000000/
C  0.0
        DATA N(75),N(76)/O000000,O000000/
C  OVFL
        DATA N(77),N(78)/O077777,O177777/
C  NOVFL
        DATA N(79),N(80)/O177777,O177777/
C  POVFL
        DATA N(81),N(82)/O077777,O177777/
C  POVFL
        DATA N(83),N(84)/O000000,O000000/
C
C  END OF IBMTB
C
C
C TABLE APTBL AND APTB2 TOGETHER
C APTBL GOES FROM LOCATION 1 TO LOCATION 57
C APTB2 GOES FROM LOCATION 58 TO LOCATION 126
C
C THIS TABLE IS OF VALUES PUT INTO AP THROUGH THE FRONT PANEL
C AND DMA BACK AND ALSO THOSE VALUES THAT ARE CHECK AFTER DMAS TO THE
C AP FROM THE TABLE ABOVE AND READ BACK THROUGH THE FRONT PANEL
C THEY ARE OF THE FORM EXPONENT,HIGH MANTISSA,LOW MANTISSA.
C
C
C  BEGINNING OF APTBL
C
C  0.0
        DATA K(1),K(2),K(3)/O000000,O000000,O000000/
C  1.0
        DATA K(4),K(5),K(6)/O001001,O002000,O000000/
C  -1.0
        DATA K(7),K(8),K(9)/O001001,O006000,O000000/
C  BITS 1
        DATA K(10),K(11),K(12)/O001052,O003255,O052520/
C  BITS 2
        DATA K(13),K(14),K(15)/O000725,O005255,O052530/
C  RAY PMAX
        DATA K(16),K(17),K(18)/O001177,O003777,O177770/
C  RAY NMAX
        DATA K(19),K(20),K(21)/O001177,O004000,O000010/
C  MIN
        DATA K(22),K(23),K(24)/O000600,O002000,O000010/
C  AP -1.0
        DATA K(25),K(26),K(27)/O001000,O004000,O000000/
C  RAY -1.0
        DATA K(28),K(29),K(30)/O001000,O004000,O000017/
C  -.9999
        DATA K(31),K(32),K(33)/O001000,O004000,O000020/
C  NMAX WITH LSB OVERFLOW
        DATA K(34),K(35),K(36)/O001177,O004000,O000000/
C  PMAX OVFL
        DATA K(37),K(38),K(39)/O001200,O002000,O000000/
C  NMAX OVFL
        DATA K(40),K(41),K(42)/O001400,O004000,O000000/
C  MIN NEG
        DATA K(43),K(44),K(45)/O000577,O004000,O000000/
C  UNFL
        DATA K(46),K(47),K(48)/O000400,O002000,O000000/
C  UNFL
        DATA K(49),K(50),K(51)/O000200,O002000,O000000/
C  AP MAX
        DATA K(52),K(53),K(54)/O001777,O003777,O177777/
C  AP MAX
        DATA K(55),K(56),K(57)/O001777,O004000,O000000/
C
C  END OF APTBL
C
C
C  BEGINNING OF APTB2
C
C  0.0
        DATA K(58),K(59),K(60)/O000000,O000000,O000000/
C  1.0
        DATA K(61),K(62),K(63)/O001001,O002000,O000000/
C  -1.0
        DATA K(64),K(65),K(66)/O001001,O006000,O000000/
C  1/2 + BITS
        DATA K(67),K(68),K(69)/O001000,O002525,O052520/
C  1/4 + BITS
        DATA K(70),K(71),K(72)/O000777,O002525,O052520/
C  1/8 + BITS
        DATA K(73),K(74),K(75)/O000776,O002525,O052500/
C  1/16 + BITS
        DATA K(76),K(77),K(78)/O000775,O002525,O052500/
C  BITS 1
        DATA K(79),K(80),K(81)/O001123,O002532,O125240/
C  BITS 2
        DATA K(82),K(83),K(84)/O000650,O005255,O052530/
C  PMAX
        DATA K(85),K(86),K(87)/O001374,O003777,O177770/
C  NMAX
        DATA K(88),K(89),K(90)/O001374,O004000,O000010/
C  MIB
        DATA K(91),K(92),K(93)/O000375,O002000,O000000/
C  -1.0
        DATA K(94),K(95),K(96)/O001000,O004000,O000000/
C  -1.0
        DATA K(97),K(98),K(99)/O001000,O004000,O000017/
C  -.9999
        DATA K(100),K(101),K(102)/O001000,O004000,O000020/
C  NOVFL THROUGH LSB
        DATA K(103),K(104),K(105)/O001374,O004000,O000000/
C  POVFL
        DATA K(106),K(107),K(108)/O001400,O002000,O000000/
C  NEG MIN
        DATA K(109),K(110),K(111)/O000374,O004000,O000000/
C  UNFL
        DATA K(112),K(113),K(114)/O000374,O002000,O000000/
C  AP NMAX
        DATA K(115),K(116),K(117)/O001777,O003777,O177777/
C  AP MAX
        DATA K(118),K(119),K(120)/O001777,O004000,O000000/
C  POVFL
        DATA K(121),K(122),K(123)/O001375,O002000,O000000/
C  POVFL
        DATA K(122),K(125),K(126)/O001376,O002000,O000000/
C
C  END OF APTB2
C
        END
 