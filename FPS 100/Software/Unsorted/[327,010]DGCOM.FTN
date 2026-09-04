C****** DGCOM =  DIAGNOSTIC COMMON ROUTINES = REL 5.0  , NOV 79 ***************
C****** CSI    = FPS COMMAND STRING INTERPRETER = REL 5.0  , NOV 79 ***********
        SUBROUTINE CSI(XTB,XTLEN,ISTAT,ISTLN,INIT)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : DGCOM>CSI                                                *
C  *    REV         : XXX                                                      *
C  *    VERSION     : 1.0                                                      *
C  *    DATE        : 12-JUN-79                                                *
C  *    HOST SYSTEM : ALL                                                      *
C  *                                                                           *
C  *    ENTRY       : XTB     => EXTERNAL COMMAND TABLE                        *
C  *                  XTLEN   => LENGTH OF EXTERNAL TABLE (NUMBER OF COMMANDS) *
C  *                  INIT    => FLAG USED TO INITIALIZE CSI TO DEFAULT        *
C  *                              VALUES.                                      *
C  *                             0 = NO INITIALIZATION                         *
C  *                             1 = INITIALIZE DEFAULT VALUES AND RETURN      *
C  *                                                                           *
C  *    EXIT        : CSI PASSES ITS PARAMETERS TO COMMON AREAS /TOTAL/        *
C  *                    AND /TOTL/. THE TWO ARRAYS WHICH RECEIVE PARAMETER     *
C  *                    INFORMATION ARE ISTAT AND IENTRY.                      *
C  *                                                                           *
C  *                  1. ISTAT  RECEIVES INFORMATION PROCESSED FROM THE        *
C  *                            EXTERNAL COMMAND TABLE.                        *
C  *                  2. IENTRY RECEIVES INFORMATION PROCESSED FROM THE        *
C  *                            INTERNAL COMMAND TABLE.                        *
C  *                                                                           *
C  *    FUNCTION    :                                                          *
C  *                                                                           *
C  *                                                                           *
C  *    THE FPS COMMAND STRING INTERPRETER READS COMMAND LINES FROM            *
C  *      A USER CONSOLE (OR BATCH STREAM) INTO A BUFFER "INBUF".              *
C  *      THE FORMAT FOR THE DATA READ IS HOLLERITH PACKED A1.                 *
C  *                                                                           *
C  *    "INBUF" IS PARSED FOR TOKENS (INDIVIDUAL COMMANDS OR PARAMETERS)       *
C  *      AND THE RESULTANT TOKENS ARE COMPARED FIRST WITH THE EXTERNAL        *
C  *      TABLE AND THEN WITH THE INTERNAL TABLE. IF A MATCH IS FOUND,         *
C  *      CONTROL IS PASSED TO THE APPROPRIATE PROGRAM SECTION FOR             *
C  *      PROCESSING. UPON COMPLETION ANOTHER TOKEN IS PARSED FROM "INBUF".    *
C  *      IF THERE ARE NO REMAINING TOKENS, CSI RETURNS TO THE INPUT MODE.     *
C  *                                                                           *
C  *    CSI HAS TWO ENTRY MODES. IF THE "INIT" PARAMETER IS SET TO 1 UPON      *
C  *      ENTRY, CSI SETS DEFAULT VALUES IN "ISTAT" AND "IENTRY" AND RETURNS   *
C  *      TO THE CALLING PROGRAM. IF THE "INIT" PARAMETER IS SET TO 0 UPON     *
C  *      ENTRY, CSI ENTERS ITS NORMAL PROCESSING MODE.                        *
C  *                                                                           *
C  *    THERE ARE TWO WAYS TO EXIT FROM CSI. ONE WAY IS TO EXECUTE THE RUN     *
C  *      COMMAND WHICH RETURNS CONTROL TO THE CALLING PROGRAM. THE OTHER      *
C  *      IS TO EXECUTE THE STOP COMMAND WHICH TERMINATES THE PROGRAM          *
C  *      CURRENTLY RUNNING.                                                   *
C  *                                                                           *
C  *                    ------------------------------                         *
C  *                                                                           *
C  *    "ITB" IS THE INTERNAL COMMAND TABLE USED BY CSI TO PROCESS             *
C  *      UNIVERSAL COMMANDS USED BY ALL DIAGNOSTIC PROGRAMS. THIS TABLE       *
C  *      IS TWO DIMENSIONAL AND USES THE FOLLOWING FORMAT:                    *
C  *                                                                           *
C  *    ITB(N,J)        WHERE -                                                *
C  *                                                                           *
C  *                    (N)          THE COMMAND ENTRY NUMBER.                 *
C  *                    (J) 1 TO 4 = THE FIRST FOUR CHARACTERS OF THE          *
C  *                                   COMMAND PACKED IN A1 FORMAT.            *
C  *                                                                           *
C  *    CSI KEYS OFF THE COMMAND ENTRY NUMBER VIA A COMPUTED GO TO             *
C  *      STATEMENT FOR INTERNAL COMMAND PROCESSING.                           *
C  *                                                                           *
C  *                    ------------------------------                         *
C  *                                                                           *
C  *                                                                           *
C  *    "XTB" IS THE EXTERNAL COMMAND TABLE USED BY CSI TO PROCESS             *
C  *      COMMANDS INDIVIDUAL TO THE DIAGNOSTIC CURRENTLY BEING RUN.           *
C  *      THE ACTUAL TABLE IS RESIDENT IN THE CURRENT DIAGNOSTIC MAINLINE      *
C  *      WITH A POINTER TO THE TABLE PASSED AS A CSI CALLING PARAMETER.       *
C  *                                                                           *
C  *    THIS TABLE IS TWO DIMENSIONAL AND USES THE FOLLOWING GENERAL FORMAT:   *
C  *                                                                           *
C  *    XTB(N,J)        WHERE -                                                *
C  *                                                                           *
C  *                    (N)        = THE COMMAND ENTRY NUMBER.                 *
C  *                    (J) 1 TO 6 = THE FIRST SIX CHARACTERS OF THE           *
C  *                                 COMMAND PACKED A1 FORMAT.                 *
C  *                    (J) 7      = COMMAND TYPE                              *
C  *                    (J) 8      = ISTAT POINTER                             *
C  *                    (J) 9      = DEPENDS ON COMMAND TYPE                   *
C  *                    (J) 10     = DEPENDS ON COMMAND TYPE                   *
C  *                    (J) 11     = DEPENDS ON COMMAND TYPE                   *
C  *                    (J) 12     = DEPENDS ON COMMAND TYPE                   *
C  *                                                                           *
C  *    THE FOLLOWING DESCRIBES EACH COMMAND TYPE AND ITS CORRESPONDING        *
C  *      PARAMETER VALUES:                                                    *
C  *                                                                           *
C  *                                                                           *
C  *    TYPE 1: INTEGER BIT FLAG.                                              *
C  *                                                                           *
C  *            THIS COMMAND TYPE SETS A SELECTED BIT IN AN INTEGER WORD.      *
C  *                                                                           *
C  *    (N,8)   POINTER TO ISTAT TABLE                                         *
C  *    (N,9)   0 = SET SELECTED BIT TO 1 - ALL OTHERS 0                       *
C  *            1 = SET SELECTED BIT TO 0 - ALL OTHERS 1                       *
C  *    (N,10)  0 = SET TO DEFAULT VALUE                                       *
C  *        NOT 0 = DO NOT SET TO DEFAULT VALUE                                *
C  *    (N,11)  BIT POSITION TO SET                                            *
C  *    (N,12)  N/A                                                            *
C  *                                                                           *
C  *                                                                           *
C  *    TYPE 2: INTEGER VALUE PARAMETER.                                       *
C  *                                                                           *
C  *            THIS COMMAND TYPE SETS AN INTEGER VALUE INTO ISTAT.            *
C  *              THE VALUE IS FOUND BY PARSING THE NEXT TOKEN FOUND           *
C  *              AFTER THE INTEGER TYPE COMMAND IS ENCOUNTERED.               *
C  *                                                                           *
C  *    (N,8)   POINTER TO ISTAT TABLE                                         *
C  *    (N,9)   DEFAULT VALUE                                                  *
C  *    (N,10)  MINIMUM VALUE TO ACCEPT                                        *
C  *    (N,11)  MAXIMUM VALUE TO ACCEPT                                        *
C  *            LIMITS ARE NOT EVALUATED IF (N,10) AND (N,11) ARE EQUAL.       *
C  *    (N,12)  0 = DEFAULT RADIX                                              *
C  *           >0 = USE THIS VALUE AS RADIX                                    *
C  *                                                                           *
C  *                                                                           *
C  *    TYPE 3: INTEGER FLAG (1 OR 0).                                         *
C  *                                                                           *
C  *            THIS COMMAND TYPE SETS A 1 OR 0 IN THE APPROPRIATE ISTAT       *
C  *              VARIABLE. IT IS A TRUE - FALSE COMMAND.                      *
C  *                                                                           *
C  *    (N,8)   POINTER TO ISTAT TABLE                                         *
C  *    (N,9)   DEFAULT VALUE (1 OR 0)                                         *
C  *    (N,10)  N/A                                                            *
C  *    (N,11)  N/A                                                            *
C  *    (N,12)  N/A                                                            *
C  *                                                                           *
C  *                                                                           *
C  *    TYPE 4: MULTIPLE BIT SET (INDICATES TEST NUMBERS).                     *
C  *                                                                           *
C  *            THIS COMMAND TYPE CAUSES CSI TO PARSE FURTHER TOKENS FROM      *
C  *              THE COMMAND STRING UNTIL A NON-INTEGER VARIABLE IS           *
C  *              ENCOUNTERED. EACH INTEGER TOKEN CAUSES A BIT POSITION        *
C  *              CORRESPONDING TO THE TOKEN VALUE TO BE SET AS 1 IN AN        *
C  *              APPROPRIATE ISTAT VARIABLE.                                  *
C  *                                                                           *
C  *    EXAMPLE:        TOKEN   ISTAT(N)        TEST NUMBERS                   *
C  *                                                                           *
C  *                    0       1               TEST 0                         *
C  *                    1       2               TEST 1                         *
C  *                    2       4               TEST 2                         *
C  *                    5       32              TEST 5                         *
C  *                                                                           *
C  *    (N,8)   POINTER TO ISTAT TABLE                                         *
C  *    (N,9)   EACH BIT POSITION CORRESPONDES TO A TEST                       *
C  *    (N,11)  MAXIMUM NUMBER OF BIT POSITIONS OR TESTS                       *
C  *    (N,12)  N/A                                                            *
C  *                                                                           *
C  *    NOTE:   IF THE MAXIMUM NUMBER OF BITS IS GREATER THAN 16,              *
C  *              ISTAT(N-1) WILL BE USED TO STORE EXCESS BITS.                *
C  *              IF THE MAXIMUM NUMBER OF BITS IS GREATER THAN 32,            *
C  *              ISTAT(N-2) WILL BE USED.                                     *
C  *                                                                           *
C  *    TYPE 5: IMMEDIATE COMMAND                                              *
C  *                                                                           *
C  *            THIS IS AN EXTERNAL COMMAND THAT IS EXECUTED                   *
C  *            IMMEDIATELY BY CALLING THE SUBROUTINE IMCMD. THIS              *
C  *            SUBROUTINE MUST BE SUPPLIED BY THE MAINLINE ROUTINE.           *
C  *                                                                           *
C  *    (N,8)   NOT USED                                                       *
C  *    (N,9)   INDEX TO COMPUTED GOTO IN IMCMD                                *
C  *    (N,10)-(N,12) NOT USED                                                 *
C  *                                                                           *
C: *****************************************************************************
C
C
C====== COMMON STORAGE AREA:
C
C====== INSTAL IS COMMON FOR ALL DIAGNOSTICS
C
        COMMON /INSTAL/ ITTI,ITTO,FUNIT
C
        INTEGER FUNIT
C
C====== TOTAL IS COMMON FOR ALL DIAGNOSTICS
C
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER PSIZE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
C
C
C====== LOCAL STORAGE AREA:
C
        INTEGER XTLEN,XTB(XTLEN,12),ISTAT(ISTLN),BLANK
        INTEGER INBUF(80),STR(8),N65R8(6),N65R10(6),
     +  N65R16(6),ITB(16,4),T4(160)
C
C====== ITB IS AN INTERNAL TABLE USED TO DEFINE HOLLERITH CONSTANTS
C       FOR INTERNAL COMMANDS.
C
        DATA ITB(01,1),ITB(01,2),ITB(01,3),ITB(01,4) /1HA,1HP,1HN,1HU/
        DATA ITB(02,1),ITB(02,2),ITB(02,3),ITB(02,4) /1HC,1HL,1HR,1HF/
        DATA ITB(03,1),ITB(03,2),ITB(03,3),ITB(03,4) /1HR,1H ,1H ,1H /
        DATA ITB(04,1),ITB(04,2),ITB(04,3),ITB(04,4) /1HC,1HL,1HR,1HA/
        DATA ITB(05,1),ITB(05,2),ITB(05,3),ITB(05,4) /1HE,1HC,1HH,1HO/
        DATA ITB(06,1),ITB(06,2),ITB(06,3),ITB(06,4) /1HR,1HU,1HN,1H /
        DATA ITB(07,1),ITB(07,2),ITB(07,3),ITB(07,4) /1HE,1H ,1H ,1H /
        DATA ITB(08,1),ITB(08,2),ITB(08,3),ITB(08,4) /1HH,1HE,1HL,1HP/
        DATA ITB(09,1),ITB(09,2),ITB(09,3),ITB(09,4) /1H?,1H ,1H ,1H /
        DATA ITB(10,1),ITB(10,2),ITB(10,3),ITB(10,4) /1HA,1H ,1H ,1H /
        DATA ITB(11,1),ITB(11,2),ITB(11,3),ITB(11,4) /1HR,1HA,1HD,1HI/
        DATA ITB(12,1),ITB(12,2),ITB(12,3),ITB(12,4) /1HS,1HT,1HA,1HT/
        DATA ITB(13,1),ITB(13,2),ITB(13,3),ITB(13,4) /1HS,1HT,1HO,1HP/
        DATA ITB(14,1),ITB(14,2),ITB(14,3),ITB(14,4) /1HQ,1HU,1HI,1HT/
        DATA ITB(15,1),ITB(15,2),ITB(15,3),ITB(15,4) /1HT,1HI,1HM,1HL/
        DATA ITB(16,1),ITB(16,2),ITB(16,3),ITB(16,4) /1H/,1H*,1H ,1H /
C
C====== HOLLERITH CONSTANTS FOR REPRESENTATION OF INTEGER VALUE "65536"
C
        DATA N65R8  /1H2,1H0,1H0,1H0,1H0,1H0/
        DATA N65R10 /1H ,1H6,1H5,1H5,1H3,1H6/
        DATA N65R16 /1H ,1H1,1H0,1H0,1H0,1H0/
C
        DATA BLANK /1H /
C
C.
C..
C...
C
C====== IF INIT IS OFF - PROCEED TO CSI
C
        IF(INIT.EQ.0) GO TO 20
C
C====== INIT IS ON - CLEAR ENTRY TABLE AND SET DEFAULT VALUES
C
        DO 10 I=1,20
          IENTRY(I)=0
10      CONTINUE
C
C====== SET DEFAULT RADIX AND DEFAULT APNUM
C
        IRADX=8
        IAPNUM=-1
C
C====== PROCESS AS A CLEAR-ALL TO SET DEFAULT VALUES
C
        GO TO 130
C
C
C
C
C====== HERE TO ENTER CSI - PROMPT USER VIA IOCNTL
C
20      ITEMP=IOCNTL(2,0,0,0)
C
C====== GET A COMMAND LINE FROM USER COURTESY OF IOCNTL
C
        ITEMP=IOCNTL(3,INBUF,0,0)
C
        IF(IECHO.NE.0)WRITE(FUNIT,2000)(INBUF(I),I=1,80)
C
C====== READ A TOKEN FROM COMMAND LINE
C
        IPOS=1
30      ITEMP=IRDTK(INBUF,IPOS,STR,IRADX,INUM,MINUS)
40      IF(ITEMP) 500,20,50
50      IF(ITEMP.GT.1) GO TO 500
C
C====== SEARCH FOR AN EXTERNAL COMMAND
C
        DO 70 I=1,XTLEN
C
          DO 60 J=1,6
            IF(STR(J).NE.XTB(I,J)) GOTO 70
60        CONTINUE
C
C====== EXTERNAL COMMAND
C
        K=XTB(I,7)
        IPTR=XTB(I,8)
        GOTO (340,360,380,400,480),K
C
70      CONTINUE
C
C
C===== SEARCH FOR AN INTERNAL COMMAND
C
        DO 90 I = 1,16
C
          DO 80 J=1,4
            IF(STR(J).NE.ITB(I,J)) GO TO 90
80        CONTINUE
C
C====== INTERNAL COMMAND
C
        GO TO (100,110,110,130,170,190,190,200,
     +         200,200,210,220,320,320,330,20),I
C
90      CONTINUE
C
C
C====== INVALID COMMAND
C
        GOTO 510
C
C
C====== PROCESS APNUM
C
100     IF(IRDTK(INBUF,IPOS,STR,IRADX,INUM,MINUS).NE.2) GO TO 490
        IAPNUM=INUM
        K=IOCNTL(4,0,NZMD,IAPNUM)
        IF(K.NE.0) GO TO 20
        WRITE(ITTO,2070) IAPNUM
        IF(IECHO.NE.0) WRITE(FUNIT,2070)IAPNUM
        GOTO 30
C
C====== PROCESS CLRFLG (R) COMMAND
C
110     DO 120 I=1,XTLEN
          K=XTB(I,8)
          IF(XTB(I,7).EQ.3.AND.XTB(I,10).EQ.0) ISTAT(K)=0
          IF(XTB(I,7).NE.1) GOTO 120
          IF(XTB(I,10).NE.0) GO TO 120
          IF (XTB(I,9).EQ.1)
     +    ISTAT(K)=IOR16(ISTAT(K),ILSH16(1,MOD(XTB(I,11),16)))
          IF (XTB(I,9).EQ.0)
     +    ISTAT(K)=NAND16(ISTAT(K),ILSH16(1,MOD(XTB(I,11),16)))
120     CONTINUE
        GO TO 30
C
C
C
C====== PROCESS CLRALL COMMAND
C
130     DO 140 I=1,ISTLN
          ISTAT(I)=0
140     CONTINUE
C
        DO 160 I=1,XTLEN
          K=XTB(I,8)
          IF (XTB(I,7).EQ.5) GOTO 160
          IF (XTB(I,7).NE.1) GOTO 150
          IF (XTB(I,9).EQ.1)
     +    ISTAT(K)=IOR16(ISTAT(K),ILSH16(1,MOD(XTB(I,11),16)))
          IF (XTB(I,9).EQ.0)
     +    ISTAT(K)=NAND16(ISTAT(K),ILSH16(1,MOD(XTB(I,11),16)))
          GOTO 160
150       ISTAT(K)=XTB(I,9)
160     CONTINUE
C
        IF(INIT.NE.0) RETURN
        GO TO 30
C
C
C
C====== PROCESS ECHO COMMAND
C
170     IF(MINUS.NE.0) GOTO 180
        IF(IECHO.NE.0) K=IOCNTL(9,STR,IPOS-ITEMP,FUNIT)
        IF(IOCNTL(5,STR,IPOS-ITEMP,FUNIT).NE.0) GO TO 520
        IECHO=1
        GO TO 30
C
C====== PROCESS -ECHO COMMAND
C
180     IF(IECHO.EQ.0) GO TO 30
        ITEMP=IOCNTL(9,STR,IPOS-ITEMP,FUNIT)
        IECHO=0
        GO TO 30
C
C
C
C====== PROCESS RUN COMMAND
C
190     CONTINUE
        IF(IAPNUM.LT.0) GO TO 530
        RETURN
C
C
C
C====== PROCESS HELP (?) (A) COMMAND
C
200     CALL ASSIST(ITTO,FUNIT,IECHO)
        GO TO 30
C
C
C
C====== PROCESS RADIX COMMAND
C
210     IF(IRDTK(INBUF,IPOS,STR,10,INUM,MINUS).NE.2) GO TO 490
        IF(INUM.NE.8.AND.INUM.NE.10.AND.INUM.NE.16) GO TO 490
        IRADX=INUM
        GO TO 30
C
C
C
C====== PROCESS STAT COMMAND
C
C
C====== WRITE OUT APNUM AND RADIX FIRST
C
220     WRITE(ITTO,2110) IAPNUM
        IF (IECHO.NE.0) WRITE(FUNIT,2110) IAPNUM
C
        WRITE(ITTO,2120)IRADX
        IF(IECHO.NE.0) WRITE(FUNIT,2120) IRADX
C
C====== LOOP FOR EACH ENTRY IN EXTERNAL COMMAND TABLE
C
        DO 310 I=1,XTLEN
          IR=IRADX
          IF(XTB(I,12).NE.0) IR=XTB(I,12)
          INDEX=XTB(I,8)
          JMP=XTB(I,7)
          GOTO (230,260,270,280,310),JMP
C
C====== TYPE 1  BIT FLAG TYPE
C
230       ITEMP=INDEX-XTB(I,11)/16
          NB=0
          IF(IAND16(ISTAT(ITEMP),ILSH16(1,MOD(XTB(I,11),16))).NE.0)NB=1
240       IF(NB.EQ.0) GOTO 250
          WRITE(ITTO,2090)(XTB(I,K),K=1,6)
          IF(IECHO.NE.0) WRITE(FUNIT,2090)(XTB(I,K),K=1,6)
          GOTO 310
250       WRITE(ITTO,2100)(XTB(I,K),K=1,6)
          IF(IECHO.NE.0) WRITE(FUNIT,2100)(XTB(I,K),K=1,6)
          GOTO 310
C
C====== TYPE 2 INTEGER NUMBERS
C
260       CALL I2ASCI(6,ISTAT(INDEX),STR,IR,1)
          WRITE(ITTO,2060)(XTB(I,K),K=1,6),(STR(J),J=1,6)
          IF(IECHO.NE.0)WRITE(FUNIT,2060)(XTB(I,K),K=1,6),(STR(J),J=1,6)
          GOTO 310
C
C====== TYPE 3  INTEGER FLAGS
C
270       NB=0
          IF(ISTAT(INDEX).NE.0) NB=1
          GOTO 240
C
C====== TYPE 4 - REQUIRES DECODING OF THE VALUE INTO BIT POSITIONS
C
280       L=0
          MBIT=XTB(I,11)+1
          DO 290 J=1,MBIT
            NB=J-1
            ITEMP=INDEX-NB/16
            K=IAND16(ISTAT(ITEMP),ILSH16(1,MOD(NB,16)))
            IF(K.EQ.0) GOTO 290
            L=L+1
            T4(L)=NB
290       CONTINUE
C
          IF(L.GT.0) GOTO 300
          WRITE(ITTO,2050)(XTB(I,K),K=1,6)
          IF(IECHO.NE.0) WRITE(FUNIT,2050)(XTB(I,K),K=1,6)
          GOTO 310
300       WRITE(ITTO,2050)(XTB(I,K),K=1,6),(T4(J),J=1,L)
          IF(IECHO.NE.0)WRITE(FUNIT,2050)(XTB(I,K),K=1,6),(T4(J),J=1,L)
310     CONTINUE
        GO TO 30
C
C
C
C====== PROCESS STOP (QUIT) COMMAND
C
320     IF(IECHO.NE.0) ITEMP=IOCNTL(9,STR,0,FUNIT)
        CALL APRLSE
        CALL EXIT
C
C
C
C====== PROCESS TIMLIM COMMAND
C
330     WRITE(ITTO,2010)
        IF(IECHO.NE.0) WRITE(FUNIT,2010)
        GO TO 30
C
C
C
C====== PROCESS BIT FLAGS ( TYPE 1 )
C
340     ITEMP=IPTR-XTB(I,11)/16
        IF (MINUS.NE.0) GOTO 350
        ISTAT(ITEMP)=IOR16(ISTAT(ITEMP),ILSH16(1,MOD(XTB(I,11),16)))
        GOTO 30
C
C====== PROCESS -BIT FLAGS
C
350     ISTAT(ITEMP)=NAND16(ISTAT(ITEMP),ILSH16(1,MOD(XTB(I,11),16)))
        GOTO 30
C
C
C
C====== PROCESS INTEGER COMMANDS (TYPE 2 )
C
360     IRX=10
        IF(XTB(I,12).EQ.0) IRX=IRADX
        IF(IRDTK(INBUF,IPOS,STR,IRX,INUM,MINUS).NE.2) GO TO 490
        IF(XTB(I,10).EQ.XTB(I,11)) GOTO 370
        IF(INUM.LT.XTB(I,10)) GOTO 490
        IF(INUM.GT.XTB(I,11)) GOTO 490
370     ISTAT(IPTR)=INUM
        ICHNGE=1
        GO TO 30
C
C
C
C====== PROCESS INTEGER FLAG COMMANDS ( TYPE 3 )
C
380     IF(MINUS.NE.0)GOTO  390
        ISTAT(IPTR)=1
        GO TO 30
C
390     ISTAT(IPTR)=0
        GO TO 30
C
C
C
C====== PROCESS N-BIT COMMANDS (COMMAND=N, N, N) ( TYPE 4 )
C
400     NWRD=(XTB(I,11)/(15.0-1.0))+1.0
C
        DO 410 N=1,NWRD
          ITEMP=IPTR-N+1
          ISTAT(ITEMP)=0
410     CONTINUE
C
        IF(MINUS.NE.0)GOTO 440
C
420     ITEMP=IRDTK(INBUF,IPOS,STR,10,INUM,MINUS)
        IF (ITEMP) 490,20,430
430     IF (ITEMP.NE.2) GOTO 40
        IF(INUM.GT.XTB(I,11))GOTO 490
        K=IPTR-INUM/16
        NB=ILSH16(1,MOD(INUM,16))
        ISTAT(K)=IOR16(ISTAT(K),NB)
        GOTO 420
C
C====== PROCESS -NBIT COMMANDS (-COMMAND=N, N, N)
C
440     DO 450 N=1,NWRD
          ITEMP=IPTR-N+1
          ISTAT(ITEMP)=INOT16(ISTAT(ITEMP))
450     CONTINUE
460     ITEMP=IRDTK(INBUF,IPOS,STR,10,INUM,MINUS)
        IF (ITEMP) 490,20,470
470     IF(ITEMP.NE.2) GO TO 40
        IF(INUM.GT.XTB(I,11)) GOTO 490
        K=IPTR-INUM/16
        NB=ILSH16(1,MOD(INUM,16))
        ISTAT(K)=NAND16(ISTAT(K),NB)
        GOTO 460
C
C====== TYPE (5) IMMEDIATE EXECUTION COMMANDS
C
480     CALL IMCMD(XTB(I,9),INBUF,IPOS,IRADX)
        GOTO 30
C
C
C
C====== ERROR PROCESSING
C
C
C====== NUMERIC ERROR
C
490     WRITE(ITTO,2020)
        IF(IECHO.NE.0) WRITE(FUNIT,2020)
        GO TO 20
C
C====== SYNTAX ERROR
C
500     WRITE(ITTO,2030)
        IF(IECHO.NE.0) WRITE(FUNIT,2030)
        GO TO 20
C
C====== ILLEGAL COMMAND
C
510     WRITE(ITTO,2040)
        IF(IECHO.NE.0) WRITE(FUNIT,2040)
        GO TO 20
C
C====== FILE INTITALIZATION ERROR
C
520     WRITE(ITTO,2080)
        IF(IECHO.NE.0) WRITE(FUNIT,2080)
        GO TO 20
C
C====== AP NOT ASSIGNED
C
530     WRITE(ITTO,2130)
        IF(IECHO.NE.0) WRITE(FUNIT,2130)
        GO TO 20
C
C
C
C====== FORMATS
C
2000    FORMAT(/,1X,2H> ,80A1,//)
2010    FORMAT(46H TIMLIM COMMAND NOT SUPPORTED IN THIS REVISION)
2020    FORMAT(14H NUMERIC ERROR)
2030    FORMAT(13H SYNTAX ERROR)
2040    FORMAT(16H ILLEGAL COMMAND)
2050    FORMAT(1H ,6A1,3H = ,16(I3,1X)/(1H ,9X,16(I3,1X)))
2060    FORMAT(1H ,6A1,3H = ,6A1)
2070    FORMAT(6H AP   ,I2,9H ASSIGNED)
2080    FORMAT(26H FILE INITIALIZATION ERROR)
2090    FORMAT(1H ,6A1,9H =     ON)
2100    FORMAT(1H ,6A1,9H =    OFF)
2110    FORMAT(/,1H ,9HAPNUM  = ,I6)
2120    FORMAT(1H ,9HRADIX  = ,I6)
2130    FORMAT(20H PLEASE ASSIGN AN AP)
C
        END
C****** IRDTK = READS A TOKEN FROM COMMAND STRING = REL 5.0  , NOV 79 *********
        INTEGER FUNCTION IRDTK(INBUF,IPOS,ITOK,IRADX,INUM,MINFLG)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : DGCOM>IRDTK                                              *
C  *    REV         : XXX                                                      *
C  *    VERSION     : 0.0                                                      *
C  *    DATE        : 11-JUN-79                                                *
C  *    HOST SYSTEM : ALL                                                      *
C  *                                                                           *
C  *    ENTRY       : INBUF   => STRING TO BE PARSED                           *
C  *                  IPOS    => STARTING CHARACTER OF TOKEN                   *
C  *                  ITOK    => N/A                                           *
C  *                  IRADX   => RADIX FOR NUMERIC VALUES                      *
C  *                  INUM    => N/A                                           *
C  *                  MINFLG  => N/A                                           *
C  *                                                                           *
C  *    EXIT        : INBUF   <= N/A                                           *
C  *                  IPOS    <= UPDATED POSITION IN "INBUF"                   *
C  *                  ITOK    <= RESULTANT TOKEN                               *
C  *                  INUM    <= INTEGER VALUE OF NUMERIC TOKEN                *
C  *                  MINFLG  <= SET TO 1 IF FIRST LETTER OF TOKEN BEGAN       *
C  *                               WITH A MINUS SIGN                           *
C  *                             OTHERWISE SET TO 0                            *
C  *                                                                           *
C  *    FUNCTION    : READS A TOKEN FROM STRING "INBUF" AND THEN UPDATES       *
C  *                    A POSITION COUNTER FOR NEXT TOKEN.                     *
C  *                                                                           *
C  *                                                                           *
C  *                  FUNCTION VALUES RETURNED:                                *
C  *                                                                           *
C  *                          0 IF A NULL TOKEN                                *
C  *                          1 IF TOKEN WAS ALPHANUMERIC                      *
C  *                          2 IF TOKEN WAS NUMERIC                           *
C  *                         -1 IF A NUMERIC ERROR                             *
C  *                                                                           *
C  *    GENERALITIES: NONE                                                     *
C  *                                                                           *
C: *****************************************************************************
C
        INTEGER INBUF(80),ITOK(9999),IN(16)
C
        DATA KBLANK,KCOMMA,KEQUAL,MINUS /1H ,1H,,1H=,1H- /
        DATA IN /1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,1HA,
     +           1HB,1HC,1HD,1HE,1HF /
C.
C..
C...
C
C====== INITIALIZE VALUES AND BUFFERS
C
        IRDTK=0
        MINFLG=0
        NUMFLG=0
        INUM=0
C
        DO 10 I=1,6
          ITOK(I)=KBLANK
10      CONTINUE
C
C
C====== SCAN FOR NON-DELIMITER. IF NONE FOUND RETURN NULL VALUE
C       IF NUMERIC DELIMITER FOUND SET NUMFLG
C
C
        IF(IPOS.GT.80)RETURN
C
        DO 20 NFIRST=IPOS,80
C
          IF (INBUF(NFIRST).NE.KBLANK.AND.
     +        INBUF(NFIRST).NE.KCOMMA.AND.
     +        INBUF(NFIRST).NE.KEQUAL   ) GOTO 30
C
          IF (INBUF(NFIRST).EQ.KCOMMA.OR.
     +        INBUF(NFIRST).EQ.KEQUAL   ) NUMFLG=1
20      CONTINUE
C
        RETURN
C
C
C====== SCAN FOR DELIMITER TO FIND END OF TOKEN
C
30      DO 40 LAST = NFIRST,80
C
          IF (INBUF(LAST).EQ.KBLANK.OR.
     +        INBUF(LAST).EQ.KCOMMA.OR.
     +        INBUF(LAST).EQ.KEQUAL   ) GOTO 50
C
40      CONTINUE
C
        LAST=81
C
50      LAST=LAST-1
C
C====== IF NUMERIC TOKEN (NUMFLG=1) SKIP TO PROCESS NUMBERS
C
        IF(NUMFLG.EQ.1) GOTO 70
C
C====== MUST BE ALPHANUMERIC - RETURN TOKEN
C
        IF (INBUF(NFIRST).EQ.MINUS) MINFLG=1
        NFIRST=NFIRST+MINFLG
C
        J=(LAST-NFIRST)+1
        I=NFIRST
        DO 60 K=1,J
          ITOK(K)=INBUF(I)
          IF(I.GT.LAST) ITOK(J)=KBLANK
          I=I+1
60      CONTINUE
C
C====== POSITION POINTER AND RETURN
C
        IRDTK=1
        GO TO 110
C
C====== CONVERT NUMERIC STRING TO UNSIGNED INTEGER VALUE
C
70      FI=0.0
C
C====== VERIFY LEGAL VALUE IN PROPER RADIX
C
        DO 100 I=NFIRST,LAST
          DO 80 J=1,IRADX
            IF(INBUF(I).EQ.IN(J)) GO TO 90
80        CONTINUE
C
C====== COULDNT FIND VALUE - REPORT ERROR
C
          IRDTK=-1
          RETURN
C
C====== PROCESS IN FLOATING POINT FORMAT
C
90        J=J-1
          FI=FI*IRADX+J
100     CONTINUE
C
C====== THEN CONVERT TO UNSIGNED INTEGER AND RETURN
C
        INUM=IPFIX(FI)
        IRDTK=2
C
110     IPOS=LAST+1
        RETURN
        END
C****** I2ASCI = INTEGER TO ASCI CONVERSION ROUTINE = REL 5.0  , NOV 79 *******
        SUBROUTINE I2ASCI(LEN,IVAL,ISTR,IR,MODE)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : DGCOM>I2ASCI                                             *
C  *    REV         : XXX                                                      *
C  *    VERSION     : 0.0                                                      *
C  *    DATE        : 11-JUN-79                                                *
C  *    HOST SYSTEM : ALL                                                      *
C  *                                                                           *
C  *    ENTRY       : IVAL    => INTEGER VALUE TO BE CONVERTED                 *
C  *                  ISTR    => N/A                                           *
C  *                  IR      => RADIX TO USE IM MAKING CONVERSION             *
C  *                  LEN     => LENGTH OF HOLLERITH STRING TO RETURN          *
C  *                                                                           *
C  *    EXIT        : IVAL    <= N/A                                           *
C  *                  ISTR    <= BUFFER CONTAINING A1 FORMATTED CONVERTED      *
C  *                               INTEGER                                     *
C  *                  IR      <= N/A                                           *
C  *                  LEN     <= N/A                                           *
C  *                                                                           *
C  *    FUNCTION    : TREATS AN INTEGER VALUE AS UNSIGNED AND CONVERTS TO      *
C  *                    HOLLERITH REPRESENTATION.                              *
C  *                                                                           *
C  *    GENERALITIES: NONE                                                     *
C  *                                                                           *
C: *****************************************************************************
C
        INTEGER IDB(16),ISTR(40)
C
        DATA IDB /1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,
     +            1H9,1HA,1HB,1HC,1HD,1HE,1HF /
C
        DATA IBLNK / 1H  /
C.
C..
C...
C
        FNUM=IVAL
C
C====== DON'T CHECK FOR MODULO 65536 IF > 6 DIGITS REQUIRED
C
        IF(LEN.GT.6) GO TO 10
C
C====== IF SIGN BIT SET ADD 65536
C
        IF(IVAL.LT.0) FNUM=FNUM+65536.0
        IF(IVAL.EQ.-32767-1) FNUM=32768.0
C
C====== GET REAL RADIX VALUE
C
10      RADIX=IR
C
C====== NUMBER CONVERSION ALGORITHM
C
        DO 20 I=1,LEN
          K=(LEN+1)-I
          N=AMOD(FNUM,RADIX)
          FNUM=FNUM/RADIX
          ISTR(K)=IDB(N+1)
20      CONTINUE
C
C====== IF MODE=0 DO NOT BLANK LEADING ZEROS
C
        IF(MODE.EQ.0) GO TO 60
C
C====== DETERMINE NUMBER OF LEADING ZEROS
C
        L=0
        DO 30 I=1,LEN
          IF(ISTR(I).EQ.IDB(1)) L=L+1
          IF(ISTR(I).NE.IDB(1)) GO TO 40
30      CONTINUE
C
C====== BLANK THE LEADING ZEROS
C
40      IF(L.EQ.0) GO TO 60
        IF(L.EQ.LEN) L=LEN-1
        DO 50 I=1,L
          ISTR(I)=IBLNK
50      CONTINUE
C
C====== HERE TO EXIT ROUTINE
C
60      CONTINUE
        RETURN
        END
C****** MEMSIZ = SIZE AP MEMORY = REL 5.0  , NOV 79 ***************************
        SUBROUTINE MEMSIZ
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : MEMSIZ                                                   *
C  *    REV         : 0                                                        *
C  *    VERSION     : 0                                                        *
C  *    DATE        : 1 AUG 79                                                 *
C  *                                                                           *
C  *    ENTRY       : IRADX IS THE CHOSEN PRINT RADIX                          *
C  *                                                                           *
C  *                                                                           *
C  *    FUNCTION    : DETERMINE AND PRINT AP MEMORY SIZE                       *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C
C====== COMMON STORAGE AREA:
C
C====== INSTAL IS COMMON FOR ALL DIAGNOSTICS
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
C====== TOTL IS COMMON FOR ALL DIAGNOSTICS
C
        COMMON /TOTL / IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        INTEGER PSIZE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
C====== LOCAL STORAGE:
C
       INTEGER STR(6),N65R8(6),N65R10(6),N65R16(6)
        DATA N65R8  /1H2,1H0,1H0,1H0,1H0,1H0/
        DATA N65R10 /1H ,1H6,1H5,1H5,1H3,1H6/
        DATA N65R16 /1H ,1H1,1H0,1H0,1H0,1H0/
C
C====== GET PROGRAM SOURCE SIZE
C
        CALL APRSET
        CALL SIZEPS(PSIZE)
        IPSIZE=PSIZE
        CALL I2ASCI(6,PSIZE,STR,IRADX,1)
        WRITE(ITTO,10)(STR(I),I=1,6)
        IF(IECHO.NE.0) WRITE(LFIL,10)(STR(I),I=1,6)
10      FORMAT(11H PS SIZE = ,6A1)
C
C====== GET MD PAGES AND SIZES
C
        NPAGES=INMPGS(RMDSIZ)
        IF(NPAGES.EQ.0) GO TO 70
        WRITE(ITTO,20) NPAGES
        IF(IECHO.NE.0) WRITE(LFIL,20) NPAGES
20      FORMAT(8H AP HAS ,I2,6H PAGES,/,12H PAGE   SIZE)
C
C====== PRINT EACH PAGE SIZE
C
        DO 60 I=1,NPAGES
          ITEMP=I-1
          CALL I2ASCI(6,IPFIX(RMDSIZ(I)),STR,IRADX,1)
          IF(RMDSIZ(I).NE.65536.0) GO TO 40
          DO 30 J=1,6
            IF(IRADX.EQ.8) STR(J)=N65R8(J)
            IF(IRADX.EQ.10) STR(J)=N65R10(J)
            IF(IRADX.EQ.16) STR(J)=N65R16(J)
30        CONTINUE
C
40        WRITE(ITTO,50) ITEMP,(STR(J),J=1,6)
          IF(IECHO.NE.0)WRITE(LFIL,50) ITEMP,(STR(J),J=1,6)
50      FORMAT(I4,2X,6A1)
60      CONTINUE
C
        IMSIZ=IPFIX(RMDSIZ(1))
        RETURN
C
C====== MD SIZING ERROR (COULD NOT SIZE MAIN DATA)
C
70      WRITE(ITTO,80)
        IF(IECHO.NE.0) WRITE(LFIL,80)
80      FORMAT(23H MAIN DATA SIZING ERROR)
        RETURN
       END
C****** MEMDMP = MEMORY DUMP ROUTINE = REL 5.0  , NOV 79 **********************
        SUBROUTINE MEMDMP(INBUF,IPOS,IRADX)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : DGCOM>MEMDMP                                             *
C  *    REV         : 5.0                                                      *
C  *    VERSION     : 1.0                                                      *
C  *    DATE        : SEPT 7,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : INBUF     => INPUT COMMAND BUFFER                        *
C  *                  IPOS      => TOKEN POSITION IN COMMAND BUFFER            *
C  *                  IRADX     => ERROR PRINTOUT RADIX                        *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : THIS ROUTINE IS USED TO PRINT OUT THE CONTENTS OF A      *
C  *                  SPECIFIED MEMORY.  THE FOLLOWING MEMORIES MAY BE         *
C  *                  EXAMINED: PROGRAM SOURCE                                 *
C  *                            MAIN DATA                                      *
C  *                            DPX                                            *
C  *                            DPY                                            *
C  *                            S-PADS                                         *
C  *                            APSTATUS                                       *
C  *                  THESE MEMORIES MAY BE REFERENCED BY THE COMMAND STRING   *
C  *                  INTERPRETTER IN THE FOLLOWING FORMAT:                    *
C  *                  PROGRAM SOURCE - DUMP PS=LOW,HIGH                        *
C  *                  MAIN DATA      - DUMP MD=LOW,HIGH                        *
C  *                  DPX            - DUMP DPX=LOW,HIGH                       *
C  *                  DPY            - DUMP DPY=LOW,HIGH                       *
C  *                  S-PADS         - DUMP SPAD=LOW,HIGH                      *
C  *                  APSTATUS       - DUMP STATUS                             *
C  *                  -                                                        *
C  *                  WHERE LOW IS THE LOW MEMORY ADDRESS AND HIGH IS THE HIGH *
C  *                  MEMORY ADDRESS.                                          *
C  *                                                                           *
C  *    GENERALITIES: MEMDMP USES THE DUMP SUBROUTINE (DGCOM ROUTINE) TO PRINT *
C  *                  OUT THE CONTENTS OF EACH SPECIFIED MEMORY.  WHEN DPX     *
C  *                  OR DPY IS SPECIFIED, THE DATA PAD ADDRESS IS ALSO GIVEN. *
C  *                                                                           *
C: *****************************************************************************
C
        INTEGER IZ(36),ITEMP(6),INBUF(80),ITOK(6)
        COMMON /INSTAL/ ITTI,ITTO,LFIL
        COMMON /TOTL/ IENTRY(20),RNDSIZ(17),IRTURN,ICHNGE
        EQUIVALENCE (IENTRY(3),IECHO)
        DATA IZ/'P','S',' ',' ',' ',' ',
     +          'M','D',' ',' ',' ',' ',
     +          'D','P','X',' ',' ',' ',
     +          'D','P','Y',' ',' ',' ',
     +          'S','P','A','D',' ',' ',
     +          'S','T','A','T','U','S'/
C
C.
C..
C...
C====== DUMP MEMORY
C
        IA=IRDTK(INBUF,IPOS,ITOK,IRADX,INUM,IMIN)
        DO 10 J=1,6
        L=J*6-5
        N=NCOMP(ITOK,1,6,IZ,L)
        IF(N.EQ.0) GO TO(20,30,40,50,60,70),J
10      CONTINUE
        RETURN
C
C====== PS
C
20      WRITE(ITTO,2000)
        IF(IECHO.NE.0) WRITE(LFIL,2000)
        CALL DUMP(INBUF,IPOS,9,4)
        RETURN
C
C====== MD
C
30      CONTINUE
        IA=IRDTK(INBUF,IPOS,ITOK,IRADX,IPG,MINFLG)
        CALL OMAE(IPG)
        WRITE(ITTO,2010) IPG
        IF(IECHO.NE.0) WRITE(LFIL,2010) IPG
        CALL DUMP(INBUF,IPOS,14,3)
        RETURN
C
C====== DPX
C
40      CALL APEXAM(IZZZ,5,0)
        CALL I2ASCI(6,IZZZ,ITEMP,IRADX,1)
        WRITE(ITTO,2040) ITEMP
        IF(IECHO.NE.0) WRITE(LFIL,2040) ITEMP
        CALL DUMP(INBUF,IPOS,12,3)
        RETURN
C
C====== DPY
C
50      CALL APEXAM(IZZZ,5,0)
        CALL I2ASCI(6,IZZZ,ITEMP,IRADX,1)
        WRITE(ITTO,2050) ITEMP
        IF(IECHO.NE.0) WRITE(LFIL,2050) ITEMP
        CALL DUMP(INBUF,IPOS,13,3)
        RETURN
C
C====== SPAD
C
60      WRITE(ITTO,2060)
        IF(IECHO.NE.0) WRITE(LFIL,2060)
        CALL DUMP(INBUF,IPOS,6,1)
        RETURN
C
C====== APSTATUS
C
70      CALL APEXAM(IXXX,7,0)
        CALL I2ASCI(6,IXXX,ITEMP,IRADX,0)
        WRITE(ITTO,2070)ITEMP
        IF(IECHO.NE.0) WRITE(LFIL,2070)ITEMP
        RETURN
C
C====== FORMAT STATEMENTS
C
2000    FORMAT(//,20H PROGRAM SOURCE DUMP,//)
2010    FORMAT(//,15H MAIN DATA DUMP,/,7H PAGE: ,I4,//)
2020    FORMAT(6H PAGE?)
2030    FORMAT(I8)
2040    FORMAT(//,9H DPX DUMP,5X,5H DPA=,6A1,//)
2050    FORMAT(//,9H DPY DUMP,5X,5H DPA=,6A1,//)
2060    FORMAT(//,10H SPAD DUMP,//)
2070    FORMAT(//,12H APSTATUS = ,6A1,//)
        END
C****** DUMP = MEMORY PRINT ROUTINE = REL 5.0  , NOV 79 ***********************
        SUBROUTINE DUMP(INBUF,IPOS,ISEL,ICNT)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : DGCOM>DUMP                                               *
C  *    REV         : XXX                                                      *
C  *    VERSION     : 1.0                                                      *
C  *    DATE        : SEPT 7,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : INBUF     => INPUT COMMAND BUFFER                        *
C  *                  IPOS      => TOKEN POSITION IN COMMAND BUFFER            *
C  *                  ISEL      => VALUE FOR APEXAM WHICH SELECTS WHICH        *
C  *                               MEMORY TO DUMP                              *
C  *                  ICNT      => NUMBER OF 16 BIT WORDS IN OUTPUT MESSAGE    *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : EXAMINES THE SPECIFIED MEMORY AND WRITES OUT THE         *
C  *                  RESULTS.                                                 *
C  *                                                                           *
C  *    GENERALITIES: DUMP USES APEXAM (APEX SUBROUTINE) TO EXAMINE THE MEMORY *
C  *                  SPECIFIED BY ISEL.  THE OUTPUT MESSAGE GIVES THE MEMORY  *
C  *                  ADDRESS FOLLOWED BY ITS CONTENTS.                        *
C  *                                                                           *
C: *****************************************************************************
C
C====== LOCAL INTEGERS
C
        INTEGER INBUF(80),IPOS,LINE(30),ITOK(6),LOC(6),IREG(4)
        COMMON /INSTAL/ ITTI,ITTO,LFIL
        COMMON /TOTL/ IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
        EQUIVALENCE (IENTRY(1),IRADX),(IENTRY(3),IECHO)
        DATA IBL /1H /
C
C.
C..
C...
C====== GET LOW AND HIGH MEMORY VALUES
C
        IF(IRDTK(INBUF,IPOS,ITOK,IRADX,ILOW,IMIN).NE.2) GO TO 60
        IF(IRDTK(INBUF,IPOS,ITOK,IRADX,IHIGH,IMIN).NE.2) GO TO 60
        IF(ILOW.GT.IHIGH) GO TO 50
C
C====== DUMP MEMORY
C
        ILOW=ILOW+1
        IHIGH=IHIGH+1
        DO 40 K=ILOW,IHIGH
        I=K-1
        CALL APEXAM(IREG,ISEL,I)
        CALL I2ASCI(6,I,LOC,IRADX,1)
        N=1
        DO 10 LNUM=1,30
10      LINE(LNUM)=IBL
        DO 20 I=1,ICNT
        CALL I2ASCI(6,IREG(I),LINE(N),IRADX,0)
        N=N+7
20      CONTINUE
        WRITE(ITTO,30) LOC,LINE
        IF(IECHO.NE.0) WRITE(LFIL,30) LOC,LINE
30      FORMAT(6H ADDR=,6A1,3X,7H VALUE=,3X,30A1)
40      CONTINUE
C
50      RETURN
C
C====== NUMERIC ERROR
C
60      WRITE(ITTO,70)
        IF(IECHO.NE.0) WRITE(LFIL,70)
70      FORMAT(14H NUMERIC ERROR)
        RETURN
C
        END
C****** NCOMP = ARRAY COMPARE ROUTINE = REL 5.0  , NOV 79 *********************
        FUNCTION NCOMP(JCARD,J,JLAST,KCARD,K)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : DGCOM>NCOMP                                              *
C  *    REV         : XXX                                                      *
C  *    VERSION     : 1.0                                                      *
C  *    DATE        : SEPT 7,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : JCARD     => FIRST ARRAY TO COMPARE                      *
C  *                  J         => STARTING JCARD LOCATION                     *
C  *                  JLAST     => ENDING JCARD LOCATION                       *
C  *                  KCARD     => SECOND ARRAY TO COMPARE                     *
C  *                  K         => BEGINNING LOCATION FOR COMPARE              *
C  *                                                                           *
C  *    EXIT        : NCOMP     <= FUNCTION VALUE                              *
C  *                                 POSITIVE - ARRAYS NOT THE SAME            *
C  *                                 ZERO     - ARRAYS THE SAME                *
C  *                                 NEGITIVE - ARRAYS NOT THE SAME            *
C  *                                                                           *
C  *    FUNCTION    : TO COMPARE TWO ARRAYS.                                   *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C
        DIMENSION JCARD(9999),KCARD(9999)
C
C.
C..
C...
        DO 10 LNUM=J,JLAST
        KNOW=K+LNUM-J
        NCOMP=KCARD(KNOW)
        NCOMP=JCARD(LNUM)-NCOMP
        IF(NCOMP) 20,10,20
10      CONTINUE
20      RETURN
        END
C****** IOCNTL = PDP11 DEPENDENT I/O FILE AND DEVICE HANDLER = REL 4.0 , JUN 79
        INTEGER FUNCTION IOCNTL(ISW,IBUF,LEN,IVAL)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : DGCOM>IOCNTL                                             *
C  *    REV         : 4.0                                                      *
C  *    VERSION     : 1.0                                                      *
C  *    DATE        : 11-JUN-79                                                *
C  *    HOST SYSTEM : PDP11/RT11/RSX11                                         *
C  *                                                                           *
C  *    ENTRY       : ISW     => ACTION CODE TO PERFORM                        *
C  *                  IBUF    => 80 CHARACTER ASCII BUFFER                     *
C  *                  LEN     => OPTIONAL LENGTH OF "IBUF"                     *
C  *                  IVAL    => INTEGER VALUE DEPENDENT UPON ACTION CODE      *
C  *                                                                           *
C  *    EXIT        : ISW     <= N/A                                           *
C  *                  IBUF    <= N/A                                           *
C  *                  LEN     <= OPTIONAL LENGTH OF "IBUF"                     *
C  *                  IVAL    <= INTEGER VALUE DEPENDENT UPON ACTION CODE      *
C  *                                                                           *
C  *    FUNCTION    :                                                          *
C  *                                                                           *
C  *                                                                           *
C  *    IOCNTL PERFORMS I/O FILE HANDLING FUNCTIONS FOR FPS FORTRAN            *
C  *      PROGRAMS. THE ROUTINE PROVIDES 10 DIFFERENT ACTION CODES WHICH       *
C  *      CALL ON HOST DEPENDENT SERVICE ROUTINES. THE FOLLOWING IS A          *
C  *      BREAKDOWN OF EACH ACTION CODE AND ITS DEPENDENT PARAMETERS.          *
C  *                                                                           *
C  *                    ------------------------------                         *
C  *                                                                           *
C  *    ACTION (1)      = SETS CONSOLE DEVICE TO PROPER LOGICAL UNITS.         *
C  *                                                                           *
C  *                    ITTI    => SET INTO COMMON /INSTAL/.                   *
C  *                    ITTO    => SET INTO COMMON /INSTAL/.                   *
C  *                                                                           *
C  *                    ------------------------------                         *
C  *                                                                           *
C  *    ACTION (2)      = PROMPTS USER FOR A COMMAND LINE.                     *
C  *                                                                           *
C  *                    IBUF     = N/A                                         *
C  *                    LEN      = N/A                                         *
C  *                    IVAL     = N/A                                         *
C  *                                                                           *
C  *                    ------------------------------                         *
C  *                                                                           *
C  *    ACTION (3)      = READS A COMMAND LINE FROM USER CONSOLE.              *
C  *                                                                           *
C  *                    IBUF    <= 80 CHARACTER A1 FORMATTED COMMAND LINE.     *
C  *                    LEN     <= OPTIONAL LENGTH OF COMMAND LINE.            *
C  *                    IVAL     = N/A                                         *
C  *                    IOCNTL  <= 0 IF OK , -1 IF ERROR.                      *
C  *                                                                           *
C  *                    ------------------------------                         *
C  *                                                                           *
C  *    ACTION (4)      = ATTEMPT TO ASSIGN AN ARRAY PROCESSOR.                *
C  *                                                                           *
C  *                    IVAL    => INTEGER VALUE TO ASSIGN AS AP NUMBER.       *
C  *                    IVAL    <= ACTUAL AP NUMBER ASSIGNED.                  *
C  *                                                                           *
C  *                    ------------------------------                         *
C  *                                                                           *
C  *    ACTION (5)      = OPENS AN ECHO FILE FOR CONSOLE DATA ECHO.            *
C  *                                                                           *
C  *                    IVAL    = LOGICAL UNIT ASSIGNED TO ECHO FILE           *
C  *                                                                           *
C  *                    ------------------------------                         *
C  *                                                                           *
C  *    ACTION (6)      = REWINDS FILE.                                        *
C  *                                                                           *
C  *                    IVAL    => LOGICAL UNIT TO REWIND.                     *
C  *                                                                           *
C  *                    ------------------------------                         *
C  *                                                                           *
C  *    ACTION (7)      = OPENS A FIXED NAME FILE FOR READ-ONLY.               *
C  *                                                                           *
C  *                    IBUF    => FILE NAME STRING IN HOST FORMAT.            *
C  *                    LEN     => OPTIONAL LENGTH OF NAME STRING.             *
C  *                    IVAL    => LOGICAL UNIT FOR FILE.                      *
C  *                                                                           *
C  *                    ------------------------------                         *
C  *                                                                           *
C  *    ACTION (8)      = OPENS A FIXED NAME FILE FOR READ-WRITE.              *
C  *                                                                           *
C  *                    SAME AS ACTION 7 EXCEPT CALL TO HOST FILE OPENING.     *
C  *                      ROUTINE SPECIFIES READ-WRITE ACCESS.                 *
C  *                                                                           *
C  *                    ------------------------------                         *
C  *                                                                           *
C  *    ACTION (9)      = CLOSE FILE.                                          *
C  *                                                                           *
C  *                    IVAL    => LOGICAL UNIT TO CLOSE.                      *
C  *                                                                           *
C  *                    ------------------------------                         *
C  *                                                                           *
C  *    ACTION (10)     = RELEASE ANY ASSIGNED ARRAY PROCESSOR.                *
C  *                                                                           *
C  *                    NO PARAMETERS NECESSARY.                               *
C  *                                                                           *
C  *                    ------------------------------                         *
C  *                                                                           *
C: *****************************************************************************
C
        INTEGER IBUF(80)
        INTEGER INAM(16)
C
        COMMON /INSTAL/ ITTI,ITTO,IDUMMY
C
        DATA IECHO / 0 /
        DATA LECHO / 10 /
C
C.
C..
C...
        IOCNTL=0
        GO TO (10,20,30,40,90,100,110,120,130,140),ISW
C
C====== ACTION (1) = SET CONSOLE DEVICE TO DEFAULT LOGICAL UNITS
C
10      CONTINUE
        ITTI=5
        ITTO=5
        RETURN
C
C====== ACTION (2) = PROMPT USER FOR A COMMAND LINE
C
20      CONTINUE
        WRITE(ITTO,170)
        RETURN
C
C====== ACTION (3) = READ A COMMAND LINE FROM USER CONSOLE
C
30      CONTINUE
        READ(ITTI,180,ERR=150)LEN, IBUF
        RETURN
C
C====== ACTION (4) = ATTEMPT TO ASSIGN AN AP
C
40      CONTINUE
        ITRY=3
        NZMD=LEN
50      IF(NZMD.EQ.0) CALL APASGN(IVAL,1,IAP)
        IF(NZMD.NE.0) CALL APINIT(IVAL,1,IAP)
        IF(IAP.GT.0) GO TO 80
        IF(IAP.NE.-3) GO TO 60
        WRITE(ITTO,200)
        IF(IECHO.NE.0) WRITE(LECHO,200)
        CALL APRLSE
        ITRY=ITRY-1
        IF(ITRY.EQ.0) GO TO 70
        GO TO 50
60      IF(IAP.NE.-1) GO TO 70
        WRITE(ITTO,220)
        IF(IECHO.NE.0) WRITE(LECHO,220)
        GO TO 150
70      WRITE(ITTO,210)
        IF(IECHO.NE.0) WRITE(LECHO,210)
        GO TO 150
80      IVAL=IAP
        RETURN
C
C====== ACTION (5) = OPEN ECHO FILE
C
90      CONTINUE
        IF(IECHO.NE.0) GO TO 150
C
        IVAL    = LECHO
        IECHO   = 1
C
C====== PROMPT FOR AND OPEN THE FILE
C
        WRITE(ITTO,230)
        READ(ITTI,190) LEN,INAM
        CALL ASSIGN(IVAL,INAM,LEN,'NEW','NC')
        RETURN
C
C====== ACTION (6) = REWIND FILE "IVAL"
C
100     CONTINUE
        IF(IVAL.EQ.0) GO TO 150
        REWIND IVAL
        RETURN
C
C====== ACTION (7) = OPEN FIXED NAME FILE FOR READ-ONLY
C
110     CONTINUE
        IF(IVAL.LT.11) IVAL=IVAL+10
        CALL ASSIGN(IVAL,IBUF,LEN,'RDO','NC')
        RETURN
C
C====== ACTION (8) = OPEN FIXED NAME FILE FOR READ-WRITE
C
120     CONTINUE
        IF(IVAL.LT.11)IVAL=IVAL+10
        CALL ASSIGN(IVAL,IBUF,LEN,'NEW','NC')
        RETURN
C
C====== ACTION (9) = CLOSE FILE "IVAL"
C
130     CONTINUE
        IF(IVAL.EQ.0) GO TO 150
        CALL CLOSE(IVAL)
        IF(IVAL.EQ.LECHO) IECHO=0
        RETURN
C
C====== ACTION (10) = RELEASE ANY ASSIGNED AP'S
C
140     CONTINUE
C       CALL APRLSE
        RETURN
C
C====== REPORT ERROR (IOCNTL= -1)
C
150     CONTINUE
        IOCNTL=-1
C
C====== RETURN
C
160     CONTINUE
        RETURN
C
C====== FORMAT STATEMENTS
C
170     FORMAT(/,1H ,$,2H> )
180     FORMAT(Q,80A1)
190     FORMAT(Q,16A2)
200     FORMAT(35H AP ASSIGNED. ATTEMPTING TO RELEASE/,1H )
210     FORMAT(17H AP NOT AVAILABLE/,1H )
220     FORMAT(8H AP BUSY/,1H )
230     FORMAT(1H ,$,15H ECHO FILENAME:)
        END
 