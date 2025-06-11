C****** ART100                                             == REL 5.0  , NOV 79
C****** ART100 = AP ARITHMETIC DIAGNOSTIC                   = REL 5.0  , NOV 79
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>MAINLINE                                          *
C  *    DATE        : NOV  1,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : N/A                                                      *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : THE PHILOSOPHY OF TESTING THE ARITHMETIC FUNCTIONS IN    *
C  *                  THIS DIAGNOSTIC IS TO EXERCISE THE ARITHMETIC HARDWARE   *
C  *                  IN THE AP USING AP MICRO-CODE AND VERIFY THE RESULTS     *
C  *                  BY USING THE SAME ARGUMENTS  IN A SIMULATOR PROGRAM.     *
C  *                  THE ARGUMENTS  ARE GENERATED USING A RANDOM NUMBER       *
C  *                  GENERATOR.  THESE ARGUMENTS  INCLUDE S-PAD, DATA PAD,    *
C  *                  DATA PAD ADDRESSES, AND AP STATUS ARGUMENTS              *
C  *                  FOR BIT-REVERSE.                                         *
C  *                  THE NUMBER OF AP MICRO-CODE INSTRUCTIONS TO BE           *
C  *                  EXECUTED IS ALSO DETERMINED AT RANDOM.  THE LAYOUT       *
C  *                  OF GENERATED TEST AP MICRO-CODE IS AS FOLLOWS:           *
C  *                  -                                                        *
C  *                  STCOD:   FADD ZERO,ZERO; FMUL TM,MD; CLR# 0              *
C  *                           FADD ZERO,ZERO; FMUL TM,MD                      *
C  *                           FMUL TM,MD                                      *
C  *                  STNUM:   LDAPS; DB=VALUE                                 *
C  *                  RNDCOD:  SOP; FADD DPY,DPX; FMUL DPY,DPX                 *
C  *                  RND1:    SOP; FADD DPY,DPX; FMUL DPY,DPX                 *
C  *                  RND2:    SOP; FADD DPY,DPX; FMUL DPY,DPX; DPY<FA         *
C  *                    .      SOP; FADD DPY,DPX; FMUL DPY,DPX; DPY<FA; DPX<FM *
C  *                    .      SOP; FADD DPY,DPX; FMUL DPY,DPX; DPY<FA; DPX<FM *
C  *                    .      SOP; FADD DPY,DPX; FMUL DPY,DPX; DPY<FA; DPX<FM *
C  *                    .      "    "             "             "       "      *
C  *                    .      "    "             "             "       "      *
C  *                    .      "    "             "             "       "      *
C  *                  RNDEND:  "    "             "             "       "      *
C  *                  FINCOD:  FSUB; FMUL; DPY<FA; DPX<FM                      *
C  *                  FIN1:    FMUL; DPY<FA; DPX<FM                            *
C  *                  FIN2:    DPX<FM; HALT                                    *
C  *                           NOP                                             *
C  *                           MOV 0,0                                         *
C  *                           LDSPNL 0; RSPFN                                 *
C  *                           HALT                                            *
C  *                           NOP                                             *
C  *                  -                                                        *
C  *                  THE SOFTWARE SIMULATORS ARE SUBROUTINES IN ART100. THEY  *
C  *                  ARE: SPADS  -  SCRATCH PAD SIMULATOR                     *
C  *                       FADDS  -  FLOATING POINT ADDER SIMULATOR            *
C  *                       FMULS  -  FLOATING POINT MULTIPLIER SIMULATOR       *
C  *                  THE FOLLOWING ROUTINES ARE USED BY THE SIMULATORS:       *
C  *                       BITREV -  BIT REVERSE FOR THE SIMULATOR             *
C  *                       ZCHCK  -  FLOATING POINT ADDER ZERO CHECK           *
C  *                       BOOTH  -  BOOTH'S ALGORITHM MULTIPLIER              *
C  *                       NORM   -  NORMALIZES FLOATING POINT NUMBERS         *
C  *                       FEXPND -  EXPAND FLOATING POINT NUMBER              *
C  *                       FCMPRS -  COMPRESS FLOATING POINT NUMBER            *
C  *                       IDNEG  -  DOUBLE WORD NEGATE                        *
C  *                       IDSHFT -  DOUBLE WORD SHIFT                         *
C  *                  -                                                        *
C  *                  THE COMPARISON OF AP GENERATED RESULTS AND SIMULATOR     *
C  *                  GENERATED RESULTS IS DONE IN THE ART100 SUBROUTINE,      *
C  *                  COMPAR.                                                  *
C  *                                                                           *
C  *    GENERALITIES: BEFORE ART100 CAN BE USED AS AN EFFECTIVE DIAGNOSTIC,    *
C  *                  APTEST (TESTING INTERFACE REGISTERS AND MEMORIES) AND    *
C  *                  APPATH (TESTING DATA PATHS IN THE AP) SHOULD BE RUN      *
C  *                  SUCCESSFULLY.                                            *
C  *                                                                           *
C: *****************************************************************************
C%.I    73      MITRA
C#
        INTEGER ITTI,ITTO,LFIL,RJUSFY,ISTAT,IRTURN,ICHNGE,WATERR,
     +  LOOPER,TYPDIS,IORST,HALT,FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,
     +  IERLIM,IPSLIM,ERRCNT,PASCNT,IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ,
     +  PSIZE,BASLOW,MDSIZ,IAPNUM,IECHO,IERRS,IMSIZ,IPSIZE,NPAGES
        INTEGER
     +  SDPX,SDPY,SSP,SSTAT,HDPX,HDPY,HSP,HSTAT,XSAVE,YSAVE,DPARG,N,
     +  STRTPS,FINPS,SP,CB,MDR,MI,TMR,DPXR,DPYR,DPXW,DPYW,SPFN,INBS
        INTEGER DPBS,PNLBS,FM,FA,ZERO,PSA,MA,TMA,DPA,STATUS,DA,FLAGS,
     +  SRA,SWITCH,LITES,APMA,HMA,WC,CTL,FV,DPXF,DPYF,XRF,YRF,XWF,YWF,
     +  BL,SOPL,SHL,SPSL,SPDL,SPADL,LDSPD,E1,E2,M1,M2,MXCNT,TRUNC,ISMF,
     +  FIXF,ZFLG,ZRES,RNDCON,NEG1,PROD,FARG1,FARG2,FA1,FA2,MARG1,MARG2
        INTEGER
     +  FM1,FM2,FM3,STNUM,FINNUM,RNDCOD,RND1,RND2,RNDEND,FINCOD,FIN1,
     +  FIN2,ENDCOD,ACOUNT,CODE(4,32),DPXARG(3,8),DPYARG(3,8),SPARG(16)
        INTEGER STARG,ALLONE,TEMP,TEMP1,I,J,TEMP2,LPCNT,INO1(6),INO2(6)
        INTEGER STATBF,STATNM,STRAD
C
        REAL RMDSIZ
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /ART100/ SDPX(3,8),SDPY(3,8),SSP(16),SSTAT,HDPX(3,8),
     1  HDPY(3,8),HSP(16),HSTAT,XSAVE,YSAVE,DPARG,N,STRTPS(4,4),
     2  FINPS(4,8)
C
        COMMON /REG/    SP(16),CB(4),MDR(3),MI(3),TMR(3),DPXR(3),
     1  DPYR(3),DPXW(3),DPYW(3),SPFN,INBS(3),DPBS(3),PNLBS,FM(3),FA(3),
     2  ZERO(4),PSA,MA,TMA,DPA,STATUS,DA,FLAGS,SRA,SWITCH,LITES,APMA,
     3  HMA,WC,CTL
C
        COMMON /FVV/    FV(26)
C
        COMMON /SPAD/   SPADL(5),LDSPD
C
        COMMON /NORM/   E1,E2,M1(2),M2(2),MXCNT,TRUNC,ISMF,FIXF,ZFLG,
     1  ZRES(16),RNDCON(4),NEG1(2),PROD(2),FARG1(3),FARG2(3),FA1(3),
     2  FA2(3),MARG1(3),MARG2(3),FM1(3),FM2(3),FM3(3)
        COMMON /STCNTL/ STATBF(60),STATNM(21),STRAD(20)
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL/  IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),FFTERR) , (ISTAT( 7),DMPSIM) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IPSLIM) , (ISTAT(10),ERRCNT) , (ISTAT(11),PASCNT)
        EQUIVALENCE
     +  (ISTAT(12),   IXN) , (ISTAT(13),   IYN) , (ISTAT(14),SIMULS),
     +  (ISTAT(15), IERRS) , (ISTAT(16),IERLIM) , (ISTAT(17), ISTEP)
C
        EQUIVALENCE (FV(11),DPXF),
     +  (FV(12),DPYF),(FV(14),XRF),(FV(15),YRF),(FV(16),XWF)
        EQUIVALENCE (FV(17),YWF),(SPADL(1),BL),(SPADL(2),SOPL),
     +  (SPADL(3),SHL),(SPADL(4),SPSL),(SPADL(5),SPDL)
C
C.
C..
C...
C
          ALLONE=IP16(-1)
          ACOUNT=0
          STNUM=4
          FINNUM=8
          RNDCOD=STNUM+1
          RND1=RNDCOD+1
          RND2=RNDCOD+2
C
C====== PROCESS CODE THROUGH 'IP16' TO MOLIFY 1'S COMPLEMENT MAC
C
          DO 10 I=1,STNUM
          DO 10 J=1,4
10        CODE(J,I)=IP16(STRTPS(J,I))
          DO 20 I=1,FINNUM
          DO 20 J=1,4
20        FINPS(J,I)=IP16(FINPS(J,I))
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
30      WRITE(ITTO,2010)
C
        CALL SETUP(1)
40      CALL SETUP(0)
        CALL APRSET
C>
        IF(IPSLIM.NE.0.AND.IPSLIM.LT.PSSFLG) PSSFLG=IPSLIM
C
C====== PRINT ERROR FILE HEADER IF SPECIFIED
C
        IF(IECHO.NE.0) WRITE(LFIL,2020)
C
        IF(IAPNUM.LT.0.OR.IAPNUM.GT.7) GO TO 30
C
C====== RESET COUNTERS
C
        PASCNT=0
        ERRCNT=0
        LPCNT=0
C
        GO TO 60
C
50      LPCNT=0
        CALL I2ASCI(6,ERRCNT,INO1,10,1)
        CALL I2ASCI(6,PASCNT,INO2,10,1)
        WRITE(ITTO,2030) INO1,INO2
        IF(IECHO.NE.0) WRITE(LFIL,2030) INO1,INO2
C
C====== THIS IS THE RE-ENTRY POINT FOR ART100
C
60      IF(PSSFLG.EQ.0) PSSFLG=1
        IF(LPCNT.EQ.PSSFLG) GO TO 50
        IF(IPSLIM.EQ.0) GO TO 70
        IF(IPSLIM.EQ.PASCNT) GO TO 40
C
70      LPCNT=LPCNT+1
        PASCNT=PASCNT+1
C
C======
C       GENERATE A NEW CASE
C       FIRST SAVE STARTING RANDOM NUMBERS
C
          XSAVE=IXN
          YSAVE=IYN
C
C====== PICK N BETWEEN 1-16, EITHER RANDOM OR USER SET
C
          N=IAND16(IGRN16(IXN,IYN),15)+1
          IF (SIMULS.NE.0) N=SIMULS
C
C====== SET POINTERS TO END OF CODE
C
          RNDEND=RNDCOD+N-1
          FINCOD=RNDCOD+N
          FIN1=FINCOD+1
          FIN2=FINCOD+2
          ENDCOD=FINCOD+FINNUM-1
C
C====== GENERATE S-PAD ARGUEMENTS
C
          DO 90 I=1,16
90        SPARG(I)=IGRN16(IXN,IYN)
C
C====== GENERATE DPX AND DPY ARGUEMENTS
C       EXPONENT, HIGH MANTISSA, LOW MANTISSA
C
          DO 100 I=1,8
          DPXARG(1,I)=IAND16(IGRN16(IXN,IYN),1023)
          DPXARG(2,I)=IAND16(IGRN16(IXN,IYN),4095)
          DPXARG(3,I)=IGRN16(IXN,IYN)
          DPYARG(1,I)=IAND16(IGRN16(IXN,IYN),1023)
          DPYARG(2,I)=IAND16(IGRN16(IXN,IYN),4095)
100       DPYARG(3,I)=IGRN16(IXN,IYN)
C
C====== GENERATE DPA, AND APSTATUS FOR BIT-REVERSE
C
          DPARG=IAND16( IGRN16(IXN,IYN), 31)
          STARG=IAND16( IGRN16(IXN,IYN), 7)
C
C====== GENERATE CODE
C
          DO 150 I=RNDCOD,RNDEND
C
C====== WORD #1
C       IF SOP=SPEC; FORCE SOP=SOP1
C       IF SOP=SOP1; FORCE SOP1=0 OR 8-11
C       ALL OF WORD #1 IS RANDOM; GET SOP FIELD
C
          TEMP=IGRN16(IXN,IYN)
          TEMP1=IAND16(TEMP,28672)
          IF (TEMP1.EQ.0) GO TO 110
          IF (TEMP1.NE.4096) GO TO 120
C
C====== SOP=1=SPEC; FORCE SOP=SOP1=0 BY CLEARING SOP FIELD
C
          TEMP=NAND16(TEMP,28672)
C
C====== SOP=SOP1, FORCE SOP1 TO 0 OR 8-11
C
110       IF (IAND16(TEMP,960).EQ.0) GO TO 120
C
C====== FORCE SOP1 BETWEEN 8-11, I.E. CLEAR BIT 7 AD SET BIT 8
C
          TEMP=IOR16(512,NAND16(TEMP,256))
C
C====== STORE WORD #1
C
120       CODE(1,I)=TEMP
C
C====== WORD #2
C       REMOVE ALL BUT FADD, THEN SET A1=DPY AND A2=DPX
C       IF FADD=7=IO, FORCE FADD=FADD1=0
C       IF FADD=FADD1=0, GENERATE RANDOM FADD1 IN A1 FIELD
C
          TEMP1=IOR16(NAND16(IGRN16(IXN,IYN),32767),13312)
          TEMP2=ILSH16(IAND16(TEMP,3),1) + NEGCHK(TEMP1)
          IF (TEMP2.EQ.0) GO TO 130
          IF (TEMP2.NE.7) GO TO 140
C
C====== IF FADD=IO=7, FORCE TO FADD1 BY CLEARING BITS 14-15 OF W
C       AND BIT 0 OF WORD #2
C
          CODE(1,I)=NAND16(TEMP,3)
          TEMP1=IAND16(TEMP1,32767)
C
C====== RANDOM FADD1
C
130       TEMP1=IOR16(IAND16(IGRN16(IXN,IYN),28672),
     1                NAND16(TEMP1,28672))
C
C====== STORE WORD #2
C
140       CODE(2,I)=TEMP1
C
C====== WORD #3
C       DPX<FM; DPY<FA; RANDOM XR,YR,XW,YW, CLEAR DPBS
C
          CODE(3,I)=IOR16(IAND16(IGRN16(IXN,IYN),511),IP16(-8192))
C
C====== WORD #4
C       RANDOM YM, SET M1=DPY, M2=DPX, CLEAR REST
C
150       CODE(4,I)=IOR16( NAND16( IGRN16(IXN,IYN), 4095), 2304)
C
C====== EXAMPLE ALTERATIONS OF RANDOM AND FINISH CODE:
C       FA AND FM STORES MUST BE REMOVED FROM THE FIRST 2 & 3 IN
C       FOR N=4:
C       SOP     +       *
C       SOP     +       *
C       SOP     +       *       <+
C       SOP     +       *       <+      <*
C
C
C
C       PUT IN FINISH CODE
C
          DO 160 I=FINCOD,ENDCOD
          TEMP=I-FINCOD+1
          DO 160 J=1,4
160       CODE(J,I)=FINPS(J,TEMP)
C
C====== PUT IN RANDOM XW IN FINCOD, FIN1, FIN2
C       PUT IN RANDOM YW IN FINCOD, FIN1
C
          DO 170 I=FINCOD,FIN2
170       CODE(3,I)=IOR16( IAND16( IGRN16(IXN,IYN), 7), CODE(3,I))
          DO 180 I=FINCOD,FIN1
180       CODE(4,I)=IOR16( NAND16( IGRN16(IXN,IYN), 8191), CODE(4,I))
C
C====== CLEAR DPX<FM FROM FIRST 3 AND DPY<FA FROM FIRST 2 RANDOM I
C
          DO 190 I=RNDCOD,RND1
          CODE(3,I)=IAND16( CODE(3,I), 504)
190       CODE(4,I)=IAND16( CODE(4,I), 7936)
          CODE(3,RND2)=IAND16( CODE(3,RND2), 12792)
C
C====== PUT APSTATUS INTO VALUE FIELD OF LAST STARTING INSTRUCTI
C
          CODE(4,STNUM)=STARG
C
C======
C       PUT CURRENT CASE INTO THE AP
C       PUT CODE INTO PS
C
          DO 200 I=1,ENDCOD
        ITMP1=I-1
200       CALL APDEP(CODE(1,I),9,ITMP1)
C
C====== LOOP HERE ---------
C       LOAD DPX
C
210       DO 220 I=1,8
        ITMP2=I+DPARG+27
220       CALL APDEP(DPXARG(1,I),12,ITMP2)
C
C====== LOAD DPY
C
          DO 230 I=1,8
        ITMP3=I+DPARG+27
230       CALL APDEP(DPYARG(1,I),13,ITMP3)
C
C====== LOAD S-PAD
C
          DO 240 I=1,16
        ITMP4=I-1
240       CALL APDEP(SPARG(I),6,ITMP4)
C
C====== SET TMA TO 177777 SO THAT TM WILL BE 0.0
C
          CALL WREG(ALLONE,515)
C
C====== LOAD DPA
C
          CALL WREG(DPARG,516)
C
C====== RUN THE AP AT 0
C       TO PSA
C       HOST SWITCHES TO SWR
C       CONTINUE TO FN
C
          CALL WREG(0,512)
          CALL SREAD(TEMP)
C>
          CALL WREG(TEMP,8192)
C
C>
C====== MAKE SURE DONE
C
250       CALL IFN(TEMP)
C>
          IF (NEGCHK(TEMP).NE.1) GO TO 250
C
C====== SEE IF TYPE OUT DISABLED, IF SO DON'T CHECK WITH THE SIMUL
C
          IF (TYPDIS.EQ.1) GO TO 340
C
C======
C       SOFTWARE SIMULATOR
C       LOAD THE SIMULATOR
C       DPX AND DPY
C
          DO 260 I=1,8
          DO 260 J=1,3
          SDPX(J,I)=DPXARG(J,I)
260       SDPY(J,I)=DPYARG(J,I)
C
C====== S-PAD, STATUS, AND DPA
C
          DO 270 I=1,16
270       SP(I)=SPARG(I)
          STATUS=STARG
          DPA=DPARG
C
C====== INITIALIZE THE PIPELINES
C
          DO 280 I=1,3
          FA1(I)=0
          FA2(I)=0
          FM1(I)=0
          FM2(I)=0
280       FM3(I)=0
C
C====== SET THE 'FZ' CONDITION BIT IN THE ADDER PIPELINE, REFLEC
C       THAT IT WAS CLEARED WITH 'FADD ZERO,ZERO'
C
          FA1(1)=4096
          FA2(1)=4096
C
C====== SET THE S-PAD LATCH TO REFLECT THE 'CLR 0'
C
          BL=0
          SOPL=0
          SHL=0
          SPSL=8
          SPDL=0
          LDSPD=0
C
C====== SIMULATOR INSTRUCTION LOOP
C
          DO 290 I=RNDCOD,FIN2
C
C====== DECODE INSTRUCTION
C
          CALL SPLIT(CODE(1,I),FV)
C
C====== FETCH DPX & DPY
C
          CALL RMOV( SDPX(1,XRF+1), DPXR, 3)
          CALL RMOV( SDPY(1,YRF+1), DPYR, 3)
C
C====== SET FM AND FA
C
          CALL RMOV( FM3, FM, 3)
          CALL RMOV( FA2, FA, 3)
C
C====== SET STATUS, OR TOGETHER:
C       BITS 0-2,5-15 OF THE OLD STATUS
C       BITS 0-1 OF FM
C       BITS 0-1,3-4 OF FA
C
          STATUS=IOR16( IOR16( NAND16( STATUS, 6144),
     1                         NAND16( FM(1),  16383)),
     2                         NAND16( FA(1),  10239))
C
C====== CLEAR STATUS INFO FROM FM & FA
C
          FM(1)=IAND16(FM(1),1023)
          FA(1)=IAND16(FA(1),1023)
C
C====== DO THE ARITHMETIC
C
          CALL SPADS
          CALL FADDS
          CALL FMULS
C
C====== STORE DPX & DPY
C
          IF (DPXF.EQ.3) CALL RMOV(FM,SDPX(1,XWF+1),3)
          IF (DPYF.EQ.2) CALL RMOV(FA,SDPY(1,YWF+1),3)
290       CONTINUE
C
C====== GET SOFTWARE ANSWERS
C
          SSTAT=STATUS
          DO 300 I=1,16
300       SSP(I)=SP(I)
C
C====== GET HARDWARE ANSWERS
C       GET STATUS
C
          CALL RREG(HSTAT,1030)
C
C>
C====== GET DPX AND DPY
C
          DO 310 I=1,8
          TEMP=I+DPARG+27
          CALL APEXAM(HDPX(1,I),12,TEMP)
          CALL APEXAM(HDPY(1,I),13,TEMP)
310     CONTINUE
C
C====== GET S-PAD
C       CONTINUE AFTER HALT TO SET SPFN=SP(SPD), THEN EXAMINE S-
C
          CALL OFN(8192)
          DO 320 I=1,16
        ITMP5=I-1
          CALL WREG(ITMP5,513)
          CALL RREG(HSP(I),1029)
C>
320     CONTINUE
C
C====== COMPARE AND CHECK FOR ERRORS
C
          CALL COMPAR
          IF (IERRS.NE.0) GO TO 330
C
          GO TO 340
C
C====== ERRORS, DUMP P.S. IF SWITCH SET
C
330       IF (DMPSIM.EQ.1) CALL DUMPPS(RNDCOD-1,FIN2-1,ITTO,IRADX)
          IF(DMPSIM .EQ. 1)CALL DUMPSP(SPARG,ITTO,IRADX)
C%.R    531     HP2100
          IF(DMPSIM .EQ. 1)CALL DUMPDP(DPXARG,DPYARG,ITTO,IRADX)
C%.E    531     HP2100
C
C======
C       END OF LOOP, CALL LOOP-TEST TO ALLOW USER BREAK
C       IRTURN' SET TO 1 MEANS THE USER WANT'S TO LOOP ON THE C
C
340       CALL LPTST(TEMP)
          IF (TEMP.EQ.1) GO TO 210
          GO TO 60
C
C====== FORMAT STATEMENTS
C
2000    FORMAT(35H  ***   ART100                  ***//)
2010    FORMAT(20H PLEASE ASSIGN AN AP)
2020    FORMAT(35H1 ***   ART100                  ***///,
     +        26H        ERROR LOGGING FILE ,////)
2030    FORMAT(17H ART100 - ERRORS=,6A1,9H, PASSES=,6A1)
C
          END
C
C
C****** IMCMD3 = IMMEDIATE EXECUTION COMMANDS               = REL 5.0  , NOV 79
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
C****** ASST3 = LISTS AVAILABLE COMMANDS FOR THIS TEST      = REL 5.0  , NOV 79
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
     +  58H ECHO     - ASKS USER FOR ECHO FILE-NAME (OPENS ECHO FILE)  /,
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
     +  50H ERRLIM=N - MAXIMUM ERRORS BEFORE RETURNING TO CSI          /,
     +  )
C
        WRITE(ITTO,16)
        IF(IECHO.NE.0) WRITE(LFIL,16)
C
16      FORMAT (
     +  43H X=N      - SET "X" RANDOM NUMBER PARAMETER                 /,
     +  43H Y=N      - SET "Y" RANDOM NUMBER PARAMETER                 /,
     +  46H N=N      - NUMBER OF INSTRUCTIONS TO SIMULATE              /,
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
     +  34H W        - RETURN TO CSI ON ERROR                          /,
     +  )
C
        WRITE(ITTO,20)
        IF(IECHO.NE.0) WRITE(LFIL,20)
C
20      FORMAT (
     +  39H F        - PRINT CONTENTS OF SIMULATOR                     /,
     +  43H /*       - /* CAN BE FOLLOWED BY A COMMENT                 /,
     +  39H DUMP XX  - DUMPS SPECIFIED MEMORY (XX)                     /,
     +  36H MEMSIZ   - DISPLAY PS AND MD SIZES                         /,
     +  )
C
        WRITE(ITTO,22)
        IF(IECHO.NE.0) WRITE(LFIL,22)
C
22      FORMAT(
     +  43H STEP     - EXECUTES IN STEP MODE (DEFAULT)                 /,
     +  36H -STEP    - EXECUTES IN CHAINED MODE                        /,
     +  )
C
        RETURN
        END
C****** SETUP3 = SETS UP DATA FOR CSI                       = REL 5.0  , NOV 79
C
        SUBROUTINE SETUP(INIT)
C
        INTEGER XTB(15,12),INIT,XTLEN
        COMMON /ERORR/  IERROR
        COMMON /TOTAL/ ISTAT(40)
C
        DATA XTB( 1, 1),XTB( 1, 2),XTB( 1, 3)   /1HB,1HE,1HT/
        DATA XTB( 1, 4),XTB( 1, 5),XTB( 1, 6)   /1HW,1HN,1H /
        DATA XTB( 1, 7),XTB( 1, 8),XTB( 1, 9)   /     2,     8,    10/
        DATA XTB( 1,10),XTB( 1,11),XTB( 1,12)   /     0,     0,    10/
C
        DATA XTB( 2, 1),XTB( 2, 2),XTB( 2, 3)   /1HP,1HA,1HS/
        DATA XTB( 2, 4),XTB( 2, 5),XTB( 2, 6)   /1HL,1HI,1HM/
        DATA XTB( 2, 7),XTB( 2, 8),XTB( 2, 9)   /     2,     9,    10/
        DATA XTB( 2,10),XTB( 2,11),XTB( 2,12)   /     0,     0,    10/
C
        DATA XTB( 3, 1),XTB( 3, 2),XTB( 3, 3)   /1HX,1H ,1H /
        DATA XTB( 3, 4),XTB( 3, 5),XTB( 3, 6)   /1H ,1H ,1H /
        DATA XTB( 3, 7),XTB( 3, 8),XTB( 3, 9)   /     2,    12,     0/
        DATA XTB( 3,10),XTB( 3,11),XTB( 3,12)   /     0,     0,    10/
C
        DATA XTB( 4, 1),XTB( 4, 2),XTB( 4, 3)   /1HY,1H ,1H /
        DATA XTB( 4, 4),XTB( 4, 5),XTB( 4, 6)   /1H ,1H ,1H /
        DATA XTB( 4, 7),XTB( 4, 8),XTB( 4, 9)   /     2,    13,     0/
        DATA XTB( 4,10),XTB( 4,11),XTB( 4,12)   /     0,     0,    10/
C
        DATA XTB( 5, 1),XTB( 5, 2),XTB( 5, 3)   /1HN,1H ,1H /
        DATA XTB( 5, 4),XTB( 5, 5),XTB( 5, 6)   /1H ,1H ,1H /
        DATA XTB( 5, 7),XTB( 5, 8),XTB( 5, 9)   /     2,    14,     0/
        DATA XTB( 5,10),XTB( 5,11),XTB( 5,12)   /     0,    16,    10/
C
        DATA XTB( 6, 1),XTB( 6, 2),XTB( 6, 3)   /1HW,1H ,1H /
        DATA XTB( 6, 4),XTB( 6, 5),XTB( 6, 6)   /1H ,1H ,1H /
        DATA XTB( 6, 7),XTB( 6, 8),XTB( 6, 9)   /     3,     1,     0/
        DATA XTB( 6,10),XTB( 6,11),XTB( 6,12)   /     0,     0,     0/
C
        DATA XTB( 7, 1),XTB( 7, 2),XTB( 7, 3)   /1HL,1H ,1H /
        DATA XTB( 7, 4),XTB( 7, 5),XTB( 7, 6)   /1H ,1H ,1H /
        DATA XTB( 7, 7),XTB( 7, 8),XTB( 7, 9)   /     3,     2,     0/
        DATA XTB( 7,10),XTB( 7,11),XTB( 7,12)   /     0,     0,     0/
C
        DATA XTB( 8, 1),XTB( 8, 2),XTB( 8, 3)   /1HD,1H ,1H /
        DATA XTB( 8, 4),XTB( 8, 5),XTB( 8, 6)   /1H ,1H ,1H /
        DATA XTB( 8, 7),XTB( 8, 8),XTB( 8, 9)   /     3,     3,     0/
        DATA XTB( 8,10),XTB( 8,11),XTB( 8,12)   /     0,     0,     0/
C
        DATA XTB( 9, 1),XTB( 9, 2),XTB( 9, 3)   /1HI,1H ,1H /
        DATA XTB( 9, 4),XTB( 9, 5),XTB( 9, 6)   /1H ,1H ,1H /
        DATA XTB( 9, 7),XTB( 9, 8),XTB( 9, 9)   /     3,     4,     0/
        DATA XTB( 9,10),XTB( 9,11),XTB( 9,12)   /     0,     0,     0/
C
        DATA XTB(10, 1),XTB(10, 2),XTB(10, 3)   /1HH,1H ,1H /
        DATA XTB(10, 4),XTB(10, 5),XTB(10, 6)   /1H ,1H ,1H /
        DATA XTB(10, 7),XTB(10, 8),XTB(10, 9)   /     3,     5,     0/
        DATA XTB(10,10),XTB(10,11),XTB(10,12)   /     0,     0,     0/
C
        DATA XTB(11, 1),XTB(11, 2),XTB(11, 3)   /1HF,1H ,1H /
        DATA XTB(11, 4),XTB(11, 5),XTB(11, 6)   /1H ,1H ,1H /
        DATA XTB(11, 7),XTB(11, 8),XTB(11, 9)   /     3,     7,     0/
        DATA XTB(11,10),XTB(11,11),XTB(11,12)   /     0,     0,     0/
C
        DATA XTB(12, 1),XTB(12, 2),XTB(12, 3)   /1HD,1HU,1HM/
        DATA XTB(12, 4),XTB(12, 5),XTB(12, 6)   /1HP,1H ,1H /
        DATA XTB(12, 7),XTB(12, 8),XTB(12, 9)   /     5,     0,     1/
        DATA XTB(12,10),XTB(12,11),XTB(12,12)   /     0,     0,     0/
C
        DATA XTB(13, 1),XTB(13, 2),XTB(13, 3)   /1HE,1HR,1HR/
        DATA XTB(13, 4),XTB(13, 5),XTB(13, 6)   /1HL,1HI,1HM/
        DATA XTB(13, 7),XTB(13, 8),XTB(13, 9)   /     2,    16,    10/
        DATA XTB(13,10),XTB(13,11),XTB(13,12)   /     0,     0,    10/
C
        DATA XTB(14, 1),XTB(14, 2),XTB(14, 3)   /1HM,1HE,1HM/
        DATA XTB(14, 4),XTB(14, 5),XTB(14, 6)   /1HS,1HI,1HZ/
        DATA XTB(14, 7),XTB(14, 8),XTB(14, 9)   /     5,     0,     2/
        DATA XTB(14,10),XTB(14,11),XTB(14,12)   /     0,     0,     0/
C
        DATA XTB(15, 1),XTB(15, 2),XTB(15, 3)   /1HS,1HT,1HE/
        DATA XTB(15, 4),XTB(15, 5),XTB(15, 6)   /1HP,1H ,1H /
        DATA XTB(15, 7),XTB(15, 8),XTB(15, 9)   /     3,    17,     1/
        DATA XTB(15,10),XTB(15,11),XTB(15,12)   /     0,     0,     0/
C
C
C.
C..
C...
        IF(INIT.NE.0) GO TO 20
C
        IERROR=0
        CALL CSI(XTB,15,ISTAT,40,0)
C
C====== STEP MODE?
C
        CALL APMODE(0)
        IF(ISTAT(38).EQ.1) CALL APMODE(1)
C
        GO TO 30
C
20      CALL CSI(XTB,15,ISTAT,40,1)
C
30      CONTINUE
        RETURN
        END
C****** LPTST3 = LOOP TEST FOR USER INPUT                   = REL 5.0  , NOV 79
        SUBROUTINE LPTST(IRTURN)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : DGCOM>LPTST                                              *
C  *    DATE        : NOV  1,1979                                             *
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
        COMMON /TOTL/  IENTRY(20),RMDSIZ(17),IDUMMY,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),FFTERR) , (ISTAT( 7),DMPSIM) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IPSLIM) , (ISTAT(10),ERRCNT) , (ISTAT(11),PASCNT)
        EQUIVALENCE
     +  (ISTAT(12),   IXN) , (ISTAT(13),   IYN) , (ISTAT(14),SIMULS),
     +  (ISTAT(15), IERRS) , (ISTAT(16),IERLIM) , (ISTAT(17), ISTEP)
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
C****** COMPAR = COMPARE ANSWERS FOR ART100                 = REL 5.0  , NOV 79
        SUBROUTINE COMPAR
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>COMPAR                                            *
C  *    DATE        : NOV  1,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : ALL THE ARGUMENTS NEEDED FOR COMPARISONS ARE PASSED      *
C  *                  IN THE COMMON /ART100/                                   *
C  *                                                                           *
C  *    EXIT        : RETURNS A FLAG IN COMMON WHICH INDICATES WHETHER AN      *
C  *                  ERROR WAS FOUND.                                         *
C  *                                                                           *
C  *    FUNCTION    : COMPAR COMPARES THE ARITHMETIC RESULTS PRODUCED BY THE   *
C  *                  ARRAY PROCESSOR AND THE RESULTS PRODUCED BY THE          *
C  *                  SOFTWARE SIMULATOR.  THE FOLLOWING VALUES ARE COMPARED   *
C  *                  IN THIS ROUTINE: STATUS                                  *
C  *                                   S-PADS                                  *
C  *                                   DATA PADS                               *
C  *                  -                                                        *
C  *                  IF ANY ERRORS ARE DETECTED, THE ROUTINE WILL PRINT OUT   *
C  *                  N, THE RANDOM NUMBER SEEDS, THE HARDWARE STATUS, AND     *
C  *                  THE DPA VALUE.  TO FIND N, SUBRACT THE RELATIVE DPX      *
C  *                  ADDRESS FROM THE BASE ADDRESS.                           *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER ITTI,ITTO,LFIL,RJUSFY,ISTAT,IRTURN,ICHNGE,WATERR,
     +  LOOPER,TYPDIS,IORST,HALT,FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,
     +  IERLIM,IPSLIM,ERRCNT,PASCNT,IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ,
     +  PSIZE,BASLOW,MDSIZ,IAPNUM,IECHO,IERRS,IMSIZ,IPSIZE,NPAGES
        INTEGER
     +  SDPX,SDPY,SSP,SSTAT,HDPX,HDPY,HSP,HSTAT,XSAVE,YSAVE,DPARG,N,
     +  STRTPS,FINPS,D1(6),D2(6),D3(6),D4(6),D5(6),DD1(6,3),DD2(6,3),I
        INTEGER J,TEMP
C
        REAL RMDSIZ
C
        COMMON /INSTAL/ ITTI,ITTO,LFIL
C
        COMMON /ART100/ SDPX(3,8),SDPY(3,8),SSP(16),SSTAT,HDPX(3,8),
     1  HDPY(3,8),HSP(16),HSTAT,XSAVE,YSAVE,DPARG,N,STRTPS(4,4),
     2  FINPS(4,8)
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL/  IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IRADX ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),FFTERR) , (ISTAT( 7),DMPSIM) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IPSLIM) , (ISTAT(10),ERRCNT) , (ISTAT(11),PASCNT)
        EQUIVALENCE
     +  (ISTAT(12),   IXN) , (ISTAT(13),   IYN) , (ISTAT(14),SIMULS),
     +  (ISTAT(15), IERRS) , (ISTAT(16),IERLIM) , (ISTAT(17), ISTEP)
C.
C..
C...
C====== CLEAR GLOBAL ERROR FLAG
C
          IERRS=0
C
C====== CHECK STATUS
C
          IF (HSTAT.EQ.SSTAT) GO TO 10
          IERRS=1
         ERRCNT=ERRCNT+1
        CALL I2ASCI(6,SSTAT,D1,IRADX,0)
        CALL I2ASCI(6,HSTAT,D2,IRADX,0)
          WRITE(ITTO,2000) D1,D2
        IF(IECHO.NE.0) WRITE(LFIL,2000) D1,D2
C
C====== CHECK S-PAD
C
10        DO 20 I=1,16
          IF (HSP(I).EQ.SSP(I)) GO TO 20
          IERRS=1
         ERRCNT=ERRCNT+1
        CALL I2ASCI(6,I-1,D1,IRADX,0)
        CALL I2ASCI(6,SSP(I),D2,IRADX,0)
        CALL I2ASCI(6,HSP(I),D3,IRADX,0)
          WRITE(ITTO,2010) D1,D2,D3
        IF(IECHO.NE.0) WRITE(LFIL,2010) D1,D2,D3
         CALL TERM(TEMP)
          IF (FFTERR.EQ.0.OR.TEMP.EQ.1) GO TO 30
20        CONTINUE
C
C====== CHECK DPX
C
30        DO 70 I=1,8
          DO 40 J=1,3
          IF (HDPX(J,I).NE.SDPX(J,I)) GO TO 50
40        CONTINUE
          GO TO 70
50        IERRS=1
         ERRCNT=ERRCNT+1
        CALL I2ASCI(6,MOD(I+DPARG+27,32),DI,IRADX,0)
          DO 60 J=1,3
        CALL I2ASCI(6,SDPX(J,I),DD1(1,J),IRADX,0)
60      CALL I2ASCI(6,HDPX(J,I),DD2(1,J),IRADX,0)
          WRITE(ITTO,2020) D1,DD1,DD2
        IF(IECHO.NE.0) WRITE(LFIL,2020) D1,DD1,DD2
          CALL TERM(TEMP)
          IF (FFTERR.EQ.0.OR.TEMP.EQ.1) GO TO 80
70        CONTINUE
C
C====== CHECK DPY
C
80        DO 120 I=1,8
          DO 90 J=1,3
          IF (HDPY(J,I).NE.SDPY(J,I)) GO TO 100
90        CONTINUE
          GO TO 120
100       IERRS=1
        ERRCNT=ERRCNT+1
        CALL I2ASCI(6,MOD(I+DPARG+27,32),D1,IRADX,0)
          DO 110 J=1,3
        CALL I2ASCI(6,SDPY(J,I),DD1(1,J),IRADX,0)
110     CALL I2ASCI(6,HDPY(J,I),DD2(1,J),IRADX,0)
          WRITE(ITTO,2030) D1,DD1,DD2
        IF(IECHO.NE.0) WRITE(LFIL,2030) D1,DD1,DD2
          CALL TERM(TEMP)
          IF (FFTERR.EQ.0.OR.TEMP.EQ.1) GO TO 130
120       CONTINUE
C
C====== IF ANY ERRORS, PRINT OUT N, SEEDS, H/W STATUS, AND DPA
C
130       IF (IERRS.EQ.0) RETURN
        CALL I2ASCI(6,N,D1,IRADX,0)
        CALL I2ASCI(6,XSAVE,D2,10,0)
        CALL I2ASCI(6,YSAVE,D3,10,0)
        CALL I2ASCI(6,HSTAT,D4,IRADX,0)
        CALL I2ASCI(6,DPARG,D5,IRADX,0)
          WRITE(ITTO,2040) D1,D2,D3,D4,D5
        IF(IECHO.NE.0) WRITE(LFIL,2040) D1,D2,D3,D4,D5
C
C====== FORMAT STATEMENTS
C
2000    FORMAT(20H APSTATUS MISCOMPARE,/,5X,3H E ,6A1,/,5X,3H A ,6A1)
2010    FORMAT(17H S-PAD MISCOMPARE,/,9H SP ADDR=,6A1,/,5X,3H E ,6A1,/,
     +  5X,3H A ,6A1)
2020    FORMAT(15H DPX MISCOMPARE,/,10H DPX ADDR=,6A1,/,5X,3H E ,
     +  3(6A1,2X),/,5X,3H A ,3(6A1,2X))
2030    FORMAT(15H DPY MISCOMPARE,/,10H DPY ADDR=,6A1,/,5X,3H E ,
     +  3(6A1,2X),/,5X,3H A ,3(6A1,2X))
2040    FORMAT(3H N ,6A1,6H  X,Y ,2(6A1,1X),/,10H APSTATUS ,6A1,
     +  6H  DPA ,6A1,//)
C
          RETURN
          END
C
C
C****** DUMPPS = DUMP PROGRAM SOURCE                        = REL 5.0  , NOV 79
        SUBROUTINE DUMPPS(FROM,TOO,ITTO,IRADX)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>DUMPPS                                            *
C  *    DATE        : NOV  1,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : FROM => BEGINNING PROGRAM SOURCE ADDRESS                 *
C  *                  TOO  => ENDING PROGRAM SOURCE ADDRESS                    *
C  *                  ITTO => DEVICE TO WHICH PROGRAM SOURCE DATA IS WRITTEN   *
C  *                  IRADX => OUTPUT RADIX
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : DUMPPS DUMPS PROGRAM SOURCE INTO AN ERROR MESSAGE IF     *
C  *                  AN ERROR IS DETECTED IN COMPAR (ART100 SUBROUTINE).      *
C  *                  PROGRAM SOURCE WORDS ARE SPLIT UP INTO 24 FIELDS.        *
C  *                  PROGRAM SOURCE MEMORY IS DUMPED BEGINNING AT LOCATION    *
C  *                  'FROM' AND ENDING AT LOCATION 'TOO'.                     *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER FROM,TOO,ITTO,ITTI,IDUMMY,LFIL,RJUSFY,REG(4),FV(26),
     +  D1(6),D24(6,24),FROM1,TO1,I,J,K,L
C
        INTEGER ISTAT,IRTURN,ICHNGE
C
        REAL RMDSIZ
C
        COMMON /INSTAL/ ITTI,IDUMMY,LFIL
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL/  IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IDUMB ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),FFTERR) , (ISTAT( 7),DMPSIM) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IPSLIM) , (ISTAT(10),ERRCNT) , (ISTAT(11),PASCNT)
        EQUIVALENCE
     +  (ISTAT(12),   IXN) , (ISTAT(13),   IYN) , (ISTAT(14),SIMULS),
     +  (ISTAT(15), IERRS) , (ISTAT(16),IERLIM) , (ISTAT(17), ISTEP)
C.
C..
C...
C
          WRITE(ITTO,2000)
        IF(IECHO.NE.0) WRITE(LFIL,2000)
          FROM1=FROM+1
          TO1=TOO+1
          DO 20 I=FROM1,TO1
          CALL APEXAM(REG,9,I-1)
C>
          CALL SPLIT(REG,FV)
C>
        CALL I2ASCI(6,I-1,D1,IRADX,0)
          DO 10 J=1,24
10      CALL I2ASCI(6,FV(J),D24(1,J),IRADX,0)
          WRITE(ITTO,2010) (D1(K),K=3,6),((D24(L,K),L=5,6),K=1,24)
        IF(IECHO.NE.0) WRITE(LFIL,2010)(D1(K),K=3,6),((D24(L,K),
     +                 L=5,6),K=1,24)
20        CONTINUE
C
C====== FORMAT STATEMENTS
C
2000      FORMAT(1X/5X,39H  B SP SH SS SD FA A1 A2 CO DI DX DY DB,
     1                 33H XR YR XW YW FM M1 M2 MI MA DP TM)
2010      FORMAT(1H ,4A1,24(1X,2A1))
C
          RETURN
          END
C
C
C****** SPADS = SPAD SIMULATOR                              = REL 5.0  , NOV 79
        SUBROUTINE SPADS
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>SPADS                                             *
C  *    DATE        : NOV  1,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : ALL AGRUMENTS ARE PASSED IN COMMON                       *
C  *                                                                           *
C  *    EXIT        : ALL AGRUMENTS ARE PASSED IN COMMON                       *
C  *                                                                           *
C  *    FUNCTION    : SIMULATES THE S-PAD OF THE AP                           *
C  *                                                                           *
C  *    GENERALITIES: 1.  THE   AP    PROGRAM SOURCE WORD DECODED IN THE ARRAY *
C  *                      'FV' OF COMMON /FVV/ DETERMINES THE OPERATIONS       *
C  *                       PERFORMED BY THE ROUTINE SPADS.                     *
C  *                  2.  IF AN S-PAD NOP IS DECODED, THEN THE MOST RECENT     *
C  *                      S-PAD OPERATION IS RE-PERFORMED USING THE 'MEMORY'   *
C  *                      IN THE COMMON /SPAD/ TO REMEMBER THAT OPERATION.     *
C  *                  3.  THE RESULTING S-PAD FUNCTION 'SPFN' IS STORED        *
C  *                      IN THE COMMON /REG/.                                 *
C  *                  4.  IF A 'NO-LOAD' IS NOT SPECIFIED, AND THE CURRENT     *
C  *                      OPERATION IS NOT A NOP, THEN THE RESULT IS STORED    *
C  *                      INTO 'SP' IN THE COMMON /REG/.                       *
C  *                  5.  THE S-PAD STATUS BITS FROM A NON S-PAD NOP ARE PUT   *
C  *                      INTO 'STATUS' IN THE COMMON /REG/.                   *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER BL,SOPL,SHL,SPSL,SPDL,SPADL,LDSPD,SOPF,SPSF,SPDF,CONDF,
     +  FV,SP,CB,MDR,MI,TMR,DPXR,DPYR,DPXW,DPYW,SPFN,INBS,DPBS,PNLBS,FM,
     +  FA,ZERO,PSA,MA,TMA,DPA,STATUS,DA,FLAGS,SRA,SWITCH,LITES,APMA,
     +  HMA,WC,CTL,BITREV,ARG1,ARG2,CARRY,I,IC,SPOP,NZC
C
        COMMON /SPAD/   SPADL(5),LDSPD
C
        COMMON /FVV/    FV(26)
C
        COMMON /REG/    SP(16),CB(4),MDR(3),MI(3),TMR(3),DPXR(3),
     1  DPYR(3),DPXW(3),DPYW(3),SPFN,INBS(3),DPBS(3),PNLBS,FM(3),FA(3),
     2  ZERO(4),PSA,MA,TMA,DPA,STATUS,DA,FLAGS,SRA,SWITCH,LITES,APMA,
     3  HMA,WC,CTL
C
        EQUIVALENCE (SPADL(1),BL),(SPADL(2),SOPL),(SPADL(3),SHL),
     +  (SPADL(4),SPSL),(SPADL(5),SPDL),(FV(2),SOPF),(FV(4),SPSF),
     +  (FV(5),SPDF),(FV(9),CONDF)
C
C.
C..
C...
C====== SPOP' REMEMBERS IF WE HAD AN S-PAD OP THIS INSTRUCTION
C
          SPOP=0
C
C====== LATCH CURRENT S-PAD OPERATION UNLESS AN S-PAD NO-OP
C
          IF (SOPF.EQ.1.OR.(SOPF.EQ.0.AND.SPSF.LE.7)) GO TO 20
          DO 10 I=1,4
10        SPADL(I)=FV(I)
          SPOP=1
C
C====== IF LAST INSTRUCTION HAD AN 'LDSPD' DON'T CHANGE SPD
C
        IF(LDSPD)20,50,20
C
C====== CLEAR CARRY AND SPFN
C
20        LDSPD=0
          CARRY=0
          SPFN=0
          NZC=0
C
C====== FETCH SPS & SPD, AND BIT-REVERSE IF CALLED FOR
C
          ARG1=SP(SPSL+1)
          ARG2=SP(SPDL+1)
          IF (BL.EQ.1) ARG1=BITREV(ARG1,IAND16(STATUS,7))
C
C====== GO DO FUNCTION
C
          I=SOPL+1
          GO TO (30,210,100,110,120,130,140,150),I
C
C====== SINGLE OP INSTRUCTIONS
C
30        I=SPSL+1
          GO TO (210,210,210,210,210,210,210,210,
     1           40,60,70,80,90,90,90,90),I
C
C====== CLR,   SET CARRY IF ARG2 NEGATIVE
C
40        SPFN=0
          IF (NAND16(ARG2,32767).NE.0) CARRY=1
          GO TO 160
50      ITEMP2=SPDF
        SPDL=ITEMP2
           GOTO 20
C
C====== INC
C
60        SPFN=IADDC(ARG2,1,CARRY)
          GO TO 160
C
C====== DEC
C
70        SPFN=IADDC(ARG2,IADD16(INOT16(1),1),CARRY)
          GO TO 160
C
C====== COM,       SET CARRY UNLESS ARG2=0
C
80        SPFN=INOT16(ARG2)
          IF (ARG2.NE.0) CARRY=1
          GO TO 160
C
C====== LDSPNL, LDSPI, LDSPI, LDSPT:    SPFN=SP(SPD),  CARRY=1
C
90        SPFN=ARG2
          CARRY=1
          GO TO 160
C
C====== DOUBLE OPERAND
C       ADD
C
100       SPFN=IADDC(ARG1,ARG2,CARRY)
          GO TO 160
C
C====== SUB, ARG2-ARG1  CARRY IS OR OF NEGATE & ADD
C
110       SPFN=IADDC(IADDC(INOT16(ARG1),1,IC),ARG2,CARRY)
          IF (IC.EQ.1) CARRY=1
          GO TO 160
C
C====== MOV,   CARRY IS CARRY FROM (ARG1.OR.ARG2) + (.NOT.ARG1.A
C
120       SPFN=ARG1
          I=IADDC(IOR16(ARG1,ARG2),IAND16(INOT16(ARG1),ARG2),CARRY)
          GO TO 160
C
C====== AND    CARRY IS CARRY FROM  .NOT.ARG1.AND.ARG2 + ARG2
C
130       SPFN=IAND16(ARG1,ARG2)
          I=IADDC(IAND16(INOT16(ARG1),ARG2),ARG2,CARRY)
          GO TO 160
C
C====== OR
C
140       SPFN=IOR16(ARG1,ARG2)
          GO TO 160
C
C====== EQUIVALENCE = .NOT.((.NOT.ARG1.AND.ARG2).OR.(ARG1.AND.NO
C       CARRY IS CARRY FROM  .NOT.ARG1 + ARG2
C
150     SPFN=INOT16(IOR16(IAND16(INOT16(ARG1),ARG2),IAND16(ARG1,
     1  INOT16(ARG2))))
          I=IADDC(INOT16(ARG1),ARG2,CARRY)
          GO TO 160
C
C====== SHIFT IF REQUIRED
C       SHIFTS ARE LOGICAL THROUGH CARRY BIT,
C       I.E. BITS ARE SHIFTED OUT THROUGH CARRY, ZEROS ARE SHIFT
C
160       IF (SHL.EQ.0) GO TO 200
          CARRY=0
          I=SHL+1
          GO TO (200,170,180,190),I
C
C====== SHIFT LEFT ONCE
C
170       IF (NEGCHK(SPFN).EQ.1) CARRY=1
          SPFN=ILSH16(SPFN,1)
          GO TO 200
C
C====== SHIFT RIGHT TWICE
C
180       IF (IAND16(SPFN,2).NE.0) CARRY=1
          SPFN=IRSH16(SPFN,2)
          GO TO 200
C
C====== SHIFT RIGHT ONCE
C
190       IF (IAND16(SPFN,1).NE.0) CARRY=1
          SPFN=IRSH16(SPFN,1)
          GO TO 200
C
C====== IF CURRENT SPAD OP IS A NOP (SPOP=0), RETURN,
C       ELSE STORE SPFN (UNLESS LO-LOAD SET), AND
C       SET 'NZC' CONDITION BITS IN AP-STATUS
C
200       IF (SPOP.EQ.0) RETURN
          IF (CONDF.NE.2) SP(SPDL+1)=SPFN
          IF (SPFN.EQ.0) NZC=NZC+1024
          IF (NEGCHK(SPFN).EQ.1) NZC=NZC+512
          IF (CARRY.EQ.1) NZC=NZC+256
          STATUS=IOR16(NAND16(STATUS,1792),NZC)
210       RETURN
          END
C
C
C****** BITREV = BIT-REVERSE FOR THE SIMULATOR              = REL 5.0  , NOV 79
        INTEGER FUNCTION BITREV(IA,ISHFT)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>BITREV                                            *
C  *    DATE        : NOV  1,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : IA    => VALUE TO WHICH THE BIT-REVERSE IS PERFORMED     *
C  *                  ISHFT => NUMBER OF RIGHT SHIFTS TO THE RESULT            *
C  *                                                                           *
C  *    EXIT        : BITREV <= THE BIT-REVERSED AND SHIFTED VERSION OF IA     *
C  *                                                                           *
C  *    FUNCTION    : THE DATA WORDS IN THIS DISCUSSION WILL HAVE THE BIT      *
C  *                  CONVENTION THAT THE MOST SIGNIFICANT BIT IS 0, AND       *
C  *                  THE LEAST SIGNIFICANT BIT IS 15.  'BITREV', WHICH        *
C  *                  IS THE RESULT OF THIS INTEGER FUNCTION, IS THE           *
C  *                  PRODUCT OF A 15-BIT WIDE BIT-REVERSE OF 'IA'(0-14), THE  *
C  *                  INPUT DATA WORD, FOLLOWED BY A RIGHT SHIFT  WITH ZERO    *
C  *                  FILL FOR 'ISHIFT' PLACES.  THE LOW BIT (BIT 15) OF       *
C  *                  THE RESULT IS RETURNED AS A ZERO.                        *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER I,J
C
C.
C..
C...
C
          BITREV=0
          J=IA
C
C====== DO 15-BIT BIT-REVERSE, I.E. RUN THE BITS FROM THE
C       HIGH END OF J INTO THE HIGH END OF BITREV
C
          DO 10 I=1,15
          BITREV=IOR16(IRSH16(BITREV,1),NAND16(J,32767))
10        J=ILSH16(J,1)
C
C====== RIGHT SHIFT 'BITREV' AND CLEAR LSB
C
          BITREV=NAND16(IRSH16(BITREV,ISHFT),1)
          RETURN
          END
C
C
C****** FADDS = FLOATING ADDER SIMULATOR                    = REL 5.0  , NOV 79
        SUBROUTINE FADDS
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>FADDS                                             *
C  *    DATE        : NOV  1,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : ARGUMENTS PASSED BY COMMON                               *
C  *                                                                           *
C  *    EXIT        : ARGUMENTS PASSED BY COMMON                               *
C  *                                                                           *
C  *    FUNCTION    : SIMULATES THE   AP    FLOATING ADDER.                    *
C  *                                                                           *
C  *    GENERALITIES: 1.  PICKS ITS INPUT AS DIRECTED BY THE 'FADD','A1', AND  *
C  *                      'A2' FIELDS OF THE PROGRAM SOURCE WORD PREVIOUSLY    *
C  *                      DECODED INTO 'FVV' IN THE COMMON /FVV/.              *
C  *                  2.  THE INPUTS ARE SELECTED FROM THE POSSIBLE INPUTS     *
C  *                      IN /REG/.                                            *
C  *                  3.  A TWO STAGE PIPELINE IS MAINTAINED TO SIMULATE THE   *
C  *                      HARDWARE.                                            *
C  *                  4.  FLOATING POINT NUMBERS ARE INPUT IN THE FORMAT:      *
C  *                    A) WORD #1 -- EXPONENT ------- IN RIGHT 10 BITS        *
C  *                    B) WORD #2 -- HIGH MANTISSA -- IN RIGHT 12 BITS        *
C  *                    C) WORD #3 -- LOW MANTISSA --- IN RIGHT 16 BITS        *
C  *                  5.  APPLICABLE ERROR AND CONDITION BITS ARE PACKED INTO  *
C  *                      THE 6 BITS ABOVE THE EXPONENT (THIS OUTPUT VALUE     *
C  *                      IS 'FA' IN THE COMMON /REG/.                         *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER E1,E2,M1,M2,MXCNT,TRUNC,ISMF,FIXF,ZFLG,ZRES,RNDCON,NEG1,
     +  PROD,FARG1,FARG2,FA1,FA2,MARG1,MARG2,FM1,FM2,FM3,M1H,M1L,M2H,
     +  M2L,FADDF,A1F,A2F,FV,SP,CB,MDR,MI,TMR,DPXR,DPYR,DPXW,DPYW,SPFN,
     +  INBS,DPBS,PNLBS,FM,FA,ZERO,PSA,MA,TMA,DPA,STATUS,DA,FLAGS,SRA
        INTEGER
     +  SWITCH,LITES,APMA,HMA,WC,CTL,IC,I
C
        COMMON /NORM/   E1,E2,M1(2),M2(2),MXCNT,TRUNC,ISMF,FIXF,ZFLG,
     1  ZRES(16),RNDCON(4),NEG1(2),PROD(2),FARG1(3),FARG2(3),FA1(3),
     2  FA2(3),MARG1(3),MARG2(3),FM1(3),FM2(3),FM3(3)
C
        COMMON /FVV/    FV(26)
C
        COMMON /REG/    SP(16),CB(4),MDR(3),MI(3),TMR(3),DPXR(3),
     1  DPYR(3),DPXW(3),DPYW(3),SPFN,INBS(3),DPBS(3),PNLBS,FM(3),FA(3),
     2  ZERO(4),PSA,MA,TMA,DPA,STATUS,DA,FLAGS,SRA,SWITCH,LITES,APMA,
     3  HMA,WC,CTL
C
        EQUIVALENCE (M1(1),M1H),(M1(2),M1L),(M2(1),M2H),(M2(2),M2L),
     +  (FV(6),FADDF),(FV(7),A1F),(FV(8),A2F)
C
C.
C..
C...
C====== SEE IF WE HAD A FADD
C
          IF (FADDF.EQ.7.OR.(FADDF.EQ.0.AND.A1F.EQ.0)) GO TO 320
C
C====== ADVANCE PIPELINE & INITIALIZE
C
          CALL RMOV(FA1,FA2,3)
          MXCNT=27
          TRUNC=0
          ISMF=0
          FIXF=0
C
C====== FETCH ARG1
C
          I=A1F+1
          GO TO (60,10,20,30,40,50,50,50),I
10        CALL RMOV(FM,FARG1,3)
          GO TO 60
20        CALL RMOV(DPXR,FARG1,3)
          GO TO 60
30        CALL RMOV(DPYR,FARG1,3)
          GO TO 60
40        CALL RMOV(TMR,FARG1,3)
          GO TO 60
50        CALL RMOV(ZERO,FARG1,3)
C
C====== GET ARG2
C
60        I=A2F+1
          GO TO (140,70,80,90,100,110,120,130),I
70        CALL RMOV(FA,FARG2,3)
          GO TO 140
80        CALL RMOV(DPXR,FARG2,3)
          GO TO 140
90        CALL RMOV(DPYR,FARG2,3)
          GO TO 140
100       CALL RMOV(MDR,FARG2,3)
          GO TO 140
110       CALL RMOV(ZERO,FARG2,3)
          GO TO 140
120       FARG2(1)=IOR16(IAND16(SPFN,511),IAND16(INOT16(SPFN),512))
          FARG2(2)=DPXR(2)
          FARG2(3)=DPXR(3)
          GO TO 140
130       FARG2(1)=DPXR(1)
          FARG2(2)=ILSH16(IAND16(SPFN,3),11)
          FARG2(3)=0
C
C====== UNPACK THE ARGUEMENTS
C
140       CALL FEXPND(FARG1,E1,M1)
          CALL FEXPND(FARG2,E2,M2)
C
C====== SOME SPECIAL THINGS FOR SINGLE OPERAND OPERATIONS
C
          IF (FADDF.NE.0) GO TO 150
          E1=0
          M1H=0
          M1L=0
C
C====== FOR FIX,FIXT SET E1 TO 28
C
          IF (A1F.EQ.1.OR.A1F.EQ.2) E1=512+28
C
C====== FOR FSCALE, FSCALT SET E1 TO SPFN
C
          IF (A1F.EQ.3.OR.A1F.EQ.6) E1=IOR16(IAND16(SPFN,511),
     1         IAND16(INOT16(SPFN),512))
C
C====== COMPARE EXPONENTS
C
150     I=IABS(E1-E2)
        I=-I
C
C====== MAKE ARG2 THE LARGER NUMBER (I.E. HAS LARGEST EXPONENT)
C
          IF (E2.GE.E1) GO TO 160
C
C====== SHIFT ARG2 RIGHT SINCE IT IS SMALLER
C
          E2=E1
          CALL IDSHFT(M2,I)
          GO TO 170
C
C====== SHIFT ARG1 RIGHT SINCE IT IS SMALLER
C
160       CALL IDSHFT(M1,I)
C
C====== GO DO THE FADDER OPERATION
C
170       I=FADDF+1
          GO TO (180,250,260,270,280,290,300,320),I
C
C====== SINGLE OP
C
180       I=A1F+1
          GO TO (320,190,200,200,210,230,190,240),I
C
C====== FIX,FSCALE
C
190       FIXF=1
          GO TO 310
C
C====== FIXT,FSCALT
C
200       TRUNC=1
          FIXF=1
          GO TO 310
C
C====== SIGN MAG TO 2'S COMPLEMENT
C
210       IF (NEGCHK(M2H).EQ.0) GO TO 310
C
C====== CLEAR SIGN BITS
C
          M2H=IAND16(M2H,16383)
C
C====== NEGATE MANTISSA
C
220       CALL IDNEG(M2)
          GO TO 310
C
C====== S COMPLEMENT TO SIGN MAGNITUDE
C
230       IF (NEGCHK(M2H).EQ.0) GO TO 310
C
C====== SET FLAG TO REMEMBER TO SET SIGN BIT WHEN DONE
C
          ISMF=1
          GO TO 220
C
C====== FABS
C
240       IF (NEGCHK(M2H).EQ.0) GO TO 310
          GO TO 220
C
C====== DOUBLE OPS
C       FSUBR    A2-FARG1
C
250       CALL IDNEG(M1)
          GO TO 270
C
C====== FSUB    FARG1-A2
C
260       CALL IDNEG(M2)
C
C====== FADD
C
270       M2L=IADDC(M1L,M2L,IC)
          M2H=IADD16(IADD16(M2H,IC),M1H)
          GO TO 310
C
C====== FEQV
C       EQUIVALENCE = .NOT.((.NOT.ARG1.AND.ARG2).OR.(ARG1.AND.NO
C
280   M2L=INOT16(IOR16(IAND16(INOT16(M1L),M2L),IAND16(M1L,INOT16(M2L))))
      M2H=INOT16(IOR16(IAND16(INOT16(M1H),M2H),IAND16(M1H,INOT16(M2H))))
          GO TO 310
C
C====== FAND
C
290       M2L=IAND16(M1L,M2L)
          M2H=IAND16(M1H,M2H)
          GO TO 310
C
C====== FOR
C
300       M2L=IOR16(M1L,M2L)
          M2H=IOR16(M1H,M2H)
          GO TO 310
C
C====== ZERO CHECK
C
310       CALL ZCHCK
C
C====== GO NORMALIZE & SET CONDITION BITS
C
          CALL NORMAL
C
C====== PACK UP AND RETURN
C
          CALL FCMPRS(FA1,E2,M2)
320       RETURN
          END
C
C
C****** ZCHCK = FLOATING ADDER ZERO CHECK                   = REL 5.0  , NOV 79
        SUBROUTINE ZCHCK
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>ZCHCK                                             *
C  *    DATE        : NOV  1,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : PARAMETERS ARE PASSED IN COMMON.                         *
C  *                  M2 - MANTISSA RESULT OF THE FLOATING ADDER               *
C  *                                                                           *
C  *    EXIT        : PARAMETERS ARE PASSED IN COMMON.                         *
C  *                  ZFLG - FLAG WHICH IS SET IF 'M2' IS ZERO                 *
C  *                                                                           *
C  *    FUNCTION    : ZFLAG CHECKS THE FLOATING ADDER FOR A ZERO RESULT.       *
C  *                  THE ROUTINE SETS 'ZFLG' IN THE COMMON /NORM/ TO          *
C  *                  A ONE IF THE MANTISSA , 'M2', OF THE FLOATING            *
C  *                  ADDER IS ZERO.                                           *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER E1,E2,M1,M2,MXCNT,TRUNC,ISMF,FIXF,ZFLG,ZRES,RNDCON,NEG1,
     +  PROD,FARG1,FARG2,FA1,FA2,MARG1,MARG2,FM1,FM2,FM3,M1H,M1L,M2H,
     +  M2L,IH,IL,IDX,IC
C
        COMMON /NORM/   E1,E2,M1(2),M2(2),MXCNT,TRUNC,ISMF,FIXF,ZFLG,
     1  ZRES(16),RNDCON(4),NEG1(2),PROD(2),FARG1(3),FARG2(3),FA1(3),
     2  FA2(3),MARG1(3),MARG2(3),FM1(3),FM2(3),FM3(3)
C
        EQUIVALENCE (M1(1),M1H),(M1(2),M1L),(M2(1),M2H),(M2(2),M2L)
C
C.
C..
C...
C====== ZERO CHECK RESIDUE TABLE (ZRES):
C       POS AND ROUND
C       NEG AND ROUND
C       POS AND TRUNC
C       NEG AND TRUNC
C
          ZFLG=0
          IH=M2H
          IL=M2L
C
C====== SET INDEX TO LOW TWO BITS OF MANTISSA
C
          IDX=IAND16(M2L,3)
C
C====== ADD 4 TO INDEX IF MANTISSA NEGATIVE
C
          IF (NEGCHK(M2H).EQ.1) IDX=IDX+4
C
C====== ADD 8 TO INDEX IF TRUNCATING
C
          IF (TRUNC.EQ.1) IDX=IDX+8
C
C====== NOW ADD FROM RESIDUE TABLE POINTED AT BY INDEX TO THE MA
C       WITH THE LOW TWO BITS CLEARED
C
          IL=IADDC(NAND16(IL,3),ZRES(IDX+1),IC)
          IH=IADD16(IH,IC)
C
C====== SET THE ZERO FLAG IF BOTH ZERO
C
          IF (IL.EQ.0.AND.IH.EQ.0) ZFLG=1
          RETURN
          END
C
C
C****** FMULS = FLOATING POINT MULTIPLIER SIMULATOR         = REL 5.0  , NOV 79
        SUBROUTINE FMULS
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>FMULS                                             *
C  *    DATE        : NOV  1,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : ARGUMENTS ARE PASSED IN COMMON                           *
C  *                                                                           *
C  *    EXIT        : ARGUMENTS ARE PASSED IN COMMON                           *
C  *                                                                           *
C  *    FUNCTION    : TO SIMULATE THE FLOATING MULTIPLIER IN THE   AP          *
C  *                                                                           *
C  *    GENERALITIES: 1.  PICKS ITS INPUT AS DIRECTED BY THE 'FM', 'M1', AND   *
C  *                      'M2' FIELDS OF THE PROGRAM SOURCE WORD CURRENTLY     *
C  *                      DECODED INTO 'FVV' IN THE COMMON /FVV/.              *
C  *                  2.  THE INPUTS ARE SELECTED FROM THE POSSIBLE INPUT      *
C  *                      VALUES IN THE COMMON /REG/.                          *
C  *                  3.  A THREE LAYER PIPELINE IS MAINTAINED TO SIMULATE     *
C  *                      THE ACTUAL HARDWARE.                                 *
C  *                  4.  FLOATING POINT NUMBERS ARE INPUT IN THE FORMAT:      *
C  *                    A) WORD #1 -- EXPONENT ------- IN RIGHT 10 BITS        *
C  *                    B) WORD #2 -- HIGH MANTISSA -- IN RIGHT 12 BITS        *
C  *                    C) WORD #3 -- LOW MANTISSA --- IN RIGHT 16 BITS        *
C  *                  5.  APPLICABLE ERROR AND CONDITION BITS ARE PACKED INTO  *
C  *                      THE 6 BITS ABOVE THE EXPONENT (THESE OUTPUT VALUES   *
C  *                      ARE IN 'FM1', 'FM2', AND 'FM3' OF THE COMMON /NORM/. *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER E1,E2,M1,M2,MXCNT,TRUNC,ISMF,FIXF,ZFLG,ZRES,RNDCON,NEG1,
     +  PROD,FARG1,FARG2,FA1,FA2,MARG1,MARG2,FM1,FM2,FM3,M1H,M1L,M2H,
     +  M2L,FMF,M1F,M2F,IVAL,FV,SP,CB,MDR,MI,TMR,DPXR,DPYR,DPXW,DPYW,
     +  SPFN,INBS,DPBS,PNLBS,FM,FA,ZERO,PSA,MA,TMA,DPA,STATUS,DA,FLAGS
        INTEGER
     +  SRA,SWITCH,LITES,APMA,HMA,WC,CTL,I
C
        COMMON /NORM/   E1,E2,M1(2),M2(2),MXCNT,TRUNC,ISMF,FIXF,ZFLG,
     1  ZRES(16),RNDCON(4),NEG1(2),PROD(2),FARG1(3),FARG2(3),FA1(3),
     2  FA2(3),MARG1(3),MARG2(3),FM1(3),FM2(3),FM3(3)
C
        COMMON /FVV/    FV(26)
C
        COMMON /REG/    SP(16),CB(4),MDR(3),MI(3),TMR(3),DPXR(3),
     1  DPYR(3),DPXW(3),DPYW(3),SPFN,INBS(3),DPBS(3),PNLBS,FM(3),FA(3),
     2  ZERO(4),PSA,MA,TMA,DPA,STATUS,DA,FLAGS,SRA,SWITCH,LITES,APMA,
     3  HMA,WC,CTL
C
        EQUIVALENCE (M1(1),M1H),(M1(2),M1L),(M2(1),M2H),(M2(2),M2L),
     +  (FV(18),FMF),(FV(19),M1F),(FV(20),M2F),(FV(26),IVAL)
C
C.
C..
C...
C====== SEE IF WE HADD A MULTIPLY
C
          IF (FMF.EQ.0.OR.IVAL.EQ.1) GO TO 110
C
C====== ADVANCE PIPELINE & INITIALIZE
C
          CALL RMOV(FM2,FM3,3)
          CALL RMOV(FM1,FM2,3)
          MXCNT=1
          TRUNC=0
          ISMF=0
          FIXF=0
          ZFLG=0
C
C====== FETCH ARG1
C
          I=M1F+1
          GO TO (10,20,30,40),I
10        CALL RMOV(FM,MARG1,3)
          GO TO 50
20        CALL RMOV(DPXR,MARG1,3)
          GO TO 50
30        CALL RMOV(DPYR,MARG1,3)
          GO TO 50
40        CALL RMOV(TMR,MARG1,3)
          GO TO 50
C
C====== FETCH ARG2
C
50        I=M2F+1
          GO TO (60,70,80,90),I
60        CALL RMOV(FA,MARG2,3)
          GO TO 100
70        CALL RMOV(DPXR,MARG2,3)
          GO TO 100
80        CALL RMOV(DPYR,MARG2,3)
          GO TO 100
90        CALL RMOV(MDR,MARG2,3)
          GO TO 100
C
C====== UNPACK THE ARGUEMENTS
C
100       CALL FEXPND(MARG1,E1,M1)
          CALL FEXPND(MARG2,E2,M2)
C
C====== ADD EXPONENTS
C
          E2=E1+E2-512
C
C====== SET UP FOR MANTISSA MULTIPLY
C       MAKE A NEGATIVE COPY OF ARG1 & CLEAR PRODUCT
C
          CALL RMOV(M1,NEG1,2)
          CALL IDNEG(NEG1)
          CALL RMOV(ZERO,PROD,2)
C
C====== DO 8 STEPS OF THE PRODUCT
C
          CALL BOOTH(8)
C
C====== NOW CLEAR THE BIT-BUCKET (THE LSB OF PROD) AND DO THE
C       REMAINING 21 STEPS
C
          PROD(2)=NAND16(PROD(2),1)
          CALL BOOTH(21)
C
C====== PUT PRODUCT INTO M2 AND TEST FOR ZERO
C
          CALL RMOV(PROD,M2,2)
          IF (M2H.EQ.0.AND.M2L.EQ.0) ZFLG=1
C
C====== NORMALIZE, PUT RESULT INTO PIPELINE & RETURN
C
          CALL NORMAL
          CALL FCMPRS(FM1,E2,M2)
110       RETURN
          END
C
C
C****** BOOTH = BOOTH'S ALGORITHM MULTIPLY                  = REL 5.0  , NOV 79
        SUBROUTINE BOOTH(ICOUNT)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>BOOTH                                             *
C  *    VERSION     : 3                                                        *
C  *    DATE        : JUNE 7,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : PARAMETERS ARE PASSED IN COMMON                          *
C  *                  M1(2) -> M1L & M1H => MULTPLICAND                        *
C  *                  M2(2) -> M2L & M2H => MULTIPLIER                         *
C  *                  ICOUNT => NUMBER OF STEPS IN THE BOOTH'S ALGORITHM       *
C  *                                                                           *
C  *    EXIT        : PROD(2) <- PL & PH <= PRODUCT OF 'M1' AND 'M2'           *
C  *                                                                           *
C  *    FUNCTION    : PERFORMS THE BOOTH'S ALGORITHM FOR MULTIPLYING TWO       *
C  *                  NUMBERS BY BIT COMPARISONS (EXACTLY SIMULATES THE        *
C  *                  HARDWARE).                                               *
C  *                   M1                                                      *
C  *                 X M2                                                      *
C  *                 ----                                                      *
C  *                 PROD                                                      *
C  *                                                                           *
C  *    GENERALITIES: BOOTH'S ALGORITHM PROCEDURE :                            *
C  *                  1.  BEGIN BY COMPARING A ZERO WITH THE LEAST SIGNIFICANT *
C  *                      BIT OF THE MULTIPLIER (M2).                          *
C  *                  2.  DO THE THE OPERATION THAT CORRESPONDS TO THE FOLLOW- *
C  *                      ING CHART:                                           *
C  *                      LEFT BIT RIGHT BIT  OPERATION        SHORT HAND      *
C  *                      ======== =========  ===============  ==========      *
C  *                         0         0      DO NOTHING         K+0           *
C  *                         0         1      ADD MULTIPLICAND   K+'M1'        *
C  *                         1         0      SUB MULTIPLICAND   K-'M1'        *
C  *                         1         1      DO NOTHING         K+0           *
C  *                      *NOTE: K IS THE PARTIAL PRODUCT                      *
C  *                      FOR STEP 1, THE LEFT BIT IS THE LSB OF THE MULT-     *
C  *                      IPLIER AND THE RIGHT BIT IS THE ZERO.  FOR STEP 1,   *
C  *                      K=0, BUT FOR ANY FURTHER COMPARISONS K=PARTIAL-      *
C  *                      PRODUCT.                                             *
C  *                  3.  AFTER STEP 1, ONLY THE TWO LEAST SIGNIFICANT BITS    *
C  *                      OF THE MULTIPLIER WILL BE COMPARED USING THE CHART   *
C  *                      IN STEP 2.  THE MULTIPLIER IS SHIFTED RIGHT ONCE     *
C  *                      AFTER EACH COMPARE TO PLACE NEW BITS IN THE LEAST    *
C  *                      SIGNIFICANT TWO BITS.                                *
C  *                  4.  THE INPUT PARAMETER, 'ICOUNT', DETERMINES THE NUMBER *
C  *                      OF SHIFT AND COMPARES PERFORMED.                     *
C  *                  5.  THE PRODUCT IS RETURNED AS 'PROD' IN THE COMMON      *
C  *                      /NORM/.                                              *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER ICOUNT,E1,E2,M1,M2,MXCNT,TRUNC,ISMF,FIXF,ZFLG,ZRES,
     +  RNDCON,NEG1,PROD,FARG1,FARG2,FA1,FA2,MARG1,MARG2,FM1,FM2,FM3,
     +  M1H,M1L,M2H,M2L,PH,PL,NEG1H,NEG1L,I,IDX,IC
C
        COMMON /NORM/   E1,E2,M1(2),M2(2),MXCNT,TRUNC,ISMF,FIXF,ZFLG,
     1  ZRES(16),RNDCON(4),NEG1(2),PROD(2),FARG1(3),FARG2(3),FA1(3),
     2  FA2(3),MARG1(3),MARG2(3),FM1(3),FM2(3),FM3(3)
C
        EQUIVALENCE (M1(1),M1H),(M1(2),M1L),(M2(1),M2H),(M2(2),M2L),
     +  (PROD(1),PH),(PROD(2),PL),(NEG1(1),NEG1H),(NEG1(2),NEG1L)
C
C.
C..
C...
C
          DO 30 I=1,ICOUNT
C
C====== SHIFT PROD RIGHT ONCE
C
          CALL IDSHFT(PROD,-1)
C
C====== SHIFT M2 RIGHT ONCE & DEFEAT BIT-BUCKET
C
          CALL IDSHFT(M2,-1)
          IDX=IAND16(M2L,3)+1
          M2L=NAND16(M2L,1)
C
C====== GO DO THE ADD/SUBTRACT
C
          GO TO (30,10,20,30), IDX
C
C====== ADD
C
10        PL=IADDC(PL,M1L,IC)
          PH=IADD16(IADD16(PH,M1H),IC)
          GO TO 30
C
C====== SUBTRACT, ADD NEG1 TO PROD
C
20        PL=IADDC(PL,NEG1L,IC)
          PH=IADD16(IADD16(PH,NEG1H),IC)
30        CONTINUE
          RETURN
          END
C
C
C****** NORMAL = NORMALIZE                                  = REL 5.0  , NOV 79
        SUBROUTINE NORMAL
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>NORMAL                                            *
C  *    DATE        : NOV  1,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : ALL PARAMETERS ARE PASSED IN COMMON.                     *
C  *                  IN COMMON /NORM/:                                        *
C  *                  E2    => INPUT AND OUTPUT EXPONENT OF FLOATING POINT     *
C  *                           NUMBER.                                         *
C  *                  M2    -> M2L & M2H => INPUT AND OUTPUT MANTISSA OF       *
C  *                                        FLOATING POINT NUMBER.             *
C  *                  MXCNT => MAXIMUM LEFT SHIFT ALLOWED DURING NORMALIZA-    *
C  *                           TION.                                           *
C  *                  TRUNC => TRUNCATION FLAG: 1 IF TRUNCATING                *
C  *                                            0 IF ROUNDING                  *
C  *                  FIXF  => FIX FLAG: 1 IF NO NORMALIZATION (I.E. A FIX)    *
C  *                                     0 IF NORMALIZATION                    *
C  *                  ISMF  => SIGN MAGNITUDE FLAG: 1 IF FLOATING POINT TO     *
C  *                                                  SIGN MAGNITUDE INSTRUC-  *
C  *                                                  TION.                    *
C  *                                                0 IF IT IS NOT.            *
C  *                  ZFLG  => ZERO FLAG: 1 IF INPUT HAS ZERO MANTISSA         *
C  *                                      0 IF INPUT HAS NON-ZERO MANTISSA     *
C  *                  RNDCON=> ROUNDING CONSTANTS (FROM BLOCK DATA)            *
C  *                                                                           *
C  *    EXIT        : ALL PARAMETERS ARE PASSED IN COMMON.                     *
C  *                  IN COMMON /NORM/:                                        *
C  *                  E2 <= INPUT AND OUTPUT EXPONENT OF FLOATING POINT        *
C  *                        NUMBER                                             *
C  *                  M2 <- M2L & M2H <= INPUT AND OUTPUT MANTISSA OF          *
C  *                                     FLOATING POINT NUMBER.                *
C  *                                                                           *
C  *    FUNCTION    : TO NORMALIZE, CONVERGANTLY ROUND, AND TO SET THE         *
C  *                  CONDITION BITS.  THE NORMAL SUBROUTINE IS USED           *
C  *                  BY 'FADDS' AND 'FMULS' WHICH DO FLOATING POINT ADDS AND  *
C  *                  MULTIPLIES.                                              *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER E1,E2,M1,M2,MXCNT,TRUNC,ISMF,FIXF,ZFLG,ZRES,RNDCON,NEG1,
     +  PROD,FARG1,FARG2,FA1,FA2,MARG1,MARG2,FM1,FM2,FM3,M1H,M1L,M2H,
     +  M2L,ROUND,FSTAT,ISGN,I,IC
C
        COMMON /NORM/   E1,E2,M1(2),M2(2),MXCNT,TRUNC,ISMF,FIXF,ZFLG,
     1  ZRES(16),RNDCON(4),NEG1(2),PROD(2),FARG1(3),FARG2(3),FA1(3),
     2  FA2(3),MARG1(3),MARG2(3),FM1(3),FM2(3),FM3(3)
C
        EQUIVALENCE (M1(1),M1H),(M1(2),M1L),(M2(1),M2H),(M2(2),M2L)
C
C.
C..
C...
C====== ROUND CONSTANTS (RNDCON):
C       POS AND ROUND
C       NEG AND ROUND
C       POS AND TRUNC
C       NEG AND TRUNC
C
          FSTAT=0
C
C====== CHECK FOR ZERO
C
          IF (ZFLG.EQ.1) GO TO 90
C
C====== REMEMBER SIGN
C
          ISGN=NEGCHK(M2H)
C
C====== SPECIAL FOR FIX & FSCALE, ETC. INHIBIT NORMALIZATION
C
          IF (FIXF.NE.1) GO TO 10
C
C====== SHIFT LEFT ONE, BUT PUT SIGN BACK INTO NEXT TO MSB
C
          E2=E2-1
          CALL IDSHFT(M2,1)
          M2H=IOR16(NAND16(M2H,16384),IAND16(IRSH16(M2H,1),16384))
          GO TO 60
C
C====== CHECK SIGN, POS OR NEG
C
10        IF (ISGN.EQ.1) GO TO 30
C
C====== POSITIVE MANTISSA:
C       X IS OVERFLOW
C       IS UNNORMALIZED
C       IS NORMALIZED
C       SEE IF OVERFLOW
C
          IF (IAND16(M2H,16384).NE.0) GO TO 50
C
C====== NORMALIZE LOOP FOR POSITIVE MANTISSA
C
          DO 20 I=1,MXCNT
          IF (IAND16(M2H,8192).NE.0) GO TO 60
          E2=E2-1
20        CALL IDSHFT(M2,1)
          GO TO 60
C
C====== NEGATIVE MANTISSA
C       X IS OVERFLOW
C       IS UNNORMALIZED
C       IS NORMALIZED
C       CHECK FOR OVERFLOW
C
30        IF (IAND16(M2H,16384).EQ.0) GO TO 50
C
C====== NORMALIZE LOOP FOR NEGATIVE MANTISSA
C
          DO 40 I=1,MXCNT
          IF (IAND16(M2H,8192).EQ.0) GO TO 60
          E2=E2-1
40        CALL IDSHFT(M2,1)
          GO TO 60
C
C====== MANTISSA OVERFLOW, DO AN ARITHMETIC RIGHT SHIFT
C
50        E2=E2+1
          CALL IDSHFT(M2,-1)
C
C====== IF ISM FLAG IS ON, SET SIGN BITS NOW
C
60        IF (ISMF.EQ.0) GO TO 70
          M2H=IOR16(M2H,INOT16(16383))
          ISGN=1
C
C====== PICK ROUNDING CONSTANT & DO THE ROUND
C
70        I=TRUNC*2+ISGN
          ROUND=RNDCON(I+1)
          M2L=IADDC(M2L,ROUND,IC)
          M2H=IADD16(M2H,IC)
C
C====== CHECK FOR MANTISSA OVERFLOW, SEE IF LOW SIGN BIT SAME AS
C
          I=IAND16(M2H,16384)
          IF((ISGN.EQ.0.AND.I.EQ.0).OR.(ISGN.EQ.1.AND.I.NE.0)) GOTO 80
C
C====== MANTISSA OVERFLOW ON ROUNDING
C       IF FIX, FSCALE, ETC, ZERO IS RESULT
C
          IF (FIXF.EQ.1) GO TO 90
C
C====== OTHERWISE SET MANTISSA TO 0.5 AND INCREMENT EXPONENT
C
          E2=E2+1
          M2H=8192
          M2L=0
C
C====== CHECK FOR EXPONENT UNDERFLOW
C       UNDERFLOW IF NEGATIVE
C
80        IF (E2.GE.0) GO TO 100
C
C====== UNDERFLOW, SET RESULT TO ZERO AND SET 'FU' ERROR BIT IN
C
          FSTAT=IOR16(FSTAT,16384)
C
C====== CLEAR RESULT AND SET FZ CONDITION BIT IN STATUS
C
90        E2=0
          M2H=0
          M2L=0
          FSTAT=IOR16(FSTAT,4096)
          GO TO 120
C
C====== CHECK FOR EXPONENT OVERFLOW
C       OVERFLOW IF GREATER THAN OR EQUAL TO 1024
C
100       IF (E2.LE.1023) GO TO 120
C
C====== OVERFLOW, SET TO SIGNED MAXIMUM AND SET 'FO' ERROR BIT
C
          E2=1023
          FSTAT=IOR16(FSTAT,INOT16(32767))
          IF (ISGN.EQ.1) GO TO 110
C
C====== POSITIVE, SET TO 3777, 177777
C
          M2H=16383
          M2L=INOT16(0)
          GO TO 120
C
C====== NEGATIVE, SET TO 4000, 0
C
110       M2H=INOT16(16383)
          M2L=0
C
C====== SET 'FN' IF NEGATIVE RESULT
C
120       IF (NEGCHK(M2H).EQ.1) FSTAT=IOR16(FSTAT,2048)
C
C====== OR STATUS BITS INTO EXPONENT WORD
C
          E2=IOR16(E2,FSTAT)
          RETURN
          END
C
C
C****** FEXPND = EXPAND FLOATING POINT WORD                 = REL 5.0  , NOV 79
        SUBROUTINE FEXPND(ARG,EXPO,MAN)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>FEXPND                                            *
C  *    DATE        : NOV  1,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : ARG  => INPUT FLOATING POINT NUMBER                      *
C  *                          ARG(1) - EXPONENT       IN RIGHT 10 BITS         *
C  *                          ARG(2) - HIGH MANTISSA  IN RIGHT 12 BITS         *
C  *                          ARG(3) - LOW MANTISSA   IN RIGHT 16 BITS         *
C  *                                                                           *
C  *    EXIT        : EXPO <= OUTPUT EXPONENT IN RIGHT 10 BITS                 *
C  *                  MAN  <= OUTPUT MANTISSA                                  *
C  *                          MAN(1) - UPPER 16 BITS  IN RIGHT 16 BITS         *
C  *                          MAN(2) - LOWER 16 BITS  IN RIGHT 16 BITS         *
C  *                                                                           *
C  *    FUNCTION    : BEFORE THE ARGUMENT IS WORKED WITH IN THE SIMULATOR      *
C  *                  SOFTWARE, THE FLOATING POINT NUMBER IS EXPANDED.         *
C  *                                                                           *
C  *    GENERALITIES: TO UNPACK: A DOUBLE WORD LOGICAL LEFT SHIFT              *
C  *                             OF FOUR IS PERFORMED, FOLLOWED BY A           *
C  *                             LOGICAL RIGHT SHIFT OF ONE.  THIS             *
C  *                             IS DONE TO THE MANTISSA. THE EXPONENT         *
C  *                             REMAINS UNCHANGED.                            *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER ARG(3),EXPO,MAN(2)
C
C.
C..
C...
C
          EXPO=ARG(1)
          MAN(1)=ARG(2)
          MAN(2)=ARG(3)
          CALL IDSHFT(MAN,4)
          CALL IDSHFT(MAN,-1)
          RETURN
          END
C
C
C****** FCMPRS = COMPRESS FLOATING POINT NUMBER             = REL 5.0  , NOV 79
        SUBROUTINE FCMPRS(ARG,EXPO,MAN)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>FCMPRS                                            *
C  *    DATE        : NOV  1,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : EXPO => INPUT EXPONENT IN RIGHT 10 BITS                  *
C  *                  MAN  => INPUT MANTISSA                                   *
C  *                          MAN(1) - UPPER 16 BITS   IN RIGHT 16 BITS        *
C  *                          MAN(2) - LOWER 16 BITS   IN RIGHT 16 BITS        *
C  *                                                                           *
C  *    EXIT        : ARG  <= OUTPUT FLOATING POINT NUMBER                     *
C  *                          ARG(1) - EXPONENT        IN RIGHT 10 BITS        *
C  *                          ARG(2) - HIGH MANTISSA   IN RIGHT 12 BITS        *
C  *                          ARG(3) - LOW MANTISSA    IN RIGHT 16 BITS        *
C  *                                                                           *
C  *    FUNCTION    : TO COMPRESSES A FLOATING POINT NUMBER AFTER IT HAS       *
C  *                  BEEN USED BY THE SOFTWARE ARITHMETIC SIMULATORS.         *
C  *                                                                           *
C  *    GENERALITIES: TO PACK: 1. MASK OUT THE LOW THREE BITS                  *
C  *                           2. PERFORM A DOUBLE WORD LOGICAL RIGHT          *
C  *                              SHIFT BY 3.                                  *
C  *                           3. MASK OUT THE UPPER 4 BITS.                   *
C  *                           4. STEPS 1, 2, AND 3 WERE PERFORMED ON THE      *
C  *                              MANTISSA.  THE EXPONENT REMAINS UNCHANGED.   *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER ARG(3),EXPO,MAN(2)
C
C.
C..
C...
C
          ARG(1)=EXPO
C
C====== CLEAR LOW 3 BITS SO BIT-BUCKET ON RIGHT SHIFT WON'T INTE
C
          MAN(2)=NAND16(MAN(2),7)
          CALL IDSHFT(MAN,-3)
          ARG(2)=IAND16(MAN(1),4095)
          ARG(3)=MAN(2)
          RETURN
          END
C
C
C****** IDNEG = DOUBLE WORD NEGATE                          = REL 5.0  , NOV 79
        SUBROUTINE IDNEG(ARG)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>IDNEG                                             *
C  *    DATE        : NOV  1,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : ARG => INPUT TWO WORD ARGUMENT                           *
C  *                                                                           *
C  *    EXIT        : ARG <= OUTPUT ARGUMENT. NEGATED VERSION OF THE           *
C  *                         INPUT ARGUMENT, 'ARG'.                            *
C  *                                                                           *
C  *    FUNCTION    : TO PERFORM A DOUBLE WORD, 2'S COMPLIMENT NEGATE          *
C  *                  OF THE INPUT ARGUMENT, 'ARG'.                            *
C  *                                                                           *
C  *    GENERALITIES: 1. LOGICAL COMPLEMENT THE DOUBLE WORD REGISTER,          *
C  *                     'ARG(1) AND ARG(2)'.                                  *
C  *                  2. ADD ONE TO THE LOW END OF ARG(1).                     *
C  *                  3. OUTPUT ARGUMENTS ARE ALSO IN THE ARRAY, 'ARG'.        *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER ARG(2),IC
C
C.
C..
C...
C
          ARG(2)=IADDC(INOT16(ARG(2)),1,IC)
          ARG(1)=IADD16(INOT16(ARG(1)),IC)
          RETURN
          END
C
C
C****** IDSHFT = DOUBLE WORD SHIFT                          = REL 5.0  , NOV 79
        SUBROUTINE IDSHFT(ARG,ICOUNT)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>IDSHFT                                            *
C  *    DATE        : NOV  1,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : ARG   => DOUBLE WORD ARGUMENT TO BE SHIFTED              *
C  *                  ICOUNT=> NUMBER OF ONE BIT SHIFTS                        *
C  *                           IF 'ICOUNT' IS LESS THAN 0 - RIGHT SHIFT        *
C  *                           IF 'ICOUNT' IS GREATER THAN 0 - LEFT SHIFT      *
C  *                                                                           *
C  *    EXIT        : ARG   <= OUTPUT DOUBLE WORD ARGUMENT. IT IS THE          *
C  *                           SHIFTED VERSION OF THE INPUT ARGUMENT, 'ARG'.   *
C  *                                                                           *
C  *    FUNCTION    : TO PERFORM A DOUBLE WORD SHIFT OF THE INPUT ARGUMENT,    *
C  *                  'ARG', BY 'ICOUNT' BITS.                                 *
C  *                                                                           *
C  *    GENERALITIES: IF LEFT SHIFT:                                           *
C  *                        A LOGICAL SHIFT IS PERFORMED. ZEROS ARE SHIFTED IN.*
C  *                  IF RIGHT SHIFT:                                          *
C  *                        AN ARITHMETIC SHIFT IS PERFORMED.  THE SIGN BIT    *
C  *                        VALUE IS SHIFTED IN.                               *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER ARG(2),ICOUNT,AH,AL,ICNT,ISBT,I
C
C.
C..
C...
C====== GET HIGH AND LOW WORD
C
          AH=ARG(1)
          AL=ARG(2)
C
C====== DO RIGHT (ICOUNT.LT.0) OR LEFT (ICOUNT.GT.0) SHIFT
C
          ICNT=IABS(ICOUNT)
          IF (ICOUNT) 10,40,50
C
C====== RIGHT SHIFT
C       IF MORE THAN 31 SHIFTS, RETURN ZERO
C
10        IF (ICNT.LE.31) GO TO 20
          ARG(1)=0
          ARG(2)=0
          RETURN
C
C====== NOW THE SHIFT LOOP
C       SAVE THE SIGN BIT
C
20        ISBT=NAND16(AH,32767)
          DO 30 I=1,ICNT
      AL=IOR16(IOR16(IRSH16(AL,1),IAND16(AL,1)),ILSH16(IAND16(AH,1),15))
30        AH=IOR16(IRSH16(AH,1),ISBT)
          GO TO 70
C
C====== NO SHIFT
C
40        RETURN
C
C====== LEFT SHIFT
C
50        DO 60 I=1,ICNT
          AH=IOR16(ILSH16(AH,1),NEGCHK(AL))
60        AL=ILSH16(AL,1)
C
C====== PUT AH & AL BACK INTO ARG AND RETURN
C
70        ARG(1)=AH
          ARG(2)=AL
          RETURN
          END
C
C
C****** RMOV = REGISTER MOVE                                = REL 5.0  , NOV 79
        SUBROUTINE RMOV(IA,IB,N)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>RMOV                                              *
C  *    DATE        : NOV  1,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : IA => SOURCE ARRAY                                       *
C  *                  N  => NUMBER OF WORDS TO BE MOVED                        *
C  *                                                                           *
C  *    EXIT        : IB <= DESTINATION ARRAY                                  *
C  *                                                                           *
C  *    FUNCTION    : TO TRANSFER 'N' VALUES  FROM THE 'IA' ARRAY TO THE       *
C  *                  'IB' ARRAY.                                              *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER N,IA(N),IB(N),I
C
C.
C..
C...
C
          DO 10 I=1,N
10        IB(I)=IA(I)
          RETURN
          END
C
C
C%.R    2364    HP2100
C****** DUMPDP = DUMP DATA PADS                             = REL 5.0  , NOV 79
        SUBROUTINE DUMPDP(DPX,DPY,ITTO,IRADX)
C%.E    2364    HP2100
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>DUMPDP                                            *
C  *    DATE        : NOV  1,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : DPX  => ARRAY CONTAINING THE DPX VALUES                  *
C  *                  DPY  => ARRAY CONTAINING THE DPY VALUES                  *
C  *                  ITTO => OUTPUT DEVICE NUMBER                             *
C  *                  IRADX => OUTPUT RADIX
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : IN THE EVENT THAT AN ERROR IS DETECTED IN THE            *
C  *                  COMPAR SUBROUTINE, THIS SUBROUTINE WILL DUMP             *
C  *                  THE DATA PAD VALUES AS PART OF THE ERROR                 *
C  *                  MESSAGE.                                                 *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER DPX(3,8),DPY(3,8),IDPX(6,3),IDPY(6,3),ITTI,IDUMMY,LFIL,
     +  RJUSFY
C
        INTEGER ISTAT,IRTURN,ICHNGE
C
        REAL RMDSIZ
C
        COMMON /INSTAL/ ITTI,IDUMMY,LFIL
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL/  IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IDUMB ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),FFTERR) , (ISTAT( 7),DMPSIM) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IPSLIM) , (ISTAT(10),ERRCNT) , (ISTAT(11),PASCNT)
        EQUIVALENCE
     +  (ISTAT(12),   IXN) , (ISTAT(13),   IYN) , (ISTAT(14),SIMULS),
     +  (ISTAT(15), IERRS) , (ISTAT(16),IERLIM) , (ISTAT(17), ISTEP)
C.
C..
C...
C
        WRITE(ITTO,2000)
        IF(IECHO.NE.0) WRITE(LFIL,2000)
        DO 20 I=1,8
        DO 10 J=1,3
        CALL I2ASCI(6,DPX(J,I),IDPX(1,J),IRADX,0)
10      CALL I2ASCI(6,DPY(J,I),IDPY(1,J),IRADX,0)
        WRITE(ITTO,2010)((IDPX(J,K),J=1,6),K=1,3),((IDPY(J,K)
     1      ,J=1,6),K=1,3)
        IF(IECHO.NE.0) WRITE(LFIL,2010)((IDPX(J,K),J=1,6),
     +                 K=1,3),((IDPY(J,K),J=1,6),K=1,3)
20      CONTINUE
C
C====== FORMAT STATEMENTS
C
2000    FORMAT(//,8X,9H DPX ARGS,20X,9H DPY ARGS )
2010    FORMAT(2(3(2X,6A1),5X),//)
C
        RETURN
        END
C
C
C****** DUMPSP = DUMP SPADS                                 = REL 5.0  , NOV 79
        SUBROUTINE DUMPSP(SPARG,ITTO,IRADX)
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>DUMPSP                                            *
C  *    DATE        : NOV  1,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : SPARG => SCRATCH PAD VALUES                              *
C  *                  ITTO  => OUTPUT DEVICE NUMBER                            *
C  *                  IRADX  => OUTPUT RADIX
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : IN THE EVENT THAT AN ERROR IS DETECTED IN THE SUBROUTINE *
C  *                  COMPAR, THIS SUBROUTINE WILL DUMP THE S-PAD VALUES,      *
C  *                  'SPARG', AS PART OF THE ERROR MESSAGE.                   *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER SPARG(16),ISPARG(6,16),ITTI,IDUMMY,LFIL,RJUSFY
C
        INTEGER ISTAT,IRTURN,ICHNGE
C
        REAL RMDSIZ
C
        COMMON /INSTAL/ ITTI,IDUMMY,LFIL
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL/  IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        EQUIVALENCE (IENTRY(1),IDUMB ), (IENTRY(2),IAPNUM)
        EQUIVALENCE (IENTRY(3),IECHO ), (IENTRY(4),LUN   )
        EQUIVALENCE (IENTRY(5),NZMD  ), (IENTRY(6),IPSIZE)
        EQUIVALENCE (IENTRY(7),NPAGES), (IENTRY(8),PSIZE )
        EQUIVALENCE (IENTRY(9),MDSIZ )
C
        EQUIVALENCE          (ISTAT( 1),WATERR) , (ISTAT( 2),LOOPER),
     +  (ISTAT( 3),TYPDIS) , (ISTAT( 4), IORST) , (ISTAT( 5),  HALT),
     +  (ISTAT( 6),FFTERR) , (ISTAT( 7),DMPSIM) , (ISTAT( 8),PSSFLG),
     +  (ISTAT( 9),IPSLIM) , (ISTAT(10),ERRCNT) , (ISTAT(11),PASCNT)
        EQUIVALENCE
     +  (ISTAT(12),   IXN) , (ISTAT(13),   IYN) , (ISTAT(14),SIMULS),
     +  (ISTAT(15), IERRS) , (ISTAT(16),IERLIM) , (ISTAT(17), ISTEP)
C.
C..
C...
C
        DO 10 I=1,16
10      CALL I2ASCI(6,SPARG(I),ISPARG(1,I),IRADX,0)
        WRITE(ITTO,2000)((ISPARG(K,J),K=1,6),J=1,16)
        IF(IECHO.NE.0) WRITE(LFIL,2000)((ISPARG(K,J),K=1,6),J=1,16)
C
C====== FORMAT STATEMENTS
C
2000    FORMAT(//,10H SPAD ARGS,/,2(8(2X,6A1),/) )
        RETURN
        END
C
C
C****** APARBK = ART100 BLOCK DATA TABLE                    = REL 5.0  , NOV 79
        BLOCK DATA
C: *****************************************************************************
C  *                                                                           *
C  *    NAME        : ART100>APARBK                                            *
C  *    DATE        : NOV  1,1979                                              *
C  *    HOST SYSTEM : DUMMY                                                    *
C  *                                                                           *
C  *    ENTRY       : N/A                                                      *
C  *                                                                           *
C  *    EXIT        : N/A                                                      *
C  *                                                                           *
C  *    FUNCTION    : APARBK IS THE BLOCK DATA TABLE FOR ART100.               *
C  *                  BLOCK DATA INITIALIZES:                                  *
C  *                  'ZERO' IN /REG/       4 WORDS OF ZERO FOR THE SIMULATOR  *
C  *                  'TMR' IN /REG/        USED BY FMUL PUSHERS               *
C  *                  'MDR' IN /REG/        USED BY FMUL PUSHERS               *
C  *                  'RNDCON' IN /NORM/    ROUNDING CONSTANTS FOR NORMALIZATION
C  *                  'ZRES' IN /NORM/      CONSTANTS FOR FLOATING ADDER ZERO  *
C  *                  'STRTPS' IN /ART100/  FIRST 5 P.S. PROGRAM WORDS         *
C  *                  'FINPS' IN /ART100/   LAST 8 P.S. PROGRAM WORDS          *
C  *                                                                           *
C  *                                                                           *
C: *****************************************************************************
C#
        INTEGER ISTAT,IRTURN,ICHNGE,WATERR,LOOPER,TYPDIS,IORST,HALT,
     +  FFTERR,DMPSIM,WRTST,FFTBAS,PSSFLG,IERLIM,IPSLIM,ERRCNT,PASCNT,
     +  IPAGE,TESTFG,IXN,IYN,SIMULS,FFTSIZ,PSIZE,BASLOW,MDSIZ,IAPNUM,
     +  IECHO,IRADX,IERRS,IMSIZ,IPSIZE,NPAGES,STATBF,STATNM,STRAD,SDPX
        INTEGER
     +  SDPY,SSP,SSTAT,HDPX,HDPY,HSP,HSTAT,XSAVE,YSAVE,DPARG,N,STRTPS,
     +  FINPS,E1,E2,M1,M2,MXCNT,TRUNC,ISMF,FIXF,ZFLG,ZRES,RNDCON,NEG1
        INTEGER PROD,FARG1,FARG2,FA1,FA2,MARG1,MARG2,FM1,FM2,FM3,M1H,
     +  M1L,M2H,M2L,SP,CB,MDR,MI,TMR,DPXR,DPYR,DPXW,DPYW,SPFN,INBS,DPBS,
     +  PNLBS,FM,FA,ZERO,PSA,MA,TMA,DPA,STATUS,DA,FLAGS,SRA,SWITCH,
     +  LITES,APMA,HMA,WC,CTL
C
        REAL RMDSIZ
C
        COMMON /TOTAL/ ISTAT(40)
        COMMON /TOTL/  IENTRY(20),RMDSIZ(17),IRTURN,ICHNGE
C
        COMMON /ART100/ SDPX(3,8),SDPY(3,8),SSP(16),SSTAT,HDPX(3,8),
     1  HDPY(3,8),HSP(16),HSTAT,XSAVE,YSAVE,DPARG,N,STRTPS(4,4),
     2  FINPS(4,8)
C
        COMMON /NORM/   E1,E2,M1(2),M2(2),MXCNT,TRUNC,ISMF,FIXF,ZFLG,
     1  ZRES(16),RNDCON(4),NEG1(2),PROD(2),FARG1(3),FARG2(3),FA1(3),
     2  FA2(3),MARG1(3),MARG2(3),FM1(3),FM2(3),FM3(3)
C
        COMMON /REG/    SP(16),CB(4),MDR(3),MI(3),TMR(3),DPXR(3),
     1  DPYR(3),DPXW(3),DPYW(3),SPFN,INBS(3),DPBS(3),PNLBS,FM(3),FA(3),
     2  ZERO(4),PSA,MA,TMA,DPA,STATUS,DA,FLAGS,SRA,SWITCH,LITES,APMA,
     3  HMA,WC,CTL
C
        EQUIVALENCE (M1(1),M1H),(M1(2),M1L),(M2(1),M2H),(M2(2),M2L)
C
C.
C..
C...
C====== INITIALIZATION
C
          DATA ZERO/0,0,0,0/
          DATA TMR/0,0,0/
          DATA MDR/0,0,0/
          DATA RNDCON/3,4,0,7/
          DATA ZRES/0,0,0,4,0,0,4,4,0,0,0,0,0,4,4,4/
          DATA STRTPS/
     X       513, -9696,     0,  7936,
     X         1, -9728,     0,  7936,
     X         0,     0,     0,  7936,
     X         3,-29696,  1024,     0/
          DATA FINPS/
     X         1,     0, -8192,  4096,
     X         0,     0, -8192,  4096,
     X         3, -4096,-16384,     0,
     X         0,     0,     0,     0,
     X     16384,     0,     0,     0,
     X       771,-26112,     0,     0,
     X         3, -4096,     0,     0,
     X         0,     0,     0,     0/
C
C
          END
