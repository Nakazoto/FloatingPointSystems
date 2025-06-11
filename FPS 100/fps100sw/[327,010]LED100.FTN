O
C       BY MAKING THE NEW LIBRARY DIRECTLY ACCESSABLE OR CONTAIN
C       FIXED LENGTH RECORDS.  THIS LAST ITEM ALLOWS THE LOADER TO
C       QUICKLY SKIP RECORDS IN THE LIBRARY WHILE SEARCHING FOR NEEDED
C       ROUTINES.
C
C       VARIABLES:
C
C       ILUN   = THE INPUT LOGICAL UNIT
C       OLUN   = THE OUTPUT LOGICAL UNIT
C       TLUN   = LOGICAL UNIT OF A TEMPORARY FILE
C       RADIX  = THE RADIX THE AP OBJECT FILE WAS WRITTEN WITH
C
        INTEGER RADIX,IFILE(36),ILUN,LUNX
        INTEGEN THE FILES
C       THE OUTPUT FILE IS OPENED WITH THE '+64' OPTION IN INFILE
C       TO OPEN A FILE THAT IS EITHER FIXED RECORD LENGTH OR DIRECTLY
C       ACCESSIBLE.
C
50      WRITE (ITTO,100)
100     FORMAT (' INPUT FILE NAME')
        ID=RDLIN (IFILE,-1,35)
        IF (INFILE (1,IFILE,ILUN) .EQ. 0) GOTO 500
C
C       BAD FILE NAME
C
        CALL ERRMES (3)
        GOTO 50
C
500     WRITE (ITTO,520)
520     FORMAT (' OUTPUT FILE NAME')
        ID=RDLIN (OFILE,-1,35)
        LUNX=3 + 64
        IF (INFILE (LUNX,OFILE,OLUN) .EQ. 0) GOTO 1000
C
C       BAD FILE NAME
C
        CALL ERRMES (3)
        GOTO 500
C
C       ASSIGN A TEMPORARY FILE
C
1000    ID=INFILE (39,TFILE,TLUN)
C
C       CALL CPYSBR TO COPY THE LIBRARY ASSIGNED TO ILUN TO OLUN
C
        CALL CPYSBR (ILUN,OLUN,TLUN,RADIX)
C
C       CLOSE INPUT, OUTPUT, AND TEMP FILES AND THEN WE'RE DONE
C
        ID=INFILE (4,IFILE,ILUN)
        ID=INFILE (4,OFILE,OLUN)
        ID=INFILE (5,TFILE,TLUN)
        CALL EXIT
        END
C+++ CPYSBR
C****** CPYSBR = COPY SUB'S INSERTING INDEX = REL.  1.00 , 09/01/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLED WAS PRODUCED BY CROCK
C    *    ON TUE, DEC 04 1979 FOR THE PDP11 COMPUTER AT 18:12:01
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE CPYSBR (ILUN,OLUN,TLUN,RADIX)
C
        INTEGER ILUN,OLUN,TLUN,RADIX
C
C       THIS ROUTINE READS AN AP LIBRARY AND INSERTS BEFORE EACH
C       ROUTINE INDEX BLOCKS OF THE FORM:
C
C       14  ENTRYCOUNT  SKIPCOUNT
C       TITLE ENTRY1 ENTRY2 ENTRY3 ... ENTRY6
C
C       ENTRYCOUNT IS THE NUMBER OF ENTRY SYMBOLS ON THE LINE FOLLOWING.
C       SKIPCOUNT IS THE NUMBER OF RECORDS IN THE NEXT ROUTINE OR ZERO
C       IF ANOTHER INDEX BLOCK FOLLOWS IMMEDIATELY.  ENTRY1 THROUGH
C       ENTRY6 ARE ENTRY SYMBOLS FOR THE ROUTINE THAT FOLLOWS THE INDEX
C       BLOCK.  TITLE IS THE TITLE OF THE ROUTINE.  ONLY 6 ENTRY SYMBOLS
C       ARE CONTAINED IN ONE INDEX BLOCK THUS IT IS POSSIBLE FOR MORE
C       THAN ONE INDEX BLOCK TO BE CREATED FOR A GIVEN ROUTINE.
C
C       CALL:
C
C       ILUN   = THE LUN USED TO READ THE INPUT LIBRARY
C       OLUN   = THE LUN USED TO WRITE THE OUTPUT LIBRARY
C       TLUN   = A SCRATCH FILE LUN
C       RADIX  = THE RADIX IN WHICH THE OBJECT FILE WAS WRITTEN
C
        INTEGER ENTCNT,STR(81),IPTR,SYM(7),IFILE(36),I
        INTEGER OFILE(36),ID,IV,SKPCNT,SENTS(81),TFILE(20)
        INTEGER BLKID,STITLE(10)
        INTEGER RDLIN,INFILE,RDREC,STOI,EXTTOK,EXTSS
C
        COMMON /LBED/ LUNMAP,ITTO
        INTEGER LUNMAP,ITTO
C
C                                                                       STARTPRE
      INTEGER SUBHDR(24),SFM(3)
C STRNG SUBHDR " 14 NNN NNNNNN ***INDEX"
      DATA SUBHDR(2)/'  '/,SUBHDR(3)/'1 '/,SUBHDR(4)/'4 '/
      DATA SUBHDR(5)/'  '/,SUBHDR(6)/'N '/,SUBHDR(7)/'N '/
      DATA SUBHDR(8)/'N '/,SUBHDR(9)/'  '/,SUBHDR(10)/'N '/
      DATA SUBHDR(11)/'N '/,SUBHDR(12)/'N '/,SUBHDR(13)/'N '/
      DATA SUBHDR(14)/'N '/,SUBHDR(15)/'N '/,SUBHDR(16)/'  '/
      DATA SUBHDR(17)/'* '/,SUBHDR(18)/'* '/,SUBHDR(19)/'* '/
      DATA SUBHDR(20)/'I '/,SUBHDR(21)/'N '/,SUBHDR(22)/'D '/
      DRDS READ WILL DEPEND ON HOW THE '+64' OPTION
C                WAS IMPLEMENTED IN INFILE.  IF THIS WAS DONE USING
C                FIXED RECORD LENGTHS, THEN THE NUMBER OF WORDS READ
C                WILL BE THAT LENGTH.
C
C       RETURNS:
C
C       BUFFER = CONTAINS THE RECORD READ.
C
        INTEGER LENX,LUNX
C
        LENX=LEN
        IF (LEN.EQ.-1) LENX=81
        LUNX=LUN+7
        READ(LUNX)LENX
        READ(LUNX)(BUFFER(I),I=1,LENX)
        RETURN
        END
C+++    TYPC
C****** TYPC = TYPE OF CHARACTER = REL.  1.00 , 09/01/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLED WAS PRODUCED BY CROCK
C    *    ON WED, DEC 05 1979 FOR THE PDP11 COMPUTER AT 08:30:45
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        INTEGER FUNCTION TYPC(CHR,RADIX)
        INTEGER CHR,RADIX
C
C       RETURNS:
C         -2  IF THE CHARACTER IS A DIGIT IN 'RADIX'
C         -1  IF THE CHARACTER IS ALPHA (A-Z) OR DIGIT (0-9)
C        ELSE  THE CHARACTER VALUE IF NON-ALPHANUMERIC
C
C       NOTE: BLANK IS RETURNED FOR A TAB
C
C       IF A CHARACTER IS BOTH A LETTER AND A DIGIT, NUMERIC (-2) WILL BE
C               RETURNED
C
        INTEGER L0,L0M1,L9,LA,LTAB,LBLANK,LZ,LAM11
C
C       DEFINED CONSTANTS USED:
        DATA   L0, L9,  LA, LZ,  LBLANK, LTAB,L0M1,LAM11/
     1     8240,8249,8257, 8282, 8224, 8283,8239,8230/
C
C       NOTE:  L0<L9  AND  LA<LZ
C
C  THIS ASSUMES CHARACTERS 0 THROUGH 9 AND A THROUGH Z ARE CONTIGUOUS
C  BLOCKS OF ASCENDING VALUES. AND THAT L9<LA.
C  HOWEVER, LA DOES NOT HAVE TO BE L9+1
C
C
C-------SEE IF A DIGIT
C
C    RADIX <=10, LOOK ONLY FOR  0 THROUGH RADIX-1
C    RADIX  >10, LOOK N,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        INTEGER I,CNT,ID,SYM(7),I1,I2
C
        INTEGER EXTTOK,STOI,RDREC
C
        IF (IDENT .EQ. 6 .OR. IDENT .EQ. 7) RETURN
        IF (IDENT .NE. 1 .AND. IDENT .NE. 3) GOTO 30
        CNT=1
        GOTO 40
C
C       GET RECORD COUNT
C
30      ID=EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX)
        CNT=STOI (SYM,RADIX)
              LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        INTEGER I,CNT,ID,SYM(7),I1,I2
C
        INTEGER EXTTOK,STOI,RDREC
C
        IF (IDENT .EQ. 6 .OR. IDENT .EQ. 7) RETURN
        IF (IDENT .NE. 1 .AND. IDENT .NE. 3) GOTO 30
        CNT=1
        GOTO 40
C
C       GET RECORD COUNT
C
30      ID=EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX)
        CNT=STOI (SYM,RADIX)
              LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        INTEGER I,CNT,ID,SYM(7),I1,I2
C
        INTEGER EXTTOK,STOI,RDREC
C
        IF (IDENT .EQ. 6 .OR. IDENT .EQ. 7) RETURN
        IF (IDENT .NE. 1 .AND. IDENT .NE. 3) GOTO 30
        CNT=1
        GOTO 40
C
C       GET RECORD COUNT
C
30      ID=EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX)
        CNT=STOI (SYM,RADIX)
              LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        INTEGER I,CNT,ID,SYM(7),I1,I2
C
        INTEGER EXTTOK,STOI,RDREC
C
        IF (IDENT .EQ. 6 .OR. IDENT .EQ. 7) RETURN
        IF (IDENT .NE. 1 .AND. IDENT .NE. 3) GOTO 30
        CNT=1
        GOTO 40
C
C       GET RECORD COUNT
C
30      ID=EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX)
        CNT=STOI (SYM,RADIX)
              LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        INTEGER I,CNT,ID,SYM(7),I1,I2
C
        INTEGER EXTTOK,STOI,RDREC
C
        IF (IDENT .EQ. 6 .OR. IDENT .EQ. 7) RETURN
        IF (IDENT .NE. 1 .AND. IDENT .NE. 3) GOTO 30
        CNT=1
        GOTO 40
C
C       GET RECORD COUNT
C
30      ID=EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX)
        CNT=STOI (SYM,RADIX)
              LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        INTEGER I,CNT,ID,SYM(7),I1,I2
C
        INTEGER EXTTOK,STOI,RDREC
C
        IF (IDENT .EQ. 6 .OR. IDENT .EQ. 7) RETURN
        IF (IDENT .NE. 1 .AND. IDENT .NE. 3) GOTO 30
        CNT=1
        GOTO 40
C
C       GET RECORD COUNT
C
30      ID=EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX)
        CNT=STOI (SYM,RADIX)
              LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        INTEGER I,CNT,ID,SYM(7),I1,I2
C
        INTEGER EXTTOK,STOI,RDREC
C
        IF (IDENT .EQ. 6 .OR. IDENT .EQ. 7) RETURN
        IF (IDENT .NE. 1 .AND. IDENT .NE. 3) GOTO 30
        CNT=1
        GOTO 40
C
C       GET RECORD COUNT
C
30      ID=EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX)
        CNT=STOI (SYM,RADIX)
40      DO 100 I=1,CNT
        IF (RDREC (OLUN,STR,IPTR)+1) 90020,90000,50
C
C       IF THIS IS A PARAMETER BLOCK (IDENT=10) WE MUST SKIP DETAIL
C       RECORDS ALSO.
C
50      IF (IDENT .NE. 10) GOTO 100
        ID=EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX)
        ID=EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX)
        ID=EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX)
        I1=STOI (SYM,RADIX)
        IF (I1 .EQ. 0) GOTO 100
        DO 90 I2=1,I1
        IF (RDREC (OLUN,STR,IPTR)+1) 90020,90000,90
90      CONTINUE
100     CONTINUE
        RETURN
C
C       ERROR MESSAGES
C
C       READ ERROR
C
90000   CALL ERRMES (1)
        RETURN
C
C       UNEXPECTED EOF
C
90020   CALL ERRMES (4)
        RETURN
        END
C+++ BLNKST
C****** BLNKST = BLANK A STRING = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE BLNKST (STRING,COUNT)
C
        INTEGER STRING(99),COUNT
C
C       THIS ROUTINE SETS 'STRING' TO 'COUNT' NUMBER OF BLANKS.
C
C       CALL:
C
C       STRING = THE STRING TO BE SET TO BLANKS.
C       COUNT  = THE NUMBER OF BLANKS.
C
C       RETURNS:
C
C       STRING = A STRING OF 'COUNT' NUMBER OF BLANKS.
C
        INTEGER I,J
C
        J=COUNT+1
        DO 300 I=2,J
        STRING(I)=8224
300     CONTINUE
        STRING(1)=COUNT
        RETURN
        END
C+++ CALLS
C****** CALLS = CALL COMMAND FOR APLOAD = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE CALLS
C
        INTEGER IVAL
C
C------- CALL <ENTRY SYMBOL> /
C
C       PICK UP ENTRY POINTS FROM STR()
C
C                                                                       MAINCOM
C  THE FIRST TWO LINES OF DECLARATION ARE NOT COMMON VARIABLES          MAINCOM
C  BUT INTEGER FUNCTIONS THAT MAY BE CALLED.                            MAINCOM
        INTEGER RDREC,EXTTOK,SRCST,EXTSL,RPLCS,EXTVT,INSST,EXTSS        MAINCOM
        INTEGER SRCSL,EXTCNT,LENT,SRCCS,STOI,IADDC,COMAND,CMPSS         MAINCOM
C                                                                       MAINCOM
        INTEGER ILUN,STR,IPTR,SYM,INDX                                  MAINCOM
        INTEGER IFLG,IV,OVSKIP,IV2,TREFLG                               MAINCOM
        INTEGER I,II,J,JJ,ID,OVMESZ,SFILE1                              MAINCOM
        INTEGER OVEPT,SYM2,LFFLG                                        MAINCOM
        INTEGER LIBFLG,COUNT,IRADIX,SFILE,IPTLUN                        MAINCOM
        INTEGER BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG                    MAINCOM
        INTEGER RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD                      MAINCOM
        INTEGER APOVEN,OVLDEN,PSCNT,IZ1QS                               MAINCOM
        COMMON /MCOMM/ ILUN,STR(81),IPTR,SYM(8),INDX,                   MAINCOM
     *   IFLG,IV,OVSKIP,IV2,TREFLG,                                     MAINCOM
     *   I,II,J,JJ,ID,OVMESZ,SFILE1(16),                                MAINCOM
     *   OVEPT,SYM2(8),LFFLG,                                           MAINCOM
     *   LIBFLG,COUNT,IRADIX,SFILE(31),IPTLUN,                          MAINCOM
     *   BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG,                          MAINCOM
     *   RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD,                            MAINCOM
     *   APOVEN,OVLDEN,PSCNT,IZ1QS                                      MAINCOM
C                                                                       MAINCOM
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
4300    IV=EXTTOK (SYM,6,STR,IPTR,IPTR,10)
        CALL PAKS (SYM,SYM,6)
        IF (IV .EQ. 0) GOTO 500
        IF (IV .NE. -1) GOTO 90060
C
C       IF SYM ISN'T IN ENTDTA() PLACE IT THERE W/ THE HOST CALLABLE FLAG
C       SET.
C
        INDX=SRCST (ENTDTA,1,-1,SYM,6)
        IF (INDX .NE. 0) GOTO 4350
        IF (ENTDTA(1)+1 .GT. ENTMAX) GOTO 90120
        INDX=INSST (ENTDTA,-1,SYM,6)
        CALL RPLVT (ENTDTA,-1,2,24,1)
4330    CALFLG=1
        IF (SRCST (EXTDTA,1,-1,SYM,6) .NE. 0) GOTO 4380
        IF (EXTDTA(1)+1 .GT. EXTMAX) GOTO 90120
        ID=INSST (EXTDTA,-1,SYM,6)
        GOTO 4380
C
C       ENTRY SYMBOL ALREADY EXISTS SO SET THE CALLABLE FLAG UNLESS
C       IT ALREADY IS.  SET NO-LOAD FLAG OFF.
C
4350    IV=EXTVT (ENTDTA,INDX,2,IVAL,1)
        IF (IAND16 (IV,16) .EQ. 16) GOTO 4380
        IF (IAND16 (IV,8) .EQ. 0) GOTO 90060
        IV=IOR16 (IV,16)
        IV=IAND16 (IV,IP16 (-33))
        CALL RPLVT (ENTDTA,INDX,2,IV,1)
        CALFLG=1
        GOTO 4330
C
C       CHECK FOR OPTIONAL SLASH ('/') - THIS SIGNIFIES THAT AN APOVLD
C       CALL SHOULD APPEAR IN THE HASI CORRESPONDING TO THE CURRENT
C       SYMBOL (IF OVERLAYS ARE BEING USED).
C
4380   IF (EXTTOK (SYM,1,STR,IPTR,IV2,10) .NE. 8239) GOTO 4300
        IPTR=IV2
        IV=EXTVT (ENTDTA,INDX,2,ID,1)
        IV=IOR16 (IV,64)
        CALL RPLVT (ENTDTA,INDX,2,IV,1)
        GOTO 4300
C
90060   CALL ERRMES(12)
        RETURN
90120   CALL ERRMES(7)
500     RETURN
        END
C+++ COMAND
C****** COMAND = COMMAND PROCESSOR = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        INTEGER FUNCTION COMAND(IFLAG,STR,MAXLEN)
        INTEGER STR(9999),MAXLEN,IFLAG
C
C        HAS THE VALUE OF THE INDEX OF THE FIRST OCCURANCE OF STRING
C       'STR' IN LIST 'CMNDS' OR 0 IF 'STR' DOES NOT OCCUR.
C       THE SEARCH BEGINS WITH THE 1ST STRING IN LIST.
C       'MAXLEN' SPECIFIES THE NUMBER OF CHARACTERS THAT MUST
C       MATCH, -1 MEANS ALL.
C
        INTEGER PC,CMPSS,L,LEN,I,J,P
C
C       ROUTINES USED:  CMPSS
C
C
C                                                                       STARTPRE
      INTEGER CMNDS(103)
C LIST CMNDS "LO"
      DATA CMNDS(3)/'L '/,CMNDS(4)/'O '/
      DATA CMNDS(2)/2/
C     "MAP"
      DATA CMNDS(6)/'M '/,CMNDS(7)/'A '/,CMNDS(8)/'P '/
      DATA CMNDS(5)/3/
C     "XX"
      DATA CMNDS(10)/'X '/,CMNDS(11)/'X '/
      DATA CMNDS(9)/2/
C     "HE"
      DATA CMNDS(13)/'H '/,CMNDS(14)/'E '/
      DATA CMNDS(12)/2/
C     "LIN"
      DATA CMNDS(16)/'L '/,CMNDS(17)/'I '/,CMNDS(18)/'N '/
      DATA CMNDS(15)/3/
C     "EX"
      DATA CMNDS(20)/'E '/,CMNDS(21)/'X '/
      DATA CMNDS(19)/2/
C     "F"
      DATA CMNDS(23)/'F '/
      DATA CMNDS(22)/1/
C     "OU"
      DATA CMNDS(25)/'O '/,CMNDS(26)/'U '/
      DATA CMNDS(24)/2/
C     "C"
      DATA CMNDS(28)/'C '/
      DATA CMNDS(27)/1/
C     "LF"
      DATA CMNDS(30)/'L '/,CMNDS(31)/'F '/
      DATA CMNDS(29)/2/
C     "OV"
      DATA CMNDS(33)/'O '/,CMNDS(34)/'V '/
      DATA CMNDS(32)/2/
C     "N"
      DATA CMNDS(36)/'N '/
      DATA CMNDS(35)/1/
C     "LM"
      DATA CMNDS(38)/'L '/,CMNDS(39)/'M '/
      DATA CMNDS(37)/2/
C     "PP"
      DATA CMNDS(41)/'P '/,CMNDS(42)/'P '/
      DATA CMNDS(40)/2/
C     "DE"
      DATA CMNDS(44)/'D '/,CMNDS(45)/'E '/
      DATA CMNDS(43)/2/
C     "B"
      DATA CMNDS(47)/'B '/
      DATA CMNDS(46)/1/
C     "INI"
      DATA CMNDS(49)/'I '/,CMNDS(50)/'N '/,CMNDS(51)/'I '/
      DATA CMNDS(48)/3/
C     "LIB"
      DATA CMNDS(53)/'L '/,CMNDS(54)/'I '/,CMNDS(55)/'B '/
      DATA CMNDS(52)/3/
C     "A"
      DATA CMNDS(57)/'A '/
      DATA CMNDS(56)/1/
C     "MD"
      DATA CMNDS(59)/'M '/,CMNDS(60)/'D '/
      DATA CMNDS(58)/2/
C     "PS"
      DATA CMNDS(62)/'P '/,CMNDS(63)/'S '/
      DATA CMNDS(61)/2/
C     "PM"
      DATA CMNDS(65)/'P '/,CMNDS(66)/'M '/
      DATA CMNDS(64)/2/
C     "MM"
      DATA CMNDS(68)/'M '/,CMNDS(69)/'M '/
      DATA CMNDS(67)/2/
C     "R"
      DATA CMNDS(71)/'R '/
      DATA CMNDS(70)/1/
C     "INP"
      DATA CMNDS(73)/'I '/,CMNDS(74)/'N '/,CMNDS(75)/'P '/
      DATA CMNDS(72)/3/
C     "EC"
      DATA CMNDS(77)/'E '/,CMNDS(78)/'C '/
      DATA CMNDS(76)/2/
C     "DU"
      DATA CMNDS(80)/'D '/,CMNDS(81)/'U '/
      DATA CMNDS(79)/2/
C     "TR"
      DATA CMNDS(83)/'T '/,CMNDS(84)/'R '/
      DATA CMNDS(82)/2/
C     "MO"
      DATA CMNDS(86)/'M '/,CMNDS(87)/'O '/
      DATA CMNDS(85)/2/
C     "HS"
      DATA CMNDS(89)/'H '/,CMNDS(90)/'S '/
      DATA CMNDS(88)/2/
C     "TA"
      DATA CMNDS(92)/'T '/,CMNDS(93)/'A '/
      DATA CMNDS(91)/2/
C     "PR"
      DATA CMNDS(95)/'P '/,CMNDS(96)/'R '/
      DATA CMNDS(94)/2/
C     "MAR"
      DATA CMNDS(98)/'M '/,CMNDS(99)/'A '/,CMNDS(100)/'R '/
      DATA CMNDS(97)/3/
C     "PU"
      DATA CMNDS(102)/'P '/,CMNDS(103)/'U '/
      DATA CMNDS(101)/2/,CMNDS(1)/34/
C                                                                       ENDPRE
C
C       CHANGE APOVEN AND OVLDEN WHENEVER THE OVERLAY HANDLER CHANGES.
C
        IF(IFLAG.NE.0)GO TO 500
        L=CMNDS(1)
        LEN = MIN0 (MAXLEN,3)
        IF (LEN.EQ.0) GO TO 300
        PC=2
        DO 200 J=1,L
        IF (CMPSS(CMNDS(PC),STR,MIN0 (CMNDS(PC),LEN)).EQ.0) GO TO 100
200     PC=PC+CMNDS(PC)+1
300     COMAND=0
        RETURN
100     COMAND=J
        RETURN
500     CONTINUE
        L=MAXLEN-1
        PC=2
        IF(L.EQ.0)GO TO 700
        DO 600 J=1,L
600     PC=PC+CMNDS(PC)+1
700     CALL WRTLIN (CMNDS(PC),-1,-1)
        GO TO 300
        END
C+++ COMPOS
C****** COMPOS = DEVINE COMMON POSITION FOR HASI = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE COMPOS (INDEX,ITEM,POS,GROUP)
C
        INTEGER INDEX,ITEM,POS,GROUP
C
        INTEGER LENX,INDX
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        INDX=INDEX
        LENX=0
300     LENX=LENX+DBDTA2(INDX,2)
        IF (LENX .LT. ITEM+1) GOTO 500
        GROUP=INDX-INDEX+1
        POS=DBDTA2(INDX,2)-LENX+ITEM+1
        RETURN
500     INDX=INDX+1
        GOTO 300
        END
C+++ DTALNK
C****** DTALNK = PROCESS DBI BLOCKS = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE DTALNK (STR)
C
        INTEGER STR(81)
C
C       THIS SUBROUTINE READS THE DATA BLOCK INITIALIZATION BLOCKS
C       (DBIB) THAT WERE WRITTEN TO DBLUN DURING THE PREVIOUS LOAD
C       SESION.  THE VALUES ARE PLACED IN THE LOAD MODULE.
C
C       CALL:
C
C       STR    = AN 80 CHARACTER STRING BUFFER
C
        DOUBLE PRECISION DPFPN
        REAL SPFPN(2)
        INTEGER RPTCNT,DT,ADDR,I2,I1,IV,SYM(20),ID,J,I,VALVEC(4),IERR,K
        INTEGER IVAL(3),IPTR,VAL,DT1,PTREXT,PTRDB
        INTEGER RDREC, EXTTOK, STOI, EXTVT, INEG16
        INTEGER DTAREL,I3,CPLX
        DOUBLE PRECISION STODF
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
C
C                                                                       STARTPRE
      INTEGER SFM(2)
C STRNG SFM "]"
      DATA SFM(2)/'] '/
      DATA SFM(1)/1/
C                                                                       ENDPRE
C
C------- INITIALIZE
C
        CALL WRTLIN (SFM,DBLUN,-1)
        ID=INFILE (6,ID,DBLUN)
7200    IF (RDREC (DBLUN,STR,IPTR)+1) 7500,90000,7600
C
C       EOF ENCOUNTERED - WERE DONE
C
7500    RETURN
C
C       CHECK FOR $ RECORD SIGNIFYING THE START OF A NEW SUBROUTINE'S
C       DBI BLOCKS
C
7600    IF (EXTTOK (SYM,6,STR,IPTR,IPTR,10) .NE. 8228) GOTO 8000
        ID=EXTTOK (SYM,6,STR,IPTR,IPTR,10)
        IV=STOI (SYM,10)
        ID=EXTVT (PRGDTA,IV,2,IVAL,1)
        PTREXT=IAND16 (IVAL(1),255)
        PTRDB=IRSH16 (IVAL(1),8)
        GOTO 7200
C
C       GET RECORD COUNT, WRITE LM HEADER, AND READ DETAIL RECORDS AND
C       AND COPY TO LMLUN.
C
8000    IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        VAL=STOI (SYM,RADIX)
        CALL WRTLM (0,1,VAL,0,DBPG-1,0,0,0,0,0.0,0.0)
        DO 9000 I=1,VAL
        IF (RDREC (DBLUN,STR,IPTR)+1) 90020,90000,8040
C
C       GET THE DB ID AND RELATIVE ADDRESS AND CALC. ABSOUTE ADDRESS
C
8040    IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        IV=STOI (SYM,RADIX)
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        I1=PTRDB+IV-1
        I2=DBDTA0(I1)
        ADDR=EXTVT (DBDTA1,I2,1,ID,1)+STOI (SYM,RADIX)
C
C       GET DATA TYPE AND REPITITION COUNT
C
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        DT=STOI (SYM,RADIX)
        DT1=DT
        IF (DT.GT.16) DT=DT-16
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        RPTCNT=STOI (SYM,RADIX)
C
C       ENCODE THE VALUE INTO THE VALVEC() BY TYPE.
C
        SPFPN(1)=0.0
        SPFPN(2)=0.0
        GOTO (8100,8200,8300,8400),DT
C
C       GET INTEGER
C
8100    IV=EXTTOK (SYM,6,STR,IPTR,IPTR,10)
        I1=1
        IF (IV.EQ.-2) GO TO 8130
        IF (IV.EQ.8235) GO TO 8120
        IF (IV.NE.8237) GO TO 90020
        I1=-1
8120    ID=EXTTOK(SYM,6,STR,IPTR,IPTR,10)
8130    CONTINUE
        VALVEC(1)=STOI (SYM,10)
        IF (I1 .EQ. -1) VALVEC(1)=INEG16 (VALVEC(1))
        IF (DT1.GT.16) VALVEC(1)=
     -  IADD16(VALVEC(1),DTAREL(STR,IPTR,PTREXT,PTRDB))
        VALVEC(2)=0
        VALVEC(3)=0
        VALVEC(4)=0
        GOTO 8800
C
C       GET FLOATING POINT NUMBER.
C
8200    CONTINUE
        CPLX=1
        I3=1
8210    CONTINUE
        I1=STR(1)
        IF (IPTR .GT. I1) GOTO 90020
        DO 8220 J=IPTR,I1
        IF (STR(J+1) .NE. 8224) GOTO 8240
8220    CONTINUE
        GOTO 90020
8240    CONTINUE
        DO 8250 K=J,I1
        IF (STR(K+1) .EQ. 8224) GO TO 8260
8250    CONTINUE
8260    IPTR=K
        IV=EXTSS (STR,J,SYM,K-J+1)
        DPFPN=STODF (SYM,IERR)
        IF (IERR.NE.0) GO TO 90020
        SPFPN(I3)=SNGL (DPFPN)
        IF (CPLX.EQ.1.OR.I3.EQ.2) GO TO 8800
        I3=2
        GO TO 8210
C
C       COMPLEX NUMBER NOT YET IMPLEMENTED
C
8300    CONTINUE
        GO TO 90020
C
C       GET TRIPLE INTEGER
C
8400    CONTINUE
        IV=EXTTOK(SYM,6,STR,IPTR,IPTR,10)
        VALVEC(1)=STOI(SYM,10)
        DO 8460 I3=2,3
        IV=EXTTOK(SYM,6,STR,IPTR,IPTR,10)
        I1=1
        IF (IV.EQ.-2) GO TO 8450
        IF (IV.EQ.8235) GO TO 8440
        IF (IV.NE.8237) GO TO 90020
        I1=-1
8440    CONTINUE
        ID=EXTTOK (SYM,6,STR,IPTR,IPTR,10)
8450    CONTINUE
        VALVEC(I3)=STOI(SYM,10)
        IF (I1.EQ.-1) VALVEC(I3)=INEG16(VALVEC(I3))
8460    CONTINUE
        IF (DT1.GT.16) VALVEC(3)=
     -  IADD16(VALVEC(3),DTAREL(STR,IPTR,PTREXT,PTRDB))
        VALVEC(4)=0
        GO TO 8800
C
C       WRITE THE DATA RECORD TO LM
C
8800    CONTINUE
        IF (DT.NE.2.AND.DT.NE.3) GO TO 8820
        DO 8810 J=1,4
8810    VALVEC(J)=0
8820    CALL WRTLM (1,DT,RPTCNT,ADDR,0,VALVEC(1),VALVEC(2),
     *              VALVEC(3),VALVEC(4),SPFPN(1),SPFPN(2))
9000    CONTINUE
        GOTO 7200
C
C------- ERROR RETURNS
C
C       READ ERROR
C
90000   CALL ERRMES (1)
        RETURN
C
C       BAD RECORD
C
90020   CALL ERRMES (2)
        CALL WRTLIN (STR,-1,-1)
        RETURN
        END
C+++ DTAREL
C****** DTAREL = PROCESS DBI RELOCATION = REL.  1.01 , 12/10/79 *****
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
C
        INTEGER FUNCTION DTAREL(STR,IPTR,PTREXT,PTRDB)
        INTEGER STR(81),IPTR,PTREXT,PTRDB
C
C       THIS SUBROUTINE PROCESSES THE RELOCATION TRIPLET FOR
C       THE DATA BLOCKS
C
        INTEGER SYM(8),FLDDES,TYPEN,ARG,INDX,VAL,IVAL,ID
        INTEGER EXTTOK,STOI,EXTVT,EXTST,SRCST
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        DTAREL=0
        IF (EXTTOK(SYM,6,STR,IPTR,IPTR,RADIX).NE.-2) GO TO 90020
        FLDDES=STOI(SYM,RADIX)+1
        IF (EXTTOK(SYM,6,STR,IPTR,IPTR,RADIX).NE.-2) GO TO 90020
        TYPEN=STOI(SYM,RADIX)
        IF (EXTTOK(SYM,6,STR,IPTR,IPTR,RADIX).NE.-2) GO TO 90020
        ARG=STOI(SYM,RADIX)
        IF (TYPEN.EQ.3) GO TO 100
        IF (TYPEN.EQ.5) GO TO 200
        GO TO 90020
C
C       DB REFERENCE
C
100     CONTINUE
        VAL=ARG+PTRDB-1
        VAL=DBDTA0(VAL)
        DTAREL=EXTVT(DBDTA1,VAL,1,IVAL,1)
150     CONTINUE
        IF (FLDDES.LE.0.OR.FLDDES.GT.16) GO TO 90020
        RETURN
C
C       EXTERNAL REFERENCE
C
200     CONTINUE
        VAL=ARG+PTREXT-1
        VAL=EXTDT1(VAL)
        ID=EXTST(EXTDTA,VAL,SYM,6)
        INDX=SRCST(ENTDTA,1,-1,SYM,6)
        IF (INDX.EQ.0) RETURN
        DTAREL=EXTVT(ENTDTA,INDX,1,VAL,1)
        GO TO 150
C
C       BAD RECORD
C
90020   CONTINUE
        CALL ERRMES(2)
        CALL WRTLIN(STR,-1,-1)
        RETURN
        END
C+++ ENDLNK
C****** ENDLNK = END COMMAND FOR APLOAD = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE ENDLNK(ENTRY)
C       FINISH UP LINK PROCESSING FOR OVERLAYS
C
C       ALLOCATE SPACE FOR THE OVERLAY MAP AND SEND THE VALUES TO
C       LOAD MODULE.  IF WERE MAKING A NON-OVERLAY OR HAVEN'T LINKED
C       THE LAST OVERLAY THEN THIS COMMAND IS ILLEGAL OR OUT OF ORDER.
C
        INTEGER IVAL(3),ENTRY
        INTEGER DUMMY
C                                                                       MAINCOM
C  THE FIRST TWO LINES OF DECLARATION ARE NOT COMMON VARIABLES          MAINCOM
C  BUT INTEGER FUNCTIONS THAT MAY BE CALLED.                            MAINCOM
        INTEGER RDREC,EXTTOK,SRCST,EXTSL,RPLCS,EXTVT,INSST,EXTSS        MAINCOM
        INTEGER SRCSL,EXTCNT,LENT,SRCCS,STOI,IADDC,COMAND,CMPSS         MAINCOM
C                                                                       MAINCOM
        INTEGER ILUN,STR,IPTR,SYM,INDX                                  MAINCOM
        INTEGER IFLG,IV,OVSKIP,IV2,TREFLG                               MAINCOM
        INTEGER I,II,J,JJ,ID,OVMESZ,SFILE1                              MAINCOM
        INTEGER OVEPT,SYM2,LFFLG                                        MAINCOM
        INTEGER LIBFLG,COUNT,IRADIX,SFILE,IPTLUN                        MAINCOM
        INTEGER BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG                    MAINCOM
        INTEGER RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD                      MAINCOM
        INTEGER APOVEN,OVLDEN,PSCNT,IZ1QS                               MAINCOM
        COMMON /MCOMM/ ILUN,STR(81),IPTR,SYM(8),INDX,                   MAINCOM
     *   IFLG,IV,OVSKIP,IV2,TREFLG,                                     MAINCOM
     *   I,II,J,JJ,ID,OVMESZ,SFILE1(16),                                MAINCOM
     *   OVEPT,SYM2(8),LFFLG,                                           MAINCOM
     *   LIBFLG,COUNT,IRADIX,SFILE(31),IPTLUN,                          MAINCOM
     *   BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG,                          MAINCOM
     *   RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD,                            MAINCOM
     *   APOVEN,OVLDEN,PSCNT,IZ1QS                                      MAINCOM
C                                                                       MAINCOM
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C                                                                       COMMON10
       INTEGER TSKDTA(26,8),TSKPTR,TSKMAX,TCBMNL,TCBMXL                 COMMON10
       INTEGER SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,SPARPT                COMMON10
       INTEGER SDB0PT,SDB1PT,SDB2PT,PSTOP                               COMMON10
       INTEGER ISRMDA,ISRPSA,ISRIDX,ISRLEN                              COMMON10
       INTEGER OVPDTA(50,3),PSPDTA(50),OVPPTR,PSPPTR,PSPMAX,OVPMAX      COMMON10
       LOGICAL TASKFL,TSKFL,ISRFL,IOPENX                                COMMON10
       COMMON /TSKCOM/ TSKDTA,TSKPTR,TSKMAX,TCBMNL,TCBMXL,              COMMON10
     -         SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,                      COMMON10
     -         SPARPT,SDB0PT,SDB1PT,SDB2PT,PSTOP,                       COMMON10
     -         ISRMDA,ISRPSA,ISRIDX,ISRLEN,                             COMMON10
     -         OVPDTA,PSPDTA,OVPPTR,PSPPTR,PSPMAX,OVPMAX,               COMMON10
     -         TASKFL,TSKFL,ISRFL                                       COMMON10
       COMMON /OPENX/ IOPENX                                            COMMON10
C                                                                       COMMON10
C
C                                                                       STARTPRE
      INTEGER SOVMAP(8),SPPA(6),TOVMAP(7)
C STRNG SOVMAP ".OVMAP."
      DATA SOVMAP(2)/'. '/,SOVMAP(3)/'O '/,SOVMAP(4)/'V '/
      DATA SOVMAP(5)/'M '/,SOVMAP(6)/'A '/,SOVMAP(7)/'P '/
      DATA SOVMAP(8)/'. '/
      DATA SOVMAP(1)/7/
C STRNG SPPA ".PPA."
      DATA SPPA(2)/'. '/,SPPA(3)/'P '/,SPPA(4)/'P '/,SPPA(5)/'A '/
      DATA SPPA(6)/'. '/
      DATA SPPA(1)/5/
C STRNG TOVMAP ".MP   "
      DATA TOVMAP(2)/'. '/,TOVMAP(3)/'M '/,TOVMAP(4)/'P '/
      DATA TOVMAP(5)/'  '/,TOVMAP(6)/'  '/,TOVMAP(7)/'  '/
      DATA TOVMAP(1)/6/
C                                                                       ENDPRE
C
           IF(ENTRY .EQ. 2 ) GO TO 6600
           IF(ENTRY .EQ. 1 ) GO TO 6500
           CALL EXIT
6500    IF (DBDTA1(1)+1 .GT. DB1MAX) GOTO 90100
        IF (.NOT.TASKFL) GO TO 6520
        CALL RPLIS (TOVMAP,4,TSKDTA(TSKPTR,1),3,IRADIX,8240)
        ID=INSST(DBDTA1,-1,TOVMAP,6)
        GO TO 6525
6520    CONTINUE
        ID=INSST (DBDTA1,-1,SOVMAP,7)
6525    CONTINUE
        IVAL(1)=DBBRK
        IVAL(2)=0
        IVAL(3)=OVPTR*OVMESZ
        IF (.NOT.TASKFL) IVAL(3)=IVAL(3)/2
        CALL RPLVT (DBDTA1,-1,1,IVAL,3)
        J=1
        DO 6550 I=1,OVPTR
        IF (.NOT.TASKFL) GO TO 6528
        IF (OVPPTR+1.GT.OVPMAX) GO TO 90120
        OVPPTR=OVPPTR+1
6528    CONTINUE
        IF (J+16 .LE. BUFSIZ) GOTO 6530
        CALL WRTLM (0,0,J-1,DBBRK,DBPG-1,1,0,0,0,0.0,0.0)
        DBBRK=IADD16 (DBBRK,(J-1)*PAKFAC/2)
        CALL WRTBR(1,BUFFER,J-1,1,0,DUMMY,DUMMY)
        J=1
6530    CONTINUE
        BUFFER(J)=IAND16 (OVDTA(I,6),15)*8
        BUFFER(J+1)=OVDTA(I,1)
        BUFFER(J+2)=IAND16 (OVDTA(I,6),15)
        BUFFER(J+3)=OVDTA(I,3)
        BUFFER(J+4)=0
        BUFFER(J+5)=OVDTA(I,2)
        BUFFER(J+6)=0
        BUFFER(J+7)=IRSH16 (OVDTA(I,4),1)
        IF (.NOT.TASKFL) GO TO 6540
        BUFFER(J+8)=0
        BUFFER(J+9)=TSKDTA(TSKPTR,1)
C TASK ID
        BUFFER(J+10)=0
      BUFFER(J+11)=0
        IF (I.EQ.1) BUFFER(J+11)=1
C SHOULD BE RESIDENT
        BUFFER(J+12)=0
      BUFFER(J+13)=0
        BUFFER(J+14)=0
      BUFFER(J+15)=0
        OVPDTA(OVPPTR,1)=DBBRK+(I-1)*OVMESZ
C MD ADDR OF MAP
        OVPDTA(OVPPTR,2)=BUFFER(J+5)
C PS ADDR
        OVPDTA(OVPPTR,3)=BUFFER(J+7)
C PS LENGTH
        J=J+8
6540    CONTINUE
        J=J+8
6550    CONTINUE
        CALL WRTLM (0,0,J-1,DBBRK,DBPG-1,1,0,0,0,0.0,0.0)
        DBBRK=IADD16 (DBBRK,(J-1)*PAKFAC/2)
        CALL WRTBR(1,BUFFER,J-1,1,0,DUMMY,DUMMY)
C
C       FINISH UP LINK COMMAND PROCESSING
C
6600    IF (DBDTA1(1)+1 .GT. DB1MAX) GOTO 90100
        IF (OVFLG .EQ. 0) GOTO 6650
        IF (TASKFL) GO TO 6650
        IV=IVAL(1)
        IV2=IVAL(3)
        GOTO 6670
6650    IV=0
        IV2=0
6670    ID=INSST (DBDTA1,-1,SPPA,6)
        IVAL(1)=DBBRK
        IVAL(2)=0
        IF (PPASZ .EQ. -1) PPASZ=ISUB16 (PGINFO(DBPG,1),DBBRK)
        IVAL(3)=PPASZ
        DBBRK=IADD16 (DBBRK,PPASZ)
        CALL WRTLM (0,2,IVAL(1),PPASZ,LMID,IV2,IV,0,0,0.0,0.0)
        CALL RPLVT (DBDTA1,-1,1,IVAL,3)
        ID=IADDC (PGINFO(DBPG,1),INEG16 (DBBRK),J)
        IF (J .EQ. 0) CALL ERRMES (26)
C
C       OUTPUT THE BLOCK DATA PROGRAM TO FLUN, WRITE END BLOCK TO LM,
C       CLOSE THE LM FILE, AND REMOVE THE NAME FROM SLMFIL ALSO.
C
        IF (CALFLG .EQ. 1 .AND. CMPFLG .EQ. 0) CALL SRCBD (IVAL(1))
        IF (CALFLG.EQ.1.AND.CMPFLG.EQ.0) CALFLG=0
        IF (.NOT.TASKFL) CALL WRTLM (0,3,0,0,0,0,0,0,0,0.0,0.0)
        DBBRK=ISUB16(DBBRK,PPASZ)
        GO TO 500
90100    CALL ERRMES(16)
        GO TO 500
C
C       OVPDTA TABLE OVERFLOW
C
90120   CONTINUE
        CALL ERRMES(41)
500      RETURN
         END
C+++ ERRMES
C****** ERRMES = ERROR MESSAGE OUTPUT = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE ERRMES (MESNUM)
C
        INTEGER MESNUM
C
C       THIS ROUTINE OUTPUTS ERROR MESSAGES BY THE NUMBER.
C
C       CALL:
C
C       MESNUM = NUMBER OF THE MESSAGE TO BE OUTPUT
C
C                                                                       MAINCOM
C  THE FIRST TWO LINES OF DECLARATION ARE NOT COMMON VARIABLES          MAINCOM
C  BUT INTEGER FUNCTIONS THAT MAY BE CALLED.                            MAINCOM
        INTEGER RDREC,EXTTOK,SRCST,EXTSL,RPLCS,EXTVT,INSST,EXTSS        MAINCOM
        INTEGER SRCSL,EXTCNT,LENT,SRCCS,STOI,IADDC,COMAND,CMPSS         MAINCOM
C                                                                       MAINCOM
        INTEGER ILUN,STR,IPTR,SYM,INDX                                  MAINCOM
        INTEGER IFLG,IV,OVSKIP,IV2,TREFLG                               MAINCOM
        INTEGER I,II,J,JJ,ID,OVMESZ,SFILE1                              MAINCOM
        INTEGER OVEPT,SYM2,LFFLG                                        MAINCOM
        INTEGER LIBFLG,COUNT,IRADIX,SFILE,IPTLUN                        MAINCOM
        INTEGER BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG                    MAINCOM
        INTEGER RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD                      MAINCOM
        INTEGER APOVEN,OVLDEN,PSCNT,IZ1QS                               MAINCOM
        COMMON /MCOMM/ ILUN,STR(81),IPTR,SYM(8),INDX,                   MAINCOM
     *   IFLG,IV,OVSKIP,IV2,TREFLG,                                     MAINCOM
     *   I,II,J,JJ,ID,OVMESZ,SFILE1(16),                                MAINCOM
     *   OVEPT,SYM2(8),LFFLG,                                           MAINCOM
     *   LIBFLG,COUNT,IRADIX,SFILE(31),IPTLUN,                          MAINCOM
     *   BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG,                          MAINCOM
     *   RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD,                            MAINCOM
     *   APOVEN,OVLDEN,PSCNT,IZ1QS                                      MAINCOM
C                                                                       MAINCOM
C
C
        WRITE (ITTO,510) MESNUM
510     FORMAT (' ERROR',I3)
        RETURN
        END
C+++ EXTCNT
C****** EXTCNT = COUNT UNSATISFIED EXTERNALS = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        INTEGER FUNCTION EXTCNT (INDX,OPTION)
C
        INTEGER INDX,OPTION,SYM(7)
C
C       THIS FUNCTION COUNTS UNSATISFIED EXTERNALS THAT
C       EXIST AT THE MOMENT AND RETURNS THAT NUMBER.
C
C       CALL:
C
C       INDX   = INDEX WHERE TO START COUNTING IN ENTDTA()
C       OPTION = 0 TO COUNT UNTIL THE END OF ENTDTA(), 1 TO FIND JUST ONE
C
C       RETURNS:
C
C       EXTCNT = THE NUMBER OF UNSATISFIED EXTERNALS
C
        INTEGER I,J,ID,K
        INTEGER SRCST,EXTST,EXTVT
C
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
C       SEARCH THROUGH EXTDTA STARTING AT THE GIVEN 'INDX'
C       J=NUMBER OF ITEMS IN THE TABLE.
C
        EXTCNT=0
        J=EXTDTA(1)
        IF (INDX .GT. J) RETURN
        DO 200 I=INDX,J
        ID=EXTST (EXTDTA,I,SYM,6)
C
C       IF THE SYMBOL IS ZEROES, IGNORE IT.  OTHERWISE LOOK FOR THE
C       SYMBOL IN ENTDTA AND SEE IF THE ASSOCIATED SUBROUTINE HAS
C       BEEN LOADED YET.
C
        IF (SYM(2) .EQ. 0) GOTO 200
        K=SRCST (ENTDTA,1,-1,SYM,6)
        IF (K .EQ. 0) GOTO 150
        IF (IAND16 (EXTVT (ENTDTA,K,2,ID,1),40) .NE. 8) GOTO 200
150     EXTCNT=EXTCNT+1
        IF (OPTION .EQ. 1) RETURN
200     CONTINUE
        RETURN
        END
C+++ FINISH
C****** FINISH = END COMMAND FOR APLOAD = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
            SUBROUTINE FINISH
C------- EXIT
C
C       CLOSE AND DELETE TEMP FILE, CLOSE FLUN AND MLUN AND TTY
C
C                                                                       MAINCOM
C  THE FIRST TWO LINES OF DECLARATION ARE NOT COMMON VARIABLES          MAINCOM
C  BUT INTEGER FUNCTIONS THAT MAY BE CALLED.                            MAINCOM
        INTEGER RDREC,EXTTOK,SRCST,EXTSL,RPLCS,EXTVT,INSST,EXTSS        MAINCOM
        INTEGER SRCSL,EXTCNT,LENT,SRCCS,STOI,IADDC,COMAND,CMPSS         MAINCOM
C                                                                       MAINCOM
        INTEGER ILUN,STR,IPTR,SYM,INDX                                  MAINCOM
        INTEGER IFLG,IV,OVSKIP,IV2,TREFLG                               MAINCOM
        INTEGER I,II,J,JJ,ID,OVMESZ,SFILE1                              MAINCOM
        INTEGER OVEPT,SYM2,LFFLG                                        MAINCOM
        INTEGER LIBFLG,COUNT,IRADIX,SFILE,IPTLUN                        MAINCOM
        INTEGER BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG                    MAINCOM
        INTEGER RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD                      MAINCOM
        INTEGER APOVEN,OVLDEN,PSCNT,IZ1QS                               MAINCOM
        COMMON /MCOMM/ ILUN,STR(81),IPTR,SYM(8),INDX,                   MAINCOM
     *   IFLG,IV,OVSKIP,IV2,TREFLG,                                     MAINCOM
     *   I,II,J,JJ,ID,OVMESZ,SFILE1(16),                                MAINCOM
     *   OVEPT,SYM2(8),LFFLG,                                           MAINCOM
     *   LIBFLG,COUNT,IRADIX,SFILE(31),IPTLUN,                          MAINCOM
     *   BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG,                          MAINCOM
     *   RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD,                            MAINCOM
     *   APOVEN,OVLDEN,PSCNT,IZ1QS                                      MAINCOM
C                                                                       MAINCOM
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
C                                                                       COMMON10
       INTEGER TSKDTA(26,8),TSKPTR,TSKMAX,TCBMNL,TCBMXL                 COMMON10
       INTEGER SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,SPARPT                COMMON10
       INTEGER SDB0PT,SDB1PT,SDB2PT,PSTOP                               COMMON10
       INTEGER ISRMDA,ISRPSA,ISRIDX,ISRLEN                              COMMON10
       INTEGER OVPDTA(50,3),PSPDTA(50),OVPPTR,PSPPTR,PSPMAX,OVPMAX      COMMON10
       LOGICAL TASKFL,TSKFL,ISRFL,IOPENX                                COMMON10
       COMMON /TSKCOM/ TSKDTA,TSKPTR,TSKMAX,TCBMNL,TCBMXL,              COMMON10
     -         SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,                      COMMON10
     -         SPARPT,SDB0PT,SDB1PT,SDB2PT,PSTOP,                       COMMON10
     -         ISRMDA,ISRPSA,ISRIDX,ISRLEN,                             COMMON10
     -         OVPDTA,PSPDTA,OVPPTR,PSPPTR,PSPMAX,OVPMAX,               COMMON10
     -         TASKFL,TSKFL,ISRFL                                       COMMON10
       COMMON /OPENX/ IOPENX                                            COMMON10
C                                                                       COMMON10
C
        INTEGER PLEN,PCNT
        INTEGER K,IVAL(3),LUNX
C
C                                                                       STARTPRE
      INTEGER RDYQUE(7),SPPA(8)
C STRNG RDYQUE "READYQ"
      DATA RDYQUE(2)/'R '/,RDYQUE(3)/'E '/,RDYQUE(4)/'A '/
      DATA RDYQUE(5)/'D '/,RDYQUE(6)/'Y '/,RDYQUE(7)/'Q '/
      DATA RDYQUE(1)/6/
C STRNG SPPA ".PPA.  "
      DATA SPPA(2)/'. '/,SPPA(3)/'P '/,SPPA(4)/'P '/,SPPA(5)/'A '/
      DATA SPPA(6)/'. '/,SPPA(7)/'  '/,SPPA(8)/'  '/
      DATA SPPA(1)/7/
C                                                                       ENDPRE
C
        IF (.NOT.TASKFL) GO TO 500
C
C       CREATE THE FPS100 INTERFACE ROUTINE.  THIS IS HOW THE USER
C       LOADS HIS LOAD MODULE AND STARTS THE SUPERVISOR RUNNING.
C       ITS LIKE A HASI.
C
        LUNX=FLUN+LUNMAP
        WRITE(LUNX,10) LMID,LMID,MXDATA,LMID,MXDATA
10      FORMAT (1X,6X,'SUBROUTINE MTS',I2,'(PSSIZ)'/
     *          1X,6X,'INTEGER CODE,PSSIZ'/
     *          1X,6X,'COMMON /CODE',I2,'/ CODE(',I6,')'/
     *          1X,6X,'CALL MTSGO (PSSIZ,',I2,',CODE,',I6,')'/
     *          1X,6X,'RETURN'/
     *          1X,6X,'END')
C
C       INITIALIZE THE REMAINING ITEMS IN THE TASK OVERLAY TABLE
C
        DO 300 I=1,OVPPTR
        CALL WRTLM(0,0,4,OVPDTA(I,1)+6,0,1,0,0,0,0.0,0.0)
        DO 100 J=1,PSPPTR
        IF (OVPDTA(I,2).EQ.PSPDTA(J)) GO TO 150
100     CONTINUE
        GO TO 90000
C SEGMENT ADDRESS NOT FOUND
150     CONTINUE
        PLEN=OVPDTA(I,2)+OVPDTA(I,3)-1
C CALCULATE PS RANGE OF TASK
        PCNT=0
        DO 200 K=J,PSPPTR
        IF (PSPDTA(K).LE.PLEN) PCNT=PCNT+1
200     CONTINUE
        CALL WRTLM(1,0,DBBRK+J-1,0,PCNT,0,0,0,0,0.0,0.0)
300     CONTINUE
C
C  ZERO OUT PARTITION TABLE
C
        CALL WRTLM (0,1,1,0,0,0,0,0,0,0.0,0.0)
        CALL WRTLM (1,4,PSPMAX,DBBRK,0,0,0,0,0,0.0,0.0)
        DBBRK=DBBRK+PSPMAX
C RESERVE ROOM FOR PARTITION TABLE
C
C       INITIALIZE THE READY QUEUE
C
        ID=SRCST(DBDTA1,1,-1,RDYQUE,6)
C LOOK FOR HEADER
        IF (ID.EQ.0) GO TO 90010
C  SKIP IF NO TASKS
        IF (TSKPTR .EQ. 1) GOTO 450
        ID= EXTVT(DBDTA1,ID,1,TSKDTA(1,8),1)
C GET RDYQUE ADDR
        DO 400 I=1,TSKPTR
        CALL WRTLM(0,0,4,TSKDTA(I,8),0,1,0,0,0,0.0,0.0)
        J=TSKDTA(I,6)
C RLINK POINTER
        K=TSKDTA(I,7)
C LLINK POINTER
        CALL WRTLM(1,0,TSKDTA(J,8),0,TSKDTA(K,8),0,0,0,0,0.0,0.0)
400     CONTINUE
450     CONTINUE
        IV=0
        IV2=0
        ID=INSST (DBDTA1,-1,SPPA,6)
        PPASZ=ISUB16 (PGINFO(DBPG,1),DBBRK)
        IF (SLMFIL(1).NE.0)
     *  CALL WRTLM (0,2,DBBRK,PPASZ,LMID,IV2,IV,0,0,0.0,0.0)
        ID=IADDC (PGINFO(DBPG,1),INEG16 (DBBRK),J)
        IF (J .EQ. 0) CALL ERRMES (26)
C
C       WRITE END BLOCK TO LM, CLOSE THE FILE
C
500     CONTINUE
        IF (SLMFIL(1).NE.0)
     -  CALL WRTLM(0,3,1,0,0,0,0,0,0,0.0,0.0)
        ID=INFILE (5,SCLUN,CLUNS(1))
        IF (OVFLG .NE. 0) ID=INFILE (5,SCLUN2,CLUNS(2))
        ID=INFILE (4,SFFIL,FLUN)
        ID=INFILE (4,SLMFIL,LMLUN)
C
C   END THE CURRENT HOST RESIDENT LOAD MODULE.
C
        IF (LMLDSW.NE.0.AND.FLMFIL(1).NE.0) CALL LMSRC2 (1)
        ID=INFILE (4,FLMFIL,FLMLUN)
        ID=INFILE (5,SFILE1,DBLUN)
        IF (EKOFLG .EQ. 1) ID=INFILE (4,ID,EKOLUN)
        IF (IPTLUN .NE. -1) ID=INFILE (4,ID,IPTLUN)
        IZ1QS = INFILE (4,0,-1)
        RETURN
C
C       SEGMENT ADDRESS NOT FOUND
C
90000   CONTINUE
        CALL ERRMES(34)
        RETURN
C
C       RDYQUE NOT DEFINED
C
90010   CONTINUE
        CALL ERRMES(35)
        RETURN
        END
C+++ FORCE
C****** FORCE = PERFORM FORCE COMMAND = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE FORCE (STR,IPTR,SYM)
C
        INTEGER STR(999),IPTR,SYM(999)
C
        INTEGER IV,J,JJ,ID
        INTEGER EXTTOK,SRCST,EXTVT,IAND16,IP16,INSST
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
C------- FORCE <ENTRY SYMBOL>
C
C       IF A SYMBOL IS ALREADY LOADED IGNORE IT.  IF AN ENTRY EXISTS IN
C       ENTDTA() TURN OFF THE NO-LOAD FLAG AND PLACE THE SYMBOL IN
C       EXTDTA() UNLESS ITS ALREADY THERE.
C
        IV=STR(1)
6050    IF (IPTR .GT. IV) RETURN
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,10) .NE. -1) GOTO 90000
        CALL PAKS (SYM,SYM,6)
        J=SRCST (ENTDTA,1,-1,SYM,6)
        IF (J .EQ. 0) GOTO 6080
        JJ=EXTVT (ENTDTA,J,2,ID,1)
        IF (IAND16 (JJ,8) .EQ. 0) GOTO 6050
        JJ=IAND16 (JJ,IP16 (-33))
        CALL RPLVT (ENTDTA,J,2,JJ,1)
6080    IF (SRCST (EXTDTA,1,-1,SYM,6) .NE. 0) GOTO 6050
        IF (EXTDTA(1)+1 .GT. EXTMAX) GOTO 90020
        ID=INSST (EXTDTA,-1,SYM,6)
        CALL RPLVT (EXTDTA,-1,1,0,1)
        GOTO 6050
C
C------- ERROR RETURNS
C
C       BAD OR MISSING PARAMETER
C
90000   CALL ERRMES (12)
        RETURN
C
C       TOO MANY ENTRIES
C
90020   CALL ERRMES (7)
        RETURN
        END
C+++ INIT
C****** INIT = INITIALIZE THE LOADER = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE INIT (FLAG1,FLAG2)
C
        INTEGER FLAG1,FLAG2
C
C       CALL:
C
C       FLAG1  = 1 IF FILE INITIALIZING IS DONE ALSO, ELSE 0.
C       FLAG2  = 1 IF FILE CLOSING SHOULD BE DONE, ELSE 0.
C
        INTEGER I,ID
        INTEGER INFILE
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C                                                                       COMMON10
       INTEGER TSKDTA(26,8),TSKPTR,TSKMAX,TCBMNL,TCBMXL                 COMMON10
       INTEGER SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,SPARPT                COMMON10
       INTEGER SDB0PT,SDB1PT,SDB2PT,PSTOP                               COMMON10
       INTEGER ISRMDA,ISRPSA,ISRIDX,ISRLEN                              COMMON10
       INTEGER OVPDTA(50,3),PSPDTA(50),OVPPTR,PSPPTR,PSPMAX,OVPMAX      COMMON10
       LOGICAL TASKFL,TSKFL,ISRFL,IOPENX                                COMMON10
       COMMON /TSKCOM/ TSKDTA,TSKPTR,TSKMAX,TCBMNL,TCBMXL,              COMMON10
     -         SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,                      COMMON10
     -         SPARPT,SDB0PT,SDB1PT,SDB2PT,PSTOP,                       COMMON10
     -         ISRMDA,ISRPSA,ISRIDX,ISRLEN,                             COMMON10
     -         OVPDTA,PSPDTA,OVPPTR,PSPPTR,PSPMAX,OVPMAX,               COMMON10
     -         TASKFL,TSKFL,ISRFL                                       COMMON10
       COMMON /OPENX/ IOPENX                                            COMMON10
C                                                                       COMMON10
C
        IF (FLAG2 .EQ. 0) GOTO 20
        IF (SLMFIL(1).NE.0)
     -  CALL WRTLM(0,3,1,0,0,0,0,0,0,0.0,0.0)
C        ID=INFILE (6,SCLUN,CLUNS(1))
C       ID=INFILE (6,SCLUN2,CLUNS(2))
C       ID=INFILE (6,SFFIL,FLUN)
C       ID=INFILE (6,SLMFIL,LMLUN)
C       ID=INFILE (6,FLMFIL,FLMLUN)
        ID=INFILE (5,SCLUN,CLUNS(1))
        ID=INFILE (5,SLP2NM,LP2LUN)
        IF (OVFLG .NE. 0) ID=INFILE (5,SCLUN2,CLUNS(2))
        ID=INFILE (4,SFFIL,FLUN)
        ID=INFILE (4,SLMFIL,LMLUN)
C
C   END CURRENT HOST RESIDENT LOAD MODULE IF THERE IS ONE.
C
        IF (LMLDSW.NE.0.AND.FLMFIL(1).NE.0) CALL LMSRC2 (1)
        ID=INFILE (4,FLMFIL,FLMLUN)
        GOTO 30
20      IF (OVFLG .NE. 0) ID=INFILE (6,SCLUN2,CLUN2)
30      LMID=1
        OVFLG=0
        PPASZ=-1
        IF (CMPFLG .EQ. 0) PSLOW=16
        IF (CMPFLG .NE. 0) PSLOW=0
        DBPG=1
        OTOFLG=0
        OVLEVL=0
        CALFLG=0
        OVPTR=0
        DO 50 I=1,16
50      PGINFO(I,2)=0
        RSTRCT=1
        MDLOW=1
        DB2PTR=0
        DB0PTR=0
        PSHLD=0
        OV1FLG=0
        IF (FLAG1 .EQ. 0) GOTO 180
        CALL RMVCS (SLMFIL,1,-1)
        CALL RMVCS (SFFIL,1,-1)
        CALL RMVCS (FLMFIL,1,-1)
C
C       OPEN BOTH CODE TEMP FILES (COPY LUNS ), AND WRITE THE
C       PROGRAM ID
C
        SCLUN(1)=1
        IF (INFILE (7,SCLUN,CLUN) .NE. 0) GOTO 90000
        SCLUN2(1)=2
        CCLUN=1
        CLUNS(1)=CLUN
        CLUNS(2)=CLUN2
C
C       INITIALIZE TABLE AND MEMORY POINTERS
C
180     CALL INTT (DBDTA1,7,3)
        CALL INTT (PRGDTA,3,2)
        CALL INTT (ENTDTA,3,2)
        CALL INTT (EXTDTA,3,1)
        OVID=0
        DBST  =MDLOW
        DBBRK =DBST
        PSBRK =PSLOW
        PARPTR=0
        ENTPT1=0
        EX1PTR=0
        TSKPTR=1
      TSKDTA(1,6)=0
      TSKDTA(1,7)=0
        SENTPT=0
      SENTDT=0
      SEXTPT=0
      SEXTDT=0
        SPARPT=0
      SDB0PT=0
      SDB1PT=0
      SDB2PT=0
        OVPPTR=0
      PSPPTR=0
        TASKFL=.FALSE.
      ISRFL=.FALSE.
      TSKFL=.FALSE.
        IOPENX=.FALSE.
      PSTOP=0
C   RESET TOP OF PS
C
C        INITIALIZE THE HOST RESIDENT LOAD MODULE VARIABLES
C        SUBROUTINE COUNT AND POINTER TO ITS CODE ARRAY ELEMENT.
C
        SUBCNT=0
        CODPTR=1
        MXDATA=200/8 * 8
        RETURN
C
C------- ERROR RETURNS
C
C       CAN NOT ASSIGN FILE
C
90000   CALL ERRMES (10)
        RETURN
        END
C+++ INPUT
C****** INPUT = INPUT COMMAND FOR APLOAD = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
         SUBROUTINE INPUT
C------- INPUT
C
C       CLOSE PREVIOUS COMMAND FILE IF THERE WAS ONE, PICK UP THE NEW
C       FILE NAME AND ASSIGN IT.
C
C                                                                       MAINCOM
C  THE FIRST TWO LINES OF DECLARATION ARE NOT COMMON VARIABLES          MAINCOM
C  BUT INTEGER FUNCTIONS THAT MAY BE CALLED.                            MAINCOM
        INTEGER RDREC,EXTTOK,SRCST,EXTSL,RPLCS,EXTVT,INSST,EXTSS        MAINCOM
        INTEGER SRCSL,EXTCNT,LENT,SRCCS,STOI,IADDC,COMAND,CMPSS         MAINCOM
C                                                                       MAINCOM
        INTEGER ILUN,STR,IPTR,SYM,INDX                                  MAINCOM
        INTEGER IFLG,IV,OVSKIP,IV2,TREFLG                               MAINCOM
        INTEGER I,II,J,JJ,ID,OVMESZ,SFILE1                              MAINCOM
        INTEGER OVEPT,SYM2,LFFLG                                        MAINCOM
        INTEGER LIBFLG,COUNT,IRADIX,SFILE,IPTLUN                        MAINCOM
        INTEGER BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG                    MAINCOM
        INTEGER RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD                      MAINCOM
        INTEGER APOVEN,OVLDEN,PSCNT,IZ1QS                               MAINCOM
        COMMON /MCOMM/ ILUN,STR(81),IPTR,SYM(8),INDX,                   MAINCOM
     *   IFLG,IV,OVSKIP,IV2,TREFLG,                                     MAINCOM
     *   I,II,J,JJ,ID,OVMESZ,SFILE1(16),                                MAINCOM
     *   OVEPT,SYM2(8),LFFLG,                                           MAINCOM
     *   LIBFLG,COUNT,IRADIX,SFILE(31),IPTLUN,                          MAINCOM
     *   BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG,                          MAINCOM
     *   RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD,                            MAINCOM
     *   APOVEN,OVLDEN,PSCNT,IZ1QS                                      MAINCOM
C                                                                       MAINCOM
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        ID=INFILE (4,SFILE,IPTLUN)
        ILUN=-1
        CALL SKPBLK (STR,IPTR)
        IF (IPTR .GT. STR(1)) GOTO 90060
        ID=EXTSS (STR,IPTR,SFILE,30)
        ILUN=IPTLUN
        IF (INFILE (1,SFILE,ILUN) .EQ. 0) GOTO 500
        CALL ERRMES (10)
        ILUN=-1
        GOTO 500
90060   CALL ERRMES(12)
500     RETURN
        END
C+++ ITOSX
C****** ITOSX = INTEGER TO STRING ROUTINE = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE ITOSX (VALUE,STRING,MAXLEN,RADIX)
C
        INTEGER VALUE,STRING(99),MAXLEN,RADIX
C
C       THIS ROUTINE WORKS JUST LIKE THE UTILITIES ITOS ROUTINE EXCEPT
C       THAT IT IS ASSUMED THAT THE DESTINATION STRING IS A SUBPOSITION
C       OF A LARGER STRING. THEREFORE, AFTER VALUE TO STRING TRANSLATION
C       TAKES PLACE THE FIRST ELEMENT OF THE STRING IS SET TO A BLANK.
C       EXCEPT FOR SETTING THE FIRST ELEMENT TO BLANK THE CALL AND
C       RESULTS ARE EXACTLY THOSE OF THE UTILITIES ITOS ROUTINE.
C
C       CALL:
C
C       VALUE  = THE VALUE TO BE CONVERTED TO A STRING
C       STRING = THE STRING TO CONTAIN THE VALUE
C       MAXLEN = THE NUMBER OF CHARACTERS TO BE USED FOR RESULTS
C       RADIX  = THE CONVERSION RADIX
C
C       RETURNS:
C
C       STRING = THE STRING EQUIVALENT OF 'VALUE' AND STRING(1)=BLANK.
C
        CALL ITOS (VALUE,STRING,MAXLEN,RADIX)
        STRING(1)=8224
        RETURN
        END
C+++ LDBMAK
C****** LDBMAK = MAKE LOCAL DATA BLOCK = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        INTEGER FUNCTION LDBMAK (SYM)
C
        INTEGER SYM(9999)
C
C       THIS ROUTINE CREATES A LOCAL DATA BLOCK FOR THE SYMBOL SYM.
C       THE SIZE IS SPECIFIED BY THE PARAM ENTRY IN ENTDTA() FOR
C       THE SAME SYMBOL.
C
C       CALL:
C
C       SYM    = THE NAME OF THE LOCAL DB.  THE CORRESPONDING ENTRY
C                IS SYM MINUS ITS FIRST CHAR.  SHOULD BE '.'
C
C       RETURNS:
C
C       LDBMAK = THE INDEX IN DBDTA1() OF THE NEW LOCAL DB.
C
C
C       ROUTINES-UTIL:  RMVCS, IRSH16, INSCS, INSST, SRCST, EXTVT
C
        INTEGER INDX,ID,VAL,LDOT,VALS(3)
        INTEGER SRCST,EXTVT,INSST
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        DATA LDOT /8238/
C
        CALL RMVCS (SYM,1,1)
        CALL PAKS (SYM,SYM,6)
        INDX=SRCST (ENTDTA,1,-1,SYM,6)
        CALL UPAKS (SYM,SYM,6)
        IF (INDX .EQ. 0) GOTO 90020
        VAL=EXTVT (ENTDTA,INDX,2,VAL,1)
        VAL=IRSH16 (VAL,8)
        IF (VAL .EQ. 0) RETURN
        CALL INSCS (SYM,1,LDOT)
        IF (DBDTA1(1)+1 .GT. DB1MAX) GOTO 90040
        INDX=INSST (DBDTA1,-1,SYM,7)
        VALS(1)=DBBRK
        VALS(2)=0
        VALS(3)=VAL
        DBBRK=IADD16 (DBBRK,VAL)
        CALL RPLVT (DBDTA1,-1,1,VALS,3)
        LDBMAK=INDX
        RETURN
C
C       ERROR RETURNS
C
C       BAD RECORD
C
90020   CALL ERRMES (2)
        CALL WRTLIN (SYM,-1,-1)
        RETURN
C
C       TOO MANY D.B.(S)
C
90040   CALL ERRMES (16)
        RETURN
        END
C+++ LIB
C****** LIB = LIB COMMAND FOR APLOAD = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE LIB
C
C
C------- LIB <FILE NAME>
C
C       SET LIBFLG TO SAY THE LOAD WAS INITIATED BY THE LIB COMMAND
C
C       JUMP TO THE LOAD CODE AND COUNT HOW MANY NEW EXTERNALS HAVE
C       BEEN ADDED THAT ARE UNSATISFIED.  IF THIS IS NON-ZERO CONTINUE
C       JUMPING TO THE LOAD CODE UNTIL IT IS.
C
C                                                                       MAINCOM
C  THE FIRST TWO LINES OF DECLARATION ARE NOT COMMON VARIABLES          MAINCOM
C  BUT INTEGER FUNCTIONS THAT MAY BE CALLED.                            MAINCOM
        INTEGER RDREC,EXTTOK,SRCST,EXTSL,RPLCS,EXTVT,INSST,EXTSS        MAINCOM
        INTEGER SRCSL,EXTCNT,LENT,SRCCS,STOI,IADDC,COMAND,CMPSS         MAINCOM
C                                                                       MAINCOM
        INTEGER ILUN,STR,IPTR,SYM,INDX                                  MAINCOM
        INTEGER IFLG,IV,OVSKIP,IV2,TREFLG                               MAINCOM
        INTEGER I,II,J,JJ,ID,OVMESZ,SFILE1                              MAINCOM
        INTEGER OVEPT,SYM2,LFFLG                                        MAINCOM
        INTEGER LIBFLG,COUNT,IRADIX,SFILE,IPTLUN                        MAINCOM
        INTEGER BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG                    MAINCOM
        INTEGER RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD                      MAINCOM
        INTEGER APOVEN,OVLDEN,PSCNT,IZ1QS                               MAINCOM
        COMMON /MCOMM/ ILUN,STR(81),IPTR,SYM(8),INDX,                   MAINCOM
     *   IFLG,IV,OVSKIP,IV2,TREFLG,                                     MAINCOM
     *   I,II,J,JJ,ID,OVMESZ,SFILE1(16),                                MAINCOM
     *   OVEPT,SYM2(8),LFFLG,                                           MAINCOM
     *   LIBFLG,COUNT,IRADIX,SFILE(31),IPTLUN,                          MAINCOM
     *   BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG,                          MAINCOM
     *   RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD,                            MAINCOM
     *   APOVEN,OVLDEN,PSCNT,IZ1QS                                      MAINCOM
C                                                                       MAINCOM
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        I=1
        LIBFLG=1
1510    IV=EXTDTA(1)+1
        CALL LOAD (I,STR,IPTR,LIBFLG,0)
        IV2=EXTCNT (IV,1)
        IF (IV2 .EQ. 0) GOTO 1580
        IZ1QS = INFILE (6,SOFIL,OLUN)
        I=0
        GOTO 1510
1580    IZ1QS = INFILE (4,SOFIL,OLUN)
        RETURN
        END
C+++ LINKS
C****** LINKS = LINK COMMAND FOR APLOAD = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE LINKS
C------- LINK
C
C       MAKE SURE FILES ARE ASSIGNED.  WRITE FM TO AND REWIND CLUN,
C       CALL LINKER.
C
C       IF DOING OVERLAYS (OVPTR .NE. 0) JUMP TO OV COMMAND CODE.  LNKFLG=1
C       SEZ WE CAME FROM LINK COMMAND.
C
C                                                                       MAINCOM
C  THE FIRST TWO LINES OF DECLARATION ARE NOT COMMON VARIABLES          MAINCOM
C  BUT INTEGER FUNCTIONS THAT MAY BE CALLED.                            MAINCOM
        INTEGER RDREC,EXTTOK,SRCST,EXTSL,RPLCS,EXTVT,INSST,EXTSS        MAINCOM
        INTEGER SRCSL,EXTCNT,LENT,SRCCS,STOI,IADDC,COMAND,CMPSS         MAINCOM
C                                                                       MAINCOM
        INTEGER ILUN,STR,IPTR,SYM,INDX                                  MAINCOM
        INTEGER IFLG,IV,OVSKIP,IV2,TREFLG                               MAINCOM
        INTEGER I,II,J,JJ,ID,OVMESZ,SFILE1                              MAINCOM
        INTEGER OVEPT,SYM2,LFFLG                                        MAINCOM
        INTEGER LIBFLG,COUNT,IRADIX,SFILE,IPTLUN                        MAINCOM
        INTEGER BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG                    MAINCOM
        INTEGER RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD                      MAINCOM
        INTEGER APOVEN,OVLDEN,PSCNT,IZ1QS                               MAINCOM
        COMMON /MCOMM/ ILUN,STR(81),IPTR,SYM(8),INDX,                   MAINCOM
     *   IFLG,IV,OVSKIP,IV2,TREFLG,                                     MAINCOM
     *   I,II,J,JJ,ID,OVMESZ,SFILE1(16),                                MAINCOM
     *   OVEPT,SYM2(8),LFFLG,                                           MAINCOM
     *   LIBFLG,COUNT,IRADIX,SFILE(31),IPTLUN,                          MAINCOM
     *   BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG,                          MAINCOM
     *   RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD,                            MAINCOM
     *   APOVEN,OVLDEN,PSCNT,IZ1QS                                      MAINCOM
C                                                                       MAINCOM
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C                                                                       COMMON10
       INTEGER TSKDTA(26,8),TSKPTR,TSKMAX,TCBMNL,TCBMXL                 COMMON10
       INTEGER SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,SPARPT                COMMON10
       INTEGER SDB0PT,SDB1PT,SDB2PT,PSTOP                               COMMON10
       INTEGER ISRMDA,ISRPSA,ISRIDX,ISRLEN                              COMMON10
       INTEGER OVPDTA(50,3),PSPDTA(50),OVPPTR,PSPPTR,PSPMAX,OVPMAX      COMMON10
       LOGICAL TASKFL,TSKFL,ISRFL,IOPENX                                COMMON10
       COMMON /TSKCOM/ TSKDTA,TSKPTR,TSKMAX,TCBMNL,TCBMXL,              COMMON10
     -         SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,                      COMMON10
     -         SPARPT,SDB0PT,SDB1PT,SDB2PT,PSTOP,                       COMMON10
     -         ISRMDA,ISRPSA,ISRIDX,ISRLEN,                             COMMON10
     -         OVPDTA,PSPDTA,OVPPTR,PSPPTR,PSPMAX,OVPMAX,               COMMON10
     -         TASKFL,TSKFL,ISRFL                                       COMMON10
       COMMON /OPENX/ IOPENX                                            COMMON10
C                                                                       COMMON10
        IF (OVPTR .EQ. 0) GOTO 4960
        CALL OVLY (ID,ID,IRADIX,1,SYM)
        IF (.NOT.TASKFL) RSTRCT=2
        IF (.NOT.ISRFL) CALL ENDLNK(1)
        GO TO 5000
4960    CONTINUE
        CALL LINKUP (OVFLG,ID,PSCNT)
        IF (.NOT.TASKFL) RSTRCT=2
        IF (.NOT.ISRFL) CALL ENDLNK(2)
5000    CONTINUE
        IF (TASKFL.AND.(TSKFL.OR.ISRFL)) CALL TSKLNK
        TSKFL=.FALSE.
      ISRFL=.FALSE.
        IF (TASKFL) PSBRK=PSTOP
C UPDATE PS POINTER
        PSTOP=0
C
C       REWIND THE TEMP OVERLAY FILES
C
        ID=INFILE(6,SCLUN,CLUNS(1))
        IF (OVFLG.NE.0) ID=INFILE(6,SCLUN2,CLUNS(2))
        OVLEVL=0
C RESET OVERLAY LEVEL
        RETURN
        END
C+++ LINKUP
C****** LINKUP = LINK COMMAND = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE LINKUP (DEST,LNKCNT,PSCNT)
C
        INTEGER DEST,LNKCNT,PSCNT
C
C       CALL:
C
C       DEST   = 0 IF WE HAVEN'T BEEN BUILDING OVERLAYS AND CODE IS TO
C                  BE LOADED DIRECTLY INTO PS (NO OVERLAY).
C                1 IF WE HAVE BEEN MAKING OVERLAYS AND CODE IS TO BE LOADED
C                  INTO MD.
C       LNKCNT = SPECIFIES THE NUMBER OF OVERLAY LEVELS TO BE LINKED
C                DURING THIS CALL.  IGNORED IF DEST=0.
C       OVLEVL = THE CURRENT OVERLAY LEVEL.  IGNORED IF DEST=0.
C       OVBASE = THIS IS THE EQUIVALENT OF PSLOW FOR AN OVERLAY.  THIS
C                IS THE LOCATION WHERE IT WILL BE LOADED INTO PS.
C
C       RETURNS:
C
C       PSCNT  = THE NUMBER OF PS WORDS WHICH WERE PLACED IN THE
C                LOAD MODULE.  THIS IS USED LATER BY THE LNKOUT ROUTINE.
C                THE VALUE IS MEANINGLESS IF OVERLAYS ARE USED.
C
C       RETURNS COMMON:
C
C       CLUN   = UPDATED TO THE ALTERNATE COPY LUN AND REWOUND
C       CCLUN  = UPDATED TO REFLECT THE CHOSEN ALTERNATE COPY LUN
C
C       VARIABLES:
C
C       CODSIZ = THE NUMBER OF AP CODE WORDS THAT WILL FIT IN THE BUFFER()
C       PACK   = THE NUMBER OF HOST WORDS PER AP CODE WORD (2 OR 4)
C       LODADR = THE FIRST ADDRESS TO BE LOADIN EITHER PS OR MD.  IF DEST
C                SPECIFIES PS, LODADR:=PSLOW, IF OVERLAYS ARE USED THE
C                VALUE WILL BE FOUND IN THE OVDTA().
C       LNKLVL = THE NUMBER OF LEVELS TO LINK IF WE ARE LINKING OVERLAYS
C                AS LONG AS THIS VALUE IS POSITIVE, RECORDS ARE READ FROM
C                CLUN, RELOCATION IS DONE IF NECCESSARY, AND WRITTEN TO
C                THE ALTERNATE CLUN (NEWCLN).  LNKLVL IS DECREMENTED
C                EVERY TIME THE END OF AN OVERLAY IS ENCOUNTERED.  WHEN
C                IT GOES TO ZERO RECORDS ARE RELOCATED AND WRITTEN TO THE
C                LOAD MODULE (LMLUN).
C       IPTRSV = THIS POINTER SAVES POSITION OF THE 4TH MICRO-CODE VALUE,
C                NOT USED IF NOT LINKING OVERLAYS.
C       PTRSV2 = SAVES THE POSITION OF THE * (DESIGNATES RELOCATION), USED
C                ONLY WITH OVERLAYS.
C
        INTEGER STR(81),IPTR,SYM(8),CODE(4),FLDDES,TYPEN,ARG,INDX
        INTEGER PTREXT,PTRDB,LDADDR,ITYP,RECCNT,LOC,VAL,RELTYP,I,J,ID
        INTEGER PTRCOD,CODSIZ,PACK,IPT1,LOCCUR,IPTRSV,LNKLVL,PTRSV2
        INTEGER IVAL(3),NEWCLN,LODADR,OVBASE,OVPAGE
C
        INTEGER RDREC,EXTTOK,STOI,EXTVT,EXTST,SRCST,LDBMAK
        REAL DUMMY
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
C                                                                       STARTPRE
      INTEGER SFM(2)
C STRNG SFM "]"
      DATA SFM(2)/'] '/
      DATA SFM(1)/1/
C                                                                       ENDPRE
C
C------- INITIALIZATION ...
C
        CODSIZ=(BUFSIZ/4)*PAKFAC
        PACK=4/PAKFAC
        PSCNT=0
        NEWCLN=3-CCLUN
        NEWCLN=CLUNS(NEWCLN)
        LNKLVL=OVLEVL-LNKCNT
        IF (DEST .EQ. 0) LNKLVL=0
        IF (DEST .EQ. 0) LODADR=PSLOW
        IF (DEST .NE. 0) ID=INFILE (6,ID,NEWCLN)
        CALL WRTLIN (SFM,CLUN,2)
        ID=INFILE (6,SCLUN,CLUN)
        OVPAGE=0
        CALL DTALNK (STR)
        ID=INFILE (6,ID,DBLUN)
C
C       READ TITLE RECORD.  SHOULD BE A $ FOLLOWED BY THE PRGDTA PTR
C       FOR THIS CODE.  IF FOLLOWED BY A ZERO THE NEXT NUMBER IS THE
C       OVERLAY MARKER.  IF ZERO THIS MARKS THE END OF AN OVERLAY, OTHER-
C       WISE IT IS AN OVERLAY ID NUMBER.
C
400     IF (RDREC (CLUN,STR,IPTR)+1) 500,90000,700
C
C       EOF ENCOUNTERED
C
500     IF (DEST .EQ. 0) RETURN
        CLUN=NEWCLN
        CCLUN=3-CCLUN
        RETURN
700     IF (LNKLVL .NE. 0) CALL WRTLIN (STR,NEWCLN,-1)
        IF (STR(1) .EQ. 0) GOTO 400
       IF (EXTTOK (SYM,1,STR,IPTR,IPTR,10) .NE. 8228) GOTO 90020
        VAL=EXTTOK (SYM,6,STR,IPTR,IPTR,10)
        IF (VAL .EQ. 0) GOTO 400
        IF (VAL .NE. -2) GOTO 90020
        INDX=STOI (SYM,10)
C
C       INDX=0 SPECIFIES THE END OF AN OVERLAY LEVEL
C
        IF (INDX .GT. 0) GOTO 800
        IF (DEST .EQ. 0) GOTO 90020
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,10) .NE. -2) GOTO 90020
        INDX=STOI (SYM,10)
        IF (INDX .EQ. 0) GOTO 770
        DO 730 J=1,CUROV
        IF (OVDTA(J,1) .EQ. INDX) GOTO 740
730     CONTINUE
        GOTO 90020
740     LODADR=OVDTA(J,3)
        OVBASE=OVDTA(J,2)
        OVPAGE=IAND16 (OVDTA(J,6),15)
        GOTO 400
770     IF (LNKLVL .NE. 0) LNKLVL=LNKLVL-1
        GOTO 400
C
C       GET EXTERNAL PTR., DATA BLOCK PTR., AND LOAD ADDRESS FOR THIS
C       ROUTINE FROM PRGDTA().
C
800     IF (INDX .GT. PRGDTA(1)) GOTO 90020
        LDADDR=EXTVT (PRGDTA,INDX,1,IVAL,2)
        PTREXT=IAND16 (IVAL(2),255)
        PTRDB=IRSH16 (IVAL(2),8)
C
C       NOW READ CODE HEADER OR END DESIGNATOR ($)
C
900     IF (RDREC (CLUN,STR,IPTR)+1) 90040,90000,1000
1000    IF (LNKLVL .NE. 0) CALL WRTLIN (STR,NEWCLN,-1)
        ITYP=EXTTOK (SYM,1,STR,IPTR,IPTR,10)
        IF (ITYP .EQ. 8228) GOTO 400
C
C       WE SHOULD HAVE A CODE RECORD.  HAVE SKIPPED BLOCK ID, GET RECORD
C       COUNT, AND LOAD LOC AND SEND CODE HEADER TO LM.
C
        IF (ITYP .NE. -2) GOTO 90020
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        RECCNT=STOI (SYM,RADIX)
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        LOC=STOI (SYM,RADIX)
        LOCCUR=IADD16 (LDADDR,LOC)
        IF (DEST .EQ. 0) VAL=LOCCUR
C       THE NEXT LINE SETS VAL=LODADR+LOC*2+(LDADDR-OVBASE)*2
        IF (DEST .EQ. 1)
     *  VAL=IADD16 (IADD16 (LODADR,ILSH16 (LOC,1)),
     *             ILSH16 (ISUB16 (LDADDR,OVBASE),1))
        IF (LNKLVL .EQ. 0) CALL WRTLM (0,0,RECCNT*PACK,VAL,OVPAGE,
     *                     DEST,0,0,0,0.0,0.0)
C
C       UPDATE THE COUNT OF THE NUMBER OF PS WORDS BEING INITIALIZED
C       IN THE LOAD MODULE (LNKLVL=0 SEZ PS DESTINATION IISS LOAD MOD.)
C       READ CODE RECORDS AND COPY THEM TO LMLUN, RELOCATING AND LINKING
C       ALONG THE WAY.  RESET OUTPUT BUFFER PRINTER.
C
        PSCNT=LOCCUR+RECCNT
        PTRCOD=0
        DO 4000 I=1,RECCNT
        IF (RDREC (CLUN,STR,IPTR)+1) 90040,90000,2000
C
C       NO RELOCATION IF LINE DOESN'T START W/ AN ASTRICK (*).
C
2000    ITYP=EXTTOK (SYM,1,STR,IPTR,IPTR,10)
        PTRSV2=IPTR-1
        IF (ITYP .NE. 8234) IPTR=1
        IF (LNKLVL .EQ. 0 .OR. ITYP .EQ. 8234) GOTO 2030
        CALL WRTLIN (STR,NEWCLN,-1)
        LOCCUR=IADD16 (LOCCUR,1)
        GOTO 4000
C
C       READ THE CODE WORDS.
C
2030    DO 2050 J=1,4
        IPTRSV=IPTR+1
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        CODE(J)=STOI (SYM,RADIX)
2050    CONTINUE
        IF (ITYP .NE. 8234) GOTO 2580
C
C       GET THE RELOCATION TRIPLET:  FIELD DESIGNATOR, TYPEN, AND ARGUEMENT
C       SPECIFIER.
C
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        FLDDES=STOI (SYM,RADIX)+1
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        TYPEN=STOI (SYM,RADIX)
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        ARG=STOI (SYM,RADIX)
C
C       CHECK TYPEN RANGE (1-5) AND PROCESS ACCORDINGLY
C
        IF (TYPEN .LE. 0 .OR. TYPEN .GT. 5) GOTO 90020
        GOTO (2150,2200,2400,2300,2200),TYPEN
C
C------- PROGRAM SOURCE RELOCATABLE
C
2150    VAL=LDADDR
        RELTYP=0
        GOTO 2500
C
C------- EXTERNAL REFERENCE:  (BOTH ABSOLUTE AND RELATIVE) FIND THE EXTERNAL
C       SYMBOL AND SEARCH ENTDTA() FOR IT AND ITS VALUE.
C
2200    VAL=ARG+PTREXT-1
        VAL=EXTDT1(VAL)
        ID=EXTST (EXTDTA,VAL,SYM,6)
        INDX=SRCST (ENTDTA,1,-1,SYM,6)
        IF (INDX .EQ. 0) GOTO 2580
        VAL=EXTVT (ENTDTA,INDX,1,IVAL,2)
        IF (IAND16 (IVAL(2),8) .NE. 0) GOTO 2580
        RELTYP=IAND16 (IVAL(2),7)
C
C       IF RELTYP = 2 RELOCATE RELATIVE TO THE CURRENT PS LOC (LOCCUR).
C       WE DON'T EVER DO THIS IF THE CALL IS FROM APFTN (TYPEN=2)
C
        IF (RELTYP .NE. 0 .AND. TYPEN .NE. 2) VAL=ISUB16 (VAL,LOCCUR)
        GOTO 2500
C
C------- SUBROUTINE PARAMETER REFERENCE:  GET THE NAME OF THE ARG(TH)
C       EXTERNAL AND RELOCATE VIA THAT ROUTINES LOCAL DATA BLOCK.  THE
C       DB NAME IS THE ENTRY POINT NAME PRECEEDED BY A PERIOD.
C
2300    VAL=ARG+PTREXT-1
        VAL=EXTDT1(VAL)
        ID=EXTST (EXTDTA,VAL,SYM,6)
        IF (SRCST (ENTDTA,1,-1,SYM,6) .EQ. 0) GOTO 2580
        CALL UPAKS (SYM,SYM,6)
        CALL INSCS (SYM,0,8238)
        INDX=SRCST (DBDTA1,1,-1,SYM,7)
        IF (INDX .NE. 0 ) GO TO 2310
        INDX=LDBMAK (SYM)
2310     CONTINUE
        VAL=EXTVT (DBDTA1,INDX,1,IVAL,1)
        RELTYP=0
        GOTO 2500
C
C------- DB REFERENCE:  GET THE BASE ADDRESS OF THE SPECIFIED BLOCK
C
2400    VAL=ARG+PTRDB-1
        VAL=DBDTA0(VAL)
        VAL=EXTVT (DBDTA1,VAL,1,IVAL,1)
        RELTYP=0
        GOTO 2500
C
C       RELOCATE THE FIELD DESIGNATED BY FLDDES, CHECK RANGE (1-16)
C
2500    IF (FLDDES .LE. 0 .OR. FLDDES .GT. 16) GOTO 90020
C
        CODE(4)=IADD16 (CODE(4),VAL)
C
C       IF WE HAVE LINKED A WORD IN AN OVERLAY THAT DOESN'T GO TO THE
C       LOAD MODULE THEN MESSAGE THE STRING INTO A NON-RELOCATABLE FORM.
C
        IF (LNKLVL .EQ. 0) GOTO 2585
        STR(IPTRSV+2)=8224
        STR(IPTRSV+3)=8224
        STR(IPTRSV+4)=8224
        STR(IPTRSV+5)=8224
        STR(IPTRSV+6)=8224
C       CALL RPLIS (STR,IPTRSV,CODE(4),6,RADIX,&ZERO)
        CALL ITOSX (CODE(4),STR(IPTRSV),6,RADIX)
        CALL RMVCS (STR,IPTRSV+6,-1)
        CALL RMVCS (STR,PTRSV2,1)
C
C       PLACE A RELOCATED CODE GROUP INTO BUFFER().  IF MORE THAN 50
C       SO FAR, WRITE THEM TO THE LOAD MODULE.  UPDATE THE LOCATION COUNT.
C
2580    IF (LNKLVL .EQ. 0) GOTO 2585
        CALL WRTLIN (STR,NEWCLN,-1)
        LOCCUR=IADD16 (LOCCUR,1)
        GOTO 4000
2585    PTRCOD=PTRCOD+1
        IF (EXTHST .NE. 1) GOTO 2600
        DO 2590 J=1,4
2590    IF (CODE(J) .GT. 32767) CODE(J)=CODE(J)-M65536
2600    LOCCUR=IADD16 (LOCCUR,1)
        IF (PTRCOD .LE. CODSIZ) GOTO 2620
        CALL WRTBR (1,BUFFER,BUFSIZ,1,0,DUMMY,DUMMY)
        PTRCOD=1
2620    IPT1=(PTRCOD-1)*PACK+1
        DO 2680 J=1,4,PAKFAC
        IF (PAKFAC .EQ. 1) BUFFER(IPT1)=CODE(J)
        IF (PAKFAC .EQ. 2) BUFFER(IPT1)=CODE(J)*256*256+CODE(J+1)
        IPT1=IPT1+1
2680    CONTINUE
4000    CONTINUE
C
C       WRITE WHAT IS LEFT IN BUFFER() TO THE LM
C
        IF (LNKLVL .EQ. 0)
     *      CALL WRTBR (1,BUFFER,(PTRCOD*4)/PAKFAC,1,0,DUMMY,DUMMY)
        GOTO 900
C
C       ERROR RETURNS
C
C       READ ERROR
C
90000   CALL ERRMES (1)
        RETURN
C
C       BAD RECORD
C
90020   CALL ERRMES (2)
        CALL WRTLIN (STR,-1,-1)
        RETURN
C
C       UNEXPECTED EOF
C
90040   CALL ERRMES (4)
        RETURN
        END
C+++ LMDCLR
C****** LMDCLR = WRITE LM DECLARATION LINES = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE LMDCLR
C
C
C       THIS ROUTINE WRITES THE HEADING LINES TO A HOST RESIDENT
C       LOAD MODULE SUBROUTINE.  NAMELY, THE SUBROUTINE AND
C       INTEGER STATEMENTS.
C             SUBROUTINE LIIJJ
C             INTEGER CODE(NNNNNN)
C       WHERE,
C             II - IS THE LOAD MODULE ID
C             JJ - IS THE SUBROUTINE COUNTER
C         NNNNNN - IS THE MAXIMUM NUMBER OF DATA STATEMENTS PER
C                   SUBROUTINE
C
C
C       COMMON:
C
C       LMID - LOAD MODULE IDENTIFICATION NUMBER
C     SUBCNT - SUBROUTINE COUNTER
C     MXDATA - MAXIMUM NUMBER OF DATA STATEMENTS PER SUBROUTINE
C     FLMLUN - HOST RESIDENT LOAD MODULE LOGICAL UNIT NUMBER
C
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        INTEGER LUN,LMNAME
C
C
C       OUTPUT TO HOST RESIDENT LOAD MODULE ONLY IF IT WAS SELECTED.
C
        IF (LMLDSW.EQ.0) RETURN
        LUN=FLMLUN+LUNMAP
        LMNAME=LMID*100+SUBCNT
        WRITE (LUN,510) LMNAME,MXDATA
510     FORMAT (1X,6X,'SUBROUTINE L',I4/
     *          1X,6X,'INTEGER CODE(',I6,')')
        RETURN
        END
C+++ LMSRC2
C****** LMSRC2 = WRITE HOST LM TRAILER LINES = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE LMSRC2 (CODE)
        INTEGER CODE
C
C
C       THIS ROUTINE WRITES THE TRAILER LINES TO A HOST RESIDENT
C       LOAD MODULE SUBROUTINE.  NAMELY, THE CALL TO FSLMLD,
C       THE CALL TO THE NEXT SUBROUTINE IN A CHAIN IF NEEDED,
C       A RETURN, AND AN END STATEMENT.
C           CALL FSLMLD(NNNNNN,CODE)
C           CALL LIIJJ
C           RETURN
C           END
C       WHERE,
C           NNNNNN - IS THE LOAD MODULE ID NUMBER
C           II - IS THE LOAD MODULE ID NUMBER
C           JJ - IS THE SUBROUTINE COUNTER + 1
C
C
C       COMMON:
C
C       LMID - LOAD MODULE IDENTIFICATION NUMBER
C     SUBCNT - SUBROUTINE COUNTER
C     FLMLUN - HOST RESIDENT LOAD MODULE LOGICAL UNIT NUMBER
C
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        INTEGER LUN,LMNAME
C
C       OUTPUT TO HOST RESIDENT LOAD MODULE ONLY IF IT WAS SELECTED.
C
        IF (LMLDSW.EQ.0) RETURN
        LUN=FLMLUN+LUNMAP
C
C   OUTPUT THE CALL TO FSLMLD.
C
        WRITE (LUN,110) LMID
110     FORMAT (1X,6X,'CALL FSLMLD(',I6,',CODE)')
C
C   IF CODE IS 0 THEN WE WANT TO CHAIN TO THE NEXT SUBROUTINE
C   SO INCREMENT SUBROUTINE COUNTER AND OUTPUT A CALL.
C
        IF (CODE.EQ.1) GOTO 600
        SUBCNT=SUBCNT+1
        LMNAME=LMID*100+SUBCNT
        WRITE (LUN,210) LMNAME
210     FORMAT (1X,6X,'CALL L',I4)
C
C   OUTPUT RETURN AND END STATEMENTS.
600     CONTINUE
        WRITE (LUN,610)
610     FORMAT (1X,6X,'RETURN'/
     *          1X,6X,'END')
        RETURN
        END
C+++ LOAD
C****** LOAD = LOAD OBJECTS FROM A FILE = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE LOAD (FLAG,STR,IPTR,LIBFLG,LFFLG)
C
        INTEGER FLAG,STR(999),IPTR,LIBFLG,LFFLG
C
C       CALL:
C
C       FLAG   = 1 IF STR() CONTAINS THE COMMAND LINE, ELSE 0 IF THE
C                OBJECT FILE IS ALREADY ASSIGNED (LIKE HSRLIB USES).
C
        INTEGER COUNT,IFLG,J,ID
        INTEGER  EXTSS, INFILE, IADDC, INEG16, EXTCNT, ISUB16
        INTEGER IXX,STROV(3),SYM(8)
        COMMON /GLOP/ IXX,STROV,SYM
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
C                                                                       STARTPRE
      INTEGER SLC(7)
C STRNG SLC "  **LC"
      DATA SLC(2)/'  '/,SLC(3)/'  '/,SLC(4)/'* '/,SLC(5)/'* '/
      DATA SLC(6)/'L '/,SLC(7)/'C '/
      DATA SLC(1)/6/
C                                                                       ENDPRE
C
C------- LOAD <FILE NAME>
C
C       IF OVFLG=1 AND OVID=0 WE ARE BUILDING OVERLAYS BUT TRYING TO LOAD
C       W/O HAVING FIRST GIVEN AN OV COMMAND.  OTHERWISE SET OVID=-1 TO
C       FLAG THAT THIS LOAD MODULE DOES NOT CONTAIN OVERLAYS.
C       GET FILE NAME, ASSIGN IT.
C
C       **** THIS CODE SHOULD NEVER USE IV AND IV2 AS THEY ARE USED
C       **** BY THE LIB COMMAND WHICH JUMPS HERE.
C
        IF (OVFLG .EQ. 0) GOTO 1040
        IF (OVID .NE. 0) GOTO 1070
        GOTO 90020
1040    OVID=-1
C
C       MAKE SURE OUTPUT FILES HAVE BEEN ASSIGNED.
C
1070    IF (FLAG .EQ. 0) GOTO 1150
        CALL SKPBLK (STR,IPTR)
        ID=EXTSS (STR,IPTR,SOFIL,30)
        IF (SOFIL(1) .EQ. 0) GOTO 90000
C    THE 65 PARAMETER IN THE NEXT CALL IS REALLY 1+64.  THE 64 MEANING
C    SOMETHING SPECIAL TO INFILE (OPEN FILE WITH FIXED LENGTH RECORDS).
        IF (INFILE (65,SOFIL,OLUN) .NE. 0) GOTO 90000
1150    CONTINUE
        IXX=0
        STROV(1)=2
1160    CONTINUE
        CALL LOAD1 (IFLG,LFFLG,LIBFLG,COUNT)
        IF (IXX.EQ.0) GO TO 1170
        ID=1
        CALL OVLY (STROV,ID,RADIX,0,SYM)
        GO TO 1160
1170    CONTINUE
        IXX=0
C
C       CHECK FOR MEMORY OVERFLOWS (PS AND MD DATA PAGE)
C
        ID=IADDC (PSMAX,INEG16 (PSBRK),J)
        IF (J .EQ. 0 .AND. PSBRK .NE. 0) CALL ERRMES (27)
        ID=IADDC (PGINFO(DBPG,1),INEG16 (DBBRK),J)
        IF (J .EQ. 0 .AND. DBBRK .NE. 0) CALL ERRMES (26)
        IF (LIBFLG .EQ. 0) ID=INFILE (4,SOFIL,OLUN)
C
C       COUNT THE NUMBER OF UNSATISFIED EXTERNALS - IF 0 OUTPUT LOAD
C       COMPLETE MESSAGE.
C
C       UPDATE OVDTA() INFO IF WE'RE MAKING OVERLAYS:  ITS LENGTH
C
C       RETURN TO LIB COMMAND CODE IF LIBFLG .NE. 0
C
        IF (EXTCNT (1,1) .EQ. 0) CALL WRTLIN (SLC,-1,-1)
        IF (OVFLG .EQ. 0) RETURN
        OVDTA(CUROV,4)=ISUB16 (PSBRK,PSHLD)*2
        RETURN
C
C
C------- ERROR RETURNS
C
C       CAN NOT ASSIGN FILE
C
90000   CALL ERRMES (10)
        RETURN
C
C       MISSING OV COMMAND
C
90020   CALL ERRMES (21)
        RETURN
        END
C+++ LOAD1
C****** LOAD1 = LOADER PASS 1 = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE LOAD1 (IFLG,LFRCFG,LBFLG,COUNT)
C
        INTEGER IFLG,LFRCFG,LBFLG,COUNT
C
C       THIS ROUTINE IS CALLED TO LOAD FROM AN OBJECT MODULE.. LOGICAL
C       UNIT OLUN CONTAINS THE LOADER BLOCKS.
C
C       CALL:
C
C       LFRCFG = 1 FOR A LIBRARY FORCE CALL, ELSE 0
C       LBFLG  = 0 FOR NORMAL LOAD, 1 FOR INITIAL FOR LIBRARY LOAD
C                2 FOR LIBRARY LOAD THAT HAS SEEN A LIBRARY START BLOCK
C
C       RETURNS:
C
C       IFLG   = 0 IF EVERYTHING READ WAS OK, ELSE 1 IF TROUBLE OCCURRED.
C       LBFLG  = 2 IF LBFLG WAS 1
C       COUNT  = THE NUMBER OF ROUTINES LOADED, 0 IF NONE.
C
C       VARIABLES:
C
C       RSTFLG = 0 THE CURRENT DBDB HAS NEVER BEEN SEEN BEFORE, 1 IF
C                IT HAS BEEN SEEN BUT NO DETAILS (DBDTA2()) EXIST.
C       SBRPTR = A POINTER TO A GIVEN ROUTINES FIRST $SUBR ENTRY POINT
C
C       VARIABLES:
C
C       SKPFLG = 0 FOR END BLOCK PROCESSING WHERE THE END BLOCK DATA
C                RECORD HAS NOT BEEN READ YET, 1 IF IT HAS BEEN READ
C
        INTEGER STR(81),ICHAR,EXTPX1,EXTPX2,DBFLG,ENTFLG,SYM(8)
        INTEGER LIBFLG,BLKTYP,RECCNT,RTBRK,RTFLG,IORDR,IVAL(3),IPTR,LOC
        INTEGER I,ID,INDX,IVAL2(2),LIBFG2,IPTRX,ILEN
        INTEGER MODDD,IX,SKPFLG,CLUNX,INDXPG,FCLFG2,PRGDAT(10),SRCFLG
        INTEGER HASIMD,IDXFCL,CODFLG,DBLUNX
        INTEGER ENPTX1,ENTPTX,FCLFLG,STR2(8),FRSTFG,PARFLG,IV
        INTEGER PARCNT,RSTFLG
        INTEGER EXTFLG,LOCPTR,J,SBRPTR
        INTEGER EXTTOK,STOI,RDREC,SRCST,EXTVT,EXTCNT,CMPSS,RPLST
        INTEGER LUNX,EXTST
        INTEGER IXX,STROV(3)
        COMMON /GLOP/ IXX,STROV,SYM
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C                                                                       COMMON10
       INTEGER TSKDTA(26,8),TSKPTR,TSKMAX,TCBMNL,TCBMXL                 COMMON10
       INTEGER SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,SPARPT                COMMON10
       INTEGER SDB0PT,SDB1PT,SDB2PT,PSTOP                               COMMON10
       INTEGER ISRMDA,ISRPSA,ISRIDX,ISRLEN                              COMMON10
       INTEGER OVPDTA(50,3),PSPDTA(50),OVPPTR,PSPPTR,PSPMAX,OVPMAX      COMMON10
       LOGICAL TASKFL,TSKFL,ISRFL,IOPENX                                COMMON10
       COMMON /TSKCOM/ TSKDTA,TSKPTR,TSKMAX,TCBMNL,TCBMXL,              COMMON10
     -         SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,                      COMMON10
     -         SPARPT,SDB0PT,SDB1PT,SDB2PT,PSTOP,                       COMMON10
     -         ISRMDA,ISRPSA,ISRIDX,ISRLEN,                             COMMON10
     -         OVPDTA,PSPDTA,OVPPTR,PSPPTR,PSPMAX,OVPMAX,               COMMON10
     -         TASKFL,TSKFL,ISRFL                                       COMMON10
       COMMON /OPENX/ IOPENX                                            COMMON10
C                                                                       COMMON10
C
C                                                                       STARTPRE
      INTEGER SDOLLR(3),SLOCAL(6),ISRMAP(7),STRTRE(7)
C STRNG SDOLLR " $"
      DATA SDOLLR(2)/'  '/,SDOLLR(3)/'$ '/
      DATA SDOLLR(1)/2/
C STRNG SLOCAL "LOCAL"
      DATA SLOCAL(2)/'L '/,SLOCAL(3)/'O '/,SLOCAL(4)/'C '/
      DATA SLOCAL(5)/'A '/,SLOCAL(6)/'L '/
      DATA SLOCAL(1)/5/
C STRNG ISRMAP "ISRMAP"
      DATA ISRMAP(2)/'I '/,ISRMAP(3)/'S '/,ISRMAP(4)/'R '/
      DATA ISRMAP(5)/'M '/,ISRMAP(6)/'A '/,ISRMAP(7)/'P '/
      DATA ISRMAP(1)/6/
C STRNG STRTRE "((  ))"
      DATA STRTRE(2)/'( '/,STRTRE(3)/'( '/,STRTRE(4)/'  '/
      DATA STRTRE(5)/'  '/,STRTRE(6)/') '/,STRTRE(7)/') '/
      DATA STRTRE(1)/6/
C                                                                       ENDPRE
C
C       INITIALIZE ...
C
        CLUNX=CLUN+LUNMAP
        DBLUNX=DBLUN+LUNMAP
        IF (IXX.NE.0) GO TO 100
        IFLG=0
        COUNT=0
        LIBFLG=0
50      RTFLG=0
        SBRPTR=0
        FCLFG2=0
        SRCFLG=0
        RTBRK=0
        FRSTFG=1
        PARFLG=0
        IORDR=0
        ENTFLG=0
        ENPTX1=ENTPT1
        ENTPTX=ENTDTA(1)+1
        LIBFG2=0
        EXTFLG=0
        DBFLG=0
        EXTPX1=EX1PTR
        EXTPX2=EXTDTA(1)+1
C
C       READ A HEADER RECORD
C
100     CONTINUE
        IXX=0
        IF (RDREC (OLUN,STR,IPTR)+1) 150,90000,300
C
C------- EOF ENCOUNTERED
C
C       MAKE SURE WERE NOT IN THE MIDDLE OF SOMETHING
C
150     IF (IORDR .NE. 0) GOTO 90180
        RETURN
300     ICHAR=EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX)
        IF (ICHAR .NE. -2) GOTO 90020
C
C       DECODE BLOCK TYPE AND CHECK RANGE.  IF IORDR=0 THEN THIS FIRST BLOCK MUS
C       NORMALLY BE A TITLE BLOCK OR LIB BLOCK IF LBFLG=1.
C
        BLKTYP=STOI (SYM,RADIX)+1
        IF (BLKTYP.LT.1.OR.BLKTYP.GT.15) GO TO 90020
        IF (IORDR .EQ. 0 .AND. BLKTYP .NE. 8 .AND.
     -  BLKTYP.NE.4.AND.BLKTYP.NE.7.AND.BLKTYP.NE.13.AND.
     -  BLKTYP.NE.14.AND.BLKTYP.NE.15) GO TO 90080
        IF (LBFLG .EQ. 1 .AND. BLKTYP .NE. 7) GOTO 90260
C
C       PROCESS:
C
C       CODE, END, N.U., TITLE, ENTRY, LIB END, LIB START, DBDB, DBIB,
C       PARAMETER, ALTERN. ENTRY, INDEX
C       TASK,ISR
C
        GOTO (1000,3000,90020,4000,4400,5000,5500,5600,6000,7000,
     -  8000,4380,2000,9000,10000),BLKTYP
C
C
C------- CODE BLOCK
C
C       GET RECORD COUNT AND LOC FROM THIS RECORD
C
1000    CONTINUE
C
C       IF LOADING FROM A LIBRARY AND HAVEN'T DECIDED WE WANT TO LOAD
C       THIS ROUTINE THEN IT'S TIME TO DUMP THE REST OF IT.
C
        IF (LIBFLG .NE. 1 .OR. LIBFG2 .NE. 0) GOTO 1020
        SKPFLG=1
        CALL SKPSUB (STR,IPTR,SYM,1)
        GOTO 3010
1020    IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        RECCNT=STOI (SYM,RADIX)
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        LOC=STOI (SYM,RADIX)
C
C       IF THIS IS THE FIRST CODE BLOCK (RTFLG=0) SET RTFLG AND SAVE
C       PROGRAM START ADDRESS IN PRGDTA().  ALSO WRITE THE $ HEADER
C       TO CLUN.
C
        IF (RTFLG .NE. 0) GOTO 1040
        RTFLG=1
        PRGDAT(5)=PSBRK
        WRITE (CLUNX,1030) INDXPG
1030    FORMAT (' $',I5)
        GOTO 1080
C
C       MAKE SURE THIS CODE BLOCK IS BEYOND THE LAST ONE.
C
1040    IF (LOC .LT. RTBRK) GOTO 90040
C
C       UPDATE RTBRK AND SKIP THROUGH THE CODE BLOCK RECORDS.
C
1080    CONTINUE
        CALL WRTLIN (STR,CLUN,-1)
        RTBRK=IADD16 (LOC,RECCNT)
        DO 1100 I=1,RECCNT
        IF (RDREC (OLUN,STR,IPTR)+1) 90060,90000,1090
1090    CONTINUE
        CALL WRTLIN (STR,CLUN,-1)
1100    CONTINUE
        GOTO 100
C
C------- INDEX
C
C       PICK UP NUMBER OF ENTRIES AND NUMBER OF RECORDS TO SKIP
C
2000    CONTINUE
        ID=EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX)
        J=STOI (SYM,RADIX)
        ID=EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX)
        IV=STOI (SYM,RADIX)
C
C       READ THE DATA RECORD, SKIP THE TITLE AND THEN SEE IF ANY OF
C       THE ENTRIES ARE NEEDED
C
        IF (RDREC (OLUN,STR,IPTR)+1) 90060,90000,2050
2050    ID=EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX)
        DO 2200 I=1,J
        ID=EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX)
        CALL PAKS (SYM,SYM,6)
        IX=SRCST (ENTDTA,1,-1,SYM,6)
        IF (IX .EQ. 0) GOTO 2100
        IF (IAND16 (EXTVT (ENTDTA,IX,2,ID,1),8) .NE. 0) GOTO 2300
        GOTO 2200
2100    IF (SRCST (EXTDTA,1,-1,SYM,6) .NE. 0) GOTO 2300
2200    CONTINUE
C
C       DIDN'T FIND AN ENTRY WE NEEDED - IF NO MORE INDEX BLOCKS
C       (IV .NE. 0) THEN SKIP THE ROUTINE
C
        IF (IV .EQ. 0) GOTO 100
C
C       SKIP THE ROUTINE
C
C
C       HERE WE WANT TO DO WHATEVER IS NECESSARY TO SKIP AS QUICKLY
C       AS APOSSIBLE   - IV -  NUMBER OF RECORDS FORWARD IN THE
C       OBJECT FILE (OLUN).  THE WORST IMPLEMENTATION OF THIS IS
C       TO DO   - IV -  FORTRAN READS,  BETTER IS TO USE SOME LOWER
C       LEVER READ ROUTINE.  IT IS NOT NECESSARY TO ACTUALLY READ
C       THE RECORD INTO A BUFFER HERE; THE IMPORTANT THING IS JUST
C       TO SKIP RECORDS AS FAST AS POSSIBLE.  THE FASTEST SOLUTION
C       IS TO SET UP LIBRARY FILES (CONTROL ONLY COMES HERE FOR
C       LIBRARY FILES BY THE WAY) THAT ARE EITHER DIRECT ACCESS
C       (AS APPOSED TO SEQUENTIAL) OR USE FIXED RECORD LENGTH.  IF
C       THIS IS POSSIBLE TO DO THEN THERE WILL BE SYSTEM ROUTINES
C       THAT SHOULD BE HERE THAT WILL LET YOU POSITION WITHING THE
C       FILE VERY QUICKLY.
C
C
C
C       CALL POSN A DEPENDENT ROUTINE TO SKIP AHEAD RECORDS IN A FILE.
C
        CALL POSN (1,IV,OLUN,IERR)
        GOTO 100
C
C       HERE WE HAVE A ROUTINE WE DO WANT TO LOAD - SKIP ANY MORE
C       INDEX BLOCKS.
C
2300    IF (IV .NE. 0) GOTO 100
        IF (RDREC (OLUN,STR,IPTR)+1) 90060,90000,2520
2520    ID=EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX)
        ID=EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX)
        IV=STOI (SYM,RADIX)
        IF (RDREC (OLUN,STR,IPTR)+1) 90060,90000,2300
C
C------- END BLOCK
C
C       IF WE WERE PROCESSING A LIBRARY ROUTINE (LIBFLG=1) AND NEVER
C       DECIDED THAT THIS ROUTINE SHOULD BE LOADED (LIBFG2=0) THEN WE NEED
C       TO REMOVE ANY DATA STHAT WAS STRORED IN THE VARIOUS TABLES.
C       SET SKPFLG=0 TO SAY THE END BLOCK DATA RECORD HASN'T BEEN
C       READ YET.
C
3000    CONTINUE
        SKPFLG=0
        IF (LIBFLG .NE. 1 .OR. LIBFG2 .NE. 0) GOTO 3200
3010    CONTINUE
        CALL RMVET (PRGDTA,INDXPG,1)
        CALL RMVET (ENTDTA,ENTPTX,-1)
        ENTPT1=ENPTX1
        CALL RMVET (EXTDTA,EXTPX2,-1)
        EX1PTR=EXTPX1
C
C       READ THE END BLOCK DATA RECORD UNLESS IT ALREADY HAS BEEN
C       (SKPFLG=1).
C
        IF (SKPFLG .NE. 0) GOTO 50
        IF (RDREC (OLUN,STR,IPTR)+1) 90040,90000,50
C
C
C       UPDATE PSBRK,
C       UPDATE THE NUMBER (COUNT) OF ROUTINES LOADED, AND SKIP
C       THE END BLOCK DETAIL RECORD.  IF LOADING FOR A LIB COMMAND
C       (LBFLG .NE. 0) SEE IF THERE ARE ANY UNSATISFIED EXTERNALS LEFT.
C       IF NOT, THEN WERE DONE.
C
3200    PSBRK=IADD16 (PSBRK,RTBRK)
        PRGDAT(9)=LOCPTR
        COUNT=COUNT+1
        IF (RDREC (OLUN,STR,IPTR)+1) 90060,90000,3220
3220    CONTINUE
        IF (FCLFG2 .EQ. 0) GOTO 3350
        IF (SRCFLG .NE. 0) GOTO 3300
        CALL SRC1 (PRGDAT(4),PRGDAT(8),IDXFCL,STR,HASIMD,CODFLG)
        IF (HASIMD .EQ. 2) HASIMD=HASIMD+CODFLG
3300    CONTINUE
        CALL SRCN (PRGDAT(4),PRGDAT(8),IDXFCL,HASIMD,LOCPTR)
3350    CONTINUE
        WRITE (CLUNX,1030)
C1030   FORMAT (' $',I5)
C
C       PACK UP THE IMPORTANT STUFF FROM PRGDAT AND PLACE IT IN PRGDTA
C
        PRGDAT(1)=PRGDAT(5)
        PRGDAT(2)=IOR16 (ILSH16 (PRGDAT(4),8),PRGDAT(3))
        CALL RPLVT (PRGDTA,-1,1,PRGDAT,2)
        IF (LBFLG .EQ. 0) GOTO 50
        IF (EXTCNT (1,1) .NE. 0) GOTO 50
        RETURN
C
C------- TITLE BLOCK
C
C       CHECK FOR DOUBLE TITLE BLOCK
C       SET IORDR FLG, MAKE ENTRY IN PRGDTA() FOR THIS ROUTINE AND PLACE
C       THE TITLE THERE.  RESET THE LOCAL DATA BLOCK POINTER.
C
4000    CONTINUE
        IF (RDREC (OLUN,STR,IPTR)+1) 90060,90000,4020
4020    CONTINUE
        IF (IORDR .NE. 0) GOTO 90160
        IF (EXTTOK (SYM,6,STR,1,ID,10) .NE. -1) GOTO 90020
        CALL PAKS (SYM,SYM,6)
        IF (SRCST (PRGDTA,1,-1,SYM,6) .EQ. 0) GOTO 4050
        CALL SKPSUB (STR,IPTR,SYM,0)
        GOTO 100
4050    IORDR=1
        IF (PRGDTA(1)+1 .GT. PRGMAX) GOTO 90100
        INDXPG=INSST (PRGDTA,-1,SYM,6)
        PARPTR=0
        PRGDAT(1)=EX1PTR+1
        PRGDAT(2)=0
        PRGDAT(3)=ENTPT1+1
        PRGDAT(4)=0
        PRGDAT(5)=PSBRK
        PRGDAT(6)=PARPTR+1
        PRGDAT(7)=0
        PRGDAT(8)=0
        PRGDAT(9)=0
        LOCPTR=0
        GOTO 100
C
C------- AENTRY BLOCK
C
C       PROCESSING IS DONE AS A REGULAR ENTRY
C
4380    CONTINUE
        GOTO 4410
C
C------- ENTRY BLOCK
C
C       GET RECCNT
C
4400    CONTINUE
        SBRPTR=-1
4410    IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        RECCNT =STOI (SYM,RADIX)
        IF (RECCNT .LE. 0) GOTO 90020
C
C       READ ENTRY RECORDS AND PLACE DATA IN ENTDTA().
C       RESET FTN CALLABLE FLAG.
C
        DO 4600 I=1,RECCNT
        FCLFLG=0
        IF (RDREC (OLUN,STR,IPTR)+1) 90060,90000,4430
4430    IF (EXTTOK (SYM,6,STR,IPTR,IPTR,10) .NE. -1) GOTO 90020
        CALL PAKS (SYM,SYM,6)
C
C       IF WE'RE PROCESSING A LIBRARY (LIBFLG=1) THEN SEE IF ANY OF
C       THE ENTRY PTS. HERE ARE IN EXTDTA().  IF YES, SET LIBFG2=1 WHICH
C       SEZ LOAD THIS LIB ROUTINE.
C
        IF (LIBFLG .EQ. 0 .OR. LIBFG2 .EQ. 1) GOTO 4460
        IF (SRCST (EXTDTA,1,-1,SYM,6) .NE. 0) LIBFG2=1
C
C       CHECK FOR DUPLICATE ENTRY OR NO-LOAD ENTRY
C
4460    INDX=SRCST (ENTDTA,1,-1,SYM,6)
        IF (INDX .EQ. 0) GOTO 4500
        ID=EXTVT (ENTDTA,INDX,1,IVAL,2)
        IF (IAND16 (IVAL(2),8) .NE. 0) GOTO 4480
        IF (LIBFLG .EQ. 0) GOTO 4495
4470    LIBFG2=0
        GOTO 4600
C
C       IF NO-LOAD SET AND THIS IS A LIBRARY SKIP THIS SYMBOL, ELSE SET
C       THE NOT-LOADED FLAG OFF.  THE NO-LOAD FLAG IS SET OFF ALSO
C       (FOR A NON-LIBRARY).
C
4480    IF (IAND16 (IVAL(2),32) .EQ. 32 .AND. LIBFLG .EQ. 1) GOTO 4470
        IVAL(2)=IAND16 (IVAL(2),IP16 (-41))
        ENTPT1=ENTPT1+1
        IF (ENTPT1 .GT. ENTMX1) GOTO 90120
        IF (IAND16 (IVAL(2),16) .EQ. 0) GOTO 4540
        FCLFLG=1
        FCLFG2=1
        IDXFCL=INDX
        GOTO 4540
C
C       WARNING - DUPLICATE ENTRY
C
4495    CALL ERRMES (8)
        CALL WRTLIN (STR,-1,-1)
        GOTO 4600
4500    IVAL(2)=0
        ENTPT1=ENTPT1+1
        IF (ENTPT1 .GT. ENTMX1) GOTO 90120
        IF (ENTDTA(1)+1 .GT. ENTMAX) GOTO 90120
        INDX=INSST (ENTDTA,-1,SYM,6)
4540    ENTDT1(ENTPT1)=INDX
        IF (SBRPTR .NE. -1) GOTO 4545
        SBRPTR=INDX
        IF (LOCPTR .EQ. 0) GOTO 4545
        CALL UPAKS (SYM,SYM,6)
        CALL INSCS (SYM,1,8238)
        ID=RPLST (DBDTA1,LOCPTR,SYM)
C
C       IF ENTFLG=0, SET IT, AND SET THE ENTRY POINTER IN PRGDTA() TO
C       ENTPT1.
C
4545    IF (ENTFLG .NE. 0) GOTO 4550
        ENTFLG=1
        PRGDAT(1)=ENTPT1
C
C       GET VALUE AND TYPE AND SAVE THEM TOO. ACTUAL ENTRY PT. IS THE REL.
C       ADDRESS + PSBRK UNLESS ITS ABSOLUTE.
C
4550    IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        IVAL(1)=STOI (SYM,RADIX)
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        IV=STOI (SYM,RADIX)
        IVAL(2)=IVAL(2)+IV
        IF (IV .NE. 0) IVAL(1)=IADD16 (IVAL(1),PSBRK)
C
C       IF FCLFLG=1 ENTRY TYPE MUST BE RELOCATABLE (TYPE=1 OR 2).  UPDATE
C       THE ENTDTA() VALUES.
C
        IF (FCLFLG .EQ. 0 .OR. IV .EQ. 1 .OR. IV .EQ. 2) GOTO 4570
C
C       ENTRY POINT DECLARED CALLABLE IS NOT RELOCATABLE
C
        CALL ERRMES (18)
        FCLFLG=0
        IVAL(2)=IAND16 (IVAL(2),IP16 (-17))
        GOTO 4590
C
C       GET THE TRAILING PARAMETER - IF TYPE=2 THIS IS AN S-PAD PARAMETER
C       OTHERWISE ITS THE NUMBER OF PARAMETERS FOR A FTN ENTRY POINT.
C       THIS WON'T BE PRESENT IF THE SYMBOL IS ABSOLUTE (IV=0)
C
4570    IF (IV .EQ. 0) GOTO 4590
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        IV=STOI (SYM,RADIX)
        IVAL(2)=IOR16 (ILSH16 (IV,8),IVAL(2))
C
C       UPDATE THE ENTDTA().
C
4590    CALL RPLVT (ENTDTA,INDX,1,IVAL,2)
        IF (FCLFLG .NE. 0) HASIMD=IAND16 (IVAL(2),7)
4600    CONTINUE
C
C       UPDATE THE END ENTRY POINTER IN PRGDTA()
C
        PRGDAT(2)=ENTPT1
        GOTO 100
C
C------- EXTERNAL BLOCK
C
C       GET RECORD COUNT
C
5000    CONTINUE
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        RECCNT=STOI (SYM,RADIX)
C
C       READ THE DETAIL RECORDS. SAVE THE EXTERNAL SYMBOLS AND THE ADDRESS OF
C       REFERENCING ROUTINE IN PRGDTA() AND PTRS. TO THEM IN EXTDT1().
C
        IF (RECCNT .LE. 0) GOTO 90020
        DO 5100 I=1,RECCNT
        IF (RDREC (OLUN,STR,IPTR)+1) 90060,90000,5050
5050    IF (EXTTOK (SYM,6,STR,IPTR,IPTR,10) .NE. -1) GOTO 90020
        CALL PAKS (SYM,SYM,6)
C
C       SEE IF SYMBOL IS ALREADY IN EXTDTA()
C
        INDX=SRCST (EXTDTA,1,-1,SYM,6)
        IF (INDX .NE. 0) GOTO 5060
        IF (EXTDTA(1)+1 .GT. EXTMAX) GOTO 90140
        INDX=INSST (EXTDTA,-1,SYM,6)
        CALL RPLVT (EXTDTA,-1,1,PSBRK,1)
5060    EX1PTR=EX1PTR+1
        IF (EX1PTR .GT. EXTMX1) GOTO 90140
        EXTDT1(EX1PTR)=INDX
C
C       IF EXTFLG=0 SET THE EXTERNAL PTR. IN PRGDTA() TO THE INDEX OF
C       THIS FIRST EXTERNAL PTR. IN EXTDT1(). SET THE FLAG.
C
        IF (EXTFLG .NE. 0) GOTO 5100
        PRGDAT(3)=EX1PTR
        EXTFLG=1
5100    CONTINUE
        GOTO 100
C
C------- LIBRARY START BLOCK (6)
C
C       JUST SET LIB FLAG, THIS IS OUT OF ORDER IF IORDR .NE. 0.
C
5500    CONTINUE
        IF (IORDR .NE. 0) GOTO 90080
        IF (LBFLG .NE. 0) LBFLG=2
        IF (LFRCFG .NE. 1) LIBFLG=1
        GOTO 100
C
C------- LIBRARY END BLOCK (7)
C
C       RESET LIB FLAG, THIS IS OUT OF ORDER IF IORDR .NE. 0.
C
5600    IF (IORDR .NE. 0) GOTO 90080
        LIBFLG=0
        IF (LBFLG .NE. 0) LBFLG=1
        GOTO 100
C
C------- DATA BLOCK DESCRIPTOR BLOCK
C
C       GET RECORD COUNT AND DATA BLOCK NAME.  IF THE NAME IS .LOCAL IT IS
C       REPLACED BY THE CURRENT ROUTINES TITLE FOLLOWED BY A DECIMAL.
C       ALSO PICK UP THE MODIFICATION PARAMETER.
C
6000    CONTINUE
C
C       IF WE'RE LOADING FROM A LIBRARY AND WE HAVEN'T DECIDED
C       TO LOAD THIS ROUTINE YET THEN IT'S TIME TO DUMP THE REST
C       OF IT
C
        IF (LIBFLG .NE. 1 .OR. LIBFG2 .NE. 0) GOTO 6010
        SKPFLG=1
        CALL SKPSUB (STR,IPTR,SYM,1)
        GOTO 3010
6010    IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        RECCNT=STOI (SYM,RADIX)
        IF (RECCNT .LE. 0) GOTO 90020
        ICHAR=EXTTOK (SYM,6,STR,IPTR,IPTR,10)
        IF (ICHAR .NE. 8238) GOTO 6030
        ICHAR=EXTTOK (SYM,6,STR,IPTR,IPTR,10)
        IF (ICHAR .NE. -1) GOTO 90020
        IF (CMPSS (SYM,SLOCAL,-1) .NE. 0) GOTO 6020
        IF (SBRPTR .NE. 0) ID=EXTST (ENTDTA,SBRPTR,SYM,6)
        IF (SBRPTR .EQ. 0) ID=EXTST (PRGDTA,PRGDTA(1),SYM,6)
        CALL UPAKS (SYM,SYM,6)
        CALL INSCS (SYM,0,8238)
        ICHAR=(8238)
        GOTO 6040
6020   CALL INSCS (SYM,0,8238)
6030    IF (ICHAR .NE. -1) GOTO 90020
        IF (EXTTOK (STR2,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        MODDD=STOI (STR2,RADIX)
C
C       IF THIS IS ALREADY IN DBDTA1() PROCESS FARTHER BELOW.
C
6040    INDX=SRCST (DBDTA1,1,-1,SYM,7)
        RSTFLG=0
        IF (INDX .NE. 0 .AND. MODDD .EQ. 0) GOTO 6500
        IF (INDX .EQ. 0) GOTO 6050
        IF (IAND16 (EXTVT (DBDTA1,INDX,2,IVAL,1),16383) .NE. 0)
     *     GOTO 6500
        RSTFLG=1
        GOTO 6070
C
C       PLACE THE DB NAME IN DBDTA1(), RESET LENGTH COUNT, SAVE PTR TO FIRST
C       EMPTY DBDTA2() SLOT.  IF ICHAR="." THEN WE HAVE A LOCAL DATA BLOCK
C       AND WANT TO SAVE A GLOBAL POINTER TO IT.
C
6050    IF (DBDTA1(1)+1 .GT. DB1MAX) GOTO 90220
        INDX=INSST (DBDTA1,-1,SYM,7)
6070    ILEN=0
        IPTRX=DB2PTR+1
        IF (ICHAR .EQ. 8238) LOCPTR=INDX
C
C       POINT NEXT AVAILABLE DBDTA0 ENTRY AT THIS DB'S DBDTA1() ENTRY.
C       SET DB END POINTER IN PRGDTA().
C       IF DBFLG=0 THEN THIS IS ROUTINES FIRST DB, SO SET DB PTR IN
C       PRGDTA(), SET FLAG.
C
        DB0PTR=DB0PTR+1
        IF (DB0PTR .GT. DB0MAX) GOTO 90220
        DBDTA0(DB0PTR)=INDX
        PRGDAT(8)=DB0PTR
        IF (DBFLG .NE. 0) GOTO 6180
        PRGDAT(4)=DB0PTR
        DBFLG=1
C
C       READ DBDB RECORDS, GET VALTYP AND LENGTH FOR EACH ELEMENT GROUP IN
C       THE DB AND SAVE THEM IN DBDTA2().  UPDATE LENGTH SO FAR (ILEN)
C
6180    DO 6300 I=1,RECCNT
        IF (RDREC (OLUN,STR,IPTR)+1) 90060,90000,6200
6200    IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        IVAL(1)=STOI (SYM,RADIX)
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        IVAL(2)=STOI (SYM,RADIX)
        ILEN=IADD16 (ILEN,IVAL(2))
        IF (MODDD .EQ. 0) GOTO 6300
        DB2PTR=DB2PTR+1
        IF (DB2PTR .GT. DB2MAX) GOTO 90220
        DBDTA2(DB2PTR,1)=IVAL(1)
        DBDTA2(DB2PTR,2)=IVAL(2)
6300    CONTINUE
C
C       SAVE DB ADDRESS, POINTER TO ITEMS IN DBDTA2(), AND DB LENGTH
C       IN DBDTA1().
C
        IVAL(1)=DBBRK
        IF (MODDD .EQ. 0) IPTRX=0
        IVAL(2)=IOR16 (ILSH16 (MODDD,14),IPTRX)
        IVAL(3)=ILEN
        IF (RSTFLG .EQ. 0) GOTO 6350
        IF (EXTVT (DBDTA1,INDX,3,ID,1) .NE. ILEN) GOTO 90200
        GO TO 100
6350    CALL RPLVT (DBDTA1,-1,1,IVAL,3)
C
C       UPDATE DBBRK
C
        IF (RSTFLG .EQ. 0) DBBRK=IADD16 (DBBRK,ILEN)
        GOTO 100
C
C       UPDATE MODDD IN PRGDTA
C
6500    CONTINUE
        IV=IAND16 (EXTVT (DBDTA1,INDX,2,ID,1),16383)
        IV=IOR16 (ILSH16 (MODDD,14),IV)
        CALL RPLVT (DBDTA1,INDX,2,IV,1)
C
C       HERE WE HAVE A DB THAT WAS SEEN EARLIER.  POINT THE NEXT
C       AVAILABLE DBDTA0 ENTRY AT THE DB'S DBDTA1 ENTRY.  IF THE DB
C       POINTER IN PRGDTA IS 0, POINT IT AT THE NEW DBDTA0 ENTRY.
C
        DB0PTR=DB0PTR+1
        IF (DB0PTR .GT. DB0MAX) GOTO 90220
        DBDTA0(DB0PTR)=INDX
        IF (DBFLG .NE. 0) GOTO 6530
        PRGDAT(4)=DB0PTR
        DBFLG=1
C
C       NOW SEE THAT THIS DB IS IDENTICAL TO THE ONE SEEN BEFORE.
C       RESET LENGTH COUNT AND GET THIS BLOCKS DBDTA2() PTR.
C
6530    ILEN=0
        IPTRX=IAND16 (EXTVT (DBDTA1,INDX,2,IVAL,2),16383)
        DO 6700 I=1,RECCNT
        IF (RDREC (OLUN,STR,IPTR)+1) 90060,90000,6600
C
C       GET VALTYP AND LEN FROM THE RECORD
C
6600    IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        IVAL2(1)=STOI (SYM,RADIX)
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        IVAL2(2)=STOI (SYM,RADIX)
C
C       SEE IF THEY MATCH THE VALUES IN DBDTA2(), UPDATE LEN COUNT
C
        IF (MODDD .EQ. 0) GOTO 6680
        IF (IVAL2(1) .NE. DBDTA2(IPTRX,1) .OR.
     *      IVAL2(2) .NE. DBDTA2(IPTRX,2)) GOTO 90200
        IPTRX=IPTRX+1
6680    ILEN=IADD16 (ILEN,IVAL2(2))
6700    CONTINUE
C
C       NEW LENGTH COUNT MUST MATCH THE OLD.  UPDATE DB END
C       POINTER IN PRGDTA().
C
        IF (ILEN .NE. IVAL(2)) GOTO 90200
        PRGDAT(8)=DB0PTR
        GOTO 100
C
C------- DATA BLOCK INITIALIZATION BLOCK
C
C       IGNORE DBIB'S DURING THIS PASS - IF THE CURRENT ROUTINE IS
C       HOST CALLABLE THEN WE LEAVE DBIB'S TO BE READ BY SRC2
C       OTHERWISE THEY GO STRAIGHT TO DBLUN
C       GET RECORD COUNT AND SKIP DETAIL RECORDS.
C
7000    CONTINUE
        WRITE (DBLUNX,1030) INDXPG
C1030   FORMAT (' $',I5)
        IF (FCLFG2 .EQ. 0) GOTO 7080
        IF (SRCFLG .NE. 0) GOTO 7050
        CALL SRC1 (PRGDAT(4),PRGDAT(8),IDXFCL,STR,HASIMD,CODFLG)
        IF (HASIMD .EQ. 2) HASIMD=HASIMD+CODFLG
        SRCFLG=1
7050    CONTINUE
        CALL SRC2 (STR,IPTR,PRGDAT(4),LOCPTR)
        GOTO 100
7080    IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        RECCNT=STOI (SYM,RADIX)
        CALL WRTLIN (STR,DBLUN,-1)
        DO 7200 I=1,RECCNT
        IF (RDREC (OLUN,STR,IPTR)+1) 90060,90000,7100
7100    CALL WRTLIN (STR,DBLUN,-1)
7200    CONTINUE
        GOTO 100
C
C------- PARAMETER BLOCK
C
C       GET THE RECORD COUNT, SAVE PTR IN PRGDTA() TO THE NEXT PARAMETER
C       SLOT IN PARDTA(), SET PARFLG, AND READ PARAMETER DESCRIPTION RECORDS
C       SAVING THE INFO IN PARDTA().
C
8000    CONTINUE
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        RECCNT=STOI (SYM,RADIX)
        IF (RECCNT .LE. 0) GOTO 90020
        PRGDAT(6)=PARPTR+1
        PARFLG=1
        PARCNT=0
        DO 8100 I=1,RECCNT
        IF (RDREC (OLUN,STR,IPTR)+1) 90060,90000,8050
C
C       GET TYPE, MOD, SIZE FROM RECORD AND SAVE IN PARDTA().
C       CHECK COUNT OF THIS ROUTINE'S PARAMETERS.  MAX IS 32.
C
8050    PARPTR=PARPTR+1
        IF (PARPTR .GT. PARMAX) GOTO 90240
        PARCNT=PARCNT+1
        IF (PARCNT .GT. 32) GOTO 90240
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        IV=STOI (SYM,RADIX)
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        IV=STOI (SYM,RADIX)*16+IV
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        ILEN=STOI (SYM,RADIX)
        PARDTA(PARPTR)=IOR16 (ILSH16 (ILEN,8),IV)
C
C       READ DETAIL RECORD FOR EACH DIMENTION, GET STATIC/DYNAMIC
C       SPECIFIER, AND SIZE/ARGPTR.
C
        IF (ILEN .EQ. 0) GOTO 8100
        DO 8090 J=1,ILEN
        IF (RDREC (OLUN,STR,IPTR)+1) 90060,90000,8070
8070    IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        IV=STOI (SYM,RADIX)
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        IV=IOR16 (ILSH16 (STOI (SYM,RADIX),1),IV)
        PARPTR=PARPTR+1
        IF (PARPTR .GT. PARMAX) GOTO 90240
        PARDTA(PARPTR)=IV
8090    CONTINUE
8100    CONTINUE
        GOTO 100
C
C------- TASK BLOCK (12)
C
9000    CONTINUE
        IF (RDREC(OLUN,STR,IPTR)+1) 90060,90000,9010
9010    CONTINUE
        CALL TASKY(STR,IPTR,1,RADIX)
        GO TO 100
C
C------- ISR BLOCK (13)
C
10000   CONTINUE
        IF (ISRFL) GO TO 90320
        IF (TSKFL) GO TO 90340
        IF (EXTTOK(SYM,2,STR,IPTR,IPTR,RADIX).NE.-2) GO TO 90020
        ID=SRCST(DBDTA1,-1,-1,ISRMAP,-1)
C LOOK FOR ISRMAP
        IF (ID.EQ.0) GO TO 90280
        ID=EXTVT(DBDTA1,ID,3,ID,1)
C LENGTH BETTER BE ISRLEN
        IF (ID.NE.ISRLEN) GO TO 90300
        ISRMDA=DBBRK
C MDADDR OF ISR
        ISRPSA=PSBRK
C PSADDR OF ISR
        ISRIDX=STOI(SYM,RADIX)
C GET ISR INDEX #
        ISRFL=.TRUE.
C SET ISR FLAG
        CALL RPLIS (STRTRE,3,ISRIDX,2,RADIX,8224)
        IPTR=1
        CALL TREE(STRTRE,IPTR,RADIX,0,SYM)
C PRETENT THERE WAS A TREE
        CALL RPLIS (STROV,1,ISRIDX,2,RADIX,8224)
        IXX=-1
        RETURN
C
C    WE RETURN TO LOAD TO CALL OVLY THEN COME BACK TO LOAD1 AND CONTINUE.
C    (FROM LABEL 100).  THIS IS FOR OVERLAY CONSIDERATIONS.  THE COMMON
C    BLOCK /GLOP/ IS FOR THIS.
C
C
C------- ERROR RETURNS
C
C       READ ERROR
C
90000   CALL ERRMES (1)
        GOTO 99910
C
C       BAD LOADER RECORD
C
90020   CALL ERRMES (2)
        GOTO 99900
C
C       CODE OVERLAP
C
90040   CALL ERRMES (3)
        GOTO 99900
C
C       UNEXPECTED EOF
C
90060   CALL ERRMES (4)
        GOTO 99910
C
C       LOADER BLOCKS OUT OF ORDER
C
90080   CALL ERRMES (5)
        GOTO 99900
C
C       TOO MANY ROUTINES
C
90100   CALL ERRMES (6)
        GOTO 99900
C
C       TOO MANY ENTRIES
C
90120   CALL ERRMES (7)
        GOTO 99900
C
C       TOO MANY EXTERNALS
C
90140   CALL ERRMES (9)
        GOTO 99900
C
C       DOUBLE TITLE BLOCK
C
90160   CALL ERRMES (13)
        GOTO 99900
C
C       MISSING END BLOCK
C
90180   CALL ERRMES (14)
        GOTO 99910
C
C       UNMATCHED COMMON BLOCK
C
90200   CALL ERRMES (15)
        GOTO 99910
C
C       TOO MUCH COMMON BLOCK DATA
C
90220   CALL ERRMES (16)
        GOTO 99900
C
C       TOO MANY PARAMETERS IN CALLED ROUTINES
C
90240   CALL ERRMES (19)
        GOTO 99900
C
C       NOT A LIBRARY
C
90260   CALL ERRMES (25)
        GOTO 99910
C
C       ISRMAP IS MISSING
C
90280   CONTINUE
        CALL ERRMES(36)
        GO TO 99900
C
C       ISRMAP IS WRONG SIZE
C
90300   CONTINUE
        CALL ERRMES(37)
        GO TO 99900
C
C       MULTIPLE ISR BLOCKS
C
90320   CONTINUE
        CALL ERRMES(40)
        GO TO 99900
C
C       BOTH TASK AND ISR NOT ALLOWED
C
90340   CONTINUE
        CALL ERRMES(42)
        GO TO 99900
C
C       WRITE THE ERRONEOUS LINE
C
99900   CALL WRTLIN (STR,-1,80)
C
C       SET ERROR FLAG AND RETURN
C
99910   IFLG=1
        RETURN
        END
C+++ LOADMP
C****** LOADMP = OUTPUT LOADER MAP = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE LOADMP
C
C
        INTEGER TLUN,IVAL(3),II1,TFILE(31)
        INTEGER OPALL,OPOV,OPDB,OPENT,OPSYM
        INTEGER LUN
        INTEGER OPTSK
        INTEGER J1,OPTION
        INTEGER EXTST
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C                                                                       MAINCOM
C  THE FIRST TWO LINES OF DECLARATION ARE NOT COMMON VARIABLES          MAINCOM
C  BUT INTEGER FUNCTIONS THAT MAY BE CALLED.                            MAINCOM
        INTEGER RDREC,EXTTOK,SRCST,EXTSL,RPLCS,EXTVT,INSST,EXTSS        MAINCOM
        INTEGER SRCSL,EXTCNT,LENT,SRCCS,STOI,IADDC,COMAND,CMPSS         MAINCOM
C                                                                       MAINCOM
        INTEGER ILUN,STR,IPTR,SYM,INDX                                  MAINCOM
        INTEGER IFLG,IV,OVSKIP,IV2,TREFLG                               MAINCOM
        INTEGER I,II,J,JJ,ID,OVMESZ,SFILE1                              MAINCOM
        INTEGER OVEPT,SYM2,LFFLG                                        MAINCOM
        INTEGER LIBFLG,COUNT,IRADIX,SFILE,IPTLUN                        MAINCOM
        INTEGER BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG                    MAINCOM
        INTEGER RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD                      MAINCOM
        INTEGER APOVEN,OVLDEN,PSCNT,IZ1QS                               MAINCOM
        COMMON /MCOMM/ ILUN,STR(81),IPTR,SYM(8),INDX,                   MAINCOM
     *   IFLG,IV,OVSKIP,IV2,TREFLG,                                     MAINCOM
     *   I,II,J,JJ,ID,OVMESZ,SFILE1(16),                                MAINCOM
     *   OVEPT,SYM2(8),LFFLG,                                           MAINCOM
     *   LIBFLG,COUNT,IRADIX,SFILE(31),IPTLUN,                          MAINCOM
     *   BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG,                          MAINCOM
     *   RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD,                            MAINCOM
     *   APOVEN,OVLDEN,PSCNT,IZ1QS                                      MAINCOM
C                                                                       MAINCOM
C                                                                       COMMON10
       INTEGER TSKDTA(26,8),TSKPTR,TSKMAX,TCBMNL,TCBMXL                 COMMON10
       INTEGER SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,SPARPT                COMMON10
       INTEGER SDB0PT,SDB1PT,SDB2PT,PSTOP                               COMMON10
       INTEGER ISRMDA,ISRPSA,ISRIDX,ISRLEN                              COMMON10
       INTEGER OVPDTA(50,3),PSPDTA(50),OVPPTR,PSPPTR,PSPMAX,OVPMAX      COMMON10
       LOGICAL TASKFL,TSKFL,ISRFL,IOPENX                                COMMON10
       COMMON /TSKCOM/ TSKDTA,TSKPTR,TSKMAX,TCBMNL,TCBMXL,              COMMON10
     -         SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,                      COMMON10
     -         SPARPT,SDB0PT,SDB1PT,SDB2PT,PSTOP,                       COMMON10
     -         ISRMDA,ISRPSA,ISRIDX,ISRLEN,                             COMMON10
     -         OVPDTA,PSPDTA,OVPPTR,PSPPTR,PSPMAX,OVPMAX,               COMMON10
     -         TASKFL,TSKFL,ISRFL                                       COMMON10
       COMMON /OPENX/ IOPENX                                            COMMON10
C                                                                       COMMON10
C
C                                                                       STARTPRE
      INTEGER SPSLOW(8),SPSBRK(8),SDBBRK(8),SOVNUM(8)
C STRNG SPSLOW "PSLOW  "
      DATA SPSLOW(2)/'P '/,SPSLOW(3)/'S '/,SPSLOW(4)/'L '/
      DATA SPSLOW(5)/'O '/,SPSLOW(6)/'W '/,SPSLOW(7)/'  '/
      DATA SPSLOW(8)/'  '/
      DATA SPSLOW(1)/7/
C STRNG SPSBRK "PSBRK  "
      DATA SPSBRK(2)/'P '/,SPSBRK(3)/'S '/,SPSBRK(4)/'B '/
      DATA SPSBRK(5)/'R '/,SPSBRK(6)/'K '/,SPSBRK(7)/'  '/
      DATA SPSBRK(8)/'  '/
      DATA SPSBRK(1)/7/
C STRNG SDBBRK "DBBRK  "
      DATA SDBBRK(2)/'D '/,SDBBRK(3)/'B '/,SDBBRK(4)/'B '/
      DATA SDBBRK(5)/'R '/,SDBBRK(6)/'K '/,SDBBRK(7)/'  '/
      DATA SDBBRK(8)/'  '/
      DATA SDBBRK(1)/7/
C STRNG SOVNUM ".ONNNNN"
      DATA SOVNUM(2)/'. '/,SOVNUM(3)/'O '/,SOVNUM(4)/'N '/
      DATA SOVNUM(5)/'N '/,SOVNUM(6)/'N '/,SOVNUM(7)/'N '/
      DATA SOVNUM(8)/'N '/
      DATA SOVNUM(1)/7/
C                                                                       ENDPRE
C
C       INITIALIZE ...
C
        DATA OPALL, OPOV, OPDB, OPENT, OPSYM / 0, 2, 1, 4, 3 /
        DATA OPTSK/5/
C
        IFLG=0
        TLUN=-1
        LUN=ITTO
        OPTION=OPALL
C
C       PROCESS PARAMETERS:  OPTION AND FILE NAME
C
C       GET OPTION (0-4), DEFAULT IS 0.
C       ALLOW OPTION = 5
C
        II=EXTTOK (SYM,6,STR,IPTR,IPTR,10)
        IF (II .EQ. 0) GOTO 2300
        IF (II .NE. -2) GOTO 90020
        OPTION=STOI (SYM,10)
        IF (OPTION.LT.0.OR.OPTION.GT.5) GO TO 90020
C
C       GET FILE NAME - DEFAULT IS TTY.
C
        CALL SKPBLK (STR,IPTR)
        IF (IPTR .GT. STR(1)) GOTO 2300
        IZ1QS = EXTSS (STR,IPTR,TFILE,30)
        TLUN=MAPLUN
        LUN=TLUN+LUNMAP
        IF (INFILE (2,TFILE,TLUN) .NE. 0) GOTO 90000
C
C       OUTPUT OVERLAY MAP.
C
2300    IF (OVPTR .EQ. 0 .OR.
     *      (OPTION .NE. OPALL .AND. OPTION .NE. OPOV)) GOTO 2325
        WRITE (LUN,2310)
2310    FORMAT(/'  OVERLAY MAP'/
     *         /1X,'     ID  PG  MDADDR     LEN  PSADDR',
     *          '  LEV BRL BRR')
        CALL RMVCS (STR,1,-1)
        DO 2320 I=1,OVPTR
        CALL RPLIS (STR,2,OVDTA(I,1),6,LRADIX,8224)
       CALL RPLIS (STR,10,IAND16 (OVDTA(I,6),15),2,LRADIX,8224)
        CALL RPLIS (STR,14,OVDTA(I,3),6,LRADIX,8224)
        CALL RPLIS (STR,22,OVDTA(I,4),6,LRADIX,8224)
        CALL RPLIS (STR,30,OVDTA(I,2),6,LRADIX,8224)
        CALL RPLIS (STR,38,IRSH16 (OVDTA(I,6),4),3,LRADIX,8224)
        CALL RPLIS (STR,42,IAND16 (OVDTA(I,5),255),3,LRADIX,8224)
        CALL RPLIS (STR,45,IRSH16 (OVDTA(I,5),8),3,LRADIX,8224)
        CALL WRTLIN (STR,TLUN,-1)
2320    CONTINUE
2325    CONTINUE
C
C       OUTPUT THE TASK MAP
C
        IF (TSKPTR.EQ.1.OR.
     -  (OPTION.NE.OPALL.AND.OPTION.NE.OPTSK)) GO TO 2330
        WRITE (LUN,2326)
2326    FORMAT(/'  TASK MAP'/
     *         /1X,'     ID  PRI  PSADDR     LEN  OPT',
     *          ' RLINK LLINK')
        DO 2328 I=2,TSKPTR,1
        CALL RMVCS(STR,1,-1)
        CALL RPLIS(STR,2,TSKDTA(I,1),6,LRADIX,8224)
        J=TSKDTA(I,2)
C GET PRIORITY
        IF (J.GT.255) J=J-255
        CALL RPLIS(STR,10,J,3,LRADIX,8224)
        CALL RPLIS(STR,15,TSKDTA(I,4),6,LRADIX,8224)
        CALL RPLIS(STR,23,TSKDTA(I,5),6,LRADIX,8224)
        IF (IAND16(TSKDTA(I,3),1).NE.0)
     -  CALL INSCS(STR,31,8269)
C M OPTION
        IF (IAND16(TSKDTA(I,3),2).NE.0)
     -  CALL INSCS(STR,32,8265)
C I OPTION
        IF (IAND16(TSKDTA(I,3),4).NE.0)
     -  CALL INSCS(STR,33,8275)
C S OPTION
        J=TSKDTA(I,6)
        IF (J.NE.0) J=TSKDTA(J,1)
C GET TASK ID
        CALL RPLIS(STR,35,J,5,LRADIX,8224)
        J=TSKDTA(I,7)
        IF (J.NE.0) J=TSKDTA(J,1)
C GET TASK ID
        CALL RPLIS(STR,41,J,5,LRADIX,8224)
        CALL WRTLIN(STR,TLUN,-1)
2328    CONTINUE
C
C       DB MAP
C
C       OUTPUT DB INFO: DB NAMES AND LOAD LOCS , AND INTERSPERSED OVERLAYS
C
2330    IF (OPTION .NE. OPALL .AND. OPTION .NE. OPDB) GOTO 2480
        J=LENT (DBDTA1)
        II=DBPG-1
        WRITE (LUN,2335) II
2335    FORMAT(/'  DB MAP ',I2/)
        II=0
        IF (OVPTR .EQ. 0) GOTO 2350
2340    II=II+1
        IF (II .GT. CUROV) GOTO 2345
        IF (IAND16 (OVDTA(II,6),15) .NE. DBPG) GOTO 2340
        GOTO 2350
2345    II=0
2350    IF (J .EQ. 0) GOTO 2390
        DO 2370 I=1,J
        ID=EXTST (DBDTA1,I,SYM,7)
        IVAL(1)=EXTVT (DBDTA1,I,1,IVAL,1)
C
C       SEE IF WE POSSIBLY HAVE AN OVERLAY BETWEEN THE DATA BLOCKS
C
        IF (II .EQ. 0) GOTO 2365
        IF (ICMP16 (IVAL(1),OVDTA(II,3)) .EQ. -1) GOTO 2365
C
C       CREATE THE OVERLAY NAME (.O#####) AND PLACE IT IN MAP
C
        CALL RPLIS (SOVNUM,3,OVDTA(II,1),5,10,8240)
        CALL MAPLIN (SOVNUM,OVDTA(II,3),0,LRADIX,TLUN)
2355    II=II+1
        IF (II .GT. CUROV) GOTO 2360
        IF (IAND16 (OVDTA(II,6),15) .NE. DBPG) GOTO 2355
        GOTO 2365
2360    II=0
2365    CALL MAPLIN (SYM,IVAL(1),0,LRADIX,TLUN)
2370    CONTINUE
C
C       PLACE ANY REMAINING OVERLAYS IN THE DB PAGE IN THE MAP
C
2390    IF (II .EQ. 0) GOTO 2450
2400   CALL RPLIS (SOVNUM,3,OVDTA(II,1),5,10,8240)
        CALL MAPLIN (SOVNUM,OVDTA(II,3),0,LRADIX,TLUN)
2410    II=II+1
        IF (II .GT. CUROV) GOTO 2450
        IF (IAND16 (OVDTA(II,6),15) .NE. DBPG) GOTO 2410
        GOTO 2400
2450    II=DBBRK
        IF (OVID.LE.0) GO TO 2470
        IF (IAND16 (OVDTA(CUROV,6),15) .EQ. DBPG)
     *      II=IADD16 (II,OVDTA(CUROV,4))
2470    CONTINUE
        CALL MAPLIN (SDBBRK,II,0,LRADIX,TLUN)
        CALL MAPLIN (ID,ID,1,ID,TLUN)
C
C       PS MAP BY TITLE
C
C       ADD PSLOW AND PSBRK TO MAP
C
2480    IF (OPTION .NE. OPALL) GOTO 2600
        WRITE (LUN,2485)
2485    FORMAT(/'  PS TITLES'/)
        CALL MAPLIN (SPSLOW,PSLOW,0,LRADIX,TLUN)
C
C       OUTPUT PS INFO BY TITLE
C
        J=LENT (PRGDTA)
        IF (J .EQ. 0) GOTO 2500
        DO 2490 I=1,J
        ID=TABGET(PRGDTA,I,IVAL,SYM)
        CALL UPAKS (SYM,SYM,6)
        CALL MAPLIN (SYM,IVAL(1),0,LRADIX,TLUN)
2490    CONTINUE
2500    CALL MAPLIN (SPSBRK,PSBRK,0,LRADIX,TLUN)
        CALL MAPLIN (ID,ID,1,ID,TLUN)
        CALL WRTLIN (0,TLUN,-1)
C
C       PS MAP BY ENTRIES
C
C       WRITE HEADER AND PSLOW AND PSBRK
C
2600    IF (OPTION .NE. OPALL .AND. OPTION .NE. OPENT) GOTO 3100
        WRITE (LUN,2610)
2610    FORMAT(/'  PS ENTRIES'/)
        CALL MAPLIN (SPSLOW,PSLOW,0,LRADIX,TLUN)
C
C       ADD ENTRIES TO THE MAP
C
        IF (LENT (ENTDTA) .EQ. 0) GOTO 2750
        DO 2700 JJ=1,ENTPT1
        I=ENTDT1(JJ)
        ID=TABGET(ENTDTA,I,IVAL,SYM)
        CALL UPAKS (SYM,SYM,6)
C
C       DON'T MAP ABSOLUTES OR UNSATISFIED FORCE ENTRIES.  IF FTN CALLABLE
C       ADD * TO SYMBOL NAME.
C
        IF (IAND16 (IVAL(2),8) .NE. 0 .OR. IAND16 (IVAL(2),7) .EQ. 0)
     *      GOTO 2700
       IF(IAND16(IVAL(2),16) .NE. 0) CALL INSCS(SYM,0,8234)
        CALL MAPLIN (SYM,IVAL(1),0,LRADIX,TLUN)
2700    CONTINUE
2750    CALL MAPLIN (SPSBRK,PSBRK,0,LRADIX,TLUN)
        CALL MAPLIN (ID,ID,1,ID,TLUN)
        CALL WRTLIN (0,TLUN,1)
C
C       MAP ABSOLUTE SYMBOLS
C
        J=LENT (ENTDTA)
        IF (J .EQ. 0) GOTO 3100
        IFLG=0
        DO 2770 I=1,J
        ID=TABGET(ENTDTA,I,IVAL,SYM)
        IF (IAND16 (IVAL(2),15) .NE. 0) GOTO 2770
        CALL UPAKS (SYM,SYM,6)
        IF (IFLG .EQ. 1) GOTO 2760
        WRITE (LUN,2755)
2755    FORMAT(/'  ABSOLUTE SYMBOLS'/)
        IFLG=1
2760    CALL MAPLIN (SYM,IVAL(1),0,LRADIX,TLUN)
2770    CONTINUE
        CALL MAPLIN (ID,ID,1,ID,TLUN)
C
C       MAP UNDEFINED SYMBOLS
C
C       WRITE HEADER, ADD EXTERNALS THAT DON'T HAVE MATCHING ENTRIES TO THE MAP
C       THE MAPPED VALUE IS THE ADDRESS OF THE FIRST REFERENCING ROUTINE.
C
3100    IF (OPTION .NE. OPALL .AND. OPTION .NE. OPSYM) GOTO 5000
        IFLG=0
        J=LENT (ENTDTA)
        IF (J .EQ. 0) GOTO 3160
        DO 3140 I=1,J
        ID=TABGET(ENTDTA,I,IVAL,SYM)
        IF (IAND16 (IVAL(2),32) .EQ. 0) GOTO 3140
        CALL UPAKS (SYM,SYM,6)
        IF (IFLG .EQ. 1) GOTO 3120
        WRITE (LUN,3110)
3110    FORMAT(/'  NO LOAD SYMBOLS'/)
        IFLG=1
3120    CALL MAPLIN (SYM,IVAL(1),0,LRADIX,TLUN)
3140    CONTINUE
        CALL MAPLIN (ID,ID,1,ID,TLUN)
3160    J=LENT (EXTDTA)
        IF (J .EQ. 0) GOTO 5000
        IFLG=0
        DO 3200 I=1,J
        ID=TABGET(EXTDTA,I,IVAL,SYM)
        IF (SYM(2) .EQ. 0) GOTO 3200
        J1=SRCST (ENTDTA,1,-1,SYM,6)
        IF (J1 .EQ. 0) GOTO 3180
        J1=EXTVT (ENTDTA,J1,2,ID,1)
        IF (IAND16 (J1,40) .NE. 8) GOTO 3200
        IF (IAND16 (J1,16) .EQ. 16) CALL INSCS (SYM,0,8234)
3180    CONTINUE
        CALL UPAKS (SYM,SYM,6)
        IF (IFLG .EQ. 1) GOTO 3190
        WRITE (LUN,3185)
3185    FORMAT(/'  UNDEFINED SYMBOLS'/)
        IFLG=1
3190    CALL MAPLIN (SYM,IVAL(1),0,LRADIX,TLUN)
3200    CONTINUE
        CALL MAPLIN (ID,ID,1,ID,TLUN)
        CALL WRTLIN (0,TLUN,1)
C
C       UNASSIGN TLUN UNLESS IT'S TTY (-1)
C
5000    IF (TLUN .EQ. -1) RETURN
        IZ1QS = INFILE (4,STR,TLUN)
        RETURN
C
C       CAN NOT ASSIGN FILE
C
90000   CALL ERRMES (10)
        RETURN
C
C       BAD OR MISSING PARAMETER
C
90020   CALL ERRMES (12)
        RETURN
        END
C+++ MAPLIN
C****** MAPLIN = CREATE STYLIZED MAP LINE = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE MAPLIN (SYM,VAL,OP,LRADIX,LUN)
C
        INTEGER SYM(8),VAL,OP,LRADIX,LUN
C
        INTEGER STR(81),ID
        INTEGER RPLCS
        INTEGER BLANKS
        DATA STR(1) / 0 /
        DATA BLANKS / '  ' /
C
        IF (OP .NE. 1) GOTO 100
        IF (STR(1) .EQ. 0) RETURN
        CALL WRTLIN (STR,LUN,-1)
        CALL RMVCS (STR,1,-1)
        RETURN
100     IF (STR(1) .LT. 60) GOTO 200
        CALL WRTLIN (STR,LUN,-1)
        CALL RMVCS (STR,1,-1)
200    CALL INSCS (STR,STR(1)+4,8224)
        IF (SYM(1) .NE. 7) SYM(8)=BLANKS
        CALL INSSS (STR,-1,SYM,-1)
        CALL RPLIS (STR,STR(1)+3,VAL,6,LRADIX,8240)
        RETURN
        END
C+++    MARK
C****** MARK = MARK PERMANENT ITEMS = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE MARK
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C                                                                       COMMON10
       INTEGER TSKDTA(26,8),TSKPTR,TSKMAX,TCBMNL,TCBMXL                 COMMON10
       INTEGER SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,SPARPT                COMMON10
       INTEGER SDB0PT,SDB1PT,SDB2PT,PSTOP                               COMMON10
       INTEGER ISRMDA,ISRPSA,ISRIDX,ISRLEN                              COMMON10
       INTEGER OVPDTA(50,3),PSPDTA(50),OVPPTR,PSPPTR,PSPMAX,OVPMAX      COMMON10
       LOGICAL TASKFL,TSKFL,ISRFL,IOPENX                                COMMON10
       COMMON /TSKCOM/ TSKDTA,TSKPTR,TSKMAX,TCBMNL,TCBMXL,              COMMON10
     -         SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,                      COMMON10
     -         SPARPT,SDB0PT,SDB1PT,SDB2PT,PSTOP,                       COMMON10
     -         ISRMDA,ISRPSA,ISRIDX,ISRLEN,                             COMMON10
     -         OVPDTA,PSPDTA,OVPPTR,PSPPTR,PSPMAX,OVPMAX,               COMMON10
     -         TASKFL,TSKFL,ISRFL                                       COMMON10
       COMMON /OPENX/ IOPENX                                            COMMON10
C                                                                       COMMON10
C
C       ALL CURRENT POINTERS TO THE TABLES ARE SAVED,
C       THEREFORE MARKING THEM AS PERMANENT
C
        SPRGDT=LENT(PRGDTA)+1
        SENTPT=ENTPT1
      SENTDT=LENT(ENTDTA)+1
        SEXTPT=EX1PTR
      SEXTDT=LENT(EXTDTA)+1
        SPARPT=PARPTR
      SDB0PT=DB0PTR
        SDB1PT=LENT(DBDTA1)+1
        SDB2PT=DB2PTR
        RETURN
        END
C+++ MDOFF
C****** MDOFF = MDOFF COMMAND FOR APLOAD = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
         SUBROUTINE MDOFF
C------- MDOFF <ADDRESS> <PAGE>
C
C       GET THE ADDRESS AND PLACE IT IN THE PGINFO().  IF PAGE IS NOT
C       SPECIFIED IT DEFAULTS TO THE CURRENT DATA PAGE.
C
C                                                                       MAINCOM
C  THE FIRST TWO LINES OF DECLARATION ARE NOT COMMON VARIABLES          MAINCOM
C  BUT INTEGER FUNCTIONS THAT MAY BE CALLED.                            MAINCOM
        INTEGER RDREC,EXTTOK,SRCST,EXTSL,RPLCS,EXTVT,INSST,EXTSS        MAINCOM
        INTEGER SRCSL,EXTCNT,LENT,SRCCS,STOI,IADDC,COMAND,CMPSS         MAINCOM
C                                                                       MAINCOM
        INTEGER ILUN,STR,IPTR,SYM,INDX                                  MAINCOM
        INTEGER IFLG,IV,OVSKIP,IV2,TREFLG                               MAINCOM
        INTEGER I,II,J,JJ,ID,OVMESZ,SFILE1                              MAINCOM
        INTEGER OVEPT,SYM2,LFFLG                                        MAINCOM
        INTEGER LIBFLG,COUNT,IRADIX,SFILE,IPTLUN                        MAINCOM
        INTEGER BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG                    MAINCOM
        INTEGER RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD                      MAINCOM
        INTEGER APOVEN,OVLDEN,PSCNT,IZ1QS                               MAINCOM
        COMMON /MCOMM/ ILUN,STR(81),IPTR,SYM(8),INDX,                   MAINCOM
     *   IFLG,IV,OVSKIP,IV2,TREFLG,                                     MAINCOM
     *   I,II,J,JJ,ID,OVMESZ,SFILE1(16),                                MAINCOM
     *   OVEPT,SYM2(8),LFFLG,                                           MAINCOM
     *   LIBFLG,COUNT,IRADIX,SFILE(31),IPTLUN,                          MAINCOM
     *   BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG,                          MAINCOM
     *   RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD,                            MAINCOM
     *   APOVEN,OVLDEN,PSCNT,IZ1QS                                      MAINCOM
C                                                                       MAINCOM
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,IRADIX) .NE. -2) GOTO 90060
        IV=STOI (SYM,IRADIX)
        J=EXTTOK (SYM,6,STR,IPTR,IPTR,IRADIX)
        IV2=DBPG
        IF (J .EQ. 0) GOTO 2640
        IF (J .NE. -2) GOTO 90060
        IV2=STOI (SYM,IRADIX)+1
        IF (IV2 .LT. 1 .OR. IV2 .GT. 16) GOTO 90060
2640    ID=IADDC (PGINFO(IV2,1),INEG16 (IV),J)
        IF (J .EQ. 0 .AND. IV .NE. 0) CALL ERRMES (26)
        PGINFO(IV2,2)=IV
        IF (DBPG .EQ. IV2) DBBRK=IV
        GOTO 500
90060   CALL ERRMES(12)
500     RETURN
        END
C+++ MMAX
C****** MMAX = MMAX COMMAND FOR APLOAD = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
         SUBROUTINE MMAX
C------- MMAX
C
C       PICK UP SIZE AND PAGE IF IT'S THERE
C
C                                                                       MAINCOM
C  THE FIRST TWO LINES OF DECLARATION ARE NOT COMMON VARIABLES          MAINCOM
C  BUT INTEGER FUNCTIONS THAT MAY BE CALLED.                            MAINCOM
        INTEGER RDREC,EXTTOK,SRCST,EXTSL,RPLCS,EXTVT,INSST,EXTSS        MAINCOM
        INTEGER SRCSL,EXTCNT,LENT,SRCCS,STOI,IADDC,COMAND,CMPSS         MAINCOM
C                                                                       MAINCOM
        INTEGER ILUN,STR,IPTR,SYM,INDX                                  MAINCOM
        INTEGER IFLG,IV,OVSKIP,IV2,TREFLG                               MAINCOM
        INTEGER I,II,J,JJ,ID,OVMESZ,SFILE1                              MAINCOM
        INTEGER OVEPT,SYM2,LFFLG                                        MAINCOM
        INTEGER LIBFLG,COUNT,IRADIX,SFILE,IPTLUN                        MAINCOM
        INTEGER BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG                    MAINCOM
        INTEGER RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD                      MAINCOM
        INTEGER APOVEN,OVLDEN,PSCNT,IZ1QS                               MAINCOM
        COMMON /MCOMM/ ILUN,STR(81),IPTR,SYM(8),INDX,                   MAINCOM
     *   IFLG,IV,OVSKIP,IV2,TREFLG,                                     MAINCOM
     *   I,II,J,JJ,ID,OVMESZ,SFILE1(16),                                MAINCOM
     *   OVEPT,SYM2(8),LFFLG,                                           MAINCOM
     *   LIBFLG,COUNT,IRADIX,SFILE(31),IPTLUN,                          MAINCOM
     *   BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG,                          MAINCOM
     *   RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD,                            MAINCOM
     *   APOVEN,OVLDEN,PSCNT,IZ1QS                                      MAINCOM
C                                                                       MAINCOM
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,IRADIX) .NE. -2) GOTO 90060
        IV=STOI (SYM,IRADIX)
        IV2=DBPG
        J=EXTTOK (SYM,6,STR,IPTR,IPTR,IRADIX)
        IF (J .EQ. 0) GOTO 2930
        IF (J .NE. -2) GOTO 90060
        IV2=STOI (SYM,IRADIX)+1
        IF (IV2 .LE. 0 .OR. IV2 .GT. 16) GOTO 90060
2930    PGINFO (IV2,1)=IV
        GOTO 500
90060    CALL ERRMES(12)
500      RETURN
         END
C+++ NOLOAD
C****** NOLOAD = NOLOAD COMMAND FOR APLOAD = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
         SUBROUTINE NOLOAD
C------- NOLOAD <ENTRY SYMBOL> ...
C
C       INHIBIT THE LOADING OF THE GIVEN ENTRY POINTS
C
C       GET SYMBOLS AND PLACE THEM IN ENTDTA() FLAGED AS NOLOADS.  IF
C       THE SYMBOL IS ALREADY THERE BUT NOT LOADED SET THE NO-LOAD FLAG.
C
C                                                                       MAINCOM
C  THE FIRST TWO LINES OF DECLARATION ARE NOT COMMON VARIABLES          MAINCOM
C  BUT INTEGER FUNCTIONS THAT MAY BE CALLED.                            MAINCOM
        INTEGER RDREC,EXTTOK,SRCST,EXTSL,RPLCS,EXTVT,INSST,EXTSS        MAINCOM
        INTEGER SRCSL,EXTCNT,LENT,SRCCS,STOI,IADDC,COMAND,CMPSS         MAINCOM
C                                                                       MAINCOM
        INTEGER ILUN,STR,IPTR,SYM,INDX                                  MAINCOM
        INTEGER IFLG,IV,OVSKIP,IV2,TREFLG                               MAINCOM
        INTEGER I,II,J,JJ,ID,OVMESZ,SFILE1                              MAINCOM
        INTEGER OVEPT,SYM2,LFFLG                                        MAINCOM
        INTEGER LIBFLG,COUNT,IRADIX,SFILE,IPTLUN                        MAINCOM
        INTEGER BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG                    MAINCOM
        INTEGER RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD                      MAINCOM
        INTEGER APOVEN,OVLDEN,PSCNT,IZ1QS                               MAINCOM
        COMMON /MCOMM/ ILUN,STR(81),IPTR,SYM(8),INDX,                   MAINCOM
     *   IFLG,IV,OVSKIP,IV2,TREFLG,                                     MAINCOM
     *   I,II,J,JJ,ID,OVMESZ,SFILE1(16),                                MAINCOM
     *   OVEPT,SYM2(8),LFFLG,                                           MAINCOM
     *   LIBFLG,COUNT,IRADIX,SFILE(31),IPTLUN,                          MAINCOM
     *   BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG,                          MAINCOM
     *   RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD,                            MAINCOM
     *   APOVEN,OVLDEN,PSCNT,IZ1QS                                      MAINCOM
C                                                                       MAINCOM
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        IV=STR(1)
5750    IF (IPTR .GT. IV) GOTO 500
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,10) .NE. -1) GOTO 90060
        CALL PAKS (SYM,SYM,6)
        J=SRCST (ENTDTA,1,-1,SYM,6)
        IF (J .EQ. 0) GOTO 5780
        JJ=EXTVT (ENTDTA,J,2,ID,1)
        IF (IAND16 (JJ,8) .EQ. 0) GOTO 5750
        JJ=IOR16 (JJ,32)
        JJ=IAND16 (JJ,IP16 (-17))
        CALL RPLVT (ENTDTA,J,2,JJ,1)
        GOTO 5750
5780    IF (ENTDTA(1)+1 .GT. ENTMAX) GOTO 90120
        ID=INSST (ENTDTA,-1,SYM,6)
        CALL RPLVT (ENTDTA,-1,2,40,1)
        GOTO 5750
90060    CALL ERRMES(12)
         RETURN
90120    CALL ERRMES(7)
500      RETURN
         END
C+++ OUTPUT
C****** OUTPUT = OUTPUT COMMAND FOR APLOAD = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
          SUBROUTINE OUTPUT
C--- OUTPUT <FTN SRC FILE NAME> <LM FILE NAME> [/D,<LM FILE NAME>/D] <DB PAGE>
C
C       GET THE FORTRAN SOURCE AND LOAD MODULE (OPTIONAL) FILE NAMES.
C       IF A FORTRAN SOURCE FILE WAS ALREADY ASSIGNED CLOSE IT FIRST.
C       IF LOAD MODULE FILE ALREADY SPECIFIED IGNORE SECOND PARAMETER.
C       THIRD PARAMETER IS DB PAGE DESIGNATOR, DEFAULT IS 0 BUT IT MUST
C       BE .LE. 15.
C
C                                                                       MAINCOM
C  THE FIRST TWO LINES OF DECLARATION ARE NOT COMMON VARIABLES          MAINCOM
C  BUT INTEGER FUNCTIONS THAT MAY BE CALLED.                            MAINCOM
        INTEGER RDREC,EXTTOK,SRCST,EXTSL,RPLCS,EXTVT,INSST,EXTSS        MAINCOM
        INTEGER SRCSL,EXTCNT,LENT,SRCCS,STOI,IADDC,COMAND,CMPSS         MAINCOM
C                                                                       MAINCOM
        INTEGER ILUN,STR,IPTR,SYM,INDX                                  MAINCOM
        INTEGER IFLG,IV,OVSKIP,IV2,TREFLG                               MAINCOM
        INTEGER I,II,J,JJ,ID,OVMESZ,SFILE1                              MAINCOM
        INTEGER OVEPT,SYM2,LFFLG                                        MAINCOM
        INTEGER LIBFLG,COUNT,IRADIX,SFILE,IPTLUN                        MAINCOM
        INTEGER BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG                    MAINCOM
        INTEGER RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD                      MAINCOM
        INTEGER APOVEN,OVLDEN,PSCNT,IZ1QS                               MAINCOM
        COMMON /MCOMM/ ILUN,STR(81),IPTR,SYM(8),INDX,                   MAINCOM
     *   IFLG,IV,OVSKIP,IV2,TREFLG,                                     MAINCOM
     *   I,II,J,JJ,ID,OVMESZ,SFILE1(16),                                MAINCOM
     *   OVEPT,SYM2(8),LFFLG,                                           MAINCOM
     *   LIBFLG,COUNT,IRADIX,SFILE(31),IPTLUN,                          MAINCOM
     *   BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG,                          MAINCOM
     *   RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD,                            MAINCOM
     *   APOVEN,OVLDEN,PSCNT,IZ1QS                                      MAINCOM
C                                                                       MAINCOM
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
C        GET THE SIZE PARAMETER.
        CALL SKPBLK (STR,IPTR)
        J=SRCCS (STR,IPTR,8239)
        IF (J.EQ.0.OR.J.NE.IPTR) GO TO 5000
        IPTR=IPTR+1
        IV=EXTTOK (SYM,6,STR,IPTR,IPTR,IRADIX)
        IF (IV.EQ.0) GO TO 5690
        IF (IV.NE.-2) GO TO 5690
        IV=STOI (SYM,IRADIX)
        IF (ICMP16(IV,200).EQ.-1.OR.ICMP16(IV,32767).EQ.1) GO TO 5690
        MXDATA=IV/8 * 8
C        GET FILE NAMES
5000    CONTINUE
        IF (SFFIL(1) .NE. 0) GOTO 5650
        IZ1QS = INFILE (4,SFFIL,FLUN)
5650    CALL SKPBLK (STR,IPTR)
        J=IPTR
        IPTR=SRCCS (STR,J,8224)
        IF (IPTR .EQ. 0) IPTR=99
        IZ1QS = EXTSS (STR,J,SFFIL,IPTR-J)
        IF (INFILE (2,SFFIL,FLUN) .NE. 0) GOTO 90000
        IF (SLMFIL(1) .NE. 0) GOTO 500
        IF (FLMFIL(1).NE.0) GO TO 500
        CALL OUTSCN
        IV=EXTTOK (SYM,6,STR,IPTR,IPTR,IRADIX)
        RSTRCT=0
        IF (IV .EQ. 0) GOTO 5670
        IF (IV .NE. -2) GOTO 5690
        IV=STOI (SYM,IRADIX)
        IF (IV .LT. 0 .OR. IV .GT. 15) GOTO 5690
        DBPG=IV+1
5670    ID=IADDC (PGINFO(DBPG,1),INEG16 (MDLOW),J)
        IF (J .EQ. 0) CALL ERRMES (26)
        PGINFO(DBPG,2)=MDLOW
        GOTO 500
5690    ID=INFILE (4,SLMFIL,LMLUN)
        CALL RMVCS (SLMFIL,1,-1)
        ID=INFILE (4,FLMFIL,FLMLUN)
        CALL RMVCS (FLMFIL,1,-1)
        RSTRCT=1
        GOTO 90060
90000    CALL ERRMES(10)
         RETURN
90060    CALL ERRMES(12)
500      RETURN
         END
C+++ OUTSCN
C****** OUTSCN = SCAN OUTPUT COMMAND = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
          SUBROUTINE OUTSCN
C
C  OUTPUT <FTN SRC FILE NAME> <LM FILE NAME> [/D,<LM FILE NAME>/D] <DB PAGE>
C
C       SCAN THE LOAD MODULE FILE NAMES PART OF THE OUTPUT COMMAND.
C       OPEN LOAD MODULE FILES AND RETURN COMMAND LINE POINTER
C       POINTING TO THE LAST PARAMETER OF THE OUTPUT COMMAND.
C       NOTE THAT ANYTHING BETWEEN THE [] IN THE COMMAND LINE
C       IS OPTIONAL BUT AT MOST ONE CHOICE GIVEN CAN BE USED.
C
C                                                                       MAINCOM
C  THE FIRST TWO LINES OF DECLARATION ARE NOT COMMON VARIABLES          MAINCOM
C  BUT INTEGER FUNCTIONS THAT MAY BE CALLED.                            MAINCOM
        INTEGER RDREC,EXTTOK,SRCST,EXTSL,RPLCS,EXTVT,INSST,EXTSS        MAINCOM
        INTEGER SRCSL,EXTCNT,LENT,SRCCS,STOI,IADDC,COMAND,CMPSS         MAINCOM
C                                                                       MAINCOM
        INTEGER ILUN,STR,IPTR,SYM,INDX                                  MAINCOM
        INTEGER IFLG,IV,OVSKIP,IV2,TREFLG                               MAINCOM
        INTEGER I,II,J,JJ,ID,OVMESZ,SFILE1                              MAINCOM
        INTEGER OVEPT,SYM2,LFFLG                                        MAINCOM
        INTEGER LIBFLG,COUNT,IRADIX,SFILE,IPTLUN                        MAINCOM
        INTEGER BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG                    MAINCOM
        INTEGER RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD                      MAINCOM
        INTEGER APOVEN,OVLDEN,PSCNT,IZ1QS                               MAINCOM
        COMMON /MCOMM/ ILUN,STR(81),IPTR,SYM(8),INDX,                   MAINCOM
     *   IFLG,IV,OVSKIP,IV2,TREFLG,                                     MAINCOM
     *   I,II,J,JJ,ID,OVMESZ,SFILE1(16),                                MAINCOM
     *   OVEPT,SYM2(8),LFFLG,                                           MAINCOM
     *   LIBFLG,COUNT,IRADIX,SFILE(31),IPTLUN,                          MAINCOM
     *   BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG,                          MAINCOM
     *   RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD,                            MAINCOM
     *   APOVEN,OVLDEN,PSCNT,IZ1QS                                      MAINCOM
C                                                                       MAINCOM
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
C------- LOCAL STORAGE
C
        INTEGER STEST(3),TEMP1(31),TEMP2(31)
C                                                                       STARTPRE
      INTEGER SLSHD(3)
C STRNG SLSHD "/D"
      DATA SLSHD(2)/'/ '/,SLSHD(3)/'D '/
      DATA SLSHD(1)/2/
C                                                                       ENDPRE
C
C------- GET THE FIRST LOAD MODULE FILE NAME.  THIS IS THE NAME
C        OF THE HOST RESIDENT LOAD MODULE IF A /D DOES'NT FOLLOW IT.
C
C   SET SWITCH TO SHOW THAT HOST RESIDENT LOAD MODULE ONLY IS USED.
        LMLDSW=1
        LEN=STR(1)
C   GET FILE NAME.
        CALL SKPBLK (STR,IPTR)
        J=IPTR
        IF(J .GT. LEN) J=LEN
C   A BLANK OR SLASH IS THE FILE NAME DELIMITER.
        DO 10 IPTR=J,LEN
        IF (STR(IPTR+1).EQ.8224 .OR. STR(IPTR+1).EQ.8239) GO TO 20
10      CONTINUE
        IPTR=LEN+1
20      CONTINUE
        IZ1QS=EXTSS (STR,J,TEMP1,IPTR-J)
C    LOOK FOR /D TO FOLLOW.
        CALL SKPBLK (STR,IPTR)
        IZ1QS=EXTSS (STR,IPTR,STEST,2)
        IF (CMPSS(STEST,SLSHD,2).NE.0) GO TO 100
C    IT FOLLOWS SO SET SWITCH TO SHOW THAT DISK RESIDENT LOAD MODULE
C    ONLY IS USED.
        LMLDSW=0
        GO TO 300
C
C------- GET THE SECOND LOAD MODULE FILE NAME, IF THERE IS ONE.
C
C   GET FILE NAME.
100     CONTINUE
        J=IPTR
C   A BLANK OR SLASH IS THE FILE NAME DELIMTER.
        DO 110 IPTR=J,LEN
        IF (STR(IPTR+1).EQ.8224 .OR. STR(IPTR+1).EQ.8239) GO TO 120
110     CONTINUE
        IPTR=99
120     CONTINUE
        IZ1QS=EXTSS (STR,J,TEMP2,IPTR-J)
C   LOOK FOR /D TO FOLLOW.
        CALL SKPBLK (STR,IPTR)
        IZ1QS=EXTSS (STR,IPTR,STEST,2)
        IF (CMPSS(STEST,SLSHD,2).NE.0) GO TO 200
C   IT FOLLOWS SO SET SWITCH TO SHOW THAT BOTH HOST AND DISK
C   RESIDENT LOAD MODULES ARE USED.
        LMLDSW=2
        GO TO 300
C
C------- ONLY THE HOST RESIDENT LOAD MODULE IS USED SO BACK UP THE
C        POINTER TO POINT TO THE LAST PARAMETER IN THE OUTPUT COMMAND.
C
200     CONTINUE
        IPTR=J
        GO TO 400
C
C------- A /D WAS DETECTED SO MOVE THE POINTER AHEAD TO POINT TO THE
C        LAST PARAMETER IN THE OUTPUT COMMAND.
C
300     CONTINUE
        IPTR=IPTR+2
        CALL SKPBLK (STR,IPTR)
C
C------- NOW OPEN THE FILES ACCORDING TO THE SETTING OF LMLDSW.
C
400     CONTINUE
        IF (LMLDSW.EQ.0) IZ1QS=EXTSS(TEMP1,1,SLMFIL,TEMP1(1))
        IF (LMLDSW.EQ.1.OR.LMLDSW.EQ.2)
     *      IZ1QS=EXTSS(TEMP1,1,FLMFIL,TEMP1(1))
        IF (LMLDSW.EQ.2) IZ1QS=EXTSS(TEMP2,1,SLMFIL,TEMP2(1))
        IF (LMLDSW.EQ.1) GO TO 450
C   THE ACTION CODE OF 35 IS ACTUALLY 32 + 3, WHICH MEANS TO OPEN
C   A UNFORMATTED DISK FILE.
        IF (INFILE (35,SLMFIL,LMLUN).EQ.0) GO TO 450
        CALL RMVCS (SLMFIL,1,-1)
        GO TO 90000
C
450     CONTINUE
        IF (LMLDSW.EQ.0) GO TO 500
        IF (INFILE (2,FLMFIL,FLMLUN).EQ.0) GO TO 500
        CALL RMVCS (FLMFIL,1,-1)
        GO TO 90000
C
90000   CALL ERRMES (10)
500     RETURN
        END
C+++ OVLY
C****** OVLY = OVERLAY COMMAND = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE OVLY(STR,IPTR,IRADIX,LNKFLG,SYM)
C
C       CALL:
C
C       STR    = THE USER INPUT STRING CONTAINING THE LOADER OVERLAY
C                COMMAND.
C       IPTR   = POINTER INTO STR() JUST AFTER THE 'OVERLAY'.
C       IRADIX = THE CURRENT INPUT RADIX
C       LNKFLG = 0 IF THIS ROUTINE IS CALLED BY THE USER GIVING AN
C                OVERLAY COMMAND, ELSE 1 IF CONTROL CAME HERE VIA THE
C                LINK COMMAND.
C       SYM()  = A GENERAL SYMBOL BUFFER
C
        INTEGER STR(81),IPTR,IRADIX,LNKFLG,SYM(7)
C
        INTEGER RSTPTR,TOVID,TOVPG,LNKCNT,IVAL(3),IV,I,IV2,INDX,J,ID,JJ
        INTEGER LUN
        INTEGER EXTTOK,EXTVT,SRCST,STOI,INSST,EXTST,RPLST
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C                                                                       COMMON10
       INTEGER TSKDTA(26,8),TSKPTR,TSKMAX,TCBMNL,TCBMXL                 COMMON10
       INTEGER SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,SPARPT                COMMON10
       INTEGER SDB0PT,SDB1PT,SDB2PT,PSTOP                               COMMON10
       INTEGER ISRMDA,ISRPSA,ISRIDX,ISRLEN                              COMMON10
       INTEGER OVPDTA(50,3),PSPDTA(50),OVPPTR,PSPPTR,PSPMAX,OVPMAX      COMMON10
       LOGICAL TASKFL,TSKFL,ISRFL,IOPENX                                COMMON10
       COMMON /TSKCOM/ TSKDTA,TSKPTR,TSKMAX,TCBMNL,TCBMXL,              COMMON10
     -         SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,                      COMMON10
     -         SPARPT,SDB0PT,SDB1PT,SDB2PT,PSTOP,                       COMMON10
     -         ISRMDA,ISRPSA,ISRIDX,ISRLEN,                             COMMON10
     -         OVPDTA,PSPDTA,OVPPTR,PSPPTR,PSPMAX,OVPMAX,               COMMON10
     -         TASKFL,TSKFL,ISRFL                                       COMMON10
       COMMON /OPENX/ IOPENX                                            COMMON10
C                                                                       COMMON10
C
        IF (LNKFLG .EQ. 1 ) GOTO 3210
        IF (OVPTR .EQ. 0) GOTO 90220
        IF(OVID .EQ. -1) GOTO 99999
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,IRADIX) .NE. -2) GOTO 90060
        IV=STOI (SYM,IRADIX)
        IF (IV .EQ. 0 .OR. NEGCHK (IV) .EQ. 1) GOTO 90060
C       CHECK FOR NON-EXISTANT OVERLAY ID NUMBER
C
        DO 3200 I=1,OVPTR
        IF (OVDTA(I,1) .EQ. IV) GOTO 3220
3200    CONTINUE
        GOTO 90060
C
C       CONTROL COMES HERE FROM LINK COMMAND
C
3210    IV=0
C
C       MAKE SURE THIS OVERLAY NUMBER IS IN ORDER
C
3220    IV2=CUROV
        LNKCNT=0
        INDX=1
        IF (IV2 .EQ. 0) GOTO 3250
        INDX=IAND16 (OVDTA(IV2,5),255)
        IF (INDX .NE. 0) GOTO 3250
        LNKCNT=1
        RSTPTR=IV2
        INDX=IRSH16 (OVDTA(IV2,5),8)
        IF (INDX .NE. 0) GOTO 3250
3240    IV2=IRSH16 (OVDTA(IV2,6),4)
        IF (IV2 .NE. 0) GOTO 3245
        IF (LNKFLG .EQ. 0) GOTO 90060
        GOTO 3300
3245    RSTPTR=IV2
        INDX=IRSH16 (OVDTA(IV2,5),8)
        LNKCNT=LNKCNT+1
        IF (INDX .EQ. 0) GOTO 3240
3250    IF (OVDTA(INDX,1) .EQ. IV) GOTO 3260
        IF (LNKFLG .EQ. 1) GOTO 90220
        GOTO 90060
C
C       GET OPTIONAL PAGE DESIGNATOR, IT MUST BE 0-15
C
3260    TOVID=IV
        IV=EXTTOK (SYM,6,STR,IPTR,IPTR,IRADIX)
        TOVPG=DBPG
        IF (IV .EQ. 0) GOTO 3300
        IF (IV .NE. -2) GOTO 90060
        IF (IV.EQ.-2) GO TO 90060
        TOVPG=STOI (SYM,IRADIX)
        IF (TOVPG .LT. 0 .OR. TOVPG .GT. 15) GOTO 90060
        TOVPG=TOVPG+1
C
C       UPDATE THE LAST OVERLAYS STUFF
C
3300    IF (OV1FLG .EQ. 0) GOTO 3350
        IF (OVPG .NE. DBPG) GOTO 3330
        OVDTA(CUROV,3)=DBBRK
        IF (DBDTA1(1)+1 .GT. DB1MAX) GOTO 90100
        CALL ITOS (OVDTA(CUROV,1),SYM,6,RADIX)
        CALL INSCS (SYM,0,8238)
        ID=INSST (DBDTA1,-1,SYM,7)
        IVAL(1)=DBBRK
        IVAL(2)=0
        IVAL(3)=OVDTA(CUROV,4)
        CALL RPLVT (DBDTA1,-1,1,IVAL,3)
        IV=IADDC (DBBRK,OVDTA(CUROV,4),J)
        ID=IADDC (PGINFO(DBPG,1),INEG16 (IV),I)
        IF (J .EQ. 1 .OR. (I .EQ. 0 .AND. IV .NE. 0)) CALL ERRMES (26)
        DBBRK=IV
        PGINFO(DBPG,2)=DBBRK
        GOTO 3350
3330    OVDTA(CUROV,3)=PGINFO(OVPG,2)
        IV=IADDC (PGINFO(OVPG,2),OVDTA(CUROV,4),J)
        ID=IADDC (PGINFO(OVPG,1),INEG16 (IV),I)
        IF (J .EQ. 1 .OR. (I .EQ. 0 .AND. IV .NE. 0)) CALL ERRMES (26)
        PGINFO(OVPG,2)=IV
C
C       LINK UP ANY PREVIOUS OVERLAYS IF IT'S TIME TO.
C
3350    ID=0
        LUN=CLUN+LUNMAP
        IF (OVLEVL .NE. 0) WRITE (LUN,3355) ID
3355    FORMAT (' $ 0',I7)
        IF (LNKCNT .EQ. 0) GOTO 33655
        CALL LINKUP (1,LNKCNT,ID)
C
C       EXAMINE ALL EXTERNALS THAT WON'T BE PURGED FROM THE EXTDTA
C       TABLE DUE TO LINKING OF RECENT OVERLAYS.  IF ANY OF THEM
C       ARE SATISFIED BY ENTRIES THAT WILL BE PURGED, THEN REPLACE
C       THE EXTERNAL SYMBOL WITH A BLANK.ZERO IN THE FIRST
C       CHARACTER.
C
        J=IAND16 (OVDTA(RSTPTR,9),255)-1
        IF (J .EQ. 0) GOTO 3365
        JJ=IAND16 (OVDTA(RSTPTR,8),255)
        IF (JJ .GT. ENTDTA(1)) GOTO 3365
        DO 3360 I=1,J
        ID=EXTST (EXTDTA,I,SYM,-1)
        IV2=SRCST (ENTDTA,JJ,-1,SYM,-1)
        IF (IV2 .EQ. 0) GOTO 3360
        IV=EXTVT (ENTDTA,IV2,2,ID,1)
        IF (IAND16 (IV,8) .NE. 0) GOTO 3360
C
C       SET FIRST CHARACTER OF THE SYMBOL TO ZERO TO MARK IT NOT USED
C
        SYM(2)=0
        ID=RPLST (EXTDTA,I,SYM)
3360    CONTINUE
C
C       BACK UP POINTERS IN:  PRGDTA, ENTDTA, EXTDTA, PARDTA, ENTDT1,
C       EXTDT1.
C       OVDTA(RSTPTR,7-9) CONTAIN THE POINTERS INTO PRGDTA, ENTDTA AND
C       EXTDTA WHERE ALREADY LOADED OVERLAY DATA MAY BE REMOVED.
C       EX1PTR AND ENTPT1 ARE ALSO SAVED WITH THE POINTERS INTO
C       EXTDTA AND ENTDTA.
C       LENGTHS.
C
3365    CONTINUE
        IV=IAND16 (OVDTA(RSTPTR,8),255)
        CALL RMVET (ENTDTA,IV,-1)
        IV=IAND16 (OVDTA(RSTPTR,9),255)
        CALL RMVET (EXTDTA,IV,-1)
        EX1PTR=IRSH16 (OVDTA(RSTPTR,9),8)
        ENTPT1=IRSH16 (OVDTA(RSTPTR,8),8)
        CALL RMVET (PRGDTA,OVDTA(RSTPTR,7),-1)
C
C       RESET PSBRK FOR NEXT OVERLAY BRANCH
C
        IF (PSBRK.GT.PSTOP) PSTOP=PSBRK
        PSBRK=OVDTA(RSTPTR,2)
C
C       IF WE GOT HERE VIA A LINK COMMAND GO BACK TO LINK PROCESSING.
C
        IF (LNKFLG .EQ. 1) GOTO 99999
33655   CONTINUE
        IF (.NOT.TASKFL) GO TO 3369
C
C       INSERT THE NEW PSADDR INTO THE PS PARTITION TABLE
C
        IF (PSPPTR+1.GT.PSPMAX) GO TO 90240
        PSPPTR=PSPPTR+1
        IF (PSPPTR.GT.1) GO TO 3366
        PSPDTA(1)=PSBRK
      GO TO 3369
3366    CONTINUE
        J=PSPPTR-1
        DO 3367 I=1,J
        IF (PSBRK.NE.PSPDTA(I)) GO TO 33665
        PSPPTR=PSPPTR-1
      GO TO 3369
33665   CONTINUE
        IF (PSBRK.LT.PSPDTA(I)) GO TO 3368
3367    CONTINUE
        PSPDTA(PSPPTR)=PSBRK
C ADD TO END
        GO TO 3369
C
C       INSERT NEW PARTITION IN THE MIDDLE
C
3368    CONTINUE
        J=PSPPTR
33685   CONTINUE
        PSPDTA(J)=PSPDTA(J-1)
      J=J-1
        IF (J.GT.I) GO TO 33685
        PSPDTA(I)=PSBRK
3369    CONTINUE
        CUROV=INDX
        OVPG=TOVPG
        OVID=TOVID
        OVDTA(CUROV,2)=PSBRK
        OVDTA(CUROV,6)=IOR16 (OVDTA(CUROV,6),OVPG-1)
        OVDTA(CUROV,7)=PRGDTA(1)+1
        OVDTA(CUROV,8)=IOR16 (ENTDTA(1)+1,ILSH16 (ENTPT1,8))
        OVDTA(CUROV,9)=IOR16 (EXTDTA(1)+1,ILSH16 (ENTPT1,8))
        OV1FLG=1
        PSHLD=PSBRK
        OVLEVL=OVLEVL+1-LNKCNT
        LUN=CLUN+LUNMAP
        WRITE (LUN,3355) OVID
C3355   FORMAT (' $ 0',I7)
        RSTRCT=0
        GOTO 99999
C
C------- ERROR RETURNS
C
C       BAD OR MISSING PARAMETER
C
90060   CALL ERRMES (12)
        GOTO 99999
C
C       TOO MANY DATA BLOCKS
C
90100   CALL ERRMES (16)
        GOTO 99999
C
C       COMMAND OUT OF ORDER
C
90220   CALL ERRMES (22)
        GOTO 99999
C
C       PSPDTA TABLE OVERFLOW
C
90240   CONTINUE
        CALL ERRMES(32)
        RETURN
C
C       RETURN
C
99999   RETURN
        END
C+++ PAKSTR
C+++ PSOFF
C****** PSOFF = PSOFF COMMAND FOR APLOAD = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE PSOFF
C------- PSOFF <ADDRESS>
C
C       GET THE ADDRESS AND RESET THE VALUE OF PSBRK
C
C                                                                       MAINCOM
C  THE FIRST TWO LINES OF DECLARATION ARE NOT COMMON VARIABLES          MAINCOM
C  BUT INTEGER FUNCTIONS THAT MAY BE CALLED.                            MAINCOM
        INTEGER RDREC,EXTTOK,SRCST,EXTSL,RPLCS,EXTVT,INSST,EXTSS        MAINCOM
        INTEGER SRCSL,EXTCNT,LENT,SRCCS,STOI,IADDC,COMAND,CMPSS         MAINCOM
C                                                                       MAINCOM
        INTEGER ILUN,STR,IPTR,SYM,INDX                                  MAINCOM
        INTEGER IFLG,IV,OVSKIP,IV2,TREFLG                               MAINCOM
        INTEGER I,II,J,JJ,ID,OVMESZ,SFILE1                              MAINCOM
        INTEGER OVEPT,SYM2,LFFLG                                        MAINCOM
        INTEGER LIBFLG,COUNT,IRADIX,SFILE,IPTLUN                        MAINCOM
        INTEGER BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG                    MAINCOM
        INTEGER RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD                      MAINCOM
        INTEGER APOVEN,OVLDEN,PSCNT,IZ1QS                               MAINCOM
        COMMON /MCOMM/ ILUN,STR(81),IPTR,SYM(8),INDX,                   MAINCOM
     *   IFLG,IV,OVSKIP,IV2,TREFLG,                                     MAINCOM
     *   I,II,J,JJ,ID,OVMESZ,SFILE1(16),                                MAINCOM
     *   OVEPT,SYM2(8),LFFLG,                                           MAINCOM
     *   LIBFLG,COUNT,IRADIX,SFILE(31),IPTLUN,                          MAINCOM
     *   BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG,                          MAINCOM
     *   RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD,                            MAINCOM
     *   APOVEN,OVLDEN,PSCNT,IZ1QS                                      MAINCOM
C                                                                       MAINCOM
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,IRADIX) .NE. -2) GOTO 90060
        IV=STOI (SYM,IRADIX)
        ID=IADDC (PSMAX,INEG16 (IV),J)
        IF (J .EQ. 0 .AND. IV .NE. 0) CALL ERRMES (26)
        PSBRK=IV
        IF (PSBRK .LT. PSLOW) PSLOW=PSBRK
        GOTO 500
90060    CALL ERRMES(12)
500      RETURN
         END
C+++    PURGE
C****** PURGE = PURGE TEMPORARY ITEMS = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE PURGE
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C                                                                       COMMON10
       INTEGER TSKDTA(26,8),TSKPTR,TSKMAX,TCBMNL,TCBMXL                 COMMON10
       INTEGER SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,SPARPT                COMMON10
       INTEGER SDB0PT,SDB1PT,SDB2PT,PSTOP                               COMMON10
       INTEGER ISRMDA,ISRPSA,ISRIDX,ISRLEN                              COMMON10
       INTEGER OVPDTA(50,3),PSPDTA(50),OVPPTR,PSPPTR,PSPMAX,OVPMAX      COMMON10
       LOGICAL TASKFL,TSKFL,ISRFL,IOPENX                                COMMON10
       COMMON /TSKCOM/ TSKDTA,TSKPTR,TSKMAX,TCBMNL,TCBMXL,              COMMON10
     -         SPRGDT,SENTPT,SENTDT,SEXTPT,SEXTDT,                      COMMON10
     -         SPARPT,SDB0PT,SDB1PT,SDB2PT,PSTOP,                       COMMON10
     -         ISRMDA,ISRPSA,ISRIDX,ISRLEN,                             COMMON10
     -         OVPDTA,PSPDTA,OVPPTR,PSPPTR,PSPMAX,OVPMAX,               COMMON10
     -         TASKFL,TSKFL,ISRFL                                       COMMON10
       COMMON /OPENX/ IOPENX                                            COMMON10
C                                                                       COMMON10
C
C       PURGE ALL THE TEMPORARY ITEMS FROM THE TABLES BY RESETTING
C       THE POINTERS WITH THE SAVED VALUES FROM THE MARK COMMAND
C
        CALL RMVET(PRGDTA,SPRGDT,-1)
        ENTPT1=SENTPT
        CALL RMVET(ENTDTA,SENTDT,-1)
        EX1PTR=SEXTPT
        CALL RMVET(EXTDTA,SEXTDT,-1)
        PARPTR=SPARPT
      DB0PTR=SDB0PT
        CALL RMVET(DBDTA1,SDB1PT,-1)
        DB2PTR=SDB2PT
        RETURN
        END
C+++ RDREC
C****** RDREC = READ FROM A FILE = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        INTEGER FUNCTION RDREC (LUN,STR,IPTR)
C
        INTEGER LUN,STR(81),IPTR
C
C       THIS ROUTINE READS A RECORD AND RETURNS A VALUE CORRESPONDING TO THE
C       RESULTS.
C
C       CALL:
C
C       LUN    = THE LOGICAL UNIT WHERE THE RECORD IS READ
C       STR    = THE INPUT BUFFER (81 ELEMENT ARRAY)
C
C       RETURNS:
C
C       STR    = THE RECORD READ
C       IPTR   = 1
C       RDREC  =  0 IF A SUCCESSFUL READ
C                -1 IF UNSUCCESSFUL READ
C                -2 IF END OF FILE READ
C
        INTEGER ID,SYM(2)
        INTEGER RDLIN,EXTTOK
C
C
        RDREC=RDLIN (STR,LUN,80)
C
C       BECAUSE OF DIFFERENT HOST'S CARRIAGE CONTROLS CHECK FIRST 2 BYTES
C       OF STRING FOR END OF FILE.
C
        IF (RDREC .EQ. 8285 .OR. STR(3) .EQ. 8285) RDREC = -2
        IF (RDREC .NE.-1 .AND. RDREC.NE.-2 ) RDREC=0
        IPTR=1
        RETURN
        END
C
C+++ SKPBLK
C****** SKPBLK = SKIP A LOADER BLOCK = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE SKPBLK (STR,PTR)
C
        INTEGER STR(81),PTR
C
        INTEGER J,I
C
        J=STR(1)
        IF (PTR .GT. J) RETURN
        DO 100 I=PTR,J
        IF (STR(I+1) .NE. 8224) GOTO 200
100     CONTINUE
        PTR=J+1
        RETURN
200     PTR=I
        RETURN
        END
C+++ SKPSUB
C****** SKPSUB = SKIP ROUTINE'S OBJECT BLOCKS = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE SKPSUB (STRX,IPTRX,SYMX,FLAG)
C
        INTEGER STRX(81),IPTRX,SYMX(7),FLAG
C
C       THIS SUBROUTINE IS CALLED TO SKIP OVER THE OBJECT MODULE BLOCK
C       OF THE CURRENT SUBROUTINE
C
C       CALL:
C
C       STRX    = AN 80 CHARACTER BUFFER
C       IPTRX   = AN INTEGER VARIABLE
C       SYMX    = A 6 CHARACTER BUFFER
C       FLAG    = 0 IF THE NEXT RECORD TO BE READ IS A HEADER RECORD,
C                 1 IF THE STRX() PARAMETER CONTAINS THE HEADER
C
        INTEGER IDENT
C
C                                                                       MAINCOM
C  THE FIRST TWO LINES OF DECLARATION ARE NOT COMMON VARIABLES          MAINCOM
C  BUT INTEGER FUNCTIONS THAT MAY BE CALLED.                            MAINCOM
        INTEGER RDREC,EXTTOK,SRCST,EXTSL,RPLCS,EXTVT,INSST,EXTSS        MAINCOM
        INTEGER SRCSL,EXTCNT,LENT,SRCCS,STOI,IADDC,COMAND,CMPSS         MAINCOM
C                                                                       MAINCOM
        INTEGER ILUN,STR,IPTR,SYM,INDX                                  MAINCOM
        INTEGER IFLG,IV,OVSKIP,IV2,TREFLG                               MAINCOM
        INTEGER I,II,J,JJ,ID,OVMESZ,SFILE1                              MAINCOM
        INTEGER OVEPT,SYM2,LFFLG                                        MAINCOM
        INTEGER LIBFLG,COUNT,IRADIX,SFILE,IPTLUN                        MAINCOM
        INTEGER BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG                    MAINCOM
        INTEGER RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD                      MAINCOM
        INTEGER APOVEN,OVLDEN,PSCNT,IZ1QS                               MAINCOM
        COMMON /MCOMM/ ILUN,STR(81),IPTR,SYM(8),INDX,                   MAINCOM
     *   IFLG,IV,OVSKIP,IV2,TREFLG,                                     MAINCOM
     *   I,II,J,JJ,ID,OVMESZ,SFILE1(16),                                MAINCOM
     *   OVEPT,SYM2(8),LFFLG,                                           MAINCOM
     *   LIBFLG,COUNT,IRADIX,SFILE(31),IPTLUN,                          MAINCOM
     *   BAKPTR,LRPTR,LNKCNT,ITTO,TOVID,TOVPG,                          MAINCOM
     *   RSTPTR,LNKFLG,OVBKSZ,OVPSLW,OVBKAD,                            MAINCOM
     *   APOVEN,OVLDEN,PSCNT,IZ1QS                                      MAINCOM
C                                                                       MAINCOM
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
C       GET A OBJECT BLOCK HEADER AND SKIP THROUGH IT
C
        IPTRX=0
        IF (FLAG .NE. 0) GOTO 200
100     IF (RDREC (OLUN,STRX,IPTRX)+1) 90020,90000,200
200     ID=EXTTOK (SYMX,6,STRX,IPTRX,IPTRX,RADIX)
        IDENT=STOI (SYMX,RADIX)
        CALL BLKSKP (STRX,IPTRX,IDENT)
C
C       IF THAT LAST BLOCK WAS AN END BLOCK (IDENT=1) WE'RE DONE
C
        IF (IDENT .NE. 1) GOTO 100
        RETURN
C
C       ERROR RETURNS
C
C       READ ERROR
C
90000   CALL ERRMES (1)
        RETURN
C
C       UNEXPECTED EOF
C
90020   CALL ERRMES (4)
        RETURN
        END
C+++ SRC1
C****** SRC1 = CREATE BEGINNING HASI ROUTINE = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE SRC1 (DBSTRT,DBEND,EINDX,STR,HASIMD,COMFLG)
C
        INTEGER DBSTRT,DBEND,EINDX,STR(81),HASIMD,COMFLG
C
C       THIS ROUTINE BEGINS THE CREATING OF A HASI ROUTINE.  IT BUILDS
C       AN APPROPRIATE SUBROUTINE STATEMENT, THE COMMON /APLDCM/ (APEX
C       RUN TIME COMMUNICATION COMMON) STATMENT, AND COMMON STATEMENTS
C       FOR ANY COMMONS REFERENCED BY THE HOST CALLABLE ROUTINE.
C
C       CALL:
C
C       DBSTRT = INDEX INTO DBDTA0 WHERE THE CURRENT ROUTINES COMMON
C                POINTERS START.
C       DBEND  = INDEX INTO DBDTA0 WHERE THE CURRENT ROUTINES COMMON
C                POINTERS END.
C       EINDX  = INDEX OF THE ENTDTA() ENTRY THAT IS FORTRAN CALLABLE.
C       STR    = GENERAL 80 CHARACTER BUFFER
C       HASIMD = 1 IF THE HASI TO BE CREATED IS AN ADC, 2 FOR A UDC
C
C       RETURNS:
C
C       COMFLG = 1 IF THE ROUTINE CONTAINS A COMMON THAT MUST BE
C                RETURNED FROM THE AP AFTER IT RUNS.
C
C       ROUTINES UTIL   - CMPSS  EXTSS  EXTST  EXTVT  EADD16  IAND16
C                         IAND16 IRSH16 RMVCS
C                         RPLSS  WRTLIN
C                APLOAD - APPND  RDREC  VARNAM
C
C       VARIABLES:
C
C       IFLG   = USED TO DETERMINE IF AN OUTPUT LINE WAS ADDED TO BY
C                THE APPND ROUTINE AND MUST THEREFORE HAVE A TRAILING
C                COMMA REMOVED BEFORE OUTPUT.
C       INDX   = GENERAL USE
C       LENX   = USED TO KEEP A RUNNING TOTAL OF THE LENGTH OF A COMMON
C       SYM()  = GENERAL SYMBOL STORAGE
C
        INTEGER SYM(11),I1,I2,LENX,VNUM,INDX,IZ1QS,COMSYM(8),LUN,I3
        INTEGER ID,I,IFLG,NUM,VAL,IPTR,IVAL2(2),SIZE,J,J1,VAL2
        INTEGER EXTVT,EXTST,RDREC,SRCST,CMPSS,EXTSS
        INTEGER BLANK,COMMA,TMP(7)
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
C                                                                       STARTPRE
      INTEGER SBLANK(7)
C STRNG SBLANK ".BLANK"
      DATA SBLANK(2)/'. '/,SBLANK(3)/'B '/,SBLANK(4)/'L '/
      DATA SBLANK(5)/'A '/,SBLANK(6)/'N '/,SBLANK(7)/'K '/
      DATA SBLANK(1)/6/
C                                                                       ENDPRE
C
C       DEFINE VARIABLES TO BE OUTPUT UNDER 'A1' FORMAT.
C
        DATA BLANK,COMMA/2H  ,2H, /
C
C
C------- INITIALIZE
C
        LUN=FLUN+LUNMAP
        COMFLG=0
C
C       GET THE ENTRY PT. SYMBOL AND MAKE THE SUBROUTINE STMT.  IF I1 IS
C       ZERO THEN THERE ARE NO PARAMETERS IN THE CALL.  WRITE THE STMT.
C       TO THE HASI.
C
        ID=EXTST (ENTDTA,EINDX,SYM,6)
        CALL UPAKS (SYM,SYM,6)
        I1=EXTVT (ENTDTA,EINDX,2,I1,1)
        I1=IRSH16 (I1,8)
        WRITE (LUN,1010) (SYM(I),I=2,7)
1010    FORMAT (1X,6X,'SUBROUTINE ',6A1)
        IF (I1 .EQ. 0) GOTO 3000
C
C       THERE ARE FORMAL PARAMETERS USED BY THIS HASI ROUTINE, SO
C       WE WILL CREATE A CONTINUATION STATEMENT SURROUNDED BY
C       PARENTHESIS TO CONTAIN THEM.  THERE ARE I1 TOTAL PARAMETERS.
C       WRITE THE STATEMENT TO THE HASI.
C
        IF (I1 .EQ. 1) GOTO 2530
        I2=COMMA
        IF (I1 .EQ. 1) I2=BLANK
        I1=I1-1
        WRITE (LUN,2510) (I,I2,I=1,I1)
2510    FORMAT (1X,4X,'* (',16('P',I2,A1)/
     *          1X,4X,'*  ',16('P',I2,A1))
        I1=I1+1
        GOTO 2540
2530    WRITE (LUN,2510)
2540    WRITE (LUN,2550) I1
2550    FORMAT (1X,4X,'*',I2,')')
C
C       OUTPUT INTEGER, REAL, OR COMPLEX STATEMENT CORRESPONDING TO THE
C       PARAMETERS.  IF HASIMD SPECIFIES A UDC (HASIMD=2) ALL FORMAL
C       PARAMETERS ARE INTEGERS.  ALSO, DUE TO DEC10 ALL ARRAY PARAMETERS
C       MUST BE DIMENSIONED.  AGAIN, THERE ARE I1 TOTAL PARAMETERS.
C
        I2=1
        NUM=0
        DO 2800 I=1,I1
        NUM=NUM+1
        IF (HASIMD .EQ. 2) GOTO 2580
C
C       BRANCH ON THE TYPE OF PARAMETER INTEGER, REAL, OR COMPLEX.
C
        ID=IAND16 (PARDTA(I2),15)
        GO TO (2580,2620,2650) ,ID
C
C        INTEGER TYPE.
C
2580    CONTINUE
        ID=IRSH16 (PARDTA(I2),8)
        IF (ID.GT.0.AND.HASIMD.EQ.1) WRITE(LUN,2610) NUM
        IF (ID.LE.0.OR.HASIMD.EQ.2) WRITE(LUN,2590) NUM
2590    FORMAT (1X,6X,'INTEGER P',I2)
2610    FORMAT (1X,6X,'INTEGER P',I2,'(1)')
        GO TO 2690
C
C       REAL TYPE.
C
2620    CONTINUE
        ID=IRSH16 (PARDTA(I2),8)
        IF (ID.GT.0.AND.HASIMD.EQ.1) WRITE(LUN,2630) NUM
        IF (ID.LE.0.OR.HASIMD.EQ.2) WRITE(LUN,2640) NUM
2630    FORMAT (1X,6X,'REAL P',I2,'(1)')
2640    FORMAT (1X,6X,'REAL P',I2)
        GO TO 2690
C
C       COMPLEX TYPE.
C
2650    CONTINUE
        ID=IRSH16 (PARDTA(I2),8)
        IF (ID.GT.0.AND.HASIMD.EQ.1) WRITE(LUN,2660) NUM
        IF (ID.LE.0.OR.HASIMD.EQ.2) WRITE(LUN,2670) NUM
2660    FORMAT (1X,6X,'COMPLEX P',I2,'(1)')
2670    FORMAT (1X,6X,'COMPLEX P',I2)
C
2690    CONTINUE
        IF (HASIMD .EQ. 2) GOTO 2800
C
C       ADJUST THE PARDTA() POINTER (I1) TO SKIP OVER ANY DIMENSION
C       DESCRIPTORS THAT MIGHT BE THERE.
C
        J=IRSH16 (PARDTA(I2),8)
        I2=I2+1+J
2800    CONTINUE
C
C       WRITE THE     COMMON /APLDCM/      STATEMENT TO THE HASI
C
3000    CONTINUE
C
C       OUTPUT THE   COMMON /APLDCM/ ...     STATEMENT.
C
        WRITE (LUN,3070)
3070    FORMAT (1X,6X,' COMMON /APLDCM/ IPAV( 33),NU2,IDLM,NU1,',
     *                       'IPPAAD,IPPAND,IOVS(33),'/
     *          1X,4X,'*  LMT(10,3),LMTE')
C
C       BUILD COMMON STATEMENTS FOR ALL THE DATA BLOCKS REFERENCED BY
C       THIS USER'S ROUTINE EXCEPT THE .LOCAL ONE AND ANY THAT ARE
C       DEFINED TO BE LOCAL TO THE AP (VIA    $COMIO NAME 0     IN APAL
C       FOR EXAMPLE).
C
C       I1 AND I2 DELIMIT THE RANGE IN THE DBDTA0() WHERE COMMON
C       INFORMATION IS STORED.
C
        I1=DBSTRT
        IF (I1 .EQ. 0) GOTO 3400
        I2=DBEND
        NUM=0
        DO 3200 I=I1,I2
        NUM=NUM+1
        INDX=DBDTA0(I)
        VAL=EXTVT (DBDTA1,INDX,2,IVAL2,2)
C
C       J IS THE MODIFICATION VALUE OF THE COMMON: 3=COMMON IS
C       TRANSFERRED BOTH WAYS BETWEED HOST AND AP, 2=TRANSFER ONLY TO
C       THE AP, 1=TRANSFER ONLY FROM THE AP, AND 0=DO NO TRANSFERS AT
C       ALL.
C
C       IVAL2(1) IS SET TO THE LENGTH OF THE COMMON.
C       IVAL2(2) IS A POINTER INTO DBDTA2() WHERE MORE INFORMATION ON
C       THE PARTICULAR COMMON IS.
C
        J=IRSH16 (VAL,14)
        IF (J .EQ. 0) GOTO 3200
        VAL=IAND16 (VAL,16383)
        IVAL2(1)=VAL
C
C       IF THE COMMON NAME (COMSYM) STARTS WITH A POINT (.) AND IS NOT
C       .BLANK THEN IT MUST BE .LOCAL AND SHOULD BE IGNORED.
C
        IF (EXTST (DBDTA1,INDX,COMSYM,-1) .NE. 8238) GOTO 3090
        IF (CMPSS (COMSYM,SBLANK,-1) .NE. 0) GOTO 3200
        CALL BLNKST (COMSYM,6)
C
C       SET COMFLG ACCORDING TO THE COMMON MODIFICATION VALUE.
C       BEGIN BUILDING A COMMON STATEMENT.  LENX IS A RUNNING TOTAL
C       OF THE LENGTH OF THE COMMON.  VNUM IS AN IDENTIFICATION
C       NUMBER USED WHEN CREATING THE NAMES OF VARIABLES IN A
C       GIVEN COMMON.  NUM IS USED AS AN IDENTIFICATION VALUE FOR
C       VARIABLES BETWEED DIFFERENT COMMONS.  THUSLY, THE THIRD
C       VARIABLE IN THE SECOND COMMON WOULD BE C02003.  A NAMES OF THIS
C       TYPE WILL BE CREATED FOR EACH DBDTA2() ENTRY ASSOCIATED WITH
C       THIS COMMON.  THE VARIABLES LENGTHS ARE ALSO IN DBDTA2().
C
3090    IF (J .NE. 2) COMFLG=1
        LENX=0
        VNUM=0
3100    VNUM=VNUM+1
        VAL2=NUM*1000+VNUM
        CALL BLNKST (SYM,10)
        CALL ITOS(DBDTA2(VAL,2),SYM,10,10)
        WRITE (LUN,3110) (COMSYM(J),J=2,7),VAL2,(SYM(J),J=2,7)
3110    FORMAT (1X,6X,'COMMON /',6A1,'/ C',I5,'(',6A1,')')
C
C       UPDATE LENX (CURRENT COMMON LENGTH) AND WHEN THIS EQUALS
C       IVAL2(2) (THE ACTUAL LENGTH) THEN WE ARE DONE AND SHOULD
C       WRITE OUT A FINISHED COMMON STATEMENT.
C
        LENX=IADD16 (LENX,DBDTA2(VAL,2))
        VAL=VAL+1
        IF (LENX .NE. IVAL2(2)) GOTO 3100
C
C       GO THROUGH DBDTA2() ONCE MORE TO BUILD AND INTEGER STMT.
C       CONTAINING ANY OF THIS COMMONS VALUES THAT ARE INTEGERS (DEFINED
C       BY INFORMATION IN DBDTA2() ).  AGAIN LENX IS USED AS A RUNNING
C       COUNTER  OF THE LENGTH OF THE COMMON AND VNUM AS AN IDENTIFYING
C       VALUE FOR THE VARIABLES IN THE COMMON.
C
        LENX=0
        VNUM=0
        VAL=IVAL2(1)
3150    VNUM=VNUM+1
C
C       IF THE DBDTA2() VALUE IS NOT 1 THE VARIABLE ISN'T AN INTEGER.
C       OTHERWISE ADD THE NAME TO THE INTEGER STATEMENT.
C
        IF (DBDTA2(VAL,1).NE.4) GO TO 3160
        CALL ERRMES(31)
        DBDTA2(VAL,1)=1
C ASSUME INTEGER
3160    CONTINUE
        IF (DBDTA2(VAL,1) .NE. 1) GOTO 3170
        J1=NUM*1000+VNUM
        WRITE (LUN,3165) J1
3165    FORMAT (1X,6X,'INTEGER C',I5)
C
C       UPDATE LENX (RUNNING COMMON LENGTH).  WHEN THIS EQUALS IVAL2(2)
C       (THE ACTUAL LENGTH) WE'VE SEEN ALL THE INFORMATION THAT GOES
C       WITH THIS COMMON AND SHOULD
C       DO THE WHOLE THING OVER FOR THE NEXT COMMON.
C
3170    LENX=IADD16 (LENX,DBDTA2(VAL,2))
        VAL=VAL+1
        IF (LENX .NE. IVAL2(2)) GOTO 3150
3200    CONTINUE
C
C       DECLARE THE BUFFER TO HOLD THE DISK LOAD MODULE.
C
3400    CONTINUE
        IF (LMLDSW.NE.0) RETURN
        WRITE(LUN,3460) LMID, MXDATA
3460    FORMAT(1X,6X,'COMMON /CODE',I2,'/ CODE'/
     *         1X,6X,'INTEGER CODE(',I6,')')
        RETURN
        END
C+++ SRC2
C****** SRC2 = ADD DATA STATEMENTS TO HASI = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE SRC2 (STR,IPTR,LINK,LOCPTR)
C
        INTEGER STR(81),IPTR,LINK,LOCPTR
C
C       THIS ROUTINE IS CALLED DURRING THE SECOND LOADER PASS WHEN A
C       DBIB IS ENCOUNTERED AND THE ROUTINE BEING LOADED IS HOST
C       CALLABLE.  DATA STATEMENTS ARE PLACED IN THE HASI FOR ALL
C       THE ITEMS IN THE DBIB AND EACH RECORD READ FROM THE OBJECT
C       FILE IS COPIED TO DBLUN FOR LATER COPYING TO THE LOAD MODULE.
C
C       CALL:
C
C       STR    = CONTAINS A DBIB HEADER RECORD
C       IPTR   = A POINTER INTO STR() JUST AFTER THE BLOCK ID NUMBER
C       LINK   = A POINTER INTO DBDTA0() WHERE THIS ROUTINE'S BLOCK
C                DATA INFORMATION BEGINS.
C       LOCPTR = A POINTER INTO DBDTA1() WHERE THIS ROUTINE'S .LOCAL
C                BLOCK INFORMATION BEGINS.
C
C       ROUTINES UTIL   - EXTSS  EXTTOK EXTVT  IAND16 INFILE INSCS
C                         INSSS  IP16   RMVCS  RPLSS
C                         STOI   WRTLIN
C                APLOAD - APPND  COMPOS ERRMES RDREC  VARNAM WRT
C
C       VARIABLES:
C
C       ADDR   = RELATIVE ADDRESS OF AN ITEM (VARIABLE) WITHIN A COMMON
C       BLKID  = A NUMBER CORRESPONDING TO A COMMON BELONGING TO THE
C                ROUTINE BEING LOADED.
C       BUF()  = A 40 CHARACTER RECORD IN WHICH A VARIABLE NAME AND
C                ITS INITIALIZING VALUE WILL BE PLACED.
C       FTNLIN = A 72 CHARACTER BUFFER USED TO BUILD THE FORTRAN SOURCE
C                OUTPUT LINE.
C       GROUP  = A VALUE RETURNED BY THE COMPOS ROUTINE SPECIFYING IN
C                WHICH VALUE TYPE GROUP OF A COMMON A GIVEN ITEM
C                BELONGS.  IF    COMMON A(10),I,J,R     THEN THE
C                VARIABLE A LOCATION 12 (J) IS IN TYPE GROUP 2 SINCE
C                THIS COMMON GOES   REAL, INTEGER, REAL
C       INDX   = GENERAL POINTER
C       POS    = INDICATES THE POSITION OF A VARIABLE WITHIN A GROUP
C                (SEE GROUP).  IN THE ABOVE EXAMPLE  J HAS POS=2
C                BECAUSE IT IS THE SECOND ITEM IN ITS GROUP.
C       RECCNT = TAKEN FROM THE DBIB HEADER:  THE NUMBER OF FOLLOWING
C                RECORDS SPECIFYING VARIABLES TO INITIALIZED.
C       RPTCNT = THE REPETITION COUNT OF AN ITEM BEING INITIALIZED.
C       SYM()  = GENERAL SYMBOL STORAGE
C       TYPEN  = THE VARIABLE TYPE OF AN ITEM BEING INITIALIZED,
C                1=INTEGER, 2=REAL
C
        INTEGER SYM(8),RECCNT,BLKID,TYPEN,ADDR,RPTCNT,FTNLIN(73)
        INTEGER GROUP,POS,ID,INDX,J,I,BUF(40),ICHAR,I1,J1,J2,K,LUN
        INTEGER SLASH
        INTEGER STOI,EXTTOK,RDREC,EXTVT,EXTSS,INEG16
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
C       DEFINE ALL ITEMS TO BE OUTPUT UNDER 'A1' FORMAT.
C
        DATA SLASH /2H/ /
C
C------- INITIALIZE ... THE CONTINUATION LINE COUNTER, INIT FTNLIN,
C                       REWIND DBLUN, AND WRITE THE DBIB HEADER TO
C                       DBLUN.
C
        LUN=FLUN+LUNMAP
        CALL WRTLIN (STR,DBLUN,-1)
C
C       GET RECORD COUNT AND READ DETAIL RECORDS.  IF THIS IS A LOCAL
C       BLOCK (FIRST CHAR IS ".") JUST SKIP ITS DETAIL RECORDS.
C       ADD INITIALIZING CODE TO A DATA STATEMENT FOR EVERY DBIB RECORD
C
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        RECCNT=STOI (SYM,RADIX)
        ICHAR=EXTTOK (SYM,1,STR,IPTR,IPTR,10)
        DO 5000 I=1,RECCNT
        IF (RDREC (OLUN,STR,IPTR)+1) 90040,90000,500
C
C       WRITE THE DBIB RECORD TO DBLUN, GET BLOCK ID (COMMON BLOCK
C       NUMBER), ADDRESS OF THE ITEM W/I THE BLOCK, VALUE TYPE,
C       AND REPETITION CONT.
C
500     CALL WRTLIN (STR,DBLUN,-1)
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        BLKID=STOI (SYM,RADIX)
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        ADDR=STOI (SYM,RADIX)
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        TYPEN=STOI(SYM,RADIX)
        IF (TYPEN.LT.16) GO TO 550
        CALL ERRMES(30)
        CALL WRTLIN (STR,-1,-1)
        TYPEN=TYPEN-16
550     CONTINUE
        IF (TYPEN.EQ.4) TYPEN=1
C ASSUME INTEGER
        IF (EXTTOK (SYM,6,STR,IPTR,IPTR,RADIX) .NE. -2) GOTO 90020
        RPTCNT=STOI (SYM,RADIX)
C
C       GET THE VARIABLE TYPE GROUP NUMBER AND RELATIVE POSITION W/I
C       THAT GROUP AND MAKE THE VARIABLE NAME.  (LINK IS THE POINTER
C       DBDTA0 WHERE THIS ROUTINE'S COMMON INFORMATION BEGINS.
C
        J=LINK+BLKID-1
        INDX=DBDTA0(J)
        IF (INDX .EQ. LOCPTR) GOTO 5000
        INDX=EXTVT (DBDTA1,INDX,2,ID,1)
C
C       SKIP AN ITEM IF ITS COMMON BLOCK IS LOCAL TO THE AP
C
        IF (IAND16 (INDX,IP16 (-16384)) .EQ. 0) GOTO 5000
        INDX=IAND16 (INDX,16383)
        CALL COMPOS (INDX,ADDR,POS,GROUP)
        J1=BLKID*1000+GROUP
C
C       PLACE VARIABLE NAMES IN THE DATA STATEMENT AS SPECIFIED BY THE
C       REPETITION COUNT.
C
        CALL RMVCS (STR,1,IPTR)
        DO 3000 J=1,RPTCNT
        J2=STR(1)+1
        WRITE (LUN,2810) J1,POS,(STR(K),K=2,J2),SLASH
2810    FORMAT (1X,6X,'DATA C',I5,'(',I5,')/',36A1)
        POS=POS+1
3000    CONTINUE
5000    CONTINUE
C
C       REWIND DBLUN AND WRITE OUT THE LAST DATA STATEMENT IF THERE
C       IS ONE
C
        RETURN
C
C------- ERROR RETURNS
C
C       READ ERROR
C
90000   CALL ERRMES (1)
        CALL EXIT
C
C       BAD RECORD
C
90020   CALL ERRMES (2)
        CALL WRTLIN (STR,-1,-1)
        CALL EXIT
C
C       UNEXPECTED EOF
C
90040   CALL ERRMES (4)
        CALL EXIT
        END
C+++ SRCBD
C****** SRCBD = OUTPUT THE DATA BLOCK PROGRAM = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE SRCBD (PPAAD)
C
        INTEGER PPAAD
C
C       CALL:
C
C       PPAAD  = THE ADDRESS OF THE PARAMETER PASSING AREA.
C
        INTEGER LUN
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)    LDTABS
        COMMON /LDTABS/FLMLUN,FLMFIL,LMLDSW,MXDATA,SUBCNT,              LDTABS
     *               CODPTR,LMHDRC                                      LDTABS
        INTEGER PRGDTA,PRGMAX,ENTDTA,ENTMAX,                            LDTABS
     *          EXTDTA,EXTMAX,EX1PTR,                                   LDTABS
     *          OLUN,RADIX,CLUN,MDLOW,PSLOW,                            LDTABS
     *          PSBRK,DBDTA1,DB1MAX,DBDTA2,DB2MAX,                      LDTABS
     *          DB2PTR,OVDTA,OVPTR,OVMAX,LRADIX,PSMAX,                  LDTABS
     *          DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0,EXTDT1,                 LDTABS
     *          EXTMX1,ENTDT1,ENTMX1,ENTPT1,LMLUN,                      LDTABS
     *          PARDTA,PARMAX,PARPTR,FLUN,VARSIZ,                       LDTABS
     *          CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,                   LDTABS
     *          OVPG,DBPG,BUFFER,BUFSIZ,PAKFAC,                         LDTABS
     *          DBLUN,EKOFLG,EKOLUN,CLUNS,CCLUN,CUROV,                  LDTABS
     *          NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536        LDTABS
        INTEGER SFFIL,SOFIL,SLMFIL,PGINFO                               LDTABS
        INTEGER SCLUN,SCLUN2,CLUN2                                      LDTABS
        INTEGER PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,PSHLD,OV1FLG   LDTABS
        INTEGER MAPLUN,NAMLUN,LP2LUN,SLP2NM                             LDTABS
        INTEGER FLMLUN,FLMFIL(31),LMLDSW,MXDATA,SUBCNT,                 LDTABS
     *           CODPTR,LMHDRC(8)                                       LDTABS
C                                                                       LDTABS
C
        LUN=FLUN+LUNMAP
        WRITE (LUN,110)
110     FORMAT (1X,6X,' BLOCK DATA'/
     *          1X,6X,' COMMON /APLDCM/ IPAV( 33),NU2,IDLM,NU1,',
     *                      'IPPAAD,IPPAND,IOVS(33),'/
     *          1X,4X,'*  LMT(10,3),LMTE'/
     *          1X,6X,' DATA NU2,IDLM,NU1,IPPAAD,IOVS(2),LMTE'/
     *          1X,4X,'*  /0,0,0,0,0,0/'/
     *          1X,6X,' END')
        RETURN
        END
C+++ SRCN
C****** SRCN = ADD EXECUTABLES TO THE HASI = REL.  1.01 , 12/10/79
C    ******* CROCK INITIALIZER FILE REV.  1.07 , 09/27/79 ********
C    *
C    *    THIS VERSION OF APLOAD WAS PRODUCED BY CROCK
C    *    ON TUE, JAN 08 1980 FOR THE PDP11 COMPUTER AT 16:26:32
C    *
C    *            CROCK VALUES FOR THE PDP11 COMPUTER
C    *
C    *  CHARACTERS PER WORD      =  1
C    *  CHARACTER SET            =  ASCII
C    *  CHARACTER STORAGE MODE   =  STRING
C    *  FORMAT                   =  1 BLANK(S)
C    *  ENDTBL SYMBOL            =  $END$
C    *
C    *************************************************************
C
        SUBROUTINE SRCN (DBSTRT,DBEND,INDX,HASIMD,LOCPTR)
C
        INTEGER DBSTRT,DBEND,INDX,HASIMD,LOCPTR
C
C       THIS ROUTINE CREATES AND PLACES EXECUTABLE STATEMENTS IN THE
C       HASI.  NAMELY THE IF STATEMENT AND APLMLD CALL, APPUT AND
C       APGET CALLS FOR COMMONS AND PARAMETERS, APOVLD CALL, APWD
C       CALLS, AND THE APRUN CALL.
C
C       CALL:
C
C       DBSTRT = INDEX INTO DBDTA0 WHERE THE CURRENT ROUTINES COMMON
C                POINTERS BEGIN.
C       DBEND  = INDEX INTO DBDTA0 WHERE THE CURRENT ROUTINES COMMON
C                POINTERS END.
C       INDX   = THE ENTDTA() INDEX FOR THE ENTRY POINT FOR THIS ROUTINE
C       HASIMD = 1 TO CREATE AN ADC HASI, 2 FOR UDC W/O COMMON, AND
C                3 FOR UDC W/ COMMON
C       LOCPTR = A POINTER TO THE LOCAL DATA BLOCK OF THE ROUTINE
C                CURRENTLY BEING LOADED.
C
C       COMMON:
C
C       OTOFLG = 1 IF THE AUTO OVLOAD FEATURE IS ON, ELSE 0 IF OFF,
C                THIS CONTROLS THE OUTPUT OF THE APOVLD CALL STMT.
C
C       LMLDSW - 0 IF CREATING DISK RESIDENT LOAD MODULE
C                1 IF CREATING HOST RESIDENT LOAD MODULE
C                2 IF CREATING BOTH
C
        INTEGER ID,IV,PARCNT,SYM(8),LINDX,IOFLG,IVV(2),ENTPT,LDBADR
        INTEGER LUN
        INTEGER EXTVT,EXTST,LDBMAK,SRCST
C
C                                                                       LDTABS
        COMMON /LDTABS/ PRGDTA (503),PRGMAX,ENTDTA(503),ENTMAX,         LDTABS
     *                EXTDTA(283),EXTMAX,EX1PTR,                        LDTABS
     *                OLUN,RADIX,CLUN,MDLOW,PSLOW,                      LDTABS
     *                PSBRK,DBDTA1(203),DB1MAX,DBDTA2(150,2),DB2MAX,    LDTABS
     *                DB2PTR,OVDTA(16,9),OVPTR,OVMAX,LRADIX,PSMAX,      LDTABS
     *                DBST,DBBRK,DB0PTR,DB0MAX,DBDTA0(70),              LDTABS
     *                EXTDT1(200),                                      LDTABS
     *                EXTMX1,ENTDT1(100),ENTMX1,ENTPT1,LMLUN,           LDTABS
     *                PARDTA(100),PARMAX,PARPTR,FLUN,VARSIZ(3),         LDTABS
     *                CNTMAX,PARADR,MAXPAR,OVID,OVFLG,LMID,             LDTABS
     *                OVPG,DBPG,BUFFER(200),BUFSIZ,PAKFAC,              LDTABS
     *                DBLUN,EKOFLG,EKOLUN,CLUNS(2),CCLUN,CUROV,         LDTABS
     *                NOTUS1,NOTUS2,NOTUS3,LUNMAP,OFFSET,EXTHST,M65536  LDTABS
        COMMON /LDTABS/ SFFIL(31),SOFIL(31),SLMFIL(31),PGINFO(16,2)     LDTABS
        COMMON /LDTABS/ SCLUN(16),SCLUN2(16),CLUN2                      LDTABS
        COMMON /LDTABS/ PPASZ,OTOFLG,OVLEVL,CALFLG,CMPFLG,RSTRCT,       LDTABS
     *                  PSHLD,OV1FLG,MAPLUN,NAMLUN,LP2LUN,SLP2NM(16)  D                                     REL.  1.00 , 09/01/79
ERRMES   OUTPUT ERROR MESSAGES                             REL.  1.00 , 09/01/79
WRTFST   FAST FILE WRITE                                   REL.  1.00 , 09/01/79
RDFST    FAST READ ROUTINE                                 REL.  1.00 , 09/01/79
TYPC     TYPE OF CHARACTER                                 REL.  1.00 , 09/01/79
================================== FILE  68 ==================================
VFC100   VECTOR FUNCTION CHAINER                           REL.  1.00 , 09/01/79
VFC100   VFC100 MAINLINE                                   REL.  1.00 , 09/01/79
FSP      SEE IF VARIABLE NAME IS AN S-PAD                  REL.  1.00 , 09/01/79
GETTAG   GET A TAG FROM A STRING                           REL.  1.00 , 09/01/79
FEOS     DETERMINE IF END OF STATEMENT                     REL.  1.00 , 09/01/79
================================== FILE  69 ==================================
SEQV1    ASM100 TEST ROUTINE FOR GLOBAL TEST              REL 0.0, AUG 79
================================== FILE  70 ==================================
SEQV2    ASM100 TEST ROUTINE FOR GLOBAL TEST              REL 0.0, AUG 79
================================== FILE  71 ==================================
INSERT   INSERT FOR SEQV1.S                               REL 0.0 , SEP 79
================================== FILE  72 ==================================
GLBTST   GLOBAL TEST FOR FPS100 PDS                        REL 0.0, AUG 79
PDSTST   GLOBAL TEST FOR FPS-100 PDS                       REL 0.0, AUG 79
================================== FILE  73 ==================================
================================== FILE  74 ==================================
GLBTST   GLOBAL TEST FOR FPS100 PDS                       REL 0.0, AUG 79
PDSTST   GLOBAL TEST FOR FPS-100 PDS                      REL 0.0, AUG 79
================================== FILE  75 ==================================
BAASRC   BASIC MATH LIBRARY (PART 1) /FAST/                REL 3.4, DEC 80
CVADD    COMPLEX VECTOR ADD /FAST/                         REL 3.0, NOV 78
CVSUB    COMPLEX VECTOR SUBTRACT /FAST/                    REL 3.0, NOV 78
CVMUL    COMPLEX VECTOR MULTIPLY /FAST/                    REL 3.1, SEP 79
CVMAGS   COMPLEX VECTOR MAGNITUDE SQUARED /FAST/           REL 3.1, SEP 79
CVCONJ   COMPLEX VECTOR CONJUGATE /FAST/                   REL 3.1, SEP 79
CVMA     COMPLEX VECTOR MULTIPLY AND ADD /FAST/            REL 3.1, SEP 79
SCJMA    SELF-CONJUGATE COMPLEX MULTIPLY /FAST/            REL 3.1, SEP 79
CDOTPR   COMPLEX DOT PRODUCT /FAST/                        REL 3.1, SEP 79
CVMOV    COMPLEX VECTOR MOVE /COMMON/                      REL 3.0, NOV 78
CVFILL   COMPLEX VECTOR FILL /COMMON/                      REL 3.1, SEP 79
CVCOMB   COMPLEX VECTOR COMBINE /COMMON/                   REL 3.0, NOV 78
CVREAL   FORM COMPLEX VECTOR OF REALS /COMMON/             REL 3.1, SEP 79
VREAL    EXTRACT REALS OF COMPLEX VECTOR /COMMON/          REL 3.0, NOV 78
VIMAG    EXTRACT IMAGINARIES OF COMPLEX VECTOR /COMMO      REL 3.0, NOV 78
CVNEG    COMPLEX VECTOR NEGATE /COMMON/                    REL 3.0, NOV 78
CVSMUL   COMPLEX VECTOR SCALAR MULTIPLY /COMMON/           REL 3.1, SEP 79
CVRCIP   COMPLEX VECTOR RECIPROCAL /COMMON/                REL 3.1, SEP 79
CRVADD   COMPLEX AND REAL VECTOR ADD /COMMON/              REL 3.0, NOV 78
CRVSUB   COMPLEX AND REAL VECTOR SUBTRACT /COMMON          REL 3.0, NOV 78
CRVMUL   COMPLEX AND REAL VECTOR MULTIPLY /COMMON          REL 3.0, NOV 78
CRVDIV   COMPLEX AND REAL VECTOR DIVIDE /COMMON/           REL 3.0, NOV 78
POLAR    RECTANGULAR TO POLAR CONVERSION /COMMON/          REL 3.1, SEP 79
RECT     POLAR TO RECTANGULAR CONVERSION /COMMON/          REL 3.3, DEC 80
CVEXP    COMPLEX VECTOR EXPONENTIAL /COMMON/               REL 3.3, DEC 80
CVMEXP   COMPLEX VECTOR MULTIPLY EXPONENTIAL /COMMON/      REL 3.3, DEC 80
VCLR     VECTOR CLEAR /FAST/                               REL 3.2, AUG 80
VMOV     VECTOR MOVE /FAST/                                REL 3.1, SEP 79
VSWAP    VECTOR SWAP /FAST/                                REL 3.2, MAR 80
VNEG     VECTOR NEGATE /FAST/                              REL 3.1, SEP 79
VADD     VECTOR ADD /FAST/                                 REL 3.1, SEP 79
VSUB     VECTOR SUBTRACT /FAST/                            REL 3.1, SEP 79
VMUL     VECTOR MULTIPLY /FAST/                            REL 3.1, SEP 79
VSADD    VECTOR-SCALAR ADD /FAST/                          REL 3.1, SEP 79
VSMUL    VECTOR-SCALAR MULTIPLY /FAST/                     REL 3.1, SEP 79
VTSADD   VECTOR TABLE SCALAR ADDITION /FAST/               REL 3.1, SEP 79
VSSQ     VECTOR SIGNED SUM OF SQUARES /FAST/               REL 3.1, SEP 79
VABS     VECTOR ABSOLUTE VALUE /FAST/                      REL 3.1, SEP 79
VMA      VECTOR MULTIPLY AND ADD /FAST/                    REL 3.1, SEP 79
VMSB     VECTOR MULTIPLY AND SUBTRACT /FAST/               REL 3.1, SEP 79
VMSA     VECTOR MULTIPLY AND SCALAR ADD /FAST/             REL 3.1, SEP 79
VSMA     VECTOR SCALAR MULTIPLY AND ADD /FAST/             REL 3.1, SEP 79
VSMSB    VECTOR SCALAR MULTIPLY AND SUB /FAST/             REL 3.1, SEP 79
VAM      VECTOR ADD AND MULTIPLY /FAST/                    REL 3.1, SEP 79
VSBM     VECTOR SUBTRACT AND MULTIPLY /FAST/               REL 3.1, SEP 79
VSMSA    VECTOR-SCALAR MULTIPLY, SCALAR ADD /FAST/         REL 3.1, SEP 79
VMMA     VECTOR MULTIPLY, MULTIPLY, ADD /FAST/             REL 3.1, SEP 79
VMMSB    VECTOR MULTIPLY, MULTIPLY, SUBTRACT /FAST/        REL 3.1, SEP 79
VAAM     VECTOR ADD, ADD, MULTIPLY /FAST/                  REL 3.1, SEP 79
VSBSBM   VECTOR SUBTRACT, SUBTRACT, MULTIPLY /FAST/        REL 3.1, SEP 79
VAND     VECTOR LOGICAL AND /FAST/                         REL 3.1, SEP 79
VEQV     VECTOR LOGICAL EQUIVALENCE /FAST/                 REL 3.1, SEP 79
VOR      VECTOR LOGICAL OR /FAST/                          REL 3.1, SEP 79
VINDEX   VECTOR INDEX /COMMON/                             REL 3.1, SEP 79
VRVRS    VECTOR REVERSER, IN PLACE /COMMON/                REL 3.1, AUG 80
VFILL    VECTOR FILL WITH CONSTANT /COMMON/                REL 3.2, AUG 80
VRAMP    VECTOR FILL WITH RAMP /COMMON/                    REL 3.1, SEP 79
VDIV     VECTOR DIVIDE FOR EITHER MEMORY /COMMON/          REL 3.1, SEP 79
VTSMUL   VECTOR TABLE SCALAR MULTIPLY /COMMON/             REL 3.1, SEP 79
VSQ      VECTOR SQUARE /COMMON/                            REL 3.1, SEP 79
VSQRT    VECTOR SQUARE ROOT FOR EITHER MEMORY /COMMON      REL 3.1, SEP 79
VLOG     VECTOR LOGARITHM (BASE 10) /COMMON/               REL 3.1, SEP 79
VLN      VECTOR LOGARITHM (NATURAL) /COMMON/               REL 3.1, SEP 79
VALOG    VECTOR ANTILOGARITHM (BASE 10)                    REL 3.1, SEP 79
VEXP     VECTOR EXPONENTIAL /COMMON/                       REL 3.1, SEP 79
VSIN     VECTOR SINE /COMMON/                              REL 3.2, MAR 80
VCOS     VECTOR COSINE /COMMON/                            REL 3.2, MAR 80
VATAN    VECTOR ARC-TANGENT /COMMON/                       REL 3.1, SEP 79
VATN2    VECTOR ARC-TANGENT OF 2 ARGUEMENTS /COMMO         REL 3.1, SEP 79
VRAND    VECTOR RANDOM NUMBERS /COMMON/                    REL 3.2, MAR 80
VINT     VECTOR TRUNCATE TO INTEGER (WHOLE NUM) /COMMON    REL 3.1, SEP 79
VFRAC    VECTOR TRUNCATE TO FRACTION /COMMON/              REL 3.1, SEP 79
DOTPR    DOT-PRODUCT /FAST/                                REL 3.2, AUG 80
RMSQV    ROOT-MEAN-SQUARE VALUE OF VECTOR ELEM /COMMON     REL 3.1, SEP 79
MEANV    MEAN VALUE OF VECTOR ELEMENTS /COMMON/            REL 3.1, SEP 79
MEAMGV   MEAN OF VECTOR ELEMENT MAGNITUDES /COMMON/        REL 3.1, SEP 79
MEASQV   MEAN OF VECTOR ELEMENT SQUARES /COMMON/           REL 3.1, SEP 79
SVE      SUM OF VECTOR ELEMENTS /COMMON/                   REL 3.1, SEP 79
SVEMG    SUM OF VECTOR ELEMENT MAGNITUDES /COMMON/         REL 3.1, SEP 79
SVESQ    SUM OF VECTOR ELEMENT SQUARES /COMMON/            REL 3.1, SEP 79
SVS      SUM OF VECTOR SQUARES /COMMON/                    REL 3.1, SEP 79
MAXV     MAXIMUM ELEMENT OF A VECTOR /COMMON/              REL 3.2, MAR 80
MINV     MINIMUM ELEMENT OF A VECTOR /COMMON/              REL 3.2, MAR 80
MAXMGV   MAXIMUM MAGNITUDE ELEMENT OF A VECTOR /COMMO      REL 3.2, MAR 80
MINMGV   MINIMUM MAGNITUDE ELEMENT OF A VECTOR /COMMO      REL 3.2, MAR 80
XMAXV    MAXIMUM ELEMENT OF A VECTOR /COMMON/              REL 3.0, MAR 80
XMINV    MINIMUM ELEMENT OF A VECTOR /COMMON/              REL 3.0, MAR 80
XMAXMG   MAXIMUM MAGNITUDE ELEMENT OF A VECTOR /COMMO      REL 3.0, MAR 80
XMINMG   MINIMUM MAGNITUDE ELEMENT OF A VECTOR /COMMO      REL 3.0, MAR 80
================================== FILE  76 ==================================
LIST OF FORTRAN CALLABLE ROUTINES IN THE FAST MEMORY VERSION OF
'BAALIB', THE BASIC MATH LIBRARY (PART 1)
================================== FILE  77 ==================================
================================== FILE  78 ==================================
HOST SOURCE FOR THE FAST MEMORY, 4.5K TMROM VERSION OF THE
BASIC MATH LIBRARY (PART 1) SUBROUTINES
================================== FILE  79 ==================================
BABSRC   BASIC MATH LIBRARY (PART 2) /FAST/                REL 3.3, AUG 80
VMAX     VECTOR MAXIMUM /FAST/                             REL 3.1, SEP 79
VMIN     VECTOR MINIMUM /FAST/                             REL 3.1, SEP 79
LVGT     LOGICAL VECTOR GREATER THAN /FAST/                REL 3.1, SEP 79
LVGE     LOGICAL VECTOR GREATER OR EQUAL /FAST/            REL 3.1, SEP 79
LVEQ     LOGICAL VECTOR EQUAL /FAST/                       REL 3.1, SEP 79
LVNE     LOGICAL VECTOR NOT EQUAL /FAST/                   REL 3.1, SEP 79
LVNOT    LOGICAL VECTOR NOT  /FAST/                        REL 3.1, SEP 79
VLMERG   VECTOR LOGICAL MERGE /FAST/                       REL 3.1, SEP 79
VMAXMG   VECTOR MAX MAGNITUDE OF TWO VECTORS /COMMON/      REL 3.1, SEP 79
VMINMG   VECTOR MIN MAGNITUDE OF TWO VECTORS /COMMON/      REL 3.1, SEP 79
VCLIP    VECTOR CLIP /COMMON/                              REL 3.1, SEP 79
VICLIP   VECTOR INVERTED CLIP /COMMON/                     REL 3.1, SEP 79
VLIM     VECTOR LIMIT /COMMON/                             REL 3.1, SEP 79
VFIX     VECTOR FIX  /FAST/                                REL 3.1, SEP 79
VFLT     VECTOR FLOAT /FAST/                               REL 3.1, SEP 79
VSMAFX   VECTOR SCALAR MULTIPLY, ADD AND FIX /FAST/        REL 3.1, AUG 80
VSEFLT   VECTOR SIGN EXTEND AND FLOAT /COMMON/             REL 3.1, SEP 79
VSHFX    VECTOR SHIFT AND FIX /COMMON/                     REL 3.1, SEP 79
VSCALE   VECTOR SCALE (POWER 2) AND FIX /COMMON/           REL 3.2, MAR 80
VSCSCL   VECTOR SCAN, SCALE (POWER 2) AND FIX /COMMO       REL 3.2, MAR 80
XVSCAL   VECTOR SCALE (POWER 2) AND FIX /COMMON/           REL 3.0, MAR 80
XVSCSC   VECTOR SCAN, SCALE (POWER 2) AND FIX /COMMO       REL 3.0, MAR 80
VFLT32   VECTOR 32-BIT INTEGER FLOAT /COMMON/              REL 3.2, AUG 80
VFIX32   VECTOR 32-BIT INTEGER FIX /COMMON/                REL 3.2, AUG 80
VUP16    VECTOR 16-BIT BYTE UNPACK /COMMON/                REL 3.2, AUG 80
VUPS16   VECTOR 16-BIT SIGNED BYTE UNPACK /COMMON/         REL 3.2, AUG 80
VPK16    VECTOR 16-BIT BYTE PACK /COMMON/                  REL 3.2, AUG 80
VUP8     VECTOR 8-BIT BYTE UNPACK /COMMON/                 REL 3.2, AUG 80
VUPS8    VECTOR 8-BIT SIGNED BYTE UNPACK /COMMON/          REL 3.2, AUG 80
VPK8     VECTOR 8-BIT BYTE PACK /COMMON/                   REL 3.2, AUG 80
MTRANS   MATRIX TRANSPOSE /FAST/                           REL 3.1, SEP 79
SOLVEQ   LINEAR EQUATION SOLVER /FAST/                     REL 3.2, MAR 80
XSOLVE   LINEAR EQUATION SOLVER /FAST/                     REL 3.0, MAR 80
FMMM     FAST MEMORY MATRIX MULTIPLY /FAST/                REL 3.1, SEP 79
FMMM32   FAST MEMORY MATRIX MULT (32 OR LESS) /FAST/       REL 3.1, SEP 79
MMUL     MATRIX MULTIPLY /COMMON/                          REL 3.1, SEP 79
MMUL32   MATRIX MULTIPLY, 32 X 32 OR LESS /COMMON          REL 3.1, SEP 79
MVML3    MATRIX-VECTOR MULTIPLY (3 X 3) /COMMON/           REL 3.1, SEP 79
MVML4    MATRIX-VECTOR MULTIPLY (4 X 4) /COMMON/           REL 3.2, AUG 80
MATINV   MATRIX INVERSE (GAUSSIAN ELIMINATION) /COMMO      REL 3.2, MAR 80
XMATIN   MATRIX INVERSE (GAUSSIAN ELIMINATION) /COMMO      REL 3.0, MAR 80
CTRN3    3-DIMENSIONAL COORDINATE TRANS /COMMON/           REL 3.1, SEP 79
RFFT     REAL FFT (IN PLACE) /COMMON/                      REL 3.1, SEP 79
RFFTB    REAL FFT (NOT IN PLACE) /COMMON/                  REL 3.1, SEP 79
CFFT     COMPLEX FFT (IN PLACE) /COMMON/                   REL 3.1, SEP 79
CFFTB    COMPLEX FFT (NOT IN PLACE) /COMMON/               REL 3.1, SEP 79
RFFTSC   REAL FFT SCALE AND/OR FORMAT /COMMON/             REL 3.2, AUG 80
CFFTSC   COMPLEX FFT SCALE /COMMON/                        REL 3.1, AUG 80
CONV     CONVOLUTION /COMMON/                              REL 3.1, SEP 79
VPOLY    VECTOR POLYNOMIAL EVALUATE /COMMON/               REL 3.1, SEP 79
DEQ22    DIFFERENCE EQUATION, 2-POLES, 2 ZEROS /COMMO      REL 3.0, NOV 78
VSUM     RUNNING SUM INTEGRATION /COMMON/                  REL 3.2, AUG 80
VTRAPZ   TRAPEZOIDAL INTEGRATION /COMMON/                  REL 3.1, SEP 79
VSIMPS   SIMPSON INTEGRATION /COMMON/                      REL 3.1, SEP 79
SETC5    SET CONTROL BIT 5 INTERRUPT /COMMON/              REL 3.0  , NOV 78
RDC5     READ CONTROL BIT 5 INTERRUPT /COMMON/             REL 3.0  , NOV 78
DAREAD   READ DEVICE ADDRESS REGISTER /COMMON/             REL 3.3, AUG 80
XDAREA   READ DEVICE ADDRESS REGISTER /COMMON/             REL 3.1, AUG 80
DAWRIT   WRITE DEVICE ADDRESS REGISTER /COMMON/            REL 3.1  , AUG 80
MDCOM    MAIN DATA FLOATING COMPARE /COMMON/               REL 3.0  , NOV 78
================================== FILE  80 ==================================
LIST OF FORTRAN CALLABLE ROUTINES IN THE FAST MEMORY VERSION OF
'BABLIB', THE BASIC MATH LIBRARY (PART 2)
================================== FILE  81 ==================================
LIBRARY (APAL-OBJECT) FOR FAST MEMORY BASIC LIBRARY (PART 2)
================================== FILE  82 ==================================
HOST SOURCE FOR THE FAST MEMORY, 4.5K TMROM VERSION OF THE
BASIC MATH LIBRARY (PART 2) SUBROUTINES
================================== FILE  83 ==================================
UTLSRC   UTILITY LIBRARY /FAST/                            REL 3.5, DEC 80
FLUSH    FLUSH PIPELINES, SAVE STATUS /COMMON/             REL 2.5, DEC 80
XRFFT    EXPANDED REAL FFT /COMMON/                        REL 2.2, SEP 78
XCFFT    EXPANDED COMPLEX FFT /COMMON/                     REL 3.0, NOV 78
XBITRE   EXPANDED BIT REVERSE /COMMON/                     REL 2.2, SEP 78
PCFFT    PARTIAL  COMPLEX FFT /COMMON/                     REL 3.0, NOV 78
XFFT4    EXPANDED RADIX FFT PASS /FAST/                    REL 2.2, SEP 78
XREALT   EXPANDED REAL FFT PASS /FAST/                     REL 3.0, NOV 78
RTOC     REAL TO COMPLEX SCRAMBLE / COMMON /               REL 2.3, MAR 80
CTOR     COMPLEX TO REAL UNSCRAMBLE / COMMON /             REL 2.3, MAR 80
BITREV   BIT REVERSE PASS /FAST/                           REL 2.0  , JAN 78
REALTR   REAL FFT PASS /FAST/                              REL 3.0, NOV 78
FFT2B    RADIX 2 1ST FFT PASS & BIT-REVERSE /FAST/         REL 2.0  , JAN 78
FFT4B    RADIX 4 FFT 1ST PASS & BIT-REVERSE /FAST/         REL 2.0  , JAN 78
FFT2     RADIX 2 FFT 1ST PASS /FAST/                       REL 2.0  , JAN 78
FFT4     RADIX 4 FFT PASS /FAST/                           REL 2.0  , JAN 78
STSTAT   (STATUS) SET FFT STATUS REGISTERS /COMMON/        REL 2.0  , JAN 78
CLSTAT   (STATUS) CLEAR FFT STAT REGISTERS /COMMON/        REL 2.0  , JAN 78
ILOG2    (STATUS) LOGARITHM BASE 2 /COMMON/                REL 2.0  , JAN 78
ADV4     (ADV) ADVANCE: RADIX 4 TO RADIX 4 PASS /COMMO     REL 2.0  , JAN 78
ADV2     (ADV) ADVANCE: RADIX 2 TO RADIX 4 PASS /COMMO     REL 2.0  , JAN 78
SET24B   SETUP FOR FFT2B AND FFT4B /COMMON/                REL 2.0  , JAN 78
VFCL1    VECTOR FUNCTION CALLER (1 ARG) /COMMON/           REL 2.1, SEP 79
VFCL2    VECTOR FUNCTION CALLER (2 ARGS) /COMMON/          REL 2.1, SEP 79
SPFLT    S-PAD FLOAT /COMMON/                              REL 2.0  , JAN 78
SPUFLT   S-PAD UNSIGNED FLOAT /COMMON/                     REL 2.2, SEP 78
SAVESP   SAVE S-PAD REGISTERS INTO P.S. /COMMON/           REL 2.2  , DEC 80
SAVSP0   SAVE S-PAD 0 INTO PROGRAM MEMORY /COMMON/         REL 2.1  , MAR 78
SETSP    LOAD S-PAD FROM P.S. /COMMON/                     REL 2.2  , AUG 80
SET2SP   LOAD 2 S-PADS FROM P.S. /COMMON/                  REL 2.1  , AUG 80
SPNEG    SPAD NEGATE /COMMON/                              REL 2.0  , JAN 78
SPNOT    SPAD NOT /COMMON/                                 REL 2.0  , JAN 78
SPADD    SPAD ADD /COMMON/                                 REL 2.0  , JAN 78
SPSUB    SPAD SUBTRACT /COMMON/                            REL 2.0  , JAN 78
SPRS     SPAD RIGHT SHIFT /COMMON/                         REL 2.0  , JAN 78
SPLS     SPAD LEFT SHIFT /COMMON/                          REL 2.0  , JAN 78
SPAND    SPAD AND /COMMON/                                 REL 2.0  , JAN 78
SPOR     SPAD OR /COMMON/                                  REL 2.0  , JAN 78
SSDM     SINGLE SINGLE DOUBLE PRECISION MULTIPLY /COMMO    REL 2.0  , JAN 78
DDDM     DOUBLE PRECISION MULTIPLY /COMMON/                REL 2.0  , JAN 78
SSDA     SINGLE SINGLE DOUBLE PRECISION ADDITION /COMMO    REL 2.0  , JAN 78
SDDA     SINGLE DOUBLE DOUBLE PRECISION ADDITION /COMMO    REL 2.0  , JAN 78
DDDA     DOUBLE PRECISION ADDITION /COMMON/                REL 2.0  , JAN 78
APNOP    DO A NOP, A DUMMY ROUTINE /COMMON/                REL 2.0, MAY 79
================================== FILE  84 ==================================
LIST OF FORTRAN CALLABLE ROUTINES IN THE FAST MEMORY VERSION OF
'UTLLIB', THE UTILITY LIBRARY
================================== FILE  85 ==================================
LIBRARY (APAL-OBJECT) FOR FAST MEMORY UTILITY LIBRARY
================================== FILE  86 ==================================
HOST SOURCE FOR THE FAST MEMORY, 4.5K TMROM VERSION OF THE
UTILITY LIBRARY SUBROUTINES
================================== FILE  87 ==================================
APFSRC   SCALAR MATH AND APFTN 1.0 LIBRARY /FAST/          REL 3.3, DEC 80
RESLVE   RESOLVE PARAMETERS FROM APFTN /COMMON/            REL 3.0, NOV 78
FUNSAV   REGISTER SAVE FOR GENERAL FUNCTION /COMMON/       REL 3.0, NOV 78
FUNRES   REGISTER RESTORE FOR GENERAL FUNCTION /COMMON/    REL 3.0, NOV 7
SAVE     REGISTER SAVE /COMMON/                            REL 3.0, NOV 78
RESTOR   REGISTER RESTORE /COMMON/                         REL 3.0, NOV 78
XSAV     GLOBAL S-PAD SAVE /COMMON/                        REL 3.0, NOV 78
XRST     GLOBAL REGISTER RESTORE  /FAST/                   REL 3.0, NOV 78
MAXMIN   BASIC FORTRAN MAX AND MIN FUNCTIONS /COMMON/      REL 3.0, NOV 78
SPDIV    SPAD DIVIDE /COMMON/                              REL 2.2  , SEP 78
SPMUL    SPAD MULTIPLY /COMMON/                            REL 2.0  , JAN 78
SQRT     SCALAR SQUARE ROOT /COMMON/                       REL 3.0  , NOV 78
SIN      (SINCOS) SCALAR SINE FUNCTION /COMMON/            REL 2.3, DEC 80
COS      (SINCOS) SCALAR COSINE FUNCTION /COMMON/          REL 2.3, DEC 80
ATAN     SCALAR ARCTANGENT (1 ARG) /COMMON/                REL 2.0  , JAN 78
ATN2     SCALAR ARCTANGENT (2 ARGS) /COMMON/               REL 2.0  , JAN 78
IABS     INTEGER ABSOLUTE VALUE /COMMON/                   REL 3.0, NOV 78
ABS      FLOATING-POINT ABSOLUTE VALUE /COMMON/            REL 3.0, NOV 78
IDIM     INTEGER POSITIVE DIFFERENCE /COMMON/              REL 3.0, NOV 78
DIM      FLOATING-POINT POSITIVE DIFFERENCE /COMMON/       REL 3.0, NOV 78
ISIGN    INTEGER TRANSFER OF SIGN /COMMON/                 REL 3.0, NOV 78
SIGN     FLOATING-POINT TRANSFER OF SIGN /COMMON/          REL 3.0, NOV 78
MOD      INTEGER REMAINDERING /COMMON/                     REL 3.0, NOV 78
AMOD     FLOATING-POINT REMAINDERING /COMMON/              REL 3.1, DEC 79
AINT     FLTING-PNT TO FLTING-PNT TRUNCATION /COMMON/      REL 3.0, NOV 78
TANH     FLOATING-POINT HYPERBOLIC TANGENT /COMMON/        REL 3.0, NOV 78
COSH     FLOATING-POINT HYPERBOLIC COSINE /COMMON/         REL 3.1, APR 79
SINH     FLOATING-POINT HYPERBOLIC SINE /COMMON/           REL 3.0, NOV 78
IEXPI    INTEGER TO INTEGER EXPONENTIAL /COMMON/           REL 3.0, NOV 78
REXPI    FLOATING-POINT TO INTEGER EXPONENTIAL /COMMON/    REL 3.0, NOV 78
REXPR    FLTING-PNT TO FLTING-PNT EXPONENTIAL /COMMON/     REL 3.0, NOV 78
LOG      (LOG) SCALAR LOGARITHM BASE 10 /COMMON/           REL 2.0  , JAN 78
LN       (LOG) SCALAR LOGARITHM BASE E /COMMON/            REL 2.0  , JAN 78
EXP      SCALAR EXPONENTIAL /COMMON/                       REL 2.0  , JAN 78
IFIX     FLOATING-POINT TO INTEGER CONVERSION /COMMON/     REL 3.0, MAR 79
INT      FLOATING-POINT TO INTEGER TRUNCATION /COMMON/     REL 3.0, MAR 79
FLOAT    INTEGER TO FLOATING-POINT CONVERSION /COMMON/     REL 3.0, NOV 78
FAPUT    STORE ADDER RESULT IN DATA-PAD /COMMON/           REL 3.0, NOV 78
FAPUSH   PUSH OUT ADDER RESULT /COMMON/                    REL 3.0, NOV 78
DIV      (DIVIDE) SCALAR DIVIDE /COMMON/                   REL 2.1, SEP 79
================================== FILE  88 ==================================
LIBRARY (APAL-OBJECT) FOR FAST MEMORY AP-FORTRAN SUPPORT ROUTINES
================================== FILE  89 ==================================
SIGSRC   SIGNAL PROCESSING LIBRARY /FAST/                  REL 3.3, DEC 80
VAVLIN   VECTOR LINEAR AVERAGING /FAST/                    REL 3.2, AUG 80
VAVEXP   VECTOR EXPONENTIAL AVERAGING /FAST/               REL 3.2, AUG 80
HIST     HISTOGRAM /COMMON/                                REL 3.1, SEP 79
HANN     HANNING WINDOW OF VECTOR /COMMON/                 REL 3.1, SEP 79
VDBPWR   VECTOR CONVERSION TO DB (POWER)  /COMMON/         REL 3.1, SEP 79
ASPEC    ACCUMUMULATING AUTO-SPECTRUM /COMMON/             REL 3.1, SEP 79
CSPEC    ACCUMULATING CROSS-SPECTRUM /COMMON/              REL 3.2, AUG 80
TRANS    TRANSFER FUNCTION /COMMON/                        REL 3.1, SEP 79
COHER    COHERENCE FUNCTION /COMMON/                       REL 3.1, SEP 79
ACORT    AUTO-CORRELATION USING TIME-DOMAIN /COMMON/       REL 3.1, SEP 79
CCORT    CROSS-CORRELATION USING TIME-DOMAIN /COMMON       REL 3.1, SEP 79
ACORF    AUTO-CORRELATION (FREQUENCY-DOMAIN) /COMMO        REL 3.1, SEP 79
CCORF    CROSS-CORRELATION (FREQUENCY-DOMAIN) /COMMON      REL 3.1, SEP 79
TCONV    POST-TAPERED CONVOLUTION /COMMON/                 REL 3.1, SEP 79
DECFIR   CONVOLUTION WITH DECIMATION /COMMON/              REL 3.0, SEP 79
ENVEL    ENVELOPE DETECTOR /COMMON/                        REL 3.0, SEP 79
HLBRT    HILBERT TRANSFORMER /COMMON/                      REL 3.0, SEP 79
PKVAL    PEAK AND VALLEY PICKER /COMMON/                   REL 3.1, AUG 80
VXCS     VECTOR TIMES COS AND SIN (TABLE LOOKUP)/COMMON/   REL 3.0, JUL 79
SHPHU    SCHAFER'S PHASE UNWRAPING /COMMON/                REL 3.0, SEP 79
UNWRAP   ADAPTIVE PHASE UNWRAPPING /COMMON/                REL 3.2, DEC 80
CPSTRM   COMPLEX CEPSTRUM /COMMON/                         REL 3.2, DEC 80
UNWCOR   UNWRAP & CPSTRM CORE CODE /COMMON/                REL 3.1, DEC 80
RDFT     REAL DISCRETE FOURIER TRANSFORM /COMMON/          REL 3.0, SEP 79
WIENER   WIENER LEVINSON ALGORITHM /COMMON/                REL 3.0, SEP 79
LPAUTO   LINEAR PREDICTION AUTOCORRELATION /COMMON/        REL 3.0, AUG 80
CFFTI    COMPLEX FFT WITH INTERPOLATION /COMMON/           REL 3.0, AUG 80
RFFTI    REAL FFT WITH INTERPOLATION /COMMON/              REL 3.0, AUG 80
BITRVI   BIT REVERSE FOR CFFTI /FAST/                      REL 3.0, AUG 80
IFFT4    FINAL RADIX-4 FOR CFFTI /COMMON/                  REL 3.0, AUG 80
IFFT4G   IFFT4 COMPUTATION KERNEL /COMMON/                 REL 3.0, AUG 80
IREALT   RFFTI UNRAVEL /COMMON/                            REL 3.0, AUG 80
================================== FILE  90 ==================================
LIST OF FORTRAN CALLABLE ROUTINES IN THE FAST MEMORY VERSION OF
'SIGLIB', THE SIGNAL PROCESSING LIBRARY
================================== FILE  91 ==================================
LIBRARY (APAL-OBJECT) FOR FAST MEMORY SIGNAL PROCESSING LIBRARY
================================== FILE  92 ==================================
HOST SOURCE FOR THE FAST MEMORY, 4.5K TMROM VERSION OF THE
SIGNAL PROCESSING LIBRARY SUBROUTINES
================================== FILE  93 ==================================
AMLSRC   ADVANCED MATH LIBRARY /FAST/                      REL 1.2, JUN 80
FGEN     FUNCTION GENERATOR /COMMON/                       REL 1.0, APR 79
FGENT    FUNCTION GENERATOR (NORMALIZED BP) /COMMON/       REL 1.0, APR 79
BIN      BINARY SEARCH /COMMON/                            REL 1.0, APR 79
STEP     STEP SEARCH /COMMON/                              REL 1.0, APR 79
FUN1     FUNCTIONS OF ONE VARIABLE  /FAST/                 REL 1.0, APR 79
FUN2     FUNCTIONS OF TWO VARIABLES /FAST/                 REL 1.0, APR 79
FUN3     FUNCTIONS OF THREE VARIABLES /FAST/               REL 1.0, APR 79
FUN4     FUNCTIONS OF FOUR VARIABLES /FAST/                REL 1.0, APR 79
EIGRS    EIGENVALUES, REAL SYMMETRIC /COMMON/              REL 1.0, APR 79
TRED2    TRIDIAGONALIZE SYMMETRIC MATRIX /FAST/            REL 1.2, JUN 80
IMTQL2   EIGENVALUES AND VECTOR, SYMMETIC /FAST/           REL 1.2, JUN 80
UDOTPR   UTILITY DOT PRODUCT /FAST/                        REL 1.1, JUN 80
INDEX    SETUP INDICES FOR TRED2, IMTQL2 /FAST/            REL 1.0, APR 79
RKGIL    RUNGE-KUTTA-GILL'S ODE INTEG /FAST/               REL 1.1, JUN 80
SKYSOL   SKYLINE EQUATION SOLVER /COMMON/                  REL 1.1, OCT 79
SCSFB    SPARSE CMPLX SYMM FWD ELIM BACK SUBS/COMMON/      REL 1.1, OCT 79
SCUFB    SPARSE CMPLX UNSYMM FWD ELIM BACK SUB/COMMON/     REL 1.1, OCT 79
SCFWD    SPARSE COMPLEX MATRIX FORWARD ELIM /COMMON/       REL 1.1, OCT 78
SCBAK    SPARSE COMPLEX BACK SUBSTITUTION /COMMON/         REL 1.1, JAN 79
SRSFB    SPARSE REAL SYMM FWD ELIM BACK SUB /COMMON/       REL 1.1, OCT 79
SRUFB    SPARSE REAL UNSYMM FWD ELIM BACK SUB/COMMON/      REL 1.1, OCT 79
SRFWD    SPARSE REAL MATRIX FORWARD ELIM /COMMON/          REL 1.1, OCT 79
SRBAK    SPARSE REAL BACK SUBSTITUTION /COMMON/            REL 1.0, JAN 79
TSCSFB   SPARSE SYMM CMPLX FWD ELIM BACK SUB/COMMON/       REL 1.1, OCT 79
TSCUFB   SPARSE CMPLX UNSYM FWD ELIM,BACK SUB/COMMON/      REL 1.1, OCT 79
TSCFWD   SPARSE COMPLEX MATRIX FORWARD ELIM /COMMON/       REL 1.0, JUN 78
TSCBAK   SPARSE COMPLEX BACK SUBSTITUTION /COMMON/         REL 1.1, JAN 79
TSRSFB   SPARSE REAL SYMM FWD ELIM BACK SUB/COMMON/        REL 1.1, OCT 79
TSRUFB   SPARSE REAL UNSYM FWD ELIM BACK SUB/COMMON/       REL 1.1, OCT 79
TSRFWD   SPARSE REAL MATRIX FORWARD ELIM /COMMON/          REL 1.0, JAN 79
TSRBAK   SPARSE REAL BACK SUBSTITUTION /COMMON/            REL 1.0, JAN 79
TCVMUL   COMPLEX VECTOR MULTIPLY WITH TMRAM /COMMON/       REL 1.1, OCT 79
================================== FILE  94 ==================================
LIST OF FORTRAN CALLABLE ROUTINES IN THE FAST MEMORY VERSION OF
'AMLLIB', THE ADVANCED MATH LIBRARY
================================== FILE  95 ==================================
LIBRARY (APAL-OBJECT) FOR FAST MEMORY ADVANCED MATH LIBRARY
================================== FILE  96 ==================================
HOST SOURCE FOR THE FAST MEMORY, 4.5K TMROM VERSION OF THE
ADVANCED MATH LIBRARY SUBROUTINES
================================== FILE  97 ==================================
IPRSRC   IMAGE PROCESSING LIBRARY /FAST/                   REL 1.2, DEC 80
GRAD2D   2-D GRADIENT FILTER /COMMON/                      REL 1.0, APR 79
GRD2DB   2-D GRADIENT FILTER WITH BDRY TEST /FAST/         REL 1.0, APR 79
LAPL2D   2-D LAPLACIAN FILTER /COMMON/                     REL 1.0, APR 79
LPL2DB   2-D LAPLACIAN FILTER WITH BDRY TEST /FAST/        REL 1.0, APR 79
MED2D    2-D MEDIAN FILTER /COMMON/                        REL 1.1, DEC 79
CONV2D   2-D CONVOLUTION (OR CORRELATION) /COMMON/         REL 1.1, JAN 80
MOVREP   SUB-IMAGE MOVE OR INTENSITY REPLACE /COMMON/      REL 1.1, DEC 80
RFFT2D   REAL TWO DIMENSIONAL FFT / COMMON /               REL 2.2, SEP 78
CFFT2D   COMPLEX 2D FFT /COMMON/                           REL 2.2, SEP 78
ERFFT2   EXTENDED MEMORY REAL 2D FFT / COMMON /            REL 1.0, APR 79
ECFFT2   EXTENDED MEMORY COMPLEX 2D FFT / COMMON /         REL 1.0, APR 79
ECVMOV   EXTENDED COMPLEX VECTOR MOVE /FAST/               REL 1.0, APR 79
UNESPF   UNNORM EXTENDED S-PAD FLOAT /COMMON/              REL 2.0  , JAN 78
ESPFLT   EXTENDED S-PAD FLOAT /COMMON/                     REL 2.0  , JAN 78
USPFLT   UNSIGNED S-PAD FLOAT /COMMON/                     REL 2.0  , JAN 78
================================== FILE  98 ==================================
LIST OF FORTRAN CALLABLE ROUTINES IN THE FAST MEMORY VERSION OF
'IPRLIB', THE IMAGE PROCESSING LIBRARY
================================== FILE  99 ==================================
LIBRARY (APAL-OBJECT) FOR FAST MEMORY IMAGE PROCESSING LIBRARY
================================== FILE 100 ==================================
HOST SOURCE FOR THE FAST MEMORY, 4.5K TMROM VERSION OF THE
IMAGE PROCESSING LIBRARY SUBROUTINES
================================== FILE 101 ==================================
COMMAND FILE TO LOAD SUPER 100
================================== FILE 102 ==================================
BASIC SUPER-100 COMMON
================================== FILE 103 ==================================
SYSDEF   SYSTEM DEFINITIONS                                REL 0.0  , AUG 79
================================== FILE 104 ==================================
CONFIGURATION COMMON DEFINITION
================================== FILE 105 ==================================
FHOSTC   HOST INTERFACE COMMON DEFN.                       REL 0.0 , JUL 79
================================== FILE 106 ==================================
SYSCOM   SYSTEM COMMON INITIALIZATIONS (BINARY VERSION)    REL 0.0  , AUG 79
CONFIG   I/O DEVICE CONFIGURATION TABLE (BINARY VER)       REL 0.0  , AUG 79
================================== FILE 107 ==================================
NORTC    REAL TIME CLOCK MODULES (BINARY FILE)             REV 0.0, AUG 79
ADDCLK   ADD ELEMENT TO RTC QUEUE                          REV 0.0, AUG 79
RMVCLK   REMOVE ELEMENT FROM RTC QUEUE                     REV 0.0, AUG 79
================================== FILE 108 ==================================
RTC      REAL TIME CLOCK MODULES (BINARY FILE)             REV 0.0, AUG 79
RESTAR   RESTART RTC                                       REV 0.0, AUG 79
ADDCLK   ADD ELEMENT TO RTC QUEUE                          REV 0.0, AUG 79
RMVCLK   REMOVE ELEMENT FROM RTC QUEUE                     REV 0.0, AUG 79
================================== FILE 109 ==================================
BOOTSP   BOOTSTRAP FOR SUPER100 (BINARY FILE)              REL 0.0  , AUG 79
================================== FILE 110 ==================================
BINARY FILE FOR THE SYSTEM SERVICE ROUTINES (FPS100>DUMMY>OPSYS>MTS100>SYSSVC)
================================== FILE 111 ==================================
BINARY VERSION OF FPS100>DUMMY>OPSYS>MTS100>KERNEL
================================== FILE 112 ==================================
BINARY FILE FOR HOST INTERRUPT SERVICE ROUTINE
================================== FILE 113 ==================================
BINARY FILE FOR HOST COMMUNICATION ROUTINES
================================== FILE 114 ==================================
BINARY VERSION OF FPS100>DUMMY>OPSYS>MTS100>FUNC.S
================================== FILE 115 ==================================
BINARY VERSION OF FPS100>DUMMY>OPSYS>MTS100>UPEX.S
================================== FILE 116 ==================================
OBJECT VERSION OF THE FILE ENABLE.S
================================== FILE 117 ==================================
ECHO.B    OBJECT FILE FOR ECHO
================================== FILE 118 ==================================
SUBR1.B    OBJECT FILE FOR SUBR1
================================== FILE 119 ==================================
SUBR2.B    OBJECT FILE FOR SUBR2
================================== FILE 120 ==================================
SHOOT.B    OBJECT FILE FOR SHOOT
================================== FILE 121 ==================================
TASK51.B    OBJECT FILE FOR TASK51
================================== FILE 122 ==================================
TASK52.B    OBJECT FILE FOR TASK52
================================== FILE 123 ==================================
TASK53.B    OBJECT FILE FOR TASK53
================================== FILE 124 ==================================
SYSCOM   SYSTEM COMMON INITIALIZATIONS                     REL 0.0  , AUG 79
CONFIG   I/O DEVICE CONFIGURATION TABLE                    REL 0.0  , AUG 79
================================== FILE 125 ==================================
NORTC    REAL TIME CLOCK MODULES                           REV 0.0, AUG 79
ADDCLK   ADD ELEMENT TO RTC QUEUE                          REV 0.0, AUG 79
RMVCLK   REMOVE ELEMENT FROM RTC QUEUE                     REV 0.0, AUG 79
================================== FILE 126 ==================================
RTC      REAL TIME CLOCK MODULES                           REV 0.0, AUG 79
RESTAR   RESTART RTC                                       REV 0.0, AUG 79
ADDCLK   ADD ELEMENT TO RTC QUEUE                          REV 0.0, AUG 79
RMVCLK   REMOVE ELEMENT FROM RTC QUEUE                     REV 0.0, AUG 79
================================== FILE 127 ==================================
BOOTSP   BOOTSTRAP FOR SUPER100                            REL 0.0  , AUG 79
================================== FILE 128 ==================================
SAVRST   SAVE/RESTORE SEQUENCE                             REL 0.0  , AUG 79
WAIT     WAIT FOR MESSAGE/ANSWER                           REL 0.0  , AUG 79
ANSWER   SEND ANSWER                                       REL 0.0  , AUG 79
SEND     SEND MESSAGE                                      REL 0.0  , AUG 79
RECEIV   RECEIVE MESSAGE                                   REL 0.0  , AUG 79
RESUME   RESUME TASK                                       REL 0.0  , AUG 79
RUNPRI   SET RUN PRIORITY                                  REL 0.0  , AUG 79
SETFPE   ENABLE/DISABLE F.P. EXCEPTION INTERRUPT           REL 0.0  , AUG 79
OVHNDL   THE OVERLAY HANDLER                               REL 0.0  , AUG 79
OVLAY    OVERLAY SVC'S                                     REL 0.0  , AUG 79
================================== FILE 129 ==================================
EXTASK   EXECUTE TASK MODULE                               REL 0.0  , AUG 79
PSMNGR   PS MANAGER                                        REL 0.0  , AUG 79
MOVER    CODE MOVER                                        REL 0.0  , AUG 79
IO       I/O INTERRUPT HANDLER                             REL 0.0  , AUG 79
INTEXT   INTERRUPT EXIT ROUTINE                            REL 0.0  , AUG 79
RTCINT   RTC INTERRUPT HANDLER                             REL 0.0  , AUG 79
CHKPT    CHECKPOINT                                        REL 0.0  , AUG 79
IDLE     IDLING ROUTINE                                    REL 0.0  , AUG 79
TRAP     TRAP HANDLER                                      REL 0.0  , AUG 79
FATAL    FATAL/EXCEPTION INTERRUPT HANDLER                 REL 0.0  , AUG 79
PRIQ     PRIORITY INSERT INTO QUEUE                        REL 0.0  , AUG 79
INSERT   INSERT INTO QUEUE                                 REL 0.0  , AUG 79
DEQ      DEQUEUE                                           REL 0.0  , AUG 79
EMPTY    CHECKS FOR EMPTY QUEUE                            REL 0.0  , AUG 79
DELETE   DELETE FROM QUEUE                                 REL 0.0  , AUG 79
================================== FILE 130 ==================================
HIRP.S   HOST INTERRUPT SVC (MTS100)                       REL 0.0  , JUL 79
HIRP     HOST INTERRUPT SERVICE ROUTINE                    REL 0.0  , JUL 79
SPADS    READ SPAD VALUES                                  REL 0.0  , JUL 79
DATUM    PROCESS DATUM VALUE FROM HOST                     REL 0.0  , JUL 79
APXMT    GET VALUE FROM HOST INTERFACE                     REL 0.0  , JUL 79
================================== FILE 131 ==================================
HSVC.S   HOST COMMUNICATION (MTS100)                       REL 0.0  , JUL 79
WATSWR   WAIT FOR HOST TO WRITE SWR                        REL 0.0  , JUL 79
WATLIT   WAIT FOR HOST TO READ LITES                       REL 0.0  , JUL 79
SENDER   SEND MESSAGE TO HOST                              REL 0.0  , JUL 79
FPUT     SEND A DATUM TO HOST                              REL 0.0  , JUL 79
FGET     GET A DATUM FROM HOST                             REL 0.0  , JUL 79
FTST     TEST IF DATUM IS AVAILABLE                        REL 0.0  , JUL 79
================================== FILE 132 ==================================
FUNC.S   FUNCTION INTERPRETER TASK                         REL 0.0  , JUL 79
FUNC     FUNCTION INTERPRETER TASK                         REL 0.0  , JUL 79
INCADD   INCREMENT ADDRESS REGISTER IF NEEDED              REL 0.0  , JUL 79
FSELEC   DECIDE REGISTER SELECT                            REL 0.0  , JUL 79
================================== FILE 133 ==================================
UPEX.S   APEX TASK FOR SUBROUTINES                         REL 0.0  , JUL 79
UPEX     USER APEX TASK                                    REL 0.0  , JUL 79
================================== FILE 134 ==================================
ENABLE   ROUTINE TO ENABLE INTERRUPTS                      REL 0.0, AUG 79
================================== FILE 135 ==================================
ECHO     PROGRAM TO TEST THE HOST INTERFACE ROUTINES       REL 1.0, MAY 79
================================== FILE 136 ==================================
SUBR1    HASI TEST ROUTINE                                 REL A.0  , AUG 79
================================== FILE 137 ==================================
SUBR2    AP FORTRAN HASI TEST ROUTINE                      REL A.0  , AUG 79
================================== FILE 138 ==================================
SHOOT    FOR HASI TEST                                     REL A.0  , AUG 79
================================== FILE 139 ==================================
TASK5    TASK FOR TEST 5.0                                 REL 3.0, JUN 79
================================== FILE 140 ==================================
TASK5    TASK FOR TEST 5.0                                 REL 3.0, JUN 79
================================== FILE 141 ==================================
TASK5    TASK FOR TEST 5.0                                 REL 3.0, JUN 79
================================== FILE 142 ==================================
================================== FILE 143 ==================================
================================== FILE 144 ==================================
================================== FILE 145 ==================================
RUN2X    MTS100 SUPV TEST STEP 2.2                         FPS-100 , MAY 79
TESTR1   TEST CONTROL SUBROUTINE                           FPS-100 , MAY 79
XHPUT    HPUT CALLING ROUTINE                              FPS-100 , MAY 79
GETV     HGET CALLING ROUTINE                              FPS-100 , MAY 79
TSTV     HTST CALLING ROUTINE                              FPS-100 , MAY 79
LOOKT
LVIRP
================================== FILE 146 ==================================
RUN5X    MTS100 SUPV TEST STEPS 5                          FPS-100 , MAY 79
TESTR1   TEST CONTROL SUBROUTINE                           FPS-100 , MAY 79
XHPUT    HPUT CALLING ROUTINE                              FPS-100 , MAY 79
GETV     HGET CALLING ROUTINE                              FPS-100 , MAY 79
TSTV     HTST CALLING ROUTINE                              FPS-100 , MAY 79
LOOKT
LVIRP
================================== FILE 147 ==================================
RUN6X    MTS100 SUPV TEST STEP 6                           FPS-100 , MAY 79
XHPUT    HPUT CALLING ROUTINE                              FPS-100 , MAY 79
GETV     HGET CALLING ROUTINE                              FPS-100 , MAY 79
TSTV     HTST CALLING ROUTINE                              FPS-100 , MAY 79
LOOKT
LVIRP
================================== FILE 148 ==================================
================================== FILE 149 ==================================
MINI-100 SUPERVISOR COMMON DEFINITION
================================== FILE 150 ==================================
MINDEF   MINI SYSTEM DEFINITIONS                           REL 1.0  , NOV 79
================================== FILE 151 ==================================
COMMAND FILE TO LOAD MINI-100
================================== FILE 152 ==================================
SYSCOM   SYSTEM COMMON INITIALIZATIONS  (BINARY)           REL 1.0  , NOV 79
================================== FILE 153 ==================================
HIRPM    HOST INTERRUPT SVC (BINARY VERSION)               REL 1.0  , NOV 79
================================== FILE 154 ==================================
HSVCM    HOST COMMUNICATION (BINARY RTS100)                REL 1.0  , NOV 79
================================== FILE 155 ==================================
RTCISR   RTC INTERRUPT HANDLER  (BINARY)                   REL 1.0  , NOV 79
================================== FILE 156 ==================================
RTCREQ   RTC REQUEST HANDLER    (BINARY9                   REL 1.0  , NOV 79
================================== FILE 157 ==================================
UPEXM    DUMMY APEX TASK  (BINARY)                         REL 1.0  , NOV 79
================================== FILE 158 ==================================
BOOTMN   BOOTSTRAP FOR MINI100  (BINARY VERSION)           REL 1.0  , NOV 79
================================== FILE 159 ==================================
IOQUE    I/O DRIVER QUEUE SUPPORT ROUTINES (BINARY)        REL 1.0  , NOV 79
================================== FILE 160 ==================================
================================== FILE 161 ==================================
================================== FILE 162 ==================================
================================== FILE 163 ==================================
================================== FILE 164 ==================================
================================== FILE 165 ==================================
================================== FILE 166 ==================================
================================== FILE 167 ==================================
================================== FILE 168 ==================================
SYSCOM   SYSTEM COMMON INITIALIZATIONS                     REL 1.0  , NOV 79
CONFIG   I/O DEVICE CONFIGURATION TABLE                    REL 1.0  , NOV 79
SAVRST   SAVE/RESTORE SEQUENCE                             REL 1.0  , NOV 79
OVHNDL   THE OVERLAY HANDLER                               REL 1.0  , NOV 79
OVLAY    OVERLAY SVC'S                                     REL 1.0  , NOV 79
PSMNGR   PS MANAGER                                        REL 1.0  , NOV 79
MOVER    CODE MOVER                                        REL 1.0  , NOV 79
TRAP     TRAP HANDLER                                      REL 1.0  , NOV 79
IO       I/O INTERRUPT HANDLER                             REL 1.0  , NOV 79
INTEXT   INTERRUPT EXIT ROUTINE                            REL 1.0  , NOV 79
FATAL    FATAL/EXCEPTION INTERRUPT HANDLER                 REL 1.0  , NOV 79
================================== FILE 169 ==================================
HIRPM    HOST INTERRUPT SVC (RTS100)                       REL 1.0  , NOV 79
HIRP     HOST INTERRUPT SERVICE ROUTINE                    REL 1.0  , NOV 79
DATUM    PROCESS DATUM VALUE FROM HOST                     REL 1.0  , NOV 79
APXMT    GET VALUE FROM HOST INTERFACE                     REL 1.0  , NOV 79
================================== FILE 170 ==================================
HSVCM    HOST COMMUNICATION (RTS100)                       REL 1.0  , NOV 79
HSVC     HOST COMMUNICATION SERVICES                       REL 1.0  , NOV 79
WATSWR   WAIT FOR HOST TO WRITE SWR                        REL 1.0  , NOV 79
WATLIT   WAIT FOR HOST TO READ LITES                       REL 1.0  , NOV 79
SENDER   SEND MESSAGE TO HOST                              REL 1.0  , NOV 79
FPUT     SEND A DATUM TO HOST                              REL 1.0  , NOV 79
FGET     GET A DATUM FROM HOST                             REL 1.0  , NOV 79
FTST     TEST IF DATUM IS AVAILABLE                        REL 1.0  , NOV 79
================================== FILE 171 ==================================
RTCISR   RTC INTERRUPT HANDLER                             REL 1.0  , NOV 79
================================== FILE 172 ==================================
RTCREQ   RTC REQUEST HANDLER                               REL 1.0  , NOV 79
================================== FILE 173 ==================================
UPEXM    DUMMY APEX TASK                                   REL 1.0  , NOV 79
================================== FILE 174 ==================================
BOOTMN   BOOTSTRAP FOR MINI100                             REL 1.0  , NOV 79
================================== FILE 175 ==================================
IOQUE    I/O DRIVER QUEUE SUPPORT ROUTINES                 REL 1.0  , NOV 79
INSERT   INSERT INTO QUEUE                                 REL 1.0  , NOV 79
EMPTY    CHECKS FOR EMPTY QUEUE                            REL 1.0  , NOV 79
DELETE   DELETE FROM QUEUE                                 REL 1.0  , NOV 79
ENABLE   ROUTINE TO ENABLE INTERRUPTS                      REL 1.0  , NOV 79
================================== FILE 176 ==================================
RTCDUM   DUMMY DATA SECTION FOR RTCTEST                    REL 1.0  , NOV 79
================================== FILE 177 ==================================
RTCTST   RTC TEST (TEST 5) FOR MINI                        REL 1.0  , NOV 79
================================== FILE 178 ==================================
SHOOTM   FOR HASI TEST                                     REL 1.0  , NOV 79
================================== FILE 179 ==================================
SUB1     HASI TEST ROUTINE                                 REL 1.0  , NOV 79
================================== FILE 180 ==================================
SUB2     AP FORTRAN HASI TEST ROUTINE                      REL 1.0  , NOV 79
================================== FILE 181 ==================================
SUBR3    FOR BASIC MECH MINI TEST 1                        REL 1.0  , NOV 79
================================== FILE 182 ==================================
SUBR4    FOR BASIC MECH MINI TEST 1                        REL 1.0  , NOV 79
================================== FILE 183 ==================================
TEST2    PROGRAM TO TEST THE HOST INTERFACE ROUTINES       REL 1.0  , NOV 79
================================== FILE 184 ==================================
================================== FILE 185 ==================================
================================== FILE 186 ==================================
================================== FILE 187 ==================================
================================== FILE 188 ==================================
RUN1     TEST1 HOST MAINLINE                               REL 1.0  , NOV 79
================================== FILE 189 ==================================
RUN2     TEST2 HOST MAINLINE                               REL 1.0  , NOV 79
TESTR1   TEST CONTROL SUBROUTINE                           REL 1.0  , NOV 79
XHPUT    HPUT CALLING ROUTINE                              REL 1.0  , NOV 79
GETV     HGET CALLING ROUTINE                              REL 1.0  , NOV 79
TSTV     HTST CALLING ROUTINE                              REL 1.0  , NOV 79
LOOKT    REL 1.0  , NOV 79
LVIRP    REL 1.0  , NOV 79
================================== FILE 190 ==================================
RUN5     TEST5 (RTC) HOST MAINLINE                         REL 1.0  , NOV 79
TESTR1   TEST CONTROL SUBROUTINE                           REL 1.0  , NOV 79
XHPUT    HPUT CALLING ROUTINE                              REL 1.0  , NOV 79
GETV     HGET CALLING ROUTINE                              REL 1.0  , NOV 79
TSTV     HTST CALLING ROUTINE                              REL 1.0  , NOV 79
LOOKT    REL 1.0  , NOV 79
LVIRP    REL 1.0  , NOV 79
================================== FILE 191 ==================================
RUN6     RTS100 SUPV TEST STEP 6                           REL 1.0  , NOV 79
XHPUT    HPUT CALLING ROUTINE                              REL 1.0  , NOV 79
GETV     HGET CALLING ROUTINE                              REL 1.0  , NOV 79
TSTV     HTST CALLING ROUTINE                              REL 1.0  , NOV 79
LOOKT    REL 1.0  , NOV 79
LVIRP    REL 1.0  , NOV 79
================================== FILE 192 ==================================
================================= END OF TAPE ================================
