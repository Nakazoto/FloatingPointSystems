C****** RUN6 = RTS100 SUPV TEST STEP 6 = REL 1.0  , NOV 79 ********************
C
C
       INTEGER*2 IST,JFN,JWR,JRD,JWW
       INTEGER*2 HOST
        REAL AA(250),BB(250),CC(250)
        INTEGER KPUT
        COMMON /PUT/ KPUT(5)
        INTEGER SVAL
        COMMON /PASS/ SVAL(18)
        INTEGER*2 TINP,TOUT
        COMMON /LUNITS/ TINP,TOUT
       COMMON /WW/ JWR,JRD, JWW,HOST(16)
C              JWR = 6 FOR PRINTER
C              JWR = 5 FOR TERMINAL
       DATA JRD /5/
        TOUT = 5
        TINP = 5
        WRITE (TOUT,942)
942     FORMAT (' OUTPUT DEVICE?    5 = TERMINAL '/
     1          '                   6 = PRINTER  ')
        READ (TINP,943) JWW
943     FORMAT (I2)
        IF(JWW.EQ.6)CALL ASSIGN(6,'LP0:',4)
        JWR = JWW
C
        KADD = 763
        WRITE (JWR,9061) KADD
9061    FORMAT (' SUBR1 APGET ADDRESS IS ',I5)
C
          CALL APINIT (0,0,JFN)
          CALL APLLI ('TEST6.LM',8,8,1,1,HOST,0)
        CALL RTSGO(1024,1)
C
        WRITE (5,9120)
9120    FORMAT (' START TIMING')
CCC
        DO 325 J=1,20000
        K=J
        CALL SUBR1(K,12,13,14,15,6,7,8,9,10,11,12,13)
325     CONTINUE
        CALL APWR
        WRITE (5,9122)
9122    FORMAT (' END TIMING')
        CALL APGET (SVAL,KADD,18,1)
        CALL APWD
        WRITE (JWR,9377) SVAL
9377    FORMAT (1X,6I8)
        CALL TSTV
        CALL TSTV
        DO 330 J=1,200
        K=J
        KA = K+3
        KB = K+100
        KC = 30
        KD = 42
         CALL SUBR2 (KA,KB,KC,KD)
330     CONTINUE
        WRITE (JWR,9610) K,KA,KPUT
9610    FORMAT (' K KA KPUT ',7I7)
        DO 344 J=1,6
        CALL TSTV
344     CONTINUE
        CALL SUBR1(51,62,73,84,95,6,7,8,9,10,11,12,13)
        CALL APWR
        CALL APGET (SVAL,KADD,18,1)
        CALL APWD
        WRITE (JWR,9377) SVAL
        KA = 22
        KB = 33
        CALL SUBR2 (KA,KB,44,55)
        CALL SUBR2 (22,22,22,22)
        CALL LOOKT (31)
        DO 725 J=1,6
        CALL TSTV
725     CONTINUE
C
C               TEST VADD ROUTINE
        DO 1290 J=1,250
        K = J
        AA(J) = J
        BB(J) = J+5
        FWAS = AA(J)+BB(J)
        CALL APPUT (AA,"4001,K,2)
        CALL APPUT (BB,"5001,K,2)
        CALL APWD
        CALL VADD ("4001,1,"5001,1,"6001,1,K)
        CALL APWR
        CALL APGET (CC,"6001,K,2)
        CALL APWD
        FNOW = CC(J)
        IF (FWAS .NE. FNOW) GO TO 1488
1290    CONTINUE
        WRITE (JWR,9790) K,FWAS,FNOW
9790    FORMAT (' K FWAS FNOW ',I5,2F10.2)
        CALL APRSET
        CALL APRLSE
        CALL EXIT
C
1488    CONTINUE
        WRITE (JWR,9795) K,FWAS,FNOW
9795    FORMAT (' VADD ERR ',I5,2F10.2)
        CALL APRLSE
        CALL EXIT
        END
C****** XHPUT = HPUT CALLING ROUTINE = REL 1.0  , NOV 79 **********************
        SUBROUTINE XHPUT (DEST,DATUM)
        INTEGER*2 DEST,DATUM
       INTEGER*2 IST,JFN,JWR,JRD,JWW
       INTEGER*2 HOST
       COMMON /WW/ JWR,JRD, JWW,HOST(16)
        WRITE (JWR,9362) DEST,DATUM
9362    FORMAT (' HPUT.  DEST =',I3,'    DATUM = ',I6)
        CALL HPUT (DEST,DATUM)
        RETURN
        END
C****** GETV = HGET CALLING ROUTINE = REL 1.0  , NOV 79 ***********************
        SUBROUTINE GETV (SORS)
        INTEGER*2 SORS
       INTEGER*2 HOST,JWW
       COMMON /WW/ JWR,JRD, JWW,HOST(16)
        JA = SORS
        CALL HGET (JA,JF,JD)
        WRITE (JWW,9233) JA,JF,JD,SORS
9233    FORMAT (1X,'HGET ',4I10)
        RETURN
        END
C****** TSTV = HTST CALLING ROUTINE = REL 1.0  , NOV 79 ***********************
C..
        SUBROUTINE TSTV
       INTEGER*2 HOST,JWW
       COMMON /WW/ JWR,JRD, JWW,HOST(16)
        JA =0
        JF =0
        JD =0
        CALL HTST (JA,JF,JD)
        WRITE (JWW,9234) JA,JF,JD
9234    FORMAT (1X,'HTST ',3I10)
        RETURN
        END
C****** LOOKT =                                             = REL 1.0  , NOV 79
C..
       SUBROUTINE LOOKT (N)
       INTEGER*2 JWW,HOST
       INTEGER*2 KK1,KK2,KK3,KK4,KK5,N
       COMMON /WW/ JWR,JRD, JWW,HOST(16)
       CALL LOOKY (KK1,KK2,KK3,KK4,KK5)
       WRITE (JWW,9090) N,KK1,KK2,KK3,KK4,KK5
9090   FORMAT (1H  /1X,'#',I4,'   SWR',I7,'  RUNFG',I7,
     1   '   FN',I7,' CTRL',I7,' LITES',I7)
        CALL LVIRP(N)
       RETURN
       END
C****** LVIRP =                                             = REL 1.0  , NOV 79
C..
        SUBROUTINE LVIRP (N)
       INTEGER*2 JWW,HOST
       INTEGER*2 KK1,KK2,KK3,KK4,KK5,N
       COMMON /WW/ JWR,JRD, JWW,HOST(16)
       CALL VIRP (KK1,KK2,KK3,KK4,KK5)
       WRITE (JWW,9190) N,KK1
9190   FORMAT (     1X,'#',I4,' SUPVR',I7,'    SUM',I7,
     1   ' HALT',I7,'  DMA',I7,'   CB5',I7)
       RETURN
       END
