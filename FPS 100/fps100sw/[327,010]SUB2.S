C****** SUB2 = AP FORTRAN HASI TEST ROUTINE = REL 1.0  , NOV 79 **************
        SUBROUTINE SUBR2(A,B,C,D)
C
        INTEGER A,B,C,D,COUNT,W,X,Y,Z
        COMMON /PUT/ COUNT,W,X,Y,Z
C
        DATA COUNT /0/
C
        A=B+C
        W=A
        X=B
        Y=C
        Z=D
        CALL APOVLD(3)
        COUNT=COUNT+1
        CALL SHOOT
        RETURN
        END
