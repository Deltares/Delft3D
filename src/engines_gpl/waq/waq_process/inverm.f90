module m_inverm
    use m_waq_precision

    implicit none

contains

    !     ----------------------------------------------------------------------C
    !                                                                           C
    !          SUBROUTINE INVERM                                                C
    !                                                                           C
    !          SOLVES SETS OF LINEAR EQUATIONS BY LU-DECOMPOSITION OF           C
    !          THE COEFFICIENT -  MATRIX, PARTIAL PIVOTING ON ROWS IS           C
    !          APPLIED ON THE MATRIX                                            C
    !                                                                           C
    !          AUTHOR : J.A.G. van Gils                                         C
    !                   van Renswoudestraat 2                                   C
    !                   2612 HX Delft                                           C
    !                                                                           C
    !          VARIABLES:                                                       C
    !                                                                           C
    !          A          SQUARE COEFFICIENT MATRIX  (COLUMN,ROW)               C
    !                                              = (UNKNOWN,EQUATION)         C
    !          B          RIGHTHANDSIDE (RHS) -VECTOR MATRIX (ROW,SET)          C
    !                                              = (EQUATION,NR. OF RHS)      C
    !          N          ORDER OF MATRIX A, NUMBER OF UNKNOWNS/EQUATIONS       C
    !          M          NUMBER OF RHS-VECTORS                                 C
    !          num_rows       RANGE OF FIRST INDEX IN A AND B MATRICES              C
    !                     (CONCERNS FORTRAN DECLARATION (num_rows,*)                C
    !          IH         INTEGER WORK-ARRAY, LENGTH N                          C
    !          WORK       REAL    WORK-ARRAY, LENGTH N                          C
    !          IER        ERROR SWITCH =  0 : NO ERRORS DETECTED                C
    !                                  = -1 : SINGULARITY IN MATRIX A DETECTED  C
    !                                                                           C
    !     ----------------------------------------------------------------------C
    SUBROUTINE INVERM (A, B, N, M, num_rows, IH, WORK, IER)
        IMPLICIT REAL(kind = 8)  (A-H, O-Z)
        IMPLICIT INTEGER (i, j, m, n)

        DIMENSION A(num_rows, *), B(num_rows, *), IH(*), WORK(*)
        IER = 0
        DO IR = 1, N
            IH(IR) = IR
        end do
        DO IK = 1, N
            DO  IR = IK + 1, N
                IF (ABS(A(IK, IH(IR)))>ABS(A(IK, IH(IK)))) THEN
                    IDUM = IH(IR)
                    IH(IR) = IH(IK)
                    IH(IK) = IDUM
                ENDIF
            end do
            IF (ABS(A(IK, IH(IK)))<1D-10) THEN
                IER = -IK
                RETURN
            ENDIF
            DO IR = IK + 1, N
                F = A(IK, IH(IR)) / A(IK, IH(IK))
                IF (ABS(F)<1E-10) GOTO 50
                DO J = 1, M
                    B(IH(IR), J) = B(IH(IR), J) - F * B(IH(IK), J)
                end do
                DO IK2 = IK, N
                    A(IK2, IH(IR)) = A(IK2, IH(IR)) - F * A(IK2, IH(IK))
                end do
                50 CONTINUE
            end do
        end do
        DO IR2 = 1, N
            IR = N + 1 - IR2
            DO J = 1, M
                DO IK = IR + 1, N
                    B(IH(IR), J) = B(IH(IR), J) - A(IK, IH(IR)) * B(IH(IK), J)
                end do
                B(IH(IR), J) = B(IH(IR), J) / A(IR, IH(IR))
            end do
        end do
        DO J = 1, M
            DO I = 1, N
                WORK(I) = B(IH(I), J)
            end do
            DO I = 1, N
                B(I, J) = WORK(I)
            end do
        end do
        RETURN
    END

end module m_inverm
