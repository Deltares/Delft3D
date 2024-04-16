!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module m_strsv
    use m_waq_precision
    use m_xerbla

    implicit none

contains


    !
    !     ***********************************************************************
    !
    SUBROUTINE STRSV (UPLO, TRANS, DIAG, N, A, LDA, X, INCX)
        use timers
        use m_lsame
        !          .. Scalar Arguments ..
        INTEGER(kind = int_wp) :: INCX, LDA, N
        character(len=1)        DIAG, TRANS, UPLO
        !          .. Array Arguments ..
        REAL(kind = dp) :: A(LDA, *), X(*)
        !          ..
        !
        !       Purpose
        !       =======
        !
        !       STRSV  solves one of the systems of equations
        !
        !          A*x = b,   or   A'*x = b,
        !
        !       where b and x are n element vectors and A is an n by n unit, or
        !       non-unit, upper or lower triangular matrix.
        !
        !       No test for singularity or near-singularity is included in this
        !       routine. Such tests must be performed before calling this routine.
        !
        !       Parameters
        !       ==========
        !
        !       UPLO   - character(len=1).
        !                On entry, UPLO specifies whether the matrix is an upper or
        !                lower triangular matrix as follows:
        !
        !                   UPLO = 'U' or 'u'   A is an upper triangular matrix.
        !
        !                   UPLO = 'L' or 'l'   A is a lower triangular matrix.
        !
        !                Unchanged on exit.
        !
        !       TRANS  - character(len=1).
        !                On entry, TRANS specifies the equations to be solved as
        !                follows:
        !
        !                   TRANS = 'N' or 'n'   A*x = b.
        !
        !                   TRANS = 'T' or 't'   A'*x = b.
        !
        !                   TRANS = 'C' or 'c'   A'*x = b.
        !
        !                Unchanged on exit.
        !
        !       DIAG   - character(len=1).
        !                On entry, DIAG specifies whether or not A is unit
        !                triangular as follows:
        !
        !                   DIAG = 'U' or 'u'   A is assumed to be unit triangular.
        !
        !                   DIAG = 'N' or 'n'   A is not assumed to be unit
        !                                       triangular.
        !
        !                Unchanged on exit.
        !
        !       N      - INTEGER.
        !                On entry, N specifies the order of the matrix A.
        !                N must be at least zero.
        !                Unchanged on exit.
        !
        !       A      - REAL             array of DIMENSION ( LDA, n ).
        !                Before entry with  UPLO = 'U' or 'u', the leading n by n
        !                upper triangular part of the array A must contain the upper
        !                triangular matrix and the strictly lower triangular part of
        !                A is not referenced.
        !                Before entry with UPLO = 'L' or 'l', the leading n by n
        !                lower triangular part of the array A must contain the lower
        !                triangular matrix and the strictly upper triangular part of
        !                A is not referenced.
        !                Note that when  DIAG = 'U' or 'u', the diagonal elements of
        !                A are not referenced either, but are assumed to be unity.
        !                Unchanged on exit.
        !
        !       LDA    - INTEGER.
        !                On entry, LDA specifies the first dimension of A as declared
        !                in the calling (sub) program. LDA must be at least
        !                max( 1, n ).
        !                Unchanged on exit.
        !
        !       X      - REAL             array of dimension at least
        !                ( 1 + ( n - 1 )*abs( INCX ) ).
        !                Before entry, the incremented array X must contain the n
        !                element right-hand side vector b. On exit, X is overwritten
        !                with the solution vector x.
        !
        !       INCX   - INTEGER.
        !                On entry, INCX specifies the increment for the elements of
        !                X. INCX must not be zero.
        !                Unchanged on exit.
        !
        !
        !       Level 2 Blas routine.
        !
        !       -- Written on 22-October-1986.
        !          Jack Dongarra, Argonne National Lab.
        !          Jeremy Du Croz, Nag Central Office.
        !          Sven Hammarling, Nag Central Office.
        !          Richard Hanson, Sandia National Labs.
        !
        !
        !          .. Parameters ..
        REAL(kind = dp) :: ZERO
        PARAMETER        (ZERO = 0.0E+0)
        !          .. Local Scalars ..
        REAL(kind = dp) :: TEMP
        INTEGER(kind = int_wp) :: I, INFO, IX, J, JX, KX
        LOGICAL            NOUNIT
        !          .. Intrinsic Functions ..
        INTRINSIC MAX
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("strsv", ithandl)
        !          ..
        !          .. Executable Statements ..
        !
        !          Test the input parameters.
        !
        INFO = 0
        IF     (.NOT.LSAME(UPLO, 'U').AND. &
                .NOT.LSAME(UPLO, 'L'))THEN
            INFO = 1
        ELSE IF(.NOT.LSAME(TRANS, 'N').AND. &
                .NOT.LSAME(TRANS, 'T').AND. &
                        .NOT.LSAME(TRANS, 'C'))THEN
            INFO = 2
        ELSE IF(.NOT.LSAME(DIAG, 'U').AND. &
                .NOT.LSAME(DIAG, 'N'))THEN
            INFO = 3
        ELSE IF(N<0)THEN
            INFO = 4
        ELSE IF(LDA<MAX(1, N))THEN
            INFO = 6
        ELSE IF(INCX==0)THEN
            INFO = 8
        END IF
        IF(INFO/=0)THEN
            CALL XERBLA('STRSV ', INFO)
            if (timon) call timstop (ithandl)
            RETURN
        END IF
        !
        !          Quick return if possible.
        !
        IF(N==0) THEN
            if (timon) call timstop (ithandl)
            RETURN
        ENDIF
        !
        NOUNIT = LSAME(DIAG, 'N')
        !
        !          Set up the start point in X if the increment is not unity. This
        !          will be  ( N - 1 )*INCX  too small for descending loops.
        !
        IF(INCX<=0)THEN
            KX = 1 - (N - 1) * INCX
        ELSE IF(INCX/=1)THEN
            KX = 1
        END IF
        !
        !          Start the operations. In this version the elements of A are
        !          accessed sequentially with one pass through A.
        !
        IF(LSAME(TRANS, 'N'))THEN
            !
            !             Form  x := inv( A )*x.
            !
            IF(LSAME(UPLO, 'U'))THEN
                IF(INCX==1)THEN
                    DO J = N, 1, -1
                        IF(X(J)/=ZERO)THEN
                            IF(NOUNIT) &
                                    X(J) = X(J) / A(J, J)
                            TEMP = X(J)
                            DO I = J - 1, 1, -1
                                X(I) = X(I) - TEMP * A(I, J)
                            end do
                        END IF
                    end do
                ELSE
                    JX = KX + (N - 1) * INCX
                    DO J = N, 1, -1
                        IF(X(JX)/=ZERO)THEN
                            IF(NOUNIT) &
                                    X(JX) = X(JX) / A(J, J)
                            TEMP = X(JX)
                            IX = JX
                            DO I = J - 1, 1, -1
                                IX = IX - INCX
                                X(IX) = X(IX) - TEMP * A(I, J)
                            end do
                        END IF
                        JX = JX - INCX
                    end do
                END IF
            ELSE
                IF(INCX==1)THEN
                    DO J = 1, N
                        IF(X(J)/=ZERO)THEN
                            IF(NOUNIT) &
                                    X(J) = X(J) / A(J, J)
                            TEMP = X(J)
                            DO I = J + 1, N
                                X(I) = X(I) - TEMP * A(I, J)
                            end do
                        END IF
                    end do
                ELSE
                    JX = KX
                    DO J = 1, N
                        IF(X(JX)/=ZERO)THEN
                            IF(NOUNIT) &
                                    X(JX) = X(JX) / A(J, J)
                            TEMP = X(JX)
                            IX = JX
                            DO I = J + 1, N
                                IX = IX + INCX
                                X(IX) = X(IX) - TEMP * A(I, J)
                            end do
                        END IF
                        JX = JX + INCX
                    end do
                END IF
            END IF
        ELSE
            !
            !             Form  x := inv( A' )*x.
            !
            IF(LSAME(UPLO, 'U'))THEN
                IF(INCX==1)THEN
                    DO J = 1, N
                        TEMP = X(J)
                        DO I = 1, J - 1
                            TEMP = TEMP - A(I, J) * X(I)
                        end do
                        IF(NOUNIT) &
                                TEMP = TEMP / A(J, J)
                        X(J) = TEMP
                    end do
                ELSE
                    JX = KX
                    DO J = 1, N
                        TEMP = X(JX)
                        IX = KX
                        DO I = 1, J - 1
                            TEMP = TEMP - A(I, J) * X(IX)
                            IX = IX + INCX
                        end do
                        IF(NOUNIT) &
                                TEMP = TEMP / A(J, J)
                        X(JX) = TEMP
                        JX = JX + INCX
                    end do
                END IF
            ELSE
                IF(INCX==1)THEN
                    DO J = N, 1, -1
                        TEMP = X(J)
                        DO I = N, J + 1, -1
                            TEMP = TEMP - A(I, J) * X(I)
                        end do
                        IF(NOUNIT) &
                                TEMP = TEMP / A(J, J)
                        X(J) = TEMP
                    end do
                ELSE
                    KX = KX + (N - 1) * INCX
                    JX = KX
                    DO J = N, 1, -1
                        TEMP = X(JX)
                        IX = KX
                        DO I = N, J + 1, -1
                            TEMP = TEMP - A(I, J) * X(IX)
                            IX = IX - INCX
                        end do
                        IF(NOUNIT) &
                                TEMP = TEMP / A(J, J)
                        X(JX) = TEMP
                        JX = JX - INCX
                    end do
                END IF
            END IF
        END IF
        !
        if (timon) call timstop (ithandl)
        RETURN
        !
        !          End of STRSV .
        !
    END

end module m_strsv
