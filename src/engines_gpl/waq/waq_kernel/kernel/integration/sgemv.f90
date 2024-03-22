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
module m_sgemv
    use m_waq_precision
    use m_xerbla

    implicit none

contains


    SUBROUTINE SGEMV (TRANS, M, N, ALPHA, A, LDA, X, INCX, &
            BETA, Y, INCY)
        use timers
        use m_lsame

        !          .. Scalar Arguments ..
        REAL(kind = dp) :: ALPHA, BETA
        INTEGER(kind = int_wp) :: INCX, INCY, LDA, M, N
        CHARACTER*1        TRANS
        !          .. Array Arguments ..
        REAL(kind = dp) :: A(LDA, *), X(*), Y(*)
        !          ..
        !
        !       Purpose
        !       =======
        !
        !       SGEMV  performs one of the matrix-vector operations
        !
        !          y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
        !
        !       where alpha and beta are scalars, x and y are vectors and A is an
        !       m by n matrix.
        !
        !       Parameters
        !       ==========
        !
        !       TRANS  - CHARACTER*1.
        !                On entry, TRANS specifies the operation to be performed as
        !                follows:
        !
        !                   TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
        !
        !                   TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
        !
        !                   TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
        !
        !                Unchanged on exit.
        !
        !       M      - INTEGER.
        !                On entry, M specifies the number of rows of the matrix A.
        !                M must be at least zero.
        !                Unchanged on exit.
        !
        !       N      - INTEGER.
        !                On entry, N specifies the number of columns of the matrix A.
        !                N must be at least zero.
        !                Unchanged on exit.
        !
        !       ALPHA  - REAL            .
        !                On entry, ALPHA specifies the scalar alpha.
        !                Unchanged on exit.
        !
        !       A      - REAL             array of DIMENSION ( LDA, n ).
        !                Before entry, the leading m by n part of the array A must
        !                contain the matrix of coefficients.
        !                Unchanged on exit.
        !
        !       LDA    - INTEGER.
        !                On entry, LDA specifies the first dimension of A as declared
        !                in the calling (sub) program. LDA must be at least
        !                max( 1, m ).
        !                Unchanged on exit.
        !
        !       X      - REAL             array of DIMENSION at least
        !                ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
        !                and at least
        !                ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
        !                Before entry, the incremented array X must contain the
        !                vector x.
        !                Unchanged on exit.
        !
        !       INCX   - INTEGER.
        !                On entry, INCX specifies the increment for the elements of
        !                X. INCX must not be zero.
        !                Unchanged on exit.
        !
        !       BETA   - REAL            .
        !                On entry, BETA specifies the scalar beta. When BETA is
        !                supplied as zero then Y need not be set on input.
        !                Unchanged on exit.
        !
        !       Y      - REAL             array of DIMENSION at least
        !                ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
        !                and at least
        !                ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
        !                Before entry with BETA non-zero, the incremented array Y
        !                must contain the vector y. On exit, Y is overwritten by the
        !                updated vector y.
        !
        !       INCY   - INTEGER.
        !                On entry, INCY specifies the increment for the elements of
        !                Y. INCY must not be zero.
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
        REAL(kind = dp) :: ONE, ZERO
        PARAMETER        (ONE = 1.0E+0, ZERO = 0.0E+0)
        !          .. Local Scalars ..
        REAL(kind = dp) :: TEMP
        INTEGER(kind = int_wp) :: I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
        !          .. Intrinsic Functions ..
        INTRINSIC MAX
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("sgemv", ithandl)
        !          ..
        !          .. Executable Statements ..
        !
        !          Test the input parameters.
        !
        INFO = 0
        IF     (.NOT.LSAME(TRANS, 'N').AND. &
                .NOT.LSAME(TRANS, 'T').AND. &
                        .NOT.LSAME(TRANS, 'C'))THEN
            INFO = 1
        ELSE IF(M<0)THEN
            INFO = 2
        ELSE IF(N<0)THEN
            INFO = 3
        ELSE IF(LDA<MAX(1, M))THEN
            INFO = 6
        ELSE IF(INCX==0)THEN
            INFO = 8
        ELSE IF(INCY==0)THEN
            INFO = 11
        END IF
        IF(INFO/=0)THEN
            CALL XERBLA('SGEMV ', INFO)
            goto 9999  !   RETURN
        END IF
        !
        !          Quick return if possible.
        !
        IF((M==0).OR.(N==0).OR. &
                ((ALPHA==ZERO).AND.(BETA==ONE))) &
                goto 9999  !   RETURN
        !
        !          Set  LENX  and  LENY, the lengths of the vectors x and y, and set
        !          up the start points in  X  and  Y.
        !
        IF(LSAME(TRANS, 'N'))THEN
            LENX = N
            LENY = M
        ELSE
            LENX = M
            LENY = N
        END IF
        IF(INCX>0)THEN
            KX = 1
        ELSE
            KX = 1 - (LENX - 1) * INCX
        END IF
        IF(INCY>0)THEN
            KY = 1
        ELSE
            KY = 1 - (LENY - 1) * INCY
        END IF
        !
        !          Start the operations. In this version the elements of A are
        !          accessed sequentially with one pass through A.
        !
        !          First form  y := beta*y.
        !
        IF(BETA/=ONE)THEN
            IF(INCY==1)THEN
                IF(BETA==ZERO)THEN
                    DO I = 1, LENY
                        Y(I) = ZERO
                    end do
                ELSE
                    DO I = 1, LENY
                        Y(I) = BETA * Y(I)
                    end do
                END IF
            ELSE
                IY = KY
                IF(BETA==ZERO)THEN
                    DO I = 1, LENY
                        Y(IY) = ZERO
                        IY = IY + INCY
                    end do
                ELSE
                    DO I = 1, LENY
                        Y(IY) = BETA * Y(IY)
                        IY = IY + INCY
                    end do
                END IF
            END IF
        END IF
        IF(ALPHA==ZERO) &
                goto 9999  !   RETURN
        IF(LSAME(TRANS, 'N'))THEN
            !
            !             Form  y := alpha*A*x + y.
            !
            JX = KX
            IF(INCY==1)THEN
                DO J = 1, N
                    IF(X(JX)/=ZERO)THEN
                        TEMP = ALPHA * X(JX)
                        DO I = 1, M
                            Y(I) = Y(I) + TEMP * A(I, J)
                        end do
                    END IF
                    JX = JX + INCX
                end do
            ELSE
                DO J = 1, N
                    IF(X(JX)/=ZERO)THEN
                        TEMP = ALPHA * X(JX)
                        IY = KY
                        DO  I = 1, M
                            Y(IY) = Y(IY) + TEMP * A(I, J)
                            IY = IY + INCY
                        end do
                    END IF
                    JX = JX + INCX
                end do
            END IF
        ELSE
            !
            !             Form  y := alpha*A'*x + y.
            !
            JY = KY
            IF(INCX==1)THEN
                DO J = 1, N
                    TEMP = ZERO
                    DO I = 1, M
                        TEMP = TEMP + A(I, J) * X(I)
                    end do
                    Y(JY) = Y(JY) + ALPHA * TEMP
                    JY = JY + INCY
                end do
            ELSE
                DO J = 1, N
                    TEMP = ZERO
                    IX = KX
                    DO I = 1, M
                        TEMP = TEMP + A(I, J) * X(IX)
                        IX = IX + INCX
                    end do
                    Y(JY) = Y(JY) + ALPHA * TEMP
                    JY = JY + INCY
                end do
            END IF
        END IF
        !
        9999 if (timon) call timstop (ithandl)
        RETURN
        !
        !          End of SGEMV .
        !
    END

end module m_sgemv
