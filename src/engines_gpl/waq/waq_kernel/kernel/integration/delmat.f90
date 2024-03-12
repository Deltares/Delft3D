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
module m_delmat
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DELMAT (N, NUC, NLC, M, A, B, IOPT)
        !
        !         THE SUBROUTINE DELMAT IS A DELFT-HYDRAULICS-LABORATORY
        !         PRODUCT TO SOLVE SETS OF LINEAR EQUATIONS DESCRIBED BY
        !         BAND MATRICES WITH LARGEST ELEMENTS ON THE DIAGONAL.
        !         THE MAKER GIVES NO WARRANTY CONCERNING PROPER SOLUTIONS.
        !         NOTHING OF THE CODE CAN BE REPRODUCED WITHOUT PERMISSION
        !         OF THE MAKER
        !
        !         THE SUBROUTINE EXPECTS THE MATRIX "A" TO BE STORED IN A
        !         ONE DIMENSIONAL EQUIVALENT OF THE TWO DIMENSIONAL MATRIX
        !         REPRESENTATION. IN TWO DIMENSIONAL FORM THE STORAGE RULES
        !         USED BY THE IMSL PACKAGE FOR BAND MATRICES MUST BE USED.
        !         HOWEVER THE ROWS AND COLLUMNS MUST BE INTERCHANGED.
        !         THE SAME HOLDS FOR THE FULL MATRIX OF KNOWN VECTORS "B".
        !
        !     SUBROUTINES CALLED  : SRSTOP, stops execution
        !
        !         NOTATION:                               CHANGED DURING
        !                                                  CALCULATION
        !
        !      N    = ORDER OF MATRIX "A"                      NO
        !      NUC  = NUMBER OF UPPER CODIAGONALS              NO
        !      NLC  = NUMBER OF LOWER CODIAGONALS              NO
        !      M    = NUMBER OF KNOWN VECTORS OF               NO
        !             ORDER N IN "B"
        !      A    = BANDMATRIX IN THE EQUIVALENT            YES
        !             ONE DIMENSIONAL FORM
        !      B    = MATRIX OF KNOWN VECTORS IN             YES/NO
        !             ONE DIMENSIONAL FORM
        !      IOPT = CALCULATION OPTION                       NO
        !
        !      OPTION 0 RETURNS THE LU-DECOMPOSITION IN MATRIX A, WHICH CAN
        !                       BE USED AGAIN FOR NEW KNOWN VECTORS AND IT
        !                       RETURNS THE SOLUTION FOR THE UNKNOWN VECTORS
        !                       IN MATRIX B
        !      OPTION 1 RETURNS ONLY THE DECOMPOSITION, B REMAINS UNCHANGED
        !      OPTION 2 RETURNS THE SOLUTION FOR THE UNKNOWN VECTORS IN B
        !                       IT NEEDS A PROPER DECOMPOSED AND STORED MATRIX
        !                       A AS AN INPUT
        !
        use m_srstop
        use timers

        REAL(kind = real_wp) :: A(*), B(*)
        integer(kind = int_wp) :: ithandl = 0

        integer(kind = int_wp) :: NUC, NLC, N, NMUC, NMLC, NDM1, ND, N1
        integer(kind = int_wp) :: L1, L2, L3, L4, L5
        integer(kind = int_wp) :: K1, K2, K3, K4, K5, K6
        integer(kind = int_wp) :: M, IOPT

        real(kind = real_wp) :: F, P

        if (timon) call timstrt ("delmat", ithandl)
        NMUC = N - NUC
        NMLC = N - NLC
        NDM1 = NUC + NLC
        ND = NDM1 + 1
        IF (IOPT==2) GOTO 1000
        !
        !           THE LU-DECOMPOSITION
        !
        !
        !           K1 IS THE OUTER LOOP VARIABLE, COUNTING THE NUMBER OF
        !              COLLUMNS WITH FULL LENGTH
        !           L1 IS THE CORRESPONDING LIMIT VARIABLE
        !           P  IS THE PIVOT ELEMENT. FOR THE ADVECTION DISPERSION
        !              EQUATION THIS IS ALMOST ALWAYS THE LARGEST ELEMENT
        !
        K1 = NLC + 1
        L1 = K1 + NMLC * ND
        100 P = A(K1)
        IF (ABS(P) < 1.0E-35) THEN
            WRITE(6, '('' Matrix for DELMAT singular at element:'',I5)') &
                    K1 / ND + 1
            CALL SRSTOP(1)
        ENDIF
        !
        !           K2 IS THE MIDDLE LOOP VARIABLE COUNTING THE NUMBER OF
        !              ELEMENTS TO BE ELEMINATED
        !           L2 IS THE CORRESPONDING LIMIT VARIABLE
        !           F  IS THE MULTIPLICATION FACTOR FOR THE CORRECTION OF
        !              THE ROW OF ELIMINATION. IT ENTERS A(K2) AS THE NEGATIVE
        !              OF THE ELEMENT OF THE NEW LOWER TRIANGULAR MATRIX
        !
        K2 = K1
        L2 = K1 + NLC * NDM1
        200 K2 = K2 + NDM1
        F = A(K2) / P
        A(K2) = F
        K3 = K1
        !
        !           K4 IS THE INNER LOOP VARIABLE COUNTING THE ELEMENTS ON
        !              THE ROW OF ELIMINATION
        !           L4 IS THE CORRESPONDING LIMMIT VARIABLE
        !           K3 IS THE CORRESPONDING COUNTER FOR THE ELEMENTS ON THE
        !              ROW OF THE DIAGONAL ELEMENT
        !
        K4 = K2
        L4 = K2 + NUC
        300 K3 = K3 + 1
        K4 = K4 + 1
        A(K4) = A(K4) - F * A(K3)
        !
        !           BOOKKEEPING OF THE LOOP VARIABLES
        !
        IF (K4<L4) GOTO 300
        IF (K2<L2) GOTO 200
        K1 = K1 + ND
        IF (K1<L1) GOTO 100
        IF (NLC==1) GOTO 700
        !
        !           THE COLLUMNS BECOME SHORTER, THE REST IS THE SAME
        !
        N1 = NLC
        L1 = (N - 1) * ND
        400 N1 = N1 - 1
        P = A(K1)
        IF (ABS(P) < 1.0E-35) THEN
            WRITE(6, '('' Matrix for DELMAT singular at element:'',I5)') &
                    K1 / ND + 1
            CALL SRSTOP(1)
        ENDIF
        K2 = K1
        L2 = K1 + N1 * NDM1
        500 K2 = K2 + NDM1
        F = A(K2) / P
        A(K2) = F
        K3 = K1
        K4 = K2
        L4 = K2 + NUC
        600 K3 = K3 + 1
        K4 = K4 + 1
        A(K4) = A(K4) - F * A(K3)
        IF (K4<L4) GOTO 600
        IF (K2<L2) GOTO 500
        K1 = K1 + ND
        IF (K1<L1) GOTO 400
        !
        !           ENTRY FOR SUBSTITUTION OPTION
        !
        700 IF(IOPT==1) goto 9999  !   RETURN
        1000 CONTINUE
        !
        !           THE FORWARD SUBSTITUTION HAS ESSENTIALLY THE SAME
        !           STRUCTURE
        !
        !           K1 = OUTER LOOP COUNTER LOWER TRIANGULAR MATRIX
        !           K2 = INNER LOOP COUNTER LOWER TRIANGULAR MATRIX
        !           L1 AND L2 ARE THE LOOP LIMITS
        !           K5 = OUTER LOOP COUNTER MATRIX OF KNOWN VECTORS
        !           K4 = ROW COUNTER IN RELATION TO K5
        !           K3 = INNER LOOP AND ROW COUNTER MATRIX OF KNOWN VECTORS
        !           L3 = ROW LIMIT FOR ONE SUBSTITUTION ELEMENT "F"
        !           F  = SUBSTITUTION ELEMENT OF LOWER TRIANGULAR MATRIX
        !
        K1 = - NUC
        L1 = K1 + NMLC * ND
        K5 = - M
        1100 K1 = K1 + ND
        K5 = K5 + M
        K2 = K1
        L2 = K1 + NLC * NDM1
        K3 = K5 + M
        L3 = K3
        1200 K2 = K2 + NDM1
        F = A(K2)
        L3 = L3 + M
        K4 = K5
        1300 K3 = K3 + 1
        K4 = K4 + 1
        B(K3) = B(K3) - F * B(K4)
        IF (K3<L3) GOTO 1300
        IF (K2<L2) GOTO 1200
        IF (K1<L1) GOTO 1100
        IF (NLC==1) GOTO 2000
        !
        !           THE COLLUMNS BECOME SHORTER, THE REST IS THE SAME
        !
        N1 = NLC
        L1 = (N - 2) * ND
        1400 K1 = K1 + ND
        K5 = K5 + M
        K2 = K1
        N1 = N1 - 1
        L2 = K1 + N1 * NDM1
        K3 = K5 + M
        L3 = K3
        1500 K2 = K2 + NDM1
        F = A(K2)
        L3 = L3 + M
        K4 = K5
        1600 K3 = K3 + 1
        K4 = K4 + 1
        B(K3) = B(K3) - F * B(K4)
        IF (K3<L3) GOTO 1600
        IF (K2<L2) GOTO 1500
        IF (K1<L1) GOTO 1400
        !
        !
        !         BACKWARD SUBSTITUTION
        !
        2000 K1 = N * ND + NLC + 1
        L1 = K1 - NMUC * ND
        K5 = N * M + 1
        2100 K1 = K1 - ND
        L5 = K5 - M
        F = A(K1)
        2200 K5 = K5 - 1
        B(K5) = B(K5) / F
        IF (K5>L5) GOTO 2200
        K2 = K1
        L2 = K1 - NUC * NDM1
        K3 = K5
        L3 = K5
        K6 = K5 + M
        2300 K2 = K2 - NDM1
        F = A(K2)
        L3 = L3 - M
        K4 = K6
        2400 K3 = K3 - 1
        K4 = K4 - 1
        B(K3) = B(K3) - F * B(K4)
        IF (K3>L3) GOTO 2400
        IF (K2>L2) GOTO 2300
        IF (K1>L1) GOTO 2100
        IF (NUC==1) GOTO 2850
        N1 = NUC
        L1 = 2 * ND
        2500 K1 = K1 - ND
        L5 = K5 - M
        F = A(K1)
        N1 = N1 - 1
        2600 K5 = K5 - 1
        B(K5) = B(K5) / F
        IF (K5>L5) GOTO 2600
        K2 = K1
        L2 = K1 - N1 * NDM1
        K3 = K5
        L3 = K5
        K6 = K5 + M
        2700 K2 = K2 - NDM1
        F = A(K2)
        L3 = L3 - M
        K4 = K6
        2800 K3 = K3 - 1
        K4 = K4 - 1
        B(K3) = B(K3) - F * B(K4)
        IF (K3>L3) GOTO 2800
        IF (K2>L2) GOTO 2700
        IF (K1>L1) GOTO 2500
        2850 K1 = K1 - ND
        L5 = K5 - M
        F = A(K1)
        2900 K5 = K5 - 1
        B(K5) = B(K5) / F
        IF (K5>L5) GOTO 2900

        9999 if (timon) call timstop (ithandl)
        RETURN
    END

end module m_delmat
