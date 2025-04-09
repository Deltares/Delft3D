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
   subroutine INVERM(A, B, N, M, num_rows, IH, WORK, IER)
      implicit real(kind=8) (A - H, O - Z)
      implicit integer(i, j, m, n)

      dimension A(num_rows, *), B(num_rows, *), IH(*), WORK(*)
      IER = 0
      do IR = 1, N
         IH(IR) = IR
      end do
      do IK = 1, N
         do IR = IK + 1, N
            if (abs(A(IK, IH(IR))) > abs(A(IK, IH(IK)))) then
               IDUM = IH(IR)
               IH(IR) = IH(IK)
               IH(IK) = IDUM
            end if
         end do
         if (abs(A(IK, IH(IK))) < 1d-10) then
            IER = -IK
            return
         end if
         do IR = IK + 1, N
            F = A(IK, IH(IR)) / A(IK, IH(IK))
            if (abs(F) < 1e-10) goto 50
            do J = 1, M
               B(IH(IR), J) = B(IH(IR), J) - F * B(IH(IK), J)
            end do
            do IK2 = IK, N
               A(IK2, IH(IR)) = A(IK2, IH(IR)) - F * A(IK2, IH(IK))
            end do
50          continue
         end do
      end do
      do IR2 = 1, N
         IR = N + 1 - IR2
         do J = 1, M
            do IK = IR + 1, N
               B(IH(IR), J) = B(IH(IR), J) - A(IK, IH(IR)) * B(IH(IK), J)
            end do
            B(IH(IR), J) = B(IH(IR), J) / A(IR, IH(IR))
         end do
      end do
      do J = 1, M
         do I = 1, N
            WORK(I) = B(IH(I), J)
         end do
         do I = 1, N
            B(I, J) = WORK(I)
         end do
      end do
      return
   end

end module m_inverm
