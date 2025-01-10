!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

module m_tranfn

   implicit none

   private

   public :: tranfn

contains

   subroutine TRANFN(X1, X2, X3, X4, &
                     Y1, Y2, Y3, Y4, &
                     mmax, nmax, imax, &
                     MFAC, NFAC, XRH, YRH)
      use m_abrel, only: abrel
      use precision, only: dp
      use m_missing

      integer :: mmax, nmax, imax, mfac, nfac
      real(kind=dp) :: X1(IMAX), X2(IMAX), X3(IMAX), X4(IMAX), XRH(MMAX, NMAX), &
                       Y1(IMAX), Y2(IMAX), Y3(IMAX), Y4(IMAX), YRH(MMAX, NMAX), &
                       B1R(IMAX), B2R(IMAX), A1R(IMAX), A2R(IMAX)

      integer :: I, J
      real(kind=dp) :: A1, A2, B1, B2, D, DX, DY, AIJ, BIJ, EX, EY, XA, YA, XB, YB, DEXY
!     1,2,B VERTICALEN, 3,4,A HORIZONTALEN

      call ABREL(X1, Y1, B1R, NFAC)
      call ABREL(X2, Y2, B2R, NFAC)
      call ABREL(X3, Y3, A1R, MFAC)
      call ABREL(X4, Y4, A2R, MFAC)

!     Dit is modified transfinite
      do I = 2, MFAC
         do J = 2, NFAC
            B1 = B1R(J)
            B2 = B2R(J)
            A1 = A1R(I)
            A2 = A2R(I)
            D = 1 - (A2 - A1) * (B2 - B1)
            AIJ = ((1 - B1) * A1 + B1 * A2) / D
            BIJ = ((1 - A1) * B1 + A1 * B2) / D

            DX = X2(J) - X1(J)
            DY = Y2(J) - Y1(J)
            EX = X4(I) - X3(I)
            EY = Y4(I) - Y3(I)

            XA = X1(J) + AIJ * DX
            YA = Y1(J) + AIJ * DY
            XB = X3(I) + BIJ * EX
            YB = Y3(I) + BIJ * EY

            DEXY = DX * EY - EX * DY
            if (DEXY == 0) then
               XRH(I, J) = XYMIS
               YRH(I, J) = XYMIS
            else
               XRH(I, J) = ((XA * DX + YA * DY) * EY - (XB * EX + YB * EY) * DY) / DEXY
               YRH(I, J) = ((XB * EX + YB * EY) * DX - (XA * DX + YA * DY) * EX) / DEXY
            end if

         end do
      end do

!     Dit is gewoon transfinite
!     X00 = X1(1)
!     X10 = X2(1)
!     X01 = X4(1)
!     X11 = X2(NFAC+1)
!
!     Y00 = Y1(1)
!     Y10 = Y2(1)
!     Y01 = Y4(1)
!     Y11 = Y2(NFAC+1)
!
!     D00 = SQRT( (X00 - X01)**2 + (Y00 - Y01)**2 )
!     D11 = SQRT( (X10 - X11)**2 + (Y10 - Y11)**2 )
!     D10 = SQRT( (X00 - X10)**2 + (Y00 - Y10)**2 )
!     D01 = SQRT( (X00 - X01)**2 + (Y00 - Y01)**2 )
!     do I = 2,MFAC
!        do J = 2,NFAC
!           B1   = B1R(J)
!           B2   = B2R(J)
!           A1   = A1R(I)
!           A2   = A2R(I)
!           RI   = REAL(I-1)/REAL(MFAC)
!           RJ   = REAL(J-1)/REAL(NFAC)
!
!           S    = (1-RJ)*A1 + RJ*A2
!           T    = (1-RI)*B1 + RI*B2
!           SM   = 1 - S
!           TM   = 1 - T
!
!           D12  = SQRT( (X1(J)-X2(J))**2 + (Y1(J)-Y2(J))**2 )
!           D34  = SQRT( (X3(J)-X4(J))**2 + (Y3(J)-Y4(J))**2 )
!
!           CALL ORTPRO(      J,      S,     X1,     Y1,
!                           D12,    X1P,    Y1P,      1,      0)
!           CALL ORTPRO(      J,     SM,     X2,     Y2,
!                           D12,    X2P,    Y2P,     -1,      0)
!
!           CALL ORTPRO(      I,      T,     X3,     Y3,
!                           D34,    X3P,    Y3P,      1,      0)
!           CALL ORTPRO(      I,     TM,     X4,     Y4,
!                           D34,    X4P,    Y4P,     -1,      0)
!
!           CALL ORTPRO(      1,      T,     X2,     Y2,
!                           D00,   X00P,   Y00P,      1,      1)
!           CALL ORTPRO(   MFAC,      T,     X2,     Y2,
!                           D00,   X00P,   Y00P,      1,      1)
!           XRH(I,J) =  (SM*X1P + S*X2P + TM*X3P + T*X4P) -
!                       (SM*TM*X00P + SM*T*X01P + S*T* X11P + S*TM*X10P)
!           YRH(I,J) =  (SM*Y1P + S*Y2P + TM*Y3P + T*Y4P) -
!                       (SM*TM*X00P + SM*T*X01P + S*T* X11P + S*TM*X10P)
!
!           XRH(I,J) =  SM*X1(J) + S*X2(J) + TM*X3(I) + T*X4(I) -
!                      (SM*TM*X00 + S*T*X11 + SM*T*X01 + S*TM*X10)
!           YRH(I,J) =  SM*Y1(J) + S*Y2(J) + TM*Y3(I) + T*Y4(I) -
!                      (SM*TM*Y00 + S*T*Y11 + SM*T*Y01 + S*TM*Y10)
!   11CONTINUE

!     vul randen in
      do I = 1, MFAC + 1
         XRH(I, 1) = X3(I)
         XRH(I, NFAC + 1) = X4(I)
         YRH(I, 1) = Y3(I)
         YRH(I, NFAC + 1) = Y4(I)
      end do

      do J = 1, NFAC + 1
         XRH(1, J) = X1(J)
         XRH(MFAC + 1, J) = X2(J)
         YRH(1, J) = Y1(J)
         YRH(MFAC + 1, J) = Y2(J)
      end do

      return
   end subroutine TRANFN

end module m_tranfn
