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

module m_atppar
   use m_somdist, only: somdist

   implicit none

   private

   public :: atppar

contains

   subroutine ATPPAR(X, Y, M1, N1, M2, N2, &
                     ATP, A, B, C, D, E)
      use precision, only: dp
      use m_grid, not1 => xc, not2 => yc
      use M_GRIDSETTINGS
      use m_orthosettings
      use M_MISSING
      use m_drawthis
      use m_planedistance

      real(kind=dp) :: af
      real(kind=dp) :: cy
      real(kind=dp) :: dg2rd
      integer :: i
      integer :: j
      integer :: m1
      integer :: m2
      integer :: n1
      integer :: n2
      integer :: ndraw8
      real(kind=dp) :: ym
!     STUURPARAMETERS (1,MC-1)
!     4 3             (1,NC-1)
!     1 2       D1: (12+43)/2   D2:(14 + 23)/2
!     En vul ATP in celmiddens

      real(kind=dp) :: X(MMAX, NMAX), Y(MMAX, NMAX), ATP(MMAX, NMAX), &
                       A(MMAX, NMAX), B(MMAX, NMAX), C(MMAX, NMAX), &
                       D(MMAX, NMAX), E(MMAX, NMAX)

      real(kind=dp) :: X1, Y1, X2, Y2, D12, X3, Y3, X4, Y4, D34, D14, D23
      save NDRAW8

      A = DXYMIS; B = DXYMIS; C = DXYMIS; D = DXYMIS; E = DXYMIS; ATP = DXYMIS

      DG2RD = (acos(-1d0)) / 180d0
!     A,B = METRISCH EN SOM, ATP,E = STUUR, C,D = SOM ATP EN E
!     A,ATP EN C IN M-RICHTING
      do I = M1, M2
         do J = N1, N2
            if (IJYES(I, J) == 1) then
               X1 = X(I, J)
               X2 = X(I + 1, J)
               X3 = X(I + 1, J + 1)
               X4 = X(I, J + 1)
               Y1 = Y(I, J)
               Y2 = Y(I + 1, J)
               Y3 = Y(I + 1, J + 1)
               Y4 = Y(I, J + 1)
               YM = (Y1 + Y2 + Y3 + Y4) / 4
               CY = cos(YM * DG2RD)
               call PLANEDISTANCE(X1, Y1, X2, Y2, D12)
               call PLANEDISTANCE(X3, Y3, X4, Y4, D34)
               call PLANEDISTANCE(X1, Y1, X4, Y4, D14)
               call PLANEDISTANCE(X2, Y2, X3, Y3, D23)
               A(I, J) = (D12 + D34) / 2
               B(I, J) = (D14 + D23) / 2
!              B(I,J)   = (C12 + C34) / 2
!              A(I,J)   = (C14 + C23) / 2
               ATP(I, J) = A(I, J)
               E(I, J) = B(I, J)
            end if
         end do
      end do

      if (MDESIGN >= 2) then
!        andere stuurparameters, in celmiddens
         NDRAW8 = NDRAW(8)
         NDRAW(8) = 0
         ! CALL INTPATP(ATP,E,C,D,X,Y,JDLA,M1,N1,M2,N2)
         NDRAW(8) = NDRAW8
      end if

      do I = M1, M2
         do J = N1, N2
            C(I, J) = ATP(I, J)
            D(I, J) = E(I, J)
         end do
      end do

!     sommmen
      call SOMDIST(A, B, C, D, M1, N1, M2, N2)

!     normeren

      AF = 1 - ATPF
      do I = M1, M2
         do J = N1, N2
            if (IJYES(I, J) == 1) then
               ATP(I, J) = ATP(I, J) * A(I, J) / C(I, J)
               ATP(I, J) = ATPF * ATP(I, J) + AF * A(I, J)
               E(I, J) = E(I, J) * B(I, J) / D(I, J)
               E(I, J) = ATPF * E(I, J) + AF * B(I, J)

               A(I, J) = ATP(I, J)
               B(I, J) = E(I, J)
            end if
         end do
      end do

      do I = M1, M2
         do J = N1, N2
            if (IJYES(I, J) == 1) then
               ATP(I, J) = B(I, J) / A(I, J)
            else
               ATP(I, J) = dmiss
            end if
         end do
      end do
      !     CALL TEKSHOW(X, Y, M2, N2, ATP, 2,'FINAL ATP')

      A = 0d0; B = 0d0; C = 0d0; D = 0d0; E = 0d0
      do I = M1 + 1, M2
         do J = N1 + 1, N2
            if (IJC(I, J) == 10) then

!              A(I,J) = ( ATP(I,J-1) + ATP(I,J) )*0.5
!              B(I,J) = ( ATP(I-1,J-1) + ATP(I-1,J) )*0.5
!              C(I,J) = 1.0 / ( ( ATP(I-1,J) + ATP(I,J) )*0.5 )
!              D(I,J) = 1.0 / ( ( ATP(I-1,J-1) + ATP(I,J-1) )*0.5 )

!              A(I,J) = ( ATP(I,J-1) + ATP(I,J) )
!              B(I,J) = ( ATP(I-1,J-1) + ATP(I-1,J) )
!              C(I,J) = 4.0 / ( ATP(I-1,J)   + ATP(I,J)   )
!              D(I,J) = 4.0 / ( ATP(I-1,J-1) + ATP(I,J-1) )

               A(I, J) = (ATP(I, J - 1) + ATP(I, J))
               B(I, J) = (ATP(I - 1, J - 1) + ATP(I - 1, J))
               C(I, J) = (1d0 / ATP(I - 1, J) + 1d0 / ATP(I, J))
               D(I, J) = (1d0 / ATP(I - 1, J - 1) + 1d0 / ATP(I, J - 1))

               E(I, J) = -(A(I, J) + B(I, J) + C(I, J) + D(I, J))
            end if
         end do
      end do

      return
   end subroutine ATPPAR

end module m_atppar
