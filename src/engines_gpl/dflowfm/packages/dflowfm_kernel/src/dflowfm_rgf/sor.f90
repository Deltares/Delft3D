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

module m_sor

implicit none

private

public :: sor

contains

      subroutine SOR(A, B, C, D, E, U, RJAC, M1, N1, M2, N2)
         use precision, only: dp
         use m_grid
         use m_gridsettings
         use m_orthosettings, only: ITIN

         real(kind=dp) :: anorm
         real(kind=dp) :: anormf
         real(kind=dp) :: half
         integer :: j
         integer :: l
         integer :: m1
         integer :: m2
         integer :: maxits
         integer :: n
         integer :: n1
         integer :: n2
         real(kind=dp) :: one
         real(kind=dp) :: qtr
         real(kind=dp) :: rjac
         real(kind=dp) :: zero
!     IMPLICIT real(kind=dp) ::(A-H,O-Z)
         real(kind=dp) :: A(MMAX, NMAX), B(MMAX, NMAX), C(MMAX, NMAX), D(MMAX, NMAX), E(MMAX, NMAX), U(MMAX, NMAX)

         parameter(ZERO=0d0, HALF=.5d0, QTR=.25d0, ONE=1d0)
         real(kind=dp) :: RESID, OMEGA
!     WRITE (MDIA,*) 'MEGS AVAILABLE SOR ', N4*4.096*0.001,
!      (N1+N2)*4.096*0.001d0
         MAXITS = ITIN
         ANORMF = ZERO
         OMEGA = ONE

         do N = 1, MAXITS
            ANORM = ZERO
            do J = max(2, M1), min(M2, MC - 1)
               do L = max(2, N1), min(N2, NC - 1)
                  if (IJC(J, L) == 10) then
!              IF(MOD(J+L,2).EQ.MOD(N,2))THEN
                     RESID = A(J, L) * U(J + 1, L) + B(J, L) * U(J - 1, L) + &
                             C(J, L) * U(J, L + 1) + D(J, L) * U(J, L - 1) + E(J, L) * U(J, L)
                     U(J, L) = U(J, L) - OMEGA * RESID / E(J, L)
!              ENDIF
                  end if
               end do
            end do
            if (N == 1) then
               OMEGA = ONE / (ONE - HALF * RJAC**2)
            else
               OMEGA = ONE / (ONE - QTR * RJAC**2 * OMEGA)
            end if
!       write(mdia,*) omega, rjac
         end do

         return
      end subroutine SOR

end module m_sor
