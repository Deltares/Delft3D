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

      !> read from rfg grid file
      subroutine ECRREA(X, MMAX, NMAX, MC, NC, MRGF, HALF)
         use m_missing
         implicit none
         character dummy * 10, REC * 132
!     LEES RGF
         integer, intent(in) :: MMAX, NMAX !< array sizes
         integer, intent(in) :: mc, nc !< grid size
         integer, intent(in) :: mrgf !< grid-file unit number
         double precision, intent(in) :: half !< progress bar length, 0:half, 0.5:full
         double precision :: X(MMAX, NMAX)
         double precision :: af
         integer :: i, j

         do J = 1, NC
            if (HALF > -1d0) then
               AF = HALF + 0.5d0 * dble(J) / dble(NC)
               call READYY('Reading Grid File', AF)
            end if
            read (MRGF, *, err=777, end=999) dummy, dummy, (X(I, J), I=1, MC)
         end do

         return

777      backspace (MRGF)
         backspace (MRGF)
         do J = 1, NC
            if (HALF > -1d0) then
               AF = HALF + 0.5d0 * dble(J) / dble(NC)
               call READYY('Reading Grid File', AF)
            end if
            read (MRGF, '(10X5F12.0)', err=888, end=999) (X(I, J), I=1, MC)
         end do

         ! where (x == 0d0) x = dxymis

         return

888      backspace (MRGF)
         read (MRGF, '(A)') REC
         call QNREADERROR('Reading Grid Coordinates but Getting', REC, MRGF)
         return

999      backspace (MRGF)
         read (MRGF, '(A)') REC
         call QNEOFERROR(MRGF)
         return
      end subroutine ECRREA
