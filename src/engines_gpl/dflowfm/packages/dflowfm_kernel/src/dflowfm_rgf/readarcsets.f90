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

      subroutine REAdarcsets(mlist)
         use precision, only: dp
         use m_netw
         use M_ARCINFO
         use M_MISSING
         use m_readyy
         use m_delpol

         implicit none

         integer :: Mlist
         integer :: Marc
         character REC * 132

         logical jawel

         integer :: i1, i2, j1, j2, k, L, Lp, numfil
         real(kind=dp) :: af, f11, f21, f12, f22, dii, djj

         numfil = 0

         call READYY('Reading arcinfosets', 0d0)

10       read (mlist, '(a)', end=888) rec
         Lp = index('.', rec)
         L = index(' ', rec(Lp:)) - 1
         inquire (file=trim(rec(1:L)), exist=jawel)

         if (jawel) then
            numfil = numfil + 1
            goto 10
         end if

888      rewind (mlist)

20       read (mlist, '(a)', end=888) rec
         Lp = index('.', rec)
         L = index(' ', rec(Lp:)) - 1
         inquire (file=trim(rec(1:L)), exist=jawel)

         if (jawel) then

            af = dble(k) / dble(numfil)
            call READYY('Reading arcinfosets', af)

            call oldfil(marc, rec(1:L))
            call savepol() ! do not use the selecting polygon to read a block from the file
            call delpol()
            call reaarc(marc, 0)
            call restorepol()
            call DOCLOSE(marc)

            do k = 1, numk

               if (zk(k) /= dmiss) cycle

               i1 = (xk(k) - x0) / dxa + 1; i2 = i1 + 1
               if (i1 < 1 .or. i2 > mca) cycle

               j1 = (yk(k) - x0) / dxa + 1; i2 = i1 + 1
               if (j1 < 1 .or. j2 > nca) cycle

               dii = xk(k) - x0 - i1 * dxa
               djj = yk(k) - y0 - j1 * dxa

               f11 = (1d0 - dii) * (1d0 - djj)
               f21 = (dii) * (1d0 - djj)
               f22 = (dii) * (djj)
               f12 = (1d0 - dii) * (djj)

               zk(k) = d(i1, j1) * f11 + &
                       d(i2, j1) * f21 + &
                       d(i2, j2) * f22 + &
                       d(i1, j2) * f12
            end do

         end if

999      if (allocated(d)) then
            deallocate (d); mca = 0; nca = 0
         end if

         call READYY(' ', -1d0)

      end subroutine REAdarcsets
