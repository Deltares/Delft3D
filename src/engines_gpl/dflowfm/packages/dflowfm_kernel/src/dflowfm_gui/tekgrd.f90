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

      subroutine TEKgrd(XC, YC, MMAX, NMAX, m1, n1, m2, n2, NCOL, MET, key, MC)
         use m_halt2
         use m_jgrline8
         use m_tek_num_netcells
         use m_set_col
         implicit none
         integer :: mmax, nmax, m1, n1, m2, n2, ncol, met, key, mc
         double precision :: XC(MMAX, NMAX), YC(MMAX, NMAX), xlist(nmax), ylist(nmax)

         integer :: i, j, kmax, ja, key_local

         key_local = key
         
         if (MET == 0 .or. MC == 0) return
         JA = 0

         call SETCOL(NCOL)
         if (MET == 2 .or. MET == 4) call IGRLINETYPE(1)

         KMAX = 8
         do J = N1, N2
            if (mod(J, 10) == 0) call HALT2(JA)
            if (JA == 1) then
               if (MET == 2 .or. MET == 4) call IGRLINETYPE(0)
               return
            end if

            call JGRLINE8(Xc(M1, J), Yc(M1, J), M2 - M1 + 1)
         end do

         do I = M1, M2
            if (mod(I, 10) == 0) call HALT2(JA)
            if (JA == 1) then
               if (MET == 2 .or. MET == 4) call IGRLINETYPE(0)
               return
            end if

            xlist(1:N2 - N1 + 1) = xc(i, N1:N2)
            ylist(1:N2 - N1 + 1) = yc(i, N1:N2)
            call JGRLINE8(xlist, ylist, N2 - N1 + 1)
         end do

         if (MET == 2 .or. MET == 4) call IGRLINETYPE(0)
         if (MET == 5) then
            call TEKnumnetcells(0)
         end if

      end subroutine tekgrd
