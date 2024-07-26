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

      subroutine write_flowdiff()
         use m_flow
         use m_samples
         implicit none

         common / DIAGNOSTICFILE / MDIAG
         integer mdiag

         double precision :: avdiffm, avdifwq, fm, wq
         integer :: k, kk, num
         double precision, external :: znod

         avdiffm = 0d0; avdifwq = 0d0; num = 0
         do k = 1, ns
            call in_flowcell(xs(k), ys(k), KK)
            if (kk > 0) then
               fm = znod(kk)
               wq = plotlin(kk)
               if (fm > 0d0 .and. wq > 0d0) then
                  write (mdiag, *) zs(k), fm, wq
                  avdiffm = avdiffm + abs(fm - zs(k))
                  avdifwq = avdifwq + abs(wq - zs(k))
                  num = num + 1
               end if
            end if
         end do
         if (num > 0) then
            avdiffm = avdiffm / num
            avdifwq = avdifwq / num
         end if
         write (mdiag, *) ' avdiffm, avdifwq,num ', avdiffm, avdifwq, num
      end subroutine write_flowdiff
