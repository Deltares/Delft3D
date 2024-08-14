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

   subroutine deleteSelectedCrossSections()
      use m_monitoring_crosssections
      use M_POLYGON
      use M_MISSING
      implicit none

      integer :: i, j
      integer :: inhul
      integer :: ja
      integer :: key
      logical :: jaAllPoints

      !IF (Npl .LE. 2) THEN
      call CONFRM('NO POLYGON will be used, SO DELETE all cross sections? ', JA)
      if (JA == 0) then
         KEY = 0
         return
      end if
      call delCrossSections()
      return
!      ENDIF

      I = 1
      do
         if (I > ncrs) exit
         jaAllPoints = .true.
         do j = 1, crs(I)%path%np
            call PINPOK(crs(i)%path%xp(j), crs(i)%path%yp(j), Npl, Xpl, Ypl, INHUL, jins, dmiss)
            jaAllPoints = jaAllPoints .and. (INHUL == 1)
         end do
         if (jaAllPoints) then
            !AvD: Disabled           call delCrossSection(I)
            ! cross sections are shifted to the left, so don't increment I.
         else
            I = I + 1
         end if
      end do

      return

   end subroutine deleteSelectedCrossSections
