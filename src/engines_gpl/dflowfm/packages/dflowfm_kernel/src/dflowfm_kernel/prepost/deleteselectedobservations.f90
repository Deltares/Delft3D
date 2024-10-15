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

   subroutine deleteSelectedObservations()
      use m_confrm
      use m_observations, only: numobs, xobs, yobs, deleteobservation, deleteobservations, purgeobservations
      use m_polygon, only: npl, xpl, ypl
      use m_missing, only: dmiss, jins
      use geometry_module, only: pinpok
      implicit none

      integer :: i
      integer :: inhul
      integer :: ja
      integer :: key

      if (Npl <= 2) then
         call CONFRM('NO POLYGON, SO DELETE all Observation Points? ', JA)
         if (JA == 0) then
            KEY = 0
            return
         end if
         call deleteObservations()
         return
      end if

      do I = 1, numobs
         call PINPOK(xobs(I), yobs(I), Npl, Xpl, Ypl, INHUL, jins, dmiss)
         if (INHUL == 1) then
            call deleteObservation(I)
         end if
      end do
      call purgeObservations()

      return

   end subroutine deleteSelectedObservations
