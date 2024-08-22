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

!> snap network meshlines to nearest land boundary
subroutine nettoland()

   use M_netw
   use M_MISSING
   use m_observations
   use gridoperations

   implicit none

   integer :: ja

   call findcells(100)
   call makenetnodescoding()

   ja = 1
   call confrm('Do you want to snap net boundary to the land boundary only?', ja)
   if (ja == 1) then
      call find_nearest_meshline(2) ! net boundaries only
      call snap_to_landboundary()
   else
      ja = 1
      call confrm('Do you want to snap inner net to the land boundary too?', ja)
      if (ja == 1) then
         call find_nearest_meshline(3)
         call snap_to_landboundary()
      else
         ja = 1
         call confrm('Do you want to snap all net to the land boundary?', ja)
         if (ja == 1) then
            call find_nearest_meshline(4)
            call snap_to_landboundary()
         end if
      end if
   end if

   if (ja == 1) then
      call confrm('Are you satisfied?', ja)
      if (ja /= 1) call restore()
   end if

   return
end subroutine nettoland
