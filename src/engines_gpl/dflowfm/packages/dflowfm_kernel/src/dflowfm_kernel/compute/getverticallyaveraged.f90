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

subroutine getverticallyaveraged(sal, mx)
   use m_flow
   use m_flowgeom
   use m_get_kbot_ktop
   implicit none
   double precision :: sal(mx)
   integer :: n, k, kb, kt, mx

   do n = 1, ndx
      call getkbotktop(n, kb, kt)
      sal(n) = 0d0
      if (vol1(n) > 0) then
         do k = kb, kt
            sal(n) = sal(n) + sal(k) * vol1(k)
         end do
         sal(n) = sal(n) / vol1(n)
      end if
   end do
end subroutine getverticallyaveraged
