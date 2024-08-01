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

!> debugging subroutine
subroutine inipole(japole)
   use unstruc_model
   use m_flow
   use m_flowgeom
   use m_sferic
   implicit none

   integer, intent(in) :: japole !< pole (1) or equator (0)

   double precision :: lambda, phi

   integer :: L
   integer, external :: flow_modelinit

!  set velocity field
   do L = 1, Lnx
      lambda = xu(L) * dg2rd
      phi = yu(L) * dg2rd

      if (japole == 1) then
         u1(L) = (sin(phi) * cos(lambda)) * csu(L) + &
                 (-sin(lambda)) * snu(L)

         u1(L) = u1(L) / sqrt(sin(phi)**2 * cos(lambda)**2 + sin(lambda)**2)
      else
         u1(L) = csu(L)
      end if
   end do

   return
end subroutine inipole
