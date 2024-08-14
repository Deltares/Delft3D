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

!> compute total grid height for a given grow factor, first grid layer height and number of grid layers
double precision function comp_h(dgrow, dheight0, nfac)
   implicit none

   double precision, intent(in) :: dgrow !< grow factor
   double precision, intent(in) :: dheight0 !< first grid layer height
   integer, intent(in) :: nfac !< number of grid layers

   if (abs(dgrow - 1d0) > 1d-8) then
      comp_h = (dgrow**nfac - 1d0) / (dgrow - 1d0) * dheight0
   else
      comp_h = nfac * dheight0
   end if

   return
end function comp_h
