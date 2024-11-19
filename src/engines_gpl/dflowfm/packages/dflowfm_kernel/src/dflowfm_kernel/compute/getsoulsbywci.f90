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

module m_getsoulsbywci

implicit none

contains

subroutine getsoulsbywci(modind, ustc2, ustw2, fw, cdrag, umod, abscos, taubpuLL, taubxuLL)
   use m_physcoef, only: rhomean
   implicit none
   integer, intent(in) :: modind
   double precision, intent(in) :: ustc2, ustw2, fw, cdrag, umod, abscos ! Cdrag = ag/C2, abscos = wav relative to link dir
   double precision, intent(out) :: taubpuLL, taubxuLL
   double precision :: ypar, ymxpar
   double precision :: tauwav, taucur

   tauwav = ustw2 * rhomean
   taucur = ustc2 * rhomean

   call getymxpar(modind, tauwav, taucur, fw, cdrag, abscos, ypar, ymxpar)

   taubpuLL = ypar * (taucur + tauwav) / (umod * rhomean + 1d-4) ! umod*ag/C2, (m/s)
   taubxuLL = ymxpar * (taucur + tauwav) ! Max shear stress needed in Erosed, (N/m2)

end subroutine getsoulsbywci

end module m_getsoulsbywci
