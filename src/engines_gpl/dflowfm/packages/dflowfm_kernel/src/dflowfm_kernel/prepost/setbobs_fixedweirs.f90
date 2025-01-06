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

!> Sets the bob values on the flow links that are overridden by a fixed weir.
!! This is based on the interpolated pliz values from the fixed weir definition.
module m_setbobs_fixedweirs

implicit none

private

public :: setbobs_fixedweirs

contains

subroutine setbobs_fixedweirs()
   use precision, only: dp
   use m_flowgeom
   use m_fixedweirs

   integer :: i, ip, iL, Lf
   real(kind=dp) :: alpha, zc

   if (nfxw == 0) return

   do i = 1, nfxw
      do iL = 1, fxw(i)%lnx
         Lf = abs(fxw(i)%ln(iL))
         ip = fxw(i)%indexp(iL)
         alpha = fxw(i)%wfp(iL)
         zc = alpha * fxw(i)%zp(ip) + (1d0 - alpha) * fxw(i)%zp(ip + 1)
         bob(1, Lf) = max(zc, bob(1, Lf)); bob(2, Lf) = max(zc, bob(2, Lf))
      end do
   end do
end subroutine setbobs_fixedweirs

end module m_setbobs_fixedweirs
