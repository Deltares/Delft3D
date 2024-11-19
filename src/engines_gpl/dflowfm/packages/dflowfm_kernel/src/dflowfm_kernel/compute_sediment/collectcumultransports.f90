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

module m_collectcumultransports

implicit none

private

public :: collectcumultransports

contains

   subroutine collectcumultransports()
      use m_flowtimes, only: dts
      use m_flowgeom, only: ndx
      use m_fm_erosed

      integer :: k, l
      double precision :: dtmor_

      ! cumulative transports
      dtmor_ = dts * morfac
      do l = 1, lsedtot
         do k = 1, ndx
            sbxcum(k, l) = sbxcum(k, l) + (sbcx(k, l) + sbwx(k, l)) * dtmor_
            sbycum(k, l) = sbycum(k, l) + (sbcy(k, l) + sbwy(k, l)) * dtmor_
            ssxcum(k, l) = ssxcum(k, l) + (sscx(k, l) + sswx(k, l)) * dtmor_
            ssycum(k, l) = ssycum(k, l) + (sscy(k, l) + sswy(k, l)) * dtmor_
         end do
      end do

   end subroutine

end module m_collectcumultransports
