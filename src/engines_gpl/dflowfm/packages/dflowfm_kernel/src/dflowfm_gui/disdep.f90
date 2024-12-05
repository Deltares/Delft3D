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

module m_disdep

   implicit none

contains

   subroutine DISDEP(m, n, dep)
      use precision, only: dp
      use m_devices, only: iws
      use m_ktext

      real(kind=dp) :: dep
      integer :: m
      integer :: n
      character distan * 23
      character fmt * 6

      DISTAN = 'M:    N:    D:         '
      write (DISTAN(3:5), '(I3)') M
      write (DISTAN(9:11), '(I3)') N
      fmt = '(f9.3)'
      call dispform(dep, fmt)
      write (DISTAN(15:23), fmt) DEP
      call KTEXT(DISTAN, IWS - 22, 4, 15)

      return
   end

end module m_disdep
