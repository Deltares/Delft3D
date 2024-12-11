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

module m_dumpstations

implicit none

private

public :: dumpstations

contains

   subroutine dumpstations(name)
      use m_observations_data
      use m_flow
      use m_flowgeom

      implicit none
      integer :: mhis2, n, k, L1

      character(len=*) :: name

      L1 = index('.', name)
      call newfil(mhis2, trim(name(1:L1))//'stat')

      do n = 1, numobs
         k = kobs(n)
         write (mhis2, '(6f16.6,2x,A)') xobs(n), yobs(n), smxobs(n), cmxobs(n), bl(k), ba(k), trim(namobs(n))
      end do
      write (mhis2, *) ' '

      do n = 1, numobs
         k = kobs(n)
         write (mhis2, *) s1(k)
      end do
      write (mhis2, *) ' '

      do n = 1, numobs
         k = kobs(n)
         write (mhis2, *) sqrt(ucx(k) * ucx(k) + ucy(k) * ucy(k))
      end do

      call doclose(mhis2)
   end subroutine dumpstations

end module m_dumpstations
