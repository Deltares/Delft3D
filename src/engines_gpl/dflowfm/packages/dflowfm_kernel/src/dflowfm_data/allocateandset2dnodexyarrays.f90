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

module m_allocateandset2dnodexyarrays

   implicit none

contains

   subroutine allocateandset2Dnodexyarrays(n)
      use precision, only: dp
      use m_netw
      use m_flowgeom
      use m_sferic

      implicit none
      integer :: n, IERR
      ! locals
      integer :: m, k, nn
      real(kind=dp) :: xmn, xmx

      nn = netcell(n)%n

! GD: memory leak
      if (allocated(nd(n)%x)) deallocate (nd(n)%x)
      if (allocated(nd(n)%y)) deallocate (nd(n)%y)
      if (allocated(nd(n)%nod)) deallocate (nd(n)%nod)

      allocate (nd(n)%x(nn), nd(n)%y(nn), nd(n)%nod(nn), stat=ierr)
      call aerr('nd(n)%x(nn), nd(n)%y(nn), nd(n)%nod(nn)', ierr, nn * 3)
      do m = 1, nn
         k = netcell(n)%NOD(m)
         nd(n)%x(m) = xk(k)
         nd(n)%y(m) = yk(k)
         nd(n)%nod(m) = k; 
      end do

      if (jsferic == 1) then ! jglobe
         xmn = minval(nd(n)%x)
         xmx = maxval(nd(n)%x)
         if (xmx - xmn > 180d0) then
            do m = 1, nn
               k = netcell(n)%NOD(m)
               if (xmx - nd(n)%x(m) > 180d0) then
                  nd(n)%x(m) = nd(n)%x(m) + 360d0
               end if
            end do
         end if
      end if

   end subroutine allocateandset2Dnodexyarrays

end module m_allocateandset2dnodexyarrays
