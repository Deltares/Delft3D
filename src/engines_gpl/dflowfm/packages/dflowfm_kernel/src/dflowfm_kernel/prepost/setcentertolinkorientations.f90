!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_setcentertolinkorientations

   implicit none

   private

   public :: setcentertolinkorientations

contains

   subroutine setcentertolinkorientations()
      use precision, only: dp
      use m_flowgeom, only: csb, snb, lnx, lncn, ln, xz, yz
      use m_sferic, only: jsferic, jasfer3d
      use m_alloc, only: aerr
      use network_data, only: xk, yk
      use geometry_module, only: half, spher2locvec
      use m_missing, only: dmiss

      real(kind=dp) :: xL, yL

      integer :: i, k, k3, k4
      integer :: L

      integer :: ierr

      real(kind=dp), parameter :: dtol = 1.0e-8_dp

      if (allocated(csb)) then
         deallocate (csb)
      end if
      if (allocated(snb)) then
         deallocate (snb)
      end if

      if (jsferic == 0 .or. jasfer3D == 0) then
         return
      end if

      allocate (csb(2, Lnx), stat=ierr)
      csb = 1.0_dp
      call aerr('csb(2,Lnx)', ierr, 2 * Lnx)
      allocate (snb(2, Lnx), stat=ierr)
      snb = 0.0_dp
      call aerr('snb(2,Lnx)', ierr, 2 * Lnx)

      do L = 1, Lnx
         k3 = lncn(1, L)
         k4 = lncn(2, L)

!      compute flowlink midpoint coordinates (xL,yL)
         call half(xk(k3), yk(k3), xk(k4), yk(k4), xL, yL, jsferic, jasfer3D)

         do i = 1, 2
            k = ln(i, L)

!         compute orientation w.r.t. link mid point
            call spher2locvec(xz(k), yz(k), 1, [xL], [yL], [1.0_dp], [0.0_dp], csb(i, L), snb(i, L), jsferic, jasfer3D, dmiss)
         end do
      end do

      return
   end subroutine setcentertolinkorientations

end module m_setcentertolinkorientations
