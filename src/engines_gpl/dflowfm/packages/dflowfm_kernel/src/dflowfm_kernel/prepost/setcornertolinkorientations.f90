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

module m_setcornertolinkorientations

   implicit none

   private

   public :: setcornertolinkorientations

contains

   subroutine setcornertolinkorientations()
      use precision, only: dp
      use m_flowgeom, only: csbn, snbn, lnx, lncn
      use m_sferic, only: jsferic, jasfer3d
      use m_alloc, only: aerr
      use network_data, only: xk, yk
      use m_missing, only: dmiss
      use geometry_module, only: half, spher2locvec

      real(kind=dp) :: xL, yL

      integer :: k3, k4
      integer :: L

      integer :: ierr

      real(kind=dp), parameter :: dtol = 1.0e-8_dp

      if (allocated(csbn)) then
         deallocate (csbn)
      end if
      if (allocated(snbn)) then
         deallocate (snbn)
      end if

      if (jsferic == 0 .or. jasfer3D == 0) then
         return
      end if

      allocate (csbn(2, Lnx), stat=ierr)
      csbn = 1.0_dp
      call aerr('csbn(2,Lnx)', ierr, 2 * Lnx)
      allocate (snbn(2, Lnx), stat=ierr)
      snbn = 0.0_dp
      call aerr('snbn(2,Lnx)', ierr, 2 * Lnx)

      do L = 1, Lnx
         k3 = lncn(1, L)
         k4 = lncn(2, L)

!      compute flowlink midpoint coordinates (xL,yL)
         call half(xk(k3), yk(k3), xk(k4), yk(k4), xL, yL, jsferic, jasfer3D)

         if (yk(k3) == 90.0_dp .or. yk(k4) == 90.0_dp) then
            continue
         end if

!      compute orientation w.r.t. link mid point
         call spher2locvec(xk(k3), yk(3), 1, [xL], [yL], [1.0_dp], [0.0_dp], csbn(1, L), snbn(1, L), jsferic, jasfer3D, dmiss)
         call spher2locvec(xk(k4), yk(4), 1, [xL], [yL], [1.0_dp], [0.0_dp], csbn(2, L), snbn(2, L), jsferic, jasfer3D, dmiss)
      end do

      return
   end subroutine setcornertolinkorientations

end module m_setcornertolinkorientations
