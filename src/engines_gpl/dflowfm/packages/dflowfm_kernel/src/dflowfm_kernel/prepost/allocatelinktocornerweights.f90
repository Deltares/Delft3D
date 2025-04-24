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
!> @file allocatelinktocornerweights.f90
!! Subroutines for allocating corner related link x- and y weights.

module m_allocatelinktocornerweights

   implicit none

   private

   public :: allocatelinktocornerweights

contains

   !> Allocate corner related link x- and y weights
   subroutine allocatelinktocornerweights()
      use m_flowgeom, only: wcnx3, wcny3, wcnx4, wcny4, wcLn, cscnw, sncnw, kcnw, nwalcnw, sfcnw, lnx, nrcnw, jacorner, lne2ln
      use m_netw, only: numk, numl, kn, lnn
      use m_alloc

      implicit none

      integer ierr
      integer :: k, L
      integer :: k1, k2

      if (allocated(wcnx3)) then
         deallocate (wcnx3)
      end if
      if (allocated(wcny3)) then
         deallocate (wcny3)
      end if
      if (allocated(wcnx4)) then
         deallocate (wcnx4)
      end if
      if (allocated(wcny4)) then
         deallocate (wcny4)
      end if
      if (allocated(wcLn)) then
         deallocate (wcLn)
      end if
      if (allocated(jacorner)) then
         deallocate (jacorner)
      end if

      allocate (wcnx3(lnx), stat=ierr); 
      call aerr('wcnx3(lnx) ', ierr, lnx)
      allocate (wcny3(lnx), stat=ierr); 
      call aerr('wcny3(lnx) ', ierr, lnx)
      allocate (wcnx4(lnx), stat=ierr); 
      call aerr('wcnx4(lnx) ', ierr, lnx)
      allocate (wcny4(lnx), stat=ierr); 
      call aerr('wcny4(lnx) ', ierr, lnx)
      allocate (wcLn(2, lnx), stat=ierr); 
      call aerr('wcLn(2,lnx)', ierr, lnx)
      allocate (jacorner(numk), stat=ierr)
      call aerr('jacorner(numk)', ierr, numk)

! count number of attached and closed boundary links, and store it temporarily in jacorner
      jacorner = 0
      do L = 1, numL
         if ((kn(3, L) == 2 .and. lnn(L) == 1 .and. lne2ln(L) <= 0)) then
            k1 = kn(1, L)
            k2 = kn(2, L)
            jacorner(k1) = jacorner(k1) + 1
            jacorner(k2) = jacorner(k2) + 1
         end if
      end do

! post-process corner indicator: use ALL boundary nodes, and project on closed boundary later
!   used to be: nmk(k) - int(wcnxy (3,k)) == 2
      do k = 1, numk
         if (jacorner(k) >= 1) then
            jacorner(k) = 1
         else
            jacorner(k) = 0
         end if
      end do

      ! exclude all nodes with a disabled netlink attached from the projection
      do L = 1, numL
         if (kn(3, L) == 0) then
            k1 = kn(1, L)
            k2 = kn(2, L)
            jacorner(k1) = 0
            jacorner(k2) = 0
         end if
      end do

      nrcnw = 0
      do k = 1, numk ! set up admin for corner velocity alignment at closed walls
         if (jacorner(k) == 1) then
            nrcnw = nrcnw + 1 ! cnw = cornerwall point (netnode)
         end if
      end do

      if (allocated(cscnw)) then
         deallocate (cscnw)
      end if
      if (allocated(sncnw)) then
         deallocate (sncnw)
      end if
      if (allocated(kcnw)) then
         deallocate (kcnw)
      end if
      if (allocated(nwalcnw)) then
         deallocate (nwalcnw)
      end if
      if (allocated(sfcnw)) then
         deallocate (sfcnw)
      end if

      allocate (cscnw(nrcnw), stat=ierr); 
      call aerr('cscnw(nrcnw)', ierr, nrcnw)
      allocate (sncnw(nrcnw), stat=ierr); 
      call aerr('sncnw(nrcnw)', ierr, nrcnw)
      allocate (kcnw(nrcnw), stat=ierr); 
      call aerr(' kcnw(nrcnw)', ierr, nrcnw)
      allocate (nwalcnw(2, nrcnw), stat=ierr); 
      call aerr(' nwalcnw(2,nrcnw)', ierr, 2 * nrcnw)
      allocate (sfcnw(nrcnw), stat=ierr); 
      call aerr(' sfcnw(nrcnw)', ierr, nrcnw)

   end subroutine allocatelinktocornerweights
end module m_allocatelinktocornerweights
