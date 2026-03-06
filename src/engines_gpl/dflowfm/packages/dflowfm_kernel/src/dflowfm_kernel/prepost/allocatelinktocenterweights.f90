!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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
!> @file allocatelinktocenterweights.f90
!! Subroutines for allocating center related link x- and y weights.
module m_allocate_linktocenterweights

   implicit none

   private

   public :: allocatelinktocenterweights

contains

   !> allocate center related linkxy weights
   subroutine allocatelinktocenterweights()

      use m_flowgeom, only: wcxy, wcx1, wcy1, wcx2, wcy2, wcl, wc, ndx, lnx
      use m_alloc, only: aerr

      integer :: ierr

      if (allocated(wcxy)) then
         deallocate (wcxy)
      end if
      if (allocated(wcx1)) then
         deallocate (wcx1)
      end if
      if (allocated(wcy1)) then
         deallocate (wcy1)
      end if
      if (allocated(wcx2)) then
         deallocate (wcx2)
      end if
      if (allocated(wcy2)) then
         deallocate (wcy2)
      end if
      if (allocated(wcL)) then
         deallocate (wcL)
      end if
      if (allocated(wc)) then
         deallocate (wc)
      end if

      allocate (wcxy(2, ndx), stat=ierr)
      call aerr('wcxy(2, ndx)', ierr, 2 * ndx)
      allocate (wcx1(lnx), stat=ierr)
      call aerr('wcx1(lnx)', ierr, lnx)
      allocate (wcy1(lnx), stat=ierr)
      call aerr('wcy1(lnx)', ierr, lnx)
      allocate (wcx2(lnx), stat=ierr)
      call aerr('wcx2(lnx)', ierr, lnx)
      allocate (wcy2(lnx), stat=ierr)
      call aerr('wcy2(lnx)', ierr, lnx)
      allocate (wcL(2, lnx), stat=ierr)
      call aerr('wcL(2,lnx)', ierr, 2 * lnx)
      allocate (wc(ndx), stat=ierr)
      call aerr('wc(ndx)', ierr, ndx)
   end subroutine allocatelinktocenterweights
end module m_allocate_linktocenterweights
