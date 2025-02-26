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
!> @file allocatelinktocenterweights.f90
!! Subroutines for allocating center related link x- and y weights.
module m_allocate_linktocenterweights

   implicit none

   private

   public :: allocatelinktocenterweights

contains

   !> allocate center related linkxy weights
   subroutine allocatelinktocenterweights()

      use m_flowgeom
      use m_alloc

      integer :: ierr

      if (allocated(wcx1)) deallocate (wcx1)
      if (allocated(wcy1)) deallocate (wcy1)
      if (allocated(wcx2)) deallocate (wcx2)
      if (allocated(wcy2)) deallocate (wcy2)
      if (allocated(wcL)) deallocate (wcL)
      if (allocated(wc)) deallocate (wc)

      allocate (wcx1(lnx), stat=ierr); 
      call aerr('wcx1(lnx)', ierr, lnx)
      allocate (wcy1(lnx), stat=ierr); 
      call aerr('wcy1(lnx)', ierr, lnx)
      allocate (wcx2(lnx), stat=ierr); 
      call aerr('wcx2(lnx)', ierr, lnx)
      allocate (wcy2(lnx), stat=ierr); 
      call aerr('wcy2(lnx)', ierr, lnx)
      allocate (wcL(2, Lnx), stat=ierr); 
      call aerr('wcL  (2,Lnx)', ierr, 2 * Lnx)
      allocate (wc(ndx), stat=ierr); 
      call aerr('wc     (ndx)', ierr, ndx)

   end subroutine allocatelinktocenterweights

end module m_allocate_linktocenterweights
