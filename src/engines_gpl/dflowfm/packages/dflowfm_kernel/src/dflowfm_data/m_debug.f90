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

module m_debug
   !
   ! Module with arrays to write debug quantities to nc map files
   ! Enable use by setting enableDebugArrays to 1 in the mdu [output] block.
   ! Code has to be uncommented and adapted to your needs in flow_modelinit
   ! and unc_write_map_filepointer_ugrid.
   !
   public
   integer :: jawritedebug
   double precision, allocatable, dimension(:) :: debugarr1d
   double precision, allocatable, dimension(:, :) :: debugarr2d
   double precision, allocatable, dimension(:, :, :) :: debugarr3d

contains

   subroutine init_debugarr(dim1, dim2, dim3)
      use m_alloc
      use m_missing

      implicit none

      integer, intent(in) :: dim1
      integer, intent(in), optional :: dim2
      integer, intent(in), optional :: dim3

      integer :: dim2_
      integer :: dim3_

      dim2_ = 0
      dim3_ = 0

      if (present(dim2)) then
         dim2_ = dim2
      end if

      if (present(dim3)) then
         dim3_ = dim3
      end if

      if (allocated(debugarr1d)) then
         deallocate (debugarr1d)
      end if

      if (allocated(debugarr2d)) then
         deallocate (debugarr2d)
      end if

      if (allocated(debugarr3d)) then
         deallocate (debugarr3d)
      end if

      call realloc(debugarr1d, dim1, keepExisting=.false., fill=dmiss)
      if (dim2_ > 0) then
         call realloc(debugarr2d, (/dim1, dim2_/), keepExisting=.false., fill=dmiss)
      end if
      if (dim2_ > 0 .and. dim3_ > 0) then
         call realloc(debugarr3d, (/dim1, dim2_, dim3_/), keepExisting=.false., fill=dmiss)
      end if
   end subroutine init_debugarr

end module m_debug
