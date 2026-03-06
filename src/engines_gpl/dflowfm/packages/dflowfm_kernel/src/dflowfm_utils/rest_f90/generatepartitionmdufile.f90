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
!

module m_generatepartitionmdufile

   implicit none

   private
   public :: generatePartitionMDUFile

contains

   !> Generate a single partition of a set of partitioned MDU files.
   subroutine generatePartitionMDUFile(filename_new)
      use unstruc_model, only: writeMDUFilepointer, md_icgsolver
      use m_flowparameters, only: icgsolver
      use m_filez, only: newfil, doclose
      use MessageHandling, only: mess, LEVEL_FATAL
      character(len=*), intent(in) :: filename_new !< Name of the new MDU file to write, including partition number.

      integer :: mout, istat

      call newfil(mout, filename_new)
      if (mout == 0) then
         call mess(LEVEL_FATAL, "Failed to open file "//trim(filename_new))
         return
      end if

      icgsolver = md_icgsolver ! `writeMDUFilepointer` uses `icgsolver` instead of `md_icgsolver`.
      call writeMDUFilepointer(mout, .false., istat)
      if (istat /= 0) then
         call mess(LEVEL_FATAL, "Failed to write a partitioned MDU file "//trim(filename_new))
      end if

      call doclose(mout)
   end subroutine generatePartitionMDUFile

end module m_generatepartitionmdufile
