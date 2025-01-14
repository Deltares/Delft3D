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

module m_stopint

   implicit none

   private

   public :: stopint

contains

   subroutine STOPINT()
      use unstruc_files
      use unstruc_netcdf, only: unc_closeall
      use m_partitioninfo
      use m_fetch_operation_utils, only: finish_fetch_proc

      call ISCREENCLOSE()
      call unc_closeall()
      call close_all_files()

      call finish_fetch_proc()
      if (jampi == 1) then
!        finalize before exit
         call partition_finalize()
      end if

!     SPvdP: close dia-file
      if (mdia /= 0 .and. mdia < maxnum) then
         close (mdia)
         mdia = 0
      end if

      stop
   end

end module m_stopint
