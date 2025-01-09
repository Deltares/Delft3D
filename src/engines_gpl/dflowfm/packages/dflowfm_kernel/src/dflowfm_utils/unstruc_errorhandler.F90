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

!> Performs a clean program stop upon errors.
!! This routine is automatically called from the MessageHandling module.
subroutine unstruc_errorhandler(level)
   use unstruc_messages, only: threshold_abort
   use unstruc_files, only: mdia, close_all_files
   use dfm_error, only: dfm_genericerror
#ifdef HAVE_MPI
   use mpi
   use m_partitioninfo, only: DFM_COMM_ALLWORLD, jampi
#endif
   implicit none
   integer, intent(in) :: level
   integer :: ierr

   ierr = 0

   if (level >= threshold_abort) then
      call close_all_files()
      close (mdia)
      mdia = 0
#ifdef HAVE_MPI
      if (jampi == 1) then
         call MPI_Abort(DFM_COMM_ALLWORLD, DFM_GENERICERROR, ierr)
      end if
#endif
      stop
   end if
end subroutine unstruc_errorhandler
