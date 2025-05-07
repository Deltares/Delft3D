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

module m_pressakey

   implicit none

   private

   public :: pressakey

contains

   subroutine pressakey()
#ifdef HAVE_MPI
      use mpi
      use m_partitioninfo, only: DFM_COMM_ALLWORLD, my_rank, jampi

      integer :: ierr

      if (jampi == 1) then
         call MPI_barrier(DFM_COMM_ALLWORLD, ierr)
      end if

      if (my_rank == 0) then
         write (6, *) "press a key from rank 0..."
         read (5, *)
      end if

      call MPI_barrier(DFM_COMM_ALLWORLD, ierr)
#else
      write (6, *) "press a key..."
      read (5, *)
#endif

   end subroutine pressakey

end module m_pressakey
