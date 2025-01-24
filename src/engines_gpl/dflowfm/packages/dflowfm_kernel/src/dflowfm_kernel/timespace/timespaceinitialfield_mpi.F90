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
module m_timespaceinitialfield_mpi

   implicit none

contains
   !> perform interpolation on rank 0 only (and save some memory with multiple ranks on one node)
   !>   note: only methods "4" (in polygon) and "5" (trangulation) supported, averaging (method "6") not supported
   function timespaceinitialfield_mpi(x, y, z, N, filename, filetype, method, operand, transformcoef, iprimpos, kc) result(success)
      use m_partitioninfo
      use timespace, only: timespaceinitialfield
      use fm_external_forcings_data, only: NTRANSFORMCOEF
#ifdef HAVE_MPI
      use mpi
#endif
      implicit none

      logical :: success

      integer, intent(in) :: N !< data size
      real(kind=dp), dimension(N), intent(in) :: x(N) !< x-coordinates
      real(kind=dp), dimension(N), intent(in) :: y(N) !< y-coordinates
      real(kind=dp), dimension(N), intent(out) :: z(N) !< interpolated values
      integer, dimension(N), intent(in) :: kc(N) !< 0=no, 1 = yes

      character(*), intent(in) :: filename !< name of data file
      integer, intent(in) :: filetype !< file type
      integer, intent(in) :: method !< interpolation method, only "4" and "5" supported
      character(1), intent(in) :: operand !< override, add
      real(kind=dp), dimension(NTRANSFORMCOEF), intent(in) :: transformcoef !< Transformation coefficients
      integer, intent(in) :: iprimpos !< only needed for averaging, but not supported

      real(kind=dp), dimension(:), allocatable :: xall, yall, zall

      integer, dimension(:), allocatable :: nums, offset, kcall

      integer :: numtot
      integer :: i, ierror

#ifdef HAVE_MPI
      if ((method == 4 .or. method == 5) .and. (jampi == 1)) then
!        ( inside polygon or triangulation ) and parallel run

!        allocate
         allocate (nums(0:ndomains - 1))

!        communicate array size to rank 0
         call mpi_gather(N, 1, MPI_INTEGER, nums, 1, MPI_INTEGER, 0, DFM_COMM_DFMWORLD, ierror)

         if (my_rank == 0) then
            numtot = sum(nums(0:ndomains - 1))
         else
            numtot = 0
         end if

         allocate (xall(numtot))
         allocate (yall(numtot))
         allocate (zall(numtot))
         allocate (kcall(numtot))
         allocate (offset(0:ndomains - 1))

         offset(0) = 0
         do i = 0, ndomains - 2
            offset(i + 1) = offset(i) + nums(i)
         end do

!        communicate coordinates to rank 0 (split for simplicity)
         call mpi_gatherv(x, N, MPI_DOUBLE_PRECISION, xall, nums, offset, MPI_DOUBLE_PRECISION, 0, DFM_COMM_DFMWORLD, ierror)
         call mpi_gatherv(y, N, MPI_DOUBLE_PRECISION, yall, nums, offset, MPI_DOUBLE_PRECISION, 0, DFM_COMM_DFMWORLD, ierror)
         call mpi_gatherv(z, N, MPI_DOUBLE_PRECISION, zall, nums, offset, MPI_DOUBLE_PRECISION, 0, DFM_COMM_DFMWORLD, ierror)
         call mpi_gatherv(kc, N, MPI_INTEGER, kcall, nums, offset, MPI_INTEGER, 0, DFM_COMM_DFMWORLD, ierror)

         if (my_rank == 0) then
!           perform interpolation on rank 0
            success = timespaceinitialfield(xall, yall, zall, numtot, filename, filetype, method, operand, transformcoef, iprimpos, kcall)
         else
            success = .true.
         end if

!        send interpolated data to other ranks
         call mpi_scatterv(zall, nums, offset, MPI_DOUBLE_PRECISION, z, N, MPI_DOUBLE_PRECISION, 0, DFM_COMM_DFMWORLD, ierror)

!        deallocate local arrays
         deallocate (nums)
         deallocate (xall)
         deallocate (yall)
         deallocate (zall)
         deallocate (kcall)
         deallocate (offset)
      else
         success = timespaceinitialfield(x, y, z, N, filename, filetype, method, operand, transformcoef, iprimpos, kc)
      end if

#else
      success = timespaceinitialfield(x, y, z, N, filename, filetype, method, operand, transformcoef, iprimpos, kc)
#endif

      return
   end function timespaceinitialfield_mpi

end module m_timespaceinitialfield_mpi
