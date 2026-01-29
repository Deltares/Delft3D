!!  Copyright (C)  Stichting Deltares, 2012-2026.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

module m_wq_processes_mpi_stubs
   !> These subroutines are stubs for functions called within the processes library
   !! for processes that require a check if a segment is in the current domain (and
   !! and not a ghost cell), and for MPI communication between domains (reduce_sum)
   !! when the processes library is integrated in D-Flexible Mesh. In Delwaq, just
   !! return results as if it is a single domain run.
   implicit none
contains
   subroutine wq_processes_mpi_init()
      use m_wq_processes_mpi, only: wq_processes_mpi, wq_processes_mpi_subroutines
      implicit none
      ! Register pointers to MPI subroutines
      wq_processes_mpi = wq_processes_mpi_subroutines(wq_processes_mpi_mydomain, wq_processes_mpi_reduce_sum, wq_processes_mpi_reduce_int_max)
   end subroutine wq_processes_mpi_init

   subroutine wq_processes_mpi_mydomain(cellid, mydomain)
      integer, intent(in) :: cellid
      logical, intent(out) :: mydomain
      mydomain = .true.
   end subroutine wq_processes_mpi_mydomain

   subroutine wq_processes_mpi_reduce_sum(size_wq_processes_data, wq_processes_data)
      use m_waq_precision, only: sp
      integer, intent(in) :: size_wq_processes_data
      real(sp), intent(inout) :: wq_processes_data(size_wq_processes_data)
   end subroutine wq_processes_mpi_reduce_sum

   subroutine wq_processes_mpi_reduce_int_max(wq_processes_data)
      integer, intent(inout) :: wq_processes_data
   end subroutine wq_processes_mpi_reduce_int_max
end module m_wq_processes_mpi_stubs
