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

module m_wq_processes_mpi
   !> This module defines an interface for MPI subroutines that can be used in water quality processes.
   !! This is needed for the dredge process, because in parallel multi-domain runs, the dump location
   !! can be in a different domain than the dredge location. It contains pointers to subroutines that
   !! handle domain checks and reductions of data across MPI processes with their abstract interfaces.
   !! Because we don't want Delwaq to be depended on MPI, it will point to dummy subroutines.
   !! D-FM will point to real MPI subroutines that are needed when running on multiple domains.
   !! The pointers to these subroutines must be set during initialisation.
   type :: wq_processes_mpi_subroutines
      procedure(wq_processes_mpi_mydomain), pointer, nopass :: mydomain
      procedure(wq_processes_mpi_reduce_sum), pointer, nopass :: reduce_sum
      procedure(wq_processes_mpi_reduce_int_max), pointer, nopass :: reduce_int_max
   end type wq_processes_mpi_subroutines
   type(wq_processes_mpi_subroutines) :: wq_processes_mpi

   abstract interface
      subroutine wq_processes_mpi_mydomain(cellid, mydomain)
         integer, intent(in) :: cellid
         logical, intent(out) :: mydomain
      end subroutine wq_processes_mpi_mydomain

      subroutine wq_processes_mpi_reduce_sum(size_wq_processes_data, wq_processes_data)
         use m_waq_precision, only: sp
         integer, intent(in) :: size_wq_processes_data
         real(sp), intent(inout) :: wq_processes_data(size_wq_processes_data)
      end subroutine wq_processes_mpi_reduce_sum

      subroutine wq_processes_mpi_reduce_int_max(wq_processes_data)
         integer, intent(inout) :: wq_processes_data
      end subroutine wq_processes_mpi_reduce_int_max
   end interface
end module m_wq_processes_mpi
