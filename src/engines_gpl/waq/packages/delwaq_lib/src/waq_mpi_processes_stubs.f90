!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

!  These subroutines are stubs for functions called within the processes library
!  for processes that require a check if a segment is in the current domain (and
!  and not a ghost cell), and for MPI communication between domains (reduce_sum)
!  when the processes library is integrated in D-Flexible Mesh. In Delwaq, just
!  return results as if it is a single domain run.

   logical function wq_processes_mydomain(iseg)
  
   implicit none
   
   integer :: iseg

   wq_processes_mydomain = .true.

   end function wq_processes_mydomain

   
   logical function reduce_sum_wq_processes(size_wq_processes_data, wq_processes_data)
   
   implicit none

   integer             :: size_wq_processes_data
   real                :: wq_processes_data(size_wq_processes_data)

   reduce_sum_wq_processes = .true.

   end function reduce_sum_wq_processes

   logical function reduce_int_max_wq_processes(wq_processes_data)
   
   implicit none

   integer             :: wq_processes_data

   reduce_int_max_wq_processes = .true.

   end function reduce_int_max_wq_processes
