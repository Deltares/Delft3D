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

!> Initializes OpenMP settings, when necessary.
!! Call this once initially, or after changing the max number of OpenMP threads setting.
!! When running in MPI-mode, OpenMP is switched off, unless (i.e., 1 OpenMP thread max).
module m_init_openmp

   implicit none

   private

   public :: init_openmp

contains

   integer function init_openmp(maxnumthreads, mpion) result(iresult)
#ifdef _OPENMP
      use omp_lib
#endif
      use dfm_error
      use messagehandling, only: mess, level_info
      
      integer, intent(in) :: maxnumthreads !< Desired maximum number of OpenMP threads.
      integer, intent(in) :: mpion !< Is MPI-mode currently on (1: yes, 0: no).

      integer :: openmp_threads
      iresult = DFM_NOERR
#ifndef _OPENMP
      associate (maxnumthreads => maxnumthreads) ! Required to prevent compiler error for unused variable in case OpenMP is not defined
      end associate
#endif

#ifdef _OPENMP
      if (mpion == 1 .and. maxnumthreads == 0) then
         ! If MPI is on for this model, *and* no user-define numthreads was set, then disable OpenMP.
            openmp_threads = 1
            ! TODO: AvD: else, reset to maximum? Especially in library mode when multiple models can be run after one another?
      else ! user defined OpenMP threads
            openmp_threads = maxnumthreads
      end if
      if (openmp_threads > 0) then
         call omp_set_num_threads(openmp_threads)
      end if
      openmp_threads = omp_get_max_threads() !check number of threads set by environment before reporting
      if (openmp_threads > 1) then
         call mess(LEVEL_INFO, 'OpenMP enabled, number of threads = ',openmp_threads)
      else
         call mess(LEVEL_INFO, 'OpenMP disabled.')
      end if
#endif

   end function init_openmp

end module m_init_openmp
