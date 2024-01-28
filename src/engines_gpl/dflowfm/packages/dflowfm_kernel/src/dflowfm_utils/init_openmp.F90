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
integer function init_openmp(maxnumthreads, mpion) result(iresult)
#ifdef _OPENMP
   use omp_lib
#endif
   use dfm_error
   implicit none
   integer, intent(in   ) :: maxnumthreads !< Desired maximum number of OpenMP threads.
   integer, intent(in   ) :: mpion         !< Is MPI-mode currently on (1: yes, 0: no).

   iresult = DFM_NOERR

   if (mpion == 1) then
#ifdef _OPENMP
      ! If MPI is on for this model, *and* no user-define numthreads was set, then disable OpenMP.
      if (maxnumthreads == 0) then
         call omp_set_num_threads(1)
      ! TODO: AvD: else, reset to maximum? Especially in library mode when multiple models can be run after one another?
      else
         call omp_set_num_threads(maxnumthreads)
      end if
#endif
   else ! No MPI, but handle OpenMP settings:
#ifdef _OPENMP
      if (maxnumthreads > 0) then
         call omp_set_num_threads(maxnumthreads)
      end if
#endif
   end if

end function init_openmp
   