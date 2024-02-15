#ifdef HAVE_CONFIG_H
#include "config.h"
#endif 

module wave_mpi
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
! Module parameters
#ifdef HAVE_MPI
use mpi
#endif
use check_mpi_env
!
! Module parameters
!
#ifndef HAVE_MPI
integer, parameter    :: MPI_COMM_NULL = -999
#endif
integer, parameter    :: master = 0                          !< rank of master process
!
integer, parameter    :: SWAN_GO   = 1
integer, parameter    :: SWAN_DONE = 2
!
integer, target       :: engine_comm_world = MPI_COMM_NULL   !< communicator to be used by D-Waves (can be get/set via BMI)
integer               :: numranks = 1                        !< number of ranks
integer               :: my_rank = 0                         !< own rank
logical               :: mpi_initialized_by_engine = .FALSE. !< flag indicating whether MPI has been initialized by this engine (if so, finalize as well)
private               :: running_in_mpi_environment
!
    contains
!
!===============================================================================
subroutine initialize_wave_mpi()
   character(256)       :: msgstr
   logical              :: mpi_is_initialized
   integer              :: ierr = 0
   !
   ! MPI mode
   !
   mpi_initialized_by_engine  = .FALSE.
   if ( running_in_mpi_environment() ) then
      ! Running inside MPI environment.
      ! * parallel wave.exe
      ! * parallel wave-lib called by parallel dimr.exe
      ! * non-parallel wave-lib called by parallel dimr.exe
#ifdef HAVE_MPI
      call mpi_initialized( mpi_is_initialized, ierr )
      if ( ierr /= MPI_SUCCESS ) then
         write (msgstr,'(a,i5)') 'MPI produces some internal error in mpi_initialized - return code is ',ierr
         call wavestop( 1, msgstr )
      endif
#else
      mpi_is_initialized = .FALSE.
#endif
      !
      if (.not.mpi_is_initialized) then
         ! * parallel wave.exe
         mpi_initialized_by_engine = .TRUE.
#ifdef HAVE_MPI
         call mpi_init ( ierr )
         engine_comm_world = MPI_COMM_WORLD
#endif
      endif
      !
      if (engine_comm_world /= MPI_COMM_NULL) then
         ! * parallel wave.exe
         ! * parallel wave-lib called by parallel dimr.exe
         ! inquire size and rank
#ifdef HAVE_MPI
         call mpi_comm_size ( engine_comm_world, numranks, ierr )
         call mpi_comm_rank ( engine_comm_world, my_rank , ierr )
#else
         call wavestop( 1, 'Trying to run WAVE in parallel without MPI support.' )
#endif
      else
         ! * non-parallel wave-lib called by parallel dimr.exe
      endif
   else
      ! Running outside MPI environment.
      ! * non-parallel wave.exe
      ! * wave-lib called by non-parallel dimr.exe
      ! Don't call MPI.
   endif
end subroutine initialize_wave_mpi
!
!
!===============================================================================
subroutine finalize_wave_mpi()
   integer :: ierr
#ifdef HAVE_MPI
   if (mpi_initialized_by_engine) then
      call mpi_finalize(ierr)
   endif
#endif
end subroutine finalize_wave_mpi


subroutine wave_mpi_bcast(command, ierr)
   integer :: command
   integer :: ierr
#ifdef HAVE_MPI
   call mpi_bcast ( command, 1, MPI_INTEGER, master, engine_comm_world, ierr )
#endif
end subroutine wave_mpi_bcast


subroutine wave_mpi_barrier(ierr)
   integer :: ierr
#ifdef HAVE_MPI
   call mpi_barrier ( engine_comm_world, ierr )
#endif
end subroutine wave_mpi_barrier

end module wave_mpi
