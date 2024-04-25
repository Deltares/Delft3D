#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
subroutine dfexitmpi ( iexit )
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
!   Exit or abort parallel application
!
!!--pseudo code and references--------------------------------------------------
!
!   if MPI has been initialized
!      synchronize nodes
!      if error
!         abort MPI
!      else
!         close MPI
!
!
!!--declarations----------------------------------------------------------------
#ifdef HAVE_MPI
    use mpi
#endif
    use dfparall
    !
    implicit none
!
! Global variables
!
    integer :: iexit          ! exit return value
!
! Local variables
!
    integer :: ierr           ! error value of MPI call
    logical :: mpi_is_initialized       ! if true, parallel process is carried out with MPI
    logical :: mpi_is_finalized         ! if true, mpi_finalize has already been called
!
!! executable statements -------------------------------------------------------
!
#ifdef HAVE_MPI
    ! Don't stop MPI if this component didn't start it
    if (.not.mpi_initialized_by_engine) return
    !
    call mpi_initialized ( mpi_is_initialized, ierr )
    call mpi_finalized   ( mpi_is_finalized  , ierr )
    if ( mpi_is_initialized .and. .not. mpi_is_finalized ) then

       call mpi_barrier ( engine_comm_world, ierr )

       if ( iexit /= 0 ) then

       ! in case of an error abort all MPI processes

          call mpi_abort ( engine_comm_world, iexit, ierr )

       else

       ! otherwise stop MPI operations on this computer

          call mpi_finalize ( ierr )

       endif

    endif
#endif

end subroutine dfexitmpi
