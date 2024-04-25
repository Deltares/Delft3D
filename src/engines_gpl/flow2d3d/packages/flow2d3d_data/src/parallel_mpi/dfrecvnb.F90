#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
subroutine dfrecvnb ( iptr, ilen, itype, isource, itag, gdp )
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
!   Data is received from a neighbour
!
!!--pseudo code and references--------------------------------------------------
!
!   wrapper for MPI_RECV
!
!
!!--declarations----------------------------------------------------------------
#ifdef HAVE_MPI
    use mpi
#endif
    use dfparall
    use globaldata
    !
    implicit none
    !
    type(globdat), target    :: gdp
!
! Global variables
!
    integer, intent(out) :: iptr    ! pointer to first element of array to be received
    integer, intent(in)  :: ilen    ! length of array to be received
    integer, intent(in)  :: itype   ! type of data
    integer, intent(in)  :: isource ! rank of the source process
    integer, intent(in)  :: itag    ! message type
!
! Local variables
!
    integer, pointer :: lundia
    integer       :: ierr                   ! error value of MPI call
#ifdef HAVE_MPI
    integer       :: istat(mpi_status_size) ! MPI status array
#endif
    character(80) :: msgstr                 ! string to pass message
!
!! executable statements -------------------------------------------------------
!
    lundia => gdp%gdinout%lundia
    !
    ! if not parallel, return
    !
    if (.not.parll) return
    !
#ifdef HAVE_MPI
    call mpi_recv ( iptr, ilen, itype, isource-1, itag, engine_comm_world, istat, ierr )
    if ( ierr /= MPI_SUCCESS ) then
       write (msgstr,'(a,i5,a,i3.3)') 'MPI produces some internal error - return code is ',ierr,' and node number is ',inode
       call prterr(lundia, 'U021', trim(msgstr))
       call d3stop(1, gdp)
    endif
#endif

end subroutine dfrecvnb
