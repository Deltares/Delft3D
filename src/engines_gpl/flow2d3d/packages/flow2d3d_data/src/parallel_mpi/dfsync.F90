#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
subroutine dfsync ( gdp )
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2016.
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
!  $Id: dfsync.F90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/dfsync.F90 $
!!--description-----------------------------------------------------------------
!
!   Synchronize nodes
!
!!--pseudo code and references--------------------------------------------------
!
!   wrapper for MPI_BARRIER
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
! Local variables
!
    integer, pointer       :: lundia
    integer                :: ierr   ! error value of MPI call
    character(80)          :: msgstr ! string to pass message
!
!! executable statements -------------------------------------------------------
!
    lundia => gdp%gdinout%lundia
    !
    ! if not parallel, return
    !
    if (.not.parll) return
    !
    ! blocks until all nodes have called this routine
    !
#ifdef HAVE_MPI
    call mpi_barrier ( MPI_COMM_WORLD, ierr )
    if ( ierr /= MPI_SUCCESS ) then
       write (msgstr,'(a,i5,a,i3.3)') 'MPI produces some internal error - return code is ',ierr,' and node number is ',inode
       call prterr(lundia, 'U021', trim(msgstr))
       call d3stop(1, gdp)
    endif
#endif

end subroutine dfsync
