#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
subroutine dfreduce ( iptr, ilen, itype, ityprd, error, msgstr )
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
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
!   Performs a global reduction of type ITYPRD on
!   array IPTR of type ITYPE to collect values from
!   all processes
!
!!--pseudo code and references--------------------------------------------------
!
!   wrapper for MPI_ALLREDUCE
!
!
!!--declarations----------------------------------------------------------------
    use precision
#ifdef HAVE_MPI
    use mpi
#endif
    use dfparall
    !
    implicit none
!
! Global variables
!
    integer     , intent(inout) :: iptr   ! pointer to first element of array to be reduced
    integer     , intent(in)    :: ilen   ! length of array to be reduced
    integer     , intent(in)    :: itype  ! type of data
    integer     , intent(in)    :: ityprd ! type of reduction operator
    logical     , intent(out)   :: error  ! error flag
    character(*), intent(out)   :: msgstr ! string to pass message
!
! Local variables
!
    integer       :: ierr   ! error value of MPI call
    integer       :: istat  ! error value of memory (de)allocation
    !
    integer , dimension(:), allocatable :: ibuff
    real(sp), dimension(:), allocatable :: sbuff
    real(hp), dimension(:), allocatable :: dbuff
!
!! executable statements -------------------------------------------------------
!
    msgstr = ' '
    error  = .false.
    !
    ! if not parallel, return
    !
    if (.not.parll) return
    !
#ifdef HAVE_MPI
    if ( itype == dfint ) then
        allocate(ibuff(ilen), stat = istat)
        if (istat /= 0) goto 999
        ibuff = 0
        call mpi_allreduce ( iptr, ibuff, ilen, itype, ityprd, engine_comm_world, ierr )
    else if ( itype == dfreal ) then
        allocate(sbuff(ilen), stat = istat)
        if (istat /= 0) goto 999
        sbuff = 0.0_sp
        call mpi_allreduce ( iptr, sbuff, ilen, itype, ityprd, engine_comm_world, ierr )
    else if ( itype == dfdble ) then
        allocate(dbuff(ilen), stat = istat)
        if (istat /= 0) goto 999
        dbuff = 0.0_hp
        call mpi_allreduce ( iptr, dbuff, ilen, itype, ityprd, engine_comm_world, ierr )
    endif
    !
    if ( ierr /= MPI_SUCCESS ) then
       write (msgstr,'(a,i5)') 'MPI produces some internal error - return code is ',ierr
       error = .true.
       return
    endif
    !
    if ( itype == dfint ) then
       call cparri ( ibuff, iptr, ilen )
       deallocate(ibuff, stat = istat)
    else if ( itype == dfreal ) then
       call cparrr ( sbuff, iptr, ilen )
       deallocate(sbuff, stat = istat)
    else if ( itype == dfdble ) then
       call cparrd ( dbuff, iptr, ilen )
       deallocate(dbuff, stat = istat)
    endif
    if (istat /= 0) goto 999
    !
    return
999 write (msgstr,'(a,i5)') 'Memory (de)allocation problem in DFREDUCE - return code is ',istat
#endif
end subroutine dfreduce
