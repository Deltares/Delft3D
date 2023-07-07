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

! @file
!     Auxiliary routines for reporting progress
!
!> Module for reporting progress
module report_progress

    implicit none

    type progress_data
        private
        logical           :: required        ! Whether the progress file is required
        real              :: secsprev        ! Previous time
        integer           :: istep           ! Current step
        integer           :: nstep           ! Number of steps
        character(len=80) :: filename        ! Name of the file
    end type progress_data


contains

!> Initialise the progress data structure
subroutine initialise_progress( progress, nstep, filename )

    type(progress_data), intent(inout) :: progress     !< Structure for reporting the progress
    integer, intent(in)                :: nstep        !< Number of steps in computation
    character(len=*), intent(in)       :: filename     !< Name of the file to use

    progress%istep    = 0
    progress%nstep    = nstep
    progress%required = filename /= ' '

    call cpu_time( progress%secsprev )

    progress%filename = filename

end subroutine initialise_progress

!> Write the progress file
subroutine write_progress( progress )

    type(progress_data), intent(inout) :: progress     !< Structure for reporting the progress

    integer :: lun
    real    :: secsnow

    if ( .not. progress%required ) then
        return
    endif

    progress%istep = progress%istep + 1

    call cpu_time( secsnow )

    if ( secsnow-progress%secsprev .gt. 0.10 .or. progress%istep >= progress%nstep ) then
        open( newunit = lun, file = progress%filename )
        write( lun, * ) 1.0, progress%nstep, progress%istep, 1, 1, 0.0
        close( lun )
        progress%secsprev = secsnow
    endif

end subroutine write_progress

end module report_progress
