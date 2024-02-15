subroutine wait_until_finished (no_dis, waitfiles, idis, filename, waitlog, error, gdp)
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
!  $Id: wait_until_finished.f90 140675 2022-01-27 11:03:54Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/wait_until_finished.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Compute the jet trajectory in "world" coordinates from cortim results
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use globaldata
    use flow2d3d_timers
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp), pointer :: nf_timeout
!
! Global variables
!
    integer                        , intent(in)  :: no_dis
    character(*), dimension(no_dis)              :: waitfiles
    integer                        , intent(out) :: idis
    character(*)                   , intent(out) :: filename
    logical                        , intent(in)  :: waitlog
    logical                        , intent(out) :: error
!
! Local variables
!
    integer , external      :: newlun
    integer                 :: lun
    integer                 :: ios
    integer                 :: i
    integer                 :: sleeptime
    integer(long)           :: tcount
    integer(long)           :: trate
    integer(long)           :: tmax
    real(hp)                :: tstart
    real(hp)                :: tnow
    real(hp)                :: twait
    logical                 :: ex_file    ! true: file exists
    logical                 :: fileok     ! true: file contains the end tag </NF2FF>
    logical                 :: opend
    character(300)          :: line
!
!! executable statements -------------------------------------------------------
!
    nf_timeout => gdp%gdnfl%nf_timeout
    error = .false.
    ! Check for how many files we are waiting to appear
    idis = 0
    do i=1, no_dis
       if (waitfiles(i) /= ' ') then
          idis = i
          if (waitlog) then
             write(*,'(3a)') "Waiting for file '", trim(waitfiles(i)), "' to appear ..."
          endif
       endif
    enddo
    !
    ! Return when all files did appear (and waitfiles is empty). idis must be 0.
    if (idis == 0) return
    !
    fileok = .false.
    do while (.not.fileok)
       ex_file = .false.
       !
       ! Examine if one of the files exists
       ! This will cost CPU time, but there is nothing else to do (Cosumo/Cormix run on another machine)
       !
       call timer_start(timer_wait, gdp)
       call system_clock(tcount, trate, tmax)
       tstart   = real(tcount,hp) / real(trate,hp)
       idis     = 0
       filename = ' '
       do while (.not. ex_file)
          do i=1, no_dis
             if (waitfiles(i) /= ' ') then
                inquire (file=waitfiles(i), exist=ex_file)
                if (ex_file) then
                   filename     = waitfiles(i)
                   idis         = i
                   !
                   ! Do not remove the found file from the waitfiles here:
                   ! It will be removed in near_field, when the full reading of the file finished successfully
                   !
                   exit
                endif
             endif
          enddo
          call system_clock(tcount, trate, tmax)
          tnow   = real(tcount,hp) / real(trate,hp)
          twait  = tnow - tstart
          if (twait/60.0 > nf_timeout) then
             write(*,*) "ERROR: Timeout: waited ", twait, " minutes for nearfield files to appear. Aborting."
             call prterr(gdp%gdinout%lundia, 'P004', "Timeout. nearfield files did not appear in specified NfTimeout period. Aborting.")
             call d3stop(1, gdp)
          endif
       enddo
       call timer_stop(timer_wait, gdp)
       !
       ! File found: open file and search for the end tag </NF2FF>
       !
       lun = newlun(gdp)
       inquire(lun, iostat=ios, opened=opend)
       if (ios /= 0) then
          ! try again
          cycle
       endif
       if (opend) close(lun, iostat=ios)
       open (lun,file=filename,iostat=ios)
       if (ios /= 0) then
          ! try again
          cycle
       endif
       ios    = 0
       do while (ios == 0)
          read (lun,'(a)',iostat=ios) line
          if (index(line,'</NF2FF>') >= 1) then
             fileok = .true.
          endif
       enddo
       close(lun)
    enddo
    write(*,'(3a)') "Scanned    file '", trim(filename), "'"
end subroutine wait_until_finished
