!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
module m_startup_screen
    use m_waq_precision

    implicit none

contains


    subroutine startup_screen(lunrep)
        !! Write intro to screen and report file

        use m_getidentification
        use timers
        use m_dattim

        implicit none

        integer(kind = int_wp), intent(in) :: lunrep   !< Unit number report file

        ! local declarations
        save

        character*20  run_date_time
        character*120 version_string
        logical       first
        integer(kind = int_wp) :: i, j
        save          first
        character*75  startup_screen_text(8)

        data     first /.true./
        data     startup_screen_text  / &
                '+-----------------------------------------------------------------------+', &
                '|                      Delft3D / D-HYDRO - DELWAQ                       |', &
                '|                                                                       |', &
                '| D-Water Quality         Water quality simulation and                  |', &
                '|                         algae simulation in 1D/2D/3D models           |', &
                '|                                                                       |', &
                '| Version xx.xxxx  xx-xx-xxxx                                           |', &
                '+-----------------------------------------------------------------------+'/

        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("startup_screen", ithndl)

        ! set version_string
        call getidentification(version_string)

        if (first) then
            first = .false.
            do i = 1, size(startup_screen_text)
                if (startup_screen_text(i)(3:15) == 'Version xx.xx') then
                    write(startup_screen_text(i)(3:72), '(a)') version_string(1:70)
                end if
                write(*, *) startup_screen_text(i)
            enddo
        endif
        write (lunrep, '(1x,a)') trim(version_string)
        call dattim(run_date_time)
        write (lunrep, '(2a)') ' Execution start: ', run_date_time

        if (timon) call timstop(ithndl)

    end subroutine startup_screen
end module m_startup_screen
