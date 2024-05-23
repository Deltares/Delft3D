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
module m_dlwq13
    use m_waq_precision

    implicit none

contains


    !> Writes a restart file (complete system dump) after making all NaN values
    !! in the concentration array equal to 0
    subroutine write_restart_file(file_unit_list, lchar, conc, itime, mname, &
            sname, notot, noseg)

        use m_open_waq_files
        use timers

        integer(kind = int_wp), intent(inout) :: file_unit_list(*)                !< logical unit numbers of output files
        character(len=*),       intent(in)    :: lchar(*)                         !< names of output files
        real(kind = real_wp),   intent(inout) :: conc(notot, noseg) !< concentration values
        integer(kind=int_wp),   intent(in)    :: itime                            !< present time in clock units
        character(len=40),      intent(in)    :: mname(*)                         !< model identification
        character(len=20),      intent(in)    :: sname(*)                         !< names of substances
        integer(kind=int_wp),   intent(in)    :: notot                    !< total number of systems
        integer(kind=int_wp),   intent(in)    :: noseg                      !< total number of segments or cells


        character(len=255) restart_file_name
        integer(kind = int_wp) :: i, j
        integer(kind = int_wp) :: nan_count, ierr
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("dlwq13", ithandl)

        ! check for NaNs
        nan_count = 0
        do j = 1, noseg
            do i = 1, notot
                if (conc(i, j) /= conc(i, j)) then
                    conc(i, j) = 0.0
                    nan_count = nan_count + 1
                endif
            enddo
        enddo
        if (nan_count /= 0) then
            write (file_unit_list(19), *) ' Corrected concentrations as written to the restart file:'
            write (file_unit_list(19), *) ' Number of values reset from NaN to zero: ', nan_count
            write (file_unit_list(19), *) ' Total amount of numbers in the array: ', notot * noseg
            write (file_unit_list(19), *) ' This may indicate that the computation was unstable'
        endif

        ! Set name for restart file
        restart_file_name = ' '
        restart_file_name(1:248) = lchar(23)(1:248)
        i = index(restart_file_name, '.', back = .true.)
        if (i == 0) then
            write (*, *) ' Invalid name of restart MAP file !'
            write (*, *) ' Restart file written to restart_temporary.map !'
            write (file_unit_list(19), *) ' Invalid name of restart MAP file !'
            write (file_unit_list(19), *) ' Restart file written to restart_temporary.map !'
            restart_file_name = 'restart_temporary.map'
        else
            restart_file_name(i:i + 7) = "_res.map"
        end if

        ! write restart file in .map format
        call open_waq_files (file_unit_list(23), restart_file_name, 23, 1, ierr)
        write (file_unit_list(23)) (mname(i), i = 1, 4)
        write (file_unit_list(23))   notot, noseg
        write (file_unit_list(23)) (sname(i), i = 1, notot)
        write (file_unit_list(23)) itime, conc
        close (file_unit_list(23))

        if (timon) call timstop (ithandl)
    end subroutine write_restart_file
end module m_dlwq13
