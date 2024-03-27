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
module m_check
    use m_waq_precision
    use m_error_status

    implicit none

contains


    subroutine check(cdummy, iwidth, iblock, ierr2, status)

        !< Handles delimiter lines and errors during read of the DELWAQ input file
        !< Logical units     : LUNUT = unitnumber output log-file
        !<                     ILUN  = array with input unit nr's stack

        use m_srstop
        use rd_token
        use timers       !   performance timers

        implicit none

        !     Parameters

        character*(*), intent(inout) :: cdummy !< character that may contain block end

        integer(kind = int_wp), intent(in) :: iblock !< number of the input block
        integer(kind = int_wp), intent(inout) :: ierr2  !< accumulative nr of errors
        integer(kind = int_wp), intent(in) :: iwidth !< width of the output file
        type(error_status), intent(inout) :: status !< current error status

        !     Local
        character(1) :: chulp ! to convert block number to character

        integer(kind = int_wp) :: idummy ! argument for token reading
        integer(kind = int_wp) :: ihulp  ! return value gettoken
        integer(kind = int_wp) :: ithndl = 0
        integer(kind = int_wp) :: itype  ! argument for token reading

        real(kind = real_wp) :: rdummy ! argument for token reading

        if (timon) call timstrt("check", ithndl)

        !        First round of dealing with ierr2

        select case (ierr2)
        case (0)               !   look for end of block
            ihulp = gettoken (cdummy, idummy, rdummy, itype, ierr2)
        case (1)               !   with error
            write (lunut, 2030) iblock
            call status%increase_error_count()
            ierr2 = 0
        case (2)               !   normal end of block found
        case (3)               !   fatal
            write (lunut, 2030) iblock
            write (lunut, 2040) status%ierr
            call srstop (1)
        end select

        !        Second round of dealing with ierr2

        select case (ierr2)
        case (0)               !   look for end of block
            call status%increase_error_count()
            write (lunut, 2010) iblock
            do while (ierr2 == 0)
                ihulp = gettoken (cdummy, idummy, rdummy, itype, ierr2)
            enddo
        case (3)               !   fatal
            write (lunut, 2020) iblock
            write (lunut, 2040) status%ierr
            call srstop (1)
        end select

        if (ierr2 == 2) then          !   end block found check number
            write (chulp, '(i1)') iblock
            if (chulp /= cdummy(2:2)) then
                write (lunut, 2020) iblock
                call status%increase_error_count()
                write (lunut, 2040) status%ierr
                call srstop (1)
            endif
        ELSE                              !   error reading
            call status%increase_error_count()
            write (lunut, 2030) iblock
            write (lunut, 2040) status%ierr
            call srstop (1)
        endif

        !        normal end (can be with error)

        if (iwidth == 5) then
            write (lunut, 2050) iblock
        else
            write (lunut, 2060) iblock
        endif
        ierr2 = 0
        if (timon) call timstop(ithndl)
        return

        !        output formats

        2010 format (/' ERROR. Too many and/or invalid data in block', I4, ' !!', &
                ' Check input file !!', /)
        2020 format (/' ERROR. End card of block', I4, ' not found !!', &
                ' Check input file !!', /)
        2030 format (/' ERROR. Reading block', I4, ' !!', &
                ' Check input file !!', /)
        2040 format (' Number of ERRORS during read of input file:', I5, ' !!', / &
                ' Further processing impossible.', /, &
                ' *************** EXECUTION HALTED !! ***************')
        2050 format (/1X, 59('*'), ' B L O C K -', I2, ' ', 5('*')/)
        2060 format (/1X, 109('*'), ' B L O C K -', I2, ' ', 5('*')/)

    end

end module m_check
