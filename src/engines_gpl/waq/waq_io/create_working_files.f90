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
module m_working_files
    use m_waq_precision

    implicit none

contains


    subroutine create_work_file_one(lun, lchar, nolun, runid)
        !! Reads the input filename* ( keyboard /command line ) ;
        !! sets filenames* ; opens system files
        !! the subroutine creates the following files lst, delwaq04.wrk, harmonic.wrk, pointers.wrk, filenaam.wrk files
        !! Logical units     : 5       = keyboard
        !!                     lun(26) = unit user input file
        !!                     lun(27) = unit stripped input file
        !!                     lun(29) = unit formatted output file
        !!                     lun( 2) = unit system-intermediate file
        !!                     lun( 3) = unit intermediate file (harmonics)
        !!                     lun( 4) = unit intermediate file (pointers)

        use m_srstop
        use m_monsys
        use m_cli_utils, only : retrieve_command_argument, get_input_filename
        use m_get_filepath_and_pathlen
        use m_open_waq_files
        use timers
        use data_processing, only : delete_file

        implicit none

        integer(kind = int_wp), intent(in) :: nolun           !< Amount of unit numbers
        integer(kind = int_wp), intent(inout) :: lun(nolun)      !< Unit numbers
        character(*), intent(inout) :: lchar(nolun)    !< File names
        character(*), intent(inout) :: runid           !< Runid

        ! Local

        integer(kind = int_wp) :: ilun
        integer(kind = int_wp) :: ioerr
        character*(93) :: check
        logical :: specout
        integer(kind = int_wp) :: idummy
        real (kind = real_wp) :: rdummy
        character*(256) :: outputpath
        character*(256) :: outputpath2
        character*(256) :: runidpath
        integer(kind = int_wp) :: pathlen
        integer(kind = int_wp) :: outpathlen
        character*(256) :: outid
        integer(kind = int_wp) :: ierr2

        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("create_work_file_one", ithndl)

        ! Get filename  ( keyboard / command line )

        check = lchar(29)
        call get_input_filename(runid, check)

        ! Specific output dir?
        call retrieve_command_argument ('-output', 3, specout, idummy, rdummy, outputpath, ierr2)
        if (specout) then
            if (ierr2==0) then
                write (*, '(A)') 'Found -output switch with the following path:'
                write (*, '(/A)') trim(outputpath)
                write (*, '(/A/)') 'Make sure this path exists, or DELWAQ will not run!'
                call get_filepath_and_pathlen (runid, runidpath, pathlen)
                call get_filepath_and_pathlen (outputpath, outputpath2, outpathlen)
                if (outpathlen == 0 .or. outputpath(1:1) == '.') then
                    ! No dir indicators found or it starts with a dot, asume it is a local subdir
                    outputpath = trim(runidpath) // trim(outputpath)
                endif
                if (pathlen == 0) then
                    outid = trim(outputpath) // '/' // runid
                else
                    outid = trim(outputpath) // '/' // trim(runid(pathlen + 1:))
                endif
            else
                write (*, '(A/)') 'Found -output switch but not path specified. This will be ignored.'
                specout = .false.
            endif
        endif

        !  Pad the model name in the file names
        do ilun = 1, nolun
            if (specout .and. index(lchar(ilun), '.wrk') == 0 .and. index(lchar(ilun), '.inp') == 0) then
                lchar(ilun) = trim(outid) // lchar(ilun)
            else
                lchar(ilun) = trim(runid) // lchar(ilun)
            endif
        enddo

        ! Remove any existing work files

        do ilun = 1, nolun
            if (index(lchar(ilun), '.wrk') > 0) call delete_file(lchar(ilun), ioerr)
        enddo

        ! Open the neccessary unit numbers
        ! create the lst file
        call open_waq_files(lun(29), lchar(29), 29, 1, ioerr)
        !
        call setmlu(lun(29))
        ! open the input file (.inp)
        call open_waq_files(lun(26), lchar(26), 26, 1, ioerr)
        if (ioerr > 0) then
            write (lun(29), 1000) lun(26), lchar(26)
            call srstop (1)
        endif
        ! create the delwaq04.wrk binary file
        call open_waq_files(lun(2), lchar(2), 2, 1, ioerr)
        ! create the harmonic.wrk file
        call open_waq_files(lun(3), lchar(3), 3, 1, ioerr)
        ! create the pointers.wrk file
        call open_waq_files(lun(4), lchar(4), 4, 1, ioerr)
        ! create the filenaam.wrk file
        call open_waq_files(lun(41), lchar(41), 41, 1, ioerr)

        if (timon) call timstop(ithndl)
        return

        ! Output formats

        1000 format (/' ERROR input file on unit:', I3, &
                /' Filename = ', A, &
                /' Does not exist.', &
                /' EXECUTION HALTED !!!!!!!!!!!!!')
    end subroutine create_work_file_one

end module m_working_files
