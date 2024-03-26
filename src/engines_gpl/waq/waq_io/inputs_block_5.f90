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
module inputs_block_5
    use m_waq_precision
    use m_string_utils
    use simulation_input_options, only : read_constants_time_variables
    use boundary_conditions, only : read_boundary_concentrations
    use m_error_status

    implicit none

    private
    public :: read_block_5_boundary_conditions

contains

    subroutine read_block_5_boundary_conditions(lun, lchar, filtype, char_arr, int_array, &
            real_array, nrftot, nrharm, nobnd, nosys, &
            notot, nobtyp, max_real_size, max_int_size, is_date_format, &
            iwidth, intsrt, is_yyddhh_format, sname, &
            max_char_size, output_verbose_level, status)
        !! Reads all inputs associated with open boundaries
        !! This routine reads:
        !!      - the boundary ID's (and names and types for modern files)
        !!      - the Tatcher-Harleman time lags
        !!      - the Open boundary concentrations
        !!
        !! Subroutines called : convert_time_format, read_constants_time_variables, CHECK, CNVTIM, RDTOK1
        !! Logical units : LUN(27) = unit stripped DELWAQ input file
        !                  LUN(29) = unit formatted output file
        !                  LUN( 2) = unit intermediate file (system)
        !                  LUN( 3) = unit intermediate file (harmonics)
        !                  LUN( 4) = unit intermediate file (pointers)
        !                  LUN(14) = unit intermediate file (boundaries)

        use error_handling, only : check_error
        use m_srstop
        use m_cli_utils, only : retrieve_command_argument
        use rd_token     !   for the reading of tokens
        use timers       !   performance timers
        use date_time_utils, only : convert_relative_time, convert_time_format

        integer(kind = int_wp), intent(in) :: max_real_size              !< size of the real workspace
        integer(kind = int_wp), intent(inout) :: lun(:)             !< array with unit numbers
        character(*), intent(inout) :: lchar(:)           !< array with file names of the files
        integer(kind = int_wp), intent(inout) :: filtype(*)         !< type of binary file
        character(*), intent(inout) :: char_arr(:)             !< character workspace
        integer(kind = int_wp), intent(inout) :: int_array(:)             !< integer workspace ( dump locations at entrance )
        real(kind = real_wp), intent(inout) :: real_array(max_real_size)         !< real    workspace
        integer(kind = int_wp), intent(inout) :: nrftot(*)          !< number of function items
        integer(kind = int_wp), intent(inout) :: nrharm(*)          !< number of harmonic items
        integer(kind = int_wp), intent(inout) :: nobnd              !< number of open model boundaries
        integer(kind = int_wp), intent(in) :: notot              !< total number of substances
        integer(kind = int_wp), intent(inout) :: nosys              !< number of transported substances
        integer(kind = int_wp), intent(out) :: nobtyp             !< number of open model boundary types
        integer(kind = int_wp), intent(in) :: max_int_size              !< size of the integer workspace
        logical, intent(in) :: is_date_format             !< 'date'-format 1st timescale
        integer(kind = int_wp), intent(in) :: iwidth             !< width of the output file
        integer(kind = int_wp), intent(in) :: intsrt             !< integration option
        logical, intent(in) :: is_yyddhh_format             !< 'date'-format (f;ddmmhhss,t;yydddhh)
        character(20), intent(inout) :: sname(:)           !< array with substance names
        integer(kind = int_wp), intent(in) :: max_char_size              !< size of the character workspace
        integer(kind = int_wp), intent(in) :: output_verbose_level             !< flag for more or less output

        integer(kind = int_wp) :: idef

        type(error_status) :: status !< current error status

        character*1   cdummy
        character*255 charachter_output
        logical       disper
        character(len = 20), allocatable :: bndid(:)             ! boundary id's 20 character
        character(len = 40), allocatable :: bndname(:)           ! boundary names
        character(len = 20), allocatable :: bndtype(:)           ! boundary types
        character(len = 256), allocatable :: bndid_long(:)        ! array to buffer the non truncated boundary id's
        character(len = 256), allocatable :: bndtype_long(:)      ! array to buffer the non truncated boundary types
        integer(kind = int_wp), allocatable :: ibndtype(:)         ! index boundary type
        real(kind = dp), allocatable :: dp_array(:)             !  double precission workspace (very large !lp)
        logical :: no_id_check            ! command line argument to skip double id check
        real(kind = real_wp) :: rdummy                  ! dummy real in argument list
        integer(kind = int_wp) :: idummy                  ! dummy integer in argument list
        integer(kind = int_wp) :: volume
        integer(kind = int_wp) :: ithndl = 0
        integer(kind = int_wp) :: k, i, ierr_alloc
        integer(kind = int_wp) :: ifact, binary_work_file, ierr2, iwar2, ifound, ityp2
        integer(kind = int_wp) :: iaropt, nover, mxover, ibnd, it, nosubs
        integer(kind = int_wp) :: ierrh, int_output, real_output, ifound2, l, itype
        if (timon) call timstrt("read_block_5_boundary_conditions", ithndl)

        ! init
        disper = .false.
        volume = 0
        ifact = 1
        lunut = lun(29)
        binary_work_file = lun(2)
        iposr = 0
        ierr2 = 0
        iwar2 = 0

        if (nobnd == 0) then
            write (lunut, 2000)
            ifound = gettoken (charachter_output, idummy, rdummy, itype, ierr2)
            if (ierr2 == 2) then
                goto 175
            else if (itype==2 .and. idummy==0) then
                write (lunut, 2120)
                goto 170
            endif
            write (lunut, 2001)
            call status%increase_error_count()
            goto 175
        endif

        ! read boundary names, from version 4.9 on names are id's names are 40 characters
        ! types are 20 characters
        allocate(bndid(nobnd), bndname(nobnd), bndtype(nobnd), bndid_long(nobnd), bndtype_long(nobnd), &
                ibndtype(nobnd), stat = ierr_alloc)
        if (ierr_alloc /= 0) then
            write (lunut, 2300) ierr_alloc
            write (lunut, 2310) nobnd
            call srstop(1)
        endif
        nobtyp = 0
        if (output_verbose_level < 3) then
            write (lunut, 2005)
        else
            if (iwidth == 5) then
                write (lunut, 2010)
            else
                write (lunut, 2020)
            endif
        endif
        ierr2 = 0

        do i = 1, nobnd
            ibndtype(i) = 0
            bndid_long(i) = ' '
            bndname(i) = ' '
            bndtype_long(i) = ' '

            ! read id, do not truncate yet
            itype = 1
            call rdtok1 (lunut, ilun, lch, lstack, cchar, &
                    iposr, npos, bndid_long(i), int_output, real_output, &
                    itype, ierr2)
            if (ierr2 > 0) goto 170


            ! read also name and type
            itype = 1
            call rdtok1 (lunut, ilun, lch, lstack, cchar, &
                    iposr, npos, bndname(i), int_output, real_output, &
                    itype, ierr2)
            if (ierr2 > 0) goto 170
            itype = 1
            call rdtok1 (lunut, ilun, lch, lstack, cchar, &
                    iposr, npos, bndtype_long(i), int_output, real_output, &
                    itype, ierr2)
            if (ierr2 > 0) goto 170

            IF (BNDID_LONG(I)  == ' ') WRITE (BNDID_LONG(I), '(''Boundary-ID'',I7)') I
            IF (BNDNAME(I)     == ' ') WRITE (BNDNAME(I), '(''Boundary name '',I7)') I
            IF (BNDTYPE_LONG(I)== ' ') BNDTYPE_LONG(I) = 'Boundary type 1'

            bndid(i) = bndid_long(i)
            bndtype(i) = bndtype_long(i)

            if (output_verbose_level >= 3) then
                if (iwidth == 5) then
                    write (lunut, 2030) bndid(i), bndname(i), bndtype(i)
                else
                    write (lunut, 2040) i, bndid(i), bndname(i), bndtype(i)
                endif
            endif

            ! check for unique ID, error if non-truncated ID is unique otherwise warning, can be skipped if input is generated
            call retrieve_command_argument ('-no_id_check', 0, no_id_check, idummy, rdummy, cdummy, ierr2)
            if (.not. no_id_check) then
                ifound = index_in_array(bndid(i), bndid(:i - 1))
                if (ifound >= 0) then
                    ifound2 = index_in_array(bndid_long(i), bndid_long(:i - 1))
                    if (ifound == ifound2) then
                        write(lunut, 2270) bndid(i)
                        call status%increase_warning_count()
                    else
                        write(lunut, 2280) bndid(i)
                        call status%increase_error_count()
                    endif
                endif
            endif

            ! check if truncated type and non truncated type give the same number
            itype = index_in_array(bndtype(i), bndtype(:nobtyp))
            ityp2 = index_in_array(bndtype_long(i), bndtype_long(:nobtyp))
            if (itype /= ityp2) then
                write(lunut, 2290) trim(bndtype_long(i))
                call status%increase_error_count()
            endif

            ! if type found set type, otherwise add type
            if (itype > 0) then
                ibndtype(i) = itype
            else
                nobtyp = nobtyp + 1
                bndtype(nobtyp) = bndtype(i)
                bndtype_long(nobtyp) = bndtype_long(i)
                ibndtype(i) = nobtyp
            endif

            ! write id and name to system file
            write (binary_work_file)  bndid(i), bndname(i)

        end do

        write (lunut, *)
        write (lunut, 2060) nobtyp
        if (output_verbose_level < 2) then
            write (lunut, 2065)
        else
            write (lunut, 2066)
            do i = 1, nobtyp
                write (lunut, 2070) i, bndtype(i)
            end do
            write (lunut, *)
        endif
        write (binary_work_file)  (bndtype(i), i = 1, nobtyp)
        write (binary_work_file)  (ibndtype(i), i = 1, nobnd)
        deallocate(bndname, bndid_long, bndtype_long, ibndtype)

        ! dummy time lags
        if (nosys == 0) then
            write (binary_work_file) (0, i = 1, nobnd)
            write (lunut, 2090)
            goto 170
        endif
        !
        ! read time lags
        ! note:
        ! we may encounter strings instead, in that case skip
        ! until we find an integer
        itype = 2
        call rdtok1 (lunut, ilun, lch, lstack, cchar, &
                iposr, npos, cdummy, iaropt, real_output, &
                itype, ierr2)
        if (ierr2 > 0) then
            write (lunut, 2101)
            itype = 1
            do
                call rdtok1 (lunut, ilun, lch, lstack, cchar, &
                        iposr, npos, cdummy, iaropt, real_output, &
                        itype, ierr2)
                read(cdummy, *, iostat = ierr2) iaropt
                if (ierr2 == 0) then
                    exit
                endif
            enddo
        endif

        write (lunut, 2100) iaropt
        goto (60, 70, 110) iaropt + 1
        write (lunut, 2110)
        call status%increase_error_count()
        goto 160

        ! no time lags
        60 write (lunut, 2120)
        write (binary_work_file) (0, i = 1, nobnd)
        goto 160

        ! time lags constant without defaults
        70 write (lunut, 2130)
        if (max_int_size < nobnd) then
            write (lunut, 2140) nobnd, max_int_size, nobnd - max_int_size
            do k = 1, nobnd
                itype = 2
                call rdtok1 (lunut, ilun, lch, lstack, cchar, &
                        iposr, npos, cdummy, idummy, real_output, &
                        itype, ierr2)
                if (ierr2 > 0) goto 170
            end do
            call status%increase_error_count()
            goto 160
        endif
        do k = 1, nobnd
            itype = 2
            call rdtok1 (lunut, ilun, lch, lstack, cchar, &
                    iposr, npos, cdummy, int_array(k), real_output, &
                    itype, ierr2)
            if (ierr2 > 0) goto 170
        end do
        if (output_verbose_level < 3) then
            write (lunut, 2145)
        else
            write (lunut, 2150)
        endif
        if (is_date_format) then
            call convert_time_format (int_array, nobnd, ifact, is_date_format, is_yyddhh_format)
            if (output_verbose_level >= 3) write (lunut, 2160) &
                    (int_array(k) / 31536000, mod(int_array(k), 31536000) / 86400, &
                    mod(int_array(k), 86400) / 3600, mod(int_array(k), 3600) / 60, &
                    mod(int_array(k), 60), k = 1, nobnd)
        else
            if (output_verbose_level >= 3) write (lunut, 2170) &
                    (int_array(k), k = 1, nobnd)
        endif
        do i = 1, nobnd
            if (int_array(i) < 0) then
                write (lunut, 2180) int_array(i)
                call status%increase_error_count()
            endif
        end do
        write (binary_work_file) (int_array(k), k = 1, nobnd)
        goto 160

        ! time lags constant with defaults
        110 write (lunut, 2190)
        itype = 2
        call rdtok1 (lunut, ilun, lch, lstack, cchar, &
                iposr, npos, cdummy, idef, real_output, &
                itype, ierr2)
        if (ierr2 > 0) goto 170
        if (idef < 0) then
            write (lunut, 2180) idef
            call status%increase_error_count()
        endif
        ! fill the array with the default
        do i = 1, min(max_int_size, nobnd)
            int_array(i) = idef
        end do
        ! nr of overridings
        itype = 2
        call rdtok1 (lunut, ilun, lch, lstack, cchar, &
                iposr, npos, cdummy, nover, real_output, &
                itype, ierr2)
        if (ierr2 > 0) goto 170
        if (is_date_format) then
            call convert_relative_time (idef, ifact, is_date_format, is_yyddhh_format)
            write (lunut, 2210) &
                    idef / 31536000, mod(idef, 31536000) / 86400, &
                    mod(idef, 86400) / 3600, mod(idef, 3600) / 60, &
                    mod(idef, 60), nover
        else
            write (lunut, 2220) idef, nover
        endif
        mxover = max_int_size - nobnd
        ! overridings
        do k = 1, min(nover, mxover)
            itype = 2
            call rdtok1 (lunut, ilun, lch, lstack, cchar, &
                    iposr, npos, cdummy, int_array(k + nobnd), real_output, &
                    itype, ierr2)
            if (ierr2 > 0) goto 170
            ibnd = max(1, min(iabs(int_array(k + nobnd)), nobnd))
            itype = 2
            call rdtok1 (lunut, ilun, lch, lstack, cchar, &
                    iposr, npos, cdummy, int_array(ibnd), real_output, &
                    itype, ierr2)
            if (ierr2 > 0) goto 170
        end do

        do k = 1, nover - mxover
            itype = 2
            call rdtok1 (lunut, ilun, lch, lstack, cchar, &
                    iposr, npos, cdummy, idummy, real_output, &
                    itype, ierr2)
            if (ierr2 > 0) goto 170
            itype = 2
            call rdtok1 (lunut, ilun, lch, lstack, cchar, &
                    iposr, npos, cdummy, idummy, real_output, &
                    itype, ierr2)
            if (ierr2 > 0) goto 170
        end do
        if (nover > mxover) then
            write (lunut, 2200) nobnd, nover, max_int_size, nobnd + nover - max_int_size
            call status%increase_error_count()
            goto 160
        endif
        if (is_date_format) &
                call convert_time_format (int_array, nobnd, ifact, is_date_format, is_yyddhh_format)
        if (nover > 0 .and. output_verbose_level >= 3) write (lunut, 2230)
        do i = 1, nover
            ibnd = iabs(int_array(i + nobnd))
            if (ibnd > nobnd .or. ibnd == 0) then
                write (lunut, 2180) int_array(i + nobnd)
                call status%increase_error_count()
            elseif (output_verbose_level >= 3) then
                it = int_array (ibnd)
                if (is_date_format) then
                    write (lunut, 2240) ibnd, &
                            it / 31536000, mod(it, 31536000) / 86400, &
                            mod(it, 86400) / 3600, mod(it, 3600) / 60, &
                            mod(it, 60)
                else
                    write (lunut, 2250)  ibnd, it
                endif
            endif
        end do
        write (binary_work_file) (int_array(k), k = 1, nobnd)

        ! read boundary concentrations
        ! this if block stands for the new input processing
        160 write (lunut, 2260)
        k = nobnd + 1
        l = nobnd + nobtyp + 1
        allocate(dp_array(max_real_size))             ! this array is 100 mb lp
        call read_boundary_concentrations (lun, lchar, 14, iwidth, max_char_size, &
                char_arr, max_int_size, int_array, max_real_size, real_array, &
                sname, bndid, bndtype(1:nobtyp), nobnd, nosys, &
                nobtyp, dp_array, is_date_format, is_yyddhh_format, &
                output_verbose_level, ierr2, status)
        deallocate(dp_array)
        deallocate(bndid, bndtype)

        IF (IERR2 ==  0) goto 180
        IF (IERR2 >  0) THEN
            call status%increase_error_count_with(ierr2)
            IERR2 = 0
            GOTO 170
        ENDIF

        IF (INTSRT == 6 .OR. INTSRT == 7) THEN
            NOSUBS = NOTOT
        ELSE
            NOSUBS = NOSYS
        ENDIF
        ! IERRH = -1 signals read_constants_time_variables that it is boundaries to deal with
        IERRH = -1
        call read_constants_time_variables   (lun, 14, 0, 0, nobnd, &
                nosubs, nosubs, nrftot(8), nrharm(8), ifact, &
                is_date_format, disper, volume, iwidth, lchar, &
                filtype, is_yyddhh_format, output_verbose_level, ierrh, &
                status, .false.)
        call status%increase_error_count_with(ierrh)

        IERR2 = 0
        170 CONTINUE
        IF (ALLOCATED(BNDID)) DEALLOCATE(BNDID)
        IF (ALLOCATED(BNDTYPE)) DEALLOCATE(BNDTYPE)
        IF (IERR2 > 0) call status%increase_error_count()
        IF (IERR2 == 3) CALL SRSTOP(1)
        175 call check_error(charachter_output, iwidth, 5, ierr2, status)
        180 if (timon) call timstop(ithndl)
        RETURN

        ! Output formats
        2000 format (//, ' No boundary conditions')
        2001 format (//, ' ERROR: Without boundary conditions only optional specification of zero time lags allowed!')
        2005 format (/, ' Names of open boundaries are printed for', &
                ' output option 3 and higher !')
        2010 format (/, ' Boundary-IDs:       boundary-names:    ', 20X, &
                ' boundary-types:', /)
        2020 format (/, ' Bound nr:      boundary-IDs:       boundary-names:', &
                '                         boundary-types:', /)
        2030 format (A20, A40, A20)
        2040 format (I7, 9X, A20, A40, A20)
        2060 format (' Number of different boundary types: ', I4)
        2065 format (' Boundary types printed for output option is 2', &
                ' or higher !')
        2066 format (' Type:  Type-string')
        2070 format (I6, 2X, A20)
        2090 format (//, ' No active systems; no boundary conditions')
        2100 format (/, ' Time lag option:', I3)
        2101 format (/, ' Note: Skipping superfluous boundary names')
        2110 format (' ERROR: Option not implemented')
        2120 format (' No time lags')
        2130 format (' Constant time lags without defaults')
        2140 format (/, ' ERROR the number of boundaries (', I7, ') exceeds', &
                ' the maximum (', I7, ').', &
                /' The maximum is limited by INTEGER array space', &
                /' Consult your system manager to obtain ', I7, ' words', &
                ' of additional storage.')
        2145 format (' Values will be printed for output option 3', &
                ' and higher !')
        2150 format (' Values :')
        2160 format (4(3X, I2, 'Y-', I3, 'D-', I2, 'H-', I2, 'M-', I2, 'S '))
        2170 format (3X, 10I7)
        2180 format (' ERROR, invalid time lag:', I10)
        2190 format (' Constant time lags with defaults')
        2200 format (/, ' ERROR the number of boundaries (', I7, ') plus the', &
                /' number of overridings (', I7, ')  exceeds', &
                ' the maximum (', I7, ').', &
                /' The maximum is limited by INTEGER array space', &
                /' Consult your system manager to obtain ', I7, ' words', &
                ' of additional storage.')
        2210 format (' Default value     :', &
                I2, 'Y-', I3, 'D-', I2, 'H-', I2, 'M-', I2, 'S ' &
                /, ' Number of overridings :', I4)
        2220 format (' Default value     :', I7, &
                /, ' Number of overridings :', I4)
        2230 format (' Number       values', /)
        2240 format (I6, 7X, I2, 'Y-', I3, 'D-', I2, 'H-', I2, 'M-', I2, 'S ')
        2250 format (I6, 7X, I7)
        2260 format (/, ' Boundary concentrations')
        2270 format (' WARNING: boundary ID is not unique:', A)
        2280 format (' ERROR: truncated boundary ID is not unique:', A)
        2290 format (' ERROR: truncated boundary type is not unique:', A)
        2300 format (' ERROR: allocating boundary arrays:', I7)
        2310 format (' number of boundaries             :', I7)
    end subroutine read_block_5_boundary_conditions

end module inputs_block_5
