!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module boundary_conditions
    use m_waq_precision
    use timers
    use m_string_utils
    use waq_timers, only : read_time_delay
    use matrix_utils, only : assign_matrix
    use boundary_condition_utils, only : read_time_series_table
    use m_error_status

    implicit none

    private
    public :: read_boundary_concentrations

contains

    subroutine read_boundary_concentrations(logical_unit, filenames, file_i, iwidth, max_char_size, &
            char_arr, max_int_size, int_workspace, max_real_size, real_workspace, &
            substances_names, bc_waste_ids, bc_waste_types, num_bc_waste, substances_count, &
            num_bc_waste_types, dp_workspace, is_date_format, is_yyddhh_format, output_verbose_level, ierr2, status)

        !> Boundary and waste data new style
        !>
        !> This routine reads blocks of input of the kind:
        !>    - ITEM
        !>    - bnd/wst item-IDs, nrs or type nrs
        !>    - CONCEN
        !>    - substance IDs or nrs (or FLOW or 0 for wastes)
        !>    - DATA
        !>    - the associated data
        !> Reading proceeds untill group end #5 or #6\n
        !> At run time the arrays are filled by wandering
        !> through the blocks and picking the values at the
        !> right time.\n
        !> At multiple definitions the last one counts.
        !> Writing starts with defaults that are all zero.
        !>    - many keywords apply
        !>    - ITEM and CONCEN sections may be interchanged
        !>    - the last section runs fastest in the matrix
        !>    - USEFOR with simple computational rules apply
        !>    - the data block may have column headers
        !>    - time indicator is absolute time string or integer
        !>    - ODS files are read here and data placed in the blocks
        !>    - BINARY files are resolved at run time

        ! LOGICAL UNITS:
        ! logical_unit(27) = unit stripped DELWAQ input file
        ! logical_unit(29) = unit formatted output file
        ! logical_unit( 2) = unit intermediate file (system)
        ! logical_unit(14) = unit intermediate file (boundaries)
        ! logical_unit(15) = unit intermediate file (wastes)

        use boundary_condition_utils, only : parse_boundary_condition_data, read_boundary_conditions_from_ods_file
        use error_handling, only : check_error
        use m_open_waq_files
        use rd_token
        use m_waq_data_structure
        use m_waq_memory_dimensions          ! System characteristics
        use m_timer_variables          ! Timer characteristics
        use m_string_utils, only : index_in_array

        integer(kind = int_wp), intent(inout) :: logical_unit(:)      !< array with unit numbers
        integer(kind = int_wp), intent(inout) :: int_workspace(max_int_size)  !< integer workspace

        character(*), intent(inout) :: filenames(:) !< filenames
        character(*), intent(inout) :: char_arr(:)   !< character workspace !max_char_size
        character(*), intent(inout) :: substances_names(:) !< substances names
        character(*), intent(inout) :: bc_waste_ids(:) !< ID's of the boundaries/wastes
        character(*), intent(in) :: bc_waste_types(:) !< Types of the boundaries/wastes

        integer(kind = int_wp), intent(in) :: file_i               !< index in logical_unit array of workfile
        integer(kind = int_wp), intent(in) :: iwidth           !< width of the output file
        integer(kind = int_wp), intent(in) :: max_char_size            !< maximum size of character workspace
        integer(kind = int_wp), intent(in) :: max_int_size            !< maximum size of integer workspace
        integer(kind = int_wp), intent(inout) :: num_bc_waste      !< number of bounds/wastes
        integer(kind = int_wp), intent(inout) :: substances_count !< number of substances
        integer(kind = int_wp), intent(in) :: num_bc_waste_types           !< number of boundary/waste types
        integer(kind = int_wp), intent(in) :: max_real_size            !< maximum size of real workspace
        integer(kind = int_wp), intent(in) :: output_verbose_level           !< how extensive will the output be
        integer(kind = int_wp), intent(out) :: ierr2            !< return code of this routine

        real(kind = real_wp), intent(inout) :: real_workspace(max_real_size) !< real workspace
        real(kind = dp), intent(inout) :: dp_workspace(*)     !< Double precision workspace

        logical, intent(in) :: is_date_format !< 'date'-format 1st time scale
        logical, intent(in) :: is_yyddhh_format !< 'date'-format (F;ddmmhhss,T;yydddhh)

        type(error_status), intent(inout) :: status !< current error status

        ! Local declarations
        character(len=10)     calit, caldit, strng1, strng2, strng3
        integer(kind = int_wp) :: iorder, count_items_in_use_rule, nodim, iflag, itype, &
                chkflg, ident, nottc, lunwr2, &
                file_size_1, file_size_2, ipro, itfacw, time_function_type, &
                num_records, itel, ioerr, iblock, k, &
                i, int_output, ioff, icm, iim, &
                noits, nconst, itmnr, idx_item_in_use_rule, nocol, &
                idmnr, nodis, nitm, nti, nti2, &
                ntr, irm, nottt, ierr3, nr2, &
                nts, ntc, ntd
        real(kind = real_wp) :: missing_value, real_output
        character(len=255)     character_output
        logical       newrec, scale, ods, binfil, tdelay
        logical :: time_dependent !< Is the BC / Waste load definition time dependent (true)? Or constant (false)?
        integer(kind = int_wp) :: ithndl = 0

        type(t_waq_data_items) :: dlwq_data_items
        type(t_waq_item) :: dlwq_foritem
        character(20) :: data_item_name
        integer(kind = int_wp) :: idata_item
        integer(kind = int_wp) :: ndata_items
        integer(kind = int_wp) :: iitem
        integer(kind = int_wp) :: count_unique_items_in_use_rule

        if (timon) call timstrt("read_boundary_concentrations", ithndl)

        ! Initialise a number of variables
        file_unit = logical_unit(29)
        lunwr2 = logical_unit(file_i)
        file_size_1 = 0
        file_size_2 = 0
        strng2 = 'Substance'
        ipro = 0
        itfacw = 1
        deltim = otime
        missing_value = -999.0
        ierr2 = dlwq_data_items%initialize()
        ierr2 = dlwq_foritem%initialize()
        !
        !          Initialise new data block
        !
        !     IORDER is the binary input flag,
        !                    0 = not set, 1 = items , 2 = concentr.
        !     IFLAG  is the ASCII input flag,
        !                    0 = not set, 1 = items , 2 = concentr.  3 = data
        !                    4 = scales
        !     num_records  is the number of breakpoints
        !     SCALE  is the Scale values flag .TRUE. is present
        !     USEFOR is the Alias flag, .TRUE. is alias string expected
        !     ODS    is the ODS   flag, .TRUE. ODS datafile expected
        !     NEWREC is the flag for new records
        !     time_function_type   is option flag 1 = block function, 2 = linear
        !                           3 = harmonics     , 4 = fourier
        !     IOFF   is offset in the array of integers and strings
        !
        if (output_verbose_level < 3) write (file_unit, 1340)
        if (output_verbose_level < 4) write (file_unit, 1350)
        iorder = 0
        iflag = 0
        time_function_type = 1
        num_records = 0
        itel = 0
        scale = .false.
        ods = .false.
        binfil = .false.
        newrec = .false.
        time_dependent = .false.

        ! Open the binary work file and provide a zero overall default
        call open_waq_files (logical_unit(file_i), filenames(file_i), file_i, 1, ioerr)
        if (file_i == 14) then
            write (lunwr2) ' 4.900BOUND '
            calit = 'BOUNDARIES'
            caldit = 'BDATA_ITEM'
            strng1 = 'boundary'
            iblock = 5
        else
            write (lunwr2) ' 4.900WASTE '
            calit = 'WASTELOADS'
            caldit = 'WDATA_ITEM'
            strng1 = 'wasteload'
            iblock = 6
        endif
        write (lunwr2) num_bc_waste, substances_count
        write (lunwr2) 1, 0, substances_count, (k, k = 1, substances_count), 1, 0
        write (lunwr2) 1
        write (lunwr2) 0, (0.0, i = 1, substances_count)
        file_size_1 = file_size_1 + 2 + 3 + substances_count + 3 + 1
        file_size_2 = file_size_2 + substances_count

        ! Get a token string (and return if something else was found)
        10 if (iflag == 0) itype = 0
        if (iflag == 1 .or. iflag == 2) itype = -3
        if (iflag == 3) then
            if (newrec) then
                itype = -3
            else
                itype = 3
            endif
        endif
        if (iflag == 4) itype = 3
        20 call rdtok1(file_unit, ilun, lch, lstack, cchar, iposr, npos, character_output, int_output, real_output, itype, ierr2)

        ! End of block detected
        if (ierr2 == 2) then
            if(itype > 0) then
                call status%increase_error_count()
            end if
            goto 530
        endif
        if (ierr2 /= 0) goto 510 !close ( lunwr2 )

        ! All the following has the old file structure
        if (abs(itype) == 1 .and. character_output == 'OLD-FILE-STRUCTURE') then
            write (file_unit, 1000)
            call status%increase_warning_count()
            ierr2 = -1
            goto 540
        endif

        ! A local redirection of the name of an item or substance is not valid here
        if (abs(itype) == 1 .and. character_output == 'USEFOR') then
            write (file_unit, 1010)
            ierr2 = 1
            goto 510
        endif

        ! Time delay for ODS files
        30 if (abs(itype) == 1 .and. character_output(1:10) == 'TIME_DELAY') then
            call read_time_delay (ierr2)
            if (ierr2 /= 0) goto 510
            goto 10
        endif

        ! Time interpolation instead of block function
        if (abs(itype) == 1 .and. character_output(1:6) == 'LINEAR') then
            if (output_verbose_level >= 3) write (file_unit, 1005)
            time_function_type = 2
            goto 10
        endif
        if (abs(itype) == 1 .and. character_output(1:5) == 'BLOCK') then
            time_function_type = 1
            goto 10
        endif

        ! Usedata_item
        if (abs(itype) == 1 .and. (character_output == 'USEDATA_ITEM')) then
            if (iorder == 1 .or. iorder == 2) then
                write (file_unit, 1011)
                ierr2 = 1
                goto 510
            endif

            ! Next token must be a name
            call rdtok1 (file_unit, ilun, lch, lstack, cchar, &
                    iposr, npos, character_output, int_output, real_output, &
                    itype, ierr2)
            if (character_output == ' ') then
                write (file_unit, 1012)
                ierr2 = 1
                goto 510
            endif
            data_item_name = character_output
            if (output_verbose_level >= 3) write (file_unit, 1015) data_item_name
            ! Next token must be 'FORITEM'
            call rdtok1 (file_unit, ilun, lch, lstack, cchar, &
                    iposr, npos, character_output, int_output, real_output, &
                    itype, ierr2)

            if (character_output /= 'FORITEM') then
                write (file_unit, 1012)
                ierr2 = 1
                goto 510
            endif
            ! Now get the list of locations to apply the DATA_ITEM
            ioff = 1
            chkflg = 1
            icm = max_char_size - ioff
            iim = max_int_size - ioff
            call parse_boundary_condition_data (file_unit, iposr, npos, cchar, char_arr(ioff:), &
                    int_workspace(ioff:), icm, iim, bc_waste_ids, bc_waste_types, &
                    num_bc_waste, num_bc_waste_types, count_items_in_use_rule, noits, chkflg, &
                    calit, ilun, lch, lstack, &
                    itype, real_workspace, nconst, itmnr, character_output, &
                    output_verbose_level, ierr2, status)
            ! Check if data_item already exists

            if (dlwq_data_items%current_size > 0) then
                idata_item = index_in_array(data_item_name, dlwq_data_items%name(1:dlwq_data_items%current_size))
            else
                idata_item = 0
            end if
            if (idata_item<=0) then ! first data_item (0) or not found (<0)
                ndata_items = dlwq_data_items%add(data_item_name, dlwq_foritem)
                idata_item = ndata_items
            endif
            if(dlwq_data_items%used(idata_item)) then
                write(file_unit, 1016)
                call status%increase_warning_count()
            end if
            do idx_item_in_use_rule = 1, count_items_in_use_rule !count_items_in_use_rule
                !          Already on the list?
                count_unique_items_in_use_rule = dlwq_data_items%for_item(idata_item)%no_item
                if (count_unique_items_in_use_rule>0) then
                    iitem = index_in_array(char_arr(ioff + idx_item_in_use_rule - 1), &
                            dlwq_data_items%for_item(idata_item)%name(1:count_unique_items_in_use_rule))
                else
                    iitem = -1
                end if

                !! if the item has not been added to dlwq_data_items and it shouldn't be ignored
                if (iitem<=0 .and. int_workspace(ioff + idx_item_in_use_rule - 1)/=-1300000000) then
                    count_unique_items_in_use_rule = count_unique_items_in_use_rule + 1
                    ierr2 = dlwq_data_items%for_item(idata_item)%resize(count_unique_items_in_use_rule)
                    dlwq_data_items%for_item(idata_item)%name(count_unique_items_in_use_rule) = &
                            char_arr(ioff + idx_item_in_use_rule - 1)
                    dlwq_data_items%for_item(idata_item)%ipnt(count_unique_items_in_use_rule) = &
                            int_workspace(ioff + idx_item_in_use_rule - 1)
                    dlwq_data_items%for_item(idata_item)%sequence(count_unique_items_in_use_rule) = &
                            count_unique_items_in_use_rule
                    dlwq_data_items%for_item(idata_item)%no_item = count_unique_items_in_use_rule
                end if
            end do
            if (ierr2 /= 0) goto 510
            goto 30
        endif

        ! Items
        if (abs(itype) == 1 .and. (character_output == 'ITEM' .or. character_output == 'IDENTICALITEM' .or. &
                character_output == 'DATA_ITEM')) then
            if (iorder == 0) then !Items
                iorder = 1 !concentr.
                ioff = 1
                if (character_output == 'IDENTICALITEM') then
                    if (output_verbose_level >= 3) write (file_unit, 1021)
                    ident = 1
                else if (character_output == 'DATA_ITEM') then
                    if (output_verbose_level >= 3) write (file_unit, 1022)
                    ident = 2 ! DATA_ITEM
                else
                    if (output_verbose_level >= 3) write (file_unit, 1020)
                    ident = 0
                endif
            elseif (iorder == 1) then
                write (file_unit, 1030)
                ierr2 = 1
                goto 510
            else
                if (output_verbose_level >= 3) write (file_unit, 1040)
                ioff = nodim + idmnr + 1
                ident = 0
            endif
            chkflg = 1
            icm = max_char_size - ioff
            iim = max_int_size - ioff
            if (ident <= 1) then ! ITEM, IDENTICALITEM
                call parse_boundary_condition_data (file_unit, iposr, npos, cchar, char_arr(ioff:), &
                        int_workspace(ioff:), icm, iim, bc_waste_ids, bc_waste_types, &
                        num_bc_waste, num_bc_waste_types, count_items_in_use_rule, noits, chkflg, &
                        calit, ilun, lch, lstack, itype, &
                        real_workspace, nconst, itmnr, character_output, output_verbose_level, &
                        ierr2, status)
            else ! DATA_ITEM
                call parse_boundary_condition_data (file_unit, iposr, npos, cchar, char_arr(ioff:), &
                        int_workspace(ioff:), icm, iim, dlwq_data_items%name(1:ndata_items), &
                        dlwq_data_items%name(1:ndata_items), &
                        ndata_items, ndata_items, count_items_in_use_rule, noits, chkflg, &
                        caldit, ilun, lch, lstack, itype, &
                        real_workspace, nconst, itmnr, character_output, output_verbose_level, &
                        ierr2, status)
                if (count_items_in_use_rule/=1) then
                    write (file_unit, 1045)
                    ierr2 = 1
                    goto 510
                end if
                idata_item = int_workspace(ioff)
                if (idata_item > 0) then
                    ! Replace result of parse_boundary_condition_data with usedata_item list
                    dlwq_data_items%used(idata_item) = .true.
                    !! for how many items is applicable this usedata_item?
                    count_items_in_use_rule = dlwq_data_items%for_item(idata_item)%no_item
                    if (count_items_in_use_rule /= 0) then
                        itmnr = count_items_in_use_rule
                        noits = count_items_in_use_rule
                        do idx_item_in_use_rule = 1, count_items_in_use_rule
                            char_arr(ioff + idx_item_in_use_rule - 1) = &
                                    dlwq_data_items%for_item(idata_item)%name(idx_item_in_use_rule)
                            char_arr(ioff + idx_item_in_use_rule - 1 + count_items_in_use_rule) = &
                                    dlwq_data_items%for_item(idata_item)%name(idx_item_in_use_rule)
                            int_workspace(ioff + idx_item_in_use_rule - 1) = &
                                    dlwq_data_items%for_item(idata_item)%ipnt(idx_item_in_use_rule)
                            int_workspace(ioff + idx_item_in_use_rule - 1 + count_items_in_use_rule) = &
                                    dlwq_data_items%for_item(idata_item)%sequence(idx_item_in_use_rule)

                            ! I think the log statements below are wrong, not the type but the DATA_ITEM should be
                            ! printed as 1st argument
                            if (int_workspace(ioff + idx_item_in_use_rule - 1) > 0) then
                                write (file_unit, 1023) char_arr(ioff), idata_item, calit, &
                                        int_workspace(ioff + idx_item_in_use_rule - 1), &
                                        dlwq_data_items%for_item(idata_item)%name(idx_item_in_use_rule)
                            else
                                write (file_unit, 1024) char_arr(ioff), idata_item, calit, &
                                        int_workspace(ioff + idx_item_in_use_rule - 1), &
                                        dlwq_data_items%for_item(idata_item)%name(idx_item_in_use_rule)
                            end if
                        end do
                    else
                        write (file_unit, 1046)
                        !data ignored
                        count_items_in_use_rule = 1
                        itmnr = 1
                        noits = 1
                        int_workspace(ioff) = -1300000000
                        int_workspace(ioff + 1) = 1
                    end if
                else
                    write (file_unit, 1047)
                    !data ignored
                    count_items_in_use_rule = 1
                    itmnr = 1
                    noits = 1
                    int_workspace(ioff) = -1300000000
                    int_workspace(ioff + 1) = 1
                end if
            endif

            nocol = noits
            if (ierr2 /= 0) goto 510
            goto 30
        endif

        ! Concentrations
        if (abs(itype) == 1 .and. character_output(1:6) == 'CONCEN') then
            if (iorder == 0) then
                if (output_verbose_level >= 3) write (file_unit, 1050)
                iorder = 2
                ioff = 1
            elseif (iorder == 1) then
                if (output_verbose_level >= 3) write (file_unit, 1060)
                ioff = count_items_in_use_rule + itmnr + 1
            else
                write (file_unit, 1070)
                ierr2 = 1
                goto 510
            endif
            chkflg = 1
            icm = max_char_size - ioff
            iim = max_int_size - ioff
            call parse_boundary_condition_data (file_unit, iposr, npos, cchar, char_arr(ioff:), &
                    int_workspace(ioff:), icm, iim, substances_names, bc_waste_types, &
                    substances_count, 0, nodim, nodis, chkflg, &
                    'CONCENTR. ', ilun, lch, lstack, &
                    itype, real_workspace, nconst, idmnr, character_output, &
                    output_verbose_level, ierr2, status)
            nocol = nodis
            if (ierr2 /= 0) goto 510
            goto 30
        endif

        ! Data
        if (abs(itype) == 1 .and. character_output(1:6) == 'DATA') then
            if (count_items_in_use_rule * nodim == 0) then ! nodim = count_subs
                write (file_unit, 1080) count_items_in_use_rule, nodim
                ierr2 = 1
                goto 510
            endif
            ! Checks if an inner loop column header exists for the data matrix
            call validate_column_headers(file_unit, int_workspace, itmnr, count_items_in_use_rule, idmnr, &
                    nodim, iorder, char_arr, iposr, &   ! max_int_size,
                    npos, ilun, lch, lstack, cchar, &
                    character_output, nocol, is_date_format, is_yyddhh_format, itfacw, &
                    itype, int_output, real_output, ierr2, status)
            if (ierr2 > 1) goto 510
            ! Reads blocks of data
            if (iorder == 2) then
                nitm = count_items_in_use_rule
            else
                nitm = nodim
            endif
            nti = count_items_in_use_rule + nodim + itmnr + idmnr + 1
            nti2 = nti + nitm
            iim = max_int_size - nti2
            ntr = nconst + itel + 1
            iim = max_int_size - nti2
            irm = max_real_size - ntr
            if (iorder == 2) then !concentrations first
                nottt = nodim * nocol
                nottc = nottt
            else ! not set (iorder==0) or items first (iorder==1)
                nottt = count_items_in_use_rule * nocol
                if (ident>=1) then  ! IDENTICALITEM (ident==1) or DATA_ITEM (ident==2)
                    nottc = nocol
                else ! ITEM (ident==0)
                    nottc = nottt
                endif
            endif

            ! harmonics or fourier
            if (time_function_type == 3 .or. time_function_type == 4) nottt = nottt + 1
            ! read time series table
            call read_time_series_table (file_unit, int_workspace(nti2:), real_workspace(ntr:), iim, irm, &
                    iposr, npos, ilun, lch, lstack, &
                    cchar, character_output, nottt, nottc, time_dependent, num_records, &
                    time_function_type, is_date_format, is_yyddhh_format, itfacw, itype, &
                    int_output, real_output, ierr2, ierr3)

            call status%increase_error_count_with(ierr3)

            if (ierr2 == 1 .or. ierr2 == 4) goto 510
            if (num_records == 0 .and. (.not. time_dependent)) then
                write(file_unit, 1360)
                call status%increase_error_count()
            endif
            if (nodim == 0) then
                write(file_unit, 1370)
                call status%increase_warning_count()
            else
                ! Assigns according to computational rules
                nr2 = ntr + nottt * num_records
                ! process parsed values in table  (process operations if any) and store results in real_workspace(nr2:)
                call assign_matrix (file_unit, int_workspace, count_items_in_use_rule, itmnr, nodim, &
                        idmnr, iorder, real_workspace, time_function_type, real_workspace(ntr:), &
                        nocol, num_records, missing_value, int_workspace(nti:), real_workspace(nr2:))
                strng3 = 'breakpoint'
                ! Writes to the binary intermediate file
                nts = nconst + 1
                ntc = nti
                icm = max_char_size - ntc
                call write_data_blocks (lunwr2, file_unit, iwidth, num_records, int_workspace, & ! writes data_item to wrk and/or lst files
                        real_workspace(nts:), real_workspace(nr2:), itmnr, idmnr, iorder, &
                        scale, .true., binfil, time_function_type, ipro, &
                        itfacw, is_date_format, is_yyddhh_format, file_size_1, file_size_2, &
                        substances_names, strng1, strng2, strng3, output_verbose_level)
            endif
            if (ierr2 == 2) goto 530
            if (ierr2 == 3) goto 510
            iorder = 0
            iflag = 0
            time_function_type = 1
            missing_value = -999.0
            num_records = 0
            itel = 0
            scale = .false.
            ods = .false.
            binfil = .false.
            newrec = .false.
            time_dependent = .false.
            if (itype == 1) goto  30
            goto 10
        endif

        ! ODS-file option selected
        if (abs(itype) == 1 .and. character_output(1:8) == 'ODS_FILE') then
            if (count_items_in_use_rule * nodim == 0) then
                write (file_unit, 1080) count_items_in_use_rule, nodim
                ierr2 = 1
                goto 510
            endif
            ods = .true.
            itype = 1
            goto 20
        endif

        ! ODS-file-data retrieval
        if (abs(itype) == 1 .and. ods) then
            nti = count_items_in_use_rule + nodim + itmnr + idmnr + 1
            ntr = itel + nconst + 1
            ntd = (ntr + 1) / 2 + 1
            nts = nconst + 1
            iim = max_int_size - nti
            irm = max_real_size - ntr
            call read_boundary_conditions_from_ods_file(character_output, file_unit, char_arr, int_workspace, real_workspace(ntr:), &
                    max_char_size, max_int_size, max_real_size, dp_workspace, count_items_in_use_rule, &
                    nodim, iorder, scale, itmnr, idmnr, &
                    missing_value, num_records, ierr2, status)
            if (ierr2 /= 0) goto 510
            nr2 = ntr + count_items_in_use_rule * nodim * num_records
            call assign_matrix (file_unit, int_workspace, count_items_in_use_rule, itmnr, nodim, &
                    idmnr, iorder, real_workspace, time_function_type, real_workspace(ntr:), &
                    nodim, num_records, missing_value, int_workspace(nti:), real_workspace(nr2:))
            strng3 = 'breakpoint'
            call write_data_blocks (lunwr2, file_unit, iwidth, num_records, int_workspace, &
                    real_workspace(nts:), real_workspace(nr2:), itmnr, idmnr, iorder, &
                    scale, ods, binfil, time_function_type, ipro, &
                    itfacw, is_date_format, is_yyddhh_format, file_size_1, file_size_2, &
                    substances_names, strng1, strng2, strng3, output_verbose_level)
            if (ierr2 == 2) then
                ierr2 = -2
                goto 530
            endif
            if (ierr2 == 3) goto 510
            iorder = 0
            iflag = 0
            ioff = 0
            time_function_type = 1
            missing_value = -999.0
            num_records = 0
            itel = 0
            scale = .false.
            ods = .false.
            binfil = .false.
            newrec = .false.
            time_dependent = .false.
            goto  10
        endif

        ! Absolute or relative timers
        if (abs(itype) == 1 .and. character_output(1:8) == 'ABSOLUTE') then
            write (file_unit, 1135)
            character_output = 'TIME'
        endif

        ! Say it is a time function
        if (abs(itype) == 1 .and. character_output(1:4) == 'TIME') then
            time_dependent = .true.
            time_function_type = 1
            goto 10
        endif

        ! Scale factors begin
        if (abs(itype) == 1 .and. character_output == 'SCALE') then
            if (nodim == 0) then
                write (file_unit, 1180)
                ierr2 = 1
                goto 510
            endif
            if (itel /= 0) then
                write (file_unit, 1190)
                ierr2 = 1
                goto 510
            endif
            iflag = 4
            scale = .true.
            goto 10
        endif

        ! Getting the scale factors
        if (iflag == 4) then
            itel = itel + 1
            real_workspace(itel + nconst) = real_output
            if (itel == idmnr) iflag = 0
            goto 10
        endif

        write (file_unit, 1320) character_output
        write (file_unit, '(A)') ' Expected character string should be a valid level 2 keyword'
        ierr2 = 1
        510 close (lunwr2)
        do i = 2, lstack
            if (lch(i) /= ' ') then
                close (ilun(i))
                lch (i) = ' '
                ilun(i) = 0
            endif
        end do
        530 newrsp = newrsp + file_size_2
        newisp = newisp + file_size_1
        call check_error(character_output, iwidth, iblock, ierr2, status)
        540 if (timon) call timstop(ithndl)
        return

        1000 format (/' WARNING: Old file structure is used for this data !')
        1005 format (/' Linear interpolation is selected !')
        1010 format (/' ERROR: USEFOR is not alowed here. Specify ITEM or CONCENTRATION first !')
        1011 format (/' ERROR: USEDATA_ITEM is not alowed here. Specify before ITEM or CONCENTRATION !')
        1012 format (/' ERROR: Expected a DATA_ITEM name')
        1015 format (/' When the DATA_ITEM ', A, ' is met, data is applied to the following items:')
        1016 format (/' WARNING: DATA_ITEM has already been used, and is now redefined!')
        1020 format (/' BLOCKED per ITEM:')
        1021 format (/' IDENTICAL DATA for all items ITEMS:')
        1022 format (/' SINGLE BLOCK for DATA_ITEM:')
        1023 FORMAT (' Input DATA_ITEM ', A, ' nr:', I5, ' will be used for ', A, ' nr:', I5, ' with ID  : ', A20)
        1024 FORMAT (' Input DATA_ITEM ', A, ' nr:', I5, ' will be used for ', A, ' nr:', I5, ' with type: ', A20)
        1030 format (/' ERROR: Second time ITEMs and/or DATA_ITEMs keyword in this block !')
        1040 format (/' ITEMs within the concentration blocks:')
        1045 format (' ERROR: Only one named DATA_ITEM allowed!')
        1046 format (' WARNING: The DATA_ITEM does not reference to any valid ITEM! Data will be ignored.')
        1047 format (' WARNING: No USEDATA_ITEM was found for DATA_ITEM! Data will be ignored.')
        1050 format (/' BLOCKED per CONCENTRATION:')
        1060 format (/' CONCENTRATIONs within the item blocks:')
        1070 format (/' ERROR: Second time CONCENs keyword in this block !')
        1080 format (/' ERROR: Nr of ITEMS (', I5, ') or nr of concentrations (', I5, ') is zero for this DATA block !')
        1135 format (' Absolute times (YYYY/MM/DD;HH:MM:SS) expected in next', ' time function block.')
        1180 format (' ERROR: number of substances is zero !')
        1190 format (' ERROR: data has already been entered !')
        1320 format (' ERROR: token found on input file: ', A)
        1340 format (' Output on administration only writen for output option 3 and higher !')
        1350 format (' Output of the data only writen for output option 4 and higher !')
        1360 format (' ERROR: no (valid) DATA record available !')
        1370 format (' WARNING: all DATA from this block is ignored !')

    end subroutine read_boundary_concentrations
    !
    !     Additional documentation on the memory lay out
    !
    !     After ITEM, 5B reads Item names. It produces:
    !     a) in char_arr: a series of that item name and then a second series
    !        with the same name, or the substituted name after USEFOR
    !        If ' ' is specified, 'Item-nnn' will appear.
    !        If a positive integer is specified, the name of the
    !        corresponding item is given. For negative numbers the name
    !        of the corresponding type is provided
    !        FLOW is a valid item name if concentrations are expected for
    !        wasteloads.
    !        If a segment number is expected, 'Segment mmmmmmmm' is produced.
    !        ITMNR counts the first series, NOITM the second series.
    !     b) in int_workspace: a series of the number of the item from the items list
    !                the same series again
    !                a series with sequence numbers
    !        ITMNR counts the first series, NOITM the second and third series
    !        For types a negative number is used
    !        If FLOW is allowed, it has number 0
    !     If a number is used after a USEFOR, it is stored in real_workspace. In the
    !        char_arr location now the word "&$&$SYSTEM_NAME&$&$!" is placed.
    !        In the second series location in int_workspace, the negative value of the
    !        location in real_workspace is placed. The sequence number is decremented
    !     If a computational sign is (*,/,+,-,MIN,MAX) is encountered:
    !        The second series location in int_workspace contains a large negative
    !           number depending on the computational sign. Sequence number
    !           and item number are incremented
    !        If now the name of a previously used item is given, the first
    !           series location of int_workspace contains this item number, the second
    !           series location in char_arr contains "&$&$SYSTEM_NAME&$&$!" and
    !           the sequence number is decremented
    !        If a new name was provided, the 3rd series of int_workspace is set at
    !           the corresponding sequence number and the second series
    !           location in char_arr contains that name
    !     At first invocation, this all starts at the beginning of the arrays
    !     ITMNR contains the number of items processed. It is the dimension
    !        of the first series in int_workspace and char_arr
    !     NOITS contains the sequence number. That many things are waiting to
    !        be resolved and this number was used as dimension for SCALE
    !     NOITM contains the second and third series dimension in int_workspace and char_arr
    !     NCONST is the number of constants that was put in real_workspace
    !     ITYPE and character_output are the type and content of the token at exit
    !
    !     After CONCENTRATION the same happens. Depending on who is first,
    !        the arrays are first filled with Items (IORDER = 1) or with
    !        Concentrations (IORDER = 2).
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !

    subroutine write_data_blocks(binary_work_file, ascii_output_file_unit, output_file_width, num_blocks, &
            integer_array, real_array, values_arr, num_items, num_dims, iorder, &
            has_scale_factors, convert_breakpoint, info_from_binary_file, time_function_type, memory_type, &
            time_factor, is_date_format, is_yyddhh_format, cum_items, cum_dims, &
            flow_ignore_string, item_string, value_conc_string, breakpoint_harm_string, output_file_option)

        !! Prints and writes blocks of data

        use date_time_utils, only : convert_time_format

        logical :: has_scale_factors, info_from_binary_file, defaults_on
        logical, intent(in) :: convert_breakpoint   !! T = Breakpoints are converted
        logical, intent(in) :: is_date_format       !! 'date'-format 1st time scale
        logical, intent(in) :: is_yyddhh_format     !! 'date'-format (F;ddmmhhss,T;yydddhh)
        character(len = *), intent(in) :: item_string, value_conc_string, breakpoint_harm_string, flow_ignore_string(:)
        integer(kind = int_wp), intent(in) :: num_dims, num_items        !! number of subs, items to write
        integer(kind = int_wp), intent(in) :: iorder                 !! 1 = groups of subs per item
        integer(kind = int_wp), intent(in) :: ascii_output_file_unit !!Unit of ASCII output file(formatted output file)
        integer(kind = int_wp), intent(in) :: binary_work_file          !! Unit of binary work file
        integer(kind = int_wp), intent(inout) :: integer_array(:)
        integer(kind = int_wp), intent(in) :: time_function_type !!1 is block, 2 is linear, 3 is harmonics, 4 is fourier
        integer(kind = int_wp), intent(in) :: memory_type                           !! 0 is non permanent memory
        integer(kind = int_wp), intent(inout) :: cum_items, cum_dims    !! cumulative integer/real space count
        integer(kind = int_wp), intent(in) :: num_blocks                !! number of blocks to write
        integer(kind = int_wp) :: output_file_option
        integer(kind = int_wp) :: output_file_width         !! Width of the output file
        integer(kind = int_wp) :: time_factor               !! factor between clocks
        real(kind = real_wp), intent(inout) :: real_array(:), values_arr(:)

        integer(kind = int_wp) :: itel2, i1dum, i2dum, nodi2
        integer(kind = int_wp) :: k, ie, ie2, i1, i2
        integer(kind = int_wp) :: itels, itel, i3
        integer(kind = int_wp) :: ioffb, ioffi, ioffs, iskip, iskp2, num_substances_total, iss
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("write_data_blocks", ithndl)

        ! write headers
        defaults_on = .false.
        if (num_dims < 0) defaults_on = .true.
        nodi2 = num_dims
        if (num_dims <= 0) nodi2 = 1
        if (iorder == 1) then
            write (ascii_output_file_unit, 1000) num_items, nodi2, value_conc_string
            write (binary_work_file) iorder, &
                    num_items, (integer_array(k), k = 1, num_items), &
                    num_dims, (integer_array(k), k = num_items + 1, num_items + num_dims), &
                    time_function_type, memory_type
        elseif (iorder == 2) then
            write (ascii_output_file_unit, 1000) nodi2, num_items, item_string
            if (binary_work_file > 0) &
                    write (binary_work_file) iorder, &
                            num_dims, (integer_array(k), k = 1, num_dims), &
                            num_items, (integer_array(k), k = num_dims + 1, num_dims + num_items), &
                            time_function_type, memory_type
        endif

        cum_items = cum_items + num_items + max(0, num_dims) + 5

        ! just declare array space for binary files and return
        if (info_from_binary_file) then
            write (ascii_output_file_unit, 1130) memory_type
            cum_items = cum_items + 3
            cum_dims = cum_dims + max(1, num_dims) * max(1, num_items) * 3
            goto 70
        endif

        if (num_blocks == 0) then
            has_scale_factors = .false.
            goto 70
        endif
        ioffb = num_items + nodi2 + 1
        ioffi = 0
        ioffs = num_items
        iskip = 1
        iskp2 = nodi2
        num_substances_total = num_items * nodi2
        if (time_function_type == 3 .or. time_function_type == 4) num_substances_total = num_substances_total + 1
        if (iorder == 2) then
            ioffi = max(num_dims, 0)
            ioffs = 0
            iskip = num_items
            iskp2 = 1
        endif

        ! scale factors
        iss = 1
        if (has_scale_factors) then
            has_scale_factors = .false.
            iss = 1
            if (output_file_option >= 4) then
                write (ascii_output_file_unit, 1010)
                do i2 = 1, num_dims, output_file_width
                    ie = min(i2 + output_file_width - 1, num_dims)
                    write (ascii_output_file_unit, 1020) (integer_array(ioffs + k), k = i2, ie)
                    write (ascii_output_file_unit, 1025) (flow_or_ignore(flow_ignore_string, integer_array(ioffs + k)), k = i2, ie)
                    write (ascii_output_file_unit, 1030) (real_array(k), k = i2, ie)
                end do
            endif
            do i1 = 1, num_blocks
                do i2 = 0, num_substances_total - 1
                    if (iorder == 1) itel2 = mod(i2, num_dims) + 1
                    if (iorder == 2) itel2 = i2 / num_dims + 1
                    values_arr(iss + i2) = values_arr(iss + i2) * real_array(itel2)
                end do
                iss = iss + num_substances_total
            end do
        endif

        ! convert breakpoints
        if (num_blocks > 1) then
            if (output_file_option >= 4) write (ascii_output_file_unit, 1040) breakpoint_harm_string, num_blocks
            if (.not. convert_breakpoint) &
                    call convert_time_format(integer_array(ioffb:), num_blocks, time_factor, is_date_format, is_yyddhh_format)
            if (defaults_on .and. output_file_option >= 4) write (ascii_output_file_unit, 1050)
        else
            if (defaults_on) then
                if (output_file_option >= 4) write (ascii_output_file_unit, 1050)
            else
                if (output_file_option >= 4) write (ascii_output_file_unit, 1060)
            endif
        endif

        ! write binary file
        if (binary_work_file > 0) then
            i1dum = 0
            i2dum = 0
            ! write table in binary format to wrk file.
            call write_breakpoint_data_blocks(binary_work_file, num_blocks, num_substances_total, 1, integer_array(ioffb:), &
                    values_arr, i1dum, i2dum)
            cum_items = cum_items + i1dum
            cum_dims = cum_dims + i2dum
        endif

        ! write formatted output
        if (output_file_option >= 4) then
            itels = 0
            do i1 = 1, num_blocks
                if (num_blocks > 1) then
                    if (time_function_type == 1) &
                            write (ascii_output_file_unit, 1070) breakpoint_harm_string, i1, integer_array(ioffb + i1 - 1)
                    if (time_function_type == 2) &
                            write (ascii_output_file_unit, 1070) breakpoint_harm_string, i1, integer_array(ioffb + i1 - 1)
                    if (time_function_type == 3) then
                        itels = itels + 1
                        write (ascii_output_file_unit, 1080) i1, integer_array(ioffb + i1 - 1), values_arr(itels)
                    endif
                    if (time_function_type == 4) then
                        itels = itels + 1
                        write (ascii_output_file_unit, 1090) i1, integer_array(ioffb + i1 - 1), values_arr(itels)
                    endif
                endif
                do i2 = 1, nodi2, output_file_width
                    ie2 = min(i2 + output_file_width - 1, nodi2)
                    if (num_dims > 0) then
                        write (ascii_output_file_unit, 1100) value_conc_string, (integer_array(ioffs + k), k = i2, ie2)
                        write (ascii_output_file_unit, 1150) item_string, &
                                (flow_or_ignore(flow_ignore_string, integer_array(ioffs + k)), k = i2, ie2)
                    endif
                    itel = itels
                    do i3 = 1, num_items
                        write (ascii_output_file_unit, 1120)  abs(integer_array(ioffi + i3)), &
                                (values_arr(itel + k), k = (i2 - 1) * iskip + 1, (ie2 - 1) * iskip + 1, iskip)
                        itel = itel + iskp2
                    end do
                end do
                itels = itels + nodi2 * num_items
            end do
        endif

        70 write (ascii_output_file_unit, 1140)
        if (timon) call timstop(ithndl)
        return

        1000 format (/' DATA grouped in', I10, ' blocks of', I10, ' ', A)
        1010 format (' Scale factors for this block of data: ')
        1020 format (' Scale    :', I6, 9I12)
        1025 format (' Substance:', 10('  ', A10))
        1030 format (' values   :', 10E12.4)
        1040 format (/' Number of ', A, 's with full data:', I5)
        1050 format (' Default values in this block.')
        1060 format (' Constant values in this block.')
        1070 format (' ', A, ' ', I7, ' :', I10)
        1080 format (' Harmonic: ', I3, ' :', I10, ' Phase: ', 10E12.4)
        1090 format (' Fourier : ', I3, ' :', I10, ' Phase: ', 10E12.4)
        1100 format (' ', A, I20, 9I12)  ! ( ' ',A,I6,9I12)
        1150 format (' ', A, ' ', 10('  ', A10))
        1120 format (I10, 2X, 1P, 10E12.4)
        1130 format (' Info comes at runtime from binary file at unit: ', I3)
        1140 format(/' ====> input item completed <==== '//)

    end subroutine write_data_blocks

    character(len=20) function flow_or_ignore(string, i)
        integer(kind = int_wp) :: i
        character(len = *) :: string(*)
        if (i > 0) then
            flow_or_ignore = string(i)
        elseif (i == 0) then
            flow_or_ignore = 'FLOW'
        else
            flow_or_ignore = 'ignored'
        endif
    end function flow_or_ignore

    subroutine write_breakpoint_data_blocks(binary_work_file, num_items, array_size, num_integers, integer_array, &
            real_array, int_count, real_count)

        !! Writes blocks of breakpoint data

        integer(kind = int_wp), intent(in) :: num_integers                  !! nr of integers per breakpoint
        integer(kind = int_wp), intent(in) :: num_items                     !! nr of breakpoints to write
        integer(kind = int_wp), intent(in) :: array_size
        integer(kind = int_wp), intent(in) :: binary_work_file                 !! unit number output work file
        integer(kind = int_wp), intent(inout) :: int_count, real_count
        integer(kind = int_wp), intent(in) :: integer_array(:)                 !! breakpoint timers
        real(kind = real_wp), intent(in) :: real_array(:)       !! matrix storage

        integer(kind = int_wp) :: itel, jtel, k, i, ithndl = 0
        if (timon) call timstrt("write_breakpoint_data_blocks", ithndl)

        ! write nr of breakpoints first
        write (binary_work_file) num_items

        ! initialize counters for the loop
        itel = 0
        jtel = 0
        do i = 1, num_items
            write (binary_work_file) (integer_array(itel + k), k = 1, num_integers), &
                    (real_array(jtel + k), k = 1, array_size)
            itel = itel + num_integers
            jtel = jtel + array_size
        end do

        ! update the space count
        int_count = int_count + num_items * num_integers + 1
        real_count = real_count + num_items * array_size

        if (timon) call timstop(ithndl)

    end subroutine write_breakpoint_data_blocks

    subroutine validate_column_headers(ascii_output_file_unit, int_array, count_items_assign, count_items_comp_rule, &
            count_subs_assign, count_subs_comp_rule, index_first, names_to_check, start_in_line, & !max_int_size,
            num_significant_char, ilun, lch, lstack, cchar, &
            character_output, nocol, is_date_format, is_yyddhh_format, itfact, &
            itype, int_output, real_output, error_idx, status)

        !! Checks if column header exists
        !! Logical Units : file_unit_list(27) = unit stripped DELWAQ input file
        !!                 file_unit_list(29) = unit formatted output file

        use m_usefor, only : compact_usefor_list
        use date_time_utils, only : convert_string_to_time_offset, convert_relative_time
        use rd_token, only : rdtok1

        integer(kind = int_wp), intent(in) :: ascii_output_file_unit
        integer(kind = int_wp), intent(in) :: count_items_comp_rule         !! number of items in computational rule
        integer(kind = int_wp), intent(inout) :: count_subs_assign          !! number of subst for assignment
        ! integer(kind = int_wp) :: max_int_size             !! max. integer workspace dimension

        integer(kind = int_wp), intent(inout) :: int_array(:)      !! in/out  integer workspace array
        type(error_status), intent(inout) :: status !< current error status

        character(len = *), intent(in) :: names_to_check(:) !! names of items to check for presence
        character(len = *), intent(in) :: lch(lstack)       !! file name stack, 4 deep
        character(len = *), intent(out) :: character_output            !! space for limiting token
        character(len=1), intent(in) :: cchar                  !! comment character
        logical, intent(in) :: is_date_format               !! true if time in 'date' format
        logical, intent(in) :: is_yyddhh_format             !! true if yyetc instead of ddetc
        integer(kind = int_wp), intent(in) :: count_subs_comp_rule          !! number of subst in computational rule
        integer(kind = int_wp), intent(inout) :: start_in_line          !! start position on input line
        integer(kind = int_wp), intent(in) :: index_first               !! 1 = items first, 2 = substances first
        integer(kind = int_wp), intent(out) :: itype                    !! type of info at end
        integer(kind = int_wp), intent(in) :: ilun(lstack)                      !! unitnumb include stack
        integer(kind = int_wp), intent(out) :: int_output                 !! parameter read to be transferred
        integer(kind = int_wp), intent(out) :: error_idx         !! error index within current subroutine
        integer(kind = int_wp), intent(out) :: nocol     !! number of collums in matrix
        integer(kind = int_wp), intent(in) :: itfact            !! factor between clocks

        integer(kind = int_wp), intent(in) :: num_significant_char                  !! number of significant characters
        integer(kind = int_wp), intent(in) :: lstack            !! include file stack size
        real(kind = real_wp), intent(out) :: real_output               !!parameter read to be transferred

        !! names_to_check
        character :: strng*8
        logical :: first, must_read_more
        integer(kind = INT64) :: ihulp8
        integer(kind = int_wp) :: ithndl = 0
        integer(kind = int_wp) :: icnt, iods, k
        integer(kind = int_wp) :: count_items_assign            !! number of items for assignment
        integer(kind = int_wp) :: count_names
        integer(kind = int_wp) :: offset_i_array                !! offset  in int_array
        integer(kind = int_wp) :: offset_common, offset_names !! comon offset in int_array and names_to_check, offset in
        integer(kind = int_wp) :: nopos, ifound, i

        if (timon) call timstrt("validate_column_headers", ithndl)

        ! Array offsets
        offset_i_array = count_items_assign + count_items_comp_rule + &
                count_subs_assign + count_subs_comp_rule
        if (index_first == 1) then ! items first
            offset_names = count_items_assign + count_items_comp_rule + count_subs_assign
            offset_common = count_items_assign + count_items_comp_rule
            count_names = count_subs_comp_rule
        else if (index_first == 2) then !substances first
            offset_names = count_items_assign + count_subs_comp_rule + count_subs_assign
            offset_common = count_subs_comp_rule + count_subs_assign
            count_names = count_items_comp_rule
        endif

        ! read loop
        first = .true.
        must_read_more = .true.
        do while (must_read_more)
            itype = 0
            call rdtok1(ascii_output_file_unit, ilun, lch, lstack, cchar, &
                    start_in_line, num_significant_char, character_output, int_output, real_output, &
                    itype, error_idx)

            if (error_idx  /= 0) then ! error occurred when reading
                if (timon) call timstop(ithndl)
                return !exit subroutine
            end if

            ! no error
            if (itype == 1) then ! a string has arrived
                call convert_string_to_time_offset (character_output, int_output, .false., .false., error_idx)
                if (error_idx == 0) then
                    error_idx = -2
                    if (first) then
                        if (timon) call timstop(ithndl)
                        return  !exit subroutine
                    else
                        exit
                    endif
                endif
                if (first) then
                    first = .false.
                    do i = 1, count_names
                        int_array(offset_i_array + i) = 0
                    end do
                    nocol = 0
                    write (ascii_output_file_unit, *)
                endif
                nocol = nocol + 1
                strng = 'NOT used'
                do i = 1, count_names
                    if (string_equals(character_output(:20), names_to_check(offset_names + i))) then
                        strng = 'used'
                        int_array(i + offset_i_array) = nocol
                    endif
                end do
                write (ascii_output_file_unit, 1000) nocol, character_output, strng
            else
                if (itype == 2) then ! an integer has arrived
                    call convert_relative_time (int_output, itfact, is_date_format, is_yyddhh_format)
                endif
                error_idx = -1
                must_read_more = .false.
                if (first) then
                    if (timon) call timstop(ithndl)
                    return !exit subroutine
                end if
            endif
        end do

        ! is everything resolved ?
        icnt = 0
        iods = 0

        do i = 1, count_names
            k = i - icnt
            if ((names_to_check(offset_names + k) /= '&$&$SYSTEM_NAME&$&$!') &
                    .and.  (int_array(offset_i_array + k) <= 0)) then
                call compact_usefor_list(ascii_output_file_unit, int_array, count_items_assign, &
                        count_items_comp_rule, count_subs_assign, &
                        count_subs_comp_rule, index_first, names_to_check, &
                        offset_i_array, offset_names, &
                        iods, offset_common, k, icnt, error_idx, status)
            end if
        end do

        1000 FORMAT (' Column:', I3, ' contains: ', A40, ' Status: ', A8)

    end subroutine validate_column_headers

end module boundary_conditions
