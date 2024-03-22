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
module m_dlwq5a
    use m_waq_precision
    use m_string_utils
    use m_read_time_delay
    use m_dlwqj3
    use m_dlwq5g
    use m_dlwq5e
    use m_dlwq5d
    use m_dlwq5c
    use m_error_status

    implicit none

contains


    subroutine dlwq5a (lun, lchar, iu, iwidth, icmax, &
            car, iimax, iar, irmax, rar, &
            sname, aname, atype, bc_wl_count, &
            substances_count, &
            nttype, drar, dtflg1, dtflg3, &
            ioutpt, ierr2, status)

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
        ! LUN(27) = unit stripped DELWAQ input file
        ! LUN(29) = unit formatted output file
        ! LUN( 2) = unit intermediate file (system)
        ! LUN(14) = unit intermediate file (boundaries)
        ! LUN(15) = unit intermediate file (wastes)

        use m_dlwq5b
        use m_check
        use m_open_waq_files
        use rd_token
        use timers       !   performance timers
        use dlwq_hyd_data
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_string_utils, only : index_in_array

        implicit none

        integer(kind = int_wp), intent(inout) :: lun  (:)      !< array with unit numbers
        integer(kind = int_wp), intent(inout) :: iar  (iimax)  !< integer workspace

        character(*), intent(inout) :: lchar(:) !< filenames
        character(*), intent(inout) :: car(:)   !< character workspace !icmax
        character(*), intent(inout) :: sname(:) !< substances names
        character(*), intent(inout) :: aname(:) !< ID's of the boundaries/wastes
        character(*), intent(in) :: atype(:) !< Types of the boundaries/wastes

        integer(kind = int_wp), intent(in) :: iu               !< index in LUN array of workfile
        integer(kind = int_wp), intent(in) :: iwidth           !< width of the output file
        integer(kind = int_wp), intent(in) :: icmax            !< maximum size of character workspace
        integer(kind = int_wp), intent(in) :: iimax            !< maximum size of integer workspace
        integer(kind = int_wp), intent(inout) :: bc_wl_count      !< number of bounds/wastes
        integer(kind = int_wp), intent(inout) :: substances_count !< number of substances
        integer(kind = int_wp), intent(in) :: nttype           !< number of boundary/waste types
        integer(kind = int_wp), intent(in) :: irmax            !< maximum size of real workspace
        integer(kind = int_wp), intent(in) :: ioutpt           !< how extensive will the output be
        integer(kind = int_wp), intent(out) :: ierr2            !< return code of this routine

        real(kind = real_wp), intent(inout) :: rar  (irmax) !< real workspace
        real(kind = dp), intent(inout) :: drar (*)     !< Double precision workspace

        logical, intent(in) :: dtflg1 !< 'date'-format 1st time scale
        logical, intent(in) :: dtflg3 !< 'date'-format (F;ddmmhhss,T;yydddhh)

        type(error_status), intent(inout) :: status !< current error status

        !     Local declarations

        character     calit*10, caldit*10, strng1*10, strng2*10, strng3*10
        integer(kind = int_wp) :: iorder, count_items_in_use_rule, nodim, iflag, itype, &
                chkflg, ident, nottc, lunwr2, &
                file_size_1, file_size_2, ipro, itfacw, iopt, &
                nobrk, itel, ioerr, iblock, k, &
                i, ihulp, ioff, icm, iim, &
                noits, nconst, itmnr, idx_item_in_use_rule, nocol, &
                idmnr, nodis, nitm, nti, nti2, &
                ntr, irm, nottt, ierr3, nr2, &
                nts, ntc, ntd
        real(kind = real_wp) :: amiss, rhulp
        character     chulp*255
        logical       newrec, scale, ods, binfil, tdelay
        logical :: time_dependent !< Is the BC / Waste load definition time dependent (true)? Or constant (false)?
        integer(kind = int_wp) :: ithndl = 0

        type(t_dlwq_data_items) :: dlwq_data_items
        type(t_dlwq_item) :: dlwq_foritem
        character(20) :: data_item_name
        integer(kind = int_wp) :: idata_item
        integer(kind = int_wp) :: ndata_items
        integer(kind = int_wp) :: iitem
        integer(kind = int_wp) :: count_unique_items_in_use_rule

        if (timon) call timstrt("dlwq5a", ithndl)
        !
        !     Initialise a number of variables
        !
        lunut = lun(29)
        lunwr2 = lun(iu)
        file_size_1 = 0
        file_size_2 = 0
        strng2 = 'Substance'
        ipro = 0
        itfacw = 1
        deltim = otime
        amiss = -999.0
        ierr2 = dlwq_init_data_items(dlwq_data_items)
        ierr2 = dlwq_init_item(dlwq_foritem)
        !
        !          Initialise new data block
        !
        !     IORDER is the binary input flag,
        !                    0 = not set, 1 = items , 2 = concentr.
        !     IFLAG  is the ASCII input flag,
        !                    0 = not set, 1 = items , 2 = concentr.  3 = data
        !                    4 = scales
        !     NOBRK  is the number of breakpoints
        !     SCALE  is the Scale values flag .TRUE. is present
        !     USEFOR is the Alias flag, .TRUE. is alias string expected
        !     ODS    is the ODS   flag, .TRUE. ODS datafile expected
        !     NEWREC is the flag for new records
        !     IOPT   is option flag 1 = block function, 2 = linear
        !                           3 = harmonics     , 4 = fourier
        !     IOFF   is offset in the array of integers and strings
        !
        if (ioutpt < 3) write (lunut, 1340)
        if (ioutpt < 4) write (lunut, 1350)
        iorder = 0
        iflag = 0
        iopt = 1
        nobrk = 0
        itel = 0
        scale = .false.
        ods = .false.
        binfil = .false.
        newrec = .false.
        time_dependent = .false.
        !
        !     Open the binary work file and provide a zero overall default
        !
        call open_waq_files (lun(iu), lchar(iu), iu, 1, ioerr)
        if (iu == 14) then
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
        write (lunwr2) bc_wl_count, substances_count
        write (lunwr2) 1, 0, substances_count, (k, k = 1, substances_count), 1, 0
        write (lunwr2) 1
        write (lunwr2) 0, (0.0, i = 1, substances_count)
        file_size_1 = file_size_1 + 2 + 3 + substances_count + 3 + 1
        file_size_2 = file_size_2 + substances_count
        !
        !          Get a token string (and return if something else was found)
        !
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
        20 call rdtok1 (lunut, ilun, lch, lstack, cchar, &
                iposr, npos, chulp, ihulp, rhulp, &
                itype, ierr2)
        !        End of block detected
        if (ierr2 == 2) then
            if(itype > 0) then
                call status%increase_error_count()
            end if
            goto 530
        endif
        if (ierr2 /= 0) goto 510 !close ( lunwr2 )
        !
        !          All the following has the old file structure
        !
        if (iabs(itype) == 1 .and. chulp == 'OLD-FILE-STRUCTURE') then
            write (lunut, 1000)
            call status%increase_warning_count()
            ierr2 = -1
            goto 540
        endif
        !
        !          A local redirection of the name of an item or substance
        !                                                   is not valid here
        !
        if (iabs(itype) == 1 .and. chulp == 'USEFOR') then
            write (lunut, 1010)
            ierr2 = 1
            goto 510
        endif
        !
        !          Time delay for ODS files
        !
        30 if (iabs(itype) == 1 .and. chulp(1:10) == 'TIME_DELAY') then
            call read_time_delay (ierr2)
            if (ierr2 /= 0) goto 510
            goto 10
        endif
        !
        !          Time interpolation instead of block function
        !
        if (iabs(itype) == 1 .and. chulp(1:6) == 'LINEAR') then
            if (ioutpt >= 3) write (lunut, 1005)
            iopt = 2
            goto 10
        endif
        if (iabs(itype) == 1 .and. chulp(1:5) == 'BLOCK') then
            iopt = 1
            goto 10
        endif
        !
        !          Usedata_item
        !
        if (iabs(itype) == 1 .and. (chulp == 'USEDATA_ITEM')) then
            if (iorder == 1 .or. iorder == 2) then
                write (lunut, 1011)
                ierr2 = 1
                goto 510
            endif
            !          Next token must be a name
            call rdtok1 (lunut, ilun, lch, lstack, cchar, &
                    iposr, npos, chulp, ihulp, rhulp, &
                    itype, ierr2)
            if (chulp == ' ') then
                write (lunut, 1012)
                ierr2 = 1
                goto 510
            endif
            data_item_name = chulp
            if (ioutpt >= 3) write (lunut, 1015) data_item_name
            !          Next token must be 'FORITEM'
            call rdtok1 (lunut, ilun, lch, lstack, cchar, &
                    iposr, npos, chulp, ihulp, rhulp, &
                    itype, ierr2)
            if (chulp /= 'FORITEM') then
                write (lunut, 1012)
                ierr2 = 1
                goto 510
            endif
            ! Now get the list of locations to apply the DATA_ITEM
            ioff = 1
            chkflg = 1
            icm = icmax - ioff
            iim = iimax - ioff
            call dlwq5b (lunut, iposr, npos, cchar, car(ioff:), &
                    iar(ioff:), icm, iim, aname, atype, &
                    bc_wl_count, nttype, count_items_in_use_rule, noits, chkflg, &
                    calit, ilun, lch, lstack, &
                    itype, rar, nconst, itmnr, chulp, &
                    ioutpt, ierr2, status)
            ! Check if data_item already exists

            if (dlwq_data_items%cursize > 0) then
                idata_item = index_in_array(data_item_name, dlwq_data_items%name(1:dlwq_data_items%cursize))
            else
                idata_item = 0
            end if
            if (idata_item<=0) then ! first data_item (0) or not found (<0)
                ndata_items = dlwq_data_itemsAdd(dlwq_data_items, data_item_name, dlwq_foritem)
                idata_item = ndata_items
            endif
            if(dlwq_data_items%used(idata_item)) then
                write(lunut, 1016)
                call status%increase_warning_count()
            end if
            do idx_item_in_use_rule = 1, count_items_in_use_rule !count_items_in_use_rule
                !          Already on the list?
                count_unique_items_in_use_rule = dlwq_data_items%dlwq_foritem(idata_item)%no_item
                if (count_unique_items_in_use_rule>0) then
                    iitem = index_in_array(car(ioff + idx_item_in_use_rule - 1), &
                            dlwq_data_items%dlwq_foritem(idata_item)%name(1:count_unique_items_in_use_rule))
                else
                    iitem = -1
                end if
                if (iitem<=0 .and. iar(ioff + idx_item_in_use_rule - 1)/=-1300000000) then ! ! if the item has not been added to dlwq_data_items and it shouldn't be ignored
                    count_unique_items_in_use_rule = count_unique_items_in_use_rule + 1
                    ierr2 = dlwq_resize_item(dlwq_data_items%dlwq_foritem(idata_item), count_unique_items_in_use_rule)
                    dlwq_data_items%dlwq_foritem(idata_item)%name(count_unique_items_in_use_rule) = car(ioff + idx_item_in_use_rule - 1)
                    dlwq_data_items%dlwq_foritem(idata_item)%ipnt(count_unique_items_in_use_rule) = iar(ioff + idx_item_in_use_rule - 1)
                    dlwq_data_items%dlwq_foritem(idata_item)%sequence(count_unique_items_in_use_rule) = count_unique_items_in_use_rule
                    dlwq_data_items%dlwq_foritem(idata_item)%no_item = count_unique_items_in_use_rule
                end if
            end do
            if (ierr2 /= 0) goto 510
            goto 30
        endif
        !
        !          Items
        !
        if (iabs(itype) == 1 .and. (chulp == 'ITEM' .or. chulp == 'IDENTICALITEM' .or. &
                chulp == 'DATA_ITEM')) then
            if (iorder == 0) then !Items
                iorder = 1 !concentr.
                ioff = 1
                if (chulp == 'IDENTICALITEM') then
                    if (ioutpt >= 3) write (lunut, 1021)
                    ident = 1
                else if (chulp == 'DATA_ITEM') then
                    if (ioutpt >= 3) write (lunut, 1022)
                    ident = 2 ! DATA_ITEM
                else
                    if (ioutpt >= 3) write (lunut, 1020)
                    ident = 0
                endif
            elseif (iorder == 1) then
                write (lunut, 1030)
                ierr2 = 1
                goto 510
            else
                if (ioutpt >= 3) write (lunut, 1040)
                ioff = nodim + idmnr + 1
                ident = 0
            endif
            chkflg = 1
            icm = icmax - ioff
            iim = iimax - ioff
            if (ident <= 1) then ! ITEM, IDENTICALITEM
                call dlwq5b (lunut, iposr, npos, cchar, car(ioff:), &
                        iar(ioff:), icm, iim, aname, atype, &
                        bc_wl_count, nttype, count_items_in_use_rule, noits, chkflg, &
                        calit, ilun, lch, lstack, itype, &
                        rar, nconst, itmnr, chulp, ioutpt, &
                        ierr2, status)
            else ! DATA_ITEM
                call dlwq5b (lunut, iposr, npos, cchar, car(ioff:), &
                        iar(ioff:), icm, iim, dlwq_data_items%name(1:ndata_items), &
                        dlwq_data_items%name(1:ndata_items), &
                        ndata_items, ndata_items, count_items_in_use_rule, noits, chkflg, &
                        caldit, ilun, lch, lstack, itype, &
                        rar, nconst, itmnr, chulp, ioutpt, &
                        ierr2, status)
                if (count_items_in_use_rule/=1) then
                    write (lunut, 1045)
                    ierr2 = 1
                    goto 510
                end if
                idata_item = iar(ioff)
                if (idata_item > 0) then
                    !          Replace result of dlwq5b with usedata_item list
                    dlwq_data_items%used(idata_item) = .true.
                    count_items_in_use_rule = dlwq_data_items%dlwq_foritem(idata_item)%no_item !for how many items is applicable this usedata_item?
                    if (count_items_in_use_rule /= 0) then
                        itmnr = count_items_in_use_rule
                        noits = count_items_in_use_rule
                        do idx_item_in_use_rule = 1, count_items_in_use_rule
                            car(ioff + idx_item_in_use_rule - 1) = dlwq_data_items%dlwq_foritem(idata_item)%name(idx_item_in_use_rule)
                            car(ioff + idx_item_in_use_rule - 1 + count_items_in_use_rule) = &
                                    dlwq_data_items%dlwq_foritem(idata_item)%name(idx_item_in_use_rule)
                            iar(ioff + idx_item_in_use_rule - 1) = dlwq_data_items%dlwq_foritem(idata_item)%ipnt(idx_item_in_use_rule)
                            iar(ioff + idx_item_in_use_rule - 1 + count_items_in_use_rule) = &
                                    dlwq_data_items%dlwq_foritem(idata_item)%sequence(idx_item_in_use_rule)
                            if (iar(ioff + idx_item_in_use_rule - 1) > 0) then ! I think the log statements below are wrong, not the type but the DATA_ITEM should be printed as 1st argument
                                write (lunut, 1023) car(ioff), idata_item, calit, iar(ioff + idx_item_in_use_rule - 1), &
                                        dlwq_data_items%dlwq_foritem(idata_item)%name(idx_item_in_use_rule)
                            else
                                write (lunut, 1024) car(ioff), idata_item, calit, iar(ioff + idx_item_in_use_rule - 1), &
                                        dlwq_data_items%dlwq_foritem(idata_item)%name(idx_item_in_use_rule)
                            end if
                        end do
                    else
                        write (lunut, 1046)
                        !data ignored
                        count_items_in_use_rule = 1
                        itmnr = 1
                        noits = 1
                        iar(ioff) = -1300000000
                        iar(ioff + 1) = 1
                    end if
                else
                    write (lunut, 1047)
                    !data ignored
                    count_items_in_use_rule = 1
                    itmnr = 1
                    noits = 1
                    iar(ioff) = -1300000000
                    iar(ioff + 1) = 1
                end if
            endif

            nocol = noits
            if (ierr2 /= 0) goto 510
            goto 30
        endif
        !
        !          Concentrations
        !
        if (iabs(itype) == 1 .and. chulp(1:6) == 'CONCEN') then
            if (iorder == 0) then
                if (ioutpt >= 3) write (lunut, 1050)
                iorder = 2
                ioff = 1
            elseif (iorder == 1) then
                if (ioutpt >= 3) write (lunut, 1060)
                ioff = count_items_in_use_rule + itmnr + 1
            else
                write (lunut, 1070)
                ierr2 = 1
                goto 510
            endif
            chkflg = 1
            icm = icmax - ioff
            iim = iimax - ioff
            call dlwq5b (lunut, iposr, npos, cchar, car(ioff:), &
                    iar(ioff:), icm, iim, sname, atype, &
                    substances_count, 0, nodim, nodis, chkflg, &
                    'CONCENTR. ', ilun, lch, lstack, &
                    itype, rar, nconst, idmnr, chulp, &
                    ioutpt, ierr2, status)
            nocol = nodis
            if (ierr2 /= 0) goto 510
            goto 30
        endif
        !
        !          Data
        !
        if (iabs(itype) == 1 .and. chulp(1:6) == 'DATA') then
            if (count_items_in_use_rule * nodim == 0) then ! nodim = count_subs
                write (lunut, 1080) count_items_in_use_rule, nodim
                ierr2 = 1
                goto 510
            endif
            !          Checks if an inner loop column header exists for the data matrix
            call dlwq5g (lunut, iar, itmnr, count_items_in_use_rule, idmnr, &
                    nodim, iorder, iimax, car, iposr, &
                    npos, ilun, lch, lstack, cchar, &
                    chulp, nocol, dtflg1, dtflg3, itfacw, &
                    itype, ihulp, rhulp, ierr2, status)
            if (ierr2 > 1) goto 510
            !          Reads blocks of data
            if (iorder == 2) then
                nitm = count_items_in_use_rule
            else
                nitm = nodim
            endif
            nti = count_items_in_use_rule + nodim + itmnr + idmnr + 1
            nti2 = nti + nitm
            iim = iimax - nti2
            ntr = nconst + itel + 1
            iim = iimax - nti2
            irm = irmax - ntr
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
            if (iopt == 3 .or. iopt == 4) nottt = nottt + 1  ! harmonics or fourier
            call dlwq5d (lunut, iar(nti2:), rar(ntr:), iim, irm, & ! read time series table
                    iposr, npos, ilun, lch, lstack, &
                    cchar, chulp, nottt, nottc, time_dependent, nobrk, &
                    iopt, dtflg1, dtflg3, itfacw, itype, &
                    ihulp, rhulp, ierr2, ierr3)

            call status%increase_error_count_with(ierr3)

            if (ierr2 == 1 .or. ierr2 == 4) goto 510
            if (nobrk == 0 .and. (.not. time_dependent)) then
                write(lunut, 1360)
                call status%increase_error_count()
            endif
            if (nodim == 0) then
                write(lunut, 1370)
                call status%increase_warning_count()
            else
                !          Assigns according to computational rules
                nr2 = ntr + nottt * nobrk
                call dlwq5e (lunut, iar, count_items_in_use_rule, itmnr, nodim, & ! process parsed values in table  (process operations if any) and store results in rar(nr2:)
                        idmnr, iorder, rar, iopt, rar(ntr:), &
                        nocol, nobrk, amiss, iar(nti:), rar(nr2:))
                strng3 = 'breakpoint'
                !          Writes to the binary intermediate file
                nts = nconst + 1
                ntc = nti
                icm = icmax - ntc
                call dlwqj3 (lunwr2, lunut, iwidth, nobrk, iar, & ! writes data_item to wrk and/or lst files
                        rar(nts:), rar(nr2:), itmnr, idmnr, iorder, &
                        scale, .true., binfil, iopt, ipro, &
                        itfacw, dtflg1, dtflg3, file_size_1, file_size_2, &
                        sname, strng1, strng2, strng3, ioutpt)
            endif
            if (ierr2 == 2) goto 530
            if (ierr2 == 3) goto 510
            iorder = 0
            iflag = 0
            iopt = 1
            amiss = -999.0
            nobrk = 0
            itel = 0
            scale = .false.
            ods = .false.
            binfil = .false.
            newrec = .false.
            time_dependent = .false.
            if (itype == 1) goto  30
            goto 10
        endif
        !
        !          ODS-file option selected
        !
        if (iabs(itype) == 1 .and. chulp(1:8) == 'ODS_FILE') then
            if (count_items_in_use_rule * nodim == 0) then
                write (lunut, 1080) count_items_in_use_rule, nodim
                ierr2 = 1
                goto 510
            endif
            ods = .true.
            itype = 1
            goto 20
        endif
        !
        !          ODS-file-data retrieval
        !
        if (iabs(itype) == 1 .and. ods) then
            nti = count_items_in_use_rule + nodim + itmnr + idmnr + 1
            ntr = itel + nconst + 1
            ntd = (ntr + 1) / 2 + 1
            nts = nconst + 1
            iim = iimax - nti
            irm = irmax - ntr
            call dlwq5c (chulp, lunut, car, iar, rar(ntr:), &
                    icmax, iimax, irmax, drar, count_items_in_use_rule, &
                    nodim, iorder, scale, itmnr, idmnr, &
                    amiss, nobrk, ierr2, status)
            if (ierr2 /= 0) goto 510
            nr2 = ntr + count_items_in_use_rule * nodim * nobrk
            call dlwq5e (lunut, iar, count_items_in_use_rule, itmnr, nodim, &
                    idmnr, iorder, rar, iopt, rar(ntr:), &
                    nodim, nobrk, amiss, iar(nti:), rar(nr2:))
            strng3 = 'breakpoint'
            call dlwqj3 (lunwr2, lunut, iwidth, nobrk, iar, &
                    rar(nts:), rar(nr2:), itmnr, idmnr, iorder, &
                    scale, ods, binfil, iopt, ipro, &
                    itfacw, dtflg1, dtflg3, file_size_1, file_size_2, &
                    sname, strng1, strng2, strng3, ioutpt)
            if (ierr2 == 2) then
                ierr2 = -2
                goto 530
            endif
            if (ierr2 == 3) goto 510
            iorder = 0
            iflag = 0
            ioff = 0
            iopt = 1
            amiss = -999.0
            nobrk = 0
            itel = 0
            scale = .false.
            ods = .false.
            binfil = .false.
            newrec = .false.
            time_dependent = .false.
            goto  10
        endif
        !
        !          Absolute or relative timers
        !
        if (iabs(itype) == 1 .and. chulp(1:8) == 'ABSOLUTE') then
            write (lunut, 1135)
            chulp = 'TIME'
        endif
        !
        !          Say it is a time function
        !
        if (iabs(itype) == 1 .and. chulp(1:4) == 'TIME') then
            time_dependent = .true.
            iopt = 1
            goto 10
        endif
        !
        !          Scale factors begin
        !
        if (iabs(itype) == 1 .and. chulp == 'SCALE') then
            if (nodim == 0) then
                write (lunut, 1180)
                ierr2 = 1
                goto 510
            endif
            if (itel /= 0) then
                write (lunut, 1190)
                ierr2 = 1
                goto 510
            endif
            iflag = 4
            scale = .true.
            goto 10
        endif
        !          Getting the scale factors
        if (iflag == 4) then
            itel = itel + 1
            rar(itel + nconst) = rhulp
            if (itel == idmnr) iflag = 0
            goto 10
        endif
        !
        write (lunut, 1320) chulp
        write (lunut, '(A)') ' Expected character string should be a valid level 2 keyword'
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
        call check  (chulp, iwidth, iblock, ierr2, status)
        540 if (timon) call timstop(ithndl)
        return
        !
        1000 format (/' WARNING: Old file structure is used for this data !')
        ! 1002 format (  ' Delay integers: IDATE = ',I6,', ITIME = ',I6,'.' )
        ! 1003 format (  ' New reference time is day: ',I2,'-',I2,'-',I4, ' / ',I2,'H-',I2,'M-',I2,'S.' )
        ! 1004 format ( /' ERROR: you specified the TIME_DELAY keyword without', &
        !               ' a valid value string for the delay !'/' 2 integers', &
        !               ' are expected in YYMMDD and HHMMSS format !')
        1005 format (/' Linear interpolation is selected !')
        1010 format (/' ERROR: USEFOR is not alowed here. Specify ITEM or CONCENTRATION first !')
        1011 format (/' ERROR: USEDATA_ITEM is not alowed here. Specify before ITEM or CONCENTRATION !')
        1012 format (/' ERROR: Expected a DATA_ITEM name')
        ! 1013 format ( /' ERROR: Expected USEDATA_ITEM keyword after USEDATA_ITEM <data_item>!' )
        1015 format (/' When the DATA_ITEM ', A, ' is met, data is applied to the following items:') ! ( /' When the DATA_ITEM ',A,' nr: ',I5,' is met, data is applied to the flowing items:'   )
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
        ! 1110 format (  ' DATA will be retrieved from ODS-file: ',A )
        ! 1120 format (  ' ERROR: Insufficient memory ! Available:',I10, ', needed:',I10,' !' )
        ! 1130 format (  ' This block consists of a time function.' )
        1135 format (' Absolute times (YYYY/MM/DD;HH:MM:SS) expected in next', ' time function block.')
        ! 1160 format (  ' Number of valid time steps found: ',I6 )
        ! 1170 format (  ' This block consists of constant data.' )
        1180 format (' ERROR: number of substances is zero !')
        1190 format (' ERROR: data has already been entered !')
        1320 format (' ERROR: token found on input file: ', A)
        ! 1330 format (/1X, 59('*'),' B L O C K -',I2,' ',5('*')/)
        1340 format (' Output on administration only writen for output option 3 and higher !')
        1350 format (' Output of the data only writen for output option 4 and higher !')
        1360 format (' ERROR: no (valid) DATA record available !')
        1370 format (' WARNING: all DATA from this block is ignored !')
        ! 2220 format (  ' Input comes from binary file: ',A      )
        !
    end subroutine dlwq5a
    !
    !     Additional documentation on the memory lay out
    !
    !     After ITEM, 5B reads Item names. It produces:
    !     a) in CAR: a series of that item name and then a second series
    !        with the same name, or the substituted name after USEFOR
    !        If ' ' is specified, 'Item-nnn' will appear.
    !        If a positive integer is specified, the name of the
    !        corresponding item is given. For negative numbers the name
    !        of the corresponding type is provided
    !        FLOW is a valid item name if concentrations are expected for
    !        wasteloads.
    !        If a segment number is expected, 'Segment mmmmmmmm' is produced.
    !        ITMNR counts the first series, NOITM the second series.
    !     b) in IAR: a series of the number of the item from the items list
    !                the same series again
    !                a series with sequence numbers
    !        ITMNR counts the first series, NOITM the second and third series
    !        For types a negative number is used
    !        If FLOW is allowed, it has number 0
    !     If a number is used after a USEFOR, it is stored in RAR. In the
    !        CAR location now the word "&$&$SYSTEM_NAME&$&$!" is placed.
    !        In the second series location in IAR, the negative value of the
    !        location in RAR is placed. The sequence number is decremented
    !     If a computational sign is (*,/,+,-,MIN,MAX) is encountered:
    !        The second series location in IAR contains a large negative
    !           number depending on the computational sign. Sequence number
    !           and item number are incremented
    !        If now the name of a previously used item is given, the first
    !           series location of IAR contains this item number, the second
    !           series location in CAR contains "&$&$SYSTEM_NAME&$&$!" and
    !           the sequence number is decremented
    !        If a new name was provided, the 3rd series of IAR is set at
    !           the corresponding sequence number and the second series
    !           location in CAR contains that name
    !     At first invocation, this all starts at the beginning of the arrays
    !     ITMNR contains the number of items processed. It is the dimension
    !        of the first series in IAR and CAR
    !     NOITS contains the sequence number. That many things are waiting to
    !        be resolved and this number was used as dimension for SCALE
    !     NOITM contains the second and third series dimension in IAR and CAR
    !     NCONST is the number of constants that was put in RAR
    !     ITYPE and CHULP are the type and content of the token at exit
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

end module m_dlwq5a
