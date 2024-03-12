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
module m_read_data_ods
    use m_waq_precision
    use m_string_utils

    implicit none

contains


    subroutine read_data_ods(lunut, fname, data_param, data_loc, amiss, &
            data_block, ierr)

        !     Deltares Software Centre

        !     function : read a block of data from ODS file

        !     Global declarations

        use m_gettme
        use m_getpar
        use m_getmat
        use m_getloc
        use m_getdim
        use dlwq_hyd_data   ! for definition and storage of data
        use timers          !   performance timers
        use m_sysi          ! Timer characteristics
        use time_module

        implicit none

        !     declaration of arguments

        integer(kind = int_wp), intent(in) :: lunut         ! report file
        character(len = *), intent(in) :: fname        ! filename ODS file
        type(t_dlwq_item), intent(inout) :: data_param   ! list of param items in the data
        type(t_dlwq_item), intent(inout) :: data_loc     ! list of loc items in the data
        real(kind = real_wp), intent(in) :: amiss         ! missing value
        type(t_dlwqdata), intent(inout) :: data_block   ! data block
        integer(kind = int_wp), intent(inout) :: ierr          ! cummulative error count

        !     local declarations

        integer(kind = int_wp) :: iorder        ! order of the parameters and locations in the data array
        integer(kind = int_wp) :: loc(3)        ! to pass the locations to ODS
        character(len = 256) :: cfile(3)     ! to pass the filename to ODS
        character(len = 3) :: cdummy       ! dummy not used
        real(kind = dp) :: afact         ! scale factor for times
        real(kind = dp) :: a1            ! time
        real(kind = dp) :: a2            ! time
        character(len = 20) :: locdef(1)    ! to strore wanted locations
        character(len = 20), allocatable :: locnam(:)    ! locations in file
        integer(kind = int_wp), allocatable :: loctyp(:)     ! locations type in file
        integer(kind = int_wp), allocatable :: locnr(:)      ! locations numbers in file
        integer(kind = int_wp), allocatable :: iloc_ods(:)   ! locations index in file of the wanted locations
        character(len = 20) :: pardef(1)    ! to strore wanted parameters
        character(len = 20), allocatable :: parnam(:)    ! parameters in file
        character(len = 20), allocatable :: parunit(:)   ! parameters unit in file
        integer(kind = int_wp), allocatable :: partyp(:)     ! parameters type in file
        integer(kind = int_wp), allocatable :: parnr(:)      ! parameters numbers in file
        integer(kind = int_wp), allocatable :: ipar_ods(:)   ! parameter index in file of the wanted parameters
        real(kind = dp) :: timdef(2)     ! to store wanted times
        real(kind = dp), allocatable :: times(:)      ! times
        integer(kind = int_wp), allocatable :: timetyp(:)    ! time types ?
        real(kind = real_wp), allocatable :: buffer(:)     ! data buffer for read
        real(kind = real_wp), allocatable :: buffer2(:, :, :) ! data buffer for read
        integer(kind = int_wp) :: nsubs         ! number of parameters in file
        integer(kind = int_wp) :: nlocs         ! number of locations in file
        integer(kind = int_wp) :: ntims         ! number of times in file
        integer(kind = int_wp) :: nopar         ! number of parameters in file
        integer(kind = int_wp) :: noloc         ! number of locations in file
        integer(kind = int_wp) :: ndim1          ! first dimension
        integer(kind = int_wp) :: ndim2          ! second dimension
        integer(kind = int_wp) :: nobrk         ! number of times in file
        integer(kind = int_wp) :: iloc          ! location index / loop counter
        integer(kind = int_wp) :: ipar          ! parameter index / loop counter
        integer(kind = int_wp) :: ibrk          ! time index / loop counter
        integer(kind = int_wp) :: iloc_found    ! location index in file
        integer(kind = int_wp) :: ipar_found    ! parameter index in file
        integer(kind = int_wp) :: ierror        ! ierror
        logical :: calculation  ! indicates that item is part of calculation
        integer(kind = int_wp) :: i, i1, i2
        integer(kind = int_wp) :: iy1, im1, id1, ih1, in1, is1
        integer(kind = int_wp) :: iy2, im2, id2, ih2, in2, is2
        real(kind = dp) :: dummy         ! second in double precision (not used)
        integer(kind = int_wp) :: maxdim
        integer(kind = int_wp) :: ierr_alloc

        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_data_ods", ithndl)

        ! write the ods file name

        write (lunut, 1000) fname

        ! get the dimensions of the ods file

        cfile(1) = fname
        cfile(3) = ' '
        call getdim (cfile, 0, cdummy, 0, 0, &
                0, loc, ierror, cfile(3))
        nsubs = loc(1)
        nlocs = loc(2)
        ntims = loc(3)

        ! get the available locations

        allocate(locnam(nlocs), loctyp(nlocs), locnr(nlocs))
        allocate(iloc_ods(data_loc%no_item))
        locdef(1) = '*'
        call getloc (cfile, 0, locdef, 1, 0, &
                0, nlocs, locnam, loctyp, locnr, &
                noloc, ierror, cfile(3))

        ! Fill an array with wanted locations

        do iloc = 1, data_loc%no_item
            if (data_loc%name(iloc) == '&$&$SYSTEM_NAME&$&$!') then
                iloc_ods(iloc) = -1
            else
                iloc_found = index_in_array(data_loc%name(iloc), locnam(:noloc))
                if (iloc_found >= 1) then
                    iloc_ods(iloc) = iloc_found
                else

                    ! location not found, warning

                    iloc_ods(iloc) = 0
                    write (lunut, 1070) data_loc%ipnt(iloc), data_loc%name(iloc)

                    ! check if location in calculation, then error, but is this check sufficient

                    calculation = data_loc%ipnt(iloc) < 0
                    if (iloc /= data_loc%no_item) calculation = calculation .or. data_loc%ipnt(iloc + 1) < 0
                    if (calculation) then
                        write (lunut, 1080)
                        ierr = 2
                        goto 510
                    endif

                endif
            endif
        enddo
        deallocate(locnam, loctyp, locnr)

        ! get the available parameters

        allocate(parnam(nsubs), parunit(nsubs), partyp(nsubs), parnr(nsubs))
        allocate(ipar_ods(data_param%no_item))
        pardef(1) = '*'
        call getpar (cfile, 0, pardef, 1, 0, &
                0, nsubs, 0, parnam, parunit, &
                partyp, parnr, nopar, ierror, cfile(3))

        ! fill an array with wanted parameters

        do ipar = 1, data_param%no_item
            if (data_param%name(ipar) == '&$&$SYSTEM_NAME&$&$!') then
                ipar_ods(ipar) = 0
            else
                ipar_found = index_in_array(data_param%name(ipar), parnam(:nopar))
                if (ipar_found > 0) then
                    ipar_ods(ipar) = ipar_found
                else

                    ! no compacting pointers for the moment, but how to deal with computation?

                    ipar_ods(ipar) = 0

                endif
            endif
        enddo
        deallocate(parnam, parunit, partyp, parnr)

        ! get the available time values

        allocate(times(ntims), timetyp(ntims))
        timdef(1) = 0.0d0
        call gettme (cfile, 0, timdef, 1, 0, &
                0, ntims, times, timetyp, nobrk, &
                ierror, cfile(3))

        ! see if the found time values are within the range

        afact = isfact / 864.0d+02
        if (isfact < 0) afact = -1.0d+00 / isfact / 864.0d+02
        if (nobrk >= 1) then
            write (lunut, 1020)
            a1 = deltim + itstrt * afact
            a2 = deltim + itstop * afact
            i1 = 1
            i2 = 1
            do ibrk = 1, nobrk
                if (times(ibrk) <= a1) i1 = ibrk
                if (times(ibrk) < a2) i2 = ibrk
            enddo
            if (i2 /= nobrk) i2 = i2 + 1
            if (times(nobrk) < a1) i2 = 1

            ! errors and warnings

            if (times(1) > a1) then
                call gregor (times(1), iy1, im1, id1, ih1, in1, is1, dummy)
                call gregor (a1, iy2, im2, id2, ih2, in2, is2, dummy)
                write (lunut, 1030)  iy1, im1, id1, ih1, in1, is1, &
                        iy2, im2, id2, ih2, in2, is2
            endif
            if (times(nobrk) < a2) then
                call gregor (times(nobrk), iy1, im1, id1, ih1, in1, is1, dummy)
                call gregor (a2, iy2, im2, id2, ih2, in2, is2, dummy)
                write (lunut, 1040)  iy1, im1, id1, ih1, in1, is1, &
                        iy2, im2, id2, ih2, in2, is2
            endif
            nobrk = i2 - i1 + 1
        endif
        write (lunut, 1050) nobrk
        if (nobrk == 1)    write (lunut, 1060)

        !      times are converted to delwaq times

        data_block%no_brk = nobrk
        allocate(data_block%times(nobrk))
        do ibrk = i1, i2
            a2 = times(ibrk) - a1
            data_block%times(ibrk - i1 + 1) = a2 / afact + 0.5
        enddo

        iorder = ORDER_PARAM_LOC
        data_block%iorder = iorder
        if (iorder == ORDER_PARAM_LOC) then
            ndim1 = data_param%no_item
            ndim2 = data_loc%no_item
        else
            ndim1 = data_loc%no_item
            ndim2 = data_param%no_item
        endif
        data_block%no_param = data_param%no_item
        data_block%no_loc = data_loc%no_item

        allocate(data_block%values(ndim1, ndim2, nobrk), buffer(nobrk))
        data_block%values = amiss

        ! set the time margins for retrieval

        timdef(1) = times(1) - afact / 2.0
        timdef(2) = times(nobrk) + afact / 2.0
        deallocate(times, timetyp)

        ! get the data themselves

        ! try the read the data in one time

        allocate(buffer2(nsubs, nlocs, nobrk), stat = ierr_alloc)
        if (ierr_alloc == 0) then
            maxdim = nsubs * nlocs * nobrk
            call getmat2(cfile, 0, ipar_ods, loc, timdef, &
                    amiss, maxdim, buffer2, ierror, &
                    cfile(3))
            do ipar = 1, data_param%no_item
                if (ipar_ods(ipar) > 0) then
                    do iloc = 1, data_loc%no_item
                        if (iloc_ods(iloc) > 0) then
                            do ibrk = 1, nobrk
                                if (iorder == ORDER_PARAM_LOC) then
                                    data_block%values(ipar, iloc, ibrk) = buffer2(ipar_ods(ipar), iloc_ods(iloc), ibrk)
                                else
                                    data_block%values(iloc, ipar, ibrk) = buffer2(ipar_ods(ipar), iloc_ods(iloc), ibrk)
                                endif
                            enddo
                        endif
                    enddo
                endif
            enddo
            deallocate(buffer2)
        else
            loc(3) = 1
            do ipar = 1, data_param%no_item
                if (ipar_ods(ipar) > 0) then
                    do iloc = 1, data_loc%no_item
                        if (iloc_ods(iloc) > 0) then
                            loc(1) = iloc_ods(iloc)
                            loc(2) = iloc_ods(iloc)
                            call getmat (cfile, 0, ipar_ods(ipar), loc, timdef, &
                                    amiss, nobrk, buffer, ierror, &
                                    cfile(3))
                            do ibrk = 1, nobrk
                                if (iorder == ORDER_PARAM_LOC) then
                                    data_block%values(ipar, iloc, ibrk) = buffer(ibrk)
                                else
                                    data_block%values(iloc, ipar, ibrk) = buffer(ibrk)
                                endif
                            enddo
                        endif
                    enddo
                endif
            enddo
        endif
        deallocate(buffer, ipar_ods, iloc_ods)

        ! the sequence is the same as we read it, maybe always set both par and loc??

        if (iorder == ORDER_PARAM_LOC) then
            do i = 1, data_param%no_item
                data_param%sequence(i) = i
            enddo
        else
            do i = 1, data_loc%no_item
                data_loc%sequence(i) = i
            enddo
        endif

        510 continue
        if (timon) call timstop(ithndl)
        return

        ! formats

        1000 FORMAT (' DATA will be retrieved from ODS-file: ', A)
        1020 FORMAT (' This block consists of a time function.')
        1030 FORMAT (' WARNING: file start time   : ', &
                I4, '.', I2, '.', I2, ' ', I2, ':', I2, ':', I2, / &
                ' after simulation start time: ', &
                I4, '.', I2, '.', I2, ' ', I2, ':', I2, ':', I2, ' !')
        1040 FORMAT (' WARNING: file stop  time   : ', &
                I4, '.', I2, '.', I2, ' ', I2, ':', I2, ':', I2, / &
                ' before simulation stop time: ', &
                I4, '.', I2, '.', I2, ' ', I2, ':', I2, ':', I2, ' !')
        1050 FORMAT (' Number of valid time steps found: ', I6)
        1060 FORMAT (' This block consists of constant data.')
        1070 FORMAT (' WARNING: location : ', I8, ' not found. Name is: ', A)
        1080 FORMAT (' ERROR  : location is used in a computation', &
                ' that will become corrupted !')
        !
    END

end module m_read_data_ods
