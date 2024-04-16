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
module monitoring_areas
    use m_waq_precision
    use m_string_utils
    use m_error_status

    implicit none

    private
    public :: read_monitoring_areas, read_monitoring_transects, create_write_monitoring_area_array

contains

    subroutine read_monitoring_areas(lun, lchar, filtype, duname, nsegdmp, &
            isegdmp, dmpbal, ndmpar, ntdmps, output_verbose_level, &
            ierr, status)

        !! Reads monitoring areas
        !!
        !!           - Reads an option\n
        !!             This can be:
        !!             - -1 for an external ASCII file, superfluous because of INCLUDE support
        !!             - 1 for continuation on this file
        !!             - 2 to indicate that no monitoring areas are used
        !!           - The amount of monitoring areas
        !!           - Per monitoring area:
        !!             - the name (20 characters) of the area
        !!             - optionally balance option NO_BALANCE/BALANCE(default)
        !!             - the number of volumes included in the area
        !!             - that many volume numbers
        !! Subroutine called : process_simulation_input_options   - to open an external file
        !!                         ZOEK   - to searchs strings
        !!
        !!     Logical units     : LUN(27) = unitnumber stripped DELWAQ input file
        !!                         LUN(29) = unitnumber formatted output file

        use simulation_input_options, only: process_simulation_input_options
        use rd_token     !   for the reading of tokens
        use timers       !   performance timers

        integer(kind = int_wp), intent(inout) :: lun    (*)         !< array with unit numbers
        character(*), intent(inout) :: lchar  (*)        !< array with file names of the files
        integer(kind = int_wp), intent(inout) :: filtype(*)         !< type of binary file
        character(20), pointer :: duname (:)        !< name of monitoring areas
        integer(kind = int_wp), pointer :: nsegdmp(:)         !< number of volumes per monitoring area
        integer(kind = int_wp), pointer :: isegdmp(:)         !< volumes numbers per monitoring area
        integer(kind = int_wp), pointer :: dmpbal(:)          !< balance option (flag) per monitoring area
        integer(kind = int_wp), intent(out) :: ndmpar             !< number of monitoring areas
        integer(kind = int_wp), intent(out) :: ntdmps             !< total number of volumes in monitoring areas
        integer(kind = int_wp), intent(in) :: output_verbose_level             !< flag for more or less output
        integer(kind = int_wp), intent(inout) :: ierr               !< error   count

        type(error_status), intent(inout) :: status !< current error status

        integer(kind = int_wp) :: idopt1            ! First option number monitoring areas
        integer(kind = int_wp) :: ierr2             ! Local error flag
        integer(kind = int_wp) :: max_ntdmps        ! Size of isegdmp
        integer(kind = int_wp), pointer :: isegdmp_2(:)      ! Help pointer for array expansion
        integer(kind = int_wp) :: ierr_alloc        ! Error indicator for allocations
        integer(kind = int_wp) :: nseg              ! Number of volumes per monitoring area
        logical             ldummy           ! Dummy logical
        integer(kind = int_wp) :: id                ! Loop variable over all monitoring areas
        integer(kind = int_wp) :: k                 ! General loop variable
        character(len = 256) :: option           ! balance option
        integer(kind = int_wp) :: itype             ! type of the returned token
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_monitoring_areas", ithndl)

        !     Read file option

        if (gettoken(idopt1, ierr2) > 0) goto 20
        select case (idopt1)
        case (:-2)
            write (lunut, 2000)  idopt1
            write (lunut, 2010)
            goto 20
        case (-1)                     ! old style <other ASCII file>
            write (lunut, 2000)  idopt1
            call process_simulation_input_options   (idopt1, lun, 0, lchar, filtype, &
                    ldummy, ldummy, 0, ierr2, status, &
                    .false.)
            if (ierr2 > 0) goto 20
            if (gettoken(ndmpar, ierr2) > 0) goto 20
        case (0)                      ! new style (October 2012) no dump areas
            write (lunut, 2020)       ! old style would have produced an error
            ndmpar = 0
            ntdmps = 0
            goto 30
        case (1)                      ! old style <this input file> or new style 1 area
            if (gettoken(option, ndmpar, itype, ierr2) > 0) goto 20
            if (itype == 1) then     ! character so: new style, 1 area
                push = .true.
                ndmpar = idopt1
            else
                write (lunut, 2000)  idopt1
            endif
        case (2)                      ! old style <no dump areas> or new style 2 areas
            if (gettoken(option, ndmpar, itype, ierr2) > 0) goto 20
            push = .true.                ! look to see what will be next
            if (itype == 1) then     ! a string, so first dump-ID from 2 areas
                ndmpar = idopt1
            else                         ! an integer, so 2 meant <not used>
                write (lunut, 2000)  idopt1
                write (lunut, 2020)
                ndmpar = 0
                ntdmps = 0
                goto 30
            endif
        case default                    ! new input processing
            ndmpar = idopt1

        end select

        ! Write number of dump areas, allocate arrays
        write(lunut, 2030) ndmpar
        ntdmps = 0
        allocate (duname(ndmpar), stat = ierr_alloc)
        if (ierr_alloc /= 0) then
            write (lunut, 2380) ierr_alloc
            goto 20
        endif
        allocate (nsegdmp(ndmpar), isegdmp(ndmpar), dmpbal(ndmpar), stat = ierr_alloc)
        if (ierr_alloc /= 0) then
            write (lunut, 2390) ierr_alloc
            goto 20
        endif
        max_ntdmps = ndmpar

        ! Read specification of the dump areas
        if (output_verbose_level < 2) write (lunut, 2040)
        if (output_verbose_level == 2) write (lunut, 2050)
        do id = 1, ndmpar
            if (gettoken(duname(id), ierr2) > 0) goto 20
            if (gettoken(option, nseg, itype, ierr2) > 0) goto 20
            if (itype == 1) then                    ! character
                if (option == 'BALANCE') then
                    dmpbal(id) = 1
                elseif (option == 'NO_BALANCE') then
                    dmpbal(id) = 0
                else
                    write(lunut, 2420) trim(option)
                    goto 20
                endif
                if (gettoken(nseg, ierr2) > 0) goto 20
            else
                dmpbal(id) = 1
            endif
            if (ntdmps + nseg > max_ntdmps) then
                max_ntdmps = 2 * (ntdmps + nseg)
                allocate (isegdmp_2(max_ntdmps), stat = ierr_alloc)
                if (ierr_alloc /= 0) then
                    write (lunut, 2400) ierr_alloc
                    goto 20
                endif
                isegdmp_2(1:ntdmps) = isegdmp(1:ntdmps)
                deallocate(isegdmp)
                isegdmp => isegdmp_2
            endif
            do k = 1, nseg
                if (gettoken(isegdmp(ntdmps + k), ierr2) > 0) goto 20
            enddo
            if (duname(id) == ' ') write(duname(id), '(''Observation-id'',i6)') id

            ! check if name is unique
            do k = 1, id - 1
                if (string_equals(duname(id), duname(k))) then
                    write(lunut, 2410) duname(id)
                    ierr = ierr + 1
                endif
            enddo

            if (output_verbose_level >= 2) then
                write(lunut, 2060) id, duname(id), nseg
                if (dmpbal(id) == 0) then
                    write(lunut, 2430)
                endif
                if (output_verbose_level >= 3) then
                    write(lunut, 2070)
                    write(lunut, 2080) (k, isegdmp(ntdmps + k), k = 1, nseg)
                endif
            endif

            ntdmps = ntdmps + nseg
            nsegdmp(id) = nseg

        end do
        goto 30

        ! Error handling
        20 ierr = ierr + 1
        30 if (timon) call timstop(ithndl)
        return

        2000 format (/, ' Dump areas:', &
                / ' option selected for input :', I5)
        2010 format (/ ' ERROR, option for dump areas not implemented !!!!')
        2020 format (' Dump areas not used.')
        2030 format (/ ' Number of dump areas :', I5)
        2040 format (' Information on dump areas will only be printed ', &
                'for output option 2 and higher !')
        2050 format (' Composition of dump areas will only be printed ', &
                'for output option 3 and higher !')
        2060 format (/ ' Dump area :', I5, ' : ', A20, &
                / ' Number of segments in area :', I15)
        2070 format ('          number   segment')
        2080 format (2I15)
        2380 format (/, ' ERROR. allocating memory for monitor names:', I8)
        2390 format (/, ' ERROR. allocating memory for monitor segments:', I8)
        2400 format (/, ' ERROR reallocating memory for monitor segments:', I8)
        2410 format (/, ' ERROR. observation ID not unique:', A)
        2420 FORMAT (/, ' ERROR, unrecognised keyword for dump area:', A)
        2430 FORMAT (' Dump area is excluded from mass balance output')
    end subroutine read_monitoring_areas

    subroutine read_monitoring_transects(lun, lchar, filtype, raname, nexcraai, &
            iexcraai, ioptraai, noraai, ntraaq, output_verbose_level, &
            ierr, status)


        !! Reads monitoring transects
        !!
        !!           - Reads an option\n
        !!             This can be:
        !!             - -1 for an external ASCII file, superfluous because of INCLUDE support
        !!             - 1 for continuation on this file
        !!             - 2 to indicate that no monitoring transects are used
        !!           - The amount of monitoring transects
        !!           - Per monitoring transect:
        !!             - the name (20 characters) of the transect
        !!             - the option for transect accumulations. This can be:
        !!               - 1 indicating that the net transport should be taken
        !!               - 2 indicating that only the positive fluxes should be taken
        !!               - 3 indicating that only the negative fluxes should be taken
        !!             - the number of exchanges included in the transect
        !!             - that many exchange numbers (negative if reversed sign is wanted)
        !!
        !!     Subroutine called : process_simulation_input_options   -
        !!                         ZOEK   - to searchs strings
        !!
        !!     Logical units     : LUN(27) = unitnumber stripped DELWAQ input file
        !!                         LUN(29) = unitnumber formatted output file

        use simulation_input_options, only : process_simulation_input_options
        use rd_token     !   for the reading of tokens
        use timers       !   performance timers
        use date_time_utils, only : convert_string_to_time_offset
        use m_waq_precision
        use m_string_utils
        use m_error_status

        integer(kind = int_wp), intent(inout) :: lun     (*)        !< array with unit numbers
        character(*), intent(inout) :: lchar   (*)       !< array with file names of the files
        integer(kind = int_wp), intent(inout) :: filtype (*)        !< type of binary file
        character(20), pointer :: raname  (:)       !< name of monitoring areas
        integer(kind = int_wp), pointer :: nexcraai(:)        !< number of exchanges per monitoring transect
        integer(kind = int_wp), pointer :: iexcraai(:)        !< exchange numbers per monitoring transect
        integer(kind = int_wp), pointer :: ioptraai(:)        !< transport summation option for transects
        integer(kind = int_wp), intent(out) :: noraai             !< number of monitoring transects
        integer(kind = int_wp), intent(out) :: ntraaq             !< total number of exchanges in monitoring transects
        integer(kind = int_wp), intent(in) :: output_verbose_level             !< flag for more or less output
        integer(kind = int_wp), intent(inout) :: ierr               !< error   count

        type(error_status), intent(inout) :: status !< current error status

        integer(kind = int_wp) :: iropt1            ! First option number monitoring areas
        integer(kind = int_wp) :: ierr2             ! Local error flag
        integer(kind = int_wp) :: max_ntraaq        ! Size of isegdmp
        integer(kind = int_wp), pointer :: iexcraai_2(:)     ! Help pointer for array expansion
        integer(kind = int_wp) :: ierr_alloc        ! Error indicator for allocations
        integer(kind = int_wp) :: nq                ! Number of exchanges per monitoring transect
        logical             ldummy                ! Dummy logical
        integer(kind = int_wp) :: ir                ! Loop variable over all monitoring transects
        integer(kind = int_wp) :: k                 ! General loop variable
        character(len = 256) :: option                ! balance option
        integer(kind = int_wp) :: itype             ! type of the returned token
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_monitoring_transects", ithndl)

        ! Read file option
        if (gettoken(iropt1, ierr2) > 0) goto 20
        select case (iropt1)
        case (:-2)
            write (lunut, 2000)  iropt1
            write (lunut, 2010)
            goto 20
        case (-1)                     ! old style <other ASCII file>
            write (lunut, 2000)  iropt1
            call process_simulation_input_options   (iropt1, lun, 0, lchar, filtype, &
                    ldummy, ldummy, 0, ierr2, status, &
                    .false.)
            if (ierr2 > 0) goto 20
            if (gettoken(noraai, ierr2) > 0) goto 20
        case (0)                      ! new style (October 2012) no dump transects
            write (lunut, 2020)       ! old style would have produced an error
            noraai = 0
            ntraaq = 0
            goto 30
        case (1)                      ! old style <this input file> or new style 1 transect
            if (gettoken(option, noraai, itype, ierr2) > 0) goto 20
            if (itype == 1) then     ! character so: new style, 1 transect
                push = .true.
                noraai = iropt1
            else
                write (lunut, 2000)  iropt1
            endif
        case (2)                      ! old style <no dump transects> or new style 2 transects
            if (gettoken(option, noraai, itype, ierr2) > 0) goto 20
            push = .true.                ! look to see what will be next
            if (itype == 1) then     ! a string, so first dump-ID from 2 areas
                call convert_string_to_time_offset (option, noraai, .false., .false., ierr2)
                if (ierr2 /= 0) then
                    noraai = iropt1
                else
                    write (lunut, 2000)  iropt1
                    write (lunut, 2020)
                    noraai = 0
                    ntraaq = 0
                    goto 30
                endif
            else                         ! an integer, so 2 meant <not used>
                write (lunut, 2020)
                noraai = 0
                ntraaq = 0
                goto 30
            endif
        case default                    ! new processing
            noraai = iropt1

        end select

        ! Write number of dump transects, allocate arrays
        write(lunut, 2030) noraai
        ntraaq = 0
        allocate (raname  (noraai), stat = ierr_alloc)
        if (ierr_alloc /= 0) then
            write (lunut, 2380) ierr_alloc
            goto 20
        endif
        allocate (nexcraai(noraai), ioptraai(noraai), &
                iexcraai(noraai * 2), stat = ierr_alloc)
        if (ierr_alloc /= 0) then
            write (lunut, 2390) ierr_alloc
            goto 20
        endif
        max_ntraaq = noraai * 2

        ! Read specification of the transects
        if (output_verbose_level < 2) write (lunut, 2040)
        if (output_verbose_level == 2) write (lunut, 2050)
        do ir = 1, noraai
            if (gettoken(raname  (ir), ierr2) > 0) goto 20
            if (gettoken(ioptraai(ir), ierr2) > 0) goto 20
            if (gettoken(nq, ierr2) > 0) goto 20
            if (ntraaq + nq > max_ntraaq) then
                max_ntraaq = 2 * (ntraaq + nq)
                allocate (iexcraai_2(max_ntraaq), stat = ierr_alloc)
                if (ierr_alloc /= 0) then
                    write (lunut, 2400) ierr_alloc
                    goto 20
                endif
                iexcraai_2(1:ntraaq) = iexcraai(1:ntraaq)
                deallocate(iexcraai)
                iexcraai => iexcraai_2
            endif
            do k = 1, nq
                if (gettoken(iexcraai(ntraaq + k), ierr2) > 0) goto 20
            enddo
            if (raname(ir) == ' ') write(raname(ir), '(''Transect-id'',i6)') ir

            ! check if name is unique

            do k = 1, ir - 1
                if (string_equals(raname(ir), raname(k))) then
                    write(lunut, 2410) raname(ir)
                    ierr = ierr + 1
                endif
            enddo

            if (output_verbose_level >= 2) then
                write(lunut, 2060) ir, raname(ir), ioptraai(ir), nq
                if (output_verbose_level >= 3) then
                    write(lunut, 2070)
                    write(lunut, 2080) (k, iexcraai(ntraaq + k), k = 1, nq)
                endif
            endif

            ntraaq = ntraaq + nq
            nexcraai(ir) = nq

        end do
        goto 30

        ! Error handling
        20 write (lunut, 2500)
        ierr = ierr + 1
        30 if (timon) call timstop(ithndl)
        return

        2000 format (/, ' Transects:', &
                / ' option selected for input :', I2)
        2010 format (/ ' ERROR, option for transects not implemented !!!!')
        2020 format (' No transects used.')
        2030 format (/ ' Number of transects :', I5)
        2040 format (' Information on transects will only be printed ', &
                'for output option 2 and higher !')
        2050 format (' Composition of transects will only be printed ', &
                'for output option 3 and higher !')
        2060 format (/ ' Transect :', I5, ' : ', A20, &
                / ' Option for transport accumulation    :', I15, &
                / ' Number of exchanges for this transect:', I15)
        2070 format ('          number   exchange')
        2080 format (2I15)
        2380 format (/, ' ERROR. allocating memory for transect names:', I8)
        2390 format (/, ' ERROR. allocating memory for transect exch.:', I8)
        2400 format (/, ' ERROR reallocating memory for transect exch.:', I8)
        2410 format (/, ' ERROR. observation ID not unique:', A)
        2500 format (/, ' ERROR. while reading transects')
    end subroutine read_monitoring_transects

    subroutine create_write_monitoring_area_array(lun, ndmpar, ntdmps, noq, noseg, &
            nobnd, ipoint, ntdmpq, ndmpq, ndmps, &
            noraai, ntraaq, nsegdmp, isegdmp, nexcraai, &
            iexcraai, ioptraai, status)

        !! Make and write the monitoring areas and crosssection administrations
        !>
        !>            The routine receives arrays for:
        !>            - the number of volumes of each monitoring area
        !>            - their indices in consecutive order
        !>            - the amount of crosssections of each monitoring transect
        !>            - their indices in consecutive order
        !>            - an option per transect on how to sum flows
        !>            plus the dimensions of these arrays./n
        !>            The routine makes for monitoring areas arrays with:
        !>            - the number of flows involved for each monitoring area
        !>            - the flow numbers in consecutive order, negative if sign needs reversed
        !>            - an array with per flow at what location it should be stored in
        !>              a condensed array with only the required flow information
        !>            - an array with per computational volume at what location it should
        !>              be stored in a condensed array with only the required volume information
        !>            plus the dimensions of these arrays./n
        !>            All arrays are written to the binary intermediate file.
        !!
        !!     Logical units      : lun( 2) = unit unformatted system file
        !!                          lun(29) = unit number output report file

        use timers       !   performance timers

        integer(kind = int_wp), intent(in) :: lun     (*)        !< array with unit numbers
        integer(kind = int_wp), intent(in) :: ndmpar             !< number of dump areas
        integer(kind = int_wp), intent(in) :: ntdmps             !< number of volumes in dump array
        integer(kind = int_wp), intent(in) :: noq                !< total number of exchange
        integer(kind = int_wp), intent(in) :: noseg              !< total number of computation volumes
        integer(kind = int_wp), intent(in) :: nobnd              !< number of open boundaries
        integer(kind = int_wp), intent(in) :: ipoint  (4, noq)    !< exchange pointers
        integer(kind = int_wp), intent(out) :: ntdmpq             !< total number exchanges in dump area
        integer(kind = int_wp), intent(out) :: ndmpq              !< number exchanges dumped
        integer(kind = int_wp), intent(out) :: ndmps              !< number segments dumped
        integer(kind = int_wp), intent(in) :: noraai             !< number of transects
        integer(kind = int_wp), intent(in) :: ntraaq             !< total number of exchanges in transects
        integer(kind = int_wp), intent(in) :: nsegdmp (ndmpar)   !< number of volumes per area
        integer(kind = int_wp), intent(inout) :: isegdmp (ntdmps)   !< volume numbers
        integer(kind = int_wp), intent(in) :: nexcraai(noraai)   !< number of exchanges per transect
        integer(kind = int_wp), intent(in) :: iexcraai(ntraaq)   !< exchange numbers
        integer(kind = int_wp), intent(in) :: ioptraai(ntraaq)   !< exchange accumulation option

        type(error_status), intent(inout) :: status !< current error status

        !     Local variables

        integer(kind = int_wp) :: lurep                        !  output report file
        integer(kind = int_wp) :: itel                         !  counter to run through isegdmp array
        integer(kind = int_wp) :: idump                        !  loop counter monitoring areas
        integer(kind = int_wp) :: iraai                        !  loop counter monitoring transects
        integer(kind = int_wp) :: nsc                          !  number of volumes in that monitoring area
        integer(kind = int_wp) :: nq                           !  number of exchanges in mon. area or transect
        integer(kind = int_wp) :: idmpq                        !  loop counter number of exchanges in mon. area or transect
        integer(kind = int_wp) :: iseg                         !  volume number
        integer(kind = int_wp) :: is                           !  volume number within an area
        integer(kind = int_wp) :: iq                           !  exchange number
        integer(kind = int_wp) :: iqr                          !  exchange number within a transect
        integer(kind = int_wp) :: i                            !  loop counter
        integer(kind = int_wp) :: ips2                         !  help variable to save offset
        integer(kind = int_wp) :: is2                          !  counter within the offset
        integer(kind = int_wp) :: ivan                         !  help variable 'from' volume number
        integer(kind = int_wp) :: inaar                        !  help variable 'to' volume number
        integer(kind = int_wp) :: max_ntdmpq                   !  maximum dimension of the ntdmpq array
        integer(kind = int_wp) :: mxnqseg                      !  maximum exchanges of any segment
        integer(kind = int_wp) :: iqs                          !  loop counter exchanges per segment
        integer(kind = int_wp) :: ierr2                        !  local error variable

        !     Local arrays

        integer(kind = int_wp), allocatable :: iqdmp(:)      !  pointer from exchange nr to condensed array
        integer(kind = int_wp), allocatable :: nqdmp(:)      !  per monitoring area the number of exchanges
        integer(kind = int_wp), allocatable :: isdmp(:)      !  pointer from volume nr to condensed array
        integer(kind = int_wp), pointer :: ipdmpq   (:)  !  array with consecutive interface numbers
        integer(kind = int_wp), pointer :: p2_ipdmpq(:)  !  help pointer to expand the consecutive array
        logical, allocatable :: indmp(:)     !  is the segment in the current area
        integer(kind = int_wp), allocatable :: nqseg(:)      !  number of exchanges per segment
        integer(kind = int_wp), allocatable :: iqseg(:, :)    !  exchanges per segment
        integer(kind = int_wp) :: ithndl = 0
        integer(kind = int_wp) :: ithndl1 = 0
        integer(kind = int_wp) :: ithndl2 = 0

        if (ndmpar == 0 .and. noraai == 0) return
        if (timon) call timstrt("create_write_monitoring_area_array", ithndl)

        allocate(iqdmp(noq))
        allocate(nqdmp(ndmpar))
        allocate(isdmp(noseg))
        allocate(indmp(noseg))

        !     init

        ntdmpq = 0
        ndmpq = 0
        ndmps = 0
        lurep = lun(29)

        !     check segment numbers in dump areas

        itel = 0
        do idump = 1, ndmpar
            nsc = nsegdmp(idump)
            do is = 1, nsc
                itel = itel + 1
                iseg = isegdmp(itel)
                if (iseg == 0) then
                    write (lurep, 2000) idump, is, iseg
                    call status%increase_error_count()
                elseif (iseg < -nobnd) then
                    write (lurep, 2000) idump, is, iseg
                    call status%increase_error_count()
                elseif (iseg < 0) then
                    if (nsc > 1) then
                        write (lurep, 2010) idump, is, iseg
                        call status%increase_error_count()
                    else
                        write (lurep, 2020) idump, is, iseg
                        call status%increase_warning_count()
                    endif
                elseif (iseg > noseg) then
                    write (lurep, 2000) idump, is, iseg
                    call status%increase_error_count()
                    isegdmp(itel) = 0
                endif
            enddo
        enddo

        if (status%ierr > 0) then
            call status%increase_error_count()
            return
        endif

        !     allocate

        !jvb max_ntdmpq = 6 * ndmpar
        max_ntdmpq = noq
        allocate (ipdmpq(max_ntdmpq), stat = ierr2)
        if (ierr2 /= 0) then
            write (lurep, 2030) ierr2, max_ntdmpq
            call status%increase_error_count()
            return
        endif

        ! analyse pointer table

        if (timon) call timstrt("dmpare1", ithndl1)
        allocate(nqseg(noseg))
        nqseg = 0
        do iq = 1, noq
            ivan = ipoint(1, iq)
            inaar = ipoint(2, iq)
            if (ivan  > 0) then
                nqseg(ivan) = nqseg(ivan) + 1
            endif
            if (inaar > 0) then
                nqseg(inaar) = nqseg(inaar) + 1
            endif
        enddo
        mxnqseg = maxval(nqseg)
        allocate(iqseg(mxnqseg, noseg))
        nqseg = 0
        do iq = 1, noq
            ivan = ipoint(1, iq)
            inaar = ipoint(2, iq)
            if (ivan  > 0) then
                nqseg(ivan) = nqseg(ivan) + 1
                iqseg(nqseg(ivan), ivan) = iq
            endif
            if (inaar > 0) then
                nqseg(inaar) = nqseg(inaar) + 1
                iqseg(nqseg(inaar), inaar) = iq
            endif
        enddo
        if (timon) call timstop(ithndl1)

        !     Zero workspace

        iqdmp = 0

        !     Loop over the dump area's

        if (timon) call timstrt("dmpare2", ithndl2)
        itel = 0
        do idump = 1, ndmpar                  ! look for all dumpareas
            nq = 0
            nsc = nsegdmp(idump)
            indmp = .false.
            ips2 = itel
            do is = 1, nsc                         ! set true if volume is in this area
                indmp(isegdmp(ips2 + is)) = .true.
            enddo
            ips2 = itel
            do is = 1, nsc                     ! for all comp. volumes therein
                itel = itel + 1
                iseg = isegdmp(itel)
                do iqs = 1, nqseg(iseg)  ! for all pointers to and from that volume
                    iq = iqseg(iqs, iseg)
                    ivan = ipoint(1, iq)
                    inaar = ipoint(2, iq)
                    if (iseg == ivan) then


                        ! skip if both are in the same dump area

                        if (inaar > 0) then
                            if (indmp(inaar)) goto 110
                        endif

                        !        raise the number of echanges for this dump area

                        nq = nq + 1

                        !        store the exchange number in the area section of this dump area

                        ntdmpq = ntdmpq + 1
                        if (ntdmpq > max_ntdmpq) then    !   extend workarray
                            max_ntdmpq = 2 * max_ntdmpq
                            allocate (p2_ipdmpq(max_ntdmpq), stat = ierr2)
                            if (ierr2 /= 0) then
                                write (lurep, 2040) ierr2, max_ntdmpq
                                call status%increase_error_count()
                                return
                            endif
                            do idmpq = 1, ntdmpq - 1
                                p2_ipdmpq(idmpq) = ipdmpq(idmpq)
                            end do
                            deallocate(ipdmpq)
                            ipdmpq => p2_ipdmpq
                        endif
                        ipdmpq(ntdmpq) = iq

                        !        mark this area to be involved and count areas that are involved

                        if (iqdmp(iq) == 0) then
                            ndmpq = ndmpq + 1
                            iqdmp(iq) = ndmpq
                        endif

                        110             continue

                    elseif (iseg == inaar) then


                        ! skip if both are in the same dump area

                        if (ivan > 0) then
                            if (indmp(ivan)) goto 210
                        endif

                        !        raise the number of echanges for this dump area

                        nq = nq + 1

                        !        store the exchange number in the area section of this dump area

                        ntdmpq = ntdmpq + 1
                        if (ntdmpq > max_ntdmpq) then    !   extend workarray
                            max_ntdmpq = 2 * max_ntdmpq
                            allocate (p2_ipdmpq(max_ntdmpq), stat = ierr2)
                            if (ierr2 /= 0) then
                                write (lurep, 2040) ierr2, max_ntdmpq
                                call status%increase_error_count()
                                return
                            endif
                            do idmpq = 1, ntdmpq - 1
                                p2_ipdmpq(idmpq) = ipdmpq(idmpq)
                            end do
                            deallocate(ipdmpq)
                            ipdmpq => p2_ipdmpq
                        endif
                        ipdmpq(ntdmpq) = -iq            ! negative sign, inward bound

                        !        mark this area to be involved and count areas that are involved

                        if (iqdmp(iq) == 0) then
                            ndmpq = ndmpq + 1
                            iqdmp(iq) = ndmpq
                        endif

                        210             continue

                    endif

                end do
            end do
            nqdmp(idump) = nq
        end do

        deallocate(indmp)
        deallocate(nqseg)
        deallocate(iqseg)

        if (timon) call timstop(ithndl2)

        !     Loop over raaien

        itel = 0
        do iraai = 1, noraai
            nq = nexcraai(iraai)
            do iq = 1, nq
                itel = itel + 1
                iqr = abs(iexcraai(itel))
                if (iqr == 0 .or. iqr > noq) then
                    write (lurep, 2050) iraai, iq, iexcraai(itel)
                    call status%increase_error_count()
                else
                    if (iqdmp(iqr) == 0) then
                        ndmpq = ndmpq + 1
                        iqdmp(iqr) = ndmpq
                    endif
                endif
            end do
        end do

        if (ndmpar > 0) then
            write(lun(2)) (nqdmp  (i), i = 1, ndmpar), (ipdmpq (i), i = 1, ntdmpq)
            write(lun(2)) (nsegdmp(i), i = 1, ndmpar), (isegdmp(i), i = 1, ntdmps)
        endif
        if (noraai > 0) then
            write(lun(2)) (ioptraai(i), i = 1, noraai)
            write(lun(2)) (nexcraai(i), i = 1, noraai)
            write(lun(2)) (iexcraai(i), i = 1, ntraaq)
        endif
        write(lun(2)) (iqdmp(i), i = 1, noq)

        deallocate(ipdmpq)

        ! Set the ISMDP array
        if (ndmpar > 0) then
            isdmp = 0
            do is = 1, ntdmps
                iseg = isegdmp(is)
                if (iseg > 0) then
                    if (isdmp(iseg) == 0) then
                        ndmps = ndmps + 1
                        isdmp(iseg) = ndmps
                    endif
                endif
            enddo
            write(lun(2)) (isdmp(i), i = 1, noseg)
        endif

        if (timon) call timstop(ithndl)
        return

        2000 format (/, ' ERROR segment number in monitoring area/point out of range', &
                /' monitor number  :', I15, &
                /' follow number   :', I15, &
                /' segment number  :', I15)
        2010 format (/, ' ERROR segment number in monitoring area is negative', &
                /' monitor number  :', I15, &
                /' follow number   :', I15, &
                /' boundary number :', I15, &
                /' Negative numbers (boundaries) only allowed as single monitor point')
        2020 format (/, ' INFO segment number in monitoring point is negative, boundary assumed', &
                /' monitor number  :', I15, &
                /' follow number   :', I15, &
                /' boundary number :', I15, &
                /' Output only valid for concentration of active substances')
        2030 format (/, ' ERROR. allocating memory for structure for dump area''s (IPDMPQ):', i4, i10)
        2040 format (/, ' ERROR. allocating memory for structure for dump area''s (P2_IPDMPQ):', i4, i10)
        2050 format (/, ' ERROR exchange in transect out of range.', &
                /' raai number     :', I15, &
                /' follow number   :', I15, &
                /' exchamge number :', I15)

    end subroutine create_write_monitoring_area_array

end module monitoring_areas
