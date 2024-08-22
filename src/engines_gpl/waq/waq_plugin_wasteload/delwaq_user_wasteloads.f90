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

module delwaq_user_wasteloads

    use m_logger_helper, only : stop_with_error
    use m_open_waq_files
    use m_string_utils
    use delwaq_loads, wsl => wasteloads   ! This will be available via the argument list
    use waq_plugin_wasteload_version_module

contains
    subroutine delwaq_user_wasteload (num_waste_loads, wasteloads, num_substances_total, num_substances_transported, num_cells, &
            itime, conc, syname)
        !DEC$ ATTRIBUTES DLLEXPORT::delwaq_user_wasteload

        ! routine to insert user functionality to the wasteloads
        ! contains currently the inlet-outlet coupling functionality

        implicit none

        ! arguments declarations

        integer, intent(in) :: num_waste_loads                  ! number of wasteloads
        type(wasteload), intent(inout) :: wasteloads(:)          ! array of all wasteloads (structure)
        integer, intent(in) :: num_substances_total                  ! total number of substances
        integer, intent(in) :: num_substances_transported                  ! number of active substances
        integer, intent(in) :: num_cells                  ! number of segments
        integer, intent(in) :: itime                  ! system time
        real, intent(in) :: conc(:, :)              ! concentration array
        character(len = *), intent(in) :: syname(:)              ! substance names

        character(len = 120) :: identification_text                  ! waq_plugin_wasteload version number

        ! local declarations

        logical, save :: first = .true.
        integer, save :: lunrep
        integer :: ierr
        integer :: iwst
        integer :: isys

        ! the inlet outlet coupling

        if (first) then
            first = .false.

            call open_waq_files (lunrep, 'delwaq_user_wasteloads.mon', 19, 1, ierr)
            if (ierr /= 0) then
                write(*, '(A)') 'Could not open delwaq_user_wasteloads.mon for writing.'
                call stop_with_error()
            endif

            ! waq_plugin_wasteload version number
            call getfullversionstring_waq_plugin_wasteload(identification_text)
            write (lunrep, *) identification_text
        endif
        ! inlet/outlet pairs - match waste loads by name

        call delwaq_user_inlet_outlet (num_waste_loads, wasteloads, num_substances_total, num_substances_transported, num_cells, &
                itime, conc, syname, lunrep)

        ! walking discharges

        call delwaq_user_walking_discharges (num_waste_loads, wasteloads, num_substances_total, num_substances_transported, num_cells, &
                itime, conc, syname, lunrep)

        ! report on wasteloads

        call delwaq_user_bubble_bath  (num_waste_loads, wasteloads, num_substances_total, num_substances_transported, num_cells, &
                itime, conc, syname, lunrep)

        ! control by D-RTC
        ! Note:
        ! We need to call this routine last (!) to ensure that the control by D-RTC
        ! to turn a waste load partially on or off works also works for outlets.
        ! (General manipulation of inlet/outlet pairs requires further consideration though
        ! as we have a chicken-and-egg problem.)

        call delwaq_user_drtc_control (num_waste_loads, wasteloads, num_substances_total, num_substances_transported, num_cells, &
                itime, conc, syname, lunrep)

        return
    end subroutine delwaq_user_wasteload

    subroutine delwaq_user_bubble_bath  (num_waste_loads, wls, num_substances_total, num_substances_transported, num_cells, &
            itime, conc, syname, lunrep)

        !       routine to set the bubble screen option for Nieuwe Meer
        !                made by Leo Postma at 6 october 2006

        !       global declarations

        implicit none

        !       arguments declarations

        integer, intent(in) :: num_waste_loads             ! number of wasteloads
        type(wasteload), intent(inout) :: wls(:)            ! array of all wasteloads (structure)
        integer, intent(in) :: num_substances_total             ! total number of substances
        integer, intent(in) :: num_substances_transported             ! number of active substances
        integer, intent(in) :: num_cells             ! number of segments
        integer, intent(in) :: itime             ! system time
        real, intent(in) :: conc(:, :)         ! concentration array
        character(len = *), intent(in) :: syname(:)         ! substance names

        !       local declarations

        logical :: first = .true.    ! initialisation indicator
        integer :: lunrep            ! logical unit of report file
        integer :: lunscr            ! logical unit of screen file
        logical :: l_exi             ! file exists or not
        integer :: noscrn            ! number of bubble screens
        integer :: iscrn             ! loop counter screens
        integer :: iwst              ! loop counter loads
        integer :: isub              ! loop counter substances
        real :: wflow             ! flow of a load
        character(len = IDLEN), pointer :: scrnam(:)    ! array with screen names
        integer, pointer :: scrloc(:)    ! array with screen locations
        real, pointer :: scrwtd(:, :)  ! withdrawal mass per substance
        real, pointer :: scrwdf(:)    ! withdrawal flow

        ! Save all local variables

        SAVE

        if (first) then
            first = .false.
            noscrn = 0
            inquire (file = 'screen.dat', exist = l_exi)
            if (l_exi) then
                open  (newunit = lunscr, file = 'screen.dat')        !  read file with
                read  (lunscr, *) noscrn                 !  screen-names
                write (lunrep, *) 'Number of screens:', noscrn
                if (noscrn > 0) then               !  may be more names
                    allocate (scrnam(noscrn))          !  than existing in the
                    do iscrn = 1, noscrn                 !  problem
                        read  (lunscr, *) scrnam(iscrn)
                        write (lunrep, *) 'Screen:', iscrn, ' is called: ', scrnam(iscrn)
                    enddo
                    close (lunscr)
                    allocate (scrloc(num_waste_loads))        !  pointer from waste to screen
                    allocate (scrwtd(noscrn, num_substances_total))
                    allocate (scrwdf(noscrn))
                    scrloc = 0
                    do iwst = 1, num_waste_loads
                        do iscrn = 1, noscrn
                            if (find_string(scrnam(iscrn), wls(iwst)%id%id)) then
                                scrloc(iwst) = iscrn
                                write (lunrep, *) 'Load:', iwst, ' is part of screen:', iscrn
                                exit
                            endif
                        enddo
                    enddo
                endif
            else
                write (lunrep, *) 'No file <screen.dat> detected'
            endif
        endif

        if (noscrn == 0) return

        !        First  step: sum the withdrawn masses and flow per screen

        scrwtd = 0.0                                  !  zero the withdrawal
        scrwdf = 0.0                                  !  accumulation arrays
        do iwst = 1, num_waste_loads
            iscrn = scrloc(iwst)
            if (iscrn /= 0) then                   !  screens only
                wflow = wls(iwst)%flow
                if (wflow < 0.0) then              !  withdrawals only
                    scrwdf(iscrn) = scrwdf(iscrn) + wflow
                    do isub = 1, num_substances_transported
                        scrwtd (iscrn, isub) = scrwtd (iscrn, isub) + &
                                wflow * conc(isub, wls(iwst)%loc%segnr)
                        wls(iwst)%loads(isub) = 0.0      !  for safety
                    enddo
                    do isub = num_substances_transported + 1, num_substances_total
                        scrwtd (iscrn, isub) = 0.0
                        wls(iwst)%loads(isub) = 0.0      !  for safety
                    enddo
                endif
            endif
        enddo

        !        Second step: mix the withdrawal to get concentrations

        do iscrn = 1, noscrn                          !  make the mixed
            wflow = scrwdf(iscrn)                    !  concentrations
            if (wflow /= 0.0) then                 !  per active screen

                do isub = 1, num_substances_total
                    scrwtd (iscrn, isub) = scrwtd (iscrn, isub) / wflow
                enddo
            endif
        enddo

        !        Third  step: set the mixed concentrations for the releases

        do iwst = 1, num_waste_loads
            iscrn = scrloc(iwst)
            if (iscrn /= 0) then                   !  screens only
                wflow = wls(iwst)%flow
                if (wflow > 0.0) then              !  releases only
                    do isub = 1, num_substances_total
                        wls(iwst)%loads(isub) = scrwtd (iscrn, isub)
                    enddo
                endif
            endif
        enddo

        !        NB: in this code it is assumed that also inactive substances are
        !            withdrawn and released like active substances ( so with a flow
        !            concentration that form together the mass )

        return

    end subroutine delwaq_user_bubble_bath

    subroutine delwaq_user_inlet_outlet (num_waste_loads, wasteloads, num_substances_total, num_substances_transported, num_cells, &
            itime, conc, syname, lunrep)

        ! routine to set the default inlet-outlet coupling

        ! global declarations

        implicit none

        ! arguments declarations

        integer, intent(in) :: num_waste_loads                  ! number of wasteloads
        type(wasteload), intent(inout) :: wasteloads(:)          ! array of all wasteloads (structure)
        integer, intent(in) :: num_substances_total                  ! total number of substances
        integer, intent(in) :: num_substances_transported                  ! number of active substances
        integer, intent(in) :: num_cells                  ! number of segments
        integer, intent(in) :: itime                  ! system time
        real, intent(in) :: conc(:, :)              ! concentration array
        character(len = *), intent(in) :: syname(:)              ! substance names

        ! local declarations

        logical, save :: first = .true.                   ! initialisation indicator
        integer :: lunrep                           ! report file
        integer :: luninout                         ! inlet/outlet file
        logical :: l_exi                            ! file exists or not
        integer :: ncomb                            ! number of possible inlet outlet combinations
        integer :: ninout                           ! actual number of inlet outlet combinations
        character(len = 20) :: c_in                             ! read buffer name inlet
        character(len = 20) :: c_out                            ! read buffer name outlet
        character(len = 20), dimension(:), allocatable :: namin  ! names inlet in the possible combinations
        character(len = 20), dimension(:), allocatable :: namout ! names outlet in the possible combinations
        integer, dimension(:), allocatable :: iwin   ! wasteload number inlet of the actual combinations
        integer, dimension(:), allocatable :: iwout  ! wasteload number outlet of the actual combinations
        real :: flow                             ! inlet flow rate
        integer :: ipin                             ! wasteload number inlet
        integer :: ipout                            ! wasteload number outlet
        integer :: iseg                             ! inlet segment number
        integer :: isego                            ! outlet segment number
        integer :: iwst                             ! loop counter wasteloads
        integer :: isys                             ! loop counter substances
        integer :: icomb                            ! loop counter combinations
        integer :: iinout                           ! loop counter of inlet outlet combinations
        integer :: ierr                             ! local I/O error
        integer :: i                                ! loop counter

        ! Save all local variables

        SAVE

        ! test if there are inlet outlet combinations

        if (first) then
            first = .false.
            write(lunrep, *)
            write(lunrep, 2000)

            ! if availeble read list of possible inlet outlet combinations

            inquire (file = 'inloutl.dat', exist = l_exi)
            if (l_exi) then
                write(lunrep, 2004)
                open (newunit = luninout, file = 'inloutl.dat')
                ncomb = 0
                do
                    read (luninout, '(2a20)', iostat = ierr) c_in, c_out
                    if (ierr /= 0) then
                        exit
                    endif

                    ncomb = ncomb + 1
                enddo

                allocate(namin(ncomb), namout(ncomb), iwin(ncomb), iwout(ncomb))

                rewind(luninout)

                do i = 1, ncomb
                    read (luninout, '(2a20)') c_in, c_out
                    namin(i) = c_in
                    namout(i) = c_out
                enddo
                close (luninout)
            else

                ! construct the default list of combination INLETxx/OUTLETxx

                ncomb = max(999, size(wasteloads))

                allocate(namin(ncomb), namout(ncomb), iwin(ncomb), iwout(ncomb))

                write(lunrep, 2006)
                do i = 1, ncomb
                    write(namin(i), '(a,i0)')  'INLET', i
                    write(namout(i), '(a,i0)') 'OUTLET', i
                enddo
            endif

            ! check the actual list of wasteloads with the list of possible combinations

            ninout = 0
            do icomb = 1, ncomb
                ipin = find_wasteload(namin(icomb), wasteloads)
                ipout = find_wasteload(namout(icomb), wasteloads)
                if (ipin > 0 .and. ipout > 0) then

                    ! a combination found, print and set administration

                    write(lunrep, *)
                    write(lunrep, 2001) ipin, wasteloads(ipin)%id%id
                    write(lunrep, 2002) ipout, wasteloads(ipout)%id%id
                    write(lunrep, *)
                    ninout = ninout + 1
                    iwin(ninout) = ipin
                    iwout(ninout) = ipout

                endif
            enddo
            if (ninout == 0) write(lunrep, 2013)
            write(lunrep, 2003)

        endif

        ! set outlet equal to inlet flow * concentration at inlet segment for all found combinations

        do iinout = 1, ninout
            ipin = iwin(iinout)
            ipout = iwout(iinout)
            iseg = wasteloads(ipin)%loc%segnr
            isego = wasteloads(ipout)%loc%segnr
            flow = wasteloads(ipin)%flow
            if (flow <= 0.0) then
                wasteloads(ipout)%flow = 0.0
                do isys = 1, num_substances_transported
                    wasteloads(ipout)%loads(isys) = -flow * conc(isys, iseg)
                enddo
                do isys = num_substances_transported + 1, num_substances_total
                    wasteloads(ipout)%loads(isys) = 0.0
                enddo
            else
                ! Reversed flow - still using the flow rate at the inlet (!)
                wasteloads(ipin)%flow = 0.0
                do isys = 1, num_substances_transported
                    wasteloads(ipin)%loads(isys) = flow * conc(isys, isego)
                enddo
                do isys = num_substances_transported + 1, num_substances_total
                    wasteloads(ipin)%loads(isys) = 0.0
                enddo
            endif
        enddo
        !
        return
        2000 format (' extra functionality INLET/OUTLET')
        2001 format ('    waste number:', i5, ' name:', a20, ' (INLET) coupled to')
        2002 format ('    waste number:', i5, ' name:', a20, ' (OUTLET)')
        2003 format (' end extra functionality INLET/OUTLET')
        2004 format ('    possible INLET/OUTLET combinations will be read from ', &
                'file <inloutl.dat>')
        2006 format ('    no file <inloutl.dat> using default combinations names')
        2013 format ('    no INLET/OUTLET combination found')

    end subroutine delwaq_user_inlet_outlet

    function find_wasteload(waste_id, wasteloads) result (iwst)

        ! function to find a wasteload on id in an array of wasteloads

        character(len = *) :: waste_id               ! wasteload id to be found
        type(wasteload) :: wasteloads(:)          ! array of all wasteloads (structure)
        integer :: iwst                   ! on return if found wasteload number, otherwise zero

        ! local declarations

        character(len = 20) :: name                   ! fixed length copy of waste_id
        integer :: num_waste_loads                  ! length of wasteloads array
        integer :: i                      ! loop counter

        ! loop over the wasteloads and compare id with delwaq routine zoek

        num_waste_loads = size(wasteloads)
        name = waste_id
        iwst = 0
        do i = 1, num_waste_loads
            if (string_equals(name, wasteloads(i)%id%id)) then
                iwst = i
                return
            endif
        end do

    end function find_wasteload

    function find_substance(substance_id, syname) result (isys)

        ! function to find a substance on id

        character(len = *) :: substance_id           ! substance id to be found
        character(len = 20) :: syname(:)              ! substance names
        integer :: isys                   ! on return if found substance number, otherwise zero

        ! call the delwaq routine zoek


        isys = index_in_array(substance_id, syname)

    end function find_substance

    function find_string(string, test) result (found)

        !            function to find a string in a test-string

        character(*) string
        character(*) test
        logical      found

        !            local declarations

        integer      lens, lent, i

        found = .true.
        lens = len_trim(string) - 1
        lent = len_trim(test)
        do i = 1, lent - lens
            if (string == test(i:i + lens)) return
        enddo
        found = .false.

    end function find_string

    integer function get_number_of_layers()
        ! gets the number of layers (from m_waq_memory_dimensions)

        use m_waq_memory_dimensions

        get_number_of_layers = num_layers

    end function get_number_of_layers

    subroutine delwaq_user_walking_discharges (num_waste_loads, wasteloads, num_substances_total, num_substances_transported, num_cells, &
            itime, conc, syname, lunrep)

        ! routine to handle walking discharges

        ! global declarations

        implicit none

        ! arguments declarations

        integer, intent(in) :: num_waste_loads                  ! number of wasteloads
        type(wasteload), intent(inout) :: wasteloads(:)          ! array of all wasteloads (structure)
        integer, intent(in) :: num_substances_total                  ! total number of substances
        integer, intent(in) :: num_substances_transported                  ! number of active substances
        integer, intent(in) :: num_cells                  ! number of segments
        integer, intent(in) :: itime                  ! system time
        real, intent(in) :: conc(:, :)              ! concentration array
        character(len = *), intent(in) :: syname(:)              ! substance names

        ! local variables

        logical, save :: first = .true.
        integer, save :: nowalk                 !< number of walking discharges
        integer, save :: next_time_in_file      !< next time to read the locations
        integer, save :: time_offset            !< time offset because of rewinding
        integer, save :: timestep               !< timestep, anticipate next time
        integer, save :: period                 !< period covered in the file
        integer, save :: nosegl                 !< number of segments per layer
        integer, save :: num_layers                  !< number of layers
        integer, dimension(:, :), allocatable, save :: lgrid           !< matrix with segment numbers

        integer :: newsegment
        integer :: i
        integer :: id
        integer :: ierr
        integer :: lunrep, lunlga
        integer, save :: lunwlk
        integer :: ix, iy, iz, jz, offset
        integer :: num_columns, num_rows, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir
        logical :: l_exi

        ! test if there are any walking discharges

        if (first) then
            first = .false.
            nowalk = 0

            inquire (file = 'walking.dat', exist = l_exi)
            if (l_exi) then
                write(lunrep, *)
                write(lunrep, 2000)
                write(lunrep, 2001)

                open(newunit = lunwlk, file = 'walking.dat')
                read(lunwlk, *) nowalk
                if (nowalk > 0) then
                    open(newunit = lunlga, file = 'walking.lga', access = 'stream', status = 'old')
                    read(lunlga) num_columns, num_rows, nosegl, num_layers, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir

                    ! check if the grids match

                    if (mod(num_cells, nosegl) /= 0) then
                        write(lunrep, 2002) num_cells, nosegl
                        nowalk = 0
                        close(lunlga)
                        close(lunwlk)
                        return
                    endif

                    num_layers = get_number_of_layers()

                    allocate(lgrid(num_columns, num_rows))
                    read(lunlga) lgrid
                    close(lunlga)
                endif
            else
                write(lunrep, 2005)
                return
            endif

            offset = 0
            do i = 1, nowalk
                read(lunwlk, *, iostat = ierr) id, ix, iy, iz
                if (id > 0 .and. id + offset <= num_waste_loads) then
                    if (iz > 0) then
                        newsegment = lgrid(ix, iy) + nosegl * (iz - 1)
                        wasteloads(id + offset)%loc%segnr = newsegment
                    else
                        do jz = 1, num_layers
                            newsegment = lgrid(ix, iy) + nosegl * (jz - 1)
                            wasteloads(id + offset)%loc%segnr = newsegment
                            offset = offset + 1
                        enddo
                    endif
                endif
                write(lunrep, *) id, ix, iy, iz
            enddo
            call reposition_file(lunwlk)

            call determine_times(lunwlk, nowalk, next_time_in_file, period, timestep)
            time_offset = 0

            write(lunrep, 2006)
        endif

        ! do not bother with this if there are no walking discharges
        if (nowalk == 0) then
            return
        endif

        write(lunrep, *) 'Time in file: ', next_time_in_file, itime, nowalk

        ! position the file pointer and read the information
        do
            if (next_time_in_file <= itime) then
                offset = 0
                do i = 1, nowalk
                    read(lunwlk, *, iostat = ierr) id, ix, iy, iz
                    if (id > 0 .and. id + offset <= num_waste_loads) then
                        if (iz > 0) then
                            newsegment = lgrid(ix, iy) + nosegl * (iz - 1)
                            wasteloads(id + offset)%loc%segnr = newsegment
                        else
                            do jz = 1, num_layers
                                newsegment = lgrid(ix, iy) + nosegl * (jz - 1)
                                wasteloads(id + offset)%loc%segnr = newsegment
                                offset = offset + 1
                            enddo
                        endif
                    endif
                    write(lunrep, *) id, ix, iy, iz
                enddo

                read(lunwlk, *, iostat = ierr) next_time_in_file
                if (ierr /= 0) then
                    call reposition_file(lunwlk)
                    time_offset = time_offset + period
                endif

                next_time_in_file = next_time_in_file + time_offset

                if (next_time_in_file + timestep > itime) then
                    exit
                endif
            else
                exit
            endif
        enddo

        2000 format (' extra functionality WALKING DISCHARGES')
        2001 format ('    walking discharges file found - walking.dat')
        2002 format ('    grid mismatch in LGA file (walking.lga): ', /, &
                '    number of segments', i10, &
                ' not a multiple of number of segments per layer:', i10)
        2005 format (' No file <walking.dat> detected')
        2006 format (' end extra functionality WALKING DISCHARGES')

    contains

        subroutine determine_times(lunwlk, nowalk, start_time, period, timestep)
            !
            ! Scan the file to determine the start time and the period
            ! Then reposition the pointer
            !
            integer :: lunwlk, nowalk, start_time, period, timestep

            integer :: i, next_time, dummy
            integer :: ierr

            read(lunwlk, *) start_time

            ! Skip the second block
            do i = 1, nowalk
                read(lunwlk, *) dummy, dummy, dummy, dummy
            enddo

            read(lunwlk, *) next_time
            timestep = next_time - start_time

            ! Read until the end of the file
            do
                do i = 1, nowalk
                    read(lunwlk, *, iostat = ierr) dummy, dummy, dummy, dummy
                    if (ierr /= 0) then
                        write(lunrep, 2004) next_time
                        call stop_with_error()
                    endif
                enddo

                read(lunwlk, *, iostat = ierr) next_time
                if (ierr /= 0) then
                    exit
                endif
            enddo

            period = next_time + timestep - start_time

            ! Reposition the file
            call reposition_file(lunwlk)

            2004 format ('   Unexpected end of file with walking discharges at tim &
                    e = ', i10)
        end subroutine determine_times

        subroutine reposition_file(lunwlk)

            integer, intent(in) :: lunwlk

            integer :: i, dummy, nolines

            rewind(lunwlk)
            read(lunwlk, *) nolines
            do i = 1, nolines
                read(lunwlk, *) dummy, dummy, dummy
            enddo

            read(lunwlk, *) dummy ! The first time - we already know that!

        end subroutine reposition_file

    end subroutine delwaq_user_walking_discharges

    subroutine delwaq_user_drtc_control (num_waste_loads, wls, num_substances_total, num_substances_transported, num_cells, &
            itime, conc, syname, lunrep)

        !       routine to allow D-RTC to control the waste load
        !       Note:
        !       As we do not know if D-RTC is part o fhte calculation or has set anything,
        !       we cannot rely on the values to be anything useful. This means:
        !       * The default value of the scale factor is part of the derived type definition.
        !       * If D-RTC sets nothing, the factor is 1, so there is no net effect.
        !

        !       global declarations

        implicit none

        !       arguments declarations

        integer, intent(in) :: num_waste_loads             ! number of wasteloads
        type(wasteload), intent(inout) :: wls(:)            ! array of all wasteloads (structure)
        integer, intent(in) :: num_substances_total             ! total number of substances
        integer, intent(in) :: num_substances_transported             ! number of active substances
        integer, intent(in) :: num_cells             ! number of segments
        integer, intent(in) :: itime             ! system time
        real, intent(in) :: conc(:, :)         ! concentration array
        character(len = *), intent(in) :: syname(:)         ! substance names
        integer, intent(in) :: lunrep            ! logical unit of report file

        !       local declarations

        integer :: i, n

        do i = 1, num_waste_loads
            !
            ! Set the flow or a specific substance
            ! (Note that index 1 is for the extra quantity "flow")
            !
            n = size(wls(i)%loads) - 1 ! Compensate error in wascal

            if (allocated(wls(i)%set_factor)) then
                if (wls(i)%set_factor(1) /= 0.0) then
                    wls(i)%flow = wls(i)%flow * wls(i)%set_factor(1)
                    wls(i)%loads(1:n) = wls(i)%loads(1:n) * wls(i)%set_factor(2:n + 1)
                else
                    wls(i)%flow = 1.0e-20   ! Avoid zero, because then the waste load magic kicks in
                    wls(i)%loads(:) = 0.0       ! Set the concentrations in the waste load to zero, so
                endif
            endif
        enddo

    end subroutine delwaq_user_drtc_control

end module delwaq_user_wasteloads
