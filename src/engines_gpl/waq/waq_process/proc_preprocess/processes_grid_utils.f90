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
module m_set_grid_all_processes
    use m_waq_precision
    use m_string_utils

    implicit none

    private
    public :: set_grid_all_processes

contains


    subroutine set_grid_all_processes(procesdef, num_grids, num_substances, grdref, sysgrd, sysndt)
        !! set grid for all processes

        use m_array_manipulation, only : resize_integer_array
        use processet
        use timers
        use math_utils, only : greatest_common_divisor

        type(procespropcoll) :: procesdef       ! all processes
        integer(kind = int_wp) :: num_grids          ! number of grids
        integer(kind = int_wp) :: num_substances           ! number of substances
        integer(kind = int_wp) :: grdref(num_grids)  ! reference grid number
        integer(kind = int_wp) :: sysgrd(num_substances)   ! grid number substances
        integer(kind = int_wp) :: sysndt(num_substances)   ! timestep multiplier substances

        ! local decalarations
        integer(kind = int_wp) :: nproc           ! number of processes
        integer(kind = int_wp) :: iproc           ! loop counter processes
        integer(kind = int_wp) :: iproc2          ! loop counter processes
        type(procesprop), pointer :: proc            ! process description
        type(procesprop), pointer :: proc2           ! process description
        integer(kind = int_wp) :: isys            ! index substance
        integer(kind = int_wp) :: i               ! loop index
        integer(kind = int_wp) :: ipgrid          ! index grid
        integer(kind = int_wp) :: istochi         ! index stochi
        integer(kind = int_wp) :: i_input         ! index input
        integer(kind = int_wp) :: ioutput         ! index output
        integer(kind = int_wp) :: imnoag          ! index routine
        integer(kind = int_wp) :: nmnoag          ! number of routines whic may not be aggregated
        integer(kind = int_wp), parameter :: mxnoag = 1000   ! dimension for local array
        character(len = 10) :: monoag(mxnoag)  ! list of routines which may not be aggregated
        integer(kind = int_wp) :: maxwrk          ! dimension for local array
        integer(kind = int_wp) :: nototg          !
        integer(kind = int_wp) :: maxndt          ! max timestep multiplier
        integer(kind = int_wp) :: ndt             ! timestep multiplier
        integer(kind = int_wp) :: nndt            ! number of ndt
        character(len = 20) :: valnam          ! name
        integer(kind = int_wp), allocatable :: grpath(:)       !
        integer(kind = int_wp), pointer :: grdwrk(:)       !
        logical :: lexi
        logical :: l_exchange      ! in or output on exchanges
        integer(kind = int_wp), allocatable :: isysto(:)       ! temp, copy of the substances in the fluxstochi
        integer(kind = int_wp) :: file_unit
        integer(kind = int_wp) :: ierr
        integer(kind = int_wp) :: ithndl = 0

        if (timon) call timstrt("set_grid_all_processes", ithndl)

        allocate(grpath(num_grids))
        maxwrk = 1000
        allocate(grdwrk(maxwrk))

        ! read list processes not to be aggragated
        monoag(1) = 'TOTDEPTH'
        monoag(2) = 'STADAY'
        monoag(3) = 'STADPT'
        monoag(4) = 'STADSC'
        monoag(5) = 'STAGEO'
        monoag(6) = 'STAPRC'
        monoag(7) = 'STAQTL'
        nmnoag = 7

        inquire (file = 'procnoag.dat', exist = lexi)
        if (lexi) then
            open(newunit = file_unit, file = 'procnoag.dat')
            do
                nmnoag = nmnoag + 1
                read(file_unit, *, iostat = ierr) monoag(nmnoag)
                if (ierr /= 0) then
                    exit
                endif
            enddo
            nmnoag = nmnoag - 1
            close (file_unit)
        endif

        ! first step, processes with fluxes set to the grid set for the
        ! substances. for processes with exchange io or in the list with
        ! processes that can not be aggregated always use base grid.
        ! what to do with specials like clcrad that use the pointer
        ! table without using exchange io?

        nproc = procesdef%current_size
        do iproc = 1, nproc
            proc => procesdef%procesprops(iproc)
            if (proc%active) then

                ! check for in and output on exchange
                l_exchange = .false.
                do i_input = 1, proc%no_input
                    if (proc%input_item(i_input)%type == IOTYPE_EXCHANG_INPUT) then
                        l_exchange = .true.
                    endif
                enddo
                do ioutput = 1, proc%no_output
                    if (proc%output_item(ioutput)%type == IOTYPE_EXCHANG_OUTPUT) then
                        l_exchange = .true.
                    endif
                enddo

                imnoag = index_in_array(proc%routine(:10), monoag(:nmnoag))
                if (imnoag <= 0) then
                    imnoag = index_in_array(proc%name(:10), monoag(:nmnoag))
                endif
                if (imnoag > 0) then
                    proc%grid = 1
                elseif (l_exchange) then
                    proc%grid = 1
                else
                    nototg = proc%no_fluxstochi
                    if (nototg > 0) then
                        allocate(isysto(nototg))
                        do i = 1, nototg
                            isysto(i) = proc%fluxstochi(i)%subindx
                        enddo
                        call set_process_aggregated_grid(num_grids, num_substances, nototg, grdref, sysgrd, isysto, &
                                grpath, ipgrid)
                        deallocate(isysto)

                        if (ipgrid < 1) then
                            ipgrid = 1
                        endif
                        proc%grid = ipgrid
                    else
                        ! no substance, highest grid
                        proc%grid = num_grids
                    endif
                endif
            endif
        enddo

        do iproc = nproc, 1, -1
            proc => procesdef%procesprops(iproc)
            if (proc%active) then
                ipgrid = proc%grid
                if (ipgrid /= 1) then
                    nototg = 1
                    grdwrk(1) = ipgrid
                    do ioutput = 1, proc%no_output

                        ! check how the output is used
                        valnam = proc%output_item(ioutput)%name
                        do iproc2 = iproc + 1, nproc
                            proc2 => procesdef%procesprops(iproc2)
                            if (proc2%active) then
                                call zoekio (valnam, proc2%no_input, proc2%input_item, 20, i_input)
                                if (i_input > 0) then
                                    nototg = nototg + 1
                                    if (nototg > maxwrk) then
                                        maxwrk = maxwrk * 2
                                        call resize_integer_array(grdwrk, maxwrk, nototg - 1)
                                    endif
                                    grdwrk(nototg) = proc2%grid
                                endif
                            endif
                        enddo
                    enddo
                    call setgr2(num_grids, nototg, grdref, grdwrk, grpath, ipgrid)

                    if (ipgrid < 1) then
                        ipgrid = 1
                    endif
                    proc%grid = ipgrid
                endif
            endif
        enddo

        ! now the steps in the fractional step.


        ! set largest step

        maxndt = 1
        do isys = 1, num_substances
            maxndt = max(sysndt(isys), maxndt)
        enddo

        ! set largest step for processes with stochi's

        do iproc = 1, nproc
            proc => procesdef%procesprops(iproc)
            if (proc%active) then
                imnoag = index_in_array(proc%routine(:10), monoag(:nmnoag))
                if (imnoag <= 0) then
                    imnoag = index_in_array(proc%name(:10), monoag(:nmnoag))
                endif
                if (imnoag > 0) then
                    proc%ndt = 1
                else
                    nndt = 0
                    do istochi = 1, proc%no_fluxstochi
                        if (proc%fluxstochi(istochi)%subindx > 0) then
                            nndt = nndt + 1
                            if (nndt > maxwrk) then
                                maxwrk = maxwrk * 2
                                call resize_integer_array(grdwrk, maxwrk, nndt - 1)
                            endif
                            grdwrk(nndt) = sysndt(proc%fluxstochi(istochi)%subindx)
                        endif
                    enddo
                    if (nndt > 0) then
                        call greatest_common_divisor (nndt, grdwrk, ndt)
                        proc%ndt = ndt
                    else

                        ! no substance, largest step

                        proc%ndt = maxndt

                    endif
                endif
            endif
        enddo

        ! rest of the processes, check use then set timestep

        do iproc = nproc, 1, -1
            proc => procesdef%procesprops(iproc)
            if (proc%active) then
                ndt = proc%ndt
                if (ndt /= 1) then
                    nndt = 1
                    grdwrk(nndt) = ndt
                    do ioutput = 1, proc%no_output

                        ! check how the output is used

                        valnam = proc%output_item(ioutput)%name
                        do iproc2 = iproc + 1, nproc
                            proc2 => procesdef%procesprops(iproc2)
                            if (proc2%active) then
                                call zoekio (valnam, proc2%no_input, proc2%input_item, 20, i_input)
                                if (i_input > 0) then
                                    nndt = nndt + 1
                                    if (nndt > maxwrk) then
                                        maxwrk = maxwrk * 2
                                        call resize_integer_array(grdwrk, maxwrk, nndt - 1)
                                    endif
                                    grdwrk(nndt) = proc2%ndt
                                endif
                            endif
                        enddo
                    enddo
                    call greatest_common_divisor (nndt, grdwrk, ndt)
                    proc%ndt = ndt
                endif
            endif
        enddo

        deallocate(grpath)
        deallocate(grdwrk)

        if (timon) call timstop(ithndl)

    end subroutine set_grid_all_processes

    subroutine set_process_aggregated_grid(num_grids, num_substances, num_grid_substances, grdref, sysgrd, &
            prosys, grpath, ipgrid)
        !! sets most aggregated grid possible for a process taken into acount the grid for each substance.
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     num_grids  INTEGER       1     INPUT   Number of grids
        !     num_substances   INTEGER       1     INPUT   Number of substances
        !     num_grid_substances  INTEGER       1     INPUT   Number of substances for this grid
        !     GRDREF  INTEGER    num_grids   INPUT   Reference grid number
        !     SYSGRD  INTEGER    num_substances    INPUT   Grid number substance
        !     PROSYS  INTEGER    num_grid_substances   INPUT   Substance numbers for this process
        !     GRPATH  INTEGER    num_grids   LOCAL   Reference path to base grid
        !     IPGRID  INTEGER       1     OUTPUT  Grid number set for this process

        use timers       !   performance timers

        integer(kind = int_wp) :: num_grids, num_substances, num_grid_substances, ipgrid
        integer(kind = int_wp) :: grdref(num_grids), sysgrd(num_substances), prosys(num_grid_substances), &
                grpath(num_grids)

        integer(kind = int_wp) :: npath, ipath, igrid, isys, isys1, igsys, ncheck
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("set_process_aggregated_grid", ithndl)

        ! check number of substances for this grid
        if (num_grid_substances < 1) then
            ipgrid = -1
            goto 9999
        endif

        ! get first valid substance
        isys1 = 0
        do isys = 1, num_grid_substances
            if (prosys(isys) > 0) then
                isys1 = isys
                goto 5
            endif
        enddo
        5 continue
        if (isys1 < 1) then
            ipgrid = -1
            goto 9999
        endif

        ! count length of path for first substance
        ipgrid = sysgrd(prosys(isys1))
        igrid = ipgrid
        npath = 1
        10 continue
        if (igrid /= 1)  then
            igrid = grdref(igrid)
            if (igrid <= 0) then

                ! not defined on reference grid
                ipgrid = -2
                goto 9999

            endif
            npath = npath + 1
            if (npath > num_grids) then

                ! base grid not found in reference
                ipgrid = -2
                goto 9999
            endif
            goto 10
        endif

        ! set path for first substance
        grpath(npath) = sysgrd(prosys(isys1))
        do ipath = npath - 1, 1, -1
            grpath(ipath) = grdref(grpath(ipath + 1))
        enddo

        ! for next substances check where the reference comes together
        do igsys = isys1 + 1, num_grid_substances
            if (prosys(igsys) > 0) then
                igrid = sysgrd(prosys(igsys))
                ncheck = 1
                40       continue

                ! check path previously found
                do ipath = npath, 1, -1
                    if (grpath(ipath) == igrid) then
                        ipgrid = igrid
                        npath = ipath
                        goto 50
                    endif
                enddo
                ncheck = ncheck + 1
                if (ncheck > num_grids) then
                    ipgrid = -2
                    goto 9999
                endif
                igrid = grdref(igrid)
                goto 40
                50       continue
            endif
        enddo

        9999 if (timon) call timstop(ithndl)
    end subroutine set_process_aggregated_grid

    subroutine setgr2(num_grids, num_substances, grdref, prosys, grpath, ipgrid)
        !! sets most aggregated grid possible for a process taken into a list of grids.
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     num_grids  INTEGER       1     INPUT   Number of grids
        !     num_substances  INTEGER       1     INPUT   Number of substances for this grid
        !     GRDREF  INTEGER    num_grids   INPUT   Reference grid number
        !     PROSYS  INTEGER    num_substances   INPUT   Substance numbers for this process
        !     GRPATH  INTEGER    num_grids   LOCAL   Reference path to base grid
        !     IPGRID  INTEGER       1     OUTPUT  Grid number set for this process

        use timers       !   performance timers

        integer(kind = int_wp) :: num_grids, num_substances, ipgrid
        integer(kind = int_wp) :: grdref(num_grids), prosys(num_substances), grpath(num_grids)
        integer(kind = int_wp) :: npath, ipath, igrid, igsys, ncheck

        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("setgr2", ithndl)

        ! check number of substances for this grid
        if (num_substances < 1) then
            ipgrid = -1
            goto 9999
        endif

        ! count length of path for first grid in list
        ipgrid = prosys(1)
        igrid = ipgrid
        npath = 1
        10 continue
        if (igrid /= 1)  then
            igrid = grdref(igrid)
            if (igrid <= 0) then
                ! not defined on reference grid
                ipgrid = -2
                goto 9999
            endif
            npath = npath + 1
            if (npath > num_grids) then

                ! base grid not found in reference
                ipgrid = -2
                goto 9999
            endif
            goto 10
        endif
        ! set path for first grid in list
        grpath(npath) = prosys(1)
        do ipath = npath - 1, 1, -1
            grpath(ipath) = grdref(grpath(ipath + 1))
        enddo

        ! for next grids in list check where the reference comes together
        do igsys = 2, num_substances
            igrid = prosys(igsys)
            ncheck = 1
            40    continue

            ! check path previously found
            do ipath = npath, 1, -1
                if (grpath(ipath) == igrid) then
                    ipgrid = igrid
                    npath = ipath
                    goto 50
                endif
            enddo
            ncheck = ncheck + 1
            if (ncheck > num_grids) then
                ipgrid = -2
                goto 9999
            endif
            igrid = grdref(igrid)
            goto 40
            50    continue

        enddo

        9999 if (timon) call timstop(ithndl)

    end subroutine setgr2

end module m_set_grid_all_processes
