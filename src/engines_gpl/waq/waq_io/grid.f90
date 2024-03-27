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


module m_waq_grid

    use m_waq_precision
    use m_read_sub_procgrid
    use m_read_proc_time
    use m_read_nobottomlay
    use m_read_grid
    use m_error_status

    implicit none

contains


    subroutine grid   (lun, noseg, notot, nototp, nolay, &
            gridps, nseg2, nogrid, syname, status)

        !       Deltares Software Centre

        !>\file
        !>             Reads optional multiple grids
        !>
        !>             Routine is called with ierr = -1 if only the base grid needs defined.
        !>             The following used input can be supplied:
        !>             - an integer. This is the number of additional grids in old input
        !>               processing. In new input processing the software counts the number
        !>               of additional grid specifications.
        !>             - ZMODEL. A Zlayer model is used, relevance unknown.
        !>             - NOLAY followed by an integer, specifies number of layers in base grid.
        !>               May be redundant because this can be done at several locations.
        !>             - BOTTOMGRID. Specifies grid in waterbed. Diverts to read_grid.
        !>             - BEDGRID. Seems to be the right name for BOTTOMGRID.
        !>             - PROCESSGRID. Specifies grid for processes. Diverts to read_grid.


        !     Created        : Past century by Jan van Beek
        !     Modified       : October 2002    Leo Postma    for Layered Bed in Delwaq
        !                      ......  200.    Jan van Beek  creation of Layered Bed special
        !                      May     2011    Leo Postma    merged with Layered Bed special

        !     Logical units  : LUN(29) = unit formatted output file
        !                      LUN( 2) = unit intermediate file (system)

        use dlwqgrid_mod        !   for the storage of contraction grids
        use rd_token     !   for the reading of tokens
        use timers       !   performance timers

        implicit none

        !     Parameters    :

        !     kind           function         name                Descriptipon

        integer(kind = int_wp), intent(inout) :: lun   (*)          !< array with unit numbers
        integer(kind = int_wp), intent(in) :: noseg              !< number of computational volumes
        integer(kind = int_wp), intent(in) :: notot              !< total number of substances
        integer(kind = int_wp), intent(in) :: nototp             !< total number of particle-substances
        integer(kind = int_wp), intent(out) :: nolay              !< number of layers
        type(GridPointerColl) :: GridPs            !< collection of grids
        integer(kind = int_wp), intent(out) :: nseg2              !< number of additional bottom volumes
        integer(kind = int_wp), intent(out) :: nogrid             !< number of grids
        character(20), intent(in) :: syname(notot)     !< names of the substances

        type(error_status), intent(inout) :: status !< current error status

        !     Local variables :

        logical :: read_input    ! is input expected?
        logical :: newinput      ! is it the newer type of grid input ?
        logical :: multigrid     ! is the multiple grid feature used ?
        type(GridPointer) :: aGrid         ! a single grid
        character*255 :: ctoken        ! the character token that is read
        integer(kind = int_wp) :: itoken         ! the integer token that is read
        integer(kind = int_wp) :: isysg(notot)   ! grid number to be used per substance
        integer(kind = int_wp) :: isyst(notot)   ! time step multiplier per substance
        integer(kind = int_wp) :: iseg           ! loop counter computational volumes
        integer(kind = int_wp) :: i_base_grid    ! the system base grid number (mostly 1)
        integer(kind = int_wp) :: i_bottom_grid  ! the system bed grid number
        logical :: zmodel        ! if true, it is a z-layer model
        integer(kind = int_wp) :: itype          ! returned type of input token
        integer(kind = int_wp) :: ierr2          ! error count in this routine
        integer(kind = int_wp) :: nosegl         ! number of computational volumes per layer
        integer(kind = int_wp) :: nolay_tmp      ! temp number of layers
        integer(kind = int_wp) :: nosegl_bottom  ! number of segments per layer (required in read_grid)
        integer(kind = int_wp) :: igrid          ! a grid number in the collection
        integer(kind = int_wp) :: noseg2         ! number of additional bed cells
        integer(kind = int_wp) :: kseg           ! volume number counter
        integer(kind = int_wp) :: ilay           ! layer counter
        integer(kind = int_wp) :: isys           ! substance number
        integer(kind = int_wp) :: nosss          ! total number of volumes inclusive bed cells
        integer(kind = int_wp) :: ioff           ! help variable for array offset
        integer(kind = int_wp) :: iseg2          ! counter for bed cells
        integer(kind = int_wp) :: iref           ! help variable to contain the reference grid
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("grid", ithndl)

        nolay = 1
        nosegl = noseg / nolay
        nogrid = 1
        newinput = .false.
        zmodel = .false.
        read_input = .false.
        multigrid = .false.
        nosegl_bottom = nosegl

        !       Always add base grid to the collection of grids

        aGrid%name = 'Base grid'
        aGrid%noseg = noseg
        aGrid%noseg_lay = noseg / nolay
        aGrid%iref = 1
        aGrid%name_ref = ' '
        aGrid%itype = BaseGrid
        aGrid%space_var_nolay = .FALSE.
        aGrid%nolay = nolay
        aGrid%nolay_var => null()
        allocate (aGrid%iarray(noseg))
        do iseg = 1, noseg
            agrid%iarray(iseg) = iseg
        enddo
        i_base_grid = GridPointerCollAdd(GridPs, aGrid)
        i_bottom_grid = 0
        GridPs%base_grid = i_base_grid
        GridPs%bottom_grid = i_bottom_grid

        isysg = i_base_grid
        isyst = 1

        !       Read number of multiple grids

        do

            if (gettoken(ctoken, itoken, itype, ierr2) > 0) goto 1000

            if (itype == 2) then                    ! integer
                if (.not. read_input) then             ! no multigrid, integer
                    write (lunut, 2000)                ! is meant for print-out
                    push = .true.                         ! grid, so push on the
                    exit                                  ! stack again
                else
                    if (.not. newinput) then            ! old way of dealing with
                        if (nogrid == 1) then          ! multiple grids expects
                            nogrid = itoken + 1             ! the number of added grids
                            write (lunut, 2010) nogrid   ! after the multigrid keyword
                            if (nogrid == 1) exit       ! no additional grids
                        else
                            if (nogrid == gridps%cursize) then
                                do isys = 1, notot - nototp
                                    if (gettoken(isysg(isys), ierr2) > 0) goto 1000
                                    if (gettoken(isyst(isys), ierr2) > 0) goto 1000
                                enddo
                                do isys = notot - nototp + 1, notot
                                    isysg(isys) = 1
                                    isyst(isys) = 1
                                enddo
                                exit
                            endif
                        endif
                    else
                        push = .true.                      ! an integer may end the
                        exit                               ! input processing but it
                    endif                                 ! should be available later on.
                    cycle
                endif
            endif

            if (.not. multigrid) then                 ! still no multigrid chosen
                select case (ctoken)                   ! NOLAY necessary here
                case ('NOLAY')                      ! Deal with number of layers
                    if (gettoken(nolay, ierr2) > 0) goto 1000
                    write (lunut, 2020) nolay
                case ('MULTIGRID')                  ! Deal with multiple grids
                    multigrid = .true.                ! Allow an integer to give
                    read_input = .true.                ! number of additional grids
                    if (gettoken(ctoken, itoken, itype, ierr2) > 0) goto 1000
                    push = .true.
                    if (itype == 1) newinput = .true.
                    write (lunut, 2040)
                case default
                    write (lunut, 2030) trim(ctoken)
                    goto 1000
                end select
                cycle
            endif

            select case (ctoken)

            case ('ZMODEL')
                newinput = .true.
                zmodel = .true.

            case ('NOLAY')
                ! nolay must precede grid definitions
                if (GridPs%cursize > 1) then
                    write(lunut, 2050)
                    goto 1000
                endif
                if (gettoken(nolay_tmp, ierr2) > 0) goto 1000
                write (lunut, 2020) nolay_tmp

                ! z model temp do not do this here but at the end of the routine

                nosegl_bottom = noseg / nolay_tmp
                if (.not. zmodel) then
                    nolay = nolay_tmp
                    nosegl = noseg / nolay
                    if (nosegl * nolay /= noseg) then
                        write (lunut, 2060)
                        goto 1000
                    endif
                    GridPs%Pointers(i_base_grid)%noseg_lay = nosegl
                    GridPs%Pointers(i_base_grid)%nolay = nolay
                endif

            case ('BOTTOMGRID', 'BEDGRID')
                aGrid%itype = Bottomgrid
                call read_grid(lun, aGrid, GridPs, .false., nosegl_bottom, status)
                igrid = GridPointerCollAdd(GridPs, aGrid)
                if (GridPs%bottom_grid /= 0) then
                    write (lunut, 2070)
                    call status%increase_warning_count()
                else
                    GridPs%bottom_grid = igrid
                endif

            case ('PROCESSGRID')
                aGrid%itype = ProcessGrid
                call read_grid (lun, aGrid, GridPs, .false., nosegl_bottom, status)
                igrid = GridPointerCollAdd(GridPs, aGrid)

            case ('SUBGRID')
                aGrid%itype = ProcessGrid
                call read_grid (lun, aGrid, GridPs, .false., nosegl_bottom, status)
                igrid = GridPointerCollAdd(GridPs, aGrid)

            case ('NOBOTTOMLAY')
                call read_nobottomlay (GridPs, status)

            case ('SUBSTANCE_PROCESSGRID')
                newinput = .true.
                call read_sub_procgrid(notot - nototp, syname, GridPs, isysg, status)

            case ('PROCESS_TIMESTEP_MULTIPLIER')
                newinput = .true.
                call read_proc_time   (notot - nototp, syname, isyst, status)

            case ('END_MULTIGRID')             ! this keyword ends the
                exit                              ! sequence of new input processing

            case default
                if (.not. newinput) then
                    aGrid%itype = ProcessGrid
                    push = .true.
                    call read_grid (lun, aGrid, GridPs, .true., nosegl_bottom, status)
                    igrid = GridPointerCollAdd(GridPs, aGrid)
                    exit
                else
                    write (lunut, 2030) trim(ctoken)
                    goto 1000
                endif

            end select

        enddo

        nogrid = GridPs%cursize

        !        Expand with layers in the base grid

        do igrid = 1, nogrid

            if (igrid == GridPs%bottom_grid) cycle

            noseg2 = GridPs%Pointers(igrid)%noseg_lay

            if (.not. GridPs%Pointers(igrid)%space_var_nolay) then
                kseg = nosegl + 1
                do ilay = 2, nolay
                    ioff = (ilay - 1) * noseg2
                    do iseg = 1, nosegl
                        GridPs%Pointers(igrid)%iarray(kseg) = &
                                GridPs%Pointers(igrid)%iarray(iseg) + ioff
                        kseg = kseg + 1
                    enddo
                enddo
                noseg2 = noseg2 * nolay
                GridPs%Pointers(igrid)%noseg = noseg2
            endif
            if (igrid /= 1) write(lunut, 2080) igrid, GridPs%Pointers(igrid)%noseg

        enddo

        !     add the bottom segments to the total segments, set nseg2

        nseg2 = 0
        i_bottom_grid = GridPs%bottom_grid
        if (i_bottom_grid > 0) then
            nseg2 = GridPs%Pointers(i_bottom_grid)%noseg
            GridPs%Pointers(i_base_grid)%noseg = noseg + nseg2
        endif
        nosss = noseg + nseg2

        !     make pointers to final grid

        do igrid = 1, nogrid
            allocate(GridPs%Pointers(igrid)%finalpointer(nosss))
            if (igrid == i_base_grid) then
                do iseg = 1, nosss
                    GridPs%Pointers(igrid)%finalpointer(iseg) = iseg
                enddo
            elseif (igrid == i_bottom_grid) then
                GridPs%Pointers(igrid)%finalpointer(1:noseg) = 0
                do iseg2 = 1, nseg2
                    GridPs%Pointers(igrid)%finalpointer(noseg + iseg2) = iseg2
                enddo
            elseif(GridPs%Pointers(igrid)%itype == BottomGrid) then
                GridPs%Pointers(igrid)%finalpointer = 0
                iref = GridPs%Pointers(igrid)%iref
                do iseg = 1, nosss
                    iseg2 = GridPs%Pointers(iref)%finalpointer(iseg)
                    if (iseg2 > 0) then
                        GridPs%Pointers(igrid)%finalpointer(iseg) = GridPs%Pointers(igrid)%iarray(iseg2)
                    endif
                enddo
            else
                GridPs%Pointers(igrid)%finalpointer = 0
                iref = GridPs%Pointers(igrid)%iref
                do iseg = 1, noseg
                    iseg2 = GridPs%Pointers(iref)%finalpointer(iseg)
                    if (iseg2 > 0) then
                        GridPs%Pointers(igrid)%finalpointer(iseg) = GridPs%Pointers(igrid)%iarray(iseg2)
                    endif
                enddo
            endif
        enddo

        ! z-model

        if (zmodel .and. nolay_tmp /= 1) nolay = nolay_tmp

        !        Write grid to system file

        do igrid = 1, nogrid
            if (igrid == GridPs%bottom_grid) then
                write(lun(2)) GridPs%Pointers(iGrid)%noseg, &
                        -GridPs%Pointers(iGrid)%nolay, &
                        GridPs%Pointers(iGrid)%finalpointer
            else
                write(lun(2)) GridPs%Pointers(iGrid)%noseg, &
                        GridPs%Pointers(iGrid)%iref, &
                        GridPs%Pointers(iGrid)%finalpointer
            endif
        enddo
        do igrid = 1, nogrid
            ierr2 = gridwrite(lun(2), gridps%pointers(igrid))
        enddo

        !     Read per substance grid and time

        if (.not. newinput .and. read_input) then
            write(lunut, 2090)
            do isys = 1, notot - nototp
                if (gettoken(isysg(isys), ierr2) > 0) goto 1000
                if (gettoken(isyst(isys), ierr2) > 0) goto 1000
                write(lunut, 2100) isys, isysg(isys), isyst(isys)
                if (isysg(isys) < 1      .or. &
                        isysg(isys) > nogrid) then
                    write(lunut, 2110) isysg(isys)
                    call status%increase_error_count()
                endif
                if (isyst(isys) < 1) then
                    write(lunut, 2120) isyst(isys)
                    call status%increase_error_count()
                endif
            enddo
            do isys = notot - nototp + 1, notot
                isysg(isys) = 1
                isyst(isys) = 1
            enddo
        endif

        !     Write substance info to system file

        write(lun(2)) isysg
        write(lun(2)) isyst

        if (timon) call timstop(ithndl)
        return

        1000 continue
        write(lunut, 2130)
        call status%increase_error_count()

        if (timon) call timstop(ithndl)
        return

        2000 format (/' NO process decomposition selected')
        2010 format (/' Multiple grid option selected process decomposition will be used', &
                /' Number of grids               :', I10)
        2020 format (' Number of layers in base grid :', I10)
        2030 format (/' ERROR, unrecognized token: ', A)
        2040 format (/' Reading MULTIGRID information')
        2050 format (/' ERROR, NOLAY definition must preceed the GRID definitions')
        2060 format (/' ERROR, nr of segments/nr of layers is no integer.')
        2070 format (/' WARNING, bottomgrid already defined, first definition prevails!')
        2080 format (' Number of segments in sub-grid', I4, ' equals:', i10)
        2090 format (/' Reading substance grid and process-time information')
        2100 format (' For substance:', I7, ' using grid :', I7, &
                ' maximum process time step multiplier :', I7)
        2110 format (/' ERROR, grid number out of range:', I7)
        2120 format (/' ERROR, process step out of range:', I7)
        2130 format (/' ERROR, reading sub-grids information.')

    end

end module m_waq_grid
