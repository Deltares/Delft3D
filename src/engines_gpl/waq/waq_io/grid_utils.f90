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


module grid_utils

    use m_waq_precision
    use m_error_status
    use timers
    use rd_token
    use m_srstop

    implicit none

    private
    public :: read_multiple_grids

contains

    subroutine read_multiple_grids(lun, noseg, notot, nototp, nolay, &
            gridps, nseg2, nogrid, syname, status)

        !!  Reads optional multiple grids
        !!
        !!      Routine is called with ierr = -1 if only the base grid needs defined.
        !!      The following used input can be supplied:
        !!          - an integer. This is the number of additional grids in old input
        !!              processing. In new input processing the software counts the number
        !!              of additional grid specifications.
        !!          - ZMODEL. A Zlayer model is used, relevance unknown.
        !!          - NOLAY followed by an integer, specifies number of layers in base grid.
        !!              May be redundant because this can be done at several locations.
        !!          - BOTTOMGRID. Specifies grid in waterbed. Diverts to read_grid.
        !!          - BEDGRID. Seems to be the right name for BOTTOMGRID.
        !!          - PROCESSGRID. Specifies grid for processes. Diverts to read_grid.

        !!     Logical units  : LUN(29) = unit formatted output file
        !!                      LUN( 2) = unit intermediate file (system)

        use dlwqgrid_mod        !   for the storage of contraction grids

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

        ! Always add base grid to the collection of grids
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

        ! Read number of multiple grids
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

        ! Expand with layers in the base grid
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

        ! add the bottom segments to the total segments, set nseg2
        nseg2 = 0
        i_bottom_grid = GridPs%bottom_grid
        if (i_bottom_grid > 0) then
            nseg2 = GridPs%Pointers(i_bottom_grid)%noseg
            GridPs%Pointers(i_base_grid)%noseg = noseg + nseg2
        endif
        nosss = noseg + nseg2

        ! make pointers to final grid
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

        ! Write grid to system file
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

        ! Read per substance grid and time

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

        ! Write substance info to system file

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

    end subroutine read_multiple_grids

    subroutine read_sub_procgrid(notot, syname, GridPs, isysg, status)
        !!  read the SUBSTANCE_PROCESSGRID information and update the isysg array
        !!
        !!  several input possibilities exist:
        !!      - ALL indicates that all substances should work on the grid that will be mentioned
        !!      - a series of substances IDs indicating that those will work on the mentioned grid
        !!          then a grid name is required, to specify the grid where the substances work on.

        use m_string_utils
        use dlwqgrid_mod

        integer(kind = int_wp), intent(in) :: notot          !< nr of substances
        character(20), intent(in) :: syname(notot) !< substance names
        type(GridPointerColl), intent(in) :: GridPs        !< collection of all grid definitions
        integer(kind = int_wp), intent(inout) :: isysg (notot)  !< process gridnr of substances
        type(error_status), intent(inout) :: status !< current error status

        integer(kind = int_wp) :: itoken            ! integer token from input
        integer(kind = int_wp) :: idummy            ! dummy which content is not used
        real(kind = real_wp) :: adummy            ! dummy which content is not used
        character(len = 255) :: ctoken           ! character token from input
        character :: cdummy           ! dummy which content is not used
        integer(kind = int_wp) :: itype             ! type of input to be needded
        integer(kind = int_wp) :: ierr2             ! local error indication
        integer(kind = int_wp) :: sysused(notot)    ! work array substance selection
        integer(kind = int_wp) :: isys              ! index substance
        integer(kind = int_wp) :: i_grid            ! index grid in collection
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_sub_procgrid", ithndl)

        sysused = 0
        write (lunut, 2000)

        ! read input
        do
            if (gettoken(ctoken, ierr2) > 0) goto 1000

            select case (ctoken)
            case ('ALL')
                ! use all substances
                sysused = 1
                write (lunut, 2030)

            case default
                ! use this substance
                isys = index_in_array(ctoken(:20), syname)
                if (isys > 0) then
                    sysused(isys) = 1
                    write (lunut, 2040) syname(isys)
                else
                    i_grid = gridpointercollfind(GridPs, ctoken)
                    if (i_grid > 0) then                       ! use this grid, input is ready
                        write (lunut, 2050) trim(ctoken)
                        exit
                    else                                            ! unrecognised token
                        write (lunut, 2020) trim(ctoken)
                        goto 1000
                    endif
                endif

            end select

        enddo

        ! update the isysg array for all substances used in this block
        do isys = 1, notot
            if (sysused(isys) == 1) isysg(isys) = i_grid
        enddo

        if (timon) call timstop(ithndl)
        return

        1000 write (lunut, 2010)
        call status%increase_error_count()

        if (timon) call timstop(ithndl)
        return

        2000 format (/' Reading SUBSTANCE_PROCESSGRID information:')
        2010 format (' ERROR, reading SUBSTANCE_PROCESSGRID information.')
        2020 format (' ERROR, unrecognized token: ', A)
        2030 format (' Processgrid will be used for ALL substances')
        2040 format (' Processgrid will be used for substance: ', A)
        2050 format (' Processgrid for these substances is: ', A)

    end subroutine read_sub_procgrid

    subroutine read_grid(lun, aGrid, GridPs, oldproc, nosegl_bottom, status)

        !!  Reads a grid definition
        !!
        !!      Sets all properties of the grid and the pointer from each horizontal cell of the
        !!      reference grid to this grid. After the pointer is specified, no further
        !!      properties are read.\n
        !!      The type of the grid has already been set in the calling grid.f routine
        !!
        !!      Poperties that can be set are:
        !!          - NOLAY followed by an integer, number of layers of the grid (default is 0)
        !!          - AGGREGATIONFILE followed by a filename of a .lga type binary aggregation file
        !!          - REFERENCEGRID followed by the name of the reference grid for this grid
        !!          If an integer is met, the routine expects as many integers as in one
        !!          layer of the reference grid. They must contain the mapping of all cells of that
        !!          reference grid on the cells of this grid.\n
        !!          The default reference grid is the system base grid.\n
        !!          The routine determines the maximum gridcell nr of this new grid.\n
        !!          The routine checks that every cell of this new grid contains at least one cell
        !!          of the reference grid.

        use m_open_waq_files
        use dlwqgrid_mod     !   for the storage of contraction grids

        integer(kind = int_wp), intent(inout) :: lun(*)         !< unit numbers used
        type(GridPointer), intent(inout) :: aGrid         !< collection off all grid definitions
        type(GridPointerColl), intent(in) :: GridPs        !< collection off all grid definitions
        logical, intent(in) :: oldproc       !< true if old processing
        integer(kind = int_wp), intent(in) :: nosegl_bottom  !< number of segments expected for bottom

        type(error_status), intent(inout) :: status !< current error status

        integer(kind = int_wp) :: itype         ! type of input that was obtained
        integer(kind = int_wp) :: itoken        ! integer token from input
        integer(kind = int_wp) :: idummy        ! dummy which content is not used
        character(len = 255) :: ctoken       ! character token from input
        integer(kind = int_wp) :: ierr2         ! local error indication
        integer(kind = int_wp) :: i_base_grid   ! index base grid in collection
        integer(kind = int_wp) :: i_grid        ! index grid in collection
        integer(kind = int_wp) :: nmax          ! nmax
        integer(kind = int_wp) :: mmax          ! mmax
        integer(kind = int_wp) :: noseg         ! number of segments
        integer(kind = int_wp) :: noseg2        ! number of segments in sub grid
        integer(kind = int_wp) :: noseg_lay     ! number of segments per layer
        integer(kind = int_wp) :: noseg_fil     ! number of segments in file
        integer(kind = int_wp) :: noseg_input   ! number of segments in input
        integer(kind = int_wp) :: iseg          ! index segment number
        integer(kind = int_wp) :: iseg2         ! second index segment number
        integer(kind = int_wp) :: nolay         ! number of layers
        integer(kind = int_wp), allocatable :: iwork(:)      ! work array
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_grid", ithndl)

        i_base_grid = GridPs%base_grid
        noseg = GridPs%pointers(i_base_grid)%noseg
        noseg_lay = GridPs%pointers(i_base_grid)%noseg_lay

        ! set default, type is already set in calling routine (bottomgrid, processgrid, subgrid )
        aGrid%name = ' '
        aGrid%noseg = 0
        aGrid%noseg_lay = 0
        aGrid%iref = i_base_grid         ! default, may be overridden
        aGrid%name_ref = ' '
        aGrid%iarray => null()
        aGrid%space_var_nolay = .false.
        aGrid%nolay = 1                  ! default, may be overridden
        aGrid%nolay_var => null()
        if (gettoken(ctoken, ierr2) > 0) goto 1000    ! get name
        aGrid%name = ctoken
        write (lunut, 2000) aGrid%name
        if (oldproc) then
            if (gettoken(agrid%iref, ierr2) > 0) goto 1000
            agrid%name_ref = gridps%pointers(agrid%iref)%name
        endif

        do

            if (gettoken(ctoken, itoken, itype, ierr2) > 0) goto 1000
            if (itype == 1) then                              ! it is a string
                select case (ctoken)

                case ('NOLAY')
                    if (gettoken(aGrid%nolay, ierr2) > 0) goto 1000
                    write (lunut, 2010) aGrid%nolay

                case ('NOAGGREGATION')
                    allocate (aGrid%iarray(noseg))
                    do iseg = 1, noseg
                        aGrid%iarray(iseg) = iseg
                    end do
                    exit                                         ! input for the grid is ready

                case ('AGGREGATIONFILE')                      ! it is the filename keyword
                    if (gettoken(ctoken, ierr2) > 0) goto 1000
                    call open_waq_files (lun(33), ctoken, 33, 1, ierr2)
                    if (ierr2 /= 0) goto 1000
                    read  (lun(33), *) nmax, mmax, noseg_fil, idummy, idummy
                    write (lunut, 2020) ctoken, nmax, mmax, noseg_fil
                    if (noseg_fil /= noseg_lay) then
                        write (lunut, 2030) noseg_fil, noseg_lay
                        goto 1000
                    endif
                    allocate (aGrid%iarray(noseg))
                    read  (lun(33), *) (aGrid%iarray(iseg), iseg = 1, noseg_fil)
                    close (lun(33))
                    exit                                         ! input for the grid is ready

                case ('BOTTOMGRID_FROM_ATTRIBUTES')       ! it is the filename keyword
                    allocate (aGrid%iarray(noseg))
                    call read_attributes_for_bottomgrid(lunut, aGrid%iarray, nosegl_bottom, status)
                    exit

                case ('REFERENCEGRID')
                    if (gettoken(ctoken, ierr2) > 0) goto 1000
                    aGrid%name_ref = ctoken
                    write (lunut, 2040) aGrid%name_ref
                    i_grid = gridpointercollfind(GridPs, aGrid%name_ref)
                    if (i_grid > 0) then
                        aGrid%iref = i_grid
                    else
                        write (lunut, 2050)
                        call status%increase_error_count()
                    endif
                    noseg_lay = GridPs%pointers(aGrid%iref)%noseg_lay

                case default
                    write (lunut, 2060) trim(ctoken)          ! ERROR, token not recognised
                    goto 1000

                end select
            else                                                  ! it was an integer.
                allocate (aGrid%iarray(noseg))
                aGrid%iarray(1) = itoken                           ! this integer is first pointer
                do iseg = 2, noseg_lay
                    if (gettoken(aGrid%iarray(iseg), ierr2) > 0) goto 1000
                    if (aGrid%iarray(iseg) > noseg_lay) then
                        write (lunut, 2070) aGrid%iarray(iseg)
                        call status%increase_error_count()
                    endif
                enddo
                exit                                               ! input for the grid is ready
            endif

        enddo

        ! Determine nr of segments in aggregated pointer
        noseg2 = 0
        allocate(iwork(noseg))
        iwork = 0
        do iseg = 1, noseg_lay
            iseg2 = aGrid%iarray(iseg)
            if (iseg2 > 0) then
                noseg2 = max(noseg2, iseg2)
                iwork(iseg2) = iwork(iseg2) + 1
            endif
        enddo
        do iseg2 = 1, noseg2
            if (iwork(iseg2) == 0) then
                write (lunut, 2080) iseg2
                call status%increase_error_count()
            endif
        enddo
        aGrid%noseg_lay = noseg2
        deallocate(iwork)

        if (timon) call timstop(ithndl)
        return

        1000 continue
        write(lunut, 2090)
        call status%increase_error_count()
        return

        2000 format (' Name of this grid is: ', A)
        2010 format (' Number of layers for this grid:', I10)
        2020 format (' Aggregationfile     : ', A, &
                /' Matrix (', I5, 'x', I5, ') of ', I7, ' elements.')
        2030 format (' ERROR, nr of cells in aggregation file is: ', I10, &
                /'        nr of hor. cells in simulation is:  ', I10)
        2040 format (' Reference grid for this grid  : ', A)
        2050 format (/' ERROR, reference grid not defined.')
        2060 format (/' ERROR, unrecognized token: ', A)
        2070 format (/' ERROR, segment in sub-grid out of range:', I15)
        2080 format (/' ERROR, segment in sub-grid not defined:', I15)
        2090 format (/' ERROR, reading GRID information.')
    end subroutine read_grid

    subroutine read_attributes_for_bottomgrid(lunut, iarray, nosegl, status)
        use m_evaluate_waq_attribute

        integer(kind = int_wp) :: lunut, nosegl
        integer(kind = int_wp), dimension(:) :: iarray

        type(error_status), intent(inout) :: status !< current error status

        integer(kind = int_wp) :: i, j, nkopt, ikopt1, ikopt2, ierr2, lunbin, iover, ikdef, idummy
        integer(kind = int_wp) :: noseg, nopt, nover, attrib, active, iknm1, iknm2, iknmrk, ivalk, iseg
        integer(kind = int_wp), allocatable, dimension(:) :: iamerge  !!  composite attribute array
        !!  array with indicators whether attributes are already set
        integer(kind = int_wp), allocatable, dimension(:) :: ikmerge
        integer(kind = int_wp), allocatable, dimension(:) :: ikenm  !!  array with attributes of an input block
        integer(kind = int_wp), allocatable, dimension(:) :: iread   !!  array to read attributes
        character(len = 255) :: filename

        noseg = size(iarray)
        allocate (iamerge(noseg))
        allocate (ikmerge(10))
        allocate (iread(noseg))
        iarray = 0
        ikmerge = 0
        iamerge = 0
        iread = 0

        if (gettoken(nkopt, ierr2) > 0) goto 900

        do i = 1, nkopt                                      !   read those blocks

            if (gettoken(nopt, ierr2) > 0) goto 900
            allocate (ikenm(nopt))
            do j = 1, nopt                                        !   get the attribute numbers
                if (gettoken(ikenm(j), ierr2) > 0) goto 900
            enddo

            if (gettoken(ikopt1, ierr2) > 0) goto 900      !   the file option for this info
            if (ikopt1 == 0) then                             !   binary file
                if (gettoken(filename, ierr2) > 0) goto 910    !   the name of the binary file
                open(newunit = lunbin, file = filename, status = 'old', access = 'stream', iostat = ierr2)
                if (ierr2 == 0) then
                    read  (lunbin, iostat = ierr2) (iread(j), j = 1, noseg)
                    close (lunbin)
                    if (ierr2 /= 0) then
                        write (lunut, 2010) trim(filename)
                    endif
                else
                    write (lunut, 2020) trim(filename)
                endif
            else
                if (gettoken(ikopt2, ierr2) > 0) goto 900   !   second option

                select case (ikopt2)

                case (1)                                      !   no defaults
                    do j = 1, noseg
                        if (gettoken(iread(j), ierr2) > 0) goto 900
                    enddo

                case (2)                                      !   default with overridings
                    if (gettoken(ikdef, ierr2) > 0) goto 900
                    if (gettoken(nover, ierr2) > 0) goto 900
                    do iseg = 1, noseg
                        iread(iseg) = ikdef
                    enddo
                    if (nover > 0) then
                        do j = 1, nover
                            if (gettoken(iover, ierr2) > 0) goto 900
                            if (gettoken(idummy, ierr2) > 0) goto 900
                            if (iover < 1 .or. iover > noseg) then
                                write (lunut, 2030) j, iover
                                call status%increase_error_count()
                            else
                                iread(iover) = idummy
                            endif
                        enddo
                    endif

                case default
                    write (lunut, 2040) ikopt2
                    call status%increase_error_count()

                end select
            endif

            ! Merge file buffer with attributes array in memory
            do iknm2 = 1, nopt
                iknm1 = ikenm(iknm2)

                ! see if merged already
                if (ikmerge(iknm1) /= 0) then
                    write (lunut, 2260) iknm2, iknm1
                    call status%increase_error_count()
                    exit
                endif

                ! see if valid
                if (iknm1 <= 0 .or. iknm1 > 10) then
                    if (iknm1 == 0) then
                        write (lunut, 2270) iknm2
                        call status%increase_error_count()
                        exit
                    else
                        write (lunut, 2280) iknm1, iknm2
                        call status%increase_error_count()
                        exit
                    endif
                endif

                ! Merge for this attribute
                ikmerge(iknm1) = 1
                iknmrk = 10**(iknm1 - 1)
                do iseg = 1, noseg
                    call evaluate_waq_attribute(iknm2, iread(iseg), ivalk)
                    iamerge(iseg) = iamerge(iseg) + iknmrk * ivalk
                enddo
            enddo
            deallocate (ikenm)
        enddo

        ! Extract the information we need
        do i = 1, noseg
            call evaluate_waq_attribute(1, iamerge(i), active)
            call evaluate_waq_attribute(2, iamerge(i), attrib)
            if (active == 1 .and. (attrib == 0 .or. attrib == 3)) then
                iarray(i) = 1 + mod(i - 1, nosegl)
            endif
        enddo

        ! We read the number of time-dependent attributes - there should be none
        if (gettoken(nopt, ierr2) > 0) goto 900   !   second option
        if (nopt /= 0) then
            write(lunut, 2050)
            goto 900
        endif

        return

        ! Handle errors
        900 continue
        if (ierr2 > 0) then
            call status%increase_error_count()
        end if

        if (ierr2 == 3) call srstop(1)
        write(lunut, 2000)
        return

        910 continue
        if (ierr2 > 0) then
            call status%increase_error_count()
        end if
        if (ierr2 == 3) call srstop(1)
        write(lunut, 2001)

        return

        2000 format(/, ' ERROR. Unexpected value in attributes file - should be an integer')
        2001 format(/, ' ERROR. Reading name of binary attributes file')
        2010 format(/, ' ERROR. Reading binary attributes file - too few data?', /, 'File: ', a)
        2020 format(/, ' ERROR. Opening binary attributes file - incorrect name?', /, 'File: ', a)
        2030 format(/, ' ERROR. Overriding out of bounds - overriding: ', i0, ' - segment: ', i0)
        2040 format(/, ' ERROR. Unknown option for attributes: ', i0)
        2050 format(/, ' ERROR. The number of time-dependent attributes should be zero - limitation in the implementation')
        2260 format (/ ' ERROR, Pointer of contribution ', I6, ' mapped to attribute', I6, ' already specified.')
        2270 format (/ ' ERROR, Pointer of contribution ', I6, ' zero.')
        2280 format (/ ' ERROR, Pointer of contribution ', I6, '=', I6, ' is out of range(1-10).')

    end subroutine read_attributes_for_bottomgrid

    subroutine read_proc_time(notot, syname, isyst, status)
        !!  read the PROCESS_TIMESTEP_MULTIPLIER information update the isyst array
        !!
        !! several input possibilities exist:
        !!      - ALL indicates that all substances should work with this time multiplier
        !!      - a series of substances IDs indicating that those will work with the multiplier
        !!          then the time multiplier to be used is required

        use m_string_utils

        integer(kind = int_wp), intent(in) :: notot           !< nr of substances
        character(20), intent(in) :: syname(notot)  !< substance names
        integer(kind = int_wp), intent(inout) :: isyst (notot)   !< process timestep multiplier
        type(error_status), intent(inout) :: status !< current error status

        character(len = 255) :: ctoken           ! character token from input
        integer(kind = int_wp) :: itype             ! type of input that was provided
        integer(kind = int_wp) :: ierr2             ! local error indication
        integer(kind = int_wp) :: sysused(notot)    ! work array substance selection
        integer(kind = int_wp) :: isys              ! index substance
        integer(kind = int_wp) :: idtmult           ! timestep multiplier
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_proc_time", ithndl)

        sysused = 0
        write (lunut, 2000)

        ! read input
        do
            if (gettoken(ctoken, idtmult, itype, ierr2) > 0) goto 1000
            if (itype == 1) then
                select case (ctoken)
                case ('ALL')
                    ! use all substances
                    sysused = 1
                    write (lunut, 2030)

                case default
                    isys = index_in_array(ctoken(1:20), syname(:notot))
                    if (isys > 0) then                      ! use this substance
                        sysused(isys) = 1
                        write (lunut, 2040) syname(isys)
                    else                                         ! unrecognised token
                        write (lunut, 2020) trim(ctoken)
                        goto 1000
                    endif
                end select

            else
                ! time multiplier read
                write (lunut, 2050) idtmult
                exit

            endif

        enddo

        ! update the isyst array for all substances used in this block
        do isys = 1, notot
            if (sysused(isys) == 1) isyst(isys) = idtmult
        enddo

        if (timon) call timstop(ithndl)
        return

        1000 continue
        write(lunut, 2010)
        call status%increase_error_count()

        if (timon) call timstop(ithndl)
        return

        2000 format (/' Reading PROCESS_TIMESTEP_MULTIPLIER information:')
        2010 format (' ERROR, reading PROCESS_TIMESTEP_MULTIPLIER information.')
        2020 format (' ERROR, unrecognized token: ', A)
        2030 format (' Timestep will be used for ALL substances')
        2040 format (' Timestep will be used for substance: ', A)
        2050 format (' Timestep multiplier for these substances is: ', I10)

    end subroutine read_proc_time

    subroutine read_nobottomlay(GridPs, status)
        !!  Reads the number of bed layers per sediment column
        !!
        !!  Several options exist:
        !!      - DEFAULT followed by the number of layers to be applied for all columns
        !!      - INPUTGRID followed by:
        !!          - an existig grid name that contains the bottom columns map
        !!          - for each cell of this grid, an integer giving the number of layers
        !!       - ALL followed by an integer for each cell of the BOTTOMGRID
        !!         Using the INPUTGRID feature specifies the number of layers for anonther
        !!         grid than the BOTTOMGRID. This grid then is expanded to the BOTTOMGRID.

        use dlwqgrid_mod

        type(GridPointerColl), intent(inout) :: GridPs     !< collection off all grid definitions
        type(error_status), intent(inout) :: status !< current error status

        type(GridPointer) :: aGrid                ! a single grid
        integer(kind = int_wp) :: itoken                ! integer token from input
        integer(kind = int_wp) :: idummy                ! dummy which content is not used
        real(kind = real_wp) :: adummy                ! dummy which content is not used
        character(len = 255) :: ctoken               ! character token from input
        character :: cdummy               ! dummy which content is not used
        integer(kind = int_wp) :: i_base_grid           ! index base grid in collection
        integer(kind = int_wp) :: i_bottom_grid         ! index bottom grid in collection
        integer(kind = int_wp) :: iseg                  ! segment index
        integer(kind = int_wp) :: iseg2                 ! segment index
        integer(kind = int_wp) :: input_grid            ! index input grid in collection
        integer(kind = int_wp) :: itype                 ! type of input to be needded
        integer(kind = int_wp) :: ierr2                 ! local error indication
        integer(kind = int_wp) :: iref                  ! index reference grid in collection
        integer(kind = int_wp) :: noseg_lay             ! number of segments per layer
        integer(kind = int_wp) :: noseg_input           ! number of segments in input
        integer(kind = int_wp) :: noseg_grid            ! number of segments in all layers
        integer(kind = int_wp) :: nolay                 ! number of layers
        integer(kind = int_wp) :: max_nolay             ! max number of layers in space_var_nolay
        integer(kind = int_wp) :: ilay                  ! index layers
        integer(kind = int_wp), pointer :: new_pointer(:)        ! new grid pointer on expanded bottom grid
        integer(kind = int_wp), allocatable :: bottom_matrix(:, :)    ! new grid on expanded bottom grid
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_nobottomlay", ithndl)

        ! check bottom grid
        i_base_grid = GridPs%base_grid
        i_bottom_grid = GridPs%bottom_grid

        if (i_bottom_grid == 0) then

            ! bottom grid not defined yet, add default bottom grid equals base grid 2d
            aGrid%name = 'Bottom grid'
            aGrid%noseg = 0
            aGrid%noseg_lay = GridPs%Pointers(i_base_grid)%noseg_lay
            aGrid%iref = i_base_grid
            aGrid%name_ref = GridPs%Pointers(i_base_grid)%name
            aGrid%itype = BottomGrid
            aGrid%space_var_nolay = .FALSE.
            aGrid%nolay = 0
            aGrid%nolay_var => NULL()
            Allocate (aGrid%iarray(GridPs%Pointers(i_base_grid)%noseg))
            do iseg = 1, GridPs%Pointers(i_base_grid)%noseg_lay
                aGrid%iarray(iseg) = iseg
            enddo
            i_bottom_grid = GridPointerCollAdd(GridPs, aGrid)
            GridPs%bottom_grid = i_bottom_grid

        endif

        ! read input
        input_grid = i_bottom_grid
        do
            if (gettoken(ctoken, ierr2) > 0) goto 1000
            select case (ctoken)

            case ('DEFAULT')
                if (gettoken(nolay, ierr2) > 0) goto 1000
                GridPs%Pointers(input_grid)%nolay = nolay
                write (lunut, 2050) nolay
                GridPs%Pointers(input_grid)%noseg = nolay * GridPs%Pointers(input_grid)%noseg_lay
                exit

            case ('INPUTGRID')
                if (gettoken(ctoken, ierr2) > 0) goto 1000
                write (lunut, 2010) trim(ctoken)
                input_grid = GridPointerCollFind(GridPs, ctoken)
                if (input_grid == 0) then
                    write (lunut, 2020)                       ! ERROR, input grid not defined
                    goto 1000
                endif
                noseg_input = GridPs%Pointers(input_grid)%noseg_lay
                GridPs%Pointers(input_grid)%space_var_nolay = .TRUE.
                allocate(GridPs%Pointers(input_grid)%nolay_var(noseg_input))
                noseg_grid = 0
                do iseg = 1, noseg_input
                    if (gettoken(nolay, ierr2) > 0) goto 1000
                    GridPs%Pointers(input_grid)%nolay_var(iseg) = nolay
                    noseg_grid = noseg_grid + nolay
                enddo
                GridPs%Pointers(input_grid)%noseg = noseg_grid
                exit

            case ('ALL')
                write (lunut, 2060)
                noseg_input = GridPs%Pointers(input_grid)%noseg_lay
                GridPs%Pointers(input_grid)%space_var_nolay = .TRUE.
                allocate(GridPs%Pointers(input_grid)%nolay_var(noseg_input))
                noseg_grid = 0
                do iseg = 1, noseg_input
                    if (gettoken(nolay, ierr2) > 0) goto 1000
                    GridPs%Pointers(input_grid)%nolay_var(iseg) = nolay
                    noseg_grid = noseg_grid + nolay
                enddo
                GridPs%Pointers(input_grid)%noseg = noseg_grid
                exit

            case default
                write (lunut, 2030) trim(ctoken)             ! ERROR, token not recognised
                goto 1000

            end select

        enddo

        ! if input grid .ne. bottom grid then expand to bottom grid (could be over a multiple reference?)

        if (input_grid /= i_bottom_grid) then
            max_nolay = maxval(GridPs%Pointers(input_grid)%nolay_var)
            do ! loop till the bottom grid is reached
                iref = GridPs%Pointers(input_grid)%iref
                if(iref <= 1) then
                    write (lunut, 2040)                          ! ERROR, input grid has no refrence to the bottom grid
                    goto 1000
                endif
                noseg_lay = GridPs%Pointers(iref)%noseg_lay
                GridPs%Pointers(iref)%space_var_nolay = .TRUE.
                allocate(GridPs%Pointers(iref)%nolay_var(noseg_lay))
                GridPs%Pointers(iref)%nolay_var = GridPs%Pointers(iref)%nolay
                noseg_grid = 0
                do iseg = 1, noseg_lay
                    itype = GridPs%Pointers(input_grid)%iarray(iseg)
                    if (itype > 0) then
                        GridPs%Pointers(iref)%nolay_var(iseg) = GridPs%Pointers(input_grid)%nolay_var(itype)
                        noseg_grid = noseg_grid + GridPs%Pointers(input_grid)%nolay_var(itype)
                    endif
                enddo
                GridPs%Pointers(iref)%noseg = noseg_grid

                ! expand pointers over the layers
                allocate(bottom_matrix(GridPs%Pointers(input_grid)%noseg_lay, max_nolay))
                bottom_matrix = 0
                iseg2 = 0
                do ilay = 1, max_nolay
                    do iseg = 1, GridPs%Pointers(input_grid)%noseg_lay
                        if (GridPs%Pointers(input_grid)%nolay_var(iseg) >= ilay) then
                            iseg2 = iseg2 + 1
                            bottom_matrix(iseg, ilay) = iseg2
                        endif
                    enddo
                enddo

                allocate(new_pointer(noseg_grid))
                iseg2 = 0
                do ilay = 1, max_nolay
                    do iseg = 1, noseg_lay
                        itype = GridPs%Pointers(input_grid)%iarray(iseg)
                        if (itype > 0) then
                            if (GridPs%Pointers(input_grid)%nolay_var(itype) >= ilay) then
                                iseg2 = iseg2 + 1
                                new_pointer(iseg2) = bottom_matrix(itype, ilay)
                            endif
                        endif
                    enddo
                enddo
                deallocate(GridPs%Pointers(input_grid)%iarray)
                GridPs%Pointers(input_grid)%iarray => new_pointer
                nullify(new_pointer)
                deallocate(bottom_matrix)

                ! exit if bottom grid else go to next reference

                input_grid = iref
                if (input_grid == i_bottom_grid) exit
            enddo
        endif

        if (timon) call timstop(ithndl)
        return

        1000 continue
        write(lunut, 2000)
        call status%increase_error_count()
        if (timon) call timstop(ithndl)
        return

        2000 format (/' ERROR, reading NOBOTTOMLAY information.')
        2010 format (/' Space varying number of bottom layers:', &
                /' Defined on grid: ', A)
        2020 format (/' ERROR, input grid not defined.')
        2030 format (/' ERROR, unrecognized token: ', A)
        2040 format (/' ERROR, input grid has no refrence to the bottom grid')
        2050 format (/' Default number of bottom layers:', I10)
        2060 format (/' Space varying number of bottom layers defined on bottom grid')

    end subroutine read_nobottomlay

end module grid_utils
