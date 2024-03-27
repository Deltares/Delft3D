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
module m_read_grid
    use m_waq_precision
    use m_error_status

    implicit none

contains


    subroutine read_grid (lun, aGrid, GridPs, oldproc, nosegl_bottom, status)

        !       Deltares Software Centre

        !>\file
        !>                    Reads a grid definition
        !>
        !>          Sets all properties of the grid and the pointer from each horizontal cell of the
        !>          reference grid to this grid. After the pointer is specified, no further
        !>          properties are read.\n
        !>          The type of the grid has already been set in the calling grid.f routine
        !>          Poperties that can be set are:
        !>          - NOLAY followed by an integer, number of layers of the grid (default is 0)
        !>          - AGGREGATIONFILE followed by a filename of a .lga type binary aggregation file
        !>          - REFERENCEGRID followed by the name of the reference grid for this grid
        !>          If an integer is met, the routine expects as many integers as in one
        !>          layer of the reference grid. They must contain the mapping of all cells of that
        !>          reference grid on the cells of this grid.\n
        !>          The default reference grid is the system base grid.\n
        !>          The routine determines the maximum gridcell nr of this new grid.\n
        !>          The routine checks that every cell of this new grid contains at least one cell
        !>          of the reference grid.

        !     Created        : ......  200.    Jan van Beek  creation of Layered Bed special
        !                      May     2011    Leo Postma    merged with standard version

        !     Subroutines called : gettoken  to read tokens from the input file is in the rd_token module
        !                          dhopenf   to open a binary file to read gridpointer from

        !     Functions called   : gridpointercollfind to find the number of the reference grid by name
        !                                              function is contained in the grids module

        use m_srstop
        use m_open_waq_files
        use dlwqgrid_mod     !   for the storage of contraction grids
        use rd_token         !   for the reading of tokens
        use timers       !   performance timers

        implicit none

        !     declaration of arguments

        integer(kind = int_wp), intent(inout) :: lun(*)         !< unit numbers used
        type(GridPointer), intent(inout) :: aGrid         !< collection off all grid definitions
        type(GridPointerColl), intent(in) :: GridPs        !< collection off all grid definitions
        logical, intent(in) :: oldproc       !< true if old processing
        integer(kind = int_wp), intent(in) :: nosegl_bottom  !< number of segments expected for bottom

        type(error_status), intent(inout) :: status !< current error status

        !     local declarations

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

        !     some init

        i_base_grid = GridPs%base_grid
        noseg = GridPs%pointers(i_base_grid)%noseg
        noseg_lay = GridPs%pointers(i_base_grid)%noseg_lay

        !     set default, type is already set in calling routine
        !                                          ( bottomgrid, processgrid, subgrid )

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
                    !jvb not set yet  noseg     = GridPs%pointers(aGrid%iref)%noseg
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

        !     Determine nr of segments in aggregated pointer

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

    contains

        subroutine read_attributes_for_bottomgrid(lunut, iarray, nosegl, status)
            use m_evaluate_waq_attribute

            integer(kind = int_wp) :: lunut, nosegl
            integer(kind = int_wp), dimension(:) :: iarray

            type(error_status), intent(inout) :: status !< current error status

            integer(kind = int_wp) :: i, j, nkopt, ikopt1, ikopt2, ierr2, lunbin, iover, ikdef, idummy
            integer(kind = int_wp) :: noseg, nopt, nover, attrib, active, iknm1, iknm2, iknmrk, ivalk
            integer(kind = int_wp), allocatable, dimension(:) :: iamerge         !  composite attribute array
            integer(kind = int_wp), allocatable, dimension(:) :: ikmerge         !  array with indicators whether attributes are already set
            integer(kind = int_wp), allocatable, dimension(:) :: ikenm           !  array with attributes of an input block
            integer(kind = int_wp), allocatable, dimension(:) :: iread           !  array to read attributes
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

                !        Merge file buffer with attributes array in memory

                do iknm2 = 1, nopt
                    iknm1 = ikenm(iknm2)

                    !                 see if merged already

                    if (ikmerge(iknm1) /= 0) then
                        write (lunut, 2260) iknm2, iknm1
                        call status%increase_error_count()
                        exit
                    endif

                    !                 see if valid

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

                    !              Merge for this attribute

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
            !
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

            ! Formats
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

    end

end module m_read_grid
