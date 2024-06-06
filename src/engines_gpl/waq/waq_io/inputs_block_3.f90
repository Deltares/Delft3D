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
module inputs_block_3
    use m_waq_precision
    use m_read_hydfile
    use simulation_input_options, only: process_simulation_input_options, read_constants_time_variables
    use grid_utils, only: read_multiple_grids

    implicit none

    private
    public :: read_block_3_grid_layout

contains

    subroutine read_block_3_grid_layout(file_unit_list, file_name_list, filtype, nrftot, nrharm, &
            ivflag, is_date_format, iwidth, is_yyddhh_format, &
            output_verbose_level, gridps, syname, status, &
            has_hydfile, nexch)

        !! Reads grid layout; attributes and the computational volumes
        !>
        !> This routine reads:
        !>      - the number of computational volumes
        !>      - the number of layers (optional)
        !>      - the number and info of additional grids (optional)
        !>      - any wish for a printed grid layout
        !>      - constant attribute arrays
        !>      - time varying attribute arrays
        !>      - information on the time series of volumes

        use error_handling, only : check_error
        use m_logger_helper, only : stop_with_error
        use m_open_waq_files
        use m_evaluate_waq_attribute
        use m_grid_utils_external !   for the storage of contraction grids
        use rd_token     !   for the reading of tokens
        use partmem      !   for PARTicle tracking
        use timers       !   performance timers
        use waq_netcdf_utils
        use results, only: lncout       !   output settings
        use m_sysn          ! System characteristics
        use m_error_status

        integer(kind = int_wp), intent(inout) :: file_unit_list    (*)         !< array with unit numbers
        character(*), intent(inout) :: file_name_list  (*)        !< array with file names of the files
        integer(kind = int_wp), intent(inout) :: filtype(*)         !< type of binary file
        integer(kind = int_wp), intent(inout) :: nrftot (*)         !< number of function items
        integer(kind = int_wp), intent(inout) :: nrharm (*)         !< number of harmonic items
        integer(kind = int_wp), intent(out) :: ivflag             !< computed volumes ?
        logical, intent(in) :: is_date_format            !< 'date'-format 1st timescale
        integer(kind = int_wp), intent(in) :: iwidth             !< width of the output file
        logical, intent(in) :: is_yyddhh_format            !< 'date'-format (F;ddmmhhss,T;yydddhh)
        integer(kind = int_wp), intent(in) :: output_verbose_level             !< flag for more or less output
        character(20), intent(in) :: syname (*)        !< array with substance names
        logical, intent(out) :: has_hydfile       !< if true, much information comes from the hyd-file
        integer(kind = int_wp), dimension(3), intent(out) :: nexch  !< number of exchanges as read via hyd-file
        type(GridPointerColl)           GridPs            !< Collection of grid pointers
        type(error_status), intent(inout) :: status

        character(len = 255)           cdummy            !  workspace to read a string
        integer(kind = int_wp) :: idummy             !  location to read an integer
        logical                 disper            !  is read_constants_time_variables called for dispersions ?
        integer(kind = int_wp) :: volume             !  is 1 if read_constants_time_variables is called for volumes ?
        integer(kind = int_wp) :: ifact              !  needed for call to read_constants_time_variables
        integer(kind = int_wp) :: itype              !  type of token that is returned
        integer(kind = int_wp), allocatable :: ikmerge(:) !! array with indicators whether attributes are already set
        integer(kind = int_wp), allocatable :: iamerge(:) !! composite attribute array
        integer(kind = int_wp), allocatable :: ikenm  (:) !! array with attributes of an input block
        integer(kind = int_wp), allocatable :: iread  (:) !!  array to read attributes
        integer(kind = int_wp), allocatable :: pgrid  (:, :)       !  workspace for matrix with printed grids
        integer(kind = int_wp) :: imopt1             !  first option for grid layout
        integer(kind = int_wp) :: i, j, k            !  loop counters
        integer(kind = int_wp) :: ikerr              !  error indicator attributes
        integer(kind = int_wp) :: nkopt              !  number of attribute blocks
        integer(kind = int_wp) :: nopt               !  that many attributes in this block
        integer(kind = int_wp) :: ikopt1             !  first (file) option attributes
        integer(kind = int_wp) :: ikopt2             !  second (default/nondefault) option attributes
        integer(kind = int_wp) :: ikdef              !  default value attributes
        integer(kind = int_wp) :: nover              !  number of overridings
        integer(kind = int_wp) :: iover              !  overriding cell number
        integer(kind = int_wp) :: iseg               !  loop counter computational volumes
        integer(kind = int_wp) :: iknm1, iknm2       !  help variables for attributes
        integer(kind = int_wp) :: iknmrk             !  help variables merged attributes
        integer(kind = int_wp) :: ivalk              !  return value dhknmrk

        logical                 exist             !  whether a file exists or not
        character(len = 255)           ugridfile         !  name of the ugrid-file
        character(len = 255)           hydfile           !  name of the hyd-file
        integer(kind = int_wp) :: ncid, ncidout
        integer(kind = int_wp) :: varid, varidout, meshid, timeid, bndtimeid, ntimeid, wqid
        integer(kind = int_wp) :: meshid2d, type_ugrid, meshid1d, networkid, network_geometryid
        integer(kind = int_wp) :: inc_error

        character(len = nf90_max_name) :: mesh_name
        character(len = nf90_max_name) :: dimname

        type(error_status) :: local_status !< local status

        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_block_3_grid_layout", ithndl)

        disper = .false.
        volume = 1
        ifact = 1
        ivflag = 0
        iposr = 0
        nexch = 0

        call local_status%initialize(0, 0, 0)

        ! Check if there is a keyword for the grid or the hyd-file
        lncout = .false.
        file_name_list (46) = ' '
        if (gettoken(cdummy, idummy, itype, local_status%ierr) > 0) goto 240
        has_hydfile = .false.
        if (cdummy == 'HYD_FILE') then
            if (gettoken(hydfile, local_status%ierr) > 0) goto 240
            !
            ! Retrieve several file names:
            ! - attributes
            ! - volumes, areas, flows, lengths, ...
            ! - UGRID-file
            !
            write (file_unit, 2450)
            write (file_unit, 2460) trim(hydfile)

            call read_hydfile(file_unit, hydfile, file_name_list, noseg, nexch, local_status)
            if (local_status%ierr /= 0) goto 240
            has_hydfile = .true.
            ugridfile = file_name_list(46)
        endif

        if (cdummy == 'UGRID' .or. (has_hydfile .and. ugridfile /= ' ')) then

            ! Turn on debug info from dlwaqnc
            inc_error = set_dlwqnc_debug_status(.true.)
            ! Check if the UGRID file is suitable for Delwaq Output
            write (file_unit, 2500)

            if (.not. has_hydfile) then
                if (gettoken(ugridfile, local_status%ierr) > 0) goto 240
            endif

            write (file_unit, 2510) trim(ugridfile)
            inquire(file = ugridfile, exist = exist)
            if (.not. exist) then
                write (file_unit, 2511)
                lncout = .false.
            else
                lncout = .true.
                file_name_list (46) = ugridfile
            endif

            if (lncout) then
                ! Write the version of the netcdf library
                write (file_unit, 2520) trim(nf90_inq_libvers())

                ! Open the ugrid-file file
                inc_error = nf90_open(trim(ugridfile), nf90_nowrite, ncid)
                if (inc_error /= nf90_noerr) then
                    write (file_unit, 2530) trim(ugridfile)
                    write (file_unit, 2599) trim(nf90_strerror(inc_error))
                    call status%increase_error_count()
                    lncout = .false.
                end if
            end if

            if (lncout) then
                ! Find all grids
                inc_error = find_meshes_by_attributes(ncid, meshid2d, type_ugrid, meshid1d, networkid, network_geometryid)
                if (inc_error /= nf90_noerr) then
                    write (file_unit, 2540)
                    lncout = .false.
                    file_name_list(46) = ' '
                    call status%increase_error_count()
                endif

                if (lncout) then
                    if (meshid2d > 0) then
                        if (type_ugrid == type_ugrid_face_crds) then
                            inc_error = nf90_get_att(ncid, meshid2d, "mesh", mesh_name)
                        else if (type_ugrid == type_ugrid_node_crds) then
                            inc_error = nf90_inquire_variable(ncid, meshid2d, mesh_name)
                        endif
                        if (inc_error /= nf90_noerr) then
                            write (file_unit, 2535) trim(ugridfile)
                            write (file_unit, 2599) trim(nf90_strerror(inc_error))
                            call status%increase_error_count()
                            lncout = .false.
                        end if
                        write (file_unit, 2550) trim(mesh_name)
                    endif

                    if (meshid1d > 0) then
                        inc_error = nf90_inquire_variable(ncid, meshid1d, mesh_name)
                        if (inc_error /= nf90_noerr) then
                            write (file_unit, 2535) trim(ugridfile)
                            write (file_unit, 2599) trim(nf90_strerror(inc_error))
                            call status%increase_error_count()
                            lncout = .false.
                        end if
                        write (file_unit, 2551) trim(mesh_name)
                    endif
                endif
                ! Everything seems to be fine for now, switch on netcdf output
            endif

            ! Read number of computational volumes - if required
            if (.not. has_hydfile) then
                if (gettoken(noseg, local_status%ierr) > 0) goto 240
            endif

            ! TODO: check the number of segments with the information in the waqgeom-file
        else
            ! Or the number of computational volumes was already read
            if (.not. has_hydfile) then
                noseg = idummy
            endif
        end if

        if (noseg > 0) then
            write (file_unit, 2000) noseg
        else
            write (file_unit, 2010) noseg
            call status%increase_error_count()
        endif
        if (.not. alone) then
            if (noseg /= nosegp) then
                write (file_unit, 2015) nosegp
                call status%increase_error_count()
            endif
        endif

        ! Read optional multiple grids
        call read_multiple_grids(file_unit_list, noseg, notot, nototp, nolay, &
                gridps, nseg2, nogrid, syname, local_status)
        if (local_status%ierr > 0) goto 240

        ! Read grid-layout for visually printed output
        if (gettoken(cdummy, imopt1, itype, local_status%ierr) > 0) goto 240
        if (itype == 1) then
            if (cdummy == 'NONE') then
                write (file_unit, 2050)
                nx = 0
                ny = 0
            else
                write (file_unit, 2045) cdummy
                call status%increase_error_count()
            endif
        else
            write (file_unit, 2030) imopt1
            select case (imopt1)

            case (:-2, 3:)
                write (file_unit, 2040)
                call status%increase_error_count()
            case (2)
                write (file_unit, 2050)
                nx = 0
                ny = 0
            case default
                ! call with record length 0 => IMOPT1 of -4 not allowed
                call process_simulation_input_options (imopt1, file_unit_list, 6, file_name_list, filtype, &
                        is_date_format, is_yyddhh_format, 0, local_status%ierr, local_status, &
                        .false.)
                if (local_status%ierr > 0) goto 240
                if (gettoken(nx, local_status%ierr) > 0) goto 240
                if (gettoken(ny, local_status%ierr) > 0) goto 240
                write (file_unit, 2060) nx, ny
                if (imopt1 /= 0) then      !  else an adequate file was given
                    allocate (pgrid(nx, ny))
                    do j = 1, ny
                        do i = 1, nx
                            if (gettoken(pgrid(i, j), local_status%ierr) > 0) goto 240
                            if (pgrid(i, j) > noseg + nseg2) then
                                write (file_unit, 2070) pgrid(i, j)
                                call status%increase_error_count()
                            endif
                        enddo
                    enddo
                    if (output_verbose_level < 2) then
                        write (file_unit, 2080)
                    else
                        do i = 1, nx, 2 * iwidth
                            write (file_unit, 2090) (k, k = i, min(nx, i + 2 * iwidth - 1))
                            do j = 1, ny
                                write (file_unit, 2100) &
                                        j, (pgrid(k, j), k = i, min(nx, i + 2 * iwidth - 1))
                            enddo
                        enddo
                        if (nx * ny > 0) then
                            call open_waq_files  (file_unit_list(6), file_name_list(6), 6, 1, local_status%ierr)
                            write (file_unit_list(6)) pgrid
                            close (file_unit_list(6))
                        else
                            write (file_unit, 2050)
                        endif
                    endif
                    deallocate (pgrid)
                endif

            end select

        endif

        ! Attribute array
        ikerr = 0
        allocate (iamerge(noseg + nseg2))                        !   composite attribute array
        allocate (ikmerge(10))                        !   flags of filles attributes
        allocate (iread  (noseg + nseg2))                        !   work array to read the attributes
        iamerge = 0
        ikmerge = 0

        if (has_hydfile) then
            local_status%ierr = force_include_file(file_name_list(40))
            if (local_status%ierr /= 0) goto 240
        endif

        if (gettoken(nkopt, local_status%ierr) > 0) goto 240
        write (file_unit, 2110) nkopt                          !   so many blocks of input are provided

        do i = 1, nkopt                                      !   read those blocks

            if (gettoken(nopt, local_status%ierr) > 0) goto 240
            write (file_unit, 2120) nopt                           !   number of attributes in this block
            allocate (ikenm(nopt))
            do j = 1, nopt                                        !   get the attribute numbers
                if (gettoken(ikenm(j), local_status%ierr) > 0) goto 240
            enddo

            if (gettoken(ikopt1, local_status%ierr) > 0) goto 240      !   the file option for this info
            write (file_unit, 2130) ikopt1
            call process_simulation_input_options (ikopt1, file_unit_list, 40, file_name_list, filtype, &
                    is_date_format, is_yyddhh_format, 0, local_status%ierr, local_status, &
                    .false.)
            if (local_status%ierr  > 0) goto 240
            if (ikopt1 == 0) then                             !   binary file
                call open_waq_files  (file_unit_list(40), file_name_list(40), 40, 2, local_status%ierr)
                read  (file_unit_list(40), end = 250, err = 260) (iread(j), j = 1, noseg)
                close (file_unit_list(40))
            else
                if (gettoken(ikopt2, local_status%ierr) > 0) goto 240   !   second option
                write (file_unit, 2140) ikopt2

                select case (ikopt2)

                case (1)                                      !   no defaults
                    write (file_unit, 2150)
                    if (output_verbose_level >= 5) then
                        write (file_unit, 2160)
                    else
                        write (file_unit, 2170)
                    endif
                    do j = 1, noseg
                        if (gettoken(iread(j), local_status%ierr) > 0) goto 240
                        if (output_verbose_level >= 5) write (file_unit, 2180) j, iread(j)
                    enddo

                case (2)                                      !   default with overridings
                    write (file_unit, 2190)
                    if (gettoken(ikdef, local_status%ierr) > 0) goto 240
                    if (gettoken(nover, local_status%ierr) > 0) goto 240
                    write (file_unit, 2200)ikdef, nover
                    if (ikerr == 0) then                     !   only assign if no previous error
                        do iseg = 1, noseg
                            iread(iseg) = ikdef
                        enddo
                    endif
                    if (nover > 0) then
                        if (output_verbose_level >= 3) then
                            write (file_unit, 2210)
                        else
                            write (file_unit, 2220)
                        endif
                        do j = 1, nover
                            if (gettoken(iover, local_status%ierr) > 0) goto 240
                            if (gettoken(idummy, local_status%ierr) > 0) goto 240
                            if (iover < 1 .or. iover > noseg) then
                                write (file_unit, 2230) j, iover
                                call status%increase_error_count()
                            else
                                if (output_verbose_level >= 3) write (file_unit, 2240) j, iover, idummy
                                iread(iover) = idummy
                            endif
                        enddo
                    endif

                case default
                    write (file_unit, 2250)
                    call status%increase_error_count()
                end select

            endif

            ! Merge file buffer with attributes array in memory
            do iknm2 = 1, nopt
                iknm1 = ikenm(iknm2)

                ! see if merged already
                if (ikmerge(iknm1) /= 0) then
                    write (file_unit, 2260) iknm2, iknm1
                    call status%increase_error_count()
                    ikerr = 1
                    exit
                endif

                ! see if valid
                if (iknm1 <= 0 .or. iknm1 > 10) then
                    if (iknm1 == 0) then
                        write (file_unit, 2270) iknm2
                        call local_status%increase_warning_count()
                    else
                        write (file_unit, 2280) iknm1, iknm2
                        call local_status%increase_warning_count()
                    endif
                    cycle                    !  skip
                endif

                ! Merge for this attribute
                write (file_unit, 2290) iknm2, iknm1
                ikmerge(iknm1) = 1
                iknmrk = 10**(iknm1 - 1)
                do iseg = 1, noseg
                    call extract_waq_attribute(iknm2, iread(iseg), ivalk)
                    iamerge(iseg) = iamerge(iseg) + iknmrk * ivalk
                enddo
            end do
            deallocate (ikenm)
        end do

        ! Time dependent attributes
        if (gettoken(ikopt2, local_status%ierr) > 0) goto 240
        write (file_unit, 2300) ikopt2

        if (ikopt2 == 1) then                                !   this file
            write (file_unit, 2310)
            if (gettoken(nopt, local_status%ierr) > 0) goto 240
            write (file_unit, 2120) nopt
            do j = 1, nopt
                if (gettoken(iknm1, local_status%ierr) > 0) goto 240
                if (iknm1 <= 0 .or. iknm1 > 10) then
                    if (iknm1 == 0) then
                        write (file_unit, 2270) j, iknm1
                        call local_status%increase_warning_count()
                    else
                        write (file_unit, 2280) iknm2, iknm1
                        call local_status%increase_warning_count()
                    endif
                    cycle
                endif

                ! Merge for this attribute is performed in DELWAQ2
                if (ikmerge(iknm1) == 0) then
                    write (file_unit, 2290) iknm2, iknm1
                    ikmerge(iknm1) = 1
                else
                    write (file_unit, 2260) iknm2, iknm1
                    call status%increase_error_count()
                    ikerr = 1
                endif
            enddo

            ifiopk = 0
            if (gettoken(ikopt1, local_status%ierr) > 0) goto 240
            write (file_unit, 2130) ikopt1
            call process_simulation_input_options(ikopt1, file_unit_list, 40, file_name_list, filtype, &
                    is_date_format, is_yyddhh_format, 0, local_status%ierr, local_status, &
                    .false.)
            if (local_status%ierr > 0) goto 240
            if (ikopt1 == 0) then
                write (file_unit, 2320)
                ifiopk = 1
            elseif(ikopt1 == -2) then
                write (file_unit, 2330)
                ifiopk = 2
            else
                write (file_unit, 2340)
                call status%increase_error_count()
            endif
        else
            write (file_unit, 2350)
        endif

        ! Set default behaviour if not specified
        if (ikmerge(1) == 0  .and. ikerr == 0) then
            write (file_unit, 2360)
            iamerge = iamerge + 1
        endif
        if (ikmerge(2) == 0) write (file_unit, 2370)
        if (nseg2      > 0) write (file_unit, 2380) nseg2
        do i = noseg + 1, noseg + nseg2      ! bottom segments are 3 - always active!
            iamerge(i) = (iamerge(i) / 10) * 10 + 3
        enddo

        ! Write to file
        if (ikerr == 0) write(file_unit_list(2)) iamerge
        deallocate (ikmerge, iamerge)

        ! read segment volumes
        write (file_unit, 2390)
        local_status%ierr = 0

        call read_constants_time_variables   (file_unit_list, 7, 0, 0, noseg, &
                1, 1, nrftot(2), nrharm(2), ifact, &
                is_date_format, disper, volume, iwidth, file_name_list, &
                filtype, is_yyddhh_format, output_verbose_level, local_status%ierr, &
                local_status, has_hydfile)

        call check_volume_time(file_unit, file_name_list(7), noseg, local_status%ierr)

        if (.not. alone) then
            if (file_name_list(7) /= fnamep(6)) then
                write (file_unit, 2395) fnamep(6)
                call status%increase_error_count()
            endif
        endif
        if (volume ==  0) ivflag = 1
        if (volume == -1) ivflag = 2
        call status%increase_error_count_with(local_status%ierr)
        local_status%ierr = 0
        240 continue
        if (local_status%ierr > 0) call status%increase_error_count()
        if (local_status%ierr == 3) call stop_with_error()
        goto 270

        ! error processing
        250 write (file_unit, 2400) file_unit_list(40), file_name_list(40)
        call status%increase_error_count()
        goto 270

        260 write (file_unit, 2410) file_unit_list(40), file_name_list(40)
        call status%increase_error_count()

        270 call check_error(cdummy, iwidth, 3, local_status%ierr, status)
        call status%increase_warning_count_with(local_status%iwar)
        if (timon) call timstop(ithndl)
        return

        2000 format (//' Number of segments :', I15)
        2010 format (/ ' ERROR, invalid number of segments:', I10)
        2015 format (' ERROR, nr of volumes in Delwaq not equal to nr of volumes in Delpar:', I10)
        2030 format (/ ' option selected for grid layout :', I2)
        2040 format (/ ' ERROR, option for grid layout not implemented !!!!!')
        2045 format (/ ' ERROR, invalid keyword for grid layout: ', A20)
        2050 format (' Grid layout not used.')
        2060 format (/ ' Grid layout, width :', I4, ' depth :', I4)
        2070 format (/ ' ERROR, invalid number in grid layout:', I6)
        2080 format (' Information on gridlayout will be printed for output option 2 and higher ')
        2090 format (10X, 20I6, /)
        2100 format (I6, 4X, 20I6)
        2110 format (/, I3, ' blocks of time independent attribute contribution')
        2120 format (/, I3, ' attributes in this block')
        2130 format (/ ' Selected file option is ', I3)
        2140 format (/ ' second selected option', I3)
        2150 format (/ ' constant values without defaults')
        2160 format ('        segment   value ')
        2170 format (' Information will be printed for output option is ', &
                '5 or higher !')
        2180 format ('       ', I6, 1X, I9.9)
        2190 format (/ ' constant values with defaults and overridings')
        2200 format (/ ' Default value :', I9.9, &
                / ' number of overridings :', I6)
        2210 format ('        number   segment   value ')
        2220 format (' Information will be printed for output option is ', &
                '3 or higher !')
        2230 format (' ERROR ', I6, ' ', I6, ' segment number out of range')
        2240 format ('       ', I6, ' ', I6, ' ', I9.9)
        2250 format (/ ' ERROR, option out of range.')
        2260 format (/ ' ERROR, Pointer of contribution ', I6, ' mapped to', &
                ' attribute', I6, ' already specified.')
        2270 format (/ ' WARNING, Pointer of contribution ', I6, ' zero,', &
                ' attribute skipped.')
        2280 format (/ ' WARNING, Pointer of contribution ', I6, '=', I6, &
                ' is out of range(1-10), attribute skipped')
        2290 format (/ ' Pointer of contribution ', I6, ' mapped to', &
                ' attribute', I6)
        2300 format (/ ' option for time dependent attributes ', I3)
        2310 format (' time dependent attributes in binary file')
        2320 format (' Binary file with one record per time step.')
        2330 format (' Block data with breakpoints in binary file.')
        2340 format (' ERROR, file option not allowed for this item.')
        2350 format (' NO time dependent attributes')
        2360 format (' No first attribute set, using default 1')
        2370 format (' No second attribute set, using default 0')
        2380 format (' Attribute of', I7, ' bottom segments set to 2')
        2390 format (/ ' Volumes:')
        2395 format (/ ' ERROR, volumes for Delpar from different file : ', A20)
        2400 format (/ ' ERROR, end of file on unit:', I3, / ' Filename: ', A20)
        2410 format (/ ' ERROR, reading file on unit:', I3, / ' Filename: ', A20)
        2450 format (/ ' Found HYD_FILE keyword')
        2460 format (/ ' Retrieving file names and grid parameters from: ', A)
        2500 format (/ ' Found UGRID keyword')
        2510 format (/ ' File containing the mesh(es): ', A)
        2511 format (/ ' Warning: the file does not exist - turning NetCDF output off')
        2520 format (/ ' NetCDF version: ', A)
        2530 format (/ ' ERROR, opening NetCDF file. Filename: ', A)
        2535 format (/ ' ERROR, unable to retrieve mesh name from file:', A)
        2540 format (/ ' ERROR, no mesh(es) found with required attribute "delwaq_role" or "cf_role"' &
                / '        this version of Delwaq is not compatible with older non-ugrid waqgeom-files')
        2550 format (/ ' Mesh used for Delwaq 2D/3D output: ', A)
        2551 format (/ ' Mesh used for Delwaq 1D output: ', A)
        2599 format (/ ' NetCDF error message: ', A)

    end subroutine read_block_3_grid_layout

    subroutine check_volume_time(file_unit, filvol, noseg, ierr2)
        !! Check the contents of the volumes file: id the time step compatible?
        use m_sysi          ! Timer characteristics

        integer(kind = int_wp), intent(in) :: file_unit       !< LU-number of the report file
        character(len = *), intent(in) :: filvol     !< Name of the volumes file to be checked
        integer(kind = int_wp), intent(in) :: noseg       !< Number of segments
        integer(kind = int_wp), intent(out) :: ierr2       !< Whether an error was found or not

        integer(kind = int_wp) :: i, ierr
        integer(kind = int_wp) :: luvol
        integer(kind = int_wp) :: time1, time2, time3
        real(kind = real_wp) :: dummy
        character(len = 14) :: string

        open(newunit = luvol, file = filvol, access = 'stream', &
                status = 'old', iostat = ierr)

        ! The existence has already been checked, if the file does
        ! not exist, skip the check
        if (ierr /= 0) then
            return
        endif

        ! For "steering files", we need an extra check
        ! - skip the check on the times though
        !
        ! Ignore the error condition - it might occur with
        ! very small models (one or two segments, for instance)
        read(luvol, iostat = ierr) string
        if (string == 'Steering file ') then
            return
        endif

        ! Regular volume files
        read(luvol, iostat = ierr, pos = 1) time1, (dummy, i = 1, noseg)
        if (ierr /= 0) then
            ierr2 = ierr2 + 1
            write (file_unit, 110) ierr
            return
        endif
        read(luvol, iostat = ierr) time2, (dummy, i = 1, noseg)
        if (ierr /= 0) then
            write (file_unit, 120)
            return
        endif
        read(luvol, iostat = ierr) time3, (dummy, i = 1, noseg)
        if (ierr /= 0) then
            write (file_unit, 130)
            return
        endif

        ! The times must be increasing and the intervals must be the same
        if (time1 >= time2 .or. time2 >= time3) then
            ierr2 = ierr2 + 1
            write (file_unit, 140) time1, time2, time3
            return
        endif
        if ((time2 - time1) /= (time3 - time2)) then
            ierr2 = ierr2 + 1
            write (file_unit, 150) time1, time2, time3
            return
        endif
        if (mod((time2 - time1), idt) /= 0) then
            ierr2 = ierr2 + 1
            write (file_unit, 160) time1, time2, time3, idt
            return
        endif

        110 format(' ERROR: the volumes file seems to be too small' &
                /, '        Error code: ', i0)
        120 format(' NOTE: the volumes file appears to hold one record only')
        130 format(' NOTE: the volumes file appears to hold two records only' &
                )
        140 format(' ERROR: the times in the volumes file are not monotonical &
                ly increasing', /, ' Successive times: ', 3i12)
        150 format(' ERROR: the times in the volumes file are not equidistant &
                ', /, ' Successive times: ', 3i12)
        160 format(' ERROR: the time step does not divide the time interval i &
                n the volumes file', &
                /, ' Successive times in the volumes file: ', 3i12, &
                /, 'Time step for water quality: ', i12)

    end subroutine check_volume_time
end module inputs_block_3
