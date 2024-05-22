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
module m_outhnc
    use m_waq_precision

    implicit none

contains


    subroutine outhnc (ncidhis, hncnam, ugridf, timeid, bndtimeid, &
            hncrec, itime, moname, idump, duname, &
            nodump, notot1, conc1, synam1, sysnm1, &
            syuni1, sydsc1, wqid1, notot2, conc2, &
            synam2, sysnm2, syuni2, sydsc2, wqid2, &
            lunut)

        !     Deltares Software Centre

        !     Function            : Writes history output to NetCDF

        use m_universally_unique_id_generator
        use m_logger, only : terminate_execution
        use timers
        use dlwq_netcdf  !   read/write grid in netcdf
        use results, only : ncopt
        implicit none

        !     Parameters          :

        !     kind           function         name                    description

        integer(kind = int_wp), intent(inout) :: ncidhis              ! NetCDF id of output history file
        character(255), intent(in) :: hncnam               ! name NetCDF output history file
        character(255), intent(in) :: ugridf               ! name of NetCDF ugrid file
        integer(kind = int_wp), intent(inout) :: timeid
        integer(kind = int_wp), intent(inout) :: bndtimeid
        integer(kind = int_wp), intent(in) :: hncrec               ! present record in NetCDF file
        integer(kind = int_wp), intent(in) :: itime                ! present time in clock units
        character(40), intent(in) :: moname(4)            ! model identification
        integer(kind = int_wp), intent(in) :: idump(nodump)        ! segment number of monitoring points and areas
        character(*), intent(in) :: duname(nodump)       ! names of monitoring points and areas
        integer(kind = int_wp), intent(in) :: nodump               ! number of monitoring points and areas
        integer(kind = int_wp), intent(in) :: notot1               ! number of variables in conc1
        real(kind = real_wp), intent(in) :: conc1 (notot1, *)     ! values
        character(20), intent(in) :: synam1(notot1)       ! names of variables in conc1
        character(100), intent(in) :: sysnm1(notot1)       ! standard names of variables in conc1
        character(40), intent(in) :: syuni1(notot1)       ! units of variables in conc1
        character(60), intent(in) :: sydsc1(notot1)       ! decriptions of variables in conc1
        integer(kind = int_wp), intent(inout) :: wqid1(notot1, 2)      ! NetCDF ids of variables in conc1
        integer(kind = int_wp), intent(in) :: notot2               ! number of variables in conc2
        real(kind = real_wp), intent(in) :: conc2 (notot2, nodump)! values
        character(20), intent(in) :: synam2(notot2)       ! names of variables in conc2
        character(100), intent(in) :: sysnm2(notot2)       ! standard names of variables in conc2
        character(40), intent(in) :: syuni2(notot2)       ! units of variables in conc2
        character(60), intent(in) :: sydsc2(notot2)       ! decriptions of variables in conc2
        integer(kind = int_wp), intent(inout) :: wqid2(notot2, 2)      ! NetCDF ids of variables in conc2
        integer(kind = int_wp), intent(in) :: lunut                ! unit number monitoring file

        character(len = len(synam1)) :: name

        integer(kind = int_wp) :: iseg                   ! loop counter for segments
        integer(kind = int_wp) :: k                      ! loop counter for substances
        real(kind = real_wp) :: missing_value = -999.0       ! missing value indicator

        integer(kind = int_wp) :: ncid
        integer(kind = int_wp) :: varid, varidout, meshid, meshidout, ntimeid, wqid, noseglmesh, nolaymesh
        integer(kind = int_wp) :: nostations_id, name_length_id
        integer(kind = int_wp) :: inc_error, ierr, nolay, iout
        integer(kind = int_wp) :: xtype
        integer(kind = int_wp) :: ndims
        logical, allocatable :: sumconc1(:), sumconc2(:)
        integer(kind = int_wp), dimension(nf90_max_var_dims) :: dimids
        integer(kind = int_wp), dimension(nf90_max_dims) :: dimsizes
        !      integer(kind=int_wp) ::naggr
        integer(kind = int_wp), allocatable :: aggr

        integer(kind = int_wp) :: values(8)
        character(len = 40) :: timestamp
        character(len = 40) :: t0string
        character(len = 40) :: uuid

        character(len = nf90_max_name) :: mesh_name
        character(len = nf90_max_name) :: dimname

        integer(kind = int_wp) :: i, j, id, cnt, errcnt
        integer(kind = int_wp) :: type_ugrid
        logical :: success
        real(kind = real_wp), dimension(:), allocatable :: dlwq_values
        character(len = nf90_max_name) :: altname

        integer(kind = int_wp) :: station_names_id, station_x_id, station_y_id, station_z_id

        integer(kind = int_wp), dimension(3) :: coord_id
        character(len = 25), dimension(5, 4) :: station_property = reshape(&
                [ 'variableName             ', 'standard_name            ', &
                        'long_name                ', 'unit                     ', 'units                    ', &
                        'station_x                ', 'projection_x_coordinate  ', &
                        'x-coordinate             ', 'm                        ', 'm                        ', &
                        'station_y                ', 'projection_y_coordinate  ', &
                        'y-coordinate             ', 'm                        ', 'm                        ', &
                        'station_z                ', 'projection_z_coordinate  ', &
                        'z-coordinate             ', 'm                        ', 'm                        '], &
                [5, 4])

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("outhnc", ithandl)

        !     Check if there are any monitoring points/areas
        !     (If the number is zero and we would not suppress the creation of the file,
        !     then we would get a run-time error from NetCDF - apparently 0 is the magic
        !     number for the unlimited-size dimension and there can be only one of those)

        if (nodump == 0) then
            goto 900
        endif

        !     Initialize file
        if (ncidhis < 0) then

            ! Turn on debug info from dlwaqnc
            inc_error = dlwqnc_debug_status(.true.)

            ! Prepare a Delwaq-NetCDF output-file for history data from the UGRID-file
            ! To do: we should check if everything went right, if not, NetCDF output is not possible...

            ! Write the version of the netcdf library
            write (lunut, 2520) trim(nf90_inq_libvers())


            ! Create the new file
            if (ncopt(1) == 4) then
                inc_error = nf90_create(hncnam, ior(nf90_clobber, nf90_netcdf4), ncidhis)
            else
                inc_error = nf90_create(hncnam, ior(nf90_clobber, nf90_format_classic), ncidhis)
            endif
            if (inc_error /= nf90_noerr) then
                write (lunut, 2560) trim(hncnam)
                goto 800
            endif

            ! Generate the UUID and store it as an attibute
            call generate_uuid(uuid)
            inc_error = nf90_put_att(ncidhis, nf90_global, "uuid", uuid)
            if (inc_error /= nf90_noerr) then
                write (lunut, 2571)
                goto 800
            endif

            ! Update the timestamp
            call date_and_time(values = values)
            write(timestamp, '(i4.4,a,i2.2,a,i2.2, a,i2.2,a,i2.2,a,i2.2,a,f5.3,a,i2.2,a,i2.2)') &
                    values(1), '-', values(2), '-', values(3), 'T', &
                    values(5), ':', values(6), ':', values(7), ':', values(8) / 1000.0, &
                    merge('+', '-', values(4)>=0), values(4) / 60, ':', mod(values(4), 60)

            inc_error = nf90_put_att(ncidhis, nf90_global, 'date_created', trim(timestamp))
            if (inc_error /= nf90_noerr) then
                write (lunut, 2573)
                goto 800
            endif
            inc_error = nf90_put_att(ncidhis, nf90_global, 'date_modified', trim(timestamp))
            if (inc_error /= nf90_noerr) then
                write (lunut, 2574)
                goto 800
            endif

            ! Create the dimensions (except time - that is done later)
            !
            inc_error = nf90_def_dim(ncidhis, "nStations", nodump, nostations_id)
            if (inc_error /= nf90_noerr) then
                write (lunut, 2575)
                goto 800
            endif
            inc_error = nf90_def_dim(ncidhis, "name_len", 20, name_length_id)
            if (inc_error /= nf90_noerr) then
                write (lunut, 2575)
                goto 800
            endif

            ! Create the variables for the station properties
            !
            inc_error = nf90_def_var(ncidhis, "station_name", nf90_char, (/ name_length_id, nostations_id /), station_names_id)
            if (inc_error /= nf90_noerr) then
                write(lunut, 2580) 'station names'
                goto 800
            endif
            inc_error = &
                    0 + nf90_put_att(ncidhis, station_names_id, &
                            'long_name', 'monitoring point/area')
            inc_error = &
                    inc_error + nf90_put_att(ncidhis, station_names_id, &
                            'cf_role', 'timeseries_id')
            if (inc_error /= 2 * nf90_noerr) then
                write(lunut, 2586) 'station_name'
                goto 800
            endif

            do i = 2, 4
                inc_error = nf90_def_var(ncidhis, station_property(1, i), nf90_double, (/ nostations_id /), coord_id(i - 1))
                if (inc_error /= nf90_noerr) then
                    write(lunut, 2580) station_property(1, i)
                    goto 800
                endif

                do j = 2, 5
                    inc_error = nf90_put_att(ncidhis, coord_id(i - 1), station_property(j, 1), station_property(j, i))
                    if (inc_error /= nf90_noerr) then
                        write(lunut, 2586) station_property(j, i)
                        goto 800
                    endif
                enddo
            enddo

            inc_error = nf90_enddef(ncidhis)
            if (inc_error /= nf90_noerr) then
                write (lunut, 2566)
                goto 800
            endif

            mesh_name = 'history'
            t0string = moname(4)
            inc_error = dlwqnc_create_wqtime(ncidhis, t0string, timeid, bndtimeid, ntimeid)
            if (inc_error /= nf90_noerr) then
                write(lunut, 2581)
                goto 800
            endif

            ! Write information on the stations to NetCDF-file (we are in data mode ...)
            !
            inc_error = nf90_put_var(ncidhis, station_names_id, duname)
            if (inc_error /= nf90_noerr) then
                write(lunut, 2587) 'names'
                goto 800
            endif
            do i = 2, 4
                inc_error = nf90_put_var(ncidhis, coord_id(i - 1), [(0.0d0, j = 1, nodump)])  ! TODO; FOR NOW
                if (inc_error /= nf90_noerr) then
                    write(lunut, 2587) station_property(1, i)
                    goto 800
                endif
            enddo

            ! Back to definition mode to define the actual variables
            !
            inc_error = nf90_redef(ncidhis)
            if (inc_error /= nf90_noerr) then
                write (lunut, 2565)
                goto 800
            endif

            ! Write output variables and proces library info to NetCDF-file
            ! long name and unit will follow later, they are in the ouput.wrk-file!
            !
            do iout = 1, notot1
                name = nospaces(synam1(iout))
                inc_error = nf90_def_var(ncidhis, trim(name), nf90_float, &
                        [nostations_id, ntimeid], wqid1(iout, 1))
                if (inc_error /= nf90_noerr) then
                    if (inc_error /= nf90_enameinuse) then
                        write(lunut, 2582)
                        goto 800
                    else
                        success = .false.
                        do cnt = 2, 100
                            write(altname, '(i0,2a)') cnt, '-', name
                            inc_error = nf90_def_var(ncidhis, trim(altname), nf90_float, &
                                    [nostations_id, ntimeid], wqid1(iout, 1))
                            if (inc_error == nf90_noerr) then
                                success = .true.
                                exit
                            endif
                        enddo
                        if (.not. success) then
                            write(lunut, 2582)
                            goto 800
                        endif
                    endif
                endif

                errcnt = 6
                inc_error = &
                        0 + nf90_put_att(ncidhis, wqid1(iout, 1), &
                                '_FillValue', -999.0)
                inc_error = &
                        inc_error + nf90_put_att(ncidhis, wqid1(iout, 1), &
                                'coordinates', &
                                'station_x station_y station_z station_name')
                inc_error = &
                        inc_error + nf90_put_att(ncidhis, wqid1(iout, 1), &
                                'delwaq_name', trim(synam1(iout)))
                inc_error = &
                        inc_error + nf90_put_att(ncidhis, wqid1(iout, 1), &
                                'long_name', trim(sydsc1(iout)))
                if (len_trim(sysnm1(iout)) > 0) then
                    errcnt = errcnt + 1
                    inc_error = &
                            inc_error + nf90_put_att(ncidhis, wqid1(iout, 1), &
                                    'standard_name', trim(sysnm1(iout)))
                endif
                inc_error = &
                        inc_error + nf90_put_att(ncidhis, wqid1(iout, 1), &
                                'unit', syuni1(iout))
                inc_error = &
                        inc_error + nf90_put_att(ncidhis, wqid1(iout, 1), &
                                'units', syuni1(iout))
                if (inc_error /= errcnt * nf90_noerr) then
                    write(lunut, 2586) 'substance ' // synam1(iout)
                    goto 800
                endif

            enddo

            do iout = 1, notot2
                name = nospaces(synam2(iout))
                inc_error = nf90_def_var(ncidhis, trim(name), nf90_float, &
                        [nostations_id, ntimeid], wqid2(iout, 1))
                if (inc_error /= nf90_noerr) then
                    if (inc_error /= nf90_enameinuse) then
                        write(lunut, 2582)
                        goto 800
                    else
                        success = .false.
                        do cnt = 2, 100
                            write(altname, '(i0,2a)') cnt, '-', name
                            inc_error = nf90_def_var(ncidhis, trim(altname), nf90_float, &
                                    [nostations_id, ntimeid], wqid2(iout, 1))
                            if (inc_error == nf90_noerr) then
                                success = .true.
                                exit
                            endif
                        enddo
                        if (.not. success) then
                            write(lunut, 2582)
                            goto 800
                        endif
                    endif
                endif

                errcnt = 6
                inc_error = &
                        0 + nf90_put_att(ncidhis, wqid2(iout, 1), &
                                '_FillValue', -999.0)
                inc_error = &
                        inc_error + nf90_put_att(ncidhis, wqid2(iout, 1), &
                                'coordinates', &
                                'station_x station_y station_z station_name')
                inc_error = &
                        inc_error + nf90_put_att(ncidhis, wqid2(iout, 1), &
                                'delwaq_name', trim(synam2(iout)))
                inc_error = &
                        inc_error + nf90_put_att(ncidhis, wqid2(iout, 1), &
                                'long_name', trim(sydsc2(iout)))
                if (len_trim(sysnm2(iout)) > 0) then
                    errcnt = errcnt + 1
                    inc_error = &
                            inc_error + nf90_put_att(ncidhis, wqid2(iout, 1), &
                                    'standard_name', trim(sysnm2(iout)))
                endif
                inc_error = &
                        inc_error + nf90_put_att(ncidhis, wqid2(iout, 1), &
                                'unit', syuni2(iout))
                inc_error = &
                        inc_error + nf90_put_att(ncidhis, wqid2(iout, 1), &
                                'units', syuni2(iout))
                if (inc_error /= errcnt * nf90_noerr) then
                    write(lunut, 2586) 'substance ' // synam2(iout)
                    goto 800
                endif
            enddo

            ! Flush after first stage of preparing NetCDF file
            inc_error = nf90_enddef(ncidhis)
            if (inc_error /= nf90_noerr) then
                write(lunut, 2591)
                goto 800
            endif
            inc_error = nf90_sync(ncidhis)
            if (inc_error /= nf90_noerr) then
                write(lunut, 2591)
                goto 800
            endif
        endif

        !     Perform output

        allocate(dlwq_values(nodump))

        !     New time record
        inc_error = dlwqnc_write_wqtime(ncidhis, timeid, bndtimeid, hncrec, itime)
        if (inc_error /= nf90_noerr) then
            if (inc_error /= nf90_noerr) then
                write(lunut, 2590)
                goto 800
            endif
        endif

        !     First set of output
        do iout = 1, notot1
            do id = 1, nodump
                dlwq_values(id) = conc1(iout, idump(id))
            enddo

            inc_error = dlwqnc_write_wqvariable(ncidhis, wqid1(iout, 1), hncrec, dlwq_values)
            if (inc_error /= nf90_noerr) then
                write(lunut, 2591)
                goto 800
            endif
        enddo

        !     Second set of output
        do iout = 1, notot2
            do id = 1, nodump
                dlwq_values(id) = conc2(iout, id)
            enddo

            inc_error = dlwqnc_write_wqvariable(ncidhis, wqid2(iout, 1), hncrec, dlwq_values)
            if (inc_error /= nf90_noerr) then
                write(lunut, 2591)
                goto 800
            endif
        enddo

        deallocate(dlwq_values)

        ! Flush after each map write
        inc_error = nf90_sync(ncidhis)
        if (inc_error /= nf90_noerr) then
            write(lunut, 2591)
            goto 800
        endif
        goto 900

        800  continue
        ! There were errors!
        write (lunut, 2600) inc_error
        write (lunut, 2610) trim(nf90_strerror(inc_error))
        call terminate_execution (1)

        900  continue
        if (timon) call timstop (ithandl)
        return

        2520 format (/ ' History file - NetCDF version: ', A)
        2560 format (/ ' Creating the output file failed. Filename:', A)
        2565 format (/ ' ERROR: Reopening NetCDF definition failed')
        2566 format (/ ' ERROR: Closing NetCDF definition failed')
        2571 format (/ ' Writing the UUID failed')
        2573 format (/ ' Writing date_created failed')
        2574 format (/ ' Writing date_modified failed')
        2575 format (/ ' Creating dimensions failed')
        2580 format (/ ' Creating station information failed - ', a)
        2581 format (/ ' Creating time dimension failed')
        2582 format (/ ' Creating variable failed')
        2586 format (/ ' Adding attribute failed - ', a)
        2587 format (/ ' Writing station information failed - ', a)
        2590 format (/ ' Writing new NetCDF history time failed')
        2591 format (/ ' Writing new NetCDF history output data failed')
        2600 format (/ ' NetCDF error number: ', I6)
        2610 format (/ ' NetCDF error message: ', A)

    contains

        ! Replace embedded spaces by underscores (and remove leading spaces)
        !
        function nospaces(string)

            character(len = *), intent(in) :: string
            character(len = len(string)) :: nospaces

            integer(kind = int_wp) :: i

            nospaces = adjustl(string)
            do i = 1, len_trim(nospaces)
                if (nospaces(i:i) == ' ') then
                    nospaces(i:i) = '_'
                endif
            enddo

        end function nospaces
    end

end module m_outhnc
