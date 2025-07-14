!!  Copyright (C)  Stichting Deltares, 2012-2025.
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

module m_write_netcdf_output
    use m_waq_precision
    use m_logger_helper, only: stop_with_error
    use m_universally_unique_id_generator, only: generate_uuid
    use m_string_manipulation, only: replace_space_by_underscore

    implicit none

    ! Hint:
    ! There are a lot of variables passed from the netcdf module to this module then from this module out.
    ! these variables also needs to be declared as public here
    !    private
    !    public :: write_netcdf_map_output, write_netcdf_history_output

contains

    subroutine write_netcdf_map_output(ncidmap, mncnam, ugridf, timeid, bndtimeid, mncrec, itime, moname, &
            num_cells, notot1, conc1, synam1, sysnm1, syuni1, sydsc1, wqid1, notot2, &
            conc2, synam2, sysnm2, syuni2, sydsc2, wqid2, volume, iknmrk, lunut)
        !! Writes map output to NetCDF

        use timers
        use waq_netcdf_utils
        use delwaq_version_module
        use results, only: ncopt

        integer(kind = int_wp), intent(inout) :: ncidmap              ! NetCDF id of output map file
        character(255), intent(in) :: mncnam               ! name NetCDF output map file
        character(255), intent(in) :: ugridf               ! name of NetCDF ugrid file
        integer(kind = int_wp), intent(inout) :: timeid
        integer(kind = int_wp), intent(inout) :: bndtimeid
        integer(kind = int_wp), intent(in) :: mncrec               ! present record in NetCDF file
        integer(kind = int_wp), intent(in) :: itime                ! present time in clock units
        character(40), intent(in) :: moname(4)            ! model identification
        integer(kind = int_wp), intent(in) :: num_cells                ! number of computational volumes
        integer(kind = int_wp), intent(in) :: notot1               ! number of variables in conc1
        real(kind = real_wp), intent(in) :: conc1 (notot1, num_cells) ! values
        character(20), intent(in) :: synam1(notot1)       ! names of variables in conc1
        character(100), intent(in) :: sysnm1(notot1)       ! standard names of variables in conc1
        character(40), intent(in) :: syuni1(notot1)       ! units of variables in conc1
        character(60), intent(in) :: sydsc1(notot1)       ! decriptions of variables in conc1
        integer(kind = int_wp), intent(inout) :: wqid1(notot1, 3)      ! NetCDF ids of variables in conc1
        integer(kind = int_wp), intent(in) :: notot2               ! number of variables in conc2
        real(kind = real_wp), intent(in) :: conc2 (notot2, num_cells) ! values
        character(20), intent(in) :: synam2(notot2)       ! names of variables in conc2
        character(100), intent(in) :: sysnm2(notot2)       ! standard names of variables in conc1
        character(40), intent(in) :: syuni2(notot2)       ! units of variables in conc1
        character(60), intent(in) :: sydsc2(notot2)       ! decriptions of variables in conc1
        integer(kind = int_wp), intent(inout) :: wqid2(notot2, 3)      ! NetCDF ids of variables in conc1
        real(kind = real_wp), intent(in) :: volume(num_cells)        ! values
        integer(kind = int_wp), intent(in) :: iknmrk(num_cells)        ! Feature array. Bit zero set means active.
        integer(kind = int_wp), intent(in) :: lunut                ! unit number monitoring file

        integer(kind = int_wp) :: iseg                         ! loop counter for segments
        integer(kind = int_wp) :: k                            ! loop counter for substances
        real(kind = real_wp), parameter :: missing_value = -999.0 ! missing value indicator

        integer(kind = int_wp) :: ncid
        integer(kind = int_wp) :: varid, varidout, meshidout, ntimeid, wqid, noseglmesh2d, nosegmesh2d3d, nosegmesh1d, num_layers
        integer(kind = int_wp) :: meshid2d, meshid1d, networkid, network_geometryid
        integer(kind = int_wp) :: inc_error, ierr, iout
        integer(kind = int_wp) :: xtype
        integer(kind = int_wp) :: ndims
        logical, allocatable :: sumconc1(:), sumconc2(:)
        integer(kind = int_wp), dimension(nf90_max_var_dims) :: dimids
        integer(kind = int_wp), dimension(nf90_max_dims) :: dimsizes
        !      integer(kind=int_wp) ::naggr
        integer(kind = int_wp), dimension(:, :), allocatable :: aggr2d3d
        integer(kind = int_wp), dimension(:), allocatable :: aggr1d

        integer(kind = int_wp) :: values(8)
        character(len = 40) :: timestamp
        character(len = 40) :: t0string
        character(len = 40) :: uuid
        character(len = 80) :: versionid

        character(len = nf90_max_name) :: mesh_name2d
        character(len = nf90_max_name) :: mesh_name1d
        character(len = nf90_max_name) :: network_name
        character(len = nf90_max_name) :: network_geometry_name
        character(len = nf90_max_name) :: dimname
        character(len = nf90_max_name) :: history

        integer(kind = int_wp) :: i, j
        integer(kind = int_wp) :: type_ugrid
        integer(kind = int_wp) :: noseglid2d, nolayid, nosegid1d
        integer(kind = int_wp) :: isegl, ilay
        integer(kind = int_wp) :: wqidvolume_2d3d, wqidvolume_2d, wqidvolume_1d
        real(kind = real_wp), dimension(:), allocatable :: laythickness
        real(kind = real_wp), dimension(:, :), allocatable :: dlwq_values_2d3d
        real(kind = real_wp), dimension(:), allocatable :: dlwq_values_2d, dlwq_volume_2d, dlwq_values_1d

        save ncid, noseglmesh2d, num_layers, nosegmesh1d, varid, meshid2d, meshid1d, networkid, network_geometryid
        save nosegmesh2d3d, aggr2d3d, aggr1d, wqidvolume_2d3d, wqidvolume_2d, wqidvolume_1d
        save sumconc1, sumconc2

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("write_netcdf_map_output", ithandl)

        !     Initialize file
        if (ncidmap < 0) then

            ! Turn on debug info from dlwaqnc
            inc_error = set_dlwqnc_debug_status(.true.)

            ! Prepare a Delwaq-NetCDF output-file for map data from the UGRID-file
            ! To do: we should check if everything went right, if not, NetCDF output is not possible...

            ! Write the version of the netcdf library
            write (lunut, 2520) trim(nf90_inq_libvers())

            ! Open the ugrid-file file
            inc_error = nf90_open(ugridf, nf90_nowrite, ncid)
            if (inc_error /= nf90_noerr) then
                write (lunut, 2530) trim(ugridf)
                goto 800
            end if

            inc_error = read_dimensions(ncid, dimsizes)
            if (inc_error /= nf90_noerr) then
                write (lunut, 2531) trim(ugridf)
                goto 800
            end if

            ! Copy the global attributes to a new file
            if (ncopt(1) == 4) then
                inc_error = nf90_create(mncnam, ior(nf90_clobber, nf90_netcdf4), ncidmap)
            else
                inc_error = nf90_create(mncnam, ior(nf90_clobber, nf90_format_classic), ncidmap)
            endif
            if (inc_error /= nf90_noerr) then
                write (lunut, 2560) trim(mncnam)
                goto 800
            endif

            ! We don't assume variable names, but we try to find all meshes by their attributes using
            ! find_meshes_by_attributes
            inc_error = find_meshes_by_attributes(ncid, meshid2d, type_ugrid, meshid1d, networkid, network_geometryid)
            if (inc_error /= nf90_noerr) then
                write (lunut, 2540)
                goto 800
            endif

            ! Find the 1D mesh name and number of 1D segments when available
            mesh_name1d = ' '
            nosegmesh1d = 0
            if (meshid1d > 0) then
                ! Find the 1D mesh name
                inc_error = nf90_inquire_variable(ncid, meshid1d, mesh_name1d)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2535) trim(mncnam)
                    goto 800
                end if
                ! get number of segments from dimension mentiond in the attribute node_dimension (e.g. "mesh1d_nNodes")
                inc_error = nf90_get_att(ncid, meshid1d, 'node_dimension', dimname)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2593)
                    goto 800
                endif
                inc_error = nf90_inq_dimid(ncid, dimname, nosegid1d)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2593) trim(dimname)
                    goto 800
                endif
                nosegmesh1d = dimsizes(nosegid1d)
            endif

            ! Find the network name
            network_name = ' '
            if (networkid > 0) then
                ! Find the network name
                inc_error = nf90_inquire_variable(ncid, networkid, network_name)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2535) trim(mncnam)
                    goto 800
                end if
            endif

            ! Find the 2D mesh name, number of 2D segments and number of layers in the 2D part of the grid when
            ! available
            mesh_name2d = ' '
            noseglmesh2d = 0
            nosegmesh2d3d = 0
            num_layers = 1
            if (type_ugrid == type_ugrid_face_crds) then
                ! Get the dimensions of UNTRIM
                inc_error = nf90_inquire_variable(ncid, meshid2d, xtype = xtype, ndims = ndims, dimids = dimids)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2541)
                    goto 800
                endif

                noseglid2d = dimids(1)
                nolayid = dimids(2)
                noseglmesh2d = dimsizes(noseglid2d)
                num_layers = dimsizes(nolayid)

                nosegmesh2d3d = noseglmesh2d * num_layers
                type_ugrid = type_ugrid_face_crds

                ! Read aggregation table
                allocate (aggr2d3d(noseglmesh2d, num_layers))
                inc_error = nf90_get_var(ncid, meshid2d, aggr2d3d)

                ! Determine the 2D mesh variable from that
                mesh_name2d = ' '
                inc_error = nf90_get_att(ncid, meshid2d, "mesh", mesh_name2d)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2555)
                    goto 800
                endif
            else if (type_ugrid == type_ugrid_node_crds) then
                ! get number of segments from face_dimension = "mesh2d_nFaces" (2D)
                inc_error = nf90_inquire_variable(ncid, meshid2d, mesh_name2d)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2535) trim(mncnam)
                    goto 800
                end if
                ! Get the number of faces by the dimension mentioned in the variable name 'face_dimension')
                inc_error = nf90_get_att(ncid, meshid2d, 'face_dimension', dimname)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2594)
                    goto 800
                endif
                inc_error = nf90_inq_dimid(ncid, dimname, noseglid2d)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2595) trim(dimname)
                    goto 800
                endif
                noseglmesh2d = dimsizes(noseglid2d)

                ! calculate number of layers
                num_layers = (num_cells - nosegmesh1d) / noseglmesh2d
                nosegmesh2d3d = noseglmesh2d * num_layers

                ! check number of layers
                if (nosegmesh2d3d + nosegmesh1d /= num_cells) then
                    write (lunut, 2596) num_cells, noseglmesh2d, num_layers, nosegmesh1d
                    goto 800
                endif

                ! We do not have an aggregation table: construct one
                allocate (aggr2d3d(noseglmesh2d, num_layers))
                do j = 1, num_layers
                    do i = 1, noseglmesh2d
                        aggr2d3d(i, j) = i + (j - 1) * noseglmesh2d
                    enddo
                enddo
            endif

            if (meshid1d > 0) then
                ! We do not have an aggregation table: construct one
                allocate (aggr1d(nosegmesh1d))
                do j = 1, num_layers
                    do i = 1, nosegmesh1d
                        aggr1d(i) = i + nosegmesh2d3d
                    enddo
                enddo
            endif

            if (meshid2d > 0) then
                write (lunut, 2550) trim(mesh_name2d)
            endif
            if (meshid1d > 0) then
                write (lunut, 2551) trim(mesh_name1d)
            endif

            inc_error = copy_variable_attributes(ncid, ncidmap, nf90_global, nf90_global)
            if (inc_error /= nf90_noerr) then
                write (lunut, 2570)
                goto 800
            endif

            ! Generate the UUID and store it as an attibute
            call generate_uuid(uuid)
            inc_error = nf90_put_att(ncidmap, nf90_global, "uuid", uuid)
            if (inc_error /= nf90_noerr) then
                write (lunut, 2571)
                goto 800
            endif

            ! For now we can simply copy the mesh data
            if (meshid2d > 0) then
                inc_error = copy_mesh(ncid, ncidmap, meshid2d, mesh_name2d, type_ugrid)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2572)
                    goto 800
                endif
            endif
            if (networkid > 0) then
                inc_error = copy_mesh(ncid, ncidmap, networkid, network_name, type_ugrid_network)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2572)
                    goto 800
                endif
                inc_error = copy_mesh(ncid, ncidmap, network_geometryid, network_geometry_name, type_ugrid_network_geometry)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2572)
                    goto 800
                endif
            endif
            if (meshid1d > 0) then
                inc_error = copy_mesh(ncid, ncidmap, meshid1d, mesh_name1d, type_ugrid_mesh1d)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2572)
                    goto 800
                endif
            endif

            ! Add a "layer" dimension for DELWAQ and update the IDs
            ! (Must happen after copying the mesh - otherwise the dimension IDs do not match)
            if (type_ugrid == type_ugrid_node_crds) then
                inc_error = create_dimension(ncidmap, noseglmesh2d, num_layers, dimids, dimsizes)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2570)
                    goto 800
                endif
            endif

            ! Update the timestamp
            call date_and_time(values = values)
            write(timestamp, '(i4.4,a,i2.2,a,i2.2, a,i2.2,a,i2.2,a,i2.2,a,f5.3,a,i2.2,a,i2.2)') &
                    values(1), '-', values(2), '-', values(3), 'T', &
                    values(5), ':', values(6), ':', values(7), ':', values(8) / 1000.0, &
                    merge('+', '-', values(4)>=0), values(4) / 60, ':', mod(values(4), 60)

            inc_error = nf90_redef(ncidmap)
            if (inc_error /= nf90_noerr) then
                write (lunut, 2565)
                goto 800
            endif

            call getfullversionstring_delwaq(versionid)
            inc_error = nf90_put_att(ncidmap, nf90_global, 'source', trim(versionid))
            if (inc_error /= nf90_noerr) then
                write (lunut, 2573)
                goto 800
            endif

            history = "Created on " // trim(timestamp) // ", DELWAQ"
            inc_error = nf90_put_att(ncidmap, nf90_global, 'history', trim(history))
            if (inc_error /= nf90_noerr) then
                write (lunut, 2573)
                goto 800
            endif

            inc_error = nf90_put_att(ncidmap, nf90_global, 'date_created', trim(timestamp))
            if (inc_error /= nf90_noerr) then
                write (lunut, 2573)
                goto 800
            endif
            inc_error = nf90_put_att(ncidmap, nf90_global, 'date_modified', trim(timestamp))
            if (inc_error /= nf90_noerr) then
                write (lunut, 2574)
                goto 800
            endif

            inc_error = nf90_enddef(ncidmap)
            if (inc_error /= nf90_noerr) then
                write (lunut, 2566)
                goto 800
            endif

            if (meshid2d > 0) then
                allocate(laythickness(num_layers))
                laythickness = 1.0 / real(num_layers)  ! Uniform distribution for now !!
                inc_error = create_layer_dimension(ncidmap, mesh_name2d, num_layers, laythickness, nolayid)

                if (num_layers == 1) then
                    nolayid = dlwqnc_type2d
                endif

                if (inc_error /= nf90_noerr) then
                    write (lunut, 2580)
                    goto 800
                endif
            endif

            t0string = moname(4)
            inc_error = create_time_variable(ncidmap, t0string, timeid, bndtimeid, ntimeid)
            if (inc_error /= nf90_noerr) then
                write(lunut, 2581)
                goto 800
            endif


            ! Write output variables and process library info to NetCDF-file
            ! long name and unit will follow later, they are in the ouput.wrk-file!
            !
            ! Create 3D and only when num_layers > 1 also 2D variables. This is controlled by the NetCDF ID for the layer
            ! dimension
            if (meshid2d > 0) then
                allocate(sumconc1(notot1), sumconc2(notot2))
                do iout = 1, notot1
                    inc_error = create_variable(ncidmap, mesh_name2d, synam1(iout), sydsc1(iout), &
                            sysnm1(iout), syuni1(iout), ntimeid, noseglid2d, nolayid, wqid1(iout, 1))
                    if (inc_error /= nf90_noerr) then
                        write(lunut, 2582)
                        goto 800
                    endif

                    if (num_layers > 1) then
                        inc_error = create_variable(ncidmap, mesh_name2d, synam1(iout), sydsc1(iout), &
                                sysnm1(iout), syuni1(iout), ntimeid, noseglid2d, dlwqnc_type2d, wqid1(iout, 2))
                        if (inc_error /= nf90_noerr) then
                            write(lunut, 2582)
                            goto 800
                        endif
                    else
                        wqid1(iout, 2) = -1
                    endif
                    if (index(syuni1(iout), 'm-2') > 0 .or. index(syuni1(iout), '/m2') > 0) then
                        sumconc1(iout) = .true.
                    else
                        sumconc1(iout) = .false.
                    endif
                enddo
                do iout = 1, notot2
                    inc_error = create_variable(ncidmap, mesh_name2d, synam2(iout), sydsc2(iout), &
                            sysnm2(iout), syuni2(iout), ntimeid, noseglid2d, nolayid, wqid2(iout, 1))
                    if (inc_error /= nf90_noerr) then
                        write(lunut, 2582)
                        goto 800
                    endif
                    if (num_layers > 1) then
                        inc_error = create_variable(ncidmap, mesh_name2d, synam2(iout), sydsc2(iout), &
                                sysnm2(iout), syuni2(iout), ntimeid, noseglid2d, dlwqnc_type2d, wqid2(iout, 2))
                        if (inc_error /= nf90_noerr) then
                            write(lunut, 2582)
                            goto 800
                        endif
                    endif
                    if (index(syuni2(iout), 'm-2') > 0 .or. index(syuni2(iout), '/m2') > 0) then
                        sumconc2(iout) = .true.
                    else
                        sumconc2(iout) = .false.
                    endif
                enddo

                !           Always add volume
                !           Note: the standard name for "volume" is fixed for the moment, but the NetCDF standard is
                !           far from complete.
                inc_error = create_variable(ncidmap, mesh_name2d, 'volume', 'volume (m3)', &
                        'sea_water_volume', 'm3', ntimeid, noseglid2d, nolayid, wqidvolume_2d3d)
                if (inc_error /= nf90_noerr) then
                    write(lunut, 2583)
                    goto 800
                endif
                if (num_layers > 1) then
                    inc_error = create_variable(ncidmap, mesh_name2d, 'volume', 'volume (m3)', &
                            'sea_water_volume', 'm3', ntimeid, noseglid2d, dlwqnc_type2d, wqidvolume_2d)
                    if (inc_error /= nf90_noerr) then
                        write(lunut, 2583)
                        goto 800
                    endif
                endif
            endif

            if (meshid1d > 0) then
                do iout = 1, notot1
                    inc_error = create_variable(ncidmap, mesh_name1d, synam1(iout), sydsc1(iout), &
                            sysnm1(iout), syuni1(iout), ntimeid, nosegid1d, dlwqnc_type1d, wqid1(iout, 3))
                    if (inc_error /= nf90_noerr) then
                        write(lunut, 2582)
                        goto 800
                    endif
                enddo
                do iout = 1, notot2
                    inc_error = create_variable(ncidmap, mesh_name1d, synam2(iout), sydsc2(iout), &
                            sysnm2(iout), syuni2(iout), ntimeid, nosegid1d, dlwqnc_type1d, wqid2(iout, 3))
                    if (inc_error /= nf90_noerr) then
                        write(lunut, 2582)
                        goto 800
                    endif
                enddo

                !           Always add volume
                !           Note: the standard name for "volume" is fixed for the moment, but the NetCDF standard is
                !           far from complete.
                inc_error = create_variable(ncidmap, mesh_name1d, 'volume', 'volume (m3)', &
                        'sea_water_volume', 'm3', ntimeid, nosegid1d, dlwqnc_type1d, wqidvolume_1d)
                if (inc_error /= nf90_noerr) then
                    write(lunut, 2583)
                    goto 800
                endif
            endif

            ! Flush after first stage of preparing NetCDF file
            inc_error = nf90_sync(ncidmap)
            if (inc_error /= nf90_noerr) then
                write(lunut, 2591)
                goto 800
            endif
        endif

        !     Perform output
        !     New time record
        inc_error = write_time(ncidmap, timeid, bndtimeid, mncrec, itime)
        if (inc_error /= nf90_noerr) then
            if (inc_error /= nf90_noerr) then
                write(lunut, 2590)
                goto 800
            endif
        endif

        if (noseglmesh2d > 0) then
            !     Output arrays
            allocate(dlwq_values_2d3d(noseglmesh2d, num_layers), dlwq_values_2d(noseglmesh2d), dlwq_volume_2d(noseglmesh2d))
            !     Total volume in 2D
            dlwq_volume_2d = 0.0
            !     Write volumes
            do ilay = 1, num_layers
                do isegl = 1, noseglmesh2d
                    if (aggr2d3d(isegl, ilay)>0) then
                        dlwq_values_2d3d(isegl, ilay) = volume(aggr2d3d(isegl, ilay))
                        if (num_layers > 1 .and. btest(iknmrk(aggr2d3d(isegl, ilay)), 0)) then
                            dlwq_volume_2d(isegl) = dlwq_volume_2d(isegl) + volume(aggr2d3d(isegl, ilay))
                        endif
                    else
                        dlwq_values_2d3d(isegl, ilay) = -999.0
                        if (num_layers > 1) then
                            dlwq_volume_2d(isegl) = -999.0
                        endif
                    endif
                enddo
            enddo

            inc_error = dlwqnc_write_wqvariable(ncidmap, wqidvolume_2d3d, mncrec, dlwq_values_2d3d)
            if (inc_error /= nf90_noerr) then
                write(lunut, 2591)
                goto 800
            endif

            if (num_layers > 1) then
                inc_error = dlwqnc_write_wqvariable(ncidmap, wqidvolume_2d, mncrec, dlwq_volume_2d)
                if (inc_error /= nf90_noerr) then
                    write(lunut, 2591)
                    goto 800
                endif
            endif

            !        First set of output
            do iout = 1, notot1
                if (num_layers > 1) then
                    dlwq_values_2d = 0.0
                endif
                do ilay = 1, num_layers
                    do isegl = 1, noseglmesh2d
                        if (aggr2d3d(isegl, ilay)>0) then
                            dlwq_values_2d3d(isegl, ilay) = conc1(iout, aggr2d3d(isegl, ilay))
                            if (num_layers > 1 .and. btest(iknmrk(aggr2d3d(isegl, ilay)), 0)) then
                                if (.not. sumconc1(iout)) then
                                    dlwq_values_2d(isegl) = dlwq_values_2d(isegl) + &
                                            conc1(iout, aggr2d3d(isegl, ilay)) * &
                                                    volume(aggr2d3d(isegl, ilay))
                                else
                                    dlwq_values_2d(isegl) = dlwq_values_2d(isegl) + &
                                            conc1(iout, aggr2d3d(isegl, ilay))
                                endif
                            endif
                        else
                            dlwq_values_2d3d(isegl, ilay) = -999.0
                            if (num_layers > 1) then
                                dlwq_values_2d(isegl) = -999.0
                            endif
                        endif
                    enddo
                enddo

                if (num_layers > 1) then
                    inc_error = dlwqnc_write_wqvariable(ncidmap, wqid1(iout, 1), mncrec, dlwq_values_2d3d)
                else
                    inc_error = dlwqnc_write_wqvariable(ncidmap, wqid1(iout, 1), mncrec, dlwq_values_2d3d(:, 1))
                endif
                if (inc_error /= nf90_noerr) then
                    write(lunut, 2591)
                    goto 800
                endif

                !           Add 2D output in 3D models
                if (num_layers > 1) then
                    if (.not. sumconc1(iout)) then
                        do isegl = 1, noseglmesh2d
                            if(dlwq_volume_2d(isegl)/=-999.0) then
                                dlwq_values_2d(isegl) = dlwq_values_2d(isegl) / dlwq_volume_2d(isegl)
                            endif
                        enddo
                    endif

                    inc_error = dlwqnc_write_wqvariable(ncidmap, wqid1(iout, 2), mncrec, dlwq_values_2d)
                    if (inc_error /= nf90_noerr) then
                        write(lunut, 2591)
                        goto 800
                    endif
                endif
            enddo

            !        Second set of output
            do iout = 1, notot2
                if (num_layers > 1) then
                    dlwq_values_2d = 0.0
                endif

                do ilay = 1, num_layers
                    do isegl = 1, noseglmesh2d
                        if (aggr2d3d(isegl, ilay)>0) then
                            dlwq_values_2d3d(isegl, ilay) = conc2(iout, aggr2d3d(isegl, ilay))
                            if (num_layers > 1 .and. btest(iknmrk(aggr2d3d(isegl, ilay)), 0)) then
                                if (.not. sumconc2(iout)) then
                                    dlwq_values_2d(isegl) = dlwq_values_2d(isegl) + &
                                            conc2(iout, aggr2d3d(isegl, ilay)) * &
                                                    volume(aggr2d3d(isegl, ilay))
                                else
                                    dlwq_values_2d(isegl) = dlwq_values_2d(isegl) + &
                                            conc2(iout, aggr2d3d(isegl, ilay))
                                endif
                            endif
                        else
                            dlwq_values_2d3d(isegl, ilay) = -999.0
                            if (num_layers > 1) then
                                dlwq_values_2d(isegl) = -999.0
                            endif
                        endif
                    enddo
                enddo

                if (num_layers >  1) then
                    inc_error = dlwqnc_write_wqvariable(ncidmap, wqid2(iout, 1), mncrec, dlwq_values_2d3d)
                else
                    inc_error = dlwqnc_write_wqvariable(ncidmap, wqid2(iout, 1), mncrec, dlwq_values_2d3d(:, 1))
                endif
                if (inc_error /= nf90_noerr) then
                    write(lunut, 2591)
                    goto 800
                endif

                !           Add 2D output in 3D models
                if (num_layers > 1) then
                    if (.not. sumconc2(iout)) then
                        do isegl = 1, noseglmesh2d
                            if(dlwq_volume_2d(isegl)/=-999.0) then
                                dlwq_values_2d(isegl) = dlwq_values_2d(isegl) / dlwq_volume_2d(isegl)
                            endif
                        enddo
                    endif

                    inc_error = dlwqnc_write_wqvariable(ncidmap, wqid2(iout, 2), mncrec, dlwq_values_2d)
                    if (inc_error /= nf90_noerr) then
                        write(lunut, 2591)
                        goto 800
                    endif
                endif
            enddo

            deallocate(dlwq_values_2d3d)
            deallocate(dlwq_values_2d)
            deallocate(dlwq_volume_2d)
        endif

        if (nosegmesh1d > 0) then
            !     Output arrays
            allocate(dlwq_values_1d(nosegmesh1d))
            !     Write volumes
            do isegl = 1, nosegmesh1d
                if (aggr1d(isegl)>0) then
                    dlwq_values_1d(isegl) = volume(aggr1d(isegl))
                else
                    dlwq_values_1d(isegl) = -999.0
                endif
            enddo

            inc_error = dlwqnc_write_wqvariable(ncidmap, wqidvolume_1d, mncrec, dlwq_values_1d)
            if (inc_error /= nf90_noerr) then
                write(lunut, 2591)
                goto 800
            endif

            !        First set of output
            do iout = 1, notot1
                do isegl = 1, nosegmesh1d
                    if (aggr1d(isegl)>0) then
                        dlwq_values_1d(isegl) = conc1(iout, aggr1d(isegl))
                    else
                        dlwq_values_1d(isegl) = -999.0
                    endif
                enddo

                inc_error = dlwqnc_write_wqvariable(ncidmap, wqid1(iout, 3), mncrec, dlwq_values_1d)
                if (inc_error /= nf90_noerr) then
                    write(lunut, 2591)
                    goto 800
                endif
            enddo

            !        Second set of output
            do iout = 1, notot2
                do isegl = 1, nosegmesh1d
                    if (aggr1d(isegl)>0) then
                        dlwq_values_1d(isegl) = conc2(iout, aggr1d(isegl))
                    else
                        dlwq_values_1d(isegl) = -999.0
                    endif
                enddo

                inc_error = dlwqnc_write_wqvariable(ncidmap, wqid2(iout, 3), mncrec, dlwq_values_1d)
                if (inc_error /= nf90_noerr) then
                    write(lunut, 2591)
                    goto 800
                endif
            enddo

            deallocate(dlwq_values_1d)
        endif

        ! Flush after each map write
        inc_error = nf90_sync(ncidmap)
        if (inc_error /= nf90_noerr) then
            write(lunut, 2591)
            goto 800
        endif
        goto 900

        800  continue
        ! There were errors!
        write (*, 2600) inc_error
        write (lunut, 2600) inc_error
        write (*, 2610) trim(nf90_strerror(inc_error))
        write (lunut, 2610) trim(nf90_strerror(inc_error))
        write (*, 2542)
        write (lunut, 2542)
        call stop_with_error()

        900  continue
        if (timon) call timstop (ithandl)
        return

        2520 format (/ ' NetCDF version: ', A)
        2530 format (/ ' ERROR, opening NetCDF file. Filename: ', A)
        2531 format (/ ' ERROR, reading dimensions from NetCDF file.')
        2535 format (/ ' ERROR, unable to retrieve mesh name from file:', A)
        2540 format (/ ' ERROR, no mesh(es) found with required attribute "delwaq_role" or "cf_role"' &
                / '        this version of Delwaq is not compatible with older non-ugrid waqgeom-files')
        2541 format (/ ' ERROR, reading dimensions from "delwaq_role"/"cf_type" in NetCDF file.')
        2542 format (/ ' ERROR, this version of Delwaq is not compatible with older non-ugrid waqgeom-files')
        2550 format (/ ' Mesh used for Delwaq 2D/3D output: ', A)
        2551 format (/ ' Mesh used for Delwaq 1D output: ', A)
        2555 format (/ ' ERROR, Getting the mesh name failed')
        2560 format (/ ' Creating the output file failed. Filename:', A)
        2565 format (/ ' ERROR: Reopening NetCDF definition failed')
        2566 format (/ ' ERROR: Closing NetCDF definition failed')
        2570 format (/ ' Copying the attributes/dimensions failed')
        2571 format (/ ' Writing the UUID failed')
        2572 format (/ ' Copying the mesh data failed')
        2573 format (/ ' Writing date_created failed')
        2574 format (/ ' Writing date_modified failed')
        2580 format (/ ' Creating layer dimension failed')
        2581 format (/ ' Creating time dimension failed')
        2582 format (/ ' Creating variable failed')
        2583 format (/ ' Creating volume variable failed')
        2590 format (/ ' Writing new NetCDF map time failed')
        2591 format (/ ' Writing new NetCDF map output data failed')
        2593 format (/ ' Attribute "node_dimension" in mesh not found')
        2594 format (/ ' Attribute "face_dimension" in mesh not found')
        2595 format (/ ' Dimension "', A, '" not found')
        2596 format (/ ' ERROR: Dimension mismatch!' &
                / ' num_cells        :', I8 &
                / ' noseglmesh2d :', I8 &
                / ' num_layers    :', I8 &
                / ' nosegmesh1d  :', I8)
        2600 format (/ ' NetCDF error number: ', I6)
        2610 format (/ ' NetCDF error message: ', A)

    end subroutine write_netcdf_map_output

    subroutine write_netcdf_history_output(ncidhis, hncnam, ugridf, timeid, bndtimeid, &
            hncrec, itime, moname, idump, duname, &
            num_monitoring_points, notot1, conc1, synam1, sysnm1, &
            syuni1, sydsc1, wqid1, notot2, conc2, &
            synam2, sysnm2, syuni2, sydsc2, wqid2, &
            lunut)

        !! Writes history output to NetCDF
        use m_universally_unique_id_generator
        use m_logger_helper, only: stop_with_error
        use timers
        use waq_netcdf_utils    !, only: set_dlwqnc_debug_status, create_time_variable, write_time
        use results, only: ncopt

        integer(kind = int_wp), intent(inout) :: ncidhis              ! NetCDF id of output history file
        character(255), intent(in) :: hncnam               ! name NetCDF output history file
        character(255), intent(in) :: ugridf               ! name of NetCDF ugrid file
        integer(kind = int_wp), intent(inout) :: timeid
        integer(kind = int_wp), intent(inout) :: bndtimeid
        integer(kind = int_wp), intent(in) :: hncrec               ! present record in NetCDF file
        integer(kind = int_wp), intent(in) :: itime                ! present time in clock units
        character(40), intent(in) :: moname(4)            ! model identification
        integer(kind = int_wp), intent(in) :: idump(num_monitoring_points)        ! segment number of monitoring points and areas
        character(*), intent(in) :: duname(num_monitoring_points)       ! names of monitoring points and areas
        integer(kind = int_wp), intent(in) :: num_monitoring_points               ! number of monitoring points and areas
        integer(kind = int_wp), intent(in) :: notot1               ! number of variables in conc1
        real(kind = real_wp), intent(in) :: conc1 (notot1, *)     ! values
        character(20), intent(in) :: synam1(notot1)       ! names of variables in conc1
        character(100), intent(in) :: sysnm1(notot1)       ! standard names of variables in conc1
        character(40), intent(in) :: syuni1(notot1)       ! units of variables in conc1
        character(60), intent(in) :: sydsc1(notot1)       ! decriptions of variables in conc1
        integer(kind = int_wp), intent(inout) :: wqid1(notot1, 2)      ! NetCDF ids of variables in conc1
        integer(kind = int_wp), intent(in) :: notot2               ! number of variables in conc2
        real(kind = real_wp), intent(in) :: conc2 (notot2, num_monitoring_points)! values
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
        integer(kind = int_wp) :: inc_error, ierr, num_layers, iout
        integer(kind = int_wp) :: xtype
        integer(kind = int_wp) :: ndims
        logical, allocatable :: sumconc1(:), sumconc2(:)
        integer(kind = int_wp), dimension(nf90_max_var_dims) :: dimids
        integer(kind = int_wp), dimension(nf90_max_dims) :: dimsizes
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
        if (timon) call timstrt ("write_netcdf_history_output", ithandl)

        ! Check if there are any monitoring points/areas
        ! (If the number is zero and we would not suppress the creation of the file,
        ! then we would get a run-time error from NetCDF - apparently 0 is the magic
        ! number for the unlimited-size dimension and there can be only one of those)
        if (num_monitoring_points == 0) then
            goto 900
        endif

        ! Initialize file
        if (ncidhis < 0) then
            ! Turn on debug info from dlwaqnc
            inc_error = set_dlwqnc_debug_status(.true.)

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
            inc_error = nf90_def_dim(ncidhis, "nStations", num_monitoring_points, nostations_id)
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
            inc_error = create_time_variable(ncidhis, t0string, timeid, bndtimeid, ntimeid)
            if (inc_error /= nf90_noerr) then
                write(lunut, 2581)
                goto 800
            endif

            ! Write information on the stations to NetCDF-file (we are in data mode ...)
            inc_error = nf90_put_var(ncidhis, station_names_id, duname)
            if (inc_error /= nf90_noerr) then
                write(lunut, 2587) 'names'
                goto 800
            endif
            do i = 2, 4
                inc_error = nf90_put_var(ncidhis, coord_id(i - 1), [(0.0d0, j = 1, num_monitoring_points)])  ! TODO; FOR NOW
                if (inc_error /= nf90_noerr) then
                    write(lunut, 2587) station_property(1, i)
                    goto 800
                endif
            enddo

            ! Back to definition mode to define the actual variables
            inc_error = nf90_redef(ncidhis)
            if (inc_error /= nf90_noerr) then
                write (lunut, 2565)
                goto 800
            endif

            ! Write output variables and proces library info to NetCDF-file
            ! long name and unit will follow later, they are in the ouput.wrk-file!
            do iout = 1, notot1
                name = replace_space_by_underscore(synam1(iout))
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
                name = replace_space_by_underscore(synam2(iout))
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

        ! Perform output
        allocate(dlwq_values(num_monitoring_points))

        ! New time record
        inc_error = write_time(ncidhis, timeid, bndtimeid, hncrec, itime)
        if (inc_error /= nf90_noerr) then
            if (inc_error /= nf90_noerr) then
                write(lunut, 2590)
                goto 800
            endif
        endif

        ! First set of output
        do iout = 1, notot1
            do id = 1, num_monitoring_points
                dlwq_values(id) = conc1(iout, idump(id))
            enddo

            inc_error = dlwqnc_write_wqvariable(ncidhis, wqid1(iout, 1), hncrec, dlwq_values)
            if (inc_error /= nf90_noerr) then
                write(lunut, 2591)
                goto 800
            endif
        enddo

        ! Second set of output
        do iout = 1, notot2
            do id = 1, num_monitoring_points
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
        call stop_with_error()

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

    end subroutine write_netcdf_history_output


end module m_write_netcdf_output
