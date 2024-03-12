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

module m_outmnc
    use m_waq_precision
    use m_srstop
    use m_universally_unique_id_generator

contains

    subroutine outmnc (ncidmap, mncnam, ugridf, timeid, bndtimeid, mncrec, itime, moname, &
            noseg, notot1, conc1, synam1, sysnm1, syuni1, sydsc1, wqid1, notot2, &
            conc2, synam2, sysnm2, syuni2, sydsc2, wqid2, volume, iknmrk, lunut)

        !     Deltares Software Centre

        !     Function            : Writes map output to NetCDF

        use timers
        use dlwq_netcdf  !   read/write grid in netcdf
        use delwaq_version_module
        use results, only : ncopt
        implicit none

        !     Parameters          :

        !     kind           function         name                    description

        integer(kind = int_wp), intent(inout) :: ncidmap              ! NetCDF id of output map file
        character(255), intent(in) :: mncnam               ! name NetCDF output map file
        character(255), intent(in) :: ugridf               ! name of NetCDF ugrid file
        integer(kind = int_wp), intent(inout) :: timeid
        integer(kind = int_wp), intent(inout) :: bndtimeid
        integer(kind = int_wp), intent(in) :: mncrec               ! present record in NetCDF file
        integer(kind = int_wp), intent(in) :: itime                ! present time in clock units
        character(40), intent(in) :: moname(4)            ! model identification
        integer(kind = int_wp), intent(in) :: noseg                ! number of computational volumes
        integer(kind = int_wp), intent(in) :: notot1               ! number of variables in conc1
        real(kind = real_wp), intent(in) :: conc1 (notot1, noseg) ! values
        character(20), intent(in) :: synam1(notot1)       ! names of variables in conc1
        character(100), intent(in) :: sysnm1(notot1)       ! standard names of variables in conc1
        character(40), intent(in) :: syuni1(notot1)       ! units of variables in conc1
        character(60), intent(in) :: sydsc1(notot1)       ! decriptions of variables in conc1
        integer(kind = int_wp), intent(inout) :: wqid1(notot1, 3)      ! NetCDF ids of variables in conc1
        integer(kind = int_wp), intent(in) :: notot2               ! number of variables in conc2
        real(kind = real_wp), intent(in) :: conc2 (notot2, noseg) ! values
        character(20), intent(in) :: synam2(notot2)       ! names of variables in conc2
        character(100), intent(in) :: sysnm2(notot2)       ! standard names of variables in conc1
        character(40), intent(in) :: syuni2(notot2)       ! units of variables in conc1
        character(60), intent(in) :: sydsc2(notot2)       ! decriptions of variables in conc1
        integer(kind = int_wp), intent(inout) :: wqid2(notot2, 3)      ! NetCDF ids of variables in conc1
        real(kind = real_wp), intent(in) :: volume(noseg)        ! values
        integer(kind = int_wp), intent(in) :: iknmrk(noseg)        ! Feature array. Bit zero set means active.
        integer(kind = int_wp), intent(in) :: lunut                ! unit number monitoring file

        integer(kind = int_wp) :: iseg                         ! loop counter for segments
        integer(kind = int_wp) :: k                            ! loop counter for substances
        real(kind = real_wp), parameter :: amiss = -999.0 ! missing value indicator

        integer(kind = int_wp) :: ncid
        integer(kind = int_wp) :: varid, varidout, meshidout, ntimeid, wqid, noseglmesh2d, nosegmesh2d3d, nosegmesh1d, nolay
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

        save ncid, noseglmesh2d, nolay, nosegmesh1d, varid, meshid2d, meshid1d, networkid, network_geometryid
        save nosegmesh2d3d, aggr2d3d, aggr1d, wqidvolume_2d3d, wqidvolume_2d, wqidvolume_1d
        save sumconc1, sumconc2

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("outmnc", ithandl)

        !     Initialize file
        if (ncidmap < 0) then

            ! Turn on debug info from dlwaqnc
            inc_error = dlwqnc_debug_status(.true.)

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

            inc_error = dlwqnc_read_dims(ncid, dimsizes)
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

            ! We don't assume variable names, but we try to find all meshes by their attributes using dlwqnc_find_meshes_by_att
            inc_error = dlwqnc_find_meshes_by_att(ncid, meshid2d, type_ugrid, meshid1d, networkid, network_geometryid)
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

            ! Find the 2D mesh name, number of 2D segments and number of layers in the 2D part of the grid when available
            mesh_name2d = ' '
            noseglmesh2d = 0
            nosegmesh2d3d = 0
            nolay = 1
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
                nolay = dimsizes(nolayid)

                nosegmesh2d3d = noseglmesh2d * nolay
                type_ugrid = type_ugrid_face_crds

                ! Read aggregation table
                allocate (aggr2d3d(noseglmesh2d, nolay))
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
                nolay = (noseg - nosegmesh1d) / noseglmesh2d
                nosegmesh2d3d = noseglmesh2d * nolay

                ! check number of layers
                if (nosegmesh2d3d + nosegmesh1d /= noseg) then
                    write (lunut, 2596) noseg, noseglmesh2d, nolay, nosegmesh1d
                    goto 800
                endif

                ! We do not have an aggregation table: construct one
                allocate (aggr2d3d(noseglmesh2d, nolay))
                do j = 1, nolay
                    do i = 1, noseglmesh2d
                        aggr2d3d(i, j) = i + (j - 1) * noseglmesh2d
                    enddo
                enddo
            endif

            if (meshid1d > 0) then
                ! We do not have an aggregation table: construct one
                allocate (aggr1d(nosegmesh1d))
                do j = 1, nolay
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

            inc_error = dlwqnc_copy_var_atts(ncid, ncidmap, nf90_global, nf90_global)
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
                inc_error = dlwqnc_copy_mesh(ncid, ncidmap, meshid2d, mesh_name2d, type_ugrid)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2572)
                    goto 800
                endif
            endif
            if (networkid > 0) then
                inc_error = dlwqnc_copy_mesh(ncid, ncidmap, networkid, network_name, type_ugrid_network)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2572)
                    goto 800
                endif
                inc_error = dlwqnc_copy_mesh(ncid, ncidmap, network_geometryid, network_geometry_name, type_ugrid_network_geometry)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2572)
                    goto 800
                endif
            endif
            if (meshid1d > 0) then
                inc_error = dlwqnc_copy_mesh(ncid, ncidmap, meshid1d, mesh_name1d, type_ugrid_mesh1d)
                if (inc_error /= nf90_noerr) then
                    write (lunut, 2572)
                    goto 800
                endif
            endif

            ! Add a "layer" dimension for DELWAQ and update the IDs
            ! (Must happen after copying the mesh - otherwise the dimension IDs do not match)
            if (type_ugrid == type_ugrid_node_crds) then
                inc_error = dlwqnc_create_delwaq_dims(ncidmap, noseglmesh2d, nolay, dimids, dimsizes)
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
                allocate(laythickness(nolay))
                laythickness = 1.0 / real(nolay)  ! Uniform distribution for now !!
                inc_error = dlwqnc_create_layer_dim(ncidmap, mesh_name2d, nolay, laythickness, nolayid)

                if (nolay == 1) then
                    nolayid = dlwqnc_type2d
                endif

                if (inc_error /= nf90_noerr) then
                    write (lunut, 2580)
                    goto 800
                endif
            endif

            t0string = moname(4)
            inc_error = dlwqnc_create_wqtime(ncidmap, t0string, timeid, bndtimeid, ntimeid)
            if (inc_error /= nf90_noerr) then
                write(lunut, 2581)
                goto 800
            endif


            ! Write output variables and process library info to NetCDF-file
            ! long name and unit will follow later, they are in the ouput.wrk-file!
            !
            ! Create 3D and only when nolay > 1 also 2D variables. This is controlled by the NetCDF ID for the layer dimension
            !
            if (meshid2d > 0) then
                allocate(sumconc1(notot1), sumconc2(notot2))
                do iout = 1, notot1
                    inc_error = dlwqnc_create_wqvariable(ncidmap, mesh_name2d, synam1(iout), sydsc1(iout), &
                            sysnm1(iout), syuni1(iout), ntimeid, noseglid2d, nolayid, wqid1(iout, 1))
                    if (inc_error /= nf90_noerr) then
                        write(lunut, 2582)
                        goto 800
                    endif

                    if (nolay > 1) then
                        inc_error = dlwqnc_create_wqvariable(ncidmap, mesh_name2d, synam1(iout), sydsc1(iout), &
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
                    inc_error = dlwqnc_create_wqvariable(ncidmap, mesh_name2d, synam2(iout), sydsc2(iout), &
                            sysnm2(iout), syuni2(iout), ntimeid, noseglid2d, nolayid, wqid2(iout, 1))
                    if (inc_error /= nf90_noerr) then
                        write(lunut, 2582)
                        goto 800
                    endif
                    if (nolay > 1) then
                        inc_error = dlwqnc_create_wqvariable(ncidmap, mesh_name2d, synam2(iout), sydsc2(iout), &
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
                inc_error = dlwqnc_create_wqvariable(ncidmap, mesh_name2d, 'volume', 'volume (m3)', &
                        'sea_water_volume', 'm3', ntimeid, noseglid2d, nolayid, wqidvolume_2d3d)
                if (inc_error /= nf90_noerr) then
                    write(lunut, 2583)
                    goto 800
                endif
                if (nolay > 1) then
                    inc_error = dlwqnc_create_wqvariable(ncidmap, mesh_name2d, 'volume', 'volume (m3)', &
                            'sea_water_volume', 'm3', ntimeid, noseglid2d, dlwqnc_type2d, wqidvolume_2d)
                    if (inc_error /= nf90_noerr) then
                        write(lunut, 2583)
                        goto 800
                    endif
                endif
            endif

            if (meshid1d > 0) then
                do iout = 1, notot1
                    inc_error = dlwqnc_create_wqvariable(ncidmap, mesh_name1d, synam1(iout), sydsc1(iout), &
                            sysnm1(iout), syuni1(iout), ntimeid, nosegid1d, dlwqnc_type1d, wqid1(iout, 3))
                    if (inc_error /= nf90_noerr) then
                        write(lunut, 2582)
                        goto 800
                    endif
                enddo
                do iout = 1, notot2
                    inc_error = dlwqnc_create_wqvariable(ncidmap, mesh_name1d, synam2(iout), sydsc2(iout), &
                            sysnm2(iout), syuni2(iout), ntimeid, nosegid1d, dlwqnc_type1d, wqid2(iout, 3))
                    if (inc_error /= nf90_noerr) then
                        write(lunut, 2582)
                        goto 800
                    endif
                enddo

                !           Always add volume
                !           Note: the standard name for "volume" is fixed for the moment, but the NetCDF standard is
                !           far from complete.
                inc_error = dlwqnc_create_wqvariable(ncidmap, mesh_name1d, 'volume', 'volume (m3)', &
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
        inc_error = dlwqnc_write_wqtime(ncidmap, timeid, bndtimeid, mncrec, itime)
        if (inc_error /= nf90_noerr) then
            if (inc_error /= nf90_noerr) then
                write(lunut, 2590)
                goto 800
            endif
        endif

        if (noseglmesh2d > 0) then
            !     Output arrays
            allocate(dlwq_values_2d3d(noseglmesh2d, nolay), dlwq_values_2d(noseglmesh2d), dlwq_volume_2d(noseglmesh2d))
            !     Total volume in 2D
            dlwq_volume_2d = 0.0
            !     Write volumes
            do ilay = 1, nolay
                do isegl = 1, noseglmesh2d
                    if (aggr2d3d(isegl, ilay)>0) then
                        dlwq_values_2d3d(isegl, ilay) = volume(aggr2d3d(isegl, ilay))
                        if (nolay > 1 .and. btest(iknmrk(aggr2d3d(isegl, ilay)), 0)) then
                            dlwq_volume_2d(isegl) = dlwq_volume_2d(isegl) + volume(aggr2d3d(isegl, ilay))
                        endif
                    else
                        dlwq_values_2d3d(isegl, ilay) = -999.0
                        if (nolay > 1) then
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

            if (nolay > 1) then
                inc_error = dlwqnc_write_wqvariable(ncidmap, wqidvolume_2d, mncrec, dlwq_volume_2d)
                if (inc_error /= nf90_noerr) then
                    write(lunut, 2591)
                    goto 800
                endif
            endif

            !        First set of output
            do iout = 1, notot1
                if (nolay > 1) then
                    dlwq_values_2d = 0.0
                endif
                do ilay = 1, nolay
                    do isegl = 1, noseglmesh2d
                        if (aggr2d3d(isegl, ilay)>0) then
                            dlwq_values_2d3d(isegl, ilay) = conc1(iout, aggr2d3d(isegl, ilay))
                            if (nolay > 1 .and. btest(iknmrk(aggr2d3d(isegl, ilay)), 0)) then
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
                            if (nolay > 1) then
                                dlwq_values_2d(isegl) = -999.0
                            endif
                        endif
                    enddo
                enddo

                if (nolay > 1) then
                    inc_error = dlwqnc_write_wqvariable(ncidmap, wqid1(iout, 1), mncrec, dlwq_values_2d3d)
                else
                    inc_error = dlwqnc_write_wqvariable(ncidmap, wqid1(iout, 1), mncrec, dlwq_values_2d3d(:, 1))
                endif
                if (inc_error /= nf90_noerr) then
                    write(lunut, 2591)
                    goto 800
                endif

                !           Add 2D output in 3D models
                if (nolay > 1) then
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
                if (nolay > 1) then
                    dlwq_values_2d = 0.0
                endif

                do ilay = 1, nolay
                    do isegl = 1, noseglmesh2d
                        if (aggr2d3d(isegl, ilay)>0) then
                            dlwq_values_2d3d(isegl, ilay) = conc2(iout, aggr2d3d(isegl, ilay))
                            if (nolay > 1 .and. btest(iknmrk(aggr2d3d(isegl, ilay)), 0)) then
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
                            if (nolay > 1) then
                                dlwq_values_2d(isegl) = -999.0
                            endif
                        endif
                    enddo
                enddo

                if (nolay >  1) then
                    inc_error = dlwqnc_write_wqvariable(ncidmap, wqid2(iout, 1), mncrec, dlwq_values_2d3d)
                else
                    inc_error = dlwqnc_write_wqvariable(ncidmap, wqid2(iout, 1), mncrec, dlwq_values_2d3d(:, 1))
                endif
                if (inc_error /= nf90_noerr) then
                    write(lunut, 2591)
                    goto 800
                endif

                !           Add 2D output in 3D models
                if (nolay > 1) then
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
        call srstop (1)

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
                / ' noseg        :', I8 &
                / ' noseglmesh2d :', I8 &
                / ' nolay    :', I8 &
                / ' nosegmesh1d  :', I8)
        2600 format (/ ' NetCDF error number: ', I6)
        2610 format (/ ' NetCDF error message: ', A)

    end subroutine outmnc
end module m_outmnc
