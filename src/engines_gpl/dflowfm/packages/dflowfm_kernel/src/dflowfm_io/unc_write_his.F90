!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

!> Write history data in NetCDF format.
subroutine unc_write_his(tim)            ! wrihis
    use Timers
    use m_flowtimes
    use m_flow
    use m_flowgeom
    use network_data, only: xk, yk
    use m_observations
    use m_monitoring_crosssections
    use m_monitoring_runupgauges
    use m_missing
    use netcdf
    use netcdf_utils
    use coordinate_reference_system, only: transform_and_put_latlon_coordinates
    use unstruc_files, only: defaultFilename
    use unstruc_netcdf, only: unc_create, unc_close, unc_addcoordatts, unc_addcoordmapping, unc_def_var_nonspatial, definencvar, unc_meta_add_user_defined
    use unstruc_netcdf, only: ihisfile
    use unstruc_netcdf, only: UNC_LOC_S3D, UNC_LOC_WU, UNC_LOC_W
    use unstruc_netcdf, only: unc_writeopts, unc_noforcedflush, UG_WRITE_LATLON, nccrs => crs
    use unstruc_netcdf, only: unc_add_time_coverage
    use unstruc_netcdf, only: unc_write_struc_input_coordinates
    use unstruc_messages
    use m_map_his_precision
    use m_sferic, only: jsferic
    use m_partitioninfo
    use m_timer
    use m_sediment
    use m_flowexternalforcings, only: numtracers, trnames
    use m_transport, only: NUMCONST_MDU, ITRA1, ITRAN, ISED1, ISEDN, const_names, const_units, NUMCONST, itemp, isalt
    use m_structures
    use m_fm_wq_processes
    use string_module
    use m_dad
    use m_filter, only: checkmonitor
    use m_alloc
    use unstruc_channel_flow, only: network
    use simple_geometry, only: sgeom_def_geometry_variables
    use m_1d_structures
    use m_structures
    use m_GlobalParameters
    use m_longculverts
    use m_lateral, only : numlatsg, nNodesLat, lat_ids, qplat, qplatAve, qLatRealAve, geomXLat, geomYLat, nlatnd, qLatReal, nodeCountLat

    implicit none

    double precision, intent(in) :: tim                  !< Current time, should in fact be time1, since the data written is always s1, ucx, etc.

    ! locals
    integer, save :: id_laydim , id_laydimw, &
                     id_statdim, id_strlendim, id_crsdim, id_crslendim, id_crsptsdim, id_timedim, &
                     id_statx, id_staty, id_statid, id_statname, id_time, id_timestep, &
                     id_statlon, id_statlat, id_crsname, &
                     id_vars, id_varucx, id_varucy, id_varucz, id_varsal, id_vartem, id_varsed, id_varrhop, id_varrho, id_bruv,  &
                     id_varQ, id_varQint, id_varb, id_varumag, id_varqmag,&
                     id_varAu,  &
                     id_varu,  id_varwx, id_varwy, id_varrain, id_varpatm, &
                     id_infiltcap, id_infiltact, &
                     id_qsun, id_qeva, id_qcon, id_qlong, id_qfreva, id_qfrcon, id_qtot, &
                     id_turkin, id_tureps , id_vicwwu, id_rich, id_zcs, id_zws, id_zwu, &
                     id_wind, id_tair, id_rhum, id_clou, id_airdensity, &
                     id_R, id_WH, id_WD, id_WL, id_WT, id_WU, id_hs, &
                     id_pumpdim,    id_pump_id,     id_pump_dis,     id_pump_cap,      id_pump_s1up,      id_pump_s1dn,     id_pump_head,      &
                     id_pump_xmid,  id_pump_ymid,   id_pump_struhead,id_pump_stage,    id_pump_redufact,  id_pump_s1del,    id_pump_s1suc,     id_pump_disdir, &
                     id_gatedim,    id_gatename,    id_gate_dis,    id_gate_edgel,     id_gate_s1up,      id_gate_s1dn,    &
                     id_cdamdim,    id_cdamname,    id_cdam_dis,    id_cdam_crestl,    id_cdam_s1up,      id_cdam_s1dn,    &
                     id_weirgendim, id_weirgen_id, id_weirgen_dis, id_weirgen_crestl, id_weirgen_crestw, id_weirgen_s1up,  id_weirgen_s1dn,  &
                     id_weir_stat,  id_weirgen_vel, id_weirgen_au,  id_weirgen_head,   id_weirgen_forcedif, id_weirgen_s1crest,               &
                     id_gategendim, id_gategenname, id_gategen_dis, id_gategen_sillh,  id_gategen_sillw,  id_gategen_edgel, id_gategen_openw, &
                     id_gategen_flowh, id_gategen_s1up, id_gategen_s1dn,                                                                      &
                     id_genstrudim, id_genstru_id, id_genstru_dis, id_genstru_crestl, id_genstru_crestw, id_genstru_edgel, id_genstru_openw, &
                     id_genstru_s1up, id_genstru_s1dn, id_genstru_dis_gate_open, id_genstru_dis_gate_over, id_genstru_dis_gate_under, id_genstru_openh, id_genstru_uppl,  &
                     id_genstru_vel, id_genstru_au, id_genstru_au_open, id_genstru_au_over, id_genstru_au_under, id_genstru_stat, id_genstru_head,  id_genstru_velgateopen, &
                     id_genstru_velgateover, id_genstru_velgateunder, id_genstru_s1crest, id_genstru_forcedif, &
                     id_orifgendim, id_orifgen_id, id_orifgen_dis, id_orifgen_crestl, id_orifgen_crestw, id_orifgen_edgel, id_orifgen_stat,  &
                     id_orifgen_s1dn, id_orifgen_openh, id_orifgen_vel, id_orifgen_au, id_orifgen_s1up, id_orifgen_head, id_orifgen_s1crest, id_orifgen_forcedif,&
                     id_bridgedim, id_bridge_id, id_bridge_dis, id_bridge_s1up,  id_bridge_s1dn, id_bridge_vel, id_bridge_au,  id_bridge_head, &
                     id_bridge_blup, id_bridge_bldn, id_bridge_bl_act, &
                     id_culvertdim, id_culvert_id, id_culvert_dis, id_culvert_s1up,  id_culvert_s1dn, id_culvert_crestl, id_culvert_openh, &
                     id_culvert_edgel, id_culvert_vel, id_culvert_stat, id_culvert_au,  id_culvert_head, &
                     id_sedbtrans, id_sedstrans,&
                     id_srcdim, id_srclendim, id_srcname, id_qsrccur, id_vsrccum, id_qsrcavg, id_pred, id_presa, id_pretm, id_srcx, id_srcy, id_srcptsdim, &
                     id_partdim, id_parttime, id_partx, id_party, id_partz, &
                     id_dredlinkdim, id_dreddim, id_dumpdim, id_dredlink_dis, id_dred_dis, id_dump_dis, id_dred_tfrac, id_plough_tfrac, id_sedtotdim, id_dred_name, id_dump_name, id_frac_name, &
                     id_dambreakdim, id_dambreak_id, id_dambreak_s1up, id_dambreak_s1dn, id_dambreak_discharge, id_dambreak_cumulative_discharge, &
                     id_dambreak_au, id_dambreak_head, id_dambreak_cresth, id_dambreak_crestw, &
                     id_uniweirdim, id_uniweir_id, id_uniweir_dis, id_uniweir_s1up,  id_uniweir_s1dn, id_uniweir_crestl, &
                     id_uniweir_vel, id_uniweir_au, id_uniweir_head, &
                     id_dambreak_breach_width_time_derivative, id_dambreak_water_level_jump, id_dambreak_normal_velocity, id_checkmon, id_num_timesteps, id_comp_time, &
                     id_cmpstrudim, id_cmpstru_id, id_cmpstru_dis, id_cmpstru_s1up,  id_cmpstru_s1dn, &
                     id_cmpstru_vel, id_cmpstru_au, id_cmpstru_head, &
                     id_longculvertdim, id_longculvert_id, id_longculvert_dis, id_longculvert_s1up,  id_longculvert_s1dn, id_longculvert_vel, id_longculvert_au,  id_longculvert_head, id_longculvert_valveopen,&
                     id_sscx, id_sscy, id_sswx, id_sswy, id_sbcx, id_sbcy, id_sbwx, id_sbwy, &
                     id_varucxq, id_varucyq, id_sf, id_ws, id_seddif, id_sink, id_sour, id_sedsusdim, &
                     id_latdim, id_lat_id, id_lat_predis_inst, id_lat_predis_ave, id_lat_realdis_inst, id_lat_realdis_ave, &
                     id_ustx, id_usty, id_nlyrdim, id_bodsed, id_dpsed, id_msed, id_thlyr, id_poros, id_lyrfrac, id_frac, id_mudfrac, id_sandfrac, id_fixfac, id_hidexp, id_taub, id_mfluff, &
                     id_rugdim, id_rugx, id_rugy, id_rugid, id_rugname, id_varruh, id_taux, id_tauy
    ! ids for geometry variables, only use them once at the first time of history output
    integer :: id_statgeom_node_count,        id_statgeom_node_coordx,        id_statgeom_node_coordy,    &
                                              id_statgeom_node_lon,           id_statgeom_node_lat,       &
               id_crsgeom_node_count,         id_crsgeom_node_coordx,         id_crsgeom_node_coordy,     &
               id_weirgeom_node_count,        id_weirgeom_node_coordx,        id_weirgeom_node_coordy,    &
               id_orifgeom_node_count,        id_orifgeom_node_coordx,        id_orifgeom_node_coordy,    &
               id_genstrugeom_node_count,     id_genstrugeom_node_coordx,     id_genstrugeom_node_coordy, &
               id_uniweirgeom_node_count,     id_uniweirgeom_node_coordx,     id_uniweirgeom_node_coordy, &
               id_culvertgeom_node_count,     id_culvertgeom_node_coordx,     id_culvertgeom_node_coordy, &
               id_gategengeom_node_count,     id_gategengeom_node_coordx,     id_gategengeom_node_coordy, &
               id_pumpgeom_node_count,        id_pumpgeom_node_coordx,        id_pumpgeom_node_coordy,    &
               id_bridgegeom_node_count,      id_bridgegeom_node_coordx,      id_bridgegeom_node_coordy,  &
               id_srcgeom_node_count,         id_srcgeom_node_coordx,         id_srcgeom_node_coordy,     &
               id_latgeom_node_count,         id_latgeom_node_coordx,         id_latgeom_node_coordy,     &
               id_longculvertgeom_node_count, id_longculvertgeom_node_coordx, id_longculvertgeom_node_coordy

    double precision, allocatable :: geom_x(:), geom_y(:)
    integer, allocatable          :: node_count(:)
    integer, allocatable, save :: id_tra(:)
    integer, allocatable, save :: id_hwq(:)
    integer, allocatable, save :: id_hwqb(:)
    integer, allocatable, save :: id_hwqb3d(:)
    integer, allocatable, save :: id_const(:), id_const_cum(:), id_voltot(:)
    integer, allocatable, save :: id_sedbtransfrac(:)
    integer, allocatable, save :: id_sedstransfrac(:)
    double precision, allocatable, save :: valobsT(:,:)
    integer :: maxlocT, maxvalT !< row+column count of valobsT

    integer                      :: IP, num, ntmp, n, nlyrs

    double precision, save       :: curtime_split = 0d0 ! Current time-partition that the file writer has open.
    integer                      :: ntot, k, i, j, jj, ierr, mnp, kk, idims(3),L, Lf, k3, k4, nNodeTot, nNodes, L0, k1, k2, nlinks
    character(len=255)           :: station_geom_container_name, crs_geom_container_name, weir_geom_container_name, orif_geom_container_name, &
                                    genstru_geom_container_name, uniweir_geom_container_name, culvert_geom_container_name, longculvert_geom_container_name, &
                                    gategen_geom_container_name, pump_geom_container_name, bridge_geom_container_name, src_geom_container_name, &
                                    lat_geom_container_name
    double precision             :: cof0

    integer                      :: strlen_netcdf  ! string length definition for (station) names on history file
    character(len=255)           :: filename
    character(len=25)            :: transpunit
    character(len=1024)          :: statcoordstring
    integer                      :: igen, istru
    integer                      :: ndims
    character(len=255)           :: tmpstr
    integer                      :: jawrizc = 0
    integer                      :: jawrizw = 0
    double precision             :: w1, pumplensum, pumplenmid, pumpxmid, pumpymid
    double precision             :: rhol
    double precision, allocatable:: toutput1(:), toutputx(:,:), toutputy(:,:), toutput3(:,:,:)
    double precision, allocatable:: toutput_cum, toutput_cur
    integer                      :: lsed
    logical                      :: add_latlon
    
    integer                      :: nc_precision

    if (jahiszcor > 0) then
       jawrizc = 1
       jawrizw = 1
    endif

    nc_precision = netcdf_data_type(md_nc_his_precision)

    if (timon) call timstrt ( "unc_write_his", handle_extra(54))

    ! Another time-partitioned file needs to start, reset iteration count (and file).
    if (ti_split > 0d0 .and. curtime_split /= time_split0) then
        it_his        = 0
        curtime_split = time_split0
    end if

    ! Close/reset any previous hisfile.
    if (ihisfile/=0) then  ! reset stord ncid to zero if file not open
       ierr = nf90_inquire(ihisfile, ndims)
       if (ierr/=0) ihisfile = 0
    end if

    if (ihisfile > 0 .and. it_his == 0) then
        ierr = unc_close(ihisfile)
        ihisfile = 0
    end if

    ! When no crs/obs present, return immediately.
    if (numobs+nummovobs <= 0 .and. ncrs <= 0 .and. jahisbal <= 0 .and. jahiscgen <= 0 .and. nrug <= 0) then
        if (ihisfile == 0) then
            call mess(LEVEL_WARN, 'No observations nor cross sections defined. Will not produce a history file.')
        end if
        ihisfile = -1 ! -1 stands for: no file open, no obs/crs defined.
        return
    end if

    ! Only add auto-tranformed lat/lon coordinates if model is Cartesian and user has requested extra latlon output.
#ifdef HAVE_PROJ
    add_latlon = jsferic == 0 .and. iand(unc_writeopts, UG_WRITE_LATLON) == UG_WRITE_LATLON
#else
    add_latlon = .false.
#endif

    if (ihisfile == 0) then
        if (timon) call timstrt ( "unc_write_his INIT/DEF", handle_extra(61))

        call realloc(id_tra, ITRAN-ITRA1+1, keepExisting = .false.)
        call realloc(id_const, NUMCONST_MDU, keepExisting = .false.)
        call realloc(id_const_cum, NUMCONST_MDU, keepExisting = .false.)

        call realloc(id_voltot, MAX_IDX, keepExisting = .false.)

        ! Possibly a different model, so make valobs transpose at correct size again.
        maxlocT = max(size(valobs, 2), npumpsg, network%sts%numPumps, ngatesg, ncdamsg, ncgensg, ngategen, &
                      nweirgen, network%sts%numWeirs, ngenstru,  network%sts%numGeneralStructures, &
                      ndambreak, network%sts%numOrifices, network%sts%numBridges, network%sts%numculverts, &
                      network%sts%numuniweirs, network%cmps%count, nlongculverts)
        maxvalT = max(size(valobs, 1), NUMVALS_PUMP, NUMVALS_GATE, NUMVALS_CDAM, NUMVALS_CGEN, NUMVALS_GATEGEN, &
                      NUMVALS_WEIRGEN, NUMVALS_GENSTRU, &
                      NUMVALS_DAMBREAK, NUMVALS_ORIFGEN, NUMVALS_BRIDGE, NUMVALS_CULVERT, &
                      NUMVALS_UNIWEIR, NUMVALS_CMPSTRU, NUMVALS_LONGCULVERT)
        call realloc(valobsT, (/ maxlocT, maxvalT /), keepExisting = .false.)

        if (ti_split > 0d0) then
            filename = defaultFilename('his', timestamp=time_split0)
        else
            filename = defaultFilename('his')
        end if

        ierr = unc_create(filename, 0, ihisfile)
        if (ierr /= nf90_noerr) then
            call mess(LEVEL_WARN, 'Could not create history file.')
        end if

        ierr = unc_meta_add_user_defined(ihisfile)

        ierr = unc_add_time_coverage(ihisfile, ti_hiss, ti_hise, ti_his)

        !if (unc_nounlimited > 0) then ! UNST-4764: His file has never shown good results with NcNoUnlimited option on.
        ierr = nf90_def_dim(ihisfile, 'time', nf90_unlimited, id_timedim)

        strlen_netcdf = idlen  !< Max string length of Ids.
        ierr = nf90_def_dim(ihisfile, 'name_len', strlen_netcdf, id_strlendim)

        if (kmx > 0) then
           ierr = nf90_def_dim(ihisfile, 'laydim', kmx, id_laydim)
           ierr = nf90_def_dim(ihisfile, 'laydimw', kmx+1, id_laydimw)
        end if

        if (numobs+nummovobs > 0) then
            ierr = unc_addcoordmapping(ihisfile, jsferic)

            ierr = nf90_def_dim(ihisfile, 'stations', numobs+nummovobs, id_statdim)

            if (nummovobs > 0) then
               ierr = nf90_def_var(ihisfile, 'station_x_coordinate', nf90_double, (/ id_statdim, id_timedim /), id_statx) ! TODO: AvD: decide on UNST-1606 (trajectory_id vs. timeseries_id)
               ierr = nf90_def_var(ihisfile, 'station_y_coordinate', nf90_double, (/ id_statdim, id_timedim /), id_staty)
            else
               ierr = nf90_def_var(ihisfile, 'station_x_coordinate', nf90_double, id_statdim, id_statx)
               ierr = nf90_def_var(ihisfile, 'station_y_coordinate', nf90_double, id_statdim, id_staty)
            endif
            ierr = unc_addcoordatts(ihisfile, id_statx, id_staty, jsferic)
            ierr = nf90_put_att(ihisfile, id_statx, 'long_name', 'original x-coordinate of station (non-snapped)')
            ierr = nf90_put_att(ihisfile, id_staty, 'long_name', 'original y-coordinate of station (non-snapped)')

            statcoordstring = 'station_x_coordinate station_y_coordinate station_name'
            if (add_latlon) then
               ierr = ncu_clone_vardef(ihisfile, ihisfile, id_statx, 'station_lon', id_statlon, &
                             'longitude', 'original lon-coordinate of station (non-snapped)', 'degrees_east')
               ierr = ncu_clone_vardef(ihisfile, ihisfile, id_staty, 'station_lat', id_statlat, &
                             'latitude', 'original lat-coordinate of station (non-snapped)', 'degrees_north')

               statcoordstring = trim(statcoordstring) // ' station_lon station_lat'
            end if

            ierr = nf90_def_var(ihisfile, 'station_id',         nf90_char,   (/ id_strlendim, id_statdim /), id_statid)
            ierr = nf90_put_att(ihisfile, id_statid,  'long_name'    , 'observation station identifier') ! REF

            ierr = nf90_def_var(ihisfile, 'station_name',         nf90_char,   (/ id_strlendim, id_statdim /), id_statname)
            ierr = nf90_put_att(ihisfile, id_statname,  'cf_role', 'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_statname,  'long_name'    , 'observation station name') ! REF

            ! Define geometry related variables
            station_geom_container_name = 'station_geom'
            nNodeTot = numobs+nummovobs
            ierr = sgeom_def_geometry_variables(ihisfile, station_geom_container_name, 'station', 'point', nNodeTot, id_statdim, &
               id_statgeom_node_count, id_statgeom_node_coordx, id_statgeom_node_coordy, add_latlon, id_statgeom_node_lon, id_statgeom_node_lat)


            if ( jahiswatlev > 0 ) then
               ierr = nf90_def_var(ihisfile, 'waterlevel', nc_precision, (/ id_statdim, id_timedim /), id_vars)
               ierr = nf90_put_att(ihisfile, id_vars, 'standard_name', 'sea_surface_height') ! sorry for inland water people
               ierr = nf90_put_att(ihisfile, id_vars, 'long_name', 'water level')
               ierr = nf90_put_att(ihisfile, id_vars, 'units', 'm')
               ierr = nf90_put_att(ihisfile, id_vars, 'coordinates', statcoordstring)
               ierr = nf90_put_att(ihisfile, id_vars, 'geometry', station_geom_container_name)
               ierr = write_real_fill_value(id_vars)
            endif

            if ( jahisbedlev > 0 ) then
               if( stm_included ) then
                  ierr = nf90_def_var(ihisfile, 'bedlevel', nc_precision, (/ id_statdim, id_timedim /), id_varb)
               else
                  ierr = nf90_def_var(ihisfile, 'bedlevel', nc_precision, (/ id_statdim /), id_varb)
               endif
               ierr = nf90_put_att(ihisfile, id_varb, 'long_name', 'bottom level')
               ierr = nf90_put_att(ihisfile, id_varb, 'units', 'm')
               ierr = nf90_put_att(ihisfile, id_varb, 'coordinates', statcoordstring)
               ierr = nf90_put_att(ihisfile, id_varb, 'geometry', station_geom_container_name)
               ierr = write_real_fill_value(id_varb)
            endif

            idims(1) = id_statdim
            idims(2) = id_timedim
            if (jahiswatdep > 0) then
               call definencvar(ihisfile,id_hs, nc_precision, idims, 2, 'waterdepth'  , 'water depth', 'm', statcoordstring, station_geom_container_name)
            endif

            if( jahisvelvec > 0 ) then
               if ( kmx.gt.0 ) then
                  ierr = nf90_def_var(ihisfile, 'x_velocity', nc_precision, (/ id_laydim, id_statdim, id_timedim /), id_varucx)
                  ierr = nf90_put_att(ihisfile, id_varucx, 'coordinates', trim(statcoordstring) // ' zcoordinate_c')
                  ierr = nf90_def_var(ihisfile, 'y_velocity', nc_precision, (/ id_laydim, id_statdim, id_timedim /), id_varucy)
                  ierr = nf90_put_att(ihisfile, id_varucy, 'coordinates', trim(statcoordstring) // ' zcoordinate_c')
                  ierr = nf90_def_var(ihisfile, 'z_velocity', nc_precision, (/ id_laydim, id_statdim, id_timedim /), id_varucz)
                  ierr = nf90_put_att(ihisfile, id_varucz, 'standard_name', 'upward_sea_water_velocity')
                  ierr = nf90_put_att(ihisfile, id_varucz, 'long_name', 'vertical/upward component of flow element center velocity vector') ! sorry for inland water people
                  ierr = nf90_put_att(ihisfile, id_varucz, 'units', 'm s-1')
                  ierr = nf90_put_att(ihisfile, id_varucz, 'coordinates', trim(statcoordstring) // ' zcoordinate_c')
                  ierr = nf90_put_att(ihisfile, id_varucz, 'geometry', station_geom_container_name)
                  ierr = write_real_fill_value(id_varucz)
                  jawrizc = 1
                  ierr = nf90_def_var(ihisfile, 'depth-averaged_x_velocity', nc_precision, (/ id_statdim, id_timedim /), id_varucxq)
                  ierr = nf90_put_att(ihisfile, id_varucxq, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_varucxq, 'standard_name', 'sea_water_depth-averaged_x_velocity')
                  ierr = nf90_put_att(ihisfile, id_varucxq, 'long_name', 'flow element center depth-averaged velocity vector, x-component')
                  ierr = nf90_put_att(ihisfile, id_varucxq, 'units', 'm s-1')
                  ierr = nf90_put_att(ihisfile, id_varucxq, 'geometry', station_geom_container_name)
                  ierr = write_real_fill_value(id_varucxq)

                  ierr = nf90_def_var(ihisfile, 'depth-averaged_y_velocity', nc_precision, (/ id_statdim, id_timedim /), id_varucyq)
                  ierr = nf90_put_att(ihisfile, id_varucyq, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_varucyq, 'standard_name', 'sea_water_depth-averaged_y_velocity')
                  ierr = nf90_put_att(ihisfile, id_varucyq, 'long_name', 'flow element center depth-averaged velocity vector, y-component')
                  ierr = nf90_put_att(ihisfile, id_varucyq, 'units', 'm s-1')
                  ierr = nf90_put_att(ihisfile, id_varucyq, 'geometry', station_geom_container_name)
                  ierr = write_real_fill_value(id_varucyq)
               else
                  ierr = nf90_def_var(ihisfile, 'x_velocity', nc_precision, (/ id_statdim, id_timedim /), id_varucx)
                  ierr = nf90_put_att(ihisfile, id_varucx, 'coordinates', statcoordstring)
                  ierr = nf90_def_var(ihisfile, 'y_velocity', nc_precision, (/ id_statdim, id_timedim /), id_varucy)
                  ierr = nf90_put_att(ihisfile, id_varucy, 'coordinates', statcoordstring)
               end if
               ierr = nf90_put_att(ihisfile, id_varucx, 'standard_name', 'sea_water_x_velocity')
               ierr = nf90_put_att(ihisfile, id_varucx, 'long_name', 'x-component of flow element center velocity vector')
               ierr = nf90_put_att(ihisfile, id_varucx, 'units', 'm s-1')
               ierr = nf90_put_att(ihisfile, id_varucx, 'geometry', station_geom_container_name)
               ierr = write_real_fill_value(id_varucx)
               ierr = nf90_put_att(ihisfile, id_varucy, 'standard_name', 'sea_water_y_velocity')
               ierr = nf90_put_att(ihisfile, id_varucy, 'long_name', 'y-component of flow element center velocity vector')
               ierr = nf90_put_att(ihisfile, id_varucy, 'units', 'm s-1')
               ierr = nf90_put_att(ihisfile, id_varucy, 'geometry', station_geom_container_name)
               ierr = write_real_fill_value(id_varucy)
            endif

            if ( kmx.gt.0 ) then

               !idims(1) = id_laydim
               !idims(2) = id_statdim
               !idims(3) = id_timedim
               !call definencvar   (ihisfile, id_zcs, nf90_double, idims,3, 'zcoordinate_c' , 'vertical coordinate at center of flow element and layer'   , 'm',  'station_x_coordinate station_y_coordinate station_name zcoordinate_c')
               !ierr = nf90_put_att(ihisfile, id_zcs, 'positive' , 'up')

               idims(1) = id_laydimw
               idims(2) = id_statdim
               idims(3) = id_timedim
               !call definencvar   (ihisfile, id_zws, nf90_double, idims,3, 'zcoordinate_w' , 'vertical coordinate at center of flow element and at layer interface'   , 'm',  'station_x_coordinate station_y_coordinate station_name zcoordinate_w')
               !ierr = nf90_put_att(ihisfile, id_zws, 'positive' , 'up')

               if (iturbulencemodel >= 3 .and. jahistur > 0) then
                  call definencvar(ihisfile,id_turkin, nc_precision, idims,3, 'tke'   , 'turbulent kinetic energy'   , 'm2 s-2',  trim(statcoordstring) // ' zcoordinate_wu', station_geom_container_name, fillVal = dmiss)
                  ierr = nf90_put_att(ihisfile, id_turkin, 'standard_name', 'specific_turbulent_kinetic_energy_of_sea_water')
                  jawrizw = 1
               endif
               if (iturbulencemodel > 1 .and. jahistur > 0 ) then
                  call definencvar(ihisfile,id_vicwwu, nc_precision, idims,3, 'vicww' , 'turbulent vertical eddy viscosity'    , 'm2 s-1' ,  trim(statcoordstring) // ' zcoordinate_wu', station_geom_container_name, fillVal = dmiss)
                  jawrizw = 1
               endif
               if (iturbulencemodel == 3 .and. jahistur > 0) then
                  call definencvar(ihisfile,id_tureps, nc_precision, idims,3, 'eps'   , 'turbulent energy dissipation', 'm2 s-3'  ,  trim(statcoordstring) // ' zcoordinate_wu', station_geom_container_name, fillVal = dmiss)
                  ierr = nf90_put_att(ihisfile, id_tureps, 'standard_name', 'specific_turbulent_kinetic_energy_dissipation_in_sea_water')
                  jawrizw = 1
               else if (iturbulencemodel == 4 .and. jahistur > 0) then
                  call definencvar(ihisfile,id_tureps, nc_precision, idims,3, 'tau'   , 'turbulent time scale', 's-1'  ,  trim(statcoordstring) // ' zcoordinate_wu', station_geom_container_name, fillVal = dmiss)
                  jawrizw = 1
               endif

               if (jarichardsononoutput > 0) then
                  call definencvar(ihisfile,id_rich, nc_precision, idims,3, 'rich' , 'Richardson Nr'    , '  ' ,  trim(statcoordstring) // ' zcoordinate_wu', station_geom_container_name, fillVal = dmiss)
                  jawrizw = 1
               end if

            end if
            if (jsferic == 0) then
               ierr = nf90_put_att(ihisfile, id_varucx, 'standard_name', 'sea_water_x_velocity')
               ierr = nf90_put_att(ihisfile, id_varucy, 'standard_name', 'sea_water_y_velocity')
            else
               ierr = nf90_put_att(ihisfile, id_varucx, 'standard_name', 'eastward_sea_water_velocity')
               ierr = nf90_put_att(ihisfile, id_varucy, 'standard_name', 'northward_sea_water_velocity')
            end if

            if (jaeulervel==0) then
               ierr = nf90_put_att(ihisfile, id_varucx, 'long_name', 'flow element center velocity vector, x-component')
               ierr = nf90_put_att(ihisfile, id_varucy, 'long_name', 'flow element center velocity vector, y-component')
            else
               ierr = nf90_put_att(ihisfile, id_varucx, 'long_name', 'flow element center Eulerian velocity vector, x-component')
               ierr = nf90_put_att(ihisfile, id_varucy, 'long_name', 'flow element center Eulerian velocity vector, y-component')
            endif
            ierr = nf90_put_att(ihisfile, id_varucx, 'units', 'm s-1')
            ierr = nf90_put_att(ihisfile, id_varucy, 'units', 'm s-1')

            if (kmx > 0) then
               ierr = nf90_put_att(ihisfile, id_varucx, 'coordinates', trim(statcoordstring) // ' zcoordinate_c')
               ierr = nf90_put_att(ihisfile, id_varucy, 'coordinates', trim(statcoordstring) // ' zcoordinate_c')
            else
               ierr = nf90_put_att(ihisfile, id_varucx, 'coordinates', statcoordstring)
               ierr = nf90_put_att(ihisfile, id_varucy, 'coordinates', statcoordstring)
            end if

            ierr = nf90_put_att(ihisfile, id_varucx, 'geometry', station_geom_container_name)
            ierr = nf90_put_att(ihisfile, id_varucy, 'geometry', station_geom_container_name)
            ierr = write_real_fill_value(id_varucx)
            ierr = write_real_fill_value(id_varucy)


            if (jasal > 0 .and. jahissal > 0) then
               if ( kmx.gt.0 ) then
                  ierr = nf90_def_var(ihisfile, 'salinity', nc_precision, (/ id_laydim, id_statdim, id_timedim /), id_varsal)
                  ierr = nf90_put_att(ihisfile, id_varsal, 'coordinates', trim(statcoordstring) // ' zcoordinate_c')
                  jawrizc = 1
               else
                  ierr = nf90_def_var(ihisfile, 'salinity', nc_precision, (/ id_statdim, id_timedim /), id_varsal)
                  ierr = nf90_put_att(ihisfile, id_varsal, 'coordinates', statcoordstring)
               end if
               ierr = nf90_put_att(ihisfile, id_varsal, 'units', '1e-3')
               ierr = nf90_put_att(ihisfile, id_varsal, 'geometry', station_geom_container_name)
               ierr = write_real_fill_value(id_varsal)
               ierr = nf90_put_att(ihisfile, id_varsal, 'standard_name', 'sea_water_salinity')
            endif

            if ( jahisvelocity > 0 ) then
               if (kmx > 0) then
                  ierr = nf90_def_var(ihisfile, 'velocity_magnitude', nc_precision, (/ id_laydim, id_statdim, id_timedim /), id_varumag)
                  ierr = nf90_put_att(ihisfile, id_varumag, 'coordinates', trim(statcoordstring) // ' zcoordinate_c')
               else
                  ierr = nf90_def_var(ihisfile, 'velocity_magnitude', nc_precision, (/ id_statdim, id_timedim /), id_varumag)
                  ierr = nf90_put_att(ihisfile, id_varumag, 'coordinates', statcoordstring)
               endif
               if (jaeulervel==0) then
                  ierr = nf90_put_att(ihisfile, id_varumag, 'long_name', 'velocity magnitude')
                  ierr = nf90_put_att(ihisfile, id_varumag, 'standard_name', 'sea_water_speed')
               else
                  ierr = nf90_put_att(ihisfile, id_varumag, 'standard_name', 'sea_water_eulerian_speed')
                  ierr = nf90_put_att(ihisfile, id_varumag, 'long_name', 'Eulerian velocity magnitude')
               end if
               ierr = nf90_put_att(ihisfile, id_varumag, 'units', 'm s-1')
               ierr = nf90_put_att(ihisfile, id_varumag, 'geometry', station_geom_container_name)
               ierr = write_real_fill_value(id_varumag)
            endif

            if ( jahisdischarge > 0 ) then
               if (kmx > 0) then
                  ierr = nf90_def_var(ihisfile, 'discharge_magnitude', nc_precision, (/ id_laydim, id_statdim, id_timedim /), id_varqmag)
                  ierr = nf90_put_att(ihisfile, id_varqmag, 'coordinates', trim(statcoordstring))
               else
                  ierr = nf90_def_var(ihisfile, 'discharge_magnitude', nc_precision, (/ id_statdim, id_timedim /), id_varqmag)
                  ierr = nf90_put_att(ihisfile, id_varqmag, 'coordinates', trim(statcoordstring) // ' zcoordinate_c')
               endif

               ierr = nf90_put_att(ihisfile, id_varqmag, 'standard_name', 'water_volume_transport_in_river_channel')
               ierr = nf90_put_att(ihisfile, id_varqmag, 'long_name', 'average discharge magnitude')
               ierr = nf90_put_att(ihisfile, id_varqmag, 'units', 'm3 s-1')
               ierr = nf90_put_att(ihisfile, id_varqmag, 'coordinates', statcoordstring)
               ierr = nf90_put_att(ihisfile, id_varqmag, 'geometry', station_geom_container_name)
               ierr = write_real_fill_value(id_varqmag)
            endif


!           JRE surfbeat
            if (jawave .eq. 4) then

               ierr = nf90_def_var(ihisfile, 'R',  nc_precision, ((/ id_statdim, id_timedim /)) , id_R)
               ierr = nf90_put_att(ihisfile, id_R,   'coordinates'  , statcoordstring)
               ierr = nf90_put_att(ihisfile, id_R,   'standard_name', 'sea_surface_bulk_roller_energy')                          ! not CF
               ierr = nf90_put_att(ihisfile, id_R,   'long_name'    , 'roller energy per square meter')
               ierr = nf90_put_att(ihisfile, id_R,   'units'        , 'J m-2')
               ierr = nf90_put_att(ihisfile, id_R, 'geometry', station_geom_container_name)
               ierr = write_real_fill_value(id_R)

            end if

            if (jawave > 0 .and. jahiswav > 0) then
               ierr = nf90_def_var(ihisfile, 'hwav',  nc_precision, ((/ id_statdim, id_timedim /)) , id_WH)
               ierr = nf90_put_att(ihisfile, id_WH,   'coordinates'  , statcoordstring)
               ierr = nf90_put_att(ihisfile, id_WH,   'standard_name', 'sea_surface_wave_significant_wave_height')     ! Default behaviour
               ierr = nf90_put_att(ihisfile, id_WH,   'long_name'    , 'Significant wave height')
               if (jahissigwav==0) then
                  ierr = nf90_put_att(ihisfile, id_WH,   'standard_name', 'sea_surface_wave_rms_height')
                  ierr = nf90_put_att(ihisfile, id_WH,   'long_name'    , 'Root mean square wave height based on wave energy')
               end  if
               ierr = nf90_put_att(ihisfile, id_WH,   'units'        , 'm')
               ierr = nf90_put_att(ihisfile, id_WH, 'geometry', station_geom_container_name)
               ierr = write_real_fill_value(id_WH)

               ierr = nf90_def_var(ihisfile, 'twav',  nc_precision, ((/ id_statdim, id_timedim /)) , id_WT)
               ierr = nf90_put_att(ihisfile, id_WT,   'coordinates'  , statcoordstring)
               ierr = nf90_put_att(ihisfile, id_WT,   'standard_name', 'sea_surface_wave_period')
               ierr = nf90_put_att(ihisfile, id_WT,   'long_name'    , 'Wave period')
               ierr = nf90_put_att(ihisfile, id_WT,   'units'        , 's')
               ierr = nf90_put_att(ihisfile, id_WT, 'geometry', station_geom_container_name)
               ierr = write_real_fill_value(id_WT)

               ierr = nf90_def_var(ihisfile, 'phiwav',  nc_precision, ((/ id_statdim, id_timedim /)) , id_WD)
               ierr = nf90_put_att(ihisfile, id_WD,   'coordinates'  , statcoordstring)
               ierr = nf90_put_att(ihisfile, id_WD,   'standard_name', 'sea_surface_wave_from_direction')
               ierr = nf90_put_att(ihisfile, id_WD,   'long_name'    , 'Wave from direction')
               ierr = nf90_put_att(ihisfile, id_WD,   'units'        , 'deg from N')
               ierr = nf90_put_att(ihisfile, id_WD, 'geometry', station_geom_container_name)
               ierr = write_real_fill_value(id_WD)

               ierr = nf90_def_var(ihisfile, 'rlabda',  nc_precision, ((/ id_statdim, id_timedim /)) , id_WL)
               ierr = nf90_put_att(ihisfile, id_WL,   'coordinates'  , statcoordstring)
               ierr = nf90_put_att(ihisfile, id_WL,   'standard_name', 'sea_surface_wave_length')
               ierr = nf90_put_att(ihisfile, id_WL,   'long_name'    , 'Wave length')
               ierr = nf90_put_att(ihisfile, id_WL,   'units'        , 'm')
               ierr = nf90_put_att(ihisfile, id_WL, 'geometry', station_geom_container_name)
               ierr = write_real_fill_value(id_WL)

               ierr = nf90_def_var(ihisfile, 'uorb',  nc_precision, ((/ id_statdim, id_timedim /)) , id_WU)
               ierr = nf90_put_att(ihisfile, id_WU,   'coordinates'  , statcoordstring)
               ierr = nf90_put_att(ihisfile, id_WU,   'standard_name', 'sea_surface_wave_orbital_velocity')
               ierr = nf90_put_att(ihisfile, id_WU,   'long_name'    , 'Orbital velocity')
               ierr = nf90_put_att(ihisfile, id_WU,   'units'        , 'm/s')
               ierr = nf90_put_att(ihisfile, id_WU, 'geometry', station_geom_container_name)
               ierr = write_real_fill_value(id_WU)

               if (.not. flowWithoutWaves) then
                  if (kmx==0) then
                      ierr = nf90_def_var(ihisfile, 'ustokes',  nc_precision, ((/ id_statdim, id_timedim /)) , id_USTX)
                      ierr = nf90_put_att(ihisfile, id_USTX,   'coordinates'  , statcoordstring)
                      ierr = nf90_def_var(ihisfile, 'vstokes',  nc_precision, ((/ id_statdim, id_timedim /)) , id_USTY)
                      ierr = nf90_put_att(ihisfile, id_USTY,   'coordinates'  , statcoordstring)
                  else
                      ierr = nf90_def_var(ihisfile, 'ustokes',  nc_precision, ((/ id_laydim, id_statdim, id_timedim /)) , id_USTX)
                      ierr = nf90_put_att(ihisfile, id_USTX,   'coordinates'  , trim(statcoordstring) // ' zcoordinate_c')
                      ierr = nf90_def_var(ihisfile, 'vstokes',  nc_precision, ((/ id_laydim, id_statdim, id_timedim /)) , id_USTY)
                      ierr = nf90_put_att(ihisfile, id_USTY,   'coordinates'  , trim(statcoordstring) // ' zcoordinate_c')
                      jawrizc = 1
                  endif

                  ierr = nf90_put_att(ihisfile, id_USTX,   'standard_name', 'sea_surface_wave_stokes_drift_x')
                  ierr = nf90_put_att(ihisfile, id_USTX,   'long_name'    , 'Stokes drift, x-component')
                  ierr = nf90_put_att(ihisfile, id_USTX,   'units'        , 'm s-1')
                  ierr = write_real_fill_value(id_USTX)

                  ierr = nf90_put_att(ihisfile, id_USTY,   'standard_name', 'sea_surface_wave_stokes_drift_y')
                  ierr = nf90_put_att(ihisfile, id_USTY,   'long_name'    , 'Stokes drift, y-component')
                  ierr = nf90_put_att(ihisfile, id_USTY,   'units'        , 'm s-1')
                  ierr = write_real_fill_value(id_USTY)
               endif
            endif

            if (jahistaucurrent>0) then

               ierr = nf90_def_var(ihisfile, 'tausx',  nc_precision, ((/ id_statdim, id_timedim /)) , id_TAUX)
               ierr = nf90_put_att(ihisfile, id_TAUX, 'coordinates'  , statcoordstring)
               ierr = nf90_put_att(ihisfile, id_TAUX, 'standard_name', 'mean_bottom_shear_stress vector, x-component')
               ierr = nf90_put_att(ihisfile, id_TAUX, 'long_name'    , 'Mean bottom shear stress vector, x-component')
               ierr = nf90_put_att(ihisfile, id_TAUX, 'units'        , 'Pa')
               ierr = nf90_put_att(ihisfile, id_TAUX, 'geometry', station_geom_container_name)
               ierr = write_real_fill_value(id_TAUX)

               ierr = nf90_def_var(ihisfile, 'tausy',  nc_precision, ((/ id_statdim, id_timedim /)) , id_TAUY)
               ierr = nf90_put_att(ihisfile, id_TAUY, 'coordinates'  , statcoordstring)
               ierr = nf90_put_att(ihisfile, id_TAUY, 'standard_name', 'mean_bottom_shear_stress vector, y-component')
               ierr = nf90_put_att(ihisfile, id_TAUY, 'long_name'    , 'Mean bottom shear stress vector, y-component')
               ierr = nf90_put_att(ihisfile, id_TAUY, 'units'        , 'Pa')
               ierr = nf90_put_att(ihisfile, id_TAUY, 'geometry', station_geom_container_name)
               ierr = write_real_fill_value(id_TAUY)
            endif

            if (jatem > 0 .and. jahistem > 0) then
               if ( kmx.gt.0 ) then
                  ierr = nf90_def_var(ihisfile, 'temperature', nc_precision, (/ id_laydim, id_statdim, id_timedim /), id_vartem)
                  ierr = nf90_put_att(ihisfile, id_vartem, 'coordinates', trim(statcoordstring) // ' zcoordinate_c')
                  jawrizc = 1
               else
                  ierr = nf90_def_var(ihisfile, 'temperature', nc_precision, (/ id_statdim, id_timedim /), id_vartem)
                  ierr = nf90_put_att(ihisfile, id_vartem, 'coordinates', statcoordstring)
               end if
               ierr = nf90_put_att(ihisfile, id_vartem, 'units', 'degC')
               ierr = nf90_put_att(ihisfile, id_vartem, 'geometry', station_geom_container_name)
               ierr = write_real_fill_value(id_vartem)
               ierr = nf90_put_att(ihisfile, id_vartem, 'standard_name', 'sea_water_temperature')

               if (jatem > 1 .and. jahisheatflux > 0) then ! here less verbose
                  idims(1) = id_statdim
                  idims(2) = id_timedim
                  call definencvar(ihisfile,id_wind, nc_precision,idims,2, 'wind'  , 'windspeed', 'm s-1', statcoordstring, station_geom_container_name)
                  call definencvar(ihisfile,id_tair, nc_precision,idims,2, 'Tair'  , 'air temperature', 'degC', statcoordstring, station_geom_container_name)
                  if (jatem == 5) then
                     call definencvar(ihisfile,id_rhum, nc_precision,idims,2, 'rhum'  , 'relative humidity', ' ',statcoordstring, station_geom_container_name)
                     call definencvar(ihisfile,id_clou, nc_precision,idims,2, 'clou'  , 'cloudiness', ' ', statcoordstring, station_geom_container_name)

                     call definencvar(ihisfile,id_qsun, nc_precision,idims,2, 'Qsun'  , 'solar influx', 'W m-2', statcoordstring, station_geom_container_name)
                     call definencvar(ihisfile,id_Qeva, nc_precision,idims,2, 'Qeva'  , 'evaporative heat flux', 'W m-2', statcoordstring, station_geom_container_name)
                     call definencvar(ihisfile,id_Qcon, nc_precision,idims,2, 'Qcon'  , 'sensible heat flux', 'W m-2', statcoordstring, station_geom_container_name)
                     call definencvar(ihisfile,id_Qlong, nc_precision,idims,2, 'Qlong' , 'long wave back radiation', 'W m-2', statcoordstring, station_geom_container_name)
                     call definencvar(ihisfile,id_Qfreva, nc_precision,idims,2, 'Qfreva', 'free convection evaporative heat flux', 'W m-2', statcoordstring, station_geom_container_name)
                     call definencvar(ihisfile,id_Qfrcon, nc_precision,idims,2, 'Qfrcon', 'free convection sensible heat flux', 'W m-2', statcoordstring, station_geom_container_name)
                  endif
                  if (jatem > 1) then
                     call definencvar(ihisfile,id_Qtot, nc_precision,idims,2, 'Qtot'  , 'total heat flux', 'W m-2', statcoordstring, station_geom_container_name)
                  end if
               endif
            endif

            if (ja_airdensity + ja_computed_airdensity > 0 .and. jahis_airdensity > 0) then
               call definencvar(ihisfile,id_airdensity, nc_precision,(/ id_statdim, id_timedim /),2, 'rhoair'  , 'air density', 'kg m-3 ', statcoordstring, station_geom_container_name)
            endif

            if ((jasal > 0 .or. jatem > 0 .or. jased > 0) .and. jahisrho > 0) then
               if ( kmx.gt.0 ) then
                  idims(1) = id_laydim
                  idims(2) = id_statdim
                  jawrizc = 1
                  if ( density_is_pressure_dependent() ) then
                      call definencvar(ihisfile,id_varrho,  nc_precision, idims,3, 'density',           'density',           'kg m-3',  trim(statcoordstring) // ' zcoordinate_c', station_geom_container_name, fillVal = dmiss)
                  else
                      call definencvar(ihisfile,id_varrhop, nc_precision, idims,3, 'potential_density', 'potential_density', 'kg m-3',  trim(statcoordstring) // ' zcoordinate_c', station_geom_container_name, fillVal = dmiss)
                  endif
                  idims(1) = id_laydimw
                  call definencvar(ihisfile,id_bruv   , nc_precision, idims,3,'Brunt_Vaisala_N2'  , 'Brunt_Vaisala_N2'  , '1/s2'   ,  trim(statcoordstring) // ' zcoordinate_w', station_geom_container_name, fillVal = dmiss)
               else
                  call definencvar(ihisfile,id_varrhop, nc_precision, idims,2,'potential_density'       , 'potential_density'         , 'kg m-3' ,  trim(statcoordstring)                    , station_geom_container_name, fillVal = dmiss)
               end if  ! en misschien kan iemand de kmx > 0 repetitie ook in definencvar afvangen en // ' zcoordinate_c' en idims,3  automatisch maken voor kmx>0
            endif

            if (ITRA1 > 0 .and. jahisconst > 0) then
               do j=ITRA1,ITRAN
                  i = j-ITRA1+1  ! tracer nr

                  tmpstr = const_names(j)
                  ! Forbidden chars in NetCDF names: space, /, and more.
                  call replace_char(tmpstr,32,95)

                  if ( kmx > 0 ) then
                     ierr = nf90_def_var(ihisfile, trim(tmpstr), nc_precision, (/ id_laydim, id_statdim, id_timedim /), id_tra(i))
                     ierr = nf90_put_att(ihisfile, id_tra(i), 'coordinates', trim(statcoordstring) // ' zcoordinate_c')
                     jawrizc = 1
                  else
                     ierr = nf90_def_var(ihisfile, trim(tmpstr), nc_precision, (/ id_statdim, id_timedim /), id_tra(i))
                     ierr = nf90_put_att(ihisfile, id_tra(i), 'coordinates', statcoordstring)
                  end if
                  if (const_units(j).ne.' ') then
                     tmpstr = const_units(j)
                  else
                     tmpstr = '-'
                  endif
                  ierr = nf90_put_att(ihisfile, id_tra(i), 'units', tmpstr)
                  ierr = nf90_put_att(ihisfile, id_tra(i), 'geometry', station_geom_container_name)
                  ierr = write_real_fill_value(id_tra(i))
                  ierr = nf90_put_att(ihisfile, id_tra(i), 'long_name', const_names(j))
               enddo
            endif

            if (numwqbots > 0) then
               call realloc(id_hwqb, numwqbots, keepExisting = .false.)
               do i=1,numwqbots
                  tmpstr = wqbotnames(i)
                  ! Forbidden chars in NetCDF names: space, /, and more.
                  call replace_char(tmpstr,32,95)

                  ierr = nf90_def_var(ihisfile, trim(tmpstr), nc_precision, (/ id_statdim, id_timedim /), id_hwqb(i))
                  ierr = nf90_put_att(ihisfile, id_hwqb(i), 'coordinates', statcoordstring)

                  tmpstr = wqbotunits(i)
                  ierr = nf90_put_att(ihisfile, id_hwqb(i), 'units', tmpstr)
                  ierr = nf90_put_att(ihisfile, id_hwqb(i), 'geometry', station_geom_container_name)
                  ierr = write_real_fill_value(id_hwqb(i))
                  ierr = nf90_put_att(ihisfile, id_hwqb(i), 'long_name', wqbotnames(i))
               enddo
               if (wqbot3D_output == 1) then
                  call realloc(id_hwqb3d, numwqbots, keepExisting = .false.)
                  do i=1,numwqbots
                     tmpstr = wqbotnames(i)
                     ! Forbidden chars in NetCDF names: space, /, and more.
                     call replace_char(tmpstr,32,95)

                     ierr = nf90_def_var(ihisfile, trim(tmpstr)//'_3D', nc_precision, (/ id_laydim, id_statdim, id_timedim /), id_hwqb3d(i))
                     ierr = nf90_put_att(ihisfile, id_hwqb3d(i), 'coordinates', statcoordstring)

                     tmpstr = wqbotunits(i)
                     ierr = nf90_put_att(ihisfile, id_hwqb3d(i), 'units', tmpstr)
                     ierr = nf90_put_att(ihisfile, id_hwqb3d(i), 'geometry', station_geom_container_name)
                     ierr = write_real_fill_value(id_hwqb3d(i))
                     ierr = nf90_put_att(ihisfile, id_hwqb3d(i), 'long_name', trim(wqbotnames(i))//' (3D)')
                  enddo
            endif
            endif

!          waq output
             if(jawaqproc > 0) then
               if (noout_user > 0) then
                  call realloc(id_hwq, noout_user, keepExisting = .false.)
                  do j=1,noout_user
                     tmpstr = ' '
                     write (tmpstr, "('water_quality_output_',I0)") j
                     if ( kmx > 0 ) then  !        3D
                        ierr = nf90_def_var(ihisfile, trim(tmpstr), nc_precision, (/ id_laydim, id_statdim, id_timedim /), id_hwq(j))
                        ierr = nf90_put_att(ihisfile, id_hwq(j), 'coordinates', trim(statcoordstring) // ' zcoordinate_c')
                     else
                        ierr = nf90_def_var(ihisfile, trim(tmpstr), nc_precision, (/ id_statdim, id_timedim /), id_hwq(j))
                        ierr = nf90_put_att(ihisfile, id_hwq(j), 'coordinates', statcoordstring)
                     end if
                     tmpstr = trim(outputs%names(j))//' - '//trim(outputs%description(j))//' in flow element'
                     call replace_multiple_spaces_by_single_spaces(tmpstr)
                     ierr = write_real_fill_value(id_hwq(j))
                     ierr = nf90_put_att(ihisfile, id_hwq(j), 'long_name', trim(outputs%names(j)))
                     ierr = nf90_put_att(ihisfile, id_hwq(j), 'units', trim(outputs%units(j)))
                     ierr = nf90_put_att(ihisfile, id_hwq(j), 'description', tmpstr)
                     ierr = nf90_put_att(ihisfile, id_hwq(j), 'geometry', station_geom_container_name)
                  enddo
               endif
               if (noout_statt > 0) then
                  call realloc(id_hwq, noout_user + noout_statt, keepExisting = .true.)
                  do j=1,noout_statt
                     jj = noout_user + j
                     tmpstr = ' '
                     write (tmpstr, "('water_quality_stat_',I0)") j
                     if ( kmx > 0 ) then  !        3D
                        ierr = nf90_def_var(ihisfile, trim(tmpstr), nc_precision, (/ id_laydim, id_statdim, id_timedim /), id_hwq(jj))
                        ierr = nf90_put_att(ihisfile, id_hwq(jj), 'coordinates', trim(statcoordstring) // ' zcoordinate_c')
                     else
                        ierr = nf90_def_var(ihisfile, trim(tmpstr), nc_precision, (/ id_statdim, id_timedim /), id_hwq(jj))
                        ierr = nf90_put_att(ihisfile, id_hwq(jj), 'coordinates', statcoordstring)
                     end if
                     tmpstr = trim(outputs%names(jj))//' - '//trim(outputs%description(jj))//' in flow element'
                     call replace_multiple_spaces_by_single_spaces(tmpstr)
                     ierr = write_real_fill_value(id_hwq(jj))
                     ierr = nf90_put_att(ihisfile, id_hwq(jj), 'long_name', trim(outputs%names(jj)))
                     ierr = nf90_put_att(ihisfile, id_hwq(jj), 'units', trim(outputs%units(jj)))
                     ierr = nf90_put_att(ihisfile, id_hwq(jj), 'description', tmpstr)
                     ierr = nf90_put_att(ihisfile, id_hwq(jj), 'geometry', station_geom_container_name)
                  enddo
               endif
               if (noout_state> 0) then
                  call realloc(id_hwq, noout, keepExisting = .true.)
                  do j=1,noout_state
                     jj = noout_user + noout_statt + j
                     tmpstr = ' '
                     write (tmpstr, "('water_quality_stat_',I0)") noout_statt + j
                     if ( kmx > 0 ) then  !        3D
                        ierr = nf90_def_var(ihisfile, trim(tmpstr), nc_precision, (/ id_laydim, id_statdim /), id_hwq(jj))
                        ierr = nf90_put_att(ihisfile, id_hwq(jj), 'coordinates', statcoordstring)
                     else
                        ierr = nf90_def_var(ihisfile, trim(tmpstr), nc_precision, (/ id_statdim /), id_hwq(jj))
                        ierr = nf90_put_att(ihisfile, id_hwq(jj), 'coordinates', statcoordstring)
                     end if
                     tmpstr = trim(outputs%names(jj))//' - '//trim(outputs%description(jj))//' in flow element'
                     call replace_multiple_spaces_by_single_spaces(tmpstr)
                     ierr = write_real_fill_value(id_hwq(jj))
                     ierr = nf90_put_att(ihisfile, id_hwq(jj), 'long_name', trim(outputs%names(jj)))
                     ierr = nf90_put_att(ihisfile, id_hwq(jj), 'units', trim(outputs%units(jj)))
                     ierr = nf90_put_att(ihisfile, id_hwq(jj), 'description', tmpstr)
                     ierr = nf90_put_att(ihisfile, id_hwq(jj), 'geometry', station_geom_container_name)
                  enddo
               endif
            endif

            if (stm_included .and. ISED1 > 0 .and. jahissed > 0) then
               ! New implementation, sedsus fraction is additional dimension
               ierr = nf90_def_dim(ihisfile, 'nSedTot', stmpar%lsedtot, id_sedtotdim)
               ierr = nf90_def_dim(ihisfile, 'nSedSus', stmpar%lsedsus, id_sedsusdim)
               !
               ierr = nf90_def_var(ihisfile, 'sedfrac_name', nf90_char, (/ id_strlendim, id_sedtotdim /), id_frac_name)
               ierr = nf90_put_att(ihisfile, id_frac_name,'long_name', 'sediment fraction identifier')
               !
               if (kmx>0) then
                  ierr = nf90_def_var(ihisfile, 'sed', nc_precision, (/  id_laydim, id_statdim, id_sedsusdim, id_timedim /), id_sf)
                  ierr = nf90_def_var(ihisfile, 'ws', nc_precision, (/  id_laydimw, id_statdim, id_sedsusdim, id_timedim /), id_ws)
                  ierr = nf90_def_var(ihisfile, 'seddif', nc_precision, (/  id_laydimw, id_statdim, id_sedsusdim, id_timedim /), id_seddif)
                  ierr = nf90_put_att(ihisfile, id_seddif, 'long_name', 'Sediment vertical diffusion')
                  ierr = nf90_put_att(ihisfile, id_seddif, 'units', 'm2 s-1')
                  ierr = write_real_fill_value(id_seddif)
                  ierr = nf90_put_att(ihisfile, id_seddif, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_seddif, 'geometry', station_geom_container_name)
                  !
                  jawrizc = 1
                  jawrizw = 1
               else
                  ierr = nf90_def_var(ihisfile, 'sed', nc_precision, (/  id_statdim, id_sedsusdim, id_timedim /), id_sf)
                  ierr = nf90_def_var(ihisfile, 'ws', nc_precision, (/ id_statdim, id_sedsusdim, id_timedim /), id_ws)
               endif
               !
               ierr = nf90_put_att(ihisfile, id_sf, 'long_name', 'Sediment concentration')
               ierr = nf90_put_att(ihisfile, id_sf, 'units', 'kg m-3')
               ierr = nf90_put_att(ihisfile, id_sf, 'coordinates', statcoordstring)
               ierr = nf90_put_att(ihisfile, id_sf, 'geometry', station_geom_container_name)
               !
               ierr = nf90_put_att(ihisfile, id_ws, 'long_name', 'Sediment settling velocity')
               ierr = nf90_put_att(ihisfile, id_ws, 'units', 'm s-1')
               ierr = nf90_put_att(ihisfile, id_ws, 'coordinates', statcoordstring)
               ierr = nf90_put_att(ihisfile, id_ws, 'geometry', station_geom_container_name)
               !
            endif
            !
            ! Sediment transports
            !
            if (jased > 0 .and. stm_included .and. jahissed > 0) then
               !
               if (stmpar%morpar%moroutput%taub) then
                  ierr = nf90_def_var(ihisfile, 'taub', nc_precision, (/ id_statdim, id_timedim /), id_taub)
                  ierr = nf90_put_att(ihisfile, id_taub, 'long_name', 'Bed shear stress for morphology')
                  ierr = nf90_put_att(ihisfile, id_taub, 'units', 'Pa')
                  ierr = write_real_fill_value(id_taub)
                  ierr = nf90_put_att(ihisfile, id_taub, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_taub, 'geometry', station_geom_container_name)
               endif
               !
               select case(stmpar%morpar%moroutput%transptype)
                  case (0)
                     transpunit = 'kg s-1 m-1'
                  case (1)
                     transpunit = 'm3 s-1 m-1'
                  case (2)
                     transpunit = 'm3 s-1 m-1'
               end select
               !
               if (stmpar%morpar%moroutput%sbcuv) then
                  ierr = nf90_def_var(ihisfile, 'sbcx', nc_precision, (/  id_statdim, id_sedtotdim, id_timedim /), id_sbcx)
                  ierr = nf90_put_att(ihisfile, id_sbcx, 'long_name', 'Current related bedload transport, x-component')
                  ierr = nf90_put_att(ihisfile, id_sbcx, 'units', transpunit)
                  ierr = nf90_put_att(ihisfile, id_sbcx, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_sbcx, 'geometry', station_geom_container_name)
                  ierr = nf90_def_var(ihisfile, 'sbcy', nc_precision, (/ id_statdim, id_sedtotdim, id_timedim /), id_sbcy)
                  ierr = nf90_put_att(ihisfile, id_sbcy, 'long_name', 'Current related bedload transport, y-component')
                  ierr = nf90_put_att(ihisfile, id_sbcy, 'units', transpunit)
                  ierr = nf90_put_att(ihisfile, id_sbcy, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_sbcy, 'geometry', station_geom_container_name)
               endif
               if (stmpar%morpar%moroutput%sbwuv .and. jawave>0 .and. .not. flowWithoutWaves) then
                  ierr = nf90_def_var(ihisfile, 'sbwx', nc_precision, (/ id_statdim, id_sedtotdim, id_timedim /), id_sbwx)
                  ierr = nf90_put_att(ihisfile, id_sbwx, 'long_name', 'Wave related bedload transport, x-component')
                  ierr = nf90_put_att(ihisfile, id_sbwx, 'units', transpunit)
                  ierr = nf90_put_att(ihisfile, id_sbwx, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_sbwx, 'geometry', station_geom_container_name)
                  ierr = nf90_def_var(ihisfile, 'sbwy', nc_precision, (/ id_statdim, id_sedtotdim, id_timedim /), id_sbwy)
                  ierr = nf90_put_att(ihisfile, id_sbwy, 'long_name', 'Wave related bedload transport, y-component')
                  ierr = nf90_put_att(ihisfile, id_sbwy, 'units', transpunit)
                  ierr = nf90_put_att(ihisfile, id_sbwy, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_sbwy, 'geometry', station_geom_container_name)
               endif
               if (stmpar%morpar%moroutput%sswuv .and. jawave>0 .and. .not. flowWithoutWaves) then
                  ierr = nf90_def_var(ihisfile, 'sswx', nc_precision, (/ id_statdim, id_sedtotdim, id_timedim /), id_sswx)
                  ierr = nf90_put_att(ihisfile, id_sswx, 'long_name', 'Wave related suspended transport, x-component')
                  ierr = nf90_put_att(ihisfile, id_sswx, 'units', transpunit)
                  ierr = nf90_put_att(ihisfile, id_sswx, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_sswx, 'geometry', station_geom_container_name)
                  ierr = nf90_def_var(ihisfile, 'sswy', nc_precision, (/ id_statdim, id_sedtotdim, id_timedim /), id_sswy)
                  ierr = nf90_put_att(ihisfile, id_sswy, 'long_name', 'Wave related suspended transport, y-component')
                  ierr = nf90_put_att(ihisfile, id_sswy, 'units', transpunit)
                  ierr = nf90_put_att(ihisfile, id_sswy, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_sswy, 'geometry', station_geom_container_name)
               end if
               if (stmpar%morpar%moroutput%sscuv) then
                  ierr = nf90_def_var(ihisfile, 'sscx', nc_precision, (/ id_statdim, id_sedtotdim, id_timedim /), id_sscx)
                  ierr = nf90_put_att(ihisfile, id_sscx, 'long_name', 'Current related suspended transport, x-component')
                  ierr = nf90_put_att(ihisfile, id_sscx, 'units', transpunit)
                  ierr = nf90_put_att(ihisfile, id_sscx, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_sscx, 'geometry', station_geom_container_name)
                  ierr = nf90_def_var(ihisfile, 'sscy', nc_precision, (/ id_statdim, id_sedtotdim, id_timedim /), id_sscy)
                  ierr = nf90_put_att(ihisfile, id_sscy, 'long_name', 'Current related suspended transport, y-component')
                  ierr = nf90_put_att(ihisfile, id_sscy, 'units', transpunit)
                  ierr = nf90_put_att(ihisfile, id_sscy, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_sscy, 'geometry', station_geom_container_name)
               endif
               !
               ! Source and sink terms
               !
               if (stmpar%morpar%moroutput%sourcesink) then
                  ierr = nf90_def_var(ihisfile, 'sourse', nc_precision, (/ id_statdim, id_sedsusdim, id_timedim /), id_sour)
                  ierr = nf90_put_att(ihisfile, id_sour, 'long_name', 'Source term suspended sediment transport')
                  ierr = nf90_put_att(ihisfile, id_sour, 'units', 'kg m-3 s-1')
                  ierr = nf90_put_att(ihisfile, id_sour, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_sour, 'geometry', station_geom_container_name)

                  ierr = nf90_def_var(ihisfile, 'sinkse', nc_precision, (/ id_statdim, id_sedsusdim, id_timedim /), id_sink)
                  ierr = nf90_put_att(ihisfile, id_sink, 'long_name', 'Sink term suspended sediment transport')
                  ierr = nf90_put_att(ihisfile, id_sink, 'units', 's-1')
                  ierr = nf90_put_att(ihisfile, id_sink, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_sink, 'geometry', station_geom_container_name)
               endif
               !
               ! Bed composition variables
               !
               select case (stmpar%morlyr%settings%iunderlyr)
                  case (1)
                     ierr = nf90_def_var(ihisfile, 'bodsed', nc_precision, (/ id_statdim, id_sedtotdim, id_timedim /), id_bodsed)
                     ierr = nf90_put_att(ihisfile, id_bodsed, 'long_name', 'Available sediment mass in the bed')
                     ierr = nf90_put_att(ihisfile, id_bodsed, 'units', 'kg m-2')
                     ierr = write_real_fill_value(id_bodsed)
                     ierr = nf90_put_att(ihisfile, id_bodsed, 'coordinates', statcoordstring)
                     ierr = nf90_put_att(ihisfile, id_bodsed, 'geometry', station_geom_container_name)
                     !
                     ierr = nf90_def_var(ihisfile, 'dpsed', nc_precision, (/ id_statdim, id_timedim /), id_dpsed)
                     ierr = nf90_put_att(ihisfile, id_dpsed, 'long_name', 'Sediment thickness in the bed')
                     ierr = nf90_put_att(ihisfile, id_dpsed, 'units', 'm')
                     ierr = write_real_fill_value(id_dpsed)
                     ierr = nf90_put_att(ihisfile, id_dpsed, 'coordinates', statcoordstring)
                     ierr = nf90_put_att(ihisfile, id_dpsed, 'geometry', station_geom_container_name)
                  case (2)
                     ierr = nf90_def_dim(ihisfile, 'nBedLayers', stmpar%morlyr%settings%nlyr, id_nlyrdim)
                     !
                     ierr = nf90_def_var(ihisfile, 'msed', nc_precision, (/ id_nlyrdim, id_statdim, id_sedtotdim, id_timedim /), id_msed)
                     ierr = nf90_put_att(ihisfile, id_msed, 'long_name', 'Available sediment mass in a layer of the bed')
                     ierr = nf90_put_att(ihisfile, id_msed, 'units', 'kg m-2')
                     ierr = write_real_fill_value(id_msed)
                     ierr = nf90_put_att(ihisfile, id_msed, 'coordinates', statcoordstring)
                     ierr = nf90_put_att(ihisfile, id_msed, 'geometry', station_geom_container_name)
                     !
                     ierr = nf90_def_var(ihisfile, 'thlyr', nc_precision, (/ id_nlyrdim, id_statdim, id_timedim /), id_thlyr)
                     ierr = nf90_put_att(ihisfile, id_thlyr, 'long_name', 'Thickness of a layer of the bed')
                     ierr = nf90_put_att(ihisfile, id_thlyr, 'units', 'm')
                     ierr = write_real_fill_value(id_thlyr)
                     ierr = nf90_put_att(ihisfile, id_thlyr, 'coordinates', statcoordstring)
                     ierr = nf90_put_att(ihisfile, id_thlyr, 'geometry', station_geom_container_name)
                     !
                     if (stmpar%morlyr%settings%iporosity>0) then
                        ierr = nf90_def_var(ihisfile, 'poros', nc_precision, (/ id_nlyrdim, id_statdim, id_timedim /), id_poros)
                        ierr = nf90_put_att(ihisfile, id_poros, 'long_name', 'Porosity of a layer of the bed')
                        ierr = nf90_put_att(ihisfile, id_poros, 'units', '-')
                        ierr = write_real_fill_value(id_poros)
                        ierr = nf90_put_att(ihisfile, id_poros, 'coordinates', statcoordstring)
                        ierr = nf90_put_att(ihisfile, id_poros, 'geometry', station_geom_container_name)
            endif
            !
                     ierr = nf90_def_var(ihisfile, 'lyrfrac', nc_precision, (/ id_nlyrdim, id_statdim,  id_sedtotdim, id_timedim /), id_lyrfrac)
                     ierr = nf90_put_att(ihisfile, id_lyrfrac, 'long_name', 'Volume fraction in a layer of the bed')
                     ierr = nf90_put_att(ihisfile, id_lyrfrac, 'units', 'm')
                     ierr = write_real_fill_value(id_lyrfrac)
                     ierr = nf90_put_att(ihisfile, id_lyrfrac, 'coordinates', statcoordstring)
                     ierr = nf90_put_att(ihisfile, id_lyrfrac, 'geometry', station_geom_container_name)
               end select
		         !
		         if (stmpar%morpar%moroutput%frac) then
                  ierr = nf90_def_var(ihisfile, 'frac', nc_precision, (/ id_statdim, id_sedtotdim, id_timedim /), id_frac)
                  ierr = nf90_put_att(ihisfile, id_frac, 'long_name', 'Availability fraction in top layer')
                  ierr = nf90_put_att(ihisfile, id_frac, 'units', '-')
                  ierr = write_real_fill_value(id_frac)
                  ierr = nf90_put_att(ihisfile, id_frac, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_frac, 'geometry', station_geom_container_name)
               endif
               !
               if (stmpar%morpar%moroutput%mudfrac) then
                  ierr = nf90_def_var(ihisfile, 'mudfrac', nc_precision, (/ id_statdim, id_timedim /), id_mudfrac)
                  ierr = nf90_put_att(ihisfile, id_mudfrac, 'long_name', 'Mud fraction in top layer')
                  ierr = nf90_put_att(ihisfile, id_mudfrac, 'units', '-')
                  ierr = write_real_fill_value(id_mudfrac)
                  ierr = nf90_put_att(ihisfile, id_mudfrac, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_mudfrac, 'geometry', station_geom_container_name)
               endif
               !
               if (stmpar%morpar%moroutput%sandfrac) then
                  ierr = nf90_def_var(ihisfile, 'sandfrac', nc_precision, (/ id_statdim, id_timedim /), id_sandfrac)
                  ierr = nf90_put_att(ihisfile, id_sandfrac, 'long_name', 'Sand fraction in top layer')
                  ierr = nf90_put_att(ihisfile, id_sandfrac, 'units', '-')
                  ierr = write_real_fill_value(id_sandfrac)
                  ierr = nf90_put_att(ihisfile, id_sandfrac, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_sandfrac, 'geometry', station_geom_container_name)
               endif
               !
               if (stmpar%morpar%moroutput%fixfac) then
                  ierr = nf90_def_var(ihisfile, 'fixfac', nc_precision, (/ id_statdim, id_sedtotdim, id_timedim /), id_fixfac)
                  ierr = nf90_put_att(ihisfile, id_fixfac, 'long_name', 'Reduction factor due to limited sediment thickness')
                  ierr = nf90_put_att(ihisfile, id_fixfac, 'units', '-')
                  ierr = write_real_fill_value(id_fixfac)
                  ierr = nf90_put_att(ihisfile, id_fixfac, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_fixfac, 'geometry', station_geom_container_name)
               endif
               !
               if (stmpar%morpar%moroutput%hidexp) then
                  ierr = nf90_def_var(ihisfile, 'hidexp', nc_precision, (/ id_statdim, id_sedtotdim, id_timedim /), id_hidexp)
                  ierr = nf90_put_att(ihisfile, id_hidexp, 'long_name', 'Hiding and exposure factor')
                  ierr = nf90_put_att(ihisfile, id_hidexp, 'units', '-')
                  ierr = write_real_fill_value(id_hidexp)
                  ierr = nf90_put_att(ihisfile, id_hidexp, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_hidexp, 'geometry', station_geom_container_name)
               endif
               !
               if (stmpar%morpar%flufflyr%iflufflyr>0 .and. stmpar%lsedsus>0) then
                  ierr = nf90_def_var(ihisfile, 'mfluff', nc_precision, (/ id_statdim, id_sedsusdim, id_timedim /), id_mfluff)
                  ierr = nf90_put_att(ihisfile, id_mfluff, 'long_name', 'Sediment mass in fluff layer')
                  ierr = nf90_put_att(ihisfile, id_mfluff, 'units', '-')
                  ierr = write_real_fill_value(id_mfluff)
                  ierr = nf90_put_att(ihisfile, id_mfluff, 'coordinates', statcoordstring)
                  ierr = nf90_put_att(ihisfile, id_mfluff, 'geometry', station_geom_container_name)
               end if
            endif
            !
            if (jased > 0 .and. .not. stm_included .and. jahissed > 0) then
               if ( kmx.gt.0 ) then
                  ierr = nf90_def_var(ihisfile, 'sediment_concentration', nc_precision, (/ id_laydim, id_statdim, id_timedim /), id_varsed)
                  ierr = nf90_put_att(ihisfile, id_varsed, 'coordinates', trim(statcoordstring) // ' zcoordinate_c')
                  jawrizc = 1
               else
                  ierr = nf90_def_var(ihisfile, 'sediment_concentration', nc_precision, (/ id_statdim, id_timedim /), id_varsed)
                  ierr = nf90_put_att(ihisfile, id_varsed, 'coordinates', statcoordstring)
               end if
               ierr = nf90_put_att(ihisfile, id_varsed, 'units', 'kg m-3')
               ierr = nf90_put_att(ihisfile, id_varsed, 'geometry', station_geom_container_name)
               ierr = write_real_fill_value(id_varsed)
               ierr = nf90_put_att(ihisfile, id_varsed, 'long_name', 'sediment_concentration')
            endif

            if (japatm > 0 .and. jahiswind > 0) then
               call definencvar(ihisfile,id_varpatm, nc_precision,(/ id_statdim, id_timedim /),2, 'patm'  , 'atmospheric pressure', 'N m-2', statcoordstring, station_geom_container_name)
            endif

            if (jawind > 0 .and. jahiswind > 0) then
               ierr = nf90_def_var(ihisfile, 'windx', nc_precision, (/ id_statdim, id_timedim /), id_varwx)
               ierr = nf90_put_att(ihisfile, id_varwx, 'units', 'm s-1')
               ierr = write_real_fill_value(id_varwx)
               ierr = nf90_put_att(ihisfile, id_varwx, 'coordinates', statcoordstring)
               ierr = nf90_put_att(ihisfile, id_varwx, 'geometry', station_geom_container_name)
               ierr = nf90_def_var(ihisfile, 'windy', nc_precision, (/ id_statdim, id_timedim /), id_varwy)
               ierr = nf90_put_att(ihisfile, id_varwy, 'units', 'm s-1')
               ierr = write_real_fill_value(id_varwy)
               ierr = nf90_put_att(ihisfile, id_varwy, 'coordinates', statcoordstring)
               ierr = nf90_put_att(ihisfile, id_varwy, 'geometry', station_geom_container_name)
               if (jsferic == 0) then
                  ierr = nf90_put_att(ihisfile, id_varwx, 'standard_name', 'x_wind')
                  ierr = nf90_put_att(ihisfile, id_varwy, 'standard_name', 'y_wind')
               else
                  ierr = nf90_put_att(ihisfile, id_varwx, 'standard_name', 'eastward_wind')
                  ierr = nf90_put_att(ihisfile, id_varwy, 'standard_name', 'northward_wind')
               end if
               ierr = nf90_put_att(ihisfile, id_varwx, 'long_name', 'velocity of air on flow element center, x-component')
               ierr = nf90_put_att(ihisfile, id_varwy, 'long_name', 'velocity of air on flow element center, y-component')
            endif

            if (jarain > 0 .and. jahisrain > 0) then
               ierr = nf90_def_var(ihisfile, 'rain', nc_precision, (/ id_statdim, id_timedim /), id_varrain)
               ierr = nf90_put_att(ihisfile, id_varrain, 'units', 'mm day-1')
               ierr = write_real_fill_value(id_varrain)
               ierr = nf90_put_att(ihisfile, id_varrain, 'coordinates', statcoordstring)
               ierr = nf90_put_att(ihisfile, id_varrain, 'standard_name', 'lwe_precipitation_rate')
               ierr = nf90_put_att(ihisfile, id_varrain, 'long_name', 'precipitation depth per time unit')
               ierr = nf90_put_att(ihisfile, id_varrain, 'geometry', station_geom_container_name)
            endif

            if ((infiltrationmodel == DFM_HYD_INFILT_CONST .or. infiltrationmodel == DFM_HYD_INFILT_HORTON) .and. jahisinfilt > 0) then
               ierr = nf90_def_var(ihisfile, 'infiltration_cap', nc_precision, (/ id_statdim, id_timedim /), id_infiltcap)
               ierr = nf90_put_att(ihisfile, id_infiltcap, 'units', 'mm hr-1')
               ierr = write_real_fill_value(id_infiltcap)
               ierr = nf90_put_att(ihisfile, id_infiltcap, 'coordinates', statcoordstring)
               !ierr = nf90_put_att(ihisfile, id_infiltcap, 'standard_name', 'infiltration_rate)
               ierr = nf90_put_att(ihisfile, id_infiltcap, 'long_name', 'Infiltration capacity')
               ierr = nf90_put_att(ihisfile, id_infiltcap, 'geometry', station_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'infiltration_actual', nc_precision, (/ id_statdim, id_timedim /), id_infiltact)
               ierr = nf90_put_att(ihisfile, id_infiltact, 'units', 'mm hr-1')
               ierr = write_real_fill_value(id_infiltact)
               ierr = nf90_put_att(ihisfile, id_infiltact, 'coordinates', statcoordstring)
               ierr = nf90_put_att(ihisfile, id_infiltact, 'long_name', 'Actual infiltration rate')
               ierr = nf90_put_att(ihisfile, id_infiltact, 'geometry', station_geom_container_name)
            endif

            if (kmx.gt.0 .and. jawrizc == 1) then
               idims(1) = id_laydim
               idims(2) = id_statdim
               idims(3) = id_timedim
               call definencvar   (ihisfile, id_zcs, nc_precision, idims,3, 'zcoordinate_c' , 'vertical coordinate at center of flow element and layer'   , 'm',  'station_x_coordinate station_y_coordinate station_name zcoordinate_c', station_geom_container_name)
               ierr = nf90_put_att(ihisfile, id_zcs, 'positive' , 'up')
               ierr = write_real_fill_value(id_zcs)
            endif
            if (kmx.gt.0 .and. jawrizw == 1) then
               idims(1) = id_laydimw
               idims(2) = id_statdim
               idims(3) = id_timedim
               call definencvar   (ihisfile, id_zws, nc_precision, idims,3, 'zcoordinate_w' , 'vertical coordinate at centre of flow element and at layer interface'   , 'm',  'station_x_coordinate station_y_coordinate station_name zcoordinate_w', station_geom_container_name)
               ierr = nf90_put_att(ihisfile, id_zws, 'positive' , 'up')
               ierr = write_real_fill_value(id_zws)
               call definencvar   (ihisfile, id_zwu, nc_precision, idims,3, 'zcoordinate_wu' , 'vertical coordinate at edge of flow element and at layer interface'   , 'm',  'station_x_coordinate station_y_coordinate station_name zcoordinate_wu', station_geom_container_name)
               ierr = nf90_put_att(ihisfile, id_zwu, 'positive' , 'up')
               ierr = write_real_fill_value(id_zwu)
            endif
        end if

        if (ncrs > 0) then
            mnp = 0
            do i=1,ncrs
                mnp = max(mnp, crs(i)%path%np)
            end do
            ierr = nf90_def_dim(ihisfile, 'cross_section', ncrs, id_crsdim)
            ierr = nf90_def_dim(ihisfile, 'cross_section_name_len', strlen_netcdf, id_crslendim)
            ierr = nf90_def_dim(ihisfile, 'cross_section_pts', mnp+1, id_crsptsdim)

            !ierr = nf90_def_var(ihisfile, 'cross_section_x_coordinate', nf90_double, (/ id_crsptsdim, id_crsdim /), id_crsx)
            !ierr = nf90_def_var(ihisfile, 'cross_section_y_coordinate', nf90_double, (/ id_crsptsdim, id_crsdim /), id_crsy)
            ierr = nf90_def_var(ihisfile, 'cross_section_name',         nf90_char,   (/ id_crslendim, id_crsdim /), id_crsname)
            ierr = nf90_put_att(ihisfile, id_crsname,  'cf_role', 'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_crsname,  'long_name', 'cross section name'    )
            !ierr = unc_addcoordatts(ihisfile, id_crsx, id_crsy, jsferic)

            ! Define geometry related variables
            crs_geom_container_name = 'cross_section_geom'
            nNodeTot = nNodesCrs

            ierr = sgeom_def_geometry_variables(ihisfile, crs_geom_container_name, 'cross section', 'line', nNodeTot, id_crsdim, &
               id_crsgeom_node_count, id_crsgeom_node_coordx, id_crsgeom_node_coordy)

            ierr = nf90_def_var(ihisfile, 'cross_section_discharge', nc_precision, (/ id_crsdim, id_timedim /), id_varQ)
            ierr = nf90_put_att(ihisfile, id_varQ,    'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_varQ,    'coordinates', 'cross_section_name')
            ierr = nf90_put_att(ihisfile, id_varQ, 'geometry', crs_geom_container_name)
            ierr = nf90_def_var(ihisfile, 'cross_section_cumulative_discharge', nc_precision, (/ id_crsdim, id_timedim /), id_varQint)
            ierr = nf90_put_att(ihisfile, id_varQint, 'units', 'm3')
            ierr = nf90_put_att(ihisfile, id_varQint, 'coordinates', 'cross_section_name')
            ierr = nf90_put_att(ihisfile, id_varQint, 'geometry', crs_geom_container_name)
            !ierr = nf90_def_var(ihisfile, 'cross_section_discharge_avg', nf90_double, (/ id_crsdim, id_timedim /), id_varQavg)
            !ierr = nf90_put_att(ihisfile, id_varQavg, 'units', 'm3 s-1')
            !ierr = nf90_put_att(ihisfile, id_varQavg, 'coordinates', 'cross_section_name')

            ierr = nf90_def_var(ihisfile, 'cross_section_area', nc_precision, (/ id_crsdim, id_timedim /), id_varAu)
            ierr = nf90_put_att(ihisfile, id_varAu,    'units', 'm2')
            ierr = nf90_put_att(ihisfile, id_varAu,    'coordinates', 'cross_section_name')
            ierr = nf90_put_att(ihisfile, id_varAu, 'geometry', crs_geom_container_name)
            !ierr = nf90_def_var(ihisfile, 'cross_section_area_avg', nf90_double, (/ id_crsdim, id_timedim /), id_varAuavg)
            !ierr = nf90_put_att(ihisfile, id_varAuavg, 'units', 'm2')
            !ierr = nf90_put_att(ihisfile, id_varAuavg, 'coordinates', 'cross_section_name')

            ierr = nf90_def_var(ihisfile, 'cross_section_velocity', nc_precision, (/ id_crsdim, id_timedim /), id_varu)
            ierr = nf90_put_att(ihisfile, id_varu,    'units', 'm s-1')
            ierr = nf90_put_att(ihisfile, id_varu,    'coordinates', 'cross_section_name')
            ierr = nf90_put_att(ihisfile, id_varu, 'geometry', crs_geom_container_name)
            ! Disable writing cross_section_velocity_avg (see UNST-1148), because in a parallel run, it is impossible to compute
            ! summation of area (denominator) at each computational time step in a cheap way, i.e. without communication between
            ! partitions. @see subroutines: sumvalueOnCrossSections, updateValuesOnCrossSections
            !ierr = nf90_def_var(ihisfile, 'cross_section_velocity_avg', nf90_double, (/ id_crsdim, id_timedim /), id_varuavg)
            !ierr = nf90_put_att(ihisfile, id_varuavg, 'units', 'm s-1')
            !ierr = nf90_put_att(ihisfile, id_varuavg, 'coordinates', 'cross_section_name')

            do num = 1,NUMCONST_MDU
               tmpstr = const_names(num)
               ! Forbidden chars in NetCDF names: space, /, and more.
               call replace_char(tmpstr,32,95)
               call replace_char(tmpstr,47,95)
               ierr = nf90_def_var(ihisfile, 'cross_section_cumulative_'//trim(tmpstr), nc_precision, (/ id_crsdim, id_timedim /), id_const_cum(num))
               ierr = nf90_put_att(ihisfile, id_const_cum(num), 'long_name', 'cumulative flux (based on upwind flow cell) for '//trim(tmpstr)//'.')

               ierr = nf90_def_var(ihisfile, 'cross_section_'//trim(tmpstr), nc_precision, (/ id_crsdim, id_timedim /), id_const(num))
               ierr = nf90_put_att(ihisfile, id_const(num), 'long_name', 'flux (based on upwind flow cell) for '//trim(tmpstr)//'.')

               if (num >= ISED1 .and. num <= ISEDN) then    ! if the constituent is sediment
                  select case(stmpar%morpar%moroutput%transptype)
                  case (0)
                     tmpstr = 'kg'
                  case (1, 2)
                     tmpstr = 'm3'
                  end select
               else
                  if (const_units(num) /= ' ') then
                     tmpstr = trim(const_units(num)) // ' m3'
                  else
                     tmpstr = '-'
                  endif
               endif
               ierr = nf90_put_att(ihisfile, id_const_cum(num), 'units', tmpstr)
               ierr = nf90_put_att(ihisfile, id_const_cum(num), 'coordinates', 'cross_section_name')
               ierr = nf90_put_att(ihisfile, id_const_cum(num), 'geometry', crs_geom_container_name)

               if (num >= ISED1 .and. num <= ISEDN) then    ! if the constituent is sediment
                  select case(stmpar%morpar%moroutput%transptype)
                  case (0)
                     tmpstr = 'kg/s'
                  case (1, 2)
                     tmpstr = 'm3/s'
                  end select
               else
                  if (const_units(num) /= ' ') then
                     tmpstr = trim(const_units(num)) // ' m3/s'
                  else
                     tmpstr = '-'
                  endif
               endif
               ierr = nf90_put_att(ihisfile, id_const(num), 'units', tmpstr)
               ierr = nf90_put_att(ihisfile, id_const(num), 'coordinates', 'cross_section_name')
               ierr = nf90_put_att(ihisfile, id_const(num), 'geometry', crs_geom_container_name)
            enddo
         endif

         if( jased == 4 ) then 
            if ( stmpar%lsedtot > 0 ) then
               ierr = nf90_def_var(ihisfile, 'cross_section_bedload_sediment_transport', nc_precision, (/ id_crsdim, id_timedim /), id_sedbtrans)
               ierr = nf90_put_att(ihisfile, id_sedbtrans, 'long_name', 'cumulative bed load transport')
               ierr = nf90_put_att(ihisfile, id_sedbtrans, 'units', 'kg')
               ierr = nf90_put_att(ihisfile, id_sedbtrans, 'coordinates', 'cross_section_name')
               ierr = nf90_put_att(ihisfile, id_sedbtrans, 'geometry', crs_geom_container_name)
               if (.not. allocated(id_sedbtransfrac)) then
                  allocate(id_sedbtransfrac(stmpar%lsedtot))
                  id_sedbtransfrac = 0
               endif
               do lsed = 1,stmpar%lsedtot    ! Making bedload on crosssections per fraction
                  ierr = nf90_def_var(ihisfile, 'cross_section_bedload_sediment_transport_'//trim(stmpar%sedpar%namsed(lsed)), nc_precision, &
                      (/ id_crsdim, id_timedim /), id_sedbtransfrac(lsed))
                  ierr = nf90_put_att(ihisfile, id_sedbtransfrac(lsed), 'long_name', 'cumulative bed load transport '//trim(stmpar%sedpar%namsed(lsed)))
                  ierr = nf90_put_att(ihisfile, id_sedbtransfrac(lsed), 'units', 'kg')
                  ierr = nf90_put_att(ihisfile, id_sedbtransfrac(lsed), 'coordinates', 'cross_section_name')
                  ierr = nf90_put_att(ihisfile, id_sedbtransfrac(lsed), 'geometry', crs_geom_container_name)
               enddo
            endif
            if( stmpar%lsedsus > 0 ) then
               ierr = nf90_def_var(ihisfile, 'cross_section_suspended_sediment_transport', nc_precision, (/ id_crsdim, id_timedim /), id_sedstrans)
               ierr = nf90_put_att(ihisfile, id_sedstrans, 'long_name', 'cumulative suspended load transport')
               ierr = nf90_put_att(ihisfile, id_sedstrans, 'units', 'kg')
               ierr = nf90_put_att(ihisfile, id_sedstrans, 'coordinates', 'cross_section_name')
               ierr = nf90_put_att(ihisfile, id_sedstrans, 'geometry', crs_geom_container_name)
               if (.not. allocated(id_sedstransfrac)) then
                  allocate(id_sedstransfrac(stmpar%lsedsus))
                  id_sedstransfrac = 0
               endif
               do lsed = 1,stmpar%lsedsus    ! Making suspended load on crosssections per fraction
                  ierr = nf90_def_var(ihisfile, 'cross_section_suspended_sediment_transport_'//trim(stmpar%sedpar%namsed(lsed)), nc_precision, &
                      (/ id_crsdim, id_timedim /), id_sedstransfrac(lsed))
                  ierr = nf90_put_att(ihisfile, id_sedstransfrac(lsed), 'long_name', 'cumulative suspended load transport '//trim(stmpar%sedpar%namsed(lsed)))
                  ierr = nf90_put_att(ihisfile, id_sedstransfrac(lsed), 'units', 'kg')
                  ierr = nf90_put_att(ihisfile, id_sedstransfrac(lsed), 'coordinates', 'cross_section_name')
                  ierr = nf90_put_att(ihisfile, id_sedstransfrac(lsed), 'geometry', crs_geom_container_name)
               enddo
            endif
         endif

         ! runup gauges
         if (nrug > 0) then
            ierr = nf90_def_dim(ihisfile, 'runupgauges', nrug, id_rugdim)

            ierr = nf90_def_var(ihisfile, 'rug_x_coordinate', nc_precision, (/ id_rugdim, id_timedim /), id_rugx)
            ierr = nf90_def_var(ihisfile, 'rug_y_coordinate', nc_precision, (/ id_rugdim, id_timedim /), id_rugy)

            ierr = unc_addcoordatts(ihisfile, id_rugx, id_rugy, jsferic)
            ierr = nf90_put_att(ihisfile, id_rugx, 'long_name', 'time-varying x-coordinate of shoreline position')
            ierr = nf90_put_att(ihisfile, id_rugy, 'long_name', 'time-varying y-coordinate of shoreline position')

            ierr = nf90_def_var(ihisfile, 'rug_id', nf90_char,   (/ id_strlendim, id_rugdim /), id_rugid)
            ierr = nf90_put_att(ihisfile, id_rugid,  'long_name'    , 'runup gauge identifier') ! REF

            ierr = nf90_def_var(ihisfile, 'rug_name', nf90_char,   (/ id_strlendim, id_rugdim /), id_rugname)
            ierr = nf90_put_att(ihisfile, id_rugname,  'cf_role', 'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_rugname,  'long_name'    , 'runup gauge name') ! REF

            ierr = nf90_def_var(ihisfile, 'runup_height', nc_precision, (/ id_rugdim, id_timedim /), id_varruh)
            ierr = nf90_put_att(ihisfile, id_varruh, 'standard_name', 'runup_height')
            ierr = nf90_put_att(ihisfile, id_varruh, 'long_name', 'runup height')
            ierr = nf90_put_att(ihisfile, id_varruh, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_varruh, 'coordinates', 'rug_x_coordinate rug_y_coordinate rug_name')
            ierr = write_real_fill_value(id_varruh)
         endif

         if (jahissourcesink > 0 .and. numsrc > 0) then
            ierr = nf90_def_dim(ihisfile, 'source_sink', numsrc, id_srcdim)
            ierr = nf90_def_dim(ihisfile, 'source_sink_name_len', strlen_netcdf, id_srclendim)
            ierr = nf90_def_dim(ihisfile, 'source_sink_pts', msrc, id_srcptsdim)

            ierr = nf90_def_var(ihisfile, 'source_sink_name', nf90_char, (/ id_srclendim, id_srcdim/), id_srcname)
            ierr = nf90_put_att(ihisfile, id_srcname,  'cf_role', 'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_srcname,  'long_name', 'source and sink name'    )

            ! Define geometry related variables
            src_geom_container_name = 'source_sink_geom'
            nNodeTot = 0
            do i = 1, numsrc
               nNodes = 0
               k1 = ksrc(1,i)
               k2 = ksrc(4,i)
               if (k1 /= 0) then
                  nNodes = nNodes + 1
               end if
               if (k2 /= 0) then
                  nNodes = nNodes + 1
               end if
               nNodeTot = nNodeTot + nNodes
            end do

            ierr = sgeom_def_geometry_variables(ihisfile, src_geom_container_name, 'source_sink', 'line', nNodeTot, id_srcdim, &
               id_srcgeom_node_count, id_srcgeom_node_coordx, id_srcgeom_node_coordy)

            ierr = nf90_def_var(ihisfile, 'source_sink_x_coordinate', nc_precision, (/ id_srcdim, id_srcptsdim  /), id_srcx)
            ierr = nf90_def_var(ihisfile, 'source_sink_y_coordinate', nc_precision, (/ id_srcdim, id_srcptsdim /), id_srcy)
            ierr = unc_addcoordatts(ihisfile, id_srcx, id_srcy, jsferic)
            ierr = write_real_fill_value(id_srcx)
            ierr = write_real_fill_value(id_srcy)

            ierr = nf90_def_var(ihisfile, 'source_sink_prescribed_discharge', nc_precision, (/ id_srcdim, id_timedim /), id_pred)
            ierr = nf90_put_att(ihisfile, id_pred,    'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_pred,    'coordinates', 'source_sink_name')
            ierr = nf90_put_att(ihisfile, id_pred,    'geometry', src_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'source_sink_prescribed_salinity_increment', nc_precision, (/ id_srcdim, id_timedim /), id_presa)
            ierr = nf90_put_att(ihisfile, id_presa,    'units', '1e-3')
            ierr = nf90_put_att(ihisfile, id_presa,    'coordinates', 'source_sink_name')
            ierr = nf90_put_att(ihisfile, id_presa,    'geometry', src_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'source_sink_prescribed_temperature_increment', nc_precision, (/ id_srcdim, id_timedim /), id_pretm)
            ierr = nf90_put_att(ihisfile, id_pretm,    'units', 'degC')
            ierr = nf90_put_att(ihisfile, id_pretm,    'coordinates', 'source_sink_name')
            ierr = nf90_put_att(ihisfile, id_pretm,    'geometry', src_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'source_sink_current_discharge', nc_precision, (/ id_srcdim, id_timedim /), id_qsrccur)
            ierr = nf90_put_att(ihisfile, id_qsrccur,    'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_qsrccur,    'coordinates', 'source_sink_name')
            ierr = nf90_put_att(ihisfile, id_qsrccur,    'geometry', src_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'source_sink_cumulative_volume', nc_precision, (/ id_srcdim, id_timedim /), id_vsrccum)
            ierr = nf90_put_att(ihisfile, id_vsrccum,    'long_name', 'Cumulative volume from the start time until current time at each source/sink')
            ierr = nf90_put_att(ihisfile, id_vsrccum,    'units', 'm3')
            ierr = nf90_put_att(ihisfile, id_vsrccum,    'coordinates', 'source_sink_name')
            ierr = nf90_put_att(ihisfile, id_vsrccum,    'geometry', src_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'source_sink_discharge_average', nc_precision, (/ id_srcdim, id_timedim /), id_qsrcavg)
            ierr = nf90_put_att(ihisfile, id_qsrcavg,    'long_name', 'Average discharge in the past his-file output-interval at each source/sink')
            ierr = nf90_put_att(ihisfile, id_qsrcavg,    'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_qsrcavg,    'coordinates', 'source_sink_name')
            ierr = nf90_put_att(ihisfile, id_qsrcavg,    'geometry', src_geom_container_name)
         endif

         if (timon) call timstrt ( "unc_write_his DEF bal", handle_extra(59))
         if (jahisbal > 0) then
            do num = 1,MAX_IDX
               if ( num.eq.IDX_InternalTidesDissipation ) then
                  if ( jaFrcInternalTides2D.eq.1 ) then
                     ierr = nf90_def_var(ihisfile, trim(voltotname(num)), nc_precision, (/ id_timedim /), id_voltot(num))
                     ierr = nf90_put_att(ihisfile, id_voltot(num), 'units', 'TJ')
                  end if
               else if ( num.eq.IDX_GravInput ) then
                  if ( jatidep > 0 ) then
                     ierr = nf90_def_var(ihisfile, trim(voltotname(num)), nc_precision, (/ id_timedim /), id_voltot(num))
                     ierr = nf90_put_att(ihisfile, id_voltot(num), 'units', 'TJ')
                  end if
               else if ( num.eq.IDX_SALInput .or. num.eq.IDX_SALInput2 ) then
                  if ( jaselfal.gt.0 ) then
                     ierr = nf90_def_var(ihisfile, trim(voltotname(num)), nc_precision, (/ id_timedim /), id_voltot(num))
                     ierr = nf90_put_att(ihisfile, id_voltot(num), 'units', 'TJ')
                  end if
               else
                  ierr = nf90_def_var(ihisfile, 'water_balance_'//trim(voltotname(num)), nc_precision, (/ id_timedim /), id_voltot(num))
                  ierr = nf90_put_att(ihisfile, id_voltot(num), 'units', 'm3')
               end if
            enddo
         end if
         if (timon) call timstop (handle_extra(59))

         if (timon) call timstrt ( "unc_write_his DEF structures", handle_extra(60))
         if (jaoldstr == 1) then
            ntmp = ncgensg
         else
            ntmp = ngenstru
         end if
         if(jahiscgen > 0 .and. ntmp > 0) then

            ierr = nf90_def_dim(ihisfile, 'general_structures', ntmp, id_genstrudim)
            ierr = nf90_def_var(ihisfile, 'general_structure_id',  nf90_char,   (/ id_strlendim, id_genstrudim /), id_genstru_id)
            ierr = nf90_put_att(ihisfile, id_genstru_id,  'cf_role',   'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_genstru_id,  'long_name', 'Id of general structure'    )

            ! Define geometry related variables
            genstru_geom_container_name = 'general_structure_geom'
            nNodeTot = 0
            if (network%sts%numGeneralStructures > 0) then ! new general structure
               nNodeTot = nNodesGenstru
            else ! old general structure
               do n = 1, ngenstru
                  i = genstru2cgen(n)
                  nlinks = L2cgensg(i) - L1cgensg(i) + 1
                  if (nlinks > 0) then
                     nNodes = nlinks + 1
                  else if (nlinks == 0) then
                     nNodes = 0
                  end if
                  nNodeTot = nNodeTot + nNodes
               end do
            end if

            ierr = sgeom_def_geometry_variables(ihisfile, genstru_geom_container_name, 'general_structure', 'line', nNodeTot, id_genstrudim, &
               id_genstrugeom_node_count, id_genstrugeom_node_coordx, id_genstrugeom_node_coordy)


            ierr = nf90_def_var(ihisfile, 'general_structure_discharge', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_dis)
            !ierr = nf90_put_att(ihisfile, id_genstru_dis, 'standard_name', 'integral_of_discharge_wrt_time') ! TODO: introduce time windows in nc
            ierr = nf90_put_att(ihisfile, id_genstru_dis, 'long_name', 'Total discharge through general structure')
            ierr = nf90_put_att(ihisfile, id_genstru_dis, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_genstru_dis, 'coordinates', 'general_structure_id')
            ierr = nf90_put_att(ihisfile, id_genstru_dis, 'geometry', genstru_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'general_structure_crest_level', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_crestl)
            !ierr = nf90_put_att(ihisfile, id_genstru_crestl, 'standard_name', 'cgen_crest_level')
            ierr = nf90_put_att(ihisfile, id_genstru_crestl, 'long_name', 'Crest level of general structure')
            ierr = nf90_put_att(ihisfile, id_genstru_crestl, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_genstru_crestl, 'coordinates', 'general_structure_id')
            ierr = nf90_put_att(ihisfile, id_genstru_crestl, 'geometry', genstru_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'general_structure_gate_lower_edge_level', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_edgel)
            ierr = nf90_put_att(ihisfile, id_genstru_edgel, 'long_name', 'Gate lower edge level of general structure')
            ierr = nf90_put_att(ihisfile, id_genstru_edgel, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_genstru_edgel, 'coordinates', 'general_structure_id')
            ierr = nf90_put_att(ihisfile, id_genstru_edgel, 'geometry', genstru_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'general_structure_gate_opening_width', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_openw)
            ierr = nf90_put_att(ihisfile, id_genstru_openw, 'long_name', 'Gate opening width of general structure')
            ierr = nf90_put_att(ihisfile, id_genstru_openw, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_genstru_openw, 'coordinates', 'general_structure_id')
            ierr = nf90_put_att(ihisfile, id_genstru_openw, 'geometry', genstru_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'general_structure_s1up', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_s1up)
            ierr = nf90_put_att(ihisfile, id_genstru_s1up, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_genstru_s1up, 'long_name', 'Water level upstream of general structure')
            ierr = nf90_put_att(ihisfile, id_genstru_s1up, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_genstru_s1up, 'coordinates', 'general_structure_id')
            ierr = nf90_put_att(ihisfile, id_genstru_s1up, 'geometry', genstru_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'general_structure_s1dn', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_s1dn)
            ierr = nf90_put_att(ihisfile, id_genstru_s1dn, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_genstru_s1dn, 'long_name', 'Water level downstream of general structure')
            ierr = nf90_put_att(ihisfile, id_genstru_s1dn, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_genstru_s1dn, 'coordinates', 'general_structure_id')
            ierr = nf90_put_att(ihisfile, id_genstru_s1dn, 'geometry', genstru_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'general_structure_head', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_head)
            ierr = nf90_put_att(ihisfile, id_genstru_head, 'long_name', 'Head difference across general structure')
            ierr = nf90_put_att(ihisfile, id_genstru_head, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_genstru_head, 'coordinates', 'general_structure_id')
            ierr = nf90_put_att(ihisfile, id_genstru_head, 'geometry', genstru_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'general_structure_flow_area', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_au)
            ierr = nf90_put_att(ihisfile, id_genstru_au, 'long_name', 'Flow area at general structure')
            ierr = nf90_put_att(ihisfile, id_genstru_au, 'units', 'm2')
            ierr = nf90_put_att(ihisfile, id_genstru_au, 'coordinates', 'general_structure_id')
            ierr = nf90_put_att(ihisfile, id_genstru_au, 'geometry', genstru_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'general_structure_velocity', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_vel)
            ierr = nf90_put_att(ihisfile, id_genstru_vel, 'long_name', 'Velocity through general structure')
            ierr = nf90_put_att(ihisfile, id_genstru_vel, 'units', 'm s-1')
            ierr = nf90_put_att(ihisfile, id_genstru_vel, 'coordinates', 'general_structure_id')
            ierr = nf90_put_att(ihisfile, id_genstru_vel, 'geometry', genstru_geom_container_name)

            if (network%sts%numGeneralStructures > 0) then ! write extra fields for new general structure
               ierr = nf90_def_var(ihisfile, 'general_structure_crest_width', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_crestw)
               ierr = nf90_put_att(ihisfile, id_genstru_crestw, 'long_name', 'Crest width of general structure')
               ierr = nf90_put_att(ihisfile, id_genstru_crestw, 'units', 'm')
               ierr = nf90_put_att(ihisfile, id_genstru_crestw, 'coordinates', 'general_structure_id')
               ierr = nf90_put_att(ihisfile, id_genstru_crestw, 'geometry', genstru_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'general_structure_discharge_through_gate_opening', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_dis_gate_open)
               ierr = nf90_put_att(ihisfile, id_genstru_dis_gate_open, 'long_name', 'Discharge through gate opening of general structure')
               ierr = nf90_put_att(ihisfile, id_genstru_dis_gate_open, 'units', 'm3 s-1')
               ierr = nf90_put_att(ihisfile, id_genstru_dis_gate_open, 'coordinates', 'general_structure_id')
               ierr = nf90_put_att(ihisfile, id_genstru_dis_gate_open, 'geometry', genstru_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'general_structure_discharge_over_gate', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_dis_gate_over)
               ierr = nf90_put_att(ihisfile, id_genstru_dis_gate_over, 'long_name', 'Discharge over gate of general structure')
               ierr = nf90_put_att(ihisfile, id_genstru_dis_gate_over, 'units', 'm3 s-1')
               ierr = nf90_put_att(ihisfile, id_genstru_dis_gate_over, 'coordinates', 'general_structure_id')
               ierr = nf90_put_att(ihisfile, id_genstru_dis_gate_over, 'geometry', genstru_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'general_structure_discharge_under_gate', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_dis_gate_under)
               ierr = nf90_put_att(ihisfile, id_genstru_dis_gate_under, 'long_name', 'Discharge under gate of general structure')
               ierr = nf90_put_att(ihisfile, id_genstru_dis_gate_under, 'units', 'm3 s-1')
               ierr = nf90_put_att(ihisfile, id_genstru_dis_gate_under, 'coordinates', 'general_structure_id')
               ierr = nf90_put_att(ihisfile, id_genstru_dis_gate_under, 'geometry', genstru_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'general_structure_gate_opening_height', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_openh)
               ierr = nf90_put_att(ihisfile, id_genstru_openh, 'long_name', 'Gate opening height of general structure')
               ierr = nf90_put_att(ihisfile, id_genstru_openh, 'units', 'm')
               ierr = nf90_put_att(ihisfile, id_genstru_openh, 'coordinates', 'general_structure_id')
               ierr = nf90_put_att(ihisfile, id_genstru_openh, 'geometry', genstru_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'general_structure_gate_upper_edge_level', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_uppl)
               ierr = nf90_put_att(ihisfile, id_genstru_uppl, 'long_name', 'Gate upper edge level of general structure')
               ierr = nf90_put_att(ihisfile, id_genstru_uppl, 'units', 'm')
               ierr = nf90_put_att(ihisfile, id_genstru_uppl, 'coordinates', 'general_structure_id')
               ierr = nf90_put_att(ihisfile, id_genstru_uppl, 'geometry', genstru_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'general_structure_velocity_through_gate_opening', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_velgateopen)
               ierr = nf90_put_att(ihisfile, id_genstru_velgateopen, 'long_name', 'Velocity through gate opening of general structure')
               ierr = nf90_put_att(ihisfile, id_genstru_velgateopen, 'units', 'm s-1')
               ierr = nf90_put_att(ihisfile, id_genstru_velgateopen, 'coordinates', 'general_structure_id')
               ierr = nf90_put_att(ihisfile, id_genstru_velgateopen, 'geometry', genstru_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'general_structure_velocity_over_gate', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_velgateover)
               ierr = nf90_put_att(ihisfile, id_genstru_velgateover, 'long_name', 'Velocity over gate of general structure')
               ierr = nf90_put_att(ihisfile, id_genstru_velgateover, 'units', 'm s-1')
               ierr = nf90_put_att(ihisfile, id_genstru_velgateover, 'coordinates', 'general_structure_id')
               ierr = nf90_put_att(ihisfile, id_genstru_velgateover, 'geometry', genstru_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'general_structure_velocity_under_gate', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_velgateunder)
               ierr = nf90_put_att(ihisfile, id_genstru_velgateunder, 'long_name', 'Velocity under gate of general structure')
               ierr = nf90_put_att(ihisfile, id_genstru_velgateunder, 'units', 'm s-1')
               ierr = nf90_put_att(ihisfile, id_genstru_velgateunder, 'coordinates', 'general_structure_id')
               ierr = nf90_put_att(ihisfile, id_genstru_velgateunder, 'geometry', genstru_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'general_structure_flow_area_in_gate_opening', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_au_open)
               ierr = nf90_put_att(ihisfile, id_genstru_au_open, 'long_name', 'Flow area in gate opening of general structure')
               ierr = nf90_put_att(ihisfile, id_genstru_au_open, 'units', 'm2')
               ierr = nf90_put_att(ihisfile, id_genstru_au_open, 'coordinates', 'general_structure_id')
               ierr = nf90_put_att(ihisfile, id_genstru_au_open, 'geometry', genstru_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'general_structure_flow_area_over_gate', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_au_over)
               ierr = nf90_put_att(ihisfile, id_genstru_au_over, 'long_name', 'Flow area over gate of general structure')
               ierr = nf90_put_att(ihisfile, id_genstru_au_over, 'units', 'm2')
               ierr = nf90_put_att(ihisfile, id_genstru_au_over, 'coordinates', 'general_structure_id')
               ierr = nf90_put_att(ihisfile, id_genstru_au_over, 'geometry', genstru_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'general_structure_flow_area_under_gate', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_au_under)
               ierr = nf90_put_att(ihisfile, id_genstru_au_under, 'long_name', 'Flow area under gate of general structure')
               ierr = nf90_put_att(ihisfile, id_genstru_au_under, 'units', 'm2')
               ierr = nf90_put_att(ihisfile, id_genstru_au_under, 'coordinates', 'general_structure_id')
               ierr = nf90_put_att(ihisfile, id_genstru_au_under, 'geometry', genstru_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'general_structure_state', nf90_int, (/ id_genstrudim, id_timedim /), id_genstru_stat)
               ierr = nf90_put_att(ihisfile, id_genstru_stat, 'long_name', 'Flow state at general structure')
               ierr = nf90_put_att(ihisfile, id_genstru_stat, 'units', '-')
               ierr = nf90_put_att(ihisfile, id_genstru_stat, 'units', '-')
               ierr = nf90_put_att(ihisfile, id_genstru_stat, 'coordinates', 'general_structure_id')
               ierr = nf90_put_att(ihisfile, id_genstru_stat, 'flag_values', (/ 0, 1, 2, 3, 4 /))
               ierr = nf90_put_att(ihisfile, id_genstru_stat, 'flag_meanings', 'no_flow weir_free weir_submerged gate_free gate_submerged')
               ierr = nf90_put_att(ihisfile, id_genstru_stat, 'valid_range', (/ 0, 4 /))
               ierr = nf90_put_att(ihisfile, id_genstru_stat, '_FillValue', int(dmiss))
               ierr = nf90_put_att(ihisfile, id_genstru_stat, 'geometry', genstru_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'general_structure_s1_on_crest', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_s1crest)
               ierr = nf90_put_att(ihisfile, id_genstru_s1crest, 'long_name', 'Water level on crest of general structure')
               ierr = nf90_put_att(ihisfile, id_genstru_s1crest, 'units', 'm')
               ierr = nf90_put_att(ihisfile, id_genstru_s1crest, 'coordinates', 'general_structure_id')
               ierr = nf90_put_att(ihisfile, id_genstru_s1crest, 'geometry', genstru_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'general_structure_force_difference', nc_precision, (/ id_genstrudim, id_timedim /), id_genstru_forcedif)
               ierr = nf90_put_att(ihisfile, id_genstru_forcedif, 'long_name', 'Force difference per unit at general structure')
               ierr = nf90_put_att(ihisfile, id_genstru_forcedif, 'units', 'N m-1')
               ierr = nf90_put_att(ihisfile, id_genstru_forcedif, 'coordinates', 'general_structure_id')
               ierr = nf90_put_att(ihisfile, id_genstru_forcedif, 'geometry', genstru_geom_container_name)
            end if
        endif

        if(jahispump > 0 .and. npumpsg > 0) then
            ierr = nf90_def_dim(ihisfile, 'pumps', npumpsg, id_pumpdim)
            ierr = nf90_def_var(ihisfile, 'pump_id',  nf90_char,   (/ id_strlendim, id_pumpdim /), id_pump_id)
            ierr = nf90_put_att(ihisfile, id_pump_id,  'cf_role',   'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_pump_id,  'long_name', 'Id of pump'    )

            ! Define geometry related variables
            pump_geom_container_name = 'pump_geom'
            nNodeTot = 0
            if (network%sts%numPumps > 0) then ! newpump
               nNodeTot = nNodesPump
            else ! old pump
               do n = 1, npumpsg
                  nlinks = L2pumpsg(n) - L1pumpsg(n) + 1
                  if (nlinks > 0) then
                     nNodes = nlinks + 1
                  else if (nlinks == 0) then
                     nNodes = 0
                  end if
                  nNodeTot = nNodeTot + nNodes
               end do
            end if

            ierr = sgeom_def_geometry_variables(ihisfile, pump_geom_container_name, 'pump', 'line', nNodeTot, id_pumpdim, &
               id_pumpgeom_node_count, id_pumpgeom_node_coordx, id_pumpgeom_node_coordy)

            ierr = nf90_def_var(ihisfile, 'pump_xmid', nc_precision, (/ id_pumpdim /), id_pump_xmid)
            ierr = nf90_def_var(ihisfile, 'pump_ymid', nc_precision, (/ id_pumpdim /), id_pump_ymid)
            ierr = unc_addcoordatts(ihisfile, id_pump_xmid, id_pump_ymid, jsferic)
            ierr = nf90_put_att(ihisfile, id_pump_xmid, 'long_name', 'x-coordinate of representative mid point of pump location (snapped polyline)')
            ierr = nf90_put_att(ihisfile, id_pump_ymid, 'long_name', 'y-coordinate of representative mid point of pump location (snapped polyline)')

            ierr = nf90_def_var(ihisfile, 'pump_structure_discharge', nc_precision, (/ id_pumpdim, id_timedim /), id_pump_dis)
            ierr = nf90_put_att(ihisfile, id_pump_dis, 'long_name', 'Discharge through pump')
            ierr = nf90_put_att(ihisfile, id_pump_dis, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_pump_dis, 'coordinates', 'pump_id')
            ierr = nf90_put_att(ihisfile, id_pump_dis, 'geometry', pump_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'pump_capacity', nc_precision, (/ id_pumpdim, id_timedim /), id_pump_cap)
            ierr = nf90_put_att(ihisfile, id_pump_cap, 'long_name', 'Capacity of pump')
            ierr = nf90_put_att(ihisfile, id_pump_cap, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_pump_cap, 'coordinates', 'pump_id')
            ierr = nf90_put_att(ihisfile, id_pump_cap, 'geometry', pump_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'pump_discharge_dir', nc_precision, (/ id_pumpdim, id_timedim /), id_pump_disdir)
            ierr = nf90_put_att(ihisfile, id_pump_disdir, 'long_name', 'Discharge of pump w.r.t. pump orientation')
            ierr = nf90_put_att(ihisfile, id_pump_disdir, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_pump_disdir, 'coordinates', 'pump_id')
            ierr = nf90_put_att(ihisfile, id_pump_disdir, 'geometry', pump_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'pump_s1up', nc_precision, (/ id_pumpdim, id_timedim /), id_pump_s1up)    ! Nabi
            ierr = nf90_put_att(ihisfile, id_pump_s1up, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_pump_s1up, 'long_name', 'Water level upstream of pump')
            ierr = nf90_put_att(ihisfile, id_pump_s1up, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_pump_s1up, 'coordinates', 'pump_id')
            ierr = nf90_put_att(ihisfile, id_pump_s1up, 'geometry', pump_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'pump_s1dn', nc_precision, (/ id_pumpdim, id_timedim /), id_pump_s1dn)
            ierr = nf90_put_att(ihisfile, id_pump_s1dn, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_pump_s1dn, 'long_name', 'Water level downstream of pump')
            ierr = nf90_put_att(ihisfile, id_pump_s1dn, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_pump_s1dn, 'coordinates', 'pump_id')
            ierr = nf90_put_att(ihisfile, id_pump_s1dn, 'geometry', pump_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'pump_structure_head', nc_precision, (/ id_pumpdim, id_timedim /), id_pump_struhead)
            ierr = nf90_put_att(ihisfile, id_pump_struhead, 'long_name', 'Head difference across pump structure')
            ierr = nf90_put_att(ihisfile, id_pump_struhead, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_pump_struhead, 'coordinates', 'pump_id')
            ierr = nf90_put_att(ihisfile, id_pump_struhead, 'geometry', pump_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'pump_actual_stage',    nf90_int, (/ id_pumpdim, id_timedim /), id_pump_stage)
            ierr = nf90_put_att(ihisfile, id_pump_stage, 'long_name', 'Actual stage of pump')
            ierr = nf90_put_att(ihisfile, id_pump_stage, 'units', '-')
            ierr = nf90_put_att(ihisfile, id_pump_stage, 'coordinates', 'pump_id')
            ierr = nf90_put_att(ihisfile, id_pump_stage, '_FillValue', int(dmiss))
            ierr = nf90_put_att(ihisfile, id_pump_stage, 'geometry', pump_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'pump_head', nc_precision, (/ id_pumpdim, id_timedim /), id_pump_head)
            ierr = nf90_put_att(ihisfile, id_pump_head, 'long_name', 'Head difference in pumping direction')
            ierr = nf90_put_att(ihisfile, id_pump_head, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_pump_head, 'coordinates', 'pump_id')
            ierr = nf90_put_att(ihisfile, id_pump_head, 'geometry', pump_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'pump_reduction_factor', nc_precision, (/ id_pumpdim, id_timedim /), id_pump_redufact)
            ierr = nf90_put_att(ihisfile, id_pump_redufact, 'long_name', 'Reduction factor of pump')
            ierr = nf90_put_att(ihisfile, id_pump_redufact, 'units', '-')
            ierr = nf90_put_att(ihisfile, id_pump_redufact, 'coordinates', 'pump_id')
            ierr = nf90_put_att(ihisfile, id_pump_redufact, 'geometry', pump_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'pump_s1_delivery_side', nc_precision, (/ id_pumpdim, id_timedim /), id_pump_s1del)
            ierr = nf90_put_att(ihisfile, id_pump_s1del, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_pump_s1del, 'long_name', 'Water level at delivery side of pump')
            ierr = nf90_put_att(ihisfile, id_pump_s1del, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_pump_s1del, 'coordinates', 'pump_id')
            ierr = nf90_put_att(ihisfile, id_pump_s1del, 'geometry', pump_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'pump_s1_suction_side', nc_precision, (/ id_pumpdim, id_timedim /), id_pump_s1suc)
            ierr = nf90_put_att(ihisfile, id_pump_s1suc, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_pump_s1suc, 'long_name', 'Water level at suction side of pump')
            ierr = nf90_put_att(ihisfile, id_pump_s1suc, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_pump_s1suc, 'coordinates', 'pump_id')
            ierr = nf90_put_att(ihisfile, id_pump_s1suc, 'geometry', pump_geom_container_name)
        endif

        if(jahisgate > 0 .and. ngatesg > 0 ) then
            ierr = nf90_def_dim(ihisfile, 'gates', ngatesg, id_gatedim)
            ierr = nf90_def_var(ihisfile, 'gate_name',  nf90_char,   (/ id_strlendim, id_gatedim /), id_gatename)
            ierr = nf90_put_att(ihisfile, id_gatename,  'cf_role',   'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_gatename,  'long_name', 'gate name'    )

            ierr = nf90_def_var(ihisfile, 'gate_discharge', nc_precision, (/ id_gatedim, id_timedim /), id_gate_dis)
            !ierr = nf90_put_att(ihisfile, id_gate_dis, 'standard_name', 'gate_discharge')
            ierr = nf90_put_att(ihisfile, id_gate_dis, 'long_name', 'gate discharge')
            ierr = nf90_put_att(ihisfile, id_gate_dis, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_gate_dis, 'coordinates', 'gate_name')

            ierr = nf90_def_var(ihisfile, 'gate_lower_edge_level', nc_precision, (/ id_gatedim, id_timedim /), id_gate_edgel)
            ierr = nf90_put_att(ihisfile, id_gate_edgel, 'standard_name', 'gate_lower_edge_level')
            ierr = nf90_put_att(ihisfile, id_gate_edgel, 'long_name', 'gate lower edge level')
            ierr = nf90_put_att(ihisfile, id_gate_edgel, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_gate_edgel, 'coordinates', 'gate_name')

            ierr = nf90_def_var(ihisfile, 'gate_s1up', nc_precision, (/ id_gatedim, id_timedim /), id_gate_s1up)
            ierr = nf90_put_att(ihisfile, id_gate_s1up, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_gate_s1up, 'long_name', 'gate water level up')
            ierr = nf90_put_att(ihisfile, id_gate_s1up, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_gate_s1up, 'coordinates', 'gate_name')

            ierr = nf90_def_var(ihisfile, 'gate_s1dn', nc_precision, (/ id_gatedim, id_timedim /), id_gate_s1dn)
            ierr = nf90_put_att(ihisfile, id_gate_s1dn, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_gate_s1dn, 'long_name', 'gate water level down')
            ierr = nf90_put_att(ihisfile, id_gate_s1dn, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_gate_s1dn, 'coordinates', 'gate_name')
        endif

         if(jahisgate > 0 .and. ngategen > 0 ) then
            ierr = nf90_def_dim(ihisfile, 'gategens', ngategen, id_gategendim)
            ierr = nf90_def_var(ihisfile, 'gategen_name',  nf90_char,   (/ id_strlendim, id_gategendim /), id_gategenname)
            ierr = nf90_put_att(ihisfile, id_gategenname,  'cf_role',   'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_gategenname,  'long_name', 'gate name'    )

            ! Define geometry related variables
            gategen_geom_container_name = 'gategen_geom'
            nNodeTot = 0
            do n = 1, ngategen
               i = gate2cgen(n)
               nlinks = L2cgensg(i) - L1cgensg(i) + 1
               if (nlinks > 0) then
                  nNodes = nlinks + 1
               else if (nlinks == 0) then
                  nNodes = 0
               end if
               nNodeTot = nNodeTot + nNodes
            end do
            ierr = sgeom_def_geometry_variables(ihisfile, gategen_geom_container_name, 'gategen', 'line', nNodeTot, id_gategendim, &
               id_gategengeom_node_count, id_gategengeom_node_coordx, id_gategengeom_node_coordy)

            ierr = nf90_def_var(ihisfile, 'gategen_discharge', nc_precision, (/ id_gategendim, id_timedim /), id_gategen_dis)
            !ierr = nf90_put_att(ihisfile, id_gategen_dis, 'standard_name', 'integral_of_discharge_wrt_time') ! TODO: introduce time windows in nc
            ierr = nf90_put_att(ihisfile, id_gategen_dis, 'long_name', 'gate discharge (via general structure)')
            ierr = nf90_put_att(ihisfile, id_gategen_dis, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_gategen_dis, 'coordinates', 'gategen_name')
            ierr = nf90_put_att(ihisfile, id_gategen_dis, 'geometry', gategen_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'gategen_crest_level', nc_precision, (/ id_gategendim, id_timedim /), id_gategen_sillh)
            ierr = nf90_put_att(ihisfile, id_gategen_sillh, 'long_name', 'gate crest level (via general structure)')
            ierr = nf90_put_att(ihisfile, id_gategen_sillh, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_gategen_sillh, 'coordinates', 'gategen_name')
            ierr = nf90_put_att(ihisfile, id_gategen_sillh, 'geometry', gategen_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'gategen_crest_width', nc_precision, (/ id_gategendim, id_timedim /), id_gategen_sillw)
            ierr = nf90_put_att(ihisfile, id_gategen_sillw, 'long_name', 'gate crest width (via general structure)')
            ierr = nf90_put_att(ihisfile, id_gategen_sillw, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_gategen_sillw, 'coordinates', 'gategen_name')
            ierr = nf90_put_att(ihisfile, id_gategen_sillw, 'geometry', gategen_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'gategen_gate_lower_edge_level', nc_precision, (/ id_gategendim, id_timedim /), id_gategen_edgel)
            ierr = nf90_put_att(ihisfile, id_gategen_edgel, 'long_name', 'gate lower edge level (via general structure)')
            ierr = nf90_put_att(ihisfile, id_gategen_edgel, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_gategen_edgel, 'coordinates', 'gategen_name')
            ierr = nf90_put_att(ihisfile, id_gategen_edgel, 'geometry', gategen_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'gategen_flow_through_height', nc_precision, (/ id_gategendim, id_timedim /), id_gategen_flowh)
            ierr = nf90_put_att(ihisfile, id_gategen_flowh, 'standard_name', 'gategen_flow_through_height')
            ierr = nf90_put_att(ihisfile, id_gategen_flowh, 'long_name', 'gate flow through height (via general structure)')
            ierr = nf90_put_att(ihisfile, id_gategen_flowh, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_gategen_flowh, 'coordinates', 'gategen_name')
            ierr = nf90_put_att(ihisfile, id_gategen_flowh, 'geometry', gategen_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'gategen_gate_opening_width', nc_precision, (/ id_gategendim, id_timedim /), id_gategen_openw)
            ierr = nf90_put_att(ihisfile, id_gategen_openw, 'long_name', 'gate opening width (via general structure)')
            ierr = nf90_put_att(ihisfile, id_gategen_openw, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_gategen_openw, 'coordinates', 'gategen_name')
            ierr = nf90_put_att(ihisfile, id_gategen_openw, 'geometry', gategen_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'gategen_s1up', nc_precision, (/ id_gategendim, id_timedim /), id_gategen_s1up)
            ierr = nf90_put_att(ihisfile, id_gategen_s1up, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_gategen_s1up, 'long_name', 'gate water level up (via general structure)')
            ierr = nf90_put_att(ihisfile, id_gategen_s1up, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_gategen_s1up, 'coordinates', 'gategen_name')
            ierr = nf90_put_att(ihisfile, id_gategen_s1up, 'geometry', gategen_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'gategen_s1dn', nc_precision, (/ id_gategendim, id_timedim /), id_gategen_s1dn)
            ierr = nf90_put_att(ihisfile, id_gategen_s1dn, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_gategen_s1dn, 'long_name', 'gate water level down (via general structure)')
            ierr = nf90_put_att(ihisfile, id_gategen_s1dn, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_gategen_s1dn, 'coordinates', 'gategen_name')
            ierr = nf90_put_att(ihisfile, id_gategen_s1dn, 'geometry', gategen_geom_container_name)
        endif

        if(jahiscdam > 0 .and. ncdamsg > 0 ) then
            ierr = nf90_def_dim(ihisfile, 'dams', ncdamsg, id_cdamdim)
            !ierr = nf90_def_dim(ihisfile, 'cdam_name_len', strlen_netcdf, id_cdamlendim)
            ierr = nf90_def_var(ihisfile, 'cdam_name',  nf90_char,   (/ id_strlendim, id_cdamdim /), id_cdamname)
            ierr = nf90_put_att(ihisfile, id_cdamname,  'cf_role',   'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_cdamname,  'long_name', 'controllable dam name'    )

            ierr = nf90_def_var(ihisfile, 'cdam_discharge', nc_precision, (/ id_cdamdim, id_timedim /), id_cdam_dis)
            !ierr = nf90_put_att(ihisfile, id_cdam_dis, 'standard_name', 'cdam_discharge')
            ierr = nf90_put_att(ihisfile, id_cdam_dis, 'long_name', 'controllable dam discharge')
            ierr = nf90_put_att(ihisfile, id_cdam_dis, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_cdam_dis, 'coordinates', 'cdam_name')

            ierr = nf90_def_var(ihisfile, 'cdam_crest_level', nc_precision, (/ id_cdamdim, id_timedim /), id_cdam_crestl)
            !ierr = nf90_put_att(ihisfile, id_cdam_crestl, 'standard_name', 'cdam_crest_level')
            ierr = nf90_put_att(ihisfile, id_cdam_crestl, 'long_name', 'controllable dam crest level')
            ierr = nf90_put_att(ihisfile, id_cdam_crestl, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_cdam_crestl, 'coordinates', 'cdam_name')

            ierr = nf90_def_var(ihisfile, 'cdam_s1up', nc_precision, (/ id_cdamdim, id_timedim /), id_cdam_s1up)
            ierr = nf90_put_att(ihisfile, id_cdam_s1up, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_cdam_s1up, 'long_name', 'controllable dam water level up')
            ierr = nf90_put_att(ihisfile, id_cdam_s1up, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_cdam_s1up, 'coordinates', 'cdam_name')

            ierr = nf90_def_var(ihisfile, 'cdam_s1dn', nc_precision, (/ id_cdamdim, id_timedim /), id_cdam_s1dn)
            ierr = nf90_put_att(ihisfile, id_cdam_s1dn, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_cdam_s1dn, 'long_name', 'controllable dam water level down')
            ierr = nf90_put_att(ihisfile, id_cdam_s1dn, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_cdam_s1dn, 'coordinates', 'cdam_name')
        endif

        if(jahisweir > 0 .and. nweirgen > 0 ) then
           ierr = nf90_def_dim(ihisfile, 'weirgens', nweirgen, id_weirgendim)
           ierr = nf90_def_var(ihisfile, 'weirgen_id',  nf90_char,   (/ id_strlendim, id_weirgendim /), id_weirgen_id)
           ierr = nf90_put_att(ihisfile, id_weirgen_id,  'cf_role',   'timeseries_id')
           ierr = nf90_put_att(ihisfile, id_weirgen_id,  'long_name', 'Id of weir'    )

           ! Define geometry related variables
           weir_geom_container_name = 'weirgen_geom'
           nNodeTot = 0
           i = 1
           if (network%sts%numWeirs > 0) then ! new weir
            else ! old weir
               do n = 1, nweirgen
                  i = weir2cgen(n)
                  nlinks = L2cgensg(i) - L1cgensg(i) + 1
                  if (nlinks > 0) then
                     nNodes = nlinks + 1
                  else if (nlinks == 0) then
                     nNodes = 0
                  end if
                  nNodeTot = nNodeTot + nNodes
               end do
            end if


            ierr = sgeom_def_geometry_variables(ihisfile, weir_geom_container_name, 'weir', 'line', nNodesWeir, id_weirgendim, &
               id_weirgeom_node_count, id_weirgeom_node_coordx, id_weirgeom_node_coordy)

            ierr = nf90_def_var(ihisfile, 'weirgen_discharge', nc_precision, (/ id_weirgendim, id_timedim /), id_weirgen_dis)
            !ierr = nf90_put_att(ihisfile, id_weirgen_dis, 'standard_name', 'integral_of_discharge_wrt_time') ! TODO: introduce time windows in nc
            ierr = nf90_put_att(ihisfile, id_weirgen_dis, 'long_name', 'Discharge through weir')
            ierr = nf90_put_att(ihisfile, id_weirgen_dis, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_weirgen_dis, 'coordinates', 'weirgen_id')
            ierr = nf90_put_att(ihisfile, id_weirgen_dis, 'geometry', weir_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'weirgen_crest_level', nc_precision, (/ id_weirgendim, id_timedim /), id_weirgen_crestl)
            !ierr = nf90_put_att(ihisfile, id_weirgen_crestl, 'standard_name', 'weirgen_crest_level')
            ierr = nf90_put_att(ihisfile, id_weirgen_crestl, 'long_name', 'Crest level of weir')
            ierr = nf90_put_att(ihisfile, id_weirgen_crestl, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_weirgen_crestl, 'coordinates', 'weirgen_id')
            ierr = nf90_put_att(ihisfile, id_weirgen_crestl, 'geometry', weir_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'weirgen_crest_width', nc_precision, (/ id_weirgendim, id_timedim /), id_weirgen_crestw)
            !ierr = nf90_put_att(ihisfile, id_weirgen_crestw, 'standard_name', 'weirgen_crest_width')
            ierr = nf90_put_att(ihisfile, id_weirgen_crestw, 'long_name', 'Crest width of weir')
            ierr = nf90_put_att(ihisfile, id_weirgen_crestw, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_weirgen_crestw, 'coordinates', 'weirgen_id')
            ierr = nf90_put_att(ihisfile, id_weirgen_crestw, 'geometry', weir_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'weirgen_s1up', nc_precision, (/ id_weirgendim, id_timedim /), id_weirgen_s1up)
            ierr = nf90_put_att(ihisfile, id_weirgen_s1up, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_weirgen_s1up, 'long_name', 'Water level upstream of weir')
            ierr = nf90_put_att(ihisfile, id_weirgen_s1up, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_weirgen_s1up, 'coordinates', 'weirgen_id')
            ierr = nf90_put_att(ihisfile, id_weirgen_s1up, 'geometry', weir_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'weirgen_s1dn', nc_precision, (/ id_weirgendim, id_timedim /), id_weirgen_s1dn)
            ierr = nf90_put_att(ihisfile, id_weirgen_s1dn, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_weirgen_s1dn, 'long_name', 'Water level downstream of weir')
            ierr = nf90_put_att(ihisfile, id_weirgen_s1dn, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_weirgen_s1dn, 'coordinates', 'weirgen_id')
            ierr = nf90_put_att(ihisfile, id_weirgen_s1dn, 'geometry', weir_geom_container_name)

            if (network%sts%numWeirs > 0) then ! write extra files for new weirs
               ierr = nf90_def_var(ihisfile, 'weirgen_structure_head', nc_precision, (/ id_weirgendim, id_timedim /), id_weirgen_head)
               ierr = nf90_put_att(ihisfile, id_weirgen_head, 'long_name', 'Head difference across weir')
               ierr = nf90_put_att(ihisfile, id_weirgen_head, 'units', 'm')
               ierr = nf90_put_att(ihisfile, id_weirgen_head, 'coordinates', 'weirgen_id')
               ierr = nf90_put_att(ihisfile, id_weirgen_head, 'geometry', weir_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'weirgen_velocity', nc_precision, (/ id_weirgendim, id_timedim /), id_weirgen_vel)
               ierr = nf90_put_att(ihisfile, id_weirgen_vel, 'long_name', 'Velocity through weir')
               ierr = nf90_put_att(ihisfile, id_weirgen_vel, 'units', 'm s-1')
               ierr = nf90_put_att(ihisfile, id_weirgen_vel, 'coordinates', 'weirgen_id')
               ierr = nf90_put_att(ihisfile, id_weirgen_vel, 'geometry', weir_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'weirgen_flow_area', nc_precision, (/ id_weirgendim, id_timedim /), id_weirgen_au)
               ierr = nf90_put_att(ihisfile, id_weirgen_au, 'long_name', 'Flow area at weir')
               ierr = nf90_put_att(ihisfile, id_weirgen_au, 'units', 'm2')
               ierr = nf90_put_att(ihisfile, id_weirgen_au, 'coordinates', 'weirgen_id')
               ierr = nf90_put_att(ihisfile, id_weirgen_au, 'geometry', weir_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'weirgen_state', nf90_int, (/ id_weirgendim, id_timedim /), id_weir_stat)
               ierr = nf90_put_att(ihisfile, id_weir_stat, 'long_name', 'Flow state at weir')
               ierr = nf90_put_att(ihisfile, id_weir_stat, 'units', '-')
               ierr = nf90_put_att(ihisfile, id_weir_stat, 'coordinates', 'weirgen_id')
               ierr = nf90_put_att(ihisfile, id_weir_stat, 'flag_values', (/ 0, 1, 2 /))
               ierr = nf90_put_att(ihisfile, id_weir_stat, 'flag_meanings', 'no_flow weir_free weir_submerged')
               ierr = nf90_put_att(ihisfile, id_weir_stat, 'valid_range', (/ 0, 2 /))
               ierr = nf90_put_att(ihisfile, id_weir_stat, '_FillValue', int(dmiss))
               ierr = nf90_put_att(ihisfile, id_weir_stat, 'geometry', weir_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'weirgen_force_difference', nc_precision, (/ id_weirgendim, id_timedim /), id_weirgen_forcedif)
               ierr = nf90_put_att(ihisfile, id_weirgen_forcedif, 'long_name', 'Force difference per unit width at weir')
               ierr = nf90_put_att(ihisfile, id_weirgen_forcedif, 'units', 'N m-1')
               ierr = nf90_put_att(ihisfile, id_weirgen_forcedif, 'coordinates', 'weirgen_id')
               ierr = nf90_put_att(ihisfile, id_weirgen_forcedif, 'geometry', weir_geom_container_name)

               ierr = nf90_def_var(ihisfile, 'weirgen_s1_on_crest', nc_precision, (/ id_weirgendim, id_timedim /), id_weirgen_s1crest)
               ierr = nf90_put_att(ihisfile, id_weirgen_s1crest, 'long_name', 'Water level on crest of weir')
               ierr = nf90_put_att(ihisfile, id_weirgen_s1crest, 'units', 'm')
               ierr = nf90_put_att(ihisfile, id_weirgen_s1crest, 'coordinates', 'weirgen_id')
               ierr = nf90_put_att(ihisfile, id_weirgen_s1crest, 'geometry', weir_geom_container_name)
            end if
        endif

        ! Orifice
        if(jahisorif > 0 .and. network%sts%numOrifices > 0) then
            ierr = nf90_def_dim(ihisfile, 'orifice', network%sts%numOrifices, id_orifgendim)
            ierr = nf90_def_var(ihisfile, 'orifice_id',  nf90_char,   (/ id_strlendim, id_orifgendim /), id_orifgen_id)
            ierr = nf90_put_att(ihisfile, id_orifgen_id,  'cf_role',   'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_orifgen_id,  'long_name', 'Id of orifice')

            ! Define geometry related variables
            orif_geom_container_name = 'orifice_geom'
            nNodeTot = nNodesOrif

            ierr = sgeom_def_geometry_variables(ihisfile, orif_geom_container_name, 'orifice', 'line', nNodeTot, id_orifgendim, &
               id_orifgeom_node_count, id_orifgeom_node_coordx, id_orifgeom_node_coordy)

            ierr = nf90_def_var(ihisfile, 'orifice_discharge', nc_precision, (/ id_orifgendim, id_timedim /), id_orifgen_dis)
            ierr = nf90_put_att(ihisfile, id_orifgen_dis, 'long_name', 'Discharge through orifice')
            ierr = nf90_put_att(ihisfile, id_orifgen_dis, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_orifgen_dis, 'coordinates', 'orifice_id')
            ierr = nf90_put_att(ihisfile, id_orifgen_dis, 'geometry', orif_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'orifice_crest_level', nc_precision, (/ id_orifgendim, id_timedim /), id_orifgen_crestl)
            ierr = nf90_put_att(ihisfile, id_orifgen_crestl, 'long_name', 'Crest level of orifice')
            ierr = nf90_put_att(ihisfile, id_orifgen_crestl, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_orifgen_crestl, 'coordinates', 'orifice_id')
            ierr = nf90_put_att(ihisfile, id_orifgen_crestl, 'geometry', orif_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'orifice_crest_width', nc_precision, (/ id_orifgendim, id_timedim /), id_orifgen_crestw)
            ierr = nf90_put_att(ihisfile, id_orifgen_crestw, 'long_name', 'Crest width of orifice')
            ierr = nf90_put_att(ihisfile, id_orifgen_crestw, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_orifgen_crestw, 'coordinates', 'orifice_id')
            ierr = nf90_put_att(ihisfile, id_orifgen_crestw, 'geometry', orif_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'orifice_gate_lower_edge_level', nc_precision, (/ id_orifgendim, id_timedim /), id_orifgen_edgel)
            ierr = nf90_put_att(ihisfile, id_orifgen_edgel, 'long_name', 'Gate lower edge level of orifice')
            ierr = nf90_put_att(ihisfile, id_orifgen_edgel, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_orifgen_edgel, 'coordinates', 'orifice_id')
            ierr = nf90_put_att(ihisfile, id_orifgen_edgel, 'geometry', orif_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'orifice_s1up', nc_precision, (/ id_orifgendim, id_timedim /), id_orifgen_s1up)
            ierr = nf90_put_att(ihisfile, id_orifgen_s1up, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_orifgen_s1up, 'long_name', 'Water level upstream of orifice')
            ierr = nf90_put_att(ihisfile, id_orifgen_s1up, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_orifgen_s1up, 'coordinates', 'orifice_id')
            ierr = nf90_put_att(ihisfile, id_orifgen_s1up, 'geometry', orif_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'orifice_s1dn', nc_precision, (/ id_orifgendim, id_timedim /), id_orifgen_s1dn)
            ierr = nf90_put_att(ihisfile, id_orifgen_s1dn, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_orifgen_s1dn, 'long_name', 'Water level downstream of orifice')
            ierr = nf90_put_att(ihisfile, id_orifgen_s1dn, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_orifgen_s1dn, 'coordinates', 'orifice_id')
            ierr = nf90_put_att(ihisfile, id_orifgen_s1dn, 'geometry', orif_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'orifice_gate_opening_height', nc_precision, (/ id_orifgendim, id_timedim /), id_orifgen_openh)
            ierr = nf90_put_att(ihisfile, id_orifgen_openh, 'long_name', 'Gate opening height of orifice')
            ierr = nf90_put_att(ihisfile, id_orifgen_openh, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_orifgen_openh, 'coordinates', 'orifice_id')
            ierr = nf90_put_att(ihisfile, id_orifgen_openh, 'geometry', orif_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'orifice_head', nc_precision, (/ id_orifgendim, id_timedim /), id_orifgen_head)
            ierr = nf90_put_att(ihisfile, id_orifgen_head, 'long_name', 'Head difference across orifice')
            ierr = nf90_put_att(ihisfile, id_orifgen_head, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_orifgen_head, 'coordinates', 'orifice_id')
            ierr = nf90_put_att(ihisfile, id_orifgen_head, 'geometry', orif_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'orifice_flow_area', nc_precision, (/ id_orifgendim, id_timedim /), id_orifgen_au)
            ierr = nf90_put_att(ihisfile, id_orifgen_au, 'long_name', 'Flow area at orifice')
            ierr = nf90_put_att(ihisfile, id_orifgen_au, 'units', 'm2')
            ierr = nf90_put_att(ihisfile, id_orifgen_au, 'coordinates', 'orifice_id')
            ierr = nf90_put_att(ihisfile, id_orifgen_au, 'geometry', orif_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'orifice_state', nf90_int, (/ id_orifgendim, id_timedim /), id_orifgen_stat)
            ierr = nf90_put_att(ihisfile, id_orifgen_stat, 'long_name', 'Flow state at orifice')
            ierr = nf90_put_att(ihisfile, id_orifgen_stat, 'units', '-')
            ierr = nf90_put_att(ihisfile, id_orifgen_stat, 'units', '-')
            ierr = nf90_put_att(ihisfile, id_orifgen_stat, 'coordinates', 'orifice_id')
            ierr = nf90_put_att(ihisfile, id_orifgen_stat, 'flag_values', (/ 0, 1, 2, 3, 4 /))
            ierr = nf90_put_att(ihisfile, id_orifgen_stat, 'flag_meanings', 'no_flow weir_free weir_submerged gate_free gate_submerged')
            ierr = nf90_put_att(ihisfile, id_orifgen_stat, 'valid_range', (/ 0, 4 /))
            ierr = nf90_put_att(ihisfile, id_orifgen_stat, '_FillValue', int(dmiss))
            ierr = nf90_put_att(ihisfile, id_orifgen_stat, 'geometry', orif_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'orifice_s1_on_crest', nc_precision, (/ id_orifgendim, id_timedim /), id_orifgen_s1crest)
            ierr = nf90_put_att(ihisfile, id_orifgen_s1crest, 'long_name', 'Water level on crest of orifice')
            ierr = nf90_put_att(ihisfile, id_orifgen_s1crest, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_orifgen_s1crest, 'coordinates', 'orifice_id')
            ierr = nf90_put_att(ihisfile, id_orifgen_s1crest, 'geometry', orif_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'orifice_velocity', nc_precision, (/ id_orifgendim, id_timedim /), id_orifgen_vel)
            ierr = nf90_put_att(ihisfile, id_orifgen_vel, 'long_name', 'Velocity through orifice')
            ierr = nf90_put_att(ihisfile, id_orifgen_vel, 'units', 'm s-1')
            ierr = nf90_put_att(ihisfile, id_orifgen_vel, 'coordinates', 'orifice_id')
            ierr = nf90_put_att(ihisfile, id_orifgen_vel, 'geometry', orif_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'orifice_force_difference', nc_precision, (/ id_orifgendim, id_timedim /), id_orifgen_forcedif)
            ierr = nf90_put_att(ihisfile, id_orifgen_forcedif, 'long_name', 'Force difference per unit width at orifice')
            ierr = nf90_put_att(ihisfile, id_orifgen_forcedif, 'units', 'N m-1')
            ierr = nf90_put_att(ihisfile, id_orifgen_forcedif, 'coordinates', 'orifice_id')
            ierr = nf90_put_att(ihisfile, id_orifgen_forcedif, 'geometry', orif_geom_container_name)
        endif

        ! Bridge
        if(jahisbridge > 0 .and. network%sts%numBridges > 0) then
            ierr = nf90_def_dim(ihisfile, 'bridge', network%sts%numbridges, id_bridgedim)
            ierr = nf90_def_var(ihisfile, 'bridge_id',  nf90_char,   (/ id_strlendim, id_bridgedim /), id_bridge_id)
            ierr = nf90_put_att(ihisfile, id_bridge_id,  'cf_role',   'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_bridge_id,  'long_name', 'Id of bridge')

            ! Define geometry related variables
            bridge_geom_container_name = 'bridge_geom'
            nNodeTot = nNodesBridge

            ierr = sgeom_def_geometry_variables(ihisfile, bridge_geom_container_name, 'bridge', 'line', nNodeTot, id_bridgedim, &
               id_bridgegeom_node_count, id_bridgegeom_node_coordx, id_bridgegeom_node_coordy)

            ierr = nf90_def_var(ihisfile, 'bridge_discharge', nc_precision, (/ id_bridgedim, id_timedim /), id_bridge_dis)
            ierr = nf90_put_att(ihisfile, id_bridge_dis, 'long_name', 'Discharge through bridge')
            ierr = nf90_put_att(ihisfile, id_bridge_dis, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_bridge_dis, 'coordinates', 'bridge_id')
            ierr = nf90_put_att(ihisfile, id_bridge_dis, 'geometry', bridge_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'bridge_s1up', nc_precision, (/ id_bridgedim, id_timedim /), id_bridge_s1up)
            ierr = nf90_put_att(ihisfile, id_bridge_s1up, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_bridge_s1up, 'long_name', 'Water level upstream of bridge')
            ierr = nf90_put_att(ihisfile, id_bridge_s1up, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_bridge_s1up, 'coordinates', 'bridge_id')
            ierr = nf90_put_att(ihisfile, id_bridge_s1up, 'geometry', bridge_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'bridge_s1dn', nc_precision, (/ id_bridgedim, id_timedim /), id_bridge_s1dn)
            ierr = nf90_put_att(ihisfile, id_bridge_s1dn, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_bridge_s1dn, 'long_name', 'Water level downstream of bridge')
            ierr = nf90_put_att(ihisfile, id_bridge_s1dn, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_bridge_s1dn, 'coordinates', 'bridge_id')
            ierr = nf90_put_att(ihisfile, id_bridge_s1dn, 'geometry', bridge_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'bridge_head', nc_precision, (/ id_bridgedim, id_timedim /), id_bridge_head)
            ierr = nf90_put_att(ihisfile, id_bridge_head, 'long_name', 'Head difference across bridge')
            ierr = nf90_put_att(ihisfile, id_bridge_head, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_bridge_head, 'coordinates', 'bridge_id')
            ierr = nf90_put_att(ihisfile, id_bridge_head, 'geometry', bridge_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'bridge_flow_area', nc_precision, (/ id_bridgedim, id_timedim /), id_bridge_au)
            ierr = nf90_put_att(ihisfile, id_bridge_au, 'long_name', 'Flow area at bridge')
            ierr = nf90_put_att(ihisfile, id_bridge_au, 'units', 'm2')
            ierr = nf90_put_att(ihisfile, id_bridge_au, 'coordinates', 'bridge_id')
            ierr = nf90_put_att(ihisfile, id_bridge_au, 'geometry', bridge_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'bridge_velocity', nc_precision, (/ id_bridgedim, id_timedim /), id_bridge_vel)
            ierr = nf90_put_att(ihisfile, id_bridge_vel, 'long_name', 'Velocity through bridge')
            ierr = nf90_put_att(ihisfile, id_bridge_vel, 'units', 'm s-1')
            ierr = nf90_put_att(ihisfile, id_bridge_vel, 'coordinates', 'bridge_id')
            ierr = nf90_put_att(ihisfile, id_bridge_vel, 'geometry', bridge_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'bridge_blup',  nc_precision, (/ id_bridgedim, id_timedim /), id_bridge_blup)
            ierr = nf90_put_att(ihisfile, id_bridge_blup, 'standard_name', 'altitude')
            ierr = nf90_put_att(ihisfile, id_bridge_blup, 'long_name', 'Bed level at upstream of bridge')
            ierr = nf90_put_att(ihisfile, id_bridge_blup, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_bridge_blup, 'coordinates', 'bridge_id')
            ierr = nf90_put_att(ihisfile, id_bridge_blup, 'geometry', bridge_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'bridge_bldn',  nc_precision, (/ id_bridgedim, id_timedim /), id_bridge_bldn)
            ierr = nf90_put_att(ihisfile, id_bridge_bldn, 'standard_name', 'altitude')
            ierr = nf90_put_att(ihisfile, id_bridge_bldn, 'long_name', 'Bed level at downstream of bridge')
            ierr = nf90_put_att(ihisfile, id_bridge_bldn, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_bridge_bldn, 'coordinates', 'bridge_id')
            ierr = nf90_put_att(ihisfile, id_bridge_bldn, 'geometry', bridge_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'bridge_bl_actual', nc_precision, (/ id_bridgedim, id_timedim /), id_bridge_bl_act)
            ierr = nf90_put_att(ihisfile, id_bridge_bl_act, 'standard_name', 'altitude')
            ierr = nf90_put_att(ihisfile, id_bridge_bl_act, 'long_name', 'Actual bed level of bridge (crest)')
            ierr = nf90_put_att(ihisfile, id_bridge_bl_act, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_bridge_bl_act, 'coordinates', 'bridge_id')
            ierr = nf90_put_att(ihisfile, id_bridge_bl_act, 'geometry', bridge_geom_container_name)
        endif

        ! Culvert
        if(jahisculv > 0 .and. network%sts%numculverts > 0) then
            ierr = nf90_def_dim(ihisfile, 'culvert', network%sts%numculverts, id_culvertdim)
            ierr = nf90_def_var(ihisfile, 'culvert_id',  nf90_char,   (/ id_strlendim, id_culvertdim /), id_culvert_id)
            ierr = nf90_put_att(ihisfile, id_culvert_id,  'cf_role',   'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_culvert_id,  'long_name', 'Id of culvert'    )

            ! Define geometry related variables
            culvert_geom_container_name = 'culvert_geom'
            nNodeTot = nNodesCulv

            ierr = sgeom_def_geometry_variables(ihisfile, culvert_geom_container_name, 'culvert', 'line', nNodeTot, id_culvertdim, &
               id_culvertgeom_node_count, id_culvertgeom_node_coordx, id_culvertgeom_node_coordy)

            ierr = nf90_def_var(ihisfile, 'culvert_discharge', nc_precision, (/ id_culvertdim, id_timedim /), id_culvert_dis)
            ierr = nf90_put_att(ihisfile, id_culvert_dis, 'long_name', 'Discharge through culvert')
            ierr = nf90_put_att(ihisfile, id_culvert_dis, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_culvert_dis, 'coordinates', 'culvert_id')
            ierr = nf90_put_att(ihisfile, id_culvert_dis, 'geometry', culvert_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'culvert_crest_level', nc_precision, (/ id_culvertdim, id_timedim /), id_culvert_crestl)
            ierr = nf90_put_att(ihisfile, id_culvert_crestl, 'long_name', 'Crest level of culvert')
            ierr = nf90_put_att(ihisfile, id_culvert_crestl, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_culvert_crestl, 'coordinates', 'culvert_id')
            ierr = nf90_put_att(ihisfile, id_culvert_crestl, 'geometry', culvert_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'culvert_gate_lower_edge_level', nc_precision, (/ id_culvertdim, id_timedim /), id_culvert_edgel)
            ierr = nf90_put_att(ihisfile, id_culvert_edgel, 'long_name', 'Gate lower edge level of culvert')
            ierr = nf90_put_att(ihisfile, id_culvert_edgel, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_culvert_edgel, 'coordinates', 'culvert_id')
            ierr = nf90_put_att(ihisfile, id_culvert_edgel, 'geometry', culvert_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'culvert_s1up', nc_precision, (/ id_culvertdim, id_timedim /), id_culvert_s1up)
            ierr = nf90_put_att(ihisfile, id_culvert_s1up, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_culvert_s1up, 'long_name', 'Water level upstream of culvert')
            ierr = nf90_put_att(ihisfile, id_culvert_s1up, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_culvert_s1up, 'coordinates', 'culvert_id')
            ierr = nf90_put_att(ihisfile, id_culvert_s1up, 'geometry', culvert_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'culvert_s1dn', nc_precision, (/ id_culvertdim, id_timedim /), id_culvert_s1dn)
            ierr = nf90_put_att(ihisfile, id_culvert_s1dn, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_culvert_s1dn, 'long_name', 'Water level downstream of culvert')
            ierr = nf90_put_att(ihisfile, id_culvert_s1dn, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_culvert_s1dn, 'coordinates', 'culvert_id')
            ierr = nf90_put_att(ihisfile, id_culvert_s1dn, 'geometry', culvert_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'culvert_gate_opening_height', nc_precision, (/ id_culvertdim, id_timedim /), id_culvert_openh)
            ierr = nf90_put_att(ihisfile, id_culvert_openh, 'long_name', 'Gate opening height of culvert')
            ierr = nf90_put_att(ihisfile, id_culvert_openh, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_culvert_openh, 'coordinates', 'culvert_id')
            ierr = nf90_put_att(ihisfile, id_culvert_openh, 'geometry', culvert_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'culvert_head', nc_precision, (/ id_culvertdim, id_timedim /), id_culvert_head)
            ierr = nf90_put_att(ihisfile, id_culvert_head, 'long_name', 'Head difference across culvert')
            ierr = nf90_put_att(ihisfile, id_culvert_head, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_culvert_head, 'coordinates', 'culvert_id')
            ierr = nf90_put_att(ihisfile, id_culvert_head, 'geometry', culvert_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'culvert_flow_area', nc_precision, (/ id_culvertdim, id_timedim /), id_culvert_au)
            ierr = nf90_put_att(ihisfile, id_culvert_au, 'long_name', 'Flow area in culvert')
            ierr = nf90_put_att(ihisfile, id_culvert_au, 'units', 'm2')
            ierr = nf90_put_att(ihisfile, id_culvert_au, 'coordinates', 'culvert_id')
            ierr = nf90_put_att(ihisfile, id_culvert_au, 'geometry', culvert_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'culvert_state', nf90_int, (/ id_culvertdim, id_timedim /), id_culvert_stat)
            ierr = nf90_put_att(ihisfile, id_culvert_stat, 'long_name', 'Flow state in culvert')
            ierr = nf90_put_att(ihisfile, id_culvert_stat, 'units', '-')
            ierr = nf90_put_att(ihisfile, id_culvert_stat, 'coordinates', 'culvert_id')
            ierr = nf90_put_att(ihisfile, id_culvert_stat, 'flag_values', (/ 0, 1, 2/))
            ierr = nf90_put_att(ihisfile, id_culvert_stat, 'flag_meanings', 'no_flow culvert_free culvert_submerged')
            ierr = nf90_put_att(ihisfile, id_culvert_stat, 'valid_range', (/ 0, 2 /))
            ierr = nf90_put_att(ihisfile, id_culvert_stat, '_FillValue', int(dmiss))
            ierr = nf90_put_att(ihisfile, id_culvert_stat, 'geometry', culvert_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'culvert_velocity', nc_precision, (/ id_culvertdim, id_timedim /), id_culvert_vel)
            ierr = nf90_put_att(ihisfile, id_culvert_vel, 'long_name', 'Velocity in culvert')
            ierr = nf90_put_att(ihisfile, id_culvert_vel, 'units', 'm s-1')
            ierr = nf90_put_att(ihisfile, id_culvert_vel, 'coordinates', 'culvert_id')
            ierr = nf90_put_att(ihisfile, id_culvert_vel, 'geometry', culvert_geom_container_name)
        endif


        ! Dambreak
        if (jahisdambreak > 0 .and. ndambreaksg > 0 ) then
            ierr = nf90_def_dim(ihisfile, 'dambreaks', ndambreaksg, id_dambreakdim)
            ierr = nf90_def_var(ihisfile, 'dambreak_id',  nf90_char,   (/ id_strlendim, id_dambreakdim /), id_dambreak_id)
            ierr = nf90_put_att(ihisfile, id_dambreak_id,  'cf_role',   'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_dambreak_id,  'long_name', 'Id of dambreak')

            ierr = nf90_def_var(ihisfile, 'dambreak_s1up', nc_precision, (/ id_dambreakdim, id_timedim /), id_dambreak_s1up)
            ierr = nf90_put_att(ihisfile, id_dambreak_s1up, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_dambreak_s1up, 'long_name', 'Water level upstream of dambreak')
            ierr = nf90_put_att(ihisfile, id_dambreak_s1up, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_dambreak_s1up, 'coordinates', 'dambreak_id')
            ierr = write_real_fill_value(id_dambreak_s1up)

            ierr = nf90_def_var(ihisfile, 'dambreak_s1dn', nc_precision, (/ id_dambreakdim, id_timedim /), id_dambreak_s1dn)
            ierr = nf90_put_att(ihisfile, id_dambreak_s1dn, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_dambreak_s1dn, 'long_name', 'Water level downstream of dambreak')
            ierr = nf90_put_att(ihisfile, id_dambreak_s1dn, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_dambreak_s1dn, 'coordinates', 'dambreak_id')
            ierr = write_real_fill_value(id_dambreak_s1dn)

            ierr = nf90_def_var(ihisfile, 'dambreak_discharge', nc_precision, (/ id_dambreakdim, id_timedim /), id_dambreak_discharge)
            ierr = nf90_put_att(ihisfile, id_dambreak_discharge, 'long_name', 'Instantaneous discharge through dambreaks')
            ierr = nf90_put_att(ihisfile, id_dambreak_discharge, 'units', 'm3 s-1') !link_sum
            ierr = nf90_put_att(ihisfile, id_dambreak_discharge, 'coordinates', 'dambreak_id')
            ierr = write_real_fill_value(id_dambreak_discharge)

            ierr = nf90_def_var(ihisfile, 'dambreak_cumulative_discharge', nc_precision, (/ id_dambreakdim, id_timedim /), id_dambreak_cumulative_discharge)
            ierr = nf90_put_att(ihisfile, id_dambreak_cumulative_discharge, 'long_name', 'Cumulative discharge through dambreaks')
            ierr = nf90_put_att(ihisfile, id_dambreak_cumulative_discharge, 'units', 'm3') !link_sum
            ierr = nf90_put_att(ihisfile, id_dambreak_cumulative_discharge, 'coordinates', 'dambreak_id')
            ierr = write_real_fill_value(id_dambreak_cumulative_discharge)

            ierr = nf90_def_var(ihisfile, 'dambreak_breach_width_time_derivative', nc_precision, (/ id_dambreakdim, id_timedim /), id_dambreak_breach_width_time_derivative)
            ierr = nf90_put_att(ihisfile, id_dambreak_breach_width_time_derivative, 'long_name', 'Breach width time derivative of dambreak')
            ierr = nf90_put_att(ihisfile, id_dambreak_breach_width_time_derivative, 'units', 'm s-1')
            ierr = nf90_put_att(ihisfile, id_dambreak_breach_width_time_derivative, 'coordinates', 'dambreak_id')
            ierr = write_real_fill_value(id_dambreak_breach_width_time_derivative)

            ierr = nf90_def_var(ihisfile, 'dambreak_water_level_jump', nc_precision, (/ id_dambreakdim, id_timedim /), id_dambreak_water_level_jump)
            ierr = nf90_put_att(ihisfile, id_dambreak_water_level_jump, 'long_name', 'Breach water level jump of dambreak')
            ierr = nf90_put_att(ihisfile, id_dambreak_water_level_jump, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_dambreak_water_level_jump, 'coordinates', 'dambreak_id')
            ierr = write_real_fill_value(id_dambreak_water_level_jump)

            ierr = nf90_def_var(ihisfile, 'dambreak_normal_velocity', nc_precision, (/ id_dambreakdim, id_timedim /), id_dambreak_normal_velocity)
            ierr = nf90_put_att(ihisfile, id_dambreak_normal_velocity, 'long_name', 'Normal velocity through dambreak')
            ierr = nf90_put_att(ihisfile, id_dambreak_normal_velocity, 'units', 'm s-1')
            ierr = nf90_put_att(ihisfile, id_dambreak_normal_velocity, 'coordinates', 'dambreak_id')
            ierr = write_real_fill_value(id_dambreak_normal_velocity)

            ierr = nf90_def_var(ihisfile, 'dambreak_structure_head', nc_precision, (/ id_dambreakdim, id_timedim /), id_dambreak_head)
            ierr = nf90_put_att(ihisfile, id_dambreak_head, 'long_name', 'Head difference across dambreak')
            ierr = nf90_put_att(ihisfile, id_dambreak_head, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_dambreak_head, 'coordinates', 'dambreak_id')
            ierr = write_real_fill_value(id_dambreak_head)

            ierr = nf90_def_var(ihisfile, 'dambreak_flow_area', nc_precision, (/ id_dambreakdim, id_timedim /), id_dambreak_au)
            ierr = nf90_put_att(ihisfile, id_dambreak_au, 'long_name', 'Flow area at dambreak')
            ierr = nf90_put_att(ihisfile, id_dambreak_au, 'units', 'm2')
            ierr = nf90_put_att(ihisfile, id_dambreak_au, 'coordinates', 'dambreak_id')
            ierr = write_real_fill_value(id_dambreak_au)

            ierr = nf90_def_var(ihisfile, 'dambreak_crest_level', nc_precision, (/ id_dambreakdim, id_timedim /), id_dambreak_cresth)
            ierr = nf90_put_att(ihisfile, id_dambreak_cresth, 'long_name', 'Crest level of dambreak')
            ierr = nf90_put_att(ihisfile, id_dambreak_cresth, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_dambreak_cresth, 'coordinates', 'dambreak_id')

            ierr = nf90_def_var(ihisfile, 'dambreak_crest_width', nc_precision, (/ id_dambreakdim, id_timedim /), id_dambreak_crestw)
            ierr = nf90_put_att(ihisfile, id_dambreak_crestw, 'long_name', 'Crest width of dambreak')
            ierr = nf90_put_att(ihisfile, id_dambreak_crestw, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_dambreak_crestw, 'coordinates', 'dambreak_id')
        endif

        ! Universal weir
        if(jahisuniweir > 0 .and. network%sts%numuniweirs > 0) then
            ierr = nf90_def_dim(ihisfile, 'universalWeirs', network%sts%numuniweirs, id_uniweirdim)
            ierr = nf90_def_var(ihisfile, 'uniweir_id',  nf90_char,   (/ id_strlendim, id_uniweirdim /), id_uniweir_id)
            ierr = nf90_put_att(ihisfile, id_uniweir_id,  'cf_role',   'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_uniweir_id,  'long_name', 'Id of universal weir'    )

            ! Define geometry related variables
            uniweir_geom_container_name = 'uniweir_geom'
            nNodeTot = nNodesUniweir

            ierr = sgeom_def_geometry_variables(ihisfile, uniweir_geom_container_name, 'uniweir', 'line', nNodeTot, id_uniweirdim, &
               id_uniweirgeom_node_count, id_uniweirgeom_node_coordx, id_uniweirgeom_node_coordy)

            ierr = nf90_def_var(ihisfile, 'uniweir_discharge', nc_precision, (/ id_uniweirdim, id_timedim /), id_uniweir_dis)
            ierr = nf90_put_att(ihisfile, id_uniweir_dis, 'long_name', 'Discharge through universal weir')
            ierr = nf90_put_att(ihisfile, id_uniweir_dis, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_uniweir_dis, 'coordinates', 'uniweir_id')
            ierr = nf90_put_att(ihisfile, id_uniweir_dis, 'geometry', uniweir_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'uniweir_crest_level', nc_precision, (/ id_uniweirdim, id_timedim /), id_uniweir_crestl)
            ierr = nf90_put_att(ihisfile, id_uniweir_crestl, 'long_name', 'Crest level of universal weir')
            ierr = nf90_put_att(ihisfile, id_uniweir_crestl, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_uniweir_crestl, 'coordinates', 'uniweir_id')
            ierr = nf90_put_att(ihisfile, id_uniweir_crestl, 'geometry', uniweir_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'uniweir_s1up', nc_precision, (/ id_uniweirdim, id_timedim /), id_uniweir_s1up)
            ierr = nf90_put_att(ihisfile, id_uniweir_s1up, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_uniweir_s1up, 'long_name', 'Water level upstream of universal weir')
            ierr = nf90_put_att(ihisfile, id_uniweir_s1up, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_uniweir_s1up, 'coordinates', 'uniweir_id')
            ierr = nf90_put_att(ihisfile, id_uniweir_s1up, 'geometry', uniweir_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'uniweir_s1dn', nc_precision, (/ id_uniweirdim, id_timedim /), id_uniweir_s1dn)
            ierr = nf90_put_att(ihisfile, id_uniweir_s1dn, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_uniweir_s1dn, 'long_name', 'Water level downstream of universal weir')
            ierr = nf90_put_att(ihisfile, id_uniweir_s1dn, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_uniweir_s1dn, 'coordinates', 'uniweir_id')
            ierr = nf90_put_att(ihisfile, id_uniweir_s1dn, 'geometry', uniweir_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'uniweir_head', nc_precision, (/ id_uniweirdim, id_timedim /), id_uniweir_head)
            ierr = nf90_put_att(ihisfile, id_uniweir_head, 'long_name', 'Head difference across universal weir')
            ierr = nf90_put_att(ihisfile, id_uniweir_head, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_uniweir_head, 'coordinates', 'uniweir_id')
            ierr = nf90_put_att(ihisfile, id_uniweir_head, 'geometry', uniweir_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'uniweir_flow_area', nc_precision, (/ id_uniweirdim, id_timedim /), id_uniweir_au)
            ierr = nf90_put_att(ihisfile, id_uniweir_au, 'long_name', 'Flow area in universal weir')
            ierr = nf90_put_att(ihisfile, id_uniweir_au, 'units', 'm2')
            ierr = nf90_put_att(ihisfile, id_uniweir_au, 'coordinates', 'uniweir_id')
            ierr = nf90_put_att(ihisfile, id_uniweir_au, 'geometry', uniweir_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'uniweir_velocity', nc_precision, (/ id_uniweirdim, id_timedim /), id_uniweir_vel)
            ierr = nf90_put_att(ihisfile, id_uniweir_vel, 'long_name', 'Velocity through universal weir')
            ierr = nf90_put_att(ihisfile, id_uniweir_vel, 'units', 'm s-1')
            ierr = nf90_put_att(ihisfile, id_uniweir_vel, 'coordinates', 'uniweir_id')
            ierr = nf90_put_att(ihisfile, id_uniweir_vel, 'geometry', uniweir_geom_container_name)
        endif

        ! compound structure
        if(jahiscmpstru > 0 .and. network%cmps%count > 0) then
            ierr = nf90_def_dim(ihisfile, 'compoundStructures', network%cmps%count, id_cmpstrudim)
            ierr = nf90_def_var(ihisfile, 'cmpstru_id',  nf90_char,   (/ id_strlendim, id_cmpstrudim /), id_cmpstru_id)
            ierr = nf90_put_att(ihisfile, id_cmpstru_id,  'cf_role',   'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_cmpstru_id,  'long_name', 'Id of compound structure')

            ierr = nf90_def_var(ihisfile, 'cmpstru_discharge', nc_precision, (/ id_cmpstrudim, id_timedim /), id_cmpstru_dis)
            ierr = nf90_put_att(ihisfile, id_cmpstru_dis, 'long_name', 'Discharge through compound structure')
            ierr = nf90_put_att(ihisfile, id_cmpstru_dis, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_cmpstru_dis, 'coordinates', 'cmpstru_id')

            ierr = nf90_def_var(ihisfile, 'cmpstru_s1up', nc_precision, (/ id_cmpstrudim, id_timedim /), id_cmpstru_s1up)
            ierr = nf90_put_att(ihisfile, id_cmpstru_s1up, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_cmpstru_s1up, 'long_name', 'Water level upstream of compound structure')
            ierr = nf90_put_att(ihisfile, id_cmpstru_s1up, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_cmpstru_s1up, 'coordinates', 'cmpstru_id')

            ierr = nf90_def_var(ihisfile, 'cmpstru_s1dn', nc_precision, (/ id_cmpstrudim, id_timedim /), id_cmpstru_s1dn)
            ierr = nf90_put_att(ihisfile, id_cmpstru_s1dn, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_cmpstru_s1dn, 'long_name', 'Water level downstream of compound structure')
            ierr = nf90_put_att(ihisfile, id_cmpstru_s1dn, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_cmpstru_s1dn, 'coordinates', 'cmpstru_id')

            ierr = nf90_def_var(ihisfile, 'cmpstru_head', nc_precision, (/ id_cmpstrudim, id_timedim /), id_cmpstru_head)
            ierr = nf90_put_att(ihisfile, id_cmpstru_head, 'long_name', 'Head difference across compound structure')
            ierr = nf90_put_att(ihisfile, id_cmpstru_head, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_cmpstru_head, 'coordinates', 'cmpstru_id')

            ierr = nf90_def_var(ihisfile, 'cmpstru_flow_area', nc_precision, (/ id_cmpstrudim, id_timedim /), id_cmpstru_au)
            ierr = nf90_put_att(ihisfile, id_cmpstru_au, 'long_name', 'Flow area in compound structure')
            ierr = nf90_put_att(ihisfile, id_cmpstru_au, 'units', 'm2')
            ierr = nf90_put_att(ihisfile, id_cmpstru_au, 'coordinates', 'cmpstru_id')

            ierr = nf90_def_var(ihisfile, 'cmpstru_velocity', nc_precision, (/ id_cmpstrudim, id_timedim /), id_cmpstru_vel)
            ierr = nf90_put_att(ihisfile, id_cmpstru_vel, 'long_name', 'Velocity through compound structure')
            ierr = nf90_put_att(ihisfile, id_cmpstru_vel, 'units', 'm s-1')
            ierr = nf90_put_att(ihisfile, id_cmpstru_vel, 'coordinates', 'cmpstru_id')
        endif

        ! Long culvert
        if(jahislongculv > 0 .and. nlongculverts > 0) then
            ierr = nf90_def_dim(ihisfile, 'longculvert', nlongculverts, id_longculvertdim)
            ierr = nf90_def_var(ihisfile, 'longculvert_id',  nf90_char,   (/ id_strlendim, id_longculvertdim /), id_longculvert_id)
            ierr = nf90_put_att(ihisfile, id_longculvert_id,  'cf_role',   'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_longculvert_id,  'long_name', 'Id of long culvert')

            ! Define geometry related variables
            longculvert_geom_container_name = 'longculvert_geom'
            nNodeTot = 0
            nNodeTot = nNodesLongCulv

            ierr = sgeom_def_geometry_variables(ihisfile, longculvert_geom_container_name, 'longculvert', 'line', nNodeTot, id_longculvertdim, &
               id_longculvertgeom_node_count, id_longculvertgeom_node_coordx, id_longculvertgeom_node_coordy)
            !ierr = ncu_clone_vardef(ihisfile, ihisfile, id_longculvertgeom_node_coordx, 'longculvert_geom_lon', id_laydim, & !id_statlon, &
            !              'longitude', 'blabla1', 'degrees_east')

            ierr = nf90_def_var(ihisfile, 'longculvert_discharge', nc_precision, (/ id_longculvertdim, id_timedim /), id_longculvert_dis)
            ierr = nf90_put_att(ihisfile, id_longculvert_dis, 'long_name', 'Discharge through long culvert')
            ierr = nf90_put_att(ihisfile, id_longculvert_dis, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_longculvert_dis, 'coordinates', 'longculvert_id')
            ierr = nf90_put_att(ihisfile, id_longculvert_dis, 'geometry', longculvert_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'longculvert_s1up', nc_precision, (/ id_longculvertdim, id_timedim /), id_longculvert_s1up)
            ierr = nf90_put_att(ihisfile, id_longculvert_s1up, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_longculvert_s1up, 'long_name', 'Water level upstream of long culvert')
            ierr = nf90_put_att(ihisfile, id_longculvert_s1up, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_longculvert_s1up, 'coordinates', 'longculvert_id')
            ierr = nf90_put_att(ihisfile, id_longculvert_s1up, 'geometry', longculvert_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'longculvert_s1dn', nc_precision, (/ id_longculvertdim, id_timedim /), id_longculvert_s1dn)
            ierr = nf90_put_att(ihisfile, id_longculvert_s1dn, 'standard_name', 'sea_surface_height')
            ierr = nf90_put_att(ihisfile, id_longculvert_s1dn, 'long_name', 'Water level downstream of long culvert')
            ierr = nf90_put_att(ihisfile, id_longculvert_s1dn, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_longculvert_s1dn, 'coordinates', 'longculvert_id')
            ierr = nf90_put_att(ihisfile, id_longculvert_s1dn, 'geometry', longculvert_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'longculvert_head', nc_precision, (/ id_longculvertdim, id_timedim /), id_longculvert_head)
            ierr = nf90_put_att(ihisfile, id_longculvert_head, 'long_name', 'Head difference across long culvert')
            ierr = nf90_put_att(ihisfile, id_longculvert_head, 'units', 'm')
            ierr = nf90_put_att(ihisfile, id_longculvert_head, 'coordinates', 'longculvert_id')
            ierr = nf90_put_att(ihisfile, id_longculvert_head, 'geometry', longculvert_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'longculvert_flow_area', nc_precision, (/ id_longculvertdim, id_timedim /), id_longculvert_au)
            ierr = nf90_put_att(ihisfile, id_longculvert_au, 'long_name', 'Flow area in long culvert')
            ierr = nf90_put_att(ihisfile, id_longculvert_au, 'units', 'm2')
            ierr = nf90_put_att(ihisfile, id_longculvert_au, 'coordinates', 'longculvert_id')
            ierr = nf90_put_att(ihisfile, id_longculvert_au, 'geometry', longculvert_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'longculvert_velocity', nc_precision, (/ id_longculvertdim, id_timedim /), id_longculvert_vel)
            ierr = nf90_put_att(ihisfile, id_longculvert_vel, 'long_name', 'Velocity in long culvert')
            ierr = nf90_put_att(ihisfile, id_longculvert_vel, 'units', 'm s-1')
            ierr = nf90_put_att(ihisfile, id_longculvert_vel, 'coordinates', 'longculvert_id')
            ierr = nf90_put_att(ihisfile, id_longculvert_vel, 'geometry', longculvert_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'longculvert_valve_relative_opening', nc_precision, (/ id_longculvertdim, id_timedim /), id_longculvert_valveopen)
            ierr = nf90_put_att(ihisfile, id_longculvert_valveopen, 'long_name', 'Valve relative opening in long culvert')
            ierr = nf90_put_att(ihisfile, id_longculvert_valveopen, 'units', '1')
            ierr = nf90_put_att(ihisfile, id_longculvert_valveopen, 'coordinates', 'longculvert_id')
            ierr = nf90_put_att(ihisfile, id_longculvert_valveopen, 'geometry', longculvert_geom_container_name)
        endif

        ! Lateral
        if(jahislateral > 0 .and. numlatsg > 0) then
            ierr = nf90_def_dim(ihisfile, 'lateral', numlatsg, id_latdim)
            ierr = nf90_def_var(ihisfile, 'lateral_id',  nf90_char,   (/ id_strlendim, id_latdim /), id_lat_id)
            ierr = nf90_put_att(ihisfile, id_lat_id,  'cf_role',   'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_lat_id,  'long_name', 'Id of lateral')

            ! Define geometry related variables
            lat_geom_container_name = 'lateral_geom'
            nNodeTot = nNodesLat
            ierr = sgeom_def_geometry_variables(ihisfile, lat_geom_container_name, 'lateral', 'point', nNodeTot, id_latdim, &
               id_latgeom_node_count, id_latgeom_node_coordx, id_latgeom_node_coordy)

            ierr = nf90_def_var(ihisfile, 'lateral_prescribed_discharge_instantaneous', nc_precision, (/ id_latdim, id_timedim /), id_lat_predis_inst)
            ierr = nf90_put_att(ihisfile, id_lat_predis_inst, 'long_name', 'Prescribed discharge through lateral at current time step (instantaneous)')
            ierr = nf90_put_att(ihisfile, id_lat_predis_inst, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_lat_predis_inst, 'coordinates', 'lateral_id')
            ierr = nf90_put_att(ihisfile, id_lat_predis_inst, 'geometry', lat_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'lateral_prescribed_discharge_average', nc_precision, (/ id_latdim, id_timedim /), id_lat_predis_ave)
            ierr = nf90_put_att(ihisfile, id_lat_predis_ave, 'long_name', 'Prescribed discharge through lateral, average over the last history time interval')
            ierr = nf90_put_att(ihisfile, id_lat_predis_ave, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_lat_predis_ave, 'coordinates', 'lateral_id')
            ierr = nf90_put_att(ihisfile, id_lat_predis_ave, 'geometry', lat_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'lateral_realized_discharge_instantaneous', nc_precision, (/ id_latdim, id_timedim /), id_lat_realdis_inst)
            ierr = nf90_put_att(ihisfile, id_lat_realdis_inst, 'long_name', 'Realized discharge through lateral at current time step (instantaneous)')
            ierr = nf90_put_att(ihisfile, id_lat_realdis_inst, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_lat_realdis_inst, 'coordinates', 'lateral_id')
            ierr = nf90_put_att(ihisfile, id_lat_realdis_inst, 'geometry', lat_geom_container_name)

            ierr = nf90_def_var(ihisfile, 'lateral_realized_discharge_average', nc_precision, (/ id_latdim, id_timedim /), id_lat_realdis_ave)
            ierr = nf90_put_att(ihisfile, id_lat_realdis_ave, 'long_name', 'Realized discharge through lateral, average over the last history time interval')
            ierr = nf90_put_att(ihisfile, id_lat_realdis_ave, 'units', 'm3 s-1')
            ierr = nf90_put_att(ihisfile, id_lat_realdis_ave, 'coordinates', 'lateral_id')
            ierr = nf90_put_att(ihisfile, id_lat_realdis_ave, 'geometry', lat_geom_container_name)
        endif
        if (timon) call timstop (handle_extra(60))

        if(dad_included) then  ! Output for dredging and dumping
            ierr = nf90_def_dim(ihisfile, 'ndredlink', dadpar%nalink, id_dredlinkdim)
            ierr = nf90_def_dim(ihisfile, 'ndred', dadpar%nadred+dadpar%nasupl, id_dreddim)
            ierr = nf90_def_dim(ihisfile, 'ndump', dadpar%nadump, id_dumpdim)

            ierr = nf90_def_var(ihisfile, 'dredge_area_name',         nf90_char,   (/ id_strlendim, id_dreddim /), id_dred_name)
            ierr = nf90_put_att(ihisfile, id_dred_name,  'long_name'    , 'dredge area identifier')

            ierr = nf90_def_var(ihisfile, 'dump_area_name',         nf90_char,   (/ id_strlendim, id_dumpdim /), id_dump_name)
            ierr = nf90_put_att(ihisfile, id_dump_name,  'long_name'    , 'dump area identifier')

            ierr = nf90_def_var(ihisfile, 'dred_link_discharge', nc_precision, (/ id_dredlinkdim, id_sedtotdim, id_timedim /), id_dredlink_dis)
            ierr = nf90_put_att(ihisfile, id_dredlink_dis, 'long_name', 'Cumulative dredged material transported via links per fraction')
            ierr = nf90_put_att(ihisfile, id_dredlink_dis, 'units', 'm3') !link_sum

            ierr = nf90_def_var(ihisfile, 'dred_discharge', nc_precision, (/ id_dreddim, id_timedim /), id_dred_dis)
            ierr = nf90_put_att(ihisfile, id_dred_dis, 'long_name', 'Cumulative dredged material for dredge areas')
            ierr = nf90_put_att(ihisfile, id_dred_dis, 'units', 'm3') !totvoldred

            ierr = nf90_def_var(ihisfile, 'dump_discharge', nc_precision, (/ id_dumpdim, id_timedim /), id_dump_dis)
            ierr = nf90_put_att(ihisfile, id_dump_dis, 'long_name', 'Cumulative dredged material for dump areas')
            ierr = nf90_put_att(ihisfile, id_dump_dis, 'units', 'm3') !totvoldump

            ierr = nf90_def_var(ihisfile, 'dred_time_frac', nc_precision, (/ id_dreddim, id_timedim /), id_dred_tfrac)
            ierr = nf90_put_att(ihisfile, id_dred_tfrac, 'long_name', 'Time fraction spent dredging')
            ierr = nf90_put_att(ihisfile, id_dred_tfrac, 'units', '-') !ndredged

            ierr = nf90_def_var(ihisfile, 'plough_time_frac', nc_precision, (/ id_dreddim, id_timedim /), id_plough_tfrac)
            ierr = nf90_put_att(ihisfile, id_plough_tfrac, 'long_name', 'Time fraction spent ploughing')
            ierr = nf90_put_att(ihisfile, id_plough_tfrac, 'units', '-') !nploughed
        endif

        if ( jacheckmonitor.eq.1 ) then
           ierr = nf90_def_var(ihisfile, 'checkerboard_monitor', nc_precision, (/ id_laydim, id_timedim /), id_checkmon)
           ierr = nf90_put_att(ihisfile, id_checkmon, 'long_name', 'Checkerboard mode monitor')
           ierr = nf90_put_att(ihisfile, id_checkmon, 'unit', 'm s-1')

           ierr = nf90_def_var(ihisfile, 'num_timesteps', nf90_int, id_timedim, id_num_timesteps)
           ierr = nf90_def_var(ihisfile, 'comp_time', nc_precision, id_timedim, id_comp_time)
        end if

        ierr = nf90_def_var(ihisfile, 'time', nf90_double, id_timedim, id_time)
        ierr = nf90_put_att(ihisfile, id_time,  'units'        , trim(Tudunitstr))
        ierr = nf90_put_att(ihisfile, id_time,  'standard_name', 'time')

        ! Size of latest timestep
        ierr = unc_def_var_nonspatial(ihisfile, id_timestep, nc_precision, (/ id_timedim /), 'timestep', '',     'latest computational timestep size in each output interval', 's')
        
        ierr = nf90_enddef(ihisfile)
        if (timon) call timstop (handle_extra(61))

        if (timon) call timstrt ('unc_write_his timeindep data', handle_extra(63))
        do i=1,numobs+nummovobs
!           ierr = nf90_put_var(ihisfile, id_statx,    xobs(i),         (/ i /))
!           ierr = nf90_put_var(ihisfile, id_staty,    yobs(i),         (/ i /))
           ierr = nf90_put_var(ihisfile, id_statid, trimexact(namobs(i), strlen_netcdf), (/ 1, i /)) ! Extra for OpenDA-wrapper
           ierr = nf90_put_var(ihisfile, id_statname, trimexact(namobs(i), strlen_netcdf), (/ 1, i /))
        end do

        if (ncrs > 0) then
            do i=1,ncrs
                !ierr = nf90_put_var(ihisfile, id_crsx,     crs(i)%path%xp(1:crs(i)%path%np), (/ 1, i /))
                !ierr = nf90_put_var(ihisfile, id_crsy,     crs(i)%path%yp(1:crs(i)%path%np), (/ 1, i /))
                ierr = nf90_put_var(ihisfile, id_crsname,  trimexact(crs(i)%name, strlen_netcdf),      (/ 1, i /))
            end do
            if (it_his == 0) then
               ierr = nf90_put_var(ihisfile, id_crsgeom_node_coordx, geomXCrs,     start = (/ 1 /), count = (/ nNodesCrs /))
               ierr = nf90_put_var(ihisfile, id_crsgeom_node_coordy, geomYCrs,     start = (/ 1 /), count = (/ nNodesCrs /))
               ierr = nf90_put_var(ihisfile, id_crsgeom_node_count,  nodeCountCrs)
               if (allocated(geomXCrs))     deallocate(geomXCrs)
               if (allocated(geomYCrs))     deallocate(geomYCrs)
               if (allocated(nodeCountCrs)) deallocate(nodeCountCrs)
               end if
                  end if

        if (nrug>0) then
            do i=1,nrug
                ierr = nf90_put_var(ihisfile, id_rugname,  trimexact(rug(i)%name, strlen_netcdf), (/ 1, i /))
                ierr = nf90_put_var(ihisfile, id_rugid,    trimexact(rug(i)%name, strlen_netcdf), (/ 1, i /))
            end do
        endif

        if (jahiscgen > 0 .and. ntmp > 0) then
            do i=1,ntmp
               if (jaoldstr == 1) then
                  igen = i
               else
                  if (network%sts%numGeneralStructures > 0) then
                     istru = network%sts%generalStructureIndices(i)
                     ierr = nf90_put_var(ihisfile, id_genstru_id,  trimexact(network%sts%struct(istru)%id, strlen_netcdf),  (/ 1, i /))
                     cycle
                  else
                     igen = genstru2cgen(i)
                  end if
               end if

               ierr = nf90_put_var(ihisfile, id_genstru_id,  trimexact(cgen_ids(igen), strlen_netcdf), (/ 1, i /))
            end do
        end if

        if (jahisorif > 0 .and. network%sts%numOrifices > 0) then
           do i = 1, network%sts%numOrifices
              istru = network%sts%orificeIndices(i)
              ierr = nf90_put_var(ihisfile, id_orifgen_id,  trimexact(network%sts%struct(istru)%id, strlen_netcdf),  (/ 1, i /))
           end do
        end if

        if (jahisbridge > 0 .and. network%sts%numBridges > 0) then
           do i = 1, network%sts%numBridges
              istru = network%sts%bridgeIndices(i)
              ierr = nf90_put_var(ihisfile, id_bridge_id,  trimexact(network%sts%struct(istru)%id, strlen_netcdf),  (/ 1, i /))
           end do
        end if

        if (jahisculv > 0 .and. network%sts%numCulverts > 0) then
           do i = 1, network%sts%numCulverts
              istru = network%sts%culvertIndices(i)
              ierr = nf90_put_var(ihisfile, id_culvert_id,  trimexact(network%sts%struct(istru)%id, strlen_netcdf),  (/ 1, i /))
           end do
        end if

        if (jahisuniweir > 0 .and. network%sts%numuniweirs > 0) then
           do i = 1, network%sts%numuniweirs
              istru = network%sts%uniweirIndices(i)
              ierr = nf90_put_var(ihisfile, id_uniweir_id,  trimexact(network%sts%struct(istru)%id, strlen_netcdf),  (/ 1, i /))
           end do
        end if

        if (jahiscmpstru > 0 .and. network%cmps%count > 0) then
           do i = 1, network%cmps%count
              ierr = nf90_put_var(ihisfile, id_cmpstru_id,  trimexact(network%cmps%compound(i)%id, strlen_netcdf),  (/ 1, i /))
           end do
        end if

        if (jahislateral > 0 .and. numlatsg > 0) then
           do i = 1, numlatsg
              ierr = nf90_put_var(ihisfile, id_lat_id,  trimexact(lat_ids(i), strlen_netcdf), (/ 1, i /))
           end do
        end if

        if (jahispump > 0 .and. npumpsg > 0) then
            do i=1,npumpsg
               ierr = nf90_put_var(ihisfile, id_pump_id,  trimexact(pump_ids(i), strlen_netcdf),      (/ 1, i /))

               ! NOTE: the code below is now only active for pumps (DELFT3D-36341). Should be
               ! generalized for all structure locations that are polyline based.
               !
               ! Store one single representative x/y point for each pump in the time series file,
               ! because CF conventions require that for variables on discrete geometries.
               ! Computed at half the total length of the snapped flow links
               ! (so, it lies on an edge, not per se on the input pump pli)).
               pumplensum = 0d0
               do k = L1pumpsg(i), L2pumpsg(i)
                  Lf = abs(kpump(3,k))
                  pumplensum = pumplensum + wu(Lf)
               end do
               pumplenmid = pumplensum / 2

               ! Find the mid point on the snapped flow link path
               pumplensum = 0d0
               do k = L1pumpsg(i), L2pumpsg(i)
                  Lf = abs(kpump(3,k))
                  if (pumplensum + wu(Lf) >= pumplenmid) then
                     if (kcu(Lf) == 2) then ! 2D
                        if (kpump(3,k) > 0) then
                           k3 = lncn(1,Lf)
                           k4 = lncn(2,Lf)
                        else
                           k3 = lncn(2,Lf)
                           k4 = lncn(1,Lf)
                        end if
                        w1 = (pumplenmid-pumplensum)/wu(Lf)
                        pumpxmid = w1*xk(k3) + (1d0-w1)*xk(k4)
                        pumpymid = w1*yk(k3) + (1d0-w1)*yk(k4)
                     else                   ! 1D
                        pumpxmid = xu(Lf)
                        pumpymid = yu(Lf)
                     end if
                     exit ! mid point was found
                  else
                     pumplensum = pumplensum + wu(Lf)
                  end if
               end do

               ierr = nf90_put_var(ihisfile, id_pump_xmid,  pumpxmid,      (/ i /))
               ierr = nf90_put_var(ihisfile, id_pump_ymid,  pumpymid,      (/ i /))
            end do
        end if
        if (jahisgate > 0 .and. ngatesg > 0) then
            do i=1,ngatesg
               ierr = nf90_put_var(ihisfile, id_gatename,  trimexact(gate_ids(i), strlen_netcdf),      (/ 1, i /))
            end do
        end if
        if (jahisgate > 0 .and. ngategen > 0) then
           do i=1,ngategen
              igen = gate2cgen(i)
              ierr = nf90_put_var(ihisfile, id_gategenname,  trimexact(cgen_ids(igen), strlen_netcdf),      (/ 1, i /))
           end do
        end if
        if (jahiscdam > 0 .and. ncdamsg > 0) then
            do i=1,ncdamsg
               ierr = nf90_put_var(ihisfile, id_cdamname,  trimexact(cdam_ids(i), strlen_netcdf),      (/ 1, i /))
            end do
        end if
        if (jahisweir > 0 .and. nweirgen > 0 ) then
           if (allocated(weir2cgen)) then
              do i=1,nweirgen
                 igen = weir2cgen(i)
                 ierr = nf90_put_var(ihisfile, id_weirgen_id,  trimexact(cgen_ids(igen), strlen_netcdf),      (/ 1, i /))
              end do
           else if (network%sts%numWeirs > 0) then
              do i=1,nweirgen
                 istru = network%sts%weirIndices(i)
                 ierr = nf90_put_var(ihisfile, id_weirgen_id,  trimexact(network%sts%struct(istru)%id, strlen_netcdf),      (/ 1, i /))
              end do
           end if
        end if

        if (jahisdambreak > 0 .and. ndambreak > 0) then
            do i = 1,ndambreaksg
               ierr = nf90_put_var(ihisfile, id_dambreak_id, trimexact(dambreak_ids(i), strlen_netcdf),(/ 1, i /))
            end do
        end if

        if (jahislongculv > 0 .and. nlongculverts > 0) then
           do i = 1, nlongculverts
              ierr = nf90_put_var(ihisfile, id_longculvert_id,  trimexact(longculverts(i)%id, strlen_netcdf),  (/ 1, i /))
           end do
        end if

        if (jased>0 .and. stm_included .and. jahissed>0) then
           do i=1,stmpar%lsedtot
              ierr = nf90_put_var(ihisfile, id_frac_name, trimexact(stmpar%sedpar%namsed(i), strlen_netcdf), (/ 1, i /))
           enddo
        end if

        if (dad_included) then
           !
           !do i=1,stmpar%lsedtot
           !   ierr = nf90_put_var(ihisfile, id_frac_name, trimexact(stmpar%sedpar%namsed(i), strlen_netcdf), (/ 1, i /))
           !enddo
           !ierr = nf90_put_var(ihisfile, id_frac_name, 'subsoil sediment', (/ 1, stmpar%lsedtot+1 /))        ! rest category
           !
           do i=1,(dadpar%nadred+dadpar%nasupl)
              ierr = nf90_put_var(ihisfile, id_dred_name, trimexact(dadpar%dredge_areas(i), strlen_netcdf), (/ 1, i /))
           enddo
           !
           do i=1,dadpar%nadump
              ierr = nf90_put_var(ihisfile, id_dump_name, trimexact(dadpar%dump_areas(i), strlen_netcdf), (/ 1, i /))
           enddo
        endif
        if (timon) call timstop ( handle_extra(63))
    endif
    ! Increment output counters in m_flowtimes.
    time_his = tim
    it_his   = it_his + 1

    if (timon) call timstrt ('unc_write_his time data', handle_extra(64))

    ierr = nf90_put_var(ihisfile, id_time, time_his, (/ it_his /))
    ierr = nf90_put_var(ihisfile, id_timestep, dts, (/ it_his /))
    if (timon) call timstop ( handle_extra(64))

!   Observation points (fixed+moving)

    ntot = numobs + nummovobs
    valobsT(1:ntot, 1:IPNT_NUM) = transpose(valobs)
    if (ntot > 0) then
       ierr = nf90_put_var(ihisfile,    id_vars,   valobsT(:,IPNT_S1),    start = (/ 1, it_his /), count = (/ ntot, 1 /))
       ierr = nf90_put_var(ihisfile,    id_hs  ,   valobsT(:,IPNT_HS),    start = (/ 1, it_his /), count = (/ ntot, 1 /))
       if( stm_included ) then
          ierr = nf90_put_var(ihisfile,    id_varb,   valobsT(:,IPNT_BL),    start = (/ 1, it_his /), count = (/ ntot, 1 /))
       else
          ierr = nf90_put_var(ihisfile,    id_varb,   valobsT(:,IPNT_BL),    start = (/ 1 /) )
       endif

       ! write geometry variables at the first time of history output
       if (it_his == 1) then
          call realloc(node_count, numobs)
          node_count = 1
          ierr = nf90_put_var(ihisfile,    id_statgeom_node_count, node_count)
          ierr = nf90_put_var(ihisfile,    id_statgeom_node_coordx,  xobs(:), start = (/ 1 /), count = (/ numobs /))
          ierr = nf90_put_var(ihisfile,    id_statgeom_node_coordy,  yobs(:), start = (/ 1 /), count = (/ numobs /))
#ifdef HAVE_PROJ
          if (add_latlon) then
             call transform_and_put_latlon_coordinates(ihisfile, id_statgeom_node_lon, id_statgeom_node_lat, nccrs%proj_string, xobs, yobs)
       end if
#endif
       end if

       if ( nummovobs > 0 ) then
          ierr = nf90_put_var(ihisfile,    id_statx,  xobs(:),            start = (/ 1, it_his /), count = (/ ntot, 1 /))
          ierr = nf90_put_var(ihisfile,    id_staty,  yobs(:),            start = (/ 1, it_his /), count = (/ ntot, 1 /))
#ifdef HAVE_PROJ
          if (add_latlon) then
             call transform_and_put_latlon_coordinates(ihisfile, id_statlon, id_statlat, nccrs%proj_string, xobs, yobs, start = (/ 1, it_his /), count = (/ ntot, 1 /))
          end if
#endif
       else
          ierr = nf90_put_var(ihisfile,    id_statx,  xobs(:),            start = (/ 1 /), count = (/ ntot /))
          ierr = nf90_put_var(ihisfile,    id_staty,  yobs(:),            start = (/ 1 /), count = (/ ntot /))
#ifdef HAVE_PROJ
          if (add_latlon) then
             call transform_and_put_latlon_coordinates(ihisfile, id_statlon, id_statlat, nccrs%proj_string, xobs, yobs)
       endif
#endif
    endif
    endif

    if (timon) call timstrt('unc_write_his obs data 1', handle_extra(56))
    if (japatm > 0) then
       ierr = nf90_put_var(ihisfile, id_varpatm, valobsT(:,IPNT_patm), start = (/ 1, it_his /), count = (/ ntot, 1 /))
    endif

    if (jawind > 0) then
       ierr = nf90_put_var(ihisfile, id_varwx,  valobsT(:,IPNT_wx),    start = (/ 1, it_his /), count = (/ ntot, 1 /))
       ierr = nf90_put_var(ihisfile, id_varwy,  valobsT(:,IPNT_wy),    start = (/ 1, it_his /), count = (/ ntot, 1 /))
    endif

    if ((jarain > 0) .and. (jahisrain > 0)) then
       ierr = nf90_put_var(ihisfile, id_varrain,  valobsT(:,IPNT_rain),    start = (/ 1, it_his /), count = (/ ntot, 1 /))
    endif

    if ((infiltrationmodel == DFM_HYD_INFILT_CONST .or. infiltrationmodel == DFM_HYD_INFILT_HORTON) .and. jahisinfilt > 0) then
       ierr = nf90_put_var(ihisfile, id_infiltcap,  valobsT(:,IPNT_infiltcap),    start = (/ 1, it_his /), count = (/ ntot, 1 /))
       ierr = nf90_put_var(ihisfile, id_infiltact,  valobsT(:,IPNT_infiltact),    start = (/ 1, it_his /), count = (/ ntot, 1 /))
    endif
    if (timon) call timstop(handle_extra(56))

    if (timon) call timstrt('unc_write_his obs/crs data 2', handle_extra(57))

    if (numobs+nummovobs > 0) then
      if ( kmx>0 ) then
!      3D
       ierr = nf90_put_var(ihisfile,    id_varucxq, valobsT(:,IPNT_UCXQ),  start = (/ 1, it_his /), count = (/ ntot, 1 /)) ! depth-averaged velocity
       ierr = nf90_put_var(ihisfile,    id_varucyq, valobsT(:,IPNT_UCYQ),  start = (/ 1, it_his /), count = (/ ntot, 1 /))

       do kk = 1,kmx
             if (jahisvelvec > 0) then
          ierr = nf90_put_var(ihisfile,    id_varucx, valobsT(:,IPNT_UCX+kk-1),  start = (/ kk, 1, it_his /), count = (/ 1, ntot, 1 /))
          ierr = nf90_put_var(ihisfile,    id_varucy, valobsT(:,IPNT_UCY+kk-1),  start = (/ kk, 1, it_his /), count = (/ 1, ntot, 1 /))
          ierr = nf90_put_var(ihisfile,    id_varucz, valobsT(:,IPNT_UCZ+kk-1),  start = (/ kk, 1, it_his /), count = (/ 1, ntot, 1 /))
             end if
       if (jasal > 0) then
             ierr = nf90_put_var(ihisfile, id_varsal, valobsT(:,IPNT_SA1 +kk-1), start = (/ kk, 1, it_his /), count = (/ 1, ntot, 1 /))
       end if
       if (jatem > 0) then
             ierr = nf90_put_var(ihisfile, id_vartem, valobsT(:,IPNT_TEM1+kk-1), start = (/ kk, 1, it_his /), count = (/ 1, ntot, 1 /))
       end if
       if( (jasal > 0 .or. jatem > 0 .or. jased > 0 ) .and. jahisrho > 0) then
          if ( density_is_pressure_dependent() ) then
             ierr = nf90_put_var(ihisfile, id_varrho  , valobsT(:,IPNT_RHO +kk-1) , start = (/ kk, 1, it_his /), count = (/ 1, ntot, 1 /))
          else
             ierr = nf90_put_var(ihisfile, id_varrhop , valobsT(:,IPNT_RHOP +kk-1), start = (/ kk, 1, it_his /), count = (/ 1, ntot, 1 /))
          endif
       end if
       if (jased > 0 .and. .not. stm_included) then
             ierr = nf90_put_var(ihisfile, id_varsed, valobsT(:,IPNT_SED +kk-1), start = (/ kk, 1, it_his /), count = (/ 1, ntot, 1 /))
       end if
             if (jahisvelocity > 0) then
                ierr = nf90_put_var(ihisfile, id_varumag, valobsT(:,IPNT_UMAG+kk-1),  start = (/ kk, 1, it_his /), count = (/ 1, ntot, 1 /))
             end if
             if (jahisdischarge > 0) then
                ierr = nf90_put_var(ihisfile, id_varqmag, valobsT(:,IPNT_QMAG+kk-1),  start = (/ kk, 1, it_his /), count = (/ 1, ntot, 1 /))
             end if
          if (IVAL_TRA1 > 0) then
             do j = IVAL_TRA1,IVAL_TRAN   ! enumerators of tracers in valobs array (not the pointer)
               i = j - IVAL_TRA1 + 1
               ierr = nf90_put_var(ihisfile, id_tra(i), valobsT(:,IPNT_TRA1 + (i-1)*kmx+kk-1), start = (/ kk, 1, it_his /), count = (/ 1, ntot, 1/))
             enddo
          end if
          if (IVAL_HWQ1 > 0) then
             do j = IVAL_HWQ1,IVAL_HWQN   ! enumerators of waq output in valobs array (not the pointer)
               i = j - IVAL_HWQ1 + 1
               if (i .le. noout_user + noout_statt) then
                  ierr = nf90_put_var(ihisfile, id_hwq(i), valobsT(:,IPNT_HWQ1 + (i-1)*kmx+kk-1), start = (/ kk, 1, it_his /), count = (/ 1, ntot, 1/))
               else if (comparereal(tim, ti_hise, eps10) == 0) then
                  ierr = nf90_put_var(ihisfile, id_hwq(i), valobsT(:,IPNT_HWQ1 + (i-1)*kmx+kk-1), start = (/ kk, 1 /), count = (/ 1, ntot, 1/))
               endif
                enddo
          end if
          if (IVAL_WQB3D1 > 0) then
             do j = IVAL_WQB3D1,IVAL_WQB3DN   ! enumerators of 3d waqbot output in valobs array (not the pointer)
               i = j - IVAL_WQB3D1 + 1
               ierr = nf90_put_var(ihisfile, id_hwqb3d(i), valobsT(:,IPNT_WQB3D1 + (i-1)*kmx+kk-1), start = (/ kk, 1, it_his /), count = (/ 1, ntot, 1/))
             enddo
          end if
          if (IVAL_SF1 > 0) then
             call realloc(toutputx, (/ntot, stmpar%lsedsus /), keepExisting=.false., fill = dmiss)
             do j = IVAL_SF1,IVAL_SFN
               i = j - IVAL_SF1 + 1
               toutputx(:,i) = valobsT(:,IPNT_SF1 + (i-1)*(kmx+1)+kk-1)
             enddo
             ierr = nf90_put_var(ihisfile, id_sf, toutputx, start = (/ kk, 1, 1, it_his /), count = (/ 1, ntot, stmpar%lsedsus, 1/))
          end if
          if (jawave>0 .and. .not. flowwithoutwaves) then
             ierr = nf90_put_var(ihisfile,    id_ustx, valobsT(:,IPNT_UCXST+kk-1),  start = (/ kk, 1, it_his /), count = (/ 1, ntot, 1 /))
             ierr = nf90_put_var(ihisfile,    id_usty, valobsT(:,IPNT_UCYST+kk-1),  start = (/ kk, 1, it_his /), count = (/ 1, ntot, 1 /))
          endif
       enddo
     else
!      2D
          if (jahisvelvec > 0) then
       ierr = nf90_put_var(ihisfile,    id_varucx, valobsT(:,IPNT_UCX),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
       ierr = nf90_put_var(ihisfile,    id_varucy, valobsT(:,IPNT_UCY),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
          end if
          if (jahisvelocity > 0) then
             ierr = nf90_put_var(ihisfile, id_varumag, valobsT(:,IPNT_UMAG),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
          end if
          if (jahisdischarge > 0) then
            ierr = nf90_put_var(ihisfile, id_varqmag, valobsT(:,IPNT_QMAG),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
          end if
       if (jasal > 0) then
          ierr = nf90_put_var(ihisfile, id_varsal, valobsT(:,IPNT_SA1),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
       endif
       if (jatem > 0) then
          ierr = nf90_put_var(ihisfile, id_vartem, valobsT(:,IPNT_TEM1), start = (/ 1, it_his /), count = (/ ntot, 1 /))
       end if
       if ( (jasal > 0 .or. jatem > 0 .or. jased > 0 )  .and. jahisrho > 0) then
          ierr = nf90_put_var(ihisfile, id_varrhop, valobsT(:,IPNT_RHOP) ,  start = (/ 1, it_his /), count = (/ ntot, 1 /))
       end if

       if (IVAL_TRA1 > 0) then
          do j = IVAL_TRA1,IVAL_TRAN   ! enumerators of tracers in valobs array (not the pointer)
            i = j - IVAL_TRA1 + 1
            ierr = nf90_put_var(ihisfile, id_tra(i), valobsT(:,IPNT_TRA1 + i-1), start = (/ 1, it_his /), count = (/ ntot, 1/))
          end do
       end if
       if (IVAL_HWQ1 > 0) then
          do j = IVAL_HWQ1,IVAL_HWQN   ! enumerators of extra waq output in valobs array (not the pointer)
            i = j - IVAL_HWQ1 + 1
            ierr = nf90_put_var(ihisfile, id_hwq(i), valobsT(:,IPNT_HWQ1 + i-1), start = (/ 1, it_his /), count = (/ ntot, 1/))
          end do
       end if
       if (IVAL_WQB3D1 > 0) then
          do j = IVAL_WQB3D1,IVAL_WQB3DN   ! enumerators of waqbot variables in valobs array (not the pointer)
            i = j - IVAL_WQB3D1 + 1
            ierr = nf90_put_var(ihisfile, id_hwqb3d(i), valobsT(:,IPNT_WQB3D1 + i-1), start = (/ 1, it_his /), count = (/ ntot, 1/))
          end do
       end if
       if (IVAL_SF1 > 0) then
          call realloc(toutputx, (/ntot, stmpar%lsedsus /), keepExisting=.false., fill = dmiss)
          do j = IVAL_SF1,IVAL_SFN
            i = j - IVAL_SF1 + 1
            toutputx(:,i) = valobsT(:,IPNT_SF1 + i-1)
          end do
          ierr = nf90_put_var(ihisfile, id_sf, toutputx, start = (/ 1, 1, it_his /), count = (/ ntot, stmpar%lsedsus, 1/))
       end if
       if (IVAL_WS1 > 0) then
          call realloc(toutputx, (/ntot, stmpar%lsedsus /), keepExisting=.false., fill = dmiss)
          do j = IVAL_WS1,IVAL_WSN
            i = j - IVAL_WS1 + 1
            toutputx(:,i) = valobsT(:,IPNT_WS1 + i-1)
            ierr = nf90_put_var(ihisfile, id_ws, toutputx, start = (/ 1, 1, it_his /), count = (/ ntot, stmpar%lsedsus, 1/))
          enddo
       end if
       !
       if (jased > 0 .and. .not. stm_included) then
          ierr = nf90_put_var(ihisfile, id_varsed, valobsT(:,IPNT_SED),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
       end if
       !
       if (jawave>0 .and. .not. flowwithoutwaves) then
          ierr = nf90_put_var(ihisfile,    id_ustx, valobsT(:,IPNT_UCXST),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
          ierr = nf90_put_var(ihisfile,    id_usty, valobsT(:,IPNT_UCYST),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
       endif
     endif

    if (jahistaucurrent>0) then
       ierr = nf90_put_var(ihisfile, id_TAUX,   valobsT(:,IPNT_TAUX), start = (/ 1, it_his /), count = (/ ntot, 1 /))
       ierr = nf90_put_var(ihisfile, id_TAUY,   valobsT(:,IPNT_TAUY), start = (/ 1, it_his /), count = (/ ntot, 1 /))
    endif

    if ( jawave.eq.4 ) then
       ierr = nf90_put_var(ihisfile, id_R,      valobsT(:,IPNT_WAVER), start = (/ 1, it_his /), count = (/ ntot, 1 /))
    end if

    if (jawave>0) then
       ierr = nf90_put_var(ihisfile, id_WH,      valobsT(:,IPNT_WAVEH),   start = (/ 1, it_his /), count = (/ ntot, 1 /))
       ierr = nf90_put_var(ihisfile, id_WD,      valobsT(:,IPNT_WAVED),   start = (/ 1, it_his /), count = (/ ntot, 1 /))
       ierr = nf90_put_var(ihisfile, id_WL,      valobsT(:,IPNT_WAVEL),   start = (/ 1, it_his /), count = (/ ntot, 1 /))
       ierr = nf90_put_var(ihisfile, id_WT,      valobsT(:,IPNT_WAVET),   start = (/ 1, it_his /), count = (/ ntot, 1 /))
       ierr = nf90_put_var(ihisfile, id_WU,      valobsT(:,IPNT_WAVEU),   start = (/ 1, it_his /), count = (/ ntot, 1 /))
    endif

   !    waq bottom variables are always 2D
     if (IVAL_WQB1 > 0) then
       do j = IVAL_WQB1,IVAL_WQBN   ! enumerators of tracers in valobs array (not the pointer)
         i = j - IVAL_WQB1 + 1
         ierr = nf90_put_var(ihisfile, id_hwqb(i), valobsT(:,IPNT_WQB1 + i-1), start = (/ 1, it_his /), count = (/ ntot, 1/))
       end do
     endif
    endif

    if (jatem > 1 .and. jahisheatflux > 0) then
       ierr = nf90_put_var(ihisfile,    id_Wind   , valobsT(:,IPNT_WIND),  start = (/ 1, it_his /), count = (/ ntot, 1 /))

       if ( jatem.gt.1 ) then   ! also heat modelling involved
          ierr = nf90_put_var(ihisfile, id_Tair   , valobsT(:,IPNT_TAIR),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
       end if

       if (jatem == 5 .and. allocated(Rhum) .and. allocated(Clou) ) then
           ierr = nf90_put_var(ihisfile, id_Rhum   , valobsT(:,IPNT_RHUM),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
           ierr = nf90_put_var(ihisfile, id_Clou   , valobsT(:,IPNT_CLOU),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
       end if

       if (jatem == 5 ) then
          ierr = nf90_put_var(ihisfile, id_Qsun   , valobsT(:,IPNT_QSUN),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
          ierr = nf90_put_var(ihisfile, id_Qeva   , valobsT(:,IPNT_QEVA),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
          ierr = nf90_put_var(ihisfile, id_Qcon   , valobsT(:,IPNT_QCON),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
          ierr = nf90_put_var(ihisfile, id_Qlong  , valobsT(:,IPNT_QLON),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
          ierr = nf90_put_var(ihisfile, id_Qfreva , valobsT(:,IPNT_QFRE),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
          ierr = nf90_put_var(ihisfile, id_Qfrcon , valobsT(:,IPNT_QFRC),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
       endif

       ierr = nf90_put_var(ihisfile,    id_Qtot   , valobsT(:,IPNT_QTOT),  start = (/ 1, it_his /), count = (/ ntot, 1 /))

    end if ! jamapheatflux > 0! jatem > 0

    if (ja_airdensity + ja_computed_airdensity > 0 .and. jahis_airdensity> 0) then
       ierr = nf90_put_var(ihisfile, id_airdensity   , valobsT(:,IPNT_AIRDENSITY),  start = (/ 1, it_his /), count = (/ ntot, 1 /))
    end if

    ! 3d layer interface quantities
    if (kmx > 0 ) then
       do kk = 1, kmx+1
          ierr = nf90_put_var(ihisfile,    id_zws,    valobsT(:,IPNT_ZWS+kk-1),   start = (/ kk,  1, it_his /), count = (/ 1, ntot, 1 /))
          ierr = nf90_put_var(ihisfile,    id_zwu,    valobsT(:,IPNT_ZWU+kk-1),   start = (/ kk,  1, it_his /), count = (/ 1, ntot, 1 /))
          if (kk > 1) then
             ierr = nf90_put_var(ihisfile, id_zcs,    valobsT(:,IPNT_ZCS+kk-2),   start = (/ kk-1,1, it_his /), count = (/ 1, ntot, 1 /))
          endif
          if ( (jasal > 0 .or. jatem > 0 .or. jased > 0) .and. jahisrho > 0) then
             ierr = nf90_put_var(ihisfile, id_bruv,   valobsT(:,IPNT_BRUV+kk-1),  start = (/ kk,  1, it_his /), count = (/ 1, ntot, 1 /))
          end if
       if (iturbulencemodel >= 3 .and. jahistur > 0) then
             ierr = nf90_put_var(ihisfile, id_turkin, valobsT(:,IPNT_TKIN +kk-1), start = (/ kk,  1, it_his /), count = (/ 1, ntot, 1 /))
             ierr = nf90_put_var(ihisfile, id_tureps, valobsT(:,IPNT_TEPS +kk-1), start = (/ kk,  1, it_his /), count = (/ 1, ntot, 1 /))
       end if
       if (iturbulencemodel > 1) then
             ierr = nf90_put_var(ihisfile, id_vicwwu, valobsT(:,IPNT_VICWW+kk-1), start = (/ kk,  1, it_his /), count = (/ 1, ntot, 1 /))
       end if
       if (idensform > 0 .and. jaRichardsononoutput > 0) then
             ierr = nf90_put_var(ihisfile, id_rich,   valobsT(:,IPNT_RICH +kk-1), start = (/ kk,  1, it_his /), count = (/ 1, ntot, 1 /))
       end if
          !
          if (IVAL_WS1 > 0) then
             call realloc(toutputx, (/ntot, stmpar%lsedsus /), keepExisting=.false., fill = dmiss)
             do j = IVAL_WS1,IVAL_WSN
               i = j - IVAL_WS1 + 1
               toutputx(:,i) = valobsT(:,IPNT_WS1 + (i-1)*(kmx+1)+kk-1)
             enddo
             ierr = nf90_put_var(ihisfile, id_ws, toutputx, start = (/ kk, 1, 1, it_his /), count = (/ 1, ntot, stmpar%lsedsus, 1/))
          end if
          !
          if (IVAL_SEDDIF1 > 0) then
             call realloc(toutputx, (/ntot, stmpar%lsedsus /), keepExisting=.false., fill = dmiss)
             do j = IVAL_SEDDIF1,IVAL_SEDDIFN
               i = j - IVAL_SEDDIF1 + 1
               toutputx(:,i) = valobsT(:,IPNT_SEDDIF1 + (i-1)*(kmx+1)+kk-1)
             enddo
             ierr = nf90_put_var(ihisfile, id_seddif, toutputx, start = (/ kk, 1, 1, it_his /), count = (/ 1, ntot, stmpar%lsedsus, 1/))
          end if
          !
       enddo
    endif
    !
    ! Bed composition variables
    if (jahissed>0 .and. jased>0 .and. stm_included) then
       if (stmpar%morpar%moroutput%taub) then
          ierr = nf90_put_var(ihisfile, id_taub, valobsT(:,IPNT_TAUB), start = (/ 1, it_his /), count = (/ ntot, 1 /))
       endif
       !
       select case (stmpar%morlyr%settings%iunderlyr)
          case (1)
             ! dpsed
             ierr = nf90_put_var(ihisfile, id_dpsed, valobsT(:,IPNT_DPSED),   start = (/ 1, it_his /), count = (/ ntot, 1 /))
             ! bodsed
             call realloc(toutputx, (/ntot, stmpar%lsedtot /), keepExisting=.false., fill = dmiss)
             do j = IVAL_BODSED1, IVAL_BODSEDN
               i = j - IVAL_BODSED1 + 1
               toutputx(:,i) = valobsT(:,IPNT_BODSED1 + i-1)
             end do
             ierr = nf90_put_var(ihisfile, id_bodsed, toutputx, start = (/ 1, 1, it_his /), count = (/ ntot, stmpar%lsedtot, 1/))
          case (2)
             nlyrs = stmpar%morlyr%settings%nlyr
             ! msed
             call realloc(toutput3, (/nlyrs, ntot, stmpar%lsedtot /), keepExisting=.false., fill = dmiss)
             toutput3 = dmiss
             do j = IVAL_MSED1,IVAL_MSEDN
                i = j - IVAL_MSED1 + 1
                do kk = 1,nlyrs
                   toutput3(kk,:,i) = valobsT(:,IPNT_MSED1 + (i-1)*(nlyrs)+kk-1)
                enddo
             enddo
             ierr = nf90_put_var(ihisfile, id_msed, toutput3, start = (/ 1, 1, 1, it_his /), count = (/ nlyrs, ntot, stmpar%lsedtot, 1/))
             ! lyrfrac
             toutput3=dmiss
             do j = IVAL_LYRFRAC1,IVAL_LYRFRACN
                i = j - IVAL_LYRFRAC1 + 1
                do kk = 1,nlyrs
                   toutput3(kk,:,i) = valobsT(:,IPNT_LYRFRAC1 + (i-1)*(nlyrs)+kk-1)
                enddo
             enddo
             ierr = nf90_put_var(ihisfile, id_lyrfrac, toutput3, start = (/ 1, 1, 1, it_his /), count = (/ nlyrs, ntot, stmpar%lsedtot, 1/))
             ! thlyr
             call realloc(toutputx, (/nlyrs, ntot /), keepExisting=.false., fill = dmiss)
             do kk = 1,nlyrs
                toutputx(kk,:) = valobsT(:,IPNT_THLYR+kk-1)
             enddo
             ierr = nf90_put_var(ihisfile, id_thlyr, toutputx,  start = (/ 1, 1, it_his /), count = (/ nlyrs, ntot, 1 /))
             ! poros
             if (stmpar%morlyr%settings%iporosity>0) then
                do kk = 1,nlyrs
                   toutputx(kk,:) = valobsT(:,IPNT_POROS+kk-1)
                enddo
                ierr = nf90_put_var(ihisfile, id_poros, toutputx,  start = (/ 1, 1, it_his /), count = (/ nlyrs, ntot, 1 /))
             endif
       end select
       !
       if (stmpar%morpar%moroutput%frac) then
          ! frac
          call realloc(toutputx, (/ntot, stmpar%lsedtot /), keepExisting=.false., fill = dmiss)
          do j = IVAL_FRAC1,IVAL_FRACN
            i = j - IVAL_FRAC1 + 1
            toutputx(:,i) = valobsT(:,IPNT_FRAC1 + i-1)
          enddo
          ierr = nf90_put_var(ihisfile, id_frac, toutputx, start = (/ 1, 1, it_his /), count = (/ ntot, stmpar%lsedtot, 1/))
       endif
       if (stmpar%morpar%moroutput%mudfrac) then
          ! mudfrac
          ierr = nf90_put_var(ihisfile, id_mudfrac, valobsT(:,IPNT_MUDFRAC), start = (/ 1, it_his /), count = (/ ntot, 1 /))
       endif
       if (stmpar%morpar%moroutput%sandfrac) then
          ! sandfrac
          ierr = nf90_put_var(ihisfile, id_sandfrac, valobsT(:,IPNT_SANDFRAC), start = (/ 1, it_his /), count = (/ ntot, 1 /))
       endif
       if (stmpar%morpar%moroutput%fixfac) then
          !fixfac
          call realloc(toutputx, (/ntot, stmpar%lsedtot /), keepExisting=.false., fill = dmiss)
          do j = IVAL_FIXFAC1,IVAL_FIXFACN
            i = j - IVAL_FIXFAC1 + 1
            toutputx(:,i) = valobsT(:,IPNT_FIXFAC1 + i-1)
          enddo
          ierr = nf90_put_var(ihisfile, id_fixfac, toutputx, start = (/ 1, 1, it_his /), count = (/ ntot, stmpar%lsedtot, 1/))
       endif
       if (stmpar%morpar%moroutput%hidexp) then
          ! hidexp
          call realloc(toutputx, (/ntot, stmpar%lsedtot /), keepExisting=.false., fill = dmiss)
          do j = IVAL_HIDEXP1,IVAL_HIDEXPN
            i = j - IVAL_HIDEXP1 + 1
            toutputx(:,i) = valobsT(:,IPNT_HIDEXP1 + i-1)
          enddo
          ierr = nf90_put_var(ihisfile, id_hidexp, toutputx, start = (/ 1, 1, it_his /), count = (/ ntot, stmpar%lsedtot, 1/))
       endif
       !
       if (stmpar%morpar%flufflyr%iflufflyr>0 .and. stmpar%lsedsus>0) then
          call realloc(toutputx, (/ntot, stmpar%lsedsus /), keepExisting=.false., fill = dmiss)
          do j = IVAL_MFLUFF1,IVAL_MFLUFFN
            i = j - IVAL_MFLUFF1 + 1
            toutputx(:,i) = valobsT(:,IPNT_MFLUFF1 + i-1)
          enddo
          ierr = nf90_put_var(ihisfile, id_mfluff, toutputx, start = (/ 1, 1, it_his /), count = (/ ntot, stmpar%lsedsus, 1/))
       end if
    endif

    !
    ! Cross sections
    if (ncrs > 0) then
       do i=1,ncrs
          ! Discharges Q
          ierr = nf90_put_var(ihisfile, id_varQ,    crs(i)%sumvalcur(IPNT_Q1C), (/ i, it_his /))
          ierr = nf90_put_var(ihisfile, id_varQint, crs(i)%sumvalcum(IPNT_Q1C), (/ i, it_his /))
!          ierr = nf90_put_var(ihisfile, id_varQavg, crs(i)%sumvalavg(IPNT_Q1C), (/ i, it_his /))

          ! Cross sectional areas A*u
          ierr = nf90_put_var(ihisfile, id_varAu,    crs(i)%sumvalcur(IPNT_AUC), (/ i, it_his /))
!          ierr = nf90_put_var(ihisfile, id_varAuavg, crs(i)%sumvalavg(IPNT_AUC), (/ i, it_his /))

          ! Average velocity Q/Au
          ierr = nf90_put_var(ihisfile, id_varu,    crs(i)%sumvalcur(IPNT_U1A), (/ i, it_his /))
!          ierr = nf90_put_var(ihisfile, id_varuavg, crs(i)%sumvalavg(IPNT_U1A), (/ i, it_his /))

             IP = IPNT_HUA
             do num = 1,NUMCONST_MDU
                IP = IP + 1
                if (num >= ISED1 .and. num <= ISEDN) then
                   l = sedtot2sedsus(num-ISED1+1)
                   select case(stmpar%morpar%moroutput%transptype)
                   case (0)
                      rhol = 1d0
                   case (1)
                      rhol = stmpar%sedpar%cdryb(l)
                   case (2)
                      rhol = stmpar%sedpar%rhosol(l)
                   end select
                   toutput_cum = crs(i)%sumvalcum(IP)/rhol
                   toutput_cur = crs(i)%sumvalcur(IP)/rhol
                else
                  toutput_cum = crs(i)%sumvalcum(IP)
                  toutput_cur = crs(i)%sumvalcur(IP)
                endif
                ierr = nf90_put_var(ihisfile, id_const_cum(num), toutput_cum, (/ i, it_his /))
                ierr = nf90_put_var(ihisfile, id_const(num),     toutput_cur, (/ i, it_his /))
             end do

          if( jased == 4 ) then
             if( stmpar%lsedtot > 0 ) then
             IP = IPNT_HUA + NUMCONST_MDU + 1
             ierr = nf90_put_var(ihisfile, id_sedbtrans, crs(i)%sumvalcum(IP), (/ i, it_his /))
             do lsed = 1,stmpar%lsedtot    ! bed load on crosssections per fraction
                IP = IP + 1
                ierr = nf90_put_var(ihisfile, id_sedbtransfrac(lsed), crs(i)%sumvalcum(IP), (/ i, it_his /))
             enddo
             endif
             if( stmpar%lsedsus > 0 ) then
                IP = IP + 1
                ierr = nf90_put_var(ihisfile, id_sedstrans, crs(i)%sumvalcum(IP), (/ i, it_his /))
                do lsed = 1,stmpar%lsedsus    ! suspended load on crosssections per fraction
                   IP = IP + 1
                   ierr = nf90_put_var(ihisfile, id_sedstransfrac(lsed), crs(i)%sumvalcum(IP), (/ i, it_his /))
                enddo
             endif
          endif
       end do
    end if
    if (timon) call timstop(handle_extra(57))

    if (timon) call timstrt('unc_write_his RUG', handle_extra(65))

    ! runup gauges
    if (nrug>0) then
       call realloc(geom_x,   nrug, fill=dmiss)
       call realloc(geom_y,   nrug, fill=dmiss)
       call realloc(toutput1, nrug, fill=dmiss)
       do i=1,nrug
          geom_x(i)   = rug(i)%maxx
          geom_y(i)   = rug(i)%maxy
          toutput1(i) = rug(i)%maxruh
       end do
       ierr = nf90_put_var(ihisfile, id_rugx,   geom_x(:),   start = (/ 1, it_his /), count = (/ nrug, 1 /))
       ierr = nf90_put_var(ihisfile, id_rugy,   geom_y(:),   start = (/ 1, it_his /), count = (/ nrug, 1 /))
       ierr = nf90_put_var(ihisfile, id_varruh, toutput1(:), start = (/ 1, it_his /), count = (/ nrug, 1 /))
    endif

    if (timon) call timstop(handle_extra(65))

    if (timon) call timstrt ( "unc_write_his str write", handle_extra(62))
    if (jahissourcesink > 0 .and. numsrc > 0) then
      if (tim == tstart_user) then
        do i = 1, numsrc
          ierr = nf90_put_var(ihisfile, id_srcname, trimexact(srcname(i), strlen_netcdf), (/ 1, i/) )
          ierr = nf90_put_var(ihisfile, id_qsrccur, qstss((numconst+1)*(i-1)+1), (/ i, it_his /))
        enddo
        ierr = nf90_put_var(ihisfile, id_srcx, xsrc)
        ierr = nf90_put_var(ihisfile, id_srcy, ysrc)
      else
         ierr = nf90_put_var(ihisfile, id_qsrccur, qsrc, (/ 1, it_his /))
      endif

      ! write geometry variables at the first time of history output
       if (it_his == 1) then
          j = 1
          call realloc(node_count, numsrc, fill = 0)
          call realloc(geom_x, 2)
          call realloc(geom_y, 2)
          do i = 1, numsrc
             k1 = ksrc(1,i)
             k2 = ksrc(4,i)
             nNodes = 0
             if (k1 > 0) then
                nNodes = nNodes + 1
                geom_x(nNodes) = xz(k1)
                geom_y(nNodes) = yz(k1)
             end if
             if (k2 > 0) then
                nNodes = nNodes + 1
                geom_x(nNodes) = xz(k2)
                geom_y(nNodes) = yz(k2)
             end if

             node_count(i) = nNodes
             if (nNodes > 0) then
                ierr = nf90_put_var(ihisfile, id_srcgeom_node_coordx,  geom_x(1:nNodes), start = (/ j /), count = (/ nNodes /))
                ierr = nf90_put_var(ihisfile, id_srcgeom_node_coordy,  geom_y(1:nNodes), start = (/ j /), count = (/ nNodes /))
             end if

             j = j + nNodes
          end do
          ierr = nf90_put_var(ihisfile, id_srcgeom_node_count, node_count)
       end if

      ierr = nf90_put_var(ihisfile, id_vsrccum, vsrccum, (/ 1, it_his /))
      ierr = nf90_put_var(ihisfile, id_qsrcavg, qsrcavg, (/ 1, it_his /))
      do i = 1, numsrc
        ierr = nf90_put_var(ihisfile, id_pred,  qstss((numconst+1)*(i-1) + 1), (/ i, it_his /))
        j = 1
        if (isalt > 0) then
           j = j + 1
           ierr = nf90_put_var(ihisfile, id_presa, qstss((numconst+1)*(i-1) + j), (/ i, it_his /))
        endif
        if (itemp > 0) then
           j = j + 1
           ierr = nf90_put_var(ihisfile, id_pretm, qstss((numconst+1)*(i-1) + j), (/ i, it_his /))
        endif
      enddo
    endif

      if (jahiscgen > 0 ) then
         if (ncgensg > 0) then
            do i = 1,ncgensg
               igen = i
               ierr = nf90_put_var(ihisfile, id_genstru_dis   , valcgen(2,i)   , (/ i, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_crestl, zcgen(3*igen-2), (/ i, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_edgel , zcgen(3*igen-1), (/ i, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_openw , zcgen(3*igen  ), (/ i, it_his /)) ! TODO: AvD: this part seems not entirely correct, double check with block below and duplication with gategen, etc.
               ierr = nf90_put_var(ihisfile, id_genstru_s1up  , valcgen(3,i)   , (/ i, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_s1dn  , valcgen(4,i)   , (/ i, it_his /))
            enddo
         end if
         if (ngenstru > 0) then
            valobsT(1:ngenstru, 1:NUMVALS_GENSTRU) = transpose(valgenstru)
            !do i=1,ngenstru
            !   !igen = genstru2cgen(i)
            !   ierr = nf90_put_var(ihisfile, id_genstru_dis   , valgenstru(2,i),  (/ i, it_his /))
            !   ierr = nf90_put_var(ihisfile, id_genstru_crestl, valgenstru(9,i),  (/ i, it_his /)) ! changed
            !   ierr = nf90_put_var(ihisfile, id_genstru_edgel , valgenstru(14,i), (/ i, it_his /)) ! changed
            !   ierr = nf90_put_var(ihisfile, id_genstru_openw , valgenstru(13,i), (/ i, it_his /)) ! changed
            !   ierr = nf90_put_var(ihisfile, id_genstru_s1up  , valgenstru(3,i),  (/ i, it_his /))
            !   ierr = nf90_put_var(ihisfile, id_genstru_s1dn  , valgenstru(4,i),  (/ i, it_his /))
            !   ierr = nf90_put_var(ihisfile, id_genstru_head,          valgenstru(5,i),  (/ i, it_his /))
            !   ierr = nf90_put_var(ihisfile, id_genstru_au,            valgenstru(6,i),  (/ i, it_his /))
            !   ierr = nf90_put_var(ihisfile, id_genstru_vel,           valgenstru(7,i),  (/ i, it_his /))
            !   if (network%sts%numGeneralStructures > 0) then
            !      ierr = nf90_put_var(ihisfile, id_genstru_s1crest,       valgenstru(8,i),  (/ i, it_his /))
            !      ierr = nf90_put_var(ihisfile, id_genstru_crestw,        valgenstru(10,i), (/ i, it_his /))
            !      ierr = nf90_put_var(ihisfile, id_genstru_stat,     int(valgenstru(11,i)), (/ i, it_his /))
            !      ierr = nf90_put_var(ihisfile, id_genstru_forcedif,      valgenstru(12,i), (/ i, it_his /))
            !      ierr = nf90_put_var(ihisfile, id_genstru_openh,         valgenstru(15,i), (/ i, it_his /))
            !      ierr = nf90_put_var(ihisfile, id_genstru_uppl,          valgenstru(16,i), (/ i, it_his /))
            !      ierr = nf90_put_var(ihisfile, id_genstru_dis_gate_open, valgenstru(17,i), (/ i, it_his /))
            !      ierr = nf90_put_var(ihisfile, id_genstru_dis_gate_over, valgenstru(18,i), (/ i, it_his /))
            !      ierr = nf90_put_var(ihisfile, id_genstru_dis_gate_under,valgenstru(19,i), (/ i, it_his /))
            !      ierr = nf90_put_var(ihisfile, id_genstru_au_open,       valgenstru(20,i), (/ i, it_his /))
            !      ierr = nf90_put_var(ihisfile, id_genstru_au_over,        valgenstru(21,i), (/ i, it_his /))
            !      ierr = nf90_put_var(ihisfile, id_genstru_au_under,      valgenstru(22,i), (/ i, it_his /))
            !      ierr = nf90_put_var(ihisfile, id_genstru_velgateopen,   valgenstru(23,i), (/ i, it_his /))
            !      ierr = nf90_put_var(ihisfile, id_genstru_velgateover,   valgenstru(24,i), (/ i, it_his /))
            !      ierr = nf90_put_var(ihisfile, id_genstru_velgateunder,  valgenstru(25,i), (/ i, it_his /))
            !   end if
            !enddo
            ierr = nf90_put_var(ihisfile, id_genstru_dis   , valobsT(1:ngenstru,IVAL_DIS),       (/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_genstru_crestl, valobsT(1:ngenstru,IVAL_CRESTL),    (/ 1, it_his /)) ! changed
            ierr = nf90_put_var(ihisfile, id_genstru_edgel , valobsT(1:ngenstru,IVAL_EDGEL),     (/ 1, it_his /)) ! changed
            ierr = nf90_put_var(ihisfile, id_genstru_openw , valobsT(1:ngenstru,IVAL_OPENW),     (/ 1, it_his /)) ! changed
            ierr = nf90_put_var(ihisfile, id_genstru_s1up  , valobsT(1:ngenstru,IVAL_S1UP),      (/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_genstru_s1dn  , valobsT(1:ngenstru,IVAL_S1DN),      (/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_genstru_head,   valobsT(1:ngenstru,IVAL_HEAD),      (/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_genstru_au,     valobsT(1:ngenstru,IVAL_AREA),      (/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_genstru_vel,    valobsT(1:ngenstru,IVAL_VEL),       (/ 1, it_his /))
            if (network%sts%numGeneralStructures > 0) then
               ierr = nf90_put_var(ihisfile, id_genstru_s1crest,       valobsT(1:ngenstru,IVAL_S1ONCREST),  (/ 1, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_crestw,        valobsT(1:ngenstru,IVAL_CRESTW),     (/ 1, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_stat,      int(valobsT(1:ngenstru,IVAL_STATE)),     (/ 1, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_forcedif,      valobsT(1:ngenstru,IVAL_FORCEDIF),   (/ 1, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_openh,         valobsT(1:ngenstru,IVAL_OPENH),      (/ 1, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_uppl,          valobsT(1:ngenstru,IVAL_UPPL),       (/ 1, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_dis_gate_open, valobsT(1:ngenstru,IVAL_DIS_OPEN),   (/ 1, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_dis_gate_over, valobsT(1:ngenstru,IVAL_DIS_OVER),   (/ 1, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_dis_gate_under,valobsT(1:ngenstru,IVAL_DIS_UNDER),  (/ 1, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_au_open,       valobsT(1:ngenstru,IVAL_AREA_OPEN),  (/ 1, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_au_over,       valobsT(1:ngenstru,IVAL_AREA_OVER),  (/ 1, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_au_under,      valobsT(1:ngenstru,IVAL_AREA_UNDER), (/ 1, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_velgateopen,   valobsT(1:ngenstru,IVAL_VEL_OPEN),   (/ 1, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_velgateover,   valobsT(1:ngenstru,IVAL_VEL_OVER),   (/ 1, it_his /))
               ierr = nf90_put_var(ihisfile, id_genstru_velgateunder,  valobsT(1:ngenstru,IVAL_VEL_UNDER),  (/ 1, it_his /))
            end if
            ! write geometry variables at the first time of history output
            if (it_his == 1) then
               if (network%sts%numGeneralStructures > 0) then ! new general structure
                  ierr = nf90_put_var(ihisfile, id_genstrugeom_node_coordx, geomXGenstru,     start = (/ 1 /), count = (/ nNodesGenstru /))
                  ierr = nf90_put_var(ihisfile, id_genstrugeom_node_coordy, geomYGenstru,     start = (/ 1 /), count = (/ nNodesGenstru /))
                  ierr = nf90_put_var(ihisfile, id_genstrugeom_node_count,  nodeCountGenstru, start = (/ 1 /), count = (/ network%sts%numGeneralStructures /))
                  if (allocated(geomXGenstru))     deallocate(geomXGenstru) ! Deallocate the geometry arrays after writing them to his-file.
                  if (allocated(geomYGenstru))     deallocate(geomYGenstru)
                  if (allocated(nodeCountGenstru)) deallocate(nodeCountGenstru)
               else ! old general structure
                  j = 1
                  call realloc(node_count, ngenstru, fill = 0)
                  do n = 1, ngenstru
                     i = genstru2cgen(n)
                     nlinks = L2cgensg(i) - L1cgensg(i) + 1
                     if (nlinks > 0) then
                        nNodes = nlinks + 1
                     else if (nlinks == 0) then
                        nNodes = 0
                     end if
                     node_count(n) = nNodes

                     if (nNodes > 0) then
                        call get_geom_coordinates_of_structure_old(i, nNodes, geom_x, geom_y)
                        ierr = nf90_put_var(ihisfile, id_genstrugeom_node_coordx, geom_x(1:nNodes), start = (/ j /), count = (/ nNodes /))
                        ierr = nf90_put_var(ihisfile, id_genstrugeom_node_coordy, geom_y(1:nNodes), start = (/ j /), count = (/ nNodes /))
                        j = j + nNodes
                     end if
                  end do
                  ierr = nf90_put_var(ihisfile, id_genstrugeom_node_count, node_count, start = (/ 1 /), count = (/ ngenstru /))
               end if
            end if
         endif
      endif

      if (jahispump > 0 .and. npumpsg > 0) then
         valobsT(1:npumpsg, 1:NUMVALS_PUMP) = transpose(valpump)
         !do i=1,npumpsg
         !   ierr = nf90_put_var(ihisfile, id_pump_dis,     valpump(2,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_pump_s1up,    valpump(3,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_pump_s1dn,    valpump(4,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_pump_struhead,valpump(5,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_pump_cap,     valpump(6,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_pump_disdir,  valpump(12,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_pump_stage,int(valpump(7,i)),(/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_pump_head,    valpump(8,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_pump_redufact,valpump(9,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_pump_s1del,   valpump(10,i),(/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_pump_s1suc,   valpump(11,i),(/ i, it_his /))
         !end do
         ierr = nf90_put_var(ihisfile, id_pump_dis,     valobsT(1:npumpsg,IVAL_DIS),      (/ 1, it_his /))
         ierr = nf90_put_var(ihisfile, id_pump_s1up,    valobsT(1:npumpsg,IVAL_S1UP),     (/ 1, it_his /))
         ierr = nf90_put_var(ihisfile, id_pump_s1dn,    valobsT(1:npumpsg,IVAL_S1DN),     (/ 1, it_his /))
         ierr = nf90_put_var(ihisfile, id_pump_struhead,valobsT(1:npumpsg,IVAL_HEAD),     (/ 1, it_his /))
         ierr = nf90_put_var(ihisfile, id_pump_cap,     valobsT(1:npumpsg,IVAL_PP_CAP),   (/ 1, it_his /))
         ierr = nf90_put_var(ihisfile, id_pump_disdir,  valobsT(1:npumpsg,IVAL_PP_DISDIR),(/ 1, it_his /))
         ierr = nf90_put_var(ihisfile, id_pump_stage,int(valobsT(1:npumpsg,IVAL_PP_STAG)),(/ 1, it_his /))
         ierr = nf90_put_var(ihisfile, id_pump_head,    valobsT(1:npumpsg,IVAL_PP_HEAD),  (/ 1, it_his /))
         ierr = nf90_put_var(ihisfile, id_pump_redufact,valobsT(1:npumpsg,IVAL_PP_RED),   (/ 1, it_his /))
         ierr = nf90_put_var(ihisfile, id_pump_s1del,   valobsT(1:npumpsg,IVAL_PP_S1DEL), (/ 1, it_his /))
         ierr = nf90_put_var(ihisfile, id_pump_s1suc,   valobsT(1:npumpsg,IVAL_PP_S1SUC), (/ 1, it_his /))
         ! write geometry variables at the first time of history output
         if (it_his == 1) then
            if (network%sts%numPumps > 0) then ! new pump
               ierr = nf90_put_var(ihisfile, id_pumpgeom_node_coordx, geomXPump,     start = (/ 1 /), count = (/ nNodesPump /))
               ierr = nf90_put_var(ihisfile, id_pumpgeom_node_coordy, geomYPump,     start = (/ 1 /), count = (/ nNodesPump /))
               ierr = nf90_put_var(ihisfile, id_pumpgeom_node_count,  nodeCountPump, start = (/ 1 /), count = (/ network%sts%numPumps /))
               if (allocated(geomXPump))     deallocate(geomXPump)
               if (allocated(geomYPump))     deallocate(geomYPump)
               if (allocated(nodeCountPump)) deallocate(nodeCountPump)
            else
               j = 1
               call realloc(node_count, npumpsg, fill = 0)
               do i = 1, npumpsg
                  nlinks = L2pumpsg(i) - L1pumpsg(i) + 1
                  if (nlinks > 0) then
                     nNodes = nlinks + 1
                  else if (nlinks == 0) then
                     nNodes = 0
                  end if
                  node_count(i) = nNodes

                  if (nNodes > 0) then
                     call realloc(geom_x, nNodes)
                     call realloc(geom_y, nNodes)

                     L0 = L1pumpsg(i)
                     L = abs(kpump(3,L0))
                     k1 = lncn(1,L)
                     k2 = lncn(2,L)
                     geom_x(1) = xk(k1)
                     geom_x(2) = xk(k2)
                     geom_y(1) = yk(k1)
                     geom_y(2) = yk(k2)
                     if (nlinks > 1) then
                        k = 3
                        do L0 = L1pumpsg(i)+1, L2pumpsg(i)
                           L = abs(kpump(3,L0))
                           k3 = lncn(2,L)
                           geom_x(k) = xk(k3)
                           geom_y(k) = yk(k3)
                           k = k+1
                        end do
                     end if

                     ierr = nf90_put_var(ihisfile, id_pumpgeom_node_coordx, geom_x(1:nNodes), start = (/ j /), count = (/ nNodes /))
                     ierr = nf90_put_var(ihisfile, id_pumpgeom_node_coordy, geom_y(1:nNodes), start = (/ j /), count = (/ nNodes /))
                     j = j + nNodes
                  end if
               end do
               ierr = nf90_put_var(ihisfile, id_pumpgeom_node_count, node_count, start = (/ 1 /), count = (/ npumpsg /))
            end if
         end if
      end if

      if (jahisorif > 0 .and. network%sts%numOrifices > 0) then
         do i=1,network%sts%numOrifices
            ierr = nf90_put_var(ihisfile, id_orifgen_dis   ,        valorifgen(IVAL_DIS,i),        (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_orifgen_s1up  ,        valorifgen(IVAL_S1UP,i),       (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_orifgen_s1dn  ,        valorifgen(IVAL_S1DN,i),       (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_orifgen_head,          valorifgen(IVAL_HEAD,i),       (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_orifgen_au,            valorifgen(IVAL_AREA,i),       (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_orifgen_vel,           valorifgen(IVAL_VEL,i),        (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_orifgen_s1crest,       valorifgen(IVAL_S1ONCREST,i),  (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_orifgen_crestl,        valorifgen(IVAL_CRESTL,i),     (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_orifgen_crestw,        valorifgen(IVAL_CRESTW,i),     (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_orifgen_stat,      int(valorifgen(IVAL_STATE,i)),     (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_orifgen_forcedif,      valorifgen(IVAL_FORCEDIF,i),   (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_orifgen_edgel ,        valorifgen(IVAL_EDGEL,i),      (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_orifgen_openh,         valorifgen(IVAL_OPENH,i),      (/ i, it_his /))
         enddo
         ! write geometry variables at the first time of history output
         if (it_his == 1) then
            ierr = nf90_put_var(ihisfile, id_orifgeom_node_coordx, geomXOrif,     start = (/ 1 /), count = (/ nNodesOrif /))
            ierr = nf90_put_var(ihisfile, id_orifgeom_node_coordy, geomYOrif,     start = (/ 1 /), count = (/ nNodesOrif /))
            ierr = nf90_put_var(ihisfile, id_orifgeom_node_count,  nodeCountOrif, start = (/ 1 /), count = (/ network%sts%numOrifices /))
            if (allocated(geomXOrif))     deallocate(geomXOrif)
            if (allocated(geomYOrif))     deallocate(geomYOrif)
            if (allocated(nodeCountOrif)) deallocate(nodeCountOrif)
               end if
         end if

      if (timon) call timstrt('unc_write_his bridge data', handle_extra(58))
      if (jahisbridge > 0 .and. network%sts%numBridges > 0) then
         !do i=1,network%sts%numBridges
         !   ierr = nf90_put_var(ihisfile, id_bridge_dis,   valbridge(2,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_bridge_s1up,  valbridge(3,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_bridge_s1dn,  valbridge(4,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_bridge_head,  valbridge(5,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_bridge_au,    valbridge(6,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_bridge_vel,   valbridge(7,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_bridge_blup,  valbridge(8,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_bridge_bldn,  valbridge(9,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_bridge_bl_act,valbridge(10,i),(/ i, it_his /))
         !enddo
            ierr = nf90_put_var(ihisfile, id_bridge_dis,   valbridge(IVAL_DIS, 1:network%sts%numBridges),     (/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_bridge_s1up,  valbridge(IVAL_S1UP, 1:network%sts%numBridges),    (/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_bridge_s1dn,  valbridge(IVAL_S1DN, 1:network%sts%numBridges),    (/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_bridge_head,  valbridge(IVAL_HEAD, 1:network%sts%numBridges),    (/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_bridge_au,    valbridge(IVAL_AREA, 1:network%sts%numBridges),    (/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_bridge_vel,   valbridge(IVAL_VEL, 1:network%sts%numBridges),     (/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_bridge_blup,  valbridge(IVAL_BLUP, 1:network%sts%numBridges),    (/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_bridge_bldn,  valbridge(IVAL_BLDN, 1:network%sts%numBridges),    (/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_bridge_bl_act,valbridge(IVAL_BLACTUAL,1:network%sts%numBridges), (/ 1, it_his /))
         ! write geometry variables at the first time of history output
         if (it_his == 1) then
            if (timon) call timstrt('Bridge geom', handle_extra(74))
               ierr = nf90_put_var(ihisfile, id_bridgegeom_node_coordx, geomXBridge,     start = (/ 1 /), count = (/ nNodesBridge /))
               ierr = nf90_put_var(ihisfile, id_bridgegeom_node_coordy, geomYBridge,     start = (/ 1 /), count = (/ nNodesBridge /))
               ierr = nf90_put_var(ihisfile, id_bridgegeom_node_count,  nodeCountBridge, start = (/ 1 /), count = (/ network%sts%numBridges /))
               if (allocated(geomXBridge))     deallocate(geomXBridge)
               if (allocated(geomYBridge))     deallocate(geomYBridge)
               if (allocated(nodeCountBridge)) deallocate(nodeCountBridge)
            if (timon) call timstop(handle_extra(74))
               end if
         end if
      if (timon) call timstop(handle_extra(58))

      if (jahisculv > 0 .and. network%sts%numCulverts > 0) then
         do i=1,network%sts%numCulverts
            ierr = nf90_put_var(ihisfile, id_culvert_dis,    valculvert(IVAL_DIS,i),           (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_culvert_s1up,   valculvert(IVAL_S1UP,i),          (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_culvert_s1dn,   valculvert(IVAL_S1DN,i),          (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_culvert_head,   valculvert(IVAL_HEAD,i),          (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_culvert_au,     valculvert(IVAL_AREA,i),          (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_culvert_vel,    valculvert(IVAL_VEL,i),           (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_culvert_crestl, valculvert(IVAL_CL_CRESTL,i),     (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_culvert_stat,  int(valculvert(IVAL_CL_STATE,i)),  (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_culvert_edgel , valculvert(IVAL_CL_EDGEL,i),      (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_culvert_openh,  valculvert(IVAL_CL_OPENH,i),      (/ i, it_his /))
         enddo
         ! write geometry variables at the first time of history output
         if (it_his == 1) then
            ierr = nf90_put_var(ihisfile, id_culvertgeom_node_coordx, geomXCulv,     start = (/ 1 /), count = (/ nNodesCulv /))
            ierr = nf90_put_var(ihisfile, id_culvertgeom_node_coordy, geomYCulv,     start = (/ 1 /), count = (/ nNodesCulv /))
            ierr = nf90_put_var(ihisfile, id_culvertgeom_node_count,  nodeCountCulv, start = (/ 1 /), count = (/ network%sts%numCulverts /))
            if (allocated(geomXCulv))     deallocate(geomXCulv)
            if (allocated(geomYCulv))     deallocate(geomYCulv)
            if (allocated(nodeCountCulv)) deallocate(nodeCountCulv)
               end if
         end if

      if (jahisuniweir > 0 .and. network%sts%numuniweirs > 0) then
         do i=1,network%sts%numuniweirs
            ierr = nf90_put_var(ihisfile, id_uniweir_dis,    valuniweir(IVAL_DIS,i),       (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_uniweir_s1up,   valuniweir(IVAL_S1UP,i),      (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_uniweir_s1dn,   valuniweir(IVAL_S1DN,i),      (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_uniweir_head,   valuniweir(IVAL_HEAD,i),      (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_uniweir_au,     valuniweir(IVAL_AREA,i),      (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_uniweir_vel,    valuniweir(IVAL_VEL,i),       (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_uniweir_crestl, valuniweir(IVAL_UW_CRESTL,i), (/ i, it_his /))
         enddo
         ! write geometry variables at the first time of history output
         if (it_his == 1) then
            ierr = nf90_put_var(ihisfile, id_uniweirgeom_node_coordx, geomXUniweir,     start = (/ 1 /), count = (/ nNodesUniweir /))
            ierr = nf90_put_var(ihisfile, id_uniweirgeom_node_coordy, geomYUniweir,     start = (/ 1 /), count = (/ nNodesUniweir /))
            ierr = nf90_put_var(ihisfile, id_uniweirgeom_node_count,  nodeCountUniweir, start = (/ 1 /), count = (/ network%sts%numuniweirs /))
            if (allocated(geomXUniweir))     deallocate(geomXUniweir)
            if (allocated(geomYUniweir))     deallocate(geomYUniweir)
            if (allocated(nodeCountUniweir)) deallocate(nodeCountUniweir)
               end if
         end if

      if (jahiscmpstru > 0 .and. network%cmps%count > 0) then
         do i=1,network%cmps%count
            ierr = nf90_put_var(ihisfile, id_cmpstru_dis,            valcmpstru(IVAL_DIS,i),  (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_cmpstru_s1up,           valcmpstru(IVAL_S1UP,i), (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_cmpstru_s1dn,           valcmpstru(IVAL_S1DN,i), (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_cmpstru_head,           valcmpstru(IVAL_HEAD,i), (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_cmpstru_au,             valcmpstru(IVAL_AREA,i), (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_cmpstru_vel,            valcmpstru(IVAL_VEL,i),  (/ i, it_his /))
         enddo
      end if

      if (jahislongculv > 0 .and. nlongculverts > 0) then
         do i=1,nlongculverts
            ierr = nf90_put_var(ihisfile, id_longculvert_dis,       vallongculvert(IVAL_DIS,i),      (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_longculvert_s1up,      vallongculvert(IVAL_S1UP,i),     (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_longculvert_s1dn,      vallongculvert(IVAL_S1DN,i),     (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_longculvert_head,      vallongculvert(IVAL_HEAD,i),     (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_longculvert_au,        vallongculvert(IVAL_AREA,i),     (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_longculvert_vel,       vallongculvert(IVAL_VEL,i),      (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_longculvert_valveopen, vallongculvert(IVAL_LC_VALVE,i), (/ i, it_his /))
         enddo
         ! write geometry variables at the first time of history output
         if (it_his == 1) then
            ierr = nf90_put_var(ihisfile, id_longculvertgeom_node_coordx, geomXLongCulv,     start = (/ 1 /), count = (/ nNodesLongCulv /))
            ierr = nf90_put_var(ihisfile, id_longculvertgeom_node_coordy, geomYLongCulv,     start = (/ 1 /), count = (/ nNodesLongCulv /))
            ierr = nf90_put_var(ihisfile, id_longculvertgeom_node_count,  nodeCountLongCulv, start = (/ 1 /), count = (/ nlongculverts /))
            if (allocated(geomXLongCulv))     deallocate(geomXLongCulv)
            if (allocated(geomYLongCulv))     deallocate(geomYLongCulv)
            if (allocated(nodeCountLongCulv)) deallocate(nodeCountLongCulv)
               end if
         end if

      if (jahislateral > 0 .and. numlatsg > 0) then
         ierr = nf90_put_var(ihisfile, id_lat_predis_inst,  qplat,       start = (/1,it_his/), count = (/numlatsg,1/))
         ierr = nf90_put_var(ihisfile, id_lat_predis_ave,   qplatAve,    start = (/1,it_his/), count = (/numlatsg,1/))
         ierr = nf90_put_var(ihisfile, id_lat_realdis_inst, qLatReal,    start = (/1,it_his/), count = (/numlatsg,1/))
         ierr = nf90_put_var(ihisfile, id_lat_realdis_ave,  qLatRealAve, start = (/1,it_his/), count = (/numlatsg,1/))
         ! write geometry variables at the first time of history output
         if (it_his == 1) then
            ierr = nf90_put_var(ihisfile, id_latgeom_node_coordx, geomXLat(1:nNodesLat), start = (/ 1 /), count = (/ nlatnd /))
            ierr = nf90_put_var(ihisfile, id_latgeom_node_coordy, geomYLat(1:nNodesLat), start = (/ 1 /), count = (/ nlatnd /))
            ierr = nf90_put_var(ihisfile, id_latgeom_node_count,  nodeCountLat)
         end if
      end if

      if (jahisgate > 0 .and. ngatesg > 0) then
         do i=1,ngatesg
            ierr = nf90_put_var(ihisfile, id_gate_dis  , valgate(2,i) , (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_gate_edgel, zgate(i)     , (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_gate_s1up , valgate(3,i) , (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_gate_s1dn , valgate(4,i) , (/ i, it_his /))
         end do
      end if

      if (jahisgate > 0 .and. ngategen > 0) then
         do i=1,ngategen
            igen = gate2cgen(i)
            ierr = nf90_put_var(ihisfile, id_gategen_dis  , valgategen(IVAL_DIS,i),        (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_gategen_sillh, valgategen(IVAL_GATE_SILLH,i), (/ i, it_his /))   ! changed
            ierr = nf90_put_var(ihisfile, id_gategen_edgel, valgategen(IVAL_GATE_EDGEL,i), (/ i, it_his /))   ! changed
            ierr = nf90_put_var(ihisfile, id_gategen_flowh, valgategen(IVAL_GATE_FLOWH,i), (/ i, it_his /))   ! TODO: AvD sillw
            ierr = nf90_put_var(ihisfile, id_gategen_openw, valgategen(IVAL_GATE_OPENW,i), (/ i, it_his /))   ! changed
            ierr = nf90_put_var(ihisfile, id_gategen_s1up , valgategen(IVAL_S1UP,i),       (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_gategen_s1dn , valgategen(IVAL_S1DN,i),       (/ i, it_his /))
         end do
         ! write geometry variables at the first time of history output
         if (it_his == 1) then
            j = 1
            call realloc(node_count, ngategen, fill = 0)
            do n = 1, ngategen
               i = gate2cgen(n)
               nlinks = L2cgensg(i) - L1cgensg(i) + 1
               if (nlinks > 0) then
                  nNodes = nlinks + 1
               else if (nlinks == 0) then
                  nNodes = 0
               end if
               node_count(n) = nNodes

               if (nNodes > 0) then
                  call get_geom_coordinates_of_structure_old(i, nNodes, geom_x, geom_y)
                  ierr = nf90_put_var(ihisfile, id_gategengeom_node_coordx, geom_x(1:nNodes), start = (/ j /), count = (/ nNodes /))
                  ierr = nf90_put_var(ihisfile, id_gategengeom_node_coordy, geom_y(1:nNodes), start = (/ j /), count = (/ nNodes /))
                  j = j + nNodes
               end if
            end do
            ierr = nf90_put_var(ihisfile, id_gategengeom_node_count, node_count, start = (/ 1 /), count = (/ ngategen /))
         end if
      end if

      if (jahiscdam > 0 .and. ncdamsg > 0) then
         do i = 1,ncdamsg
            ierr = nf90_put_var(ihisfile, id_cdam_dis   , valcdam(2,i), (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_cdam_crestl, zcdam(i)    , (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_cdam_s1up  , valcdam(3,i), (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_cdam_s1dn  , valcdam(4,i), (/ i, it_his /))
         end do
      end if

      if (jahisweir > 0 .and. nweirgen > 0) then
         valobsT(1:nweirgen, 1:NUMVALS_WEIRGEN) = transpose(valweirgen)
         !do i = 1,nweirgen
         !   ierr = nf90_put_var(ihisfile, id_weirgen_dis   , valweirgen(2,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_weirgen_s1up  , valweirgen(3,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_weirgen_s1dn  , valweirgen(4,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_weirgen_crestl, valweirgen(9,i), (/ i, it_his /))
         !   ierr = nf90_put_var(ihisfile, id_weirgen_crestw, valweirgen(10,i),(/ i, it_his /))
         !   if (network%sts%numWeirs > 0) then ! write extra files for new weirs
         !      ierr = nf90_put_var(ihisfile, id_weirgen_head  , valweirgen(5,i),  (/ i, it_his /))
         !      ierr = nf90_put_var(ihisfile, id_weirgen_au    , valweirgen(6,i),  (/ i, it_his /))
         !      ierr = nf90_put_var(ihisfile, id_weirgen_vel   , valweirgen(7,i),  (/ i, it_his /))
         !      ierr = nf90_put_var(ihisfile, id_weirgen_s1crest,valweirgen(8,i),  (/ i, it_his /))
         !      ierr = nf90_put_var(ihisfile, id_weir_stat, int(valweirgen(11,i)),(/ i, it_his /))
         !      ierr = nf90_put_var(ihisfile, id_weirgen_forcedif,valweirgen(12,i),(/i, it_his /))
         !   end if
         !end do
         ierr = nf90_put_var(ihisfile, id_weirgen_dis   , valobsT(1:nweirgen,IVAL_DIS),    (/ 1, it_his /))
         ierr = nf90_put_var(ihisfile, id_weirgen_s1up  , valobsT(1:nweirgen,IVAL_S1UP),   (/ 1, it_his /))
         ierr = nf90_put_var(ihisfile, id_weirgen_s1dn  , valobsT(1:nweirgen,IVAL_S1DN),   (/ 1, it_his /))
         ierr = nf90_put_var(ihisfile, id_weirgen_crestl, valobsT(1:nweirgen,IVAL_CRESTL), (/ 1, it_his /))
         ierr = nf90_put_var(ihisfile, id_weirgen_crestw, valobsT(1:nweirgen,IVAL_CRESTW), (/ 1, it_his /))
         if (network%sts%numWeirs > 0) then ! write extra files for new weirs
            ierr = nf90_put_var(ihisfile, id_weirgen_head  , valobsT(1:nweirgen,IVAL_HEAD),     (/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_weirgen_au    , valobsT(1:nweirgen,IVAL_AREA),     (/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_weirgen_vel   , valobsT(1:nweirgen,IVAL_VEL),      (/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_weirgen_s1crest,valobsT(1:nweirgen,IVAL_S1ONCREST),(/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_weir_stat, int(valobsT(1:nweirgen,IVAL_STATE)),    (/ 1, it_his /))
            ierr = nf90_put_var(ihisfile, id_weirgen_forcedif,valobsT(1:nweirgen,IVAL_FORCEDIF),(/ 1, it_his /))
         end if
         ! write geometry variables at the first time of history output
         if (it_his == 1) then

            if (network%sts%numWeirs > 0) then ! new weir

               ierr = nf90_put_var(ihisfile, id_weirgeom_node_coordx, geomXWeir,     start = (/ 1 /), count = (/ nNodesWeir /))
               ierr = nf90_put_var(ihisfile, id_weirgeom_node_coordy, geomYWeir,     start = (/ 1 /), count = (/ nNodesWeir /))
               ierr = nf90_put_var(ihisfile, id_weirgeom_node_count,  nodeCountWeir, start = (/ 1 /), count = (/ network%sts%numWeirs /))

               if (allocated(geomXWeir))     deallocate(geomXWeir)
               if (allocated(geomYWeir))     deallocate(geomYWeir)
               if (allocated(nodeCountWeir)) deallocate(nodeCountWeir)
            else
               j = 1
               call realloc(node_count, nweirgen, fill = 0)
               do n = 1, nweirgen
                  i = weir2cgen(n)
                  nlinks = L2cgensg(i) - L1cgensg(i) + 1
                  if (nlinks > 0) then
                     nNodes = nlinks + 1
                  else if (nlinks == 0) then
                     nNodes = 0
                  end if
                  node_count(n) = nNodes

                  if (nNodes > 0) then
                     call get_geom_coordinates_of_structure_old(i, nNodes, geom_x, geom_y)
                     ierr = nf90_put_var(ihisfile, id_weirgeom_node_coordx, geom_x(1:nNodes), start = (/ j /), count = (/ nNodes /))
                     ierr = nf90_put_var(ihisfile, id_weirgeom_node_coordy, geom_y(1:nNodes), start = (/ j /), count = (/ nNodes /))
                     j = j + nNodes
                  end if
               end do
               ierr = nf90_put_var(ihisfile, id_weirgeom_node_count, node_count, start = (/ 1 /), count = (/ nweirgen /))
            end if
         end if
      end if

      if (jahisdambreak > 0 .and. ndambreak > 0) then
         do i = 1,ndambreaksg
            ierr = nf90_put_var(ihisfile, id_dambreak_discharge,                    valdambreak(IVAL_DIS,i),        (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_dambreak_s1up,                         valdambreak(IVAL_S1UP,i),       (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_dambreak_s1dn,                         valdambreak(IVAL_S1DN,i),       (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_dambreak_head,                         valdambreak(IVAL_HEAD,i),       (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_dambreak_au,                           valdambreak(IVAL_AREA,i),       (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_dambreak_normal_velocity,              valdambreak(IVAL_VEL,i),        (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_dambreak_cresth,                       valdambreak(IVAL_DB_CRESTH,i),  (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_dambreak_crestw,                       valdambreak(IVAL_DB_CRESTW,i),  (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_dambreak_water_level_jump,             valdambreak(IVAL_DB_JUMP,i),    (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_dambreak_breach_width_time_derivative, valdambreak(IVAL_DB_TIMEDIV,i), (/ i, it_his /))
            ierr = nf90_put_var(ihisfile, id_dambreak_cumulative_discharge,         valdambreak(IVAL_DB_DISCUM,i),  (/ i, it_his /))
         end do
      end if
      if (timon) call timstop ( handle_extra(62))
      !
      if (timon) call timstrt('unc_write_his sed data', handle_extra(66))
      if (jased>0 .and. stm_included .and. jahissed>0 .and. stmpar%lsedtot>0) then
         if (stmpar%morpar%moroutput%sbcuv) then
            call realloc(toutputx, (/ntot, stmpar%lsedtot /), keepExisting=.false., fill = dmiss)
            call realloc(toutputy, (/ntot, stmpar%lsedtot /), keepExisting=.false., fill = dmiss)
            do l = 1, stmpar%lsedtot
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(l)
               case (2)
                  rhol = stmpar%sedpar%rhosol(l)
               end select
               toutputy(:,l) = valobsT(:,IPNT_SBCY1+l-1)/rhol
               toutputx(:,l) = valobsT(:,IPNT_SBCX1+l-1)/rhol
            end do
            ierr = nf90_put_var(ihisfile, id_sbcx, toutputx  , start = (/ 1, 1, it_his /), count = (/ ntot, stmpar%lsedtot, 1 /))
            ierr = nf90_put_var(ihisfile, id_sbcy, toutputy  , start = (/ 1, 1, it_his /), count = (/ ntot, stmpar%lsedtot, 1 /))
         endif
         !
         if (stmpar%morpar%moroutput%sscuv) then
            call realloc(toutputx, (/ntot, stmpar%lsedtot /), keepExisting=.false., fill = dmiss)
            call realloc(toutputy, (/ntot, stmpar%lsedtot /), keepExisting=.false., fill = dmiss)
            do l = 1, stmpar%lsedtot
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(l)
               case (2)
                  rhol = stmpar%sedpar%rhosol(l)
               end select
               toutputy(:,l) = valobsT(:,IPNT_SSCY1+l-1)/rhol
               toutputx(:,l) = valobsT(:,IPNT_SSCX1+l-1)/rhol
            end do
            ierr = nf90_put_var(ihisfile, id_sscx, toutputx  , start = (/ 1, 1, it_his /), count = (/ ntot, stmpar%lsedtot, 1 /))
            ierr = nf90_put_var(ihisfile, id_sscy, toutputy  , start = (/ 1, 1, it_his /), count = (/ ntot, stmpar%lsedtot, 1 /))
         endif
         !
         if (stmpar%morpar%moroutput%sbwuv .and. jawave>0 .and. .not. flowWithoutWaves) then
            call realloc(toutputx, (/ntot, stmpar%lsedtot /), keepExisting=.false., fill = dmiss)
            call realloc(toutputy, (/ntot, stmpar%lsedtot /), keepExisting=.false., fill = dmiss)
            do l = 1, stmpar%lsedtot
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(l)
               case (2)
                  rhol = stmpar%sedpar%rhosol(l)
               end select
               toutputy(:,l) = valobsT(:,IPNT_SBWY1+l-1)/rhol
               toutputx(:,l) = valobsT(:,IPNT_SBWX1+l-1)/rhol
            end do
            ierr = nf90_put_var(ihisfile, id_sbwx, toutputx  , start = (/ 1, 1, it_his /), count = (/ ntot, stmpar%lsedtot, 1 /))
            ierr = nf90_put_var(ihisfile, id_sbwy, toutputy  , start = (/ 1, 1, it_his /), count = (/ ntot, stmpar%lsedtot, 1 /))
         endif
         !
         if (stmpar%morpar%moroutput%sswuv .and. jawave>0 .and. .not. flowWithoutWaves) then
            call realloc(toutputx, (/ntot, stmpar%lsedtot /), keepExisting=.false., fill = dmiss)
            call realloc(toutputy, (/ntot, stmpar%lsedtot /), keepExisting=.false., fill = dmiss)
            do l = 1, stmpar%lsedtot
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(l)
               case (2)
                  rhol = stmpar%sedpar%rhosol(l)
               end select
               toutputy(:,l) = valobsT(:,IPNT_SSWY1+l-1)/rhol
               toutputx(:,l) = valobsT(:,IPNT_SSWX1+l-1)/rhol
            end do
            ierr = nf90_put_var(ihisfile, id_sswx, toutputx  , start = (/ 1, 1, it_his /), count = (/ ntot, stmpar%lsedtot, 1 /))
            ierr = nf90_put_var(ihisfile, id_sswy, toutputy  , start = (/ 1, 1, it_his /), count = (/ ntot, stmpar%lsedtot, 1 /))
         endif
         !
         !
         if (stmpar%morpar%moroutput%sourcesink .and. IVAL_SOUR1>0) then
            call realloc(toutputx, (/ntot, stmpar%lsedsus /), keepExisting=.false., fill = dmiss)
            call realloc(toutputy, (/ntot, stmpar%lsedsus /), keepExisting=.false., fill = dmiss)
            do l = 1, stmpar%lsedsus
               toutputx(:,l) = valobsT(:,IPNT_SOUR1+l-1)
               toutputy(:,l) = valobsT(:,IPNT_SINK1+l-1)
            end do
            ierr = nf90_put_var(ihisfile, id_sour, toutputx  , start = (/ 1, 1, it_his /), count = (/ ntot, stmpar%lsedsus, 1 /))
            ierr = nf90_put_var(ihisfile, id_sink, toutputy  , start = (/ 1, 1, it_his /), count = (/ ntot, stmpar%lsedsus, 1 /))
         endif
      endif

    if ( dad_included ) then  ! Output for dredging and dumping
       ierr = nf90_put_var(ihisfile, id_dredlink_dis, dadpar%link_sum  , start = (/ 1, 1, it_his /), count = (/ dadpar%nalink, stmpar%lsedtot, 1 /))
       ierr = nf90_put_var(ihisfile, id_dred_dis    , dadpar%totvoldred, start = (/ 1, it_his /), count = (/ dadpar%nadred+dadpar%nasupl, 1 /))
       ierr = nf90_put_var(ihisfile, id_dump_dis    , dadpar%totvoldump, start = (/ 1, it_his /), count = (/ dadpar%nadump, 1 /))

       cof0 = 1d0 ; if( time_his > 0d0 ) cof0 = time_his
       ierr = nf90_put_var(ihisfile, id_dred_tfrac  , dadpar%tim_dredged/cof0  , start = (/ 1, it_his /), count = (/ dadpar%nadred+dadpar%nasupl, 1 /))
       ierr = nf90_put_var(ihisfile, id_plough_tfrac, dadpar%tim_ploughed/cof0 , start = (/ 1, it_his /), count = (/ dadpar%nadred+dadpar%nasupl, 1 /))
    endif
    if (timon) call timstop(handle_extra(66))

    if (timon) call timstrt('unc_write_his IDX data', handle_extra(67))
    do num = 1,MAX_IDX
       if ( num.eq.IDX_InternalTidesDissipation ) then
          if ( jaFrcInternalTides2D.eq.1 ) then
             ierr = nf90_put_var(ihisfile, id_voltot(num), 1d-12*voltot(num),  start=(/ it_his /))
          end if
       else if ( num.eq.IDX_GravInput ) then
          if ( jatidep > 0 .or. jaselfal > 0 ) then
             ierr = nf90_put_var(ihisfile, id_voltot(num), 1d-12*voltot(num),  start=(/ it_his /))
          end if
       else if ( num.eq.IDX_SALInput .or. num.eq.IDX_SALInput2 ) then
          if ( jaselfal.gt.0 ) then
             ierr = nf90_put_var(ihisfile, id_voltot(num), 1d-12*voltot(num),  start=(/ it_his /))
          end if
       else
          ierr = nf90_put_var(ihisfile, id_voltot(num), voltot(num),  start=(/ it_his /))
       end if
    enddo
    if (timon) call timstop(handle_extra(67))

    if( jahisgate > 0 .and. ngatesg+ngategen > 0) then
       ! todo: remove all do loops
       ! ngatesg ! Old-fashioned gates 'gateloweredgelevel'
       ! Actual discharge:
       !ierr = nf90_put_var(ihisfile, id_gatedisch, gatedisch(1:ngatesg+ngategen),  start=(/ 1, it_his /), count = (/ ngatesg+ngategen, 1 /))

       !'pump_discharge_pumpA'
       !'pump_discharge_pumpB' (1:ntimes)

       !'pump_names' (1:npumps)
       !'pump_discharge'  (1:ntimes, 1:npumps)

       ! Door lower edge level
      ! ierr = nf90_put_var(ihisfile, id_zgate,     work ... (1:ngatesg+ngategen),  start=(/ 1, it_his /), count = (/ ngatesg+ngategen, 1 /))

       ! id_gatesill: not for old style gates, they are just at bed level, so leave empty value in the file on columns 1:ngatesg
!       ierr = nf90_put_var(ihisfile, id_gatesill, gatesill(ngatesg+1:ngatesg+ngategen),  start=(/ ngatesg+1, it_his /), count = (/ ngategen, 1 /))

!       ierr = nf90_put_var(ihisfile, id_zgate(num)    , zgate(num)    ,  start=(/ it_his /))

!       do num=1,ngategen ! New-style gate, via generalstructure
!          igen=gate2cgen(num)
!          ipos = ! Just add new style gates at the back of the old style gates
!          ierr = nf90_put_var(ihisfile, id_gatedisch,      gatedisch(ipos),  start=(/ it_his /))
!          ierr = nf90_put_var(ihisfile, id_zgate(num)    , zgate(num)    ,  start=(/ it_his /))
    endif
        if( jahiscdam > 0 .and. ncdamsg + nweirgen > 0) then
           ! see gates
       do num = 1,ncdamsg
!          ierr = nf90_put_var(ihisfile, id_cdamdisch(num), cdamdisch(num),  start=(/ it_his /))
!          ierr = nf90_put_var(ihisfile, id_zcdam(num)    , zcdam(num)    ,  start=(/ it_his /))
       enddo
    endif

    if (it_his == 1) then
      do n = 1, ST_MAX_TYPE
        call unc_write_struc_input_coordinates(ihisfile,n)
      enddo
    endif

    if ( jacheckmonitor.eq.1 ) then
      ierr = nf90_put_var(ihisfile, id_checkmon, checkmonitor, start=(/ 1, it_his /))

      ierr = nf90_put_var(ihisfile, id_num_timesteps, int(dnt), start=(/ it_his /))
      ierr = nf90_put_var(ihisfile, id_comp_time, tim_get_wallclock(handle_steps), start=(/ it_his /))
    end if

    if (unc_noforcedflush == 0) then
    ierr = nf90_sync(ihisfile) ! Flush file
    end if

    if (timon) call timstop (handle_extra(54))
    
    contains

    !> write fill value in double precision or single precision
    function write_real_fill_value(id_var) result(error)
    
    integer,          intent(in)  :: id_var           !< variable id
    integer                       :: error            !< NetCDF error 

    if ( nc_precision == nf90_double ) then
        error = nf90_put_att(ihisfile, id_var, '_FillValue', dmiss)
    else if ( nc_precision == nf90_float ) then
        error = nf90_put_att(ihisfile, id_var, '_FillValue', SNGL(dmiss))
    end if
    
    end function write_real_fill_value


end subroutine unc_write_his

    
