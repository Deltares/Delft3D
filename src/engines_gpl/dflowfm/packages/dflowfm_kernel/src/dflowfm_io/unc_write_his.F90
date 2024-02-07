!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2023.
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
    use odugrid
    use m_statistical_output
    use fm_statistical_output
    use m_output_config
    use m_particles
    use MessageHandling, only: err
   
    implicit none

    double precision, intent(in) :: tim                  !< Current time, should in fact be time1, since the data written is always s1, ucx, etc.


    integer, save :: id_laydim , id_laydimw, &
               id_statdim, id_strlendim, id_crsdim, id_crslendim, id_crsptsdim, id_timedim, &
               id_statx, id_staty, id_stat_id, id_statname, id_time, id_timestep, &
               id_statlon, id_statlat, id_crs_id, id_crsname, &
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
               id_gatedim,    id_gate_id,    id_gate_dis,    id_gate_edgel,     id_gate_s1up,      id_gate_s1dn,    &                              ! id_gate_head,
               id_cdamdim,    id_cdam_id,    id_cdam_dis,    id_cdam_crestl,    id_cdam_s1up,      id_cdam_s1dn,    &                              ! id_cdam_head,
               id_weirgendim, id_weirgen_id, id_weirgen_dis, id_weirgen_crestl, id_weirgen_crestw, id_weirgen_s1up,  id_weirgen_s1dn,  &        ! id_weirgen_head,
               id_weir_stat,  id_weirgen_vel, id_weirgen_au,  id_weirgen_head,   id_weirgen_forcedif, id_weirgen_s1crest,               &
               id_gategendim, id_gategen_id, id_gategen_dis, id_gategen_sillh,  id_gategen_sillw,  id_gategen_edgel, id_gategen_openw, &           ! id_gategen_head,
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
    integer, save :: id_statgeom_node_count,        id_statgeom_node_coordx,        id_statgeom_node_coordy,    &
                                              id_statgeom_node_lon,           id_statgeom_node_lat,       &
               id_crsgeom_node_count,         id_crsgeom_node_coordx,         id_crsgeom_node_coordy,     &
               id_weirgengeom_input_node_count, id_weirgengeom_input_node_coordx, id_weirgengeom_input_node_coordy,&
               id_weirgengeom_node_count,     id_weirgengeom_node_coordx,     id_weirgengeom_node_coordy,    &
               id_orifgengeom_node_count,     id_orifgengeom_node_coordx,     id_orifgengeom_node_coordy,    &
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

    integer                      :: IP, num, ngenstru_, n, nlyrs

    double precision, save       :: curtime_split = 0d0 ! Current time-partition that the file writer has open.
    integer                      :: ntot, k, i, j, ierr, kk, L, Lf, k3, k4, nNodeTot, nNodes, L0, k1, k2, nlinks
    double precision             :: cof0

    integer                      :: strlen_netcdf  ! string length definition for (station) names on history file
    character(len=255)           :: filename
    character(len=25)            :: transpunit
    character(len=1024)          :: statcoordstring, local_statcoordstring
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

    ! NOTE: below new variables based on statistical output modules
    character(len=255)           :: var_name, var_standard_name, var_long_name
    type(t_output_quantity_config), pointer:: config
    integer :: ivar
    integer, pointer :: id_var

    integer :: id_twodim, nc_precision
    integer, save :: id_timebds
    double precision, save :: time_his_prev

    character(len=4)  :: stat_name_postfix
    character(len=11) :: stat_name_filter_postfix
    character(len=16) :: stat_long_name_postfix
    character(len=16) :: stat_cell_methods
    character(len=43) :: stat_cell_methods_filter_postfix

    if (jahiszcor > 0) then
       jawrizc = 1
       jawrizw = 1
    endif
    
    nc_precision = nf90_double
    if ( md_nc_his_precision == SINGLE_PRECISION ) then
       nc_precision = nf90_float
    endif

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

        ierr = unc_create(filename, 0, ihisfile, .false.)
        if (ierr /= nf90_noerr) then
            call mess(LEVEL_WARN, 'Could not create history file.')
        end if

        !
        ! Global metadata
        !
        ierr = unc_meta_add_user_defined(ihisfile)

        ierr = unc_add_time_coverage(ihisfile, ti_hiss, ti_hise, ti_his)

        !
        ! General purpose dimensions
        !
        !if (unc_nounlimited > 0) then ! UNST-4764: His file has never shown good results with NcNoUnlimited option on.
        ierr = nf90_def_dim(ihisfile, 'time', nf90_unlimited, id_timedim)
        ierr = nf90_def_dim(ihisfile, 'two', 2, id_twodim)

        strlen_netcdf = idlen  !< Max string length of Ids.
        ierr = nf90_def_dim(ihisfile, 'name_len', strlen_netcdf, id_strlendim)

        if (kmx > 0) then
           ierr = nf90_def_dim(ihisfile, 'laydim', kmx, id_laydim)
           ierr = nf90_def_dim(ihisfile, 'laydimw', kmx+1, id_laydimw)
        end if

        if (stm_included .and. ISED1 > 0 .and. jahissed > 0) then
           ! New implementation, sedsus fraction is additional dimension
           ierr = nf90_def_dim(ihisfile, 'nSedTot', stmpar%lsedtot, id_sedtotdim)
           ierr = nf90_def_dim(ihisfile, 'nSedSus', stmpar%lsedsus, id_sedsusdim)
        endif

        !
        ! Time
        !
        ierr = nf90_def_var(ihisfile, 'time', nf90_double, id_timedim, id_time)
        ierr = nf90_put_att(ihisfile, id_time,  'units'        , trim(Tudunitstr))
        ierr = nf90_put_att(ihisfile, id_time,  'standard_name', 'time')
        ierr = nf90_put_att(ihisfile, id_time,  'bounds', 'time_bds')

        ierr = nf90_def_var(ihisfile, 'time_bds', nf90_double, (/ id_twodim, id_timedim /), id_timebds)
        ierr = nf90_put_att(ihisfile, id_timebds,  'units'        , trim(Tudunitstr))
        ierr = nf90_put_att(ihisfile, id_timebds,  'standard_name', 'time')
        ierr = nf90_put_att(ihisfile, id_timebds,  'long_name', 'Time interval for each point in time.')

        ! Size of latest timestep
        ierr = unc_def_var_nonspatial(ihisfile, id_timestep, nf90_double, (/ id_timedim /), 'timestep', '',     'latest computational timestep size in each output interval', 's')

        !
        ! Observation stations
        !
        if (numobs+nummovobs > 0) then
            ierr = unc_addcoordmapping(ihisfile, jsferic)
            
            nNodeTot = numobs+nummovobs
            ierr = unc_def_his_structure_static_vars(ihisfile, 'station', 'observation station', 1, numobs+nummovobs, 'point', nNodeTot, id_strlendim, &
                                                     id_statdim, id_stat_id, id_statgeom_node_count, id_statgeom_node_coordx, id_statgeom_node_coordy)
            ierr = nf90_def_var(ihisfile, 'station_name',         nf90_char,   (/ id_strlendim, id_statdim /), id_statname)
            ! ierr = nf90_put_att(ihisfile, id_statname,  'cf_role', 'timeseries_id') ! UNST-6901: only one cf_role var allowed, is now "station_id". Backwards incompatible for some postprocessors?
            ierr = nf90_put_att(ihisfile, id_statname,  'long_name'    , 'observation station name') ! REF

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

            statcoordstring = 'station_x_coordinate station_y_coordinate station_id'
            if (add_latlon) then
               ierr = ncu_clone_vardef(ihisfile, ihisfile, id_statx, 'station_lon', id_statlon, &
                             'longitude', 'original lon-coordinate of station (non-snapped)', 'degrees_east')
               ierr = ncu_clone_vardef(ihisfile, ihisfile, id_staty, 'station_lat', id_statlat, &
                             'latitude', 'original lat-coordinate of station (non-snapped)', 'degrees_north')

               statcoordstring = trim(statcoordstring) // ' station_lon station_lat'
            end if

        end if

        if (ncrs > 0) then
            ierr = unc_def_his_structure_static_vars(ihisfile, 'cross_section', 'observation cross section', 1, ncrs, 'line', nNodesCrs, id_strlendim, &
                                                     id_crsdim, id_crs_id, id_crsgeom_node_count, id_crsgeom_node_coordx, id_crsgeom_node_coordy)

            ierr = nf90_def_var(ihisfile, 'cross_section_name',         nf90_char,   (/ id_strlendim, id_crsdim /), id_crsname)
            ierr = nf90_put_att(ihisfile, id_crsname,  'cf_role', 'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_crsname,  'long_name', 'cross section name'    )
        endif
        

        ! Runup gauges
        ierr = unc_def_his_structure_static_vars(ihisfile, 'runup_gauge', 'runup gauge', 1, nrug, 'none', 0, id_strlendim, &
                                                 id_rugdim, id_rugid) ! No geometry
        if (nrug > 0) then
           ierr = nf90_def_var(ihisfile, 'rug_x_coordinate', nf90_double, (/ id_rugdim, id_timedim /), id_rugx)
           ierr = nf90_def_var(ihisfile, 'rug_y_coordinate', nf90_double, (/ id_rugdim, id_timedim /), id_rugy)

           ierr = unc_addcoordatts(ihisfile, id_rugx, id_rugy, jsferic)
           ierr = nf90_put_att(ihisfile, id_rugx, 'long_name', 'time-varying x-coordinate of shoreline position')
           ierr = nf90_put_att(ihisfile, id_rugy, 'long_name', 'time-varying y-coordinate of shoreline position')

           ierr = nf90_def_var(ihisfile, 'rug_name', nf90_char,   (/ id_strlendim, id_rugdim /), id_rugname)
           ierr = nf90_put_att(ihisfile, id_rugname,  'cf_role', 'timeseries_id')
           ierr = nf90_put_att(ihisfile, id_rugname,  'long_name'    , 'runup gauge name') ! REF

           ierr = nf90_def_var(ihisfile, 'runup_height', nf90_double, (/ id_rugdim, id_timedim /), id_varruh)
           ierr = nf90_put_att(ihisfile, id_varruh, 'standard_name', 'runup_height')
           ierr = nf90_put_att(ihisfile, id_varruh, 'long_name', 'runup height')
           ierr = nf90_put_att(ihisfile, id_varruh, 'units', 'm')
           ierr = nf90_put_att(ihisfile, id_varruh, 'coordinates', 'rug_x_coordinate rug_y_coordinate rug_name')
           ierr = nf90_put_att(ihisfile, id_varruh, '_FillValue', dmiss)
        endif

        ! Source-sinks
        if (jahissourcesink > 0 .and. numsrc > 0) then
           ! Define geometry related variables
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
        end if
        
        ierr = unc_def_his_structure_static_vars(ihisfile, 'source_sink', 'source and sink', jahissourcesink, numsrc, 'line', nNodeTot, id_strlendim, &
                                                 id_srcdim, id_srcname, id_srcgeom_node_count, id_srcgeom_node_coordx, id_srcgeom_node_coordy)
        if (jahissourcesink > 0 .and. numsrc > 0) then
           ierr = nf90_def_var(ihisfile, 'source_sink_x_coordinate', nf90_double, (/ id_srcdim, id_srcptsdim  /), id_srcx)
           ierr = nf90_def_var(ihisfile, 'source_sink_y_coordinate', nf90_double, (/ id_srcdim, id_srcptsdim /), id_srcy)
           ierr = unc_addcoordatts(ihisfile, id_srcx, id_srcy, jsferic)
           ierr = nf90_put_att(ihisfile, id_srcx, '_FillValue', dmiss)
           ierr = nf90_put_att(ihisfile, id_srcy, '_FillValue', dmiss)
        end if

        if (timon) call timstrt ( "unc_write_his DEF structures", handle_extra(60))

        ! General structure (either via old .ext file or new structures.ini file)
        if (jaoldstr == 1) then
           ngenstru_ = ncgensg
        else
           ngenstru_ = ngenstru
        end if
        if (jahiscgen > 0 .and. ngenstru_ > 0) then
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
        end if
        ierr = unc_def_his_structure_static_vars(ihisfile, 'general_structure', 'general structure', jahiscgen, ngenstru_, 'line', nNodeTot, id_strlendim, &
                                                 id_genstrudim, id_genstru_id, id_genstrugeom_node_count, id_genstrugeom_node_coordx, id_genstrugeom_node_coordy)

        ! Pump
        if(jahispump > 0 .and. npumpsg > 0) then
            ierr = nf90_def_dim(ihisfile, 'pumps', npumpsg, id_pumpdim)
            ierr = nf90_def_var(ihisfile, 'pump_id',  nf90_char,   (/ id_strlendim, id_pumpdim /), id_pump_id)
            ierr = nf90_put_att(ihisfile, id_pump_id,  'cf_role',   'timeseries_id')
            ierr = nf90_put_att(ihisfile, id_pump_id,  'long_name', 'Id of pump'    )

            ! Define geometry related variables
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
        end if
        ierr = unc_def_his_structure_static_vars(ihisfile, 'pump', 'pump', jahispump, npumpsg, 'line', nNodeTot, id_strlendim, &
                                                 id_pumpdim, id_pump_id, id_pumpgeom_node_count, id_pumpgeom_node_coordx, id_pumpgeom_node_coordy)
        ! TODO: UNST-6904: check x/ymid:
            !ierr = nf90_def_var(ihisfile, 'pump_xmid', nf90_double, (/ id_pumpdim /), id_pump_xmid)
            !ierr = nf90_def_var(ihisfile, 'pump_ymid', nf90_double, (/ id_pumpdim /), id_pump_ymid)
            !ierr = unc_addcoordatts(ihisfile, id_pump_xmid, id_pump_ymid, jsferic)
            !ierr = nf90_put_att(ihisfile, id_pump_xmid, 'long_name', 'x-coordinate of representative mid point of pump location (snapped polyline)')
            !ierr = nf90_put_att(ihisfile, id_pump_ymid, 'long_name', 'y-coordinate of representative mid point of pump location (snapped polyline)')

        ! Gate (Old .ext file, QUANTITY='gateloweredgelevel')
        ierr = unc_def_his_structure_static_vars(ihisfile, 'gate', 'gate', jahisgate, ngatesg, 'none', 0, id_strlendim, &
                                                 id_gatedim, id_gate_id)

        if(jahisgate > 0 .and. ngategen > 0 ) then
            ! Define geometry related variables
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
        end if
        ierr = unc_def_his_structure_static_vars(ihisfile, 'gategen', 'gate', jahisgate, ngategen, 'line', nNodeTot, id_strlendim, &
                                                 id_gategendim, id_gategen_id, id_gategengeom_node_count, id_gategengeom_node_coordx, id_gategengeom_node_coordy)

        ! Controllable dam (Old .ext file QUANTITY='damlevel')
        ierr = unc_def_his_structure_static_vars(ihisfile, 'cdam', 'controllable dam', jahiscdam, ncdamsg, 'none', 0, id_strlendim, &
                                                 id_cdamdim, id_cdam_id)

        ! Weir
        if(jahisweir > 0 .and. nweirgen > 0 ) then
            ! Define geometry related variables
            nNodeTot = 0
            if (network%sts%numWeirs > 0) then ! new weir
               nNodeTot = nNodesWeir
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
        end if
        ierr = unc_def_his_structure_static_vars(ihisfile, 'weirgen', 'weir', jahisweir, nweirgen, 'line', nNodeTot, id_strlendim, &
                                                 id_weirgendim, id_weirgen_id, id_weirgengeom_node_count, id_weirgengeom_node_coordx, id_weirgengeom_node_coordy)

        ! Orifice
        ierr = unc_def_his_structure_static_vars(ihisfile, 'orifice', 'orifice', jahisorif, network%sts%numOrifices, 'line', nNodesOrif, id_strlendim, &
                                                 id_orifgendim, id_orifgen_id, id_orifgengeom_node_count, id_orifgengeom_node_coordx, id_orifgengeom_node_coordy)

        ! Bridge
        ierr = unc_def_his_structure_static_vars(ihisfile, 'bridge', 'bridge', jahisbridge, network%sts%numBridges, 'line', nNodesBridge, id_strlendim, &
                                                 id_bridgedim, id_bridge_id, id_bridgegeom_node_count, id_bridgegeom_node_coordx, id_bridgegeom_node_coordy)

        ! Culvert
        ierr = unc_def_his_structure_static_vars(ihisfile, 'culvert', 'culvert', jahisculv, network%sts%numculverts, 'line', nNodesCulv, id_strlendim, &
                                                 id_culvertdim, id_culvert_id, id_culvertgeom_node_count, id_culvertgeom_node_coordx, id_culvertgeom_node_coordy)

        ! Dambreak
        ierr = unc_def_his_structure_static_vars(ihisfile, 'dambreak', 'dambreak', jahisdambreak, ndambreaksg, 'none', 0, id_strlendim, &
                                                 id_dambreakdim, id_dambreak_id)

        ! Universal weir
        ierr = unc_def_his_structure_static_vars(ihisfile, 'uniweir', 'universal weir', jahisuniweir, network%sts%numuniweirs, 'line', nNodesUniweir, id_strlendim, &
                                                 id_uniweirdim, id_uniweir_id, id_uniweirgeom_node_count, id_uniweirgeom_node_coordx, id_uniweirgeom_node_coordy)


        ! compound structure
        ierr = unc_def_his_structure_static_vars(ihisfile, 'cmpstru', 'compound structure', jahiscmpstru, network%cmps%count, 'none', 0, id_strlendim, &
                                                 id_cmpstrudim, id_cmpstru_id)

        ! Long culvert
        ierr = unc_def_his_structure_static_vars(ihisfile, 'longculvert', 'long culvert', jahislongculv, nlongculverts, 'line', nNodesLongCulv, id_strlendim, &
                                                 id_longculvertdim, id_longculvert_id, id_longculvertgeom_node_count, id_longculvertgeom_node_coordx, id_longculvertgeom_node_coordy)

        ! Lateral
        ierr = unc_def_his_structure_static_vars(ihisfile, 'lateral', 'lateral', jahislateral, numlatsg, 'point', nNodesLat, id_strlendim, &
                                                 id_latdim, id_lat_id, id_latgeom_node_count, id_latgeom_node_coordx, id_latgeom_node_coordy)
        ! TODO: UNST-7239: remove separate average IDX?
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

         do ivar = 1,out_variable_set_his%count
            config => out_variable_set_his%statout(ivar)%output_config
            id_var => out_variable_set_his%statout(ivar)%id_var
               
            if (config%location_specifier /= UNC_LOC_STATION &
                  .and. config%location_specifier /= UNC_LOC_OBSCRS &
                  .and. config%location_specifier /= UNC_LOC_GLOBAL &
                  .and. config%location_specifier /= UNC_LOC_SOSI &
                  .and. config%location_specifier /= UNC_LOC_RUG &
                  .and. config%location_specifier /= UNC_LOC_GENSTRU &
                  .and. config%location_specifier /= UNC_LOC_DAM &
                  .and. config%location_specifier /= UNC_LOC_PUMP &
                  .and. config%location_specifier /= UNC_LOC_GATE &
                  .and. config%location_specifier /= UNC_LOC_WEIRGEN &
                  .and. config%location_specifier /= UNC_LOC_ORIFICE &
                  .and. config%location_specifier /= UNC_LOC_BRIDGE &
                  .and. config%location_specifier /= UNC_LOC_CULVERT &
                  .and. config%location_specifier /= UNC_LOC_DAMBREAK &
                  .and. config%location_specifier /= UNC_LOC_UNIWEIR &
                  .and. config%location_specifier /= UNC_LOC_CMPSTRU &
                  .and. config%location_specifier /= UNC_LOC_LONGCULVERT &
                  .and. config%location_specifier /= UNC_LOC_LATERAL &
            ) then
               call mess(LEVEL_DEBUG, 'unc_write_his: skipping item '//trim(config%name)//', because it''s not on a known output location.')
               cycle
            end if

            select case(out_variable_set_his%statout(ivar)%operation_type)
            case(SO_CURRENT)
               stat_name_postfix      = ''
               stat_long_name_postfix = ''
               stat_cell_methods      = 'time: point'
            case(SO_AVERAGE)
               stat_name_postfix      = '_avg'
               stat_long_name_postfix = ' (average)'
               stat_cell_methods      = 'time: mean'
            case(SO_MAX)
               stat_name_postfix      = '_max'
               stat_long_name_postfix = ' (maximum)'
               stat_cell_methods      = 'time: maximum'
            case(SO_MIN)
               stat_name_postfix      = '_min'
               stat_long_name_postfix = ' (minimum)'
               stat_cell_methods      = 'time: minimum'
            end select
            stat_name_filter_postfix = ''
            stat_cell_methods_filter_postfix = ''
            if (out_variable_set_his%statout(ivar)%moving_average_window > 1) then
               write(stat_name_filter_postfix, '(a,i0)') '_filter', out_variable_set_his%statout(ivar)%moving_average_window
               write(stat_cell_methods_filter_postfix, '(a,i0,a)') ' (moving average filter using ', out_variable_set_his%statout(ivar)%moving_average_window, ' samples)'
            end if

            var_name          = trim(config%name) // trim(stat_name_postfix) // trim(stat_name_filter_postfix)
            var_standard_name = config%standard_name ! Intentionally no pre/postfix for standard_name
            if (len_trim(config%long_name) > 0) then
               var_long_name = trim(config%long_name) // trim(stat_long_name_postfix)
            else
               var_long_name = ''
            end if

            select case(config%location_specifier)
            case (UNC_LOC_SOSI)
               call definencvar(ihisfile, id_var, config%nc_type, (/ id_srcdim,         id_timedim /), var_name, var_long_name, config%unit, 'source_sink_id', fillVal=dmiss, attset=config%additional_attributes)
            case (UNC_LOC_RUG)
               call definencvar(ihisfile, id_var, config%nc_type, (/ id_rugdim,         id_timedim /), var_name, var_long_name, config%unit, 'rug_x_coordinate rug_y_coordinate rug_id', fillVal=dmiss, attset=config%additional_attributes)
            case (UNC_LOC_GENSTRU)
               call definencvar(ihisfile, id_var, config%nc_type, (/ id_genstrudim,     id_timedim /), var_name, var_long_name, config%unit, 'general_structure_id', fillVal=dmiss, attset=config%additional_attributes)
            case (UNC_LOC_DAM)
               call definencvar(ihisfile, id_var, config%nc_type, (/ id_cdamdim,        id_timedim /), var_name, var_long_name, config%unit, 'cdam_id', fillVal=dmiss, attset=config%additional_attributes)
            case (UNC_LOC_PUMP)
               call definencvar(ihisfile, id_var, config%nc_type, (/ id_pumpdim,        id_timedim /), var_name, var_long_name, config%unit, 'pump_id', fillVal=dmiss, attset=config%additional_attributes)
            case (UNC_LOC_GATE)
               call definencvar(ihisfile, id_var, config%nc_type, (/ id_gategendim,     id_timedim /), var_name, var_long_name, config%unit, 'gategen_id', fillVal=dmiss, attset=config%additional_attributes)
            case (UNC_LOC_WEIRGEN)
               call definencvar(ihisfile, id_var, config%nc_type, (/ id_weirgendim,     id_timedim /), var_name, var_long_name, config%unit, 'weirgen_id', fillVal=dmiss, attset=config%additional_attributes)
            case (UNC_LOC_ORIFICE)
               call definencvar(ihisfile, id_var, config%nc_type, (/ id_orifgendim,     id_timedim /), var_name, var_long_name, config%unit, 'orif_id', fillVal=dmiss, attset=config%additional_attributes)
            case (UNC_LOC_BRIDGE)
               call definencvar(ihisfile, id_var, config%nc_type, (/ id_bridgedim,      id_timedim /), var_name, var_long_name, config%unit, 'bridge_id', fillVal=dmiss, attset=config%additional_attributes)
            case (UNC_LOC_CULVERT)
               call definencvar(ihisfile, id_var, config%nc_type, (/ id_culvertdim,     id_timedim /), var_name, var_long_name, config%unit, 'culvert_id', fillVal=dmiss, attset=config%additional_attributes)
            case (UNC_LOC_DAMBREAK)
               call definencvar(ihisfile, id_var, config%nc_type, (/ id_dambreakdim,    id_timedim /), var_name, var_long_name, config%unit, 'dambreak_id', fillVal=dmiss, attset=config%additional_attributes)
            case (UNC_LOC_UNIWEIR)
               call definencvar(ihisfile, id_var, config%nc_type, (/ id_uniweirdim,     id_timedim /), var_name, var_long_name, config%unit, 'uniweir_id', fillVal=dmiss, attset=config%additional_attributes)
            case (UNC_LOC_CMPSTRU)
               call definencvar(ihisfile, id_var, config%nc_type, (/ id_cmpstrudim,     id_timedim /), var_name, var_long_name, config%unit, 'cmpstru_id', fillVal=dmiss, attset=config%additional_attributes)
            case (UNC_LOC_LONGCULVERT)
               call definencvar(ihisfile, id_var, config%nc_type, (/ id_longculvertdim, id_timedim /), var_name, var_long_name, config%unit, 'longculvert_id', fillVal=dmiss, attset=config%additional_attributes)
            case (UNC_LOC_LATERAL)
               call definencvar(ihisfile, id_var, config%nc_type, (/ id_latdim,         id_timedim /), var_name, var_long_name, config%unit, 'lat_id', fillVal=dmiss, attset=config%additional_attributes)
            case (UNC_LOC_STATION)
               if (allocated(config%nc_dim_ids)) then
                  if (config%nc_dim_ids%laydim) then
                     local_statcoordstring = trim(statcoordstring) // ' zcoordinate_c'
                  else if (config%nc_dim_ids%laydim_interface_center) then
                     local_statcoordstring = trim(statcoordstring) // ' zcoordinate_w'
                  else if (config%nc_dim_ids%laydim_interface_edge) then
                     local_statcoordstring = trim(statcoordstring) // ' zcoordinate_wu'
                  else
                     local_statcoordstring = statcoordstring
                  end if
                  call definencvar(ihisfile, id_var, config%nc_type, build_nc_dimension_id_list(config%nc_dim_ids), var_name, var_long_name, &
                                   config%unit, local_statcoordstring, fillVal=dmiss, add_gridmapping = .true., attset=config%additional_attributes)
               else
                  call err('Programming error: UNC_LOC_STATION variable '//trim(config%name)//' does not have nc_dim_ids set.')
               end if
            case (UNC_LOC_OBSCRS)
               call definencvar(ihisfile, id_var, config%nc_type, (/ id_crsdim, id_timedim /), var_name, var_long_name, config%unit, 'cross_section_name', fillVal=dmiss, attset=config%additional_attributes)
            case (UNC_LOC_GLOBAL)
               if (timon) call timstrt ( "unc_write_his DEF bal", handle_extra(59))
               call definencvar(ihisfile, id_var, config%nc_type, (/ id_timedim /), var_name, var_long_name, config%unit, "", fillVal=dmiss, attset=config%additional_attributes)
               if (timon) call timstop (handle_extra(59))
            end select

            if (len_trim(var_standard_name) > 0) then
               ierr = nf90_put_att(ihisfile, id_var, 'standard_name', trim(var_standard_name))
            end if
            if (len_trim(stat_cell_methods) > 0) then
               ierr = nf90_put_att(ihisfile, id_var, 'cell_methods', trim(stat_cell_methods) // trim(stat_cell_methods_filter_postfix))
            end if
         end do

        ierr = nf90_enddef(ihisfile)
        if (timon) call timstop (handle_extra(61))

        if (timon) call timstrt ('unc_write_his timeindep data', handle_extra(63))
        
        ! Observation stations
        do i=1,numobs+nummovobs
!           ierr = nf90_put_var(ihisfile, id_statx,    xobs(i),         (/ i /))
!           ierr = nf90_put_var(ihisfile, id_staty,    yobs(i),         (/ i /))
           ierr = nf90_put_var(ihisfile, id_stat_id, trimexact(namobs(i), strlen_netcdf), (/ 1, i /)) ! Extra for OpenDA-wrapper
           ierr = nf90_put_var(ihisfile, id_statname, trimexact(namobs(i), strlen_netcdf), (/ 1, i /))
        end do

        ! Observation cross sections
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

        ! Run-up gauges
        if (nrug>0) then
            do i=1,nrug
                ierr = nf90_put_var(ihisfile, id_rugname,  trimexact(rug(i)%name, strlen_netcdf), (/ 1, i /))
                ierr = nf90_put_var(ihisfile, id_rugid,    trimexact(rug(i)%name, strlen_netcdf), (/ 1, i /))
            end do
        endif

        ! Source-sinks
        if (jahissourcesink > 0 .and. numsrc > 0) then
           do i = 1, numsrc
              ierr = nf90_put_var(ihisfile, id_srcname, trimexact(srcname(i), strlen_netcdf), (/ 1, i/) )
              ierr = nf90_put_var(ihisfile, id_qsrccur, qstss((numconst+1)*(i-1)+1), (/ i, it_his /)) ! Intentionally here for the first output time
           enddo
           ierr = nf90_put_var(ihisfile, id_srcx, xsrc)
           ierr = nf90_put_var(ihisfile, id_srcy, ysrc)
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

        ! General structures
        if (jahiscgen > 0 .and. ngenstru_ > 0) then
            do i=1,ngenstru_
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

        ! Lateral discharges
        if (jahislateral > 0 .and. numlatsg > 0) then
           do i = 1, numlatsg
              ierr = nf90_put_var(ihisfile, id_lat_id,  trimexact(lat_ids(i), strlen_netcdf), (/ 1, i /))
           end do
           ierr = nf90_put_var(ihisfile, id_latgeom_node_coordx, geomXLat(1:nNodesLat), start = (/ 1 /), count = (/ nlatnd /))
           ierr = nf90_put_var(ihisfile, id_latgeom_node_coordy, geomYLat(1:nNodesLat), start = (/ 1 /), count = (/ nlatnd /))
           ierr = nf90_put_var(ihisfile, id_latgeom_node_count,  nodeCountLat)
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
               ierr = nf90_put_var(ihisfile, id_gate_id,  trimexact(gate_ids(i), strlen_netcdf),      (/ 1, i /))
            end do
        end if
        if (jahisgate > 0 .and. ngategen > 0) then
           do i=1,ngategen
              igen = gate2cgen(i)
              ierr = nf90_put_var(ihisfile, id_gategen_id,  trimexact(cgen_ids(igen), strlen_netcdf),      (/ 1, i /))
           end do
        end if
        if (jahiscdam > 0 .and. ncdamsg > 0) then
            do i=1,ncdamsg
               ierr = nf90_put_var(ihisfile, id_cdam_id,  trimexact(cdam_ids(i), strlen_netcdf),      (/ 1, i /))
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
    if (it_his == 0) then
       time_his_prev = tim
    end if
    time_his = tim
    it_his   = it_his + 1

    if (timon) call timstrt ('unc_write_his time data', handle_extra(64))

    ierr = nf90_put_var(ihisfile, id_time, time_his, (/ it_his /))
    ierr = nf90_put_var(ihisfile, id_timebds, (/ time_his_prev, time_his /), (/ 1, it_his /))
    time_his_prev = time_his
    ierr = nf90_put_var(ihisfile, id_timestep, dts, (/ it_his /))
    if (timon) call timstop ( handle_extra(64))

!   Observation points (fixed+moving)

    ntot = numobs + nummovobs
    if (it_his == 1) then !Fill average discharge with different array on first timestep
       qsrc(i) = qstss((numconst+1)*(i-1)+1)
    endif
    
   do ivar = 1,out_variable_set_his%count
      config => out_variable_set_his%statout(ivar)%output_config
      id_var => out_variable_set_his%statout(ivar)%id_var
               
      if (config%location_specifier /= UNC_LOC_STATION &
            .and. config%location_specifier /= UNC_LOC_OBSCRS &
            .and. config%location_specifier /= UNC_LOC_GLOBAL &
            .and. config%location_specifier /= UNC_LOC_SOSI &
            .and. config%location_specifier /= UNC_LOC_RUG &
            .and. config%location_specifier /= UNC_LOC_GENSTRU &
            .and. config%location_specifier /= UNC_LOC_DAM &
            .and. config%location_specifier /= UNC_LOC_PUMP &
            .and. config%location_specifier /= UNC_LOC_GATE &
            .and. config%location_specifier /= UNC_LOC_WEIRGEN &
            .and. config%location_specifier /= UNC_LOC_ORIFICE &
            .and. config%location_specifier /= UNC_LOC_BRIDGE &
            .and. config%location_specifier /= UNC_LOC_CULVERT &
            .and. config%location_specifier /= UNC_LOC_DAMBREAK &
            .and. config%location_specifier /= UNC_LOC_UNIWEIR &
            .and. config%location_specifier /= UNC_LOC_CMPSTRU &
            .and. config%location_specifier /= UNC_LOC_LONGCULVERT &
            .and. config%location_specifier /= UNC_LOC_LATERAL &
            ) then
         call mess(LEVEL_DEBUG, 'unc_write_his: skipping item '//trim(config%name)//', because it''s not on a known statistical output location.')
         cycle
      end if

      select case(config%location_specifier)
      case ( UNC_LOC_OBSCRS, &
         UNC_LOC_SOSI, &
         UNC_LOC_RUG, &
         UNC_LOC_GENSTRU, &
         UNC_LOC_DAM, &
         UNC_LOC_PUMP, &
         UNC_LOC_GATE, &
         UNC_LOC_WEIRGEN, &
         UNC_LOC_ORIFICE, &
         UNC_LOC_BRIDGE, &
         UNC_LOC_CULVERT, &
         UNC_LOC_DAMBREAK, &
         UNC_LOC_UNIWEIR, &
         UNC_LOC_CMPSTRU, &
         UNC_LOC_LONGCULVERT, &
         UNC_LOC_LATERAL &
         )
         ierr = nf90_put_var(ihisfile, id_var, out_variable_set_his%statout(ivar)%stat_output, start = (/ 1, it_his /))
      case (UNC_LOC_STATION)
         ierr = nf90_put_var(ihisfile, id_var, out_variable_set_his%statout(ivar)%stat_output, count = build_nc_dimension_id_count_array(config%nc_dim_ids), start = build_nc_dimension_id_start_array(config%nc_dim_ids))
      case (UNC_LOC_GLOBAL)
         if (timon) call timstrt('unc_write_his IDX data', handle_extra(67))
         ierr = nf90_put_var(ihisfile, id_var, out_variable_set_his%statout(ivar)%stat_output,  start=(/ it_his /))
         if (timon) call timstop(handle_extra(67))
      end select
   end do
   
   
    valobsT(1:ntot, 1:IPNT_NUM) = transpose(valobs)
    if (ntot > 0 .and. .false.) then
       if ( jahiswatlev > 0 ) then
         ivar = 1 ! Remove after testing
         config => out_variable_set_his%statout(ivar)%output_config
         id_var => out_variable_set_his%statout(ivar)%id_var
         ierr = nf90_put_var(ihisfile, id_var,   out_variable_set_his%statout(ivar)%stat_output,    start = (/ 1, it_his /), count = (/ ntot, 1 /))
       end if

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

    if (ntot > 0 .and. .false.) then
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

!    if (timon) call timstrt('unc_write_his obs/crs data 2', handle_extra(57))

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
          ierr = nf90_put_var(ihisfile, id_varrhop , valobsT(:,IPNT_RHOP +kk-1), start = (/ kk, 1, it_his /), count = (/ 1, ntot, 1 /))
          if (idensform > 10) then
             ierr = nf90_put_var(ihisfile, id_varrho  , valobsT(:,IPNT_RHO +kk-1) , start = (/ kk, 1, it_his /), count = (/ 1, ntot, 1 /))
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
          if( (jasal > 0 .or. jatem > 0 .or. jased > 0 )  .and. jahisrho > 0) then
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
    endif !(ntot > 0 .and. .false.)


    if (timon) call timstrt('unc_write_his RUG', handle_extra(65))

    ! runup gauges
    if (nrug>0) then
       call realloc(geom_x,   nrug, fill=dmiss)
       call realloc(geom_y,   nrug, fill=dmiss)
       do i=1,nrug
          geom_x(i)   = rug(i)%maxx
          geom_y(i)   = rug(i)%maxy
       end do
       ierr = nf90_put_var(ihisfile, id_rugx,   geom_x(:),   start = (/ 1, it_his /), count = (/ nrug, 1 /))
       ierr = nf90_put_var(ihisfile, id_rugy,   geom_y(:),   start = (/ 1, it_his /), count = (/ nrug, 1 /))
       ! ierr = nf90_put_var(ihisfile, id_varruh, toutput1(:), start = (/ 1, it_his /), count = (/ nrug, 1 /)) ! Via stat output
    endif

    if (timon) call timstop(handle_extra(65))

    if (timon) call timstrt ( "unc_write_his str write", handle_extra(62))
    ! TODO: UNST-7239: ensure that stat output items have correct value also at it_his==1
      !if (tim > tstart_user) then
      !   ierr = nf90_put_var(ihisfile, id_qsrccur, qsrc, (/ 1, it_his /))
      !endif

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
                        call get_geom_coordinates_of_generalstructure_oldext(i, nNodes, geom_x, geom_y)
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
            ierr = nf90_put_var(ihisfile, id_orifgengeom_node_coordx, geomXOrif,     start = (/ 1 /), count = (/ nNodesOrif /))
            ierr = nf90_put_var(ihisfile, id_orifgengeom_node_coordy, geomYOrif,     start = (/ 1 /), count = (/ nNodesOrif /))
            ierr = nf90_put_var(ihisfile, id_orifgengeom_node_count,  nodeCountOrif, start = (/ 1 /), count = (/ network%sts%numOrifices /))
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
                  call get_geom_coordinates_of_generalstructure_oldext(i, nNodes, geom_x, geom_y)
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

               ierr = nf90_put_var(ihisfile, id_weirgengeom_node_coordx, geomXWeir,     start = (/ 1 /), count = (/ nNodesWeir /))
               ierr = nf90_put_var(ihisfile, id_weirgengeom_node_coordy, geomYWeir,     start = (/ 1 /), count = (/ nNodesWeir /))
               ierr = nf90_put_var(ihisfile, id_weirgengeom_node_count,  nodeCountWeir, start = (/ 1 /), count = (/ network%sts%numWeirs /))
               
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
                     call get_geom_coordinates_of_generalstructure_oldext(i, nNodes, geom_x, geom_y)
                     ierr = nf90_put_var(ihisfile, id_weirgengeom_node_coordx, geom_x(1:nNodes), start = (/ j /), count = (/ nNodes /))
                     ierr = nf90_put_var(ihisfile, id_weirgengeom_node_coordy, geom_y(1:nNodes), start = (/ j /), count = (/ nNodes /))
                     j = j + nNodes
                  end if
               end do
               ierr = nf90_put_var(ihisfile, id_weirgengeom_node_count, node_count, start = (/ 1 /), count = (/ nweirgen /))
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
      if (.false.) then
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
    endif ! (.false.)


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
   !> Define the static variables for a single structure type.
   !! This includes: NetCDF dimension ids, character Id variable and simple geometry container variables.
   !! Note: the writing ('putting') of data is done by another subroutine: unc_put_his_structure_static_vars.
   function unc_def_his_structure_static_vars(ncid, prefix, name, output_enabled, count, geom_type, ngeom_node, id_strlendim, &
                                             id_strdim, id_strid, id_geom_node_count, id_geom_coordx, id_geom_coordy) result(ierr)
   use string_module, only: strcmpi
      integer,           intent(in   ) :: ncid       !< NetCDF id of already open dataset
      character(len=*),  intent(in   ) :: prefix     !< Base name of this structure type, e.g., 'uniweir'
      character(len=*),  intent(in   ) :: name       !< Human readable name of this structure type, e.g., 'universal weir'
      integer,           intent(in   ) :: output_enabled !< Whether or not (1/0) this structure's output must be written.
      integer,           intent(in   ) :: count      !< Number of structures for this structure_type
      character(len=*),  intent(in   ) :: geom_type  !< Geometry type, one of: 'point', 'line', 'polygon' (or 'none')
      integer,           intent(in   ) :: ngeom_node !< Total number of geometry nodes for this structure_type
      integer,           intent(in   ) :: id_strlendim !< Already created NetCDF dimension id for max string length of the character Ids.
      integer,           intent(  out) :: id_strdim  !< NetCDF dimension id created for this structure type
      integer,           intent(  out) :: id_strid   !< NetCDF variable id created for the character Ids of the structures of this type
      integer, optional, intent(  out) :: id_geom_node_count !< NetCDF variable id created for the node count of the structures of this type
      integer, optional, intent(  out) :: id_geom_coordx     !< NetCDF variable id created for the node x coordinates for all structures of this type
      integer, optional, intent(  out) :: id_geom_coordy     !< NetCDF variable id created for the node y coordinates for all structures of this type

      integer                         :: ierr       !< Result status (NF90_NOERR if successful)

      ierr = NF90_NOERR

      if (output_enabled > 0 .and. count > 0) then
         ierr = nf90_def_dim(ihisfile, prefix, count, id_strdim)
         ierr = nf90_def_var(ihisfile, prefix//'_id',  nf90_char,   (/ id_strlendim, id_strdim /), id_strid)
         ierr = nf90_put_att(ihisfile, id_strid,  'cf_role',   'timeseries_id')
         ierr = nf90_put_att(ihisfile, id_strid,  'long_name', 'Id of '//trim(name))
     
         if (.not. strcmpi(geom_type, 'none') .and. len_trim(geom_type) > 0) then
            ! Define geometry related variables
            ierr = sgeom_def_geometry_variables(ihisfile, prefix//'_geom', name, geom_type, ngeom_node, id_strdim, &
                                                id_geom_node_count, id_geom_coordx, id_geom_coordy)
         end if

      end if

   end function unc_def_his_structure_static_vars

!> Convert t_nc_dim_ids to integer array of NetCDF dimension ids
function build_nc_dimension_id_list(nc_dim_ids) result(res)
   type(t_nc_dim_ids), intent(in) :: nc_dim_ids !< The active NetCDF dimensions for this variable
   integer, allocatable           :: res(:)     !< Array of NetCDF dimension ids

   res = pack([id_laydim, id_laydimw, id_nlyrdim, id_statdim, id_sedsusdim, id_sedtotdim, id_timedim], &
              make_mask_from_dim_ids(nc_dim_ids))
end function build_nc_dimension_id_list

!> Return array of NetCDF dimension start indices corresponding to NetCDF dimensions
function build_nc_dimension_id_start_array(nc_dim_ids) result(starts)
   type(t_nc_dim_ids), intent(in) :: nc_dim_ids !< The active NetCDF dimensions for this variable
   integer, allocatable           :: starts(:)  !< Array of start indices for each NetCDF dimension
   
   starts = pack([1, 1, 1, 1, 1, 1, it_his], &
              make_mask_from_dim_ids(nc_dim_ids))
end function build_nc_dimension_id_start_array

!> Return array of NetCDF dimension counts corresponding to NetCDF dimensions
function build_nc_dimension_id_count_array(nc_dim_ids) result(counts)
   type(t_nc_dim_ids), intent(in) :: nc_dim_ids !< The active NetCDF dimensions for this variable
   integer, allocatable           :: counts(:)  !< NetCDF dimension counts
   
   counts = pack([get_dimid_len(id_laydim),get_dimid_len(id_laydimw), get_dimid_len(id_nlyrdim), get_dimid_len(id_statdim), get_dimid_len(id_sedsusdim), get_dimid_len(id_sedtotdim), 1], &
              make_mask_from_dim_ids(nc_dim_ids))
end function build_nc_dimension_id_count_array

!> Build mask of which dimensions to include in netcdf variable, based on nc_dim_ids
pure function make_mask_from_dim_ids(nc_dim_ids) result(mask)
   type(t_nc_dim_ids), intent(in) :: nc_dim_ids  !< The active NetCDF dimensions for this variable
   logical                        :: mask(7)     !< The same but as a 1-D array of logicals
   
   mask = [nc_dim_ids%laydim, &
           nc_dim_ids%laydim_interface_center .or. nc_dim_ids%laydim_interface_edge, &
           nc_dim_ids%nlyrdim, &
           nc_dim_ids%statdim, &
           nc_dim_ids%sedsusdim, &
           nc_dim_ids%sedtotdim, &
           nc_dim_ids%timedim]
end function make_mask_from_dim_ids

!> Gets dimension length from NetCDF dimension id
integer function get_dimid_len(id)
   integer, intent(in) :: id !< NetCDF id obtained from nf90_def_dim

   ierr =  nf90_inquire_dimension(ihisfile, id, len = get_dimid_len)
end function get_dimid_len

end subroutine unc_write_his

    
