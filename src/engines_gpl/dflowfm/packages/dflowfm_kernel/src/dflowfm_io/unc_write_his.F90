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
module m_unc_write_his
   use MessageHandling, only: idLen
   implicit none
   private

   public :: unc_write_his

   integer, parameter :: strlen_netcdf = idLen ! string length definition for (station) names on history file

   !> local ids of netcdf variables and dims
   integer :: id_timedim, id_num_timesteps, id_comp_time, &
              id_laydim, id_laydimw, id_sedtotdim, id_sedsusdim, id_frac_name, &
              id_statdim, id_strlendim, id_crsdim, &
              id_statx, id_staty, id_stat_id, id_statname, id_time, id_timestep, &
              id_statlon, id_statlat, id_crs_id, id_varb, id_nlyrdim, &
              id_zcs, id_zws, id_zwu, id_checkmon, &
              id_pumpdim, id_pump_id, &
              id_gatedim, id_gate_id, &
              id_cdamdim, id_cdam_id, &
              id_weirgendim, id_weirgen_id, &
              id_gategendim, id_gategen_id, &
              id_genstrudim, id_genstru_id, &
              id_orifgendim, id_orifgen_id, &
              id_bridgedim, id_bridge_id, &
              id_culvertdim, id_culvert_id, &
              id_srcdim, id_srcname, id_srcx, id_srcy, id_srcptsdim, &
              id_dredlinkdim, id_dreddim, id_dumpdim, id_dred_name, id_dump_name, &
              id_dambreakdim, id_dambreak_id, &
              id_uniweirdim, id_uniweir_id, &
              id_cmpstrudim, id_cmpstru_id, &
              id_longculvertdim, id_longculvert_id, &
              id_latdim, id_lat_id, &
              id_rugdim, id_rugname

   ! ids for geometry variables, only use them once at the first time of history output
   integer :: &
      id_statgeom_node_count, id_statgeom_node_coordx, id_statgeom_node_coordy, id_statgeom_node_lon, id_statgeom_node_lat, &
      id_latgeom_node_count, id_latgeom_node_coordx, id_latgeom_node_coordy, &
      id_weirgengeom_node_count, id_weirgengeom_node_coordx, id_weirgengeom_node_coordy, id_weirgen_xmid, id_weirgen_ymid, &
      id_crsgeom_node_count, id_crsgeom_node_coordx, id_crsgeom_node_coordy, id_crs_xmid, id_crs_ymid, &
      id_orifgengeom_node_count, id_orifgengeom_node_coordx, id_orifgengeom_node_coordy, &
      id_genstrugeom_node_count, id_genstrugeom_node_coordx, id_genstrugeom_node_coordy, id_genstru_xmid, id_genstru_ymid, &
      id_uniweirgeom_node_count, id_uniweirgeom_node_coordx, id_uniweirgeom_node_coordy, id_uniweir_xmid, id_uniweir_ymid, &
      id_culvertgeom_node_count, id_culvertgeom_node_coordx, id_culvertgeom_node_coordy, id_culvert_xmid, id_culvert_ymid, &
      id_gategengeom_node_count, id_gategengeom_node_coordx, id_gategengeom_node_coordy, id_gategen_xmid, id_gategen_ymid, &
      id_pumpgeom_node_count, id_pumpgeom_node_coordx, id_pumpgeom_node_coordy, id_pump_xmid, id_pump_ymid, &
      id_bridgegeom_node_count, id_bridgegeom_node_coordx, id_bridgegeom_node_coordy, id_bridge_xmid, id_bridge_ymid, &
      id_srcgeom_node_count, id_srcgeom_node_coordx, id_srcgeom_node_coordy, id_src_xmid, id_src_ymid, &
      id_longculvertgeom_node_count, id_longculvertgeom_node_coordx, id_longculvertgeom_node_coordy, id_longculvert_xmid, id_longculvert_ymid

contains
   !> Write history data in NetCDF format.
   subroutine unc_write_his(tim) ! wrihis
      use Timers
      use m_flowtimes
      use m_flow
      use m_flowgeom
      use m_observations_data
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
      use fm_external_forcings_data, only: numtracers, trnames
      use m_transport, only: ITRA1, ITRAN, ISED1, NUMCONST
      use m_structures
      use m_fm_wq_processes, only: wq_user_outputs => outputs, noout_statt, noout_state, noout_user, jawaqproc
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
      use m_laterals, only: numlatsg, nNodesLat, geomXLat, geomYLat, nlatnd, nodeCountLat
      use odugrid
      use m_statistical_output_types, only: SO_CURRENT, SO_AVERAGE, SO_MAX, SO_MIN
      use fm_statistical_output
      use fm_location_types
      use m_output_config
      use MessageHandling, only: err, mess, LEVEL_WARN, LEVEL_ERROR
      use m_ug_nc_attribute, only: ug_nc_attribute

      implicit none

      double precision, intent(in) :: tim !< Current time, should in fact be time1, since the data written is always s1, ucx, etc.

      double precision, allocatable :: geom_x(:), geom_y(:)
      integer, allocatable :: node_count(:)
      integer, allocatable, save :: id_tra(:)
      integer, allocatable, save :: id_hwq(:)
      integer :: maxlocT, maxvalT !< row+column count of valobs

      integer :: ngenstru_, n

      double precision, save :: curtime_split = 0d0 ! Current time-partition that the file writer has open.
      integer :: ntot, i, j, ierr, nNodeTot, nNodes, k1, k2, nlinks

      character(len=255) :: filename
      character(len=25) :: transpunit
      character(len=1024) :: statcoordstring, local_statcoordstring
      integer :: ndims
      integer :: jawrizc = 0
      integer :: jawrizw = 0
      logical :: add_latlon

      ! NOTE: below new variables based on statistical output modules
      character(len=255) :: var_name, var_standard_name, var_long_name
      type(ug_nc_attribute), target :: attributes(4)
      integer :: ivar

      integer :: id_twodim, nc_precision
      integer, save :: id_timebds
      double precision, save :: time_his_prev

      character(len=4) :: stat_name_postfix
      character(len=11) :: stat_name_filter_postfix
      character(len=16) :: stat_long_name_postfix
      character(len=16) :: stat_cell_methods
      character(len=43) :: stat_cell_methods_filter_postfix

      if (jahiszcor > 0) then
         jawrizc = 1
         jawrizw = 1
      end if

      nc_precision = netcdf_data_type(md_nc_his_precision)

      if (timon) call timstrt("unc_write_his", handle_extra(54))

      ! Another time-partitioned file needs to start, reset iteration count (and file).
      if (ti_split > 0d0 .and. curtime_split /= time_split0) then
         it_his = 0
         curtime_split = time_split0
      end if

      ! Close/reset any previous hisfile.
      if (ihisfile /= 0) then ! reset stord ncid to zero if file not open
         ierr = nf90_inquire(ihisfile, ndims)
         if (ierr /= 0) ihisfile = 0
      end if

      if (ihisfile > 0 .and. it_his == 0) then
         ierr = unc_close(ihisfile)
         ihisfile = 0
      end if

      ! When no crs/obs present, return immediately.
      if (.not. model_has_obs_stations() .and. ncrs <= 0 .and. jahisbal <= 0 .and. jahiscgen <= 0 .and. num_rugs <= 0) then
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
         if (timon) call timstrt("unc_write_his INIT/DEF", handle_extra(61))

         call realloc(id_tra, ITRAN - ITRA1 + 1, keepExisting=.false.)

         ! Possibly a different model, so make valobs transpose at correct size again.
         maxlocT = max(size(valobs, 2), npumpsg, network%sts%numPumps, ngatesg, ncdamsg, ncgensg, ngategen, &
                       nweirgen, network%sts%numWeirs, ngenstru, network%sts%numGeneralStructures, &
                       ndambreaklinks, network%sts%numOrifices, network%sts%numBridges, network%sts%numculverts, &
                       network%sts%numuniweirs, network%cmps%count, nlongculverts)
         maxvalT = max(size(valobs, 1), NUMVALS_PUMP, NUMVALS_GATE, NUMVALS_CDAM, NUMVALS_CGEN, NUMVALS_GATEGEN, &
                       NUMVALS_WEIRGEN, NUMVALS_GENSTRU, &
                       NUMVALS_DAMBREAK, NUMVALS_ORIFGEN, NUMVALS_BRIDGE, NUMVALS_CULVERT, &
                       NUMVALS_UNIWEIR, NUMVALS_CMPSTRU, NUMVALS_LONGCULVERT)

         if (ti_split > 0d0) then
            filename = defaultFilename('his', timestamp=time_split0)
         else
            filename = defaultFilename('his')
         end if

         ierr = unc_create(filename, 0, ihisfile)
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
         call check_netcdf_error(nf90_def_dim(ihisfile, 'time', nf90_unlimited, id_timedim))
         call check_netcdf_error(nf90_def_dim(ihisfile, 'two', 2, id_twodim))

         call check_netcdf_error(nf90_def_dim(ihisfile, 'name_len', strlen_netcdf, id_strlendim))

         if (kmx > 0) then
            call check_netcdf_error(nf90_def_dim(ihisfile, 'laydim', kmx, id_laydim))
            call check_netcdf_error(nf90_def_dim(ihisfile, 'laydimw', kmx + 1, id_laydimw))
         end if

         if (stm_included .and. jahissed > 0) then
            if (ISED1 > 0) then
               call check_netcdf_error(nf90_def_dim(ihisfile, 'nSedSus', stmpar%lsedsus, id_sedsusdim))
            end if
            ! New implementation, sedsus fraction is additional dimension
            call check_netcdf_error(nf90_def_dim(ihisfile, 'nSedTot', stmpar%lsedtot, id_sedtotdim))
            call definencvar(ihisfile, id_frac_name, nf90_char, (/id_strlendim, id_sedtotdim/), 'sedfrac_name', 'sediment fraction identifier')
            if (jased > 0 .and. stmpar%morlyr%settings%iunderlyr == 2) then
               call check_netcdf_error(nf90_def_dim(ihisfile, 'nBedLayers', stmpar%morlyr%settings%nlyr, id_nlyrdim))
            end if
         end if

         ! Time
         !
         call ncu_set_att(attributes(1), 'standard_name', 'time')
         call ncu_set_att(attributes(2), 'bounds', 'time_bds')
         call definencvar(ihisfile, id_time, nf90_double, (/id_timedim/), 'time', unit=trim(Tudunitstr), extra_attributes=attributes(1:2))
         call definencvar(ihisfile, id_timebds, nf90_double, (/id_twodim, id_timedim/), 'time_bds', 'Time interval for each point in time.', unit=trim(Tudunitstr), extra_attributes=attributes(1:1))

         ! Size of latest timestep
         ierr = unc_def_var_nonspatial(ihisfile, id_timestep, nf90_double, (/id_timedim/), 'timestep', '', 'latest computational timestep size in each output interval', 's')
         !
         ! Observation stations
         !
         if (model_has_obs_stations()) then
            ierr = unc_addcoordmapping(ihisfile, jsferic)

            nNodeTot = numobs + nummovobs
            ierr = unc_def_his_structure_static_vars(ihisfile, ST_OBS_STATION, 1, numobs + nummovobs, 'point', nNodeTot, id_strlendim, &
                                                     id_statdim, id_statname, id_statgeom_node_count, id_statgeom_node_coordx, id_statgeom_node_coordy, &
                                                     add_latlon, id_statgeom_node_lon, id_statgeom_node_lat)

            ! Special definition of station_id for backwards compatibility reasons..
            call ncu_set_att(attributes(1), 'cf_role', 'timeseries_id')
            call definencvar(ihisfile, id_stat_id, nf90_char, (/id_strlendim, id_statdim/), 'station_id', 'id of station', extra_attributes=attributes(1:1))

            ! Define the x/y, lat/lon, and z coordinate variables for the station type.
            ierr = unc_def_his_station_coord_vars(ihisfile, id_laydim, id_laydimw, id_statdim, id_timedim, &
                                                  add_latlon, jawrizc, jawrizw, &
                                                  id_statx, id_staty, id_statlat, id_statlon, statcoordstring, &
                                                  id_zcs, id_zws, id_zwu)

         end if

         ierr = unc_def_his_structure_static_vars(ihisfile, ST_CROSS_SECTION, 1, ncrs, 'line', nNodesCrs, id_strlendim, &
                                                  id_crsdim, id_crs_id, id_crsgeom_node_count, id_crsgeom_node_coordx, id_crsgeom_node_coordy, &
                                                  id_poly_xmid=id_crs_xmid, id_poly_ymid=id_crs_ymid)

         ! Runup gauges
         ierr = unc_def_his_structure_static_vars(ihisfile, ST_RUNUP_GAUGE, 1, num_rugs, 'none', 0, id_strlendim, &
                                                  id_rugdim, id_rugname) ! No geometry

         ! Source-sinks
         if (jahissourcesink > 0 .and. numsrc > 0) then
            ! Define geometry related variables
            nNodeTot = 0
            do i = 1, numsrc
               nNodes = 0
               k1 = ksrc(1, i)
               k2 = ksrc(4, i)
               if (k1 /= 0) then
                  nNodes = nNodes + 1
               end if
               if (k2 /= 0) then
                  nNodes = nNodes + 1
               end if
               nNodeTot = nNodeTot + nNodes
            end do
         end if

         ierr = unc_def_his_structure_static_vars(ihisfile, ST_SOURCE_SINK, jahissourcesink, numsrc, 'line', nNodeTot, id_strlendim, &
                                                  id_srcdim, id_srcname, id_srcgeom_node_count, id_srcgeom_node_coordx, id_srcgeom_node_coordy, &
                                                  id_poly_xmid=id_src_xmid, id_poly_ymid=id_src_ymid)
         if (jahissourcesink > 0 .and. numsrc > 0) then
            call check_netcdf_error(nf90_def_dim(ihisfile, 'source_sink_points', msrc, id_srcptsdim))
            call definencvar(ihisfile, id_srcx, nf90_double, (/id_srcdim, id_srcptsdim/), 'source_sink_x_coordinate')
            call definencvar(ihisfile, id_srcy, nf90_double, (/id_srcdim, id_srcptsdim/), 'source_sink_y_coordinate')
            ierr = unc_addcoordatts(ihisfile, id_srcx, id_srcy, jsferic)
         end if

         if (timon) call timstrt("unc_write_his DEF structures", handle_extra(60))

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
         ierr = unc_def_his_structure_static_vars(ihisfile, ST_GENERAL_ST, jahiscgen, ngenstru_, 'line', nNodeTot, id_strlendim, &
                                                  id_genstrudim, id_genstru_id, id_genstrugeom_node_count, id_genstrugeom_node_coordx, id_genstrugeom_node_coordy, &
                                                  id_poly_xmid=id_genstru_xmid, id_poly_ymid=id_genstru_ymid)

         ! Pump
         ierr = unc_def_his_structure_static_vars(ihisfile, ST_PUMP, jahispump, npumpsg, 'line', number_of_pump_nodes(), id_strlendim, &
                                                  id_pumpdim, id_pump_id, id_pumpgeom_node_count, id_pumpgeom_node_coordx, id_pumpgeom_node_coordy, &
                                                  id_poly_xmid=id_pump_xmid, id_poly_ymid=id_pump_ymid)

         ! Gate (Old .ext file, QUANTITY='gateloweredgelevel')
         ierr = unc_def_his_structure_static_vars(ihisfile, ST_GATE, jahisgate, ngatesg, 'none', 0, id_strlendim, &
                                                  id_gatedim, id_gate_id)

         if (jahisgate > 0 .and. ngategen > 0) then
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
         ierr = unc_def_his_structure_static_vars(ihisfile, ST_GATEGEN, jahisgate, ngategen, 'line', nNodeTot, id_strlendim, &
                                                  id_gategendim, id_gategen_id, id_gategengeom_node_count, id_gategengeom_node_coordx, id_gategengeom_node_coordy, &
                                                  id_poly_xmid=id_gategen_xmid, id_poly_ymid=id_gategen_ymid)

         ! Controllable dam (Old .ext file QUANTITY='damlevel')
         ierr = unc_def_his_structure_static_vars(ihisfile, ST_DAM, jahiscdam, ncdamsg, 'none', 0, id_strlendim, &
                                                  id_cdamdim, id_cdam_id)

         ! Weir
         if (jahisweir > 0 .and. nweirgen > 0) then
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
         ierr = unc_def_his_structure_static_vars(ihisfile, ST_WEIR, jahisweir, nweirgen, 'line', nNodeTot, id_strlendim, &
                                                  id_weirgendim, id_weirgen_id, id_weirgengeom_node_count, id_weirgengeom_node_coordx, id_weirgengeom_node_coordy, &
                                                  id_poly_xmid=id_weirgen_xmid, id_poly_ymid=id_weirgen_ymid)

         ! Orifice
         ierr = unc_def_his_structure_static_vars(ihisfile, ST_ORIFICE, jahisorif, network%sts%numOrifices, 'line', nNodesOrif, id_strlendim, &
                                                  id_orifgendim, id_orifgen_id, id_orifgengeom_node_count, id_orifgengeom_node_coordx, id_orifgengeom_node_coordy, &
                                                  id_poly_xmid=id_weirgen_xmid, id_poly_ymid=id_weirgen_ymid)

         ! Bridge
         ierr = unc_def_his_structure_static_vars(ihisfile, ST_BRIDGE, jahisbridge, network%sts%numBridges, 'line', nNodesBridge, id_strlendim, &
                                                  id_bridgedim, id_bridge_id, id_bridgegeom_node_count, id_bridgegeom_node_coordx, id_bridgegeom_node_coordy, &
                                                  id_poly_xmid=id_bridge_xmid, id_poly_ymid=id_bridge_ymid)

         ! Culvert
         ierr = unc_def_his_structure_static_vars(ihisfile, ST_CULVERT, jahisculv, network%sts%numculverts, 'line', nNodesCulv, id_strlendim, &
                                                  id_culvertdim, id_culvert_id, id_culvertgeom_node_count, id_culvertgeom_node_coordx, id_culvertgeom_node_coordy, &
                                                  id_poly_xmid=id_culvert_xmid, id_poly_ymid=id_culvert_ymid)

         ! Dambreak
         ierr = unc_def_his_structure_static_vars(ihisfile, ST_DAMBREAK, jahisdambreak, ndambreaksignals, 'none', 0, id_strlendim, &
                                                  id_dambreakdim, id_dambreak_id)

         ! Universal weir
         ierr = unc_def_his_structure_static_vars(ihisfile, ST_UNI_WEIR, jahisuniweir, network%sts%numuniweirs, 'line', nNodesUniweir, id_strlendim, &
                                                  id_uniweirdim, id_uniweir_id, id_uniweirgeom_node_count, id_uniweirgeom_node_coordx, id_uniweirgeom_node_coordy, &
                                                  id_poly_xmid=id_uniweir_xmid, id_poly_ymid=id_uniweir_ymid)

         ! compound structure
         ierr = unc_def_his_structure_static_vars(ihisfile, ST_COMPOUND, jahiscmpstru, network%cmps%count, 'none', 0, id_strlendim, &
                                                  id_cmpstrudim, id_cmpstru_id)

         ! Long culvert
         ierr = unc_def_his_structure_static_vars(ihisfile, ST_LONGCULVERT, jahislongculv, nlongculverts, 'line', nNodesLongCulv, id_strlendim, &
                                                  id_longculvertdim, id_longculvert_id, id_longculvertgeom_node_count, id_longculvertgeom_node_coordx, id_longculvertgeom_node_coordy, &
                                                  id_poly_xmid=id_longculvert_xmid, id_poly_ymid=id_longculvert_ymid)

         ! Lateral
         ierr = unc_def_his_structure_static_vars(ihisfile, ST_LATERAL, jahislateral, numlatsg, 'point', nNodesLat, id_strlendim, &
                                                  id_latdim, id_lat_id, id_latgeom_node_count, id_latgeom_node_coordx, id_latgeom_node_coordy)
         ! TODO: UNST-7239: remove separate average IDX?
         if (timon) call timstop(handle_extra(60))

         if (dad_included) then ! Output for dredging and dumping
            call check_netcdf_error(nf90_def_dim(ihisfile, 'ndredlink', dadpar%nalink, id_dredlinkdim))
            call check_netcdf_error(nf90_def_dim(ihisfile, 'ndred', dadpar%dredge_dimension_length, id_dreddim))
            call check_netcdf_error(nf90_def_dim(ihisfile, 'ndump', dadpar%nadump, id_dumpdim))
            call definencvar(ihisfile, id_dred_name, nf90_char, (/id_strlendim, id_dreddim/), 'dredge_area_name', 'dredge area identifier')
            call definencvar(ihisfile, id_dump_name, nf90_char, (/id_strlendim, id_dreddim/), 'dump_area_name', 'dump area identifier')
         end if

         if (jacheckmonitor == 1) then
            call definencvar(ihisfile, id_checkmon, nc_precision, (/id_laydim, id_timedim/), 'checkerboard_monitor', 'Checkerboard mode monitor', unit='m s-1')
            call definencvar(ihisfile, id_num_timesteps, nf90_int, (/id_timedim/), 'num_timesteps')
            call definencvar(ihisfile, id_comp_time, nc_precision, (/id_timedim/), 'comp_time')
         end if

         ! set sediment transport unit after modelinit
         if (jahissed > 0 .and. jased > 0 .and. stm_included) then
            select case (stmpar%morpar%moroutput%transptype)
            case (0)
               transpunit = 'kg s-1 m-1'
            case (1)
               transpunit = 'm3 s-1 m-1'
            case (2)
               transpunit = 'm3 s-1 m-1'
            end select
            do ivar = IDX_HIS_SBCX, IDX_HIS_SSCY
               config_set_his%configs(ivar)%unit = transpunit
            end do
         end if

         ! WAQ statistic outputs are kept outside of the statistical output framework
         if (jawaqproc > 0) then
            ierr = unc_def_his_station_waq_statistic_outputs(id_hwq)
         end if

         if (jahisbedlev > 0 .and. model_has_obs_stations() .and. .not. stm_included) then
            call definencvar(ihisfile, id_varb, nc_precision, (/id_statdim/), 'bedlevel', 'bottom level', unit='m', namecoord=statcoordstring)
         end if

         do ivar = 1, out_variable_set_his%count
            associate (config => out_variable_set_his%statout(ivar)%output_config, &
                       id_var => out_variable_set_his%statout(ivar)%id_var)

               if (config%location_specifier /= UNC_LOC_STATION &
                   .and. config%location_specifier /= UNC_LOC_OBSCRS &
                   .and. config%location_specifier /= UNC_LOC_GLOBAL &
                   .and. config%location_specifier /= UNC_LOC_SOSI &
                   .and. config%location_specifier /= UNC_LOC_RUG &
                   .and. config%location_specifier /= UNC_LOC_GENSTRU &
                   .and. config%location_specifier /= UNC_LOC_DAM &
                   .and. config%location_specifier /= UNC_LOC_PUMP &
                   .and. config%location_specifier /= UNC_LOC_GATE &
                   .and. config%location_specifier /= UNC_LOC_GATEGEN &
                   .and. config%location_specifier /= UNC_LOC_WEIRGEN &
                   .and. config%location_specifier /= UNC_LOC_ORIFICE &
                   .and. config%location_specifier /= UNC_LOC_BRIDGE &
                   .and. config%location_specifier /= UNC_LOC_CULVERT &
                   .and. config%location_specifier /= UNC_LOC_DAMBREAK &
                   .and. config%location_specifier /= UNC_LOC_UNIWEIR &
                   .and. config%location_specifier /= UNC_LOC_CMPSTRU &
                   .and. config%location_specifier /= UNC_LOC_LONGCULVERT &
                   .and. config%location_specifier /= UNC_LOC_LATERAL &
                   .and. config%location_specifier /= UNC_LOC_DREDGE &
                   .and. config%location_specifier /= UNC_LOC_DUMP &
                   .and. config%location_specifier /= UNC_LOC_DRED_LINK &
                   ) then
                  call mess(LEVEL_DEBUG, 'unc_write_his: skipping item '//trim(config%name)//', because it''s not on a known output location.')
                  cycle
               end if

               select case (out_variable_set_his%statout(ivar)%operation_type)
               case (SO_CURRENT)
                  stat_name_postfix = ''
                  stat_long_name_postfix = ''
                  stat_cell_methods = 'time: point'
               case (SO_AVERAGE)
                  stat_name_postfix = '_avg'
                  stat_long_name_postfix = ' (average)'
                  stat_cell_methods = 'time: mean'
               case (SO_MAX)
                  stat_name_postfix = '_max'
                  stat_long_name_postfix = ' (maximum)'
                  stat_cell_methods = 'time: maximum'
               case (SO_MIN)
                  stat_name_postfix = '_min'
                  stat_long_name_postfix = ' (minimum)'
                  stat_cell_methods = 'time: minimum'
               end select
               stat_name_filter_postfix = ''
               stat_cell_methods_filter_postfix = ''
               if (out_variable_set_his%statout(ivar)%moving_average_window > 1) then
                  write (stat_name_filter_postfix, '(a,i0)') '_filter', out_variable_set_his%statout(ivar)%moving_average_window
                  write (stat_cell_methods_filter_postfix, '(a,i0,a)') ' (moving average filter using ', out_variable_set_his%statout(ivar)%moving_average_window, ' samples)'
               end if

               var_name = trim(config%name)//trim(stat_name_postfix)//trim(stat_name_filter_postfix)
               var_standard_name = config%standard_name ! Intentionally no pre/postfix for standard_name
               if (len_trim(config%long_name) > 0) then
                  var_long_name = trim(config%long_name)//trim(stat_long_name_postfix)
               else
                  var_long_name = ''
               end if

               select case (config%location_specifier)
               case (UNC_LOC_SOSI)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_srcdim, id_timedim/), var_name, var_long_name, config%unit, 'source_sink_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_RUG)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_rugdim, id_timedim/), var_name, var_long_name, config%unit, 'runup_gauge_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_GENSTRU)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_genstrudim, id_timedim/), var_name, var_long_name, config%unit, 'general_structure_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_DAM)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_cdamdim, id_timedim/), var_name, var_long_name, config%unit, 'cdam_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_PUMP)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_pumpdim, id_timedim/), var_name, var_long_name, config%unit, 'pump_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_GATE)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_gatedim, id_timedim/), var_name, var_long_name, config%unit, 'gate_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_GATEGEN)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_gategendim, id_timedim/), var_name, var_long_name, config%unit, 'gategen_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_WEIRGEN)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_weirgendim, id_timedim/), var_name, var_long_name, config%unit, 'weirgen_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_ORIFICE)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_orifgendim, id_timedim/), var_name, var_long_name, config%unit, 'orifice_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_BRIDGE)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_bridgedim, id_timedim/), var_name, var_long_name, config%unit, 'bridge_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_CULVERT)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_culvertdim, id_timedim/), var_name, var_long_name, config%unit, 'culvert_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_DAMBREAK)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_dambreakdim, id_timedim/), var_name, var_long_name, config%unit, 'dambreak_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_UNIWEIR)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_uniweirdim, id_timedim/), var_name, var_long_name, config%unit, 'uniweir_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_CMPSTRU)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_cmpstrudim, id_timedim/), var_name, var_long_name, config%unit, 'cmpstru_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_LONGCULVERT)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_longculvertdim, id_timedim/), var_name, var_long_name, config%unit, 'longculvert_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_LATERAL)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_latdim, id_timedim/), var_name, var_long_name, config%unit, 'lateral_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_DREDGE)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_dreddim, id_timedim/), var_name, var_long_name, config%unit, 'dredge_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_DUMP)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_dumpdim, id_timedim/), var_name, var_long_name, config%unit, 'dump_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_DRED_LINK)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_dredlinkdim, id_sedtotdim, id_timedim/), var_name, var_long_name, config%unit, 'dredge_link_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_STATION)
                  if (allocated(config%nc_dim_ids)) then
                     if (config%nc_dim_ids%laydim) then
                        local_statcoordstring = trim(statcoordstring)//' zcoordinate_c'
                     else if (config%nc_dim_ids%laydim_interface_center) then
                        local_statcoordstring = trim(statcoordstring)//' zcoordinate_w'
                     else if (config%nc_dim_ids%laydim_interface_edge) then
                        local_statcoordstring = trim(statcoordstring)//' zcoordinate_wu'
                     else
                        local_statcoordstring = statcoordstring
                     end if
                     call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), build_nc_dimension_id_list(config%nc_dim_ids), var_name, var_long_name, &
                                      config%unit, local_statcoordstring, fillVal=dmiss, add_gridmapping=.true., extra_attributes=config%additional_attributes%atts)
                  else
                     call err('Internal error, please report: UNC_LOC_STATION variable '//trim(config%name)//' does not have nc_dim_ids set.')
                  end if
               case (UNC_LOC_OBSCRS)
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_crsdim, id_timedim/), var_name, var_long_name, config%unit, 'cross_section_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
               case (UNC_LOC_GLOBAL)
                  if (timon) call timstrt("unc_write_his DEF bal", handle_extra(59))
                  call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/id_timedim/), var_name, var_long_name, config%unit, "", fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
                  if (timon) call timstop(handle_extra(59))
               end select

               if (len_trim(var_standard_name) > 0) then
                  call check_netcdf_error(nf90_put_att(ihisfile, id_var, 'standard_name', trim(var_standard_name)))
               end if
               if (len_trim(stat_cell_methods) > 0) then
                  call check_netcdf_error(nf90_put_att(ihisfile, id_var, 'cell_methods', trim(stat_cell_methods)//trim(stat_cell_methods_filter_postfix)))
               end if
            end associate
         end do

         call check_netcdf_error(nf90_enddef(ihisfile))
         if (timon) call timstop(handle_extra(61))

         if (timon) call timstrt('unc_write_his timeindep data', handle_extra(63))
         if (it_his == 0) then
            ! Observation stations
            do i = 1, numobs + nummovobs
               call check_netcdf_error(nf90_put_var(ihisfile, id_stat_id, trimexact(namobs(i), strlen_netcdf), (/1, i/))) ! Extra for OpenDA-wrapper
            end do
            !
            ! Observation cross sections
            if (ncrs > 0) then
               call check_netcdf_error(nf90_put_var(ihisfile, id_crsgeom_node_coordx, geomXCrs, start=(/1/), count=(/nNodesCrs/)))
               call check_netcdf_error(nf90_put_var(ihisfile, id_crsgeom_node_coordy, geomYCrs, start=(/1/), count=(/nNodesCrs/)))
               call check_netcdf_error(nf90_put_var(ihisfile, id_crsgeom_node_count, nodeCountCrs))
               if (allocated(geomXCrs)) deallocate (geomXCrs)
               if (allocated(geomYCrs)) deallocate (geomYCrs)
               if (allocated(nodeCountCrs)) deallocate (nodeCountCrs)
            end if

            ! Source-sinks
            if (jahissourcesink > 0 .and. numsrc > 0) then
               call check_netcdf_error(nf90_put_var(ihisfile, id_srcx, xsrc))
               call check_netcdf_error(nf90_put_var(ihisfile, id_srcy, ysrc))
               j = 1
               call realloc(node_count, numsrc, fill=0)
               call realloc(geom_x, 2)
               call realloc(geom_y, 2)
               do i = 1, numsrc
                  k1 = ksrc(1, i)
                  k2 = ksrc(4, i)
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
                     call check_netcdf_error(nf90_put_var(ihisfile, id_srcgeom_node_coordx, geom_x(1:nNodes), start=(/j/), count=(/nNodes/)))
                     call check_netcdf_error(nf90_put_var(ihisfile, id_srcgeom_node_coordy, geom_y(1:nNodes), start=(/j/), count=(/nNodes/)))
                  end if
                  j = j + nNodes
               end do
               call check_netcdf_error(nf90_put_var(ihisfile, id_srcgeom_node_count, node_count))
            end if

            ! Lateral discharges
            if (jahislateral > 0 .and. numlatsg > 0) then
               call check_netcdf_error(nf90_put_var(ihisfile, id_latgeom_node_coordx, geomXLat(1:nNodesLat), start=(/1/), count=(/nlatnd/)))
               call check_netcdf_error(nf90_put_var(ihisfile, id_latgeom_node_coordy, geomYLat(1:nNodesLat), start=(/1/), count=(/nlatnd/)))
               call check_netcdf_error(nf90_put_var(ihisfile, id_latgeom_node_count, nodeCountLat))
            end if

            if (jased > 0 .and. stm_included .and. jahissed > 0 .and. ISED1 > 0) then
               do i = 1, stmpar%lsedtot
                  call check_netcdf_error(nf90_put_var(ihisfile, id_frac_name, trimexact(stmpar%sedpar%namsed(i), strlen_netcdf), (/1, i/)))
               end do
            end if

            if (dad_included) then
               do i = 1, dadpar%dredge_dimension_length
                  call check_netcdf_error(nf90_put_var(ihisfile, id_dred_name, trimexact(dadpar%dredge_areas(i), strlen_netcdf), (/1, i/)))
               end do
               !
               do i = 1, dadpar%nadump
                  call check_netcdf_error(nf90_put_var(ihisfile, id_dump_name, trimexact(dadpar%dump_areas(i), strlen_netcdf), (/1, i/)))
               end do
            end if

            call unc_put_his_structure_static_vars(ihisfile)
            if (timon) call timstop(handle_extra(63))
         end if
      end if
      ! Increment output counters in m_flowtimes.
      if (it_his == 0) then
         time_his_prev = tim
      end if
      time_his = tim
      it_his = it_his + 1

      if (timon) call timstrt('unc_write_his time data', handle_extra(64))

      call check_netcdf_error(nf90_put_var(ihisfile, id_time, time_his, (/it_his/)))
      call check_netcdf_error(nf90_put_var(ihisfile, id_timebds, (/time_his_prev, time_his/), (/1, it_his/)))
      time_his_prev = time_his
      call check_netcdf_error(nf90_put_var(ihisfile, id_timestep, dts, (/it_his/)))
      if (timon) call timstop(handle_extra(64))

!   Observation points (fixed+moving)

      ntot = numobs + nummovobs
      !Fill average source-sink discharge with different array on first timestep
      if (it_his == 1) then
         do i = 1, numsrc
            qsrc(i) = qstss((numconst + 1) * (i - 1) + 1)
         end do
      end if
      !Bottom level is written separately from statout if it is static
      if (ntot > 0 .and. .not. stm_included .and. jahisbedlev > 0) then
         call check_netcdf_error(nf90_put_var(ihisfile, id_varb, valobs(:, IPNT_BL), start=(/1/)))
      end if

      ! WAQ statistic outputs are kept outside of the statistical output framework
      if (jawaqproc > 0) then
         ierr = unc_put_his_station_waq_statistic_outputs(id_hwq)
      end if

      do ivar = 1, out_variable_set_his%count
         associate (config => out_variable_set_his%statout(ivar)%output_config, &
                    id_var => out_variable_set_his%statout(ivar)%id_var)

            if (config%location_specifier /= UNC_LOC_STATION &
                .and. config%location_specifier /= UNC_LOC_OBSCRS &
                .and. config%location_specifier /= UNC_LOC_GLOBAL &
                .and. config%location_specifier /= UNC_LOC_SOSI &
                .and. config%location_specifier /= UNC_LOC_RUG &
                .and. config%location_specifier /= UNC_LOC_GENSTRU &
                .and. config%location_specifier /= UNC_LOC_DAM &
                .and. config%location_specifier /= UNC_LOC_PUMP &
                .and. config%location_specifier /= UNC_LOC_GATE &
                .and. config%location_specifier /= UNC_LOC_GATEGEN &
                .and. config%location_specifier /= UNC_LOC_WEIRGEN &
                .and. config%location_specifier /= UNC_LOC_ORIFICE &
                .and. config%location_specifier /= UNC_LOC_BRIDGE &
                .and. config%location_specifier /= UNC_LOC_CULVERT &
                .and. config%location_specifier /= UNC_LOC_DAMBREAK &
                .and. config%location_specifier /= UNC_LOC_UNIWEIR &
                .and. config%location_specifier /= UNC_LOC_CMPSTRU &
                .and. config%location_specifier /= UNC_LOC_LONGCULVERT &
                .and. config%location_specifier /= UNC_LOC_LATERAL &
                .and. config%location_specifier /= UNC_LOC_DREDGE &
                .and. config%location_specifier /= UNC_LOC_DUMP &
                .and. config%location_specifier /= UNC_LOC_DRED_LINK &
                ) then
               call mess(LEVEL_DEBUG, 'unc_write_his: skipping item '//trim(config%name)//', because it''s not on a known statistical output location.')
               cycle
            end if

            select case (config%location_specifier)
            case (UNC_LOC_OBSCRS, &
                  UNC_LOC_RUG, &
                  UNC_LOC_SOSI, &
                  UNC_LOC_GENSTRU, &
                  UNC_LOC_DAM, &
                  UNC_LOC_PUMP, &
                  UNC_LOC_GATE, &
                  UNC_LOC_GATEGEN, &
                  UNC_LOC_WEIRGEN, &
                  UNC_LOC_ORIFICE, &
                  UNC_LOC_BRIDGE, &
                  UNC_LOC_CULVERT, &
                  UNC_LOC_DAMBREAK, &
                  UNC_LOC_UNIWEIR, &
                  UNC_LOC_CMPSTRU, &
                  UNC_LOC_LONGCULVERT, &
                  UNC_LOC_LATERAL, &
                  UNC_LOC_DREDGE, &
                  UNC_LOC_DUMP &
                  )
               call check_netcdf_error(nf90_put_var(ihisfile, id_var, out_variable_set_his%statout(ivar)%stat_output, start=(/1, it_his/)))
            case (UNC_LOC_STATION)
               call write_station_netcdf_variable(out_variable_set_his%statout(ivar), ihisfile, it_his)
            case (UNC_LOC_DRED_LINK)
               call check_netcdf_error(nf90_put_var(ihisfile, id_var, out_variable_set_his%statout(ivar)%stat_output, start=(/1, 1, it_his/), count=(/dadpar%nalink, stmpar%lsedtot, 1/)))
            case (UNC_LOC_GLOBAL)
               if (timon) call timstrt('unc_write_his IDX data', handle_extra(67))
               call check_netcdf_error(nf90_put_var(ihisfile, id_var, out_variable_set_his%statout(ivar)%stat_output, start=(/it_his/)))
               if (timon) call timstop(handle_extra(67))
            end select
         end associate
      end do

      ! Write x/y-, lat/lon- and z-coordinates for the observation stations every time (needed for moving observation stations)
      ierr = unc_put_his_station_coord_vars(ihisfile, numobs, nummovobs, add_latlon, jawrizc, jawrizw, &
                                            id_statx, id_staty, id_statlat, id_statlon, &
                                            id_zcs, id_zws, id_zwu, it_his, &
                                            id_statgeom_node_count, id_statgeom_node_coordx, id_statgeom_node_coordy, &
                                            id_statgeom_node_lon, id_statgeom_node_lat)

      if (it_his == 1) then
         do n = 1, ST_MAX_TYPE
            call unc_write_struc_input_coordinates(ihisfile, n)
         end do
      end if

      if (jacheckmonitor == 1) then
         call check_netcdf_error(nf90_put_var(ihisfile, id_checkmon, checkmonitor, start=(/1, it_his/)))

         call check_netcdf_error(nf90_put_var(ihisfile, id_num_timesteps, int(dnt), start=(/it_his/)))
         call check_netcdf_error(nf90_put_var(ihisfile, id_comp_time, tim_get_wallclock(handle_steps), start=(/it_his/)))
      end if

      if (unc_noforcedflush == 0) then
         call check_netcdf_error(nf90_sync(ihisfile)) ! Flush file
      end if

      if (timon) call timstop(handle_extra(54))

   contains
      !> Define the static variables for a single structure type.
      !! This includes: NetCDF dimension ids, character Id variable and simple geometry container variables.
      !! Note: the writing ('putting') of data is done by another subroutine: unc_put_his_structure_static_vars.
      function unc_def_his_structure_static_vars(ncid, struc_type_id, output_enabled, count, geom_type, ngeom_node, id_strlendim, &
                                                 id_strdim, id_strid, id_geom_node_count, id_geom_coordx, id_geom_coordy, &
                                                 add_latlon, id_geom_coordlon, id_geom_coordlat, id_poly_xmid, id_poly_ymid) result(ierr)
         use string_module, only: strcmpi
         use MessageHandling, only: mess, LEVEL_WARN
         use unstruc_netcdf, only: unc_addcoordatts
         use dfm_error, only: DFM_NOERR

         integer, intent(in) :: ncid !< NetCDF id of already open dataset
         integer, intent(in) :: struc_type_id !< The id of the type of the structure (e.g. ST_CULVERT)
         integer, intent(in) :: output_enabled !< Whether or not (1/0) this structure's output must be written.
         integer, intent(in) :: count !< Number of structures for this structure_type
         character(len=*), intent(in) :: geom_type !< Geometry type, one of: 'point', 'line', 'polygon' (or 'none')
         integer, intent(in) :: ngeom_node !< Total number of geometry nodes for this structure_type
         integer, intent(in) :: id_strlendim !< Already created NetCDF dimension id for max string length of the character Ids.
         integer, intent(out) :: id_strdim !< NetCDF dimension id created for this structure type
         integer, intent(out) :: id_strid !< NetCDF variable id created for the character Ids of the structures of this type
         integer, optional, intent(out) :: id_geom_node_count !< NetCDF variable id created for the node count of the structures of this type
         integer, optional, intent(out) :: id_geom_coordx !< NetCDF variable id created for the node x coordinates for all structures of this type
         integer, optional, intent(out) :: id_geom_coordy !< NetCDF variable id created for the node y coordinates for all structures of this type
         logical, optional, intent(in) :: add_latlon !< Whether or not to add extra lon/lat coordinates for the nodes
         !< (only applicable when the coordx/y variables contain projected coordinates,
         !< and requires id_node_lon/lat to be passed as well).
         integer, optional, intent(out) :: id_geom_coordlon !< NetCDF variable id created for the node longitude coordinates for all structures of this type
         integer, optional, intent(out) :: id_geom_coordlat !< NetCDF variable id created for the node latitude  coordinates for all structures of this type
         integer, optional, intent(out) :: id_poly_xmid !< NetCDF variable id created for the x-coordinate of the structure's polyline midpoint
         integer, optional, intent(out) :: id_poly_ymid !< NetCDF variable id created for the y-coordinate of the structure's polyline midpoint

         integer :: ierr !< Result status (NF90_NOERR if successful)

         character(len=255) :: prefix !< Base name of this structure type, e.g., 'uniweir'
         character(len=255) :: name !< Human readable name of this structure type, e.g., 'universal weir'

         ierr = DFM_NOERR

         if (output_enabled == 0 .or. count == 0) then
            return
         end if

         call get_prefix_and_name_from_struc_type_id(struc_type_id, prefix, name)

         call check_netcdf_error(nf90_def_dim(ncid, trim(prefix), count, id_strdim))
         call check_netcdf_error(nf90_def_var(ncid, trim(prefix)//'_name', nf90_char, (/id_strlendim, id_strdim/), id_strid))
         call check_netcdf_error(nf90_put_att(ncid, id_strid, 'cf_role', 'timeseries_id'))
         call check_netcdf_error(nf90_put_att(ncid, id_strid, 'long_name', 'name of '//trim(name)))

         if (.not. strcmpi(geom_type, 'none') .and. len_trim(geom_type) > 0) then
            ! Define geometry related variables
            ierr = sgeom_def_geometry_variables(ncid, trim(prefix)//'_geom', trim(name), geom_type, ngeom_node, id_strdim, &
                                                id_geom_node_count, id_geom_coordx, id_geom_coordy, add_latlon, id_geom_coordlon, id_geom_coordlat)
         end if

         ! Polyline midpoint coordinates
         if (strcmpi(trim(prefix), 'pump')) then ! TODO (UNST-7919): define xmid,ymid for all polyline structures by replacing this line with:   if (strcmpi(geom_type,'line')) then
            if (.not. (present(id_poly_xmid) .and. present(id_poly_ymid))) then
               call mess(LEVEL_WARN, 'unc_def_his_structure_static_vars should return id_poly_xmid and id_poly_ymid for polyline structures')
            end if
            call check_netcdf_error(nf90_def_var(ncid, trim(prefix)//'_xmid', nc_precision, [id_strdim], id_poly_xmid))
            call check_netcdf_error(nf90_def_var(ncid, trim(prefix)//'_ymid', nc_precision, [id_strdim], id_poly_ymid))
            ! jsferic: xy pair is in : 0=cart, 1=sferic coordinates
            ierr = unc_addcoordatts(ncid, id_poly_xmid, id_poly_ymid, jsferic)
            call check_netcdf_error(nf90_put_att(ncid, id_poly_xmid, 'long_name', 'x-coordinate of representative mid point of '//trim(prefix)//' location (snapped polyline)'))
            call check_netcdf_error(nf90_put_att(ncid, id_poly_ymid, 'long_name', 'y-coordinate of representative mid point of '//trim(prefix)//' location (snapped polyline)'))
         end if

      end function unc_def_his_structure_static_vars

      !> Define the x/y, lat/lon, and z coordinate variables for the station type.
      function unc_def_his_station_coord_vars(ihisfile, id_laydim, id_laydimw, id_statdim, id_timedim, &
                                              add_latlon, jawrizc, jawrizw, &
                                              id_statx, id_staty, id_statlat, id_statlon, statcoordstring, &
                                              id_zcs, id_zws, id_zwu) result(ierr)
         use dfm_error, only: DFM_NOERR

         integer, intent(in) :: ihisfile !< NetCDF id of already open dataset
         integer, intent(in) :: id_laydim !< NetCDF dimension id for the vertical layers
         integer, intent(in) :: id_laydimw !< NetCDF dimension id for the staggered vertical layers
         integer, intent(in) :: id_statdim !< NetCDF dimension id for the station type
         integer, intent(in) :: id_timedim !< NetCDF dimension id for the time dimension
         logical, intent(in) :: add_latlon !< Whether or not to include station lat/lon coordinates in the his file
         integer, intent(in) :: jawrizc !< Whether or not to write observation station zcoordinate_c to the his file
         integer, intent(in) :: jawrizw !< Whether or not to write observation station zcoordinate_w + zcoordinate_wu to the his file
         integer, intent(out) :: id_statx !< NetCDF variable id created for the station x-coordinate
         integer, intent(out) :: id_staty !< NetCDF variable id created for the station y-coordinate
         integer, intent(out) :: id_statlat !< NetCDF variable id created for the station lat-coordinate
         integer, intent(out) :: id_statlon !< NetCDF variable id created for the station lon-coordinate
         character(len=*), intent(out) :: statcoordstring !< String listing the coordinate variables associated with the stations
         integer, intent(out) :: id_zcs !< NetCDF variable id created for the station zcoordinate_c
         integer, intent(out) :: id_zws !< NetCDF variable id created for the station zcoordinate_w
         integer, intent(out) :: id_zwu !< NetCDF variable id created for the station zcoordinate_wu

         integer :: ierr !< Result status (NF90_NOERR if successful)

         ierr = DFM_NOERR

         ! Define the x,y-coordinate variables
         ierr = unc_def_his_station_coord_vars_xy(ihisfile, id_statdim, id_timedim, id_statx, id_staty)
         if (ierr /= DFM_NOERR) then
            call mess(LEVEL_ERROR, 'Programming error, please report: unc_def_his_station_coord_vars_xy returned non-zero error code')
         end if

         statcoordstring = 'station_x_coordinate station_y_coordinate station_name'

         ! If so specified, add lat/lon-coordinates
         if (add_latlon) then
            ierr = unc_def_his_station_coord_vars_latlon(ihisfile, id_statx, id_statlat, id_statlon)
            if (ierr /= DFM_NOERR) then
               call mess(LEVEL_ERROR, 'Programming error, please report: unc_def_his_station_coord_vars_latlon returned non-zero error code')
            end if
            statcoordstring = trim(statcoordstring)//' station_lon station_lat'
         end if

         ! If so specified, add the z coordinates
         ierr = unc_def_his_station_coord_vars_z(ihisfile, id_laydim, id_laydimw, id_statdim, id_timedim, &
                                                 jawrizc, jawrizw, id_zcs, id_zws, id_zwu)
         if (ierr /= DFM_NOERR) then
            call mess(LEVEL_ERROR, 'Programming error, please report: unc_def_his_station_coord_vars_z returned non-zero error code')
         end if

      end function unc_def_his_station_coord_vars

      !> Define the x/y-coordinate variables for the station type.
      function unc_def_his_station_coord_vars_xy(ihisfile, id_statdim, id_timedim, &
                                                 id_statx, id_staty) result(ierr)
         use dfm_error, only: DFM_NOERR

         integer, intent(in) :: ihisfile !< NetCDF id of already open dataset
         integer, intent(in) :: id_statdim !< NetCDF dimension id for the station type
         integer, intent(in) :: id_timedim !< NetCDF dimension id for the time dimension
         integer, intent(out) :: id_statx !< NetCDF variable id created for the station x-coordinate
         integer, intent(out) :: id_staty !< NetCDF variable id created for the station y-coordinate

         integer :: ierr !< Result status (NF90_NOERR if successful)

         integer, dimension(:), allocatable :: dim_ids

         ierr = DFM_NOERR

         ! If there are moving observation stations, include a time dimension for the x/y-coordinates
         if (model_has_moving_obs_stations()) then
            allocate (dim_ids(2))
            dim_ids = [id_statdim, id_timedim] ! TODO: AvD: decide on UNST-1606 (trajectory_id vs. timeseries_id)
         else
            allocate (dim_ids(1))
            dim_ids = [id_statdim]
         end if

         call definencvar(ihisfile, id_statx, nf90_double, dim_ids, 'station_x_coordinate', 'original x-coordinate of station (non-snapped)')
         call definencvar(ihisfile, id_staty, nf90_double, dim_ids, 'station_y_coordinate', 'original y-coordinate of station (non-snapped)')

         ! jsferic: xy pair is in : 0=cart, 1=sferic coordinates
         ierr = unc_addcoordatts(ihisfile, id_statx, id_staty, jsferic)

      end function unc_def_his_station_coord_vars_xy

      !> Define the lat/lon-coordinate variables for the station type.
      function unc_def_his_station_coord_vars_latlon(ihisfile, id_statx, id_statlat, id_statlon) result(ierr)
         use dfm_error, only: DFM_NOERR

         integer, intent(in) :: ihisfile !< NetCDF id of already open dataset
         integer, intent(in) :: id_statx !< NetCDF variable id for the station x-coordinate
         integer, intent(out) :: id_statlat !< NetCDF variable id created for the station lat-coordinate
         integer, intent(out) :: id_statlon !< NetCDF variable id created for the station lon-coordinate

         integer :: ierr !< Result status (NF90_NOERR if successful)

         ierr = DFM_NOERR

         ! Simply clone the x/y-variables
         ierr = ncu_clone_vardef(ihisfile, ihisfile, id_statx, 'station_lat', id_statlat, &
                                 'latitude', 'original lat-coordinate of station (non-snapped)', 'degrees_north')
         ierr = ncu_clone_vardef(ihisfile, ihisfile, id_statx, 'station_lon', id_statlon, &
                                 'longitude', 'original lon-coordinate of station (non-snapped)', 'degrees_east')

      end function unc_def_his_station_coord_vars_latlon

      !> Define the z-coordinate variables for the station type.
      function unc_def_his_station_coord_vars_z(ihisfile, id_laydim, id_laydimw, id_statdim, id_timedim, &
                                                jawrizc, jawrizw, id_zcs, id_zws, id_zwu) result(ierr)
         use dfm_error, only: DFM_NOERR

         integer, intent(in) :: ihisfile !< NetCDF id of already open dataset
         integer, intent(in) :: id_laydim !< NetCDF dimension id for the vertical layers
         integer, intent(in) :: id_laydimw !< NetCDF dimension id for the staggered vertical layers
         integer, intent(in) :: id_statdim !< NetCDF dimension id for the station type
         integer, intent(in) :: id_timedim !< NetCDF dimension id for the time dimension
         integer, intent(in) :: jawrizc !< Whether or not to write observation station zcoordinate_c to the his file
         integer, intent(in) :: jawrizw !< Whether or not to write observation station zcoordinate_w + zcoordinate_wu to the his file
         integer, intent(out) :: id_zcs !< NetCDF variable id created for the station zcoordinate_c
         integer, intent(out) :: id_zws !< NetCDF variable id created for the station zcoordinate_w
         integer, intent(out) :: id_zwu !< NetCDF variable id created for the station zcoordinate_wu

         integer :: ierr !< Result status (NF90_NOERR if successful)
         type(ug_nc_attribute) :: extra_attributes(1)
         ierr = DFM_NOERR

         if (.not. model_is_3D()) then
            return
         end if
         call ncu_set_att(extra_attributes(1), 'positive', 'up')
         ! If so specified, add the zcoordinate_c
         if (jawrizc == 1) then
            call definencvar(ihisfile, id_zcs, nc_precision, [id_laydim, id_statdim, id_timedim], &
                             'zcoordinate_c', 'vertical coordinate at center of flow element and layer', 'm', &
                             trim(statcoordstring)//' zcoordinate_c', geometry='station_geom', fillVal=dmiss, extra_attributes=extra_attributes)
         end if

         ! If so specified, add the zcoordinate_w + zcoordinate_wu
         if (jawrizw == 1) then
            call definencvar(ihisfile, id_zws, nc_precision, [id_laydimw, id_statdim, id_timedim], &
                             'zcoordinate_w', 'vertical coordinate at centre of flow element and at layer interface', 'm', &
                             trim(statcoordstring)//' zcoordinate_w', geometry='station_geom', fillVal=dmiss, extra_attributes=extra_attributes)

            call definencvar(ihisfile, id_zwu, nc_precision, [id_laydimw, id_statdim, id_timedim], &
                             'zcoordinate_wu', 'vertical coordinate at nearest edge of flow element and at layer interface', 'm', &
                             trim(statcoordstring)//' zcoordinate_wu', geometry='station_geom', fillVal=dmiss, extra_attributes=extra_attributes)
         end if
      end function unc_def_his_station_coord_vars_z

      !> Write (put) the x/y-, lat/lon- and z-coordinate variables for the station type.
      function unc_put_his_station_coord_vars(ihisfile, numobs, nummovobs, add_latlon, jawrizc, jawrizw, &
                                              id_statx, id_staty, id_statlat, id_statlon, &
                                              id_zcs, id_zws, id_zwu, it_his, &
                                              id_geom_node_count, id_geom_node_coordx, id_geom_node_coordy, &
                                              id_geom_node_coordlon, id_geom_node_coordlat) result(ierr)
         use dfm_error, only: DFM_NOERR

         integer, intent(in) :: ihisfile !< NetCDF id of already open dataset
         integer, intent(in) :: numobs !< Number of fixed observation stations
         integer, intent(in) :: nummovobs !< Number of moving observation stations
         logical, intent(in) :: add_latlon !< Whether or not to include station lat/lon coordinates in the his file
         integer, intent(in) :: jawrizc !< Whether or not to write observation station zcoordinate_c to the his file
         integer, intent(in) :: jawrizw !< Whether or not to write observation station zcoordinate_w + zcoordinate_wu to the his file
         integer, intent(in) :: id_statx !< NetCDF variable id created for the station x-coordinate
         integer, intent(in) :: id_staty !< NetCDF variable id created for the station y-coordinate
         integer, intent(in) :: id_statlat !< NetCDF variable id created for the station lat-coordinate
         integer, intent(in) :: id_statlon !< NetCDF variable id created for the station lon-coordinate
         integer, intent(in) :: id_zcs !< NetCDF variable id for the station zcoordinate_c
         integer, intent(in) :: id_zws !< NetCDF variable id for the station zcoordinate_w
         integer, intent(in) :: id_zwu !< NetCDF variable id for the station zcoordinate_wu
         integer, intent(in) :: it_his !< Timeframe to write to in the his file
         integer, intent(in) :: id_geom_node_count !< NetCDF variable id created for the node count of the structures of this type
         integer, intent(in) :: id_geom_node_coordx !< NetCDF variable id created for the station geometry node x-coordinate
         integer, intent(in) :: id_geom_node_coordy !< NetCDF variable id created for the station geometry node y-coordinate
         integer, intent(in) :: id_geom_node_coordlon !< NetCDF variable id created for the station geometry node longitude coordinate
         integer, intent(in) :: id_geom_node_coordlat !< NetCDF variable id created for the station geometry node latitude coordinate

         integer :: ierr !< Result status (NF90_NOERR if successful)

         ierr = DFM_NOERR

         if (.not. model_has_obs_stations()) then
            return
         end if

         ierr = unc_put_his_station_coord_vars_xy(ihisfile, numobs, nummovobs, id_statx, id_staty, it_his)

#ifdef HAVE_PROJ
         if (add_latlon) then
            ierr = unc_put_his_station_coord_vars_latlon(ihisfile, numobs, nummovobs, id_statlat, id_statlon, it_his)
         end if
#endif

         ierr = unc_put_his_station_coord_vars_z(ihisfile, numobs, nummovobs, jawrizc, jawrizw, id_zcs, id_zws, id_zwu, it_his)

         ierr = unc_put_his_station_geom_coord_vars_xy(ihisfile, numobs, it_his, id_geom_node_count, id_geom_node_coordx, id_geom_node_coordy, &
                                                       add_latlon, id_geom_node_coordlon, id_geom_node_coordlat)

      end function unc_put_his_station_coord_vars

      !> Write (put) the x/y-coordinate variables for the station type.
      function unc_put_his_station_coord_vars_xy(ihisfile, numobs, nummovobs, id_statx, id_staty, it_his) result(ierr)
         use dfm_error, only: DFM_NOERR

         integer, intent(in) :: ihisfile !< NetCDF id of already open dataset
         integer, intent(in) :: numobs !< Number of fixed observation stations
         integer, intent(in) :: nummovobs !< Number of moving observation stations
         integer, intent(in) :: id_statx !< NetCDF variable id created for the station x-coordinate
         integer, intent(in) :: id_staty !< NetCDF variable id created for the station y-coordinate
         integer, intent(in) :: it_his !< Timeframe to write to in the his file

         integer :: ierr !< Result status (NF90_NOERR if successful)

         integer, dimension(:), allocatable :: start, count

         ierr = DFM_NOERR

         ! If there are moving observation stations, include a time dimension for the x/y-coordinates
         if (model_has_moving_obs_stations()) then
            start = [1, it_his]
            count = [numobs + nummovobs, 1]
         else
            start = [1]
            count = [numobs + nummovobs]
         end if

         call check_netcdf_error(nf90_put_var(ihisfile, id_statx, xobs(:), start=start, count=count))
         call check_netcdf_error(nf90_put_var(ihisfile, id_staty, yobs(:), start=start, count=count))

         deallocate (start) ! TODO: TB: paragraph 4.4 of the style guide recommends using deallocate even though it is no longer necessary, should this recommendation be removed?
         deallocate (count)

      end function unc_put_his_station_coord_vars_xy

      !> Write (put) the lat/lon-coordinate variables for the station type.
      function unc_put_his_station_coord_vars_latlon(ihisfile, numobs, nummovobs, id_statlat, id_statlon, it_his) result(ierr)
         use m_observations_data, only: xobs, yobs
         use dfm_error, only: DFM_NOERR

         integer, intent(in) :: ihisfile !< NetCDF id of already open dataset
         integer, intent(in) :: numobs !< Number of fixed observation stations
         integer, intent(in) :: nummovobs !< Number of moving observation stations
         integer, intent(in) :: id_statlat !< NetCDF variable id created for the station lat-coordinate
         integer, intent(in) :: id_statlon !< NetCDF variable id created for the station lon-coordinate
         integer, intent(in) :: it_his !< Timeframe to write to in the his file

         integer :: ierr !< Result status (NF90_NOERR if successful)

         integer, dimension(:), allocatable :: start, count

         ierr = DFM_NOERR

         ! If there are moving observation stations, include a time dimension for the lat/lon-coordinates
         if (model_has_moving_obs_stations()) then
            start = [1, it_his]
            count = [numobs + nummovobs, 1]
         else
            start = [1]
            count = [numobs + nummovobs]
         end if

         call transform_and_put_latlon_coordinates(ihisfile, id_statlon, id_statlat, &
                                                   nccrs%proj_string, xobs, yobs, start=start, count=count)
      end function unc_put_his_station_coord_vars_latlon

      !> Write (put) the z-coordinate variables for the station type.
      function unc_put_his_station_coord_vars_z(ihisfile, numobs, nummovobs, jawrizc, jawrizw, id_zcs, id_zws, id_zwu, it_his) result(ierr)
         use dfm_error, only: DFM_NOERR

         integer, intent(in) :: ihisfile !< NetCDF id of already open dataset
         integer, intent(in) :: numobs !< Number of fixed observation stations
         integer, intent(in) :: nummovobs !< Number of moving observation stations
         integer, intent(in) :: jawrizc !< Whether or not to write observation station zcoordinate_c to the his file
         integer, intent(in) :: jawrizw !< Whether or not to write observation station zcoordinate_w + zcoordinate_wu to the his file
         integer, intent(in) :: id_zcs !< NetCDF variable id for the station zcoordinate_c
         integer, intent(in) :: id_zws !< NetCDF variable id for the station zcoordinate_w
         integer, intent(in) :: id_zwu !< NetCDF variable id for the station zcoordinate_wu
         integer, intent(in) :: it_his !< Timeframe to write to in the his file

         integer :: ierr !< Result status (NF90_NOERR if successful)

         integer :: layer

         ierr = DFM_NOERR

         if (.not. model_is_3D()) then
            return
         end if

         if (jawrizc == 1) then
            do layer = 1, kmx
               call check_netcdf_error(nf90_put_var(ihisfile, id_zcs, valobs(:, IPNT_ZCS + layer - 1), start=[layer, 1, it_his], count=[1, numobs + nummovobs, 1]))
            end do
         end if

         if (jawrizw == 1) then
            do layer = 1, kmx + 1
               call check_netcdf_error(nf90_put_var(ihisfile, id_zws, valobs(:, IPNT_ZWS + layer - 1), start=[layer, 1, it_his], count=[1, numobs + nummovobs, 1]))
               call check_netcdf_error(nf90_put_var(ihisfile, id_zwu, valobs(:, IPNT_ZWU + layer - 1), start=[layer, 1, it_his], count=[1, numobs + nummovobs, 1]))
            end do
         end if

      end function unc_put_his_station_coord_vars_z

      !> Define variables for WAQ statistic outputs (not to be confused with the general statistical output framework).
      !! They are not part of the statistical output framework, because it is useless to allow statistics about statistics,
      !! because writing output only in the final time step is currently not supported in statistical output,
      !! and because statistic outputs may be redundant in the future when the statistical output framework is feature complete.
      function unc_def_his_station_waq_statistic_outputs(waq_statistics_ids) result(ierr)
         use dfm_error, only: DFM_NOERR

         integer, allocatable, intent(out) :: waq_statistics_ids(:) !< NetCDF ids for the water quality statistic output variables
         integer :: ierr !< D-Flow FM error code

         character(len=255) :: variable_name, description
         character(len=1024) :: station_coordinate_string
         integer :: statistics_index, output_index
         integer, allocatable :: nc_dimensions(:), specific_nc_dimensions(:)

         ierr = DFM_NOERR

         allocate (waq_statistics_ids(noout_statt + noout_state))

         if (model_is_3D()) then
            nc_dimensions = [id_laydim, id_statdim, id_timedim]
            station_coordinate_string = trim(statcoordstring)//' zcoordinate_c'
         else
            nc_dimensions = [id_statdim, id_timedim]
            station_coordinate_string = statcoordstring
         end if

         do statistics_index = 1, noout_statt + noout_state
            if (statistics_index > noout_statt) then
               specific_nc_dimensions = nc_dimensions(1:size(nc_dimensions) - 1) ! Drop time dimension for end statistics (stat-e) variables
            else
               specific_nc_dimensions = nc_dimensions
            end if
            output_index = statistics_index + noout_user
            variable_name = ' '
            write (variable_name, "('water_quality_stat_',I0)") statistics_index
            call definencvar(ihisfile, waq_statistics_ids(statistics_index), nc_precision, specific_nc_dimensions, &
                             trim(variable_name), trim(wq_user_outputs%names(output_index)), &
                             trim(wq_user_outputs%units(output_index)), trim(station_coordinate_string), 'station_geom', fillVal=dmiss)
            description = trim(wq_user_outputs%names(output_index))//' - '//trim(wq_user_outputs%description(output_index))//' in flow element'
            call replace_multiple_spaces_by_single_spaces(description)
            call check_netcdf_error(nf90_put_att(ihisfile, waq_statistics_ids(statistics_index), 'description', description))
         end do
      end function unc_def_his_station_waq_statistic_outputs

      !> Write data to WAQ statistic output variables (not to be confused with the general statistical output framework).
      function unc_put_his_station_waq_statistic_outputs(waq_statistics_ids) result(ierr)
         use dfm_error, only: DFM_NOERR
         use precision

         integer, intent(in) :: waq_statistics_ids(:) !< NetCDF ids for the water quality statistic output variables
         integer :: ierr !< D-Flow FM error code

         integer, allocatable :: nc_start(:), nc_count(:)
         integer :: start_index_valobs, statistics_index, num_layers

         ierr = DFM_NOERR

         ! Default start and count for stat-t variables
         if (model_is_3D()) then
            nc_start = [1, 1, it_his]
            nc_count = [kmx, ntot, 1]
         else
            nc_start = [1, it_his]
            nc_count = [ntot, 1]
         end if

         do statistics_index = 1, noout_statt + noout_state
            if (statistics_index == noout_statt + 1) then
               if (comparereal(tim, ti_hise, eps10) < 0) then
                  return ! The end statistic outputs (stat-e) are only written in the last his time step
               end if
               if (model_is_3D()) then
                  nc_start = [1, 1]
                  nc_count = [kmx, ntot]
               else
                  nc_start = [1]
                  nc_count = [ntot]
               end if
            end if

            num_layers = max(kmx, 1)

            start_index_valobs = IPNT_HWQ1 - 1 + (noout_user + statistics_index - 1) * num_layers + 1
            call check_netcdf_error(nf90_put_var(ihisfile, waq_statistics_ids(statistics_index), &
                                                 transpose(valobs(:, start_index_valobs:start_index_valobs + num_layers - 1)), &
                                                 start=nc_start, count=nc_count))
         end do
      end function unc_put_his_station_waq_statistic_outputs

      !> Write (put) the geometry x/y-coordinate variables for the station type.
      function unc_put_his_station_geom_coord_vars_xy(ihisfile, numobs, it_his, id_geom_node_count, id_geom_node_coordx, id_geom_node_coordy, &
                                                      add_latlon, id_geom_node_coordlon, id_geom_node_coordlat) result(ierr)
         use m_observations_data, only: xobs, yobs
         use dfm_error, only: DFM_NOERR

         integer, intent(in) :: ihisfile !< NetCDF id of already open dataset
         integer, intent(in) :: numobs !< Number of fixed observation stations
         integer, intent(in) :: it_his !< Timeframe to write to in the his file
         integer, intent(in) :: id_geom_node_count !< NetCDF variable id created for the node count of the structures of this type
         integer, intent(in) :: id_geom_node_coordx !< NetCDF variable id created for the station geometry node x-coordinate
         integer, intent(in) :: id_geom_node_coordy !< NetCDF variable id created for the station geometry node y-coordinate
         logical, intent(in) :: add_latlon !< Whether or not to add extra lon/lat coordinates for the nodes
         !< (only applicable when the coordx/y variables contain projected coordinates,
         !< and requires id_node_lon/lat to be passed as well).
         integer, intent(in) :: id_geom_node_coordlon !< NetCDF variable id created for the station geometry node longitude coordinate
         integer, intent(in) :: id_geom_node_coordlat !< NetCDF variable id created for the station geometry node latitude coordinate

         integer :: ierr !< Result status (NF90_NOERR if successful)

         integer, dimension(numobs) :: node_count

         ierr = DFM_NOERR

         ! Write geometry variables only at the first time of history output
         if (it_his /= 1) then
            return
         end if

         node_count = 1

         call check_netcdf_error(nf90_put_var(ihisfile, id_geom_node_count, node_count))
         call check_netcdf_error(nf90_put_var(ihisfile, id_geom_node_coordx, xobs(:), start=[1], count=[numobs]))
         call check_netcdf_error(nf90_put_var(ihisfile, id_geom_node_coordy, yobs(:), start=[1], count=[numobs]))

#ifdef HAVE_PROJ
         if (add_latlon) then
            call transform_and_put_latlon_coordinates(ihisfile, id_geom_node_coordlon, id_geom_node_coordlat, &
                                                      nccrs%proj_string, xobs, yobs)
         end if
#endif

      end function unc_put_his_station_geom_coord_vars_xy
   end subroutine unc_write_his

   !> Convert t_station_nc_dimensions to integer array of NetCDF dimension ids
   function build_nc_dimension_id_list(nc_dim_ids) result(res)
      use m_output_config, only: t_station_nc_dimensions
      use MessageHandling, only: mess, LEVEL_ERROR
      type(t_station_nc_dimensions), intent(in) :: nc_dim_ids !< The active NetCDF dimensions for this variable
      integer, allocatable :: res(:) !< Array of NetCDF dimension ids

      res = pack([id_laydim, id_laydimw, id_nlyrdim, id_statdim, id_sedsusdim, id_sedtotdim, id_timedim], &
                 make_mask_from_dim_ids(nc_dim_ids))
      if (any(res == 0)) then
         call mess(LEVEL_ERROR, 'A dimension ID was used without being defined!')
      end if
   end function build_nc_dimension_id_list

   !> Return array of NetCDF dimension start indices corresponding to NetCDF dimensions
   function build_nc_dimension_id_start_array(nc_dim_ids, it_his) result(starts)
      use m_output_config, only: t_station_nc_dimensions
      type(t_station_nc_dimensions), intent(in) :: nc_dim_ids !< The active NetCDF dimensions for this variable
      integer, intent(in) :: it_his !< Snapshot index of the history file
      integer, allocatable :: starts(:) !< Array of start indices for each NetCDF dimension

      starts = pack([1, 1, 1, 1, 1, 1, it_his], &
                    make_mask_from_dim_ids(nc_dim_ids))
   end function build_nc_dimension_id_start_array

   !> Return array of NetCDF dimension counts corresponding to NetCDF dimensions
   function build_nc_dimension_id_count_array(nc_dim_ids, ihisfile) result(counts)
      use m_output_config, only: t_station_nc_dimensions
      type(t_station_nc_dimensions), intent(in) :: nc_dim_ids !< The active NetCDF dimensions for this variable
      integer, intent(in) :: ihisfile !< NetCDF id of already open dataset
      integer, allocatable :: counts(:) !< NetCDF dimension counts

      integer, allocatable :: dim_ids(:)

      dim_ids = build_nc_dimension_id_list(nc_dim_ids)
      counts = [(get_dimid_len(ihisfile, dim_ids(i)), integer :: i=1, size(dim_ids))]
      if (nc_dim_ids%timedim) then
         counts(size(counts)) = 1 ! Only write one element for time dimension, which comes last
      end if
   end function build_nc_dimension_id_count_array

   !> Build mask of which dimensions to include in netcdf variable, based on nc_dim_ids
   pure function make_mask_from_dim_ids(nc_dim_ids) result(mask)
      use m_output_config, only: t_station_nc_dimensions
      type(t_station_nc_dimensions), intent(in) :: nc_dim_ids !< The active NetCDF dimensions for this variable
      logical :: mask(7) !< The same but as a 1-D array of logicals

      mask = [nc_dim_ids%laydim, &
              nc_dim_ids%laydim_interface_center .or. nc_dim_ids%laydim_interface_edge, &
              nc_dim_ids%nlyrdim, &
              nc_dim_ids%statdim, &
              nc_dim_ids%sedsusdim, &
              nc_dim_ids%sedtotdim, &
              nc_dim_ids%timedim]
   end function make_mask_from_dim_ids

   !> Gets dimension length from NetCDF dimension id
   integer function get_dimid_len(ihisfile, id)
      use netcdf, only: nf90_inquire_dimension
      use netcdf_utils, only: check_netcdf_error
      integer, intent(in) :: ihisfile !< NetCDF id of already open dataset
      integer, intent(in) :: id !< NetCDF id obtained from nf90_def_dim

      call check_netcdf_error(nf90_inquire_dimension(ihisfile, id, len=get_dimid_len))
   end function get_dimid_len

   subroutine write_station_netcdf_variable(output_variable_item, ihisfile, it_his)
      use netcdf, only: nf90_put_var
      use netcdf_utils, only: check_netcdf_error
      use m_reshape, only: reshape_implicit
      use MessageHandling, only: err
      use m_statistical_output_types, only: t_output_variable_item
      use m_output_config, only: t_output_quantity_config
      type(t_output_variable_item), intent(in) :: output_variable_item !< The output variable item to write
      integer, intent(in) :: ihisfile !< The history file id
      integer, intent(in) :: it_his !< The history file time index

      integer :: local_id_var, station_id_index
      integer, allocatable :: counts(:), starts(:), positions(:)
      double precision, allocatable :: transformed_data(:)

      local_id_var = output_variable_item%id_var

      associate (nc_dim_ids => output_variable_item%output_config%nc_dim_ids)
         if (.not. nc_dim_ids%statdim) then
            call err('Programming error, please report: Station NetCDF variable must have the station dimension')
         end if

         counts = build_nc_dimension_id_count_array(nc_dim_ids, ihisfile)
         starts = build_nc_dimension_id_start_array(nc_dim_ids, it_his)

         positions = [(i, integer :: i=1, size(counts))]
         station_id_index = findloc(build_nc_dimension_id_list(nc_dim_ids), value=id_statdim, dim=1)
      end associate

      ! Bring the dimension corresponding to stations to the front, because it comes first in valobs
      positions(1) = station_id_index
      positions(station_id_index) = 1
      ! Unflatten the array to its proper dimensions (counts), reorder the dimensions to place stations to the front, and flatten it back
      transformed_data = reshape_implicit(output_variable_item%stat_output, counts, positions)

      call check_netcdf_error(nf90_put_var(ihisfile, local_id_var, transformed_data, count=counts, start=starts))
   end subroutine write_station_netcdf_variable

   !> Write static data such as names, coordintates, and geometry of structures to the history file
   subroutine unc_put_his_structure_static_vars(ncid)
      use fm_external_forcings_data, only: weir2cgen, nweirgen, cgen_ids, pump_ids, npumpsg, gate_ids, ngatesg, ncgensg, genstru2cgen, ngenstru, dambreak_ids, ndambreaksignals, cdam_ids, ncdamsg, srcname, numsrc, gate2cgen, ngategen
      use unstruc_channel_flow, only: network
      use m_flowparameters, only: jahisweir, jahisorif, jahispump, jahisgate, jahiscgen, jahisuniweir, jahisdambreak, jahisculv, jahisbridge, jahiscmpstru, jahislongculv, jahiscdam, jahissourcesink, jahislateral
      use m_longculverts, only: longculverts, nlongculverts
      use m_GlobalParameters, only: ST_PUMP
      use m_structures, only: number_of_pump_nodes, jaoldstr
      use m_laterals, only: lat_ids, numlatsg
      use m_monitoring_crosssections, only: crs, ncrs
      use m_monitoring_runupgauges, only: rug, num_rugs
      use string_module, only: trimexact
      use m_observations_data, only: numobs, nummovobs, namobs
      integer, intent(in) :: ncid !< NetCDF id of already open dataset

      character(len=strlen_netcdf), dimension(:), allocatable :: structure_names
      integer, dimension(:), allocatable :: indices

      if (allocated(weir2cgen)) then
         indices = [(weir2cgen(i), integer :: i=1, nweirgen)]
         structure_names = [(trimexact(cgen_ids(indices(i)), strlen_netcdf), integer :: i=1, nweirgen)]
      else if (network%sts%numWeirs > 0) then
         indices = [(network%sts%weirIndices(i), integer :: i=1, nweirgen)]
         structure_names = [(trimexact(network%sts%struct(indices(i))%id, strlen_netcdf), integer :: i=1, nweirgen)]
      else
         allocate (structure_names(0))
      end if
      call unc_put_his_structure_names(ncid, jahisweir, id_weirgen_id, structure_names)

      indices = [(network%sts%orificeIndices(i), integer :: i=1, network%sts%numOrifices)]
      structure_names = [(trimexact(network%sts%struct(indices(i))%id, strlen_netcdf), integer :: i=1, network%sts%numOrifices)]
      call unc_put_his_structure_names(ncid, jahisorif, id_orifgen_id, structure_names)

      structure_names = [(pump_ids(i), integer :: i=1, npumpsg)]
      call unc_put_his_structure_names(ncid, jahispump, id_pump_id, structure_names)
      call unc_put_his_structure_mid_points(ncid, ST_PUMP, jahispump, npumpsg, 'line', id_poly_xmid=id_pump_xmid, id_poly_ymid=id_pump_ymid)

      structure_names = [(gate_ids(i), integer :: i=1, ngatesg)]
      call unc_put_his_structure_names(ncid, jahisgate, id_gate_id, structure_names)

      if (jaoldstr == 1) then
         structure_names = [(cgen_ids(i), integer :: i=1, ncgensg)]
      else if (network%sts%numGeneralStructures > 0) then
         indices = [(network%sts%generalStructureIndices(i), integer :: i=1, ngenstru)]
         structure_names = [(trimexact(network%sts%struct(indices(i))%id, strlen_netcdf), integer :: i=1, ngenstru)]
      else
         indices = [(genstru2cgen(i), integer :: i=1, ngenstru)]
         structure_names = [(cgen_ids(indices(i)), integer :: i=1, ngenstru)]
      end if
      call unc_put_his_structure_names(ncid, jahiscgen, id_genstru_id, structure_names)

      indices = [(network%sts%uniweirIndices(i), integer :: i=1, network%sts%numuniweirs)]
      structure_names = [(trimexact(network%sts%struct(indices(i))%id, strlen_netcdf), integer :: i=1, network%sts%numuniweirs)]
      call unc_put_his_structure_names(ncid, jahisuniweir, id_uniweir_id, structure_names)

      structure_names = [(dambreak_ids(i), integer :: i=1, ndambreaksignals)]
      call unc_put_his_structure_names(ncid, jahisdambreak, id_dambreak_id, structure_names)

      indices = [(network%sts%culvertIndices(i), integer :: i=1, network%sts%numCulverts)]
      structure_names = [(trimexact(network%sts%struct(indices(i))%id, strlen_netcdf), integer :: i=1, network%sts%numCulverts)]
      call unc_put_his_structure_names(ncid, jahisculv, id_culvert_id, structure_names)

      indices = [(network%sts%bridgeIndices(i), integer :: i=1, network%sts%numBridges)]
      structure_names = [(trimexact(network%sts%struct(indices(i))%id, strlen_netcdf), integer :: i=1, network%sts%numBridges)]
      call unc_put_his_structure_names(ncid, jahisbridge, id_bridge_id, structure_names)

      structure_names = [(network%cmps%compound(i)%id, integer :: i=1, network%cmps%count)]
      call unc_put_his_structure_names(ncid, jahiscmpstru, id_cmpstru_id, structure_names)

      structure_names = [(longculverts(i)%id, integer :: i=1, nlongculverts)]
      call unc_put_his_structure_names(ncid, jahislongculv, id_longculvert_id, structure_names)

      structure_names = [(cdam_ids(i), integer :: i=1, ncdamsg)]
      call unc_put_his_structure_names(ncid, jahiscdam, id_cdam_id, structure_names)

      structure_names = [(namobs(i), integer :: i=1, numobs + nummovobs)]
      call unc_put_his_structure_names(ncid, 1, id_statname, structure_names)

      structure_names = [(crs(i)%name, integer :: i=1, ncrs)]
      call unc_put_his_structure_names(ncid, 1, id_crs_id, structure_names)

      structure_names = [(rug(i)%name, integer :: i=1, num_rugs)]
      call unc_put_his_structure_names(ncid, 1, id_rugname, structure_names)

      structure_names = [(srcname(i), integer :: i=1, numsrc)]
      call unc_put_his_structure_names(ncid, jahissourcesink, id_srcname, structure_names)

      indices = [(gate2cgen(i), integer :: i=1, ngategen)]
      structure_names = [(cgen_ids(indices(i)), integer :: i=1, ngategen)]
      call unc_put_his_structure_names(ncid, jahisgate, id_gategen_id, structure_names)

      structure_names = [(lat_ids(i), integer :: i=1, numlatsg)]
      call unc_put_his_structure_names(ncid, jahislateral, id_lat_id, structure_names)
   end subroutine unc_put_his_structure_static_vars

   !> Get the NetCDF variable prefix and human-readable name of a structure type from its type id
   subroutine get_prefix_and_name_from_struc_type_id(struc_type_id, prefix, name)
      use MessageHandling, only: mess, LEVEL_ERROR
      use m_GlobalParameters, only: ST_UNSET, ST_WEIR, ST_ORIFICE, ST_PUMP, ST_GATE, ST_GENERAL_ST, ST_UNI_WEIR, ST_DAMBREAK, ST_CULVERT, ST_BRIDGE, ST_COMPOUND, ST_LONGCULVERT, ST_DAM, ST_OBS_STATION, ST_CROSS_SECTION, ST_RUNUP_GAUGE, ST_SOURCE_SINK, ST_GATEGEN, ST_LATERAL
      integer, intent(in) :: struc_type_id !< The id of the type of the structure (e.g. ST_CULVERT)
      character(len=*), intent(out) :: prefix !< Base name of this structure type, e.g., 'uniweir'
      character(len=*), intent(out) :: name !< Human readable name of this structure type, e.g., 'universal weir'

      select case (struc_type_id)
      case default
         call mess(LEVEL_ERROR, 'Programming error, please report: unrecognised struc_type_id in unc_write_his/get_prefix_and_name_from_struc_type_id')
      case (ST_UNSET)
         call mess(LEVEL_ERROR, 'Programming error, please report: unrecognised struc_type_id in unc_write_his/get_prefix_and_name_from_struc_type_id')
      case (ST_WEIR)
         prefix = 'weirgen'
         name = 'weir'
      case (ST_ORIFICE)
         prefix = 'orifice'
         name = 'orifice'
      case (ST_PUMP)
         prefix = 'pump'
         name = 'pump'
      case (ST_GATE)
         prefix = 'gate'
         name = 'gate'
      case (ST_GENERAL_ST)
         prefix = 'general_structure'
         name = 'general structure'
      case (ST_UNI_WEIR)
         prefix = 'uniweir'
         name = 'universal weir'
      case (ST_DAMBREAK)
         prefix = 'dambreak'
         name = 'dambreak'
      case (ST_CULVERT)
         prefix = 'culvert'
         name = 'culvert'
      case (ST_BRIDGE)
         prefix = 'bridge'
         name = 'bridge'
      case (ST_COMPOUND)
         prefix = 'cmpstru'
         name = 'compound structure'
      case (ST_LONGCULVERT)
         prefix = 'longculvert'
         name = 'long culvert'
      case (ST_DAM)
         prefix = 'cdam'
         name = 'controllable dam'
      case (ST_OBS_STATION)
         prefix = 'station'
         name = 'observation station'
      case (ST_CROSS_SECTION)
         prefix = 'cross_section'
         name = 'observation cross section'
      case (ST_RUNUP_GAUGE)
         prefix = 'runup_gauge'
         name = 'runup gauge'
      case (ST_SOURCE_SINK)
         prefix = 'source_sink'
         name = 'source and sink'
      case (ST_GATEGEN)
         prefix = 'gategen'
         name = 'gate'
      case (ST_LATERAL)
         prefix = 'lateral'
         name = 'lateral'
      end select
   end subroutine get_prefix_and_name_from_struc_type_id

   !> Write ('put') the static variables for a single structure type.
   subroutine unc_put_his_structure_names(ncid, output_enabled, nc_id_structure_names, structure_names)
      use netcdf, only: nf90_put_var
      use netcdf_utils, only: check_netcdf_error
      integer, intent(in) :: ncid !< NetCDF id of already open dataset
      integer, intent(in) :: output_enabled !< Whether or not (1/0) this structure's output must be written.
      integer, intent(in) :: nc_id_structure_names !< NetCDF variable id created for the character names of the structures of this type
      character(len=*), dimension(:), intent(in) :: structure_names !< Identifying names of the structures

      integer :: i

      if (output_enabled == 0) then
         return
      end if

      do i = 1, size(structure_names)
         call check_netcdf_error(nf90_put_var(ncid, nc_id_structure_names, trim(structure_names(i)), [1, i]))
      end do
   end subroutine unc_put_his_structure_names

   !> Write ('put') the static variables for a single structure type.
   subroutine unc_put_his_structure_mid_points(ncid, struc_type_id, output_enabled, count, geom_type, id_poly_xmid, id_poly_ymid)
      use string_module, only: strcmpi
      integer, intent(in) :: ncid !< NetCDF id of already open dataset
      integer, intent(in) :: struc_type_id !< The id of the type of the structure (e.g. ST_CULVERT)
      integer, intent(in) :: output_enabled !< Whether or not (1/0) this structure's output must be written.
      integer, intent(in) :: count !< Number of structures for this structure_type
      character(len=*), intent(in) :: geom_type !< Geometry type, one of: 'point', 'line', 'polygon' (or 'none')
      !integer, intent(in) :: ngeom_node !< Total number of geometry nodes for this structure_type
      !integer, intent(in) :: id_strdim !< NetCDF dimension id created for this structure type
      !integer, optional, intent(in) :: id_geom_node_count !< NetCDF variable id created for the node count of the structures of this type
      !integer, optional, intent(in) :: id_geom_coordx !< NetCDF variable id created for the node x coordinates for all structures of this type
      !integer, optional, intent(in) :: id_geom_coordy !< NetCDF variable id created for the node y coordinates for all structures of this type
      !logical, optional, intent(in) :: add_latlon !< Whether or not to add extra lon/lat coordinates for the nodes
      !< (only applicable when the coordx/y variables contain projected coordinates,
      !< and requires id_node_lon/lat to be passed as well).
      !integer, optional, intent(in) :: id_geom_coordlon !< NetCDF variable id created for the node longitude coordinates for all structures of this type
      !integer, optional, intent(in) :: id_geom_coordlat !< NetCDF variable id created for the node latitude  coordinates for all structures of this type
      integer, optional, intent(in) :: id_poly_xmid !< NetCDF variable id created for the x-coordinate of the structure's polyline midpoint
      integer, optional, intent(in) :: id_poly_ymid !< NetCDF variable id created for the y-coordinate of the structure's polyline midpoint

      if (output_enabled == 0 .or. count == 0) then
         return
      end if
      ! TODO (UNST-7900): actually write structure geometry data here!

      ! Polyline midpoint coordinates
      if (strcmpi(geom_type, 'line')) then
         call unc_put_his_structure_static_vars_polyline_midpoints(ncid, struc_type_id, count, id_poly_xmid, id_poly_ymid)
      end if
   end subroutine unc_put_his_structure_mid_points

   !> Write ('put') the static variables for a single structure type.
   !! Store one single representative x/y point for each structure in the his-file,
   !! because CF conventions require that for variables on discrete geometries.
   !! Computed at half the total length of the snapped flow links
   !! (so, it lies on an edge, not per se on the input polyline)).
   subroutine unc_put_his_structure_static_vars_polyline_midpoints(ncid, struc_type_id, count, id_poly_xmid, id_poly_ymid)
      use precision, only: dp
      use netcdf, only: nf90_put_var
      use netcdf_utils, only: check_netcdf_error
      use m_structures, only: retrieve_set_of_flowlinks_for_polyline_structure, calc_midpoint_coords_of_set_of_flowlinks
      integer, intent(in) :: ncid !< NetCDF id of already open dataset
      integer, intent(in) :: struc_type_id !< The id of the type of the structure (e.g. ST_CULVERT)
      integer, intent(in) :: count !< Number of structures for this structure_type
      integer, intent(in) :: id_poly_xmid !< NetCDF variable id created for the x-coordinate of the structure's polyline midpoint
      integer, intent(in) :: id_poly_ymid !< NetCDF variable id created for the y-coordinate of the structure's polyline midpoint

      integer :: i_struc
      integer, dimension(:), allocatable :: links !< The set of flowlinks that this structure has been snapped to
      real(kind=dp) :: xmid, ymid

      do i_struc = 1, count
         call retrieve_set_of_flowlinks_for_polyline_structure(struc_type_id, i_struc, links)
         call calc_midpoint_coords_of_set_of_flowlinks(links, xmid, ymid)
         ! Write the coordinates of this structure's midpoint to the his-file
         call check_netcdf_error(nf90_put_var(ncid, id_poly_xmid, xmid, [i_struc]))
         call check_netcdf_error(nf90_put_var(ncid, id_poly_ymid, ymid, [i_struc]))
      end do
   end subroutine unc_put_his_structure_static_vars_polyline_midpoints
end module m_unc_write_his
