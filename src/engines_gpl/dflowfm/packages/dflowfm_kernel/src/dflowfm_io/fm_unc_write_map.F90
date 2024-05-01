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
   
module fm_unc_write_map

implicit none

contains

!> Writes a single snapshot of the unstructured flow net + flow data to a netCDF file.
!! If file exists, it will be overwritten. Therefore, only use this routine
!! for separate snapshots, the automated map file should be filled by calling
!! unc_write_map_filepointer directly instead!
subroutine unc_write_map(filename, iconventions)
   use m_flowparameters, only: jamapbnd
   use unstruc_netcdf, only: t_unc_mapids, UNC_CONV_CFOLD, UNC_CONV_UGRID, unc_create, unc_close, check_error
   use netcdf, only: nf90_noerr
   use MessageHandling, only: mess, LEVEL_ERROR

   character(len=*),  intent(in) :: filename
   integer, optional, intent(in) :: iconventions        !< Unstructured NetCDF conventions (either UNC_CONV_CFOLD or UNC_CONV_UGRID)

   type(t_unc_mapids) :: mapids
   integer :: ierr, iconv
   integer :: jabndnd

   if (.not. present(iconventions)) then
      iconv = UNC_CONV_CFOLD
   else
      iconv = iconventions
   endif

   ierr = unc_create(filename, 0, mapids%ncid)
   if (ierr /= nf90_noerr) then
       call mess(LEVEL_ERROR, 'Could not create map file '''//trim(filename)//'''.')
       call check_error(ierr)
       return
   endif

   if (iconv == UNC_CONV_UGRID) then
      jabndnd = 0
      if (jamapbnd > 0) jabndnd = 1
      call unc_write_map_filepointer_ugrid(mapids, 0d0, jabndnd)
   else
      call unc_write_map_filepointer(mapids%ncid, 0d0, 1)
   endif

   ierr = unc_close(mapids%ncid)
end subroutine unc_write_map


!> Writes map/flow data to an already opened netCDF dataset. NEW version according to UGRID conventions + much cleanup.
!! The netnode and -links have been written already.
subroutine unc_write_map_filepointer_ugrid(mapids, tim, jabndnd) ! wrimap
   use m_flow
   use m_flowtimes
   use m_flowgeom
   use m_heatfluxes
   use m_sferic
   use network_data
   use m_sediment
   use m_bedform
   use m_wind
   use m_flowparameters, only: jatrt, ibedlevtyp
   use m_mass_balance_areas
   use m_fm_wq_processes
   use m_xbeach_data
   use m_transportdata
   use m_alloc
   use m_waves, hminlw_waves=>hminlw
   use m_missing
   use m_CrossSections
   use unstruc_channel_flow, only: network
   use string_module, only: replace_multiple_spaces_by_single_spaces
   use m_save_ugrid_state, only: mesh1dname, mesh2dname
   use m_hydrology_data, only : jadhyd, ActEvap, PotEvap, interceptionmodel, DFM_HYD_NOINTERCEPT, InterceptHs
   use m_subsidence, only: jasubsupl, subsout, subsupl, subsupl_t0
   use Timers
   use m_map_his_precision
   use m_fm_icecover, only: ice_mapout, ice_af, ice_h, ice_p, ice_t, snow_h, snow_t, ja_icecover, ICECOVER_SEMTNER
   use unstruc_netcdf, only: UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_S, UNC_LOC_U, UNC_LOC_W, UNC_LOC_WU, UNC_LOC_CN, UNC_LOC_L, MAX_ID_VAR, &
                             t_unc_mapids, ug_addglobalatts, ug_meta_fm, unc_meta_add_user_defined, unc_add_time_coverage, &
                             unc_nounlimited, unc_def_var_nonspatial, unc_def_var_map, unc_put_att, unc_cmode, unc_put_var_map, &
                             write_array_with_dmiss_for_dry_cells_into_netcdf_file, write_array_with_dmiss_for_dry_faces_into_netcdf_file, &
                             check_error, unc_write_flowgeom_filepointer_ugrid, linktonode2
   use MessageHandling, only: mess, LEVEL_WARN, LEVEL_ERROR, msgbuf, err_flush
   use netcdf, only: nf90_inquire, nf90_def_dim, nf90_unlimited, nf90_double, nf90_put_att, nf90_int, nf90_char, nf90_def_var, &
                     nf90_enddef, nf90_evarsize, nf90_netcdf4, nf90_noerr, nf90_put_var
   use io_ugrid, only: ug_addglobalatts
   use m_enum_mesh_dimensions_ids, only: mdim_two
   use m_debug, only: jawritedebug, debugarr1d, debugarr2d, debugarr3d
   use string_module, only: replace_char

   type(t_unc_mapids), intent(inout) :: mapids               !< Set of file and variable ids for this map-type file.
   real(kind=hp),      intent(in)    :: tim
   integer, optional,  intent(in)    :: jabndnd              !< Whether to include boundary nodes (1) or not (0). Default: no.

   integer                           :: jabndnd_             !< Flag specifying whether boundary nodes are to be written.
   integer                           :: ndxndxi              !< Last node to be saved. Equals ndx when boundary nodes are written, or ndxi otherwise.
   integer, save                     :: ierr, ndim

   double precision, allocatable                       :: ust_x(:), ust_y(:), wavout(:), wavout2(:), scaled_rain(:)
   character(len=255)                                  :: tmpstr
   integer                                             :: nm
   integer                                             :: Lf
   character(16)                                       :: dxname
   character(64)                                       :: dxdescr
   character(15)                                       :: transpunit
   double precision                                    :: rhol, mortime, wavfac
   double precision                                    :: moravg, dmorft, dmorfs, rhodt
   double precision                                    :: um, ux, uy
   double precision, dimension(:,:), allocatable       :: poros, toutputx, toutputy, sxtotori, sytotori
   double precision, dimension(:,:,:), allocatable     :: frac
   integer, dimension(:), allocatable                  :: flag_val
   character(len=10000)                                :: flag_mean

   double precision, dimension(:), allocatable         :: numlimdtdbl
   double precision, dimension(:), allocatable         :: work1d
   double precision                                    :: vicc, dicc

   double precision, dimension(:), pointer             :: dens
   
!    Secondary Flow
!        id_rsi, id_rsiexact, id_dudx, id_dudy, id_dvdx, id_dvdy, id_dsdx, id_dsdy

   integer :: i, j, jj, itim, n, LL, L, Lb, Lt, k, k1, k2
   integer :: id_twodim
   integer :: kk, kb, kt, kkk, found, iloc
   integer :: nlayb, nrlay
   integer :: Ltx, nlaybL, nrlayLx
   integer :: iLocS ! Either UNC_LOC_S or UNC_LOC_S3D, depending on whether layers are present.
   integer :: iLocU ! Either UNC_LOC_U or UNC_LOC_U3D, depending on whether layers are present.
   integer :: isrc, kbot_, ktop_, nk, nkbot, nktop
   double precision, dimension(:), allocatable :: windx, windy, toutput, rks, wa
   double precision :: zwu0
   character( len = 4 ) :: str

   type(t_CSType), pointer                       :: pCS
   type(t_CSType), pointer, dimension(:)         :: pCSs
   integer                                       :: ndx1d
   integer, save                                 :: jmax, nCrs
   double precision, dimension(:,:), allocatable :: work1d_z, work1d_n
   double precision, dimension(:,:,:), allocatable :: work3d, work3d2
   character(3)                                  :: sednr     !< string representation of sediment fraction number
   character(256)                                :: varname   !< name of netCDF variable
   character(1024)                               :: longname  !< long, descriptive name of netCDF variable content
   
   integer            :: nc_precision
   integer, parameter :: FIRST_ARRAY = 1
   integer, parameter :: SECOND_ARRAY = 2

   nc_precision = netcdf_data_type(md_nc_map_precision)

   if (ndxi <= 0) then
      call mess(LEVEL_WARN, 'No flow elements in model, will not write flow geometry.')
      return
   endif
   if (timon) call timstrt ( "unc_write_map_filepointer_ugrid", handle_extra(70))

   if (present(jabndnd)) then
      jabndnd_ = jabndnd
   else
      jabndnd_ = 0
   endif

   ! Include boundary cells in output (ndx) or not (ndxi)
   if (jabndnd_ == 1) then
      ndxndxi   = ndx
   else
      ndxndxi   = ndxi
   endif

   ndx1d = ndxi - ndx2d

   ! Prepare the U/S location for either 2D or 3D for subsequent def_var and put_var sequences.
   if (kmx > 0) then ! If layers present.
      iLocS = UNC_LOC_S3D
      iLocU = UNC_LOC_U3D
   else
      iLocS = UNC_LOC_S
      iLocU = UNC_LOC_U
   endif


   call realloc(mapids%id_const, (/ MAX_ID_VAR, NUMCONST/), keepExisting=.false.)

   ! Use nr of dimensions in netCDF file a quick check whether vardefs were written
   ! before in previous calls.
   ndim = 0
   ierr = nf90_inquire(mapids%ncid, nDimensions=ndim)

   ! Only write net and flow geometry data the first time, or for a separate map file.
   if (ndim == 0) then
      if (timon) call timstrt ( "unc_write_flowgeom_filepointer_ugrid INIT", handle_extra(71))

      ierr = ug_addglobalatts(mapids%ncid, ug_meta_fm)

      ierr = unc_meta_add_user_defined(mapids%ncid)

      call unc_write_flowgeom_filepointer_ugrid(mapids%ncid, mapids%id_tsp, jabndnd_)

      ierr = unc_add_time_coverage(mapids%ncid, ti_maps, ti_mape, ti_map)

      ! Current time t1
      if (unc_nounlimited > 0) then
         ierr = nf90_def_dim(mapids%ncid, 'time', ceiling((ti_mape-ti_maps)/ti_map) + 1, mapids%id_tsp%id_timedim)
      else
         ierr = nf90_def_dim(mapids%ncid, 'time', nf90_unlimited, mapids%id_tsp%id_timedim)
      endif

      call check_error(ierr, 'def time dim')
      ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_time, nf90_double, (/ mapids%id_tsp%id_timedim /), 'time', 'time', '', trim(Tudunitstr))
      mapids%id_tsp%idx_curtime = 0


      ! Size of latest timestep
      ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_timestep, nc_precision, (/ mapids%id_tsp%id_timedim /), 'timestep', '',     'Latest computational timestep size in each output interval', 's')

      if (jamapnumlimdt > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_numlimdt   , nc_precision, UNC_LOC_S, 'Numlimdt'  , '', 'Number of times flow element was Courant limiting', '1', cell_method = 'point', jabndnd=jabndnd_)
      endif

      ! Time dependent grid layers
      if (kmx > 0 .and. jafullgridoutput == 1) then
         ! Face-centred z-coordinates:
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowelemzcc, nc_precision, UNC_LOC_S3D, 'flowelem_zcc', 'altitude', 'Vertical coordinate of layer centres at pressure points'   , 'm' , jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowelemzw , nc_precision, UNC_LOC_W  , 'flowelem_zw' , 'altitude', 'Vertical coordinate of layer interfaces at pressure points', 'm' , jabndnd=jabndnd_)

         if (ndx2d > 0) then ! Borrow the "2-dimension" from the already defined mesh (either 2d or 1d, does not matter)
            id_twodim = mapids%id_tsp%meshids2d%dimids(mdim_two)
         else
            id_twodim = mapids%id_tsp%meshids1d%dimids(mdim_two)
      endif

         ! Bounds variable for face-centred z-coordinates:
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowelemzcc_bnd, nc_precision, UNC_LOC_S3D, 'flowelem_zcc_bnd', 'altitude', 'Bounds of vertical coordinate of layers at pressure points'   , 'm' , &
            dimids = (/ id_twodim, -3, -2, -1 /), jabndnd=jabndnd_)
         ierr = nf90_put_att(mapids%ncid, mapids%id_flowelemzcc(2), 'bounds', trim(mesh2dname)//'_flowelem_zcc_bnd')
         ierr = nf90_put_att(mapids%ncid, mapids%id_flowelemzcc(1), 'bounds', trim(mesh1dname)//'_flowelem_zcc_bnd')

         ! Edge-centred z-coordinates:
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowlinkzu, nc_precision, UNC_LOC_U3D, 'flowlink_zu', 'altitude', 'Vertical coordinate of layer centres at velocity points'   , 'm' , jabndnd=jabndnd_)

         ! Bounds variable for edge-centred z-coordinates:
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowlinkzu_bnd, nc_precision, UNC_LOC_U3D, 'flowlink_zu_bnd', 'altitude', 'Bounds of vertical coordinate of layers at velocity points'   , 'm' , &
            dimids = (/ id_twodim, -3, -2, -1 /), jabndnd=jabndnd_)
         ierr = nf90_put_att(mapids%ncid, mapids%id_flowlinkzu(2), 'bounds', trim(mesh2dname)//'_flowlink_zu_bnd')
         ierr = nf90_put_att(mapids%ncid, mapids%id_flowlinkzu(1), 'bounds', trim(mesh1dname)//'_flowlink_zu_bnd')
      endif

      ! Water levels
      if (jamaps1 > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s1, nc_precision, UNC_LOC_S, 's1',         'sea_surface_height',                'Water level', 'm', jabndnd=jabndnd_)
      endif
      if (jamaps0 > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s0, nc_precision, UNC_LOC_S, 's0', 'sea_surface_height', 'Water level on previous timestep', 'm', jabndnd=jabndnd_)
      endif

      ! Influx
      if (jamapqin > 0 .and. jaqin > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qin, nc_precision, UNC_LOC_S, 'qin', '', 'Sum of all water influx', 'm3 s-1', jabndnd=jabndnd_)
      endif

      if (jamapFlowAnalysis > 0) then
         ! Flow analysis
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_negdpt,       nc_precision, UNC_LOC_S, 'negdpt',         '', 'Number of times negative depth was calculated', '1', cell_method = 'point', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_negdpt_cum,   nc_precision, UNC_LOC_S, 'negdpt_cum',     '', 'Cumulative number of times negative depth was calculated', '1', cell_method = 'point', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_noiter,       nc_precision, UNC_LOC_S, 'noiter',         '', 'Number of times no nonlinear convergence was caused', '1', cell_method = 'point', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_noiter_cum,   nc_precision, UNC_LOC_S, 'noiter_cum',     '', 'Cumulative number of times no nonlinear convergence was caused', '1', cell_method = 'point', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_limtstep,     nc_precision, UNC_LOC_S, 'limtstep',       '', 'Number of times a node was limiting for the computational time step', '1', cell_method = 'point', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_limtstep_cum, nc_precision, UNC_LOC_S, 'limtstep_cum',   '', 'Cumulative number of times a node was limiting for the computational time step', '1', cell_method = 'point', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_courant,      nc_precision, UNC_LOC_S, 'courant',     '', 'Courant number', '1', cell_method = 'point', jabndnd=jabndnd_)
      endif

      ! Evaporation
      if (jamapevap > 0) then
         if (jadhyd == 1) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_potevap, nc_precision, UNC_LOC_S, 'potevap', 'water_potential_evaporation_flux', 'Potential evaporation rate at pressure points', 'm s-1', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_actevap, nc_precision, UNC_LOC_S, 'actevap', 'lwe_water_evaporation_rate', 'Actual evaporation rate at pressure points', 'm s-1', jabndnd=jabndnd_) ! Intentionally did not use standard_name='water_potential_evaporation_flux', because that one requires other units: kg m-2 s-1.
         endif
         if (jaevap == 1) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_evap, nc_precision, UNC_LOC_S, 'prescrevap', '', 'Prescribed evaporation rate at pressure points', 'm s-1', jabndnd=jabndnd_)
         endif
      endif


      ! Volumes
      if (jamapvol1 > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vol1, nc_precision, iLocS, 'vol1',         '',                'volume of water in grid cell', 'm3', jabndnd=jabndnd_)
      endif

      ! Calculated time step per cell based on CFL number
      if (jamapdtcell > 0) then
          ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dtcell, nc_precision, iLocS,  'dtcell', '', 'Time step per cell based on CFL', 's', jabndnd=jabndnd_)
      endif

      ! Water depths
      if (jamaphs > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hs, nc_precision, UNC_LOC_S, 'waterdepth', 'sea_floor_depth_below_sea_surface', 'Water depth at pressure points', 'm', jabndnd=jabndnd_)
      endif

      if (jamaphu > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hu, nc_precision, UNC_LOC_U, 'hu', 'sea_floor_depth_below_sea_surface', 'water depth at velocity points', 'm', jabndnd=jabndnd_)
      endif

      ! Velocities
      if (jamapau > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_au, nc_precision, iLocU, 'au',         '',                'normal flow area between two neighbouring grid cells', 'm2', jabndnd=jabndnd_)
      endif

      if (jamapu1 > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_u1, nc_precision, iLocU, 'u1', '', 'Velocity at velocity point, n-component', 'm s-1', jabndnd=jabndnd_)
         ierr = unc_put_att(mapids%ncid, mapids%id_u1, 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
      endif
      if (jamapu0 > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_u0, nc_precision, iLocU, 'u0', '', 'Velocity at velocity point at previous time step, n-component', 'm s-1', jabndnd=jabndnd_)
         ierr = unc_put_att(mapids%ncid, mapids%id_u0, 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
      endif
      if (jamapucvec > 0) then
         if (jaeulervel==1 .and. jawave>0 .and. .not. flowWithoutWaves) then ! TODO: AvD:refactor such that yes<->no Eulerian velocities are in parameters below:
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucx, nc_precision, iLocS, 'ucx', 'sea_water_x_eulerian_velocity',      'Flow element center eulerian velocity vector, x-component', 'm s-1', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucy, nc_precision, iLocS, 'ucy', 'sea_water_y_eulerian_velocity',      'Flow element center eulerian velocity vector, y-component', 'm s-1', jabndnd=jabndnd_)
         else
            if (jsferic == 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucx, nc_precision, iLocS, 'ucx', 'sea_water_x_velocity',      'Flow element center velocity vector, x-component', 'm s-1', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucy, nc_precision, iLocS, 'ucy', 'sea_water_y_velocity',      'Flow element center velocity vector, y-component', 'm s-1', jabndnd=jabndnd_)
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucx, nc_precision, iLocS, 'ucx', 'eastward_sea_water_velocity',      'Flow element center velocity vector, x-component', 'm s-1', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucy, nc_precision, iLocS, 'ucy', 'northward_sea_water_velocity',      'Flow element center velocity vector, y-component', 'm s-1', jabndnd=jabndnd_)
            endif
         endif
         if (kmx > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucz, nc_precision, UNC_LOC_S3D, 'ucz', 'upward_sea_water_velocity', 'Flow element center velocity vector, z-component', 'm s-1', jabndnd=jabndnd_)
            ! Depth-averaged cell-center velocities in 3D:
            if (jsferic == 0) then
               if (jaeulervel==1 .and. jawave>0) then
                  ! GLM indication needed to report that depth-averaged values are always GLM, even when eulervelocities==1
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxa, nc_precision, UNC_LOC_S, 'ucxa', 'sea_water_glm_x_velocity', 'Flow element center GLM depth-averaged velocity, x-component', 'm s-1', jabndnd=jabndnd_) ! depth-averaged magnitude has no stokes drift
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucya, nc_precision, UNC_LOC_S, 'ucya', 'sea_water_glm_y_velocity', 'Flow element center GLM depth-averaged velocity, y-component', 'm s-1', jabndnd=jabndnd_)
               else
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxa, nc_precision, UNC_LOC_S, 'ucxa', 'sea_water_x_velocity', 'Flow element center depth-averaged velocity, x-component', 'm s-1', jabndnd=jabndnd_)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucya, nc_precision, UNC_LOC_S, 'ucya', 'sea_water_y_velocity', 'Flow element center depth-averaged velocity, y-component', 'm s-1', jabndnd=jabndnd_)
               endif
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxa, nc_precision, UNC_LOC_S, 'ucxa', 'eastward_sea_water_velocity', 'Flow element center depth-averaged velocity, x-component', 'm s-1', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucya, nc_precision, UNC_LOC_S, 'ucya', 'northward_sea_water_velocity', 'Flow element center depth-averaged velocity, y-component', 'm s-1', jabndnd=jabndnd_)
            endif
         endif
      endif
      if (jamapucmag > 0) then
         if (jaeulervel==1 .and. jawave>0 .and. .not. flowWithoutWaves) then ! TODO: AvD:refactor such that yes<->no Eulerian velocities are in parameters below:
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmag, nc_precision, iLocS, 'ucmag', 'sea_water_eulerian_speed', 'Flow element center eulerian velocity magnitude', 'm s-1', jabndnd=jabndnd_)
         else
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmag, nc_precision, iLocS, 'ucmag', 'sea_water_speed', 'Flow element center velocity magnitude', 'm s-1', jabndnd=jabndnd_)
         endif
         if (kmx > 0) then
            if (jaeulervel==1 .and. jawave>0) then ! TODO: AvD:refactor such that yes<->no Eulerian velocities are in parameters below:
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmaga, nc_precision, UNC_LOC_S, 'ucmaga', 'sea_water_speed', 'Flow element center depth-averaged GLM velocity magnitude', 'm s-1', jabndnd=jabndnd_)  ! depth-averaged magnitude has no stokes drift
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmaga, nc_precision, UNC_LOC_S, 'ucmaga', 'sea_water_speed', 'Flow element center depth-averaged velocity magnitude', 'm s-1', jabndnd=jabndnd_)
            endif
         endif
      endif
      if (jamapucqvec > 0) then
         if (jaeulervel==1 .and. jawave>0 .and. .not. flowWithoutWaves) then ! TODO: AvD:refactor such that yes<->no Eulerian velocities are in parameters below:
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxq, nc_precision, iLocS, 'ucxq', 'ucxq_eulerian_velocity', 'Flow element center eulerian velocity vector based on discharge, x-component', 'm s-1', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucyq, nc_precision, iLocS, 'ucyq', 'ucyq_eulerian_velocity', 'Flow element center eulerian velocity vector based on discharge, y-component', 'm s-1', jabndnd=jabndnd_)
         else
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxq, nc_precision, iLocS, 'ucxq', 'ucxq_velocity', 'Flow element center velocity vector based on discharge, x-component', 'm s-1', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucyq, nc_precision, iLocS, 'ucyq', 'ucyq_velocity', 'Flow element center velocity vector based on discharge, y-component', 'm s-1', jabndnd=jabndnd_)
         endif
      endif
      if (kmx > 0) then
         if (jamapww1 > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ww1, nc_precision, UNC_LOC_W, 'ww1', 'upward_sea_water_velocity', 'Upward velocity on vertical interface, n-component', 'm s-1', jabndnd=jabndnd_)
         endif
         if (jamaprho > 0) then
             if ( density_is_pressure_dependent() ) then
                 ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rho,  nc_precision, UNC_LOC_S3D, 'density', 'sea_water_density',           'Flow element center mass density',      'kg m-3', jabndnd=jabndnd_)
             else
                 ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rhop, nc_precision, UNC_LOC_S3D, 'rho',     'sea_water_potential_density', 'Flow element center potential density', 'kg m-3', jabndnd=jabndnd_)
             endif
         endif
      endif

      if (jamapq1 > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1, nc_precision, iLocU, 'q1', 'discharge', 'Discharge through flow link at current time', 'm3 s-1', cell_method = 'sum', jabndnd=jabndnd_)
         ierr = unc_put_att(mapids%ncid, mapids%id_q1, 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
      endif

      if (jamapq1main > 0 .and. allocated(q1_main)) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1main, nc_precision, iLocU, 'q1_main', '', 'Main channel discharge through flow link at current time', 'm3 s-1', cell_method = 'sum', jabndnd=jabndnd_)
         ierr = unc_put_att(mapids%ncid, mapids%id_q1main, 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
      endif

      if (jamapfw > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_fwel, nc_precision, UNC_LOC_U, 'fixed weir energy loss', '', 'Fixed weir energy loss', 'm', jabndnd=jabndnd_)
         ierr = unc_put_att(mapids%ncid, mapids%id_fwel, '', '')
      endif

      if (jamapviu > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_viu, nc_precision, iLocU, 'viu', '', 'Horizontal eddy viscosity', 'm2 s-1', jabndnd=jabndnd_)
      endif
      if (jamapdiu > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_diu, nc_precision, iLocU, 'diu', '', 'Horizontal eddy diffusivity', 'm2 s-1', jabndnd=jabndnd_)
      endif

      ! Bed shear stress
      if (jamaptaucurrent > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_tausx     , nc_precision, UNC_LOC_S, 'tausx'  , '', 'Total bed shear stress vector, x-component', 'N m-2', jabndnd=jabndnd_)   ! vect shear stress
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_tausy     , nc_precision, UNC_LOC_S, 'tausy'  , '', 'Total bed shear stress vector, y-component', 'N m-2', jabndnd=jabndnd_)   ! vect shear stress
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_taus      , nc_precision, UNC_LOC_S, 'taus'   , '', 'Total bed shear stress magnitude', 'N m-2', jabndnd=jabndnd_)
         if (stm_included) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_tausmax   , nc_precision, UNC_LOC_S, 'tausmax'  , '', 'Bed shear stress magnitude for morphology', 'N m-2', jabndnd=jabndnd_)   ! max shear stress
         endif
      endif

      if ( jamaptidep > 0 .and. jatidep > 0 ) then
          ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tidep, nc_precision, UNC_LOC_S, &
               'TidalPotential', 'TidalPotential', 'Tidal Potential generated by celestial forces in flow element center', 'm2 s-2', &
               jabndnd=jabndnd_)
     endif
     if ( jamapselfal > 0 ) then
        if ( jaselfal >  0 ) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_salp, nc_precision, UNC_LOC_S, &
                'SALPotential', 'SALPotential', 'Self-attraction and loading Potential in flow element center', 'm2 s-2', jabndnd=jabndnd_)
        endif
     endif

     if ( jaFrcInternalTides2D > 0 .and. jamapIntTidesDiss > 0 ) then
        ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_IntTidesDiss, nc_precision, UNC_LOC_S, &
            'internal_tides_dissipation', 'internal_tides_dissipation', 'internal tides dissipation in flow element center',&
            'J s-1 m-2', jabndnd=jabndnd_)
     endif

      ! Chezy data on flow nodes and flow links
      ! Input roughness value and type on flow links for input check (note: overwritten when jatrt==1)
      if (jamap_chezy_elements > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_czs , nc_precision, UNC_LOC_S, 'czs'  , '', 'Chezy roughness in flow element center', 'm0.5s-1', jabndnd=jabndnd_)
            ! WO: m0.5s-1 does not follow standard ? (which accepts only integral powers?)
      endif
      if (jamap_chezy_links > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_czu , nc_precision, UNC_LOC_U, 'czu'  , '', 'Chezy roughness on flow links', 'm0.5s-1', jabndnd=jabndnd_)
      endif
      if (jamap_chezy_input > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_cfu , nc_precision, UNC_LOC_U, 'cfu'  , '', 'Input roughness on flow links', '-', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_cfutyp , nf90_int, UNC_LOC_U, 'cfutyp'  , '', 'Input roughness type on flow links', '-', jabndnd=jabndnd_)
      endif


      ! Constituents
      if (jamapsal > 0 .and. jasal > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sa1, nc_precision, iLocS, 'sa1', 'sea_water_salinity', 'Salinity in flow element', '1e-3', jabndnd=jabndnd_)
      endif

      if (jamaptem > 0 .and. jatem > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tem1, nc_precision, iLocS, 'tem1', 'sea_water_temperature', 'Temperature in flow element', 'degC', jabndnd=jabndnd_)
      endif

      if (jamapspir > 0 .and. jasecflow > 0) then
         if (kmx < 1) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_spircrv, nc_precision, UNC_LOC_S, 'spircrv', 'streamline_curvature', 'Flow streamline curvature'  , '1/m', jabndnd=jabndnd_ )
         endif
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_spirint, nc_precision, UNC_LOC_S, 'spirint', 'spiral_intensity'    , 'Spiral flow intensity'       , 'm/s', jabndnd=jabndnd_)
      endif

      ! Tracers
      if (jamapconst > 0 .and. ITRA1 > 0) then
         call realloc(mapids%id_const, (/ MAX_ID_VAR, NUMCONST /), keepExisting=.false., fill = 0)
         do j=ITRA1,ITRAN
            tmpstr = const_names(j)
            ! Forbidden chars in NetCDF names: space, /, and more.
            call replace_char(tmpstr,32,95)
            call replace_char(tmpstr,47,95)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_const(:,j), nc_precision, iLocS, trim(tmpstr), &
                                   '', trim(const_names(j)) // ' in flow element', const_units(j), jabndnd=jabndnd_)
         enddo
      endif
      ! Discharges
      ! TODO: AVD...
      ! TIDAL TURBINES: Insert equivalent of addturbine_cnst and addturbine_time here

    ! water quality bottom variables
      if (numwqbots > 0) then
         call realloc(mapids%id_wqb, (/ 3, numwqbots /), keepExisting=.false., fill = 0)
         do j=1,numwqbots
            tmpstr = wqbotnames(j)
            ! Forbidden chars in NetCDF names: space, /, and more.
            call replace_char(tmpstr,32,95)
            call replace_char(tmpstr,47,95)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqb(:,j), nc_precision, UNC_LOC_S, trim(tmpstr), &
                                   '', trim(wqbotnames(j)) // ' in flow element', wqbotunits(j), jabndnd=jabndnd_)
         enddo
         if (wqbot3D_output == 1) then
            call realloc(mapids%id_wqb3d, (/ 3, numwqbots /), keepExisting=.false., fill = 0)
            do j=1,numwqbots
               tmpstr = wqbotnames(j)
               ! Forbidden chars in NetCDF names: space, /, and more.
               call replace_char(tmpstr,32,95)
               call replace_char(tmpstr,47,95)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqb3d(:,j), nc_precision, UNC_LOC_S3D, trim(tmpstr)//'_3D', &
                                      '', trim(wqbotnames(j)) // ' in flow element (3D)', wqbotunits(j), jabndnd=jabndnd_)
            enddo
         endif
      endif

      ! WAQ extra outputs
      if (jawaqproc > 0) then
         if (noout_map > 0) then
            call realloc(mapids%id_waq, (/ 3, noout_map /), keepExisting=.false., fill = 0)
            do j=1,noout_map
               tmpstr = ' '
               write (tmpstr, "('water_quality_output_',I0)") j
               ierr = unc_def_var_map(mapids%ncid,  mapids%id_tsp, mapids%id_waq(:,j), nc_precision, iLocS, tmpstr, &
                                      '', outputs%names(j), outputs%units(j), jabndnd=jabndnd_)
               tmpstr = trim(outputs%names(j))//' - '//trim(outputs%description(j))//' in flow element'
               call replace_multiple_spaces_by_single_spaces(tmpstr)
               ierr = nf90_put_att(mapids%ncid, mapids%id_waq(2,j),  'description'  , tmpstr)
            enddo
         endif
         if (noout_statt > 0) then
            call realloc(mapids%id_wqst, (/ 3, noout_statt /), keepExisting=.false., fill = 0)
            do j=1,noout_statt
               jj = noout_user + j
               tmpstr = ' '
               write (tmpstr, "('water_quality_stat_',I0)") j
               ierr = unc_def_var_map(mapids%ncid,  mapids%id_tsp, mapids%id_wqst(:,j), nc_precision, iLocS, tmpstr, &
                                      '', outputs%names(jj), outputs%units(jj), jabndnd=jabndnd_)
               tmpstr = trim(outputs%names(jj))//' - '//trim(outputs%description(jj))//' in flow element'
               call replace_multiple_spaces_by_single_spaces(tmpstr)
               ierr = nf90_put_att(mapids%ncid, mapids%id_wqst(2,j),  'description'  , tmpstr)
            enddo
         endif
         if (noout_state > 0) then
            call realloc(mapids%id_wqse, (/ 3, noout_state /), keepExisting=.false., fill = 0)
            do j=1,noout_state
               jj = noout_user + noout_statt + j
               tmpstr = ' '
               write (tmpstr, "('water_quality_stat_',I0)") noout_statt + j
               ierr = unc_def_var_map(mapids%ncid,  mapids%id_tsp, mapids%id_wqse(:,j), nc_precision, iLocS, tmpstr, &
                                      '', outputs%names(jj), outputs%units(jj), 0, jabndnd=jabndnd_)
               tmpstr = trim(outputs%names(jj))//' - '//trim(outputs%description(jj))//' in flow element'
               call replace_multiple_spaces_by_single_spaces(tmpstr)
               ierr = nf90_put_att(mapids%ncid, mapids%id_wqse(2,j),  'description'  , tmpstr)
            enddo
         endif
      endif

      ! mass balance areas
      if (nomba > 0) then
         ierr = unc_def_var_map(mapids%ncid,  mapids%id_tsp, mapids%id_mba(:), nf90_int, UNC_LOC_S, 'water_quality_mba', '', 'Water quality mass balance areas', '', is_timedep=0, jabndnd=jabndnd_)
         call realloc(flag_val, nomba, keepExisting = .false., fill = 0)
         flag_mean = ' '
         do j=nomba,1,-1
            flag_val(j) = j
            flag_mean = trim(mbaname(j))//' '//flag_mean
         enddo
         ierr = nf90_put_att(mapids%ncid, mapids%id_mba(2), 'flag_values', flag_val)
         ierr = nf90_put_att(mapids%ncid, mapids%id_mba(2), 'flag_meanings', flag_mean)
      endif

      ! Meteo forcings
      if (jamaprain > 0 .and. jarain /= 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rain,  nc_precision, UNC_LOC_S, 'rainfall_rate',  'rainfall_rate', 'Rainfall rate', 'm s-1', jabndnd=jabndnd_)
      endif

      ! interception
      if (jamapicept > 0 .and. interceptionmodel /= DFM_HYD_NOINTERCEPT) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_icepths,  nc_precision, UNC_LOC_S, 'interception_waterdepth',  '', 'Waterdepth in interception layer', 'm', jabndnd=jabndnd_)
      endif

      if (jamapwind > 0 .and. japatm /= 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_patm,  nc_precision, UNC_LOC_S, 'Patm',  'surface_air_pressure', 'Atmospheric pressure near surface', 'N m-2', jabndnd=jabndnd_)
      endif

      if (ice_mapout) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ice_af,  nf90_double, UNC_LOC_S, 'ice_af',  'sea_ice_area_fraction', 'Fraction of surface area covered by floating ice', '1', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ice_h,  nf90_double, UNC_LOC_S, 'ice_h',  'sea_ice_area_fraction', 'Thickness of the floating ice cover', 'm', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ice_p,  nf90_double, UNC_LOC_S, 'ice_p',  '', 'Pressure exerted by the floating ice cover', 'N m-2', jabndnd=jabndnd_)
         if (ja_icecover == ICECOVER_SEMTNER) then
            ! need to convert this to K if we want to comply with the CF standard name "sea_ice_temperature"
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ice_t,  nf90_double, UNC_LOC_S, 'ice_t',  '', 'Temperature of the floating ice cover', 'degC', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_snow_h, nf90_double, UNC_LOC_S, 'snow_h', '', 'Thickness of the snow layer', 'm', jabndnd=jabndnd_)
            ! need to convert this to K if we want to comply with the CF standard name "temperature_in_surface_snow"
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_snow_t, nf90_double, UNC_LOC_S, 'snow_t', '', 'Temperature of the snow layer', 'degC', jabndnd=jabndnd_)
         endif
      end if

      if (jawind > 0) then
         if (jamapwind > 0) then
            if (jsferic == 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windx,  nc_precision, UNC_LOC_S, 'windx',  'x_wind', 'velocity of air on flow element center, x-component', 'm s-1', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windy,  nc_precision, UNC_LOC_S, 'windy',  'y_wind', 'velocity of air on flow element center, y-component', 'm s-1', jabndnd=jabndnd_)
               ! Also wind on flow links
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windxu, nc_precision, UNC_LOC_U, 'windxu', 'x_wind', 'velocity of air on flow links, x-component', 'm s-1', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windyu, nc_precision, UNC_LOC_U, 'windyu', 'y_wind', 'velocity of air on flow links, y-component', 'm s-1', jabndnd=jabndnd_)
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windx,  nc_precision, UNC_LOC_S, 'windx',  'eastward_wind',  'velocity of air on flow element center, x-component', 'm s-1', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windy,  nc_precision, UNC_LOC_S, 'windy',  'northward_wind', 'velocity of air on flow element center, y-component', 'm s-1', jabndnd=jabndnd_)
               ! Also wind on flow links
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windxu, nc_precision, UNC_LOC_U, 'windxu', 'eastward_wind', 'velocity of air on flow links, x-component', 'm s-1', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windyu, nc_precision, UNC_LOC_U, 'windyu', 'northward_wind', 'velocity of air on flow links, y-component', 'm s-1', jabndnd=jabndnd_)
            endif
         endif
         if (jamapwindstress > 0) then
            if (jsferic == 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressx, nc_precision, UNC_LOC_S, 'windstressx',  &
                  'surface_downward_x_stress', 'wind stress on flow element center, x-component', 'N m-2', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressy, nc_precision, UNC_LOC_S, 'windstressy',  &
                  'surface_downward_y_stress', 'wind stress on flow element center, y-component', 'N m-2', jabndnd=jabndnd_)
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressx, nc_precision, UNC_LOC_S, 'windstressx',  &
                  'surface_downward_eastward_stress',  'wind stress on flow element center, x-component', 'N m-2', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressy, nc_precision, UNC_LOC_S, 'windstressy',  &
                  'surface_downward_northward_stress', 'wind stress on flow element center, y-component', 'N m-2', jabndnd=jabndnd_)
            endif
         endif
      endif

      if (ja_airdensity + ja_computed_airdensity > 0 .and. jamap_airdensity > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp   , mapids%id_airdensity   , nc_precision, UNC_LOC_S, 'rhoair' , 'air_density'      , 'Air density'     , 'kg m-3', jabndnd=jabndnd_)
      endif

      ! Heat fluxes
      if (jamapheatflux > 0 .and. jatem > 1) then ! here less verbose

         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp   , mapids%id_tair   , nc_precision, UNC_LOC_S, 'Tair' , 'surface_temperature'      , 'Air temperature near surface'     , 'degC', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp   , mapids%id_rhum   , nc_precision, UNC_LOC_S, 'Rhum' , 'surface_specific_humidity', 'Relative humidity near surface'    , '', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp   , mapids%id_clou   , nc_precision, UNC_LOC_S, 'Clou' , 'cloud_area_fraction'      , 'Cloudiness'                       , '1', jabndnd=jabndnd_)

         if (jatem == 5) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qsun  , nc_precision, UNC_LOC_S, 'Qsun'  , 'surface_net_downward_shortwave_flux'                     , 'Solar influx'                         , 'W m-2', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Qeva  , nc_precision, UNC_LOC_S, 'Qeva'  , 'surface_downward_latent_heat_flux'                       , 'Evaporative heat flux'                , 'W m-2', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Qcon  , nc_precision, UNC_LOC_S, 'Qcon'  , 'surface_downward_sensible_heat_flux'                     , 'Sensible heat flux'                   , 'W m-2', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Qlong , nc_precision, UNC_LOC_S, 'Qlong' , 'surface_net_downward_longwave_flux'                      , 'Long wave back radiation'             , 'W m-2', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Qfreva, nc_precision, UNC_LOC_S, 'Qfreva', 'downward_latent_heat_flux_in_sea_water_due_to_convection', 'Free convection evaporative heat flux', 'W m-2', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Qfrcon, nc_precision, UNC_LOC_S, 'Qfrcon', 'surface_downward_sensible_heat_flux_due_to_convection'   , 'Free convection sensible heat flux'   , 'W m-2', jabndnd=jabndnd_)
         endif

         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_Qtot   , nc_precision, UNC_LOC_S, 'Qtot'  , 'surface_downward_heat_flux_in_sea_water'                 , 'Total heat flux'                      , 'W m-2', jabndnd=jabndnd_)

      endif

      ! Turbulence.
      if (jamaptur > 0 .and. kmx > 0) then
         if (iturbulencemodel >= 3) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_turkin1, nc_precision, UNC_LOC_WU, 'turkin1', 'specific_turbulent_kinetic_energy_of_sea_water', 'turbulent kinetic energy',          'm2 s-2', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vicwwu,  nc_precision, UNC_LOC_WU, 'vicwwu',  'eddy_viscosity', 'turbulent vertical eddy viscosity at velocity points', 'm2 s-1', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vicwws,  nc_precision, UNC_LOC_W,  'vicwws',  'eddy_viscosity', 'turbulent vertical eddy viscosity at pressure points', 'm2 s-1', jabndnd=jabndnd_)
            if (iturbulencemodel == 3) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tureps1, nc_precision, UNC_LOC_WU, 'tureps1', 'specific_turbulent_kinetic_energy_dissipation_in_sea_water',    'turbulent energy dissipation', 'm2 s-3', jabndnd=jabndnd_)
            else if (iturbulencemodel == 4) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tureps1, nc_precision, UNC_LOC_WU, 'tureps1', '', 'turbulent time scale',         's-1', jabndnd=jabndnd_)
            endif
         endif
      endif

      ! Sediment transport (via morphology module)
      if ((jamapsed > 0 .and. jased > 0 .and. stm_included).or.(jasubsupl>0)) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_mor_bl   , nc_precision, UNC_LOC_S, 'mor_bl'  , '', 'Time-varying bottom level in flow cell center', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
      endif
      !
      if (jasubsupl>0) then
         select case (ibedlevtyp)
            case (1)
               iloc = UNC_LOC_S
            case (2)
               iloc = UNC_LOC_U
            case (3,4,5,6)
               iloc = UNC_LOC_CN
         end select
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_subsupl, nc_precision, iloc, 'subsupl'  , '', 'Cumulative subsidence/uplift', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
      endif
      !
      if (jamapz0>0) then
         ! roughness heights for current and current and wave related roughness
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_z0c   , nc_precision, UNC_LOC_U, 'z0ucur'  , '', 'Current related roughness height'        , 'm', dimids = (/ -2,  -1 /), jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_z0r   , nc_precision, UNC_LOC_U, 'z0urou'  , '', 'Current-wave related roughness height'   , 'm', dimids = (/ -2,  -1 /), jabndnd=jabndnd_)
      endif
      !
      if (jamapsed > 0 .and. jased > 0 .and. stm_included) then
         ierr = nf90_def_dim(mapids%ncid, 'nSedTot', stmpar%lsedtot, mapids%id_tsp%id_sedtotdim)
         ierr = nf90_def_dim(mapids%ncid, 'nSedSus', stmpar%lsedsus, mapids%id_tsp%id_sedsusdim)
         ierr = nf90_def_dim(mapids%ncid, 'nBedLayers', stmpar%morlyr%settings%nlyr, mapids%id_tsp%id_nlyrdim)
         ierr = nf90_def_dim(mapids%ncid, 'nStringlen', 100, mapids%id_tsp%id_strlendim)
         !
         if (.not. stmpar%morpar%moroutput%cumavg) then   ! only one average transport value at end of model run
            ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_sedavgtim, nc_precision, (/  1  /), 'sedAvgTim', '', 'Time interval over which cumulative transports are calculated', 's')
         endif
         !
         call realloc(mapids%id_dxx, (/stmpar%morpar%nxx, 3 /), keepExisting=.false.)
         !
         ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_morfac, nc_precision, (/ mapids%id_tsp%id_timedim /), 'morfac', '', 'Average morphological factor over elapsed morphological time', '-')
         ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_morft, nc_precision,  (/ mapids%id_tsp%id_timedim /), 'morft',  '', 'Current morphological time', 's')
         !
         ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_frac_name, nf90_char,  (/ mapids%id_tsp%id_strlendim, mapids%id_tsp%id_sedtotdim /), 'sedfrac_name', '', 'Sediment fraction name', '-')
         if (stmpar%lsedsus > 0) then
            ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_susfrac_name, nf90_char,  (/ mapids%id_tsp%id_strlendim, mapids%id_tsp%id_sedsusdim /), 'sussedfrac_name', '', 'Suspended sediment fraction name', '-')
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
         ! Suspended transport related quantities
         !
         if (stmpar%lsedsus .gt. 0) then
            !
            if ( kmx > 0 ) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_kmxsed, nf90_int, UNC_LOC_S, 'kmxsed', '', 'Bottom layer for sed calculations', '-', dimids = (/  -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ws, nc_precision, UNC_LOC_W, 'ws', '', 'Sediment settling velocity', 'm s-1', dimids = (/ -3, -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ws, nc_precision, UNC_LOC_S, 'ws', '', 'Sediment settling velocity', 'm s-1', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            endif
            !
            if (kmx == 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rsedeq, nc_precision, UNC_LOC_S, 'rsedeq', '', 'Equilibrium sediment concentration', 'kg m-3', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            endif
            !
            if (stmpar%morpar%moroutput%aks) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_aks, nc_precision, UNC_LOC_S, 'aks', '', 'Near-bed reference concentration height', 'm', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rca, nc_precision, UNC_LOC_S, 'rca', '', 'Near-bed reference concentration', 'kg m-3', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            endif
            !
            if (stmpar%morpar%moroutput%sourcesink) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sourse , nc_precision, UNC_LOC_S, 'sourse'  , '', 'Source term suspended sediment fractions', 'kg m-3 s-1', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sinkse , nc_precision, UNC_LOC_S, 'sinkse'  , '', 'Sink term suspended sediment fractions', 's-1', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            endif
            !
            if ( kmx > 0 ) then
               if (stmpar%morpar%moroutput%suvcor) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_scrn , nc_precision, UNC_LOC_U, 'e_scrn'  , '', 'Near-bed transport correction in face-normal direction', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
               endif
            endif
            !
            if (kmx > 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sedfrac, nc_precision, UNC_LOC_S3D, 'sedfrac_concentration', '', 'Sediment concentration in flow cell', 'kg m-3',dimids = (/ -3, -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sedfrac, nc_precision, UNC_LOC_S, 'sedfrac_concentration', '', 'Sediment concentration in flow cell', 'kg m-3',dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            endif
            !
         endif

         ! intermediate output for sediment formulas
         if (stmpar%morpar%moroutput%sedpar) then
            call realloc(mapids%id_sedpar, (/3, stmpar%trapar%npar, stmpar%lsedtot/), keepExisting=.false.)
            do l = 1, stmpar%lsedtot
               write(sednr,'(I3.3)') l
               do k = 1, stmpar%trapar%noutpar(l)
                  varname = trim(stmpar%trapar%outpar_name(k,l))//trim(sednr)
                  longname = trim(stmpar%trapar%outpar_longname(k,l))//' for '//trim(stmpar%sedpar%namsed(l))
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sedpar(:,k,l), nf90_double, UNC_LOC_S, trim(varname), '', trim(longname) , '', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
               enddo
            enddo
         endif

         ! default sediment transport output (suspended and bedload) on flow links
         if (stmpar%lsedsus > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ssn   , nc_precision, UNC_LOC_U, 'ssn'  , '', 'Suspended load transport, n-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sst   , nc_precision, UNC_LOC_U, 'sst'  , '', 'Suspended load transport, t-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)

         endif

         if (stmpar%lsedtot > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbn   , nc_precision, UNC_LOC_U, 'sbn'  , '', 'Bed load transport, n-component'         , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbt   , nc_precision, UNC_LOC_U, 'sbt'  , '', 'Bed load transport, t-component'         , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
         endif

         if (stmpar%morpar%moroutput%dzduuvv) then ! bedslope
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_e_dzdn , nc_precision, UNC_LOC_U, 'e_dzdn'  , '', 'Bed slope, n-component', '-', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_e_dzdt , nc_precision, UNC_LOC_U, 'e_dzdt'  , '', 'Bed slope, t-component', '-', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         endif

         if (stmpar%morpar%moroutput%uuuvvv) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_uuu , nc_precision, UNC_LOC_S, 'uuu'  , '', 'Characteristic velocity in cell centre, x-component', 'm s-1', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_vvv , nc_precision, UNC_LOC_S, 'vvv'  , '', 'Characteristic velocity in cell centre, y-component', 'm s-1', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         endif

         if (stmpar%morpar%moroutput%umod) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_umod , nc_precision, UNC_LOC_S, 'umod'  , '', 'Characteristic velocity magnitude in cell centre', 'm s-1', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         endif

         if (stmpar%morpar%moroutput%zumod) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_zumod , nc_precision, UNC_LOC_S, 'zumod'  , '', 'Height above bed for characteristic velocity in cell centre', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         endif

         if (stmpar%morpar%moroutput%ustar) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ustar , nc_precision, UNC_LOC_S, 'ustar'  , '', 'Bed shear velocity in cell centre', 'm s-1', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         endif

         if (stmpar%morpar%moroutput%sbcuv) then
            if (stmpar%morpar%moroutput%rawtransports) then    ! if either of these is true, the reconstruction is done outside this subroutine, invalidating Willem's approach to have 'unspoiled' transports
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcx   , nc_precision, UNC_LOC_S, 'sbcx'  , '', 'Bed load transport due to currents, x-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcy   , nc_precision, UNC_LOC_S, 'sbcy'  , '', 'Bed load transport due to currents, y-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
            endif
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcx_reconstructed   , nc_precision, UNC_LOC_S, 'sbcx_reconstructed'  , '', 'Bed load transport due to currents (reconstructed), x-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcy_reconstructed   , nc_precision, UNC_LOC_S, 'sbcy_reconstructed'  , '', 'Bed load transport due to currents (reconstructed), y-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
         endif

         if (stmpar%morpar%moroutput%sbwuv) then
            if (stmpar%morpar%moroutput%rawtransports) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwx   , nc_precision, UNC_LOC_S, 'sbwx'  , '', 'Bed load transport due to waves, x-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwy   , nc_precision, UNC_LOC_S, 'sbwy'  , '', 'Bed load transport due to waves, y-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
            endif
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwx_reconstructed   , nc_precision, UNC_LOC_S, 'sbwx_reconstructed'  , '', 'Bed load transport due to waves (reconstructed), x-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwy_reconstructed   , nc_precision, UNC_LOC_S, 'sbwy_reconstructed'  , '', 'Bed load transport due to waves (reconstructed), y-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
         endif

         if (stmpar%morpar%moroutput%sscuv) then    ! This differs from Delft3D 4
            if (stmpar%morpar%moroutput%rawtransports) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscx   , nc_precision, UNC_LOC_S, 'sscx'  , '', 'Suspended load transport due to currents, x-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscy   , nc_precision, UNC_LOC_S, 'sscy'  , '', 'Suspended load transport due to currents, y-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            endif
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscx_reconstructed   , nc_precision, UNC_LOC_S, 'sscx_reconstructed'  , '', 'Suspended load transport due to currents (reconstructed), x-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscy_reconstructed   , nc_precision, UNC_LOC_S, 'sscy_reconstructed'  , '', 'Suspended load transport due to currents (reconstructed), y-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
         endif

         if (stmpar%morpar%moroutput%sswuv) then
            if (stmpar%morpar%moroutput%rawtransports) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswx   , nc_precision, UNC_LOC_S, 'sswx'  , '', 'Suspended load transport due to waves, x-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswy   , nc_precision, UNC_LOC_S, 'sswy'  , '', 'Suspended load transport due to waves, y-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            endif
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswx_reconstructed   , nc_precision, UNC_LOC_S, 'sswx_reconstructed'  , '', 'Suspended load transport due to waves (reconstructed), x-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswy_reconstructed   , nc_precision, UNC_LOC_S, 'sswy_reconstructed'  , '', 'Suspended load transport due to waves (reconstructed), y-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
         endif

         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sxtot   , nc_precision, UNC_LOC_S, 'sxtot'  , '', 'Total sediment transport in flow cell center (reconstructed), x-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sytot   , nc_precision, UNC_LOC_S, 'sytot'  , '', 'Total sediment transport in flow cell center (reconstructed), y-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)

         ! Time averaged sediment transport values
         if (stmpar%morpar%moroutput%cumavg) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbxcum   , nc_precision, UNC_LOC_S, 'sbxcum'  , '', 'Time-averaged bed load transport, x-component',       transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbycum   , nc_precision, UNC_LOC_S, 'sbycum'  , '', 'Time-averaged bed load transport, y-component',       transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ssxcum   , nc_precision, UNC_LOC_S, 'ssxcum'  , '', 'Time-averaged suspended load transport, x-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ssycum   , nc_precision, UNC_LOC_S, 'ssycum'  , '', 'Time-averaged suspended load transport, y-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
         else
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbxcum   , nc_precision, UNC_LOC_S, 'sbxcum'  , '', 'Time-averaged bed load transport, x-component',       transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbycum   , nc_precision, UNC_LOC_S, 'sbycum'  , '', 'Time-averaged bed load transport, y-component',       transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ssxcum   , nc_precision, UNC_LOC_S, 'ssxcum'  , '', 'Time-averaged suspended load transport, x-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ssycum   , nc_precision, UNC_LOC_S, 'ssycum'  , '', 'Time-averaged suspended load transport, y-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim /), jabndnd=jabndnd_)
         endif

         select case (stmpar%morlyr%settings%iunderlyr)
            case (1)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_bodsed  , nc_precision, UNC_LOC_S, 'bodsed'  , '', 'Available sediment mass in the bed in flow cell center', 'kg m-2', dimids = (/ mapids%id_tsp%id_sedtotdim, -2, -1 /), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dpsed   , nc_precision, UNC_LOC_S, 'dpsed'  , '', 'Sediment thickness in the bed in flow cell center', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
            case (2)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_msed    , nc_precision, UNC_LOC_S, 'msed'  , '', 'Available sediment mass in a layer of the bed in flow cell center', 'kg m-2', dimids = (/ mapids%id_tsp%id_sedtotdim, mapids%id_tsp%id_nlyrdim, -2, -1 /), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_thlyr   , nc_precision, UNC_LOC_S, 'thlyr'  , '', 'Thickness of a layer of the bed in flow cell center', 'm', dimids = (/ mapids%id_tsp%id_nlyrdim, -2, -1 /), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_lyrfrac , nc_precision, UNC_LOC_S, 'lyrfrac'  , '', 'Volume fraction in a layer of the bed in flow cell center', '-', dimids = (/ mapids%id_tsp%id_sedtotdim, mapids%id_tsp%id_nlyrdim, -2, -1 /), jabndnd=jabndnd_)
               !
               if (stmpar%morlyr%settings%iporosity>0) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_poros, nc_precision, UNC_LOC_S, 'poros'  , '', 'Porosity of a layer of the bed in flow cell center', '-', dimids = (/ mapids%id_tsp%id_nlyrdim, -2, -1 /), jabndnd=jabndnd_)
               endif
               !
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_preload , nc_precision, UNC_LOC_S, 'preload'  , '', 'Historical largest load on layer of the bed in flow cell center', 'kg', dimids = (/ mapids%id_tsp%id_nlyrdim, -2, -1 /), jabndnd=jabndnd_)
         end select
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sedshort, nc_precision, UNC_LOC_S, 'sedshort' , '', 'Sediment shortage of transport layer in flow cell center', 'kg m-2', dimids = (/ mapids%id_tsp%id_sedtotdim, -2, -1 /), jabndnd=jabndnd_)
         !
         if (stmpar%morpar%moroutput%taub) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_taub  , nc_precision, UNC_LOC_S, 'taub'  , '', 'Bed shear stress for morphology', 'N m-2', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         endif
         if (stmpar%morpar%moroutput%taurat) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_taurat  , nc_precision, UNC_LOC_S, 'taurat'  , '', 'Excess bed shear ratio', '-', dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
         endif
         if (stmpar%morpar%moroutput%dm) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dm  , nc_precision, UNC_LOC_S, 'dm'  , '', 'Arithmetic mean sediment diameter', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         endif
         if (stmpar%morpar%moroutput%dg) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dg  , nc_precision, UNC_LOC_S, 'dg'  , '', 'Geometric mean sediment diameter', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         endif
         if (stmpar%morpar%moroutput%dgsd) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dgsd  , nc_precision, UNC_LOC_S, 'dgsd'  , '', 'Geometric standard deviation of particle size mix', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         endif
         if (stmpar%morpar%moroutput%percentiles) then
            do l = 1, stmpar%morpar%nxx
               write(dxname,'(A,I2.2)') 'DXX',l
               write(dxdescr,'(A,F4.1,A)') 'Sediment diameter percentile '    , stmpar%morpar%xx(l)*100d0,' %'
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dxx(l,:)  , nc_precision, UNC_LOC_S, dxname  , '', dxdescr, 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
            enddo
         endif
         if (stmpar%morpar%moroutput%frac) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_frac  , nc_precision, UNC_LOC_S, 'frac'  , '', 'Availability fraction in top layer', '-', dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
         endif
         if (stmpar%morpar%moroutput%mudfrac) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_mudfrac  , nc_precision, UNC_LOC_S, 'mudfrac'  , '', 'Mud fraction in top layer', '-', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         endif
         if (stmpar%morpar%moroutput%sandfrac) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sandfrac  , nc_precision, UNC_LOC_S, 'sandfrac'  , '', 'Sand fraction in top layer', '-', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         endif
         if (stmpar%morpar%moroutput%fixfac) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_fixfac  , nc_precision, UNC_LOC_S, 'fixfac'  , '', 'Reduction factor due to limited sediment thickness', '-', dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
         endif
         if (stmpar%morpar%moroutput%hidexp) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_hidexp  , nc_precision, UNC_LOC_S, 'hidexp'  , '', 'Hiding and exposure factor', '-', dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
         endif
         !
         if (stmpar%morpar%flufflyr%iflufflyr>0 .and. stmpar%lsedsus>0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_mfluff  , nc_precision, UNC_LOC_S, 'mfluff'  , '', 'Sediment mass in fluff layer', 'kg m-2', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
         endif
         !
         ! 1D cross sections
         if (ndx1d > 0 .and. stm_included) then
            if (stmpar%morpar%bedupd) then
               nCrs = 0
               do i = 1,size(network%crs%cross)
                  if (network%crs%cross(i)%crossindx == 0) exit
                  nCrs = nCrs + 1
               enddo
               pCSs => network%CSDefinitions%CS
               jmax = 0
               do i = 1,size(pCSs)
                  if (pCSs(i)%levelscount == 0) exit
                  jmax = max(jmax,pCSs(i)%levelscount)
               enddo
               ierr = nf90_def_dim(mapids%ncid, trim(mesh1dname)//'_crs_maxdim', jmax, mapids%id_tsp%id_jmax)
               ierr = nf90_def_dim(mapids%ncid, trim(mesh1dname)//'_ncrs'      , nCrs, mapids%id_tsp%id_nCrs)
               ierr = nf90_def_var(mapids%ncid, trim(mesh1dname)//'_mor_crs_z', nc_precision, (/ mapids%id_tsp%id_jmax, mapids%id_tsp%id_nCrs, mapids%id_tsp%id_timedim /), mapids%id_tsp%id_flowelemcrsz(1))
               ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_flowelemcrsz(1), 'long_name','time-varying cross-section points level')
               ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_flowelemcrsz(1), 'unit', 'm')
               ierr = nf90_def_var(mapids%ncid, trim(mesh1dname)//'_mor_crs_n', nc_precision, (/ mapids%id_tsp%id_jmax, mapids%id_tsp%id_nCrs, mapids%id_tsp%id_timedim /), mapids%id_tsp%id_flowelemcrsn(1))
               ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_flowelemcrsn(1), 'long_name','time-varying cross-section points width')
               ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_flowelemcrsn(1), 'unit', 'm')
               ierr = nf90_def_var(mapids%ncid, trim(mesh1dname)//'_mor_crs_name', nf90_char, (/ mapids%id_tsp%id_strlendim, mapids%id_tsp%id_nCrs /), mapids%id_tsp%id_morCrsName)
               ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_morCrsName, 'long_name','name of cross-section')
            endif
            if (stmpar%morpar%moroutput%blave) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_blave, nc_precision, UNC_LOC_S, 'bl_ave', '', 'Main channel averaged bed level', 'm', dimids = (/ -2, -1 /), which_meshdim = 1, jabndnd=jabndnd_)
               !ierr = nf90_def_var(mapids%ncid, trim(mesh1dname)//'_bl_ave', nf90_double, (/ mapids%id_tsp%id_ndx1d, mapids%id_tsp%id_timedim /), mapids%id_tsp%id_blave)
               !ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_blave, 'long_name','Main channel averaged bed level')
               !ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_blave, 'unit', 'm')
            endif
            if (stmpar%morpar%moroutput%bamor) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_bamor, nc_precision, UNC_LOC_S, 'mor_area', '', 'Main channel cell area', 'm2', is_timedep = 0, which_meshdim = 1, jabndnd=jabndnd_)
            endif
            if (stmpar%morpar%moroutput%wumor) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wumor, nc_precision, UNC_LOC_U, 'mor_width_u', '', 'Main channel cell width at flow link', 'm', is_timedep = 0, which_meshdim = 1, jabndnd=jabndnd_)
            endif
         endif
      endif
      !
      ! BEDFORMS
      !
      if (bfmpar%lfbedfrmout) then
         if (bfmpar%lfbedfrm) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_duneheight, nc_precision, UNC_LOC_S, 'duneheight'  , '', 'Time-varying dune height in flow cell center', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dunelength, nc_precision, UNC_LOC_S, 'dunelength'  , '', 'Time-varying dune length in flow cell center', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         endif
         !
         if (bfmpar%lfbedfrmrou) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ksr,  nc_precision, UNC_LOC_S, 'ksr'  , '', 'Ripple roughness height in flow cell center', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ksmr, nc_precision, UNC_LOC_S, 'ksmr'  , '', 'Megaripple roughness height in flow cell center', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ksd,  nc_precision, UNC_LOC_S, 'ksd'  , '', 'Dune roughness height in flow cell center', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ks,   nc_precision, UNC_LOC_S, 'ks'  , '', 'Bedform roughness height in flow cell center', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         endif
      endif

      ! Sediment transport (via own built-in sed)
      if (jamapsed > 0 .and. jased > 0 .and. .not. stm_included) then
         ierr = nf90_def_dim(mapids%ncid, 'nFrac', mxgr, mapids%id_tsp%id_maxfracdim)
         if ( .not. allocated(mapids%id_sed) ) then
            allocate( mapids%id_sed(MAX_ID_VAR,mxgr), mapids%id_ero(MAX_ID_VAR,mxgr) )
            mapids%id_sed = -1
            mapids%id_ero = -1
         endif
         do j = 1,mxgr
            write(str,"(I4)") j
            str = adjustl( str )
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sed(:,j), nc_precision, UNC_LOC_S, 'sed'//trim(str), 'sediment_concentration'      , 'Sediment concentration'   , 'kg m-3', jabndnd=jabndnd_) !, dimids = (/ mapids%id_maxfracdim, -2, -1 /))
         enddo
         if (jaceneqtr == 1) then ! Bed level in cell center
            do j = 1,mxgr
               write(str,"(I4)") j
               str = adjustl( str )
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ero(:,j), nc_precision, UNC_LOC_S, 'ero'//trim(str), 'layer_thickness_per_fraction', 'Erodable layer thickness per size fraction in flow element centers'   , 'm', jabndnd=jabndnd_)
            enddo
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_bl,  nc_precision, UNC_LOC_S, 'flowelem_bedlevel_bl', ''   , 'Flow element center bedlevel (bl)'                             , 'm', jabndnd=jabndnd_)
         else                     ! Bed level at cell corner
            do j = 1,mxgr
               write(str,"(I4)") j
               str = adjustl( str )
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ero(:,j), nc_precision, UNC_LOC_CN, 'ero'//trim(str), 'layer_thickness_per_fraction', 'Erodable layer thickness per size fraction in flow element corners'   , 'm', jabndnd=jabndnd_)
            enddo
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_zk , nc_precision, UNC_LOC_CN,'netnode_bedlevel_zk', ''      , 'Flow element corner bedlevel (zk)'                          , 'm', jabndnd=jabndnd_)
         endif
      endif

      if (jamapwav>0) then
         if (flowWithoutWaves) then      ! Check the external forcing wave quantities and their associated arrays
            if (jamapwav_hwav > 0      .and. allocated(hwav)) then
               if (jamapsigwav==0) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav     , nc_precision, UNC_LOC_S, 'hwav'         , 'sea_surface_wave_rms_height'          , 'RMS wave height'          , 'm'    , jabndnd=jabndnd_) ! not CF
               else
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav     , nc_precision, UNC_LOC_S, 'hwav'         , 'sea_surface_wave_significant_wave_height'          , 'Significant wave height'          , 'm'    , jabndnd=jabndnd_)
               endif
            endif
            if (jamapwav_twav > 0   .and. allocated(twav)) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_twav        , nc_precision, UNC_LOC_S, 'tp'  , ''        , 'Peak wave period'          , 's'    )
            endif
            if (jamapwav_phiwav > 0 .and. allocated(phiwav)) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_phiwav      , nc_precision, UNC_LOC_S, 'dir' , ''        , 'Mean direction of wave propagation relative to ksi-dir. ccw'   , 'deg', jabndnd=jabndnd_) ! not CF
            endif
            if (jamapwav_sxwav > 0  .and. allocated(sxwav)) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxwav       , nc_precision, UNC_LOC_S, 'sxwav' , 'sea_surface_x_wave_force_surface', 'Surface layer wave forcing term, x-component'   , 'N m-2', jabndnd=jabndnd_) ! not CF
            endif
            if (jamapwav_sywav > 0  .and. allocated(sywav)) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sywav       , nc_precision, UNC_LOC_S, 'sywav' , 'sea_surface_y_wave_force_surface', 'Surface layer wave forcing term, y-component'   , 'N m-2', jabndnd=jabndnd_) ! not CF
            endif
            if (jamapwav_sxbwav > 0 .and. allocated(sbxwav)) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxbwav      , nc_precision, UNC_LOC_S, 'sxbwav', 'sea_surface_x_wave_force_bottom' , 'Bottom layer wave forcing term, x-component'    , 'N m-2', jabndnd=jabndnd_) ! not CF
            endif
            if (jamapwav_sybwav > 0 .and. allocated(sbywav)) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sybwav      , nc_precision, UNC_LOC_S, 'sybwav', 'sea_surface_y_wave_force_bottom' , 'Bottom layer wave forcing term, y-component'    , 'N m-2', jabndnd=jabndnd_) ! not CF
            endif
            if (jamapwav_mxwav > 0  .and. allocated(mxwav)) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mxwav       , nc_precision, UNC_LOC_S, 'mx' , '', 'Wave-induced volume flux in x-direction'   , 'm3 s-1 m-1', jabndnd=jabndnd_) ! not CF
            endif
            if (jamapwav_mywav > 0  .and. allocated(mywav)) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mywav       , nc_precision, UNC_LOC_S, 'my' , '', 'Wave-induced volume flux in y-direction'   , 'm3 s-1 m-1', jabndnd=jabndnd_) ! not CF
            endif
            if (jamapwav_dsurf > 0  .and. allocated(dsurf)) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dsurf       , nc_precision, UNC_LOC_S, 'dissurf' , '', 'Wave energy dissipation rate at the free surface'   , 'w m-2', jabndnd=jabndnd_) ! not CF
            endif
            if (jamapwav_dwcap > 0  .and. allocated(dwcap)) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dwcap       , nc_precision, UNC_LOC_S, 'diswcap' , '', 'Wave energy dissipation rate due to white capping'   , 'w m-2', jabndnd=jabndnd_) ! not CF
            endif
            if (jamapwav_distot > 0  .and. allocated(distot)) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_distot      , nc_precision, UNC_LOC_S, 'distot' , '', 'Total wave energy dissipation'                       , 'w m-2', jabndnd=jabndnd_) ! not CF
            endif
            if (jamapwav_uorb > 0   .and. allocated(uorbwav)) then
                ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_uorb       , nc_precision, UNC_LOC_S, 'uorb'            , 'sea_surface_wave_orbital_velocity'    , 'Wave orbital velocity'    , 'm s-1', jabndnd=jabndnd_) ! not CF
            endif
         else   ! flow With Waves
            ! JRE waves
            if (jawave .eq. 4) then
               ierr = nf90_def_dim(mapids%ncid, 'ntheta', ntheta, mapids%id_tsp%id_ntheta)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_E        , nc_precision, UNC_LOC_S, 'E'        , 'sea_surface_bulk_wave_energy'         , 'Wave energy per square meter'                     , 'J m-2', jabndnd=jabndnd_) ! not CF
               if (roller>0) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_R        , nc_precision, UNC_LOC_S, 'R'        , 'sea_surface_bulk_roller_energy'       , 'Roller energy per square meter'                   , 'J m-2', jabndnd=jabndnd_) ! not CF
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_DR       , nc_precision, UNC_LOC_S, 'DR'       , 'sea_surface_bulk_roller_dissipation'  , 'Roller energy dissipation per square meter'       , 'W m-2', jabndnd=jabndnd_) ! not CF
               endif
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_D        , nc_precision, UNC_LOC_S, 'D'        , 'sea_surface_wave_breaking_dissipation', 'Wave breaking energy dissipation per square meter', 'W m-2', jabndnd=jabndnd_) ! not CF
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Df        , nc_precision, UNC_LOC_S, 'Df'        , 'sea_surface_wave_bottom_dissipation', 'Wave bottom energy dissipation per square meter', 'W m-2', jabndnd=jabndnd_) ! not CF

               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Sxx      , nc_precision, UNC_LOC_S, 'Sxx'      , ''         , 'Radiation stress, x-component'          , 'N m-2', jabndnd=jabndnd_) ! not CF
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Syy      , nc_precision, UNC_LOC_S, 'Syy'      , ''        , 'Radiation stress, y-component'          , 'N m-2', jabndnd=jabndnd_) ! not CF
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Sxy      , nc_precision, UNC_LOC_S, 'Sxy'      , 'sea_surface_wave_radiation_stress_NE'         , 'Radiation stress, xy-component'           , 'N m-2', jabndnd=jabndnd_) ! not CF

               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cwav     , nc_precision, UNC_LOC_S, 'cwav'     , 'sea_surface_wave_phase_celerity'      , 'Sea_surface_wave_phase_celerity'                  , 'm s-1', jabndnd=jabndnd_) ! not CF
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cgwav    , nc_precision, UNC_LOC_S, 'cgwav'    , 'sea_surface_wave_group_celerity'      , 'Sea_surface_wave_group_celerity'                  , 'm s-1', jabndnd=jabndnd_) ! not CF
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sigmwav  , nc_precision, UNC_LOC_S, 'sigmwav'  , 'sea_surface_wave_mean_frequency'      , 'Sea_surface_wave_mean_frequency'                  , 'rad s-1', jabndnd=jabndnd_) ! not CF
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_kwav     , nc_precision, UNC_LOC_S, 'kwav'     , 'sea_surface_wave_wavenumber'          , 'Sea_surface_wave_wavenumber'                      , 'rad m-1', jabndnd=jabndnd_) ! not CF
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nwav     , nc_precision, UNC_LOC_S, 'nwav'     , 'sea_surface_wave_cg_over_c'           , 'Sea_surface_wave_ratio_group_phase_speed'         , '-', jabndnd=jabndnd_) ! not CF
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ctheta   , nc_precision, UNC_LOC_S, 'ctheta'   , 'sea_surface_wave_refraction_celerity' , 'Sea_surface_wave_refraction_celerity'             , 'rad s-1', dimids = (/ mapids%id_tsp%id_ntheta, -2,  -1 /), jabndnd=jabndnd_) ! not CF
               !
               !if (windmodel.eq.0) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_l1       , nc_precision, UNC_LOC_S, 'L1'       , 'sea_surface_wave_wavelength'          , 'Sea_surface_wave_wavelength'                      , 'm', jabndnd=jabndnd_      ) ! not CF
               !elseif ( (windmodel .eq. 1) .and. (jawsource .eq. 1) ) then
               !   ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_SwE  , nf90_double, UNC_LOC_S, 'SwE'  , 'source_term_wind_on_E'      , 'wind source term on wave energy'                  , 'J m-2 s-1', jabndnd=jabndnd_) ! not CF
               !   ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_SwT  , nf90_double, UNC_LOC_S, 'SwT'  , 'source_term_wind_on_T'      , 'wind source term on wave period'                  , 's s-1', jabndnd=jabndnd_) ! not CF
               !endif
            endif

            if ((jawave==3 .or. jawave==4).and. kmx>0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxwav       , nc_precision, UNC_LOC_S, 'sxwav' , 'sea_surface_x_wave_force_surface', 'Surface layer wave forcing term, x-component'   , 'N m-2', jabndnd=jabndnd_) ! not CF
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sywav       , nc_precision, UNC_LOC_S, 'sywav' , 'sea_surface_y_wave_force_surface', 'Surface layer wave forcing term, y-component'   , 'N m-2', jabndnd=jabndnd_) ! not CF
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxbwav      , nc_precision, UNC_LOC_S, 'sxbwav', 'sea_surface_x_wave_force_bottom' , 'Water body wave forcing term, x-component'    , 'N m-2', jabndnd=jabndnd_) ! not CF
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sybwav      , nc_precision, UNC_LOC_S, 'sybwav', 'sea_surface_y_wave_force_bottom' , 'Water body wave forcing term, y-component'    , 'N m-2', jabndnd=jabndnd_) ! not CF
            endif

            if (jawave .gt. 0) then
               if (jamapsigwav==0) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav        , nc_precision, UNC_LOC_S, 'hwav'         , 'sea_surface_wave_rms_height'          , 'RMS wave height'          , 'm' , jabndnd=jabndnd_   ) ! not CF
               else
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav        , nc_precision, UNC_LOC_S, 'hwav'         , 'sea_surface_wave_significant_wave_height'          , 'Significant wave height'          , 'm' , jabndnd=jabndnd_   )
               endif
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_uorb     , nc_precision, UNC_LOC_S, 'uorb'            , 'sea_surface_wave_orbital_velocity'    , 'Wave orbital velocity'    , 'm s-1', jabndnd=jabndnd_) ! not CF
               !
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ustokes      , nc_precision, iLocS, 'ust_cc'     , 'sea_surface_x_stokes_drift'        , 'Stokes drift, x-component'   , 'm s-1', jabndnd=jabndnd_) ! not CF
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vstokes      , nc_precision, iLocS, 'vst_cc'     , 'sea_surface_y_stokes_drift'       , 'Stokes drift, y-component'    , 'm s-1', jabndnd=jabndnd_) ! not CF

               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ustokeslink      , nc_precision, iLocU, 'ustokes'     , ''        , 'Stokes drift, n-component'   , 'm s-1', jabndnd=jabndnd_) ! not CF
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vstokeslink      , nc_precision, iLocU, 'vstokes'     , ''        , 'Stokes drift, t-component'   , 'm s-1', jabndnd=jabndnd_) ! not CF

               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_thetamean, nc_precision, UNC_LOC_S, 'thetamean'       , 'sea_surface_wave_from_direction'      , 'Wave from direction'      , 'deg from N', jabndnd=jabndnd_) ! not CF
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_twav,      nc_precision, UNC_LOC_S, 'twav'       ,      'sea_surface_wave_period'      , 'Wave period'      , 's') ! not CF
               if (jawave==3 .or. jawave==4) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fx       , nc_precision, iLocS, 'Fx'              , 'sea_surface_x_wave_force'          , 'Wave force, x-component'     , 'N m-2', jabndnd=jabndnd_) ! not CF
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fy       , nc_precision, iLocS, 'Fy'              , 'sea_surface_y_wave_force'         , 'Wave force, y-component'      , 'N m-2', jabndnd=jabndnd_) ! not CF

                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fxlink, nc_precision, iLocU, 'wavfu', '', 'Wave force at velocity point, n-component', 'N m-2', jabndnd=jabndnd_)! not CF
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fylink, nc_precision, iLocU, 'wavfv', '', 'Wave force at velocity point, t-component', 'N m-2', jabndnd=jabndnd_)! not CF
               endif
            endif
         endif
      endif
      !
      ! Trachytope roughnesses on NET links
      if (jamaptrachy > 0 .and. jatrt == 1) then

         if (ifrctypuni == 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, nc_precision, UNC_LOC_L, 'cftrt',   '', 'Chezy roughness from trachytopes', '', jabndnd=jabndnd_)
            ierr = unc_put_att(mapids%ncid, mapids%id_cftrt, 'non_si_units', 'm0.5s-1')
         else if (ifrctypuni == 1) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, nc_precision, UNC_LOC_L, 'cftrt',   '', 'Manning roughness from trachytopes', '', jabndnd=jabndnd_)
            ierr = unc_put_att(mapids%ncid, mapids%id_cftrt, 'non_si_units', 'sm-0.333')
         else if ((ifrctypuni == 2) .or. (ifrctypuni == 3)) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, nc_precision, UNC_LOC_L, 'cftrt',   '', 'White-Colebrook roughness from trachytopes', '', jabndnd=jabndnd_)
            ierr = unc_put_att(mapids%ncid, mapids%id_cftrt, 'non_si_units', 'm')
         else
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, nc_precision, UNC_LOC_L, 'cftrt',   '', 'Roughness from trachytopes', '', jabndnd=jabndnd_)
            ierr = unc_put_att(mapids%ncid, mapids%id_cftrt, 'non_si_units', ' ')
         endif
      endif

      if (javeg > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rnveg        	, nc_precision, UNC_LOC_S, 'rnveg'        , 'stem density of vegetation'      , 'stem density per square meter', 'm-2')
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_diaveg        , nc_precision, UNC_LOC_S, 'diaveg'       , 'stem diameter of vegetation'     , 'stem diameter of vegetation', 'm')
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_veg_stemheight, nc_precision, UNC_LOC_S, 'stemheight'   , 'stem height of vegetation'       , 'stem height of vegetation', 'm')
      endif

      if (jamapcali > 0 .and. jacali == 1) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cfcl, nc_precision, UNC_LOC_L, 'cfcl',   '', 'Calibration factor for roughness', '', jabndnd=jabndnd_)
         ierr = unc_put_att(mapids%ncid, mapids%id_cfcl, 'non_si_units', 'm0.5s-1')
      endif

      ! Secondary Flow ! TODO: AvD: add secondary flow
           !if (jasecflow == 1) then
           !    ierr = nf90_def_var(imapfile, 'rsi' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_rsi)
           !    ierr = nf90_put_att(imapfile, id_rsi,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           !    ierr = nf90_put_att(imapfile, id_rsi,  'standard_name', '')
           !    ierr = nf90_put_att(imapfile, id_rsi,  'long_name'    , 'inverse streamline curvature in flow element center')
           !    ierr = nf90_put_att(imapfile, id_rsi,  'units'        , 'm-1')
           !    ierr = nf90_def_var(imapfile, 'rsiexact' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_rsiexact)
           !    ierr = nf90_put_att(imapfile, id_rsiexact,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           !    ierr = nf90_put_att(imapfile, id_rsiexact,  'standard_name', '')
           !    ierr = nf90_put_att(imapfile, id_rsiexact,  'long_name'    , 'inverse streamline curvature in flow element center')
           !    ierr = nf90_put_att(imapfile, id_rsiexact,  'units'        , 'm-1')
           !    ierr = nf90_def_var(imapfile, 'dsdx' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dsdx)
           !    ierr = nf90_put_att(imapfile, id_dsdx,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           !    ierr = nf90_put_att(imapfile, id_dsdx,  'standard_name', '')
           !    ierr = nf90_put_att(imapfile, id_dsdx,  'long_name'    , 'water level gradient in x direction')
           !    ierr = nf90_put_att(imapfile, id_dsdx,  'units'        , 's-1')
           !    ierr = nf90_def_var(imapfile, 'dsdy' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dsdy)
           !    ierr = nf90_put_att(imapfile, id_dsdy,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           !    ierr = nf90_put_att(imapfile, id_dsdy,  'standard_name', '')
           !    ierr = nf90_put_att(imapfile, id_dsdy,  'long_name'    , 'water level gradient in y direction')
           !    ierr = nf90_put_att(imapfile, id_dsdy,  'units'        , 's-1')
           !    ierr = nf90_def_var(imapfile, 'dudx' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dudx)
           !    ierr = nf90_put_att(imapfile, id_dudx,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           !    ierr = nf90_put_att(imapfile, id_dudx,  'standard_name', '')
           !    ierr = nf90_put_att(imapfile, id_dudx,  'long_name'    , 'x-velocity gradient in x direction')
           !    ierr = nf90_put_att(imapfile, id_dudx,  'units'        , 's-1')
           !    ierr = nf90_def_var(imapfile, 'dudy' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dudy)
           !    ierr = nf90_put_att(imapfile, id_dudy,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           !    ierr = nf90_put_att(imapfile, id_dudy,  'standard_name', '')
           !    ierr = nf90_put_att(imapfile, id_dudy,  'long_name'    , 'x-velocity gradient in y direction')
           !    ierr = nf90_put_att(imapfile, id_dudy,  'units'        , 's-1')
           !    ierr = nf90_def_var(imapfile, 'dvdx' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dvdx)
           !    ierr = nf90_put_att(imapfile, id_dvdx,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           !    ierr = nf90_put_att(imapfile, id_dvdx,  'standard_name', '')
           !    ierr = nf90_put_att(imapfile, id_dvdx,  'long_name'    , 'y-velocity gradient in x direction')
           !    ierr = nf90_put_att(imapfile, id_dvdx,  'units'        , 's-1')
           !    ierr = nf90_def_var(imapfile, 'dvdy' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dvdy)
           !    ierr = nf90_put_att(imapfile, id_dvdy,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           !    ierr = nf90_put_att(imapfile, id_dvdy,  'standard_name', '')
           !    ierr = nf90_put_att(imapfile, id_dvdy,  'long_name'    , 'y-velocity gradient in y direction')
           !    ierr = nf90_put_att(imapfile, id_dvdy,  'units'        , 's-1')
           !endif

      if ( janudge.gt.0 .and. jamapNudge.gt.0 ) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_time, nc_precision, UNC_LOC_S, 'Tnudge', 'nudging_time', 'Nudging relaxing time', 's', is_timedep=0, jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_tem, nc_precision, UNC_LOC_S3D, 'nudge_tem', 'nudging_tem', 'Nudging temperature', 'degC', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_sal, nc_precision, UNC_LOC_S3D, 'nudge_sal', 'nudging_sal', 'Nudging salinity', '1e-3, jabndnd=jabndnd_', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_Dtem, nc_precision, UNC_LOC_S3D, 'nudge_Dtem', 'nudging_Dtem', 'Difference of nudging temperature with temperature', 'degC', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_Dsal, nc_precision, UNC_LOC_S3D, 'nudge_Dsal', 'nudging_Dsal', 'Difference of nudging salinity with salinity', '1e-3', jabndnd=jabndnd_)

      endif

      ! for 1D only
      if (ndxi-ndx2d>0 .and. jamapPure1D_debug) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_adve, nc_precision, UNC_LOC_U, 'adve', '', 'Explicit advection term', 's', which_meshdim = 1, jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_advi, nc_precision, UNC_LOC_U, 'advi', '', 'Implicit advection term', 's', which_meshdim = 1, jabndnd=jabndnd_)
      endif

      if (ndxi-ndx2d>0 .and. jaPure1D >= 3 .and. jamapPure1D_debug) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1d_1, nc_precision, UNC_LOC_U, 'q1d_1', '', 'Discharge at begin of flow link', 's', which_meshdim = 1, jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1d_2, nc_precision, UNC_LOC_U, 'q1d_2', '', 'Discharge at end of flow link'  , 's', which_meshdim = 1, jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_volu1d, nc_precision, UNC_LOC_U, 'volu1d', '', 'Volume of flow link', 's', which_meshdim = 1, jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_au1d_1, nc_precision, UNC_LOC_U, 'au1d_1', '', 'Flow area at begin of flow link', 's', which_meshdim = 1, jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_au1d_2, nc_precision, UNC_LOC_U, 'au1d_2', '', 'Flow area at end of flow link'  , 's', which_meshdim = 1, jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wu1d_1, nc_precision, UNC_LOC_U, 'wu1d_1', '', 'Total width at begin of flow link', 's', which_meshdim = 1, jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wu1d_2, nc_precision, UNC_LOC_U, 'wu1d_2', '', 'Total width at end of flow link'  , 's', which_meshdim = 1, jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sar1d_1, nc_precision, UNC_LOC_U, 'sar1d_1', '', 'Surface area at begin of flow link', 's', which_meshdim = 1, jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sar1d_2, nc_precision, UNC_LOC_U, 'sar1d_2', '', 'Surface area at end of flow link'  , 's', which_meshdim = 1, jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_alpha_mom_1d, nc_precision, UNC_LOC_S, 'alpha_mom_1d', '', 'Alpha factor momentum conservation', 's', which_meshdim = 1, jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_alpha_ene_1d, nc_precision, UNC_LOC_S, 'alpha_ene_1d', '', 'Alpha factor kinetic energy conservation', 's', which_meshdim = 1, jabndnd=jabndnd_)
      endif

      ! for 1D only, urban
      if (ndxi-ndx2d>0 .and. network%loaded) then
         if (jamapTimeWetOnGround > 0) then ! cumulative time when water is above ground level
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_timewetground, nc_precision, UNC_LOC_S, 'time_water_on_ground', '', 'Cumulative time water above ground level', 's', which_meshdim = 1, jabndnd=jabndnd_)
         endif
         if (jamapFreeboard > 0) then ! freeboard
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_freeboard, nc_precision, UNC_LOC_S, 'freeboard', '', 'Freeboard', 'm', which_meshdim = 1, jabndnd=jabndnd_)
         endif
         if (jamapDepthOnGround > 0) then ! waterdpth that is above ground level
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hs_on_ground, nc_precision, UNC_LOC_S, 'waterdepth_on_ground', '', 'Waterdepth above ground level', 'm', which_meshdim = 1, jabndnd=jabndnd_)
         endif
         if (jamapVolOnGround > 0) then ! volume that is above ground level
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vol_on_ground, nc_precision, UNC_LOC_S, 'volume_on_ground', '', 'Volume above ground level', 'm3', which_meshdim = 1, jabndnd=jabndnd_)
         endif
         if (jamapTotalInflow1d2d > 0) then ! total 1d2d net inflow
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qCur1d2d, nc_precision, UNC_LOC_S, 'current_total_net_inflow_1d2d', '', 'Current total net inflow via all connected 1d2d links at each 1D node', 'm3 s-1', which_meshdim = 1, jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vTot1d2d, nc_precision, UNC_LOC_S, 'cumulative_total_net_inflow_1d2d', '', 'Cumulative total net inflow via all connected 1d2d links at each 1D node', 'm3', which_meshdim = 1, jabndnd=jabndnd_)
         endif
         if (jamapTotalInflowLat > 0) then ! total lateral net inflow
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qCurLat, nc_precision, UNC_LOC_S, 'current_total_net_inflow_lateral', '', 'Current total net inflow via all laterals at each 1D node', 'm3 s-1', which_meshdim = 1, jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vTotLat, nc_precision, UNC_LOC_S, 'cumulative_total_net_inflow_lateral', '', 'Cumulative total net inflow via all laterals at each 1D node', 'm3', which_meshdim = 1, jabndnd=jabndnd_)
         endif
      endif
      if (lnx1d > 0) then
         if (jamapS1Gradient > 0) then ! water level gradient
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s1Gradient, nc_precision, UNC_LOC_U, 'water_level_gradient', '', 'Water level gradient at each 1D flow link', '1', which_meshdim = 1, jabndnd=jabndnd_)
         endif
      endif
      if (jamapNearField > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nrfld, nc_precision, UNC_LOC_S3D, 'nrfld', 'nearfield_discharges', 'Nearfield related discharges', 'm3 s-1', jabndnd=jabndnd_)
      endif
      !
      ! END OF DEFINITION PART
      !
      ierr = nf90_enddef(mapids%ncid)
      if (ierr == NF90_EVARSIZE .and. unc_cmode /= NF90_NETCDF4) then
         call mess(LEVEL_ERROR, 'Error while writing map file. Probably model grid is too large for classic NetCDF format. Try setting [output] NcFormat = 4 in your MDU.')
      else if (ierr /= NF90_NOERR) then
         write (msgbuf, '(a,i0,a)') 'Error while writing map file. Error code: ', ierr, '.'
         call err_flush()
      endif

      if ( janudge.gt.0 .and. jamapnudge.gt.0 ) then
!        output static nudging time
         workx = 0d0
         do k=1,Ndx
            if ( nudge_rate(k).gt.0d0 ) then
               workx(k) = 1d0/nudge_rate(k)
            endif
         enddo
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_time, UNC_LOC_S, workx, jabndnd=jabndnd_)
      endif

      if (nomba > 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mba(:), UNC_LOC_S, mbadef, jabndnd=jabndnd_)
      endif

      if (jased==4 .and. stm_included) then
         do j=1,stmpar%lsedtot
            ierr = nf90_put_var(mapids%ncid,mapids%id_frac_name,trim(stmpar%sedpar%namsed(j)),(/ 1, j /),(/ len(trim(stmpar%sedpar%namsed(j))), 1 /))  ! only write once
         enddo
         if (stmpar%lsedsus > 0) then
            do j=1,stmpar%lsedsus
               ierr = nf90_put_var(mapids%ncid,mapids%id_susfrac_name,trim(stmpar%sedpar%namsed(j)),(/ 1, j /),(/ len(trim(stmpar%sedpar%namsed(j))), 1 /))  ! only write once
            enddo
         endif
      endif
      !
      ! 1D cross sections
      if (ndx1d > 0 .and. stm_included) then
         if (stmpar%morpar%bedupd) then
            do i = 1,nCrs
               ierr = nf90_put_var(mapids%ncid, mapids%id_tsp%id_morCrsName,trim(network%crs%cross(i)%CSID),(/ 1, i /),(/ len(trim(network%crs%cross(i)%CSID)), 1 /))  ! only write once
            enddo
         endif
      endif

      ! Enable the following when needed:
      ! if (jawritedebug) then
      !    ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dbg1d  , nf90_double, UNC_LOC_U, 'debug1d', 'debug1d', 'debug1d', '-', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
      !    !
      !    if (allocated(debugarr2d)) then
      !       ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dbg2d, nf90_double, UNC_LOC_S, 'debug2d', 'debug2d', 'debug2d', '-', dimids = (/ -2, mapids%id_tsp%id_sedtotdim,-1 /), jabndnd=jabndnd_) ! not CF
      !    endif
      !    !
      !    if (allocated(debugarr3d)) then
      !       !ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dbg3d, nf90_double, UNC_LOC_S, 'debug3d', 'debug3d', 'debug3d', '-', dimids = (/ -2, -1 /), jabndnd=jabndnd_) ! not CF
      !    endif
      ! endif

   if (timon) call timstop (handle_extra(71))

   endif
   ! End of writing time-independent flow geometry data.

   ! -- Start data writing (flow data) ------------------------
   if (timon) call timstrt ( "unc_write_map_filepointer_ugrid TIME write", handle_extra(72))

   mapids%id_tsp%idx_curtime = mapids%id_tsp%idx_curtime+1      ! Increment time dimension index
   itim               = mapids%id_tsp%idx_curtime

   ! Time
   ierr = nf90_put_var(mapids%ncid, mapids%id_time    , tim, (/ itim /))
   ierr = nf90_put_var(mapids%ncid, mapids%id_timestep, dts, (/ itim /))

   if (timon) call timstop (handle_extra(72))
   if (timon) call timstrt ( "unc_write_map_filepointer_ugrid vars", handle_extra(73))
   if (jamapnumlimdt > 0) then
      ! ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_numlimdt, UNC_LOC_S, numlimdt) ! TODO: AvD: integer version of this routine
      call realloc(numlimdtdbl, ndxndxi, keepExisting=.false.)
      numlimdtdbl = dble(numlimdt) ! To prevent stack overflow. TODO: remove once integer version is available.
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_numlimdt, UNC_LOC_S, numlimdtdbl, jabndnd=jabndnd_)
      deallocate(numlimdtdbl)
   endif

   ! Time dependent grid layers
   if (kmx > 0 .and. jafullgridoutput == 1) then
      call realloc(work1d, ndkx, keepExisting = .false.)
      call realloc(work3d2, (/ 2, kmx, max(lnx, ndxndxi) /), keepExisting=.false., fill = dmiss)
      do kk = 1,ndxndxi
         call getkbotktop(kk,kb,kt)
         call getlayerindices(kk, nlayb,nrlay)
         do k = kb,kt
            work1d(k) = (zws(k) + zws(k-1)) * 0.5d0 ! middle z-coord of this cell in this layer
            work3d2(1:2,k-kb+nlayb,kk) = zws(k-1:k) ! vertical z-bounds of this cell in this layer
         enddo
      enddo
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowelemzcc, UNC_LOC_S3D, work1d, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowelemzw , UNC_LOC_W  , zws   , jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowelemzcc_bnd, UNC_LOC_S, work3d2, locdim = 3, jabndnd=jabndnd_)
      !ierr = nf90_put_var(mapids%ncid, mapids%id_flowelemzcc_bnd(2), work3d2(1:2, 1:kmx, 1:ndxndxi), start=(/ 1, 1, 1, itim /), count=(/ 2, kmx, ndxndxi, 1 /))
      ! TODO: support this in 1D or 1D2D as well, via unc_put_var_map interfaces.

      call realloc(work1d, lnkx, keepExisting = .false., fill = dmiss)
      ! work3d2 already sufficiently allocated above.
      do LL = 1,lnx
         !DIR$ INLINE
         zwu0 = blup(LL) ! cached from latest sethu()
         call getLbotLtopmax(LL,Lb,Ltx)
         call getlayerindicesLmax(LL, nlaybL, nrlayLx)

         do L = Lb,Ltx
            work1d(L) = zwu0 + .5d0 * (hu(L) + hu(L-1))
            work3d2(1:2,L-Lb+nlaybL,LL) = (/ zwu0 + hu(L-1), zwu0 + hu(L) /) ! vertical z-bounds of this cell in this layer
         enddo
      enddo
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowlinkzu, UNC_LOC_U3D, work1d, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowlinkzu_bnd, UNC_LOC_U, work3d2, locdim = 3, jabndnd=jabndnd_)
   endif

   ! Water level
   if (jamaps1 == 1) then
      ierr = write_array_with_dmiss_for_dry_cells_into_netcdf_file(mapids%ncid, mapids%id_tsp, mapids%id_s1, UNC_LOC_S, s1, jabndnd=jabndnd_)
   end if

   if (jamaps0 == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s0, UNC_LOC_S, s0, jabndnd=jabndnd_)
   end if

   if (jamapqin > 0 .and. jaqin > 0) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qin, UNC_LOC_S, qin, jabndnd=jabndnd_)
   endif

   ! Water depth
   if (jamaphs == 1) then
      !ierr = nf90_inq_varid(mapids%ncid, 'mesh2d'//'_waterdepth', mapids%id_hs(2))
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hs, UNC_LOC_S, hs, jabndnd=jabndnd_)
   endif

   ! Evaporation
   if (jamapevap == 1) then
      if (jadhyd == 1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_potevap, UNC_LOC_S, PotEvap, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_actevap, UNC_LOC_S, ActEvap, jabndnd=jabndnd_)
      endif
      if (jaevap == 1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_evap, UNC_LOC_S, evap, jabndnd=jabndnd_)
      endif
   endif

   ! Volumes
   if (jamapvol1 == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vol1, ilocS, vol1, jabndnd=jabndnd_)
   endif

   ! Flow areas
   if (jamapau == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_au, iLocU, au, jabndnd=jabndnd_)
   endif

   if (jamapflowanalysis == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_negdpt,       iLocS, negativeDepths,     jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_negdpt_cum,   iLocS, negativeDepths_cum, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_noiter,       iLocS, noIterations,       jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_noiter_cum,   iLocS, noIterations_cum,   jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_limtstep,     iLocS, limitingTimestepEstimation,       jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_limtstep_cum, iLocS, limitingTimestepEstimation_cum,   jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_courant,      iLocS, flowCourantNumber,  jabndnd=jabndnd_)
   endif

   ! Velocities
   if (jamapu1 > 0) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_u1, iLocU, u1, 0d0, jabndnd=jabndnd_)
   end if
   if (jamaphu > 0) then
      ierr = write_array_with_dmiss_for_dry_faces_into_netcdf_file(mapids%ncid, mapids%id_tsp, mapids%id_hu, UNC_LOC_U, hu, jabndnd=jabndnd_)
   end if
   if (jamapu0 == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_u0, iLocU, u0, 0d0, jabndnd=jabndnd_)
   endif
   if (jamapdtcell == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dtcell, iLocS, dtcell, jabndnd=jabndnd_)
   endif

   if (jamapucvec == 1 .or. jamapucmag == 1 .or. jamapucqvec == 1) then
      workx=DMISS
      worky=DMISS
      call getucxucyeulmag(ndkx, workx, worky, ucmag, jaeulervel, jamapucmag)
      !
      if (jamapucvec == 1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucx, iLocS, workx, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucy, iLocS, worky, jabndnd=jabndnd_)
      endif
      !
      if (jamapucmag == 1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmag, iLocS, ucmag, jabndnd=jabndnd_)
      endif
      !
      if (kmx > 0) then
         call reconstructucz(0)
         if (jamapucvec == 1) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucz, UNC_LOC_S3D, ucz, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxa, UNC_LOC_S, ucx(1:ndxndxi), jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucya, UNC_LOC_S, ucy(1:ndxndxi), jabndnd=jabndnd_)
         endif

         if (jamapucmag == 1) then
            call realloc(work1d, ndkx, keepExisting = .false., fill=0d0)
            do k=1,ndxndxi                               ! NOTE: this does not include Stokes drift, no Eulerian velocities here!
               work1d(k) = sqrt(ucx(k)**2 + ucy(k)**2)   ! TODO: this does not include vertical/w-component now.
            enddo
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmaga, UNC_LOC_S, work1d, jabndnd=jabndnd_)
         endif
      endif

      if (jamapucqvec == 1) then
         ! TODO: AvD/MN: consider removing entire loop and simply unc_put_var_map( ..., ucqx,..)
         if (kmx > 0) then
            do kk = 1,ndx
                call getkbotktop(kk,kb,kt)
                do k = kb,kt
                    workx(k) = ucxq(k)
                    worky(k) = ucyq(k)
                enddo
            enddo
         else
            do kk = 1,ndx
                workx(kk) = ucxq(kk)
                worky(kk) = ucyq(kk)
            enddo
         endif
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxq, iLocS, workx, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucyq, iLocS, worky, jabndnd=jabndnd_)
      endif

   endif
   if (kmx > 0) then
      if (jamapww1 > 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ww1, UNC_LOC_W, ww1, jabndnd=jabndnd_)
      endif
      if (jamaprho > 0) then
          if ( density_is_pressure_dependent() ) then
              ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rho,  UNC_LOC_S3D, rho, jabndnd=jabndnd_)
          else
              ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rhop, UNC_LOC_S3D, rho, jabndnd=jabndnd_)
          endif
      endif
   endif

   if (jamapq1 == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1, iLocU, q1, 0d0, jabndnd=jabndnd_)
   endif

   if (jamapq1main == 1 .and. allocated(q1_main)) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1main, iLocU, q1_main, 0d0, jabndnd=jabndnd_)
   endif

   if (jamapfw == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_fwel, UNC_LOC_U, map_fixed_weir_energy_loss, 0d0, jabndnd=jabndnd_)
   endif

   ! TIDAL TURBINES: Insert equivalent of wrturbine_cnst and wrturbine_time here

   if (kmx > 0) then
      if (jamapviu > 0) then
         ! For all flowlinks and layers add user defined part (viusp(LL) or vicouv) to modeled part (viu(LL)).
         ! Values for inactive layers are set to missing in function unc_put_var_map.
         call realloc(work1d, lnkx, keepExisting = .false.)
         do LL = 1,lnx
            if (javiusp == 1) then ! If horizontal eddy viscosity is spatially varying.
               vicc = viusp(LL)
            else
               vicc = vicouv
            endif
            call getLbotLtopmax(LL, Lb, Lt)
            do L = Lb,Lt
               work1d(L) = viu(L) + vicc
            enddo
         enddo
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_viu, iLocU, work1d, jabndnd=jabndnd_)
      endif

      if (jamapdiu > 0) then
         ! For all flowlinks and layers add user defined part (diusp(LL) or dicouv) to modeled part (viu(LL)/0.7).
         ! Values for inactive layers are set to missing in function unc_put_var_map.
         call realloc(work1d, lnkx, keepExisting = .false.)
         do LL = 1,lnx
            if (jadiusp == 1) then ! If horizontal eddy viscosity is spatially varying.
               dicc = diusp(LL)
            else
               dicc = dicouv
            endif
            call getLbotLtopmax(LL, Lb, Lt)
            do L = Lb,Lt
               work1d(L) = viu(L) / 0.7 + dicc
            enddo
         enddo
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_diu, iLocU, work1d, jabndnd=jabndnd_)
      endif
   endif

   if (kmx == 0) then
      if (jamapviu > 0) then
         ! For all flowlinks add user defined part (viusp(LL) or vicouv) to modeled part (viu(LL)).
         call realloc(work1d, lnx, keepExisting = .false.)
         do LL = 1,lnx
            if (javiusp == 1) then ! If horizontal eddy viscosity is spatially varying.
               vicc = viusp(LL)
            else
               vicc = vicouv
            endif
            work1d(LL) = viu(LL) + vicc
         enddo
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_viu, iLocU, work1d, jabndnd=jabndnd_)
      endif

      if (jamapdiu > 0) then
         ! For all flowlinks add user defined part (diusp(LL) or dicouv) to modeled part (viu(LL)/0.7).
         call realloc(work1d, lnx, keepExisting = .false.)
         do LL = 1,lnx
            if (jadiusp == 1) then ! If horizontal eddy viscosity is spatially varying.
               dicc = diusp(LL)
            else
               dicc = dicouv
            endif
            work1d(LL) = viu(LL) / 0.7 + dicc
         enddo
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_diu, iLocU, work1d, jabndnd=jabndnd_)
      endif
   endif

   if (allocated(work1d)) deallocate(work1d)

   ! Salinity
   if (jasal > 0 .and. jamapsal > 0) then
      do k = 1,ndkx
         sa1(k) = constituents(isalt, k )
      enddo
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sa1, iLocS, sa1, jabndnd=jabndnd_)
   endif

   ! Temperature
   if (jatem > 0 .and. jamaptem > 0) then
      do k = 1,ndkx
         tem1(k) = constituents(itemp, k )
      enddo
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tem1, iLocS, tem1, jabndnd=jabndnd_)
   endif

   if (jasecflow > 0 .and. jamapspir > 0) then
      if (kmx == 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_spircrv, UNC_LOC_S, spircrv, jabndnd=jabndnd_)
      endif
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_spirint, UNC_LOC_S, spirint, jabndnd=jabndnd_)
   endif

   ! Constituents

!   The following is not stack-safe:
!   if (jamapconst > 0 .and. ITRA1 > 0) then
!      do j=ITRA1,ITRAN
!         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_const(:,j), iLocS, constituents(j,:))
!      enddo
!   endif

!   The following is (almost) copied from unc_wite_map_filepointer
    if (jamapconst > 0 .and. ITRA1 > 0) then

       do j=ITRA1,ITRAN
          workx = DMISS ! For proper fill values in z-model runs.
          if ( kmx>0 ) then
!            3D
             do kk=1,ndxndxi
                call getkbotktop(kk,kb,kt)
                do k = kb,kt
                   workx(k) = constituents(j,k)
                enddo
             enddo
!             ierr = nf90_put_var(imapfile, mapids%id_const(:,j), work1(1:kmx,1:ndxndxi), (/ 1, 1, itim /), (/ kmx, ndxndxi, 1 /))
             ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_const(:,j), UNC_LOC_S3D, workx, jabndnd=jabndnd_)
             !   if ( ierr.ne.0 ) exit  ! probably newly added tracer in the GUI
          else
             do kk=1,NdxNdxi
                workx(kk) = constituents(j,kk)
             enddo
!             ierr = nf90_put_var(imapfile, id_const(iid,j), dum, (/ 1, itim /), (/ NdxNdxi, 1 /) )
             ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_const(:,j), UNC_LOC_S, workx, jabndnd=jabndnd_)
          endif
       enddo
    endif

   ! Turbulence.
   if (jamaptur > 0 .and. kmx > 0) then
      if (iturbulencemodel >= 3) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_turkin1, UNC_LOC_WU, turkin1, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vicwwu,  UNC_LOC_WU, vicwwu, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vicwws,  UNC_LOC_W,  vicwws, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tureps1, UNC_LOC_WU, tureps1, jabndnd=jabndnd_)
      endif
   endif

   !
   ! Sediment transport (via morphology module)
   !
if ((jamapsed > 0 .and. jased > 0 .and. stm_included).or.(jasubsupl>0)) then
   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mor_bl, UNC_LOC_S, bl, jabndnd=jabndnd_)
endif

if (jasubsupl>0) then
   select case (ibedlevtyp)
      case (1)
         iloc = UNC_LOC_S
      case (2)
         iloc = UNC_LOC_U
      case (3,4,5,6)
         iloc = UNC_LOC_CN
   end select
   do k = 1, size(subsupl)
      subsout(k) = subsupl(k) - subsupl_t0(k)
   enddo
   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_subsupl, iloc, subsout, jabndnd=jabndnd_)
endif

if (jamapz0>0) then
   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_z0c, UNC_LOC_U, z0ucur, jabndnd=jabndnd_)    ! from setcfuhi
   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_z0r, UNC_LOC_U, z0urou, jabndnd=jabndnd_)    ! from tauwave, update_vp, or above
endif

if (jamapsed > 0 .and. jased > 0 .and. stm_included) then

   ! intermediate output for sediment formulas
   if (stmpar%morpar%moroutput%sedpar) then
      call realloc(toutput, ndx)
      do l = 1, stmpar%lsedtot
         do k = 1, stmpar%trapar%noutpar(l)
            i = stmpar%trapar%ioutpar(k,l)
            toutput = stmpar%trapar%outpar(i,:)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sedpar(:,k,l), UNC_LOC_S, toutput, jabndnd=jabndnd_)
         enddo
      enddo
   endif

   if (stmpar%lsedsus > 0) then
      if (kmx>0) then
         call realloc(toutputx, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
         !
         ! convert kmxsed to kmx administration
         if (itim==1) then
            toutputx = 1      ! set to bottom layer on first time step
         else
            do l=1,stmpar%lsedsus
               do k=1,ndx
                  kk = sedtra%kmxsed(k,l)
                  call getkbotktop(k,kb,kt)
                  found=0
                  do kkk=kb,kt
                     found=found+1
                     if (kkk==kk) exit   ! meh...
                  enddo
                  toutputx(k,l) = found
               enddo
            enddo
         endif
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_kmxsed, UNC_LOC_S, toutputx, jabndnd=jabndnd_)
      endif
      !
      call realloc(toutputx, (/lnx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
      call realloc(toutputy, (/lnx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
      do l = 1, stmpar%lsedsus
         select case(stmpar%morpar%moroutput%transptype)
         case (0)
            rhol = 1d0
         case (1)
            rhol = stmpar%sedpar%cdryb(sedtot2sedsus(l))
         case (2)
            rhol = stmpar%sedpar%rhosol(sedtot2sedsus(l))
         end select
         toutputx(:,l) = sedtra%e_ssn(:,l)/rhol
         toutputy(:,l) = sedtra%e_sst(:,l)/rhol
      enddo
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ssn  , UNC_LOC_U, toutputx, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sst  , UNC_LOC_U, toutputy, jabndnd=jabndnd_)
   endif
   if (stmpar%lsedtot > 0) then
      call realloc(toutputx, (/lnx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      call realloc(toutputy, (/lnx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      do l = 1, stmpar%lsedtot
         select case(stmpar%morpar%moroutput%transptype)
         case (0)
            rhol = 1d0
         case (1)
            rhol = stmpar%sedpar%cdryb(l)
         case (2)
            rhol = stmpar%sedpar%rhosol(l)
         end select
         toutputx(:,l) = sedtra%e_sbn(:,l)/rhol
         toutputy(:,l) = sedtra%e_sbt(:,l)/rhol
      enddo
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbn  , UNC_LOC_U, toutputx, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbt  , UNC_LOC_U, toutputy, jabndnd=jabndnd_)
   endif
   !
   if (stmpar%morpar%moroutput%uuuvvv) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_uuu, UNC_LOC_S, sedtra%uuu, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vvv, UNC_LOC_S, sedtra%vvv, jabndnd=jabndnd_)
   endif
   !
   if (stmpar%lsedsus .gt. 0) then

      call realloc(work3d, (/kmx, ndxndxi, stmpar%lsedsus /), keepExisting=.false., fill = dmiss)
      if (kmx>0) then
         ! Concentrations
         do kk = 1, ndxndxi
            call getkbotktop(kk,kb,kt)
            call getlayerindices(kk, nlayb,nrlay)
            do k = kb, kt
               work3d(k-kb+nlayb,kk,:) = constituents(ISED1:ISEDN,k)
            enddo
         enddo
         ierr = nf90_put_var(mapids%ncid,mapids%id_sedfrac(2),work3d(1:kmx,1:ndxndxi,1:stmpar%lsedsus),start=(/1,1,1,itim/), count=(/kmx,ndxndxi,stmpar%lsedsus,1/))
         work3d = dmiss
         ! Settling velocity
         do kk = 1, ndxndxi
            call getkbotktop(kk,kb,kt)
            call getlayerindices(kk,nlayb,nrlay)
            do k = kb, kt
               work3d(k-kb+nlayb,kk,:) = mtd%ws(k,1:stmpar%lsedsus)
            enddo
         enddo
         ierr = nf90_put_var(mapids%ncid,mapids%id_ws(2),work3d(1:kmx,1:ndxndxi,1:stmpar%lsedsus),start=(/1,1,1,itim/), count=(/kmx,ndxndxi,stmpar%lsedsus,1/))
      else
         call realloc(work1d_z,(/ndxndxi, stmpar%lsedsus /), keepExisting=.false., fill = dmiss)
         work1d_z = transpose(constituents(ISED1:ISEDN,:))  ! avoid array slice on stack
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sedfrac, UNC_LOC_S, work1d_z, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ws, UNC_LOC_S, mtd%ws, jabndnd=jabndnd_)
      endif
!
      if (kmx == 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rsedeq, UNC_LOC_S, sedtra%rsedeq, jabndnd=jabndnd_)
      endif
!
      if (stmpar%morpar%moroutput%aks) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_aks, UNC_LOC_S, sedtra%aks, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rca, UNC_LOC_S, sedtra%rca, jabndnd=jabndnd_)
      endif
!
      if (stmpar%morpar%moroutput%sourcesink) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sourse, UNC_LOC_S, sedtra%sourse, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sinkse, UNC_LOC_S, sedtra%sinkse, jabndnd=jabndnd_)
      endif

      if (stmpar%morpar%moroutput%suvcor) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_scrn ,UNC_LOC_U, sedtra%e_scrn, jabndnd=jabndnd_)
      endif
   endif
   !
   if (stmpar%morpar%moroutput%dzduuvv) then ! bedslope
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_e_dzdn ,UNC_LOC_U, sedtra%e_dzdn, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_e_dzdt ,UNC_LOC_U, sedtra%e_dzdt, jabndnd=jabndnd_)
   endif
!
   if (stmpar%morpar%moroutput%umod) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_umod , UNC_LOC_S, sedtra%umod, jabndnd=jabndnd_)
   endif
!
   if (stmpar%morpar%moroutput%zumod) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_zumod , UNC_LOC_S, sedtra%zumod, jabndnd=jabndnd_)
   endif
!
   if (stmpar%morpar%moroutput%ustar) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ustar , UNC_LOC_S, sqrt(sedtra%ust2), jabndnd=jabndnd_)
   endif
!
   if (stmpar%morpar%moroutput%rawtransports) then
      if (stmpar%morpar%moroutput%sbcuv) then
         call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
         call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
         do l = 1, stmpar%lsedtot
            select case(stmpar%morpar%moroutput%transptype)
            case (0)
               rhol = 1d0
            case (1)
               rhol = stmpar%sedpar%cdryb(l)
            case (2)
               rhol = stmpar%sedpar%rhosol(l)
            end select
            toutputx(:,l) = sbcx_raw(:,l)/rhol
            toutputy(:,l) = sbcy_raw(:,l)/rhol
         enddo
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcx   , UNC_LOC_S, toutputx, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcy   , UNC_LOC_S, toutputy, jabndnd=jabndnd_)
      endif
      !
      if (stmpar%morpar%moroutput%sbwuv) then
         call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
         call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
         do l = 1, stmpar%lsedtot
            select case(stmpar%morpar%moroutput%transptype)
            case (0)
               rhol = 1d0
            case (1)
               rhol = stmpar%sedpar%cdryb(l)
            case (2)
               rhol = stmpar%sedpar%rhosol(l)
            end select
            toutputx(:,l) = sbwx_raw(:,l)/rhol
            toutputy(:,l) = sbwy_raw(:,l)/rhol
         enddo
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwx   , UNC_LOC_S, toutputx, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwy   , UNC_LOC_S, toutputy, jabndnd=jabndnd_)
      endif
      !
      if (stmpar%morpar%moroutput%sswuv) then
         call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
         call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
         do l = 1, stmpar%lsedtot
            select case(stmpar%morpar%moroutput%transptype)
            case (0)
               rhol = 1d0
            case (1)
               rhol = stmpar%sedpar%cdryb(l)
            case (2)
               rhol = stmpar%sedpar%rhosol(l)
            end select
            toutputx(:,l) = sswx_raw(:,l)/rhol
            toutputy(:,l) = sswy_raw(:,l)/rhol
         enddo
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswx   , UNC_LOC_S, toutputx, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswy   , UNC_LOC_S, toutputy, jabndnd=jabndnd_)
      endif
   endif
   !
   ! Get cell centre transport values
   call reconstructsedtransports()

   if (stmpar%morpar%moroutput%rawtransports) then
      if (stmpar%morpar%moroutput%sscuv) then
         call realloc(toutputx, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
         call realloc(toutputy, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
         do l = 1, stmpar%lsedsus
            select case(stmpar%morpar%moroutput%transptype)
            case (0)
               rhol = 1d0
            case (1)
               rhol = stmpar%sedpar%cdryb(sedtot2sedsus(l))
            case (2)
               rhol = stmpar%sedpar%rhosol(sedtot2sedsus(l))
            end select
            toutputx(:,l) = sedtra%sscx(:,sedtot2sedsus(l))/rhol
            toutputy(:,l) = sedtra%sscy(:,sedtot2sedsus(l))/rhol
         enddo
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscx   , UNC_LOC_S, toutputx, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscy   , UNC_LOC_S, toutputy, jabndnd=jabndnd_)
      endif
   endif

   if (stmpar%morpar%moroutput%sbcuv) then
      call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      do l = 1, stmpar%lsedtot
         select case(stmpar%morpar%moroutput%transptype)
         case (0)
            rhol = 1d0
         case (1)
            rhol = stmpar%sedpar%cdryb(l)
         case (2)
            rhol = stmpar%sedpar%rhosol(l)
         end select
         toutputx(:,l) = sedtra%sbcx(:,l)/rhol
         toutputy(:,l) = sedtra%sbcy(:,l)/rhol
      enddo
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcx_reconstructed   , UNC_LOC_S, toutputx, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcy_reconstructed   , UNC_LOC_S, toutputy, jabndnd=jabndnd_)
   endif
!
   if (stmpar%morpar%moroutput%sbwuv) then
      call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      do l = 1, stmpar%lsedtot
         select case(stmpar%morpar%moroutput%transptype)
         case (0)
            rhol = 1d0
         case (1)
            rhol = stmpar%sedpar%cdryb(l)
         case (2)
            rhol = stmpar%sedpar%rhosol(l)
         end select
         toutputx(:,l) = sedtra%sbwx(:,l)/rhol
         toutputy(:,l) = sedtra%sbwy(:,l)/rhol
      enddo
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwx_reconstructed   , UNC_LOC_S, toutputx, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwy_reconstructed   , UNC_LOC_S, toutputy, jabndnd=jabndnd_)
   endif
!
   if (stmpar%morpar%moroutput%sswuv) then
      call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      do l = 1, stmpar%lsedtot
         select case(stmpar%morpar%moroutput%transptype)
         case (0)
            rhol = 1d0
         case (1)
            rhol = stmpar%sedpar%cdryb(l)
         case (2)
            rhol = stmpar%sedpar%rhosol(l)
         end select
         toutputx(:,l) = sedtra%sswx(:,l)/rhol
         toutputy(:,l) = sedtra%sswy(:,l)/rhol
      enddo
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswx_reconstructed   , UNC_LOC_S, toutputx, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswy_reconstructed   , UNC_LOC_S, toutputy, jabndnd=jabndnd_)
   endif
   !
   if (stmpar%morpar%moroutput%sscuv) then
      call realloc(toutputx, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
      call realloc(toutputy, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
      do l = 1, stmpar%lsedsus
         select case(stmpar%morpar%moroutput%transptype)
         case (0)
            rhol = 1d0
         case (1)
            rhol = stmpar%sedpar%cdryb(sedtot2sedsus(l))
         case (2)
            rhol = stmpar%sedpar%rhosol(sedtot2sedsus(l))
         end select
         toutputx(:,l) = sedtra%sscx(:,sedtot2sedsus(l))/rhol
         toutputy(:,l) = sedtra%sscy(:,sedtot2sedsus(l))/rhol
      enddo
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscx_reconstructed   , UNC_LOC_S, toutputx, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscy_reconstructed   , UNC_LOC_S, toutputy, jabndnd=jabndnd_)
   endif
   !
   if (stmpar%morpar%duneavalan) then
      ! Add avalanching fluxes to total transport for output
      call realloc(sxtotori,(/ndx, stmpar%lsedtot/), stat=ierr, keepExisting=.false.,fill=0d0)
      call realloc(sytotori,(/ndx, stmpar%lsedtot/), stat=ierr, keepExisting=.false.,fill=0d0)
      sxtotori = sedtra%sxtot
      sytotori = sedtra%sytot
      !
      do l = 1, stmpar%lsedtot
         do Lf = 1, lnx
            sedtra%e_sbcn(Lf,l) = sedtra%e_sbcn(Lf,l) + avalflux(Lf,l)*wu_mor(Lf)
         enddo
      enddo
      call reconstructsedtransports()
   endif
   !
   call realloc(toutputx, (/ndxndxi, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
   call realloc(toutputy, (/ndxndxi, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
   do l = 1, stmpar%lsedtot
      select case(stmpar%morpar%moroutput%transptype)
      case (0)
         rhol = 1d0
      case (1)
         rhol = stmpar%sedpar%cdryb(l)
      case (2)
         rhol = stmpar%sedpar%rhosol(l)
      end select
      toutputx(1:ndxndxi,l) = (sedtra%sxtot(1:ndxndxi,l))/rhol
      toutputy(1:ndxndxi,l) = (sedtra%sytot(1:ndxndxi,l))/rhol
   enddo
   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sxtot   , UNC_LOC_S, toutputx, jabndnd=jabndnd_)
   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sytot   , UNC_LOC_S, toutputy, jabndnd=jabndnd_)
   !
   if (stmpar%morpar%duneavalan) then
      ! restore for timestep calculation
      sedtra%sxtot = sxtotori
      sedtra%sytot = sytotori
   endif
   !
   ! Time averaged transports, could probably be more concise...
   !morstarthyd = tstart_user + stmpar%morpar%tmor*tfac        ! seconds
   dmorft      = stmpar%morpar%morft - stmpar%morpar%morft0    ! days since morstart
   dmorfs      = dmorft*86400.0d0                              ! seconds
   mortime     = stmpar%morpar%morft*86400d0                   ! seconds*morfac since tstart_user
   if (stmpar%morpar%hydrt > stmpar%morpar%hydrt0) then
      moravg = dmorft/(stmpar%morpar%hydrt - stmpar%morpar%hydrt0)
   else
      moravg = 0d0
   endif
   !
   ierr = nf90_put_var(mapids%ncid, mapids%id_morfac  , moravg,(/ itim /))
   ierr = nf90_put_var(mapids%ncid, mapids%id_morft   , mortime,(/ itim /))
   !
   if (stmpar%morpar%moroutput%cumavg) then
      ! Bedload components
      call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      if ( dmorft > 0d0 ) then
         do l = 1, stmpar%lsedtot
            select case(stmpar%morpar%moroutput%transptype)
            case (0)
               rhodt = dmorfs
            case (1)
               rhodt = stmpar%sedpar%cdryb(l)*dmorfs
            case (2)
               rhodt = stmpar%sedpar%rhosol(l)*dmorfs
            end select
            toutputx(:,l) = sedtra%sbxcum(:,l)/rhodt
            toutputy(:,l) = sedtra%sbycum(:,l)/rhodt
         enddo
      else
          toutputx = 0d0
          toutputy = 0d0
      endif
      ierr = nf90_put_var(mapids%ncid, mapids%id_sbxcum(2), toutputx(1:ndxndxi,:), start = (/ 1, 1, itim /), count = (/ ndxndxi, stmpar%lsedtot, 1 /))
      ierr = nf90_put_var(mapids%ncid, mapids%id_sbycum(2), toutputy(1:ndxndxi,:), start = (/ 1, 1, itim /), count = (/ ndxndxi, stmpar%lsedtot, 1 /))
      !
      ! Suspended load
      if ( dmorft > 0d0 ) then
         do l = 1, stmpar%lsedtot
            select case(stmpar%morpar%moroutput%transptype)
            case (0)
               rhodt = dmorfs
            case (1)
               rhodt = stmpar%sedpar%cdryb(l)*dmorfs
            case (2)
               rhodt = stmpar%sedpar%rhosol(l)*dmorfs
            end select
            toutputx(:,l) = sedtra%ssxcum(:,l)/rhodt
            toutputy(:,l) = sedtra%ssycum(:,l)/rhodt
         enddo
      else
          toutputx = 0d0
          toutputy = 0d0
      endif
      ierr = nf90_put_var(mapids%ncid, mapids%id_ssxcum(2), toutputx(1:ndxndxi,:), start = (/ 1, 1, itim /), count = (/ ndxndxi, stmpar%lsedtot, 1 /))
      ierr = nf90_put_var(mapids%ncid, mapids%id_ssycum(2), toutputy(1:ndxndxi,:), start = (/ 1, 1, itim /), count = (/ ndxndxi, stmpar%lsedtot, 1 /))
   else
      if (time_map >= ti_mape) then   ! to check, last timestep?
         ierr = nf90_put_var(mapids%ncid, mapids%id_sedavgtim    , mortime, (/ 1 /))
         ! Bedload components
         call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
         call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
         if ( dmorft > 0d0 ) then
            do l = 1, stmpar%lsedtot
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhodt = dmorfs
               case (1)
                  rhodt = stmpar%sedpar%cdryb(l)*dmorfs
               case (2)
                  rhodt = stmpar%sedpar%rhosol(l)*dmorfs
               end select
               toutputx(:,l) = sedtra%sbxcum(:,l)/rhodt
               toutputy(:,l) = sedtra%sbycum(:,l)/rhodt
            enddo
         else
             toutputx = 0d0
             toutputy = 0d0
         endif
         ierr = nf90_put_var(mapids%ncid, mapids%id_sbxcum(2), toutputx(1:ndxndxi,:), start = (/ 1, 1 /), count = (/ ndxndxi, stmpar%lsedtot /))
         ierr = nf90_put_var(mapids%ncid, mapids%id_sbycum(2), toutputy(1:ndxndxi,:), start = (/ 1, 1 /), count = (/ ndxndxi, stmpar%lsedtot /))
         !
         ! Suspended load
         if ( dmorft > 0d0 ) then
            do l = 1, stmpar%lsedtot
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhodt = dmorfs
               case (1)
                  rhodt = stmpar%sedpar%cdryb(l)*dmorfs
               case (2)
                  rhodt = stmpar%sedpar%rhosol(l)*dmorfs
               end select
               toutputx(:,l) = sedtra%ssxcum(:,l)/rhodt
               toutputy(:,l) = sedtra%ssycum(:,l)/rhodt
            enddo
         else
             toutputx = 0d0
             toutputy = 0d0
         endif
         ierr = nf90_put_var(mapids%ncid, mapids%id_ssxcum(2), toutputx(1:ndxndxi,:), start = (/ 1, 1 /), count = (/ ndxndxi, stmpar%lsedtot /))
         ierr = nf90_put_var(mapids%ncid, mapids%id_ssycum(2), toutputy(1:ndxndxi,:), start = (/ 1, 1 /), count = (/ ndxndxi, stmpar%lsedtot /))
      endif
   endif
!
   select case (stmpar%morlyr%settings%iunderlyr)
      case (1)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_bodsed  , UNC_LOC_S, stmpar%morlyr%state%bodsed, locdim=2, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dpsed   , UNC_LOC_S, stmpar%morlyr%state%dpsed, jabndnd=jabndnd_)
      case (2)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_msed  , UNC_LOC_S, stmpar%morlyr%state%msed , locdim=3, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_thlyr , UNC_LOC_S, stmpar%morlyr%state%thlyr, locdim=2, jabndnd=jabndnd_)
         !
         if (.not. allocated(frac) ) allocate( frac(stmpar%lsedtot, 1:stmpar%morlyr%settings%nlyr, 1:ndx) )
         frac = -999d0
         if (stmpar%morlyr%settings%iporosity==0) then
            dens => stmpar%sedpar%cdryb
         else
            dens => stmpar%sedpar%rhosol
         endif
         do k = 1, stmpar%morlyr%settings%nlyr
            do nm = 1, ndxndxi
               if (stmpar%morlyr%state%thlyr(k,nm)>0.0_fp) then
                  do l = 1, stmpar%lsedtot
                       frac(l, k, nm) = stmpar%morlyr%state%msed(l, k, nm)/(dens(l)*stmpar%morlyr%state%svfrac(k, nm) * &
                                        stmpar%morlyr%state%thlyr(k, nm))
                  enddo
               else
                  frac(:, k, nm) = 0d0
               endif
            enddo
         enddo
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_lyrfrac  , UNC_LOC_S, frac, locdim=3, jabndnd=jabndnd_)
         !
         if (stmpar%morlyr%settings%iporosity>0) then
            if (.not. allocated(poros) ) allocate( poros(1:stmpar%morlyr%settings%nlyr, 1:ndx ) )
            poros = 1d0-stmpar%morlyr%state%svfrac
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_poros , UNC_LOC_S, poros, locdim=2, jabndnd=jabndnd_)
         endif
         !
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_preload , UNC_LOC_S, stmpar%morlyr%state%preload, locdim=2, jabndnd=jabndnd_)
      case default
         ! do nothing
      end select
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sedshort, UNC_LOC_S, stmpar%morlyr%state%sedshort, locdim=2, jabndnd=jabndnd_)

      if (stmpar%morpar%moroutput%taub) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_taub  , UNC_LOC_S, sedtra%taub, jabndnd=jabndnd_)
      endif
      if (stmpar%morpar%moroutput%taurat) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_taurat, UNC_LOC_S, sedtra%taurat, jabndnd=jabndnd_)
      endif
      if (stmpar%morpar%moroutput%dm) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dm    , UNC_LOC_S, sedtra%dm, jabndnd=jabndnd_)
      endif
      if (stmpar%morpar%moroutput%dg) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dg    , UNC_LOC_S, sedtra%dg, jabndnd=jabndnd_)
      endif
      if (stmpar%morpar%moroutput%dgsd) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dgsd  , UNC_LOC_S, sedtra%dgsd, jabndnd=jabndnd_)
      endif
      if (stmpar%morpar%moroutput%percentiles) then
         do l = 1, stmpar%morpar%nxx
            call realloc(toutput, ndx, keepExisting=.false., fill = -999d0)
            toutput = sedtra%dxx(1:ndx, l)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dxx(l,:)   , UNC_LOC_S, toutput, jabndnd=jabndnd_)
         enddo
      endif
      if (stmpar%morpar%moroutput%frac) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_frac , UNC_LOC_S, sedtra%frac, jabndnd=jabndnd_)
      endif
      if (stmpar%morpar%moroutput%mudfrac) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_mudfrac , UNC_LOC_S, sedtra%mudfrac, jabndnd=jabndnd_)
      endif
      if (stmpar%morpar%moroutput%sandfrac) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sandfrac , UNC_LOC_S, sedtra%sandfrac, jabndnd=jabndnd_)
      endif
      if (stmpar%morpar%moroutput%fixfac) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_fixfac , UNC_LOC_S, sedtra%fixfac, jabndnd=jabndnd_)
      endif
      if (stmpar%morpar%moroutput%hidexp) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_hidexp , UNC_LOC_S, sedtra%hidexp, jabndnd=jabndnd_)
      endif
      !
      if (stmpar%morpar%flufflyr%iflufflyr>0 .and. stmpar%lsedsus>0) then
         do l = 1, stmpar%lsedsus
            call realloc(toutput, ndx, keepExisting=.false., fill = -999d0)
            toutput = stmpar%morpar%flufflyr%mfluff(l,1:ndx)
            ! ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_mfluff , UNC_LOC_S, stmpar%morpar%flufflyr%mfluff)
            ierr = nf90_put_var(mapids%ncid, mapids%id_mfluff(2)   , toutput(1:ndxndxi) , start = (/ 1, l, itim /), count = (/ ndxndxi, 1, 1 /))
         enddo
      endif
      !
      if (ndx1d > 0 .and. stm_included) then
         if (stmpar%morpar%bedupd) then
            if (allocated(work1d_z)) deallocate(work1d_z)
            if (allocated(work1d_n)) deallocate(work1d_n)
            allocate (work1d_z(jmax,nCrs), work1d_n(jmax,nCrs))
            work1d_z = dmiss
            work1d_n = dmiss
            do i = 1,nCrs
               pCS => network%crs%cross(i)%tabdef
               do j = 1,pCS%levelscount
                  work1d_z(j,i) = pCS%height(j)
                  work1d_n(j,i) = pCS%flowwidth(j)
               enddo
            enddo
            ierr = nf90_put_var(mapids%ncid, mapids%id_tsp%id_flowelemcrsz(1), work1d_z(1:jmax,1:nCrs), start=(/ 1, 1, mapids%id_tsp%idx_curtime /), count=(/ jmax, nCrs, 1 /))
            ierr = nf90_put_var(mapids%ncid, mapids%id_tsp%id_flowelemcrsn(1), work1d_n(1:jmax,1:nCrs), start=(/ 1, 1, mapids%id_tsp%idx_curtime /), count=(/ jmax, nCrs, 1 /))
         endif
         if (stmpar%morpar%moroutput%blave) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_blave, UNC_LOC_S, bl_ave(ndx2d+1:ndxndxi), jabndnd=jabndnd_)
         endif
         if (stmpar%morpar%moroutput%bamor) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_bamor, UNC_LOC_S, ba_mor(ndx2d+1:ndxndxi), jabndnd=jabndnd_)
         endif
         if (stmpar%morpar%moroutput%wumor) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wumor, UNC_LOC_U, wu_mor, jabndnd=jabndnd_)
         endif
      endif
   endif
   !
   ! BEDFORMS
   !
   if (bfmpar%lfbedfrmout) then
      if (bfmpar%lfbedfrm) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_duneheight, UNC_LOC_S, bfmpar%duneheight(1:ndxndxi), jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dunelength, UNC_LOC_S, bfmpar%dunelength(1:ndxndxi), jabndnd=jabndnd_)
      endif
      !
      if (bfmpar%lfbedfrmrou) then
         if (.not. allocated(rks)) then
            allocate(rks(1:ndx))
            rks = 0d0
         endif
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ksr,  UNC_LOC_S, bfmpar%rksr(1:ndxndxi), jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ksmr, UNC_LOC_S, bfmpar%rksmr(1:ndxndxi), jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ksd,  UNC_LOC_S, bfmpar%rksd(1:ndxndxi), jabndnd=jabndnd_)

         do nm = 1,ndxndxi
            rks(nm) = sqrt(bfmpar%rksr(nm)**2 + bfmpar%rksmr(nm)**2 + bfmpar%rksd(nm)**2)
         enddo
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ks,   UNC_LOC_S, rks(1:ndxndxi), jabndnd=jabndnd_)
      endif
   endif

   ! Sediment transport (via own built-in sed)
   if (jamapsed > 0 .and. jased > 0 .and. .not.stm_included) then
      do j = 1,mxgr
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sed(:,j), UNC_LOC_S, sed(j,:), jabndnd=jabndnd_) ! ,  (/ 1, 1, itim /), (/ mxgr, ndxndxi, 1 /))
      enddo
      if (jaceneqtr .eq. 1) then
         do j = 1,mxgr
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ero(:,j), UNC_LOC_S, grainlay(j,:), jabndnd=jabndnd_)
         enddo
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_bl , UNC_LOC_S, bl, jabndnd=jabndnd_)
      else
         do j = 1,mxgr
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ero(:,j), UNC_LOC_CN, grainlay(j,:), jabndnd=jabndnd_)
         enddo
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_bl , UNC_LOC_CN, zk, jabndnd=jabndnd_)
      endif

      ! TODO: AvD: size(grainlay,2) is always correct (mxn), but we have a problem if jaceneqtr==2 and mxn/=numk,
      ! because then the dimension for ero is set to nNetNode, and coordinate attribute refers to NetNode_x
      ! (both length numk), whereas ero itself is shorter than numk.
   endif

   ! Meteo forcings
   if (jawind > 0) then
      allocate (windx(ndxndxi), windy(ndxndxi), stat=ierr)
      if (ierr /= 0) call aerr( 'windx/windy', ierr, ndxndxi)

      if (jamapwind > 0) then
         call linktonode2(wx,wy,windx,windy, ndxndxi)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windx , UNC_LOC_S, windx, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windy , UNC_LOC_S, windy, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windxu, UNC_LOC_U, wx   , jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windyu, UNC_LOC_U, wy   , jabndnd=jabndnd_)
      endif

      if (jamapwindstress > 0) then
         call linktonode2(wdsu_x,wdsu_y,windx,windy, ndxndxi)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressx, UNC_LOC_S, windx, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressy, UNC_LOC_S, windy, jabndnd=jabndnd_)
      endif

      deallocate(windx, windy, stat=ierr)

   endif

   if (ja_airdensity + ja_computed_airdensity > 0 .and. jamap_airdensity > 0) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_airdensity  , UNC_LOC_S, airdensity, jabndnd=jabndnd_)
   endif

   ! Rain
   if (jamaprain > 0 .and. jarain /= 0) then
      call realloc(scaled_rain, ndx, keepExisting = .false., fill = dmiss)
      do n=1,ndxndxi
         scaled_rain(n) = rain(n)*bare(n)/ba(n)*1d-3/(24d0*3600d0) ! mm/day->(m3/s / m2) Average actual rainfall rate on grid cell area (maybe zero bare).
      enddo
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rain  , UNC_LOC_S, scaled_rain, jabndnd=jabndnd_)
      deallocate(scaled_rain)
   endif

   ! Interception
   if (jamapicept > 0 .and. interceptionmodel /= DFM_HYD_NOINTERCEPT) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_icepths  , UNC_LOC_S, InterceptHs, jabndnd=jabndnd_)
   endif

   if (jamapwind > 0 .and. japatm > 0) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_patm  , UNC_LOC_S, patm, jabndnd=jabndnd_)
   endif

   if (ice_mapout) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ice_af , UNC_LOC_S, ice_af, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ice_h  , UNC_LOC_S, ice_h, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ice_p  , UNC_LOC_S, ice_p, jabndnd=jabndnd_)
      if (ja_icecover == ICECOVER_SEMTNER) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ice_t  , UNC_LOC_S, ice_t, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_snow_h , UNC_LOC_S, snow_h, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_snow_t , UNC_LOC_S, snow_t, jabndnd=jabndnd_)
      endif
   endif



   ! Heat flux models
   if (jamapheatflux > 0 .and. jatem > 1) then ! here less verbose

      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp   , mapids%id_tair  , UNC_LOC_S, Tair, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp   , mapids%id_rhum  , UNC_LOC_S, Rhum, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp   , mapids%id_clou  , UNC_LOC_S, Clou, jabndnd=jabndnd_)


      if (jatem == 5) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qsun  , UNC_LOC_S, Qsunmap, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qeva  , UNC_LOC_S, Qevamap, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qcon  , UNC_LOC_S, Qconmap, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qlong , UNC_LOC_S, Qlongmap, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qfreva, UNC_LOC_S, Qfrevamap, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qfrcon, UNC_LOC_S, Qfrconmap, jabndnd=jabndnd_)
      endif
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp   , mapids%id_qtot  , UNC_LOC_S, Qtotmap, jabndnd=jabndnd_)
   endif

   if (jamapwav>0) then
      if (flowWithoutWaves) then      ! Check the external forcing wave quantities and their associated arrays
         if (jamapwav_hwav > 0      .and. allocated(hwav)) then
            if (jamapsigwav==0) then
               wavfac = 1d0
            else
               wavfac = sqrt(2d0)
            endif
            if (allocated(wa)) deallocate(wa, stat = ierr)
            allocate(wa(1:ndx), stat=ierr)
            wa = wavfac*hwav
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav        , UNC_LOC_S, wa, jabndnd=jabndnd_)
         endif
         if (jamapwav_twav > 0   .and. allocated(twav)) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_twav        , UNC_LOC_S, twav, jabndnd=jabndnd_)
         endif
         if (jamapwav_phiwav > 0 .and. allocated(phiwav)) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_phiwav      , UNC_LOC_S, phiwav, jabndnd=jabndnd_)
         endif
         if (jamapwav_sxwav > 0  .and. allocated(sxwav)) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxwav       , UNC_LOC_S, sxwav, jabndnd=jabndnd_)
         endif
         if (jamapwav_sywav > 0  .and. allocated(sywav)) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sywav       , UNC_LOC_S, sywav, jabndnd=jabndnd_)
         endif
         if (jamapwav_sxbwav > 0 .and. allocated(sbxwav)) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxbwav      , UNC_LOC_S, sbxwav, jabndnd=jabndnd_)
         endif
         if (jamapwav_sybwav > 0 .and. allocated(sbywav)) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sybwav      , UNC_LOC_S, sbywav, jabndnd=jabndnd_)
         endif
         if (jamapwav_mxwav > 0  .and. allocated(mxwav)) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mxwav       , UNC_LOC_S, mxwav, jabndnd=jabndnd_)
         endif
         if (jamapwav_mywav > 0  .and. allocated(mywav)) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mywav       , UNC_LOC_S, mywav, jabndnd=jabndnd_)
         endif
         if (jamapwav_dsurf > 0  .and. allocated(dsurf)) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dsurf       , UNC_LOC_S, dsurf, jabndnd=jabndnd_)
         endif
         if (jamapwav_dwcap > 0  .and. allocated(dwcap)) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dwcap       , UNC_LOC_S, dwcap, jabndnd=jabndnd_)
         endif
         if (jamapwav_distot > 0  .and. allocated(distot)) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_distot      , UNC_LOC_S, distot, jabndnd=jabndnd_)
         endif
         if (jamapwav_uorb > 0   .and. allocated(uorbwav)) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_uorb        , UNC_LOC_S, uorbwav, jabndnd=jabndnd_)
         endif
      else   ! flowWithoutWaves
        ! JRE - XBeach
        if (jawave .eq. 4) then
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_E        , UNC_LOC_S, E, jabndnd=jabndnd_)
           if (roller>0) then
              ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_R        , UNC_LOC_S, R, jabndnd=jabndnd_)
              ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_DR       , UNC_LOC_S, DR, jabndnd=jabndnd_)
           endif
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_D        , UNC_LOC_S, D, jabndnd=jabndnd_)
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Df       , UNC_LOC_S, Df, jabndnd=jabndnd_)
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Sxx      , UNC_LOC_S, Sxx, jabndnd=jabndnd_)
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Syy      , UNC_LOC_S, Syy, jabndnd=jabndnd_)
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Sxy      , UNC_LOC_S, Sxy, jabndnd=jabndnd_)

           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sigmwav  , UNC_LOC_S, sigmwav, jabndnd=jabndnd_)
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cwav     , UNC_LOC_S, cwav, jabndnd=jabndnd_)
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cgwav    , UNC_LOC_S, cgwav, jabndnd=jabndnd_)
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_kwav     , UNC_LOC_S, kwav, jabndnd=jabndnd_)
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nwav     , UNC_LOC_S, nwav, jabndnd=jabndnd_)

           !if (windmodel.eq.0) then
              ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_l1       , UNC_LOC_S, L1, jabndnd=jabndnd_)
           !elseif ( (windmodel.eq.1) .and. (jawsource.eq.1 ) ) then
           !   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_SwE      , UNC_LOC_S, SwE, jabndnd=jabndnd_)
           !   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_SwT      , UNC_LOC_S, SwT, jabndnd=jabndnd_)
           !endif

           ierr = nf90_put_var(mapids%ncid, mapids%id_ctheta(2)   , ctheta(:,1:ndxndxi) , start = (/ 1, 1, itim /), count = (/ ntheta, ndxndxi, 1 /))
        endif

        if ((jawave == 3 .or. jawave==4) .and. kmx>0) then
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxwav,  UNC_LOC_S, sxwav, jabndnd=jabndnd_)
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sywav,  UNC_LOC_S, sywav, jabndnd=jabndnd_)
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxbwav, UNC_LOC_S, sbxwav, jabndnd=jabndnd_)
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sybwav, UNC_LOC_S, sbywav, jabndnd=jabndnd_)
        endif

        if (jawave .gt. 0) then
           if (jamapsigwav==0) then
              wavfac = 1d0
           else
              wavfac = sqrt(2d0)
           endif
           if (allocated(wa)) deallocate(wa, stat = ierr)
           allocate(wa(1:ndx), stat=ierr)
           wa = wavfac*hwav
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav, UNC_LOC_S, wa, jabndnd=jabndnd_)
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_uorb, UNC_LOC_S, uorb, jabndnd=jabndnd_)

           wa = modulo(270d0 - phiwav, 360d0)
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_thetamean, UNC_LOC_S, wa, jabndnd=jabndnd_)
           deallocate(wa)

           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_twav, UNC_LOC_S, twav)

           call realloc(ust_x, ndkx, keepExisting=.false.)
           call realloc(ust_y, ndkx, keepExisting=.false.)
           call reconstruct_cc_stokesdrift(ndkx,ust_x, ust_y)

           ! then write:
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ustokes      , iLocS, ust_x, jabndnd=jabndnd_)
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vstokes      , iLocS, ust_y, jabndnd=jabndnd_)
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ustokeslink  , iLocU, ustokes, jabndnd=jabndnd_)
           ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vstokeslink  , iLocU, vstokes, jabndnd=jabndnd_)

           ! Wave forces
           if (jawave == 3 .or. jawave==4) then
              call realloc(windx,ndkx,keepExisting=.false.,fill=0d0)   ! reuse scratch wind arrays, ust_x, y still needed for tausx,y
              call realloc(windy,ndkx,keepExisting=.false.,fill=0d0)
              call realloc(wavout,lnkx,keepExisting=.false.,fill=0d0)
              call realloc(wavout2,lnkx,keepExisting=.false.,fill=0d0)
              wavout=0d0; wavout2=0d0
              if (kmx==0) then
                 do L= 1, lnx
                    k1 = ln(1,L); k2=ln(2,L)
                    windx(k1)  = windx(k1) + wcx1(L)*wavfu(L)*hu(L)*rhomean
                    windx(k2)  = windx(k2) + wcx2(L)*wavfu(L)*hu(L)*rhomean
                    windy(k1)  = windy(k1) + wcy1(L)*wavfu(L)*hu(L)*rhomean
                    windy(k2)  = windy(k2) + wcy2(L)*wavfu(L)*hu(L)*rhomean
                    wavout(L)  = wavfu(L)*hu(L)*rhomean   ! stack
                    wavout2(L) = wavfv(L)*hu(L)*rhomean
                 enddo
              else
                 do L = 1, lnx
                    call getLbotLtop(L,Lb,Lt)
                    if (Lt<Lb) cycle
                    do LL=Lb, Lt
                       k1 = ln(1,LL); k2 = ln(2,LL)
                       windx(k1)   = windx(k1) + wcx1(L)*wavfu(LL)*hu(L)*rhomean   ! consider rhoL here
                       windx(k2)   = windx(k2) + wcx2(L)*wavfu(LL)*hu(L)*rhomean
                       windy(k1)   = windy(k1) + wcy1(L)*wavfu(LL)*hu(L)*rhomean
                       windy(k2)   = windy(k2) + wcy2(L)*wavfu(LL)*hu(L)*rhomean
                       wavout(LL)  = wavfu(LL)*hu(L)*rhomean   ! stack
                       wavout2(LL) = wavfv(LL)*hu(L)*rhomean
                    enddo
                 enddo
              endif
              ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fx       , iLocS, windx,   jabndnd=jabndnd_)
              ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fy       , iLocS, windy,   jabndnd=jabndnd_)
              ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fxlink   , iLocU, wavout,  jabndnd=jabndnd_)
              ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fylink   , iLocU, wavout2, jabndnd=jabndnd_)
              deallocate(wavout, wavout2)
           endif
        endif
      endif   ! flowWithoutWaves
   endif

   ! Bed shear stress and roughness
   !
   ! Tau current and Chezy
   ! The "logic":
   ! - If no waves:
   !     + Bed shear stress is derived from current-only u* in gettaus (jawaveswartdelwaq=0);
   !     + Bed shear stress for morphology from settaubxu_nowave, or Soulsby-Clarke, filled in array sedtra%taub in fm_erosed.
   ! - If waves present:
   !  * for fetch models (jawave 1,2):
   !     + taus depends on jawaveswartdelwaq;
   !     + taus derived from taubxu (jawaveswartdelwaq==2) calculated in getustbcfuhi (3D), in 2D in tauwave().
   !  * for swan etc (jawave>2):
   !     + taus for output are calculated in gettauswave(), based on jawaveswartdelwaq:
   !        * 0: taus based on soulsby wave-current formulas taubu's
   !        * 1: taus linear sum like gettau2
   !        * 2: taus = sedtra%taub if sediment included, otherwise based on taubxu from wave shear stress subroutines

   !
   if (jamaptaucurrent > 0 .or. jamap_chezy_elements > 0 .or. jamap_chezy_links > 0 ) then
      if (jawave==0) then        ! Else, get taus from subroutine tauwave (taus = f(taucur,tauwave))
         call gettaus(1,1)
         workx=DMISS; worky=DMISS
         if (kmx==0) then
            do k = 1, ndx   ! stack
               workx(k) = taus(k)*ucx(k)/max(hypot(ucx(k),ucy(k)),1d-4)  ! could use ucmag, but not guaranteed to exist
               worky(k) = taus(k)*ucy(k)/max(hypot(ucx(k),ucy(k)),1d-4)
            enddo
         else
            do k = 1, ndx
               call getkbotktop(k,kb,kt)
               ux = ucx(kb); uy = ucy(kb)
               um = max(hypot(ux,uy),1d-4)
               workx(k) = taus(k)*ux/um
               worky(k) = taus(k)*uy/um
            enddo
         endif
      else if (jamap_chezy_links > 0) then
         call gettaus(2,1)       ! Only update czs
      endif

      if (jawave>0 .and. .not. flowWithoutWaves) then
         call gettauswave(jawaveswartdelwaq)
      endif
   endif

   if (jamap_chezy_links > 0) then
      do LL = 1,lnx
         if (frcu(LL) > 0d0) then
            call getcz (hu(LL), frcu(LL), ifrcutp(LL), czu(LL), LL)  ! in gettaus czu is calculated but not stored
         endif
      enddo
   endif
   !
   if (jamaptaucurrent>0) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tausx, UNC_LOC_S, workx(1:ndx), jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tausy, UNC_LOC_S, worky(1:ndx), jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_taus, UNC_LOC_S, taus, jabndnd=jabndnd_)
      if (stm_included) then
        ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tausmax, UNC_LOC_S, sedtra%taub, jabndnd=jabndnd_)   ! sedtra%taub=reconstruction of tausmax, or Soulsby-Clarke
      endif                                                                                                               ! JRE+BJ to do: keep this one, or through moroutput
   endif

   if ( jatidep > 0 .and. jamaptidep == 1 ) then
     if ( jaselfal == 0 ) then
        do k = 1, Ndx
            workx(k) = tidep(1,k)
        enddo
     else ! write potential without SAL and SAL potential
        do k = 1, Ndx
          workx(k) = tidep(1,k) - tidep(2,k)
        enddo
     endif
     ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tidep, UNC_LOC_S, workx(1:ndx), jabndnd=jabndnd_)
   endif
   if ( jaselfal > 0 .and. jamapselfal == 1 ) then
     do k = 1, Ndx
        workx(k) = tidep(2,k)
     enddo
     ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_salp, UNC_LOC_S, workx(1:ndx), jabndnd=jabndnd_)
   endif

   if ( jaFrcInternalTides2D >  0 .and. jamapIntTidesDiss == 1 ) then
     ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_inttidesdiss, UNC_LOC_S, DissInternalTidesPerArea(1:ndx), jabndnd=jabndnd_)
   endif


   if (jamap_chezy_elements > 0) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_czs , UNC_LOC_S, czs, jabndnd=jabndnd_)
   endif
   if (jamap_chezy_links > 0) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_czu , UNC_LOC_U, czu, jabndnd=jabndnd_)
   endif
   if (jamap_chezy_input > 0) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cfu , UNC_LOC_U, frcu, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cfutyp , UNC_LOC_U, ifrcutp, jabndnd=jabndnd_)
   endif

   ! Roughness from trachytopes
   if (jamaptrachy > 0 .and. jatrt == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, UNC_LOC_L, cftrt(:,2), jabndnd=jabndnd_)
   endif

   ! Calibration factor for roughness from trachytopes
   if (jamapcali > 0 .and. jacali == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cfcl, UNC_LOC_L, cfclval, jabndnd=jabndnd_)
   endif

   ! JRE debug variables
   if (jawritedebug) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dbg1d, UNC_LOC_U, debugarr1d(1:lnx), jabndnd=jabndnd_)

      if (allocated(debugarr2d)) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dbg2d, UNC_LOC_S, debugarr2d(1:ndxndxi,:), jabndnd=jabndnd_)
      endif

      if (allocated(debugarr3d)) then
         !ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dbg3d, UNC_LOC_L, cfclval, jabndnd=jabndnd_)
      endif

   endif


   ! water quality bottom variables
    if (numwqbots > 0) then
       do j=1,numwqbots
          do k=1,ndxndxi
             call getkbotktop(k,kb,kt)
             workx(k) = wqbot(j,kb)
          enddo
          ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqb(:,j), UNC_LOC_S, workx(1:ndxndxi), jabndnd=jabndnd_)
          if (wqbot3D_output == 1) then
!         also write 3D
             do kk=1,ndxndxi
                call getkbotktop(kk,kb,kt)
                do k = kb,kt
                   workx(k) = wqbot(j,k)
                enddo
             enddo
             ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqb3d(:,j), UNC_LOC_S3D, workx, jabndnd=jabndnd_)
          endif
       enddo
    endif

    ! WAQ output
    if (jawaqproc > 0) then
       do j=1,noout_map
          if (outvar(j)>0)then
             workx = DMISS ! For proper fill values in z-model runs.
             if ( kmx>0 ) then
!               3D
                do kk=1,ndxndxi
                   call getkbotktop(kk,kb,kt)
                   do k = kb,kt
                      workx(k) = waqoutputs(j,k-kbx+1)
                   enddo
                enddo
                ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_waq(:,j), UNC_LOC_S3D, workx, jabndnd=jabndnd_)
             else
!               2D
                do kk=1,NdxNdxi
                   workx(kk) = waqoutputs(j,kk)
                enddo
                ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_waq(:,j), UNC_LOC_S, workx, jabndnd=jabndnd_)
             endif
          endif
       enddo
       do j=1,noout_statt
          jj = noout_user + j
          if (outvar(jj)>0)then
             workx = DMISS ! For proper fill values in z-model runs.
             if ( kmx>0 ) then
!               3D
                do kk=1,ndxndxi
                   call getkbotktop(kk,kb,kt)
                   do k = kb,kt
                      workx(k) = waqoutputs(jj,k-kbx+1)
                   enddo
                enddo
                ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqst(:,j), UNC_LOC_S3D, workx, jabndnd=jabndnd_)
             else
!               2D
                do kk=1,NdxNdxi
                   workx(kk) = waqoutputs(jj,kk)
                enddo
                ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqst(:,j), UNC_LOC_S, workx, jabndnd=jabndnd_)
             endif
          endif
       enddo
       if (comparereal(tim, ti_mape, eps10) == 0) then
          do j=1,noout_state
             jj = noout_user + noout_statt + j
             if (outvar(jj)>0)then
                workx = DMISS ! For proper fill values in z-model runs.
                if ( kmx>0 ) then
!                  3D
                   do kk=1,ndxndxi
                      call getkbotktop(kk,kb,kt)
                      do k = kb,kt
                         workx(k) = waqoutputs(jj,k-kbx+1)
                      enddo
                   enddo
                   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqse(:,j), UNC_LOC_S3D, workx, jabndnd=jabndnd_)
                else
!                  2D
                   do kk=1,NdxNdxi
                      workx(kk) = waqoutputs(jj,kk)
                   enddo
                   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqse(:,j), UNC_LOC_S, workx, jabndnd=jabndnd_)
                endif
             endif
          enddo
      endif
    endif

   if ( janudge.gt.0 .and. jamapnudge.gt.0 ) then
!    nudging
     ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_tem, UNC_LOC_S3D, nudge_tem, jabndnd=jabndnd_)
     ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_sal, UNC_LOC_S3D, nudge_sal, jabndnd=jabndnd_)

     workx = DMISS
     do k=1,ndkx
        if ( nudge_tem(k).ne.DMISS ) then
           workx(k) = nudge_tem(k)-constituents(itemp, k)
        endif
     enddo
     ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_Dtem, UNC_LOC_S3D, workx, jabndnd=jabndnd_)

     workx = DMISS
     do k=1,ndkx
        if ( nudge_tem(k).ne.DMISS ) then
           workx(k) = nudge_sal(k)-constituents(isalt,k)
        endif
     enddo
     ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_Dsal, UNC_LOC_S3D, workx, jabndnd=jabndnd_)
   endif

   if (javeg > 0) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rnveg , UNC_LOC_S, rnveg, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_diaveg , UNC_LOC_S, diaveg, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_veg_stemheight , UNC_LOC_S, stemheight, jabndnd=jabndnd_)
   endif
   
   if (ndxi-ndx2d>0 .and. jamapPure1D_debug) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_adve, UNC_LOC_U, adve(:), jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_advi, UNC_LOC_U, advi(:), jabndnd=jabndnd_)
   endif
   
   if (ndxi-ndx2d>0 .and. jaPure1D>=3 .and. jamapPure1D_debug) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1d_1, UNC_LOC_U, q1d(1,:), jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1d_2, UNC_LOC_U, q1d(2,:), jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_volu1d, UNC_LOC_U, volu1D(:), jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_au1d_1, UNC_LOC_U, au1d(1,:), jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_au1d_2, UNC_LOC_U, au1d(2,:), jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wu1d_1, UNC_LOC_U, wu1d(1,:), jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wu1d_2, UNC_LOC_U, wu1d(2,:), jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sar1d_1, UNC_LOC_U, sar1d(1,:), jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sar1d_2, UNC_LOC_U, sar1d(2,:), jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_alpha_mom_1d, UNC_LOC_S, alpha_mom_1d, jabndnd=jabndnd_)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_alpha_ene_1d, UNC_LOC_S, alpha_ene_1d, jabndnd=jabndnd_)
   endif
      
   if (ndxi-ndx2d>0 .and. network%loaded) then
      if (jamapTimeWetOnGround > 0) then ! Cumulative time water above ground level
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_timewetground, UNC_LOC_S, time_wetground, jabndnd=jabndnd_)
      endif
      if (jamapFreeboard > 0) then ! freeboard
         ierr = nf90_put_var(mapids%ncid, mapids%id_freeboard(1), freeboard, start = (/ 1,mapids%id_tsp%idx_curtime /))
      endif
      if (jamapDepthOnGround > 0) then ! waterdepth that is above ground level
         ierr = nf90_put_var(mapids%ncid, mapids%id_hs_on_ground(1), hsOnGround, start = (/ 1,mapids%id_tsp%idx_curtime /))
      endif
      if (jamapVolOnGround > 0) then ! volume that is above ground level
         ierr = nf90_put_var(mapids%ncid, mapids%id_vol_on_ground(1), volOnGround, start = (/ 1,mapids%id_tsp%idx_curtime /))
      endif
      if (jamapTotalInflow1d2d > 0) then ! total 1d2d inflow
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qCur1d2d, UNC_LOC_S, qCur1d2d, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vTot1d2d, UNC_LOC_S, vTot1d2d, jabndnd=jabndnd_)
      endif
      if (jamapTotalInflowLat > 0) then ! total lateral inflow
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qCurLat, UNC_LOC_S, qCurLat, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vTotLat, UNC_LOC_S, vTotLat, jabndnd=jabndnd_)
      endif
   endif
   if (lnx1d > 0) then
      if (jamapS1Gradient > 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s1Gradient, UNC_LOC_U, s1Gradient, jabndnd=jabndnd_)
      endif
   endif
   !
   ! Nearfield
   !
   if (jamapNearField == 1) then
      call realloc(work1d, ndkx, keepExisting = .false., fill = 0.0d0)
      do isrc= numsrc-numsrc_nf+1, numsrc
         !
         ! Sinks
         n = ksrc(1,isrc)
         if (n /= 0) then
            call getkbotktop(n,kbot_,ktop_)
            nkbot = kbot_
            nktop = ktop_
            do nk = kbot_, ktop_
               if (zws(nk) < zsrc (1,isrc)) nkbot = nk
               if (zws(nk) < zsrc2(1,isrc)) nktop = nk
            enddo
            do nk = nkbot, nktop
               work1d(nk) = work1d(nk) - qstss((1+numconst)*(isrc-1)+1) / real(nktop-nkbot+1,hp)
            enddo
         endif
         !
         ! Sources
         n = ksrc(4,isrc)
         if (n /= 0) then
            call getkbotktop(n,kbot_,ktop_)
            nkbot = kbot_
            nktop = ktop_
            do nk = kbot_, ktop_
               if (zws(nk) < zsrc (2,isrc)) nkbot = nk
               if (zws(nk) < zsrc2(2,isrc)) nktop = nk
            enddo
            do nk = nkbot, nktop
               work1d(nk) = work1d(nk) + qstss((1+numconst)*(isrc-1)+1) / real(nktop-nkbot+1,hp)
            enddo
         endif
      enddo
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nrfld, UNC_LOC_S3D, work1d, jabndnd=jabndnd_)
   endif
   if (timon) call timstop (handle_extra(73))
   if (timon) call timstop (handle_extra(70))

end subroutine unc_write_map_filepointer_ugrid


!> Writes map/flow data to an already opened netCDF dataset.
!! The netnode and -links have been written already.
subroutine unc_write_map_filepointer(imapfile, tim, jaseparate) ! wrimap
   use m_flow
   use m_flowtimes
   use m_flowgeom
   use m_sobekdfm
   use m_heatfluxes
   use m_sferic
   use network_data
   use m_sediment
   use m_bedform
   use m_wind
   use m_flowparameters, only: jatrt, jacali
   use m_mass_balance_areas
   use m_fm_wq_processes
   use m_xbeach_data
   use m_transportdata
   use bedcomposition_module, only: bedcomp_getpointer_integer
   use m_alloc
   use m_missing
   use string_module, only: replace_multiple_spaces_by_single_spaces
   use netcdf_utils, only: ncu_append_atts
   use m_fm_icecover, only: ice_mapout, ice_af, ice_h, ice_p, ice_t, snow_h, snow_t, ja_icecover, ICECOVER_SEMTNER
   use netcdf, only: nf90_inquire, nf90_inq_dimid, nf90_def_dim, nf90_unlimited, nf90_def_var, nf90_double, nf90_put_att, &
                     nf90_int, nf90_noerr, nf90_global, nf90_inq_varid, nf90_inquire_variable, nf90_enddef, nf90_put_var
   use unstruc_netcdf, only: unc_write_flowgeom_filepointer, unc_add_gridmapping_att, unc_write_net_filepointer, &
                             check_error, linktonode2, unc_append_3dflowgeom_def, unc_append_3dflowgeom_put, &
                             definencvar
   use string_module, only: replace_char

   integer,           intent(in) :: imapfile
   real(kind=hp),     intent(in) :: tim
   integer, optional, intent(in) :: jaseparate   !< Whether this save is manual/by user (not part of the standard map write series)

   integer                       :: jaseparate_, idims(2)

   logical, dimension(2), save   :: firststep = .true.

   integer, save                 :: ierr, ndim
   integer, dimension(2), save   :: &
   !id_netcelldim, id_netcellmaxnodedim, id_netcellcontourptsdim, &
   id_laydim, id_wdim, &
       id_flowelemdim, &
   id_maxfracdim,  &
   id_erolaydim,   &
   id_flowlinkdim, &
   id_netlinkdim,  &
   id_1d2ddim,     &
   id_timedim,     &
   id_time, id_timestep, &
   id_sbcx, id_sbcy, id_sbcx_reconstructed, id_sbcy_reconstructed, &
   id_sbwx, id_sbwy, id_sbwx_reconstructed, id_sbwy_reconstructed, &
   id_sswx, id_sswy, id_sswx_reconstructed, id_sswy_reconstructed, &
   id_sourse, id_sinkse, id_ws, &
   id_sxtot, id_sytot, id_rsedeq, id_umod, id_zumod, id_ustar, id_dzdn, id_dzdt, id_morbl, id_aks, id_rca, &
   id_bodsed, id_dpsed, id_msed, id_lyrfrac, id_thlyr, id_poros, id_nlyrdim, &
   id_sedtotdim, id_sedsusdim, id_rho, id_rhop, id_viu, id_diu, id_q1, id_spircrv, id_spirint, &
   id_q1main, &
   id_s1, id_taus, id_ucx, id_ucy, id_ucz, id_ucxa, id_ucya, id_unorm, id_ww1, id_sa1, id_tem1, id_sed, id_ero, id_s0, id_u0, id_cfcl, id_cftrt, id_czs, id_czu, &
   id_qsun, id_qeva, id_qcon, id_qlong, id_qfreva, id_qfrcon, id_qtot, &
   id_patm, id_ice_af, id_ice_h, id_ice_p, id_ice_t, id_snow_h, id_snow_t, id_tair, id_rhum, id_clou, id_E, id_R, id_H, id_D, id_DR, id_urms, id_thetamean, &
   id_cwav, id_cgwav, id_sigmwav, &
   id_ust, id_vst, id_windx, id_windy, id_windxu, id_windyu, id_numlimdt, id_hs, id_bl, id_zk, &
   id_1d2d_edges, id_1d2d_zeta1d, id_1d2d_crest_level, id_1d2d_b_2di, id_1d2d_b_2dv, id_1d2d_d_2dv, id_1d2d_q_zeta, id_1d2d_q_lat, &
   id_1d2d_cfl, id_1d2d_flow_cond, id_1d2d_sb, id_1d2d_s1_2d, id_1d2d_s0_2d, id_tidep, id_salp, id_inttidesdiss, &
   id_duneheight, id_dunelength, id_ksd, id_ksr, id_ksmr, id_ks, &
   id_taurat, id_dm, id_dg, id_dgsd, id_frac, id_mudfrac, id_sandfrac, id_fixfac, id_hidexp, id_mfluff, id_scrn, id_urmscc, id_Fxcc, id_Fycc, &
   id_sscx, id_sscy, id_sscx_reconstructed, id_sscy_reconstructed, &
   id_turkin1, id_tureps1, id_vicwwu, id_vicwws, id_swanbl, &
   id_rnveg, id_diaveg, id_veg_stemheight

   integer,          dimension(:,:),   allocatable, save :: id_dxx                     ! fractions
   double precision, dimension(:),     allocatable       :: dum
   double precision, dimension(:,:),   allocatable       :: poros
   double precision, dimension(:,:,:), allocatable       :: frac
   double precision, dimension(:),     allocatable       :: toutput
   double precision, dimension(:,:),   allocatable       :: toutputx, toutputy
   double precision, dimension(:),     allocatable       :: rks

   integer,          dimension(:), allocatable :: idum

   integer :: iid, i, j, jj, itim, k, kb, kt, kk, n, LL, Ltx, Lb, L, nm, nlayb,nrlay, nlaybL, nrlayLx, varid, ndims
   integer :: ndxndxi ! Either ndx or ndxi, depending on whether boundary nodes also need to be written.
   double precision, dimension(:), allocatable :: windx, windy
   double precision, dimension(:), allocatable :: numlimdtdbl ! TODO: WO/AvD: remove this once integer version of unc_def_map_var is available
   double precision :: vicc, dicc
   integer :: jaeulerloc

   double precision   :: rhol
   character(16)      :: dxname, zw_elem, zcc_elem, zwu_link, zu_link
   character(64)      :: dxdescr
   character(10)      :: transpunit
   character(len=255) :: tmpstr

   integer, dimension(:), allocatable :: flag_val
   character(len=10000)               :: flag_mean

   double precision, dimension(:), pointer :: dens

   if (.not. allocated(id_dxx) .and. stm_included) allocate(id_dxx(1:stmpar%morpar%nxx,1:2))

   ! If jaseparate_==1 or this map file was just opened for the first time:
   ! only write net+vardefs first time, and write subsequent flow snapshots in later calls.
   ! jaseparate_==2: write com file
   if (present(jaseparate)) then
       jaseparate_ = jaseparate
   else
       jaseparate_ = 0
   endif

   if (jaseparate_ == 0 .or. jaseparate_ == 1) then
      ! mapfile, store/use ids number 1
      iid = 1
      ndxndxi = ndxi
   elseif (jaseparate_ == 2) then
      ! comfile, store/use ids number 2
      iid = 2
      ndxndxi = ndx ! Com file, include boundary nodes
   else
      ! error
      iid = 0
   endif

   ! Use nr of dimensions in netCDF file a quick check whether vardefs were written
   ! before in previous calls.
   ndim = 0
   ierr = nf90_inquire(imapfile, nDimensions=ndim)

   ! Only write net and flow geometry data the first time, or for a separate map file.
   if (ndim == 0) then

       call unc_write_net_filepointer(imapfile)      ! Write standard net data as well

       if (jaseparate_ == 2) then
          call unc_write_flowgeom_filepointer(imapfile, jabndnd = 1) ! Write time-independent flow geometry data, with boundary nodes
          ierr = nf90_inq_dimid(imapfile, 'nFlowElemWithBnd', id_flowelemdim(iid))
       else
          call unc_write_flowgeom_filepointer(imapfile) ! Write time-independent flow geometry data
          ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim(iid))
       endif

       ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim(iid))
       ierr = nf90_inq_dimid(imapfile, 'nNetLink' , id_netlinkdim(iid))

       if (nbnd1d2d > 0) then
          ierr = nf90_def_dim(imapfile, 'nBnd1d2d', nbnd1d2d, id_1d2ddim(iid))
       endif

       ! Time
       ierr = nf90_def_dim(imapfile, 'time', nf90_unlimited, id_timedim(iid))
       call check_error(ierr, 'def time dim')
       ierr = nf90_def_var(imapfile, 'time', nf90_double, id_timedim(iid),  id_time(iid))
       ierr = nf90_put_att(imapfile, id_time(iid),  'units'        , trim(Tudunitstr))
       ierr = nf90_put_att(imapfile, id_time(iid),  'standard_name', 'time')

       ! 3D
       if ( kmx > 0 ) then
          call unc_append_3dflowgeom_def(imapfile)              ! Append definition of time-independent 3d flow geometry data
          ierr = nf90_inq_dimid(imapfile, 'laydim', id_laydim(iid))
          ierr = nf90_inq_dimid(imapfile, 'wdim', id_wdim(iid))
       endif

       ! Size of latest timestep
       ierr = nf90_def_var(imapfile, 'timestep', nf90_double, id_timedim(iid),  id_timestep(iid))
       ierr = nf90_put_att(imapfile, id_timestep(iid),  'units'        , 'seconds')
       ierr = nf90_put_att(imapfile, id_timestep(iid),  'standard_name', 'timestep')

       if (jamaps1 > 0 .or. jaseparate_==2) then
           ! Flow data on centres: water level at latest timestep
           ierr = nf90_def_var(imapfile, 's1',  nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_s1(iid))
           ierr = nf90_put_att(imapfile, id_s1(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           ierr = nf90_put_att(imapfile, id_s1(iid),   'standard_name', 'sea_surface_height') ! sorry for inland water people
           ierr = nf90_put_att(imapfile, id_s1(iid),   'long_name'    , 'water level')
           ierr = nf90_put_att(imapfile, id_s1(iid),   'units'        , 'm')
           ierr = unc_add_gridmapping_att(imapfile, (/ id_s1(iid) /), jsferic)
       endif

       if (jaseparate_ == 0 .or. jaseparate_ == 1) then ! to mapfile
           ! Flow data on centres: water level timestep before the latest timestep

           if (jamaps0 > 0) then
               ierr = nf90_def_var(imapfile, 's0',  nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_s0(iid))
               ierr = nf90_put_att(imapfile, id_s0(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_s0(iid),   'standard_name', 'sea_surface_height') ! sorry for inland water people
               ierr = nf90_put_att(imapfile, id_s0(iid),   'long_name'    , 'water level at previous timestep')
               ierr = nf90_put_att(imapfile, id_s0(iid),   'units'        , 'm')
               ierr = unc_add_gridmapping_att(imapfile, (/ id_s0(iid) /), jsferic)
           endif

           idims(1) = id_flowelemdim(iid)
           idims(2) = id_timedim(iid)

           if (jamaphs > 0) then
               call definencvar(imapfile,id_hs(iid)   ,nf90_double,idims,2, 'waterdepth'  , 'water depth', 'm', 'FlowElem_xcc FlowElem_ycc')
           endif

           if (jamapheatflux > 0 .and. jatem > 1) then ! Heat modelling only
              call definencvar(imapfile,id_tair(iid)   ,nf90_double,idims,2, 'Tair'  , 'air temperature', 'degC', 'FlowElem_xcc FlowElem_ycc')
              call definencvar(imapfile,id_rhum(iid)   ,nf90_double,idims,2, 'rhum'  , 'Relative humidity', ' ','FlowElem_xcc FlowElem_ycc')
              call definencvar(imapfile,id_clou(iid)   ,nf90_double,idims,2, 'clou'  , 'cloudiness', ' ', 'FlowElem_xcc FlowElem_ycc')

              if (jatem == 5) then
                 call definencvar(imapfile,id_qsun(iid)   ,nf90_double,idims,2, 'Qsun'  , 'solar influx', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
                 call definencvar(imapfile,id_Qeva(iid)   ,nf90_double,idims,2, 'Qeva'  , 'evaporative heat flux', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
                 call definencvar(imapfile,id_Qcon(iid)   ,nf90_double,idims,2, 'Qcon'  , 'sensible heat flux', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
                 call definencvar(imapfile,id_Qlong(iid)  ,nf90_double,idims,2, 'Qlong' , 'long wave back radiation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
                 call definencvar(imapfile,id_Qfreva(iid) ,nf90_double,idims,2, 'Qfreva', 'free convection evaporative heat flux', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
                 call definencvar(imapfile,id_Qfrcon(iid) ,nf90_double,idims,2, 'Qfrcon', 'free convection sensible heat flux', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
              endif

              call definencvar(imapfile,id_Qtot(iid)   ,nf90_double,idims,2, 'Qtot'  , 'total heat flux', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
           endif

           if (jamapnumlimdt > 0) then
               call definencvar(imapfile,id_numlimdt(iid)  ,nf90_double,idims,2, 'numlimdt' , 'number of times flow element was Courant limiting', '1', 'FlowElem_xcc FlowElem_ycc')
           endif

           if (jamaptaucurrent > 0) then
               ! Flow data on centres
               ierr = nf90_def_var(imapfile, 'taus' ,  nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_taus(iid))
               ierr = nf90_put_att(imapfile, id_taus(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_taus(iid),  'standard_name', 'taucurrent')
               ierr = nf90_put_att(imapfile, id_taus(iid),  'long_name'    , 'taucurrent in flow element')
               ierr = nf90_put_att(imapfile, id_taus(iid),  'units'        , 'N m-2')
           endif

           if (jamaptidep >0 .and. jatidep >0) then
              ierr = nf90_def_var(imapfile, 'TidalPotential', nf90_double, (/ id_flowelemdim(iid), id_timedim(iid)/), id_tidep(iid))
              ierr = nf90_put_att(imapfile, id_tidep(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
              ierr = nf90_put_att(imapfile, id_tidep(iid),  'standard_name', 'TidalPotential')
              ierr = nf90_put_att(imapfile, id_tidep(iid),  'long_name'    , 'Tidal Potential generated by celestial forces in flow element center')
              ierr = nf90_put_att(imapfile, id_tidep(iid),  'units'        , 'm2 s-2')
           endif
           if (jamapselfal > 0) then
              if ( jaselfal.gt.0 ) then
                 ierr = nf90_def_var(imapfile, 'SALPotential', nf90_double, (/ id_flowelemdim(iid), id_timedim(iid)/), id_salp(iid))
                 ierr = nf90_put_att(imapfile, id_salp(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_salp(iid),  'standard_name', 'SALPotential')
                 ierr = nf90_put_att(imapfile, id_salp(iid),  'long_name'    , 'Self-attraction and loading Potential in flow element center')
                 ierr = nf90_put_att(imapfile, id_salp(iid),  'units'        , 'm2 s-2')
              endif
           endif

           if (jaFrcInternalTides2D >0 .and. jamapIntTidesDiss >0) then
              ierr = nf90_def_var(imapfile, 'internal_tides_dissipation', nf90_double, (/ id_flowelemdim(iid), id_timedim(iid)/), id_IntTidesDiss(iid))
              ierr = nf90_put_att(imapfile, id_inttidesdiss(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
              ierr = nf90_put_att(imapfile, id_inttidesdiss(iid),  'standard_name', 'internal_tides_dissipation')
              ierr = nf90_put_att(imapfile, id_inttidesdiss(iid),  'long_name'    , 'internal tides dissipation in flow element center')
              ierr = nf90_put_att(imapfile, id_inttidesdiss(iid),  'units'        , 'J s-1 m-2')
           endif

           if (kmx > 0) then
               !     3D
               if (jamapu1 > 0) then
                   ierr = nf90_def_var(imapfile, 'unorm', nf90_double, (/ id_laydim(iid), id_flowlinkdim(iid), id_timedim (iid)/) , id_unorm(iid))
               endif
               if (jamapu0 > 0) then
                   ierr = nf90_def_var(imapfile, 'u0'   , nf90_double, (/ id_laydim(iid), id_flowlinkdim(iid), id_timedim (iid)/) , id_u0(iid)   )
               endif
               if (jamapq1 > 0) then
                   ierr = nf90_def_var(imapfile, 'q1'   , nf90_double, (/ id_laydim(iid), id_flowlinkdim(iid), id_timedim (iid)/) , id_q1(iid)   )
               endif
               if (jamapq1main > 0 .and. allocated(q1_main)) then
                   ierr = nf90_def_var(imapfile, 'q1main', nf90_double, (/ id_laydim(iid), id_flowlinkdim(iid), id_timedim (iid)/) , id_q1main(iid)   )
               endif
               if (jamapviu > 0) then
                   ierr = nf90_def_var(imapfile, 'viu'   , nf90_double, (/ id_laydim(iid), id_flowlinkdim(iid), id_timedim (iid)/) , id_viu(iid)   )
               endif
               if (jamapdiu > 0) then
                   ierr = nf90_def_var(imapfile, 'diu'   , nf90_double, (/ id_laydim(iid), id_flowlinkdim(iid), id_timedim (iid)/) , id_diu(iid)   )
               endif

               if (jamapucvec > 0) then
                  ! JRE Velocity vector needs to be written, irrespective of kmx, also for com file. Statements moved down outside if-clause
                  !    ierr = nf90_def_var(imapfile, 'ucx'   , nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_ucx(iid)  )
                  !    ierr = nf90_def_var(imapfile, 'ucy'   , nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_ucy(iid)  )
                   ierr = nf90_def_var(imapfile, 'ucz'  , nf90_double, (/ id_laydim(iid), id_flowelemdim(iid), id_timedim (iid)/) , id_ucz(iid)  )

                  ! Depth-averaged cell-center velocities in 3D:
                  ierr = nf90_def_var(imapfile, 'ucxa' , nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_ucxa(iid)  )
                  ierr = nf90_def_var(imapfile, 'ucya' , nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_ucya(iid)  )

               endif
               if (jamapww1 > 0) then
                   ierr = nf90_def_var(imapfile, 'ww1'  , nf90_double, (/ id_wdim(iid), id_flowelemdim(iid), id_timedim (iid)/) , id_ww1(iid))
               endif
               if (jamaprho > 0) then
                   if ( density_is_pressure_dependent() ) then
                       ierr = nf90_def_var(imapfile, 'density', nf90_double, (/ id_laydim(iid), id_flowelemdim(iid), id_timedim (iid)/) , id_rho(iid))
                   else
                       ierr = nf90_def_var(imapfile, 'rho',     nf90_double, (/ id_laydim(iid), id_flowelemdim(iid), id_timedim (iid)/) , id_rhop(iid))
                   endif
               endif
             !
               if (jamapucvec > 0) then
                 ierr = nf90_put_att(imapfile, id_ucz(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_ucz(iid),  'standard_name', 'upward_sea_water_velocity')
                 ierr = nf90_put_att(imapfile, id_ucz(iid),  'long_name'    , 'upward velocity on flow element center')
                 ierr = nf90_put_att(imapfile, id_ucz(iid),  'units'        , 'm s-1')
                 ierr = nf90_put_att(imapfile, id_ucz(iid),  '_FillValue'   , dmiss)

                 ierr = nf90_put_att(imapfile, id_ucxa(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 if (jsferic == 0) then
                    ierr = nf90_put_att(imapfile, id_ucxa(iid),  'standard_name', 'sea_water_x_velocity')
                 else
                    ierr = nf90_put_att(imapfile, id_ucxa(iid),  'standard_name', 'eastward_sea_water_velocity')
                 endif

                 ierr = nf90_put_att(imapfile, id_ucxa(iid),  'long_name'    , 'depth-averaged velocity on flow element center, x-component')
                 ierr = nf90_put_att(imapfile, id_ucxa(iid),  'units'        , 'm s-1')

                 ierr = nf90_put_att(imapfile, id_ucya(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 if (jsferic == 0) then
                    ierr = nf90_put_att(imapfile, id_ucya(iid),  'standard_name', 'sea_water_y_velocity')
                 else
                    ierr = nf90_put_att(imapfile, id_ucya(iid),  'standard_name', 'northward_sea_water_velocity')
                 endif
                 ierr = nf90_put_att(imapfile, id_ucya(iid),  'long_name'    , 'depth-averaged velocity on flow element center, y-component')
                 ierr = nf90_put_att(imapfile, id_ucya(iid),  'units'        , 'm s-1')
               endif
               if (jamapww1 > 0) then
                 ierr = nf90_put_att(imapfile, id_ww1(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_ww1(iid),  'standard_name', 'upward_sea_water_velocity')              ! same standard name allowed?
                 ierr = nf90_put_att(imapfile, id_ww1(iid),  'long_name'    , 'upward velocity on vertical interface')  ! (upward normal or upward)?
                 ierr = nf90_put_att(imapfile, id_ww1(iid),  'units'        , 'm s-1')
                 ierr = nf90_put_att(imapfile, id_ww1(iid),  '_FillValue'   , dmiss)
                 !?elevation
               endif
               if (jamaprho > 0) then
                 if ( density_is_pressure_dependent() ) then
                   ierr = nf90_put_att(imapfile, id_rho(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                   ierr = nf90_put_att(imapfile, id_rho(iid),  'standard_name', 'sea_water_density')
                   ierr = nf90_put_att(imapfile, id_rho(iid),  'long_name'    , 'flow mass density')
                   ierr = nf90_put_att(imapfile, id_rho(iid),  'units'        , 'kg m-3')
                   ierr = nf90_put_att(imapfile, id_rho(iid),  '_FillValue'   , dmiss)
                 else
                   ierr = nf90_put_att(imapfile, id_rhop(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                   ierr = nf90_put_att(imapfile, id_rhop(iid),  'standard_name', 'sea_water_potential_density')
                   ierr = nf90_put_att(imapfile, id_rhop(iid),  'long_name'    , 'flow mass potential density')
                   ierr = nf90_put_att(imapfile, id_rhop(iid),  'units'        , 'kg m-3')
                   ierr = nf90_put_att(imapfile, id_rhop(iid),  '_FillValue'   , dmiss)
                 endif
               endif
           endif  ! kmx>0

           if (kmx == 0) then
              if (jamapu1 > 0) then
                 ierr = nf90_def_var(imapfile, 'unorm' , nf90_double, (/ id_flowlinkdim(iid), id_timedim (iid)/) , id_unorm(iid))
              endif
              if (jamapu0 > 0) then
                 ierr = nf90_def_var(imapfile, 'u0'    , nf90_double, (/ id_flowlinkdim(iid), id_timedim (iid)/) , id_u0(iid)   )
              endif
              if (jamapq1 > 0) then
                 ierr = nf90_def_var(imapfile, 'q1'    , nf90_double, (/ id_flowlinkdim(iid), id_timedim (iid)/) , id_q1(iid)   )
              endif
              if (jamapq1main > 0 .and. allocated(q1_main)) then
                 ierr = nf90_def_var(imapfile, 'q1main', nf90_double, (/ id_flowlinkdim(iid), id_timedim (iid)/) , id_q1main(iid)   )
              endif
              if (jamapviu > 0) then
                 ierr = nf90_def_var(imapfile, 'viu'    , nf90_double, (/ id_flowlinkdim(iid), id_timedim (iid)/) , id_viu(iid)   )
              endif
              if (jamapdiu > 0) then
                 ierr = nf90_def_var(imapfile, 'diu'    , nf90_double, (/ id_flowlinkdim(iid), id_timedim (iid)/) , id_diu(iid)   )
              endif
           endif

           if (jamapu1 > 0) then
              ierr = nf90_put_att(imapfile, id_unorm(iid),'coordinates'  , 'FlowLink_xu FlowLink_yu')
              ierr = nf90_put_att(imapfile, id_unorm(iid),'long_name', 'normal component of sea_water_speed')
              ierr = nf90_put_att(imapfile, id_unorm(iid),'units'        , 'm s-1')
              ierr = nf90_put_att(imapfile, id_unorm(iid),'_FillValue'   , dmiss)
           endif

           if (jamapu0 > 0) then
              ierr = nf90_put_att(imapfile, id_u0(iid)   ,'coordinates'  , 'FlowLink_xu FlowLink_yu')
              ierr = nf90_put_att(imapfile, id_u0(iid)   ,'long_name',     'normal component of sea_water_speed at previous timestep')
              ierr = nf90_put_att(imapfile, id_u0(iid)   ,'units'        , 'm s-1')
              ierr = nf90_put_att(imapfile, id_u0(iid)   ,'_FillValue'   , dmiss)
           endif
           if (jamapq1 > 0) then
              ierr = nf90_put_att(imapfile, id_q1(iid)   ,'coordinates'  , 'FlowLink_xu FlowLink_yu')
              !ierr = nf90_put_att(imapfile, id_q1(iid)   ,'standard_name', 'discharge') ! not CF
              ierr = nf90_put_att(imapfile, id_q1(iid)   ,'long_name'    , 'flow flux')
              ierr = nf90_put_att(imapfile, id_q1(iid)   ,'units'        , 'm3 s-1')
              ierr = nf90_put_att(imapfile, id_q1(iid)   ,'_FillValue'   , dmiss)
           endif
           if (jamapq1main > 0 .and. allocated(q1_main)) then
              ierr = nf90_put_att(imapfile, id_q1main(iid)   ,'coordinates'  , 'FlowLink_xu FlowLink_yu')
              !ierr = nf90_put_att(imapfile, id_q1main(iid)   ,'standard_name', 'discharge') ! not CF
              ierr = nf90_put_att(imapfile, id_q1main(iid)   ,'long_name'    , 'flow flux in main channel')
              ierr = nf90_put_att(imapfile, id_q1main(iid)   ,'units'        , 'm3 s-1')
              ierr = nf90_put_att(imapfile, id_q1main(iid)   ,'_FillValue'   , dmiss)
           endif

           if (jamapviu > 0) then
              ierr = nf90_put_att(imapfile, id_viu(iid)   ,'coordinates'  , 'FlowLink_xu FlowLink_yu')
              ierr = nf90_put_att(imapfile, id_viu(iid)   ,'long_name',     'horizontal viscosity')
              ierr = nf90_put_att(imapfile, id_viu(iid)   ,'units'        , 'm2 s-1')
              ierr = nf90_put_att(imapfile, id_viu(iid)   ,'_FillValue'   , dmiss)
           endif
           if (jamapdiu > 0) then
              ierr = nf90_put_att(imapfile, id_diu(iid)   ,'coordinates'  , 'FlowLink_xu FlowLink_yu')
              ierr = nf90_put_att(imapfile, id_diu(iid)   ,'long_name',     'horizontal diffusivity')
              ierr = nf90_put_att(imapfile, id_diu(iid)   ,'units'        , 'm2 s-1')
              ierr = nf90_put_att(imapfile, id_diu(iid)   ,'_FillValue'   , dmiss)
           endif
       endif   ! jaseparate =/ 2
       !
       if (kmx==0) then
          if (jamapucvec > 0 .or. jaseparate_==2) then
              ierr = nf90_def_var(imapfile, 'ucx'   , nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_ucx(iid)  )
              ierr = nf90_def_var(imapfile, 'ucy'   , nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_ucy(iid)  )
          endif
       else
          if (jamapucvec > 0 .or. jaseparate_==2) then
             ierr = nf90_def_var(imapfile, 'ucx'  , nf90_double, (/ id_laydim(iid), id_flowelemdim(iid), id_timedim (iid)/) , id_ucx(iid)  )
             ierr = nf90_def_var(imapfile, 'ucy'  , nf90_double, (/ id_laydim(iid), id_flowelemdim(iid), id_timedim (iid)/) , id_ucy(iid)  )
          endif
       endif

       if (jamapucvec > 0 .or. jaseparate_==2) then
           ierr = nf90_put_att(imapfile, id_ucx(iid)  ,'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           if (jsferic == 0) then
              ierr = nf90_put_att(imapfile, id_ucx(iid)  ,'standard_name', 'sea_water_x_velocity')
           else
              ierr = nf90_put_att(imapfile, id_ucx(iid)  ,'standard_name', 'eastward_sea_water_velocity')
           endif

           if (jaeulervel==0 .or. jaseparate_==2) then
              ierr = nf90_put_att(imapfile, id_ucx(iid)  ,'long_name'    , 'velocity on flow element center, x-component')
           else
              ierr = nf90_put_att(imapfile, id_ucx(iid)  ,'long_name'    , 'Eulerian velocity on flow element center, x-component')
           endif
           ierr = nf90_put_att(imapfile, id_ucx(iid)  ,'units'        , 'm s-1')
           ierr = nf90_put_att(imapfile, id_ucx(iid)  ,'_FillValue'   , dmiss)

           ierr = nf90_put_att(imapfile, id_ucy(iid)  ,'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           if (jsferic == 0) then
              ierr = nf90_put_att(imapfile, id_ucy(iid)  ,'standard_name', 'sea_water_y_velocity')
           else
              ierr = nf90_put_att(imapfile, id_ucy(iid)  ,'standard_name', 'northward_sea_water_velocity')
           endif

           if (jaeulervel==0 .or. jaseparate_==2) then
              ierr = nf90_put_att(imapfile, id_ucy(iid)  ,'long_name'    , 'velocity on flow element center, y-component')
           else
              ierr = nf90_put_att(imapfile, id_ucy(iid)  ,'long_name'    , 'Eulerian velocity on flow element center, y-component')
           endif
           ierr = nf90_put_att(imapfile, id_ucy(iid)  ,'units'        , 'm s-1')
           ierr = nf90_put_att(imapfile, id_ucy(iid)  ,'_FillValue'   , dmiss)
       endif

       if (jaseparate_ /= 2) then
          if (jamapsal > 0 .and. jasal > 0) then
             if ( kmx > 0 ) then  !        3D
                ierr = nf90_def_var(imapfile, 'sa1' , nf90_double, (/ id_laydim(iid), id_flowelemdim (iid), id_timedim (iid)/) , id_sa1(iid))
             else
                ierr = nf90_def_var(imapfile, 'sa1' , nf90_double, (/ id_flowelemdim (iid), id_timedim (iid)/) , id_sa1(iid))
             endif
             ierr = nf90_put_att(imapfile, id_sa1(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_sa1(iid),  'standard_name', 'sea_water_salinity')
             ierr = nf90_put_att(imapfile, id_sa1(iid),  'long_name'    , 'salinity')
             ierr = nf90_put_att(imapfile, id_sa1(iid),  'units'        , '1e-3')
             ierr = nf90_put_att(imapfile, id_sa1(iid),  '_FillValue'   , dmiss)
          endif

          if (jamaptem > 0 .and. jatem > 0) then
             if ( kmx > 0 ) then !        3D
               ierr = nf90_def_var(imapfile, 'tem1' , nf90_double, (/ id_laydim(iid), id_flowelemdim(iid) , id_timedim(iid) /) , id_tem1(iid))
             else
               ierr = nf90_def_var(imapfile, 'tem1' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_tem1(iid))
             endif
             ierr = nf90_put_att(imapfile, id_tem1(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_tem1(iid),  'standard_name', 'sea_water_temperature')
             ierr = nf90_put_att(imapfile, id_tem1(iid),  'long_name'    , 'temperature')
             ierr = nf90_put_att(imapfile, id_tem1(iid),  'units'        , 'degC')
             ierr = nf90_put_att(imapfile, id_tem1(iid),  '_FillValue'   , dmiss)
          endif

!         tracers
          if (jamapconst > 0 .and. ITRA1 > 0) then
             do j=ITRA1,ITRAN
                tmpstr = const_names(j)
                ! Forbidden chars in NetCDF names: space, /, and more.
                call replace_char(tmpstr,32,95)
                call replace_char(tmpstr,47,95)
                if ( kmx > 0 ) then  !        3D
                   ierr = nf90_def_var(imapfile, trim(tmpstr), nf90_double, (/ id_laydim(iid), id_flowelemdim (iid), id_timedim (iid)/) , id_const(iid,j))
                else
                   ierr = nf90_def_var(imapfile, trim(tmpstr), nf90_double, (/ id_flowelemdim (iid), id_timedim (iid)/) , id_const(iid,j))
                endif
                ierr = nf90_put_att(imapfile, id_const(iid,j),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_const(iid,j),  'standard_name', trim(tmpstr))
                ierr = nf90_put_att(imapfile, id_const(iid,j),  'long_name'    , trim(tmpstr))
                if (const_units(j).ne.' ') then
                   tmpstr = const_units(j)
                else
                   tmpstr = '-'
                endif
                ierr = nf90_put_att(imapfile, id_const(iid,j),  'units'        , tmpstr)
                ierr = nf90_put_att(imapfile, id_const(iid,j),  '_FillValue'   , dmiss)
             enddo
          endif

!         water quality bottom variables
          if (numwqbots > 0) then
             call realloc(id_wqb, (/ 3, numwqbots /), keepExisting=.false., fill = 0)
             do j=1,numwqbots
                tmpstr = wqbotnames(j)
                ! Forbidden chars in NetCDF names: space, /, and more.
                call replace_char(tmpstr,32,95)
                call replace_char(tmpstr,47,95)
                ierr = nf90_def_var(imapfile, trim(tmpstr), nf90_double, (/ id_flowelemdim (iid), id_timedim (iid)/) , id_wqb(iid,j))
                ierr = nf90_put_att(imapfile, id_wqb(iid,j),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_wqb(iid,j),  'standard_name', trim(tmpstr))
                ierr = nf90_put_att(imapfile, id_wqb(iid,j),  'long_name'    , trim(tmpstr))
                tmpstr = wqbotunits(j)
                ierr = nf90_put_att(imapfile, id_wqb(iid,j),  'units'        , tmpstr)
                ierr = nf90_put_att(imapfile, id_wqb(iid,j),  '_FillValue'   , dmiss)
             enddo
             if (wqbot3D_output == 1) then
                call realloc(id_wqb3d, (/ 3, numwqbots /), keepExisting=.false., fill = 0)
                do j=1,numwqbots
                   tmpstr = wqbotnames(j)
                   ! Forbidden chars in NetCDF names: space, /, and more.
                   call replace_char(tmpstr,32,95)
                   call replace_char(tmpstr,47,95)
                   ierr = nf90_def_var(imapfile, trim(tmpstr)//'_3D', nf90_double, (/ id_laydim(iid), id_flowelemdim (iid), id_timedim (iid)/) , id_wqb3d(iid,j))
                   ierr = nf90_put_att(imapfile, id_wqb3d(iid,j),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                   ierr = nf90_put_att(imapfile, id_wqb3d(iid,j),  'standard_name', trim(tmpstr))
                   ierr = nf90_put_att(imapfile, id_wqb3d(iid,j),  'long_name'    , trim(tmpstr))
                   tmpstr = wqbotunits(j)
                   ierr = nf90_put_att(imapfile, id_wqb3d(iid,j),  'units'        , tmpstr)
                   ierr = nf90_put_att(imapfile, id_wqb3d(iid,j),  '_FillValue'   , dmiss)
                enddo
             endif
          endif

!         waq output
          if (jawaqproc > 0) then
             if (noout_map > 0) then
                call realloc(id_waq, (/ 3, noout_map /), keepExisting=.false., fill = 0)
                do j=1,noout_map
                   tmpstr = ' '
                   write (tmpstr, "('water_quality_output_',I0)") j
                   if ( kmx > 0 ) then  !        3D
                      ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/ id_laydim(iid), id_flowelemdim (iid), id_timedim (iid)/) , id_waq(iid,j))
                   else
                      ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/ id_flowelemdim (iid), id_timedim (iid)/) , id_waq(iid,j))
                   endif
                   tmpstr = trim(outputs%names(j))//' - '//trim(outputs%description(j))//' in flow element'
                   call replace_multiple_spaces_by_single_spaces(tmpstr)
                   ierr = nf90_put_att(imapfile, id_waq(iid,j),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                   ierr = nf90_put_att(imapfile, id_waq(iid,j),  'long_name'    , trim(outputs%names(j)))
                   ierr = nf90_put_att(imapfile, id_waq(iid,j),  'units'        , trim(outputs%units(j)))
                   ierr = nf90_put_att(imapfile, id_waq(iid,j),  'description'  , tmpstr)
                   ierr = nf90_put_att(imapfile, id_waq(iid,j),  '_FillValue'   , dmiss)
                enddo
             endif
             if (noout_statt > 0) then
                call realloc(id_wqst, (/ 3, noout_statt /), keepExisting=.false., fill = 0)
                do j=1,noout_statt
                   jj = noout_user + j
                   tmpstr = ' '
                   write (tmpstr, "('water_quality_stat_',I0)") j
                   if ( kmx > 0 ) then  !        3D
                      ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/ id_laydim(iid), id_flowelemdim (iid), id_timedim (iid)/) , id_wqst(iid,j))
                   else
                      ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/ id_flowelemdim (iid), id_timedim (iid)/) , id_wqst(iid,j))
                   endif
                   tmpstr = trim(outputs%names(jj))//' - '//trim(outputs%description(jj))//' in flow element'
                   call replace_multiple_spaces_by_single_spaces(tmpstr)
                   ierr = nf90_put_att(imapfile, id_wqst(iid,j),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                   ierr = nf90_put_att(imapfile, id_wqst(iid,j),  'long_name'    , trim(outputs%names(jj)))
                   ierr = nf90_put_att(imapfile, id_wqst(iid,j),  'units'        , trim(outputs%units(jj)))
                   ierr = nf90_put_att(imapfile, id_wqst(iid,j),  'description'  , tmpstr)
                   ierr = nf90_put_att(imapfile, id_wqst(iid,j),  '_FillValue'   , dmiss)
                enddo
             endif
             if (noout_state > 0) then
                call realloc(id_wqse, (/ 3, noout_state /), keepExisting=.false., fill = 0)
                do j=1,noout_state
                   jj = noout_user + noout_statt + j
                   tmpstr = ' '
                   write (tmpstr, "('water_quality_stat_',I0)") noout_statt + j
                   if ( kmx > 0 ) then  !        3D
                      ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/ id_laydim(iid), id_flowelemdim (iid)/) , id_wqse(iid,j))
                   else
                      ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/ id_flowelemdim (iid)/) , id_wqse(iid,j))
                   endif
                   tmpstr = trim(outputs%names(jj))//' - '//trim(outputs%description(jj))//' in flow element'
                   call replace_multiple_spaces_by_single_spaces(tmpstr)
                   ierr = nf90_put_att(imapfile, id_wqse(iid,j),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                   ierr = nf90_put_att(imapfile, id_wqse(iid,j),  'long_name'    , trim(outputs%names(jj)))
                   ierr = nf90_put_att(imapfile, id_wqse(iid,j),  'units'        , trim(outputs%units(jj)))
                   ierr = nf90_put_att(imapfile, id_wqse(iid,j),  'description'  , tmpstr)
                   ierr = nf90_put_att(imapfile, id_wqse(iid,j),  '_FillValue'   , dmiss)
                enddo
             endif
          endif

          ! water quality mass balance areas
          if (nomba > 0) then
             ierr = nf90_def_var(imapfile,  'water_quality_mba', nf90_int, (/ id_flowelemdim (iid) /) , id_mba(iid))
             ierr = nf90_put_att(imapfile, id_mba(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_mba(iid),  'long_name'    , 'Water quality mass balance areas')
             ierr = unc_add_gridmapping_att(imapfile, (/ id_mba(iid) /), jsferic)
             call realloc(flag_val, nomba, keepExisting = .false., fill = 0)
             flag_mean = ' '
             do j=nomba,1,-1
                flag_val(j) = j
                flag_mean = trim(mbaname(j))//' '//flag_mean
             enddo
             ierr = nf90_put_att(imapfile, id_mba(iid), 'flag_values', flag_val)
             ierr = nf90_put_att(imapfile, id_mba(iid), 'flag_meanings', flag_mean)
          endif

          if ( jasecflow > 0 .and. jamapspir > 0) then
             if (kmx < 2) then
                ierr = nf90_def_var(imapfile, 'spircrv' , nf90_double, (/ id_flowelemdim (iid), id_timedim (iid) /) , id_spircrv(iid))
                ierr = nf90_put_att(imapfile, id_spircrv(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_spircrv(iid),  'long_name'    , 'streamline curvature')
                ierr = nf90_put_att(imapfile, id_spircrv(iid),  'units'        , 'm-1')
             endif
             ierr = nf90_def_var(imapfile, 'spirint' , nf90_double, (/ id_flowelemdim (iid), id_timedim (iid) /) , id_spirint(iid))
             ierr = nf90_put_att(imapfile, id_spirint(iid)  ,'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_spirint(iid)  ,'long_name'    , 'Spiral flow intensity')
             ierr = nf90_put_att(imapfile, id_spirint(iid)  ,'units'        , 'm/s')
          endif


          if (jamaptur > 0 .and. kmx > 0) then
             if ( iturbulencemodel >= 3 ) then
                ierr = nf90_def_var(imapfile, 'turkin1' , nf90_double, (/ id_wdim(iid), id_flowlinkdim(iid) , id_timedim(iid) /) , id_turkin1(iid))
                ierr = nf90_put_att(imapfile, id_turkin1(iid),  'coordinates'  , 'FlowLink_xu FlowLink_yu')
                ierr = nf90_put_att(imapfile, id_turkin1(iid),  'standard_name', 'specific_turbulent_kinetic_energy_of_sea_water')
                ierr = nf90_put_att(imapfile, id_turkin1(iid),  'long_name'    , 'turbulent kinetic energy')
                ierr = nf90_put_att(imapfile, id_turkin1(iid),  'units'        , 'm2 s-2')
                ierr = nf90_put_att(imapfile, id_turkin1(iid),  '_FillValue'   , dmiss)

                ierr = nf90_def_var(imapfile, 'vicwwu' , nf90_double, (/ id_wdim(iid), id_flowlinkdim(iid) , id_timedim(iid) /) , id_vicwwu(iid))
                ierr = nf90_put_att(imapfile, id_vicwwu(iid),  'coordinates'  , 'FlowLink_xu FlowLink_yu')
                ierr = nf90_put_att(imapfile, id_vicwwu(iid),  'long_name'    , 'turbulent vertical eddy viscosity at velocity points')
                ierr = nf90_put_att(imapfile, id_vicwwu(iid),  'units'        , 'm2 s-1')
                ierr = nf90_put_att(imapfile, id_vicwwu(iid),  '_FillValue'   , dmiss)

                ierr = nf90_def_var(imapfile, 'vicwws' , nf90_double, (/ id_wdim(iid), id_flowelemdim(iid) , id_timedim(iid) /) , id_vicwws(iid))
                ierr = nf90_put_att(imapfile, id_vicwws(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_vicwws(iid),  'long_name'    , 'turbulent vertical eddy viscosity at pressure points')
                ierr = nf90_put_att(imapfile, id_vicwws(iid),  'units'        , 'm2 s-1')
                ierr = nf90_put_att(imapfile, id_vicwws(iid),  '_FillValue'   , dmiss)

                ierr = nf90_def_var(imapfile, 'tureps1' , nf90_double, (/ id_wdim(iid), id_flowlinkdim(iid) , id_timedim(iid) /) , id_tureps1(iid))
                ierr = nf90_put_att(imapfile, id_tureps1(iid),  'coordinates'  , 'FlowLink_xu FlowLink_yu')
                ierr = nf90_put_att(imapfile, id_tureps1(iid),  '_FillValue'   , dmiss)

                if ( iturbulencemodel == 3 ) then
                   ierr = nf90_put_att(imapfile, id_tureps1(iid),  'standard_name', 'specific_turbulent_kinetic_energy_dissipation_in_sea_water')
                   ierr = nf90_put_att(imapfile, id_tureps1(iid),  'long_name'    , 'turbulent energy dissipation')
                   ierr = nf90_put_att(imapfile, id_tureps1(iid),  'units'        , 'm2 s-3')
                else if ( iturbulencemodel == 4 ) then
                   ierr = nf90_put_att(imapfile, id_tureps1(iid),  'long_name'    , 'turbulent time scale')
                   ierr = nf90_put_att(imapfile, id_tureps1(iid),  'units'        , 's-1')
                endif
             endif
          endif

          if (jamapsed > 0 .and. stm_included) then
             ierr = nf90_def_dim(imapfile, 'nSedTot', stmpar%lsedtot, id_sedtotdim(iid))
             ierr = nf90_def_dim(imapfile, 'nSedSus', stmpar%lsedsus, id_sedsusdim(iid))
             ierr = nf90_def_dim(imapfile, 'nBedLayers', stmpar%morlyr%settings%nlyr, id_nlyrdim(iid))
             !
             select case(stmpar%morpar%moroutput%transptype)
                case (0)
                   transpunit = 'kg/(s m)'
                case (1)
                   transpunit = 'm3/(s m)'
                case (2)
                   transpunit = 'm3/(s m)'
             end select
             !
             ! fall velocity
             if (stmpar%lsedsus > 0) then
                if (kmx > 0) then
                   ierr = nf90_def_var(imapfile, 'ws', nf90_double, (/ id_laydim(iid), id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /), id_ws(iid))
                else ! '2D' fall velocity, ref fm_erosed(), to check...
                   ierr = nf90_def_var(imapfile, 'ws', nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /), id_ws(iid))
                endif
                ierr = nf90_put_att(imapfile, id_ws(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_ws(iid) ,  'long_name'    , 'Sediment settling velocity')
                ierr = nf90_put_att(imapfile, id_ws(iid) ,  'units'        , 'm s-1')
                !
                ! equilibrium concentration, 2D only
                if (kmx == 0) then
                   ierr = nf90_def_var(imapfile, 'rsedeq', nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /), id_rsedeq(iid))
                   ierr = nf90_put_att(imapfile, id_rsedeq(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                   ierr = nf90_put_att(imapfile, id_rsedeq(iid) ,  'long_name'    , 'Equilibrium sediment concentration')
                   ierr = nf90_put_att(imapfile, id_rsedeq(iid) ,  'units'        , 'kg m-3')
                endif
                !
                ! reference height
                if (stmpar%morpar%moroutput%aks) then
                   ierr = nf90_def_var(imapfile, 'aks', nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /), id_aks(iid))
                   ierr = nf90_put_att(imapfile, id_aks(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                   ierr = nf90_put_att(imapfile, id_aks(iid) ,  'long_name'    , 'Near-bed reference concentration height')
                   ierr = nf90_put_att(imapfile, id_aks(iid) ,  'units'        , 'm')

                   ierr = nf90_def_var(imapfile, 'rca', nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /), id_rca(iid))
                   ierr = nf90_put_att(imapfile, id_rca(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                   ierr = nf90_put_att(imapfile, id_rca(iid) ,  'long_name'    , 'Near-bed reference concentration')
                   ierr = nf90_put_att(imapfile, id_rca(iid) ,  'units'        , 'kg m-3')
                endif

                if (stmpar%morpar%moroutput%sourcesink) then
                   ierr = nf90_def_var(imapfile, 'sourse' , nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /) , id_sourse(iid))
                   ierr = nf90_put_att(imapfile, id_sourse(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                   ierr = nf90_put_att(imapfile, id_sourse(iid) ,  'long_name'    , 'Source term suspended sediment fractions')
                   ierr = nf90_put_att(imapfile, id_sourse(iid) ,  'units'        , 'kg/(m3 s)')

                   ierr = nf90_def_var(imapfile, 'sinkse' , nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /) , id_sinkse(iid))
                   ierr = nf90_put_att(imapfile, id_sinkse(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                   ierr = nf90_put_att(imapfile, id_sinkse(iid) ,  'long_name'    , 'Sink term suspended sediment fractions')
                   ierr = nf90_put_att(imapfile, id_sinkse(iid) ,  'units'        , 's-1')
                endif

                if (stmpar%morpar%moroutput%suvcor) then
                   ierr = nf90_def_var(imapfile, 'e_scrn' , nf90_double, (/ id_flowlinkdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /) , id_scrn(iid))
                   ierr = nf90_put_att(imapfile, id_scrn(iid) ,  'coordinates'  , 'FlowLink_xu FlowLink_yu')
                   ierr = nf90_put_att(imapfile, id_scrn(iid) ,  'long_name'    , 'Near-bed transport correction in face-normal direction')
                   ierr = nf90_put_att(imapfile, id_scrn(iid) ,  'units'        , transpunit)

                   !ierr = nf90_def_var(imapfile, 'e_scrt' , nf90_double, (/ id_flowlinkdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /) , id_scrt(iid))
                   !ierr = nf90_put_att(imapfile, id_scrt(iid) ,  'coordinates'  , 'FlowLink_xu FlowLink_yu')
                   !ierr = nf90_put_att(imapfile, id_scrt(iid) ,  'long_name'    , 'Near-bed transport correction face-tangential direction')
                   !ierr = nf90_put_att(imapfile, id_scrt(iid) ,  'units'        , transpunit)
                endif
                !
                ! Suspended fractions
                !
                do j=ISED1,ISEDN
                   tmpstr = const_names(j)
                   ! Forbidden chars in NetCDF names: space, /, and more.
                   call replace_char(tmpstr,32,95)
                   call replace_char(tmpstr,47,95)
                   if ( kmx > 0 ) then  !        3D
                      ierr = nf90_def_var(imapfile, trim(tmpstr), nf90_double, (/ id_laydim(iid), id_flowelemdim (iid), id_timedim (iid)/) , id_const(iid,j))
                   else
                      ierr = nf90_def_var(imapfile, trim(tmpstr), nf90_double, (/ id_flowelemdim (iid), id_timedim (iid)/) , id_const(iid,j))
                   endif
                   ierr = nf90_put_att(imapfile, id_const(iid,j),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                   ierr = nf90_put_att(imapfile, id_const(iid,j),  'standard_name', trim(tmpstr)//' concentration')
                   ierr = nf90_put_att(imapfile, id_const(iid,j),  'long_name'    , trim(tmpstr)//' concentration')
                   ierr = nf90_put_att(imapfile, id_const(iid,j),  'units'        , 'kg m-3')
                enddo
             endif

             if (stmpar%morpar%moroutput%dzduuvv) then ! bedslope
                ierr = nf90_def_var(imapfile, 'e_dzdn', nf90_double, (/ id_flowlinkdim(iid) , id_timedim(iid) /), id_dzdn(iid))
                ierr = nf90_put_att(imapfile, id_dzdn(iid) ,  'coordinates'  , 'FlowLink_xu FlowLink_yu')
                ierr = nf90_put_att(imapfile, id_dzdn(iid) ,  'long_name'    , 'Bed slope, n-component')
                ierr = nf90_put_att(imapfile, id_dzdn(iid) ,  'units'        , '-')

                ierr = nf90_def_var(imapfile, 'e_dzdt', nf90_double, (/ id_flowlinkdim(iid) , id_timedim(iid) /), id_dzdt(iid))
                ierr = nf90_put_att(imapfile, id_dzdt(iid) ,  'coordinates'  , 'FlowLink_xu FlowLink_yu')
                ierr = nf90_put_att(imapfile, id_dzdt(iid) ,  'long_name'    , 'Bed slope, t-component')
                ierr = nf90_put_att(imapfile, id_dzdt(iid) ,  'units'        , '-')
             endif

             if (stmpar%morpar%moroutput%umod) then
                ierr = nf90_def_var(imapfile, 'umod', nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /), id_umod(iid))
                ierr = nf90_put_att(imapfile, id_umod(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_umod(iid) ,  'long_name'    , 'Characteristic velocity magnitude in cell centre')
                ierr = nf90_put_att(imapfile, id_umod(iid) ,  'units'        , 'm s-1')
             endif

             if (stmpar%morpar%moroutput%zumod) then
                ierr = nf90_def_var(imapfile, 'zumod', nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /), id_zumod(iid))
                ierr = nf90_put_att(imapfile, id_zumod(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_zumod(iid) ,  'long_name'    , 'Height above bed for characteristic velocity in cell centre')
                ierr = nf90_put_att(imapfile, id_zumod(iid) ,  'units'        , 'm')
             endif

             if (stmpar%morpar%moroutput%ustar) then
                ierr = nf90_def_var(imapfile, 'ustar', nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /), id_ustar(iid))
                ierr = nf90_put_att(imapfile, id_ustar(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_ustar(iid) ,  'long_name'    , 'Bed shear velocity u* in cell centre')
                ierr = nf90_put_att(imapfile, id_ustar(iid) ,  'units'        , 'm s-1')
             endif

             if (stmpar%morpar%moroutput%sbcuv) then
                ierr = nf90_def_var(imapfile, 'sbcx' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sbcx(iid))
                ierr = nf90_put_att(imapfile, id_sbcx(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_sbcx(iid) ,  'long_name'    , 'bed load transport due to currents, x-component')
                ierr = nf90_put_att(imapfile, id_sbcx(iid) ,  'units'        , transpunit)

                ierr = nf90_def_var(imapfile, 'sbcy' , nf90_double, (/ id_flowelemdim (iid), id_sedtotdim(iid), id_timedim (iid)/) , id_sbcy(iid))
                ierr = nf90_put_att(imapfile, id_sbcy (iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_sbcy (iid),  'long_name'    , 'bed load transport due to currents, y-component')
                ierr = nf90_put_att(imapfile, id_sbcy (iid),  'units'        , transpunit)

                ierr = nf90_def_var(imapfile, 'sbcx_reconstructed' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sbcx_reconstructed(iid))
                ierr = nf90_put_att(imapfile, id_sbcx(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_sbcx(iid) ,  'long_name'    , 'bed load transport due to currents (reconstructed), x-component')
                ierr = nf90_put_att(imapfile, id_sbcx(iid) ,  'units'        , transpunit)

                ierr = nf90_def_var(imapfile, 'sbcy_reconstructed' , nf90_double, (/ id_flowelemdim (iid), id_sedtotdim(iid), id_timedim (iid)/) , id_sbcy_reconstructed(iid))
                ierr = nf90_put_att(imapfile, id_sbcy (iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_sbcy (iid),  'long_name'    , 'bed load transport due to currents (reconstructed), y-component')
                ierr = nf90_put_att(imapfile, id_sbcy (iid),  'units'        , transpunit)
             endif

             if (stmpar%morpar%moroutput%sbwuv) then
                ierr = nf90_def_var(imapfile, 'sbwx' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sbwx(iid))
                ierr = nf90_put_att(imapfile, id_sbwx(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_sbwx(iid) ,  'long_name'    , 'bed load transport due to waves, x-component')
                ierr = nf90_put_att(imapfile, id_sbwx(iid) ,  'units'        , transpunit)

                ierr = nf90_def_var(imapfile, 'sbwy' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sbwy(iid))
                ierr = nf90_put_att(imapfile, id_sbwy(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_sbwy(iid) ,  'long_name'    , 'bed load transport due to waves, y-component')
                ierr = nf90_put_att(imapfile, id_sbwy(iid) ,  'units'        , transpunit)

                ierr = nf90_def_var(imapfile, 'sbwx_reconstructed' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sbwx_reconstructed(iid))
                ierr = nf90_put_att(imapfile, id_sbwx(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_sbwx(iid) ,  'long_name'    , 'bed load transport due to waves (reconstructed), x-component')
                ierr = nf90_put_att(imapfile, id_sbwx(iid) ,  'units'        , transpunit)

                ierr = nf90_def_var(imapfile, 'sbwy_reconstructed' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sbwy_reconstructed(iid))
                ierr = nf90_put_att(imapfile, id_sbwy(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_sbwy(iid) ,  'long_name'    , 'bed load transport due to waves (reconstructed), y-component')
                ierr = nf90_put_att(imapfile, id_sbwy(iid) ,  'units'        , transpunit)
             endif

             if (stmpar%morpar%moroutput%sswuv) then
                ierr = nf90_def_var(imapfile, 'sswx' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sswx(iid))
                ierr = nf90_put_att(imapfile, id_sswx(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_sswx(iid) ,  'long_name'    , 'suspended load transport due to waves, x-component')
                ierr = nf90_put_att(imapfile, id_sswx(iid) ,  'units'        , transpunit)

                ierr = nf90_def_var(imapfile, 'sswy' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sswy(iid))
                ierr = nf90_put_att(imapfile, id_sswy(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_sswy(iid) ,  'long_name'    , 'suspended load transport due to waves, y-component')
                ierr = nf90_put_att(imapfile, id_sswy(iid) ,  'units'        , transpunit)

                ierr = nf90_def_var(imapfile, 'sswx_reconstructed' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sswx_reconstructed(iid))
                ierr = nf90_put_att(imapfile, id_sswx(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_sswx(iid) ,  'long_name'    , 'suspended load transport due to waves (reconstructed), x-component')
                ierr = nf90_put_att(imapfile, id_sswx(iid) ,  'units'        , transpunit)

                ierr = nf90_def_var(imapfile, 'sswy_reconstructed' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sswy_reconstructed(iid))
                ierr = nf90_put_att(imapfile, id_sswy(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_sswy(iid) ,  'long_name'    , 'suspended load transport due to waves (reconstructed), y-component')
                ierr = nf90_put_att(imapfile, id_sswy(iid) ,  'units'        , transpunit)
             endif

             if (stmpar%morpar%moroutput%sscuv) then
                ierr = nf90_def_var(imapfile, 'sscx' , nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /) , id_sscx(iid))
                ierr = nf90_put_att(imapfile, id_sscx(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_sscx(iid) ,  'long_name'    , 'suspended load transport due to currents, x-component')
                ierr = nf90_put_att(imapfile, id_sscx(iid) ,  'units'        , transpunit)

                ierr = nf90_def_var(imapfile, 'sscy' , nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /) , id_sscy(iid))
                ierr = nf90_put_att(imapfile, id_sscy(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_sscy(iid) ,  'long_name'    , 'suspended load transport due to currents, y-component')
                ierr = nf90_put_att(imapfile, id_sscy(iid) ,  'units'        , transpunit)

                ierr = nf90_def_var(imapfile, 'sscx_reconstructed' , nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /) , id_sscx_reconstructed(iid))
                ierr = nf90_put_att(imapfile, id_sscx(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_sscx(iid) ,  'long_name'    , 'suspended load transport due to currents (reconstructed), x-component')
                ierr = nf90_put_att(imapfile, id_sscx(iid) ,  'units'        , transpunit)

                ierr = nf90_def_var(imapfile, 'sscy_reconstructed' , nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /) , id_sscy_reconstructed(iid))
                ierr = nf90_put_att(imapfile, id_sscy(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_sscy(iid) ,  'long_name'    , 'suspended load transport due to currents (reconstructed), y-component')
                ierr = nf90_put_att(imapfile, id_sscy(iid) ,  'units'        , transpunit)
             endif

             ierr = nf90_def_var(imapfile, 'sxtot' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sxtot(iid))
             ierr = nf90_put_att(imapfile, id_sxtot(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_sxtot(iid) ,  'long_name'    , 'total sediment transport in flow cell center, x-component')
             ierr = nf90_put_att(imapfile, id_sxtot(iid) ,  'units'        , transpunit)

             ierr = nf90_def_var(imapfile, 'sytot' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sytot(iid))
             ierr = nf90_put_att(imapfile, id_sytot(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_sytot(iid) ,  'long_name'    , 'total sediment transport in flow cell center, y-component')
             ierr = nf90_put_att(imapfile, id_sytot(iid) ,  'units'        , transpunit)

             ierr = nf90_def_var(imapfile, 'mor_bl' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_morbl(iid))
             ierr = nf90_put_att(imapfile, id_morbl(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_morbl(iid) ,  'long_name'    , 'Time-varying bottom level in flow cell center')
             ierr = nf90_put_att(imapfile, id_morbl(iid) ,  'units'        , 'm')


             select case (stmpar%morlyr%settings%iunderlyr)
             case (1)
                ierr = nf90_def_var(imapfile, 'bodsed' , nf90_double, (/ id_sedtotdim(iid) , id_flowelemdim(iid) , id_timedim(iid) /) , id_bodsed(iid))
                ierr = nf90_put_att(imapfile, id_bodsed(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_bodsed(iid) ,  'long_name'    , 'available sediment in the bed in flow cell center')
                ierr = nf90_put_att(imapfile, id_bodsed(iid) ,  'units'        , 'kg m-2')

                ierr = nf90_def_var(imapfile, 'dpsed' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_dpsed(iid))
                ierr = nf90_put_att(imapfile, id_dpsed(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_dpsed(iid) ,  'long_name'    , 'sediment thickness in the bed in flow cell center')
                ierr = nf90_put_att(imapfile, id_dpsed(iid) ,  'units'        , 'm')
             case (2)
                ierr = nf90_def_var(imapfile, 'msed' , nf90_double, (/ id_sedtotdim(iid) , id_nlyrdim(iid) , id_flowelemdim(iid) , id_timedim(iid) /) , id_msed(iid))
                ierr = nf90_put_att(imapfile, id_msed(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_msed(iid) ,  'long_name'    , 'available sediment in a layer of the bed in flow cell center')
                ierr = nf90_put_att(imapfile, id_msed(iid) ,  'units'        , 'kg m-2')

                ierr = nf90_def_var(imapfile, 'lyrfrac' , nf90_double, (/ id_sedtotdim(iid) , id_nlyrdim(iid) , id_flowelemdim(iid) , id_timedim(iid) /) , id_lyrfrac(iid))
                ierr = nf90_put_att(imapfile, id_lyrfrac(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_lyrfrac(iid) ,  'long_name'    , 'volume fraction in a layer of the bed in flow cell center')
                ierr = nf90_put_att(imapfile, id_lyrfrac(iid) ,  'units'        , '-')

                ierr = nf90_def_var(imapfile, 'thlyr' , nf90_double, (/ id_nlyrdim(iid) , id_flowelemdim(iid) , id_timedim(iid) /) , id_thlyr(iid))
                ierr = nf90_put_att(imapfile, id_thlyr(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_thlyr(iid) ,  'long_name'    , 'thickness of a layer of the bed in flow cell center')
                ierr = nf90_put_att(imapfile, id_thlyr(iid) ,  'units'        , 'm')

                if (stmpar%morlyr%settings%iporosity>0) then
                   ierr = nf90_def_var(imapfile, 'poros' , nf90_double, (/ id_nlyrdim(iid) , id_flowelemdim(iid) , id_timedim(iid) /) , id_poros(iid))
                   ierr = nf90_put_att(imapfile, id_poros(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                   ierr = nf90_put_att(imapfile, id_poros(iid) ,  'long_name'    , 'porosity of a layer of the bed in flow cell center')
                   ierr = nf90_put_att(imapfile, id_poros(iid) ,  'units'        , '-')
                endif
             end select

             if (stmpar%morpar%moroutput%taurat) then
                ierr = nf90_def_var(imapfile, 'taurat' , nf90_double, (/id_flowelemdim(iid) , id_sedtotdim(iid) ,id_timedim(iid) /) , id_taurat(iid))
                ierr = nf90_put_att(imapfile, id_taurat(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_taurat(iid) ,  'long_name'    , 'Excess bed shear ratio')
                ierr = nf90_put_att(imapfile, id_taurat(iid) ,  'units'        , '-')
             endif
             if (stmpar%morpar%moroutput%dm) then
                ierr = nf90_def_var(imapfile, 'dm' , nf90_double, (/id_flowelemdim(iid) , id_timedim(iid) /) , id_dm(iid))
                ierr = nf90_put_att(imapfile, id_dm(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_dm(iid) ,  'long_name'    , 'Arithmetic mean sediment diameter')
                ierr = nf90_put_att(imapfile, id_dm(iid) ,  'units'        , 'm')
             endif
             if (stmpar%morpar%moroutput%dg) then
                ierr = nf90_def_var(imapfile, 'dg' , nf90_double, (/id_flowelemdim(iid) , id_timedim(iid) /) , id_dg(iid))
                ierr = nf90_put_att(imapfile, id_dg(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_dg(iid) ,  'long_name'    , 'Geometric mean sediment diameter')
                ierr = nf90_put_att(imapfile, id_dg(iid) ,  'units'        , 'm')
             endif
             if (stmpar%morpar%moroutput%dgsd) then
                ierr = nf90_def_var(imapfile, 'dgsd' , nf90_double, (/id_flowelemdim(iid) , id_timedim(iid) /) , id_dgsd(iid))
                ierr = nf90_put_att(imapfile, id_dgsd(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_dgsd(iid) ,  'long_name'    , 'Geometric standard deviation of particle size mix')
                ierr = nf90_put_att(imapfile, id_dgsd(iid) ,  'units'        , 'm')
             endif
             if (stmpar%morpar%moroutput%percentiles) then
                do l = 1, stmpar%morpar%nxx
                   write(dxname,'(A,I2.2)') 'DXX',l
                   write(dxdescr,'(A,F4.1,A)') 'Sediment diameter percentile '    , stmpar%morpar%xx(l)*100d0,' %'
                   ierr = nf90_def_var(imapfile, dxname , nf90_double, (/id_flowelemdim(iid) , id_timedim(iid) /) , id_dxx(l,iid))
                   ierr = nf90_put_att(imapfile, id_dxx(l,iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                   ierr = nf90_put_att(imapfile, id_dxx(l,iid) ,  'long_name'    , dxdescr)
                   ierr = nf90_put_att(imapfile, id_dxx(l,iid) ,  'units'        , 'm')
                enddo
             endif
             if (stmpar%morpar%moroutput%frac) then
                ierr = nf90_def_var(imapfile, 'frac' , nf90_double, (/id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_frac(iid))
                ierr = nf90_put_att(imapfile, id_frac(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_frac(iid) ,  'long_name'    , 'Availability fraction in top layer')
                ierr = nf90_put_att(imapfile, id_frac(iid) ,  'units'        , '-')
             endif
             if (stmpar%morpar%moroutput%mudfrac) then
                ierr = nf90_def_var(imapfile, 'mudfrac' , nf90_double, (/id_flowelemdim(iid) , id_timedim(iid) /) , id_mudfrac(iid))
                ierr = nf90_put_att(imapfile, id_mudfrac(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_mudfrac(iid) ,  'long_name'    , 'Mud fraction in top layer')
                ierr = nf90_put_att(imapfile, id_mudfrac(iid) ,  'units'        , '-')
             endif
             if (stmpar%morpar%moroutput%sandfrac) then
                ierr = nf90_def_var(imapfile, 'sandfrac' , nf90_double, (/id_flowelemdim(iid) , id_timedim(iid) /) , id_sandfrac(iid))
                ierr = nf90_put_att(imapfile, id_sandfrac(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_sandfrac(iid) ,  'long_name'    , 'Sand fraction in top layer')
                ierr = nf90_put_att(imapfile, id_sandfrac(iid) ,  'units'        , '-')
             endif
             if (stmpar%morpar%moroutput%fixfac) then
                ierr = nf90_def_var(imapfile, 'fixfac' , nf90_double, (/id_flowelemdim(iid) , id_sedtotdim(iid), id_timedim(iid) /) , id_fixfac(iid))
                ierr = nf90_put_att(imapfile, id_fixfac(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_fixfac(iid) ,  'long_name'    , 'Reduction factor due to limited sediment thickness')
                ierr = nf90_put_att(imapfile, id_fixfac(iid) ,  'units'        , '-')
             endif
             if (stmpar%morpar%moroutput%hidexp) then
                ierr = nf90_def_var(imapfile, 'hidexp' , nf90_double, (/id_flowelemdim(iid) , id_sedtotdim(iid), id_timedim(iid) /) , id_hidexp(iid))
                ierr = nf90_put_att(imapfile, id_hidexp(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_hidexp(iid) ,  'long_name'    , 'Hiding and exposure factor')
                ierr = nf90_put_att(imapfile, id_hidexp(iid) ,  'units'        , '-')
             endif
             ! Fluff layers
             if (stmpar%morpar%flufflyr%iflufflyr>0 .and. stmpar%lsedsus>0) then
                ierr = nf90_def_var(imapfile, 'mfluff' , nf90_double, (/id_flowelemdim(iid) , id_sedsusdim(iid), id_timedim(iid) /) , id_mfluff(iid))
                ierr = nf90_put_att(imapfile, id_mfluff(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_mfluff(iid) ,  'long_name'    , 'Sediment mass in fluff layer')
                ierr = nf90_put_att(imapfile, id_mfluff(iid) ,  'units'        , 'kg m-2 ')
             endif
          endif

          if (bfmpar%lfbedfrmout) then
             if (bfmpar%lfbedfrm) then
                ! DUNEHEIGHT
                ierr = nf90_def_var(imapfile, 'duneheight' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_duneheight(iid))
                ierr = nf90_put_att(imapfile, id_duneheight(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_duneheight(iid) ,  'long_name'    , 'Time-varying dune height in flow cell centers')
                ierr = nf90_put_att(imapfile, id_duneheight(iid) ,  'units'        , 'm')
                ! DUNELENGTH
                ierr = nf90_def_var(imapfile, 'dunelength' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_dunelength(iid))
                ierr = nf90_put_att(imapfile, id_dunelength(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_dunelength(iid) ,  'long_name'    , 'Time-varying dune length in flow cell centers')
                ierr = nf90_put_att(imapfile, id_dunelength(iid) ,  'units'        , 'm')
             endif
             if (bfmpar%lfbedfrmrou) then
                call realloc(rks,ndx, keepExisting=.false.,fill=0d0)
                ! KSR
                ierr = nf90_def_var(imapfile, 'ksr' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_ksr(iid))
                ierr = nf90_put_att(imapfile, id_ksr(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_ksr(iid) ,  'long_name'    , 'Ripple roughness height in flow cell center')
                ierr = nf90_put_att(imapfile, id_ksr(iid) ,  'units'        , 'm')
                ! KSMR
                ierr = nf90_def_var(imapfile, 'ksmr' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_ksmr(iid))
                ierr = nf90_put_att(imapfile, id_ksmr(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_ksmr(iid) ,  'long_name'    , 'Mega-ripple roughness height in flow cell center')
                ierr = nf90_put_att(imapfile, id_ksmr(iid) ,  'units'        , 'm')
                ! KSD
                ierr = nf90_def_var(imapfile, 'ksd' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_ksd(iid))
                ierr = nf90_put_att(imapfile, id_ksd(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_ksd(iid) ,  'long_name'    , 'Dune roughness height in flow cell center')
                ierr = nf90_put_att(imapfile, id_ksd(iid) ,  'units'        , 'm')
                ! KS
                ierr = nf90_def_var(imapfile, 'ks' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_ks(iid))
                ierr = nf90_put_att(imapfile, id_ks(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_ks(iid) ,  'long_name'    , 'Bedform roughness height in flow cell center')
                ierr = nf90_put_att(imapfile, id_ks(iid) ,  'units'        , 'm')
             endif
          endif
          if (jased > 0 .and. .not.stm_included) then
             ierr = nf90_def_dim(imapfile, 'nFrac', mxgr, id_maxfracdim(iid))

             if (jaceneqtr == 1) then
                 ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_erolaydim(iid)) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
                 if (ierr /= nf90_noerr) then
                    ierr = nf90_inq_dimid(imapfile, 'nFlowElemWithBnd', id_erolaydim(iid))
                 endif
             else
                 ierr = nf90_inq_dimid(imapfile, 'nNetNode' , id_erolaydim(iid)) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
             endif

             ierr = nf90_def_var(imapfile, 'sed'  , nf90_double, (/ id_maxfracdim  (iid), id_flowelemdim(iid), id_timedim (iid)/) , id_sed(iid))
             ierr = nf90_put_att(imapfile, id_sed(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_sed(iid),  'long_name'    , 'sediment concentration')
             ierr = nf90_put_att(imapfile, id_sed(iid),  'units'        , 'kg m-3')
             ierr = nf90_def_var(imapfile, 'ero' , nf90_double, (/ id_maxfracdim  (iid), id_erolaydim(iid), id_timedim (iid)/) , id_ero(iid))
             if (jaceneqtr == 1) then
                 ierr = nf90_put_att(imapfile, id_ero(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_ero(iid),  'long_name', 'erodable layer thickness per size fraction in flow element center')
             else
                 ierr = nf90_put_att(imapfile, id_ero(iid),  'coordinates'  , 'NetNode_x NetNode_y')
                 ierr = nf90_put_att(imapfile, id_ero(iid),  'long_name', 'erodable layer thickness per size fraction at flow element corners')
             endif
             ierr = nf90_put_att(imapfile, id_ero(iid),  'standard_name'    , 'Erodable layer thickness') ! Not CF
             ierr = nf90_put_att(imapfile, id_ero(iid),  'units'        , 'm')

             if (jaceneqtr .ne. 1) then
                idims(1) = id_erolaydim(iid)
                call definencvar(imapfile,id_zk(iid)   ,nf90_double,idims,2, 'netnode_bedlevel_zk'  , 'Flow element corner bedlevel (zk)', 'm', 'NetNode_x NetNode_y')
             endif
             idims(1) = id_flowelemdim(iid)
             call definencvar(imapfile,id_bl(iid)   ,nf90_double,idims,2, 'flowelem_bedlevel_bl'  , 'Flow element center bedlevel (bl)', 'm', 'FlowElem_xcc FlowElem_ycc')

          endif

          ! JRE waves
          if (jawave .eq. 4) then
            ierr = nf90_def_var(imapfile, 'E',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_E(iid))
            ierr = nf90_put_att(imapfile, id_E(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(imapfile, id_E(iid),   'standard_name', 'sea_surface_bulk_wave_energy')                          ! not CF
            ierr = nf90_put_att(imapfile, id_E(iid),   'long_name'    , 'wave energy per square meter')
            ierr = nf90_put_att(imapfile, id_E(iid),   'units'        , 'J m-2')

            ierr = nf90_def_var(imapfile, 'R',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_R(iid))
            ierr = nf90_put_att(imapfile, id_R(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(imapfile, id_R(iid),   'standard_name', 'sea_surface_bulk_roller_energy')                          ! not CF
            ierr = nf90_put_att(imapfile, id_R(iid),   'long_name'    , 'roller energy per square meter')
            ierr = nf90_put_att(imapfile, id_R(iid),   'units'        , 'J m-2')

            ierr = nf90_def_var(imapfile, 'DR',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_DR(iid))
            ierr = nf90_put_att(imapfile, id_DR(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(imapfile, id_DR(iid),   'standard_name', 'sea_surface_bulk_roller_dissipation')                          ! not CF
            ierr = nf90_put_att(imapfile, id_DR(iid),   'long_name'    , 'roller energy dissipation per square meter')
            ierr = nf90_put_att(imapfile, id_DR(iid),   'units'        , 'W m-2')

            ierr = nf90_def_var(imapfile, 'D',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_D(iid))
            ierr = nf90_put_att(imapfile, id_D(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(imapfile, id_D(iid),   'standard_name', 'sea_surface_wave_breaking_dissipation')                          ! not CF
            ierr = nf90_put_att(imapfile, id_D(iid),   'long_name'    , 'wave breaking energy dissipation per square meter')
            ierr = nf90_put_att(imapfile, id_D(iid),   'units'        , 'W m-2')
            ! JRE TO DO: change definition in unc file to correct one
            ierr = nf90_def_var(imapfile, 'H',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_H(iid))
            ierr = nf90_put_att(imapfile, id_H(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(imapfile, id_H(iid),   'standard_name', 'sea_surface_wave_rms_height')
            ierr = nf90_put_att(imapfile, id_H(iid),   'long_name'    , 'Root mean square wave height based on wave energy')
            ierr = nf90_put_att(imapfile, id_H(iid),   'units'        , 'm')

            ierr = nf90_def_var(imapfile, 'urms_cc',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_urmscc(iid))
            ierr = nf90_put_att(imapfile, id_urmscc(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(imapfile, id_urmscc(iid),   'standard_name', 'sea_surface_wave_orbital_velocity')
            ierr = nf90_put_att(imapfile, id_urmscc(iid),   'long_name'    , 'Root mean square orbital velocity on flow centers')
            ierr = nf90_put_att(imapfile, id_urmscc(iid),   'units'        , 'm/s')

            ierr = nf90_def_var(imapfile, 'Fx_cc',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_Fxcc(iid))
            ierr = nf90_put_att(imapfile, id_Fxcc(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(imapfile, id_Fxcc(iid),   'standard_name', 'sea_surface_wave_force_east')
            ierr = nf90_put_att(imapfile, id_Fxcc(iid),   'long_name'    , 'Wave induced flow forcing in cell centre, east component')
            ierr = nf90_put_att(imapfile, id_Fxcc(iid),   'units'        , 'kg m s-2')

            ierr = nf90_def_var(imapfile, 'Fy_cc',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_Fycc(iid))
            ierr = nf90_put_att(imapfile, id_Fycc(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(imapfile, id_Fycc(iid),   'standard_name', 'sea_surface_wave_force_north')
            ierr = nf90_put_att(imapfile, id_Fycc(iid),   'long_name'    , 'Wave induced flow forcing in cell centre, north component')
            ierr = nf90_put_att(imapfile, id_Fycc(iid),   'units'        , 'kg m s-2')

            ierr = nf90_def_var(imapfile, 'thetamean',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid)/) , id_thetamean(iid))
            ierr = nf90_put_att(imapfile, id_thetamean(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(imapfile, id_thetamean(iid),   'standard_name', 'sea_surface_wave_from_direction')                          ! not CF
            ierr = nf90_put_att(imapfile, id_thetamean(iid),   'long_name'    , 'mean wave angle')
            ierr = nf90_put_att(imapfile, id_thetamean(iid),   'units'        , 'deg')

            ierr = nf90_def_var(imapfile, 'cwav',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_cwav(iid))
            ierr = nf90_put_att(imapfile, id_cwav(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(imapfile, id_cwav(iid),   'standard_name', 'sea_surface_wave_phase_celerity')                          ! not CF
            ierr = nf90_put_att(imapfile, id_cwav(iid),   'long_name'    , 'phase celerity')
            ierr = nf90_put_att(imapfile, id_cwav(iid),   'units'        , 'm s-1')

            ierr = nf90_def_var(imapfile, 'cgwav',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_cgwav(iid))
            ierr = nf90_put_att(imapfile, id_cgwav(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(imapfile, id_cgwav(iid),   'standard_name', 'sea_surface_wave_group_celerity')                          ! not CF
            ierr = nf90_put_att(imapfile, id_cgwav(iid),   'long_name'    , 'group celerity')
            ierr = nf90_put_att(imapfile, id_cgwav(iid),   'units'        , 'm s-1')

            ierr = nf90_def_var(imapfile, 'sigmwav',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_sigmwav(iid))
            ierr = nf90_put_att(imapfile, id_sigmwav(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(imapfile, id_sigmwav(iid),   'standard_name', 'sea_surface_wave_mean_frequency')                          ! not CF
            ierr = nf90_put_att(imapfile, id_sigmwav(iid),   'long_name'    , 'mean wave frequency')
            ierr = nf90_put_att(imapfile, id_sigmwav(iid),   'units'        , 'rad s-1')

            !if ( (windmodel.eq.1) .and. (jawsource.eq.1) ) then
            !
            !   ierr = nf90_def_var(imapfile, 'SwE',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_SwE(iid))
            !   ierr = nf90_put_att(imapfile, id_SwE(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
            !   ierr = nf90_put_att(imapfile, id_SwE(iid),   'standard_name', 'source_term_wind_on_E')                          ! not CF
            !   ierr = nf90_put_att(imapfile, id_SwE(iid),   'long_name'    , 'source term wind on wave energy')
            !   ierr = nf90_put_att(imapfile, id_SwE(iid),   'units'        , 'J m-2 s-1')
            !
            !   ierr = nf90_def_var(imapfile, 'SwT',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_SwT(iid))
            !   ierr = nf90_put_att(imapfile, id_SwT(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
            !   ierr = nf90_put_att(imapfile, id_SwT(iid),   'standard_name', 'source_term_wind_on_T')                          ! not CF
            !   ierr = nf90_put_att(imapfile, id_SwT(iid),   'long_name'    , 'source term wind on wave period')
            !   ierr = nf90_put_att(imapfile, id_SwT(iid),   'units'        , 's s-1')
            !
            !endif
          endif

          if ( NUMCONST.eq.0 ) then
             ierr = unc_add_gridmapping_att(imapfile, (/ id_s1(iid), id_taus(iid), id_ucx(iid), id_ucy(iid), id_unorm(iid), id_sa1(iid), id_sed(iid) /), jsferic)   ! add id_ucz(iid)?
          else
             if (allocated(idum)) deallocate(idum)
             allocate(idum(7+NUMCONST))
             idum(1:7) = (/ id_s1(iid), id_taus(iid), id_ucx(iid), id_ucy(iid), id_unorm(iid), id_sa1(iid), id_sed(iid) /)
             do j=1,NUMCONST
                idum(7+j) = id_const(iid,j)
             enddo
             ierr = unc_add_gridmapping_att(imapfile, idum, jsferic)
          endif
          if (kmx > 0) then
             if ( density_is_pressure_dependent() ) then
                 ierr = unc_add_gridmapping_att(imapfile, (/ id_ucz(iid), id_ucxa(iid), id_ucya(iid), id_ww1(iid), id_rho(iid) /), jsferic)
             else
                 ierr = unc_add_gridmapping_att(imapfile, (/ id_ucz(iid), id_ucxa(iid), id_ucya(iid), id_ww1(iid), id_rhop(iid) /), jsferic)
             endif
          endif

          if (jamaptrachy > 0 .and. jatrt == 1) then
              ! Roughness data on net-links
              ierr = nf90_def_var(imapfile, 'cftrt' , nf90_double, (/ id_netlinkdim(iid), id_timedim(iid) /) , id_cftrt(iid))
              if (ifrctypuni == 0) then
                  ierr = nf90_put_att(imapfile, id_cftrt(iid),'long_name'    , 'Chezy roughness from trachytopes')
                  ierr = nf90_put_att(imapfile, id_cftrt(iid),'units'        , 'm0.5s-1')                ! WO: does not follow standard ? (which accepts only integral powers?)
              elseif (ifrctypuni == 1) then
                  ierr = nf90_put_att(imapfile, id_cftrt(iid),'long_name'    , 'Manning roughness from trachytopes')
                  ierr = nf90_put_att(imapfile, id_cftrt(iid),'units'        , 'sm-0.333')               ! WO: does not follow standard ? (which accepts only integral powers?)
              elseif ((ifrctypuni == 2) .or. (ifrctypuni == 3)) then
                  ierr = nf90_put_att(imapfile, id_cftrt(iid),'long_name'    , 'White-Colebrook roughness from trachytopes')
                  ierr = nf90_put_att(imapfile, id_cftrt(iid),'units'        , 'm')
              else
                  ierr = nf90_put_att(imapfile, id_cftrt(iid),'long_name'    , 'roughness from trachytopes')
                  ierr = nf90_put_att(imapfile, id_cftrt(iid),'units'        , ' ')
              endif
          endif

          if (jamapcali > 0 .and. jacali == 1) then
              ! Calibration factor for roughness data on net-links
              ierr = nf90_def_var(imapfile, 'cfcl' , nf90_double, (/ id_netlinkdim(iid), id_timedim(iid) /) , id_cfcl(iid))
              ierr = nf90_put_att(imapfile, id_cfcl(iid),'long_name'    , 'Calibration factor for roughness')
              ierr = nf90_put_att(imapfile, id_cfcl(iid),'units'        , ' ')
          endif

          if (jamap_chezy_elements > 0) then
              ! Chezy data on flow-nodes
              ierr = nf90_def_var(imapfile, 'czs' , nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_czs(iid))
              ierr = nf90_put_att(imapfile, id_czs(iid),'long_name'    , 'Chezy roughness')
              ierr = nf90_put_att(imapfile, id_czs(iid),'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
              ierr = nf90_put_att(imapfile, id_czs(iid),'units'        , 'm0.5s-1')                ! WO: does not follow standard ? (which accepts only integral powers?)
          endif
          if (jamap_chezy_links > 0) then
              ! Chezy data on flow-links
              ierr = nf90_def_var(imapfile, 'czu' , nf90_double, (/ id_flowlinkdim(iid), id_timedim(iid) /) , id_czu(iid))
              ierr = nf90_put_att(imapfile, id_czu(iid),'long_name'    , 'Chezy roughness on flow links')
              ierr = nf90_put_att(imapfile, id_czu(iid),'coordinates'  , 'FlowLink_xu FlowLink_yu')
              ierr = nf90_put_att(imapfile, id_czu(iid),'units'        , 'm0.5s-1')
          endif

          ! 1D2D boundaries
          if (nbnd1d2d > 0) then
              ierr = nf90_def_var(imapfile, '1d2d_flowlinknrs' , nf90_int, (/ id_1d2ddim(iid) /) , id_1d2d_edges(iid))
              ierr = nf90_put_att(imapfile, id_czs(iid),'long_name'    , 'flow link numbers of the open 1D2D boundary links')

              ierr = nf90_def_var(imapfile, '1d2d_zeta' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_zeta1d(iid))
              ierr = nf90_put_att(imapfile, id_1d2d_zeta1d(iid),'standard_name', 'sea_surface_height_above_geoid')
              ierr = nf90_put_att(imapfile, id_1d2d_zeta1d(iid),'long_name'    , '1D water level next to each 1d2d boundary link')
              ierr = nf90_put_att(imapfile, id_1d2d_zeta1d(iid),'units'        , 'm')

              ierr = nf90_def_var(imapfile, '1d2d_crest_level' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_crest_level(iid))
              ierr = nf90_put_att(imapfile, id_1d2d_crest_level(iid),'standard_name', 'sea_surface_height_above_geoid')
              ierr = nf90_put_att(imapfile, id_1d2d_crest_level(iid),'long_name'    , 'crest level of 1d2d boundary link')
              ierr = nf90_put_att(imapfile, id_1d2d_crest_level(iid),'units'        , 'm')

              ierr = nf90_def_var(imapfile, '1d2d_b_2di' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_b_2di(iid))
              ierr = nf90_put_att(imapfile, id_1d2d_b_2di(iid),'standard_name', 'b_2di')
              ierr = nf90_put_att(imapfile, id_1d2d_b_2di(iid),'long_name'    , 'coefficient for 1d2d interface b_2di')
              ierr = nf90_put_att(imapfile, id_1d2d_b_2di(iid),'units'        , '-')

              ierr = nf90_def_var(imapfile, '1d2d_b_2dv' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_b_2dv(iid))
              ierr = nf90_put_att(imapfile, id_1d2d_b_2dv(iid),'standard_name', 'b_2dv')
              ierr = nf90_put_att(imapfile, id_1d2d_b_2dv(iid),'long_name'    , 'coefficient for 1d2d interface b_2di')
              ierr = nf90_put_att(imapfile, id_1d2d_b_2dv(iid),'units'        , '-')

              ierr = nf90_def_var(imapfile, '1d2d_d_2dv' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_d_2dv(iid))
              ierr = nf90_put_att(imapfile, id_1d2d_d_2dv(iid),'standard_name', 'd_2dv')
              ierr = nf90_put_att(imapfile, id_1d2d_d_2dv(iid),'long_name'    , 'coefficient for 1d2d interface d_2dv')
              ierr = nf90_put_att(imapfile, id_1d2d_d_2dv(iid),'units'        , '-')

              ierr = nf90_def_var(imapfile, '1d2d_qzeta' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_q_zeta(iid))
              ierr = nf90_put_att(imapfile, id_1d2d_q_zeta(iid),'standard_name', 'q_zeta_1d2d')
              ierr = nf90_put_att(imapfile, id_1d2d_q_zeta(iid),'long_name'    , 'q_zeta_1d2d')
              ierr = nf90_put_att(imapfile, id_1d2d_q_zeta(iid),'units'        , 'm2 s-1')

              ierr = nf90_def_var(imapfile, '1d2d_q_lat' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_q_lat(iid))
              ierr = nf90_put_att(imapfile, id_1d2d_q_lat(iid),'standard_name', 'q_lat')
              ierr = nf90_put_att(imapfile, id_1d2d_q_lat(iid),'long_name'    , 'q_lat')
              ierr = nf90_put_att(imapfile, id_1d2d_q_lat(iid),'units'        , 'm3 s-1')

              ierr = nf90_def_var(imapfile, '1d2d_cfl' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_cfl(iid))
              ierr = nf90_put_att(imapfile, id_1d2d_cfl(iid),'standard_name', 'cfl')
              ierr = nf90_put_att(imapfile, id_1d2d_cfl(iid),'long_name'    , 'wave flow courant')
              ierr = nf90_put_att(imapfile, id_1d2d_cfl(iid),'units'        , '-')

              ierr = nf90_def_var(imapfile, '1d2d_sb' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_sb(iid))
              ierr = nf90_put_att(imapfile, id_1d2d_sb(iid),'standard_name', '1d2d_sb')
              ierr = nf90_put_att(imapfile, id_1d2d_sb(iid),'long_name'    , 'water levels in boundary points')
              ierr = nf90_put_att(imapfile, id_1d2d_sb(iid),'units'        , 'm')

              ierr = nf90_def_var(imapfile, '1d2d_s0_2d' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_s0_2d(iid))
              ierr = nf90_put_att(imapfile, id_1d2d_s0_2d(iid),'standard_name', '1d2d_s0_2d')
              ierr = nf90_put_att(imapfile, id_1d2d_s0_2d(iid),'long_name'    , 'water levels on interface at previous time step')
              ierr = nf90_put_att(imapfile, id_1d2d_s0_2d(iid),'units'        , 'm')

              ierr = nf90_def_var(imapfile, '1d2d_s1_2d' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_s1_2d(iid))
              ierr = nf90_put_att(imapfile, id_1d2d_s1_2d(iid),'standard_name', '1d2d_s1_2d')
              ierr = nf90_put_att(imapfile, id_1d2d_s1_2d(iid),'long_name'    , 'water levels on interface at current time step')
              ierr = nf90_put_att(imapfile, id_1d2d_s1_2d(iid),'units'        , 'm')

              ierr = nf90_def_var(imapfile, '1d2d_flow_cond' , nf90_int, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_flow_cond(iid))
              ierr = nf90_put_att(imapfile, id_1d2d_flow_cond(iid),'standard_name', 'flow_condition')
              ierr = nf90_put_att(imapfile, id_1d2d_flow_cond(iid),'long_name'    , 'flow Condition 0: closed, 1: free 1d to 2d, 2: free 2d to 1d, 3: submerged')
              ierr = nf90_put_att(imapfile, id_1d2d_flow_cond(iid),'units'        , '-')

          endif
       endif

       if (jamapwind > 0 .and. japatm > 0) then
           call definencvar(imapfile,id_patm(iid)   ,nf90_double,idims,2, 'Patm'  , 'Atmospheric Pressure', 'N m-2', 'FlowElem_xcc FlowElem_ycc')
       endif

       if (ice_mapout) then
           call definencvar(imapfile,id_ice_af(iid)  ,nf90_double,idims,2, 'ice_af' , 'Fraction of the surface area covered by floating ice', '1', 'FlowElem_xcc FlowElem_ycc')
           call definencvar(imapfile,id_ice_h(iid)   ,nf90_double,idims,2, 'ice_h'  , 'Thickness of floating ice cover', 'm', 'FlowElem_xcc FlowElem_ycc')
           call definencvar(imapfile,id_ice_p(iid)   ,nf90_double,idims,2, 'ice_p'  , 'Pressure exerted by the floating ice cover', 'N m-2', 'FlowElem_xcc FlowElem_ycc')
           if (ja_icecover == ICECOVER_SEMTNER) then
              call definencvar(imapfile,id_ice_t(iid)   ,nf90_double,idims,2, 'ice_t'  , 'Temperature of the floating ice cover', 'degC', 'FlowElem_xcc FlowElem_ycc')
              call definencvar(imapfile,id_snow_h(iid)  ,nf90_double,idims,2, 'snow_h'  , 'Thickness of the snow layer', 'm', 'FlowElem_xcc FlowElem_ycc')
              call definencvar(imapfile,id_snow_t(iid)  ,nf90_double,idims,2, 'snow_t'  , 'Temperature of the snow layer', 'degC', 'FlowElem_xcc FlowElem_ycc')
           endif
       endif

       if ((jamapwind > 0 .or. jamapwindstress > 0 .or. jaseparate_==2) .and. jawind /= 0) then
          if (jawindstressgiven == 0 .or. jaseparate_==2) then
             ierr = nf90_def_var(imapfile, 'windx', nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_windx(iid))
             ierr = nf90_def_var(imapfile, 'windy', nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_windy(iid))
          else
             ierr = nf90_def_var(imapfile, 'windstressx', nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_windx(iid))
             ierr = nf90_def_var(imapfile, 'windstressy', nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_windy(iid))
          endif

          ierr = nf90_put_att(imapfile, id_windx(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
          if (jawindstressgiven == 0 .or. jaseparate_==2) then
             if (jsferic == 0 ) then
                ierr = nf90_put_att(imapfile, id_windx(iid),  'standard_name', 'x_wind')
                ierr = nf90_put_att(imapfile, id_windx(iid),  'long_name'    , 'velocity of air on flow element center, x-component')
             else
                ierr = nf90_put_att(imapfile, id_windx(iid),  'standard_name', 'eastward_wind')
                ierr = nf90_put_att(imapfile, id_windx(iid),  'long_name'    , 'eastward air velocity on flow element center, x-component')
             endif
             ierr = nf90_put_att(imapfile, id_windx(iid),  'units'        , 'm s-1')
          else
             if (jsferic == 0 ) then
                ierr = nf90_put_att(imapfile, id_windx(iid),  'standard_name', 'x_windstress')
                ierr = nf90_put_att(imapfile, id_windx(iid),  'long_name'    , 'windstress on flow element center, x-component')
             else
                ierr = nf90_put_att(imapfile, id_windx(iid),  'standard_name', 'eastward_windstress')
                ierr = nf90_put_att(imapfile, id_windx(iid),  'long_name'    , 'eastward windstress on flow element center, x-component')
             endif
             ierr = nf90_put_att(imapfile, id_windx(iid),  'units'        , 'N m-2')
          endif

          ierr = nf90_put_att(imapfile, id_windy(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
          if (jawindstressgiven == 0 .or. jaseparate_==2) then
             if (jsferic == 0 ) then
                ierr = nf90_put_att(imapfile, id_windy(iid),  'standard_name', 'y_wind')
                ierr = nf90_put_att(imapfile, id_windy(iid),  'long_name'    , 'velocity of air on flow element center, y-component')
             else
                ierr = nf90_put_att(imapfile, id_windy(iid),  'standard_name', 'northward_wind')
                ierr = nf90_put_att(imapfile, id_windy(iid),  'long_name'    , 'northward air velocity on flow element center, y-component')
             endif
             ierr = nf90_put_att(imapfile, id_windy(iid),  'units'        , 'm s-1')
          else
             if (jsferic == 0 ) then
                ierr = nf90_put_att(imapfile, id_windy(iid),  'standard_name', 'y_windstress')
                ierr = nf90_put_att(imapfile, id_windy(iid),  'long_name'    , 'windstress air on flow element center, y-component')
             else
                ierr = nf90_put_att(imapfile, id_windy(iid),  'standard_name', 'northward_windstress')
                ierr = nf90_put_att(imapfile, id_windy(iid),  'long_name'    , 'northward windstress on flow element center, y-component')
             endif
             ierr = nf90_put_att(imapfile, id_windy(iid),  'units'        , 'N m-2')
          endif
       endif

       if (jamapwind > 0 .and. jawind /= 0 .and. jawindstressgiven == 0) then
          ! Also wind on flow links
          ierr = nf90_def_var(imapfile, 'windxu', nf90_double, (/ id_flowlinkdim(iid), id_timedim (iid)/) , id_windxu(iid))
          ierr = nf90_def_var(imapfile, 'windyu', nf90_double, (/ id_flowlinkdim(iid), id_timedim (iid)/) , id_windyu(iid))

          ierr = nf90_put_att(imapfile, id_windxu(iid),  'coordinates'  , 'FlowLink_xu FlowLink_yu')
          if (jsferic == 0) then
             ierr = nf90_put_att(imapfile, id_windxu(iid),  'long_name'    , 'velocity of air on flow links, x-component')
             ierr = nf90_put_att(imapfile, id_windxu(iid),  'standard_name', 'x_velocity_wind')
          else
             ierr = nf90_put_att(imapfile, id_windxu(iid),  'long_name'    , 'eastward air velocity on flow links, x-component')
             ierr = nf90_put_att(imapfile, id_windxu(iid),  'standard_name', 'eastward_wind')
          endif
          ierr = nf90_put_att(imapfile, id_windxu(iid),  'units'        , 'm s-1')

          ierr = nf90_put_att(imapfile, id_windyu(iid),  'coordinates'  , 'FlowLink_xu FlowLink_yu')
          if (jsferic == 0) then
             ierr = nf90_put_att(imapfile, id_windyu(iid),  'long_name'    , 'velocity of air on flow links, y-component')
             ierr = nf90_put_att(imapfile, id_windyu(iid),  'standard_name', 'y_velocity_wind')
          else
             ierr = nf90_put_att(imapfile, id_windyu(iid),  'long_name'    , 'northward air velocity on flow links, y-component')
             ierr = nf90_put_att(imapfile, id_windyu(iid),  'standard_name', 'northward_wind')
          endif
          ierr = nf90_put_att(imapfile, id_windyu(iid),  'units'        , 'm s-1')
       endif
       !
       ierr = unc_add_gridmapping_att(imapfile, (/ id_windx(iid), id_windy(iid), id_windxu(iid), id_windyu(iid),  nf90_global /), jsferic)

       if (javeg > 0) then
          ierr = nf90_def_var(imapfile, 'rnveg', nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_rnveg(iid))
          ierr = nf90_put_att(imapfile, id_rnveg(iid),'long_name'    , 'Stem density of vegetation')
          ierr = nf90_put_att(imapfile, id_rnveg(iid),'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
          ierr = nf90_put_att(imapfile, id_rnveg(iid),'units'        , 'm-2')

          ierr = nf90_def_var(imapfile, 'diaveg', nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_diaveg(iid))
          ierr = nf90_put_att(imapfile, id_diaveg(iid),'long_name'    , 'Stem diameter of vegetation')
          ierr = nf90_put_att(imapfile, id_diaveg(iid),'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
          ierr = nf90_put_att(imapfile, id_diaveg(iid),'units'        , 'm')

          ierr = nf90_def_var(imapfile, 'veg_stemheight', nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_veg_stemheight(iid))
          ierr = nf90_put_att(imapfile, id_veg_stemheight(iid),'long_name'    , 'Stem height of vegetation')
          ierr = nf90_put_att(imapfile, id_veg_stemheight(iid),'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
          ierr = nf90_put_att(imapfile, id_veg_stemheight(iid),'units'        , 'm')
       endif

       ! For all 3D variables, expand the coordinate attribute with a vertical coordinate
       ierr = nf90_inq_varid( imapfile, 'LayCoord_cc', varid)
       if (ierr==NF90_NOERR) then
          zcc_elem = 'LayCoord_cc'
          zw_elem = 'LayCoord_w'
          zu_link = 'LayCoord_cc'   ! z/sigma coords are the same for u-positions and cc-positions.
          zwu_link = 'LayCoord_w'
       else
          zcc_elem = 'FlowElem_zcc'
          zw_elem = 'FlowElem_zw'
          zu_link = ''                        ! To be added, Issue UNST-4880
          zwu_link = ''
       endif
       if (nf90_inquire(imapfile, nVariables=varid)==NF90_NOERR) then
          do while (varid>0)
             if (nf90_inquire_variable(imapfile, varid, ndims=ndims)==NF90_NOERR) then
                call realloc(idum,ndims,keepexisting=.False.)
                if (nf90_inquire_variable(imapfile, varid, dimids=idum)==NF90_NOERR) then
                   if (any(idum==id_wdim(iid)) .and. any(idum==id_flowelemdim(iid))) then
                       ierr = ncu_append_atts( imapfile, varid, 'coordinates', trim(zw_elem))
                   endif
                   if (any(idum==id_laydim(iid)) .and. any(idum==id_flowelemdim(iid))) then
                       ierr = ncu_append_atts( imapfile, varid, 'coordinates', trim(zcc_elem))
                   endif
                   if (any(idum==id_wdim(iid)) .and. any(idum==id_flowlinkdim(iid))) then
                       ierr = ncu_append_atts( imapfile, varid, 'coordinates', trim(zwu_link))
                   endif
                   if (any(idum==id_laydim(iid)) .and. any(idum==id_flowelemdim(iid))) then
                       ierr = ncu_append_atts( imapfile, varid, 'coordinates', trim(zu_link))
                   endif
                endif
             endif
             varid = varid - 1
          enddo
       endif


       ierr = nf90_enddef(imapfile)

       ! 1D2D boundaries
       if (nbnd1d2d > 0 .and. jaseparate_ /= 2) then
          if (allocated(idum)) deallocate(idum)
          allocate(idum(nbnd1d2d))
          do i=1,nbnd1d2d
             idum(i) = kbnd1d2d(3, i) ! Flow link nrs
          enddo
          ierr = nf90_put_var(imapfile, id_1d2d_edges(iid), idum)
          deallocate(idum)
       endif

       if (nomba > 0) then
          ierr = nf90_put_var(imapfile, id_mba(iid), mbadef(1:NdxNdxi))
       endif

       firststep(iid) = .false.

   endif

   ! End of writing time-independent flow geometry data.
   ! -- Inquire id's belonging to map file ------------------------
   if (firststep(iid) .and. ndim>0) then ! TODO: AvD: UNST-530
      !
      !
      ! this step is necessary because if a snapshot_map.nc file is written
      ! in between two map file outputs the saved id's may have changed
      !
      firststep(iid) = .false.
      !
      ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim(iid))
      if (ierr /= nf90_noerr) then
         ierr = nf90_inq_dimid(imapfile, 'nFlowElemWithBnd', id_flowelemdim(iid))
      endif

      ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim(iid))
      !
      ! Time
      ierr = nf90_inq_dimid(imapfile, 'time', id_timedim(iid))
      ierr = nf90_inq_varid(imapfile, 'time', id_time(iid))
      !
      if ( kmx>0 ) then
         ierr = nf90_inq_dimid(imapfile, 'laydim', id_laydim(iid))
         ierr = nf90_inq_dimid(imapfile, 'wdim', id_wdim(iid))
      endif
      !
      ! Size of latest timestep

      ! Why ask for id_*, they are in a save statement no?

      ierr = nf90_inq_varid(imapfile, 'timestep', id_timestep(iid))
      ierr = nf90_inq_varid(imapfile, 'taus' ,  id_taus(iid))
      !
      if ( kmx>0 ) then     !  3D
         ierr = nf90_inq_varid(imapfile, 'ucx', id_ucx(iid))
         ierr = nf90_inq_varid(imapfile, 'ucy', id_ucy(iid))
         ierr = nf90_inq_varid(imapfile, 'ucz', id_ucz(iid))
         ierr = nf90_inq_varid(imapfile, 'ucxa', id_ucxa(iid))
         ierr = nf90_inq_varid(imapfile, 'ucya', id_ucya(iid))
         ierr = nf90_inq_varid(imapfile, 'ww1', id_ww1(iid))
         if ( density_is_pressure_dependent() ) then
           ierr = nf90_inq_varid(imapfile, 'density', id_rho(iid))
         else
           ierr = nf90_inq_varid(imapfile, 'rho', id_rhop(iid))
         endif
         if ( iturbulencemodel >= 3 ) then
            ierr = nf90_inq_varid(imapfile, 'turkin1', id_turkin1(iid))
            ierr = nf90_inq_varid(imapfile, 'tureps1', id_tureps1(iid))
            ierr = nf90_inq_varid(imapfile, 'vicwwu' , id_vicwwu(iid) )
         endif
       else
         ierr = nf90_inq_varid(imapfile, 'ucx', id_ucx(iid))
         ierr = nf90_inq_varid(imapfile, 'ucy', id_ucy(iid))
         ierr = nf90_inq_varid(imapfile, 'spircrv', id_spircrv(iid))
         ierr = nf90_inq_varid(imapfile, 'spirint', id_spirint(iid))
       endif
       !
       if (jasal > 0) then
          ierr = nf90_inq_varid(imapfile, 'sa1', id_sa1(iid))
       endif

       if (jatem > 0) then
          ierr = nf90_inq_varid(imapfile, 'tem1', id_tem1(iid))
       endif

       if (ITRA1 > 0) then
          do j=ITRA1,ITRAN
             tmpstr = const_names(j)
             ! Forbidden chars in NetCDF names: space, /, and more.
             call replace_char(tmpstr,32,95)
             call replace_char(tmpstr,47,95)
             ierr = nf90_inq_varid(imapfile, trim(tmpstr), id_const(iid,j))
          enddo
       endif

       !
       if (stm_included) then
          ierr = nf90_inq_varid(imapfile, 'nSedTot', id_sedtotdim(iid))
          ierr = nf90_inq_varid(imapfile, 'nSedSus', id_sedsusdim(iid))
          ierr = nf90_inq_varid(imapfile, 'nBedLayers', id_nlyrdim(iid))

          if (stmpar%lsedsus > 0) then
             ierr = nf90_inq_varid(imapfile, 'ws', id_ws(iid))
             !
             ! equilibrium concentration, 2D only
             if (kmx == 0) then
                ierr = nf90_inq_varid(imapfile, 'rsedeq', id_rsedeq(iid))
             endif

             if (stmpar%morpar%moroutput%sourcesink) then
                ierr = nf90_inq_varid(imapfile, 'sourse', id_sourse(iid))
                ierr = nf90_inq_varid(imapfile, 'sinkse', id_sinkse(iid))
             endif

             if (stmpar%morpar%moroutput%suvcor) then
                ierr = nf90_inq_varid(imapfile, 'e_scrn', id_scrn(iid))
                !ierr = nf90_inq_varid(imapfile, 'e_scrt', id_scrt(iid))
             endif

             if (stmpar%morpar%moroutput%aks) then
                ierr = nf90_inq_varid(imapfile, 'aks', id_aks(iid))
                ierr = nf90_inq_varid(imapfile, 'rca', id_rca(iid))
             endif
             !
             ! Suspended fractions
             if (stmpar%lsedsus .gt. 0) then
                do j=ISED1,ISEDN
                   tmpstr = const_names(j)
                   ! Forbidden chars in NetCDF names: space, /, and more.
                   call replace_char(tmpstr,32,95)
                   call replace_char(tmpstr,47,95)
                   ierr = nf90_inq_varid(imapfile, trim(tmpstr), id_const(iid,j))
                enddo
             endif
          endif

          if (stmpar%morpar%moroutput%dzduuvv) then ! bedslope
             ierr = nf90_inq_varid(imapfile, 'e_dzdn', id_dzdn(iid))
             ierr = nf90_inq_varid(imapfile, 'e_dzdt', id_dzdt(iid))
          endif

          if (stmpar%morpar%moroutput%umod) then
             ierr = nf90_inq_varid(imapfile, 'umod', id_umod(iid))
          endif

          if (stmpar%morpar%moroutput%zumod) then
             ierr = nf90_inq_varid(imapfile, 'zumod', id_zumod(iid))
          endif

          if (stmpar%morpar%moroutput%ustar) then
             ierr = nf90_inq_varid(imapfile, 'ustar', id_ustar(iid))
          endif

          if (stmpar%morpar%moroutput%sbcuv) then
             ierr = nf90_inq_varid(imapfile, 'sbcx', id_sbcx(iid))
             ierr = nf90_inq_varid(imapfile, 'sbcy', id_sbcy(iid))
             ierr = nf90_inq_varid(imapfile, 'sbcx_reconstructed', id_sbcx_reconstructed(iid))
             ierr = nf90_inq_varid(imapfile, 'sbcy_reconstructed', id_sbcy_reconstructed(iid))
          endif

          if (stmpar%morpar%moroutput%sbwuv) then
             ierr = nf90_inq_varid(imapfile, 'sbwx', id_sbwx(iid))
             ierr = nf90_inq_varid(imapfile, 'sbwy', id_sbwy(iid))
             ierr = nf90_inq_varid(imapfile, 'sbwx_reconstructed', id_sbwx_reconstructed(iid))
             ierr = nf90_inq_varid(imapfile, 'sbwy_reconstructed', id_sbwy_reconstructed(iid))
          endif

          if (stmpar%morpar%moroutput%sswuv) then
             ierr = nf90_inq_varid(imapfile, 'sswx', id_sswx(iid))
             ierr = nf90_inq_varid(imapfile, 'sswy', id_sswy(iid))
             ierr = nf90_inq_varid(imapfile, 'sswx_reconstructed', id_sswx_reconstructed(iid))
             ierr = nf90_inq_varid(imapfile, 'sswy_reconstructed', id_sswy_reconstructed(iid))
          endif

          if (stmpar%morpar%moroutput%sscuv) then
             ierr = nf90_inq_varid(imapfile, 'sscx', id_sscx(iid))
             ierr = nf90_inq_varid(imapfile, 'sscy', id_sscy(iid))
             ierr = nf90_inq_varid(imapfile, 'sscx_reconstructed', id_sscx_reconstructed(iid))
             ierr = nf90_inq_varid(imapfile, 'sscy_reconstructed', id_sscy_reconstructed(iid))
          endif

          ierr = nf90_inq_varid(imapfile, 'sxtot', id_sxtot(iid))
          ierr = nf90_inq_varid(imapfile, 'sytot', id_sytot(iid))

          ierr = nf90_inq_varid(imapfile, 'mor_bl', id_morbl(iid))

          select case (stmpar%morlyr%settings%iunderlyr)
          case (1)
             ierr = nf90_inq_varid(imapfile, 'bodsed', id_bodsed(iid))
             ierr = nf90_inq_varid(imapfile, 'dpsed', id_dpsed(iid))
          case (2)
             ierr = nf90_inq_varid(imapfile, 'msed', id_msed(iid))
             ierr = nf90_inq_varid(imapfile, 'lyrfrac', id_lyrfrac(iid))
             ierr = nf90_inq_varid(imapfile, 'thlyr', id_thlyr(iid))
             if (stmpar%morlyr%settings%iporosity>0) then
                ierr = nf90_inq_varid(imapfile, 'poros', id_poros(iid))
             endif
          end select
       !
          if (stmpar%morpar%moroutput%taurat) then
             ierr = nf90_inq_varid(imapfile, 'taurat' ,id_taurat(iid))
          endif
          if (stmpar%morpar%moroutput%dm) then
             ierr = nf90_inq_varid(imapfile, 'dm' ,id_dm(iid))
          endif
          if (stmpar%morpar%moroutput%dg) then
             ierr = nf90_inq_varid(imapfile, 'dg' ,id_dg(iid))
          endif
          if (stmpar%morpar%moroutput%dgsd) then
             ierr = nf90_inq_varid(imapfile, 'dgsd' ,id_dgsd(iid))
          endif
          if (stmpar%morpar%moroutput%percentiles) then
             do l = 1, stmpar%morpar%nxx
                write(dxname,'(A,I2.2)') 'DXX',l
                ierr = nf90_inq_varid(imapfile, dxname ,id_dxx(l,iid))
             enddo
          endif
          if (stmpar%morpar%moroutput%frac) then
             ierr = nf90_inq_varid(imapfile, 'frac' ,id_frac(iid))
          endif
          if (stmpar%morpar%moroutput%mudfrac) then
             ierr = nf90_inq_varid(imapfile, 'mudfrac' ,id_mudfrac(iid))
          endif
          if (stmpar%morpar%moroutput%sandfrac) then
             ierr = nf90_inq_varid(imapfile, 'sandfrac' ,id_sandfrac(iid))
          endif
          if (stmpar%morpar%moroutput%fixfac) then
             ierr = nf90_inq_varid(imapfile, 'fixfac' ,id_fixfac(iid))
          endif
          if (stmpar%morpar%moroutput%hidexp) then
             ierr = nf90_inq_varid(imapfile, 'hidexp' ,id_hidexp(iid))
          endif
          ! Fluff layers
          if (stmpar%morpar%flufflyr%iflufflyr>0 .and. stmpar%lsedsus>0) then
             ierr = nf90_inq_varid(imapfile, 'mfluff' ,id_mfluff(iid))
          endif
       endif

       if (bfmpar%lfbedfrmout) then
          if (bfmpar%lfbedfrm) then
             ierr = nf90_inq_varid(imapfile, 'duneheight' ,id_duneheight(iid))
             ierr = nf90_inq_varid(imapfile, 'dunelength' ,id_dunelength(iid))
          endif
          if (bfmpar%lfbedfrmrou) then
             ierr = nf90_inq_varid(imapfile, 'ksr' ,id_ksr(iid))
             ierr = nf90_inq_varid(imapfile, 'ksmr' ,id_ksmr(iid))
             ierr = nf90_inq_varid(imapfile, 'ksd' ,id_ksd(iid))
             ierr = nf90_inq_varid(imapfile, 'ks' ,id_ks(iid))
          endif
       endif
       !
       if (jased > 0 .and. .not.stm_included) then
          ierr = nf90_inq_dimid(imapfile, 'nFrac', id_maxfracdim(iid))
          if (jaceneqtr == 1) then
             ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_erolaydim(iid)) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
             if (ierr /= nf90_noerr) then
                ierr = nf90_inq_dimid(imapfile, 'nFlowElemWithBnd', id_erolaydim(iid))
             endif
          else
             ierr = nf90_inq_dimid(imapfile, 'nNetNode', id_erolaydim(iid)) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
          endif
          !
          ierr = nf90_inq_varid(imapfile, 'sed', id_sed(iid))
          !
          ierr = nf90_inq_varid(imapfile, 'ero', id_ero(iid))
       endif
       !
       ! JRE - XBeach
       if (jawave .eq. 4) then
          ierr = nf90_inq_varid(imapfile, 'E'        , id_E(iid))
          ierr = nf90_inq_varid(imapfile, 'R'        , id_R(iid))
          ierr = nf90_inq_varid(imapfile, 'H'        , id_H(iid))
          ierr = nf90_inq_varid(imapfile, 'D'        , id_D(iid))
          ierr = nf90_inq_varid(imapfile, 'DR'       , id_DR(iid))
          ierr = nf90_inq_varid(imapfile, 'urms'     , id_urms(iid))
          ierr = nf90_inq_varid(imapfile, 'urms_cc'  , id_urmscc(iid))
          ierr = nf90_inq_varid(imapfile, 'ust'      , id_ust(iid))
          ierr = nf90_inq_varid(imapfile, 'vst'      , id_vst(iid))
          ierr = nf90_inq_varid(imapfile, 'Fx_cc'    , id_Fxcc(iid))
          ierr = nf90_inq_varid(imapfile, 'Fy_cc'    , id_Fycc(iid))

          ierr = nf90_inq_varid(imapfile, 'thetamean', id_thetamean(iid))
          ierr = nf90_inq_varid(imapfile, 'cwav'     , id_cwav(iid))
          ierr = nf90_inq_varid(imapfile, 'cgwav'    , id_cgwav(iid))
          ierr = nf90_inq_varid(imapfile, 'sigmwav'  , id_sigmwav(iid))

          !if ( (windmodel .eq. 1) .and. (jawsource .eq. 1) ) then
          !   ierr = nf90_inq_varid(imapfile, 'SwE'  , id_SwE(iid))
          !   ierr = nf90_inq_varid(imapfile, 'SwT'  , id_SwT(iid))
          !endif

       endif

       ! 1D2D boundaries
       if (nbnd1d2d > 0) then
          ierr = nf90_inq_varid(imapfile, '1d2d_flowlinknrs' , id_1d2d_edges(iid))
          ierr = nf90_inq_varid(imapfile, '1d2d_zeta'        , id_1d2d_zeta1d(iid))
          ierr = nf90_inq_varid(imapfile, '1d2d_crest_level' , id_1d2d_crest_level(iid))
          ierr = nf90_inq_varid(imapfile, '1d2d_b_2di'       , id_1d2d_b_2di(iid))
          ierr = nf90_inq_varid(imapfile, '1d2d_b_2dv'       , id_1d2d_b_2dv(iid))
          ierr = nf90_inq_varid(imapfile, '1d2d_d_2dv'       , id_1d2d_d_2dv(iid))
          ierr = nf90_inq_varid(imapfile, '1d2d_q_zeta'      , id_1d2d_q_zeta(iid))
          ierr = nf90_inq_varid(imapfile, '1d2d_q_lat'       , id_1d2d_q_lat(iid))
          ierr = nf90_inq_varid(imapfile, '1d2d_cfl'         , id_1d2d_cfl(iid))
          ierr = nf90_inq_varid(imapfile, '1d2d_sb'          , id_1d2d_sb(iid))
          ierr = nf90_inq_varid(imapfile, 'id_1d2d_s0_2d'    , id_1d2d_s0_2d(iid))
          ierr = nf90_inq_varid(imapfile, 'id_1d2d_s1_2d'    , id_1d2d_s1_2d(iid))
          ierr = nf90_inq_varid(imapfile, '1d2d_flow_cond'   , id_1d2d_flow_cond(iid))
       endif

       if ( jamaptidep.eq.1 .and. jatidep > 0 ) then
          if ( jaselfal.eq.0 ) then
             ierr = nf90_inq_varid(imapfile, 'TidalPotential', id_tidep(iid))
          else
             ierr = nf90_inq_varid(imapfile, 'TidalPotential_no_SAL', id_tidep(iid))
          endif
          if ( jaselfal.gt.0 ) then
             ierr = nf90_inq_varid(imapfile, 'SALPotential', id_salp(iid))
          endif
       endif

       if ( jamapIntTidesDiss.eq.1 .and. jaFrcInternalTides2D.gt.0 ) then
          ierr = nf90_inq_varid(imapfile, 'internal_tides_dissipation', id_inttidesdiss(iid))
       endif

       !
       ! Flow data on edges
       ierr = nf90_inq_varid(imapfile, 'unorm' , id_unorm(iid))
       !
       ! Flow data on edges
       ierr = nf90_inq_varid(imapfile, 'u0'    , id_u0(iid))
       ierr = nf90_inq_varid(imapfile, 'q1'    , id_q1(iid))
       ierr = nf90_inq_varid(imapfile, 'viu'   , id_viu(iid))
       ierr = nf90_inq_varid(imapfile, 'diu'   , id_diu(iid))
       !
       if (jawind/=0) then
           ierr = nf90_inq_varid(imapfile, 'windx', id_windx(iid))
           ierr = nf90_inq_varid(imapfile, 'windy', id_windy(iid))
       endif

       if (jaseparate_==2 .and. javeg > 0) then
          ierr = nf90_inq_varid(imapfile, 'rnveg', id_rnveg(iid))
          ierr = nf90_inq_varid(imapfile, 'diaveg', id_diaveg(iid))
          ierr = nf90_inq_varid(imapfile, 'veg_stemheight', id_veg_stemheight(iid))
       endif
   endif

   ! -- Start data writing (flow data) ------------------------
   if (jaseparate_ == 1) then
       itim = 1
       firststep(iid) = .true.
   elseif (jaseparate_ == 2) then
       itim = 1
   else
       it_map   = it_map+1
       itim     = it_map ! Increment time dimension index
   endif

   ! Time
   ierr = nf90_put_var(imapfile, id_time    (iid), tim, (/ itim /))
   ierr = nf90_put_var(imapfile, id_timestep(iid), dts, (/ itim /))

   !
   ! Transform uxy/ucy into Eulerian velocities,
   ! only when the user asks for it and only if we are not writing to com-file
   !
   jaeulerloc = 0
   if (jaeulervel==1 .and. jaseparate_/=2 .and. jawave.gt.0) then
      jaeulerloc = 1
   endif
   !
   call getucxucyeulmag(ndkx, workx, worky, ucmag, jaeulerloc, 0)
   !
   !  Hack to pass time varying bottom levels to SWAN
   !  Also needed for morphostatic runs in 3D
   !
   if (jaseparate_==2) then
      ! JRE: was _zcc, but this has laydim included as dimension, which does not work in 3D
      ierr = nf90_inq_varid(imapfile, 'FlowElem_bl', id_swanbl(iid))
      ierr = nf90_put_var(imapfile, id_swanbl(iid),  -bl,   (/ 1, itim /), (/ ndxndxi, 1 /))
   endif
   !
   ! Water level
   if (jamaps1>0 .or. jaseparate_==2) then
      ierr = nf90_put_var(imapfile, id_s1(iid),  s1,   (/ 1, itim /), (/ ndxndxi, 1 /))
   endif
   !
   if (jamapucvec>0 .or. jaseparate_==2) then
      if ( kmx==0 ) then
         ierr = nf90_put_var(imapfile, id_ucx  (iid), workx,  (/ 1, itim /), (/ ndxndxi, 1 /))
         ierr = nf90_put_var(imapfile, id_ucy  (iid), worky,  (/ 1, itim /), (/ ndxndxi, 1 /))
      endif
   endif

   if ( kmx>0 ) then
      call unc_append_3dflowgeom_put(imapfile, jaseparate_, itim) ! needed for 3D wave coupling on comfile: Flowelem_zw
      if (jamapucvec>0 .or. jaseparate_==2) then
         do kk=1,ndxndxi
            work1(:, kk) = dmiss ! For proper fill values in z-model runs.
            call getkbotktop(kk,kb,kt)
            call getlayerindices(kk, nlayb, nrlay)
            do k = kb,kt
               work1(k-kb+nlayb,kk) = workx(k)
            enddo
         enddo
         ierr = nf90_put_var(imapfile, id_ucx(iid), work1(1:kmx,1:ndxndxi), start=(/ 1, 1, itim /), count=(/ kmx, ndxndxi, 1 /))

         do kk=1,ndxndxi
            work1(:, kk) = dmiss ! For proper fill values in z-model runs.
            call getkbotktop(kk,kb,kt)
            call getlayerindices(kk, nlayb, nrlay)
            do k = kb,kt
               work1(k-kb+nlayb,kk) = worky(k)
            enddo
         enddo
         ierr = nf90_put_var(imapfile, id_ucy(iid), work1(1:kmx,1:ndxndxi), start=(/ 1, 1, itim /), count=(/ kmx, ndxndxi, 1 /))
      endif
   endif

   if (jaseparate_ /= 2) then
       if (jamaps0>0) then
          ierr = nf90_put_var(imapfile, id_s0(iid),  s0,   (/ 1, itim /), (/ ndxndxi, 1 /))
       endif

       if (jamaphs>0) then
          ierr = nf90_put_var(imapfile, id_hs(iid),  hs,   (/ 1, itim /), (/ ndxndxi, 1 /))
       endif
      ! Tau current and chezy roughness
      if (jamaptaucurrent > 0 .or. jamap_chezy_elements > 0 .or. jamap_chezy_links > 0) then
         if (jawave==0) then       ! Else, get taus from subroutine tauwave (taus = f(taucur,tauwave))
            call gettaus(1,1)       ! Update taus and czs
         else if (jamap_chezy_links > 0) then
            call gettaus(2,1)       ! Only update czs
         endif
         if (jawave>0 .and. .not. flowWithoutWaves) then
            call gettauswave(jawaveswartdelwaq)
         endif
      endif
      !
      if (jamap_chezy_links > 0) then
         do LL = 1,lnx
            if (frcu(LL) > 0d0) then
               call getcz (hu(LL), frcu(LL), ifrcutp(LL), czu(LL), LL)
            endif
         enddo
      endif
      !
      if (jamaptaucurrent > 0) then
          ierr = nf90_put_var(imapfile, id_taus(iid), taus,  (/ 1, itim /), (/ ndxndxi, 1 /))
      endif
      !
      if (jamap_chezy_elements > 0) then
          ierr = nf90_put_var(imapfile, id_czs(iid), czs,  (/ 1, itim /), (/ ndxndxi, 1 /))
      endif
      if (jamap_chezy_links > 0) then
          ierr = nf90_put_var(imapfile, id_czu(iid), czu,  (/ 1, itim /), (/ lnx, 1 /))
      endif

      ! Velocities
      if ( kmx>0 ) then
!        3D
         if (jamapucvec>0) then
            call reconstructucz(0)
            !
            do kk=1,ndxndxi
               work1(:, kk) = dmiss ! For proper fill values in z-model runs.
               call getkbotktop(kk,kb,kt)
               call getlayerindices(kk, nlayb, nrlay)
               do k = kb,kt
                  work1(k-kb+nlayb,kk) = ucz(k)
               enddo
            enddo
            ierr = nf90_put_var(imapfile, id_ucz(iid), work1(1:kmx,1:ndxndxi), start=(/ 1, 1, itim /), count=(/ kmx, ndxndxi, 1 /))

            ierr = nf90_put_var(imapfile, id_ucxa(iid), ucxq(1:ndxndxi), start=(/ 1, itim /), count=(/ ndxndxi, 1 /))
            ierr = nf90_put_var(imapfile, id_ucya(iid), ucyq(1:ndxndxi), start=(/ 1, itim /), count=(/ ndxndxi, 1 /))
         endif

         if (jamapww1 > 0) then
            do kk=1,ndxndxi
               work0(:, kk) = dmiss ! For proper fill values in z-model runs.
               call getkbotktop(kk,kb,kt)
               call getlayerindices(kk, nlayb, nrlay)
               do k = kb-1,kt
                  work0(k-kb+nlayb,kk) = ww1(k)
               enddo
            enddo
            ierr = nf90_put_var(imapfile, id_ww1(iid), work0(0:kmx,1:ndxndxi), start=(/ 1, 1, itim /), count=(/ kmx+1, ndxndxi, 1 /))
         endif

         if (jamapu1>0) then
            do LL=1,lnx
               work1(:, LL) = dmiss ! For proper fill values in z-model runs.
               call getLbotLtopmax(LL,Lb,Ltx)
               call getlayerindicesLmax(LL, nlaybL, nrlayLx)
               do L = Lb,Ltx
                   work1(L-Lb+nlaybL,LL) = u1(L)
               enddo
            enddo
            ierr = nf90_put_var(imapfile, id_unorm(iid), work1(1:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx, lnx, 1 /))
         endif

         if (jamapu0>0) then
            do LL=1,lnx
               work1(:, LL) = dmiss ! For proper fill values in z-model runs.
               call getLbotLtopmax(LL,Lb,Ltx)
               call getlayerindicesLmax(LL, nlaybL, nrlayLx)
               do L = Lb,Ltx
                   work1(L-Lb+nlaybL,LL) = u0(L)
               enddo
            enddo
            ierr = nf90_put_var(imapfile, id_u0(iid)   , work1(1:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx, lnx, 1 /))
         endif

         if (jamapq1 > 0) then
            do LL=1,lnx
               work1(:, LL) = dmiss ! For proper fill values in z-model runs.
               call getLbotLtopmax(LL,Lb,Ltx)
               call getlayerindicesLmax(LL, nlaybL, nrlayLx)
               do L = Lb,Ltx
                   work1(L-Lb+nlaybL,LL) = q1(L)
               enddo
            enddo
            ierr = nf90_put_var(imapfile, id_q1(iid)   , work1(1:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx, lnx, 1 /))
         endif

         if (jamapviu>0) then
            do LL=1,lnx
               work1(:, LL) = dmiss ! For proper fill values in z-model runs.
               call getLbotLtopmax(LL,Lb,Ltx)
               call getlayerindicesLmax(LL, nlaybL, nrlayLx)
               if (javiusp == 1) then       ! user specified part
                   vicc = viusp(LL)
               else
                   vicc = vicouv
               endif
               do L = Lb,Ltx
                   work1(L-Lb+nlaybL,LL) = viu(L) + vicc
               enddo
            enddo
            ierr = nf90_put_var(imapfile, id_viu(iid)   , work1(1:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx, lnx, 1 /))
         endif

         if (jamapdiu>0) then
            do LL=1,lnx
               work1(:, LL) = dmiss ! For proper fill values in z-model runs.
               call getLbotLtopmax(LL,Lb,Ltx)
               call getlayerindicesLmax(LL, nlaybL, nrlayLx)
               if (jadiusp == 1) then
                   dicc = diusp(LL)
               else
                   dicc = dicouv
               endif
               do L = Lb,Ltx
                   work1(L-Lb+nlaybL,LL) = viu(L) / 0.7 + dicc
               enddo
            enddo
            ierr = nf90_put_var(imapfile, id_diu(iid)   , work1(1:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx, lnx, 1 /))
         endif

         if (jamaprho>0) then
            do kk=1,ndxndxi
               work1(:, kk) = dmiss ! For proper fill values in z-model runs.
               call getkbotktop(kk,kb,kt)
               call getlayerindices(kk, nlayb, nrlay)
               do k = kb,kt
                  work1(k-kb+nlayb, kk) = rho(k)
               enddo
            enddo
            if ( density_is_pressure_dependent() ) then
                ierr = nf90_put_var(imapfile, id_rho(iid),  work1(1:kmx,1:ndxndxi), start=(/ 1, 1, itim /), count=(/ kmx, ndxndxi, 1 /))
            else
                ierr = nf90_put_var(imapfile, id_rhop(iid), work1(1:kmx,1:ndxndxi), start=(/ 1, 1, itim /), count=(/ kmx, ndxndxi, 1 /))
            endif
         endif

         if (jamaptur > 0 .and. iturbulencemodel >= 3) then
            do LL=1,lnx
               work0(:, LL) = dmiss ! For proper fill values in z-model runs.
               call getLbotLtopmax(LL,Lb,Ltx)
               call getlayerindicesLmax(LL, nlaybL, nrlayLx)
               do L = Lb-1,Ltx
                  work0(L-Lb+nlaybL,LL) = turkin1(L)
               enddo
            enddo
            ierr = nf90_put_var(imapfile, id_turkin1(iid)   , work0(0:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx+1, lnx, 1 /))
            do LL=1,lnx
               work0(:, LL) = dmiss ! For proper fill values in z-model runs.
               call getLbotLtopmax(LL,Lb,Ltx)
               call getlayerindicesLmax(LL, nlaybL, nrlayLx)
               do L = Lb-1,Ltx
                  work0(L-Lb+nlaybL,LL) = tureps1(L)
               enddo
            enddo
            ierr = nf90_put_var(imapfile, id_tureps1(iid)   , work0(0:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx+1, lnx, 1 /))
            do LL=1,lnx
               work0(:, LL) = dmiss ! For proper fill values in z-model runs.
               call getLbotLtopmax(LL,Lb,Ltx)
               call getlayerindicesLmax(LL, nlaybL, nrlayLx)
               do L = Lb-1,Ltx
                  work0(L-Lb+nlaybL,LL) = vicwwu(L)
               enddo
            enddo
            ierr = nf90_put_var(imapfile, id_vicwwu(iid)   , work0(0:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx+1, lnx, 1 /))
            work0 = dmiss
            do kk = 1,ndxi
            call getkbotktop(kk,kb,kt)
            call getlayerindices(kk, nlayb, nrlay)
            do k = kb-1,kt
                work0(k-kb+nlayb, kk) = vicwws(k)
            enddo
            enddo
            ierr = nf90_put_var(imapfile, id_vicwws(iid), work0(0:kmx,1:ndxi), (/ 1, 1, itim /), (/ kmx+1, ndxi, 1 /))
         endif

      endif

      if ( jasecflow > 0 .and. jamapspir > 0) then
         ierr = nf90_put_var(imapfile, id_spirint(iid), spirint, (/ 1, itim /), (/ ndxndxi, 1 /))
         if ( kmx == 0 ) then
            ierr = nf90_put_var(imapfile, id_spircrv(iid), spircrv, (/ 1, itim /), (/ ndxndxi, 1 /))
         endif
      endif

      if ( kmx == 0 ) then
         if (jamapu1>0) then
            ierr = nf90_put_var(imapfile, id_unorm(iid), u1 ,  (/ 1, itim /), (/ lnx , 1 /))
         endif

         if (jamapu0>0) then
            ierr = nf90_put_var(imapfile, id_u0   (iid), u0 ,  (/ 1, itim /), (/ lnx , 1 /))
         endif

         if (jamapq1>0) then
            ierr = nf90_put_var(imapfile, id_q1 (iid)  , q1     , (/ 1, itim /), (/ lnx    , 1 /))
         endif

         if (jamapviu>0) then
            do LL=1,lnx
               work1(:,LL) = dmiss
               if (javiusp == 1) then       ! user specified part
                  vicc = viusp(LL)
               else
                  vicc = vicouv
               endif
               work1(1,LL) = viu(LL) + vicc
            enddo
            ierr = nf90_put_var(imapfile, id_viu (iid), work1(1:1,1:lnx) ,  (/ 1, itim /), (/ lnx , 1 /))
         endif

         if (jamapdiu>0) then
            do LL=1,lnx
               work1(:,LL) = dmiss
               if (jadiusp == 1) then
                  dicc = diusp(LL)
               else
                  dicc = dicouv
               endif
               work1(1,LL) = viu(LL) * 0.7 + dicc
            enddo
            ierr = nf90_put_var(imapfile, id_diu (iid), work1(1:1,1:lnx) ,  (/ 1, itim /), (/ lnx , 1 /))
          endif
      endif

   endif

   if (jaseparate_ /= 2) then


      ! Salinity
      if (jamapsal > 0 .and. jasal > 0) then
         if ( kmx>0 ) then
!           3D
            !do kk=1,ndxndxi
            !   call getkbotktop(kk,kb,kt)
            !   ierr = nf90_put_var(imapfile, id_sa1(iid), sa1(kb:kt), (/ 1, kk, itim /), (/ kt-kb+1, 1, 1 /))
            !enddo
            do kk=1,ndxndxi
                work1(:,kk) = dmiss ! For proper fill values in z-model runs.
               call getkbotktop(kk,kb,kt)
               call getlayerindices(kk, nlayb, nrlay)
               do k = kb,kt
                  work1(k-kb+nlayb, kk) = constituents(isalt, k)
               enddo
            enddo
            ierr = nf90_put_var(imapfile, id_sa1(iid), work1(1:kmx,1:ndxndxi), (/ 1, 1, itim /), (/ kmx, ndxndxi, 1 /))
         else
            do k = 1, ndxndxi
               sa1(k) = constituents(isalt, k)
           enddo
            ierr = nf90_put_var(imapfile, id_sa1(iid), sa1, (/ 1, itim /), (/ ndxndxi, 1 /))
         endif
      endif

      if (jamaptem > 0 .and. jatem > 0) then
         if ( kmx>0 ) then ! 3D
            !do kk=1,ndxndxi
            !   call getkbotktop(kk,kb,kt)
            !   ierr = nf90_put_var(imapfile, id_tem1(iid), tem1(kb:kt), (/ 1, kk, itim /), (/ kt-kb+1, 1, 1 /))
            !enddo
            do kk=1,ndxndxi
               work1(:,kk) = dmiss ! For proper fill values in z-model runs.
               call getkbotktop(kk,kb,kt)
               call getlayerindices(kk, nlayb, nrlay)
               do k = kb,kt
                  work1(k-kb+nlayb, kk) = constituents(itemp,k)
               enddo
            enddo
            ierr = nf90_put_var(imapfile, id_tem1(iid), work1(1:kmx,1:ndxndxi), (/ 1, 1, itim /), (/ kmx, ndxndxi, 1 /))
         else
            do k = 1, ndxndxi
               tem1(k) = constituents(itemp, k)
            enddo
            ierr = nf90_put_var(imapfile, id_tem1(iid), tem1, (/ 1, itim /), (/ ndxndxi, 1 /))
         endif
      endif

!     tracers
      if (jamapconst > 0 .and. ITRA1 > 0) then ! Note: numtracers is only counting tracer boundaries. SPvdP: now also includes tracers with initial conditions only
         allocate(dum(NdxNdxi))

         do j=ITRA1,ITRAN
            if ( kmx>0 ) then
!              3D
               do kk=1,ndxndxi
                  work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                  call getkbotktop(kk,kb,kt)
                  call getlayerindices(kk, nlayb, nrlay)
                  do k = kb,kt
                     work1(k-kb+nlayb, kk) = constituents(j,k)
                  enddo
               enddo
               ierr = nf90_put_var(imapfile, id_const(iid,j), work1(1:kmx,1:ndxndxi), (/ 1, 1, itim /), (/ kmx, ndxndxi, 1 /))
               !   if ( ierr.ne.0 ) exit  ! probably newly added tracer in the GUI
            else
               do kk=1,NdxNdxi
                  dum(kk) = constituents(j,kk)
               enddo
               ierr = nf90_put_var(imapfile, id_const(iid,j), dum, (/ 1, itim /), (/ NdxNdxi, 1 /) )
            endif
         enddo

         if ( allocated(dum) ) deallocate(dum)
      endif

      ! water quality bottom variables outputs
      if (numwqbots > 0) then
         allocate(dum(NdxNdxi))
         do j=1,numwqbots
            do kk=1,NdxNdxi
               call getkbotktop(kk,kb,kt)
               dum(kk) = wqbot(j,kb)
            enddo
            ierr = nf90_put_var(imapfile, id_wqb(iid,j), dum, (/ 1, itim /), (/ NdxNdxi, 1 /) )
         enddo
         if (wqbot3D_output == 1) then
            do j=1,numwqbots
               do kk=1,ndxndxi
                  work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                  call getkbotktop(kk,kb,kt)
                  call getlayerindices(kk, nlayb, nrlay)
                  do k = kb,kt
                     work1(k-kb+nlayb, kk) = wqbot(j,k)
                  enddo
               enddo
               ierr = nf90_put_var(imapfile, id_wqb3d(iid,j), work1(1:kmx,1:ndxndxi), (/ 1, 1, itim /), (/ kmx, ndxndxi, 1 /))
            enddo
         endif
         if ( allocated(dum) ) deallocate(dum)
      endif

      ! WAQ extra outputs
      if (jawaqproc > 0) then
         do j=1,noout_map
            if (outvar(j)>0)then
               work1 = DMISS ! For proper fill values in z-model runs.
               if ( kmx>0 ) then
!                 3D
                  do kk=1,ndxndxi
                     work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                     call getkbotktop(kk,kb,kt)
                     call getlayerindices(kk, nlayb, nrlay)
                     do k = kb,kt
                        work1(k-kb+nlayb, kk) = waqoutputs(j,k-kbx+1)
                     enddo
                  enddo
                  ierr = nf90_put_var(imapfile, id_waq(iid,j), work1(1:kmx,1:ndxndxi), (/ 1, 1, itim /), (/ kmx, ndxndxi, 1 /))
               else
                  call realloc(dum,NdxNdxi, keepExisting=.false.)
                  do kk=1,NdxNdxi
                     dum(kk) = waqoutputs(j,kk)
                  enddo
                  ierr = nf90_put_var(imapfile, id_waq(iid,j), dum, (/ 1, itim /), (/ NdxNdxi, 1 /) )
                  if (allocated(dum)) deallocate(dum)
               endif
            endif
         enddo
         do j=1,noout_statt
            jj = noout_user + j
            if (outvar(jj)>0)then
               work1 = DMISS ! For proper fill values in z-model runs.
               if ( kmx>0 ) then
!                 3D
                  do kk=1,ndxndxi
                     work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                     call getkbotktop(kk,kb,kt)
                     call getlayerindices(kk, nlayb, nrlay)
                     do k = kb,kt
                        work1(k-kb+nlayb, kk) = waqoutputs(jj,k-kbx+1)
                     enddo
                  enddo
                  ierr = nf90_put_var(imapfile, id_wqst(iid,j), work1(1:kmx,1:ndxndxi), (/ 1, 1, itim /), (/ kmx, ndxndxi, 1 /))
               else
                  call realloc(dum,NdxNdxi, keepExisting=.false.)
                  do kk=1,NdxNdxi
                     dum(kk) = waqoutputs(jj,kk)
                  enddo
                  ierr = nf90_put_var(imapfile, id_wqst(iid,j), dum, (/ 1, itim /), (/ NdxNdxi, 1 /) )
                  if (allocated(dum)) deallocate(dum)
               endif
            endif
         enddo
         if (comparereal(tim, ti_mape, eps10) == 0) then
            do j=1,noout_state
               jj = noout_user + noout_statt + j
               if (outvar(jj)>0)then
                  work1 = DMISS ! For proper fill values in z-model runs.
                  if ( kmx>0 ) then
!                    3D
                     do kk=1,ndxndxi
                        work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                        call getkbotktop(kk,kb,kt)
                        call getlayerindices(kk, nlayb, nrlay)
                        do k = kb,kt
                           work1(k-kb+nlayb, kk) = waqoutputs(jj,k-kbx+1)
                        enddo
                     enddo
                     ierr = nf90_put_var(imapfile, id_wqse(iid,j), work1(1:kmx,1:ndxndxi), (/ 1, 1 /), (/ kmx, ndxndxi, 1 /))
                  else
                     call realloc(dum,NdxNdxi, keepExisting=.false.)
                     do kk=1,NdxNdxi
                        dum(kk) = waqoutputs(jj,kk)
                     enddo
                     ierr = nf90_put_var(imapfile, id_wqse(iid,j), dum, (/ 1 /), (/ NdxNdxi, 1 /) )
                     if (allocated(dum)) deallocate(dum)
                  endif
               endif
            enddo
         endif
      endif

      if (jased>0 .and. stm_included) then
         if (stmpar%lsedsus > 0) then
            if (kmx > 0) then
               do kk = 1, ndxndxi
                  call getkbotktop(kk, kb, kt)
                  ierr = nf90_put_var(imapfile, id_ws(iid), mtd%ws(kb:kt,:), (/ 1, kk , 1 , itim /), (/ kt-kb+1, 1 , stmpar%lsedsus , 1 /))
               enddo
            else
               ierr = nf90_put_var(imapfile, id_ws(iid),mtd%ws, (/ 1 , 1 , itim /), (/ ndxndxi , stmpar%lsedsus , 1 /))
            endif
            !
            ! equilibrium concentration, 2D only
            if (kmx == 0) then
               ierr = nf90_put_var(imapfile, id_rsedeq(iid), sedtra%rsedeq(1:ndxndxi, :), (/ 1 , 1 , itim /), (/ ndxndxi , stmpar%lsedsus , 1 /))
            endif

            if (stmpar%morpar%moroutput%sourcesink) then
               ierr = nf90_put_var(imapfile, id_sourse(iid) , sedtra%sourse(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedsus, 1 /))
               ierr = nf90_put_var(imapfile, id_sinkse(iid) , sedtra%sinkse(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedsus, 1 /))
            endif

            if (stmpar%morpar%moroutput%suvcor) then
               ierr = nf90_put_var(imapfile, id_scrn(iid) , sedtra%e_scrn(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedsus, 1 /))
               !ierr = nf90_put_var(imapfile, id_scrt(iid) , sedtra%e_scrt(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedsus, 1 /))
            endif

            if (stmpar%morpar%moroutput%aks) then
               ierr = nf90_put_var(imapfile, id_aks(iid) , sedtra%aks(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedsus, 1 /))
               ierr = nf90_put_var(imapfile, id_rca(iid) , sedtra%rca(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedsus, 1 /))
            endif
            !
            ! Suspended fractions
            call realloc(dum,NdxNdxi, keepExisting=.false.)
            do j=ISED1,ISEDN
               if ( kmx>0 ) then
     !            3D
                  do kk=1,ndxndxi
                     call getkbotktop(kk,kb,kt)
                     do k = kb,kt
                        ! TODO: UNST-976, incorrect for Z-layers:
                        work1(k-kb+1,kk) = constituents(j,k)
                     enddo
                  enddo
                  ierr = nf90_put_var(imapfile, id_const(iid,j), work1(1:kmx,1:ndxndxi), (/ 1, 1, itim /), (/ kmx, ndxndxi, 1 /))
               else
                  do kk=1,NdxNdxi
                     dum(kk) = constituents(j,kk)
                  enddo
                  ierr = nf90_put_var(imapfile, id_const(iid,j), dum, (/ 1, itim /), (/ NdxNdxi, 1 /) )
               endif
            enddo
            if ( allocated(dum) ) deallocate(dum)
         endif

         if (stmpar%morpar%moroutput%dzduuvv) then ! bedslope
            ierr = nf90_put_var(imapfile, id_dzdn(iid), sedtra%e_dzdn, (/ 1, itim /), (/ lnxi , 1 /))
            ierr = nf90_put_var(imapfile, id_dzdt(iid), sedtra%e_dzdt, (/ 1, itim /), (/ lnxi , 1 /))
         endif

         if (stmpar%morpar%moroutput%umod) then
            ierr = nf90_put_var(imapfile, id_umod(iid), sedtra%umod, (/ 1, itim /), (/ ndxndxi, 1 /))
         endif

         if (stmpar%morpar%moroutput%zumod) then
            ierr = nf90_put_var(imapfile, id_zumod(iid), sedtra%zumod, (/ 1, itim /), (/ ndxndxi, 1 /))
         endif

         if (stmpar%morpar%moroutput%ustar) then
            ierr = nf90_put_var(imapfile, id_ustar(iid), sqrt(sedtra%ust2), (/ 1, itim /), (/ ndxndxi, 1 /))
         endif

         if (stmpar%morpar%moroutput%sbcuv) then
            do l = 1, stmpar%lsedtot
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(l)
               case (2)
                  rhol = stmpar%sedpar%rhosol(l)
               end select
               sedtra%sbcx(:,l) = sedtra%sbcx(:,l)/rhol
               sedtra%sbcy(:,l) = sedtra%sbcy(:,l)/rhol
            enddo
            ierr = nf90_put_var(imapfile, id_sbcx(iid) , sedtra%sbcx(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))
            ierr = nf90_put_var(imapfile, id_sbcy(iid) , sedtra%sbcy(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))
         endif

         if (stmpar%morpar%moroutput%sbwuv) then
            do l = 1, stmpar%lsedtot
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(l)
               case (2)
                  rhol = stmpar%sedpar%rhosol(l)
               end select
               sedtra%sbwx(:,l) = sedtra%sbwx(:,l)/rhol
               sedtra%sbwy(:,l) = sedtra%sbwy(:,l)/rhol
            enddo
            ierr = nf90_put_var(imapfile, id_sbwx(iid) , sedtra%sbwx(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))
            ierr = nf90_put_var(imapfile, id_sbwy(iid) , sedtra%sbwy(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))
         endif

         if (stmpar%morpar%moroutput%sswuv) then
            do l = 1, stmpar%lsedtot
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(l)
               case (2)
                  rhol = stmpar%sedpar%rhosol(l)
               end select
               sedtra%sswx(:,l) = sedtra%sswx(:,l)/rhol
               sedtra%sswy(:,l) = sedtra%sswy(:,l)/rhol
            enddo
            ierr = nf90_put_var(imapfile, id_sswx(iid) , sedtra%sswx(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))
            ierr = nf90_put_var(imapfile, id_sswy(iid) , sedtra%sswy(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))
         endif

         if (stmpar%morpar%moroutput%sscuv) then
            call realloc(toutputx, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
            call realloc(toutputy, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
            do l = 1, stmpar%lsedsus
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(sedtot2sedsus(sedtot2sedsus(l)))
               case (2)
                  rhol = stmpar%sedpar%rhosol(sedtot2sedsus(sedtot2sedsus(l)))
               end select
               toutputx(:,l) = sedtra%sscx(:,sedtot2sedsus(l))/rhol         ! mapping necessary because dim(sscx)=lsedtot
               toutputy(:,l) = sedtra%sscy(:,sedtot2sedsus(l))/rhol
            enddo
            ierr = nf90_put_var(imapfile, id_sscx(iid) , toutputx(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedsus, 1 /))
            ierr = nf90_put_var(imapfile, id_sscy(iid) , toutputy(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedsus, 1 /))
         endif

         ! Get cell centre transport values, removed from fm_erosed and fm_bott3d, and calculated here and stored in sscx, sscy, sbcx, sbcy, sbwx, sbwy, sswx, sswy
         call reconstructsedtransports()

         if (stmpar%morpar%moroutput%sbcuv) then
            do l = 1, stmpar%lsedtot
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(l)
               case (2)
                  rhol = stmpar%sedpar%rhosol(l)
               end select
               sedtra%sbcx(:,l) = sedtra%sbcx(:,l)/rhol
               sedtra%sbcy(:,l) = sedtra%sbcy(:,l)/rhol
            enddo
            ierr = nf90_put_var(imapfile, id_sbcx_reconstructed(iid) , sedtra%sbcx(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))
            ierr = nf90_put_var(imapfile, id_sbcy_reconstructed(iid) , sedtra%sbcy(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))
         endif

         if (stmpar%morpar%moroutput%sbwuv) then
            do l = 1, stmpar%lsedtot
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(l)
               case (2)
                  rhol = stmpar%sedpar%rhosol(l)
               end select
               sedtra%sbwx(:,l) = sedtra%sbwx(:,l)/rhol
               sedtra%sbwy(:,l) = sedtra%sbwy(:,l)/rhol
            enddo
            ierr = nf90_put_var(imapfile, id_sbwx_reconstructed(iid) , sedtra%sbwx(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))
            ierr = nf90_put_var(imapfile, id_sbwy_reconstructed(iid) , sedtra%sbwy(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))
         endif

         if (stmpar%morpar%moroutput%sswuv) then
            do l = 1, stmpar%lsedtot
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(l)
               case (2)
                  rhol = stmpar%sedpar%rhosol(l)
               end select
               sedtra%sswx(:,l) = sedtra%sswx(:,l)/rhol
               sedtra%sswy(:,l) = sedtra%sswy(:,l)/rhol
            enddo
            ierr = nf90_put_var(imapfile, id_sswx_reconstructed(iid) , sedtra%sswx(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))
            ierr = nf90_put_var(imapfile, id_sswy_reconstructed(iid) , sedtra%sswy(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))
         endif

         if (stmpar%morpar%moroutput%sscuv) then
            call realloc(toutputx, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
            call realloc(toutputy, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
            do l = 1, stmpar%lsedsus
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(sedtot2sedsus(sedtot2sedsus(l)))
               case (2)
                  rhol = stmpar%sedpar%rhosol(sedtot2sedsus(sedtot2sedsus(l)))
               end select
               toutputx(:,l) = sedtra%sscx(:,sedtot2sedsus(l))/rhol         ! mapping necessary because dim(sscx)=lsedtot
               toutputy(:,l) = sedtra%sscy(:,sedtot2sedsus(l))/rhol
            enddo
            ierr = nf90_put_var(imapfile, id_sscx_reconstructed(iid) , toutputx(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedsus, 1 /))
            ierr = nf90_put_var(imapfile, id_sscy_reconstructed(iid) , toutputy(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedsus, 1 /))
         endif

         do l = 1, stmpar%lsedtot
            call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
            call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
            select case(stmpar%morpar%moroutput%transptype)
            case (0)
               rhol = 1d0
            case (1)
               rhol = stmpar%sedpar%cdryb(l)
            case (2)
               rhol = stmpar%sedpar%rhosol(l)
            end select
            toutputx(:,l) = sedtra%sxtot(:,l)/rhol
            toutputy(:,l) = sedtra%sytot(:,l)/rhol
         enddo
         ierr = nf90_put_var(imapfile, id_sxtot(iid), toutputx(1:ndxndxi, :), (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))
         ierr = nf90_put_var(imapfile, id_sytot(iid), toutputy(1:ndxndxi, :), (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))

         if (stmpar%morpar%bedupd) then
            ierr = nf90_put_var(imapfile, id_morbl(iid), bl(1:ndxndxi), (/ 1, itim /), (/ ndxndxi, 1 /))
         endif

         select case (stmpar%morlyr%settings%iunderlyr)
         case (1)
            ierr = nf90_put_var(imapfile, id_bodsed(iid), stmpar%morlyr%state%bodsed(:, 1:ndxndxi), (/ 1, 1, itim /), (/ stmpar%lsedtot, ndxndxi, 1 /))
            ierr = nf90_put_var(imapfile, id_dpsed(iid), stmpar%morlyr%state%dpsed(1:ndxndxi), (/ 1, itim /), (/ ndxndxi, 1 /))
         case (2)
            !
            ! Calculate values for lyrfrac and porosity
            !
            ! lyrfrac
            if (.not. allocated(frac) ) allocate( frac(stmpar%lsedtot,1:stmpar%morlyr%settings%nlyr,1:ndx  ) )
            frac = -999d0
            if (stmpar%morlyr%settings%iporosity==0) then
               dens => stmpar%sedpar%cdryb
            else
               dens => stmpar%sedpar%rhosol
            endif
            do k = 1, stmpar%morlyr%settings%nlyr
               do nm = 1, ndxndxi
                  if (stmpar%morlyr%state%thlyr(k,nm)>0.0_fp) then
                     do l = 1, stmpar%lsedtot
                          frac(l, k, nm) = stmpar%morlyr%state%msed(l, k, nm)/(dens(l)*stmpar%morlyr%state%svfrac(k, nm) * &
                                           stmpar%morlyr%state%thlyr(k, nm))
                     enddo
                  else
                     frac(:, k, nm) = 0d0
                  endif
               enddo
            enddo
            !
            if (stmpar%morlyr%settings%iporosity>0) then
               if (.not. allocated(poros) ) allocate( poros(1:stmpar%morlyr%settings%nlyr, 1:ndx ) )
               poros = 1d0-stmpar%morlyr%state%svfrac
            endif
            !
            ! Avoid stack overflow issues with large models
            do l = 1, stmpar%lsedtot
               ierr = nf90_put_var(imapfile, id_msed(iid), stmpar%morlyr%state%msed(l,:,1:ndxndxi), (/ l, 1, 1, itim /), (/ 1, stmpar%morlyr%settings%nlyr, ndxndxi, 1 /))
               ierr = nf90_put_var(imapfile, id_lyrfrac(iid), frac(l,:,1:ndxndxi), (/ l, 1, 1, itim /), (/ 1, stmpar%morlyr%settings%nlyr, ndxndxi, 1 /))
            enddo
            ierr = nf90_put_var(imapfile, id_thlyr(iid), stmpar%morlyr%state%thlyr(:,1:ndxndxi), (/ 1, 1, itim /), (/ stmpar%morlyr%settings%nlyr, ndxndxi, 1 /))
            if (stmpar%morlyr%settings%iporosity>0) then
               ierr = nf90_put_var(imapfile, id_poros(iid), poros(:,1:ndxndxi), (/ 1, 1, itim /), (/ stmpar%morlyr%settings%nlyr, ndxndxi, 1 /))
            endif
         end select

         if (stmpar%morpar%moroutput%taurat) then
            ierr = nf90_put_var(imapfile, id_taurat(iid), sedtra%taurat(1:ndxndxi,:), (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))
         endif
         if (stmpar%morpar%moroutput%dm) then
            ierr = nf90_put_var(imapfile, id_dm(iid), sedtra%dm(1:ndxndxi), (/ 1, itim /), (/ ndxndxi, 1 /))

         endif
         if (stmpar%morpar%moroutput%dg) then
            ierr = nf90_put_var(imapfile, id_dg(iid), sedtra%dg(1:ndxndxi), (/ 1, itim /), (/ ndxndxi, 1 /))
         endif
         if (stmpar%morpar%moroutput%dgsd) then
            ierr = nf90_put_var(imapfile, id_dgsd(iid), sedtra%dgsd(1:ndxndxi), (/ 1, itim /), (/ ndxndxi, 1 /))
         endif
         if (stmpar%morpar%moroutput%percentiles) then    ! JRE to do: check with Arthur
            call realloc(dum,ndxndxi, keepExisting=.false.)
            do l = 1, stmpar%morpar%nxx
               do kk=1,NdxNdxi
                  dum(kk) = sedtra%dxx(kk, l)
               enddo
               ierr = nf90_put_var(imapfile, id_dxx(l,iid), dum, (/ 1, itim /), (/ ndxndxi, 1 /))
            enddo
         endif
         if (stmpar%morpar%moroutput%frac) then
            ierr = nf90_put_var(imapfile, id_frac(iid), sedtra%frac(1:ndxndxi, :), (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))
         endif
         if (stmpar%morpar%moroutput%mudfrac) then
            ierr = nf90_put_var(imapfile, id_mudfrac(iid), sedtra%mudfrac(1:ndxndxi), (/ 1, itim /), (/ ndxndxi,  1 /))
         endif
         if (stmpar%morpar%moroutput%sandfrac) then
            ierr = nf90_put_var(imapfile, id_sandfrac(iid), sedtra%sandfrac(1:ndxndxi), (/ 1, itim /), (/ ndxndxi,  1 /))
         endif
         if (stmpar%morpar%moroutput%fixfac) then
            ierr = nf90_put_var(imapfile, id_fixfac(iid), sedtra%fixfac(1:ndxndxi,:), (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))
         endif
         if (stmpar%morpar%moroutput%hidexp) then
            ierr = nf90_put_var(imapfile, id_hidexp(iid), sedtra%hidexp(1:ndxndxi,:), (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedtot, 1 /))
         endif
         ! Fluff layers
         if (stmpar%morpar%flufflyr%iflufflyr>0 .and. stmpar%lsedsus>0) then
            do l = 1, stmpar%lsedsus
               call realloc(toutput, ndx, keepExisting=.false., fill=-999d0)
               toutput = stmpar%morpar%flufflyr%mfluff(l,1:ndx)
               ierr = nf90_put_var(imapfile, id_mfluff(iid), toutput(1:ndxndxi), (/ 1, l, itim /), (/ ndxndxi, 1, 1 /))
            enddo
         endif
      endif ! stm

      ! Bedform pars
      if (bfmpar%lfbedfrmout) then
         if (bfmpar%lfbedfrm) then
            ierr = nf90_put_var(imapfile, id_duneheight(iid), bfmpar%duneheight(1:ndxndxi), (/ 1, itim /), (/ ndxndxi, 1 /))
            ierr = nf90_put_var(imapfile, id_dunelength(iid), bfmpar%dunelength(1:ndxndxi), (/ 1, itim /), (/ ndxndxi, 1 /))
         endif
         if (bfmpar%lfbedfrmrou) then
            ierr = nf90_put_var(imapfile, id_ksr(iid), bfmpar%rksr(1:ndxndxi), (/ 1, itim /), (/ ndxndxi, 1 /))
            ierr = nf90_put_var(imapfile, id_ksmr(iid), bfmpar%rksmr(1:ndxndxi), (/ 1, itim /), (/ ndxndxi, 1 /))
            ierr = nf90_put_var(imapfile, id_ksd(iid), bfmpar%rksd(1:ndxndxi), (/ 1, itim /), (/ ndxndxi, 1 /))

            do k = 1,ndxndxi
               rks(k) = sqrt(bfmpar%rksr(k)**2 + bfmpar%rksmr(k)**2 + bfmpar%rksd(k)**2)
            enddo
            ierr = nf90_put_var(imapfile, id_ks(iid), rks(1:ndxndxi), (/ 1, itim /), (/ ndxndxi, 1 /))
         endif
      endif
      ! Sediment Herman
      if (jased > 0 .and. .not.stm_included) then
         ierr = nf90_put_var(imapfile, id_sed(iid), sed, (/ 1, 1, itim /), (/ mxgr, ndxndxi, 1 /))
         ierr = nf90_put_var(imapfile, id_ero(iid), grainlay, (/ 1, 1, itim /), (/ mxgr, size(grainlay,2) , 1 /))

         ierr = nf90_put_var(imapfile, id_bl(iid), bl, (/ 1, itim /), (/ ndxndxi , 1 /))
         if (jaceneqtr .ne. 1) then
             ierr = nf90_put_var(imapfile, id_zk(iid), zk, (/ 1, itim /), (/ numk , 1 /))
         endif


      ! TODO: AvD: size(grainlay,2) is always correct (mxn), but we have a problem if jaceneqtr==2 and mxn/=numk,
      ! because then the dimension for ero is set to nNetNode, and coordinate attribute refers to NetNode_x
      ! (both length numk), whereas ero itself is shorter than numk.
      endif

      ! 1D2D boundaries
      if (nbnd1d2d > 0) then
         ierr = nf90_put_var(imapfile, id_1d2d_zeta1d(iid),      zbnd1d2d1,  (/ 1, itim /), (/ nbnd1d2d, 1 /))
         ierr = nf90_put_var(imapfile, id_1d2d_crest_level(iid), zcrest1d2d, (/ 1, itim /), (/ nbnd1d2d, 1 /))
         ierr = nf90_put_var(imapfile, id_1d2d_b_2di(iid),       b_2di,      (/ 1, itim /), (/ nbnd1d2d, 1 /))
         ierr = nf90_put_var(imapfile, id_1d2d_b_2dv(iid),       b_2dv,      (/ 1, itim /), (/ nbnd1d2d, 1 /))
         ierr = nf90_put_var(imapfile, id_1d2d_d_2dv(iid),       d_2dv,      (/ 1, itim /), (/ nbnd1d2d, 1 /))
         ierr = nf90_put_var(imapfile, id_1d2d_q_zeta(iid),      qzeta_1d2d, (/ 1, itim /), (/ nbnd1d2d, 1 /))
         ierr = nf90_put_var(imapfile, id_1d2d_q_lat(iid),       qlat_1d2d,  (/ 1, itim /), (/ nbnd1d2d, 1 /))
         ierr = nf90_put_var(imapfile, id_1d2d_cfl(iid),         cfl,        (/ 1, itim /), (/ nbnd1d2d, 1 /))
         ierr = nf90_put_var(imapfile, id_1d2d_sb(iid),          sb_1d2d,    (/ 1, itim /), (/ nbnd1d2d, 1 /))
         ierr = nf90_put_var(imapfile, id_1d2d_s1_2d(iid),       s1_2d,      (/ 1, itim /), (/ nbnd1d2d, 1 /))
         ierr = nf90_put_var(imapfile, id_1d2d_s0_2d(iid),       s0_2d,      (/ 1, itim /), (/ nbnd1d2d, 1 /))
         ierr = nf90_put_var(imapfile, id_1d2d_flow_cond(iid),   FlowCond,   (/ 1, itim /), (/ nbnd1d2d, 1 /))
      endif

      if ( jatidep > 0 .and. jamaptidep.eq.1 ) then
         if ( jaselfal.eq.0 ) then
            do k=1,Ndx
               workx(k) = tidep(1,k)
            enddo
            ierr = nf90_put_var(imapfile, id_tidep(iid), workx,  (/ 1, itim /), (/ ndxndxi, 1 /))
         else ! write potential without SAL and SAL potential
            do k=1,Ndx
               workx(k) = tidep(1,k) - tidep(2,k)
!               worky(k) = tidep(2,k)
            enddo
            ierr = nf90_put_var(imapfile, id_tidep(iid), workx,  (/ 1, itim /), (/ ndxndxi, 1 /))
!            ierr = nf90_put_var(imapfile, id_salp(iid),  worky,  (/ 1, itim /), (/ ndxndxi, 1 /))
         endif
      endif
      if ( jaselfal.gt.0 .and. jamapselfal.eq.1 ) then
         do k=1,Ndx
            worky(k) = tidep(2,k)
         enddo
         ierr = nf90_put_var(imapfile, id_salp(iid),  worky,  (/ 1, itim /), (/ ndxndxi, 1 /))
      endif

      if ( jaFrcInternalTides2D.gt.0 .and. jamapIntTidesDiss.eq.1 ) then
         ierr = nf90_put_var(imapfile, id_inttidesdiss(iid), DissInternalTidesPerArea,  (/ 1, itim /), (/ ndxndxi, 1 /))
      endif
   endif

   if (jawind > 0 .and. ((jamapwind > 0 .and. jawindstressgiven == 0) .or. (jaseparate_==2))) then
      allocate (windx(ndxndxi), windy(ndxndxi), stat=ierr)
      if (ierr /= 0) call aerr( 'windx/windy', ierr, ndxndxi)
      !windx/y is not set to 0.0 for flownodes without links !
      windx = 0.0d0
      windy = 0.0d0
      do n = 1,ndxndxi
         !
         ! Currently, wx/y is defined on the links
         ! TO DO: EC-module should not be asked for wind components on the links but on the cells
         !
         if (nd(n)%lnx > 0) then
            do i = 1,nd(n)%lnx
               windx(n) = windx(n) + wx(iabs(nd(n)%ln(i)))
               windy(n) = windy(n) + wy(iabs(nd(n)%ln(i)))
            enddo
            windx(n) = windx(n) / nd(n)%lnx
            windy(n) = windy(n) / nd(n)%lnx
         else
            j=1
         endif
      enddo
      ierr = nf90_put_var(imapfile, id_windx  (iid), windx,  (/ 1, itim /), (/ ndxndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_windy  (iid), windy,  (/ 1, itim /), (/ ndxndxi, 1 /))
      deallocate (windx, stat=ierr)
      ierr = nf90_put_var(imapfile, id_windxu  (iid), wx,  (/ 1, itim /), (/ lnx, 1 /))
      ierr = nf90_put_var(imapfile, id_windyu  (iid), wy,  (/ 1, itim /), (/ lnx, 1 /))
   endif

   if (jamapwind > 0 .and. japatm > 0) then
      ierr = nf90_put_var(imapfile, id_patm(iid)  , Patm, (/ 1, itim /), (/ ndxndxi, 1 /))
   endif

   if (ice_mapout) then
      ierr = nf90_put_var(imapfile, id_ice_af(iid) , ice_af, (/ 1, itim /), (/ ndxndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_ice_h(iid)  , ice_h , (/ 1, itim /), (/ ndxndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_ice_p(iid)  , ice_p , (/ 1, itim /), (/ ndxndxi, 1 /))
      if (ja_icecover == ICECOVER_SEMTNER) then
         ierr = nf90_put_var(imapfile, id_ice_t(iid)  , ice_t , (/ 1, itim /), (/ ndxndxi, 1 /))
         ierr = nf90_put_var(imapfile, id_snow_h(iid) , snow_h, (/ 1, itim /), (/ ndxndxi, 1 /))
         ierr = nf90_put_var(imapfile, id_snow_t(iid) , snow_t, (/ 1, itim /), (/ ndxndxi, 1 /))
      endif
   endif

   if (jamapheatflux > 0 .and. jatem > 1) then    ! Heat modelling only
      ierr = nf90_put_var(imapfile, id_tair(iid)  , Tair, (/ 1, itim /), (/ ndxndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_rhum(iid)  , Rhum, (/ 1, itim /), (/ ndxndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_clou(iid)  , Clou, (/ 1, itim /), (/ ndxndxi, 1 /))

       if (jatem == 5) then
          ierr = nf90_put_var(imapfile, id_qsun(iid)  , Qsunmap  , (/ 1, itim /), (/ ndxndxi, 1 /))
          ierr = nf90_put_var(imapfile, id_qeva(iid)  , Qevamap  , (/ 1, itim /), (/ ndxndxi, 1 /))
          ierr = nf90_put_var(imapfile, id_qcon(iid)  , Qconmap  , (/ 1, itim /), (/ ndxndxi, 1 /))
          ierr = nf90_put_var(imapfile, id_qlong(iid) , Qlongmap , (/ 1, itim /), (/ ndxndxi, 1 /))
          ierr = nf90_put_var(imapfile, id_qfreva(iid), Qfrevamap, (/ 1, itim /), (/ ndxndxi, 1 /))
          ierr = nf90_put_var(imapfile, id_qfrcon(iid), Qfrconmap, (/ 1, itim /), (/ ndxndxi, 1 /))
       endif
       ierr = nf90_put_var(imapfile, id_qtot(iid)  , Qtotmap  , (/ 1, itim /), (/ ndxndxi, 1 /))
   endif
   call realloc(numlimdtdbl, ndxndxi, keepExisting=.false.)
   numlimdtdbl = dble(numlimdt) ! To prevent stack overflow. TODO: remove once integer version is available.
   ierr = nf90_put_var(imapfile, id_numlimdt(iid)  , numlimdtdbl, (/ 1, itim /), (/ ndxndxi, 1 /))
   deallocate(numlimdtdbl)

   ! Roughness from trachytopes
   if (jatrt == 1) then
      ierr = nf90_put_var(imapfile, id_cftrt(iid),  cftrt(:,2),   (/ 1, itim /), (/ numl, 1 /))
   endif

   ! Roughness calibration factors
   if (jacali == 1) then
      ierr = nf90_put_var(imapfile, id_cfcl(iid),  cfclval,   (/ 1, itim /), (/ numl, 1 /))
   endif

   ! JRE - XBeach
   if (jawave .eq. 4) then
      ierr = nf90_put_var(imapfile, id_E(iid), E, (/ 1, itim /), (/ ndxndxi, 1 /)) ! direction integrated
      ierr = nf90_put_var(imapfile, id_R(iid), R, (/ 1, itim /), (/ ndxndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_H(iid), H, (/ 1, itim /), (/ ndxndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_urmscc(iid), uorb, (/ 1, itim /), (/ ndxndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_Fxcc(iid), Fx_cc, (/ 1, itim /), (/ ndxndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_Fycc(iid), Fy_cc, (/ 1, itim /), (/ ndxndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_D(iid), D, (/ 1, itim /), (/ ndxndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_DR(iid), DR, (/ 1, itim /), (/ ndxndxi, 1 /))

      ierr = nf90_put_var(imapfile, id_sigmwav(iid), sigmwav, (/ 1, itim /), (/ ndxndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_cwav(iid), cwav, (/ 1, itim /), (/ ndxndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_cgwav(iid), cgwav, (/ 1, itim /), (/ ndxndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_thetamean(iid), 270d0 - thetamean*180d0/pi, (/ 1, itim /), (/ ndxndxi, 1 /))
      !if ( (windmodel .eq. 1) .and. (jawsource .eq. 1) ) then
      !   ierr = nf90_put_var(imapfile, id_SwE(iid), SwE, (/ 1, itim /), (/ ndxndxi, 1 /))
      !   ierr = nf90_put_var(imapfile, id_SwT(iid), SwT, (/ 1, itim /), (/ ndxndxi, 1 /))
      !endif
   endif

!  deallocate
   if ( NUMCONST.gt.0 ) then
      if ( allocated(idum)     ) deallocate(idum)
   endif

   if (jaseparate_==2 .and. javeg > 0) then
      ierr = nf90_put_var(imapfile, id_rnveg(iid), rnveg, (/ 1, itim /), (/ ndxndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_diaveg(iid), diaveg, (/ 1, itim /), (/ ndxndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_veg_stemheight(iid), stemheight, (/ 1, itim /), (/ ndxndxi, 1 /))
   endif

end subroutine unc_write_map_filepointer

end module fm_unc_write_map