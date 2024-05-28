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
   
module m_unc_write_map
   
   use stdlib_kinds, only: dp
   use MessageHandling, only: mess, LEVEL_WARN, LEVEL_ERROR, msgbuf, err_flush
   use precision_basics, only: comparereal, hp
   use timers, only: timon, timstrt, timstop
   
   implicit none
   
   private
   
   public :: unc_write_map, unc_write_map_filepointer_ugrid
   
   integer :: nc_precision    !< Precision to use for NetCDF variables
   integer :: iLocS           !< Either UNC_LOC_S or UNC_LOC_S3D, depending on whether layers are present.
   integer :: iLocU           !< Either UNC_LOC_U or UNC_LOC_U3D, depending on whether layers are present.
   integer :: ndx1d, jmax, nCrs
   
   contains

!> Writes a single snapshot of the unstructured flow net + flow data to a netCDF file.
!! If file exists, it will be overwritten. Therefore, only use this routine
!! for separate snapshots, the automated map file should be filled by calling
!! unc_write_map_filepointer directly instead!
subroutine unc_write_map(filename, iconventions)
   use m_flowparameters, only: jamapbnd
   use unstruc_netcdf, only: t_unc_mapids, UNC_CONV_CFOLD, UNC_CONV_UGRID, unc_create, unc_close, check_error
   use MessageHandling, only: mess, LEVEL_ERROR
   use netcdf, only: nf90_noerr
   use m_unc_write_map_cfold, only: unc_write_map_filepointer
   
   implicit none

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
   use m_flow !< Import all the jamap_XXX flags, all the ja_XX flags, and all the flow model data
   use m_flowtimes, only: handle_extra, ti_map, ti_mape, dts, dtcell, time_map, time_wetground
   use m_flowgeom, only: ndxi, ndx, ndx2d, ntheta, lnx1d, lnx, blup, bl, wu_mor, bl_ave, ba_mor, bare, ba, ln, wcx1, wcx2, wcy1, wcy2
   use m_sferic, only: jsferic
   use network_data, only: zk
   use m_sediment, only: stm_included, stmpar, mxgr, jaceneqtr, sedtra, sedtot2sedsus, mtd, sbcx_raw, sbcy_raw, sbwx_raw, sbwy_raw, &
                         sswx_raw, sswy_raw, avalflux, sed, grainlay
   use m_bedform, only: bfmpar
   use m_flowparameters, only: jatrt, ibedlevtyp
   use m_mass_balance_areas, only: nomba, mbaname, mbadef
   use m_fm_wq_processes, only: numwqbots, wqbotnames, wqbotunits, wqbot3d_output, jawaqproc, noout_map, outputs, noout_statt, &
                                noout_user, noout_state, fp, wqbot, outvar, waqoutputs, kbx
   use m_xbeach_data, only: roller, E, R, D, DR, DF, sxx, sxy, syy, sigmwav, cwav, cgwav, kwav, nwav, l1, ctheta
   use m_transportdata, only: numconst, itra1, itran, const_names, const_units, constituents, isalt, itemp, isedn, ised1
   use m_alloc, only: realloc
   use m_waves, only: hwav, jamapsigwav, twav, phiwav, sxwav, sywav, sbxwav, sbywav, mxwav, mywav, dsurf, dwcap, distot, &
                      uorbwav, uorb, ustokes, vstokes
   use m_CrossSections, only: t_cstype
   use unstruc_channel_flow, only: network
   use m_hydrology_data, only : jadhyd, ActEvap, PotEvap, interceptionmodel, DFM_HYD_NOINTERCEPT, InterceptHs
   use m_subsidence, only: jasubsupl, subsout, subsupl, subsupl_t0
   use Timers, only: timon
   use m_output_config, only: UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_S, UNC_LOC_U, UNC_LOC_W, UNC_LOC_WU, UNC_LOC_CN, UNC_LOC_L
   use m_map_his_precision, only: netcdf_data_type, md_nc_map_precision
   use m_fm_icecover, only: ice_mapout, ice_af, ice_h, ice_p, ice_t, snow_h, snow_t, ja_icecover, ICECOVER_SEMTNER
   use unstruc_netcdf, only: t_unc_mapids, ug_meta_fm, ug_addglobalatts, unc_meta_add_user_defined, unc_add_time_coverage, &
                             unc_nounlimited, unc_def_var_nonspatial, MAX_ID_VAR, unc_def_var_map, unc_put_att, unc_cmode, &
                             unc_put_var_map, write_array_with_dmiss_for_dry_cells_into_netcdf_file, unc_put_var_map_nodes, &
                             write_array_with_dmiss_for_dry_faces_into_netcdf_file, unc_write_flowgeom_filepointer_ugrid, &
                             check_error, linktonode2, unc_append_3dflowgeom_def, unc_append_3dflowgeom_put
   use netcdf, only: nf90_inquire, nf90_def_dim, nf90_unlimited, nf90_double, nf90_put_att, nf90_int, nf90_char, nf90_def_var, &
                     nf90_enddef, nf90_evarsize, nf90_netcdf4, nf90_noerr, nf90_put_var
   use m_debug, only: jawritedebug, debugarr1d, debugarr2d, debugarr3d
   use m_save_ugrid_state, only: mesh1dname

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
   double precision                                    :: rhol, mortime, wavfac
   double precision                                    :: moravg, dmorft, dmorfs, rhodt
   double precision                                    :: um, ux, uy
   double precision, dimension(:,:), allocatable       :: poros, toutputx, toutputy, sxtotori, sytotori
   double precision, dimension(:,:,:), allocatable     :: frac

   double precision, dimension(:), allocatable         :: numlimdtdbl
   double precision, dimension(:), allocatable         :: work1d
   double precision                                    :: vicc, dicc

   double precision, dimension(:), pointer             :: dens

   integer :: i, j, jj, itim, n, LL, L, Lb, Lt, k, k1, k2
   integer :: kk, kb, kt, kkk, found, iloc
   integer :: nlayb, nrlay
   integer :: Ltx, nlaybL, nrlayLx
   integer :: isrc, kbot_, ktop_, nk, nkbot, nktop
   double precision, dimension(:), allocatable :: windx, windy, toutput, rks, wa
   double precision :: zwu0
   character( len = 4 ) :: str

   type(t_CSType), pointer                       :: pCS
   double precision, dimension(:,:), allocatable :: work1d_z, work1d_n
   double precision, dimension(:,:,:), allocatable :: work3d, work3d2
   
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
      
      ! Define all the variables and dimensions
      call unc_write_map_def(mapids, jabndnd_)
      
      ! Write to all the time-independent variables in a new map file
      call unc_write_map_write_time_independent(mapids, jabndnd_)

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

   if (allocated(work1d)) then 
      deallocate(work1d)
   endif

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
            if (allocated(work1d_z)) then 
               deallocate(work1d_z)
            endif
            if (allocated(work1d_n)) then
               deallocate(work1d_n)
            endif
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
            ierr = unc_put_var_map_nodes(mapids%ncid, mapids%id_tsp, mapids%id_ero(:,j), grainlay(j,:), jabndnd_)
         enddo
         ierr = unc_put_var_map_nodes(mapids%ncid, mapids%id_tsp, mapids%id_zk , zk, jabndnd_)
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
            if (allocated(wa)) then 
               deallocate(wa, stat = ierr)
            endif
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
           if (allocated(wa)) then 
              deallocate(wa, stat = ierr)
           endif
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

!> Define all the variables and dimensions in a new map file
subroutine unc_write_map_def(mapids, jabndnd_)
   use unstruc_netcdf, only: t_unc_mapids
   use m_flowtimes, only: handle_extra, ti_map, ti_maps, ti_mape, tudunitstr
   use unstruc_netcdf, only: ug_addglobalatts, unc_meta_add_user_defined, unc_write_flowgeom_filepointer_ugrid, &
                             unc_add_time_coverage, ug_meta_fm, unc_nounlimited, unc_def_var_nonspatial, unc_def_var_map, &
                             unc_put_att, MAX_ID_VAR, unc_cmode, check_error
   use netcdf, only: nf90_def_dim, nf90_unlimited, nf90_double, nf90_put_att, nf90_int, nf90_char, nf90_def_var, nf90_enddef, &
                     nf90_evarsize, nf90_netcdf4, nf90_noerr, nf90_strerror
   use m_flow
   use m_output_config, only: UNC_LOC_S, UNC_LOC_S3D, UNC_LOC_W, UNC_LOC_U3D, UNC_LOC_U, UNC_LOC_WU, UNC_LOC_CN, UNC_LOC_L
   use m_flowgeom, only: ndx2d, ntheta, ndxi, lnx1d
   use io_ugrid, only: mdim_two
   use m_save_ugrid_state, only: mesh1dname, mesh2dname
   use fm_statistical_output, only: model_is_3D
   use m_sferic, only: jsferic
   use m_sediment, only: stm_included, stmpar, mxgr, jaceneqtr
   use m_transport, only: itra1, numconst, itran, const_names, const_units
   use m_fm_wq_processes, only: numwqbots, wqbotnames, wqbotunits, wqbot3d_output, jawaqproc, noout_map, outputs, &
                                noout_statt, noout_state, noout_user
   use m_mass_balance_areas, only: nomba, mbaname, mbadef
   use m_alloc, only: realloc
   use m_fm_icecover, only: ice_mapout, ja_icecover, ICECOVER_SEMTNER
   use m_subsidence, only: jasubsupl, subsupl
   use unstruc_channel_flow, only: network
   use m_CrossSections, only: t_cstype
   use m_bedform, only: bfmpar
   use m_waves, only: hwav, jamapsigwav, twav, phiwav, sxwav, sywav, sbxwav, sbywav, mxwav, mywav, dsurf, dwcap, distot, &
                      uorbwav, uorb, ustokes, vstokes
   use m_xbeach_data, only: roller
   use string_module, only: replace_multiple_spaces_by_single_spaces, replace_char

   type(t_unc_mapids), intent(inout) :: mapids    !< Set of file and variable ids for this map-type file.
   integer,            intent(in   ) :: jabndnd_  !< Flag specifying whether boundary nodes are to be written.
   
   integer                               :: ierr, id_twodim, j, jj, iloc, L, k, i
   character(len=255)                    :: tmpstr
   integer, dimension(:), allocatable    :: flag_val
   character(len=10000)                  :: flag_mean
   character(15)                         :: transpunit
   character(3)                          :: sednr     !< string representation of sediment fraction number
   character(256)                        :: varname   !< name of netCDF variable
   character(1024)                       :: longname  !< long, descriptive name of netCDF variable content
   character(16)                         :: dxname
   character(64)                         :: dxdescr
   type(t_CSType), pointer, dimension(:) :: pCSs
   character(4)                          :: str
      
   if (timon) then
      call timstrt( "unc_write_flowgeom_filepointer_ugrid INIT", handle_extra(71))
   end if

   ierr = ug_addglobalatts(mapids%ncid, ug_meta_fm)
   ierr = unc_meta_add_user_defined(mapids%ncid)
   call unc_write_flowgeom_filepointer_ugrid(mapids%ncid, mapids%id_tsp, jabndnd_)
   ierr = unc_add_time_coverage(mapids%ncid, ti_maps, ti_mape, ti_map)

   if (timon) then
      call timstop(handle_extra(71))
   end if

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
   if (model_is_3D() .and. jafullgridoutput == 1) then
      ! Face-centred z-coordinates:
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowelemzcc, nc_precision, UNC_LOC_S3D, 'flowelem_zcc', 'altitude', 'Vertical coordinate of layer centres at pressure points'   , 'm' , jabndnd=jabndnd_)
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowelemzw , nc_precision, UNC_LOC_W  , 'flowelem_zw' , 'altitude', 'Vertical coordinate of layer interfaces at pressure points', 'm' , jabndnd=jabndnd_)

      if (ndx2d > 0) then ! Borrow the "2-dimension" from the already defined mesh (either 2d or 1d, does not matter)
         id_twodim = mapids%id_tsp%meshids2d%dimids(mdim_two)
      else
         id_twodim = mapids%id_tsp%meshids1d%dimids(mdim_two)
      end if

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
   end if

   ! Water levels
   if (jamaps1 > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s1, nc_precision, UNC_LOC_S, 's1',         'sea_surface_height',                'Water level', 'm', jabndnd=jabndnd_)
   end if
   if (jamaps0 > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s0, nc_precision, UNC_LOC_S, 's0', 'sea_surface_height', 'Water level on previous timestep', 'm', jabndnd=jabndnd_)
   end if

   ! Influx
   if (jamapqin > 0 .and. jaqin > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qin, nc_precision, UNC_LOC_S, 'qin', '', 'Sum of all water influx', 'm3 s-1', jabndnd=jabndnd_)
   end if

   if (jamapFlowAnalysis > 0) then
      ! Flow analysis
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_negdpt,       nc_precision, UNC_LOC_S, 'negdpt',         '', 'Number of times negative depth was calculated', '1', cell_method = 'point', jabndnd=jabndnd_)
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_negdpt_cum,   nc_precision, UNC_LOC_S, 'negdpt_cum',     '', 'Cumulative number of times negative depth was calculated', '1', cell_method = 'point', jabndnd=jabndnd_)
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_noiter,       nc_precision, UNC_LOC_S, 'noiter',         '', 'Number of times no nonlinear convergence was caused', '1', cell_method = 'point', jabndnd=jabndnd_)
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_noiter_cum,   nc_precision, UNC_LOC_S, 'noiter_cum',     '', 'Cumulative number of times no nonlinear convergence was caused', '1', cell_method = 'point', jabndnd=jabndnd_)
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_limtstep,     nc_precision, UNC_LOC_S, 'limtstep',       '', 'Number of times a node was limiting for the computational time step', '1', cell_method = 'point', jabndnd=jabndnd_)
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_limtstep_cum, nc_precision, UNC_LOC_S, 'limtstep_cum',   '', 'Cumulative number of times a node was limiting for the computational time step', '1', cell_method = 'point', jabndnd=jabndnd_)
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_courant,      nc_precision, UNC_LOC_S, 'courant',     '', 'Courant number', '1', cell_method = 'point', jabndnd=jabndnd_)
   end if

   ! Evaporation
   if (jamapevap > 0) then
      if (jadhyd == 1) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_potevap, nc_precision, UNC_LOC_S, 'potevap', 'water_potential_evaporation_flux', 'Potential evaporation rate at pressure points', 'm s-1', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_actevap, nc_precision, UNC_LOC_S, 'actevap', 'lwe_water_evaporation_rate', 'Actual evaporation rate at pressure points', 'm s-1', jabndnd=jabndnd_) ! Intentionally did not use standard_name='water_potential_evaporation_flux', because that one requires other units: kg m-2 s-1.
      end if
      if (jaevap == 1) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_evap, nc_precision, UNC_LOC_S, 'prescrevap', '', 'Prescribed evaporation rate at pressure points', 'm s-1', jabndnd=jabndnd_)
      end if
   end if

   ! Volumes
   if (jamapvol1 > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vol1, nc_precision, iLocS, 'vol1',         '',                'volume of water in grid cell', 'm3', jabndnd=jabndnd_)
   end if

   ! Calculated time step per cell based on CFL number
   if (jamapdtcell > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dtcell, nc_precision, iLocS,  'dtcell', '', 'Time step per cell based on CFL', 's', jabndnd=jabndnd_)
   end if

   ! Water depths
   if (jamaphs > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hs, nc_precision, UNC_LOC_S, 'waterdepth', 'sea_floor_depth_below_sea_surface', 'Water depth at pressure points', 'm', jabndnd=jabndnd_)
   end if

   if (jamaphu > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hu, nc_precision, UNC_LOC_U, 'hu', 'sea_floor_depth_below_sea_surface', 'water depth at velocity points', 'm', jabndnd=jabndnd_)
   end if

   ! Velocities
   if (jamapau > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_au, nc_precision, iLocU, 'au',         '',                'normal flow area between two neighbouring grid cells', 'm2', jabndnd=jabndnd_)
   end if

   if (jamapu1 > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_u1, nc_precision, iLocU, 'u1', '', 'Velocity at velocity point, n-component', 'm s-1', jabndnd=jabndnd_)
      ierr = unc_put_att(mapids%ncid, mapids%id_u1, 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
   end if
   if (jamapu0 > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_u0, nc_precision, iLocU, 'u0', '', 'Velocity at velocity point at previous time step, n-component', 'm s-1', jabndnd=jabndnd_)
      ierr = unc_put_att(mapids%ncid, mapids%id_u0, 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
   end if
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
         end if
      end if
      if (model_is_3D()) then
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
            end if
         else
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxa, nc_precision, UNC_LOC_S, 'ucxa', 'eastward_sea_water_velocity', 'Flow element center depth-averaged velocity, x-component', 'm s-1', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucya, nc_precision, UNC_LOC_S, 'ucya', 'northward_sea_water_velocity', 'Flow element center depth-averaged velocity, y-component', 'm s-1', jabndnd=jabndnd_)
         end if
      end if
   end if
      
   if (jamapucmag > 0) then
      if (jaeulervel==1 .and. jawave>0 .and. .not. flowWithoutWaves) then ! TODO: AvD:refactor such that yes<->no Eulerian velocities are in parameters below:
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmag, nc_precision, iLocS, 'ucmag', 'sea_water_eulerian_speed', 'Flow element center eulerian velocity magnitude', 'm s-1', jabndnd=jabndnd_)
      else
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmag, nc_precision, iLocS, 'ucmag', 'sea_water_speed', 'Flow element center velocity magnitude', 'm s-1', jabndnd=jabndnd_)
      end if
      if (model_is_3D()) then
         if (jaeulervel==1 .and. jawave>0) then ! TODO: AvD:refactor such that yes<->no Eulerian velocities are in parameters below:
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmaga, nc_precision, UNC_LOC_S, 'ucmaga', 'sea_water_speed', 'Flow element center depth-averaged GLM velocity magnitude', 'm s-1', jabndnd=jabndnd_)  ! depth-averaged magnitude has no stokes drift
         else
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmaga, nc_precision, UNC_LOC_S, 'ucmaga', 'sea_water_speed', 'Flow element center depth-averaged velocity magnitude', 'm s-1', jabndnd=jabndnd_)
         end if
      end if
   end if
      
   if (jamapucqvec > 0) then
      if (jaeulervel==1 .and. jawave>0 .and. .not. flowWithoutWaves) then ! TODO: AvD:refactor such that yes<->no Eulerian velocities are in parameters below:
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxq, nc_precision, iLocS, 'ucxq', 'ucxq_eulerian_velocity', 'Flow element center eulerian velocity vector based on discharge, x-component', 'm s-1', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucyq, nc_precision, iLocS, 'ucyq', 'ucyq_eulerian_velocity', 'Flow element center eulerian velocity vector based on discharge, y-component', 'm s-1', jabndnd=jabndnd_)
      else
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxq, nc_precision, iLocS, 'ucxq', 'ucxq_velocity', 'Flow element center velocity vector based on discharge, x-component', 'm s-1', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucyq, nc_precision, iLocS, 'ucyq', 'ucyq_velocity', 'Flow element center velocity vector based on discharge, y-component', 'm s-1', jabndnd=jabndnd_)
      end if
   end if
      
   if (model_is_3D()) then
      if (jamapww1 > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ww1, nc_precision, UNC_LOC_W, 'ww1', 'upward_sea_water_velocity', 'Upward velocity on vertical interface, n-component', 'm s-1', jabndnd=jabndnd_)
      end if
      if (jamaprho > 0) then
            if ( density_is_pressure_dependent() ) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rho,  nc_precision, UNC_LOC_S3D, 'density', 'sea_water_density',           'Flow element center mass density',      'kg m-3', jabndnd=jabndnd_)
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rhop, nc_precision, UNC_LOC_S3D, 'rho',     'sea_water_potential_density', 'Flow element center potential density', 'kg m-3', jabndnd=jabndnd_)
            end if
      end if
   end if

   if (jamapq1 > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1, nc_precision, iLocU, 'q1', 'discharge', 'Discharge through flow link at current time', 'm3 s-1', cell_method = 'sum', jabndnd=jabndnd_)
      ierr = unc_put_att(mapids%ncid, mapids%id_q1, 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
   end if

   if (jamapq1main > 0 .and. allocated(q1_main)) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1main, nc_precision, iLocU, 'q1_main', '', 'Main channel discharge through flow link at current time', 'm3 s-1', cell_method = 'sum', jabndnd=jabndnd_)
      ierr = unc_put_att(mapids%ncid, mapids%id_q1main, 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
   end if

   if (jamapfw > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_fwel, nc_precision, UNC_LOC_U, 'fixed weir energy loss', '', 'Fixed weir energy loss', 'm', jabndnd=jabndnd_)
      ierr = unc_put_att(mapids%ncid, mapids%id_fwel, '', '')
   end if

   if (jamapviu > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_viu, nc_precision, iLocU, 'viu', '', 'Horizontal eddy viscosity', 'm2 s-1', jabndnd=jabndnd_)
   end if
   if (jamapdiu > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_diu, nc_precision, iLocU, 'diu', '', 'Horizontal eddy diffusivity', 'm2 s-1', jabndnd=jabndnd_)
   end if

   ! Bed shear stress
   if (jamaptaucurrent > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_tausx     , nc_precision, UNC_LOC_S, 'tausx'  , '', 'Total bed shear stress vector, x-component', 'N m-2', jabndnd=jabndnd_)   ! vect shear stress
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_tausy     , nc_precision, UNC_LOC_S, 'tausy'  , '', 'Total bed shear stress vector, y-component', 'N m-2', jabndnd=jabndnd_)   ! vect shear stress
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_taus      , nc_precision, UNC_LOC_S, 'taus'   , '', 'Total bed shear stress magnitude', 'N m-2', jabndnd=jabndnd_)
      if (stm_included) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_tausmax   , nc_precision, UNC_LOC_S, 'tausmax'  , '', 'Bed shear stress magnitude for morphology', 'N m-2', jabndnd=jabndnd_)   ! max shear stress
      end if
   end if

   if (jamaptidep > 0 .and. jatidep > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tidep, nc_precision, UNC_LOC_S, &
                            'TidalPotential', 'TidalPotential', 'Tidal Potential generated by celestial forces in flow element center', 'm2 s-2', &
                            jabndnd=jabndnd_)
   end if
   if (jamapselfal > 0) then
      if (jaselfal >  0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_salp, nc_precision, UNC_LOC_S, &
                                'SALPotential', 'SALPotential', 'Self-attraction and loading Potential in flow element center', 'm2 s-2', jabndnd=jabndnd_)
      end if
   end if

   if (jaFrcInternalTides2D > 0 .and. jamapIntTidesDiss > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_IntTidesDiss, nc_precision, UNC_LOC_S, &
                             'internal_tides_dissipation', 'internal_tides_dissipation', 'internal tides dissipation in flow element center',&
                             'J s-1 m-2', jabndnd=jabndnd_)
   end if

   ! Chezy data on flow nodes and flow links
   ! Input roughness value and type on flow links for input check (note: overwritten when jatrt==1)
   if (jamap_chezy_elements > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_czs , nc_precision, UNC_LOC_S, 'czs'  , '', 'Chezy roughness in flow element center', 'm0.5s-1', jabndnd=jabndnd_)
      ! WO: m0.5s-1 does not follow standard ? (which accepts only integral powers?)
   end if
   if (jamap_chezy_links > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_czu , nc_precision, UNC_LOC_U, 'czu'  , '', 'Chezy roughness on flow links', 'm0.5s-1', jabndnd=jabndnd_)
   end if
   if (jamap_chezy_input > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cfu , nc_precision, UNC_LOC_U, 'cfu'  , '', 'Input roughness on flow links', '-', jabndnd=jabndnd_)
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cfutyp , nf90_int, UNC_LOC_U, 'cfutyp'  , '', 'Input roughness type on flow links', '-', jabndnd=jabndnd_)
   end if

   ! Constituents
   if (jamapsal > 0 .and. jasal > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sa1, nc_precision, iLocS, 'sa1', 'sea_water_salinity', 'Salinity in flow element', '1e-3', jabndnd=jabndnd_)
   end if

   if (jamaptem > 0 .and. jatem > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tem1, nc_precision, iLocS, 'tem1', 'sea_water_temperature', 'Temperature in flow element', 'degC', jabndnd=jabndnd_)
   end if

   if (jamapspir > 0 .and. jasecflow > 0) then
      if (kmx < 1) then ! is this different from kmx == 0?
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_spircrv, nc_precision, UNC_LOC_S, 'spircrv', 'streamline_curvature', 'Flow streamline curvature'  , '1/m', jabndnd=jabndnd_ )
      end if
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_spirint, nc_precision, UNC_LOC_S, 'spirint', 'spiral_intensity'    , 'Spiral flow intensity'       , 'm/s', jabndnd=jabndnd_)
   end if

   ! Tracers
   if (jamapconst > 0 .and. ITRA1 > 0) then
      call realloc(mapids%id_const, (/ MAX_ID_VAR, NUMCONST /), keepExisting=.false., fill = 0)
      do j = ITRA1, ITRAN
         tmpstr = const_names(j)
         ! Forbidden chars in NetCDF names: space, /, and more.
         call replace_char(tmpstr,32,95)
         call replace_char(tmpstr,47,95)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_const(:,j), nc_precision, iLocS, trim(tmpstr), &
                                '', trim(const_names(j)) // ' in flow element', const_units(j), jabndnd=jabndnd_)
      end do
   end if
      
   ! Discharges
   ! TODO: AVD...
   ! TIDAL TURBINES: Insert equivalent of addturbine_cnst and addturbine_time here

   ! water quality bottom variables
   if (numwqbots > 0) then
      call realloc(mapids%id_wqb, (/ 3, numwqbots /), keepExisting=.false., fill = 0)
      do j = 1, numwqbots
         tmpstr = wqbotnames(j)
         ! Forbidden chars in NetCDF names: space, /, and more.
         call replace_char(tmpstr,32,95)
         call replace_char(tmpstr,47,95)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqb(:,j), nc_precision, UNC_LOC_S, trim(tmpstr), &
                                '', trim(wqbotnames(j)) // ' in flow element', wqbotunits(j), jabndnd=jabndnd_)
      end do
      if (wqbot3D_output == 1) then
         call realloc(mapids%id_wqb3d, (/ 3, numwqbots /), keepExisting=.false., fill = 0)
         do j = 1, numwqbots
            tmpstr = wqbotnames(j)
            ! Forbidden chars in NetCDF names: space, /, and more.
            call replace_char(tmpstr,32,95)
            call replace_char(tmpstr,47,95)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqb3d(:,j), nc_precision, UNC_LOC_S3D, trim(tmpstr)//'_3D', &
                                   '', trim(wqbotnames(j)) // ' in flow element (3D)', wqbotunits(j), jabndnd=jabndnd_)
         end do
      end if
   end if

   ! WAQ extra outputs
   if (jawaqproc > 0) then
      if (noout_map > 0) then
         call realloc(mapids%id_waq, (/ 3, noout_map /), keepExisting=.false., fill = 0)
         do j = 1, noout_map
            tmpstr = ' '
            write (tmpstr, "('water_quality_output_',I0)") j
            ierr = unc_def_var_map(mapids%ncid,  mapids%id_tsp, mapids%id_waq(:,j), nc_precision, iLocS, tmpstr, &
                                    '', outputs%names(j), outputs%units(j), jabndnd=jabndnd_)
            tmpstr = trim(outputs%names(j))//' - '//trim(outputs%description(j))//' in flow element'
            call replace_multiple_spaces_by_single_spaces(tmpstr)
            ierr = nf90_put_att(mapids%ncid, mapids%id_waq(2,j),  'description'  , tmpstr)
         end do
      end if
      if (noout_statt > 0) then
         call realloc(mapids%id_wqst, (/ 3, noout_statt /), keepExisting=.false., fill = 0)
         do j = 1, noout_statt
            jj = noout_user + j
            tmpstr = ' '
            write (tmpstr, "('water_quality_stat_',I0)") j
            ierr = unc_def_var_map(mapids%ncid,  mapids%id_tsp, mapids%id_wqst(:,j), nc_precision, iLocS, tmpstr, &
                                    '', outputs%names(jj), outputs%units(jj), jabndnd=jabndnd_)
            tmpstr = trim(outputs%names(jj))//' - '//trim(outputs%description(jj))//' in flow element'
            call replace_multiple_spaces_by_single_spaces(tmpstr)
            ierr = nf90_put_att(mapids%ncid, mapids%id_wqst(2,j),  'description'  , tmpstr)
         end do
      end if
      if (noout_state > 0) then
         call realloc(mapids%id_wqse, (/ 3, noout_state /), keepExisting=.false., fill = 0)
         do j = 1, noout_state
            jj = noout_user + noout_statt + j
            tmpstr = ' '
            write (tmpstr, "('water_quality_stat_',I0)") noout_statt + j
            ierr = unc_def_var_map(mapids%ncid,  mapids%id_tsp, mapids%id_wqse(:,j), nc_precision, iLocS, tmpstr, &
                                    '', outputs%names(jj), outputs%units(jj), 0, jabndnd=jabndnd_)
            tmpstr = trim(outputs%names(jj))//' - '//trim(outputs%description(jj))//' in flow element'
            call replace_multiple_spaces_by_single_spaces(tmpstr)
            ierr = nf90_put_att(mapids%ncid, mapids%id_wqse(2,j),  'description'  , tmpstr)
         end do
      end if
   end if

   ! mass balance areas
   if (nomba > 0) then
      ierr = unc_def_var_map(mapids%ncid,  mapids%id_tsp, mapids%id_mba(:), nf90_int, UNC_LOC_S, 'water_quality_mba', '', 'Water quality mass balance areas', '', is_timedep=0, jabndnd=jabndnd_)
      call realloc(flag_val, nomba, keepExisting = .false., fill = 0)
      flag_mean = ' '
      do j = nomba, 1, -1
         flag_val(j) = j
         flag_mean = trim(mbaname(j))//' '//flag_mean
      end do
      ierr = nf90_put_att(mapids%ncid, mapids%id_mba(2), 'flag_values', flag_val)
      ierr = nf90_put_att(mapids%ncid, mapids%id_mba(2), 'flag_meanings', flag_mean)
   end if

   ! Meteo forcings
   if (jamaprain > 0 .and. jarain /= 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rain,  nc_precision, UNC_LOC_S, 'rainfall_rate',  'rainfall_rate', 'Rainfall rate', 'm s-1', jabndnd=jabndnd_)
   end if

   ! interception
   if (jamapicept > 0 .and. interceptionmodel /= DFM_HYD_NOINTERCEPT) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_icepths,  nc_precision, UNC_LOC_S, 'interception_waterdepth',  '', 'Waterdepth in interception layer', 'm', jabndnd=jabndnd_)
   end if

   if (jamapwind > 0 .and. japatm /= 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_patm,  nc_precision, UNC_LOC_S, 'Patm',  'surface_air_pressure', 'Atmospheric pressure near surface', 'N m-2', jabndnd=jabndnd_)
   end if

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
      end if
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
         end if
      end if
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
         end if
      end if
   end if

   if (ja_airdensity + ja_computed_airdensity > 0 .and. jamap_airdensity > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp   , mapids%id_airdensity   , nc_precision, UNC_LOC_S, 'rhoair' , 'air_density'      , 'Air density'     , 'kg m-3', jabndnd=jabndnd_)
   end if

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
      end if

      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_Qtot   , nc_precision, UNC_LOC_S, 'Qtot'  , 'surface_downward_heat_flux_in_sea_water'                 , 'Total heat flux'                      , 'W m-2', jabndnd=jabndnd_)

   end if

   ! Turbulence
   if (jamaptur > 0 .and. model_is_3D()) then
      if (iturbulencemodel >= 3) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_turkin1, nc_precision, UNC_LOC_WU, 'turkin1', 'specific_turbulent_kinetic_energy_of_sea_water', 'turbulent kinetic energy',          'm2 s-2', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vicwwu,  nc_precision, UNC_LOC_WU, 'vicwwu',  'eddy_viscosity', 'turbulent vertical eddy viscosity at velocity points', 'm2 s-1', jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vicwws,  nc_precision, UNC_LOC_W,  'vicwws',  'eddy_viscosity', 'turbulent vertical eddy viscosity at pressure points', 'm2 s-1', jabndnd=jabndnd_)
         if (iturbulencemodel == 3) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tureps1, nc_precision, UNC_LOC_WU, 'tureps1', 'specific_turbulent_kinetic_energy_dissipation_in_sea_water',    'turbulent energy dissipation', 'm2 s-3', jabndnd=jabndnd_)
         elseif (iturbulencemodel == 4) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tureps1, nc_precision, UNC_LOC_WU, 'tureps1', '', 'turbulent time scale',         's-1', jabndnd=jabndnd_)
         end if
      end if
   end if

   ! Sediment transport (via morphology module)
   if ((jamapsed > 0 .and. jased > 0 .and. stm_included) .or. (jasubsupl > 0)) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_mor_bl   , nc_precision, UNC_LOC_S, 'mor_bl'  , '', 'Time-varying bottom level in flow cell center', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
   end if
      
   if (jasubsupl > 0) then
      select case (ibedlevtyp)
         case (1)
            iloc = UNC_LOC_S
         case (2)
            iloc = UNC_LOC_U
         case (3,4,5,6)
            iloc = UNC_LOC_CN
      end select
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_subsupl, nc_precision, iloc, 'subsupl'  , '', 'Cumulative subsidence/uplift', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
   end if
      
   if (jamapz0 > 0) then
      ! roughness heights for current and current and wave related roughness
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_z0c   , nc_precision, UNC_LOC_U, 'z0ucur'  , '', 'Current related roughness height'        , 'm', dimids = (/ -2,  -1 /), jabndnd=jabndnd_)
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_z0r   , nc_precision, UNC_LOC_U, 'z0urou'  , '', 'Current-wave related roughness height'   , 'm', dimids = (/ -2,  -1 /), jabndnd=jabndnd_)
   end if
      
   if (jamapsed > 0 .and. jased > 0 .and. stm_included) then
      ierr = nf90_def_dim(mapids%ncid, 'nSedTot', stmpar%lsedtot, mapids%id_tsp%id_sedtotdim)
      ierr = nf90_def_dim(mapids%ncid, 'nSedSus', stmpar%lsedsus, mapids%id_tsp%id_sedsusdim)
      ierr = nf90_def_dim(mapids%ncid, 'nBedLayers', stmpar%morlyr%settings%nlyr, mapids%id_tsp%id_nlyrdim)
      ierr = nf90_def_dim(mapids%ncid, 'nStringlen', 100, mapids%id_tsp%id_strlendim)
         
      if (.not. stmpar%morpar%moroutput%cumavg) then   ! only one average transport value at end of model run
         ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_sedavgtim, nc_precision, (/  1  /), 'sedAvgTim', '', 'Time interval over which cumulative transports are calculated', 's')
      end if
         
      call realloc(mapids%id_dxx, (/stmpar%morpar%nxx, 3 /), keepExisting=.false.)
         
      ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_morfac, nc_precision, (/ mapids%id_tsp%id_timedim /), 'morfac', '', 'Average morphological factor over elapsed morphological time', '-')
      ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_morft, nc_precision,  (/ mapids%id_tsp%id_timedim /), 'morft',  '', 'Current morphological time', 's')
         
      ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_frac_name, nf90_char,  (/ mapids%id_tsp%id_strlendim, mapids%id_tsp%id_sedtotdim /), 'sedfrac_name', '', 'Sediment fraction name', '-')
      if (stmpar%lsedsus > 0) then
         ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_susfrac_name, nf90_char,  (/ mapids%id_tsp%id_strlendim, mapids%id_tsp%id_sedsusdim /), 'sussedfrac_name', '', 'Suspended sediment fraction name', '-')
      end if
         
      select case(stmpar%morpar%moroutput%transptype)
         case (0)
            transpunit = 'kg s-1 m-1'
         case (1)
            transpunit = 'm3 s-1 m-1'
         case (2)
            transpunit = 'm3 s-1 m-1'
      end select
         
      ! Suspended transport related quantities
      if (stmpar%lsedsus > 0) then
            
         if (model_is_3D()) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_kmxsed, nf90_int, UNC_LOC_S, 'kmxsed', '', 'Bottom layer for sed calculations', '-', dimids = (/  -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ws, nc_precision, UNC_LOC_W, 'ws', '', 'Sediment settling velocity', 'm s-1', dimids = (/ -3, -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
         else
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ws, nc_precision, UNC_LOC_S, 'ws', '', 'Sediment settling velocity', 'm s-1', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
         end if
            
         if (.not. model_is_3D()) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rsedeq, nc_precision, UNC_LOC_S, 'rsedeq', '', 'Equilibrium sediment concentration', 'kg m-3', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
         end if
            
         if (stmpar%morpar%moroutput%aks) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_aks, nc_precision, UNC_LOC_S, 'aks', '', 'Near-bed reference concentration height', 'm', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rca, nc_precision, UNC_LOC_S, 'rca', '', 'Near-bed reference concentration', 'kg m-3', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
         end if
            
         if (stmpar%morpar%moroutput%sourcesink) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sourse , nc_precision, UNC_LOC_S, 'sourse'  , '', 'Source term suspended sediment fractions', 'kg m-3 s-1', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sinkse , nc_precision, UNC_LOC_S, 'sinkse'  , '', 'Sink term suspended sediment fractions', 's-1', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
         end if
            
         if (model_is_3D()) then
            if (stmpar%morpar%moroutput%suvcor) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_scrn , nc_precision, UNC_LOC_U, 'e_scrn'  , '', 'Near-bed transport correction in face-normal direction', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            end if
         end if
            
         if (model_is_3D()) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sedfrac, nc_precision, UNC_LOC_S3D, 'sedfrac_concentration', '', 'Sediment concentration in flow cell', 'kg m-3',dimids = (/ -3, -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
         else
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sedfrac, nc_precision, UNC_LOC_S, 'sedfrac_concentration', '', 'Sediment concentration in flow cell', 'kg m-3',dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
         end if
            
      end if

      ! intermediate output for sediment formulas
      if (stmpar%morpar%moroutput%sedpar) then
         call realloc(mapids%id_sedpar, [3, stmpar%trapar%npar, stmpar%lsedtot], keepExisting=.false.)
         do l = 1, stmpar%lsedtot
            write(sednr,'(I3.3)') l
            do k = 1, stmpar%trapar%noutpar(l)
               varname = trim(stmpar%trapar%outpar_name(k,l))//trim(sednr)
               longname = trim(stmpar%trapar%outpar_longname(k,l))//' for '//trim(stmpar%sedpar%namsed(l))
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sedpar(:,k,l), nf90_double, UNC_LOC_S, trim(varname), '', trim(longname) , '', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
            end do
         end do
      end if

      ! default sediment transport output (suspended and bedload) on flow links
      if (stmpar%lsedsus > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ssn   , nc_precision, UNC_LOC_U, 'ssn'  , '', 'Suspended load transport, n-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sst   , nc_precision, UNC_LOC_U, 'sst'  , '', 'Suspended load transport, t-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
      end if

      if (stmpar%lsedtot > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbn   , nc_precision, UNC_LOC_U, 'sbn'  , '', 'Bed load transport, n-component'         , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbt   , nc_precision, UNC_LOC_U, 'sbt'  , '', 'Bed load transport, t-component'         , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
      end if

      if (stmpar%morpar%moroutput%dzduuvv) then ! bedslope
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_e_dzdn , nc_precision, UNC_LOC_U, 'e_dzdn'  , '', 'Bed slope, n-component', '-', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_e_dzdt , nc_precision, UNC_LOC_U, 'e_dzdt'  , '', 'Bed slope, t-component', '-', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
      end if

      if (stmpar%morpar%moroutput%uuuvvv) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_uuu , nc_precision, UNC_LOC_S, 'uuu'  , '', 'Characteristic velocity in cell centre, x-component', 'm s-1', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_vvv , nc_precision, UNC_LOC_S, 'vvv'  , '', 'Characteristic velocity in cell centre, y-component', 'm s-1', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
      end if

      if (stmpar%morpar%moroutput%umod) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_umod , nc_precision, UNC_LOC_S, 'umod'  , '', 'Characteristic velocity magnitude in cell centre', 'm s-1', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
      end if

      if (stmpar%morpar%moroutput%zumod) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_zumod , nc_precision, UNC_LOC_S, 'zumod'  , '', 'Height above bed for characteristic velocity in cell centre', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
      end if

      if (stmpar%morpar%moroutput%ustar) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ustar , nc_precision, UNC_LOC_S, 'ustar'  , '', 'Bed shear velocity in cell centre', 'm s-1', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
      end if

      if (stmpar%morpar%moroutput%sbcuv) then
         if (stmpar%morpar%moroutput%rawtransports) then    ! if either of these is true, the reconstruction is done outside this subroutine, invalidating Willem's approach to have 'unspoiled' transports
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcx   , nc_precision, UNC_LOC_S, 'sbcx'  , '', 'Bed load transport due to currents, x-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcy   , nc_precision, UNC_LOC_S, 'sbcy'  , '', 'Bed load transport due to currents, y-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
         end if
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcx_reconstructed   , nc_precision, UNC_LOC_S, 'sbcx_reconstructed'  , '', 'Bed load transport due to currents (reconstructed), x-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcy_reconstructed   , nc_precision, UNC_LOC_S, 'sbcy_reconstructed'  , '', 'Bed load transport due to currents (reconstructed), y-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
      end if

      if (stmpar%morpar%moroutput%sbwuv) then
         if (stmpar%morpar%moroutput%rawtransports) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwx   , nc_precision, UNC_LOC_S, 'sbwx'  , '', 'Bed load transport due to waves, x-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwy   , nc_precision, UNC_LOC_S, 'sbwy'  , '', 'Bed load transport due to waves, y-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
         end if
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwx_reconstructed   , nc_precision, UNC_LOC_S, 'sbwx_reconstructed'  , '', 'Bed load transport due to waves (reconstructed), x-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwy_reconstructed   , nc_precision, UNC_LOC_S, 'sbwy_reconstructed'  , '', 'Bed load transport due to waves (reconstructed), y-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
      end if

      if (stmpar%morpar%moroutput%sscuv) then    ! This differs from Delft3D 4
         if (stmpar%morpar%moroutput%rawtransports) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscx   , nc_precision, UNC_LOC_S, 'sscx'  , '', 'Suspended load transport due to currents, x-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscy   , nc_precision, UNC_LOC_S, 'sscy'  , '', 'Suspended load transport due to currents, y-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
         end if
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscx_reconstructed   , nc_precision, UNC_LOC_S, 'sscx_reconstructed'  , '', 'Suspended load transport due to currents (reconstructed), x-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscy_reconstructed   , nc_precision, UNC_LOC_S, 'sscy_reconstructed'  , '', 'Suspended load transport due to currents (reconstructed), y-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
      end if

      if (stmpar%morpar%moroutput%sswuv) then
         if (stmpar%morpar%moroutput%rawtransports) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswx   , nc_precision, UNC_LOC_S, 'sswx'  , '', 'Suspended load transport due to waves, x-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswy   , nc_precision, UNC_LOC_S, 'sswy'  , '', 'Suspended load transport due to waves, y-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
         end if
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswx_reconstructed   , nc_precision, UNC_LOC_S, 'sswx_reconstructed'  , '', 'Suspended load transport due to waves (reconstructed), x-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswy_reconstructed   , nc_precision, UNC_LOC_S, 'sswy_reconstructed'  , '', 'Suspended load transport due to waves (reconstructed), y-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
      end if

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
      end if

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
            end if
               
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_preload , nc_precision, UNC_LOC_S, 'preload'  , '', 'Historical largest load on layer of the bed in flow cell center', 'kg', dimids = (/ mapids%id_tsp%id_nlyrdim, -2, -1 /), jabndnd=jabndnd_)
      end select
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sedshort, nc_precision, UNC_LOC_S, 'sedshort' , '', 'Sediment shortage of transport layer in flow cell center', 'kg m-2', dimids = (/ mapids%id_tsp%id_sedtotdim, -2, -1 /), jabndnd=jabndnd_)
         
      if (stmpar%morpar%moroutput%taub) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_taub  , nc_precision, UNC_LOC_S, 'taub'  , '', 'Bed shear stress for morphology', 'N m-2', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
      end if
      if (stmpar%morpar%moroutput%taurat) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_taurat  , nc_precision, UNC_LOC_S, 'taurat'  , '', 'Excess bed shear ratio', '-', dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
      end if
      if (stmpar%morpar%moroutput%dm) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dm  , nc_precision, UNC_LOC_S, 'dm'  , '', 'Arithmetic mean sediment diameter', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
      end if
      if (stmpar%morpar%moroutput%dg) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dg  , nc_precision, UNC_LOC_S, 'dg'  , '', 'Geometric mean sediment diameter', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
      end if
      if (stmpar%morpar%moroutput%dgsd) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dgsd  , nc_precision, UNC_LOC_S, 'dgsd'  , '', 'Geometric standard deviation of particle size mix', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
      end if
      if (stmpar%morpar%moroutput%percentiles) then
         do l = 1, stmpar%morpar%nxx
            write(dxname,'(A,I2.2)') 'DXX',l
            write(dxdescr,'(A,F4.1,A)') 'Sediment diameter percentile '    , stmpar%morpar%xx(l)*100d0,' %'
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dxx(l,:)  , nc_precision, UNC_LOC_S, dxname  , '', dxdescr, 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         end do
      end if
      if (stmpar%morpar%moroutput%frac) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_frac  , nc_precision, UNC_LOC_S, 'frac'  , '', 'Availability fraction in top layer', '-', dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
      end if
      if (stmpar%morpar%moroutput%mudfrac) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_mudfrac  , nc_precision, UNC_LOC_S, 'mudfrac'  , '', 'Mud fraction in top layer', '-', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
      end if
      if (stmpar%morpar%moroutput%sandfrac) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sandfrac  , nc_precision, UNC_LOC_S, 'sandfrac'  , '', 'Sand fraction in top layer', '-', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
      end if
      if (stmpar%morpar%moroutput%fixfac) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_fixfac  , nc_precision, UNC_LOC_S, 'fixfac'  , '', 'Reduction factor due to limited sediment thickness', '-', dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
      end if
      if (stmpar%morpar%moroutput%hidexp) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_hidexp  , nc_precision, UNC_LOC_S, 'hidexp'  , '', 'Hiding and exposure factor', '-', dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /), jabndnd=jabndnd_)
      end if
         
      if (stmpar%morpar%flufflyr%iflufflyr>0 .and. stmpar%lsedsus>0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_mfluff  , nc_precision, UNC_LOC_S, 'mfluff'  , '', 'Sediment mass in fluff layer', 'kg m-2', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /), jabndnd=jabndnd_)
      end if
         
      ! 1D cross sections
      if (ndx1d > 0 .and. stm_included) then
         if (stmpar%morpar%bedupd) then
            nCrs = 0
            do i = 1,size(network%crs%cross)
               if (network%crs%cross(i)%crossindx == 0) exit
               nCrs = nCrs + 1
            end do
            pCSs => network%CSDefinitions%CS
            jmax = 0
            do i = 1,size(pCSs)
               if (pCSs(i)%levelscount == 0) exit
               jmax = max(jmax,pCSs(i)%levelscount)
            end do
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
         end if
         if (stmpar%morpar%moroutput%blave) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_blave, nc_precision, UNC_LOC_S, 'bl_ave', '', 'Main channel averaged bed level', 'm', dimids = (/ -2, -1 /), which_meshdim = 1, jabndnd=jabndnd_)
            !ierr = nf90_def_var(mapids%ncid, trim(mesh1dname)//'_bl_ave', nf90_double, (/ mapids%id_tsp%id_ndx1d, mapids%id_tsp%id_timedim /), mapids%id_tsp%id_blave)
            !ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_blave, 'long_name','Main channel averaged bed level')
            !ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_blave, 'unit', 'm')
         end if
         if (stmpar%morpar%moroutput%bamor) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_bamor, nc_precision, UNC_LOC_S, 'mor_area', '', 'Main channel cell area', 'm2', is_timedep = 0, which_meshdim = 1, jabndnd=jabndnd_)
         end if
         if (stmpar%morpar%moroutput%wumor) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wumor, nc_precision, UNC_LOC_U, 'mor_width_u', '', 'Main channel cell width at flow link', 'm', is_timedep = 0, which_meshdim = 1, jabndnd=jabndnd_)
         end if
      end if
   end if
   
   ! Bedforms
   if (bfmpar%lfbedfrmout) then
      if (bfmpar%lfbedfrm) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_duneheight, nc_precision, UNC_LOC_S, 'duneheight'  , '', 'Time-varying dune height in flow cell center', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dunelength, nc_precision, UNC_LOC_S, 'dunelength'  , '', 'Time-varying dune length in flow cell center', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
      end if
      if (bfmpar%lfbedfrmrou) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ksr,  nc_precision, UNC_LOC_S, 'ksr'  , '', 'Ripple roughness height in flow cell center', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ksmr, nc_precision, UNC_LOC_S, 'ksmr'  , '', 'Megaripple roughness height in flow cell center', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ksd,  nc_precision, UNC_LOC_S, 'ksd'  , '', 'Dune roughness height in flow cell center', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ks,   nc_precision, UNC_LOC_S, 'ks'  , '', 'Bedform roughness height in flow cell center', 'm', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
      end if
   end if

   ! Sediment transport (via own built-in sed)
   if (jamapsed > 0 .and. jased > 0 .and. .not. stm_included) then
      ierr = nf90_def_dim(mapids%ncid, 'nFrac', mxgr, mapids%id_tsp%id_maxfracdim)
      if (.not. allocated(mapids%id_sed) ) then
         allocate( mapids%id_sed(MAX_ID_VAR,mxgr), mapids%id_ero(MAX_ID_VAR,mxgr) )
         mapids%id_sed = -1
         mapids%id_ero = -1
      end if
      do j = 1,mxgr
         write(str,"(I4)") j
         str = adjustl( str )
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sed(:,j), nc_precision, UNC_LOC_S, 'sed'//trim(str), 'sediment_concentration'      , 'Sediment concentration'   , 'kg m-3', jabndnd=jabndnd_) !, dimids = (/ mapids%id_maxfracdim, -2, -1 /))
      end do
      if (jaceneqtr == 1) then ! Bed level in cell center
         do j = 1,mxgr
            write(str,"(I4)") j
            str = adjustl( str )
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ero(:,j), nc_precision, UNC_LOC_S, 'ero'//trim(str), 'layer_thickness_per_fraction', 'Erodable layer thickness per size fraction in flow element centers'   , 'm', jabndnd=jabndnd_)
         end do
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_bl,  nc_precision, UNC_LOC_S, 'flowelem_bedlevel_bl', ''   , 'Flow element center bedlevel (bl)'                             , 'm', jabndnd=jabndnd_)
      else                     ! Bed level at cell corner
         do j = 1,mxgr
            write(str,"(I4)") j
            str = adjustl( str )
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ero(:,j), nc_precision, UNC_LOC_CN, 'ero'//trim(str), 'layer_thickness_per_fraction', 'Erodable layer thickness per size fraction in flow element corners'   , 'm', jabndnd=jabndnd_)
         end do
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_zk , nc_precision, UNC_LOC_CN,'netnode_bedlevel_zk', ''      , 'Flow element corner bedlevel (zk)'                          , 'm', jabndnd=jabndnd_)
      end if
   end if

   if (jamapwav > 0) then
      if (flowWithoutWaves) then      ! Check the external forcing wave quantities and their associated arrays
         if (jamapwav_hwav > 0 .and. allocated(hwav)) then
            if (jamapsigwav == 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav     , nc_precision, UNC_LOC_S, 'hwav'         , 'sea_surface_wave_rms_height'          , 'RMS wave height'          , 'm'    , jabndnd=jabndnd_) ! not CF
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav     , nc_precision, UNC_LOC_S, 'hwav'         , 'sea_surface_wave_significant_wave_height'          , 'Significant wave height'          , 'm'    , jabndnd=jabndnd_)
            end if
         end if
         if (jamapwav_twav > 0 .and. allocated(twav)) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_twav        , nc_precision, UNC_LOC_S, 'tp'  , ''        , 'Peak wave period'          , 's'    )
         end if
         if (jamapwav_phiwav > 0 .and. allocated(phiwav)) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_phiwav      , nc_precision, UNC_LOC_S, 'dir' , ''        , 'Mean direction of wave propagation relative to ksi-dir. ccw'   , 'deg', jabndnd=jabndnd_) ! not CF
         end if
         if (jamapwav_sxwav > 0  .and. allocated(sxwav)) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxwav       , nc_precision, UNC_LOC_S, 'sxwav' , 'sea_surface_x_wave_force_surface', 'Surface layer wave forcing term, x-component'   , 'N m-2', jabndnd=jabndnd_) ! not CF
         end if
         if (jamapwav_sywav > 0  .and. allocated(sywav)) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sywav       , nc_precision, UNC_LOC_S, 'sywav' , 'sea_surface_y_wave_force_surface', 'Surface layer wave forcing term, y-component'   , 'N m-2', jabndnd=jabndnd_) ! not CF
         end if
         if (jamapwav_sxbwav > 0 .and. allocated(sbxwav)) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxbwav      , nc_precision, UNC_LOC_S, 'sxbwav', 'sea_surface_x_wave_force_bottom' , 'Bottom layer wave forcing term, x-component'    , 'N m-2', jabndnd=jabndnd_) ! not CF
         end if
         if (jamapwav_sybwav > 0 .and. allocated(sbywav)) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sybwav      , nc_precision, UNC_LOC_S, 'sybwav', 'sea_surface_y_wave_force_bottom' , 'Bottom layer wave forcing term, y-component'    , 'N m-2', jabndnd=jabndnd_) ! not CF
         end if
         if (jamapwav_mxwav > 0  .and. allocated(mxwav)) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mxwav       , nc_precision, UNC_LOC_S, 'mx' , '', 'Wave-induced volume flux in x-direction'   , 'm3 s-1 m-1', jabndnd=jabndnd_) ! not CF
         end if
         if (jamapwav_mywav > 0  .and. allocated(mywav)) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mywav       , nc_precision, UNC_LOC_S, 'my' , '', 'Wave-induced volume flux in y-direction'   , 'm3 s-1 m-1', jabndnd=jabndnd_) ! not CF
         end if
         if (jamapwav_dsurf > 0  .and. allocated(dsurf)) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dsurf       , nc_precision, UNC_LOC_S, 'dissurf' , '', 'Wave energy dissipation rate at the free surface'   , 'w m-2', jabndnd=jabndnd_) ! not CF
         end if
         if (jamapwav_dwcap > 0  .and. allocated(dwcap)) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dwcap       , nc_precision, UNC_LOC_S, 'diswcap' , '', 'Wave energy dissipation rate due to white capping'   , 'w m-2', jabndnd=jabndnd_) ! not CF
         end if
         if (jamapwav_distot > 0  .and. allocated(distot)) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_distot      , nc_precision, UNC_LOC_S, 'distot' , '', 'Total wave energy dissipation'                       , 'w m-2', jabndnd=jabndnd_) ! not CF
         end if
         if (jamapwav_uorb > 0   .and. allocated(uorbwav)) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_uorb       , nc_precision, UNC_LOC_S, 'uorb'            , 'sea_surface_wave_orbital_velocity'    , 'Wave orbital velocity'    , 'm s-1', jabndnd=jabndnd_) ! not CF
         end if
      else   ! flow With Waves
         ! JRE waves
         if (jawave .eq. 4) then
            ierr = nf90_def_dim(mapids%ncid, 'ntheta', ntheta, mapids%id_tsp%id_ntheta)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_E        , nc_precision, UNC_LOC_S, 'E'        , 'sea_surface_bulk_wave_energy'         , 'Wave energy per square meter'                     , 'J m-2', jabndnd=jabndnd_) ! not CF
            if (roller > 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_R        , nc_precision, UNC_LOC_S, 'R'        , 'sea_surface_bulk_roller_energy'       , 'Roller energy per square meter'                   , 'J m-2', jabndnd=jabndnd_) ! not CF
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_DR       , nc_precision, UNC_LOC_S, 'DR'       , 'sea_surface_bulk_roller_dissipation'  , 'Roller energy dissipation per square meter'       , 'W m-2', jabndnd=jabndnd_) ! not CF
            end if
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
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_l1       , nc_precision, UNC_LOC_S, 'L1'       , 'sea_surface_wave_wavelength'          , 'Sea_surface_wave_wavelength'                      , 'm', jabndnd=jabndnd_      ) ! not CF
         end if

         if ((jawave == 3 .or. jawave == 4).and. model_is_3D()) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxwav       , nc_precision, UNC_LOC_S, 'sxwav' , 'sea_surface_x_wave_force_surface', 'Surface layer wave forcing term, x-component'   , 'N m-2', jabndnd=jabndnd_) ! not CF
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sywav       , nc_precision, UNC_LOC_S, 'sywav' , 'sea_surface_y_wave_force_surface', 'Surface layer wave forcing term, y-component'   , 'N m-2', jabndnd=jabndnd_) ! not CF
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxbwav      , nc_precision, UNC_LOC_S, 'sxbwav', 'sea_surface_x_wave_force_bottom' , 'Water body wave forcing term, x-component'    , 'N m-2', jabndnd=jabndnd_) ! not CF
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sybwav      , nc_precision, UNC_LOC_S, 'sybwav', 'sea_surface_y_wave_force_bottom' , 'Water body wave forcing term, y-component'    , 'N m-2', jabndnd=jabndnd_) ! not CF
         end if

         if (jawave > 0) then
            if (jamapsigwav == 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav        , nc_precision, UNC_LOC_S, 'hwav'         , 'sea_surface_wave_rms_height'          , 'RMS wave height'          , 'm' , jabndnd=jabndnd_   ) ! not CF
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav        , nc_precision, UNC_LOC_S, 'hwav'         , 'sea_surface_wave_significant_wave_height'          , 'Significant wave height'          , 'm' , jabndnd=jabndnd_   )
            end if
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_uorb     , nc_precision, UNC_LOC_S, 'uorb'            , 'sea_surface_wave_orbital_velocity'    , 'Wave orbital velocity'    , 'm s-1', jabndnd=jabndnd_) ! not CF
               
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ustokes      , nc_precision, iLocS, 'ust_cc'     , 'sea_surface_x_stokes_drift'        , 'Stokes drift, x-component'   , 'm s-1', jabndnd=jabndnd_) ! not CF
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vstokes      , nc_precision, iLocS, 'vst_cc'     , 'sea_surface_y_stokes_drift'       , 'Stokes drift, y-component'    , 'm s-1', jabndnd=jabndnd_) ! not CF

            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ustokeslink      , nc_precision, iLocU, 'ustokes'     , ''        , 'Stokes drift, n-component'   , 'm s-1', jabndnd=jabndnd_) ! not CF
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vstokeslink      , nc_precision, iLocU, 'vstokes'     , ''        , 'Stokes drift, t-component'   , 'm s-1', jabndnd=jabndnd_) ! not CF

            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_thetamean, nc_precision, UNC_LOC_S, 'thetamean'       , 'sea_surface_wave_from_direction'      , 'Wave from direction'      , 'deg from N', jabndnd=jabndnd_) ! not CF
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_twav,      nc_precision, UNC_LOC_S, 'twav'       ,      'sea_surface_wave_period'      , 'Wave period'      , 's') ! not CF
            if (jawave == 3 .or. jawave == 4) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fx       , nc_precision, iLocS, 'Fx'              , 'sea_surface_x_wave_force'          , 'Wave force, x-component'     , 'N m-2', jabndnd=jabndnd_) ! not CF
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fy       , nc_precision, iLocS, 'Fy'              , 'sea_surface_y_wave_force'         , 'Wave force, y-component'      , 'N m-2', jabndnd=jabndnd_) ! not CF

               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fxlink, nc_precision, iLocU, 'wavfu', '', 'Wave force at velocity point, n-component', 'N m-2', jabndnd=jabndnd_)! not CF
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fylink, nc_precision, iLocU, 'wavfv', '', 'Wave force at velocity point, t-component', 'N m-2', jabndnd=jabndnd_)! not CF
            end if
         end if
      end if
   end if
   
   ! Trachytope roughnesses on NET links
   if (jamaptrachy > 0 .and. jatrt == 1) then
      if (ifrctypuni == 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, nc_precision, UNC_LOC_L, 'cftrt',   '', 'Chezy roughness from trachytopes', '', jabndnd=jabndnd_)
         ierr = unc_put_att(mapids%ncid, mapids%id_cftrt, 'non_si_units', 'm0.5s-1')
      elseif (ifrctypuni == 1) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, nc_precision, UNC_LOC_L, 'cftrt',   '', 'Manning roughness from trachytopes', '', jabndnd=jabndnd_)
         ierr = unc_put_att(mapids%ncid, mapids%id_cftrt, 'non_si_units', 'sm-0.333')
      elseif ((ifrctypuni == 2) .or. (ifrctypuni == 3)) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, nc_precision, UNC_LOC_L, 'cftrt',   '', 'White-Colebrook roughness from trachytopes', '', jabndnd=jabndnd_)
         ierr = unc_put_att(mapids%ncid, mapids%id_cftrt, 'non_si_units', 'm')
      else
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, nc_precision, UNC_LOC_L, 'cftrt',   '', 'Roughness from trachytopes', '', jabndnd=jabndnd_)
         ierr = unc_put_att(mapids%ncid, mapids%id_cftrt, 'non_si_units', ' ')
      end if
   end if

   if (javeg > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rnveg        	, nc_precision, UNC_LOC_S, 'rnveg'        , 'stem density of vegetation'      , 'stem density per square meter', 'm-2')
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_diaveg        , nc_precision, UNC_LOC_S, 'diaveg'       , 'stem diameter of vegetation'     , 'stem diameter of vegetation', 'm')
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_veg_stemheight, nc_precision, UNC_LOC_S, 'stemheight'   , 'stem height of vegetation'       , 'stem height of vegetation', 'm')
   end if

   if (jamapcali > 0 .and. jacali == 1) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cfcl, nc_precision, UNC_LOC_L, 'cfcl',   '', 'Calibration factor for roughness', '', jabndnd=jabndnd_)
      ierr = unc_put_att(mapids%ncid, mapids%id_cfcl, 'non_si_units', 'm0.5s-1')
   end if

   if (janudge > 0 .and. jamapNudge > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_time, nc_precision, UNC_LOC_S, 'Tnudge', 'nudging_time', 'Nudging relaxing time', 's', is_timedep=0, jabndnd=jabndnd_)
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_tem, nc_precision, UNC_LOC_S3D, 'nudge_tem', 'nudging_tem', 'Nudging temperature', 'degC', jabndnd=jabndnd_)
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_sal, nc_precision, UNC_LOC_S3D, 'nudge_sal', 'nudging_sal', 'Nudging salinity', '1e-3, jabndnd=jabndnd_', jabndnd=jabndnd_)
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_Dtem, nc_precision, UNC_LOC_S3D, 'nudge_Dtem', 'nudging_Dtem', 'Difference of nudging temperature with temperature', 'degC', jabndnd=jabndnd_)
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_Dsal, nc_precision, UNC_LOC_S3D, 'nudge_Dsal', 'nudging_Dsal', 'Difference of nudging salinity with salinity', '1e-3', jabndnd=jabndnd_)
   end if

   ! for 1D only
   if (ndxi - ndx2d > 0 .and. jamapPure1D_debug) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_adve, nc_precision, UNC_LOC_U, 'adve', '', 'Explicit advection term', 's', which_meshdim = 1, jabndnd=jabndnd_)
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_advi, nc_precision, UNC_LOC_U, 'advi', '', 'Implicit advection term', 's', which_meshdim = 1, jabndnd=jabndnd_)
   end if

   if (ndxi - ndx2d > 0 .and. jaPure1D >= 3 .and. jamapPure1D_debug) then
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
   end if

   ! for 1D only, urban
   if (ndxi - ndx2d > 0 .and. network%loaded) then
      if (jamapTimeWetOnGround > 0) then ! cumulative time when water is above ground level
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_timewetground, nc_precision, UNC_LOC_S, 'time_water_on_ground', '', 'Cumulative time water above ground level', 's', which_meshdim = 1, jabndnd=jabndnd_)
      end if
      if (jamapFreeboard > 0) then ! freeboard
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_freeboard, nc_precision, UNC_LOC_S, 'freeboard', '', 'Freeboard', 'm', which_meshdim = 1, jabndnd=jabndnd_)
      end if
      if (jamapDepthOnGround > 0) then ! waterdpth that is above ground level
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hs_on_ground, nc_precision, UNC_LOC_S, 'waterdepth_on_ground', '', 'Waterdepth above ground level', 'm', which_meshdim = 1, jabndnd=jabndnd_)
      end if
      if (jamapVolOnGround > 0) then ! volume that is above ground level
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vol_on_ground, nc_precision, UNC_LOC_S, 'volume_on_ground', '', 'Volume above ground level', 'm3', which_meshdim = 1, jabndnd=jabndnd_)
      end if
      if (jamapTotalInflow1d2d > 0) then ! total 1d2d net inflow
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qCur1d2d, nc_precision, UNC_LOC_S, 'current_total_net_inflow_1d2d', '', 'Current total net inflow via all connected 1d2d links at each 1D node', 'm3 s-1', which_meshdim = 1, jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vTot1d2d, nc_precision, UNC_LOC_S, 'cumulative_total_net_inflow_1d2d', '', 'Cumulative total net inflow via all connected 1d2d links at each 1D node', 'm3', which_meshdim = 1, jabndnd=jabndnd_)
      end if
      if (jamapTotalInflowLat > 0) then ! total lateral net inflow
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qCurLat, nc_precision, UNC_LOC_S, 'current_total_net_inflow_lateral', '', 'Current total net inflow via all laterals at each 1D node', 'm3 s-1', which_meshdim = 1, jabndnd=jabndnd_)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vTotLat, nc_precision, UNC_LOC_S, 'cumulative_total_net_inflow_lateral', '', 'Cumulative total net inflow via all laterals at each 1D node', 'm3', which_meshdim = 1, jabndnd=jabndnd_)
      end if
   end if
   if (lnx1d > 0) then
      if (jamapS1Gradient > 0) then ! water level gradient
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s1Gradient, nc_precision, UNC_LOC_U, 'water_level_gradient', '', 'Water level gradient at each 1D flow link', '1', which_meshdim = 1, jabndnd=jabndnd_)
      end if
   end if
   if (jamapNearField > 0) then
      ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nrfld, nc_precision, UNC_LOC_S3D, 'nrfld', 'nearfield_discharges', 'Nearfield related discharges', 'm3 s-1', jabndnd=jabndnd_)
   end if
   
   ! End of definitions, switch to data mode
   ierr = nf90_enddef(mapids%ncid)
   if (ierr == NF90_EVARSIZE .and. unc_cmode /= NF90_NETCDF4) then
      call mess(LEVEL_ERROR, 'Error while writing map file. Probably model grid is too large for classic NetCDF format. Try setting [output] NcFormat = 4 in your MDU.')
   elseif (ierr /= NF90_NOERR) then
      write (msgbuf, '(a)') 'Error while writing map file. Error code: ' // nf90_strerror(ierr) // '.'
      call err_flush()
   end if
   
end subroutine unc_write_map_def

!> Write to all the time-independent variables in a new map file
subroutine unc_write_map_write_time_independent(mapids, jabndnd_)
   use unstruc_netcdf, only: t_unc_mapids
   use m_flow
   use m_flowgeom, only: ndx!, ndx2d, ntheta, lnx1d, lnx, blup, bl, wu_mor, bl_ave, ba_mor, bare, ba, ln, wcx1, wcx2, wcy1, wcy2
   use unstruc_netcdf, only: unc_put_var_map
   use m_output_config, only: UNC_LOC_S
   use m_mass_balance_areas, only: nomba, mbadef
   use m_sediment, only: stm_included, stmpar
   use netcdf, only: nf90_put_var
   use unstruc_channel_flow, only: network

   type(t_unc_mapids), intent(inout) :: mapids    !< Set of file and variable ids for this map-type file.
   integer,            intent(in   ) :: jabndnd_  !< Flag specifying whether boundary nodes are to be written.
   
   integer :: ierr, i, j, k

   if (janudge > 0 .and. jamapnudge > 0) then
      ! output static nudging time
      workx = 0d0
      do k = 1, Ndx
         if (nudge_rate(k) > 0d0) then
            workx(k) = 1d0 / nudge_rate(k)
         end if
      end do
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_time, UNC_LOC_S, workx, jabndnd=jabndnd_)
   end if

   if (nomba > 0) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mba(:), UNC_LOC_S, mbadef, jabndnd=jabndnd_)
   end if

   if (jased == 4 .and. stm_included) then
      do j = 1, stmpar%lsedtot
         ierr = nf90_put_var(mapids%ncid,mapids%id_frac_name,trim(stmpar%sedpar%namsed(j)),(/ 1, j /),(/ len(trim(stmpar%sedpar%namsed(j))), 1 /))  ! only write once
      end do
      if (stmpar%lsedsus > 0) then
         do j = 1, stmpar%lsedsus
            ierr = nf90_put_var(mapids%ncid,mapids%id_susfrac_name,trim(stmpar%sedpar%namsed(j)),(/ 1, j /),(/ len(trim(stmpar%sedpar%namsed(j))), 1 /))  ! only write once
         end do
      end if
   end if
      
   ! 1D cross sections
   if (ndx1d > 0 .and. stm_included) then
      if (stmpar%morpar%bedupd) then
         do i = 1, nCrs
            ierr = nf90_put_var(mapids%ncid, mapids%id_tsp%id_morCrsName,trim(network%crs%cross(i)%CSID),(/ 1, i /),(/ len(trim(network%crs%cross(i)%CSID)), 1 /))  ! only write once
         end do
      end if
   end if
      
end subroutine unc_write_map_write_time_independent

end module m_unc_write_map