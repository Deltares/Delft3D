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

 !> Initializes the entire current model (geometry, boundaries, initial state)
 !! @return Error status: error (/=0) or not (0)
 integer function flow_modelinit() result(iresult) ! initialise flowmodel
    use timers
    use m_flowgeom, only: jaFlowNetChanged, ndx, lnx, ndx2d, ndxi, wcl, ln
    use waq, only: reset_waq
    use m_flow, only: kmx, kmxn, jasecflow, iperot, taubxu, ucxq, ucyq, fvcoro, vol1
    use m_flowtimes
    use m_lateral, only: numlatsg
    use network_data, only: NETSTAT_CELLS_DIRTY
    use gridoperations, only: make1D2Dinternalnetlinks
    use m_partitioninfo
    use m_timer
    use m_flowtimes
    use unstruc_model
    use unstruc_files, only: mdia
    use unstruc_netcdf
    use MessageHandling
    use m_flowparameters, only: jawave, jatrt, jacali, flowWithoutWaves, jasedtrails, jajre, modind, jaextrapbl, Corioadamsbashfordfac, flow_solver, FLOW_SOLVER_SRE
    use dfm_error
    use m_fm_wq_processes, only: jawaqproc
    use m_vegetation
    use m_hydrology, only: jadhyd, init_hydrology
    use m_integralstats, is_is_numndvals => is_numndvals
    use m_xbeach_data, only: bccreated
    use m_oned_functions
    use m_nearfield, only: reset_nearfieldData
    use m_alloc
    use m_bedform
    use m_fm_update_crosssections, only: fm_update_mor_width_area, fm_update_mor_width_mean_bedlevel
    use unstruc_netcdf_map_class
    use unstruc_caching
    use m_monitoring_crosssections, only: ncrs
    use m_setucxcuy_leastsquare, only: reconst2ndini
    use fm_external_forcings_data, only: nwbnd
    use m_sedtrails_network
    use m_sedtrails_netcdf, only: sedtrails_loadNetwork
    use m_sedtrails_stats, only: default_sedtrails_stats, alloc_sedtrails_stats
    use fm_statistical_output
    use unstruc_display, only: ntek, jaGUI
    use m_debug
    use m_flow_flowinit
    use m_pre_bedlevel, only: extrapolate_bedlevel_at_boundaries
    use m_fm_icecover, only: fm_ice_alloc, fm_ice_echo
    use m_fixedweirs, only: weirdte, nfxw
    use mass_balance_areas_routines, only: mba_init
    use m_curvature, only: get_spirucm
    use m_fm_erosed, only: taub
    use precision
    use system_utils, only: makedir
    use m_fm_erosed, only: taub
    use m_transport, only: numconst, constituents
    use m_lateral, only: reset_outgoing_lat_concentration, average_concentrations_for_laterals, apply_transport_is_used, &
                         get_lateral_volume_per_layer, lateral_volume_per_layer
    use m_initialize_flow1d_implicit, only: initialize_flow1d_implicit
    !
    ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
    ! Activate the following line (See also statements below)
    !use ifcore
    !
    implicit none

    integer :: istat, L, ierr, k1, k2
    logical :: set_hu, use_u1
    integer, external :: init_openmp
    integer, external :: set_model_boundingbox

    double precision, allocatable :: weirdte_save(:)
    double precision, allocatable :: ucxq_save(:), ucyq_save(:)
    double precision, allocatable :: fvcoro_save(:)

    !
    ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
    ! Activate the following 3 lines, See also statements below
    !INTEGER*4 OLD_FPE_FLAGS, NEW_FPE_FLAGS
    !NEW_FPE_FLAGS = FPE_M_TRAP_OVF + FPE_M_TRAP_DIV0 + FPE_M_TRAP_INV
    !OLD_FPE_FLAGS = FOR_SET_FPE (NEW_FPE_FLAGS)
    !
    iresult = DFM_GENERICERROR

    call datum2(rundat2)
    L = len_trim(rundat2)

    if (ti_waq > 0d0) then
       call makedir(getoutputdir('waq')) ! No problem if it exists already.
    end if

    call timstrt('Basic init', handle_extra(1)) ! Basic steps

    md_snapshotdir = trim(getoutputdir()) ! plot output to outputdir
    ! Make sure output dir for plot files exists
    if (len_trim(md_snapshotdir) > 0) then
       call makedir(md_snapshotdir) ! No problem if it exists already.
    end if

    if (jatimer == 1) then
       call initimer()
    end if

    call unc_set_ncformat(md_ncformat)

    call reset_unstruc_netcdf_map_class()

    call resetflow()

    call reset_waq()

    call reset_nearfieldData()

    call timstop(handle_extra(1)) ! End basic steps

    if (jagui == 1) then
       call timini() ! this seems to work, initimer and timini pretty near to each other
    end if

! JRE
    if (jawave == 4) then
       call timstrt('Surfbeat input init', handle_extra(2)) ! Wave input
       bccreated = .false. ! for reinit
       call xbeach_wave_input() ! will set swave and lwave
       call timstop(handle_extra(2)) ! End wave input
    end if

    call timstrt('Make internal links      ', handle_extra(3)) ! Internal links
    if (md_jamake1d2dlinks == 1) then
       ierr = make1D2Dinternalnetlinks()
       if (ierr /= DFM_NOERR) then
          call mess(LEVEL_WARN, 'Error, failed to create 1D2D links.')
          goto 1234
       end if
    end if
    call timstop(handle_extra(3)) ! End internal links

    ! TODO: unc_wri_map_header

    call timstrt('Flow geometry       ', handle_extra(4)) ! Flow geometry
    call mess(LEVEL_INFO, 'Initializing flow model geometry...')
    if (jampi == 0) then
       call flow_geominit(0) ! initialise flow geometry based upon present network, time independent
       ! make directional wave grid for surfbeat model

       call mess(LEVEL_INFO, 'Done initializing flow model geometry.')

       if (ndx == 0) then
          call mess(LEVEL_WARN, 'Model initialization has resulted in an empty model (0 gridcells/points). Is input grid correct?')
          iresult = DFM_MODELNOTINITIALIZED
          goto 1234
       end if
    else
       call flow_geominit(1) ! first phase only

       if (Ndx > 0) then
          call mess(LEVEL_INFO, 'Start partitioning model...')
          if (jatimer == 1) call starttimer(IPARTINIT)

          call partition_init_1D2D(md_ident, iresult) ! 1D & 2D (hence the name, thanks to Herman for pointing this out)

          if (jatimer == 1) call stoptimer(IPARTINIT)
          call mess(LEVEL_INFO, 'Done partitioning model.')

          if (iresult == 0) then
             call update_geom(1) ! update geometry in ghost area

             call flow_geominit(2) ! second phase
             call update_geom(2) ! update geometry in ghost area

             call disable_invalid_ghostcells_with_wu() ! disable ghost cells that are not being synchronised by setting wu's to zero

             call mess(LEVEL_INFO, 'Done initializing flow model geometry.')
          else
             call mess(LEVEL_WARN, 'Error in 2D partitioning initialization.')
             goto 1234
          end if
       else
          call mess(LEVEL_WARN, 'No network, please check MDU-file')
          iresult = DFM_MODELNOTINITIALIZED
          goto 1234
       end if
    end if

    iresult = set_model_boundingbox()

    call timstop(handle_extra(4)) ! End flow geometry

    if (kmx > 0 .and. jasecflow > 0) then ! An error announcement (or warning, with correction to jasecflow to 0)
       jasecflow = 0
       call mess(LEVEL_WARN, 'Warning: Secondary Flow is not applicable in 3D computation !!')
       call mess(LEVEL_WARN, '         Secondary flow is turned off')
    end if

    call timstrt('Bobsongullies       ', handle_extra(5)) ! bobsongullies
    call setbobsongullies()
    call timstop(handle_extra(5)) ! End bobsongullies

    if (javeg > 0) then
       ! NOTE: AvD: hardcoded for now: if vegetation is on, maintain max shear stresses for Peter and Jasper.
       is_is_numndvals = 3
    end if

    if (my_rank == fetch_proc_rank .and. (jawave == 1 .or. jawave == 2)) then
       ! All helpers need no further model initialization.
       call tauwavefetch(0d0)
       iresult = DFM_USERINTERRUPT
       return
    end if
    ! 3D: flow_allocflow will set kmxn, kmxL and kmxc arrays
    call timstrt('Flow allocate arrays          ', handle_extra(37)) ! alloc flow
    call flow_allocflow() ! allocate   flow arrays
    call timstop(handle_extra(37)) ! end alloc flow
    !
    if (jawave > 0 .and. .not. flowWithoutWaves) then
       call alloc9basicwavearrays()
    end if
    if (jawave > 2) then
       call flow_waveinit()
    end if
    ! Construct a default griddim struct for D3D subroutines, i.e. sedmor or trachytopes
    call timstrt('Flow grid           ', handle_extra(7)) ! Flow griddim
    if (len_trim(md_sedfile) > 0 .or. jatrt == 1) then
       call D3Dflow_dimensioninit()
    end if
    call timstop(handle_extra(7)) ! End flow griddim

    call timstrt('Bed forms init (1)  ', handle_extra(8)) ! Bed forms
    if ((jased > 0 .and. stm_included) .or. bfm_included .or. jatrt > 0 .or. (jawave > 0 .and. modind == 9)) then
       call flow_bedforminit(1) ! bedforms stage 1: datastructure init
    end if
    call timstop(handle_extra(8)) ! End bed forms

 !! flow1d -> dflowfm initialization
    call timstrt('1D roughness        ', handle_extra(9)) ! 1d roughness
    call set_1d_roughnesses()
    call timstop(handle_extra(9)) ! End 1d roughness

    ! need number of fractions for allocation of sed array
    call timstrt('Sediment transport and morphology init', handle_extra(10)) ! sedmor
    if (len_trim(md_sedfile) > 0) then
       call flow_sedmorinit()
    end if
    call timstop(handle_extra(10)) ! End sedmor

    call timstrt('Bed forms init (2)  ', handle_extra(11)) ! bedform
    if ((jased > 0 .and. stm_included) .or. bfm_included .or. jatrt > 0) then
       call flow_bedforminit(2) ! bedforms  stage 2: parameter read and process
    end if
    call timstop(handle_extra(11)) ! End bedform

    call timstrt('Vertical administration', handle_extra(12)) ! vertical administration
    if (jampi == 1) then
!   update vertical administration
       call update_vertadmin()

       !3D: partition_init needs kmxn and kmxL arrays for 3D send- and ghostlists
       if (jatimer == 1) call starttimer(IPARTINIT)
       call partition_init_3D(iresult)
       if (jatimer == 1) call stoptimer(IPARTINIT)

       if (iresult /= DFM_NOERR) then
          call mess(LEVEL_WARN, 'Error in 3D partitioning initialization.')
          goto 1234
       end if

    end if
    call timstop(handle_extra(12)) ! vertical administration

#ifdef _OPENMP
    ierr = init_openmp(md_numthreads, jampi)
#endif

    call timstrt('Net link tree 0     ', handle_extra(13)) ! netlink tree 0
    if ((jatrt == 1) .or. (jacali == 1)) then
       call netlink_tree(0)
    end if
    call timstop(handle_extra(13)) ! end netlink tree

    call timstrt('Initialise trachytopes', handle_extra(14)) ! flow trachy init
    if (jatrt == 1) then
       call flow_trachyinit() ! initialise the trachytopes module
    end if
    call timstop(handle_extra(14)) ! end flow trachy init

    call timstrt('Initialise Calibration', handle_extra(15)) ! calibration init
    if (jacali == 1) then
       call calibration_init() ! initialise the calibration memory structures and read .cld and .cll files
    end if
    call timstop(handle_extra(15)) ! end calibration init

    call timstrt('Net link tree 1     ', handle_extra(16)) ! netlink tree 1
    if ((jatrt == 1) .or. (jacali == 1)) then
       call netlink_tree(1)
    end if
    call timstop(handle_extra(16)) ! netlink tree 1

    if (iperot == -1) then
       call reconst2ndini()
    end if

 !! flow1d -> dflowfm update
    call timstrt('Save 1d             ', handle_extra(17)) ! save 1d
    if (stm_included) then
       call save_1d_nrd_vars_in_stm()
    end if
    call timstop(handle_extra(17)) ! end save 1d

! initialize waq and add to tracer administration
    call timstrt('WAQ processes init  ', handle_extra(18)) ! waq processes init
    if (len_trim(md_subfile) > 0) then
       call fm_wq_processes_ini_sub()
    end if
    call timstop(handle_extra(18)) ! end waq processes init

    call timstrt('Transport init      ', handle_extra(19)) ! transport module
    call ini_transport()
    call timstop(handle_extra(19)) ! end transport module

    call timstrt('Observations init   ', handle_extra(21)) ! observations init
    call flow_obsinit() ! initialise stations and cross sections on flow grid + structure his (1st call required for call to flow_trachy_update)
    if (ncrs > 0) then
       call fill_geometry_arrays_crs()
    end if
    call timstop(handle_extra(21)) ! end observations init

    call timstrt('Ice init', handle_extra(84)) ! ice
    call fm_ice_alloc(ndx) ! needs to happen after flow_geominit to know ndx, but before flow_flowinit where we need the arrays for the external forcings
    call timstop(handle_extra(84)) ! End ice

    call timstrt('Flow init           ', handle_extra(23)) ! flow init
    iresult = flow_flowinit() ! initialise flow arrays and time dependent params for a given user time
    if (iresult /= DFM_NOERR) then
       goto 1234
    end if
    call timstop(handle_extra(23)) ! end flow init

    ! report on final configuration of ice module; needs to happen after flow_flowinit where external forcings are initialized
    call timstrt('Ice init', handle_extra(84)) ! ice
    call fm_ice_echo(mdia)
    call timstop(handle_extra(84)) ! End ice

    if (jadhyd == 1) then
       call init_hydrology() ! initialise the hydrology module (after flow_flowinit())
    end if

    if (numlatsg > 0) then
       call init_lateral_his()
       call fill_geometry_arrays_lateral()
    end if

    ! initialize waq and add to tracer administration
    call timstrt('WAQ processes init  ', handle_extra(18)) ! waq processes init
    if (ti_waqproc /= 0d0) then
       if (jawaqproc == 1) then
          call fm_wq_processes_ini_proc()
          jawaqproc = 2
          if (ti_waqproc > 0d0) then
             call fm_wq_processes_step(ti_waqproc, tstart_user)
          else
             call fm_wq_processes_step(dt_init, tstart_user)
          end if
       end if
    end if
    call timstop(handle_extra(18)) ! end waq processes init

    call timstrt('MBA init            ', handle_extra(24)) ! MBA init
    if (ti_mba > 0) then
       call mba_init()
    end if
    call timstop(handle_extra(24)) ! end MBA init

    call timstrt('Update MOR width    ', handle_extra(25)) ! update MOR width and mean bed level
    if (stm_included) then
       call fm_update_mor_width_area()
       if (len_trim(md_dredgefile) > 0 .or. ndxi > ndx2d) then
          call flow_bl_ave_init()
          call fm_update_mor_width_mean_bedlevel()
       end if
    end if
    call timstop(handle_extra(25)) ! end update MOR width

    call timstrt('Dredging init       ', handle_extra(26)) ! dredging init
    if (len_trim(md_dredgefile) > 0 .and. stm_included) then
       call flow_dredgeinit() ! dredging and dumping. Moved here because julrefdate needed
    end if
    call timstop(handle_extra(26)) ! end dredging init

    if (jawave == 4 .and. jajre == 1) then
       call timstrt('Surfbeat init         ', handle_extra(27)) ! Surfbeat init
       if (jampi == 0) then
          if (nwbnd == 0) then
             call mess(LEVEL_ERROR, 'unstruc::flow_modelinit - No wave boundary defined for surfbeat model. Do you use the correct ext file?')
          end if
       end if
       call xbeach_wave_init()
       call timstop(handle_extra(27))
    end if

    call timstrt('Observations init 2 ', handle_extra(28)) ! observations init 2
    call flow_obsinit() ! initialise stations and cross sections on flow grid + structure his (2nd time required to fill values in observation stations)
    call timstop(handle_extra(28)) ! end observations init 2

    call timstrt('Structure parameters', handle_extra(29)) ! structure parameters
    call structure_parameters() ! initialize structure values, after flow_flowinit() so that initial water levels and discharges are already set.
    call timstop(handle_extra(29)) ! end structure parameters

    ! Prepare for his/map/clm output via statistical_output module
    call flow_init_statistical_output_his(config_set_his, out_variable_set_his)

    call timstrt('Trachy update       ', handle_extra(30)) ! trachy update
    if (jatrt == 1) then
       call flow_trachyupdate() ! Perform a trachy update step to correctly set initial field quantities
    end if ! Generally flow_trachyupdate() is called from set_external_forcings()
    call timstop(handle_extra(30)) ! end trachy update

    call timstrt('Set friction values for MOR        ', handle_extra(31)) ! set fcru mor
    if ((jased > 0) .and. stm_included) then
       call set_frcu_mor(1) !otherwise frcu_mor is set in getprof_1d()
       call set_frcu_mor(2)
    end if
    call timstop(handle_extra(31)) ! end set fcru mor

! Initialise debug array
    !if (jawritedebug) then
    !  call init_debugarr(lnx,stmpar%lsedtot)
    !endif

    if (nfxw > 0) then
       allocate (weirdte_save(nfxw), STAT=ierr)
       weirdte_save = weirdte
    end if
    set_hu = .true.
    use_u1 = .true.
    if (len_trim(md_restartfile) > 0) then !See UNST-7754
       set_hu = .false.
       use_u1 = .false.
       ucxq_save = ucxq
       ucyq_save = ucyq
       if (Corioadamsbashfordfac > 0d0) then
          fvcoro_save = fvcoro
       end if
    end if !restart
    call flow_initimestep(1, set_hu, use_u1, iresult) ! 1 also sets zws0
    if (nfxw > 0) then
       weirdte = weirdte_save
       deallocate (weirdte_save)
    end if
    if (len_trim(md_restartfile) > 0) then
       ucxq = ucxq_save
       ucyq = ucyq_save
       fvcoro = fvcoro_save
    end if

    !See UNST-7754
    if (stm_included .and. jased > 0) then
       taub = 0d0
       do L = 1, lnx
          k1 = ln(1, L); k2 = ln(2, L)
          taub(k1) = taub(k1) + wcl(1, L) * taubxu(L)
          taub(k2) = taub(k2) + wcl(2, L) * taubxu(L)
       end do
    end if
    jaFlowNetChanged = 0

    ! Secondary flow
    if (jasecflow > 0 .and. kmx == 0) then
       call get_spirucm()
    end if

    ! Initialise Fourier Analysis
    call timstrt('Fourier init        ', handle_extra(33)) ! Fourier init
    if (len_trim(md_foufile) > 0) then
       call flow_fourierinit()
    end if
    call timstop(handle_extra(33)) ! end Fourier init

    if (numconst > 0 .and. apply_transport_is_used) then
       ! During initialisation, the lateral data must be initialized correctly
       call reset_outgoing_lat_concentration()
       ! Use timestep 1 s to set outgoing_lat_concentration to the initial averaged concentrations at each lateral location.
       call average_concentrations_for_laterals(numconst, kmx, kmxn, vol1, constituents, 1._dp)
       call get_lateral_volume_per_layer(lateral_volume_per_layer)
    end if

    !Initialize flow1d_implicit
    if (flow_solver == FLOW_SOLVER_SRE) then
       call initialize_flow1d_implicit(iresult)
       if (iresult /= DFM_NOERR) then
          call mess(LEVEL_WARN, 'Error initializing 1D implicit.')
          goto 1234
       end if
    end if

    ! Initialise sedtrails statistics
    if (jasedtrails > 0) then
       call default_sedtrails_stats()
       call alloc_sedtrails_stats()
    end if

    ! Extrapolate bed level
    if (jaextrapbl == 1) then
       call extrapolate_bedlevel_at_boundaries()
    end if

    call timstrt('MDU file pointer    ', handle_extra(34)) ! writeMDUFilepointer
    call mess(LEVEL_INFO, '** Model initialization was successful **')
    call mess(LEVEL_INFO, '* Active Model definition:') ! Print model settings in diagnostics file.
    call writeMDUFilepointer(mdia, .true., istat)

    call mess(LEVEL_INFO, '**')
    call timstop(handle_extra(34)) ! end writeMDUFilepointer

    call timstrt('Flowgeom            ', handle_extra(35)) ! write flowgeom ugrid
    if (len_trim(md_flowgeomfile) > 0) then ! Save initial flow geometry to file.
       if (md_unc_conv == UNC_CONV_UGRID) then
          call unc_write_net_flowgeom_ugrid(trim(md_flowgeomfile)) ! UGRID
       else
          call unc_write_net_flowgeom(trim(md_flowgeomfile)) ! CFOLD
       end if
    end if
    call timstop(handle_extra(35)) ! end write flowgeom ugrid

    if (jasedtrails > 0) then
       call default_sedtrails_geom()
       call sedtrails_loadNetwork(md_sedtrailsfile, istat, 0)
       if (istat > 0) then
          call mess(LEVEL_ERROR, 'unstruc_model::loadModel - Could not load sedtrails network.')
       end if
       call sedtrails_get_grid_on_network()
    end if

    ! store the grid-based information in the cache file
    call timstrt('Remainder           ', handle_extra(36)) ! remainder
    call storeCachingFile(md_ident, md_usecaching)

    call timstop(handle_extra(36)) ! End remainder
    call writesomeinitialoutput()

    iresult = DFM_NOERR

    return
1234 continue

 end function flow_modelinit
