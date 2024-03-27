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
module m_flow_flowinit

implicit none

private

integer, parameter :: OFF = 0
integer, parameter :: ON  = 1
integer, parameter :: INITIALIZE = 1
integer, parameter :: LATERAL_1D2D_LINK = 3
integer, parameter :: STREET_INLET_1D2D_LINK = 5
integer, parameter :: ROOF_GUTTER_1D2D_LINK  = 7
integer, parameter :: ORIGINAL_LATERAL_OVERFLOW_ADVECTION = 8
integer, parameter :: BOUNDARY_1D = -1
integer, parameter :: SET_ZWS0    =  1
logical, parameter :: INITIALIZATION_PHASE = .true.


public :: flow_flowinit

contains
    
 !> Initialise flow model time dependent parameters
 !! @return Integer error status (0) if succesful.
 integer function flow_flowinit() result(error)
   use m_flowgeom
   use m_flow
   use m_flowtimes
   use m_sferic
   use unstruc_model,    only : md_netfile, md_input_specific, md_restartfile
   use m_reduce,         only : nodtot, lintot
   use m_transport
   use dfm_error
   use m_1d_structures,  only: initialize_structures_actual_params
   use m_oned_functions, only: set_max_volume_for_1d_nodes
   use m_structures
   use m_longculverts
   use timers,           only : timstrt, timstop
   use m_sethu
   use m_external_forcings
   use m_1d2d_fixedweirs, only : n_1d2d_fixedweirs, realloc_1d2d_fixedweirs, initialise_1d2d_fixedweirs
   use m_fm_icecover, only: ice_apply_pressure, ice_p, fm_ice_update_press
   use fm_manhole_losses, only: init_manhole_losses
   use unstruc_channel_flow, only: network
   use m_fixedweirs, only: weirdte, nfxw
   
   implicit none

   integer           :: ierror
   logical           :: jawelrestart

   double precision  :: upot, ukin, ueaa
   
   integer, external :: flow_initexternalforcings

   double precision, allocatable :: weirdte_save(:)
      
   error = DFM_NOERR

   if (ndx == 0) then
      error = DFM_MODELNOTINITIALIZED
   end if
   if( is_error_at_any_processor(error) ) then
       return
   end if

   Lnmax = 0
   Lnmin = 0
   ndmax = 0
   ndmin = 0

   call inisferic()
 
   if (icorio > OFF ) then
      call inifcori()
   end if
 
   call initialize_for_coriolis_Adams_Bashforth()

   if (jsferic == OFF) then
   !Tide generating potential and Self attraction and loading are only supported for spherical models
      jatidep  = OFF
      jaselfal = OFF
   end if
 
   call inidensconstants()                             ! Some density parameters

   if (ti_waq > 0d0 .and. max(limtypmom, limtypsa, limtypTM) <= 0) then
      call qnerror('DELWAQ requires at least one limiter (Numerical Parameters). DELWAQ output disabled for now.', ' ', ' ')
      ti_waq = 0d0
   end if

   call initialize_water_level()
   call initialize_velocity_with_uniform_value()
   call initialize_salinity_for_SAL_with_uniform_value()
   call initialize_temperature_with_uniform_value()
   call initialize_spiral_flow_with_uniform_value()
   call initialize_sediment()

   if (jasal == OFF .and. jatem == OFF .and. jased == OFF) then
      idensform = OFF
   endif
 
   volerror(:) = 0d0
   squ(:)      = 0
   sqi(:)      = 0

   call str_lower(md_netfile) ! INTERACTOR!

   if (md_input_specific > 0 ) then
       call apply_hardcoded_specific_input()
   end if 

   call statisticsini()

   call setkbotktop(1)                               ! prior to correctblforzlayerpoints, setting kbot

   call mess(LEVEL_INFO, 'Start initializing external forcings...')
   call timstrt('Initialise external forcings', handle_iniext)
   error = flow_initexternalforcings()               ! this is the general hook-up to wind and boundary conditions
   call timstop(handle_iniext)
   if( is_error_at_any_processor(error) ) then
      call qnerror('Error occurred while initializing external forcings.',&
          ' Please inspect the preceding lines in the diagnostic output for more details.', ' ')
      return
   end if
   call mess(LEVEL_INFO, 'Done initializing external forcings.')

   call set_ihorvic_related_to_horizontal_viscosity()
   call redimension_summ_arrays_in_crs()
   call set_fixed_weirs()
   call delpol() 
   call set_advection_type_for_slope_large_than_Slopedrop2D()
   call set_advection_type_for_lateral_flow_and_pipes()

   if (jaPure1D > OFF) then
      call setiadvpure1D(jaPure1D)
   end if

  ! check if at most one structure claims a flowlink
   call check_structures_and_fixed_weirs()

   ! First call to setexternalforcingsonboundaries, here only for the structure timeseries (prior to adjust_bobs_for_dams_and_structs())
   call setzminmax()                                 ! our side of preparation for 3D ec module
   call setsigmabnds()
   call flow_setexternalforcingsonboundaries(tstart_user, error)  ! set structure (and bnd) external forcings. Error handling later in 2nd call for bnds. 
   call initialize_structures_actual_params(network%sts)          ! After structure time series, and prior to adjust_bobs, to use proper crest levels.

   call adjust_bobs_for_dams_and_structs()
   call setup_structures_and_weirs_list()
   call set_floodfill_water_levels_based_on_sample_file()

   if (allocated(ibot)) then
      deallocate(ibot)                                    ! after meteoiniti of ibedlevtype
   end if

   call setFrictionForLongculverts()
   call set_friction_coefficient_by_initial_fields()
   call set_friction_uniform_value_on_links_where_friction_is_not_set()
   call set_internal_tides_friction_coefficient()
   call setupwslopes()                                   ! set upwind slope pointers and weightfactors

   if (iuvfield > OFF) then
       call setvelocityfield()           ! only when testing
   end if

   call remember_initial_water_levels_at_water_level_boundaries()
   call make_volume_tables()
   call load_restart_file(jawelrestart, error)
   if (is_error_at_any_processor(error) ) then
       call qnerror('Error occurs when reading the restart file.',' ', ' ')
       return
   end if
   if (jawelrestart) jarestart = ON                                       ! in the module

   call flow_setstarttime()                                     ! the flow time0 and time1 are managed by flow
                                                                ! this is the only function that a user can use to influence the flow times
                                                                ! TSTART MAY BE OVERWRITTEN IN REARST

   call initialize_morphological_start_time()

   call setkbotktop(1)                                            ! set sigmabnds for ec

   if ( janudge == ON ) then
       call setzcs()
   end if
   call set_external_forcings(tstart_user, INITIALIZATION_PHASE, error)
   if( is_error_at_any_processor(error) ) then
       call qnerror('Error occurred when setting external forcings.',' ', ' ')
       return
   end if

   if (len_trim(md_restartfile) == 0 ) then
       if (ice_apply_pressure) then
          call fm_ice_update_press(ag)
          s1 = s1 - ice_p / (ag*rhomean)
          s0 = s1
          hs     = s0 - bl
       endif
   end if

   ! Actual boundary forcing (now that initial water levels, etc. are also known):
   call flow_setexternalforcingsonboundaries(tstart_user, error)         ! set bnd   oriented external forcings
   if( is_error_at_any_processor(error) ) then
       call qnerror('Error occurred when setting external forcings on boundaries.',' ', ' ')
       return
   end if
   tim1bnd = tstart_user
   tim1fld = tstart_user

   if (jaoldrstfile == ON) then ! If the restart file is of old version (which does not have waterlevel etc info on boundaries), then need to set.
      call sets01zbnd(0, 0)
   end if
   if (.not. jawelrestart) then
      call sets01zbnd(1, 1)
   endif 
   
   call initialize_values_at_normal_velocity_boundaries()
   call initialize_values_at_discharge_boundaries()
   call copy_boundary_friction_and_skewness_into_flow_links()
   call set_boundaries_implicit()
   call set_implicit_for_pipes()
   call set_structure_links_implicit()

   if (.not. jawelrestart) then
      call correction_s1_for_atmospheric_pressure()
      call correction_s1init_for_self_attraction()
      u0(:) = u1(:)
   end if

   s1(:)  = max(bl(:), s1(:))
   s00(:) = s1(:)

   nonlin = max(nonlin1D, nonlin2D)
   if (nonlin >= 2) then
      if (allocated(s1m) ) deallocate (s1m, a1m)
      allocate ( s1m(ndx), a1m(ndx) , STAT=ierror)
      call aerr('s1m(ndx), a1m(ndx)', ierror, ndx)
      s1m(:) = s1(:)
   end if

   call set_data_for_ship_modelling()

   call setkbotktop(1)

   if (.not. jawelrestart) then
    call update_s0_and_hs()
   endif
   
   if ( jaselfal > OFF ) then
  !  with hs available: recompute SAL potential
     call flow_settidepotential(tstart_user/60d0)
   end if

   call include_ground_water()
   call include_infiltration_model()

   if (nfxw > 0) then 
      allocate ( weirdte_save(nfxw), STAT=ierror)
      weirdte_save=weirdte
   endif
   call calculate_hu_au_and_advection_for_dams_weirs(SET_ZWS0,.not.jawelrestart)
   if (nfxw > 0) then 
       weirdte=weirdte_save
      deallocate ( weirdte_save)
   endif
   call temporary_fix_for_sepr_3D()

   call volsur()
   call a1vol1tot()
   vol0tot = vol1tot
   a0tot   = a1tot
   vol0(:) = vol1(:)

   call set_initial_velocity_in_3D()
   call set_wave_modelling()
   call initialize_salinity_from_bottom_or_top()
   call initialize_temperature_3D()
   call initialize_sediment_3D()

 ! hk: and, make sure this is done prior to fill constituents
   if (jarestart > OFF) then
      call initialize_salinity_temperature_on_boundary()
      call restore_au_q1_3D_for_1st_history_record()
   end if

   call initialize_salinity_and_temperature_with_nudge_variables()

   if (jasal > OFF) then 
       call fill_constituents_with_salinity()
   end if
   if (itemp > OFF) then 
      call fill_constituents_with_temperature()
   end if

   call initialise_density_at_cell_centres()

   if (allocated (rho0))  then
      rho0(:) = rho(:)
   end if

   if (jaFlowNetChanged == ON .or. nodtot /= ndx .or. lintot /= lnx) then
       call reducept(Ndx,Ndxi,Lnx)                              ! also alloc arrays for reduce
       if (icgsolver == 10) then
          call alloc_jacobi(ndx,lnx)
       end if
   end if

   if (kmx < 2) then                    ! in 2D, use 1
      if ( ja_timestep_auto /= -123 ) then
         ja_timestep_auto = ON
      else
         ja_timestep_auto = OFF
      end if
   end if

   if ( jaimplicit == ON ) then
      call inisolver_advec(ierror)
   end if

   call ini_filter(jafilter, filterorder, jacheckmonitor, ierror)

   if (jabarrieradvection == 3) then
      call setstruclink()
   end if

   if (japillar > OFF) then
      call setpillars()
   end if

   ! for 1D only
   if (network%loaded .and. ndxi-ndx2d > 0) then
      if (jamapVolOnGround > 0) then
         call set_max_volume_for_1d_nodes() ! set maximal volume, it will be used to update the volume on ground level for the output
      end if
   end if

   call upotukinueaa(upot, ukin, ueaa)
   call init_manhole_losses(network%storS)
   
end function flow_flowinit
 

!> is_error_at_any_processor
logical function is_error_at_any_processor(error)
   use dfm_error
   use m_partitioninfo, only : jampi, reduce_int1_max
   use m_cell_geometry, only : ndx
   
   implicit none

   integer, intent(inout) :: error
   
   is_error_at_any_processor = .false.
   if (jampi == ON) then
      call reduce_int1_max(error)
   end if
   if (error /= DFM_NOERR) then
      is_error_at_any_processor = .true.
      ndx = 0 ! to be consistent with the old way
      if (jampi == ON) then
          call qnerror('Error occurs on one or more processes with ...',' ', ' ')
      end if
   end if
   
end function is_error_at_any_processor
 
!> initialize fvcoro array for Adams/Bashforth
subroutine initialize_for_coriolis_Adams_Bashforth()
   use m_flowparameters, only : Corioadamsbashfordfac
   use m_flow,           only : lnkx, fvcoro
   use m_alloc,          only : aerr
   
   implicit none

   integer :: error

   if (Corioadamsbashfordfac > OFF) then
      if (allocated(fvcoro) ) deallocate(fvcoro)
      allocate ( fvcoro(lnkx), stat = error )
      call aerr('fvcoro(lnkx)', error, lnkx ) 
      fvcoro(:) = 0d0
   end if
 
end  subroutine initialize_for_coriolis_Adams_Bashforth
 

!> initialize_water_level
subroutine initialize_water_level()
   use m_flowparameters, only : sini, waterdepthini1D
   use m_flow,           only : s1
   use m_flowgeom,       only : bl, ndxi
   use m_cell_geometry,  only : ndx2D
   
   implicit none

   integer :: cell

   s1(:)  = sini
   if (waterdepthini1D > OFF ) then
      do cell = ndx2D + 1, ndxi
         s1(cell) = bl(cell) + waterdepthini1D
      end do
   end if
 
end subroutine initialize_water_level

!> initialize_velocity_with_uniform_value
subroutine initialize_velocity_with_uniform_value()
   use m_flowparameters, only : uini
   use m_flow,           only : u1
   implicit none

   u1(:)  = uini

end subroutine initialize_velocity_with_uniform_value

!> initialize salinity with uniform value when SAL is used
subroutine initialize_salinity_for_SAL_with_uniform_value()
   use m_flowparameters, only : jasal, salini
   use m_flow,           only : sa1
   
   implicit none

   if (jasal > OFF) then
       sa1(:) = salini
   end if

end subroutine initialize_salinity_for_SAL_with_uniform_value


!> initialize_temperature_with_uniform_value
subroutine initialize_temperature_with_uniform_value()
   use m_flowparameters, only : jatem, temini
   use m_flow,           only : tem1
   
   implicit none

   if (jatem > OFF) then
       tem1(:) = temini
   end if

end subroutine initialize_temperature_with_uniform_value


!> initialize spiral flow
subroutine initialize_spiral_flow_with_uniform_value()
   use m_flowparameters, only : jasecflow, spirini
   use m_flow,           only : spirint
   
   implicit none

    if (jasecflow > OFF ) then
       spirint(:) = spirini
    end if
 
end subroutine initialize_spiral_flow_with_uniform_value

!> initialize sediment
subroutine initialize_sediment()
   use m_flowparameters, only : jased
   use m_flow,           only : ndkx
   use m_sediment,       only : mxgr, sed, sedini
   
   implicit none

   integer, parameter :: KRONE    = 1
   integer, parameter :: SVR      = 2
   integer, parameter :: ENGELUND = 3
  
   integer :: cell
   integer :: grain

   if (jased == KRONE .or. jased == SVR .or. jased == ENGELUND) then
      do cell = 1, ndkx
         do grain = 1, mxgr
            sed(grain,cell) = sedini(grain)
         end do
      end do
   end if

end subroutine initialize_sediment
 
!> Set ihorvic related to horizontal viscosity
subroutine set_ihorvic_related_to_horizontal_viscosity()
   use m_flowparameters, only : ihorvic, javiusp
   use m_physcoef,       only : vicouv, Smagorinsky, Elder
   
   implicit none

   ihorvic = OFF
   if (vicouv > OFF .or. javiusp == ON .or. Smagorinsky > OFF .or. Elder > OFF) then
      ihorvic = ON
   end if
 
end subroutine set_ihorvic_related_to_horizontal_viscosity


!> If constituents have been added at this point, the sum-arrays in crs require redimensioning
subroutine redimension_summ_arrays_in_crs()
   use m_monitoring_crosssections, only : crs, ReallocCrosssectionSums
   
   implicit none

   if (allocated(crs)) then
      call ReallocCrossSectionSums(crs)
   end if

end subroutine redimension_summ_arrays_in_crs

!> set fixed weirs
subroutine set_fixed_weirs()
   use m_flowparameters, only : isimplefixedweirs
   use m_1d2d_fixedweirs, only : n_1d2d_fixedweirs, realloc_1d2d_fixedweirs, initialise_1d2d_fixedweirs
   
   implicit none

   integer, parameter :: COMPLETE_CROSSECTION_PATHS_STORED = 0

   if (isimplefixedweirs == COMPLETE_CROSSECTION_PATHS_STORED) then
      call setbobs_fixedweirs()
   else
      call setfixedweirs()
      if (n_1d2d_fixedweirs > 0) then
         call realloc_1d2d_fixedweirs()
         call initialise_1d2d_fixedweirs()
      endif
   end if
 
end subroutine set_fixed_weirs


!> set advection type for slope large than Slopedrop2D
subroutine set_advection_type_for_slope_large_than_Slopedrop2D()
   use m_flowparameters, only : Slopedrop2D
   use m_flowgeom,       only : lnx1D, lnxi, ln, iadv, dxi, bl
   
   implicit none

   integer            :: link

   if (Slopedrop2D > 0d0 ) then !todo, uitsluitende test maken
      do link  = lnx1D + 1, lnxi
         if (iadv(link) /= OFF .and. &
             .not. (iadv(link) >= 21 .and. iadv(link) <=25) .and. &
             dxi(link)*abs(bl(ln(1,link)) - bl(ln(2,link))) > Slopedrop2D) then ! Not for fixed weirs itself.
            iadv(link) = ORIGINAL_LATERAL_OVERFLOW_ADVECTION
         end if
      end do
   end if

end subroutine set_advection_type_for_slope_large_than_Slopedrop2D
 
!> set advection type for slope large than Slopedrop2D
subroutine set_advection_type_for_lateral_flow_and_pipes()
   use m_flowparameters, only : iadveccorr1D2D
   use m_flowgeom,       only : lnxi, iadv, kcu

   implicit none

   integer            :: link

   do link  = 1, lnxi
       if (iadv(link) /= OFF) then
          if (kcu(link) == LATERAL_1D2D_LINK) then
             if (iadveccorr1D2D == 2) then
                iadv(link) = OFF
             else
                iadv(link) = ORIGINAL_LATERAL_OVERFLOW_ADVECTION
             end if
          else if (kcu(link) == STREET_INLET_1D2D_LINK .or. kcu(link) == ROOF_GUTTER_1D2D_LINK) then
             iadv(link) = ORIGINAL_LATERAL_OVERFLOW_ADVECTION
          end if
       end if
    end do
 
end subroutine set_advection_type_for_lateral_flow_and_pipes

!> Floodfill water levels based on sample file.
subroutine set_floodfill_water_levels_based_on_sample_file()
   use unstruc_model,      only : md_s1inifile
   use m_partitioninfo,    only : jampi
   use iso_varying_string, only : len_trim
   use m_samples,          only : NS, restoresam, savesam
   use MessageHandling,    only : LEVEL_WARN, mess
   
   implicit none

   integer :: msam

   if (len_trim(md_s1inifile) > 0) then
      call savesam()
      NS = 0
      call oldfil(msam, md_s1inifile)
      if (msam /= 0) then
          call reasam(msam, 0)
          if (jampi == ON ) then
              call mess(LEVEL_WARN, 'Filling water level using [geometry] WaterLevIniFile in .mdu does not exchange information between partitions')
          end if
          call flow_initfloodfill()
      end if
      call restoresam()
   end if

end subroutine set_floodfill_water_levels_based_on_sample_file

!> sert friction coefficient by initial fields
subroutine set_friction_coefficient_by_initial_fields()
   use m_flowgeom,    only : lnx, lnx1D, kcu
   use m_flow,        only : frcu, ifrcutp
   use m_physcoef,    only : frcuni1d, frcuni1d2d, frcunistreetinlet, frcuniroofgutterpipe, frcuni, frcmax, ifrctypuni
   use m_missing,     only : dmiss, imiss

   implicit none

   integer, parameter :: MANNING = 1

   integer            :: link 

    do link = 1, lnx
       if (frcu(link) == dmiss) then
          if (link <= lnx1D) then
             if (kcu(link) == LATERAL_1D2D_LINK) then
                frcu(link)  = frcuni1d2d
             else if (kcu(link) == STREET_INLET_1D2D_LINK) then
                ! Because frcunistreetinlet is not available in the mdu file, the friction type is always manning.
                frcu(link)    = frcunistreetinlet
                ifrcutp(link) = MANNING
             else if (kcu(link) == ROOF_GUTTER_1D2D_LINK) then
                ! Because frcuniroofgutterpipe is not available in the mdu file, the friction type is always manning
                frcu(link)    = frcuniroofgutterpipe
                ifrcutp(link) = MANNING
             else
                frcu(link)  = frcuni1d
             end if
          else
             frcu(link) =  frcuni
          end if
       end if
       if (ifrcutp(link) == imiss) then
           ifrcutp(link) = ifrctypuni
       end if
       if (frcu(link) > frcmax) then
           frcmax = frcu(link)
       end if
    end do
 
end subroutine set_friction_coefficient_by_initial_fields


!> set friction uniform value on links where_friction_is_not_set
subroutine set_friction_uniform_value_on_links_where_friction_is_not_set()
   use m_flowparameters, only : jafrculin
   use m_flowgeom,       only : lnx
   use m_flow,           only : frculin
   use m_physcoef,       only : frcunilin
   use m_missing,        only : dmiss
   
   implicit none

   integer, parameter ::  USE_LINEAR_FRICTION = 1

   integer            :: link 

   if (jafrculin == USE_LINEAR_FRICTION) then
       do link = 1, lnx
          if (frculin(link) == dmiss) then
              frculin(link) =  frcunilin
          end if
       end do
   end if

end subroutine set_friction_uniform_value_on_links_where_friction_is_not_set

!> set internal tides friction coefficient
subroutine set_internal_tides_friction_coefficient()
   use m_flowparameters, only : jaFrcInternalTides2D
   use m_flow,           only : frcInternalTides2D
   use m_cell_geometry,  only : ndx
   use m_missing,        only : dmiss

   implicit none

   integer, parameter :: USE_INTERNAL_TIDES_FRICTION = 1

   integer            :: cell 

   if ( jaFrcInternalTides2D == USE_INTERNAL_TIDES_FRICTION ) then
      do cell = 1, Ndx
         if ( frcInternalTides2D(cell) == dmiss ) then
            frcInternalTides2D(cell) = 0d0
         end if
      end do
   end if
 
end subroutine set_internal_tides_friction_coefficient


 !> remember initial water levels at the water level boundaries
 !! so that reading rst file won't influence it. This is used for restart a model with Riemann boundary conditions.
subroutine remember_initial_water_levels_at_water_level_boundaries()
   use m_flow,                 only : s1
   use m_flowgeom,             only : bl
   use m_flowexternalforcings, only : nbndz, kbndz, zbndz0

   implicit none

   integer :: index 

    do index = 1, nbndz
       zbndz0(index) = max(bl(kbndz(2,index)), s1(kbndz(2,index))) ! NOTE: the s1=max(bl, s1) step can only be done later, so do it here as well.
    end do
 
end subroutine remember_initial_water_levels_at_water_level_boundaries

!> make volume tables 
subroutine make_volume_tables()
   use MessageHandling,      only : Idlen
   use unstruc_channel_flow, only : useVolumeTables
   use unstruc_files,        only : defaultFilename
   use Timers,               only : timstrt, timstop
   use m_VolumeTables,       only : makeVolumeTables
   
   implicit none

   integer              :: ihandle
   character(len=Idlen) :: fileName

   ihandle = 0
   call timstrt('makeVolumeTables', ihandle)
   if (useVolumeTables) then
       filename = defaultFilename('volumeTables')
       call makeVolumeTables(filename)
   end if
   call timstop(ihandle)

end subroutine make_volume_tables
 
!> Load restart file (*_map.nc) assigned in the *.mdu file OR read a *.rst file
subroutine load_restart_file(file_exist, error)
   use m_flowparameters,   only : jased, iperot
   use m_flow,             only : u1, u0, s0, hs, s1, ucxyq_read_rst
   use m_flowgeom,         only : bl
   use m_sediment,         only : stm_included
   use unstruc_model,      only : md_restartfile
   use iso_varying_string, only : len_trim, index
   use m_setucxcuy_leastsquare, only: reconst2nd
   use dfm_error
   
   implicit none

   logical, intent(out)          :: file_exist
   integer, intent(out)          :: error

   integer, parameter            :: NOT_DEFINED = -1
   character(len=255)            :: rstfile
   integer                       :: mrst
   integer                       :: jw
   double precision, allocatable :: u1_tmp(:)
   
   file_exist = .false.

   if (len_trim(md_restartfile) > 0 ) then
      ! Restart from *.rst:
      if ( index(md_restartfile, '.rst') > 0 .or. index(md_restartfile, '.RST') > 0) then
          inquire(FILE = rstfile, EXIST=file_exist)
          if (file_exist) then
             call oldfil(mrst, rstfile)
             call rearst(mrst, jw)
             file_exist = (jw == ON)
          end if
      else ! Restart from *_yyyymmdd_hhmmss_rst.nc or from *_map.nc
         call read_restart_from_map(md_restartfile, error)
         if (jased > OFF .and. stm_included) then
            call setbobs()
         end if
         if (error /= DFM_NOERR) then
            return
         else
            file_exist = .true.
         end if
         
         hs(:)   = s1(:) - bl(:)
         if (iperot == NOT_DEFINED ) then
            call reconst2nd ()
         end if
         call fill_onlyWetLinks()
         
         !If we have not read `ucxq` and `ucyq` from restart, we initialize it here. If 
         !we have read it, we do not want to overwrite it. 
         if (.not. ucxyq_read_rst) then
            call setucxucyucxuucyunew() !reconstruct cell-center velocities
         endif
         
         call flow_obsinit() 
         call fill_valobs() 
       end if
   end if

end subroutine load_restart_file


!> if the morphological start time is not set to some positive value due
!! to restart from mapfile, then make sure that the morphological start
!! time corresponds to the hydrodynamic start time. This includes TStart!
subroutine initialize_morphological_start_time()
   use m_flowparameters,   only : jased, eps10
   use m_sediment,         only : stm_included, stmpar
   use m_flowtimes,        only : tstart_user

   implicit none

   if (jased > OFF .and. stm_included) then
      if (stmpar%morpar%morft < eps10) then
          stmpar%morpar%morft  = tstart_user/86400d0
          stmpar%morpar%morft0 = stmpar%morpar%morft
      end if
   end if

end subroutine initialize_morphological_start_time
 
 
!> for normal velocity boundaries, also initialise velocity on link
subroutine initialize_values_at_normal_velocity_boundaries()
   use m_flowexternalforcings, only : nbndn, kbndn, zbndn
   use m_flow,                 only : u1

   implicit none

   integer   :: velocity_point
   integer   :: link
   integer   :: flow_link
   integer   :: bottom_link
   integer   :: top_link

   do velocity_point  = 1, nbndn                                  
       flow_link      = kbndn(3, velocity_point)
       call getLbotLtop(flow_link, bottom_link, top_link)
       do link        = bottom_link, top_link
          u1(link)    = zbndn(velocity_point)
       end do
   end do

end subroutine initialize_values_at_normal_velocity_boundaries

!> initialize discharge boundaries
subroutine initialize_values_at_discharge_boundaries()
   use m_flowparameters,       only : epshu
   use m_flowexternalforcings, only : nqbnd, L1qbnd, L2qbnd, kbndu
   use m_flowgeom,             only : bob
   use m_flow,                 only : s1

   implicit none

   integer          :: discharge_boundary
   integer          :: point
   integer          :: flow_link

   logical          :: dry

   double precision :: bob_local_min
   double precision :: bob_global_min

   do discharge_boundary = 1, nqbnd
      dry = .true.
      bob_global_min = huge(1d0)
      do point      = L1qbnd(discharge_boundary), L2qbnd(discharge_boundary)             
         flow_link  = kbndu(3,point)
         bob_local_min = min( bob(1,flow_link), bob(2,flow_link) )
          if (s1(kbndu(2,point)) - bob_local_min > epshu) then
             dry = .false.
          end if
          bob_global_min = min(bob_global_min, bob_local_min)
       end do

       do point = L1qbnd(discharge_boundary), L2qbnd(discharge_boundary)  
          if ( dry ) then
           !   boundary is dry: add 1 cm of water above lowest bed level
             s1(kbndu(2,point)) = max(s1(kbndu(2,point)), bob_global_min + 0.01d0)
          end if
          s1(kbndu(1,point)) = s1(kbndu(2,point))
       end do
   end do
 
end subroutine initialize_values_at_discharge_boundaries

!>  copy 1D bnd arrays to that of internal attached link
subroutine copy_boundary_friction_and_skewness_into_flow_links()
   use m_flowparameters, only : jaconveyance2D
   use m_flowgeom,       only : lnxi, lnx, kcu, Lbnd1D, aifu
   use m_flow,           only : frcu, ifrcutp

   implicit none

   integer            :: flow_link
   integer            :: boundary_link

   do flow_link = lnxi + 1, lnx  
      if ( kcu(flow_link) == BOUNDARY_1D ) then
         boundary_link      = Lbnd1D(flow_link)
         frcu(flow_link)    = frcu(boundary_link)
         ifrcutp(flow_link) = ifrcutp(boundary_link)
         if (jaconveyance2D > OFF) then
             aifu(flow_link) = aifu(boundary_link)
         end if
      end if
   end do
 
end subroutine copy_boundary_friction_and_skewness_into_flow_links

!> boundaries always implicit
subroutine set_boundaries_implicit()
   use m_flowgeom,       only : lnxi, lnx, teta

   implicit none

   if (lnx > lnxi) then                               
      teta(lnxi+1:lnx) = 1d0
   endif
 
end subroutine set_boundaries_implicit


!> set_implicit_for_pipes
subroutine set_implicit_for_pipes()
   use m_flowparameters, only : nonlin1d, nonlin2D
   use m_flowgeom,       only : kcu, lbnd1d, lnx1D, ln, lnxi, lnx, prof1d, teta
   use m_missing,        only : dmiss

   implicit none
   
   integer, parameter :: CIRCLE      = 1
   integer, parameter :: RECTANGLE   = 2
   integer, parameter :: RECTANGLE2  = 3
   integer, parameter :: CLOSED_PIPE = 2

   integer   :: link1D
   integer   :: boundary_link

   if (nonlin1d == CLOSED_PIPE .or. nonlin1d == 3 .or. nonlin2D == 2) then
      do link1D = 1, lnx1D
         if ( abs(prof1d(3,link1D)) == CIRCLE ) then
            teta(link1D)    = 1d0                          ! closed pipes always implicit
         else if ( abs(prof1d(3,link1D)) == RECTANGLE .or. abs(prof1d(3,link1D)) == RECTANGLE2 ) then
            if ( prof1d(2,link1D) /= dmiss ) then
                teta(link1D) = 1d0
            end if
         end if
      end do
      do boundary_link = lnxi + 1, lnx
         if (kcu(boundary_link) == BOUNDARY_1D ) then
            link1D = lbnd1d(boundary_link)
            if ( abs(prof1d(3,link1D)) == CIRCLE ) then
               teta(boundary_link)   = 1d0                          ! closed pipes always implicit
            else if ( abs(prof1d(3,link1D)) == RECTANGLE .or. abs(prof1d(3,link1D)) == RECTANGLE2 ) then
               if ( prof1D(2,link1D) /= dmiss ) then
                   teta(link1D) = 1d0
               end if
            end if
         end if
      end do
 end if
 
end subroutine set_implicit_for_pipes


!> Set teta for all structure links to 1.0 (implicit)
subroutine set_structure_links_implicit()
   use m_flowgeom,           only : teta
   use m_1d_structures,      only : t_structure
   use unstruc_channel_flow, only : network

   implicit none

   integer                    :: structure_number
   integer                    :: link
   integer                    :: link_index
   type(t_structure), pointer :: pstru

   do structure_number = 1, network%sts%count
      pstru => network%sts%struct(structure_number)
      do link_index = 1, pstru%numlinks
         link = iabs(pstru%linknumbers(link_index))
         teta(link) = 1d0
      end do
   end do

end subroutine set_structure_links_implicit

!> correction_s1_for_atmospheric_pressure
subroutine correction_s1_for_atmospheric_pressure()
   use m_physcoef,       only : ag, rhomean
   use m_flowgeom,       only : ndxi
   use m_flow,           only : s1
   use m_wind,           only : japatm, PavIni, patm

   implicit none

   double precision, parameter :: ZERO_AMBIENT_PRESSURE  = 0d0

   integer           :: cell
   double precision  :: ds

   if (japatm > OFF .and. PavIni > ZERO_AMBIENT_PRESSURE ) then
       do cell     = 1, ndxi
          ds       = - ( patm(cell) - PavIni ) / (ag*rhomean)
          s1(cell) = s1(cell) + ds
       end do
    end if
    

end subroutine correction_s1_for_atmospheric_pressure

!> correction_s1_for_atmospheric_pressure
subroutine correction_s1init_for_self_attraction()
   use m_flowparameters, only : jaselfal, jaSELFALcorrectWLwithIni
   use m_flowgeom,       only : ndxi, bl
   use m_flow,           only : s1, s1init

   implicit none

   integer    :: cell

    if  ( jaselfal > OFF .and. &
          jaSELFALcorrectWLwithIni == ON ) then
       do cell = 1, ndxi
           s1init(cell) = max(s1(cell), bl(cell))
       end do
   end if
 
end subroutine correction_s1init_for_self_attraction

!> set_data_for_ship_modelling
subroutine set_data_for_ship_modelling()
   use m_ship,                 only : nshiptxy
   use m_flowparameters,       only : jasal
   use m_flow,                 only : kmx, ndkx, sa1
   use m_flowexternalforcings, only : success
   
   implicit none

   if (nshiptxy > 0) then
       call setship()                               ! in flowinit
       if (kmx > 0 .and. jasal > OFF ) then
          inquire(file = 'verticalsalinityprofile.pli', exist=success )
          call setkbotktop(1)
          if (success) then
             call setinitialverticalprofile(sa1 , ndkx, 'verticalsalinityprofile.pli')
          end if
       end if
   end if

end subroutine set_data_for_ship_modelling

!> update_s0_and_hs
subroutine update_s0_and_hs()
   use m_flow,           only : s1, s0, hs
   use m_flowgeom,       only : bl

   implicit none

   s0(:) = s1(:)
   hs(:) = s0(:) - bl(:)
 
end subroutine update_s0_and_hs

!> include_ground_water
subroutine include_ground_water()
   use m_grw
   use m_cell_geometry,  only : ndx
   use m_flow,           only : hs
   use m_flowparameters, only : epshs
   use m_flow,           only : s1
   use m_flowgeom,       only : bl
   use m_hydrology_data, only : infiltrationmodel
   
   implicit none

   integer            :: cell
   double precision   :: hunsat
   double precision   :: fac

   if (jagrw <= OFF ) then
       return
   end if 

   do cell = 1, ndx

      if (hs(cell) > epshs) then
          sgrw1(cell) = bl(cell)
      else
          if (allocated(h_unsat) ) then
             sgrw1(cell) = bl(cell) - h_unsat(cell)
          else if (infiltrationmodel == ON) then
             sgrw1(cell) = bl(cell) - Hinterceptionlayer
          else if (h_unsatini > 0d0) then
              sgrw1(cell) = bl(cell) - h_unsatini
          else
             sgrw1(cell) = sgrwini
          end if
      end if
      sgrw1(cell) = min( bl(cell), sgrw1(cell) )
      bgrw(cell)  = min( bl(cell), bgrw (cell) )
      hunsat      = bl(cell) - sgrw1(cell)
      fac         = min ( 1d0, max(0d0,  hunsat / h_transfer  )  )      ! 0 at bed, 1 at sgrw
      pgrw(cell)  = sgrw1(cell)*fac + s1(cell)*(1d0-fac)

   end do

   if (allocated(h_unsat) ) deallocate(h_unsat)
   sgrw0(:) = sgrw1(:)

end subroutine include_ground_water
 
!> include_infiltration_model
subroutine include_infiltration_model()
   use m_hydrology_data, only : infiltrationmodel, DFM_HYD_INFILT_CONST, DFM_HYD_INFILT_DARCY, infiltcap
   use m_flowgeom,       only : kcsini, prof1D, lnx, ln, lnx1D
   use m_cell_geometry,  only : ndx
   use m_alloc!,        only : realloc

   implicit none

   integer     :: link
   integer     :: cell
   integer     :: left_cell
   integer     :: right_cell

   if (infiltrationmodel == DFM_HYD_INFILT_CONST .or. &
       infiltrationmodel == DFM_HYD_INFILT_DARCY) then  ! set infiltcap=0 for closed links only
       call realloc(kcsini, ndx, keepExisting=.false., fill = 0)
       do link = 1, lnx  ! only one connected open profile will open surface runoff
          left_cell  = ln(1,link)
          right_cell = ln(2,link)
          if (link <= lnx1D) then
             if (prof1D(3,link) < 0) then ! closed profile
             else
                 kcsini(left_cell)  = 1
                 kcsini(right_cell) = 1
             end if
          else
             kcsini(left_cell)  = 1
             kcsini(right_cell) = 1
          end if
       end do
       infiltcap(:) = infiltcap(:)*kcsini(:)  ! 0 for all links closed

       deallocate(kcsini) ! GM: why is it deallocated here?
   end if
 
end subroutine include_infiltration_model

!> temporary fix for sepr 3D
subroutine temporary_fix_for_sepr_3D()
   use m_flow,                 only : kmx, hu, au
   use m_flowgeom,             only : lnx, kcu, wu

   implicit none

   integer, parameter :: link_1D = 1

   integer            :: link

   if (kmx > 0) then 
      do link = 1, lnx
         if (abs(kcu(link)) == link_1D) then
            call addlink1D(link,1)
            if (hu(link) > 0d0) then
               wu(link) = au(link) / hu(link)
            end if
         end if
      end do
   end if
 
end subroutine temporary_fix_for_sepr_3D



!> set initial velocity in 3D (needs Lbot, Ltop)
subroutine set_initial_velocity_in_3D()
   use m_flow,                 only : kmx, u1
   use m_flowparameters,       only : inivel
   use m_flowgeom,             only : lnx

   implicit none

   integer            :: link
   integer            :: bottom_link
   integer            :: top_link

   if ( inivel == INITIALIZE .and. kmx > 0 ) then
       do link = 1, lnx
          call getLbotLtop(link, bottom_link, top_link)
          u1(bottom_link:top_link) = u1(link)
       end do
   end if
 
end subroutine set_initial_velocity_in_3D

!> set wave modelling
subroutine set_wave_modelling()
   use m_flowparameters,       only : jawave, flowWithoutWaves
   use m_flow,                 only : hs, hu, kmx, kmxn
   use mathconsts,             only : sqrt2_hp
   use m_waves,                only : hwavcom, hwav, gammax, twav, phiwav, ustokes, vstokes
   use m_flowgeom,             only : lnx, ln, csu, snu
   use m_sferic,               only : dg2rd

   implicit none
   
   integer, parameter :: SWAN  = 3
   integer, parameter :: CONST = 5
   integer, parameter :: SWAN_NetCDF = 6

   integer            :: link
   integer            :: left_node
   integer            :: right_node

   double precision   :: hw
   double precision   :: tw
   double precision   :: csw
   double precision   :: snw
   double precision   :: uorbi
   double precision   :: rkw
   double precision   :: ustt
   double precision   :: hh

   if ((jawave == SWAN .or. jawave == SWAN_NetCDF ) .and. .not. flowWithoutWaves) then
      ! Normal situation: use wave info in FLOW
      hs = max(hs, 0d0)
      if (jawave == SWAN_NetCDF) then
        ! HSIG is read from SWAN NetCDF file. Convert to HRMS
        hwav = hwavcom / sqrt2_hp
      else
        hwav = hwavcom
      end if
      hwav = min(hwav, gammax*hs)
   !
      call wave_uorbrlabda()                      
      if( kmx == 0 ) then
         call wave_comp_stokes_velocities()
         call tauwave()
      end if
      call setwavfu()
      call setwavmubnd()
   end if

   if ((jawave == SWAN .or. jawave == SWAN_NetCDF ) .and. flowWithoutWaves) then
      ! Exceptional situation: use wave info not in FLOW, only in WAQ
      ! Only compute uorb
      ! Works both for 2D and 3D
      if (jawave == SWAN_NetCDF ) then
        ! HSIG is read from SWAN NetCDF file. Convert to HRMS
        hwav = hwavcom / sqrt2_hp
      else
        hwav = hwavcom
      end if
      hwav = min(hwav, gammax*hs)
      call wave_uorbrlabda()                       ! hwav gets depth-limited here
   end if

   if (jawave == CONST .and. .not. flowWithoutWaves) then
      hs = max(hs, 0d0)
      hwav = min(hwavcom, gammax*hs)
      call wave_uorbrlabda()
      if( kmx == 0 ) then
         do link = 1, lnx
            left_node  = ln(1,link)
            right_node = ln(2,link)
            hh         = hu(link)
            hw         = 0.5d0*(hwav(left_node) + hwav(right_node))
            tw         = 0.5d0*(twav(left_node) + twav(right_node))
            csw        = 0.5*(cos(phiwav(left_node)*dg2rd) + cos(phiwav(right_node)*dg2rd))
            snw        = 0.5*(sin(phiwav(left_node)*dg2rd) + sin(phiwav(right_node)*dg2rd))
            call tauwavehk(hw, tw, hh, uorbi, rkw, ustt)
            ustokes(link) = ustt*(csu(link)*csw + snu(link)*snw)
            vstokes(link) = ustt*(-snu(link)*csw + csu(link)*snw)
         end do
         call tauwave()
      end if
   end if

end subroutine set_wave_modelling
 

!> initialize_salinity_from_bottom_or_top
subroutine initialize_salinity_from_bottom_or_top()
   use m_flowparameters,       only : jasal, inisal2D, uniformsalinitybelowz, Sal0abovezlev, salmax
   use m_flow,                 only : kmx, kmxn, sa1, satop, sabot, zws
   use m_cell_geometry,        only : ndx
   use m_flowtimes,            only : jarestart
   use m_missing,              only : dmiss

   implicit none

   integer, parameter :: SALINITY_TOP = 2
   integer, parameter :: SALINITY_BOT = 3

   integer            :: cell 
   integer            :: bottom_cell 
   integer            :: top_cell 
   integer            :: cell3D

   double precision   :: rr
   double precision   :: zz


   if (jasal <= OFF ) then
       return
   end if

   if (kmx > 0 .and. inisal2D > OFF .and. jarestart == OFF ) then
       do cell = 1, ndx
          call getkbotktop(cell, bottom_cell, top_cell)
          if (inisal2D == SALINITY_TOP ) then
             do cell3D = bottom_cell, top_cell
                if (top_cell == bottom_cell) then
                   rr  = 1d0
                else
                   rr  = dble(cell3D-bottom_cell)/dble(top_cell-bottom_cell)
                end if
                sa1(cell3D) = (1d0 - rr)*sa1(bottom_cell) + rr*satop(cell)
             end do
          else if (inisal2D == SALINITY_BOT ) then          ! uniform below is specified
             do cell3D = bottom_cell, top_cell
                zz = 0.5d0*( zws(cell3D) + zws(cell3D-1) )
                if (zz < uniformsalinitybelowz .and. sabot(cell) /= dmiss) then
                   sa1(cell3D) = sabot(cell)
                else
                   sa1(cell3D) = sa1(cell)
                end if
             end do
          end if
          do cell3D = top_cell + 1, bottom_cell + kmxn(cell) - 1
             sa1(cell3D) = sa1(max(top_cell,bottom_cell))
          end do
       end do

       if ( allocated(satop) ) then
          deallocate (satop)
       end if
       if ( allocated(sabot) ) then
          deallocate (sabot)
       end if
   end if

   where(sa1 < 0d0 )
       sa1 = 0d0
   end where

   if (Sal0abovezlev /= dmiss) then
       do cell = 1, ndx
          call getkbotktop(cell, bottom_cell, top_cell)
          do cell3D = bottom_cell, top_cell
             if (zws(cell3D) > Sal0abovezlev) then
                 sa1(cell3D) = 0d0
             end if
          end do
       end do
    end if

   salmax = maxval(sa1)

end subroutine initialize_salinity_from_bottom_or_top

!> initialize_temperature_3D
subroutine initialize_temperature_3D()
   use m_flow,                 only : kmx, tem1
   use m_flowparameters,       only : initem2D
   use m_cell_geometry,        only : ndx

   implicit none

   integer            :: cell 
   integer            :: bottom_cell 
   integer            :: top_cell 
   integer            :: cell3D

   if (kmx > 0 .and. initem2D > 0 ) then
      do cell = 1, ndx
         call getkbotktop(cell,bottom_cell,top_cell)
         do cell3D = bottom_cell, top_cell
             tem1(cell3D) = tem1(cell)
         end do
      end do
   end if
 
end subroutine initialize_temperature_3D

!> initialize_sediment_3D
subroutine initialize_sediment_3D()
   use m_flow,                 only : kmx
   use m_flowparameters,       only : inised2D
   use m_cell_geometry,        only : ndx
   use m_sediment,             only : mxgr, sed, sedh
   use m_missing,              only : dmiss

   implicit none

   integer            :: cell 
   integer            :: bottom_cell 
   integer            :: top_cell 
   integer            :: cell3D

   if (kmx > 0 .and. inised2D > 0 ) then
      do cell = 1, ndx
         if (sedh(cell) /= dmiss) then
            call getkbotktop(cell, bottom_cell, top_cell)
            do cell3D = bottom_cell, top_cell
                sed(1:mxgr,cell3D) = sedh(cell)
            end do
         end if
      end do
      deallocate(sedh)
      inised2D = 0
   end if
 
end subroutine initialize_sediment_3D

!> initialize salinity, temperature, sediment on boundary
subroutine initialize_salinity_temperature_on_boundary()
   use m_flowparameters,       only : jasal, jased, jatem
   use m_flowgeom,             only : ln, lnx, lnxi
   use m_flow,                 only : sa1, q1, tem1
   use m_sediment,             only : mxgr, sed

   implicit none

   integer   :: link
   integer   :: bottom_link
   integer   :: top_link
   integer   :: link3D

   integer   :: boundary_cell
   integer   :: internal_cell
   integer   :: grain

   do link = lnxi + 1, lnx                           ! copy on outflow
       call getLbotLtop(link,bottom_link,top_link)
       if (top_link < bottom_link) then
           cycle
       end if
       do link3D = bottom_link, top_link
           if (q1(link3D) <= 0d0) then
               boundary_cell = ln(1,link3D)
               internal_cell = ln(2,link3D)
               if (jasal > OFF) then
                   sa1(boundary_cell)  = sa1(internal_cell)
               end if
               if (jatem > OFF) then
                   tem1(boundary_cell)  = tem1(internal_cell)
               end if
           end if
       end do
   end do

end subroutine initialize_salinity_temperature_on_boundary


!> initialize salinity and temperature with nudge variables
subroutine initialize_salinity_and_temperature_with_nudge_variables()
   use m_flowparameters,       only : janudge, jainiwithnudge
   use m_nudge

   implicit none

   if ( janudge == ON ) then  ! and here last actions on sal/tem nudging, before we set rho
       call set_nudgerate()
       if ( jainiwithnudge > OFF ) then
          call set_saltem_nudge()
          if (jainiwithnudge == 2) then
              janudge = OFF
              deallocate (nudge_tem, nudge_sal, nudge_rate , nudge_time)
          end if
       end if
   end if

end subroutine initialize_salinity_and_temperature_with_nudge_variables


!> fill_constituents_with_salinity
subroutine fill_constituents_with_salinity()
   use m_flow,           only : ndkx, sa1
   use m_transportdata,  only : constituents, isalt

   implicit none

   integer                      :: node3D

   do node3D = 1, ndkx
       constituents(isalt,node3D) = max( 0d0,  sa1(node3D) )
   end do

end subroutine fill_constituents_with_salinity

!> fill_constituents_with_temperature
subroutine fill_constituents_with_temperature()
   use m_flow,           only : ndkx, tem1
   use m_transportdata,  only : constituents, itemp

   implicit none

   integer                      :: node3D

   do node3D = 1, ndkx
       constituents(itemp,node3D) = tem1(node3D)
   end do

end subroutine fill_constituents_with_temperature

!> initialise_density_at_cell_centres
subroutine initialise_density_at_cell_centres()
   use m_flowparameters,       only : jainirho
   use m_flow,                 only : kmxn
   use m_cell_geometry,        only : ndx
   use m_sediment,             only : stm_included
   use m_turbulence,           only : rhowat

   implicit none

   integer            :: cell 
   integer            :: bottom_cell 
   integer            :: top_cell 
   integer            :: cell3D

   if (jainirho == INITIALIZE) then
       do cell = 1, ndx
          call setrhokk(cell)
          if (stm_included) then
             call getkbotktop(cell, bottom_cell, top_cell)
             do cell3D = top_cell + 1, bottom_cell + kmxn(cell) - 1
                rhowat(cell3D) = rhowat(top_cell)   ! UNST-5170
             end do
           end if
       end do
   end if

end subroutine initialise_density_at_cell_centres

!> apply hardcoded specific input    
subroutine apply_hardcoded_specific_input()
 use m_netw
 use m_flowgeom
 use m_flow
 use m_flowtimes
 use m_sferic
 use unstruc_model
 use m_partitioninfo
 use geometry_module , only: dbdistance, half, normalout
 use m_sethu

 implicit none

 logical, parameter :: SET_HU      =  .true.
 
 integer          :: itest = 1, kk, La, j, Lb, Lt
 integer          :: k, L, k1, k2, n, jw, msam
 integer          :: kb, kt, LL

 double precision :: xzmin, xzmax, yzmin, yzmax
 double precision :: xx1, yy1, xx2, yy2, ux1, uy1, ux2, uy2, csl, snl
 double precision :: fout, foutk, aa, dis, dmu, var, rho1, zi, zido, ziup, saldo, salup
 double precision :: xx, yy, zz, ux, uy, pin, xli, slope, cs, cz, z00, cst
 double precision :: r, eer, r0, dep, Rossby, amp, csth, sqghi, snth
 double precision :: rr, rmx, x0, y0, dxx, dyy, ucmk, phi, dphi
 double precision :: xm, ym

 double precision, external :: rho_Eckart

 call dminmax(   xz, ndx, xzmin, xzmax, ndx)
 call dminmax(   xk, numk, xkmin, xkmax, numk)


 if (md_IDENT(1:6) == 'wetbed' .or. md_IDENT(1:6) == 'drybed') then  ! wetbed, drybed
    call setbobs()
    jw = ON
    if (md_IDENT(1:6) == 'drybed') then
       jw = OFF
    endif

    do k = 1, ndx
       if (xz(k) .le. 0.5d0*(xzmin+xzmax) ) then
          s1(k)  = bl(k) + 2d0
       else if (jw == ON) then
          s1(k)  = bl(k) + hwetbed
       else
          s1(k)  = bl(k)
       endif
    enddo

    if (kmx > 0) then
       call setkbotktop(1)  ! wetbed
       if (jasal /= OFF) then
          do k = 1,ndx
             if (xz(k) .le. 0.5d0*(xzmin+xzmax) ) then
                call getkbotktop(k,kb,kt)
                do kk = kb,kt
                   sa1(kk) = 2d0
                enddo
             endif
          enddo
       endif
    endif

 else if (md_IDENT(1:7) == 'barocin') then  ! baroclinic instability

    xx1 = 0.5d0*(xzmin+xzmax) ; yy1 = 0.5d0*(xzmin+xzmax)
    call setkbotktop(1)  ! barocin

    do k = 1,ndx
       rr = dbdistance( xx1, yy1, xz(k), yz(k), jsferic, jasfer3D, dmiss)
       if (rr < 3000d0 ) then
           call getkbotktop(k,kb,kt)
           do kk = kb+kmx/2,kt
              sa1(kk) = 1.1d0*(rr/3000d0)**8 + 33.75d0
           enddo
       endif
    enddo

 else if (md_IDENT(1:16) == 'internalseichexx') then  ! internal seiche hofmeister 2010

    call setkbotktop(1)  ! internalseichexx
    salup = 0d0 ; saldo = 30d0
    do k = 1,ndx
       zi = -10d0*( 1d0 - 0.2d0*sin( pi*xz(k)/(xkmax-xkmin) ) ) ; ziup = zi + 2d0 ; zido = zi - 2d0
       call getkbotktop(k,kb,kt)
       do kk = kb,kt
          zz = 0.5d0*( zws(kk) + zws(kk-1) )
          if (zz > ziup) then
             sa1(kk) = salup
          else if (zz < zido) then
             sa1(kk) = saldo
          else
             rr      = (zz - zido) / (ziup-zido)
             sa1(kk) = saldo*(1d0-rr) + salup*rr
          endif
       enddo
    enddo

 else if (md_IDENT  == 'hump' .or. md_IDENT  == 'humpc') then

    xx1 = 5000d0 ; yy1 = 5000d0
    var = 1d0    ; dmu = 0d0
    do k = 1,numk
       dis = dbdistance(xk(k), yk(k), xx1, yy1, jsferic, jasfer3D, dmiss)
       if (dis < 5d3) then
          xx = dis/1000d0
          yy = 5d0*1d0*sqrt(twopi*var)/sqrt(twopi*var)* exp( -(xx-dmu)**2/(2d0*var) )
          zk(k) = zk(k) + yy
       endif
    enddo
    call setbobs()

 else if (md_IDENT  == 'twohump') then

    xx1 = 5000d0 ; yy1 = 5000d0
    var = 1d0    ; dmu = 0d0

    do kk = 1,2
       if (kk == 1) then
           xx1 = 5000d0 ; yy1 = 6500d0
       else
           xx1 = 5000d0 ; yy1 = 3500d0
       endif

       do k = 1,numk
          dis = dbdistance(xk(k), yk(k), xx1, yy1, jsferic, jasfer3D, dmiss)
          if (dis < 5d3) then
             xx = dis/1000d0
             yy = 11d0*1d0*sqrt(twopi*var)/sqrt(twopi*var)* exp( -(xx-dmu)**2/(2d0*var) )
             zk(k) = zk(k) + yy
          endif
       enddo
    enddo

    call setbobs()

  else if (md_IDENT == '21' ) then

   s1(1) = s1(1) + 1d0

  else if (md_netfile(1:4) == 'rivs') then

    do k = 1,ndx
       if (xz(k) < 4.5d0 ) then
          s1(k) = s1(k) + 1d0
       endif
    enddo
    nplot = 450

 else if (md_netfile(1:4) == 'goot') then

    slope = 1d0/3004d0

    do k  = 1,ndx
       s1(k)  = -slope*( xz(k) - 1d0 )
    enddo

 else if (md_netfile(1:7) == 'evenaar') then

    bl = -5d0; s1 = 0
    ibedlevtyp   = 1 ; call setbobs()

 else if (index(md_ident,'saltwedge') > 0) then                   !


    call setkbotktop(1) ! inisaltwedge

    do k = 1,ndx
       if (xz(k) < 0.5*(xzmin+xzmax) ) then
           call getkbotktop(k,kb,kt)
           do kk = kb,kt
              sa1(kk) = 10d0
              rho1    = rho_Eckart(sa1(kk), backgroundwatertemperature)
           enddo
       else
           !s1(k) = bl(k) + 0.5d0*( s1(k)-bl(k) )*sqrt(rho1/998.200)   ! rho = 1020 etc
       endif
    enddo

 else if (index(md_ident,'salthori') > 0 .and. kmx > 0) then                   !

    call setkbotktop(1) ! ini vertical salinity gradient
     do k = 1,ndx
       call getkbotktop(k,kb,kt)
       do kk = kb,kt
          sa1(kk) = max(0d0, abs( 0.5d0*(zws(kk) + zws(kk-1))  )   )
       enddo
    enddo

 else if (index(md_ident,'lockexchange') > 0) then                   !

    call dminmax(   xz, ndx, xzmin, xzmax, ndx)
    call setkbotktop(1) ! inisaltwedge

    do k = 1,ndx
       call getkbotktop(k,kb,kt)
       do kk = kb,kt
          if (xz(k) > 0.5*(xzmin+xzmax) ) then
              sa1(kk) = 6.5d0
          else
              sa1(kk) = 5.0d0
          endif
       enddo
    enddo
 else if (index(md_ident,'locxx') > 0 .or. index(md_ident,'t0st') > 0 .or. trim(md_specific) == 'lockexchange') then                   ! Commented: It triggers on mdu-names that just include 'loc' in the name
                                                            !            For instance: 'locationDelft.mdu',
    call dminmax(   xz, ndx, xzmin, xzmax, ndx)             !                          'normalvelocities.mdu',   etc.

    if ( jampi == ON) then
       call reduce_double_min(xzmin)
       call reduce_double_max(xzmax)
    end if

    if ( index(md_ident,'locxxfix') > 0) then
       kplot = kmx-1
       do k = 1,ndx
          if (xz(k) < 0.5d0*(xzmin+xzmax) ) then
              s1 (k) = s1(k) - 6d0
          endif
       enddo
    else
        do k = 1,ndx
          if (xz(k) > 0.5d0*(xzmin+xzmax) ) then
              s1 (k) = s1(k) + hs(k)*.004d0*0.5d0
          endif
       enddo
    endif

    call setkbotktop(1) ! inisaltwedge

    do k = 1, ndx

       call getkbotktop(k,kb,kt)
       do kk = kb,kt
          if ( index(md_ident,'locxxfix') > 0) then
             if (kk == kb) then
                sa1(kk) = 1d0
             else
                sa1(kk) = 1d0
             endif
          else
             if (xz(k) > 0.5d0*(xzmin+xzmax) .and. (kk-kb+1) <= locsaltlev * kmx ) then
                sa1(kk) = locsaltmax
                if (jatem > 0) then
                   tem1(kk) = 5d0
                endif
             else
                sa1(kk) = locsaltmin
                if (jatem > 0) then
                   tem1(kk) = 10d0
                endif
          endif
          endif
          sa1(k)  = sa1(k) + vol1(kk)*sa1(kk)

          if (jatem > 0) then
             tem1(k)  = tem1(k) + vol1(kk)*tem1(kk)
          endif
       enddo
       sa1(k) = sa1(k) / vol1(k)
       if (jatem > 0) then
          tem1(k) = tem1(k) / vol1(k)
       endif
    enddo

  else if (trim(md_specific) == 'splitter') then
     jamodelspecific = ON
  else if (index(md_ident,'canal-lake') > 0 ) then

    call setkbotktop(1) ! inisaltwedge
    do k = 1, ndx

       call getkbotktop(k,kb,kt)
       do kk = kb,kt
          if (zws(kk) < -5d0 ) then
              sa1(kk) = 10d0
          endif
       enddo

    enddo

  else if (index(md_ident,'internalwave') > 0) then                   !

    call dminmax(   xz, ndx, xzmin, xzmax, ndx)
    call setkbotktop(1) ! inisaltwedge

    do k = 1,ndx
       call getkbotktop(k,kb,kt)
       do kk = kb,kt
          sa1(kk) = 0.001d0*xz(k) - (0.5d0*(zws(kk) + zws(kk-1)) - zws(kb-1) ) + 11d0
       enddo
    enddo

 else if (index(md_ident,'slope1_5') > 0) then                   !

    call dminmax(   xz, ndx, xzmin, xzmax, ndx)
    call setkbotktop(1) ! inisaltwedge
    sa1 = 5d0

    s1  = bl + 2d0

 else if (index(md_ident,'huump3d') > 0) then                         !

    call dminmax(   xz, ndx, xzmin, xzmax, ndx)
    call setkbotktop(1) !inihump3D

    do k = 1,ndx
       if (xz(k) < 0.5*(xzmin+xzmax) ) then
           call getkbotktop(k,kb,kt)
           do kk = kb,kt
              sa1(kk) = 10d0
           enddo
       else
          ! s1(k) = -10d0 + 10d0*sqrt(1005.750/998.200)   ! rho = 1020 etc
       endif
     enddo


 else if (index(md_ident,'thacker1d')  > 0 ) then                        ! parab300.net

    call thacker1d(1,xz,yz,s1,bl,ndx,0d0)

    if (kmx > 0) then
       call setkbotktop(1) ! inisaltwedge

       do k = 1,ndx
          if (s1(k) > 0.5d0) then
             call getkbotktop(k,kb,kt)
             do kk = kb,kt
                sa1(kk) = 30d0
             enddo
          endif
       enddo
    endif

 else if (md_IDENT(1:12) == 'coriolistilt') then
     call coriolistilt(0d0 )
 else if (md_IDENT(1:14) == 'corioliskelvin') then
     call corioliskelvin(0d0)
 else if (md_IDENT(1:9) == 'oceaneddy') then
     call oceaneddy(0d0)
 else if (index(md_ident,'checkerboard') > 0 ) then     ! v40.net, v100.net

    bl    = 0d0
    ibedlevtyp   = 1 ; call setbobs()

    call dminmax(   xk, numk, xkmin, xkmax, numk)

    n   = 2
    if (index(md_ident,'4') > 0 ) n = 4
    if (index(md_ident,'8') > 0 ) n = 8

    xli = 1d0/(xkmax-xkmin)
    amp = .01d0
    dep = .01d0

    pin = n*pi
    do L = 1,lnx
       k1 = ln(1,L) ; k2 = ln(2,L)
       xx = 0.5d0* ( xz(k1) + xz(k2) ) * xli
       yy = 0.5d0* ( yz(k1) + yz(k2) ) * xli
       ux  = 0d0 ; uy = 0d0
       ux  =  ux + amp*sin(pin*xx)*cos(pin*yy)          ! poisson
       uy  =  uy - amp*cos(pin*xx)*sin(pin*yy)
       u1(L) = csu(L)*ux + snu(L)*uy
    enddo

    do k = 1,ndx
       xx = xz(k) * xli
       yy = yz(k) * xli
       s1(k) = dep + amp*amp*(cos(2*pin*xx)+cos(2*pin*yy))/(8*ag*pin*pin)
       if (jasal > 0) then
           if (yy > 0.20 .and. yy < 0.30) sa1(k) = 30.
       endif
    enddo

    do j = 1,300
       fout = 0d0
       call calculate_hu_au_and_advection_for_dams_weirs(SET_ZWS0, SET_HU)             ! was just call sethu()
       do k = 1,ndx
          sq(k) = 0d0
          do kk = 1,nd(k)%lnx
             L  = nd(k)%ln(kk)
             La = iabs(L)
             if (L > 0) then
                sq(k) = sq(k) + u1(La)*hu(La)
             else
                sq(k) = sq(k) - u1(La)*hu(La)
             endif
          enddo
          fout = fout + abs(sq(k))
       enddo

       do k = 1,ndx
          foutk = 0
          do kk = 1,nd(k)%lnx
             L  = nd(k)%ln(kk)
             La = iabs(L)
             if (L > 0) then
                foutk = foutk + sq(ln(2,La))
             endif
          enddo
          s0(k) = s0(k) - foutk*1d-1
       enddo

    enddo

    chkadvd = 0.0d0
    s1(ndx/2)   = s1(ndx/2) + 1d-5

 else if (index(md_netfile,'kelvin') > 0 ) then

    call dminmax(   xz, ndx, xzmin, xzmax, ndx)
    call dminmax(   yz, ndx, yzmin, yzmax, ndx)
    r0  = 0.5d0* (xzmax - xzmin)
    x0  = 0.5d0* (xzmax + xzmin)
    y0  = 0.5d0* (yzmax + yzmin)

    amp    = 0.05d0
    dep    = 10d0
    call inisferic()
    Rossby = sqrt(ag*dep) / fcorio

    sqghi  = sqrt(ag/dep)
    bl     = -dep

    do k = 1,ndx
       xx     = xz(k) - x0 ; yy     = yz(k) - y0
       r      = sqrt(  xx*xx + yy*yy )
       csth   = xx/r ; snth = yy/r
       eer    = (r-r0) / Rossby
       s1(k)  = amp*exp(eer)*csth
       ucmk   = sqghi*s1(k)
       ucx(k) = -ucmk*snth
       ucy(k) =  ucmk*csth
    enddo

    do l  = 1, lnx
       k1    = ln(1,L)   ; k2 = ln(2,L)
       ux    = acl(L)*ucx(k1) + (1d0-acl(L))*ucx(k2)
       uy    = acl(L)*ucy(k1) + (1d0-acl(L))*ucy(k2)
       u1(L) =   ux*csu(L)    + uy*snu(L)
    enddo
 else if (index(md_netfile,'thacker2d') > 0 ) then

    call thacker2d(time0,1)

 else if (md_netfile == 'chan650.net') then

    bl = -5.d0 ; ibedlevtyp   = 1 ; call setbobs()
    s1 =  0.d0

    sa1(275:375) = 5d0

 else if (md_netfile == '640x480.net') then

    bl = -5.d0 ; ibedlevtyp   = 1 ; call setbobs()
    s1 =  0.d0

 else if (md_netfile == 'rec10x10.net') then


    do n = 1,ndx
      if (xz(n) < 1) s1(n) = s1(n) + 1d0
    enddo

 else if (md_netfile == 'g04.net') then

 !   bl = -20.0
    s1 = max(0d0,bl)

 else if (md_netfile == 'sqhex.net' .or. md_netfile      == 'sqquad.net' .or.    &
          md_netfile == 'sqtri.net' .or. md_netfile(1:6) == 'sqcurv' ) then                   ! sqhex.net

    itest = 1
    if (itest == 1) then
       r0     = 250000d0                               ! basin width
       dep    = 5d0                                    ! depth

       x0 = -180 ; y0 = 0 ; rmx = 350
       do k = 1,ndx
          s1(k) = dep
          dxx = xz(k) - x0 ; dyy = yz(k) - y0
          rr  = sqrt(dxx*dxx + dyy*dyy)
          if (rr < 0.5d0*rmx) then
             !sa1(k) = 5d0 + 5d0*cos(twopi*rr/rmx)
             sa1(k) = 10d0
          endif
       enddo

       do l   = 1, lnx
          k1  = lncn(1,L) ; k2 = lncn(2,L)
          xx1 = xk(k1) ; yy1 =  yk(k1)
          ux1 = yy1    ; uy1 = -xx1
          xx2 = xk(k2) ; yy2 =  yk(k2)
          ux2 = yy1    ; uy2 = -xx1

          call normalout(xx1, yy1, xx2, yy2, csl, snl, jsferic, jasfer3D, dmiss, dxymis)

          ux  = 0.5d0*(ux1+ux2)
          uy  = 0.5d0*(uy1+uy2)

          k1  = ln(1,L) ; k2 = ln(2,L)
          xx  = 0.5d0*(xz(k1) + xz(k2))
          yy  = 0.5d0*(yz(k1) + yz(k2))
          ux  =  yy
          uy  = -xx

          u1(L) =  ux*csL + uy*snL
       enddo
       u0 = u1
    endif

 else if (md_ident == 'leveque') then

     do L   = 1, lnx
          k1  = lncn(1,L) ; k2 = lncn(2,L)
          xx1 = xk(k1) ; yy1 =  yk(k1)
          ux1 = yy1    ; uy1 = -xx1
          xx2 = xk(k2) ; yy2 =  yk(k2)

          ux2 = yy2    ; uy2 = -xx2

          ux  = 0.5d0*(ux1+ux2)/64d0
          uy  = 0.5d0*(uy1+uy2)/64d0
          u1(L) =  ux*csu(L) + uy*snu(L)
     enddo
     u0 = u1

     call dminmax(   xk, numk, xkmin, xkmax, numk)
     call dminmax(   yk, numk, ykmin, ykmax, numk)

     x0  = 0.50d0
     y0  = 0.75d0
     rmx = 0.15d0
     sa1 = 0d0
     do k = 1,ndx
        xx = ( xz(k) - xkmin ) / (xkmax-xkmin)
        yy = ( yz(k) - ykmin ) / (ykmax-ykmin)
        dxx = xx - x0
        dyy = yy - y0
        rr  = sqrt(dxx*dxx + dyy*dyy)

       if (xx > 0.4d0 .and. xx < 0.6d0 .and. yy > 0.7d0 .and. yy < 0.9d0 ) then
           sa1(k) = 10d0
       endif

     enddo

     itstep = 0
 else if (md_ident(1:6) == 'teacup') then

     call dminmax(   xk, numk, xkmin, xkmax, numk)
     call dminmax(   yk, numk, ykmin, ykmax, numk)

     call half(xkmin,ykmin,xkmax,ykmin, xx1,yy1,  jsferic, jasfer3D)
     call half(xkmin,ykmax,xkmax,ykmax, xx2,yy2,  jsferic, jasfer3D)
     rmx = 0.5d0*dbdistance(xx1,yy1,xx2,yy2, jsferic, jasfer3D, dmiss)
     call half(xx1,yy1,xx2,yy2,x0,y0,  jsferic, jasfer3D)

     do L   = 1, lnx
        k1  = lncn(1,L) ; k2 = lncn(2,L)
        xx1 = xk(k1)-x0 ; yy1 =  yk(k1)-y0
        ux1 = yy1       ; uy1 = -xx1
        xx2 = xk(k2)-x0 ; yy2 =  yk(k2)-y0
        ux2 = yy2       ; uy2 = -xx2

        ux    = 0.5d0*(ux1+ux2)/rmx
        uy    = 0.5d0*(uy1+uy2)/rmx

        u1(L) = ux*csu(L) + uy*snu(L)
        do LL = Lbot(L), Lbot(L) + kmxL(L) - 1
           u1(LL) = u1(L)
        enddo
     enddo
     u0 = u1

     do k = 1,numk
        rr    = dbdistance(xk(k),yk(k),x0,y0, jsferic, jasfer3D, dmiss)
        ux    = min(1d0, rr/rmx)
        zk(k) = zkuni*sqrt( 1d0 - ux**2)
     enddo

     npl = 401 ; dphi = 1d0/ (npl-1) ; phi = 0d0 ; k = 0
     k = k + 1 ; xpl(k) = x0 + 1.1*rmx ; ypl(k) = y0
     do L = 1,npl
        k      = k + 1
        xpl(k) = x0  + rmx*cos(phi)
        ypl(k) = y0  + rmx*sin(phi)
        phi    = phi + dphi*twopi
     enddo
     k = k + 1 ; xpl(k) = x0 + 1.1*rmx ; ypl(k) = y0
     k = k + 1 ; xpl(k) = x0 + 1.1*rmx ; ypl(k) = y0 - 1.1*rmx
     k = k + 1 ; xpl(k) = x0 - 1.1*rmx ; ypl(k) = y0 - 1.1*rmx
     k = k + 1 ; xpl(k) = x0 - 1.1*rmx ; ypl(k) = y0 + 1.1*rmx
     k = k + 1 ; xpl(k) = x0 + 1.1*rmx ; ypl(k) = y0 + 1.1*rmx
     npl = k
     call newfil(msam, 'teacup.pli')
     call wripol(msam)

 else if (index(md_ident,'horvic') > 0) then

    if (ibedlevtyp == 1) then
       bl = zkuni + xz*bedslope
    else
       zk = zkuni + xk*bedslope
    endif

    call setbobs()
    s1 = xz*bedslope  ! bl + 10d0

    call Poiseuille(1)

    do L = 1,-Lnx ! Lnx
       u1(L) = 3d0*csu(L)
    enddo

 else if (index(md_ident,'slope') > 0) then

    call setkbotktop(1)
    do LL = 1,Lnx
       Ltop(LL) = lbot(LL) + max(kmx,1) - 1
       hu(LL)   = 5d0 ; frcu(LL) = frcuni
       call getczz0(hu(LL), frcu(LL), ifrcutp(LL), cz, z00)
       ustb(LL) = sqrt(ag*5d0*5d-5)
       cs = csu(LL)
       Lb = Lbot(LL) ; Lt = Ltop(LL)
       do L = Lb,Lt
          zz    = 5d0*dble(L - Lb + 1 - 0.5d0) / dble(Lt-Lb+1)
          u1(L) = cs*ustb(LL)*log(c9of1 + zz/z00) / vonkar
       enddo
    enddo

 else if (md_ident == 'equator1d') then

    call equatorial(0d0)

 else if (md_ident == 'tank_1d') then

      bl = 0d0 ; s1 = -10d0
      do k = 1,ndx
          if      (xz(k) < 0.2d0) then   ! linkerwand
             bl(k) = 50d0
          else if (xz(k) < 20d0) then
             s1(k) = 30d0
             if (xz(k) > 19.8d0) then
                bl(k) = bl(k) + 0.01
             endif
          else if (xz(k) > 25d0 .and. xz(k) < 25.2d0 + 2) then
             bl(k) = 3.0
          else if (xz(k) > 30d0) then
             bl(k) = -20d0
             s1(k) = -4d0
          endif
      enddo
      ibedlevtyp    = 1 ; call setbobs()
 else if ( md_ident(1:3).eq.'lts' ) then
    if ( md_ident(4:6).eq.'rot' ) then
       xkmin =  huge(1d0)
       xkmax = -huge(1d0)
       ykmin =  huge(1d0)
       ykmax = -huge(1d0)
       do k=1,numk
          xkmin = min(xkmin,xk(k))
          xkmax = max(xkmax,xk(k))
          ykmin = min(ykmin,yk(k))
          ykmax = max(ykmax,yk(k))
       end do

       if ( jampi.eq.1 ) then
         call reduce_double_max(xkmax)
         call reduce_double_max(ykmax)
         call reduce_double_min(xkmin)
         call reduce_double_min(ykmin)
       end if

       xm = 0.5d0*(xkmin+xkmax)
       ym = 0.5d0*(ykmin+ykmax)
       R  = 0.5d0*max(xkmax-xkmin, ykmax-ykmin)
       if ( kmx.eq.0 ) then
          do L=1,Lnx
             u1(L) = (-(yu(L)-ym)*csu(L) + (xu(L)-xm)*snu(L))/R
!             u1(L) = (csu(L)+snu(L))
          end do
       else
          do LL=1,Lnx
             Ltop(LL) = Lbot(LL)+kmx-1
             do L=lbot(LL),ltop(LL)
                u1(L) = -yu(LL)*csu(LL) + xu(L)*snu(L)
             end do
          end do
       end if

    else
       if ( kmx.eq.0 ) then
          do L=1,Lnx
             u1(L) = csu(L)
          end do
       else
          do LL=1,Lnx
             Ltop(LL) = Lbot(LL)+kmx-1
             do L=lbot(LL),ltop(LL)
                u1(L) = csu(LL)
             end do
          end do
       end if
    end if

    itstep = 0

 endif

end subroutine apply_hardcoded_specific_input
    
    
!> restore au and q1 for 3D case for the first write into a history file    
subroutine restore_au_q1_3D_for_1st_history_record()
   use m_flow,                 only : q1, LBot, kmx, kmxL   
   use m_flowexternalforcings, only : fusav, rusav, ausav, ncgen
   use m_flowgeom,             only : lnx

   implicit none

   integer                       :: i_q1_v, i_q1_0
   double precision, allocatable :: fu_temp(:,:), ru_temp(:,:), au_temp(:,:)

   if ( kmx > 0 ) then
      if (ncgen > 0) then
         fu_temp = fusav
         ru_temp = rusav
         au_temp = ausav
      endif   
      call furusobekstructures() ! to have correct au values but it provides incorrect q1 values for structures
      if (ncgen > 0) then
         fusav = fu_temp
         rusav = ru_temp
         ausav = au_temp
      endif   
!  restore correct discharge values
      do i_q1_0 = 1, lnx
         q1(i_q1_0) = 0d0 
         do i_q1_v = Lbot(i_q1_0), Lbot(i_q1_0) - 1 + kmxL(i_q1_0)
            q1(i_q1_0) = q1(i_q1_0) + q1(i_q1_v)       ! depth integrated result
         end do
      end do
   end if
    
end subroutine restore_au_q1_3D_for_1st_history_record
    
end module m_flow_flowinit
    
