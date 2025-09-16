!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_fm_icecover
   use precision
   use icecover_module, only: icecover_type, icecover_output_flags
   use icecover_module, only: ICECOVER_NONE, ICECOVER_EXT, ICECOVER_SEMTNER, ICE_WINDDRAG_NONE, FRICT_AS_DRAG_COEFF
   use icecover_module, only: null_icecover, alloc_icecover, clr_icecover, is_allocated_icecover
   use icecover_module, only: freezing_temperature, update_icepress, ice_drag_effect, icecover_prepare_output
   use icecover_input_module, only: read_icecover, echo_icecover, late_activation_ext_force_icecover
   implicit none

!
! Global data
!
   type(icecover_type), target :: ice_data !< module instance of the icecover data structure
!
   real(fp), dimension(:), pointer :: ice_area_fraction !< module pointer to the ice area fraction array inside ice_data
   real(fp), dimension(:), pointer :: ice_thickness !< module pointer to the ice thickness array inside ice_data
   real(fp), dimension(:), pointer :: ice_pressure !< module pointer to the ice pressure array inside ice_data
   real(fp), dimension(:), pointer :: ice_temperature !< module pointer to array temperature inside ice_data
   real(fp), dimension(:), pointer :: qh_air2ice !< module pointer to the qh_air2ice array inside ice_data
   real(fp), dimension(:), pointer :: qh_ice2wat !< module pointer to the qh_ice2wat array inside ice_data
   real(fp), dimension(:), pointer :: snow_thickness !< module pointer to the snow thickness array inside ice_data
   real(fp), dimension(:), pointer :: snow_temperature !< module pointer to the snow temperature array inside ice_data

   real(fp), dimension(:), pointer :: ice_s1 !< module pointer to the open water level array inside ice_data
   real(fp), dimension(:), pointer :: ice_zmin !< module pointer to the lower ice cover surface height array inside ice_data
   real(fp), dimension(:), pointer :: ice_zmax !< module pointer to the upper ice cover surface height array inside ice_data

   integer, pointer :: ja_ice_area_fraction_read !< flag indicating whether ice area fraction is available via EC module
   integer, pointer :: ja_ice_thickness_read !< flag indicating whether ice thickness is available via EC module

   type(icecover_output_flags), pointer :: ice_mapout !< module pointer to mapout flags inside ice_data

   logical, pointer :: ice_apply_pressure !< module pointer to flag apply_pressure inside ice_data
   logical, pointer :: ice_apply_friction !< module pointer to flag apply_friction inside ice_data
   logical, pointer :: ice_reduce_surface_fluxes !< module pointer to flag reduce_surface_fluxes inside ice_data
   logical, pointer :: ice_reduce_waves !< module pointer to flag reduce_waves inside ice_data
   integer, pointer :: ice_modify_winddrag !< module pointer to flag modify_winddrag inside ice_data

   integer, pointer :: ja_icecover !< module pointer to modeltype flag inside ice_data that specifies the ice cover model
   integer, pointer :: ice_frict_type !< module pointer to frict_type inside ice_data

   real(fp), pointer :: ice_density !< module pointer to ice_density inside ice_data
   real(fp), pointer :: ice_albedo !< module pointer to ice_albedo inside ice_data
   real(fp), pointer :: ice_conductivity !< module pointer to ice_conductivity inside ice_data
   real(fp), pointer :: ice_latentheat !< module pointer to ice_latentheat inside ice_data
   real(fp), pointer :: ice_frcuni !< module pointer to frict_val inside ice_data

   real(fp), pointer :: snow_albedo !< module pointer to snow_albedo inside ice_data
   real(fp), pointer :: snow_conductivity !< module pointer to snow_conductivity inside ice_data
   real(fp), pointer :: snow_latentheat !< module pointer to snow_latentheat inside ice_data

   character(len=*), parameter :: MDU_ICE_CHAPTER = 'ice' !< name of the ice chapter in the mdu file

contains

!> Nullify/initialize ice data structure.
   subroutine fm_ice_null()
      !
      ! Function/routine arguments
      !
      ! NONE
      !
      ! Local variables
      !
      integer :: istat !< status flag for allocation
!
!! executable statements -------------------------------------------------------
!
      istat = null_icecover(ice_data)
      call fm_ice_update_all_pointers()
   end subroutine fm_ice_null

!> Update all ice data structure.
   subroutine fm_ice_update_all_pointers()
      !
      ! Function/routine arguments
      !
      ! NONE
      !
      ! Local variables
      !
      ! NONE
!
!! executable statements -------------------------------------------------------
!
      ja_ice_area_fraction_read => ice_data%ice_area_fraction_forcing_available
      ja_ice_thickness_read => ice_data%ice_thickness_forcing_available

      ja_icecover => ice_data%model_type

      ice_mapout => ice_data%mapout

      ice_apply_pressure => ice_data%apply_pressure
      ice_apply_friction => ice_data%apply_friction
      ice_reduce_waves => ice_data%reduce_waves
      ice_modify_winddrag => ice_data%modify_winddrag

      ice_albedo => ice_data%ice_albedo
      ice_conductivity => ice_data%ice_conductivity
      ice_latentheat => ice_data%ice_latentheat
      ice_density => ice_data%ice_density
      ice_frict_type => ice_data%frict_type
      ice_frcuni => ice_data%frict_val

      snow_albedo => ice_data%snow_albedo
      snow_conductivity => ice_data%snow_conductivity
      snow_latentheat => ice_data%snow_latentheat

      call fm_ice_update_spatial_pointers()
   end subroutine fm_ice_update_all_pointers

!> Update spatial pointers after (de)allocation
   subroutine fm_ice_update_spatial_pointers()
      !
      ! Function/routine arguments
      !
      ! NONE
      !
      ! Local variables
      !
!
!! executable statements -------------------------------------------------------
!
      ice_area_fraction => ice_data%ice_area_fraction
      ice_thickness => ice_data%ice_thickness
      !
      ice_temperature => ice_data%ice_temperature
      snow_thickness => ice_data%snow_thickness
      snow_temperature => ice_data%snow_temperature
      !
      ice_s1 => ice_data%ice_s1
      ice_zmin => ice_data%ice_zmin
      ice_zmax => ice_data%ice_zmax
      !
      ice_pressure => ice_data%pressure
      qh_air2ice => ice_data%qh_air2ice
      qh_ice2wat => ice_data%qh_ice2wat
   end subroutine fm_ice_update_spatial_pointers

   !> logical flag for allocation
   function fm_is_allocated_ice() result(flag)
      logical :: flag !< logical flag for allocation

      flag = is_allocated_icecover(ice_data)
   end function fm_is_allocated_ice

!> activation of icecover module based on external forcing input
   subroutine fm_ice_activate_by_ext_forces(ndx, md_ptr)
      use properties, only: tree_data
      !
      ! Function/routine arguments
      !
      integer, intent(in) :: ndx !< number of cells in the D-Flow FM domain
      type(tree_data), pointer :: md_ptr !< pointer to the input file
      !
      ! Local variables
      !
      integer :: istat !< status flag for allocation
!
!! executable statements -------------------------------------------------------
!
      istat = late_activation_ext_force_icecover(ice_data, md_ptr, MDU_ICE_CHAPTER)
      call fm_ice_alloc(ndx)
   end subroutine fm_ice_activate_by_ext_forces

!> Allocate the arrays of ice data structure.
   subroutine fm_ice_alloc(ndx)
      integer, intent(in) :: ndx !< number of cells in the D-Flow FM domain

      integer :: istat !< status flag for allocation

      if (.not. is_allocated_icecover(ice_data)) then
         istat = alloc_icecover(ice_data, 1, ndx)
         call fm_ice_update_spatial_pointers()
      end if
   end subroutine fm_ice_alloc

!> Clear the arrays of ice data structure.
   subroutine fm_ice_clr()
      integer :: istat !< status flag for allocation

      istat = clr_icecover(ice_data)
      call fm_ice_null()
   end subroutine fm_ice_clr

!> Read the ice cover module configuration from the mdu file
   subroutine fm_ice_read(md_ptr, ierror)
      use dfm_error, only: DFM_WRONGINPUT
      use tree_data_types, only: tree_data

      type(tree_data), pointer :: md_ptr !< pointer to the input file
      integer, intent(inout) :: ierror !< D-Flow FM error flag

      logical :: error !< ice module error flag

      call read_icecover(ice_data, md_ptr, MDU_ICE_CHAPTER, error)
      call fm_ice_update_spatial_pointers()

      if (error) then
         ierror = DFM_WRONGINPUT
      end if
   end subroutine fm_ice_read

!> Report the ice configuration to the diagnostic output.
   subroutine fm_ice_echo(mdia)
      integer, intent(in) :: mdia !< unit number of diagnostic output

      logical :: error !< ice module error flag

      error = echo_icecover(ice_data, mdia)
   end subroutine fm_ice_echo

!> Update the ice pressure array.
   subroutine fm_ice_update_press(ag)
      real(dp), intent(in) :: ag !< gravitational acceleration (m/s2)

      real(fp) :: ag_fp !< gravitational acceleration (m/s2)

      ag_fp = real(ag, fp)
      call update_icepress(ice_data, ag_fp)
   end subroutine fm_ice_update_press

!> preprocessing for ice cover, because in subroutine HEATUN some ice cover quantities have to be computed
!! this subroutine is comparable with subroutine HEA_ICE.F90 of the Delft3D-FLOW ice module
   subroutine preprocess_icecover(n, Qlong_ice, tempwat, saltcon, wind)
      use MessageHandling
      use m_flow, only: hu
      use m_flowgeom, only: nd
      use m_physcoef, only: vonkar
      use physicalconsts, only: celsius_to_kelvin, kelvin_to_celsius
      use m_heatfluxes, only: cpw
      use m_wind, only: air_temperature
      use ieee_arithmetic, only: ieee_is_nan

      integer, intent(in) :: n !< node number
      real(fp), intent(in) :: Qlong_ice !< part of Qlong computed in HEATUN
      real(fp), intent(in) :: tempwat !< temperature of water at top layer [degC]
      real(fp), intent(in) :: saltcon !< salinity of water at top layer [ppt]
      real(fp), intent(in) :: wind !< wind speed [m/s]

      integer :: iter !< iteration number
      integer :: icount !< number of flow links
      integer :: LL !< flow link index
      logical :: converged !< flag for convergence in iterative process for computation of effective back radiation based on ice or snow

      real(fp) :: b !< empirical constant in computation of c_tz
      real(fp) :: p_r !< molecular Prandtl number (-)
      real(fp) :: p_rt !< turbulent Prandtl number (-)
      real(fp) :: kin_vis !< kinematic viscosity (kg m-1 s-1)
      real(fp) :: t_freeze !< freezing temperature of water (degC)
      real(fp) :: t_ref !< reference temperature (K)
      real(fp) :: sum !< sum of water depths at flow links (m)
      real(fp) :: b_t !< molecular sublayer correction
      real(fp) :: c_tz !< heat transfer coefficient (J m-2 s-1 K-1)
      real(fp) :: D_t !< temperature difference (degC or K)
      real(fp) :: effective_ice_thickness !< auxiliary variable with the effective ice thickness (m)
      real(fp) :: temp_min !< lower limit for the ice/snow temperature (K)
      real(fp) :: temp_max !< upper limit for the ice/snow temperature (K)
      real(fp) :: tsi !< surface temperature with surface being either water, ice or snow (K)
      real(fp) :: coef1 !< auxiliary variable; see D-Flow FM Technical Reference Manual for a detailed description (J m-2 s-1)
      real(fp) :: coef2 !< auxiliary variable; see D-Flow FM Technical Reference Manual for a detailed description (J m-2 s-1 k-1)
      real(fp) :: alpha !< relaxation factor (-)
      real(fp) :: z00 !< open water roughness height (m)
      real(fp) :: ustar !< wind shear velocity (m s-1)
      real(fp) :: hdz !< Vertical coordinate corresponding to the temperature, for which the mid of the water column is taken  [m]
      real(fp) :: rhow !< density of water (kg m-3)
      real(fp) :: Qlong !< effective back radiation, computed after convergence of iteration process (J m-2 s)

      ! Initialization
      b = 3.0_fp
      p_r = 13.0_fp
      p_rt = 0.85_fp
      kin_vis = 0.0000018_fp
      rhow = 1000.0_fp
      z00 = 2e-4_fp
      ustar = 0.025_fp * wind ! See Eq. (12.5) in D-Flow FM User Manual: ustar = sqrt(C_D) * U_10
      hdz = 0.0_fp
      converged = .false.
      temp_min = celsius_to_kelvin(-25.0_fp)
      temp_max = celsius_to_kelvin(0.0_fp)

      ! Compute freezing point
      t_freeze = freezing_temperature(saltcon)
      t_ref = celsius_to_kelvin(0.0_fp) ! 0.0 or t_freeze ?

      select case (ja_icecover)
      case (ICECOVER_SEMTNER)
         ! follow Semtner (1975)
         !
         ! Compute conductivity, depending on the presence of both ice and snow
         !
         if (snow_thickness(n) < 0.001_fp) then
            effective_ice_thickness = max(0.01_fp, ice_thickness(n))
            tsi = ice_temperature(n)
         else
            effective_ice_thickness = max(0.01_fp, ice_thickness(n)) + max(0.01_fp, snow_thickness(n)) * (ice_conductivity / snow_conductivity)
            tsi = snow_temperature(n)
         end if
         !
         ! Compute long wave radiation radiation flux from ice surface according to Eq. (7) in (Wang, 2005)
         ! including an iteration proces
         !
         do iter = 1, 5
            coef1 = Qlong_ice * tsi**4.0_fp
            coef2 = 4.0_fp * Qlong_ice * tsi**3.0_fp
            D_t = (qh_air2ice(n) - coef1 - ice_conductivity * (tsi - t_ref) / effective_ice_thickness) &
               & / (coef2 + ice_conductivity / effective_ice_thickness)
            tsi = tsi + D_t
            if (abs(D_t) < 1e-2_fp) then
               converged = .true.
               if (tsi > 0.0_fp) then
                  ! melting
                  Qlong = coef1
               else
                  ! freezing
                  Qlong = max(0.0_fp, coef1 + coef2 * D_t)
               end if
               !
               ! apply relaxation for stability reasons
               !
               alpha = 0.5_fp
               if (snow_thickness(n) > 0.001_fp) then
                  snow_temperature(n) = alpha * tsi + (1.0_fp - alpha) * snow_temperature(n)
                  snow_temperature(n) = max(TEMP_MIN, min(snow_temperature(n), TEMP_MAX))
               else
                  ice_temperature(n) = alpha * tsi + (1.0_fp - alpha) * ice_temperature(n)
                  ice_temperature(n) = max(TEMP_MIN, min(ice_temperature(n), TEMP_MAX))
               end if
               !
               qh_air2ice(n) = qh_air2ice(n) - Qlong
               !
               ! no freezing in case of air temperatures above zero
               !
               if (air_temperature(n) > 0.0_fp .and. qh_air2ice(n) < 0.0_fp) then
                  qh_air2ice(n) = 0.0_fp
               end if
               !
               ! no melting in case of air temperatures below zero
               !
               if (air_temperature(n) < 0.0_fp .and. qh_air2ice(n) > 0.0_fp) then
                  qh_air2ice(n) = 0.0_fp
               end if
               exit ! jump out of the iteration loop
            end if
         end do
         !
         ! Compute ice to water flux according to Wang (2015)
         !
         ! Calculate the molecular sublayer correction b_t
         !
         b_t = b * sqrt(z00 * ustar / kin_vis) * (p_r)**0.666_fp
         !
         ! Calculate HDZ to be used for the computation of c_tz (NB. In this implementation the same for 2D and 3D)
         !
         sum = 0.0_fp
         icount = 0
         do LL = 1, nd(n)%lnx
            sum = sum + hu(LL)
            icount = icount + 1
         end do
         hdz = 0.5_fp * sum / max(1, icount)
         !
         ! Calculate heat transfer coefficient c_tz
         !
         c_tz = ustar / (b_t + p_rt * log(hdz / z00) / vonkar)
         !
         ! Calculate heat flux out of the ocean
         !
         qh_ice2wat(n) = rhow * cpw * c_tz * min(-0.01_fp, max(0.0_fp, tempwat - t_freeze))
         !
         ! adaptation of QH_ICE2WAT conform KNMI approach (QH_ICE2WAT = 2.4 W/m2)
         !
        !! qh_ice2wat(n) = -2.4_fp
         !
         if (ieee_is_nan(qh_ice2wat(n))) then
            write (msgbuf, '(a,i5,10f10.3)') 'NAN in PREPROCESS_ICECOVER', n, qh_ice2wat(n); call msg_flush()
         end if
         !
      case default
         ! no preparation needed
      end select
   end subroutine preprocess_icecover

!> update the ice cover -- initial coding here with full access to D-Flow FM arrays via use statements
!! let's see if we can make it gradually more modular and move functionality to the icecover_module.
   subroutine update_icecover()
      use precision, only: fp
      use m_flowgeom, only: ndx
      use m_flowtimes, only: dts
      use m_wind, only: air_temperature, rain, jarain
      use physicalconsts, only: celsius_to_kelvin
      real(fp), parameter :: MM_TO_M = 1.0_fp / 1000.0_fp !< factor for converting mm to m
      real(fp), parameter :: PER_DAY_TO_PER_S = 1.0_fp / 86400.0_fp !< factor for converting 1/day to 1/s
      real(fp), parameter :: CONV_FACTOR = MM_TO_M * PER_DAY_TO_PER_S !< factor for converting rain in mm/day to m/s

      integer :: n !< loop index, grid cell number
      real(fp) :: ice_thickness_change !< change in ice thickness based on heat exchange (m)
      real(fp) :: snow_thickness_change !< change in snow thickness based on heat exchange (m)

      select case (ja_icecover)
      case (ICECOVER_SEMTNER)
         ! follow Semtner (1975)

         ! Compute snow growth (NB. presence of ice is required)
         if (jarain == 1) then ! check whether rainfall input is prescribed
            do n = 1, ndx
               if (air_temperature(n) < 0.0_fp .and. ice_thickness(n) > 0.01_fp .and. rain(n) > 0.0_fp) then
                  snow_thickness(n) = snow_thickness(n) + dts * rain(n) * CONV_FACTOR
               end if
            end do
         end if

         ! Compute ice growth or melt of snow and ice
         do n = 1, ndx
            if (air_temperature(n) < 0.0_fp .or. ice_thickness(n) > 0.0_fp) then
               if (qh_air2ice(n) > qh_ice2wat(n) .and. snow_thickness(n) > 0.0_fp) then
                  ! melting of snow due to heat exchange with air
                  snow_thickness_change = dts * (-qh_air2ice(n) + 0.0_fp) / snow_latentheat
                  if (-snow_thickness_change < snow_thickness(n)) then
                     ! snow melt less than snow layer thickness
                     snow_thickness(n) = snow_thickness(n) + snow_thickness_change
                     ice_thickness_change = 0.0_fp
                  else
                     ! snow melt more than snow layer thickness
                     ice_thickness_change = (snow_thickness_change + snow_thickness(n)) * snow_latentheat / ice_latentheat
                     snow_thickness(n) = 0.0_fp
                     snow_temperature(n) = celsius_to_kelvin(0.0_fp)
                  end if
                  ! ice_thickness_change initialize based on remaining heat exchange with air
                  ! additional ice_thickness_change due to heat exchange with water
                  ice_thickness_change = ice_thickness_change + dts * (0.0_fp + qh_ice2wat(n)) / ice_latentheat
               else
                  ! no snow: ice freezes or melts due to net heat exchange with air and water
                  ice_thickness_change = dts * (-qh_air2ice(n) + qh_ice2wat(n)) / ice_latentheat
               end if

               ice_thickness(n) = ice_thickness(n) + ice_thickness_change
               if (ice_thickness(n) > 0.0_fp) then
                  ice_area_fraction(n) = 1.0_fp
               else
                  ice_thickness(n) = 0.0_fp
                  ice_area_fraction(n) = 0.0_fp
                  ice_temperature(n) = celsius_to_kelvin(0.0_fp)
               end if
            end if
         end do

      case default
         ! by default no growth
      end select
   end subroutine update_icecover

!> determine effective drag coefficient when ice may be present
   pure function fm_ice_drag_effect(ice_area_fraction, cdw) result(cdeff)
      !
      ! Function/routine arguments
      !
      real(fp), intent(in) :: ice_area_fraction !< ice area fraction (-)
      real(fp), intent(in) :: cdw !< wind drag exerted via open water
      real(fp) :: cdeff !< effective wind drag coefficient
      !
      ! Local variables
      !
      ! NONE
!
!! executable statements -------------------------------------------------------
!
      cdeff = ice_drag_effect(ice_data, ice_area_fraction, cdw)
   end function fm_ice_drag_effect

!> compute the icecover quantities that are only needed for output
   subroutine fm_icecover_prepare_output(water_level, rho, ag)
      use m_flowgeom, only: ndx
      use m_get_kbot_ktop, only: getkbotktop

      real(dp), dimension(:), intent(in) :: water_level !< water level (m+REF)
      real(dp), dimension(:), intent(in) :: rho !< water density (kg/m3)
      real(dp), intent(in) :: ag !< gravitational acceleration (m/s2)

      real(fp), dimension(:), allocatable :: water_level_fp !< water level (m+REF) cast to fp precision
      real(fp), dimension(:), allocatable :: water_density !< near surface water density (kg/m3)

      integer :: n !< loop index, grid cell number
      integer :: kb !< index of bottom layer
      integer :: kt !< index of top layer
      real(fp) :: ag_fp !< gravitational acceleration (m/s2)

      ag_fp = real(ag, fp)

      allocate (water_level_fp(ndx), water_density(ndx))
      do n = 1, ndx
         call getkbotktop(n, kb, kt)
         water_density(n) = real(rho(kt), fp)
      end do
      water_level_fp(:) = real(water_level(:), fp)

      call icecover_prepare_output(ice_data, water_level_fp, water_density, ag_fp)
   end subroutine fm_icecover_prepare_output

end module m_fm_icecover
