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

module icecover_module
use precision
use MessageHandling, only: mess, LEVEL_ALL, LEVEL_FATAL
implicit none
private

!
! public data types
!
public icecover_type
public icecover_output_flags

! public parameters
!
integer, parameter, public :: ICECOVER_NONE    = 0 !< no ice cover
integer, parameter, public :: ICECOVER_EXT     = 1 !< externally forced ice cover --> EC module, or BMI?
integer, parameter, public :: ICECOVER_SEMTNER = 2 !< ice thickness computed based on Semtner (1975)
! ... add IcePack

integer, parameter, public :: FRICT_AS_DRAG_COEFF = 11 ! should be extension of D-Flow FM friction numbers

integer, parameter, public :: ICE_WINDDRAG_NONE        = 0 !< no effect, normal wind drag
integer, parameter, public :: ICE_WINDDRAG_CUBIC       = 1 !< Based on ADCIRC (Chapman & Massey)
integer, parameter, public :: ICE_WINDDRAG_LB05        = 2 !< Lupkes and Birnbaum (2005)
integer, parameter, public :: ICE_WINDDRAG_AN10        = 3 !< Andreas et al (2010)
integer, parameter, public :: ICE_WINDDRAG_LINEAR      = 4 !< no wind drag below ice
integer, parameter, public :: ICE_WINDDRAG_RAYS        = 5 !< Based on ADCIRC (Chapman et al., 2005)
integer, parameter, public :: ICE_WINDDRAG_JOYCE19     = 6 !< Joyce et al (2019)

!
! public routines
!
public freezing_temperature
public null_icecover
public select_icecover_model
public late_activation_ext_force_icecover
public alloc_icecover
public clr_icecover
public :: update_icepress
public :: ice_drag_effect
public :: set_default_output_flags
public :: icecover_prepare_output

! ice cover output
type icecover_output_flags
    logical :: default  !< default flag for output writing
    logical :: ice_s1   !< sea surface height of open water
    logical :: ice_zmin !< lower surface height of ice/snow cover
    logical :: ice_zmax !< upper surface height of ice/snow cover
    logical :: ice_area_fraction   !< area fraction covered by ice
    logical :: ice_thickness    !< ice thickness
    logical :: ice_pressure    !< pressure of ice cover
    logical :: ice_temperature    !< temperature of ice cover
    logical :: snow_thickness   !< snow thickness
    logical :: snow_temperature   !< temperature of snow cover
end type icecover_output_flags

! ice cover type
type icecover_type
    !
    ! input
    !
    type(icecover_output_flags) :: mapout         !< flags indicating whether ice cover should be written to map-file
    !
    logical  :: apply_pressure                    !< flag indicating whether pressure of ice cover should be applied
    logical  :: apply_friction                    !< flag indicating whether ice cover friction should be applied
    logical  :: reduce_surface_exchange           !< flag indicating whether precipitation, evaporation and heat exchange should be reduced
    logical  :: reduce_waves                      !< flag indicating whether waves should be reduced
    integer  :: modify_winddrag                   !< flag indicating option to modify the wind drag coefficient (one of ICE_WINDDRAG_...)
    !
    integer  :: modeltype                         !< type of the ice cover (one of ICECOVER_...)
    integer  :: frict_type                        !< friction type exerted by the ice cover
    !
    integer  :: ice_area_fraction_forcing_available    !< flag indicating whether ice area fraction is available via external forcing
    integer  :: ice_thickness_forcing_available   !< flag indicating whether ice thickness is available via external forcing
    !
    real(fp) :: ice_albedo                        !< albedo of ice (-)
    real(fp) :: ice_conductivity                  !< conductivity of ice (W m-1 K-1)
    real(fp) :: ice_density                       !< ice density (kg m-3)
    real(fp) :: ice_latentheat                    !< latent heat of ice (kJ kg-1)
    real(fp) :: ice_skin_drag                     !< skin drag of ice floes (N m-2)
    real(fp) :: maximum_ice_form_drag             !< maximum form drag of ice floes (N m-2)
    real(fp) :: snow_albedo                       !< albedo of snow (-)
    real(fp) :: snow_conductivity                 !< conductivity of snow (W m-1 K-1)
    real(fp) :: snow_latentheat                   !< latent heat of snow (kJ kg-1)
    real(fp) :: frict_val                         !< friction coefficient of ice cover (unit depends on frict_type)
    !
    ! state
    !
    real(fp), dimension(:), pointer :: ice_area_fraction     => null() !< area fraction covered by ice (-)
    real(fp), dimension(:), pointer :: ice_thickness    => null() !< ice cover thickness (m)
    real(fp), dimension(:), pointer :: snow_thickness   => null() !< snow cover thickness (m)
    real(fp), dimension(:), pointer :: ice_temperature  => null() !< ice temperature (degC)
    real(fp), dimension(:), pointer :: snow_temperature => null() !< snow temperature (degC)
    !
    ! extra
    !
    real(fp), dimension(:), pointer :: qh_air2ice => null() !< heat flux from air to ice (W m-2)
    real(fp), dimension(:), pointer :: qh_ice2wat => null() !< heat flux from ice to water (W m-2)
    real(fp), dimension(:), pointer :: pressure   => null() !< pressure exerted by the ice cover (Pa)
    real(fp), dimension(:), pointer :: ice_s1     => null() !< open water level (m+REF)
    real(fp), dimension(:), pointer :: ice_zmin   => null() !< lower ice cover surface height (m+REF)
    real(fp), dimension(:), pointer :: ice_zmax   => null() !< upper ice cover surface height (m+REF)
end type icecover_type

contains

!> Compute the freezing temperature based on NEMO (2022), Fofonoff and Millard (1983)
!! Parameter names consistent with the latter publication.
pure function freezing_temperature(salinity, pressure) result (t_freeze)
    real(fp)          , intent(in)    :: salinity            !< salinity (ppt)
    real(fp), optional, intent(in)    :: pressure            !< pressure (Pa)
    real(fp)                          :: t_freeze            !< freezing temperature of water (degC)

    real(fp), parameter  :: a0 = -0.0575_fp      !< coefficient a0
    real(fp), parameter  :: a1 =  1.710523e-3_fp !< coefficient a1
    real(fp), parameter  :: a2 = -2.154996e-4_fp !< coefficient a2
    real(fp), parameter  :: b  = -7.53e-8_fp     !< coefficient b. Note Fofonoff & Millard define pressure in decibar, we use Pascal.

    t_freeze = ( a0 + a1*sqrt(salinity) + a2*salinity )*salinity
    if (present(pressure)) then
        ! pressure can often be ignored since the typical atmospheric pressure of 1 bar
        ! makes only a difference of 0.007 degC
        t_freeze = t_freeze + b * pressure
    end if
end function freezing_temperature

!> Nullify/initialize an icecover data structure.
function null_icecover(icecover) result(istat)
    !
    ! Function/routine arguments
    !
    type (icecover_type)                       , intent(inout) :: icecover  !< data structure containing ice cover data
    integer                                                    :: istat     !< status flag for allocation
    !
    ! Local variables
    !
    ! None
!
!! executable statements -------------------------------------------------------
!
    istat = select_icecover_model(icecover, ICECOVER_NONE)
    !
    ! state
    !
    nullify(icecover%ice_area_fraction)
    nullify(icecover%ice_thickness)
    nullify(icecover%snow_thickness)
    nullify(icecover%ice_temperature)
    nullify(icecover%snow_temperature)
    !
    ! extra
    !
    nullify(icecover%qh_air2ice)
    nullify(icecover%qh_ice2wat)
    nullify(icecover%pressure)
    nullify(icecover%ice_s1)
    nullify(icecover%ice_zmin)
    nullify(icecover%ice_zmax)
end function null_icecover


!> activation of icecover module based on external forcing input
function late_activation_ext_force_icecover(icecover) result(istat)
    !
    ! Function/routine arguments
    !
    type (icecover_type)                       , intent(inout) :: icecover  !< data structure containing ice cover data
    integer                                                    :: istat     !< status flag for allocation
    !
    ! Local variables
    !
    ! None
!
!! executable statements -------------------------------------------------------
!
    istat = 0
    if (icecover%modeltype == ICECOVER_EXT) then
       ! icecover already set to externally forced
    elseif (icecover%modeltype == ICECOVER_NONE) then
       ! activate icecover and switch on the pressure effect
       icecover%modeltype = ICECOVER_EXT
       icecover%apply_pressure = .true.
       call mess(LEVEL_ALL, 'Activating ice cover module based on external forcing.')
       call set_default_output_flags(icecover%mapout, icecover%modeltype)
       ! note: spatial arrays haven't been allocated yet!
    else
       ! don't overrule previously selected icecover ...
       call mess(LEVEL_FATAL, 'Ice cover forcing data conflicts with selected ice cover model.')
    endif
end function late_activation_ext_force_icecover


!> set default values for selected ice cover model and allocate
function select_icecover_model(icecover, modeltype) result(istat)
    type (icecover_type), intent(inout) :: icecover !< data structure containing ice cover data
    integer, intent(in) :: modeltype !< desired ice cover type
    integer :: istat !< status flag for allocation

    icecover%modeltype                 = modeltype

    call set_default_output_flags(icecover%mapout, modeltype, .false.)
    
    icecover%ice_area_fraction_forcing_available   = 0
    icecover%ice_thickness_forcing_available  = 0
    
    if (modeltype == ICECOVER_NONE) then
       icecover%apply_pressure         = .false.
    else
       icecover%apply_pressure         = .true.
    endif
    icecover%apply_friction            = .false.
    icecover%reduce_surface_exchange   = .false.
    icecover%reduce_waves              = .false.
    icecover%modify_winddrag           = ICE_WINDDRAG_NONE

    icecover%ice_albedo                = 0.75_fp
    icecover%ice_conductivity          = 2.04_fp
    icecover%ice_latentheat            = 302.0_fp * 1000000.0_fp
    icecover%ice_density               = 917.0_fp
    icecover%snow_albedo               = 0.9_fp
    icecover%snow_conductivity         = 0.31_fp
    icecover%snow_latentheat           = 110.0_fp * 1000000.0_fp
    icecover%frict_type                = FRICT_AS_DRAG_COEFF
    icecover%frict_val                 = 0.005_fp
    
    if (modeltype == ICECOVER_NONE) then
        istat = clr_icecover(icecover)
    else
        istat = 0
    endif
end function select_icecover_model

!> set default values for output flags
subroutine set_default_output_flags(flags, modeltype, default)
   type(icecover_output_flags), intent(inout) :: flags !< output flags
   integer, intent(in) :: modeltype !< ice cover model type
   logical, optional, intent(in) :: default !< default value for output flags
   
   logical :: default_ !< local default value for output flags
   
   if (present(default)) then
      flags%default = default
   end if
   default_ = flags%default
   
   if (modeltype == ICECOVER_NONE) then
      default_ = .false.
   end if
   
   flags%ice_s1 = default_
   flags%ice_zmin = default_
   flags%ice_zmax = default_
   flags%ice_area_fraction = default_
   flags%ice_thickness = default_
   flags%ice_pressure = default_
   
   if (modeltype /= ICECOVER_SEMTNER) then
      default_ = .false.
   end if
   
   flags%ice_temperature = default_
   flags%snow_thickness = default_
   flags%snow_temperature = default_
end subroutine set_default_output_flags

!> Allocate the arrays of an icecover data structure.
function alloc_icecover(icecover, nmlb, nmub) result(istat)
    type (icecover_type), intent(inout) :: icecover !< data structure containing ice cover data
    integer, intent(in) :: nmlb !< lower bound index for spatial data arrays
    integer, intent(in) :: nmub !< upper bound index for spatial data arrays
    integer :: istat !< status flag for allocation

    istat = 0

    ! state variables
    if (icecover%modeltype /= ICECOVER_NONE) then
       if (istat==0) allocate(icecover%ice_area_fraction(nmlb:nmub), STAT = istat)
       if (istat==0) allocate(icecover%ice_thickness(nmlb:nmub), STAT = istat)
       if (istat==0) then
          icecover%ice_area_fraction = 0.0_fp
          icecover%ice_thickness = 0.0_fp
       endif
       if (icecover%modeltype == ICECOVER_SEMTNER) then
          if (istat==0) allocate(icecover%ice_temperature(nmlb:nmub), STAT = istat)
          if (istat==0) allocate(icecover%snow_thickness(nmlb:nmub), STAT = istat)
          if (istat==0) allocate(icecover%snow_temperature(nmlb:nmub), STAT = istat)
          if (istat==0) then
             icecover%ice_temperature = 0.0_fp
             icecover%snow_thickness = 0.0_fp
             icecover%snow_temperature = 0.0_fp
          endif
       endif

       ! other variables
       if (istat==0) allocate(icecover%qh_air2ice(nmlb:nmub), STAT = istat)
       if (istat==0) allocate(icecover%qh_ice2wat(nmlb:nmub), STAT = istat)
       if (istat==0) allocate(icecover%pressure(nmlb:nmub), STAT = istat)
       if (istat==0) allocate(icecover%ice_s1(nmlb:nmub), STAT = istat)
       if (istat==0) allocate(icecover%ice_zmin(nmlb:nmub), STAT = istat)
       if (istat==0) allocate(icecover%ice_zmax(nmlb:nmub), STAT = istat)
       if (istat==0) then
          icecover%qh_air2ice = 0.0_fp
          icecover%qh_ice2wat = 0.0_fp
          icecover%pressure = 0.0_fp
          icecover%ice_s1 = 0.0_fp
          icecover%ice_zmin = 0.0_fp
          icecover%ice_zmax = 0.0_fp
       endif
    endif
end function alloc_icecover


!> Clear the arrays of sedtra_type data structure.
function clr_icecover(icecover) result (istat)
    type (icecover_type), intent(inout) :: icecover !< data structure containing ice cover data
    integer :: istat !< status flag for deallocation

    istat = 0

    ! state variables
    if (associated(icecover%ice_area_fraction)) deallocate(icecover%ice_area_fraction, STAT = istat)
    if (associated(icecover%ice_thickness)) deallocate(icecover%ice_thickness, STAT = istat)
    if (associated(icecover%ice_temperature)) deallocate(icecover%ice_temperature, STAT = istat)
    if (associated(icecover%snow_thickness)) deallocate(icecover%snow_thickness, STAT = istat)
    if (associated(icecover%snow_temperature)) deallocate(icecover%snow_temperature, STAT = istat)

    ! other variables
    if (associated(icecover%qh_air2ice)) deallocate(icecover%qh_air2ice, STAT = istat)
    if (associated(icecover%qh_ice2wat)) deallocate(icecover%qh_ice2wat, STAT = istat)
    if (associated(icecover%pressure)) deallocate(icecover%pressure, STAT = istat)
    if (associated(icecover%ice_s1)) deallocate(icecover%ice_s1, STAT = istat)
    if (associated(icecover%ice_zmin)) deallocate(icecover%ice_zmin, STAT = istat)
    if (associated(icecover%ice_zmax)) deallocate(icecover%ice_zmax, STAT = istat)
end function clr_icecover

!--------------- following routines should move to ice kernel ---------------

!> Update the ice pressure array. I hope that we can extract the initial update_icecover from m_fm_icecover to here ...
!subroutine update_icecover(icecover, nm)
!!!--declarations----------------------------------------------------------------
!    type (icecover_type), intent(inout) :: icecover !< data structure containing ice cover data
!    integer, intent(in) :: nm !< Spatial index
!
!    select case (icecover%modeltype)
!    case (ICECOVER_SEMTNER)
!        ! follow Semtner (1975)
!    case default
!        ! by default no growth
!    end select
!end subroutine update_icecover


!> Update the ice pressure array.
subroutine update_icepress(icecover, ag)
    type (icecover_type), intent(inout) :: icecover !< data structure containing ice cover data
    real(fp), intent(in) :: ag !< gravitational accelaration (m/s2)

    integer :: nm !< Spatial loop index
    real(fp) :: density !< Local variable for ice density
    real(fp), dimension(:), pointer :: areafrac !< Pointer to ice area fraction array
    real(fp), dimension(:), pointer :: pressure !< Pointer to ice pressure array
    real(fp), dimension(:), pointer :: thickness !< Pointer to ice thickness array

    areafrac  => icecover%ice_area_fraction
    pressure  => icecover%pressure
    thickness => icecover%ice_thickness
    density = icecover%ice_density
    do nm = lbound(pressure,1),ubound(pressure,1)
        pressure(nm) = areafrac(nm) * thickness(nm) * density * ag
        ! + optionally snow or is that weight always negligible?
    enddo
end subroutine update_icepress


!> determine effective drag coefficient when ice may be present
pure function ice_drag_effect(icecover, ice_area_fraction, cdw_open) result (cdw_eff)
    type (icecover_type), intent(in) :: icecover !< data structure containing ice cover data
    real(fp), intent(in) :: ice_area_fraction !< area fraction covered by ice (-) 
    real(fp), intent(in) :: cdw_open  !< wind drag exerted via open water (N m-2)
    real(fp) :: cdw_eff !< effective wind drag coefficient (N m-2)

    real(fp) :: c0 !< constant coefficient of cubic drag formula (N m-2)
    real(fp) :: c1 !< linear coefficient of cubic drag formula (N m-2)
    real(fp) :: c2 !< quadratic coefficient of cubic drag formula (N m-2)
    real(fp) :: c3 !< cubic coefficient of cubic drag formula (N m-2)
    real(fp) :: cdw_floe !< wind drag exerted via ice floes (N m-2)
    real(fp) :: cdw_floe_max !< maximum wind drag exerted via ice floes (maximum form drag) (N m-2)
    real(fp) :: cdw_ice !< wind drag exerted via ice cover (N m-2)
    real(fp) :: water_area_fraction !< open water area fraction (-)
    real(fp) :: num !< numerator
    real(fp) :: den !< denominator

    water_area_fraction = 1.0_fp - ice_area_fraction
    
    select case (icecover%modify_winddrag)
    case (ICE_WINDDRAG_NONE) ! no wind drag modification
        
        cdw_eff = cdw_open
        
    case (ICE_WINDDRAG_CUBIC) ! Chapman & Massey (ADCIRC)
        
        ! ADCIRC default "IceCube" formula:
        ! cdrag = c0 + c1*A + c2*A^2 + c3*A^3 with A = ice_area_fraction
        !
        ! where drag coefficients c0, c1, c2, c3 follow from the following conditions:
        ! cdrag(A = 0) = 0.00075
        ! cdrag(A = 0.5) = 0.0025
        ! d cdrag/d A (A = 0.5) = 0
        ! cdrag(A = 1) = 0.00125
        !
        c0 =  0.00075_fp
        c1 =  0.00750_fp
        c2 = -0.00900_fp
        c3 =  0.00200_fp
        cdw_ice = c0 + (c1 + (c2 + c3 * ice_area_fraction) * ice_area_fraction ) * ice_area_fraction
        cdw_eff = max(cdw_ice, cdw_open)
        
    case (ICE_WINDDRAG_RAYS) ! Chapman et al (ADCIRC)

        ! ADCIRC "RaysIce" formula:
        ! cdrag = c0 + c1*A*(1-A) with A = ice_area_fraction

        ! Jensen & Ebersole (2012) ERDC/CHL TR-12-26
        ! Modeling of Lake Michigan Storm Waves and Water Levels
        ! refer to Chapman et al. (2005, 2009) for
        ! cdw_eff = 0.001_fp * (0.125_fp + 0.5_fp * ice_area_fraction * (1.0_fp  ice_area_fraction))

        c0 =  0.00125_fp
        c1 =  0.00500_fp
        cdw_ice = c0 + c1 * ice_area_fraction * water_area_fraction
        cdw_eff = max(cdw_ice, cdw_open)

    case (ICE_WINDDRAG_LB05) ! Lupkes and Birnbaum (2005)
        
        cdw_ice = 1.5e-3_fp
        num = water_area_fraction * (water_area_fraction**0.8_fp + 0.5_fp * (1.0_fp - 0.5_fp * ice_area_fraction)**2)
        den = 31.0_fp + 90.0_fp * ice_area_fraction * water_area_fraction
        cdw_floe = 0.34e-3_fp * ice_area_fraction * ice_area_fraction * num / den
        
        cdw_eff = water_area_fraction * cdw_open + ice_area_fraction * cdw_ice + cdw_floe
        
    case (ICE_WINDDRAG_AN10) ! Andreas et al. (2010)
        
        c0 = 1.5e-3_fp
        c1 = 2.233e-3_fp
        cdw_eff = c0 + c1 * ice_area_fraction * water_area_fraction

    case (ICE_WINDDRAG_LINEAR)
        
        cdw_eff = water_area_fraction * cdw_open
        
    case (ICE_WINDDRAG_JOYCE19)

        cdw_ice = icecover%ice_skin_drag
        cdw_floe_max = icecover%maximum_ice_form_drag
        
        ! Eq. (6) of Joyce et al, 2019 equivalent to Eq. (A4) of Lupkes et al, 2012
        cdw_eff = cdw_open * water_area_fraction + cdw_ice * ice_area_fraction + 4.0_fp * cdw_floe_max * ice_area_fraction * water_area_fraction

    end select
end function ice_drag_effect


!> compute the icecover quantities that are only needed for output
subroutine icecover_prepare_output(icecover, water_level, water_density, ag)
    type (icecover_type), intent(inout) :: icecover !< data structure containing ice cover data
    real(fp), dimension(:) :: water_level !< water level (m+REF)
    real(fp), dimension(:) :: water_density !< water density (kg m-3)
    real(fp) :: ag !< gravitational acceleration (m/s2)
    
    integer :: ndx !< number of spatial points
    integer :: n !< loop index
    
    real(fp) :: open_water_level !< open water level (m+REF)
    real(fp) :: zmin !< lower ice cover surface height (m+REF)
    real(fp) :: zmax !< upper ice cover surface height (m+REF)
    
    ndx = size(icecover%ice_area_fraction)
    do n = 1, ndx
       open_water_level = water_level(n) + icecover%pressure(n) / water_density(n) / ag
       zmin = open_water_level - icecover%ice_thickness(n) * icecover%ice_density / water_density(n)
       zmax = zmin + icecover%ice_thickness(n)

       icecover%ice_s1(n) = open_water_level
       icecover%ice_zmin(n) = zmin
       icecover%ice_zmax(n) = zmax
    end do
end subroutine icecover_prepare_output

end module icecover_module
