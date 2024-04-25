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
private

!
! public data types
!
public icecover_type

! public parameters
!
integer, parameter, public :: ICECOVER_NONE    = 0 !< no ice cover
integer, parameter, public :: ICECOVER_EXT     = 1 !< externally forced ice cover --> EC module, or BMI?
integer, parameter, public :: ICECOVER_SEMTNER = 2 !< ice thickness computed based on Semtner (1975)
! ... add IcePack

integer, parameter, public :: FRICT_AS_DRAG_COEFF = 11 ! should be extension of D-Flow FM friction numbers

integer, parameter, public :: ICE_WINDDRAG_NONE    = 0 !< no effect, normal wind drag
integer, parameter, public :: ICE_WINDDRAG_CUBIC   = 1 !< Based on ADCIRC (Chapman & Massey)
integer, parameter, public :: ICE_WINDDRAG_LB05    = 2 !< Lupkes and Birnbaum (2005)
integer, parameter, public :: ICE_WINDDRAG_AN10    = 3 !< Andreas et al (2010)
integer, parameter, public :: ICE_WINDDRAG_LINEAR  = 4 !< no wind drag below ice
integer, parameter, public :: ICE_WINDDRAG_RAYS    = 5 !< Based on ADCIRC (Chapman et al., 2005)

!
! public routines
!
public null_icecover
public select_icecover_model
public late_activation_ext_force_icecover
public alloc_icecover
public clr_icecover
!public update_icecover
public update_icepress
public ice_drag_effect

! ice cover type
type icecover_type
    !
    ! input
    !
    logical  :: hisout                            !< flag indicating whether ice cover should be written to history-file
    logical  :: mapout                            !< flag indicating whether ice cover should be written to map-file
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
    integer  :: ice_areafrac_forcing_available    !< flag indicating whether ice area fraction is available via external forcing
    integer  :: ice_thickness_forcing_available   !< flag indicating whether ice thickness is available via external forcing
    !
    real(fp) :: ice_albedo                        !< albedo of ice (-)
    real(fp) :: ice_conductivity                  !< conductivity of ice (W m-1 K-1)
    real(fp) :: ice_latentheat                    !< latent heat of ice (kJ kg-1)
    real(fp) :: ice_density                       !< ice density (kg m-3)
    real(fp) :: snow_albedo                       !< albedo of snow (-)
    real(fp) :: snow_conductivity                 !< conductivity of snow (W m-1 K-1)
    real(fp) :: snow_latentheat                   !< latent heat of snow (kJ kg-1)
    real(fp) :: frict_val                         !< friction coefficient of ice cover (unit depends on frict_type)
    !
    ! state
    !
    real(fp), dimension(:), pointer :: ice_areafrac     => null() !< area fraction covered by ice (-)
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
end type icecover_type

contains

!> Nullify/initialize an icecover data structure.
function null_icecover(icecover) result(istat)
!!--declarations----------------------------------------------------------------
    implicit none
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
    nullify(icecover%ice_areafrac)
    nullify(icecover%ice_thickness)
    nullify(icecover%snow_thickness)
    !
    ! extra
    !
    nullify(icecover%qh_air2ice)
    nullify(icecover%qh_ice2wat)
    nullify(icecover%pressure)
end function null_icecover


!> activation of icecover module based on external forcing input
function late_activation_ext_force_icecover(icecover) result(istat)
!!--declarations----------------------------------------------------------------
    implicit none
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
       ! note: spatial arrays haven't been allocated yet!
    else
       ! don't overrule previously selected icecover ...
       call mess(LEVEL_FATAL, 'Ice cover forcing data conflicts with selected ice cover model.')
    endif
end function late_activation_ext_force_icecover


!> set default values for selected ice cover model and allocate
function select_icecover_model(icecover, modeltype) result(istat)
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Function/routine arguments
    !
    type (icecover_type)                       , intent(inout) :: icecover  !< data structure containing ice cover data
    integer                                    , intent(in)    :: modeltype !< desired ice cover type
    integer                                                    :: istat     !< status flag for allocation
    !
    ! Local variables
    !
    ! None
!
!! executable statements -------------------------------------------------------
!
    icecover%modeltype                 = modeltype

    icecover%hisout                    = .false.
    icecover%mapout                    = .false.
    
    icecover%ice_areafrac_forcing_available   = 0
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


!> Allocate the arrays of an icecover data structure.
function alloc_icecover(icecover, nmlb, nmub) result(istat)
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Function/routine arguments
    !
    type (icecover_type)                       , intent(inout) :: icecover  !< data structure containing ice cover data
    integer                                    , intent(in)    :: nmlb      !< lower bound index for spatial data arrays
    integer                                    , intent(in)    :: nmub      !< upper bound index for spatial data arrays
    integer                                                    :: istat     !< status flag for allocation
    !
    ! Local variables
    !
    ! NONE
!
!! executable statements -------------------------------------------------------
!
    istat = 0
    !
    ! state
    !
    if (icecover%modeltype /= ICECOVER_NONE) then
       if (istat==0) allocate(icecover%ice_areafrac (nmlb:nmub), STAT = istat)
       if (istat==0) allocate(icecover%ice_thickness(nmlb:nmub), STAT = istat)
       if (istat==0) then
          icecover%ice_areafrac    = 0.0_fp
          icecover%ice_thickness   = 0.0_fp
       endif
       if (icecover%modeltype == ICECOVER_SEMTNER) then
          if (istat==0) allocate(icecover%ice_temperature (nmlb:nmub), STAT = istat)
          if (istat==0) allocate(icecover%snow_thickness  (nmlb:nmub), STAT = istat)
          if (istat==0) allocate(icecover%snow_temperature(nmlb:nmub), STAT = istat)
          if (istat==0) then
             icecover%ice_temperature  = 0.0_fp
             icecover%snow_thickness   = 0.0_fp
             icecover%snow_temperature = 0.0_fp
          endif
       endif
    endif
    !
    ! extra
    !
    if (icecover%modeltype /= ICECOVER_NONE) then
       if (istat==0) allocate(icecover%qh_air2ice(nmlb:nmub), STAT = istat)
       if (istat==0) allocate(icecover%qh_ice2wat(nmlb:nmub), STAT = istat)
       if (istat==0) allocate(icecover%pressure  (nmlb:nmub), STAT = istat)
       if (istat==0) then
          icecover%qh_air2ice = 0.0_fp
          icecover%qh_ice2wat = 0.0_fp
          icecover%pressure   = 0.0_fp
       endif
    endif
end function alloc_icecover


!> Clear the arrays of sedtra_type data structure.
function clr_icecover(icecover) result (istat)
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Function/routine arguments
    !
    type (icecover_type)                       , intent(inout) :: icecover  !< data structure containing ice cover data
    integer                                                    :: istat     !< status flag for deallocation
    !
    ! Local variables
    !
    ! NONE
!
!! executable statements -------------------------------------------------------
!
    istat = 0
    !
    ! state
    !
    if (associated(icecover%ice_areafrac    )) deallocate(icecover%ice_areafrac    , STAT = istat)
    if (associated(icecover%ice_thickness   )) deallocate(icecover%ice_thickness   , STAT = istat)
    if (associated(icecover%ice_temperature )) deallocate(icecover%ice_temperature , STAT = istat)
    if (associated(icecover%snow_thickness  )) deallocate(icecover%snow_thickness  , STAT = istat)
    if (associated(icecover%snow_temperature)) deallocate(icecover%snow_temperature, STAT = istat)
    !
    ! extra
    !
    if (associated(icecover%qh_air2ice)) deallocate(icecover%qh_air2ice, STAT = istat)
    if (associated(icecover%qh_ice2wat)) deallocate(icecover%qh_ice2wat, STAT = istat)
    if (associated(icecover%pressure  )) deallocate(icecover%pressure  , STAT = istat)
end function clr_icecover

!--------------- following routines should move to ice kernel ---------------

!> Update the ice pressure array. I hope that we can extract the initial update_icecover from m_fm_icecover to here ...
!subroutine update_icecover(icecover, nm)
!!!--declarations----------------------------------------------------------------
!    !
!    ! Function/routine arguments
!    !
!    type (icecover_type)                       , intent(inout) :: icecover  !< data structure containing ice cover data
!    integer                                    , intent(in)    :: nm        !< Spatial index
!    !
!    ! Local variables
!    !
!!
!!! executable statements -------------------------------------------------------
!!
!    select case (icecover%modeltype)
!    case (ICECOVER_SEMTNER)
!        ! follow Semtner (1975)
!    case default
!        ! by default no growth
!    end select
!end subroutine update_icecover


!> Update the ice pressure array.
subroutine update_icepress(icecover, ag)
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Function/routine arguments
    !
    type (icecover_type)                       , intent(inout) :: icecover  !< data structure containing ice cover data
    real(fp)                                   , intent(in)    :: ag        !< gravitational accelaration (m/s2)
    !
    ! Local variables
    !
    integer                         :: nm            !< Spatial loop index
    real(fp)                        :: density       !< Local variable for ice density
    real(fp), dimension(:), pointer :: areafrac      !< Pointer to ice area fraction array
    real(fp), dimension(:), pointer :: pressure      !< Pointer to ice pressure array
    real(fp), dimension(:), pointer :: thickness     !< Pointer to ice thickness array
!
!! executable statements -------------------------------------------------------
!
    areafrac  => icecover%ice_areafrac
    pressure  => icecover%pressure
    thickness => icecover%ice_thickness
    density  =  icecover%ice_density
    do nm = lbound(pressure,1),ubound(pressure,1)
        pressure(nm) = areafrac(nm) * thickness(nm) * density * ag
        ! + optionally snow or is that weight always negligible?
    enddo
end subroutine update_icepress


!> determine effective drag coefficient when ice may be present
pure function ice_drag_effect(icecover, ice_af, cdw) result (cdeff)
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Function/routine arguments
    !
    type (icecover_type)                       , intent(in)    :: icecover  !< data structure containing ice cover data
    real(fp)                                   , intent(in)    :: ice_af    !< area fraction covered by ice (-) 
    real(fp)                                   , intent(in)    :: cdw       !< wind drag exerted via open water
    real(fp)                                                   :: cdeff     !< effective wind drag coefficient
    !
    ! Local variables
    !
    real(fp) :: c0     !< constant coefficient of cubic drag formula
    real(fp) :: c1     !< linear coefficient of cubic drag formula
    real(fp) :: c2     !< quadratic coefficient of cubic drag formula
    real(fp) :: c3     !< cubic coefficient of cubic drag formula 
    real(fp) :: cdf    !< wind drag exerted via ice floes
    real(fp) :: cdi    !< wind drag exerted via ice cover
    real(fp) :: wat_af !< open water area fraction
    real(fp) :: num    !< numerator
    real(fp) :: den    !< denominator
!
!! executable statements -------------------------------------------------------
!
    wat_af = 1.0_fp - ice_af
    
    select case (icecover%modify_winddrag)
    case (ICE_WINDDRAG_NONE) ! no wind drag modification
        
        cdeff = cdw
        
    case (ICE_WINDDRAG_CUBIC) ! Chapman & Massey (ADCIRC)
        
        ! ADCIRC default "IceCube" formula:
        ! cdrag = c0 + c1*A + c2*A^2 + c3*A^3 with A = ice_af
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
        cdi = c0 + (c1 + (c2 + c3 * ice_af) * ice_af ) * ice_af
        cdeff = max(cdi, cdw)
        
    case (ICE_WINDDRAG_RAYS) ! Chapman et al (ADCIRC)

        ! ADCIRC "RaysIce" formula:
        ! cdrag = c0 + c1*A*(1-A) with A = ice_af

        ! Jensen & Ebersole (2012) ERDC/CHL TR-12-26
        ! Modeling of Lake Michigan Storm Waves and Water Levels
        ! refer to Chapman et al. (2005, 2009) for
        ! cdeff = 0.001_fp * (0.125_fp + 0.5_fp * ice_af * (1.0_fp  ice_af))

        c0 =  0.00125_fp
        c1 =  0.00500_fp
        cdi = c0 + c1 * ice_af * wat_af
        cdeff = max(cdi, cdw)

    case (ICE_WINDDRAG_LB05) ! Lupkes and Birnbaum (2005)
        
        cdi = 1.5e-3_fp
        num = wat_af * (wat_af**0.8_fp + 0.5_fp * (1.0_fp - 0.5_fp * ice_af)**2)
        den = 31.0_fp + 90.0_fp * ice_af * wat_af
        cdf = 0.34e-3_fp * ice_af * ice_af * num / den
        
        cdeff = wat_af * cdw + ice_af * cdi + cdf
        
    case (ICE_WINDDRAG_AN10) ! Andreas et al. (2010)
        
        c0 = 1.5e-3_fp
        c1 = 2.233e-3_fp
        cdeff = c0 + c1 * ice_af * wat_af

    case (ICE_WINDDRAG_LINEAR)
        
        cdeff = wat_af * cdw
        
    end select
end function ice_drag_effect

end module icecover_module
