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

module m_fm_icecover
use precision
use icecover_module
use icecover_input_module
implicit none

!
! Global data
!
type(icecover_type), target                                :: ice_data                     !< module instance of the icecover data structure
!
real(fp), dimension(:), pointer                            :: ice_af                       !< module pointer to array ice areafrac inside ice_data
real(fp), dimension(:), pointer                            :: ice_h                        !< module pointer to array ice thickness inside ice_data
real(fp), dimension(:), pointer                            :: ice_p                        !< module pointer to array pressure inside ice_data
real(fp), dimension(:), pointer                            :: ice_t                        !< module pointer to array temperature inside ice_data
real(fp), dimension(:), pointer                            :: qh_air2ice                   !< module pointer to array qh_air2ice inside ice_data
real(fp), dimension(:), pointer                            :: qh_ice2wat                   !< module pointer to array qh_ice2wat inside ice_data
real(fp), dimension(:), pointer                            :: snow_h                       !< module pointer to array snow thickness inside ice_data
real(fp), dimension(:), pointer                            :: snow_t                       !< module pointer to array snow temperature inside ice_data

integer, pointer                                           :: ja_ice_area_fraction_read    !< flag indicating whether ice area fraction is available via EC module
integer, pointer                                           :: ja_ice_thickness_read        !< flag indicating whether ice thickness is available via EC module

logical, pointer                                           :: ice_hisout                   !< module pointer to flag hisout inside ice_data
logical, pointer                                           :: ice_mapout                   !< module pointer to flag mapout inside ice_data

logical, pointer                                           :: ice_apply_pressure           !< module pointer to flag apply_pressure inside ice_data
logical, pointer                                           :: ice_apply_friction           !< module pointer to flag apply_friction inside ice_data
logical, pointer                                           :: ice_reduce_surface_fluxes    !< module pointer to flag reduce_surface_fluxes inside ice_data
logical, pointer                                           :: ice_reduce_waves             !< module pointer to flag reduce_waves inside ice_data
integer, pointer                                           :: ice_modify_winddrag          !< module pointer to flag modify_winddrag inside ice_data

integer, pointer                                           :: ja_icecover                  !< module pointer to modeltype flag inside ice_data that specifies the ice cover model
integer, pointer                                           :: ice_frctp                    !< module pointer to frict_type inside ice_data

real(fp), pointer                                          :: ice_density                  !< module pointer to ice_density inside ice_data
real(fp), pointer                                          :: ice_albedo                   !< module pointer to ice_albedo inside ice_data
real(fp), pointer                                          :: ice_conductivity             !< module pointer to ice_conductivity inside ice_data
real(fp), pointer                                          :: ice_latentheat               !< module pointer to ice_latentheat inside ice_data
real(fp), pointer                                          :: ice_frcuni                   !< module pointer to frict_val inside ice_data

real(fp), pointer                                          :: snow_albedo                  !< module pointer to snow_albedo inside ice_data
real(fp), pointer                                          :: snow_conductivity            !< module pointer to snow_conductivity inside ice_data
real(fp), pointer                                          :: snow_latentheat              !< module pointer to snow_latentheat inside ice_data


contains


!> Nullify/initialize ice data structure.
subroutine fm_ice_null()
!!--declarations----------------------------------------------------------------
    !
    implicit none
    !
    ! Function/routine arguments
    !
    ! NONE
    !
    ! Local variables
    !
    integer                                                    :: istat     !< status flag for allocation
!
!! executable statements -------------------------------------------------------
!
    istat = null_icecover(ice_data)
    call fm_ice_update_all_pointers()
end subroutine fm_ice_null


!> Update all ice data structure.
subroutine fm_ice_update_all_pointers()
!!--declarations----------------------------------------------------------------
    !
    implicit none
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
    ja_ice_area_fraction_read => ice_data%ice_areafrac_forcing_available
    ja_ice_thickness_read => ice_data%ice_thickness_forcing_available

    ja_icecover => ice_data%modeltype
   
    ice_hisout => ice_data%hisout
    ice_mapout => ice_data%mapout
    
    ice_apply_pressure  => ice_data%apply_pressure
    ice_apply_friction  => ice_data%apply_friction
    ice_reduce_waves    => ice_data%reduce_waves
    ice_modify_winddrag => ice_data%modify_winddrag
    
    ice_albedo          => ice_data%ice_albedo
    ice_conductivity    => ice_data%ice_conductivity
    ice_latentheat      => ice_data%ice_latentheat
    ice_density         => ice_data%ice_density
    ice_frctp           => ice_data%frict_type
    ice_frcuni          => ice_data%frict_val
    
    snow_albedo         => ice_data%snow_albedo
    snow_conductivity   => ice_data%snow_conductivity
    snow_latentheat     => ice_data%snow_latentheat
    
    call fm_ice_update_spatial_pointers()
end subroutine fm_ice_update_all_pointers


!> Update spatial pointers after (de)allocation
subroutine fm_ice_update_spatial_pointers()
!!--declarations----------------------------------------------------------------
    !
    implicit none
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
    ice_af => ice_data%ice_areafrac
    ice_h  => ice_data%ice_thickness
    !
    ice_t  => ice_data%ice_temperature
    snow_h => ice_data%snow_thickness
    snow_t => ice_data%snow_temperature
    !
    ice_p  => ice_data%pressure
    qh_air2ice => ice_data%qh_air2ice
    qh_ice2wat => ice_data%qh_ice2wat
end subroutine fm_ice_update_spatial_pointers


!> activation of icecover module based on external forcing input
subroutine fm_ice_activate_by_ext_forces(ndx)
!!--declarations----------------------------------------------------------------
    !
    implicit none
    !
    ! Function/routine arguments
    !
    integer                                    , intent(in)    :: ndx       !< number of cells in the D-Flow FM domain
    !
    ! Local variables
    !
    integer                                                    :: istat     !< status flag for allocation
!
!! executable statements -------------------------------------------------------
!
    istat = late_activation_ext_force_icecover(ice_data)
    call fm_ice_alloc(ndx)
end subroutine fm_ice_activate_by_ext_forces


!> Allocate the arrays of ice data structure.
subroutine fm_ice_alloc(ndx)
!!--declarations----------------------------------------------------------------
    !
    implicit none
    !
    ! Function/routine arguments
    !
    integer                                    , intent(in)    :: ndx       !< number of cells in the D-Flow FM domain
    !
    ! Local variables
    !
    integer                                                    :: istat     !< status flag for allocation
!
!! executable statements -------------------------------------------------------
!
    if (associated(ice_af)) return ! don't allocate if already allocated - or should we deallocate and realloc?
    
    istat = alloc_icecover(ice_data, 1, ndx)
    call fm_ice_update_spatial_pointers()
end subroutine fm_ice_alloc


!> Clear the arrays of ice data structure.
subroutine fm_ice_clr()
!!--declarations----------------------------------------------------------------
    !
    implicit none
    !
    ! Function/routine arguments
    !
    ! NONE
    !
    ! Local variables
    !
    integer                                                    :: istat     !< status flag for allocation
!
!! executable statements -------------------------------------------------------
!
    istat = clr_icecover(ice_data)
    call fm_ice_null()
end subroutine fm_ice_clr


!> Read the ice cover module configuration from the mdu file
subroutine fm_ice_read(md_ptr, ierror)
!!--declarations----------------------------------------------------------------
    use dfm_error, only: DFM_WRONGINPUT
    use properties, only: tree_data
    implicit none
    !
    ! Function/routine arguments
    !
    type(tree_data)                            , pointer       :: md_ptr   !< pointer to the input file
    integer                                    , intent(inout) :: ierror   !< D-Flow FM error flag
    !
    ! Local variables
    !
    logical                                                    :: error    !< ice module error flag
!
!! executable statements -------------------------------------------------------
!
    call read_icecover(ice_data, md_ptr, 'ice',  error)
    call fm_ice_update_spatial_pointers()
    !
    if (error) then
        ierror = DFM_WRONGINPUT
    endif
end subroutine fm_ice_read


!> Report the ice configuration to the diagnostic output.
subroutine fm_ice_echo(mdia)
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Function/routine arguments
    !
    integer                                    , intent(in)    :: mdia     !< unit number of diagnostic output
    !
    ! Local variables
    !
    logical                                                    :: error    !< ice module error flag
!
!! executable statements -------------------------------------------------------
!
    error = echo_icecover(ice_data, mdia)
end subroutine fm_ice_echo


!> Update the ice pressure array.
subroutine fm_ice_update_press(ag)
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Function/routine arguments
    !
    real(fp)                                   , intent(in)    :: ag       !< gravitational accelaration (m/s2)
    !
    ! Local variables
    !
    ! NONE
!
!! executable statements -------------------------------------------------------
!
    call update_icepress(ice_data, ag)
end subroutine fm_ice_update_press


!> preprocessing for ice cover, because in subroutine HEATUN some ice cover quantities have to be computed
!! this subroutine is comparable with subroutine HEA_ICE.F90 of the Delft3D-FLOW ice module
subroutine preprocess_icecover(n, Qlong_ice, tempwat, saltcon, wind, timhr)
!!--declarations----------------------------------------------------------------
    use MessageHandling
    use m_flow                         ! test om tair(.) te gebruiken
    use m_flowgeom   , only: nd
    use m_flowtimes  , only: dts
    use m_physcoef   , only: vonkar
    use physicalconsts, only: CtoKelvin
    use m_heatfluxes , only: cpw
    implicit none
    !
    ! Function/routine arguments
    !
    integer                                    , intent(in)    :: n             !< node number
    real(fp)                                   , intent(in)    :: Qlong_ice     !< part of Qlong computed in HEATUN
    real(fp)                                   , intent(in)    :: tempwat       !< temperature of water at top layer [degC]
    real(fp)                                   , intent(in)    :: saltcon       !< salinity of water at top layer [degC]
    real(fp)                                   , intent(in)    :: wind          !< wind speed [m/s]
    real(fp)                                   , intent(in)    :: timhr         !< time [h]
    !
    ! Local variables
    !
    integer             :: iter      !< iteration number
    integer             :: icount    !< number of flow links
    integer             :: LL        !< flow link index
    logical             :: converged !< flag for convergence in iterative process for computation of effective back radiation based on ice or snow
    real(fp)            :: b         !< empirical constant in computation of c_tz
    real(fp)            :: p_r       !< molecular Prandtl number (-)
    real(fp)            :: p_rt      !< turbulent Prandtl number (-)
    real(fp)            :: kin_vis   !< kinematic viscosity (kg m-1 s-1)
    real(fp)            :: t_freeze  !< freezing temperature of water (degC)
    real(fp)            :: sum       !< sum of water depths at flow links (m)
    real(fp)            :: b_t       !< molecular sublayer correction
    real(fp)            :: c_tz      !< heat transfer coefficient (J m-2 s-1 K-1)
    real(fp)            :: conduc    !< auxiliary variable with conductivity of ice (J m-1 s-1 K-1) or product of conductivity of ice times conductivity of snow (J**2 m-2 s-2 K-2)
    real(fp)            :: D_t       !< temperature difference (degC)
    real(fp)            :: D_ice     !< auxiliary variable with thickness of ice (m) or product of thickness of ice&snow times conductivity of ice&snow (J s-1 K-1)
    real(fp)            :: tsi       !< surface temperature with surface being either water, ice or snow (degC)
    real(fp)            :: coef1     !< auxiliary variable; see D-Flow FM Technical Reference Manual for a detailed description (J m-2 s-1)
    real(fp)            :: coef2     !< auxiliary variable; see D-Flow FM Technical Reference Manual for a detailed description (J m-2 s-1 k-1)
    real(fp)            :: alpha     !< relaxation factor (-)
    real(fp)            :: z00       !< open water roughness height (m)
    real(fp)            :: ustar     !< wind shear velocity (m s-1)
    real(fp)            :: hdz       !< Vertical coordinate corresponding to the temperature, for which the mid of the water column is taken  [m]
    real(fp)            :: rhow      !< density of water (kg m-3)
    real(fp)            :: Qlong     !< effective back radiation, computed after convergence of iteration process (J m-2 s)
    
!
!! executable statements -------------------------------------------------------
!
    ! Initialization
    b        = 3.0_fp
    p_r      = 13.0_fp
    p_rt     = 0.85_fp
    kin_vis  = 0.0000018_fp
    rhow     = 1000.0_fp
    z00      = 2e-4_fp
    ustar    = 0.025_fp * wind ! See Eq. (12.5) in D-Flow FM User Manual: ustar = sqrt(C_D) * U_10
    hdz      = 0.0_fp  
    converged = .false.
    
    ! Compute freezing point
    t_freeze = ( -0.0575d0 - 2.154996d-4*saltcon ) * saltcon

    select case (ja_icecover)
    case (ICECOVER_SEMTNER)
        ! follow Semtner (1975)
        !
        ! Compute conductivity, depending on the presence of both ice and snow 
        ! 
        if ( snow_h(n) < 0.001_fp ) then
           conduc = ice_conductivity
           D_ice = max (0.01_fp, ice_h(n))
           tsi = ice_t(n)
        else
           conduc = (ice_conductivity * snow_conductivity)
           D_ice = ( max (0.01_fp, ice_h(n)) * snow_conductivity + max (0.01_fp, snow_h(n)) * ice_conductivity)
           tsi = snow_t(n)
        endif    
        !
        ! Compute longwave radiation flux from ice surface according to Eq. (7) in (Wang, 2005)
        ! including an iteration proces
        !
        do iter = 1,5
           coef1 = Qlong_ice * (tsi + CtoKelvin)**4.0_fp
           coef2 = 4.0_fp * Qlong_ice * (tsi + CtoKelvin)**3.0_fp
           D_t = (qh_air2ice(n) - coef1 - conduc * tsi / D_ice) / (coef2 + conduc / D_ice)
           tsi = tsi + D_t    
           if (abs(D_t) < 1e-2_fp) then
              converged = .true.
              if (tsi > 0.0_fp) then
                 ! melting
                 Qlong = coef1
              else
                 ! freezing
                 Qlong = max(0.0_fp, coef1 + coef2 * D_t)
              endif
              !
              ! apply relaxation for stability reasons
              !
              alpha = 0.5_fp
              if (snow_h(n) < 0.001_fp) then
                 ice_t(n) = alpha * tsi + (1.0_fp - alpha) * ice_t(n)
              else
                 snow_t(n) = alpha * tsi + (1.0_fp - alpha) * snow_t(n)
              endif
              !
              ! limit ice and snow temperature
              !
              if (ice_h(n) > 0.001_fp) then
                  ice_t(n)  = max (-25.0_fp, min(ice_t(n), 0.0_fp))
              endif    
              if (snow_h(n) > 0.001_fp) then
                  snow_t(n) = max (-25.0_fp, min(snow_t(n), 0.0_fp))
              endif    
              !
              qh_air2ice(n) = qh_air2ice(n) - Qlong
              !
              ! no freezing in case of air temperatures above zero
              !
              if (tair(n) > 0.0_fp .and. qh_air2ice(n) < 0.0_fp) then
                 qh_air2ice(n) = 0.0_fp
              endif 
              !
              ! no melting in case of air temperatures below zero
              !
              if (tair(n) < 0.0_fp .and. qh_air2ice(n) > 0.0_fp) then
                 qh_air2ice(n) = 0.0_fp
              endif 
              exit ! jump out of the iteration loop
           endif
        enddo
        !
        if (.not. converged) then
            !! write (lundia,*) 'Ice iteration not converged for NM =',nm
        endif    
        !
        ! Compute ice to water flux according to Wang (2015)
        !
        ! Calculate the molecular sublayer correction b_t
        !
        b_t  = b * sqrt(z00 * ustar / kin_vis ) * (p_r)**0.666_fp
        !
        ! Calculate HDZ to be used for the computation of c_tz (NB. In this implementation the same for 2D and 3D)
        !
        sum = 0.0_fp
        icount = 0
        do LL  = 1, nd(n)%lnx
           sum = sum + hu(LL)
           icount = icount +1
        enddo
        hdz = 0.5_fp * sum / max(1, icount)
        !
        ! Calculate heat transfer coefficient c_tz
        !
        c_tz = ustar / ( b_t + p_rt * log (hdz/z00) / vonkar )
        !
        ! Calculate heat flux out of the ocean
        !
        qh_ice2wat(n) = rhow * cpw * c_tz * min(-0.01, tempwat - t_freeze ) 
        !
        ! extra output for ice testbasin
        !if (n==25) then
        !    write (msgbuf,'(a,3f10.3,a,4f10.3,a,3f10.3)') 'ice fluxes:',timhr/24,qh_air2ice(n), qh_ice2wat(n), ' ice/snow h/t:', ice_h(n), ice_t(n), snow_h(n), snow_t(n), &
        !                                    ' rest:',tempwat, tair(n), tsi; call msg_flush()
        !endif
        !
        ! adaptation of QH_ICE2WAT conform KNMI approach (QH_ICE2WAT = 2.4 W/m2) 
        !
        !! qh_ice2wat(n) = -2.4_fp 
        !
        if ( isnan(qh_ice2wat(n)) ) then
           write (msgbuf,'(a,i5,10f10.3)') 'NAN in PREPROCESS_ICECOVER',n,qh_ice2wat(n); call msg_flush()
        endif
        ! 
    case default
        ! no preparation needed
    end select
end subroutine preprocess_icecover



!> update the ice cover -- initial coding here with full access to D-Flow FM arrays via use statements
!! let's see if we can make it gradually more modular and move functionality to the icecover_module.
subroutine update_icecover()
!!--declarations----------------------------------------------------------------
    use m_flowgeom   , only: ndx
    use m_flowtimes  , only: dts
    use m_wind       , only: tair, rain, jarain
    implicit none
    !
    ! Function/routine arguments
    !
    ! NONE
    !
    ! Local variables
    !
    integer          :: n
    double precision :: conv_factor
!
!! executable statements -------------------------------------------------------
!

    conv_factor = 1.0_fp / (1000.0_fp * 86400.0_fp)
    select case (ja_icecover)
    case (ICECOVER_SEMTNER)
        ! follow Semtner (1975)
 
        ! Compute snow growth (NB. presence of ice is required)
        if (jarain == 1) then   ! check whether rainfall input is prescribed
            do n = 1, ndx
               if (tair(n) < 0.0_fp .and. ice_h(n) > 0.01_fp .and. rain(n) > 0.0_fp ) then
                  snow_h(n) = snow_h(n) + dts * rain(n) * conv_factor
               endif
            enddo
        endif
    
        ! Compute ice growth or melt or melting of snow
        do n = 1, ndx
           if (tair(n) < 0.0_fp .or. ice_h(n) > 0.001_fp ) then
               if (qh_air2ice(n) > 0.0_fp) then
                   if ( snow_h(n) < 0.001_fp ) then
                       ! melting of ice
                       !
                       ice_h(n) = ice_h(n) + dts * ( -qh_air2ice(n) + qh_ice2wat(n) ) / ice_latentheat
                   else
                       ! melting of snow
                       !
                       snow_h(n) = snow_h(n) + dts * ( 0.0_fp - qh_air2ice(n) ) / snow_latentheat
                   endif
               else
                   ! freezing of ice
                   !
                   ice_h(n) = ice_h(n) + dts * ( -qh_air2ice(n) + qh_ice2wat(n) ) / ice_latentheat
               endif
           endif
        enddo
        
    case default
        ! by default no growth
    end select
end subroutine update_icecover


!> determine effective drag coefficient when ice may be present
pure function fm_ice_drag_effect(ice_af, cdw) result (cdeff)
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Function/routine arguments
    !
    real(fp)                                   , intent(in)    :: ice_af   !< ice area fraction (-)
    real(fp)                                   , intent(in)    :: cdw      !< wind drag exerted via open water
    real(fp)                                                   :: cdeff    !< effective wind drag coefficient
    !
    ! Local variables
    !
    ! NONE
!
!! executable statements -------------------------------------------------------
!
    cdeff = ice_drag_effect(ice_data, ice_af, cdw)
end function fm_ice_drag_effect

end module m_fm_icecover
