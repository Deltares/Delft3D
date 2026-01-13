subroutine hea_ice      (nm         ,toth_i    ,toth_w    ,f_w       , &
                        & t_freeze  ,twat      ,wmagn     ,rhow      ,ustar     , &
                        & hdz       ,z00       ,cp        ,qbl       , &
                        & coef_qbl  ,kfssnw    ,tair      , &
                        & h_ice     ,h_snow    ,t_ice     ,t_snow    ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                     
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: hea_ice.f90 66456 2020-04-26 14:09:37Z goede $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20190705_ice_modelling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_ice/hea_ice.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes heat exchange in case of ice
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use timers
    use globaldata
!
    implicit none
!
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: k_ice
    real(fp)               , pointer :: k_snow
    real(fp)               , pointer :: timjan
    real(fp)               , pointer :: timhr
    real(fp)               , pointer :: vonkar
    integer                , pointer :: lundia

! Global variables
!
    integer , intent(in)  :: nm     !  computation cell number
    real(fp), intent(in)  :: coef_qbl !  auxilirary value for QBL
    real(fp), intent(in)  :: cp     !  specified heat of water
    real(fp), intent(in)  :: hdz    !  acceleration due to gravity
    real(fp)              :: qbl    !  back radiation
    real(fp), intent(in)  :: rhow   !  density of water
    real(fp), intent(in)  :: ustar  !  friction velocity
    real(fp), intent(in)  :: z00    !  Z0 value
    real(fp), intent(in)  :: tair   !  Air temperature 
    real(fp), intent(in)  :: t_freeze   !  Freezing temperature [C]
    real(fp), intent(in)  :: twat   !  Computed temperature at water surface [C]
    real(fp), intent(in)  :: wmagn  !  wind speed (magnitude) [m/s]
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                :: toth_i !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                :: toth_w !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                :: f_w    !  Description and declaration in esm_alloc_real.f90
    integer,  dimension(gdp%d%nmlb:gdp%d%nmub)                :: kfssnw
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                :: h_ice
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                :: h_snow
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                :: t_ice
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                :: t_snow
!
! Local variables
!
    real(fp)      :: b, p_r, p_t, kin_vis
    real(fp)      :: b_t, c_tz, tm
    real(fp)      :: conduc, D_t, D_ice, tsi, coef1, coef2, alpha
    logical       :: converged
    integer       :: nmin, iter
!
!! executable statements -------------------------------------------------------
!
    k_ice       => gdp%gdice%k_ice
    k_snow      => gdp%gdice%k_snow
    timjan      => gdp%gdheat%timjan
    timhr       => gdp%gdinttim%timhr
    vonkar      => gdp%gdphysco%vonkar
    lundia      => gdp%gdinout%lundia
    !
    tm = timjan + timhr
    nmin = nint(tm*60)
    !
    ! initialize local parameters
    !
    b = 0.0000495_fp       ! empirical constant
    p_r  = 13.0            ! molecular Prandtl number
    p_t  = 0.85_fp         ! turbulent Prandtl number
    kin_vis = 0.0000018_fp ! kinematic viscosity of sea water
    !
    ! Compute surface ice temperature and total heat flux in case of ice:
    !
    if (kfssnw(nm) == 0) then
       conduc = k_ice 
       D_ice = max (0.01, h_ice(nm))
       tsi = t_ice(nm)
    else
       conduc = (k_ice * k_snow)
       D_ice = ( max (0.01, h_ice(nm)) * k_snow + max (0.01, h_snow(nm)) * k_ice)
       tsi = t_snow(nm)
    endif
    !
    ! Iteration proces
    !
    do iter =1,5
       coef1 = coef_qbl * (tsi + 273.15_fp)**4.0_fp
       coef2 = 4.0_fp * coef_qbl * (tsi + 273.15_fp)**3.0_fp
       D_t = (toth_i(nm) - coef1 - conduc * tsi / D_ice) / (coef2 + conduc / D_ice)
       tsi = tsi + D_t    
       if (abs(D_t) .lt. 1e-2 ) then
          converged = .true.
          qbl = coef1 + coef2 * D_t
          if (qbl .lt. 0.0_fp) then
              qbl = 0.0_fp
          endif
          if (tsi .gt. t_freeze) then
             !
             ! in case of melting recompute qbl
             !
             qbl = coef_qbl * (tsi + 273.15_fp)**4.0_fp
          else   
             !
             ! apply relaxation for stability reasons
             !
             alpha = 0.2_fp
             if (kfssnw(nm) == 0 ) then
                t_ice(nm) = alpha * tsi + (1.0_fp - alpha) * t_ice(nm)
             else
                t_snow(nm) = alpha * tsi + (1.0_fp - alpha) * t_snow(nm)
             endif
          endif
          !
          ! limit ice and snow temperature
          !
          t_ice(nm) = min (25.0_fp,  t_ice(nm))
          t_ice(nm) = max (-50.0_fp, t_ice(nm))
          t_snow(nm) = min (25.0_fp,  t_snow(nm))
          t_snow(nm) = max (-50.0_fp, t_snow(nm))
          !
          toth_i(nm) = toth_i(nm) - qbl
          !
          ! no freezing in case of air temperatures above zero
          !
          if (tair .gt. t_freeze .and. toth_i(nm) .lt. 0.0_fp) then
             toth_i(nm) = 0.0_fp
          endif 
          !
          ! no melting in case of air temperatures below zero
          !
          if (tair .lt. t_freeze .and. toth_i(nm) .gt. 0.0_fp) then
             toth_i(nm) = 0.0_fp
          endif 
          goto 123          
       endif
    enddo           
123 continue
    if (.not. converged) then
        !! write (lundia,*) 'Ice iteration not converged for NM =',nm
    endif    
    !
    ! Compute f_w
    ! Calculate the molecular sublayer correction b_t
    !
    b_t  = b * sqrt(z00 * ustar / kin_vis ) * (p_r)**0.666
    !
    ! Calculate heat transfer coefficient c_tz
    !
    c_tz = ustar / ( b_t + p_t * log (hdz/z00) / vonkar )
    !
    ! Calculate heat flux out of the ocean
    !
    ! Wang formula for F_W:
    !
    !! f_w(nm) = rhow * cp * c_tz * (t_freeze - max (0.01_fp,twat) ) ! threshold value is important  
    !
    if ( nm==41) then
       !! write (lundia,'(a,10f10.5)') 'F_W in HEA_ICE',f_w(nm),t_ice(nm), twat
    endif
    !
    ! adaptation if F_W conform KNMI approach (F_W = 2.4 W/m2) 
    !
    ! f_w(nm) = -2.4_fp 
    !
    if ( isnan(f_w(nm)) ) then
       write (lundia,*) 'NAN in HEA_ICE',nm,f_w(nm),t_ice(nm), twat
    endif
    !    
end subroutine hea_ice