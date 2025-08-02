subroutine bedtr2004(u2dh      ,d50       ,d90       ,h1        ,rhosol    , &
                   & tp        ,teta      ,uon       ,uoff      ,uwb       , &
                   & taucr     ,delm      ,ra        ,z0cur     ,fc1       , &
                   & fw1       ,dstar     ,drho      ,phicur    ,qbcu      , &
                   & qbcv      ,qbwu      ,qbwv      ,qswu      ,qswv      , &
                   & tetacr    ,aks       ,fsilt     ,sig       ,thick     , &
                   & concin    ,num_layers_grid      ,deltas    ,ws        ,rksrs     , &
                   & dzduu     ,dzdvv     ,rhowat    ,ag        ,bedw      , &
                   & pangle    ,fpco      ,susw      ,wave      ,eps       , &
                   & subiw     ,vcr       ,error     ,message   ,wform     , &
                   & r         ,phi_phase ,uwbih     )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
!  
!  
!!--description-----------------------------------------------------------------
!
! Compute bed load transport according to Van Rijn
! Note: Formulation used depends on presence of waves in the
! simulation.
! If no waves then use traditional Van Rijn formulation
! If waves then use new parameterization which
! includes wave asymmetry
! Note: The two methods are known to give different results
! (order factor 2) for situations without waves
! Van Rijn (1993,2000)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use sediment_basics_module
    !
    implicit none
!
! Arguments
!
    integer                  , intent(in)  :: num_layers_grid     !< number of layers (counted top to bottom)
    real(fp)                 , intent(in)  :: aks
    real(fp)                 , intent(in)  :: d50
    real(fp)                 , intent(in)  :: d90
    real(fp)                 , intent(in)  :: delm
    real(fp)                 , intent(in)  :: drho
    real(fp)                 , intent(in)  :: dstar
    real(fp)                 , intent(in)  :: dzduu    !< bed slope in U-direction
    real(fp)                 , intent(in)  :: dzdvv    !< bed slope in V-direction
    real(fp)                 , intent(in)  :: fc1
    real(fp)                 , intent(in)  :: fsilt
    real(fp)                 , intent(in)  :: fw1
    real(fp)                 , intent(in)  :: h1
    real(fp)                 , intent(in)  :: phicur
    real(fp)                 , intent(out) :: qbcu
    real(fp)                 , intent(out) :: qbcv
    real(fp)                 , intent(out) :: qbwu
    real(fp)                 , intent(out) :: qbwv
    real(fp)                 , intent(out) :: qswu
    real(fp)                 , intent(out) :: qswv
    real(fp)                 , intent(in)  :: ra
    real(fp)                 , intent(in)  :: rhosol   !< specific density of sediment
    real(fp)                 , intent(in)  :: teta     !< wave direction in degrees
    real(fp)                 , intent(in)  :: tetacr   !< critical Shields parameter
    real(fp)                 , intent(in)  :: taucr
    real(fp)                 , intent(in)  :: tp       !< peak wave period
    real(fp)                 , intent(in)  :: u2dh
    real(fp)                 , intent(in)  :: uwb
    real(fp)                 , intent(inout)  :: uon  
    real(fp)                 , intent(inout)  :: uoff 
    real(fp)                 , intent(in)  :: z0cur
    real(fp)                 , intent(in)  :: rksrs
    real(fp)                 , intent(in)  :: ws
    real(fp)                 , intent(in)  :: deltas
    real(fp), dimension(num_layers_grid), intent(in)  :: sig      !< sigma coordinate of the centre of each layer
    real(fp), dimension(num_layers_grid), intent(in)  :: thick    !< thickness of each layer
    real(fp), dimension(num_layers_grid), intent(in)  :: concin
    real(fp)                 , intent(in)  :: rhowat
    real(fp)                 , intent(in)  :: ag
    real(fp)                 , intent(in)  :: bedw
    real(fp)                 , intent(in)  :: pangle
    real(fp)                 , intent(in)  :: fpco
    real(fp)                 , intent(in)  :: susw
    logical                  , intent(in)  :: wave
    real(fp)                 , intent(in)  :: eps
    integer                  , intent(in)  :: subiw
    real(fp)                 , intent(out) :: vcr
    logical                  , intent(out) :: error
    character(*)             , intent(out) :: message  !< error message in case of error
    integer                  , intent(in)  :: wform
    real(fp)                 , intent(in)  :: r
    real(fp)                 , intent(in)  :: phi_phase
    real(fp)                 , intent(in)  :: uwbih
!
! Local variables
!
    integer       :: ntime
    integer       :: ii
    integer       :: k
    real(fp)      :: uau
    real(fp)      :: uav
    real(fp)      :: acw
    real(fp)      :: betacw
    real(fp)      :: ceavg
    real(fp)      :: cosphiwav
    real(fp)      :: dif_aks
    real(fp)      :: dif_upp
    real(fp)      :: dtt
    real(fp)      :: dzdn
    real(fp)      :: dzds
    real(fp)      :: fac1
    real(fp)      :: fac2
    real(fp)      :: fac_slp
    real(fp)      :: fcw1
    real(fp)      :: phiwav
    real(fp)      :: plead1
    real(fp)      :: plead2
    real(fp)      :: pi_tfor
    real(fp)      :: pi_tback
    real(fp)      :: p2
    real(fp)      :: phi
    real(fp)      :: qbtu
    real(fp)      :: qbtv
    real(fp)      :: rc
    real(fp)      :: rrr2
    real(fp)      :: sbt
    real(fp)      :: sinphiwav
    real(fp)      :: tau1t
    real(fp)      :: tback
    real(fp)      :: tfor
    real(fp)      :: time
    real(fp)      :: tt
    real(fp)      :: vrdelm
    real(fp)      :: vrra
    real(fp)      :: ua
    real(fp)      :: ubtotu
    real(fp)      :: ubtotv
    real(fp)      :: udt
    real(fp)      :: udt1
    real(fp)      :: udt2
    real(fp)      :: dudt
    real(fp)      :: ustar1t
    real(fp)      :: utvec
    real(fp)      :: uut
    real(fp)      :: uvt
    real(fp)      :: veff
    real(fp)      :: rpower
    real(fp)      :: z
    real(fp)      :: ceavgtmp
    real(fp)      :: omega
    real(fp)      :: deltat
    real(fp)      :: f
    real(fp)      :: tc	
!
!! executable statements -------------------------------------------------------
!
    error = .false.
    message = ''
    !
    ! CALCULATE BED LOAD TRANSPORT
    ! Based on intra-wave near bed velocity signal of Isobe-Horikawa
    ! Implementation of TR2004
    !
    phiwav    = teta * degrad
    cosphiwav = cos(phiwav)
    sinphiwav = sin(phiwav)
    rc        = 30.0_fp * z0cur
    ceavg     = 0.0_fp
    !
    ! Make assumptions for friction angle
    !
    phi = 30.0_fp * degrad
    !
    !   BED LOAD TRANSPORT (instantaneous and averaging)
    !
    if (delm < ra/30.0_fp) then
       vrra = 0.0_fp
    else
       !
       ! No distinction between 2DH and 3D yet
       ! in 2DH vrdelm is okay
       ! in 3D better to base vrdelm on uuu and vvv (includes effects of streaming etc.)
       ! in 3D interpolate velocity to delm
       !
       vrdelm = u2dh   * log(30.0_fp*delm/ra) / (-1.0_fp+log(30.0_fp*h1  /ra))
       vrra   = vrdelm * log(30.0_fp*aks /rc) /          log(30.0_fp*delm/rc)
    endif
    ubtotu   = vrra * cos(phicur)
    ubtotv   = vrra * sin(phicur)
    !
    ! coefficient related to vertical structure of velocity profile
    !
    betacw   = 0.25_fp * ((-1.0_fp+log(30.0_fp*h1/rc))/log(30.0_fp*aks/rc))**2
    !
    ! coefficient related to relative strength of wave and current motion: uc/(uc+Uw)
    !
    acw      = u2dh / max(1.0e-6_fp , uwb + u2dh)
    !
    ! grain friction coefficient due to currents and waves
    !
    fcw1     = acw*betacw*fc1 + (1.0_fp-acw)*fw1
    !
    fac1   = 0.5_fp * rhowat * fcw1
    fac2   = 0.5_fp * d50  * rhosol / (dstar**0.3_fp)
    qbtu   = 0.0_fp
    qbtv   = 0.0_fp
    qbcu   = 0.0_fp
    qbcv   = 0.0_fp
    !
    ! phase lead in shear stress according to Nielsen (1992) now included
    !
    if (pangle > 0.0_fp) then
       plead1 = cos(pangle*degrad)
       plead2 = sin(pangle*degrad)
    endif
    !
    if (wave .and. tp>1.0_fp .and. bedw>0.0_fp) then
       ntime = subiw
    else
       ntime = 1
    endif
    dtt      = tp / real(ntime,fp)
    !
    if (wform == 1) then
       !
       ! WAVE VELOCITY ASYMMETRY ACCORDING TO ISOBE-HORIKAWA
       !
       ! Split peak period Tp in an onshore and offshore period
       !
       if (uwb > 0.0_fp) then
           tfor = uoff / (uon+uoff) * tp
           tback    = tp - tfor
           pi_tfor  = pi / tfor
           pi_tback = pi / tback
       endif
    elseif (wform == 2) then
       !
       ! WAVE VELOCITY SKEWNESS & ASYMMETRY ACCORDING TO RUESSINK et al 2012 CE 
       ! Adition made by Marcio Boechat Albernaz
       !
       ! Makes u(0)=0 from ABREU et al 2010 CE (Part 1)
       omega  = 2.0_fp*pi/tp
       f      = sqrt(1.0_fp-r**2.0_fp)                            ! this ensures that (umax-umin)/2 equals Uw
       deltat = (1.0_fp/omega)*asin(r*sin(phi_phase)/(1.0_fp+f))
       !
       ! substitute the algebraic solution computed @bedbc2004]
       uon    = 0.0_fp
       uoff   = 0.0_fp
    endif
    !
    ! wave period subdivision
    !
    do ii = 1,ntime
       !
       ! Construct instantaneous wave velocity
       !
       if (wform == 1) then
          !
          ! WAVE VELOCITY ASYMMETRY ACCORDING TO ISOBE-HORIKAWA
          !
          if (uwb>0.0_fp .and. tp>1.0_fp) then
             time = real(ii-1,fp) * dtt
             if (time < tfor) then
                udt = uon * sin(pi_tfor*time)
                if (pangle > 0.0_fp) then
                  udt2 = uon * sin(pi_tfor*(time+dtt))
                  udt1 = uon * sin(pi_tfor*(time-dtt))
                endif
             else
                udt = -uoff * sin(pi_tback*(time-tfor))
                if (pangle > 0.0_fp) then
                  udt2 = -uoff * sin(pi_tback*(time+dtt-tfor))
                  udt1 = -uoff * sin(pi_tback*(time-dtt-tfor))
                endif
             endif
             if (pangle > 0.0_fp) then
                udt = plead1*udt + plead2*tp*(udt2-udt1)/(4.0_fp*pi*dtt)
             endif
          else
             udt  = 0.0_fp
             dudt = 0.0_fp
          endif
       elseif (wform == 2) then
          !
          ! WAVE VELOCITY SKEWNESS & ASYMMETRY ACCORDING TO RUESSINK et al 2012 CE 
          !
          if (uwb>0.0_fp .and. tp>1.0_fp) then
             time = real(ii-1,fp) * dtt
             ! Makes u(0)=0 from ABREU et al 2010 CE (Part 2)
             tc     = time-deltat
             !
             udt = uwbih*f*(sin(omega*tc)+(r*sin(phi_phase))/(1.0_fp+f))/(1.0_fp-r*cos(omega*tc+phi_phase))   ! Velocity u(t) from Eq. (4)
             if (pangle > 0.0_fp) then !pangle is phase lead defined in .tra file
                udt2 = uwbih*f*(sin(omega*(tc+dtt))+(r*sin(phi_phase))/(1.0_fp+f))/(1.0_fp-r*cos(omega*(tc+dtt)+phi_phase))   ! Velocity u(t) from Eq. (4) with phase lead to t+dtt
                udt1 = uwbih*f*(sin(omega*(tc-dtt))+(r*sin(phi_phase))/(1.0_fp+f))/(1.0_fp-r*cos(omega*(tc-dtt)+phi_phase))   ! Velocity u(t) from Eq. (4) with phase lead to t-dtt
                udt  = plead1*udt + plead2*tp*(udt2-udt1)/(4.0_fp*pi*dtt)                                                     ! Velocity u(t) with phase lead
             endif
             ! update uon and uoff values
             uon  = max(udt,uon )
             uoff = min(udt,uoff)
          else
             udt  = 0.0_fp
             dudt = 0.0_fp
          endif
       endif
       !
       ! Total instantaneous velocity
       !
       uut     = udt*cosphiwav + ubtotu
       uvt     = udt*sinphiwav + ubtotv
       utvec   = sqrt(max(1.0e-4_fp ,(uut**2 +uvt**2)))
       !
       ! bed slope effects on critical shear stress
       ! using Dey (2001) as modified by Van Rijn (Z4056)
       ! (approximation where Schocklitsch and Leitner factors are combined)
       ! positive values refer to downsloping beds
       !
       dzds    =  (dzduu*uut + dzdvv*uvt)/utvec
       dzdn    =  abs(dzduu*uvt + dzdvv*uut)/utvec
       fac_slp = max((1.0_fp-(atan(dzds)/phi)),0.001_fp)**0.75_fp * max((1.0_fp-(atan(dzdn)/phi)),0.001_fp)**0.37_fp
       !
       ! Instantaneous grain-related bed-shear stress due to both currents and waves
       !
       tau1t   = fac1 * utvec * utvec
       !
       ! Total bed-load transport including bed slope effect
       !
       rrr2    = max(min(0.8_fp+0.2_fp*((tau1t/(fac_slp*taucr)-0.8_fp)/1.2_fp) , 1.0_fp) , 0.8_fp )
       tt      = max(0.0001_fp , (tau1t-rrr2*fac_slp*taucr)/(fac_slp*taucr))
       ustar1t = sqrt(tau1t/rhowat)
       sbt     = fsilt * fac2 * ustar1t * tt
       !
       ! Total bed-load transport components
       !
       qbtu    = qbtu + uut/utvec*sbt
       qbtv    = qbtv + uvt/utvec*sbt
       !
       ! Current-related transport only
       !
       qbcu    = qbcu + ubtotu/utvec*sbt
       qbcv    = qbcv + ubtotv/utvec*sbt
    enddo 
    !
    qbcu = qbcu/real(ntime,fp)
    qbcv = qbcv/real(ntime,fp)
    qbwu = qbtu/real(ntime,fp) - qbcu
    qbwv = qbtv/real(ntime,fp) - qbcv
    !
    if (susw>0.0_fp .and. tp>1.0_fp) then
       !
       ! Calculate critical (depth averaged) velocity
       !
       if (d50 >= dclay) then
          vcr = 5.75_fp * sqrt(drho*ag*d50*tetacr) * log10(4.0_fp*h1/d90)
       else
          write(message,'(a,e12.4,a)') 'd50 < ', dclay, ' not allowed'
          error = .true.
          return
       endif
       !
       ! Suspended transport qswu/v due to waves, oriented in wave
       ! direction
       ! Expression modified by Walstra et al (2007)
       !
       ! STEP 1
       ! Integrate concentration between aks and 3*deltas
       !
       do k = num_layers_grid, 1, -1
          dif_aks  = aks/h1           - (1.0_fp+sig(k)-thick(k)/2.0_fp)
          dif_upp  = 3.0_fp*deltas/h1 - (1.0_fp+sig(k)-thick(k)/2.0_fp)
          if (dif_aks<=thick(k) .and. dif_aks>=0.0_fp) then
             !
             ! k-layer contains aks (take part above)
             !
             if (concin(k-1)<1.0e-6_fp .or. concin(k)<1.0e-6_fp) then
                ceavg    = ceavg + concin(k)*(1.0_fp-dif_aks/thick(k))*thick(k)*h1
             else
                rpower   = log(concin(k-1)/concin(k)) / log(  (h1*(1.0_fp+sig(k  ))*(h1-h1*(1.0_fp+sig(k-1)))) &
                &                                           / (h1*(1.0_fp+sig(k-1))*(h1-h1*(1.0_fp+sig(k  )))) )
                z        = ((1.0_fp+sig(k)+0.5_fp*thick(k))*h1 + aks) / 2.0_fp
                ceavgtmp = concin(k) * ((h1*(1.0_fp+sig(k))*(h1-z))/(z*(h1-h1*(1.0_fp+sig(k)))))**rpower
                ceavg    = ceavg + ceavgtmp*(1.0_fp-dif_aks/thick(k))*thick(k)*h1
             endif
          elseif (dif_aks<=0.0_fp .and. dif_upp>=0.0_fp) then
             !
             ! layer between aks and 3*deltas
             !
             ceavg = ceavg + concin(k)*thick(k)*h1
          elseif (dif_upp <= thick(k)) then
             !
             ! k-layer contains 3*deltas (take part below)
             !
             ceavg = ceavg + concin(k)*dif_upp*h1
             exit
          endif
       enddo
       !
       ! STEP 2
       ! Multiply integrated transports with orbital & drift velocity component
       !
       veff = sqrt(u2dh*u2dh+uon*uon) - vcr
       !    
       if (veff > eps) then
          !
          ! Added phase function following Walstra et al (2007)
          !
          if (fpco == 0.0_fp) then
             p2 = -tanh(100.0_fp*(rksrs/(ws*tp)-0.1_fp))
          else
             p2 = fpco
          endif
          !
          ! Gamma has been reduced from 0.2 in TR2000 to 0.1 in TR2004
          !
          if (abs(uon+uoff) < 1e-6_fp) then
             ua = 0.0_fp
          else
             ua = 0.1_fp * p2 * (uon**4-uoff**4) / (uon**3+uoff**3)
          endif
          uau   = ua * cosphiwav
          uav   = ua * sinphiwav
          !
          qswu = uau * ceavg
          qswv = uav * ceavg
       else
          qswu = 0.0_fp
          qswv = 0.0_fp
       endif
    else
       !
       ! SUSW == 0
       !
       qswu = 0.0_fp
       qswv = 0.0_fp
    endif
end subroutine bedtr2004
