module sed_support_routines
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
!-------------------------------------------------------------------------------

implicit none

private

public shld
public ruessink_etal_2012
public calculate_critical_velocities
public calculate_velocity_asymmetry
public calculate_urms

contains

!> determines Shields parameter according to Shields curve
function shld(dstar)
    use precision
    implicit none
!
! arguments
!
    real(fp), intent(in) :: dstar !< critical dimensionless grain size parameter
    real(fp)             :: shld  !< corresponding Shields parameter
!
!! executable statements -------------------------------------------------------
!
    if (dstar <= 4.0_fp) then
       shld = 0.240_fp / dstar
    elseif (dstar <= 10.0_fp) then
       shld = 0.140_fp / dstar**0.64_fp
    elseif (dstar <= 20.0_fp) then
       shld = 0.040_fp / dstar**0.10_fp
    elseif (dstar <= 150.0_fp) then
       shld = 0.013_fp * dstar**0.29_fp
    else
       shld = 0.055_fp
    endif
end function shld

subroutine ruessink_etal_2012(k, hs, h, sk, as, phi_phase, urs, bm)
    use precision
    use mathconsts, only: pi
    implicit none
!
! arguments
!
    real(fp)  , intent(in)  :: k         !< wave number                   [rad/m]
    real(fp)  , intent(in)  :: hs        !< significant wave height       [m]
    real(fp)  , intent(in)  :: h         !< water depth                   [m]

    real(fp)  , intent(out) :: sk        !< skewness                      [-]
    real(fp)  , intent(out) :: as        !< asymmetry                     [-]
    real(fp)  , intent(out) :: phi_phase !< acceration skewness           [rad]
    real(fp)  , intent(out) :: urs       !< Ursell number                 [-]
    real(fp)  , intent(out) :: bm        !< ...
!
! local variables
!
    real(fp)               :: aw
    real(fp)               :: p1
    real(fp)               :: p2
    real(fp)               :: p3
    real(fp)               :: p4
    real(fp)               :: p5
    real(fp)               :: p6
    real(fp)               :: psi
!
!! executable statements -------------------------------------------------------
!
!   parameters
    p1 = 0.0_fp    ! a =  0
    p2 = 0.857_fp  ! b =  0.857 +/- 0.016
    p3 =-0.471_fp  ! c = -0.471 +/- 0.025
    p4 = 0.297_fp  ! d =  0.297 +/- 0.021
    p5 = 0.815_fp  ! e =  0.815 +/- 0.055
    p6 = 0.672_fp  ! f =  0.672 +/- 0.073

!   asymmetry Ruessink et al. (here based on hs and h)
    aw = 0.5_fp*hs
!   asymmetry & skewness based on Ruessink & van Rijn (based on XBeach code) Ursell number (eq. 6)
    if (comparereal(k,0.0_fp) == 0) then
        urs = 0.0_fp
        bm  = 0.0_fp
        psi = 0.0_fp
    else
        urs = 0.75_fp * aw * k / ((k*h)**3)
        urs = max(urs,1e-12_fp)                                             ! Ursell number > 20 -> Use shape with care
!       Boltzmann sigmoid (eq 9)
        bm  = p1 + (p2-p1)/(1.0_fp+exp((p3-log10(urs))/p4))
        psi = 0.5_fp * pi * (tanh(p5/(urs**p6)) - 1.0_fp)
    endif
!   skewness (beteen eq. 8 and 9)
    sk = bm * cos(psi)
!   asymmetry (beteen eq. 8 and 9)
    as = bm * sin(psi)
!   Ruessink et al. (2012), eq. (12)
    phi_phase = -psi - (pi/2.0_fp)
end subroutine ruessink_etal_2012


subroutine calculate_critical_velocities(dilatancy, bedslpeffini, dzbdt, ag, vicmol, d15, poros, pormax, rheea, delta, u, v, &
    dzdx, dzdy, dtol, phi, ucr, ucrb, Ucrs)
    use precision
    use mathconsts
    implicit none
    
    integer                  , intent(in)    :: dilatancy
    integer                  , intent(in)    :: bedslpeffini
    real(fp)                 , intent(in)    :: dzbdt    !<  Erosion/sedimentation velocity
    real(fp)                 , intent(in)    :: ag
    real(fp)                 , intent(in)    :: vicmol
    real(fp)                 , intent(in)    :: d15
    real(fp)                 , intent(in)    :: poros
    real(fp)                 , intent(in)    :: pormax
    real(fp)                 , intent(in)    :: rheea
    real(fp)                 , intent(in)    :: delta
    real(fp)                 , intent(in)    :: u
    real(fp)                 , intent(in)    :: v
    real(fp)                 , intent(in)    :: dzdx   
    real(fp)                 , intent(in)    :: dzdy
    real(fp)                 , intent(in)    :: dtol
    real(fp)                 , intent(in)    :: phi   
    real(fp)                 , intent(in)    :: ucr
    real(fp)                 , intent(out)   :: ucrb
    real(fp)                 , intent(out)   :: Ucrs

    
    real(fp)                       :: srftotal
    real(fp)                       :: srfrhee
    real(fp)                       :: vero      !< Erosion velocity
    real(fp)                       :: kl
    real(fp)                       :: alpha1, alpha2, beta, psi
    
    srfRhee  = 0.0_fp
    srfTotal = 1.0_fp
    if (dilatancy == 1) then
       vero = max(0.0_fp,-dzbdt)
       ! Permeability, Adel 1987
       kl = ag/(160.0_fp*vicmol)*(d15**2)*((poros**3)/(1.0_fp-poros)**2)
       ! Reduction factor on the critical Shields parameter by dilatancy (Van Rhee, 2010)
       srfRhee = vero/kl*(pormax-poros)/(1.0_fp-pormax)*rheea/delta
    endif
    !
    if (bedslpeffini == 0) then
         srfTotal = 1.0_fp + srfRhee
    elseif (bedslpeffini == 1 .or. bedslpeffini == 2) then
       if  ((abs(u) > dtol .or. abs(v) > dtol) .and. (abs(dzdx) > dtol .or. abs(dzdy) > dtol)) then
          ! 
          alpha1 = atan2(v,u)
          ! Angle between the x-axis and the bed slope vector directed in down-slope direction
          alpha2 = mod(atan2(-dzdy,-dzdx),2.0_fp*pi)
          psi = alpha1-(alpha2-pi) 
          if (abs(dzdx) < dtol) then 
              !  Beta purely based on dzdy
              beta = atan(abs(dzdy))
          else
              ! Maximum absolute bed slope angle, derived in de Vet 2014
              beta = atan(abs(dzdx/sin(atan(dzdx/max(dzdy,DTOL)))))
          endif
          beta = min(beta,phi)
          if (dilatancy == 1) then
             ! Soulsby (1997), modified by de Vet 2014
             srfTotal = (cos(psi)*sin(beta)+sqrt( &
                        (srfRhee**2+2.0_fp*srfRhee*cos(beta)+cos(beta)**2) * &
                         tan(phi)**2-sin(psi)**2*sin(beta)**2)) / tan(phi)
          else
             ! Soulsby (1997)
             srfTotal = (cos(psi)*sin(beta) + &
                         sqrt(cos(beta)**2*tan(phi)**2-sin(psi)**2*sin(beta)**2))/tan(phi)
          endif
       endif
    endif
   ! Calculate the new critical velocity based on the modification factors on the Shields parameter
   Ucrb = Ucr*sqrt(srfTotal)
   if (bedslpeffini == 1) then
      ! bed+sus
      Ucrs = Ucrb
   else
      ! bed only
      Ucrs = Ucr*(1.0_fp+sqrt(srfRhee))
   endif
end subroutine calculate_critical_velocities


subroutine calculate_velocity_asymmetry(waveform, facas, facsk, sws, h, hrms, rlabda, ag, tp, urms, uamag)
    use precision
    use mathconsts
    implicit none
    
    integer                     , intent(in)     :: waveform
    integer                     , intent(in)     :: sws
    real(fp)                    , intent(in)     :: facas
    real(fp)                    , intent(in)     :: facsk
    real(fp)                    , intent(in)     :: h
    real(fp)                    , intent(in)     :: hrms
    real(fp)                    , intent(in)     :: rlabda
    real(fp)                    , intent(in)     :: ag
    real(fp)                    , intent(in)     :: tp
    real(fp)                    , intent(in)     :: urms
    real(fp)                    , intent(out)    :: uamag
        
    uamag = 0.0_fp
    if (waveform==1) then
       call ua_rvr(facas    ,facsk  ,sws   ,h   ,hrms   , &
                 & rlabda   ,urms   ,uamag )
    else if (waveform==2) then
       call ua_vt(facas    ,facsk   ,sws   ,h      ,   &
                & hrms     ,tp      ,ag    ,urms   ,   &
                & uamag    )
    end if

end subroutine calculate_velocity_asymmetry

subroutine calculate_urms(hrms, tp, h, ag, ubot_from_com, ubot, kwtur, urms, urms2)
    use precision
    use mathconsts

    implicit none

    logical , intent(in)           :: ubot_from_com
    real(fp), intent(in)           :: hrms
    real(fp), intent(in)           :: tp
    real(fp), intent(in)           :: h
    real(fp), intent(in)           :: ag
    real(fp), intent(in)           :: ubot
    real(fp), intent(in)           :: kwtur
    real(fp), intent(out)          :: urms
    real(fp), intent(out)          :: urms2

    real(fp)                       :: tp2
    real(fp)                       :: k
    real(fp)                       :: uorb

    if ( tp > 1.e-6_fp ) then
       !
       !     Prevent small tp
       !
       tp2 = max(tp, 1.0_fp)
       !
       call wavenr(h         ,tp2        ,k         ,ag        )
       if (ubot_from_com) then
          uorb = ubot
       else
          uorb = pi*hrms/tp2/sinh(k*h)
       endif
       urms = uorb*0.7071_fp
       urms2 = urms**2 + 1.45_fp*kwtur
    else
       urms  = 0.0_fp
       urms2 = 0.0_fp
    endif

end subroutine calculate_urms

end module sed_support_routines