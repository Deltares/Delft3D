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
!> computes sediment transport according to the bed load transport formula of Soulsby;
!! first implementation assumes transport in current direction only.
subroutine trab12(u         ,v         ,hrms      ,h         ,tp        , &
                & dir       ,d50       ,npar      ,par       ,sbotx     , &
                & sboty     ,ssusx     ,ssusy     ,ubot      ,vonkar    , &
                & ubot_from_com )
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    implicit none
!
! Arguments
!
    logical                  , intent(in)    :: ubot_from_com
    integer                  , intent(in)    :: npar
    real(fp)                 , intent(in)    :: d50
    real(fp)                 , intent(in)    :: dir
    real(fp)                                 :: h
    real(fp)                                 :: hrms  !< Description and declaration in esm_alloc_real.f90
    real(fp), dimension(npar), intent(in)    :: par
    real(fp)                                 :: tp    !< Description and declaration in esm_alloc_real.f90
    real(fp)                 , intent(in)    :: u
    real(fp)                                 :: ubot  !< Description and declaration in esm_alloc_real.f90
    real(fp)                 , intent(in)    :: v
    real(fp)                 , intent(in)    :: vonkar
    !
    real(fp)                 , intent(out)   :: sbotx
    real(fp)                 , intent(out)   :: sboty
    real(fp)                 , intent(out)   :: ssusx
    real(fp)                 , intent(out)   :: ssusy
!
!
! Local variables
!
    real(fp), parameter            :: ASTARC  = 30.*pi**2
    real(fp), parameter            :: EPS = 1.E-6
    real(fp), parameter            :: WAVEPS = 1.E-4
    
    integer                        :: modind
    real(fp)                       :: abscos
    real(fp)                       :: acal
    real(fp)                       :: astar
    real(fp)                       :: cdrag
    real(fp)                       :: cj
    real(fp)                       :: coeffb
    real(fp)                       :: coeffp
    real(fp)                       :: coeffq
    real(fp)                       :: delta
    real(fp)                       :: dstar
    real(fp)                       :: facth
    real(fp)                       :: factr
    real(fp)                       :: fw
    real(fp)                       :: ag                   !<  gravity acceleration
    real(fp)                       :: k                    !< wave number
    real(fp)                       :: lfc
    real(fp)                       :: phi
    real(fp)                       :: phicur
    real(fp)                       :: phiwav
    real(fp)                       :: phix
    real(fp)                       :: phix1
    real(fp)                       :: phix2
    real(fp)                       :: phiy
    real(fp)                       :: rho                  !<  array with densities [kg/m3]???
    real(fp)                       :: rnu
    real(fp)                       :: taucur
    real(fp)                       :: taum
    real(fp)                       :: tauwav
    real(fp)                       :: thetam
    real(fp)                       :: thetaw
    real(fp)                       :: thetcr
    real(fp)                       :: thetmx
    real(fp)                       :: uorb                 !< orbital velocity at the bottom layer
    real(fp)                       :: utot                 !< flow velocity
    real(fp)                       :: xpar
    real(fp)                       :: ypar
    real(fp)                       :: z0
!
    real(fp), dimension(8, 4) :: bb, pp, qq
    real(fp), dimension(8) :: coeffj
    !
    data bb/0.29, 0.65, 0.27, 0.73, 0.22, 0.32, 0.47, -0.06, 0.55, 0.29, 0.51,  &
       & 0.40, 0.73, 0.55, 0.29, 0.26, -0.10, -0.30, -0.10, -0.23, -0.05, 0.00, &
       & -0.09, 0.08, -0.14, -0.21, -0.24, -0.24, -0.35, 0.00, -0.12, -0.03/
    !
    data pp/ - 0.77, -0.60, -0.75, -0.68, -0.86, -0.63, -0.70, -1.00, 0.10,     &
       & 0.10, 0.13, 0.13, 0.26, 0.05, 0.13, 0.31, 0.27, 0.27, 0.12, 0.24, 0.34,&
       & 0.00, 0.28, 0.25, 0.14, -0.06, 0.02, -0.07, -0.07, 0.00, -0.04, -0.26/
    !
    data qq/0.91, 1.19, 0.89, 1.04, -0.89, 1.14, 1.65, 0.38, 0.25, -0.68, 0.40, &
       & -0.56, 2.33, 0.18, -1.19, 1.19, 0.50, 0.22, 0.50, 0.34, 2.60, 0.00,    &
       & -0.42, 0.25, 0.45, -0.21, -0.28, -0.27, -2.50, 0.00, 0.49, -0.66/
    !
    data coeffj/3.00, 0.50, 2.70, 0.50, 2.70, 3.00, 0.60, 1.50/
!
!
!! executable statements -------------------------------------------------------
!
    !
    !     Initialize Transports to zero
    !     in case of small u, small h, very large h, u<ucr
    !
    sbotx = 0.0_fp
    sboty = 0.0_fp
    ssusx = 0.0_fp
    ssusy = 0.0_fp
    utot = sqrt(u**2 + v**2)
    if ( utot < 0.000001_fp .or. h > 200.0_fp .or. h < 0.01_fp ) return
    
    !
    !     Initialisations
    !
    ag     = par(1)
    rho    = par(2)
    delta  = par(4)
    rnu    = par(5)
    acal   = par(11)
    modind = nint(par(12))
    z0     = d50/par(13)
    facth  = 1.0_fp / (rho*ag*delta*d50)
    !
    !     Wave number k
    !
    if ( tp > 1.E-6_fp ) then
       !
       !     Prevent small tp
       !
       tp = max(tp, 1.0_fp)
       !
       call wavenr(h         ,tp        ,k         ,ag        )
       if (ubot_from_com) then
          uorb = ubot
       else
          uorb = pi*hrms/tp/sinh(k*h)
       endif
       ! urms = uorb*0.7071
    else
       uorb = 0.0_fp
    endif
    !
    !
    !-----------wave (skin) friction factor (Swart) and drag coefficient
    !
    astar = tp*uorb/z0
    !     Swart
    if ( astar > ASTARC ) then
       fw = 0.00251_fp * exp(14.1_fp / (astar**0.19_fp))
    else
       fw = 0.3_fp
    endif
    !     Soulsby
    !            fw=1.39*(astar/2./pi)**(-0.52)
    !
    !-----------magnitude of bottom skin friction due to waves alone
    !           and due to current alone
    !
    tauwav = 0.5_fp*rho*fw*uorb**2
    cdrag  = (vonkar/(1. + log(z0/h)))**2
    taucur = rho*cdrag*utot**2
    !
    !           Angle between current and waves
    !
    phiwav = dir*degrad
    phicur = atan2(v, u)
    phi    = phiwav - phicur
    abscos = abs(cos(phi))
    !
    !-----------parameterized models
    !
    if ( tauwav < 1.0E-8_fp ) then
       xpar = 1.0_fp
       ypar = 1.0_fp
    else
       xpar = taucur/(taucur + tauwav)
       if ( xpar < 1.0E-8_fp ) then
          ypar = 0.0_fp
       else
          lfc = log10(fw/cdrag)
          if ( abscos > EPS ) then
             cj = abscos**coeffj(modind)
          else
             cj = 0.0_fp
          endif
          coeffb = (bb(modind, 1) + bb(modind, 2)*cj)                           &
                 & + (bb(modind, 3) + bb(modind, 4)*cj)*lfc
          coeffp = (pp(modind, 1) + pp(modind, 2)*cj)                           &
                 & + (pp(modind, 3) + pp(modind, 4)*cj)*lfc
          coeffq = (qq(modind, 1) + qq(modind, 2)*cj)                           &
                 & + (qq(modind, 3) + qq(modind, 4)*cj)*lfc
          ypar = xpar*(1.0_fp + coeffb*(xpar**coeffp)*((1.0_fp - xpar)**coeffq))
       endif
    endif
    !
    !-----------bottom friction for combined waves and current
    !
    taum   = ypar*(taucur + tauwav)
    thetam = taum*facth
    thetaw = tauwav*facth
    !
    !           Soulsby p. 104, (ag(s-1)/nu^2)^(1/3) = 25926
    !                    for ag=9.81, s=2.65, nu=1e-6
    !
    !     dstar  = 25296.*d50
    dstar = (ag*delta/rnu**2)**(1.0_fp/3.0_fp)*d50
    !
    !           Critical shear stress
    !           Soulsby p. 106, eq. 77
    !
    thetcr = 0.30_fp/(1.0_fp + 1.2_fp*dstar) + 0.055_fp*(1.0_fp - exp( - 0.020_fp*dstar))
    !
    !          Tau max needed to check the critical tau
    !          Soulsby  p. 168
    !
    thetmx = sqrt((thetam + thetaw*cos(phi))**2 + (thetaw*sin(phi))**2)
    if ( thetmx <= thetcr ) then
    ! critical tau exceeded
        return
    end if
    !
    !           Soulsby 167 eq. 129 a-d
    !           Dimensionless transport in current direction
    !
    phix1 = 12.0_fp*sqrt(thetam + WAVEPS)*(thetam - thetcr)
    phix2 = 12.0_fp*(0.95_fp + 0.19_fp*cos(2.0_fp*phi))*sqrt(thetaw + WAVEPS)*thetam
    phix = max(phix1, phix2)
    !
    !
    !           Dimensionless transport perpendicular to current direction
    !
    phiy = 12.0_fp*(0.19_fp*thetam*thetaw**2*sin(2.0_fp*phi))                              &
         & /((thetaw + WAVEPS)**1.5_fp + 1.5_fp*(thetam + WAVEPS)**1.5_fp)
    !
    !
    !           Dimensionless transport perpendicular to current direction
    !
    factr = sqrt(ag*delta*d50**3)
    sbotx = acal*factr/utot*(phix*u - phiy*v)
    sboty = acal*factr/utot*(phix*v + phiy*u)

end subroutine trab12
