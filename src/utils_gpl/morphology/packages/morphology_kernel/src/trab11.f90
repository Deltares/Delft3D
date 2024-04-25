subroutine trab11(u         ,v         ,hrms      ,h         ,tp        , &
                & d50       ,npar      ,par       ,sbotx     ,sboty     , &
                & ssusx     ,ssusy     ,ubot      ,vonkar    ,ubot_from_com )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
! computes sediment transport according to
! the transport formula of Soulsby / Van Rijn
! -
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
    real(fp)                                 :: h
    real(fp)                                 :: hrms   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(npar), intent(in)    :: par
    real(fp)                                 :: tp     !  Description and declaration in esm_alloc_real.f90
    real(fp)                 , intent(in)    :: ubot   !  Description and declaration in esm_alloc_real.f90
    real(fp)                 , intent(in)    :: u
    real(fp)                 , intent(in)    :: v
    real(fp)                 , intent(in)    :: vonkar
    !
    real(fp)                 , intent(out)   :: sbotx
    real(fp)                 , intent(out)   :: sboty
    real(fp)                 , intent(out)   :: ssusx
    real(fp)                 , intent(out)   :: ssusy
!
! Local variables
!
    real(fp)                       :: acal
    real(fp)                       :: asb
    real(fp)                       :: ass
    real(fp)                       :: cd
    real(fp)                       :: d90
    real(fp)                       :: delta
    real(fp)                       :: dstar
    real(fp)                       :: ag         !  gravity acceleration
    real(fp)                       :: k          ! wave number
    real(fp)                       :: rnu
    real(fp)                       :: term1
    real(fp)                       :: term2
    real(fp)                       :: ucr
    real(fp)                       :: uorb       !  orbital velocity at the bottom layer
    real(fp)                       :: urms
    real(fp)                       :: utot       ! flow velocity
    real(fp)                       :: z0
    real(fp)                       :: alfaurms   ! Calibration factor wave influence on transport
!
!
!! executable statements -------------------------------------------------------
!
    !
    !     Initiliaze Transports to zero
    !
    sbotx = 0.0
    sboty = 0.0
    ssusx = 0.0
    ssusy = 0.0
    !
    !     Initialisations
    !
    ag = par(1)
    delta = par(4)
    rnu = par(5)
    acal = par(11)
    d90 = par(12)*d50
    z0 = par(13)
    alfaurms = par(14)
    !
    !     Velocity magnitude
    !
    utot = u**2 + v**2
    if (utot>0.) utot = sqrt(utot)
    if (utot<0.000001 .or. h>200. .or. h<0.01) goto 999
    !
    !     Wave number k, urms orbital velocity
    !
    if (tp>1.E-6) then
       !
       !     Prevent small tp
       !
       tp = max(tp,1.0_fp)
       !
       call wavenr(h         ,tp        ,k         ,ag        )
       if (ubot_from_com) then
          uorb = ubot
       else
          uorb = pi*hrms/tp/sinh(k*h)
       endif
       urms = uorb*0.7071
    else
       urms = 0.0_fp
    endif
    !
    !     Soulsby p. 184, (ag(s-1)/nu^2)^(1/3) = 25926
    !                    for ag=9.81, s=2.65, nu=1e-6
    !
    dstar = (ag*delta/rnu**2)**(1./3.)*d50
    !
    !     Soulsby p. 176, eq. 133d,e
    !     For d50 > 2 mm in SEDINP an warning is generated
    !     and we will define Ucr as if 0.0005 < d50 < 0.002
    !
    if (d50<=0.0005) then
       ucr = 0.19*d50**0.1*log10(4.*h/d90)
    elseif (d50<0.002) then
       ucr = 8.5*d50**0.6*log10(4.*h/d90)
    else
       ucr = 8.5*d50**0.6*log10(4.*h/d90)
    endif
    !
    !     Soulsby p. 183
    !
    cd = (vonkar/(log(h/z0) - 1.))**2
    asb = 0.005*h*(d50/h/(delta*ag*d50))**1.2
    ass = 0.012*d50*dstar**( - 0.6)/(delta*ag*d50)**1.2
    term1 = (utot*utot + alfaurms*0.018/cd*urms*urms)**0.5
    if (term1>ucr) then
       term2 = (term1 - ucr)**2.4
    else
       goto 999
    endif
    !
    !     Soulsby p. 183, eq. 136a; bed slope effects are left out,
    !     since they are taken into account elsewhere
    !
    sbotx = acal*asb*u*term2
    sboty = acal*asb*v*term2
    ssusx = acal*ass*u*term2
    ssusy = acal*ass*v*term2
    !
  999 continue
end subroutine trab11
