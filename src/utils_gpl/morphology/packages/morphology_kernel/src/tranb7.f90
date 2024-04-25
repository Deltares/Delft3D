subroutine tranb7(utot      ,d50       ,d90       ,h         ,npar      , &
                & par       ,sbot      ,ssus      ,vonkar    ,mudfrac   )
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
!
! computes sediment transport according to
! van rijn (1984)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sed_support_routines, only: shld
    implicit none
!
! Arguments
!
    integer                  , intent(in)    :: npar
    real(fp)                 , intent(in)    :: d50     ! grain size diameter (first specified diameter)
    real(fp)                 , intent(in)    :: d90     ! grain size diameter (first specified diameter)
    real(fp)                 , intent(in)    :: h       ! water depth
    real(fp)                 , intent(in)    :: mudfrac ! fraction of mud
    real(fp), dimension(npar), intent(in)    :: par     ! sediment parameter list
    real(fp)                 , intent(in)    :: utot    ! flow velocity
    real(fp)                 , intent(in)    :: vonkar
    !
    real(fp)                 , intent(out)   :: sbot    ! bed load transport
    real(fp)                 , intent(out)   :: ssus    ! suspended sediment transport
!
! Local variables
!
    real(fp)       :: a
    real(fp)       :: ah
    real(fp)       :: alf1
    real(fp)       :: beta   ! lowest level of integration interval over vertical
    real(fp)       :: betam  !  power coefficient for adjusting critical bed shear stress for sand-mud interaction
    real(fp)       :: ca
    real(fp)       :: del
    real(fp)       :: dstar
    real(fp)       :: fc
    real(fp)       :: ff     ! coriolis coefficient
    real(fp)       :: ag     ! gravity acceleration
    real(fp)       :: psi
    real(fp)       :: rhosol ! density of sediment
    real(fp)       :: rhowat ! density of water
    real(fp)       :: rksc
    real(fp)       :: rmuc
    real(fp)       :: rnu    ! laminar viscosity of water
    real(fp)       :: t      ! dimensionless relative shear stress
    real(fp)       :: tbc
    real(fp)       :: tbce
    real(fp)       :: tbcr
    real(fp)       :: thetcr
    real(fp)       :: ustar
    real(fp)       :: ws     ! settling velocity
    real(fp)       :: zc
!
!! executable statements -------------------------------------------------------
!
    sbot = 0.0
    ssus = 0.0
    !
    ag = par(1)
    rhowat = par(2)
    rhosol = par(3)
    del  = par(4)
    rnu = par(5)
    alf1 = par(11)
    rksc = par(13)
    ws = par(14)
    betam = par(15)
    !
    if (h/rksc<1.33 .or. utot<1.E-3) then
       return
    endif
    !
    a = rksc
    dstar = d50*(del*ag/rnu/rnu)**(1./3.)
    !
    rmuc = (log10(12.*h/rksc)/log10(12.*h/3./d90))**2
    fc = .24*(log10(12.*h/rksc))**( - 2)
    tbc = .125*rhowat*fc*utot**2
    tbce = rmuc*tbc
    thetcr = shld(dstar)
    tbcr = (rhosol - rhowat)*ag*d50*thetcr*(1.0+mudfrac)**betam
    t = (tbce - tbcr)/tbcr
    !
    if (t<.000001) t = .000001
    ca = .015*alf1*d50/a*t**1.5/dstar**.3
    !
    ustar = sqrt(.125*fc)*utot
    zc = 0.
    beta = 1. + 2.*(ws/ustar)**2
    beta = min(beta, 1.5_fp)
    psi = 2.5*(ws/ustar)**0.8*(ca/0.65)**0.4
    if (ustar>0.) zc = ws/vonkar/ustar/beta + psi
    if (zc>20.) zc = 20.
    ah = a/h
    fc = 0.
    if (abs(zc - 1.2)>1.E-4) then
       fc = (ah**zc - ah**1.2)/(1. - ah)**zc/(1.2 - zc)
    else
       fc = -(ah/(1. - ah))**1.2*log(ah)
    endif
    ff = fc
    ssus = ff*utot*h*ca
    !
    if (t<3.) then
       sbot = 0.053*(del)**0.5*sqrt(ag)*d50**1.5*dstar**( - 0.3)*t**2.1
    else
       sbot = 0.100*(del)**0.5*sqrt(ag)*d50**1.5*dstar**( - 0.3)*t**1.5
    endif
end subroutine tranb7
