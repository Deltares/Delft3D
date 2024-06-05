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

! 
! 

 subroutine setcdwcoefficient(uwi, cd10, L)
 use m_wind
 use m_flow     , only : ag, hs, jaCdwusp, Cdwusp
 use m_flowgeom , only : ln
 use m_sferic   , only: pi, twopi
 use m_waves    , only: twav
 use m_physcoef
 use m_missing

 implicit none
 integer, intent (in) :: L
 integer              :: k1, maxnit = 100, nit, jalightwind = 0
 double precision     :: uwi, cd10, rk, hsurf = 10d0, ust, ust2, ust0, z0w 
 double precision     :: omw, cdL2, dkpz0, s, sold, eps = 1d-4, awin
 double precision     :: p = -12d0, pinv = -0.083333d0, A, A10log, bvis, bfit, balf, r

 if (icdtyp == 1) then                       ! Constant

    cd10 = cdb(1)

    if (jaCdwusp == 1) then
       if  (Cdwusp(L) .ne. dmiss) then
          cd10 = Cdwusp(L)
       endif
    endif

 else if (icdtyp == 2 ) then                 ! Smith and Banks 2 breakpoints

    if      (uwi <= wdb(1) ) then
       cd10 = cdb(1)
    else if (uwi <= wdb(2) ) then
       cd10 = cdb(1) + (uwi - wdb(1)) * (cdb(2) - cdb(1) ) / (wdb(2) - wdb(1) )
    else
       cd10 = cdb(2)
    endif


 else if (icdtyp == 3 ) then                 ! Smith and Banks like 3 breakpoints

    if      (uwi <= wdb(1) ) then
       cd10 = cdb(1)
    else if (uwi <= wdb(2) ) then
       cd10 = cdb(1) + (uwi - wdb(1)) * (cdb(2) - cdb(1) ) / (wdb(2) - wdb(1) )
    else if (uwi <= wdb(3) ) then
       cd10 = cdb(2) + (uwi - wdb(2)) * (cdb(3) - cdb(2) ) / (wdb(3) - wdb(2))
    else
       cd10 = cdb(3)
    endif

 else if (icdtyp == 4) then                  ! Charnock 1955

  ! Charnock drag coefficient formulation, logarithmic wind velocity profile in the turbulent layer
  ! above the free surface:

  ! uwi      1        z            z=10 m, for U10
  ! ---- = ----- ln (---)
  ! u*      kappa    z0

  ! where u* is the friction velocity, kappa is the Von Karman constant
  ! z is the vertical height above the free surface and z0 is the roughness height:

  ! z0 = b * u*^2/g

  ! with b the dimensionless Charnock coefficient and g the gravity acceleration.
  ! Cd = u*^2/uwi^2, so we have an implicit relation between drag coefficient Cd and wind speed Ws.
  ! Newton-Raphson :

    nit  = 0
    s    = 19.6d0
    sold = 0d0

    do while ( abs(sold-s).gt.(eps*s) )

         nit  = nit + 1
         sold = s
         s    = sold*(log(hsurf*ag*sold*sold / (max(0.001,cdb(1)*uwi*uwi)))-2d0)/(vonkarw*sold-2d0)

         if ( nit.ge.maxnit ) then
            cd10 = 1d-3
            exit
         endif

    enddo

    if (s > 0d0) then
       cd10 = 1d0/(s*s)
    endif

    !z0w  = cdb(2)*viskinair / ust + cdb(1)*ust2 / ag 
    !ust2 = cd10*uwi*uwi
    !z0w  = cdb(1)*ust2/ag 
    
 else if (icdtyp == 5) then                     ! Hwang 2005, wave frequency dependent

    ! (A.)=http://onlinelibrary.wiley.com/doi/10.1029/2005JC002912/full

    k1    = ln(1,L)
    if (uwi < 4d0) then
       awin  = max(0.1d0,uwi)
       cd10  = 0.0044d0 / awin**1.15d0
    else if (twav(k1) < 0.1d0) then
       cd10 = 0.00063d0 + 0.000066d0*uwi
    else
       omw   = twopi/max(0.1d0, twav(k1) )       ! wave frequency
       cdL2  = 0.001289d0*(omw*uwi/ag)**0.815d0 ! Cd at half wavelength above surface(11a)    (A7)

       dkpz0 = pi*exp(-vonkarw/sqrt(CdL2))      ! (5)                                         (A2)
       call getwavenr(hs(k1),twav(k1),rk)
       cd10  = (vonkarw / log(10d0*rk/dkpz0) )**2                                           ! (A4b)
     endif

 else if (icdtyp == 6) then                     ! Wuest 2003 & Smith en Banke, uit Rob's DPM

    if (uwi > 4d0) then
        cd10 = 0.00063d0 + 0.000066d0*uwi
    else
        awin = max(0.1d0,uwi)
        cd10 = 0.0044d0 / awin**1.15d0
    endif

 else if (icdtyp == 7) then                     ! Hans Hersbach, July 2010, ECMWF fit (CHarnock plus viscous term)
                                                ! https://journals.ametsoc.org/doi/full/10.1175/2010JPO4567.1
    A      = ( cdb(1)*(vonkarw*uwi)**2 ) / (ag*hsurf)
    A10log = log(A)                             ! (2) shows that log actually means: ln
    balf   = 2.65d0 - 1.44d0*A10log - 0.015d0*A10log*A10log
    R      = (hsurf*vonkarw*uwi)/(cdb(2)*viskinair)
    bvis   = -1.47d0 + 0.93d0*log(R)
    bfit   = (bvis**p + balf**p)**pinv
    cd10   = (vonkarw/bfit)**2

 else if (icdtyp == 8) then                     ! Charnock 1955 + viscous term
    
    ust     = uwi/25d0
    do nit  = 1,10                              ! good for about 8 decimals of cd10
       z0w  = cdb(1)*ust*ust / ag  + cdb(2)*viskinair / ust
       ust  = uwi*vonkarw/log(hsurf/z0w) 
    enddo
    cd10 = (ust/uwi)**2

 else if (icdtyp == 9) then   ! Garratt, J. R., 1977: Review of Drag Coefficients over Oceans and Continents. Mon. Wea. Rev., 105, 915-929.
     
     cd10 = min(1.0d-3 * (0.75d0 + 0.067d0 * uwi), 0.0035d0)
     
 endif

 if (jalightwind == 1 .and. icdtyp .ne. 8 .and. icdtyp .ne. 7 .and. icdtyp .ne. 6 .and. icdtyp .ne. 5) then
    if (uwi < 4d0) then     ! for wind < 4 m/s use wuest anyway
        awin = max(0.1d0,uwi)
        cd10 = max(cd10, 0.0044d0 / awin**1.15d0)
    endif
 endif
 end subroutine setcdwcoefficient
