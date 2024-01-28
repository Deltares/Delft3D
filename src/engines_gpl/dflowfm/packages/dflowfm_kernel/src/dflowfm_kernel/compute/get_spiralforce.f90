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

subroutine get_spiralforce    ! Effect of secondary flow on momentum equations
                              ! This subroutine calculates the forces fx and fy for momentum equations
   use m_flow
   use m_flowgeom

   implicit none
   integer :: k, k1, k2, LL, L, n
   double precision :: cofa, cofb, cofc, cofd, cofe, coff, cofg, cofw, cofx, cofy, coftxx, coftxy, coftyy, cof0
   double precision :: dtxxdx, dtxxdy, dtxydx, dtxydy
   double precision :: betas, beta, alfa
   double precision :: fx, fy, fxl

   ht_xx = 0d0 ; ht_xy = 0d0

   do k = 1,ndxi
      ht_xx(k) = 0d0
      ht_xy(k) = 0d0
      if( spirucm(k) < 1.0d-3 ) cycle
      if( hs(k) < epshu ) cycle
      alfa  = sag / vonkar / czssf(k)
      betas = spirbeta * ( 5.0d0 * alfa - 15.6d0 * alfa**2 + 37.5d0 * alfa**3 )
      beta = betas * spirint(k) / spirucm(k)
      ht_xx(k) = -2.0d0 * hs(k) * beta * ucx(k) * ucy(k)
      ht_xy(k) = hs(k) * beta * ( ucx(k) * ucx(k) - ucy(k) * ucy(k) )
   enddo

   do L = lnxi+1,lnx                        ! Boundary conditions for spiral flow forces
      k1 = ln(1,L) ; k2 = ln(2,L)
      ht_xy(k1) = 0d0
      ht_xy(k1) = 0d0
      if( hs(k2) < epshu ) cycle
      ht_xx(k1) = ht_xx(k2)
      ht_xy(k1) = ht_xy(k2)
   enddo

   do k = 1,ndxi
      k1 = k
      spirfx(k1) = 0d0
      spirfy(k1) = 0d0
      if( hs(k1) < epshu ) cycle
      cofa = 0.0d0
      cofb = 0.0d0
      cofc = 0.0d0
      cofd = 0.0d0
      cofe = 0.0d0
      coff = 0.0d0
      cofg = 0.0d0
      n = 0
      do LL = 1,nd(k1)%lnx
         L = abs( nd(k1)%ln(LL) )
         k2 = ln(1,L) + ln(2,L) - k1
         !if( hs(k2) < epshu ) cycle
         n = n + 1
         cofx = xz(k2) - xz(k1)
         cofy = yz(k2) - yz(k1)
         coftxx = ht_xx(k2) - ht_xx(k1)
         coftxy = ht_xy(k2) - ht_xy(k1)
         cof0 = sqrt( cofx * cofx + cofy * cofy )
         cofw = 1.0d0 / cof0
         if( cof0 < 1.0d-6 ) cofw = 1.0d6
         cofx = cofw * cofx
         cofy = cofw * cofy
         coftxx = cofw * coftxx
         coftxy = cofw * coftxy
         cofa = cofa + cofx   * cofx
         cofb = cofb + cofx   * cofy
         cofc = cofc + cofy   * cofy
         cofd = cofd + coftxx * cofx
         cofe = cofe + coftxx * cofy
         coff = coff + coftxy * cofx
         cofg = cofg + coftxy * cofy
      enddo
      cof0   = cofa * cofc - cofb * cofb

      if( cof0 == 0d0 .or. n < 2 ) cycle
      dtxxdx = ( cofd * cofc - cofb * cofe ) / cof0
      dtxxdy = ( cofa * cofe - cofd * cofb ) / cof0
      dtxydx = ( coff * cofc - cofb * cofg ) / cof0
      dtxydy = ( cofa * cofg - coff * cofb ) / cof0
      spirfx(k1) = ( dtxxdx + dtxydy ) / hs(k1)
      spirfy(k1) = ( dtxydx - dtxxdy ) / hs(k1)
   enddo

   do L = lnxi+1,lnx                        ! Boundary conditions for spiral flow forces
      k1 = ln(1,L) ; k2 = ln(2,L)
      spirfx(k1) = spirfx(k2)
      spirfy(k1) = spirfy(k2)
   enddo

   do L = 1,lnx                             ! Mapping forces from global coordinates to local
      k1    = ln(1,L) ; k2 = ln(2,L)
      fx = acl(L) * spirfx(k1) + ( 1.0d0 - acl(L) ) * spirfx(k2)
      fy = acl(L) * spirfy(k1) + ( 1.0d0 - acl(L) ) * spirfy(k2)
      fxl = csu(L) * fx + snu(L) * fy
      adve(L) = adve(L) - fxl               ! Adding the local forces to the momentum equation
   enddo

end subroutine get_spiralforce
