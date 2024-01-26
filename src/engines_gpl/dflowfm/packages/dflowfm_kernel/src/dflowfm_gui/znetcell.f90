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

   double precision function znetcell(k)

   use unstruc_display
   use m_netw
   use m_flowgeom
   use m_missing
   use geometry_module, only: dbdistance
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   integer          :: k, k1, k2, k3, n, ja
   double precision :: uu1, vv1, uu2, vv2 ! not used here
   double precision :: xdum, ydum, area, phimin, phimax
   double precision :: xx1,yy1,zz1,xx2,yy2,zz2,xx3, yy3, zz3, xy, rn, R3, XN, YN, ZN, DEPTH, TSIG, SLOPE, RK

   COMMON /DRAWTHIS/  ndraw(50)
   integer        ::  ndraw

   znetcell = DMISS

   if ( NDRAW(33)>= 3 .and. NDRAW(33)<=5) then
      call orthonet_compute_orientation(znetcell, uu1, vv1, uu2, vv2, k)
   else if ( ndraw(33)==6) then   ! cell area
      znetcell = ba(k)
   else if ( ndraw(33)==2 ) then  ! cell numbers
      if ( netcell(k)%N.gt.0 ) znetcell = dble(k)
   else if ( ndraw(33)==8 ) then  ! cell tri, 4, 5etc
       znetcell = dble( netcell(k)%n )
   else if ( ndraw(33)==9 ) then  ! cell normalised centre of gravity - circumcentre distance
       if (ba(k) > 0) then
           znetcell = dbdistance(xz(k),yz(k), xzw(k), yzw(k), jsferic, jasfer3D, dmiss) / sqrt(ba(k))
       else
           znetcell = 0d0
       endif
   else if ( ndraw(33)==10 .or. ndraw(33)==11 ) then ! slope
       k1 = netcell(k)%nod(1)
       k2 = netcell(k)%nod(2)
       k3 = netcell(k)%nod(3)

       XX1 =  Xk(k1) - Xk(k2)  ! getdx etc
       YY1 =  Yk(k1) - Yk(k2)
       ZZ1 =  Zk(k1) - Zk(k2)
       XX2 =  Xk(k1) - Xk(k3)
       YY2 =  Yk(k1) - Yk(k3)
       ZZ2 =  Zk(k1) - Zk(k3)
       XX3 =  (YY1*ZZ2 - YY2*ZZ1)
       YY3 = -(XX1*ZZ2 - XX2*ZZ1)
       ZZ3 =  (XX1*YY2 - XX2*YY1)
       R3 =  SQRT(XX3*XX3 + YY3*YY3 + ZZ3*ZZ3)
       IF (R3 .NE. 0) THEN
         XN = XX3/R3
         YN = YY3/R3
         ZN = ZZ3/R3
         XY = SQRT(XN*XN + YN*YN)
         IF (ZN .NE. 0) THEN
            slope = ABS(XY/ZN)
            znetcell = slope
            IF (ndraw(33) == 11) THEN
               DEPTH    = - (ZK(K1) + ZK(K2) + ZK(K3)) / 3
               IF (DEPTH .GE. .01) THEN
                  TSIG  = 5D0
                  CALL getwavenr(depth,tsig,rk)
                  znetcell = SLOPE/(DEPTH*RK)
               else
                  znetcell = dmiss
               ENDIF
            ENDIF
         ENDIF
      ENDIF

   else if ( ndraw(33)== 12 .or. ndraw(33)==13 ) then ! min, max angles
       call CHECKTRIANGLEnetcell(k,JA,phimin,phimax)
       if ( ndraw(33)== 12) then
           znetcell = phimin
       else
           znetcell = phimax
       endif
   endif

   end function znetcell
