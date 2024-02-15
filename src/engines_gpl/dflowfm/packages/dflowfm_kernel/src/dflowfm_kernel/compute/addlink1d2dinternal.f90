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

 subroutine addlink1D2Dinternal(L,japerim)                         ! and add area's and volumes of 1D2D links
 use m_flowgeom
 use m_flow
 use m_missing
 use unstruc_channel_flow

 implicit none

 integer           :: japerim, L

 integer           :: k1, k2, k3, k4, K, jaconv, jaconvu,ifrctyp
 double precision  :: hpr1, ar1, wid1, aconv1, hpr2, ar2, wid2, aconv2, aru, widu, aconvu
 double precision  :: dx1, dx2, frcn, BL1, BL2, b21, wu2, ai
 double precision  :: beta, bt2, deltaa,hyr, uucn, ucna, Cz
 double precision, external :: get_hpr_nostruc

 k1  = ln(1,L) ; k2 = ln(2,L)
  if (bob0(1,L) < bob0(2,L)) then
    BL1 = bob0(1,L); BL2 = bob0(2,L)
 else
    BL1 = bob0(2,L); BL2 = bob0(1,L)
 endif
 wu2 = wu(L)

 b21 = BL2 - BL1
 ai  = b21/wu2

 if (japerim == 0) then

    hpr1   = s1(k1)-BL1
    ! Also include waterdepth ==0 in order to make a1 /=0, this prevents SAAD errors                                                                       ! == 1,2: (ibedlevtyp=3), hrad = A/P   , link or node
    if (hpr1 >= 0) then
       call getlinkareawid2D(L,wu2,b21,ai,hpr1,ar1,wid1)
       dx1   = 0.5d0*dx(L)*0.5d0 ! acl(L)
       !if (k1 > ndx2D) dx1 = 2*dx1
       a1(k1)   = a1(k1)   + dx1*wid1
       vol1(k1)   = vol1(k1)   + dx1*ar1
    endif

    hpr2 = s1(k2)-BL1                                                                              ! == 5,6: (ibedlevtyp=3), 2D conveyance, link or node
    ! Also include waterdepth ==0 in order to make a1 /=0, this prevents SAAD errors
    if (hpr2 >= 0) then
       call getlinkareawid2D(L,wu2,b21,ai,hpr2,ar2,wid2)
       dx2      = 0.5d0*dx(L)*0.5d0 ! (1d0-acl(L))
       !if (k2 > ndx2D) dx2 = 2*dx2
       a1(k2)   = a1(k2)   + dx2*wid2
       vol1(k2)   = vol1(k2)   + dx2*ar2
    endif

 else
    if (hu(L) > 0d0) then

       !DIR$ INLINE
       hpr1 = get_hpr_nostruc(L)
    frcn    = frcu(L)
    ifrctyp = ifrcutp(L)
    if (jaconveyance2D > 0) then

       jaconv = min(2,jaconveyance2D)
       CALL getprof2d(hpr1,wu2,b21,ai,frcn,ifrctyp, widu,aru,aconvu,jaconv, beta, deltaa,hyr)

       if (frcn >  0) then
           cfuhi(L) = aifu(L)*ag*aconvu
       else
           cfuhi(L) = 0d0
       endif
       au(L) = aru
    else
       au(L) = hpr1*wu(L)
       if (frcn >  0) then
          call getcz(hu(L), frcn, ifrctyp, Cz, L)
          cfuhi(L) = ag / (hu(L)*Cz*Cz)
       else
          cfuhi(L) = 0d0
       end if
    endif
 endif

    if(network%loaded) then
       hpr1   = s1(k1)-BL1
       ! Also include waterdepth ==0 in order to make a1 /=0, this prevents SAAD errors                                                                       ! == 1,2: (ibedlevtyp=3), hrad = A/P   , link or node
       if (hpr1 >= 0) then
          call getlinkareawid2D(L,wu2,b21,ai,hpr1,ar1,wid1)
          dx1   = 0.5d0*dx(L)*0.5d0 ! acl(L)
          !if (k1 > ndx2D) dx1 = 2*dx1
          vol1_f(k1) = vol1_f(k1) + dx1*ar1
       endif

       hpr2 = s1(k2)-BL1                                                                              ! == 5,6: (ibedlevtyp=3), 2D conveyance, link or node
       ! Also include waterdepth ==0 in order to make a1 /=0, this prevents SAAD errors
       if (hpr2 >= 0) then
          call getlinkareawid2D(L,wu2,b21,ai,hpr2,ar2,wid2)
          dx2      = 0.5d0*dx(L)*0.5d0 ! (1d0-acl(L))
          !if (k2 > ndx2D) dx2 = 2*dx2
          vol1_f(k2) = vol1_f(k2) + dx2*ar2
       endif
    endif
 endif

 end subroutine addlink1D2Dinternal
