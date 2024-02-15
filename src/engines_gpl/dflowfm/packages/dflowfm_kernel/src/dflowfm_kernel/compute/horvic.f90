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

 !> compute viscous flux balance of cell ln (n12,L) in coordinate frame of L
 double precision function horvic(n12,L)             ! horizontal viscosity term
 use m_flow
 use m_flowgeom
 use m_missing
 use m_sferic
 implicit none

 integer :: L                                        ! in direction of link L
 integer :: n12                                      ! find hor visc term for cell 1 or 2

 ! locals
 integer :: LL, LLL, LLLL                            ! for links LL,
 integer :: k12, k1, k2, k3, k4, isig                ! relevant node, 1 or 2
 double precision :: cs, sn, csL, snL
 double precision :: duxdn, duydn, duxdt, duydt, txx, tyy, c11,c12,c22, vicl

 double precision :: txx_k12, tyy_k12
 integer          :: ik1, ik2, in3, in4

 double precision, external :: nod2linx, nod2liny, lin2nodx, lin2nody, cor2linx, cor2liny

 horvic = 0d0
 csL    = csu(L) ; snL = snu(L)
 k12    = ln(n12,L)


 do LL   = 1, nd(k12)%lnx                            ! loop over all attached links

    LLL  = nd(k12)%ln(LL)
    LLLL = iabs(LLL)

    if (LLLL .ne. L) then

       if (LLL < 0) then
          cs =  csu(LLLL)
          sn =  snu(LLLL)
          k1 = ln  (1,LLLL) ; k2 = ln  (2,LLLL)    ! k1 = k12
          k3 = lncn(1,LLLL) ; k4 = lncn(2,LLLL)
          ik1 = 1
          ik2 = 2
          in3 = 1
          in4 = 2
       else
          cs = -csu(LLLL)
          sn = -snu(LLLL)
          k1 = ln  (2,LLLL) ; k2 = ln  (1,LLLL)    ! k1 = k12
          k3 = lncn(2,LLLL) ; k4 = lncn(1,LLLL)
          ik1 = 2
          ik2 = 1
          in3 = 2
          in4 = 1
       endif

       if (jasfer3D == 0) then
          duxdn =  ( ucx(k2) -  ucx(k1)) * dxi(LLLL)
          duydn =  ( ucy(k2) -  ucy(k1)) * dxi(LLLL)
          duxdt =  (ucnx(k4) - ucnx(k3)) * wui(LLLL)
          duydt =  (ucny(k4) - ucny(k3)) * wui(LLLL)
       else
          duxdn = ( nod2linx(LLLL,ik2,ucx(k2),ucy(k2))-nod2linx(LLLL,ik1,ucx(k1),ucy(k1)) ) * dxi(LLLL)
          duydn = ( nod2liny(LLLL,ik2,ucx(k2),ucy(k2))-nod2liny(LLLL,ik1,ucx(k1),ucy(k1)) ) * dxi(LLLL)
          duxdt = ( cor2linx(LLLL,in4,ucnx(k4),ucny(k4))-cor2linx(LLLL,in3,ucnx(k3),ucny(k3)) ) * wui(LLLL)
          duydt = ( cor2liny(LLLL,in4,ucnx(k4),ucny(k4))-cor2liny(LLLL,in3,ucnx(k3),ucny(k3)) ) * wui(LLLL)
       endif

       c11   = cs*cs ; c12=cs*sn ; c22=sn*sn
       txx   = duxdn + c11*duxdn + c12*(duydn - duxdt) - c22*duydt
       tyy   = duydn + c11*duxdt + c12*(duxdn + duydt) + c22*duydn

       if (javiusp == 1) then
           vicl = viusp(LLLL)
       else
           vicl = vicouv
       endif


       txx_k12 = lin2nodx(LLLL,ik1,txx,tyy)
       tyy_k12 = lin2nody(LLLL,ik1,txx,tyy)

       if (istresstyp == 4) then
          if (jasfer3D == 0) then
             horvic = horvic + ( txx*csl + tyy*snl )*wu(LLLL)*vicL
          else
             horvic = horvic + ( nod2linx(L,n12,txx_k12,tyy_k12)*csl + nod2liny(L,n12,txx_k12,tyy_k12)*snl )*wu(LLLL)*vicL
          endif

       else if (istresstyp == 5) then  ! volume averaged
          ! horvic = horvic + ( txx*csl + tyy*snl )*au(LLLL)*vicL
          horvic = horvic + ( nod2linx(L,n12,txx_k12,tyy_k12)*csl + nod2liny(L,n12,txx_k12,tyy_k12)*snl )*au(LLLL)*vicL
       endif

    endif

 enddo
 horvic = horvic*bai(k12)
 if (istresstyp == 5) then  ! volume averaged
    horvic = horvic / hs(k12)
 endif
 end function Horvic
