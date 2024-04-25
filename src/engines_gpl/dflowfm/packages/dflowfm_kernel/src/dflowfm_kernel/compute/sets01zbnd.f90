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

 !> Sets s1 or s0 water levels at zbndz-type boundaries.
 subroutine sets01zbnd(n01, jasetBlDepth)
 use m_flowgeom
 use m_flow
 use m_flowtimes
 use m_missing
 use m_sobekdfm

 use m_fm_icecover, only: ice_apply_pressure, ice_p
 
 implicit none
 
 integer, intent(in) :: n01          !< Selects whether s0 or s1 has to be set.
 integer, intent(in) :: jasetBlDepth !< Whether or not (1/0) to set the boundary node bed levels, based on depth below s1. Typically only upon model init (based on initial water levels).

 integer          :: n, kb, k2, itpbn, L, ibnd
 double precision :: zb, hh
 
 do n  = 1, nbndz                                    ! overrides for waterlevel boundaries
    kb      = kbndz(1,n)
    k2      = kbndz(2,n)
    L       = kbndz(3,n)
    itpbn   = kbndz(4,n)
    if     (itpbn == 1) then                         ! waterlevelbnd
       zb   = zbndz(n)
       if (alfsmo < 1d0) then
          zb = alfsmo*zb + (1d0-alfsmo)*zbndz0(n)
       endif
    else if (itpbn == 2) then                        ! neumannbnd, positive specified slope leads to inflow
       !zb   = s0(k2) + zbndz(n)*dx(L)
       zb   = s1(kb)
    else if (itpbn == 5) then                        ! Riemannbnd
       hh   = max(epshs, 0.5d0*( hs(kb) + hs(k2) ) )
       zb   = 2d0*zbndz(n) - zbndz0(n) - sqrt(hh/ag)*u1(L)
    else if (itpbn == 6) then                        ! outflowbnd
       if (u0(L) > 0) then   ! on inflow, copy inside
          zb = s0(k2)
          if (n01 == 0) then
             s0(kb) = max(zb, bl(kb)) ! TODO: AvD: if single time step is being restarted, then this line will have overwritten some of the old s0 values.
          else
             s1(kb) = max(zb, bl(kb))
          endif
      endif
    else if (itpbn == 7) then                         ! qhbnd
       zb   = zbndz(n)
       if (alfsmo < 1d0) then
          zb = alfsmo*zb + (1d0-alfsmo)*zbndz0(n)
       endif
    endif

    if (japatm > 0 .and. PavBnd > 0) then
       zb = zb - ( patm(kb) - PavBnd )/(ag*rhomean)
    endif

    if (ice_apply_pressure) then
       zb = zb - ice_p(kb) /(ag*rhomean)
    endif

!    zb = max( zb, bl(kb) + 1d-3 )

    ! When requested, set bl of bnd nodes to a certain depth below (initial) water level.
    if (jasetBlDepth == 1 .and. allocated(bndBlDepth)) then
       ibnd = kbndz(5,n)
       if (bndBlDepth(ibnd) /= dmiss) then
          bl(kb) = min(bl(kb), zb - bndBlDepth(ibnd))
          bob(1,L) = bl(kb)
          bob(2,L) = bl(kb)
          bob0(1,L) = bl(kb)
          bob0(2,L) = bl(kb)
          bl(k2) = bl(kb)
       end if
    end if

    if (itpbn < 6 .or. itpbn == 7) then
       if (n01 == 0) then
          s0(kb) = max(zb, bl(kb)) ! TODO: AvD: if single time step is being restarted, then this line will have overwritten some of the old s0 values.
          hs(kb) = s0(kb)- bl(kb)
       else
          s1(kb) = max(zb, bl(kb))
          hs(kb) = s1(kb)- bl(kb)
       endif
    endif

 enddo

 call  set_1d2d_01()

 end subroutine sets01zbnd
