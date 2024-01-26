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

!< update runup values per dts
subroutine updateValuesOnRunupGauges()
    use m_monitoring_runupgauges
    use m_missing
    use m_flow, only: s1, hu, hs
    use m_cell_geometry, only: xz, yz
    use m_flowgeom, only: ln, acl, xu, yu, bl
    use m_flowparameters, only: epshs, epshu

    implicit none

    integer                                       :: irug
    integer                                       :: k1, k2, k
    integer                                       :: L, il
    double precision                              :: maxx, maxy, maxz, maxk

!   update runup on gauge locations
    hs = max(s1-bl,0d0)
    do irug = 1, nrug
       maxz = -huge(0d0)
       maxx = dmiss
       maxy = dmiss
       maxk = 0
       ! determine runup value
       if (rug(irug)%path%lnx==0) cycle
       do il = 1, rug(irug)%path%lnx
          L = abs(rug(irug)%path%ln(il))

          k1 = ln(1,L); k2 = ln(2,L)

          if (hs(k1)>epshu .and. hs(k2)<=epshu) then
             if (s1(k1)>=maxz) then
                maxz = s1(k1)
                maxx = xz(k1)
                maxy = yz(k1)
             endif
          elseif (hs(k2)>epshu .and. hs(k1)<=epshu) then
             if (s1(k2)>=maxz) then
                maxz = s1(k2)
                maxx = xz(k2)
                maxy = yz(k2)
             endif
          endif
       enddo
       if (rug(irug)%maxruh<=maxz) then
          rug(irug)%maxruh = maxz     ! collected at dts, written at dt_user (or longer). Reset after writing
          rug(irug)%maxx   = maxx
          rug(irug)%maxy   = maxy
       endif
    enddo

end subroutine updateValuesOnRunupGauges
