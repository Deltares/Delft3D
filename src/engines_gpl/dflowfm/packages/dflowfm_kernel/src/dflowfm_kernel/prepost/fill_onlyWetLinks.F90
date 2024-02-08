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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

!> Fills array onlyWetLinks, which contains indices of flowlinks that are wet.
subroutine fill_onlyWetLinks()
   use m_flowgeom, only: lnx, lnx1d, lnxi, wetLinkCount, onlyWetLinks, wetLink2D, wetLinkBnd, bl, ln
   use m_flow, only: hu, s1
   implicit none

   integer :: L

   wetLinkCount = 0
   ! Set wetLink2D and welLinkBnd to the number of links +1. Otherwise the do loops in VOL12D will be 1 item to short for
   ! models without a 2d mesh and no boundaries
   wetLink2D = lnx+1
   wetLinkBnd = lnx+1
   do L = 1, lnx
      if ( (hu(L) > 0d0) .or. (s1(ln(1,L)) - bl(ln(1,L)) > 0d0) .or. (s1(ln(2,L)) - bl(ln(2,L)) > 0d0) ) then
        wetLinkCount = wetLinkCount +1
        onlyWetLinks(wetLinkCount) = L
        if (L > lnx1d .and. wetLinkCount < wetLink2D) then
           wetLink2D = wetLinkCount
        endif
        if (L > lnxi .and. wetLinkCount < wetLinkBnd) then
           wetLinkBnd = wetLinkCount
        endif

      endif
   enddo
   wetLink2D  = min(wetLink2D,  wetLinkCount+1)
   wetLinkBnd = min(wetLinkBnd, wetLinkCount+1)

end subroutine fill_onlyWetLinks
