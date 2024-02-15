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

     SUBROUTINE KPLOTPLUSMIN(IPM)
      USE M_FLOWGEOM
      USE M_FLOW
      use m_xbeach_data, only: itheta_view
      implicit none
      integer :: IP, IPM, NRLAY

      if (kmx >= 1) then

         ip = ipm
         if (kplotfrombedorsurface .ne. 1) then
            ip = -1*ipm
         endif

         KPLOT = KPLOT+ip
         kplot = max(1,min(kplot,kmx))

         CALL TEXTFLOW()
      else if ( jawave.eq.4 ) then
         itheta_view = max(min(itheta_view + sign(1,ipm), ntheta), 1)
      end if
     END SUBROUTINE KPLOTPLUSMIN
