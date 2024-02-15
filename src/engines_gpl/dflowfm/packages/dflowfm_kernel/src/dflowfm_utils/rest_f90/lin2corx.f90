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

!>    return x-component in corner (netnode) coordinate frame of a vector in link coordinate frame
      double precision function lin2corx(L,i12,ux,uy)
         use m_flowgeom, only: csbn, snbn
         use m_sferic
         implicit none

         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: i12 !< left (1) or right (2) corner (netnode)
         double precision, intent(in) :: ux, uy !< vector components in flowlink coordinate frame

         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            lin2corx = ux
         else
            lin2corx =  csbn(i12,L) * ux - snbn(i12,L) * uy
         end if

         return
      end function lin2corx
