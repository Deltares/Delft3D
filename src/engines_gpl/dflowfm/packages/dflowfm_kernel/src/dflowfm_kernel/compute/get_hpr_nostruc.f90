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

!> Gets hydraulic radius at a flow link intended for use in flow area
!! calculation for velocities, optionally ignoring any hydraulic structure
!! on it, based on current water depth.
!! By default, hu(L) is returned when no structure is present, or when the
!! setting ChangeVelocityAtStructures is off (0).
pure function get_hpr_nostruc(L) result(hpr)
   use m_flowgeom, only: bob, bob0
   use m_flow, only: hu, u1
   use m_flowparameters, only: changeVelocityAtStructures
   implicit none
   integer, intent(in   ) :: L   !< Flow link number
   double precision       :: hpr !< Hydraulic radius for current water depth.

   hpr = hu(L)
   if (changeVelocityAtStructures) then
      if ( bob(1,L) /= bob0(1,L) .or. bob(1,L) /= bob0(1,L) ) then
         if (u1(L) > 0d0) then
            ! In case of structure links Hu is the waterdepth at the crest. We want to use the waterdepth 
            ! with respect to the channel bed level for a correct flow area of the channel
            hpr = hu(L)+bob(1,L)-bob0(1,L)
         else
            hpr = hu(L)+bob(2,L)-bob0(2,L)
         endif
      endif
   endif
end function get_hpr_nostruc
