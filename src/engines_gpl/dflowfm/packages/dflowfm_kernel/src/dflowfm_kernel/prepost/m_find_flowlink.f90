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
   
module m_find_flowlink
   
   implicit none
   
   private
   
   public :: find_1D_or_boundary_flowlink_bruteforce
   
   contains
   
   !> Find the 1-D or boundary flowlink with the shortest perpendicular distance to the point [x,y]
   !! Brute-force approach: simply check all flowlinks in the entire grid
   subroutine find_1D_or_boundary_flowlink_bruteforce(x, y, link_id_closest)
      use stdlib_kinds, only: dp
      use m_flowgeom, only: lnx, lnx1D, lnxi, ln, xz, yz, ndx
      use m_sferic, only: jsferic, jasfer3D
      use geometry_module, only: dlinedis
      use m_missing, only: dmiss
      
      real(dp), intent(in   ) :: x, y            !< x,y-coordinates
      integer,  intent(  out) :: link_id_closest !< id of the flowlink whose midpoint lies closest to the point [x,y]
      
      integer  :: link_id, ka, kb
      real(dp) :: dist_perp, dist_perp_min
      integer  :: ja
      real(dp) :: xa, ya, xb, yb, xn, yn
      
      link_id_closest = 0
      dist_perp_min   = huge(dist_perp_min)
   
      do link_id = 1, lnx
         if (link_id <= lnx1D .or. link_id > lnxi) then ! Only check 1-D and boundary flowlinks
            ka = ln(1,link_id)
            kb = ln(2,link_id)
            xa = xz(ka)
            ya = yz(ka)
            xb = xz(kb)
            yb = yz(kb)
            call dlinedis(x, y, xa, ya, xb, yb, ja, dist_perp, xn, yn, jsferic, jasfer3D, dmiss)
            if (ja == 1) then
               if (dist_perp < dist_perp_min) then
                  link_id_closest = link_id
                  dist_perp_min = dist_perp
               end if
            end if
         end if
      end do
      
   end subroutine find_1D_or_boundary_flowlink_bruteforce
   
end module m_find_flowlink