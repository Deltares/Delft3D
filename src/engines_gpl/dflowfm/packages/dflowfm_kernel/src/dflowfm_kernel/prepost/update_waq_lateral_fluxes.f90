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

!> Update the cumulative waq sink source fluxes for the just set timestep.
!!
!! Should be called at the end of each computational timestep. In the waq-output, the cumulative values should be divided
!! by ti_waq, as the cumulative values are multiplied by each timestep dts (necessary because of non-constant timestep).
!!
!! The code uses similair ways to distribute discharges over layers as the calling subroutine setsorsin. Changes in the
!! calling subroutine should also be taken over in this routine!
subroutine update_waq_lateral_fluxes()
use waq
use m_partitioninfo, only: is_ghost_node
use m_flow
use m_flowgeom
use m_flowtimes
use m_lateral, only: numlatsg, nodeCountLat, n1latsg, n2latsg, nnlat, qqlat
implicit none

integer :: k, k1, ilat
integer :: ilatwaq, nlayer, num_layers

! Accumulate lateral discharges for waq
ilatwaq = 0
num_layers = max(1,kmx)
do ilat = 1,numlatsg
   do k1=n1latsg(ilat),n2latsg(ilat)
      k = nnlat(k1)
      if (k > 0) then
         if (.not. is_ghost_node(k)) then
            ilatwaq = ilatwaq + 1
            do nlayer = 1, num_layers
               qlatwaq(ilatwaq) = qlatwaq(ilatwaq) + dts*qqLat(nlayer,k)
            end do
         end if
      end if
   end do
end do

end subroutine update_waq_lateral_fluxes
