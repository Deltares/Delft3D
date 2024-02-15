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

! updates zk value at specified net node index using diven delta
! TODO: extend it to multiple indices
subroutine update_land_nodes(node_index, new_zk)
  use network_data
  use m_missing
  use m_polygon
  use m_flowgeom
  use m_flow
  use unstruc_display
  use m_sediment
  implicit none
  integer, intent(in) :: node_index
  double precision, intent(in) :: new_zk

  ! locals
  integer :: kk, k, n, nn, ncol, j, i
  double precision :: old_zk

  if (ndx == 0) return

  k = node_index

  if (npl > 2) then
    old_zk = zk(k)
    zk(k) = new_zk
    if (jaceneqtr == 2 .and.jased > 0) then
        do j = 1,mxgr
            grainlay(j, k ) = max(0d0, grainlay(j, k ) + (old_zk - new_zk)/mxgr)
        enddo
    endif
    call isocol(zk(k), ncol)
    call movabs(xk(k), yk(k))
    call hlcir2(rcir, ncol, 30)
  else
    old_zk = zk(k)
    zk(k) = new_zk
    if (jaceneqtr == 2 .and.jased > 0) then
    do j = 1,mxgr
        grainlay(j, k ) = max(0d0, grainlay(j, k) + (old_zk - new_zk)/mxgr)
    enddo
    endif
    call isocol(zk(k),ncol)
    call movabs(xk(k),yk(k))
    call hlcir2(rcir,ncol,30)
  endif

  ! NOTE: update of bobs/bl/s1/hs is now in subroutine on_land_change().
  !       Should be called separately on call-site!

end subroutine update_land_nodes
