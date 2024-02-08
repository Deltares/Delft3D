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

!> check if new node is valid
subroutine checkvalidnode(node, i, j, lconflict)

 use m_grid
 use m_missing

 implicit none

 integer, intent(in)   :: node                      !< node
 integer, intent(in)   :: i, j                      !< indices

 logical, intent(out)  :: lconflict                 !< .false. if valid, .true. otherwise

 integer               :: iL, iR, jB, jT
 integer, dimension(2) :: low, upp                  ! dimensions of ijc

 if ( ijc(i,j) .ne. node .and. ijc(i,j).gt.0 ) then ! conflict
    lconflict = .true.
    return
 end if

! check the four possible connections for conflicts

 low = lbound(ijc)
 upp = ubound(ijc)

 iL = i-1
 if ( iL.ge.low(1) ) then
    if ( ijc(iL,j) .gt. 0 ) call checkgridline(node, ijc(iL,j), lconflict)
    if ( lconflict ) return
 end if

 iR = i+1
 if ( iR.le.upp(1) ) then
    if ( ijc(iR,j) .gt. 0 ) call checkgridline(node, ijc(iR,j), lconflict)
    if ( lconflict ) return
 end if

 jB = j-1
 if ( jB.ge.low(2) ) then
    if ( ijc(i,jB) .gt. 0 ) call checkgridline(node, ijc(i,jB), lconflict)
    if ( lconflict ) return
 end if

 jT = j+1
 if ( jT.le.upp(2) ) then
    if ( ijc(i,jT) .gt. 0 ) call checkgridline(node, ijc(i,jT), lconflict)
    if ( lconflict ) return
 end if

end subroutine
