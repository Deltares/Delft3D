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

 !> Gets the local layer numbers for a given flow link.
 !! Always uses the maximum possible layer range instead of the actual range.
 !! Note: works both for sigma and z, but for sigma, the return values are trivial: nlaybL==1, nrlayLx==kmx.
 subroutine getlayerindicesLmax(L, nlaybL, nrlayLx)
 use m_flow
 implicit none

 integer, intent(in)  :: L       !< Flow link (L \in [1,lnx] )
 integer, intent(out) :: nlaybL  !< Layer number for the bottom layer (in 1:kmxL(L))
 integer, intent(out) :: nrlayLx !< Max nr. of active layers for this flow link.

 nrlayLx = kmxL(L)
 nlaybL = kmx - nrlayLx + 1

 end subroutine getlayerindicesLmax
