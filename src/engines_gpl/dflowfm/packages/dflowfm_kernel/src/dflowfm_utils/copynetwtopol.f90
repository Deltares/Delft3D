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

subroutine copynetwtopol( )
use m_polygon
use m_missing
use network_data
use unstruc_display
implicit none
integer :: n, L, k1, k2, key

call increasepol(3*numl+1000, 0)

n = 0
do L = 1,numL
   n = n + 1 ; k1 = kn(1,L) ; xpl(n) = xk(k1) ; ypl(n) = yk(k1) ; zpl(n) = zk(k1)
   n = n + 1 ; k2 = kn(2,L) ; xpl(n) = xk(k2) ; ypl(n) = yk(k2) ; zpl(n) = zk(k2)
   n = n + 1 ; k2 = kn(2,L) ; xpl(n) = dmiss  ; ypl(n) = dmiss  ; zpl(n) = dmiss
enddo
npl = n
ndrawpol = 3
numk = 0 ; numL = 0; kn = 0

end subroutine copynetwtopol
