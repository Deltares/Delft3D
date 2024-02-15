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

  subroutine makepdf(r,n)
  use m_statistics
  integer :: n
  real    :: r(n), s
  integer :: k,L
  if (n == 0) return
  xpdf = 0d0
  do L = 1,n
     do k = 1,npdf
        if (r(L)  >= ypdf(k)) then
           xpdf(k) = xpdf(k) + 1d0
           exit
        endif
     enddo
  enddo

  s = 0
  do k = 1,npdf
     s = s+xpdf(k)
  enddo
  if (s == 0) return
  xpdf = xpdf / s
  do k = 2,npdf
     xpdf(k) = xpdf(k) + xpdf(k-1)
  enddo
  end subroutine makepdf
