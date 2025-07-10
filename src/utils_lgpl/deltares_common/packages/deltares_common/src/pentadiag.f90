!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2025.                                
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

! =================================================================================================
! =================================================================================================
   subroutine pentadiag( aavec, avec, bvec, cvec, ccvec, dvec, x, n )
      implicit none
      integer          :: i, n
      double precision :: aa(n), a(n), b(n), c(n), cc(n), d(n), x(n)
      double precision :: aavec(n), avec(n), bvec(n), cvec(n), ccvec(n), dvec(n)
      double precision :: cof0

      aa = aavec
      a  = avec
      b  = bvec
      c  = cvec
      cc = ccvec
      d  = dvec

      do i = 2,n-1
         cof0 = a(i) / b(i-1)
         b(i) = b(i) - cof0 *  c(i-1)
         c(i) = c(i) - cof0 * cc(i-1)
         d(i) = d(i) - cof0 *  d(i-1)
         cof0 = aa(i+1) / b(i-1)
         a(i+1) = a(i+1) - cof0 *  c(i-1)
         b(i+1) = b(i+1) - cof0 * cc(i-1)
         d(i+1) = d(i+1) - cof0 *  d(i-1)
      enddo

      cof0 = a(n) / b(n-1)
      b(n) = b(n) - cof0 *  c(n-1)
      c(n) = c(n) - cof0 * cc(n-1)
      d(n) = d(n) - cof0 *  d(n-1)

      x(n)   = d(n) / b(n)
      x(n-1) = ( d(n-1) - c(n-1) * x(n) ) / b(n-1)
      do i = n-2,1,-1
         x(i) = ( d(i) - c(i) * x(i+1) - cc(i) * x(i+2) ) / b(i)
      enddo

   end subroutine pentadiag
