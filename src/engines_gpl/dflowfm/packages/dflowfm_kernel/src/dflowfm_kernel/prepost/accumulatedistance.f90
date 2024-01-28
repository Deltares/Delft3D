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

      !> Stop afstand tussen polygoonpunten vanaf begin in array
      SUBROUTINE accumulateDistance(X,Y,T,MMAX)

      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none

      integer :: mmax
      DOUBLE PRECISION, intent(in)  :: X(MMAX), Y(MMAX) !< Input polyline coordinates
      double precision, intent(out) :: T(MMAX)          !< Output accumulated distances along polyline segments.

      integer :: k

      T(1) = 0d0
      DO K = 2,MMAX
         T(K)  = T(K-1) + dbdistance( x(k), y(k), x(k-1), y(k-1), jsferic, jasfer3D, dmiss)
      ENDDO
      RETURN
      END SUBROUTINE accumulateDistance
