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

      SUBROUTINE SPLINT(YA,Y2A,N,X,Y)
      implicit none

      integer                                               :: N     !< number of control points
      double precision, dimension(N)                        :: ya    !< control point values
      double precision, dimension(N)                        :: y2a   !< control point second order derivatives
      double precision,                         intent(in)  :: x     !< spline coordinate
      double precision,                         intent(out) :: y     !< interpolated value at prescribed spline coordinate

!     AANGEPAST VOOR GEBRUIK BIJ XA IS ENKEL 0,1,2...N-1
!     ZOEKEN KAN GESLOOPT DOOR DEFINITIE VAN XA IS 0,1,

      double precision                                      :: EPS, A,B, SPLFAC = 1D0

      integer                                               :: intx
      integer                                               :: KLO, KHI

      EPS  = 0.00001D0
      INTX = INT(X)
      IF (X-INTX .LT. EPS) THEN
         Y = YA(INTX+1)
      ELSE
         KLO = INTX + 1
         KHI = KLO  + 1
         A   = ((KHI-1)-X)
         B   = (X-(KLO-1))
         Y   = A*YA(KLO) + B*YA(KHI) + SPLFAC*( (A**3-A)*Y2A(KLO) + (B**3-B)*Y2A(KHI) )/6D0
      ENDIF
      RETURN
      END SUBROUTINE SPLINT
