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

      SUBROUTINE MINMAX(      X, MXLAN,   XMIN,   XMAX, MAXLAN)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: maxlan
      integer :: mxlan
      double precision :: xmax
      double precision :: xmin
      double precision :: xx
!     BEPAAL MINIMUM EN MAXIMUM VAN EEN EENDIMENSIONALE ARRAY
      double precision :: X(MAXLAN)

      IF (MXLAN .EQ. 0) THEN
         XMIN = 0
         XMAX = 0
         RETURN
      ENDIF

      XMIN =  10D20
      XMAX = -10D20
      do I = 1,MXLAN
         XX   = X(I)
         IF (XX .NE. dmiss) THEN
            XMIN = MIN(XMIN,XX)
            XMAX = MAX(XMAX,XX)
         ENDIF
      end do
      IF (XMIN .EQ. 10D20) XMIN = 0
      IF (XMAX .EQ.-10D20) XMAX = 0
      RETURN
      END
