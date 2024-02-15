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

      SUBROUTINE DISVAL1(DEP)
      use unstruc_colors
      implicit none
      double precision :: DEP
      CHARACTER TEX*8
      IF (ABS(DEP) .LT. 10) THEN
         WRITE(TEX(1:),'(F8.5)') DEP
      ELSE IF (ABS(DEP) .LT. 100) THEN
         WRITE(TEX(1:),'(F8.4)') DEP
      ELSE IF (ABS(DEP) .LT. 1000) THEN
         WRITE(TEX(1:),'(F8.3)') DEP
      ELSE IF (ABS(DEP) .LT. 10000) THEN
         WRITE(TEX(1:),'(F8.2)') DEP
      ELSE IF (ABS(DEP) .LT. 100000) THEN
         WRITE(TEX(1:),'(F8.1)') DEP
      ELSE
         WRITE(TEX(1:),'(E8.1)') DEP
      ENDIF
      CALL KTEXT(TEX,IWS-7,4,15)
      RETURN
      END SUBROUTINE DISVAL1
