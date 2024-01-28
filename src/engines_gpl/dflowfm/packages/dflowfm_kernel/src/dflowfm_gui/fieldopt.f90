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

      SUBROUTINE FIELDOPT(NFLD)
      USE M_GRID
      implicit none
      integer :: nfld
      integer, PARAMETER :: MAXOP = 64
      integer :: nwhat2, maxexp, maxopt, i
      CHARACTER*40 OPTION(MAXOP),EXP(MAXOP),FIELDOP
      EXP(1)    = 'MENU 10                                 '
      EXP(2)    = 'GRID EDIT OPTIONS                       '
      MAXOPT    = 22
      DO 10 I = 1,MAXOPT
         OPTION(I) =  FIELDOP(I)
    10 CONTINUE
      NWHAT2  = NFLD
      CALL MENUV2(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .GE. 1) THEN
         IF (NWHAT2 == 19) THEN
            CALL ORTHOGRID(1,1,MC,NC)
         else IF (NWHAT2 == 20) THEN
            call LOCALREFINE(Nwhat2, 1, 1, mc, nc, 1)
         else IF (NWHAT2 == 21) THEN
            call LOCALREFINE(Nwhat2, 1, 1, mc, nc, 2)
         ELSE
            NFLD = NWHAT2
         ENDIF
      ENDIF
      RETURN
      END subroutine fieldopt
