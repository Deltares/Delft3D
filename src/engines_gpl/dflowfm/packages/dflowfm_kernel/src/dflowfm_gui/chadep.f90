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

      SUBROUTINE CHADEP(XP,YP,RD,KEY)
      USE M_MISSING
      implicit none
      double precision :: XP,YP,RD
      INTEGER :: KEY

      double precision :: f
      double precision :: fac
      integer :: jplus
      double precision :: rdol
      CHARACTER WRDKEY*40
      WRDKEY = 'CHANGE SCALAR VALUE'
      RDOL  = RD
      JPLUS = 0
      CALL DISPUT(21)
   10 CONTINUE
      CALL DISVAL1(RD)
      CALL KCIR(XP,YP,RD)
      CALL INKEYEVENT(KEY)

      IF (KEY .EQ. 171) THEN
         CALL HELP(WRDKEY,3)
      ELSE IF (KEY .EQ. 45 .OR. KEY .EQ. 160) THEN
         IF (RD .EQ. dmiss) RD  = 6.9d0
         IF (JPLUS .NE. -1) THEN
            FAC = 1d0
            F   = MAX(.001d0,.01d0*RD)
         ENDIF
         RD     = RD - F*FAC
         FAC    = FAC*1.01d0
         JPLUS  = -1
      ELSE IF (KEY .EQ. 43 .OR. KEY .EQ. 162) THEN
         IF (RD .EQ. dmiss) RD = 6.9d0
         IF (JPLUS .NE. 1) THEN
            FAC = 1d0
            F   = MAX(.001d0,.01d0*RD)
         ENDIF
         RD     = RD + F*FAC
         FAC    = FAC*1.01d0
         JPLUS  = 1
      ELSE IF (KEY .EQ. 32) THEN
         CALL TYPEVALUE(RD,KEY)
         CALL DISVAL1(RD)
         CALL KCIR(XP,YP,RD)
         RETURN
      ELSE IF (KEY .EQ. 68 .OR. KEY .EQ. 68+32 .OR. KEY .EQ. 143) THEN
         RD     = dmiss
         CALL DISVAL1(RD)
         CALL KCIR(XP,YP,RD)
         RETURN
      ELSE IF (KEY .EQ. 27) THEN
         RD = RDOL
         CALL DISVAL1(RD)
         CALL KCIR(XP,YP,RD)
         RETURN
      ELSE IF (KEY .NE. 254 .AND. KEY .NE. 257) THEN
         RETURN
      ENDIF
      GOTO 10
      END SUBROUTINE CHADEP
