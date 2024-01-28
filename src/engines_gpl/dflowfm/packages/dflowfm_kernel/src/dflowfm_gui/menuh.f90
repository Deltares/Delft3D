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

      SUBROUTINE MENUH (JA,NUM,NWHAT)
      use m_devices
      implicit none
      integer :: ja
      integer :: num
      integer :: nwhat

      integer :: infoinput
      integer :: imenuhoriz
      integer :: iw
      integer :: key
      integer :: maxop
      integer :: maxopt
      integer :: nlevel
      PARAMETER (MAXOP = 20)
      CHARACTER*10 OPTION(MAXOP)
      CHARACTER WRDKEY*40
      COMMON /HELPNOW/ WRDKEY,NLEVEL
!
!     Keuzemenu horizontaal
!
      OPTION (1) = 'FILES     '
      OPTION (2) = 'OPERATIONS'
      OPTION (3) = 'DISPLAY   '
      OPTION (4) = 'EDIT      '
      OPTION (5) = 'ADDSUBDEL '
      OPTION (6) = 'VARIOUS   '
      MAXOPT     = 6
      KEY        = 0
!
      IW    = IWS
!
   10 CONTINUE
!
   20 CONTINUE
      IF (JA .EQ. 1) THEN
         CALL TIMLIN()
         CALL BOTLIN(0,1,KEY)
         IF (NOPSYS .EQ. 1) THEN
            CALL ITEXTCOLOUR('BBLUE','BWHITE')
         ELSE
            CALL ITEXTCOLOUR('BLACK','BWHITE')
         ENDIF
         CALL INHIGHLIGHT('BWHITE','RED')
         NUM    = IMenuHoriz(OPTION,MAXOPT,1,1,IW,0,1)
         CALL TIMLIN()
      ENDIF
      IF (NOPSYS .EQ. 1) THEN
         CALL InHighlight('BWHITE','WHITE')
         CALL ITEXTCOLOUR('BWHITE','WHITE')
      ELSE
         CALL InHighlight('BLACK','WHITE')
         CALL ITEXTCOLOUR('BLACK','WHITE')
      ENDIF
      CALL IOUTMenuHoriz(OPTION,MAXOPT,1,1,IW,0,1)
      IF (JA .NE. 1) RETURN
!
      KEY = InfoInput(55)
      IF (KEY .NE. 23) THEN
         NLEVEL = 1
         WRDKEY = OPTION(NUM)
      ENDIF
      IF (KEY .EQ. 21 .OR. KEY .EQ. 22) THEN
!        INS KEY
         CALL MENUV1(NUM,NWHAT)
         IF (NWHAT .EQ. 0) GOTO 20
         CALL IOUTSTRINGXY(1,2,' OPTION : '//WRDKEY)
         RETURN
      ELSE IF (KEY .EQ. 23 .OR. KEY .EQ. -2) THEN
!        ESC OR OUTSIDE
         NUM = 0
         RETURN
      ELSE
         CALL FKEYS(KEY)
         IF (KEY .EQ. 3) RETURN
      ENDIF
      GOTO 10
!
      END
