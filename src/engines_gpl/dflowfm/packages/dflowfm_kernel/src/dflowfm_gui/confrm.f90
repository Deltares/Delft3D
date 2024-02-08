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

      SUBROUTINE CONFRM(TEXT,JAZEKR)
      use unstruc_display
      implicit none

      CHARACTER TEXT*(*)
      integer :: jazekr

      integer :: imenutwo
      integer :: infoattribute
      integer :: infoinput
      integer :: iopt
      integer :: iw
      integer :: ixp
      integer :: iyp
      integer :: key
      integer :: nbckgr
      integer :: nforgr
      integer :: nlevel
      CHARACTER WRDKEY*40
      COMMON /HELPNOW/ WRDKEY,NLEVEL

      if ( jaGUI.ne.1 ) then
         if ( jazekr.ne.1 ) then
            jazekr=0
         end if
         return
      end if

      IW     = NPOS(3)
      IXP    = NPOS(1) + (IWS-IW)/2
      IYP    = NPOS(2)
!     IXP    = INFOCURSOR(1)
!     IYP    = INFOCURSOR(2)
      NFORGR = InfoAttribute(13)
      NBCKGR = InfoAttribute(14)
      CALL INPOPUP('ON')
   20 CONTINUE
      CALL ITEXTCOLOUR('BWHITE','RED')
      CALL INHIGHLIGHT('BLUE','BWHITE')
      CALL TIMLIN()
      if (jazekr.eq.1) then ! SPvdP: if jazekr.eq.1, default to yes
         IOPT = IMenuTwo('NO','YES',IXP,IYP,TEXT,1,2)
      else
         IOPT = IMenuTwo('NO','YES',IXP,IYP,TEXT,1,1)
      end if
      CALL TIMLIN()
      KEY = InfoInput(55)
      CALL INFLUSH()
      IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         NLEVEL = 3
         WRDKEY = TEXT
         CALL FKEYS(KEY)
         IF (KEY .EQ. 3) THEN
            CALL INPOPUP('OFF')
            CALL ITEXTCOLOURN(NFORGR,NBCKGR)
            RETURN
         ENDIF
         GOTO 20
      ELSE IF (KEY .EQ. 21 .OR. KEY .EQ. 22) THEN
         IF (IOPT .EQ. 2) THEN
            JAZEKR = 1
         ELSE
            JAZEKR = 0
         ENDIF
      ELSE IF (KEY .EQ. 23) THEN
            JAZEKR = 0
      ELSE
         GOTO 20
      ENDIF
      CALL INPOPUP('OFF')
      CALL ITEXTCOLOURN(NFORGR,NBCKGR)

      RETURN
      END
