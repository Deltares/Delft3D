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

      SUBROUTINE GETINT(TEXT,IVAL)
      use m_devices
      USE M_MISSING
      implicit none
      integer :: infoattribute
      integer :: infoinput
      integer :: ixp
      integer :: iyp
      integer :: key
      integer :: nbckgr
      integer :: nforgr
      integer :: nlevel
      integer :: iv
      integer :: ival
      CHARACTER WRDKEY*40, TEXT*(*)
      COMMON /HELPNOW/   WRDKEY,NLEVEL

      IV     = IVAL
      IXP    = IWS/2
      IYP    = IHS/2
      NFORGR = InfoAttribute(13)
      NBCKGR = InfoAttribute(14)
      CALL INPOPUP('ON')
   20 CONTINUE
      CALL ITEXTCOLOUR('BWHITE','RED')
      CALL INHIGHLIGHT('BLUE','BWHITE')
      CALL TIMLIN()
!      CALL INDOUBLEXYDEF(IXP,IYP,TEXT,1,VAL,6,'(F6.1)')
      CALL ININTEGERXYDEF(IXP,IYP,TEXT,1,IV,12)
      CALL TIMLIN()
      KEY = InfoInput(55)
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
         IVAL = IV
      ELSE
         IVAL = int(dmiss)
      ENDIF
      CALL INPOPUP('OFF')
      CALL ITEXTCOLOURN(NFORGR,NBCKGR)
      RETURN
   END
