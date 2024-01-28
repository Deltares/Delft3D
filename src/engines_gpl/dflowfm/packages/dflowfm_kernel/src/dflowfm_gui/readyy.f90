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

!>    plot a statusbar in the GUI
      SUBROUTINE READYY(TEXT,AF)
      use m_devices
      use unstruc_display, only: jaGUI
      implicit none

      CHARACTER TEXT*(*), BALK*400
      double precision :: af

      integer, save :: ih
      integer, save :: ini = 0
      integer, save :: iw
      integer, save :: ixp
      integer, save :: iyp
      integer :: naf

      if ( jaGUI.ne.1 ) return

      IF (INI .EQ. 0) THEN
         INI    = 1
         IXP    = 10
         IYP    = 10
         IW     = IWS - 10 - 10
         IH     = 2
         CALL ITEXTCOLOUR('BWHITE','BLUE')
         CALL IWinAction('FCP')
         CALL IWinOpenTitle(IXP,IYP,IW,IH,TEXT)
         CALL FILLUP(BALK,' ',IW)
         CALL ITEXTCOLOUR('BLACK','BWHITE')
         CALL IWinOutStringXY(2,2,BALK(1:IW))
      ELSE
         NAF = MAX(AF*IW,1d0)
         CALL FILLUP(BALK,'X',NAF)
         CALL IWinOutStringXY(1,2,BALK(1:NAF))
      ENDIF
      IF (AF .EQ. -1) THEN
         CALL IWinClose(1)
         INI = 0
         RETURN
      ENDIF
      RETURN
      END SUBROUTINE READYY
