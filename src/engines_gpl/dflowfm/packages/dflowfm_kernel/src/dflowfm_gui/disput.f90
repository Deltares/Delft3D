!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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
module m_disput
   implicit none
contains
   subroutine DISPUT(NPUT)
      use M_SFERIC
      use M_DEVICES
      use network_data, only: kn3typ
      use m_missing, only: JINS
      use m_howtoview
      use m_ktext

      integer :: NPUT
      character TEX * 32

      if (NPUT == 0) then
         TEX = ' GET A POINT                    '
      else if (NPUT == 1) then
         TEX = ' PUT A POINT                    '
      else if (NPUT == -1) then
         TEX = ' INSERT A POINT                 '
      else if (NPUT == -2) then
         TEX = ' DELETE A POINT                 '
      else if (NPUT == -3) then
         TEX = ' DELETE A SPLINE                '
      else if (NPUT == -4 .or. NPUT == -5) then
         TEX = ' GET A SPLINE                   '
      else if (NPUT == -41 .or. NPUT == -51) then
         TEX = ' PUT A SPLINE                   '
      else if (NPUT == -6) then
         TEX = ' GET A SPLINE                   '
      else if (NPUT == 2) then
         TEX = ' GET SECOND POINT               '
      else if (NPUT == 3) then
         TEX = ' CLICK GRID POINT               '
      else if (NPUT == 4) then
         TEX = '                                '
      else if (NPUT == 5) then
         TEX = 'CLICK VIEWPOINT                 '
      else if (NPUT == 6) then
         TEX = 'PRESS + OR -                    '
      else if (NPUT == 7) then
         TEX = 'PRESS ANY KEY                   '
      else if (NPUT == 8) then
         TEX = 'CLICK BLOCK POINT 1             '
      else if (NPUT == 9) then
         TEX = 'CLICK BLOCK POINT 2             '
      else if (NPUT == 10) then
         TEX = 'CLICK LINE POINT 1              '
      else if (NPUT == 11) then
         TEX = 'CLICK LINE POINT 2              '
      else if (NPUT == 12) then
         TEX = 'ENTER OR ESC                    '
      else if (NPUT == 13) then
         TEX = 'GET POINT ON LINE               '
      else if (NPUT == 14) then
         TEX = 'CLICK INFLUENCE 1 OR RIGHT MOUSE'
      else if (NPUT == 15) then
         TEX = 'CLICK INFLUENCE 2 OR RIGHT MOUSE'
      else if (NPUT == 16) then
         TEX = 'REPLACE POINT                   '
      else if (NPUT == 17) then
         TEX = 'CLICK BLOCK 3 OR RIGHT MOUSE    '
      else if (NPUT == 18) then
         TEX = 'CLICK BLOCK 4 OR RIGHT MOUSE    '
      else if (NPUT == 19) then
         TEX = 'CLICK RIGHT MOUSE OR Escape     '
      else if (NPUT == 20) then
         TEX = ' GET A POINT ON LINE OR RIGHT MS'
      else if (NPUT == 21) then
         TEX = 'PRESS + OR - , SPACE BAR or Del '
      else if (NPUT == 22) then
         TEX = ' CLICK DEPTH POINT              '
      else if (NPUT == 23) then
         TEX = ' CLICK SAMPLE POINT             '
      else if (NPUT == 24) then
         TEX = ' PUT SAMPLE POINT               '
      else if (NPUT == 25) then
         TEX = ' INSERT SAMPLE POINT            '
      else if (NPUT == 26) then
         TEX = ' DELETE SAMPLE POINT            '
      else if (NPUT == 27) then
         TEX = ' CLICK SAMPLE POINT, CHANGE VAL.'
      else if (NPUT == 28) then
         TEX = ' GET DDBOUNDARY POINT           '
      else if (NPUT == 29) then
         TEX = ' PUT DDBOUNDARY POINT           '
      else if (NPUT == 30) then
         TEX = ' INSERT DDBOUNDARY POINT 1      '
      else if (NPUT == 31) then
         TEX = ' INSERT DDBOUNDARY POINT 2      '
      else if (NPUT == 32) then
         TEX = ' DELETE DDBOUNDARY              '
      else if (NPUT == 33) then
         TEX = ' CLICK COLOR TO CHANGE          '
      else if (NPUT == 34) then
         TEX = ' CLICK COLOR IN TABLE           '
      else if (NPUT == 35) then
         TEX = ' USE ARROW KEYS TO CHANGE COLOUR'
      else if (NPUT == 36) then
         TEX = ' INDICATE WATER RELEASE POINT   '
      else if (NPUT == 37) then
         TEX = ' PRESS + OR SPACE BAR           '
      else if (NPUT == 38) then
         TEX = ' CLICK FIRST NODE               '
      else if (NPUT == 39) then
         TEX = ' CLICK NEXT #D NODE             '
         write (TEX(13:13), '(i1)') KN3TYP ! 1D or 2D
      else if (NPUT == 40) then
         TEX = ' CLICK FIRST POL.POINT NEAR LDB '
      else if (NPUT == 41) then
         TEX = ' CLICK SECOND POL.POINT NEAR LDB'
      else if (NPUT == 42) then
         TEX = ' CLICK FIRST POL.POINT NEAR NET '
      else if (NPUT == 43) then
         TEX = ' CLICK SECOND POL.POINT NEAR NET'
      else if (NPUT == 44) then
         TEX = ' CLICK 1ST POL. START/END POINT '
      else if (NPUT == 45) then
         TEX = ' CLICK 2ND POL. START/END POINT '
      else if (NPUT == 46) then
         TEX = ' CLICK 1ST POL. START/END POINT '
      else if (NPUT == 47) then
         TEX = ' CLICK 2ND POL. START/END POINT '
      else if (NPUT == 48) then
         TEX = ' CLICK LINE'
      else if (NPUT == 49) then
         TEX = ' CLICK Sample for isocol minval '
      else if (NPUT == 50) then
         TEX = ' CLICK Sample for isocol maxval '
      else if (NPUT == 51) then
         TEX = ' CLICK FLOW NODE                '
      else if (NPUT == 52) then
         TEX = ' CLICK FLOW LINK                '
      else if (NPUT == 53) then
         TEX = ' CLICK flow node for isocol minval '
      else if (NPUT == 54) then
         TEX = ' CLICK flow node for isocol maxval '
      else if (NPUT == 55) then
         TEX = ' CLICK NET LINK                '
      else if (NPUT == 56) then
         TEX = ' GET A POINT                    '
      else if (NPUT == 57) then
         TEX = ' PUT A POINT                    '
      else if (NPUT == 58) then
         TEX = ' CLICK FIRST POINT              '
      else if (NPUT == 59) then
         TEX = ' CLICK A BOUNDARY POINT         '
      else if (NPUT == 60) then
         TEX = ' CLICK NETWORK POINT, CHANGE VAL'
      else if (NPUT == 61) then
         TEX = ' CLICK POLYGON POINT, CHANGE VAL'
      else if (NPUT == 62) then
         TEX = ' CLICK FIRST POLYGON POINT      '
      else if (NPUT == 63) then
         TEX = ' CLICK SECOND POLYGON POINT     '
      else if (NPUT == 64) then
         TEX = ' CLICK THIRD POLYGON POINT      '
      else if (NPUT == 65) then
         TEX = ' CLICK NET NODE                 '
      else if (NPUT == 466) then
         TEX = ' CLICK 1ST POL. START/END POINT '
      else if (NPUT == 477) then
         TEX = ' CLICK 2ND POL. START/END POINT '
      end if
      call KTEXT(TEX, 1, 4, 15)
!

      ! IF (JVIEW .EQ. 1) THEN
      !    CALL KTEXT(' NORMAL   ',IWS-9,IHS-1,15)
      ! ELSE IF (JVIEW .EQ. 2) THEN
      !    CALL KTEXT(' FROM LEFT',IWS-9,IHS-1,15)
      ! ELSE IF (JVIEW .EQ. 3) THEN
      !    CALL KTEXT(' FROM TOP ',IWS-9,IHS-1,15)
      ! ELSE IF (JVIEW .EQ. 4) THEN
      !    CALL KTEXT(' PERSP-view ',IWS-11,IHS-1,15)
      ! ENDIF
      if (JINS /= 1) then
         call KTEXT(' JINS=0', IWS - 16, IHS - 2, 15)
      end if

      if (JSFERIC == 1) then
         call KTEXT(' SPHERICAL', IWS - 9, IHS - 2, 15)
      else
         call KTEXT(' CARTESIAN', IWS - 9, IHS - 2, 15)
      end if

      return
   end subroutine DISPUT
end module m_disput
