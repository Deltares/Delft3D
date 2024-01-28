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

   SUBROUTINE DISPUT(NPUT)
   USE M_SFERIC
   USE M_DEVICES
   use network_data, only: kn3typ
   use m_missing, only: JINS
   implicit none
   integer :: jav
   integer :: jview
   double precision :: xyz
   integer :: NPUT

   COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
   CHARACTER TEX*32
   IF (NPUT .EQ. 0) THEN
      TEX =' GET A POINT                    '
   ELSE IF (NPUT .EQ. 1) THEN
      TEX =' PUT A POINT                    '
   ELSE IF (NPUT .EQ. -1) THEN
      TEX =' INSERT A POINT                 '
   ELSE IF (NPUT .EQ. -2) THEN
      TEX =' DELETE A POINT                 '
   ELSE IF (NPUT .EQ. -3) THEN
      TEX =' DELETE A SPLINE                '
   ELSE IF (NPUT .EQ. -4 .or. NPUT .EQ. -5) THEN
      TEX =' GET A SPLINE                   '
   ELSE IF (NPUT .EQ. -41 .or. NPUT .EQ. -51) THEN
      TEX =' PUT A SPLINE                   '
   ELSE IF (NPUT .EQ. -6 ) THEN
      TEX =' GET A SPLINE                   '
   ELSE IF (NPUT .EQ.  2) THEN
      TEX =' GET SECOND POINT               '
   ELSE IF (NPUT .EQ.  3) THEN
      TEX =' CLICK GRID POINT               '
   ELSE IF (NPUT .EQ.  4) THEN
      TEX ='                                '
   ELSE IF (NPUT .EQ.  5) THEN
      TEX= 'CLICK VIEWPOINT                 '
   ELSE IF (NPUT .EQ.  6) THEN
      TEX ='PRESS + OR -                    '
   ELSE IF (NPUT .EQ.  7) THEN
      TEX ='PRESS ANY KEY                   '
   ELSE IF (NPUT .EQ.  8) THEN
      TEX ='CLICK BLOCK POINT 1             '
   ELSE IF (NPUT .EQ.  9) THEN
      TEX ='CLICK BLOCK POINT 2             '
   ELSE IF (NPUT .EQ. 10) THEN
      TEX ='CLICK LINE POINT 1              '
   ELSE IF (NPUT .EQ. 11) THEN
      TEX ='CLICK LINE POINT 2              '
   ELSE IF (NPUT .EQ. 12) THEN
      TEX ='ENTER OR ESC                    '
   ELSE IF (NPUT .EQ. 13) THEN
      TEX ='GET POINT ON LINE               '
   ELSE IF (NPUT .EQ. 14) THEN
      TEX ='CLICK INFLUENCE 1 OR RIGHT MOUSE'
   ELSE IF (NPUT .EQ. 15) THEN
      TEX ='CLICK INFLUENCE 2 OR RIGHT MOUSE'
   ELSE IF (NPUT .EQ. 16) THEN
      TEX ='REPLACE POINT                   '
   ELSE IF (NPUT .EQ. 17) THEN
      TEX ='CLICK BLOCK 3 OR RIGHT MOUSE    '
   ELSE IF (NPUT .EQ. 18) THEN
      TEX ='CLICK BLOCK 4 OR RIGHT MOUSE    '
   ELSE IF (NPUT .EQ. 19) THEN
      TEX ='CLICK RIGHT MOUSE OR Escape     '
   ELSE IF (NPUT .EQ. 20) THEN
      TEX =' GET A POINT ON LINE OR RIGHT MS'
   ELSE IF (NPUT .EQ. 21) THEN
      TEX ='PRESS + OR - , SPACE BAR or Del '
   ELSE IF (NPUT .EQ. 22) THEN
      TEX =' CLICK DEPTH POINT              '
   ELSE IF (NPUT .EQ. 23) THEN
      TEX =' CLICK SAMPLE POINT             '
   ELSE IF (NPUT .EQ. 24) THEN
      TEX =' PUT SAMPLE POINT               '
   ELSE IF (NPUT .EQ. 25) THEN
      TEX =' INSERT SAMPLE POINT            '
   ELSE IF (NPUT .EQ. 26) THEN
      TEX =' DELETE SAMPLE POINT            '
   ELSE IF (NPUT .EQ. 27) THEN
      TEX =' CLICK SAMPLE POINT, CHANGE VAL.'
   ELSE IF (NPUT .EQ. 28) THEN
      TEX =' GET DDBOUNDARY POINT           '
   ELSE IF (NPUT .EQ. 29) THEN
      TEX =' PUT DDBOUNDARY POINT           '
   ELSE IF (NPUT .EQ. 30) THEN
      TEX =' INSERT DDBOUNDARY POINT 1      '
   ELSE IF (NPUT .EQ. 31) THEN
      TEX =' INSERT DDBOUNDARY POINT 2      '
   ELSE IF (NPUT .EQ. 32) THEN
      TEX =' DELETE DDBOUNDARY              '
   ELSE IF (NPUT .EQ. 33) THEN
      TEX =' CLICK COLOR TO CHANGE          '
   ELSE IF (NPUT .EQ. 34) THEN
      TEX =' CLICK COLOR IN TABLE           '
   ELSE IF (NPUT .EQ. 35) THEN
      TEX =' USE ARROW KEYS TO CHANGE COLOUR'
   ELSE IF (NPUT .EQ. 36) THEN
      TEX =' INDICATE WATER RELEASE POINT   '
   ELSE IF (NPUT .EQ. 37) THEN
      TEX =' PRESS + OR SPACE BAR           '
   ELSE IF (NPUT .EQ. 38) THEN
      TEX =' CLICK FIRST NODE               '
   ELSE IF (NPUT .EQ. 39) THEN
      TEX =' CLICK NEXT #D NODE             '
      write (TEX(13:13), '(i1)') KN3TYP ! 1D or 2D
   ELSE IF (NPUT .EQ. 40) THEN
      TEX =' CLICK FIRST POL.POINT NEAR LDB '
   ELSE IF (NPUT .EQ. 41) THEN
      TEX =' CLICK SECOND POL.POINT NEAR LDB'
   ELSE IF (NPUT .EQ. 42) THEN
      TEX =' CLICK FIRST POL.POINT NEAR NET '
   ELSE IF (NPUT .EQ. 43) THEN
      TEX =' CLICK SECOND POL.POINT NEAR NET'
   ELSE IF (NPUT .EQ. 44) THEN
      TEX =' CLICK 1ST POL. START/END POINT '
   ELSE IF (NPUT .EQ. 45) THEN
      TEX =' CLICK 2ND POL. START/END POINT '
   ELSE IF (NPUT .EQ. 46) THEN
      TEX =' CLICK 1ST POL. START/END POINT '
   ELSE IF (NPUT .EQ. 47) THEN
      TEX =' CLICK 2ND POL. START/END POINT '
   ELSE IF (NPUT .EQ. 48) THEN
      TEX =' CLICK LINE'
   ELSE IF (NPUT .EQ. 49) THEN
      TEX =' CLICK Sample for isocol minval '
   ELSE IF (NPUT .EQ. 50) THEN
      TEX =' CLICK Sample for isocol maxval '
   ELSE IF (NPUT .EQ. 51) THEN
      TEX =' CLICK FLOW NODE                '
   ELSE IF (NPUT .EQ. 52) THEN
      TEX =' CLICK FLOW LINK                '
   ELSE IF (NPUT .EQ. 53) THEN
      TEX =' CLICK flow node for isocol minval '
   ELSE IF (NPUT .EQ. 54) THEN
      TEX =' CLICK flow node for isocol maxval '
   ELSE IF (NPUT .EQ. 55) THEN
      TEX =' CLICK NET LINK                '
   ELSE IF (NPUT .EQ. 56) THEN
      TEX =' GET A POINT                    '
   ELSE IF (NPUT .EQ. 57) THEN
      TEX =' PUT A POINT                    '
   ELSE IF (NPUT .EQ. 58) THEN
      TEX =' CLICK FIRST POINT              '
   ELSE IF (NPUT .EQ. 59) THEN
      TEX =' CLICK A BOUNDARY POINT         '
   ELSE IF (NPUT .EQ. 60) THEN
      TEX =' CLICK NETWORK POINT, CHANGE VAL'
   ELSE IF (NPUT .EQ. 61) THEN
      TEX =' CLICK POLYGON POINT, CHANGE VAL'
   ELSE IF (NPUT .EQ. 62) THEN
      TEX =' CLICK FIRST POLYGON POINT      '
   ELSE IF (NPUT .EQ. 63) THEN
      TEX =' CLICK SECOND POLYGON POINT     '
   ELSE IF (NPUT .EQ. 64) THEN
      TEX =' CLICK THIRD POLYGON POINT      '
   ELSE IF (NPUT .EQ. 65) THEN
      TEX =' CLICK NET NODE                 '
   ELSE IF (NPUT .EQ. 466) THEN
      TEX =' CLICK 1ST POL. START/END POINT '
   ELSE IF (NPUT .EQ. 477) THEN
      TEX =' CLICK 2ND POL. START/END POINT '
   ENDIF
   CALL KTEXT(TEX,1,4,15)
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
   IF (JINS /= 1) THEN
      CALL KTEXT(' JINS=0',IWS-16,IHS-2,15)
   END IF

   IF (JSFERIC == 1) THEN
      CALL KTEXT(' SPHERICAL',IWS-9,IHS-2,15)
   ELSE
      CALL KTEXT(' CARTESIAN',IWS-9,IHS-2,15)
   ENDIF

   RETURN
   END SUBROUTINE DISPUT
