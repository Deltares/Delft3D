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

      SUBROUTINE EDITCOLOURTABLE(MODE,KEY)
      use unstruc_colors
      implicit none
      integer :: key
      integer :: mode
      integer :: n1
      integer :: n1c
      integer :: n2
      integer :: n2c
      integer :: n3
      integer :: n3c
      integer :: nlevel
      integer :: nput
      integer :: num
      integer :: numb
      integer :: numcol
      integer :: numcolc
      integer :: nwhat
      double precision :: xp
      double precision :: yp

      COMMON /HELPNOW/ WRDKEY,NLEVEL

      CHARACTER TEX*26, WRDKEY*40, TEX2*4

      TEX    = ' EDIT COLORTABLE          '
      WRDKEY = TEX
      NLEVEL =  2
      NUM    =  0
      NWHAT  =  0
      NPUT   =  33
      NUMB   =  14

10    continue
      CALL DRAWNU(KEY)
      CALL ALLCOLOURS()
      CALL KTEXT(TEX,1,2,15)
      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)

      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            KEY = 3
            RETURN
         ELSE
            CALL CHOICES(MODE,NUM,NWHAT,KEY)
         ENDIF
      ELSE IF (KEY .EQ. 21) THEN
!        INS KEY
         IF (NPUT .EQ. 33) THEN
            CALL GETCOLORNUMBER(XP,YP,NUMCOLC,N1C,N2C,N3C)
!           WRITE(TEX2,'(I4)') NUMCOLC
!           CALL QNMESSAGE('COLOUR NR 1 = '//TEX2)
            NPUT = 34
         ELSE IF (NPUT .EQ. 34) THEN
            CALL GETCOLORNUMBER(XP,YP,NUMCOL,N1,N2,N3)
            WRITE(TEX2,'(I4)') NUMCOL
!           CALL QNMESSAGE('IS CHANGED TO THE COLOUR OF NR : '//TEX2)
            CALL IGRPALETTERGB(NUMCOLC,N1,N2,N3)
            NPUT = 33
         ENDIF
      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY
         CALL CHANGECOLOR(XP,YP)
      ELSE IF (KEY .EQ. 23) THEN
!        ESC
         CALL IGRPALETTERGB(NUMCOLC,N1C,N2C,N3C)
      ELSE IF (KEY .EQ. 98) THEN
!        b RINGS BELL
         CALL KTEXT('B RINGS BELL',2,6,11)
         CALL OKAY(0)
      ENDIF
!
      GOTO 10
!
      END
