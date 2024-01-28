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

      SUBROUTINE DISPOS()
      use m_devices
      use m_sferic
      implicit none
      integer :: jashow, jav, jmouse, jview, ixmax, ixmin, ixy, ndec, nxy
      double precision :: xa, ya, xlc, ylc
      double precision :: xyz
      COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW
      common /dispfor/ xyform, zform, disform
      character*7      xyform, zform, disform
      CHARACTER POSITI*25

      POSITI = 'X,Y:         ,         '
      IF (JVIEW .EQ. 2) THEN
         POSITI = 'Z,Y:         ,         '
      ELSE IF (JVIEW .EQ. 3) THEN
         POSITI = 'X-Z:         ,         '
      ENDIF

      if (jsferic == 1) then ! nou ja, laat maar even staan
         IXMIN = INT(LOG10(MAX(1d-6,min(abs(xlc),abs(ylc)))))
         IXMax = INT(LOG10(MAX(1d-6,max(abs(xlc),abs(ylc)))))

         Ixy  = abs(max(ixmin,ixmax))
         NXY  = IXY + 3
         NDEC = 9 - NXY
         IF (NDEC .GE. 1) THEN
            XYFORM = '(F10.1)'
            WRITE ( XYFORM(6:6),'(I1)') NDEC
         ELSE
           disFORM = '(E10.3)'
         ENDIF
      endif

      WRITE(POSITI (5:14),xyform) XLC
      WRITE(POSITI(16:25),xyform) YLC
      CALL KTEXT(POSITI,IWS-24,2,15)

      RETURN
      END
